// ADIOS2 external plugin engine that writes one parallel NetCDF-4 (HDF5/MPI-IO) file.
//
// Load it with engine "Plugin" and the parameters
//   PluginName=NC4Writer  PluginLibrary=<path to libadios2_nc4_plugin.so>
//   FileName=<path>       output file; ADIOS2 passes the plugin name, not the name
//                         given to Open, to the plugin
// Optional parameters (case-insensitive keys):
//   MPIComm=<Fortran handle>   communicator for the file (default MPI_COMM_WORLD)
//   NumSteps=N            length of the time dimension; 0 (default) makes it unlimited,
//                         which forces chunked storage and collective record writes,
//                         so every rank must then Put every record variable each step
//   TimeDimension=name    record dimension name (default "time")
//   Layout=contiguous|chunked   storage of arrays without an unlimited dimension
//                         (chunked: full extent on the two fastest dims, 1 elsewhere)
//   Access=collective|independent   MPI-IO mode for distributed arrays
//   ArrayOrder=RowMajor|ColumnMajor   order of Shape/Start/Count; set ColumnMajor
//                         from Fortran (the public C++ API does not expose the IO order)
//
// Variable mapping:
//   - Dimension names come from the variable attribute "dimensions" (blank- or
//     comma-separated, in the variable's own array order). One extra name equal to
//     TimeDimension makes the variable a record variable. Without the attribute, a
//     1-D array is a coordinate variable named after itself, and other axes use the
//     unique 1-D variable of the same length, or "dim<len>".
//   - A global value named TimeDimension is the time coordinate.
//   - With ArrayOrder=ColumnMajor dims are reversed, so the file matches nf90 output.
//   - Global values and arrays whose Count equals Shape are written independently,
//     since typically only one rank writes them.
//   - Each Put is written in the current step: sync puts immediately, deferred puts
//     at PerformPuts/EndStep/Close.

#include "adios2/plugin/PluginEngineInterface.h"

#include <mpi.h>
#include <netcdf.h>
#include <netcdf_par.h>

#include <algorithm>
#include <cctype>
#include <cstdint>
#include <functional>
#include <iostream>
#include <map>
#include <set>
#include <sstream>
#include <stdexcept>
#include <string>
#include <vector>

namespace adios2
{
namespace plugin
{

namespace
{

template <class T>
struct NcType;
template <> struct NcType<int8_t> { static constexpr nc_type value = NC_BYTE; };
template <> struct NcType<int16_t> { static constexpr nc_type value = NC_SHORT; };
template <> struct NcType<int32_t> { static constexpr nc_type value = NC_INT; };
template <> struct NcType<int64_t> { static constexpr nc_type value = NC_INT64; };
template <> struct NcType<uint8_t> { static constexpr nc_type value = NC_UBYTE; };
template <> struct NcType<uint16_t> { static constexpr nc_type value = NC_USHORT; };
template <> struct NcType<uint32_t> { static constexpr nc_type value = NC_UINT; };
template <> struct NcType<uint64_t> { static constexpr nc_type value = NC_UINT64; };
template <> struct NcType<float> { static constexpr nc_type value = NC_FLOAT; };
template <> struct NcType<double> { static constexpr nc_type value = NC_DOUBLE; };
template <> struct NcType<char> { static constexpr nc_type value = NC_CHAR; };

// ADIOS2 types that have a NetCDF-4 equivalent.
#define NC4_FOREACH_TYPE(MACRO)                                                                    \
    MACRO(char)                                                                                    \
    MACRO(int8_t)                                                                                  \
    MACRO(int16_t)                                                                                 \
    MACRO(int32_t)                                                                                 \
    MACRO(int64_t)                                                                                 \
    MACRO(uint8_t)                                                                                 \
    MACRO(uint16_t)                                                                                \
    MACRO(uint32_t)                                                                                \
    MACRO(uint64_t)                                                                                \
    MACRO(float)                                                                                   \
    MACRO(double)

std::string Lower(std::string s)
{
    std::transform(s.begin(), s.end(), s.begin(), [](unsigned char c) { return std::tolower(c); });
    return s;
}

std::string Trim(const std::string &s)
{
    const auto b = s.find_first_not_of(" \t\n\r");
    if (b == std::string::npos)
        return "";
    return s.substr(b, s.find_last_not_of(" \t\n\r") - b + 1);
}

std::vector<std::string> SplitNames(const std::string &s)
{
    std::string t = s;
    std::replace(t.begin(), t.end(), ',', ' ');
    std::istringstream in(t);
    std::vector<std::string> out;
    for (std::string w; in >> w;)
        out.push_back(w);
    return out;
}

} // end anonymous namespace

class NC4WriterPlugin : public PluginEngineInterface
{
public:
    NC4WriterPlugin(adios2::IO io, const std::string &name, const Mode mode)
    : PluginEngineInterface(io, name, mode)
    {
    }

    ~NC4WriterPlugin() override
    {
        if (m_NcId >= 0)
            nc_close(m_NcId);
        if (m_NcComm != MPI_COMM_NULL)
            MPI_Comm_free(&m_NcComm);
    }

    void Init() override
    {
        Params params;
        for (const auto &p : m_IO.Parameters())
            params[Lower(p.first)] = p.second;
        auto get = [&](const std::string &key, const std::string &def) {
            auto it = params.find(Lower(key));
            return it == params.end() ? def : Trim(it->second);
        };

        const std::string fcomm = get("MPIComm", "");
        MPI_Comm comm = fcomm.empty() ? MPI_COMM_WORLD : MPI_Comm_f2c(std::stoi(fcomm));
        MPI_Comm_dup(comm, &m_NcComm);
        MPI_Comm_rank(m_NcComm, &m_Rank);
        if (m_OpenMode != Mode::Write)
            Fail("Init", "only Mode::Write is supported", NC_NOERR);
        m_NumSteps = std::stoul(get("NumSteps", "0"));
        m_TimeDim = get("TimeDimension", "time");
        const std::string layout = Lower(get("Layout", "contiguous"));
        const std::string access = Lower(get("Access", "collective"));
        if (layout != "contiguous" && layout != "chunked")
            Fail("Init", "Layout must be contiguous|chunked", NC_NOERR);
        if (access != "collective" && access != "independent")
            Fail("Init", "Access must be collective|independent", NC_NOERR);
        const std::string order = Lower(get("ArrayOrder", "RowMajor"));
        if (order != "rowmajor" && order != "columnmajor")
            Fail("Init", "ArrayOrder must be RowMajor|ColumnMajor", NC_NOERR);
        m_Chunked = (layout == "chunked");
        m_Collective = (access == "collective");
        m_ColumnMajor = (order == "columnmajor");

        // Same ROMIO hints as nc4_parallel_cs_writer.x.
        MPI_Info info;
        MPI_Info_create(&info);
        MPI_Info_set(info, "romio_cb_write", "enable");
        MPI_Info_set(info, "romio_ds_write", "disable");
        const std::string fileName = get("FileName", m_Name);
        Check(nc_create_par(fileName.c_str(), NC_CLOBBER | NC_NETCDF4, m_NcComm, info, &m_NcId),
              "nc_create_par " + fileName);
        MPI_Info_free(&info);
        int oldMode;
        Check(nc_set_fill(m_NcId, NC_NOFILL, &oldMode), "nc_set_fill");
        m_DefineMode = true;

        SyncDefinitions();
    }

    StepStatus BeginStep(StepMode, const float) override
    {
        if (m_InStep)
            Fail("BeginStep", "BeginStep called twice without EndStep", NC_NOERR);
        m_InStep = true;
        SyncDefinitions();
        return StepStatus::OK;
    }

    void EndStep() override
    {
        PerformPuts();
        m_InStep = false;
        ++m_CurrentStep;
    }

    size_t CurrentStep() const override { return m_CurrentStep; }

    void PerformPuts() override
    {
        auto pending = std::move(m_Deferred);
        m_Deferred.clear();
        for (auto &f : pending)
            f();
    }

#define declare(T)                                                                                 \
    void DoPutSync(adios2::Variable<T> variable, const T *values) override                         \
    {                                                                                              \
        Put(variable, values, false);                                                              \
    }                                                                                              \
    void DoPutDeferred(adios2::Variable<T> variable, const T *values) override                     \
    {                                                                                              \
        Put(variable, values, true);                                                               \
    }
    ADIOS2_FOREACH_STDTYPE_1ARG(declare)
#undef declare

    void DoClose(const int) override
    {
        PerformPuts();
        if (m_NcId >= 0)
        {
            Check(nc_close(m_NcId), "nc_close");
            m_NcId = -1;
        }
    }

private:
    struct VarInfo
    {
        int varid = -1;
        bool record = false;      // has the time dimension first (C order)
        bool reverse = false;     // ADIOS2 dims are column-major
        bool whole = false;       // one block covers the whole variable
    };

    int m_NcId = -1;
    MPI_Comm m_NcComm = MPI_COMM_NULL;
    int m_Rank = 0;
    size_t m_NumSteps = 0;
    std::string m_TimeDim;
    bool m_Chunked = false;
    bool m_Collective = true;
    bool m_ColumnMajor = false;
    bool m_DefineMode = false;
    bool m_InStep = false;
    size_t m_CurrentStep = 0;
    std::map<std::string, VarInfo> m_Vars;
    std::map<std::string, std::pair<int, size_t>> m_Dims; // name -> (dimid, length)
    std::set<std::string> m_Attrs;
    std::vector<std::function<void()>> m_Deferred;

    [[noreturn]] void Fail(const std::string &where, const std::string &msg, int status) const
    {
        std::string text = "NC4WriterPlugin::" + where + ": " + msg;
        if (status != NC_NOERR)
            text += ": " + std::string(nc_strerror(status));
        std::cerr << "rank " << m_Rank << ": " << text << std::endl;
        throw std::runtime_error(text);
    }

    void Check(int status, const std::string &where) const
    {
        if (status != NC_NOERR)
            Fail(where, "NetCDF error", status);
    }

    int DefineDim(const std::string &name, size_t len)
    {
        auto it = m_Dims.find(name);
        if (it != m_Dims.end())
        {
            if (it->second.second != len)
                Fail("DefineDim", "dimension " + name + " redefined with a different length",
                     NC_NOERR);
            return it->second.first;
        }
        int dimid;
        Check(nc_def_dim(m_NcId, name.c_str(), len, &dimid), "nc_def_dim " + name);
        m_Dims[name] = {dimid, len};
        return dimid;
    }

    std::string DimsAttribute(const std::string &varName)
    {
        auto attr = m_IO.InquireAttribute<std::string>("dimensions", varName, "/");
        if (!attr)
            return "";
        auto data = attr.Data();
        return data.empty() ? "" : Trim(data.front());
    }

    // Name of a 1-D variable with this length if exactly one exists.
    std::string CoordinateFor(size_t len, const std::map<std::string, Params> &vars)
    {
        std::string found;
        int n = 0;
        for (const auto &v : vars)
        {
            auto it = v.second.find("Shape");
            if (it == v.second.end())
                continue;
            if (it->second.find(',') == std::string::npos && Trim(it->second) == std::to_string(len))
            {
                found = v.first;
                ++n;
            }
        }
        return n == 1 ? found : "dim" + std::to_string(len);
    }

    template <class T>
    void DefineVariable(const std::string &name, const std::map<std::string, Params> &vars,
                        std::vector<std::string> &newVars)
    {
        auto v = m_IO.InquireVariable<T>(name);
        if (!v)
            return;
        const auto shapeId = v.ShapeID();
        if (shapeId != ShapeID::GlobalValue && shapeId != ShapeID::GlobalArray)
        {
            if (m_Rank == 0)
                std::cerr << "NC4WriterPlugin: skipping local/joined variable " << name << std::endl;
            m_Vars[name] = VarInfo{};
            return;
        }

        VarInfo vi;
        vi.reverse = m_ColumnMajor;
        const Dims shape = (shapeId == ShapeID::GlobalArray) ? v.Shape() : Dims();
        std::vector<std::string> names = SplitNames(DimsAttribute(name));

        if (shapeId == ShapeID::GlobalValue)
        {
            vi.record = (name == m_TimeDim) || (names.size() == 1 && names[0] == m_TimeDim);
            names.clear();
        }
        else if (names.empty())
        {
            if (shape.size() == 1)
                names.push_back(name);
            else
                for (size_t len : shape)
                    names.push_back(CoordinateFor(len, vars));
        }
        else if (names.size() == shape.size() + 1)
        {
            const std::string &extra = vi.reverse ? names.back() : names.front();
            if (extra != m_TimeDim)
                Fail("DefineVariable", name + ": extra dimension must be " + m_TimeDim, NC_NOERR);
            vi.record = true;
            if (vi.reverse)
                names.pop_back();
            else
                names.erase(names.begin());
        }
        else if (names.size() != shape.size())
        {
            Fail("DefineVariable", name + ": 'dimensions' does not match the shape", NC_NOERR);
        }

        // NetCDF order: C order, record dimension first.
        std::vector<size_t> lens(shape.begin(), shape.end());
        if (vi.reverse)
        {
            std::reverse(names.begin(), names.end());
            std::reverse(lens.begin(), lens.end());
        }
        std::vector<int> dimids;
        if (vi.record)
            dimids.push_back(DefineDim(m_TimeDim, m_NumSteps > 0 ? m_NumSteps : NC_UNLIMITED));
        for (size_t k = 0; k < names.size(); ++k)
            dimids.push_back(DefineDim(names[k], lens[k]));

        Check(nc_def_var(m_NcId, name.c_str(), NcType<T>::value, static_cast<int>(dimids.size()),
                         dimids.data(), &vi.varid),
              "nc_def_var " + name);

        const bool unlimited = vi.record && m_NumSteps == 0;
        if (dimids.size() >= 2 && (m_Chunked || unlimited))
        {
            std::vector<size_t> chunks;
            if (vi.record)
                chunks.push_back(1);
            for (size_t k = 0; k < lens.size(); ++k)
                chunks.push_back(k + 2 >= lens.size() ? lens[k] : 1);
            Check(nc_def_var_chunking(m_NcId, vi.varid, NC_CHUNKED, chunks.data()),
                  "nc_def_var_chunking " + name);
        }
        else if (!dimids.empty() && !unlimited)
        {
            Check(nc_def_var_chunking(m_NcId, vi.varid, NC_CONTIGUOUS, nullptr),
                  "nc_def_var_chunking " + name);
        }

        vi.whole = (shapeId == ShapeID::GlobalValue) || (v.Count() == v.Shape());
        m_Vars[name] = vi;
        newVars.push_back(name);
    }

    template <class T>
    void PutAttribute(const std::string &fullName, int varid, const std::string &attName)
    {
        auto a = m_IO.InquireAttribute<T>(fullName);
        if (!a)
            return;
        auto data = a.Data();
        Check(nc_put_att(m_NcId, varid, attName.c_str(), NcType<T>::value, data.size(), data.data()),
              "nc_put_att " + fullName);
    }

    void WriteAttribute(const std::string &fullName)
    {
        int varid = NC_GLOBAL;
        std::string attName = fullName;
        const auto slash = fullName.rfind('/');
        if (slash != std::string::npos)
        {
            auto it = m_Vars.find(fullName.substr(0, slash));
            if (it != m_Vars.end())
            {
                if (it->second.varid < 0)
                    return;
                varid = it->second.varid;
                attName = fullName.substr(slash + 1);
            }
        }
        if (varid != NC_GLOBAL && attName == "dimensions")
            return;

        const std::string type = m_IO.AttributeType(fullName);
        if (type == GetType<std::string>())
        {
            auto a = m_IO.InquireAttribute<std::string>(fullName);
            std::string text;
            for (const auto &s : a.Data())
                text += (text.empty() ? "" : "\n") + s;
            Check(nc_put_att_text(m_NcId, varid, attName.c_str(), text.size(), text.c_str()),
                  "nc_put_att_text " + fullName);
        }
#define declare(T)                                                                                 \
    else if (type == GetType<T>()) { PutAttribute<T>(fullName, varid, attName); }
        NC4_FOREACH_TYPE(declare)
#undef declare
    }

    // Define in the file every variable and attribute not yet there.
    void SyncDefinitions()
    {
        const auto vars = m_IO.AvailableVariables();
        const auto attrs = m_IO.AvailableAttributes();
        bool pending = false;
        for (const auto &v : vars)
            pending = pending || !m_Vars.count(v.first);
        for (const auto &a : attrs)
            pending = pending || !m_Attrs.count(a.first);
        if (!pending)
            return;

        if (!m_DefineMode)
        {
            Check(nc_redef(m_NcId), "nc_redef");
            m_DefineMode = true;
        }
        std::vector<std::string> newVars;
        for (const auto &v : vars)
        {
            if (m_Vars.count(v.first))
                continue;
            const std::string type = m_IO.VariableType(v.first);
            if (false)
            {
            }
#define declare(T)                                                                                 \
    else if (type == GetType<T>()) { DefineVariable<T>(v.first, vars, newVars); }
            NC4_FOREACH_TYPE(declare)
#undef declare
            else
            {
                if (m_Rank == 0)
                    std::cerr << "NC4WriterPlugin: skipping variable " << v.first << " of type "
                              << type << std::endl;
                m_Vars[v.first] = VarInfo{};
            }
        }
        for (const auto &a : attrs)
        {
            if (m_Attrs.insert(a.first).second)
                WriteAttribute(a.first);
        }
        Check(nc_enddef(m_NcId), "nc_enddef");
        m_DefineMode = false;

        for (const auto &name : newVars)
        {
            const VarInfo &vi = m_Vars[name];
            const int mode = (vi.whole || !m_Collective) ? NC_INDEPENDENT : NC_COLLECTIVE;
            // Extending an unlimited dimension requires collective access.
            const int access = (vi.record && m_NumSteps == 0) ? NC_COLLECTIVE : mode;
            Check(nc_var_par_access(m_NcId, vi.varid, access), "nc_var_par_access " + name);
        }
    }

    template <class T>
    void Put(adios2::Variable<T> variable, const T *values, bool deferred)
    {
        const std::string name = variable.Name();
        if (!m_Vars.count(name))
            SyncDefinitions();
        const VarInfo vi = m_Vars.at(name);
        if (vi.varid < 0)
            return;

        std::vector<size_t> start, count;
        if (variable.ShapeID() == ShapeID::GlobalArray)
        {
            const Dims s = variable.Start(), c = variable.Count();
            start.assign(s.begin(), s.end());
            count.assign(c.begin(), c.end());
            if (vi.reverse)
            {
                std::reverse(start.begin(), start.end());
                std::reverse(count.begin(), count.end());
            }
        }
        if (vi.record)
        {
            if (m_NumSteps > 0 && m_CurrentStep >= m_NumSteps)
                Fail("Put", name + ": step exceeds NumSteps", NC_NOERR);
            start.insert(start.begin(), m_CurrentStep);
            count.insert(count.begin(), 1);
        }
        if (start.empty())
        {
            start.push_back(0);
            count.push_back(1);
        }

        auto write = [this, name, vi, start, count, values]() {
            Check(nc_put_vara(m_NcId, vi.varid, start.data(), count.data(), values),
                  "nc_put_vara " + name);
        };
        if (deferred)
            m_Deferred.push_back(write);
        else
            write();
    }
};

} // end namespace plugin
} // end namespace adios2

// The PluginEngine in Baselibs' ADIOS2 2.12 is compiled without MPI and calls the
// three-argument EngineCreate; the communicator comes from the MPIComm parameter.
extern "C" {

adios2::plugin::PluginEngineInterface *EngineCreate(adios2::IO io, const std::string &name,
                                                    const adios2::Mode mode)
{
    return new adios2::plugin::NC4WriterPlugin(io, name, mode);
}

void EngineDestroy(adios2::plugin::PluginEngineInterface *obj) { delete obj; }
}
