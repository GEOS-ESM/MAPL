#!/usr/bin/env python3
"""
latlon_to_face.py — Convert a cubed-sphere file in lat/lon (tiled-lat) format
(lat = 6 * lon) to the nf/Xdim/Ydim (face) format.

A template face file is required to supply the grid coordinate variables
(Xdim, Ydim, nf, ncontact, contacts, anchor, lons, lats, corner_lons, corner_lats)
and grid-related global attributes.

Usage:
    latlon_to_face.py --input <latlon_file> --template <face_file> \
                      --output <output_file> [--add_time | --remove_time]
"""

import argparse
import sys
import numpy as np
import netCDF4 as nc

# Variables in the lat/lon format that are not data variables.
LATLON_NON_DATA_VARS = {"lon", "lat", "lev", "time"}

# Variables in the face format that describe the grid (sourced from template).
FACE_NON_DATA_VARS = {
    "Xdim", "Ydim", "nf", "ncontact", "contacts", "anchor",
    "lons", "lats", "corner_lons", "corner_lats", "lev", "time",
}

# Global attributes that belong to the grid description (taken from template).
FACE_GRID_GLOBAL_ATTRS = {
    "Gridname", "grid_mapping_name", "file_format_version",
    "additional_vars", "gridspec_file",
}


def parse_args():
    parser = argparse.ArgumentParser(
        description="Convert cubed-sphere lat/lon format to face format (nf/Xdim/Ydim)."
    )
    parser.add_argument("--input", required=True, help="Input lat/lon-format NetCDF file.")
    parser.add_argument("--template", required=True,
                        help="Template face-format NetCDF file (provides grid metadata).")
    parser.add_argument("--output", required=True, help="Output face-format NetCDF file.")
    group = parser.add_mutually_exclusive_group()
    group.add_argument(
        "--add_time",
        action="store_true",
        help="Make data variables depend on the time dimension.",
    )
    group.add_argument(
        "--remove_time",
        action="store_true",
        help="Remove time dependence from data variables (time dim/var still written).",
    )
    return parser.parse_args()


def get_data_vars(ds):
    return [v for v in ds.variables if v not in LATLON_NON_DATA_VARS]


def has_time_dim(var):
    return "time" in var.dimensions


def copy_var_attrs(src_var, dst_var):
    for attr in src_var.ncattrs():
        if attr == "_FillValue":
            continue
        setattr(dst_var, attr, getattr(src_var, attr))


def main():
    args = parse_args()

    with nc.Dataset(args.input, "r") as src, \
         nc.Dataset(args.template, "r") as tmpl:

        # ------------------------------------------------------------------ #
        # Validate input dimensions
        # ------------------------------------------------------------------ #
        if "lon" not in src.dimensions or "lat" not in src.dimensions:
            sys.exit("ERROR: Input file does not look like a lat/lon-format file "
                     "(missing lon or lat dimensions).")

        C = len(src.dimensions["lon"])
        lat_size = len(src.dimensions["lat"])
        if lat_size != 6 * C:
            sys.exit(f"ERROR: Expected lat = 6 * lon = {6*C} but got lat={lat_size}.")

        # ------------------------------------------------------------------ #
        # Validate template dimensions
        # ------------------------------------------------------------------ #
        for dim in ("nf", "Xdim", "Ydim"):
            if dim not in tmpl.dimensions:
                sys.exit(f"ERROR: Template file is missing dimension '{dim}'.")

        tmpl_C = len(tmpl.dimensions["Xdim"])
        if tmpl_C != C:
            sys.exit(f"ERROR: Template grid size C={tmpl_C} does not match "
                     f"input grid size C={C}.")

        # ------------------------------------------------------------------ #
        # Validate --add_time / --remove_time
        # ------------------------------------------------------------------ #
        data_vars = get_data_vars(src)
        if not data_vars:
            sys.exit("ERROR: No data variables found in input file.")

        sample = src.variables[data_vars[0]]
        data_has_time = has_time_dim(sample)

        if args.add_time and data_has_time:
            sys.exit("ERROR: --add_time specified but data variables already depend on time.")
        if args.remove_time and not data_has_time:
            sys.exit("ERROR: --remove_time specified but data variables do not depend on time.")

        # ------------------------------------------------------------------ #
        # Build output file
        # ------------------------------------------------------------------ #
        with nc.Dataset(args.output, "w", format="NETCDF4") as dst:

            # --- Dimensions from template ---
            nf_size = len(tmpl.dimensions["nf"])
            ncontact_size = len(tmpl.dimensions["ncontact"])
            XCdim_size = len(tmpl.dimensions["XCdim"]) if "XCdim" in tmpl.dimensions else C + 1
            YCdim_size = len(tmpl.dimensions["YCdim"]) if "YCdim" in tmpl.dimensions else C + 1

            dst.createDimension("nf", nf_size)
            dst.createDimension("Xdim", C)
            dst.createDimension("Ydim", C)
            dst.createDimension("ncontact", ncontact_size)
            dst.createDimension("XCdim", XCdim_size)
            dst.createDimension("YCdim", YCdim_size)
            dst.createDimension("time", 1)

            if "orientationStrLen" in tmpl.dimensions:
                dst.createDimension("orientationStrLen",
                                    len(tmpl.dimensions["orientationStrLen"]))

            has_lev = "lev" in src.dimensions
            if has_lev:
                lev_size = len(src.dimensions["lev"])
                dst.createDimension("lev", lev_size)

            # --- Grid coordinate variables from template ---
            TMPL_GRID_VARS = {
                "Xdim", "Ydim", "nf", "ncontact", "contacts", "anchor",
                "lons", "lats", "corner_lons", "corner_lats",
            }
            for vname in TMPL_GRID_VARS:
                if vname not in tmpl.variables:
                    continue
                tv = tmpl.variables[vname]
                fill = getattr(tv, "_FillValue", None)
                dv = dst.createVariable(vname, tv.dtype, tv.dimensions,
                                        fill_value=fill)
                copy_var_attrs(tv, dv)
                dv[:] = tv[:]

            # --- lev: from input ---
            if has_lev:
                src_lev = src.variables["lev"]
                fill = getattr(src_lev, "_FillValue", None)
                lev_var = dst.createVariable("lev", src_lev.dtype, ("lev",),
                                             fill_value=fill)
                copy_var_attrs(src_lev, lev_var)
                lev_var[:] = src_lev[:]

            # --- time: always from input (size-1) ---
            src_time = src.variables["time"]
            fill = getattr(src_time, "_FillValue", None)
            time_var = dst.createVariable("time", src_time.dtype, ("time",),
                                          fill_value=fill)
            copy_var_attrs(src_time, time_var)
            time_var[:] = src_time[:]

            # --- Data variables ---
            for vname in data_vars:
                src_var = src.variables[vname]
                src_dims = src_var.dimensions  # e.g. ('time','lev','lat','lon') or subset

                # Build output dimensions by replacing lat->nf,Ydim and lon->Xdim,
                # and handling time add/remove.
                out_dims = []
                for d in src_dims:
                    if d == "time":
                        if args.remove_time:
                            continue
                        out_dims.append("time")
                    elif d == "lat":
                        out_dims.append("nf")
                        out_dims.append("Ydim")
                    elif d == "lon":
                        out_dims.append("Xdim")
                    elif d == "lev":
                        out_dims.append("lev")
                    else:
                        if d in dst.dimensions:
                            out_dims.append(d)

                if args.add_time and "time" not in out_dims:
                    out_dims.insert(0, "time")

                out_dims = tuple(out_dims)

                fill = getattr(src_var, "_FillValue", None)
                dst_var = dst.createVariable(
                    vname, src_var.dtype, out_dims,
                    fill_value=fill,
                )
                copy_var_attrs(src_var, dst_var)

                # Add face-format coordinate attributes
                dst_var.coordinates = "lons lats"
                dst_var.grid_mapping = "cubed_sphere"

                # Reshape data: split lat axis into (nf, Ydim)
                data = src_var[:]  # masked array

                src_dim_list = list(src_dims)
                lat_ax = src_dim_list.index("lat")

                old_shape = data.shape
                new_shape = old_shape[:lat_ax] + (nf_size, C) + old_shape[lat_ax + 1:]
                data = data.reshape(new_shape)

                # Handle time dimension
                if args.remove_time:
                    time_ax = src_dim_list.index("time")
                    data = np.squeeze(data, axis=time_ax)
                elif args.add_time:
                    data = np.expand_dims(data, axis=0)

                dst_var[:] = data

            # --- Global attributes ---
            # Start with input's attributes, then overlay template's grid attrs.
            for attr in src.ncattrs():
                setattr(dst, attr, getattr(src, attr))
            for attr in tmpl.ncattrs():
                if attr in FACE_GRID_GLOBAL_ATTRS or attr not in {a for a in src.ncattrs()}:
                    setattr(dst, attr, getattr(tmpl, attr))

    print(f"Done. Written to {args.output}")


if __name__ == "__main__":
    main()
