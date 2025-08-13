#include "MAPL_ErrLog.h"
module mapl_BW_BenchmarkSpec
   use mapl_BW_Benchmark
   use mapl_ErrorHandlingMod
   use fArgParse
   use mpi
   use, intrinsic :: iso_fortran_env, only: INT64
   implicit none
   private

   public :: BW_BenchmarkSpec
   public :: BW_Benchmark
   public :: make_BW_BenchmarkSpec
   public :: make_BW_Benchmark

   type :: BW_BenchmarkSpec
      integer :: nx
      integer :: n_levs
      integer :: n_writers
      integer :: n_tries
      character(len=:), allocatable :: file_type
   end type BW_BenchmarkSpec

contains

   function make_BW_BenchmarkSpec(rc) result(spec)
      type(BW_BenchmarkSpec) :: spec
      integer, optional, intent(out) :: rc

      integer :: status
      type(ArgParser) :: parser
      type(StringUnlimitedMap) :: options
      class(*), pointer :: option

      parser = ArgParser()
      call add_cli_options(parser)
      options = parser%parse_args()
      
      option => options%at('nx')
      _ASSERT(associated(option), 'nx not found')
      call cast(option, spec%nx, _RC)

      option => options%at('n_levs')
      _ASSERT(associated(option), 'n_levs not found')
      call cast(option, spec%n_levs, _RC)


      option => options%at('n_writers')
      _ASSERT(associated(option), 'n_writers not found')
      call cast(option, spec%n_writers, _RC)


      option => options%at('n_tries')
      _ASSERT(associated(option), 'n_tries not found')
      call cast(option, spec%n_tries, _RC)

      option => options%at('file_type')
      _ASSERT(associated(option), 'file_type not found')
      call cast(option, spec%file_type)

      _RETURN(_SUCCESS)
   end function make_BW_BenchmarkSpec

   subroutine add_cli_options(parser)
      type(ArgParser), intent(inout) :: parser

      call parser%add_argument('--nx', &
           help='dimension of cube face', &
           type='integer',  &
           action='store')

      call parser%add_argument('--n_levs', &
           help='number of vertical levels', &
           type='integer',  &
           action='store')

      call parser%add_argument('--n_writers', &
           help='number of simultaneous, independent writes to disk', &
           type='integer',  &
           action='store')

      call parser%add_argument('--n_tries', &
           help='number of times to call benchmark to get statistics', &
           type='integer',  &
           default=1, &
           action='store')

      call parser%add_argument('--file_type', &
           help='type of file, options netcdf or binary', &
           type='string', &
           action='store', &
           default='binary')

   end subroutine add_cli_options


   ! (1) Allocate and initialize buffer
   ! (2) Open per-process file, and save i/o unit
   function make_BW_Benchmark(spec, comm, rc) result(benchmark)
      type(BW_Benchmark) :: benchmark
      type(BW_BenchmarkSpec), intent(in) :: spec
      integer, intent(in) :: comm
      integer, optional, intent(out) :: rc

      integer :: status
      integer :: rank

      associate (packet_size => int(spec%nx,kind=INT64)**2 * 6* spec%n_levs/spec%n_writers)
        allocate(benchmark%buffer(packet_size), _STAT)
        call random_number(benchmark%buffer)
      end associate

      call MPI_Comm_rank(comm, rank, status)
      _VERIFY(status)
      benchmark%filename = make_filename(base='scratch.', rank=rank, width=5, _RC)
      if (spec%file_type == 'binary') benchmark%file_type = BINARY_FILE
      if (spec%file_type == 'netcdf') benchmark%file_type = NETCDF_FILE

      _RETURN(_SUCCESS)
   end function make_BW_Benchmark

   ! helper function
   function make_filename(base, rank, width, rc) result(filename)
      character(:), allocatable :: filename
      character(*), intent(in) :: base
      integer, intent(in) :: rank
      integer, intent(in) :: width
      integer, optional, intent(out) :: rc

      integer :: status
      character(len=width) :: suffix
      character(30) :: fmt

      write(fmt,'("(i",i0,".",i0,")")', iostat=status) width, width
      _VERIFY(status)

      write(suffix,trim(fmt), iostat=status) rank
      _VERIFY(status)
      filename = base // suffix
      
      _RETURN(_SUCCESS)
   end function make_filename

end module mapl_BW_BenchmarkSpec
