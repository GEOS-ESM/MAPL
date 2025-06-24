#include "MAPL_ErrLog.h"
module mapl_ComboSpec
   use mapl_GathervKernel
   use mapl_BW_Benchmark
   use mapl_ErrorHandlingMod
   use fArgParse
   use mpi
   use, intrinsic :: iso_fortran_env, only: INT64
   implicit none
   private

   public :: ComboSpec
   public :: make_ComboSpec
   public :: make_GathervKernel
   public :: make_BW_Benchmark

   type :: ComboSpec
      integer :: nx
      integer :: n_levs
      integer :: n_writers
      integer :: n_tries
   end type ComboSpec

contains

   function make_ComboSpec(rc) result(spec)
      type(ComboSpec) :: spec
      integer, optional, intent(out) :: rc

      integer :: status
      type(ArgParser) :: parser
      type(StringUnlimitedMap) :: options
      class(*), pointer :: option

      parser = ArgParser()
      call add_cli_options(parser)
      options = parser%parse_args()
      
      option => options%at('nx')
      __ASSERT(associated(option), 'nx not found')
      call cast(option, spec%nx, __RC)

      option => options%at('n_levs')
      __ASSERT(associated(option), 'n_levs not found')
      call cast(option, spec%n_levs, __RC)


      option => options%at('n_writers')
      __ASSERT(associated(option), 'n_writers not found')
      call cast(option, spec%n_writers, __RC)


      option => options%at('n_tries')
      __ASSERT(associated(option), 'n_tries not found')
      call cast(option, spec%n_tries, __RC)
      
      __RETURN(__SUCCESS)
   end function make_ComboSpec


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
           help='number of times to call kernel to get statistics', &
           type='integer',  &
           default=1, &
           action='store')

   end subroutine add_cli_options


   ! (1) Allocate and initialize buffer
   ! (2) Open per-process file, and save i/o unit
   function make_GathervKernel(spec, comm, rc) result(kernel)
      type(GathervKernel) :: kernel
      type(ComboSpec), intent(in) :: spec
      integer, intent(in) :: comm
      integer, optional, intent(out) :: rc

      integer :: status
      integer :: npes
      integer :: n

      call MPI_Comm_size(MPI_COMM_WORLD, npes, __IERROR)
      n = int(spec%nx,kind=INT64)**2 * 6 * spec%n_levs / npes

      kernel = GathervKernel(n, comm)
      call kernel%init(__RC)

      __RETURN(__SUCCESS)
   end function make_GathervKernel

   ! (1) Allocate and initialize buffer
   ! (2) Open per-process file, and save i/o unit
   function make_BW_Benchmark(spec, comm, rc) result(benchmark)
      type(BW_Benchmark) :: benchmark
      type(ComboSpec), intent(in) :: spec
      integer, intent(in) :: comm
      integer, optional, intent(out) :: rc

      integer :: status
      integer :: rank

      associate (packet_size => int(spec%nx,kind=INT64)**2 * 6* spec%n_levs/spec%n_writers)
        allocate(benchmark%buffer(packet_size), __STAT)
        call random_number(benchmark%buffer)
      end associate

      call MPI_Comm_rank(comm, rank, __IERROR)
      benchmark%filename = make_filename(base='scratch.', rank=rank, width=5, __RC)

      __RETURN(__SUCCESS)
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
      __VERIFY(status)

      write(suffix,trim(fmt), iostat=status) rank
      __VERIFY(status)
      filename = base // suffix
      
      __RETURN(__SUCCESS)
   end function make_filename

end module mapl_ComboSpec
