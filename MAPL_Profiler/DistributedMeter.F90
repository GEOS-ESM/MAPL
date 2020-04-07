module MAPL_DistributedMeter
   use, intrinsic :: iso_fortran_env, only: REAL64
   use MAPL_AdvancedMeter
   use MAPL_AbstractGauge
   use MPI
   implicit none
   private

   public :: DistributedMeter
   public :: DistributedReal64
   public :: DistributedInteger
   public :: DistributedStatistics
   public :: operator(.reduce.)

   interface operator(.reduce.)
      module procedure reduce_distributed_real64
      module procedure reduce_distributed_integer
      module procedure reduce_distributed_data
   end interface

   type :: DistributedReal64
      sequence
      real(kind=REAL64) :: total = 0
      real(kind=REAL64) :: min = huge(1._REAL64)
      real(kind=REAL64) :: max = -huge(1._REAL64)
      integer :: min_pe = huge(1)
      integer :: max_pe = -1
      integer :: num_pes = 1
      integer :: pad
   end type DistributedReal64

   type :: DistributedInteger
      sequence
      integer :: total = 0
      integer :: min
      integer :: max
      integer :: min_pe = huge(1)
      integer :: max_pe = -1
      integer :: num_pes = 1
   end type DistributedInteger

   type :: DistributedStatistics
      sequence
      type(DistributedReal64) :: total
      type(DistributedReal64) :: exclusive
      type(DistributedReal64) :: min_cycle
      type(DistributedReal64) :: max_cycle
      type(DistributedReal64) :: mean_cycle
      type(DistributedReal64) :: sum_square_deviation
      type(DistributedInteger) :: num_cycles
   end type DistributedStatistics

   type, extends(AdvancedMeter) :: DistributedMeter
      private
      type(DistributedStatistics) :: statistics
   contains
      !procedure :: reduce_global
      procedure :: reduce_mpi
      generic :: reduce => reduce_mpi !,reduce_global

      procedure :: get_statistics
      procedure :: get_stats_total
      procedure :: get_stats_min_cycle
      procedure :: get_stats_max_cycle
      procedure :: get_stats_num_cycles
!!$      procedure :: get_stats_sum_square_deviation

      procedure :: make_mpi_type_distributed_data
      procedure :: make_mpi_type_distributed_real64
      procedure :: make_mpi_type_distributed_integer
      generic :: make_mpi_type => make_mpi_type_distributed_data
      generic :: make_mpi_type => make_mpi_type_distributed_real64
      generic :: make_mpi_type => make_mpi_type_distributed_integer
   end type DistributedMeter


   interface DistributedReal64
      module procedure :: new_DistributedReal64
   end interface DistributedReal64

   interface DistributedInteger
      module procedure :: new_DistributedInteger
   end interface DistributedInteger

   interface DistributedMeter
      module procedure :: new_DistributedMeter
   end interface DistributedMeter


   logical, save :: initialized = .false.

   integer, save :: mpi_dist_type
   integer, save :: mpi_reduce_op

contains

   function new_DistributedReal64(value, rank) result(distributed_real64)
      type(DistributedReal64) :: distributed_real64
      real(kind=REAL64), intent(in) :: value
      integer, intent(in) :: rank

      distributed_real64%total = value
      distributed_real64%min = value
      distributed_real64%max = value
      distributed_real64%min_pe = rank
      distributed_real64%max_pe = rank
      distributed_real64%num_pes = 1

   end function new_DistributedReal64

   function new_DistributedInteger(value, rank) result(distributed_integer)
      type(DistributedInteger) :: distributed_integer
      integer, intent(in) :: value
      integer, intent(in) :: rank

      distributed_integer%total = value
      distributed_integer%min = value
      distributed_integer%max = value
      distributed_integer%min_pe = rank
      distributed_integer%max_pe = rank
      distributed_integer%num_pes = 1
   end function new_DistributedInteger


   function new_DistributedMeter(gauge) result(distributed_meter)
      type(DistributedMeter) :: distributed_meter
      class(AbstractGauge), intent(in) :: gauge

      integer :: ierror
   
      if (.not. initialized) then
         call initialize(ierror)
         initialized = .true.
      end if

      distributed_meter%AdvancedMeter = AdvancedMeter(gauge)

   end function new_DistributedMeter

   subroutine initialize(ierror)
      integer, intent(out) :: ierror

      type (DistributedMeter) :: dummy
      logical :: commute
      
      call dummy%make_mpi_type(dummy%statistics, mpi_dist_type, ierror)
      call MPI_Type_commit(mpi_dist_type, ierror)

      commute = .true.
      call MPI_Op_create(true_reduce, commute, mpi_reduce_op, ierror)

   end subroutine initialize

   function get_statistics(this) result(statistics)
      type (DistributedStatistics) :: statistics
      class (DistributedMeter), intent(in) :: this
      statistics = this%statistics
   end function get_statistics

   function reduce_distributed_real64(a, b) result(c)
      type(DistributedReal64) :: c
      type(DistributedReal64), intent(in) :: a
      type(DistributedReal64), intent(in) :: b

      c%total = a%total + b%total
      
      if (b%min < a%min) then
         c%min_pe = b%min_pe
      elseif (a%min < b%min) then
         c%min_pe = a%min_pe
      else ! tie
         c%min_pe = min(a%min_pe, b%min_pe)
      end if
      c%min = min(a%min, b%min)
      
      if (b%max > a%max) then
         c%max_pe = b%max_pe
      elseif (a%max < b%max) then
         c%max_pe = a%max_pe
      else ! tie
         c%max_pe = min(a%max_pe, b%max_pe)
      end if
      c%max = max(a%max, b%max)

      c%num_pes = a%num_pes + b%num_pes

   end function reduce_distributed_real64
      

   function reduce_distributed_integer(a, b) result(c)
      type(DistributedInteger) :: c
      type(DistributedInteger), intent(in) :: a
      type(DistributedInteger), intent(in) :: b

      c%total = a%total + b%total

      if (b%min < a%min) then
         c%min_pe = b%min_pe
      elseif (a%min < b%min) then
         c%min_pe = a%min_pe
      else ! tie
         c%min_pe = min(a%min_pe, b%min_pe)
      end if
      c%min = min(a%min, b%min)
      
      if (b%max > a%max) then
         c%max_pe = b%max_pe
      elseif (a%max < b%max) then
         c%max_pe = a%max_pe
      else ! tie
         c%max_pe = min(a%max_pe, b%max_pe)
      end if
      c%max = max(a%max, b%max)

      c%num_pes = a%num_pes + b%num_pes

   end function reduce_distributed_integer
      

   function reduce_distributed_data(a, b) result(c)
      type(DistributedStatistics) :: c
      type(DistributedStatistics), intent(in) :: a
      type(DistributedStatistics), intent(in) :: b

      c%total = a%total .reduce. b%total
      c%exclusive = a%exclusive .reduce. b%exclusive
      c%min_cycle = a%min_cycle .reduce. b%min_cycle
      
      c%max_cycle = a%max_cycle .reduce. b%max_cycle
      c%sum_square_deviation = a%sum_square_deviation .reduce. b%sum_square_deviation
      c%num_cycles = a%num_cycles .reduce. b%num_cycles
      
   end function reduce_distributed_data


   function get_stats_total(this) result(total)
      type(DistributedReal64) :: total
      class(DistributedMeter), intent(in) :: this

      total = this%statistics%total
   end function get_stats_total
   
   function get_stats_min_cycle(this) result(min_cycle)
      type(DistributedReal64) :: min_cycle
      class(DistributedMeter), intent(in) :: this

      min_cycle = this%statistics%min_cycle
   end function get_stats_min_cycle
   
   function get_stats_max_cycle(this) result(max_cycle)
      type(DistributedReal64) :: max_cycle
      class(DistributedMeter), intent(in) :: this

      max_cycle = this%statistics%max_cycle
   end function get_stats_max_cycle
   
   function get_stats_num_cycles(this) result(num_cycles)
      type(DistributedInteger) :: num_cycles
      class(DistributedMeter), intent(in) :: this

      num_cycles = this%statistics%num_cycles
   end function get_stats_num_cycles

   
   subroutine reduce_global(this, exclusive)
      class(DistributedMeter), intent(inout) :: this
      real(kind=REAL64), intent(in) :: exclusive
      call this%reduce(MPI_COMM_WORLD, exclusive)
   end subroutine reduce_global
   

   subroutine reduce_mpi(this, comm, exclusive)
      class(DistributedMeter), intent(inout) :: this
      integer, intent(in) :: comm
      real(kind=REAL64), intent(in) :: exclusive

      integer :: ierror

      integer :: dist_type
      integer :: rank
      type(DistributedStatistics) :: tmp, tmp2

      call MPI_Comm_rank(comm, rank, ierror)

      this%statistics%total = DistributedReal64(this%get_total(), rank)
      this%statistics%exclusive = DistributedReal64(exclusive, rank)
      this%statistics%min_cycle = DistributedReal64(this%get_min_cycle(), rank)
      this%statistics%max_cycle = DistributedReal64(this%get_max_cycle(), rank)
      this%statistics%sum_square_deviation = DistributedReal64(this%get_sum_square_deviation(), rank)
      this%statistics%num_cycles = DistributedInteger(this%get_num_cycles(), rank)

      tmp = this%statistics
      call MPI_Reduce(tmp, this%statistics, 1, mpi_dist_type, mpi_reduce_op, 0, comm, ierror)

   end subroutine reduce_mpi


   subroutine make_mpi_type_distributed_real64(this, r64, new_type, ierror)
      class (DistributedMeter), intent(in) :: this
      type (DistributedReal64), intent(in) :: r64 ! used only for generic resolution
      integer, intent(out) :: new_type
      integer, intent(out) :: ierror

      integer(kind=MPI_ADDRESS_KIND) :: displacements(2)
      integer(kind=MPI_ADDRESS_KIND) :: lb, sz

      call MPI_Type_get_extent_x(MPI_REAL8, lb, sz, ierror)
      displacements = [0_MPI_ADDRESS_KIND, 3*sz]

      call MPI_Type_create_struct(2, [3,4], displacements, [MPI_REAL8, MPI_INTEGER], new_type, ierror)

   end subroutine make_mpi_type_distributed_real64


   subroutine make_mpi_type_distributed_integer(this, int, new_type, ierror)
      class (DistributedMeter), intent(in) :: this
      type (DistributedInteger), intent(in) :: int ! used only for generic resolution
      integer, intent(out) :: new_type
      integer, intent(out) :: ierror

      integer(kind=MPI_ADDRESS_KIND) :: displacements(1)

      displacements = [0_MPI_ADDRESS_KIND]
      call MPI_Type_create_struct(1, [6], displacements, [MPI_INTEGER], new_type, ierror)

   end subroutine make_mpi_type_distributed_integer


   subroutine make_mpi_type_distributed_data(this, d, new_type, ierror)
      class (DistributedMeter), intent(in) :: this
      type (DistributedStatistics), intent(in) :: d ! used only for generic resolution
      integer, intent(out) :: new_type
      integer, intent(out) :: ierror

      integer(kind=MPI_ADDRESS_KIND) :: displacements(2)
      integer(kind=MPI_ADDRESS_KIND) :: lb, sz, sz2
      integer :: type_dist_real64, type_dist_integer

      call this%make_mpi_type(this%statistics%total, type_dist_real64, ierror)
      call this%make_mpi_type(this%statistics%num_cycles, type_dist_integer, ierror)

      call MPI_Type_get_extent_x(type_dist_real64, lb, sz, ierror)
      displacements = [0_MPI_ADDRESS_KIND, 6*sz]
      call MPI_Type_create_struct(2, [6,1], displacements, [type_dist_real64, type_dist_integer], new_type, ierror)
      call MPI_Type_get_extent_x(new_type, lb, sz2, ierror)

   end subroutine make_mpi_type_distributed_data



   subroutine true_reduce(invec, inoutvec, len, type)
      integer, intent(in) :: len
      type(DistributedStatistics), intent(in) :: invec(len)
      type(DistributedStatistics), intent(inout) :: inoutvec(len)
      integer, intent(in) :: type

      integer :: i

      do i = 1, len
         inoutvec(i) = invec(i) .reduce. inoutvec(i)
      end do
   
   end subroutine true_reduce
   
end module MAPL_DistributedMeter


