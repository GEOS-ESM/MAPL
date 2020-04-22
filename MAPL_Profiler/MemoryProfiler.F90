module MAPL_MemoryProfiler_private
   use MAPL_BaseProfiler, only: BaseProfiler
   use MAPL_BaseProfiler, only: MemoryProfilerIterator => BaseProfilerIterator

   use MAPL_RssMemoryGauge
   use MAPL_VmstatMemoryGauge
   use MAPL_AdvancedMeter
   use MAPL_AbstractMeter
   use MAPL_MeterNode
   implicit none
   private

   public :: MemoryProfiler
   public :: MemoryProfilerIterator
   public :: get_global_memory_profiler


   type, extends(BaseProfiler) :: MemoryProfiler
      private
   contains
      procedure :: make_meter
      procedure :: copy
   end type MemoryProfiler

   interface MemoryProfiler
      module procedure new_MemoryProfiler
   end interface MemoryProfiler

   type(MemoryProfiler), protected, target :: global_memory_profiler

contains


   function new_MemoryProfiler(name, comm_world) result(prof)
      type(MemoryProfiler), target :: prof
      character(*), intent(in) :: name
      integer, optional, intent(in) :: comm_world

      call prof%set_comm_world(comm_world = comm_world)
      call prof%set_node(MeterNode(name, prof%make_meter()))
      call prof%start()

   end function new_MemoryProfiler

   function make_meter(this) result(meter)
      class(AbstractMeter), allocatable :: meter
      class(MemoryProfiler), intent(in) :: this
      meter = AdvancedMeter(RssMemoryGauge())
!!$      meter = AdvancedMeter(VmstatMemoryGauge())
   end function make_meter


   function get_global_memory_profiler() result(memory_profiler)
      type(MemoryProfiler), pointer :: memory_profiler

      memory_profiler => global_memory_profiler

   end function get_global_memory_profiler


   subroutine copy(new, old)
      class(MemoryProfiler), target, intent(inout) :: new
      class(BaseProfiler), target, intent(in) :: old

      call new%copy_profiler(old)

   end subroutine copy


end module MAPL_MemoryProfiler_private



module MAPL_MemoryProfiler
   use MAPL_BaseProfiler
   use MAPL_MemoryProfiler_private
   implicit none
   private

   public :: MemoryProfiler
   public :: MemoryProfilerIterator
   public :: get_global_memory_profiler
   public :: initialize
   public :: finalize
   public :: start
   public :: stop

contains

   subroutine initialize(name)
      character(*), optional, intent(in) :: name

      type(MemoryProfiler), pointer :: memory_profiler
      character(:), allocatable :: name_

      if (present(name)) then
         name_ = name
      else
         name_ = 'top'
      end if

      memory_profiler => get_global_memory_profiler()
      memory_profiler = MemoryProfiler(name_)

   end subroutine initialize


   subroutine finalize()

      type(MemoryProfiler), pointer :: memory_profiler

      memory_profiler => get_global_memory_profiler()
      call memory_profiler%finalize()

   end subroutine finalize


   subroutine start(name)
      character(*), intent(in) :: name
      
      type(MemoryProfiler), pointer :: memory_profiler

      memory_profiler => get_global_memory_profiler()
      call memory_profiler%start(name)

   end subroutine start

   
   subroutine stop(name)
      character(*), intent(in) :: name

      type(MemoryProfiler), pointer :: memory_profiler

      memory_profiler => get_global_memory_profiler()
      call memory_profiler%stop(name)

   end subroutine stop



end module MAPL_MemoryProfiler
