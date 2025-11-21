#include "MAPL_ErrLog.h"

!#include "unused_dummy.H"
module mapl_GlobalProfilers
   use mapl_AbstractGauge
   use mapl_DistributedProfiler
   use mapl_StubProfiler
   use mapl_KeywordEnforcerMod
   use mapl_ErrorHandlingMod
   use mapl_MpiTimerGauge
   use mapl_MallocGauge
   implicit none
   private
   

   public :: get_global_time_profiler
   public :: get_global_memory_profiler

   public :: initialize_global_time_profiler
   public :: initialize_global_memory_profiler

   class(DistributedProfiler), allocatable, target, save :: global_time_profiler
   class(DistributedProfiler), allocatable, target, save :: global_memory_profiler

contains

   function get_global_time_profiler() result(time_profiler)
      class(DistributedProfiler), pointer :: time_profiler
      time_profiler => global_time_profiler
   end function get_global_time_profiler

   function get_global_memory_profiler() result(memory_profiler)
      class(DistributedProfiler), pointer :: memory_profiler
      memory_profiler => global_memory_profiler
   end function get_global_memory_profiler

   subroutine initialize_global_time_profiler(name, unusable, comm, enabled, rc)
      character(*), intent(in) :: name
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(in) :: comm
      logical, optional, intent(in) :: enabled
      integer, optional, intent(out) :: rc

      integer :: status

      call initialize_global_profiler(global_time_profiler, MpiTimerGauge(), name, comm=comm, enabled=enabled, _RC)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine initialize_global_time_profiler

   subroutine initialize_global_memory_profiler(name, unusable, comm, enabled, rc)
      character(*), intent(in) :: name
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(in) :: comm
      logical, optional, intent(in) :: enabled
      integer, optional, intent(out) :: rc

      integer :: status

      call initialize_global_profiler(global_memory_profiler, MallocGauge(), name, comm=comm, enabled=enabled, _RC)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine initialize_global_memory_profiler


   subroutine initialize_global_profiler(profiler, gauge, name, unusable, comm, enabled, rc)
      class(DistributedProfiler), allocatable, intent(inout) :: profiler
      class(AbstractGauge), intent(in) :: gauge
      character(*), intent(in) :: name
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(in) :: comm
      logical, optional, intent(in) :: enabled
      integer, optional, intent(out) :: rc

      logical :: enabled_
      integer :: status

      enabled_ = .false.
      if (present(enabled)) enabled_ = enabled

      if (enabled_) then
!!$         profiler = DistributedProfiler(name, gauge, comm=comm)
         ! Compiler workaround for ifort 2021.3
         allocate(profiler, source=DistributedProfiler(name, gauge, comm=comm))
      else
!!$         profiler = StubProfiler(name)
         ! Compiler workaround for ifort 2021.3
         allocate(profiler, source=StubProfiler(name))
      end if
      call profiler%start(_RC)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine initialize_global_profiler

end module MAPL_GlobalProfilers
