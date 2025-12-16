#include "MAPL.h"

submodule (mapl3g_OuterMetaComponent) start_timer_smod

   use mapl_ErrorHandling
   use MAPL_Profiler, only: DistributedProfiler, get_global_time_profiler

   implicit none

contains

   module subroutine start_timer(this, name, rc)
      class(OuterMetaComponent), intent(inout) :: this
      character(len=*), intent(in) :: name
      integer, optional, intent(out) :: rc

      class(DistributedProfiler), pointer :: t_profiler
      integer :: status

      t_profiler => get_global_time_profiler()
      call t_profiler%start(name, _RC)

      _RETURN(ESMF_SUCCESS)
   end subroutine start_timer

end submodule start_timer_smod
