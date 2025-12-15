#include "MAPL.h"

submodule (mapl3g_OuterMetaComponent) stop_time_profiler_smod

   use mapl_ErrorHandling
   use MAPL_Profiler, only: DistributedProfiler, get_global_time_profiler

   implicit none

contains

   module subroutine stop_time_profiler(this, name, rc)
      class(OuterMetaComponent), intent(inout) :: this
      character(len=*), intent(in) :: name
      integer, optional, intent(out) :: rc

      class(DistributedProfiler), pointer :: t_profiler
      integer :: status

      t_profiler => get_global_time_profiler()
      call t_profiler%stop(name, _RC)

      _RETURN(ESMF_SUCCESS)
   end subroutine stop_time_profiler

end submodule stop_time_profiler_smod
