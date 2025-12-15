#include "MAPL.h"

submodule (mapl3g_OuterMetaComponent) start_time_profiler_smod

   use mapl_ErrorHandling
   use MAPL_Profiler, only: DistributedProfiler, get_global_time_profiler

   implicit none

contains

   module subroutine start_time_profiler(this, name, rc)
      class(OuterMetaComponent), intent(inout) :: this
      character(len=*), intent(in) :: name
      integer, optional, intent(out) :: rc

      class(DistributedProfiler), pointer :: t_profiler
      integer :: status

      t_profiler => get_global_time_profiler()
      call t_profiler%start(name, _RC)

      _RETURN(ESMF_SUCCESS)
   end subroutine start_time_profiler

end submodule start_time_profiler_smod
