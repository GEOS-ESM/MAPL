#include "MAPL.h"

submodule (mapl3g_OuterMetaComponent) init_meta_smod
   use mapl_ErrorHandling
   use pFlogger, only: logging
   implicit none

contains

   ! NOTE: _Not_ an ESMF phase - this is initializing the object itself.
   ! Constructor (new_outer_meta) only copies basic parameters.  All
   ! other initialization is in this procedure.

   module subroutine init_meta(this, rc)
      class(OuterMetaComponent), intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status
      character(:), allocatable :: user_gc_name

      user_gc_name = this%user_gc_driver%get_name(_RC)
      this%registry = StateRegistry(user_gc_name)

      this%lgr => logging%get_logger(user_gc_name)

      _RETURN(_SUCCESS)

   end subroutine init_meta

end submodule init_meta_smod
