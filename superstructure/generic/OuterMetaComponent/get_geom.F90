#include "MAPL.h"

submodule (mapl_OuterMetaComponent_mod) get_geom_smod

   use mapl_OpenMP_Support_mod, only: get_current_thread
   use mapl_ErrorHandling_mod
   implicit none(type,external)

contains

   module function get_geom(this, rc) result(geom)
      type(ESMF_Geom) :: geom
      class(OuterMetaComponent), intent(inout) :: this
      integer, intent(out), optional :: rc

      integer :: thread

      ! While threading is active, each thread sees the geometry of its
      ! own "mini" component.
      if (this%threading_active) then
         thread = get_current_thread()
         _ASSERT(thread < size(this%subcomponents), 'thread id exceeds number of sub components')
         geom = this%subcomponents(thread+1)%geom
         _RETURN(_SUCCESS)
      end if

      geom = this%geom

      _RETURN(_SUCCESS)
   end function get_geom

end submodule get_geom_smod
