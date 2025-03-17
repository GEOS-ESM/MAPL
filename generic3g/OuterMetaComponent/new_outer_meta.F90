#include "MAPL_Generic.h"

submodule (mapl3g_OuterMetaComponent) new_outer_meta_smod
   implicit none

contains

   ! Keep the constructor simple
   module function new_outer_meta(gridcomp, user_gc_driver, user_setServices, hconfig) result(outer_meta)
      type(OuterMetaComponent) :: outer_meta
      type(ESMF_GridComp), intent(in) :: gridcomp
      type(GriddedComponentDriver), intent(in) :: user_gc_driver
      class(AbstractUserSetServices), intent(in) :: user_setservices
      type(ESMF_HConfig), intent(in) :: hconfig
      type(ESMF_TimeInterval) :: offset
         
      outer_meta%self_gridcomp = gridcomp
      outer_meta%user_gc_driver = user_gc_driver
      allocate(outer_meta%user_setServices, source=user_setServices)
      outer_meta%hconfig = hconfig

      call ESMF_TimeIntervalSet(offset, s=0)
      outer_meta%user_offset = offset

      counter = counter + 1
      outer_meta%counter = counter
      call initialize_phases_map(outer_meta%user_phases_map)

   end function new_outer_meta


end submodule new_outer_meta_smod
