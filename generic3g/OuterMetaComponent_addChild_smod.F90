#include "MAPL_ErrLog.h"

submodule (mapl3g_OuterMetaComponent) OuterMetaComponent_addChild_smod
   use mapl_keywordenforcer, only: KE => KeywordEnforcer
   use mapl3g_GenericGridComp
   use mapl3g_ChildComponent
   implicit none
   
contains

   module subroutine add_child_by_name(this, child_name, setservices, config, rc)
      class(OuterMetaComponent), intent(inout) :: this
      character(len=*), intent(in) :: child_name
      class(AbstractUserSetServices), intent(in) :: setservices
      type(GenericConfig), intent(in) :: config
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_GridComp) :: child_gc
      type(ChildComponent) :: child_comp

!!$      call validate_component_name(child_name, _RC)
      child_gc = create_grid_comp(child_name, setservices, config, _RC)
      child_comp = ChildComponent(child_gc)
      call this%children%insert(child_name, child_comp)

      _RETURN(ESMF_SUCCESS)
   end subroutine add_child_by_name


   
end submodule OuterMetaComponent_addChild_smod
