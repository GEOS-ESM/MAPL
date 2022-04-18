#include "MAPL_ErrLog.h"

submodule (mapl3g_OuterMetaComponent) OuterMetaComponent_setservices_smod
   use mapl_keywordenforcer, only: KE => KeywordEnforcer
   use mapl3g_GenericGridComp
   use mapl3g_ChildComponent
   implicit none
   
contains

   module subroutine add_child_by_name(this, child_name, config, rc)
      class(OuterMetaComponent), intent(inout) :: this
      character(len=*), intent(in) :: child_name
      class(YAML_Node), intent(inout) :: config
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_GridComp) :: child_gc
      type(ChildComponent) :: child_comp

      print*,__FILE__,__LINE__, child_name, config

      child_gc = create_grid_comp(child_name, config, _RC)
      child_comp%gridcomp = child_gc 
      call this%children%insert(child_name, child_comp)

      _RETURN(ESMF_SUCCESS)
   end subroutine add_child_by_name


   
end submodule OuterMetaComponent_setservices_smod
