#include "MAPL_ErrLog.h"

submodule (mapl3g_ComponentSpecParser) parse_children_smod
   
contains

   module function parse_children(hconfig, rc) result(children)
      type(ChildSpecMap) :: children
      type(ESMF_HConfig), intent(in) :: hconfig
      integer, optional, intent(out) :: rc

      integer :: status
      logical :: has_children
      logical :: is_map
      type(ESMF_HConfig) :: children_cfg, child_cfg
      type(ESMF_HConfigIter) :: iter, iter_begin, iter_end
      type(ChildSpec) :: child_spec
      character(:), allocatable :: child_name


      has_children = ESMF_HConfigIsDefined(hconfig, keyString=COMPONENT_CHILDREN_SECTION, _RC)
      _RETURN_UNLESS(has_children)

      children_cfg = ESMF_HConfigCreateAt(hconfig, keyString=COMPONENT_CHILDREN_SECTION, _RC)
      is_map = ESMF_HConfigIsMap(children_cfg, _RC)

      _ASSERT(is_map, 'children spec must be mapping')

      iter_begin = ESMF_HCOnfigIterBegin(children_cfg, _RC)
      iter_end = ESMF_HConfigIterEnd(children_cfg, _RC)
      iter = iter_begin
      do while (ESMF_HConfigIterLoop(iter, iter_begin, iter_end))
         child_name = ESMF_HConfigAsStringMapKey(iter, _RC)
         child_cfg = ESMF_HConfigCreateAtMapVal(iter, _RC)
         child_spec = parse_child(child_cfg, _RC)
         call children%insert(child_name, child_spec)
         call ESMF_HConfigDestroy(child_cfg, _RC)
      end do

      call ESMF_HConfigDestroy(children_cfg, _RC)

      _RETURN(_SUCCESS)
   end function parse_children

end submodule parse_children_smod

