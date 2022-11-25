#include "MAPL_ErrLog.h"

submodule (mapl3g_HierarchicalRegistry) HierarchicalRegistry_smod
contains

   function new_HierarchicalRegistry_children(children, rc) result(registry)
      use mapl3g_OuterMetaComponent
      use mapl3g_ChildComponent
      use mapl3g_ChildComponentMap
      type(HierarchicalRegistry) :: registry
      type(ChildComponentMap), intent(in) :: children
      integer, optional, intent(out) :: rc

      type(ChildComponentMapIterator) :: iter
      character(:), pointer :: name
      type(ChildComponent), pointer :: child
      type(Outermetacomponent), pointer :: child_meta
      
      associate (e => children%end())
        iter = children%begin()

        do while (iter /= e)
           name => iter%first()
           child => iter%second()
           child_meta => get_outer_meta(child%gridcomp)
           call registry%add_subregistry(name, child_meta%get_registry())
           call iter%next()
        end do

      end associate

      _RETURN(_SUCCESS)
   end function new_HierarchicalRegistry_children

end submodule HierarchicalRegistry_smod
