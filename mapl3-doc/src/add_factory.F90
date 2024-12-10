#include "MAPL_Generic.h"

submodule (mapl3g_GeomManager) add_factory_smod

   implicit none

contains
   
   module subroutine add_factory(this, factory)
      class(GeomManager), intent(inout) :: this
      class(GeomFactory), intent(in) :: factory

      call this%factories%push_back(factory)
   end subroutine add_factory

end submodule add_factory_smod
