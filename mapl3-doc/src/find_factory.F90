#include "MAPL_Generic.h"

submodule (mapl3g_GeomManager) find_factory_smod

   implicit none

!   abstract interface
!      logical function I_FactoryPredicate(factory)
!         import GeomFactory
!         class(GeomFactory), intent(in) :: factory
!      end function I_FactoryPredicate
!   end interface
      
contains
   
   ! If factory not found, return a null pointer _and_ a nonzero rc.
   module function find_factory(factories, predicate, rc) result(factory)
      class(GeomFactory), pointer :: factory
      type(GeomFactoryVector), pointer, intent(in) :: factories ! Force TARGET attr on actual
      procedure(I_FactoryPredicate) :: predicate
      integer, optional, intent(out) :: rc

      integer :: status
      type(GeomFactoryVectorIterator) :: iter

      factory => null()
      iter = find_if(factories%begin(), factories%end(), predicate)
      _ASSERT(iter /= factories%end(), "No factory found satisfying given predicate.")
      factory => iter%of()

      _RETURN(_SUCCESS)
   end function find_factory

end submodule find_factory_smod
