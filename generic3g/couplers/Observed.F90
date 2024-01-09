#include "MAPL_Generic.h"

module mapl3g_Observable
   use mapl3g_Observer
   implicit none
   private

   public :: Observable

   type :: Observable
      type(ObserverPtrVector) :: observers
   contains
      procedure :: update_observers
   end type Observable

contains

   subroutine update_observers(this, rc)
      class(Observable), intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status

      associate (e => this%observers%end())
        iter = this%observers%begin()
        do while (iter /= e)
           call iter%next()
           obsrvr => iter%of()
           call obsrvr%update(_RC)
         end do
       end associate
       _RETURN(_SUCCESS)
   end subroutine update_observers

end module mapl3g_Observable
