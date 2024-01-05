#include "Generic.h"

module mapl3g_GenericCoupler
   use mapl_ErrorHandlingMod
   implicit none
   private

   public :: setServices
   public :: make_coupler

contains

   function make_coupler(observed, rc) result(gridcomp)
      type(Observable) :: observed

      type(BidirectionalObserver), pointer :: observer

      gridcomp = ESMF_GridCompCreate(...)
      observer = BidirectionalObserver(observed)
      _SET_PRIVATE_STATE(gridcomp, observer, ...)

      _RETURN(_SUCCESS)
   end function make_coupler
   
   subroutine setServices(gridcomp, rc)
   end subroutine setServices

   subroutine update_self(gridcomp, clock, import, export, ...)

      observer => ...
      call observer%udpate_self(_RC)

      _RETURN(_SUCCESS)
   end subroutine update_self

   subroutine update_imports(this, rc)
      class(GenericCoupler), intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status
      integer :: i

      observer => ...
      call observer%update_imports(_RC)

      _RETURN(_SUCCESS)
   end subroutine notify_dependencies

   subroutine invalidate_exports(this, rc)
      class(GenericCoupler), intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status

      observer => ...
      call observer%invalidate_exports(_RC)

      _RETURN(_SUCCESS)
   end subroutine notify_subscribers


   subroutine add_dependency(this, dependency)
      class(GenericCoupler), intent(inout) :: this
      class(BidirectionalObserver), pointer, intent(in) :: dependency
      call this%dependencies%push_back(BidirectionObserverPtr(dependency))
   end subroutine add_dependency


   subroutine add_subscriber(this, subscriber)
      class(GenericCoupler), intent(inout) :: this
      class(BidirectionalObserver), pointer, intent(in) :: subscriber
      call this%subscribers%push_back(BidirectionObserverPtr(subscriber))
   end subroutine add_subscriber

end module mapl3g_GenericCoupler
