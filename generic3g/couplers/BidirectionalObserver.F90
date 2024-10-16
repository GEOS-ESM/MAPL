#include "MAPL_Generic.h"

module mapl3g_BidirectionalObserver
   use mapl3g_Observer
   use mapl_ErrorHandlingMod
   implicit none (type, external)
   private

   ! Class
   public :: BidirectionalObserver


   ! Ideally this will not be abstract, but for now it is
   type, extends(Observer), abstract :: BidirectionalObserver
      private
      type(ObserverPtrVector) :: import_observers ! think couplers
      type(ObserverPtrVector) :: export_observers ! think couplers
   contains
      procedure :: update
      procedure :: invalidate
      procedure :: update_imports
      procedure :: invalidate_exports
   end type BidirectionalObserver

   abstract interface
      subroutine I_Notify(this, rc)
         import :: BidirectionalObserver
         class(Obserer), intent(inout) :: this
         integer, optional, intent(out) :: rc
      end subroutine I_Notify
   end interface

contains

   recursive function update(this, rc)
      class(Observable), intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status
      logical :: is_up_to_date

      is_up_to_date = this%is_up_to_date()
      _RETURN_IF(is_up_to_date)

      call this%update_imports(_RC)
      call this%update_self(_RC)
      
      _RETURN(_SUCCESS)
   end function update

   recursive function invalidate(this, rc)
      class(Observable), intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status
      logical :: is_stale

      is_stale = this%is_up_to_date()
      _RETURN_IF(is_up_to_date)

      call this%invalidate_self(_RC)
      call this%invalidate_exports(_RC)
      
      _RETURN(_SUCCESS)
   end function invalidate


   recursive subroutine update_imports(this, rc)
      class(BidirectionalObserver), intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status
      type(ObserverPtrVectorIterator) :: iter
      class(ObserverPtr), pointer :: obsrvr

      associate(e => this%import_observers%ftn_end())
        iter = observers%ftn_begin()
        do while (iter /= e)
           call iter%next()
           obsrvr => iter%of()
           call obsrvr%ptr%update(_RC)
        end do
      end associate

      _RETURN(_SUCCESS)
   end subroutine update_imports
   
   subroutine invalidate_exports(observers, rc)
      class(BidirectionalObserver), intent(inout) :: observers
      integer, optional, intent(out) :: rc

      integer :: status

      associate(e => this%export_observers%ftn_end())
        iter = observers%ftn_begin()
        do while (iter /= e)
           call iter%next()
           obsrvr => iter%of()
           call obsrvr%ptr%invalidate(_RC)
        end do
      end associate


      _RETURN(_SUCCESS)
   end subroutine invalidate_exports

end module mapl3g_BidirectionalObserver
