#include "MAPL_Generic.h"

module mapl3g_Observable
   use mapl_ErrorHandlingMod
   implicit none (type, external)
   private

   ! Class
   public :: Observable
   ! procedures
   public :: update_observable
   public :: invalidate_observable

   
   type, abstract :: Observable
      private
      logical :: stale = .true.
   contains
      procedure(I_Notify), deferred :: should_update ! ??? needed?
      procedure(I_Notify), deferred :: update_self
      procedure(I_Notify), deferred :: invalidate_self

      ! Accessors
      procedure, non_overridable :: is_up_to_date
      procedure, non_overridable :: is_stale
      procedure, non_overridable :: set_up_to_date
      procedure, non_overridable :: set_stale
   end type Observable

   abstract interface
      subroutine I_Notify(this, rc)
         import :: Observable
         class(Obserer), intent(inout) :: this
         integer, optional, intent(out) :: rc
      end subroutine I_Notify
   end interface

contains

   subroutine update_observable(this, rc)
      class(Observable), intent(inout) :: this
      integer, optional, intent(in) :: rc

      _RETURN_IF(this%is_up_to_date())

      call this%update_self(_RC)
      call this%set_up_to_date()

      _RETURN(_SUCCESS)
   end subroutine update

   subroutine invalidate(this, rc)
      class(Observable), intent(inout) :: this
      integer, optional, intent(in) :: rc

      _RETURN_IF(this%is_stale())
      
      call this%invalidate_self(_RC)
      call this%set_stale()

      _RETURN(_SUCCESS)
   end subroutine invalidate
   
   pure subroutine set_up_to_date(this)
      class(Observable), intent(inout) :: this
      this%up_to_date = .true
   end subroutine set_up_to_date

   pure subroutine set_stale(this)
      class(Observable), intent(inout) :: this
      this%up_to_date = .false
    end subroutine set_stale

    pure logical function is_up_to_date(this)
      class(Observable), intent(in) :: this
      is_up_to_date = this%up_to_date
   end function is_up_to_date

   pure logical function is_stale(this)
      class(Observable), intent(in) :: this
      is_stale = .not. this%up_to_date
   end function is_up_to_date

end module mapl3g_Observable
