#include "MAPL_Generic.h"

module mapl3g_Observer
   use mapl_ErrorHandlingMod
   implicit none (type, external)
   private

   ! Class
   public :: Observer
   public :: ObserverPtr

   ! procedures
   public :: update
   public :: invalidate

   
   type, abstract :: Observer
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
   end type Observer

   type :: ObserverPtr
      class(Observer), pointer :: ptr => null()
   end type ObserverPtr

   abstract interface
      subroutine I_Notify(this, rc)
         import :: Observer
         class(Observer), intent(inout) :: this
         integer, optional, intent(out) :: rc
      end subroutine I_Notify
   end interface

contains

   subroutine update(this, rc)
      class(Observer), intent(inout) :: this
      integer, optional, intent(out) :: rc

       integer :: status

     _RETURN_IF(this%is_up_to_date())

      call this%update_self(_RC)
      call this%set_up_to_date()

      _RETURN(_SUCCESS)
   end subroutine update

   subroutine invalidate(this, rc)
      class(Observer), intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status

      _RETURN_IF(this%is_stale())
      
      call this%invalidate_self(_RC)
      call this%set_stale()

      _RETURN(_SUCCESS)
   end subroutine invalidate
   
   pure subroutine set_up_to_date(this)
      class(Observer), intent(inout) :: this
      this%stale = .false.
   end subroutine set_up_to_date

   pure subroutine set_stale(this)
      class(Observer), intent(inout) :: this
      this%stale = .true.
    end subroutine set_stale

    pure logical function is_up_to_date(this)
      class(Observer), intent(in) :: this
      is_up_to_date = .not. this%stale
   end function is_up_to_date

   pure logical function is_stale(this)
      class(Observer), intent(in) :: this
      is_stale = this%stale
   end function is_stale

end module mapl3g_Observer
