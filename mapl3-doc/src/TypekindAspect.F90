#include "MAPL_Generic.h"

module mapl3g_TypekindAspect
   use mapl3g_StateItemAspect
   use mapl3g_ExtensionAction
   use mapl3g_Copyaction
   use mapl3g_NullAction
   use mapl_ErrorHandling
   use mapl3g_ESMF_Utilities, only: MAPL_TYPEKIND_MIRROR
   use esmf
   implicit none
   private

   public :: TypekindAspect


   type, extends(StateItemAspect) :: TypekindAspect
!#      private
      type(ESMF_Typekind_Flag) :: typekind = ESMF_TYPEKIND_R4 ! default
   contains
      procedure :: matches
      procedure :: supports_conversion_general
      procedure :: supports_conversion_specific
      procedure :: make_action

      procedure :: set_typekind
      procedure :: get_typekind
   end type TypekindAspect

   interface TypekindAspect
      procedure new_TypekindAspect
   end interface

contains

   ! Time dependent ungridded_dims is not supported.
   function new_TypekindAspect(typekind) result(aspect)
      type(TypekindAspect) :: aspect
      type(ESMF_Typekind_Flag), optional, intent(in) :: typekind

      call aspect%set_mirror(.true.)
      if (present(typekind)) then
         aspect%typekind = typekind
         call aspect%set_mirror(typekind == MAPL_TYPEKIND_MIRROR)
      end if

   end function new_TypekindAspect

   logical function supports_conversion_general(src)
      class(TypekindAspect), intent(in) :: src
      supports_conversion_general = .true.
   end function supports_conversion_general

   logical function supports_conversion_specific(src, dst)
      class(TypekindAspect), intent(in) :: src
      class(StateItemAspect), intent(in) :: dst
      supports_conversion_specific = .true.
   end function supports_conversion_specific

   logical function matches(src, dst)
      class(TypekindAspect), intent(in) :: src
      class(StateItemAspect), intent(in) :: dst

      select type(dst)
      class is (TypekindAspect)
         matches = (src%typekind == dst%typekind) .or. count([src%typekind,dst%typekind]==MAPL_TYPEKIND_MIRROR) == 1
      class default
         matches = .false.
      end select

   end function matches

   function make_action(src, dst, rc) result(action)
      class(ExtensionAction), allocatable :: action
      class(TypekindAspect), intent(in) :: src
      class(StateItemAspect), intent(in)  :: dst
      integer, optional, intent(out) :: rc

      select type(dst)
      class is (TypekindAspect)
         action = CopyAction(src%typekind, dst%typekind)
      class default
         _FAIL('src is TypekindAspect, but dst is not.')
      end select

      _RETURN(_SUCCESS)
   end function make_action

   subroutine set_typekind(this, typekind)
      class(TypekindAspect), intent(inout) :: this
      type(ESMF_Typekind_Flag), intent(in) :: typekind

      this%typekind = typekind
   end subroutine set_typekind

   function get_typekind(this) result(typekind)
      type(ESMF_Typekind_Flag) :: typekind
      class(TypekindAspect), intent(in) :: this

      typekind = this%typekind
   end function get_typekind

end module mapl3g_TypekindAspect
