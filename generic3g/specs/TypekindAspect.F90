#include "MAPL_Generic.h"

module mapl3g_TypekindAspect
   use mapl3g_ActualConnectionPt
   use mapl3g_AspectId
   use mapl3g_StateItemAspect
   use mapl3g_ExtensionAction
   use mapl3g_Copyaction
   use mapl3g_NullAction
   use mapl_ErrorHandling
   use mapl3g_ESMF_Utilities, only: MAPL_TYPEKIND_MIRROR
   use esmf
   implicit none(type,external)
   private

   public :: TypekindAspect
   public :: to_TypekindAspect
   
   interface to_TypekindAspect
      procedure :: to_typekind_from_poly
      procedure :: to_typekind_from_map
   end interface to_TypekindAspect

   type, extends(StateItemAspect) :: TypekindAspect
!#      private
      type(ESMF_Typekind_Flag) :: typekind = ESMF_TYPEKIND_R4 ! default
   contains
      procedure :: matches
      procedure :: supports_conversion_general
      procedure :: supports_conversion_specific
      procedure :: make_action
      procedure :: connect_to_export
      procedure, nopass :: get_aspect_id

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

  function make_action(src, dst, other_aspects, rc) result(action)
      class(ExtensionAction), allocatable :: action
      class(TypekindAspect), intent(in) :: src
      class(StateItemAspect), intent(in)  :: dst
      type(AspectMap), target, intent(in)  :: other_aspects
      integer, optional, intent(out) :: rc

      integer :: status
      type(TypekindAspect) :: dst_

      allocate(action,source=NullAction()) ! just in case
      dst_ = to_TypekindAspect(dst, _RC)

      deallocate(action)
      allocate(action, source=CopyAction(src%typekind, dst_%typekind))

      _RETURN(_SUCCESS)
   end function make_action

   ! Copy from src - might have been mirror.

   subroutine connect_to_export(this, export, actual_pt, rc)
      class(TypekindAspect), intent(inout) :: this
      class(StateItemAspect), intent(in) :: export
      type(ActualConnectionPt), intent(in) :: actual_pt
      integer, optional, intent(out) :: rc

      type(TypekindAspect) :: export_
      integer :: status

      export_ = to_TypekindAspect(export, _RC)
      this%typekind = export_%typekind
      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(actual_pt)
   end subroutine connect_to_export

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

   function get_aspect_id() result(aspect_id)
      type(AspectId) :: aspect_id
      aspect_id = TYPEKIND_ASPECT_ID
   end function get_aspect_id

   function to_typekind_from_poly(aspect, rc) result(typekind_aspect)
      type(TypekindAspect) :: typekind_aspect
      class(StateItemAspect), intent(in) :: aspect
      integer, optional, intent(out) :: rc

      integer :: status

      select type(aspect)
      class is (TypekindAspect)
         typekind_aspect = aspect
      class default
         _FAIL('aspect is not TypekindAspect')
      end select

      _RETURN(_SUCCESS)
   end function to_typekind_from_poly

   function to_typekind_from_map(map, rc) result(typekind_aspect)
      type(TypekindAspect) :: typekind_aspect
      type(AspectMap), target, intent(in) :: map
      integer, optional, intent(out) :: rc

      integer :: status
      class(StateItemAspect), pointer :: poly

      poly => map%at(TYPEKIND_ASPECT_ID, _RC)
      typekind_aspect = to_TypekindAspect(poly, _RC)

      _RETURN(_SUCCESS)
   end function to_typekind_from_map
   
 
end module mapl3g_TypekindAspect
