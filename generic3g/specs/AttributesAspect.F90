#include "MAPL_Generic.h"

! We require that an export provides all attributes that an import
! specifies as a shared attribute.  Some attributes of the export may
! be unused and/or correspond to attributes needed by other imports.

module mapl3g_AttributesAspect
   use mapl3g_ActualConnectionPt
   use mapl3g_AspectId
   use mapl3g_StateItemAspect
   use mapl3g_ExtensionTransform
   use mapl3g_NullTransform
   use mapl_ErrorHandling
   use gftl2_StringVector
   implicit none
   private

   public :: AttributesAspect


   type, extends(StateItemAspect) :: AttributesAspect
!#      private
      type(StringVector) :: attribute_names
   contains
      procedure :: matches
      procedure :: supports_conversion_general
      procedure :: supports_conversion_specific
      procedure :: make_transform
      procedure :: connect_to_export
      procedure, nopass :: get_aspect_id
   end type AttributesAspect

   interface AttributesAspect
      procedure new_AttributesAspect
   end interface

contains

   ! Time dependent ungridded_dims is not supported.
   function new_AttributesAspect(attribute_names) result(aspect)
      type(AttributesAspect) :: aspect
      type(StringVector), optional, intent(in) :: attribute_names

      call aspect%set_mirror(.false.)
      if (present(attribute_names)) then
         aspect%attribute_names = attribute_names
      end if

   end function new_AttributesAspect

   logical function supports_conversion_general(src)
      class(AttributesAspect), intent(in) :: src
      supports_conversion_general = .false.
   end function supports_conversion_general

   logical function supports_conversion_specific(src, dst)
      class(AttributesAspect), intent(in) :: src
      class(StateItemAspect), intent(in) :: dst
      supports_conversion_specific = .false.
   end function supports_conversion_specific

   logical function matches(src, dst)
      class(AttributesAspect), intent(in) :: src
      class(StateItemAspect), intent(in) :: dst

      select type(dst)
      class is (AttributesAspect)
         matches = includes(src%attribute_names, dst%attribute_names)
      class default
         matches = .false.
      end select

   contains

      logical function includes(provided_names, mandatory_names)
         type(StringVector), target, intent(in) :: provided_names
         type(StringVector), target, intent(in) :: mandatory_names

         integer :: i, j
         character(:), pointer :: attr_name

         m: do i = 1, mandatory_names%size()
            attr_name => mandatory_names%of(i)
            p: do j = 1, provided_names%size()
               if (attr_name == provided_names%of(j)) cycle m ! good
            end do p
            ! ith not found
            includes = .false.
            return
         end do m
         
         includes = .true.

      end function includes

   end function matches

   function make_transform(src, dst, other_aspects, rc) result(transform)
      class(ExtensionTransform), allocatable :: transform
      class(AttributesAspect), intent(in) :: src
      class(StateItemAspect), intent(in)  :: dst
      type(AspectMap), target, intent(in)  :: other_aspects
      integer, optional, intent(out) :: rc

      transform = NullTransform()

      _RETURN(_SUCCESS)
   end function make_transform

   function get_aspect_id() result(aspect_id)
      type(AspectId) :: aspect_id
      aspect_id = ATTRIBUTES_ASPECT_ID
   end function get_aspect_id

   ! No-op (cannot mirror)
   subroutine connect_to_export(this, export, actual_pt, rc)
      class(AttributesAspect), intent(inout) :: this
      class(StateItemAspect), intent(in) :: export
      type(ActualConnectionPt), intent(in) :: actual_pt
      integer, optional, intent(out) :: rc

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(export)
      _UNUSED_DUMMY(actual_pt)
   end subroutine connect_to_export

end module mapl3g_AttributesAspect
