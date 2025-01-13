#include "MAPL_Generic.h"

! We require that an export provides all attributes that an import
! specifies as a shared attribute.  Some attributes of the export may
! be unused and/or correspond to attributes needed by other imports.

module mapl3g_AttributesAspect
   use mapl3g_StateItemAspect
   use mapl3g_ExtensionAction
   use mapl3g_NullAction
   use mapl_ErrorHandling
   use gftl2_StringVector
   implicit none
   private

   public :: AttributesAspect


   type, extends(StateItemAspect) :: AttributesAspect
!#      private
      type(StringVector), allocatable :: attribute_names
   contains
      procedure :: matches
      procedure :: supports_conversion_general
      procedure :: supports_conversion_specific
      procedure :: make_action
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
         type(StringVector), intent(in) :: provided_names
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

   function make_action(src, dst, rc) result(action)
      class(ExtensionAction), allocatable :: action
      class(AttributesAspect), intent(in) :: src
      class(StateItemAspect), intent(in)  :: dst
      integer, optional, intent(out) :: rc

      action = NullAction()

      _RETURN(_SUCCESS)
   end function make_action

end module mapl3g_AttributesAspect
