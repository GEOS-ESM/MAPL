#include "MAPL_Generic.h"

module mapl3g_UngriddedDimsAspect
   use mapl3g_StateItemAspect
   use mapl3g_ExtensionAction
   use mapl3g_UngriddedDims
   use mapl3g_NullAction
   use mapl_ErrorHandling
   implicit none
   private

   public :: UngriddedDimsAspect


   type, extends(StateItemAspect) :: UngriddedDimsAspect
!#      private
      type(UngriddedDims), allocatable :: ungridded_dims
   contains
      procedure :: matches
      procedure :: supports_conversion_general
      procedure :: supports_conversion_specific
      procedure :: make_action

      procedure :: get_description
   end type UngriddedDimsAspect

   interface UngriddedDimsAspect
      procedure new_UngriddedDimsAspect
   end interface

contains

   ! Time dependent ungridded_dims is not supported.
   function new_UngriddedDimsAspect(ungridded_dims) result(aspect)
      type(UngriddedDimsAspect) :: aspect
      type(UngriddedDims), optional, intent(in) :: ungridded_dims

      call aspect%set_mirror(.true.)
      if (present(ungridded_dims)) then
         aspect%ungridded_dims = ungridded_dims
         call aspect%set_mirror(.false.)
      end if

   end function new_UngriddedDimsAspect

   logical function supports_conversion_general(src)
      class(UngriddedDimsAspect), intent(in) :: src
      supports_conversion_general = .false.
   end function supports_conversion_general

   logical function supports_conversion_specific(src, dst)
      class(UngriddedDimsAspect), intent(in) :: src
      class(StateItemAspect), intent(in) :: dst
      supports_conversion_specific = .false.
   end function supports_conversion_specific

   logical function matches(src, dst)
      class(UngriddedDimsAspect), intent(in) :: src
      class(StateItemAspect), intent(in) :: dst

      select type(dst)
      class is (UngriddedDimsAspect)
         matches = (src%ungridded_dims == dst%ungridded_dims)
      class default
         matches = .false.
      end select

   end function matches

   function make_action(src, dst, rc) result(action)
      class(ExtensionAction), allocatable :: action
      class(UngriddedDimsAspect), intent(in) :: src
      class(StateItemAspect), intent(in)  :: dst
      integer, optional, intent(out) :: rc

      action = NullAction()

      _RETURN(_SUCCESS)
   end function make_action

   function get_description(this) result(s)
      character(:), allocatable :: s
      class(UngriddedDimsAspect), intent(in) :: this

      ! Should not get here in mirror'd case, but ...
      s = 'description not implemented'

   end function get_description

end module mapl3g_UngriddedDimsAspect
