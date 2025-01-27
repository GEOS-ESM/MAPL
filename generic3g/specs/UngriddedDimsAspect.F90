#include "MAPL_Generic.h"

module mapl3g_UngriddedDimsAspect
   use mapl3g_ActualConnectionPt
   use mapl3g_AspectId
   use mapl3g_StateItemAspect
   use mapl3g_ExtensionAction
   use mapl3g_UngriddedDims
   use mapl3g_NullAction
   use mapl_ErrorHandling
   implicit none
   private

   public :: UngriddedDimsAspect
   public :: to_UngriddedDimsAspect
   
   interface to_UngriddedDimsAspect
      procedure :: to_ungridded_dims_from_poly
      procedure :: to_ungridded_dims_from_map
   end interface to_UngriddedDimsAspect

   type, extends(StateItemAspect) :: UngriddedDimsAspect
      private
      type(UngriddedDims), allocatable :: ungridded_dims
   contains
      procedure :: matches
      procedure :: connect_to_export
      procedure :: supports_conversion_general
      procedure :: supports_conversion_specific
      procedure :: make_action
      procedure, nopass :: get_aspect_id

      procedure :: get_ungridded_dims
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
      aspect%ungridded_dims = UngriddedDims()

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


   function to_ungridded_dims_from_poly(aspect, rc) result(ungridded_dims_aspect)
      type(UngriddedDimsAspect) :: ungridded_dims_aspect
      class(StateItemAspect), intent(in) :: aspect
      integer, optional, intent(out) :: rc

      integer :: status

      select type(aspect)
      class is (UngriddedDimsAspect)
         ungridded_dims_aspect = aspect
      class default
         _FAIL('aspect is not UngriddedDimsAspect')
      end select

      _RETURN(_SUCCESS)
   end function to_ungridded_dims_from_poly

   function to_ungridded_dims_from_map(map, rc) result(ungridded_dims_aspect)
      type(UngriddedDimsAspect) :: ungridded_dims_aspect
      type(AspectMap), target, intent(in) :: map
      integer, optional, intent(out) :: rc

      integer :: status
      class(StateItemAspect), pointer :: poly

      poly => map%at(UNGRIDDED_DIMS_ASPECT_ID, _RC)
      ungridded_dims_aspect = to_UngriddedDimsAspect(poly, _RC)

      _RETURN(_SUCCESS)
   end function to_ungridded_dims_from_map


   function make_action(src, dst, other_aspects, rc) result(action)
      class(ExtensionAction), allocatable :: action
      class(UngriddedDimsAspect), intent(in) :: src
      class(StateItemAspect), intent(in)  :: dst
      type(AspectMap), target, intent(in)  :: other_aspects
      integer, optional, intent(out) :: rc

      allocate(action,source=NullAction()) ! just in case

      _RETURN(_SUCCESS)
   end function make_action

   subroutine connect_to_export(this, export, actual_pt, rc)
      class(UngriddedDimsAspect), intent(inout) :: this
      class(StateItemAspect), intent(in) :: export
      type(ActualConnectionPt), intent(in) :: actual_pt
      integer, optional, intent(out) :: rc

      type(UngriddedDimsAspect) :: export_
      integer :: status
      
      export_ = to_UngriddedDimsAspect(export, _RC)
      this%ungridded_dims = export_%ungridded_dims

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(actual_pt)
   end subroutine connect_to_export
   
   function get_aspect_id() result(aspect_id)
      type(AspectId) :: aspect_id
      aspect_id = UNGRIDDED_DIMS_ASPECT_ID
   end function get_aspect_id

   function get_ungridded_dims(this, rc)  result(ungridded_dims)
      type(UngriddedDims) :: ungridded_dims
      class(UngriddedDimsAspect), intent(in) :: this
      integer, optional, intent(out) :: rc

      integer :: status

      _ASSERT(allocated(this%ungridded_dims), "ungridded_dims not allocated.")
      ungridded_dims = this%ungridded_dims

      _RETURN(_SUCCESS)
   end function get_ungridded_dims

end module mapl3g_UngriddedDimsAspect
