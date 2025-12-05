#include "MAPL.h"

module mapl3g_UngriddedDimsAspect
   use mapl3g_ActualConnectionPt
   use mapl3g_AspectId
   use mapl3g_StateItemAspect
   use mapl3g_ExtensionTransform
   use mapl3g_UngriddedDims
   use mapl3g_NullTransform
   use mapl3g_Field_Api
   use mapl3g_FieldBundle_Api
   use mapl_KeywordEnforcer
   use mapl_ErrorHandling
   use esmf
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
      procedure :: make_transform
      procedure, nopass :: get_aspect_id

      procedure :: get_ungridded_dims
      procedure :: update_from_payload
      procedure :: update_payload
      
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


   function make_transform(src, dst, other_aspects, rc) result(transform)
      class(ExtensionTransform), allocatable :: transform
      class(UngriddedDimsAspect), intent(in) :: src
      class(StateItemAspect), intent(in)  :: dst
      type(AspectMap), target, intent(in)  :: other_aspects
      integer, optional, intent(out) :: rc

      allocate(transform,source=NullTransform()) ! just in case

      _RETURN(_SUCCESS)
   end function make_transform

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

   subroutine update_from_payload(this, field, bundle, state, rc)
      class(UngriddedDimsAspect), intent(inout) :: this
      type(esmf_Field), optional, intent(in) :: field
      type(esmf_FieldBundle), optional, intent(in) :: bundle
      type(esmf_State), optional, intent(in) :: state
      integer, optional, intent(out) :: rc

      integer :: status

      _RETURN_UNLESS(present(field) .or. present(bundle))

      if (present(field)) then
         call mapl_FieldGet(field, ungridded_dims=this%ungridded_dims, _RC)
      else if (present(bundle)) then
         call mapl_FieldBundleGet(bundle, ungridded_dims=this%ungridded_dims, _RC)
      end if

      ! In practice there is no way that this can happen unless it was already mirror ...
      call this%set_mirror(.not. allocated(this%ungridded_dims))

      _RETURN(_SUCCESS)
   end subroutine update_from_payload

   subroutine update_payload(this, field, bundle, state, rc)
      class(UngriddeddimsAspect), intent(in) :: this
      type(esmf_Field), optional, intent(inout) :: field
      type(esmf_FieldBundle), optional, intent(inout) :: bundle
      type(esmf_State), optional, intent(inout) :: state
      integer, optional, intent(out) :: rc

      integer :: status

      _RETURN_UNLESS(present(field) .or. present(bundle))

      if (present(field)) then
         call mapl_FieldSet(field, ungridded_dims=this%ungridded_dims, _RC)
      else if (present(bundle)) then
         call mapl_FieldBundleSet(bundle, ungridded_dims=this%ungridded_dims, _RC)
      end if

      _RETURN(_SUCCESS)
   end subroutine update_payload
 
end module mapl3g_UngriddedDimsAspect
