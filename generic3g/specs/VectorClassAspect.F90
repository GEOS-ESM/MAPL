#include "MAPL_Generic.h"

module mapl3g_VectorClassAspect
   use mapl3g_FieldBundleGet
   use mapl3g_ActualConnectionPt
   use mapl3g_AspectId
   use mapl3g_StateItemAspect
   use mapl3g_ClassAspect
   use mapl3g_FieldClassAspect
   use mapl3g_GeomAspect
   use mapl3g_VerticalGridAspect
   use mapl3g_UnitsAspect
   use mapl3g_TypekindAspect
   use mapl3g_UngriddedDimsAspect

   use mapl3g_VerticalGrid
   use mapl3g_VerticalStaggerLoc
   use mapl3g_VerticalStaggerLoc
   use mapl3g_UngriddedDims

   use mapl3g_NullTransform
   use mapl3g_ExtensionTransform
   use mapl3g_MultiState
   use mapl3g_ESMF_Utilities, only: get_substate

   use mapl3g_FieldCreate
   use mapl_FieldUtilities

   use mapl_ErrorHandling
   use esmf
   implicit none(type,external)
   private

   public :: VectorClassAspect
   public :: to_VectorClassAspect

   interface to_VectorClassAspect
      procedure :: to_vectorclassaspect_from_poly
      procedure :: to_vectorclassaspect_from_map
   end interface to_VectorClassAspect

   integer, parameter :: NUM_COMPONENTS = 2
   type, extends(ClassAspect) :: VectorClassAspect
      private
      type(ESMF_FieldBundle) :: payload
      type(FieldClassAspect) :: component_specs(2)
   contains
      procedure :: get_aspect_order
      procedure :: supports_conversion_general
      procedure :: supports_conversion_specific
      procedure :: make_transform
      procedure :: matches
      procedure :: connect_to_import
      procedure :: connect_to_export

      procedure :: create
      procedure :: allocate
      procedure :: destroy
      procedure :: add_to_state

      procedure :: get_payload
      procedure, nopass :: get_aspect_id
   end type VectorClassAspect

   interface VectorClassAspect
      procedure :: new_VectorClassAspect_basic
   end interface VectorClassAspect


contains

   function new_VectorClassAspect_basic(component_specs) result(aspect)
      type(VectorClassAspect) :: aspect
      type(FieldClassAspect), intent(in) :: component_specs(2)
      aspect%component_specs = component_specs
   end function new_VectorClassAspect_basic


   ! Should always be the same as for Field
   function get_aspect_order(this, goal_aspects, rc) result(aspect_ids)
      type(AspectId), allocatable :: aspect_ids(:)
      class(VectorClassAspect), intent(in) :: this
      type(AspectMap), intent(in) :: goal_aspects
      integer, optional, intent(out) :: rc

      integer :: status
      
      aspect_ids = this%component_specs(1)%get_aspect_order(goal_aspects, _RC)

      _RETURN(_SUCCESS)

      _UNUSED_DUMMY(goal_aspects)
   end function get_aspect_order

   function matches(src, dst)
      logical :: matches
      class(VectorClassAspect), intent(in) :: src
      class(StateItemAspect), intent(in) :: dst

      matches = .false.
      select type(dst)
      class is (VectorClassAspect)
         matches = .true.
      end select
   end function matches

   subroutine create(this, rc)
      class(VectorClassAspect), intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status

      this%payload = ESMF_FieldBundleCreate(_RC)

      _RETURN(ESMF_SUCCESS)
   end subroutine create

   ! Tile / Grid   X  or X, Y
   subroutine allocate(this, other_aspects, rc)
      class(VectorClassAspect), intent(inout) :: this
      type(AspectMap), intent(in) :: other_aspects
      integer, optional, intent(out) :: rc

      integer :: status
      integer :: i
      type(FieldClassAspect) :: tmp

      do i = 1, NUM_COMPONENTS
         call this%component_specs(i)%create(_RC)
         call this%component_specs(i)%allocate(other_aspects, _RC)
         call this%component_specs(i)%add_to_bundle(this%payload, _RC)
      end do

      _RETURN(ESMF_SUCCESS)
   end subroutine allocate


   subroutine destroy(this, rc)
      class(VectorClassAspect), intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status

      integer :: i
      type(ESMF_Field), allocatable :: fieldList(:)

      call MAPL_FieldBundleGet(this%payload, fieldList=fieldList, _RC)
      if (size(fieldList) > 0) then ! might be empty if import item
         do i = 1, NUM_COMPONENTS
            call ESMF_FieldDestroy(fieldList(i), noGarbage=.true., _RC)
         end do
      end if
      call ESMF_FieldBundleDestroy(this%payload, nogarbage=.true., _RC)

      _RETURN(ESMF_SUCCESS)
   end subroutine destroy


   ! No-op
   subroutine connect_to_import(this, import, rc)
      class(VectorClassAspect), intent(inout) :: this
      class(StateItemAspect), intent(in) :: import
      integer, optional, intent(out) :: rc

      _RETURN(_SUCCESS)
   end subroutine connect_to_import

   subroutine connect_to_export(this, export, actual_pt, rc)
      class(VectorClassAspect), intent(inout) :: this
      class(StateItemAspect), intent(in) :: export
      type(ActualConnectionPt), intent(in) :: actual_pt
      integer, optional, intent(out) :: rc

      type(VectorClassAspect) :: export_
      integer :: status

      export_ = to_VectorClassAspect(export, _RC)
      call this%destroy(_RC) ! import is replaced by export/extension
      this%payload = export_%payload

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(actual_pt)

   contains

     subroutine mirror(dst, src)
        real, allocatable, intent(inout) :: dst
        real, allocatable, intent(in) :: src

        if (.not. allocated(src)) return

        if (.not. allocated(dst)) then
           dst = src
           return
        end if

        ! TODO: Problematic case: both allocated with different values.
        if (dst /= src) then
           _HERE, 'WARNING: mismatched default values for ', actual_pt
           _HERE, '    src = ', src, '; dst = ',dst, ' (src value wins)'
        end if

      end subroutine mirror
      
   end subroutine connect_to_export

   function to_vectorclassaspect_from_poly(aspect, rc) result(vector_aspect)
      type(VectorClassAspect) :: vector_aspect
      class(StateItemAspect), intent(in) :: aspect
      integer, optional, intent(out) :: rc

      select type(aspect)
      class is (VectorClassAspect)
         vector_aspect = aspect
      class default
         _FAIL('aspect is not VectorClassAspect')
      end select

      _RETURN(_SUCCESS)
   end function to_vectorclassaspect_from_poly

   function to_vectorclassaspect_from_map(map, rc) result(vector_aspect)
      type(VectorClassAspect) :: vector_aspect
      type(AspectMap), target, intent(in) :: map
      integer, optional, intent(out) :: rc

      integer :: status
      class(StateItemAspect), pointer :: poly

      poly => map%at(CLASS_ASPECT_ID, _RC)
      vector_aspect = to_vectorclassaspect(poly, _RC)

      _RETURN(_SUCCESS)
   end function to_vectorclassaspect_from_map

   function make_transform(src, dst, other_aspects, rc) result(transform)
      class(ExtensionTransform), allocatable :: transform
      class(VectorClassAspect), intent(in) :: src
      class(StateItemAspect), intent(in) :: dst
      type(AspectMap), target, intent(in) :: other_aspects
      integer, optional, intent(out) :: rc
      
      transform = NullTransform()

      _RETURN(_SUCCESS)
   end function make_transform

   logical function supports_conversion_general(src)
      class(VectorClassAspect), intent(in) :: src
      supports_conversion_general = .false.
   end function supports_conversion_general

   logical function supports_conversion_specific(src, dst)
      class(VectorClassAspect), intent(in) :: src
      class(StateItemAspect), intent(in) :: dst

      supports_conversion_specific = .false.

      _UNUSED_DUMMY(dst)
   end function supports_conversion_specific

   subroutine add_to_state(this, multi_state, actual_pt, rc)
      class(VectorClassAspect), intent(in) :: this
      type(MultiState), intent(inout) :: multi_state
      type(ActualConnectionPt), intent(in) :: actual_pt
      integer, optional, intent(out) :: rc

      type(ESMF_FieldBundle) :: alias
      integer :: status
      type(ESMF_State) :: state, substate
      character(:), allocatable :: full_name, inner_name
      integer :: idx

      call multi_state%get_state(state, actual_pt%get_state_intent(), _RC)

      full_name = actual_pt%get_full_name()
      idx = index(full_name, '/', back=.true.)
      call get_substate(state, full_name(:idx-1), substate=substate, _RC)
      inner_name = full_name(idx+1:)

      alias = ESMF_NamedAlias(this%payload, name=inner_name, _RC)
      call ESMF_StateAdd(substate, [alias], _RC)

      _RETURN(_SUCCESS)
   end subroutine add_to_state


   function get_payload(this) result(field_bundle)
      type(ESMF_FieldBundle) :: field_bundle
      class(VectorClassAspect), intent(in) :: this
      field_bundle = this%payload
   end function get_payload

   
   function get_aspect_id() result(aspect_id)
      type(AspectId) :: aspect_id
      aspect_id = CLASS_ASPECT_ID
   end function get_aspect_id

end module mapl3g_VectorClassAspect
