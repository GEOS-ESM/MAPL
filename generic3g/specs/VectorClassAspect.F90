#include "MAPL.h"

module mapl3g_VectorClassAspect
   use mapl3g_Field_API
   use mapl3g_FieldBundle_API
   use mapl3g_ActualConnectionPt
   use mapl3g_AspectId
   use mapl3g_StateItemAspect
   use mapl3g_ClassAspect
   use mapl3g_FieldClassAspect
   use mapl3g_GeomAspect
   use mapl3g_VerticalGridAspect
   use mapl3g_UnitsAspect
   use mapl3g_TypeKindAspect
   use mapl3g_UngriddedDimsAspect
   use mapl3g_FieldBundleInfo, only: FieldBundleInfoSetInternal

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
   use gftl2_StringVector
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
      type(StringVector) :: short_names
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
      procedure :: activate
      procedure :: allocate
      procedure :: destroy
      procedure :: add_to_state

      procedure, nopass :: get_aspect_id
      procedure :: get_payload

   end type VectorClassAspect

   interface VectorClassAspect
      procedure :: new_VectorClassAspect_basic
   end interface VectorClassAspect


contains

   function new_VectorClassAspect_basic(short_names, component_specs) result(aspect)
      type(VectorClassAspect) :: aspect
      type(StringVector), intent(in) :: short_names
      type(FieldClassAspect), intent(in) :: component_specs(2)

      aspect%short_names = short_names
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

   subroutine create(this, other_aspects, handle, rc)
      class(VectorClassAspect), intent(inout) :: this
      type(AspectMap), intent(in) :: other_aspects
      integer, optional, intent(in) :: handle(:)
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_Info) :: info
      class(StateItemAspect), pointer :: aspect
      type(AspectId), allocatable :: ids(:)
      integer :: i

      this%payload = MAPL_FieldBundleCreate(fieldBundleType=FIELDBUNDLETYPE_VECTOR, _RC)
      _RETURN_UNLESS(present(handle))
      
      ids = [UNITS_ASPECT_ID, TYPEKIND_ASPECT_ID, UNGRIDDED_DIMS_ASPECT_ID]
      do i = 1, size(ids)
         aspect => other_aspects%at(ids(i), _RC)
         call aspect%update_payload(bundle=this%payload, _RC)
      end do

      call ESMF_InfoGetFromHost(this%payload, info, _RC)
      call FieldBundleInfoSetInternal(info, spec_handle=handle, _RC)
      call MAPL_FieldBundleSet(this%payload, allocation_status=STATEITEM_ALLOCATION_CREATED, _RC)

      _RETURN(ESMF_SUCCESS)
   end subroutine create

   subroutine activate(this, rc)
      class(VectorClassAspect), intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status

      call MAPL_FieldBundleSet(this%payload, allocation_status=STATEITEM_ALLOCATION_ACTIVE, _RC)

      _RETURN(ESMF_SUCCESS)
   end subroutine activate

   ! Tile / Grid   X  or X, Y
   subroutine allocate(this, other_aspects, rc)
      class(VectorClassAspect), intent(inout) :: this
      type(AspectMap), intent(in) :: other_aspects
      integer, optional, intent(out) :: rc

      integer :: status
      integer :: i
      type(FieldClassAspect) :: tmp

      do i = 1, NUM_COMPONENTS
         call this%component_specs(i)%create(other_aspects, _RC)
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

      type(ESMF_FieldBundle) :: alias, existing_bundle
      type(esmf_StateItem_Flag) :: itemType
      logical :: is_alias
      integer :: status
      type(ESMF_State) :: state, substate
      character(:), allocatable :: full_name, inner_name
      integer :: idx
      character(:), allocatable :: intent
      
      intent = actual_pt%get_state_intent()
      call multi_state%get_state(state, intent, _RC)

      full_name = actual_pt%get_full_name()
      idx = index(full_name, '/', back=.true.)
      call get_substate(state, full_name(:idx-1), substate=substate, _RC)
      inner_name = full_name(idx+1:)

      alias = ESMF_NamedAlias(this%payload, name=inner_name, _RC)
      call ESMF_StateGet(substate, itemName=inner_name, itemType=itemType, _RC)
      if (itemType /= ESMF_STATEITEM_NOTFOUND) then
         if (intent /= 'import') then
            call ESMF_StateGet(substate, itemName=inner_name, fieldBundle=existing_bundle, _RC)
            is_alias = mapl_FieldBundlesAreAliased(alias, existing_bundle, _RC)
            _ASSERT(is_alias, 'Different fields added under the same name in state.')
         end if
      end if
      call ESMF_StateAddReplace(substate, [alias], _RC)

      _RETURN(_SUCCESS)
   end subroutine add_to_state


   function get_aspect_id() result(aspect_id)
      type(AspectId) :: aspect_id
      aspect_id = CLASS_ASPECT_ID
   end function get_aspect_id


   subroutine get_payload(this, field, bundle, state, rc)
      class(VectorClassAspect), intent(in) :: this
      type(esmf_Field), optional, allocatable, intent(out) :: field
      type(esmf_FieldBundle), optional, allocatable, intent(out) :: bundle
      type(esmf_State), optional, allocatable, intent(out) :: state
      integer, optional, intent(out) :: rc

      integer :: status

      _ASSERT(present(bundle), 'Must request bundle from BracketClassAspect')
      bundle = this%payload

      _RETURN(_SUCCESS)
   end subroutine get_payload

end module mapl3g_VectorClassAspect
