#include "MAPL.h"

module mapl3g_VectorBracketClassAspect
   use mapl3g_Field_API
   use mapl3g_FieldBundle_API
   use mapl3g_ActualConnectionPt
   use mapl3g_AspectId
   use mapl3g_StateItemAspect
   use mapl3g_GeomAspect
   use mapl3g_ClassAspect
   use mapl3g_FieldClassAspect
   use mapl3g_VectorClassAspect
   use mapl3g_GeomAspect
   use mapl3g_VerticalGridAspect
   use mapl3g_UnitsAspect
   use mapl3g_TypekindAspect
   use mapl3g_UngriddedDimsAspect
   use mapl3g_FieldBundleInfo, only: FieldBundleInfoSetInternal

   use mapl3g_VerticalGrid
   use mapl3g_VerticalStaggerLoc
   use mapl3g_UngriddedDims

   use mapl3g_NullTransform
   use mapl3g_TimeInterpolateTransform
   use mapl3g_ExtensionTransform
   use mapl3g_MultiState
   use mapl3g_ESMF_Utilities, only: get_substate

   use mapl3g_FieldCreate
   use mapl_FieldUtilities

   use mapl_KeywordEnforcer
   use mapl_ErrorHandling
   use esmf
   implicit none(type,external)
   private

   public :: VectorBracketClassAspect
   public :: to_VectorBracketClassAspect

   interface to_VectorBracketClassAspect
      procedure :: to_VectorBracketClassAspect_from_poly
      procedure :: to_VectorBracketClassAspect_from_map
   end interface to_VectorBracketClassAspect
   
   type, extends(ClassAspect) :: VectorBracketClassAspect
      private
      type(ESMF_FieldBundle) :: payload
      type(FieldClassAspect), allocatable :: field_aspect ! reference

      integer :: bracket_size   ! allocate only if not time dependent
      character(:), allocatable :: standard_name
      character(:), allocatable :: long_name

   contains
      procedure :: get_aspect_order
      procedure :: supports_conversion_general
      procedure :: supports_conversion_specific
      procedure :: make_transform
      procedure :: matches
      procedure :: connect_to_export

      procedure :: create
      procedure :: activate
      procedure :: allocate
      procedure :: destroy
      procedure :: add_to_state

      procedure :: get_payload
      
   end type VectorBracketClassAspect

   interface VectorBracketClassAspect
      procedure :: new_VectorBracketClassAspect
   end interface VectorBracketClassAspect

contains

   function new_VectorBracketClassAspect(bracket_size, standard_name, long_name) result(aspect)
      type(VectorBracketClassAspect) :: aspect
      integer, intent(in) :: bracket_size
      character(*), optional, intent(in) :: standard_name
      character(*), optional, intent(in) :: long_name

      aspect%field_aspect = FieldClassAspect(standard_name, long_name)
      aspect%bracket_size = bracket_size
      if (present(standard_name)) then
         aspect%standard_name = standard_name
      end if
      if (present(long_name)) then
         aspect%long_name = long_name
      end if
      
   end function new_VectorBracketClassAspect

   function get_aspect_order(this, goal_aspects, rc) result(aspect_ids)
      type(AspectId), allocatable :: aspect_ids(:)
      class(VectorBracketClassAspect), intent(in) :: this
      type(AspectMap), intent(in) :: goal_aspects
      integer, optional, intent(out) :: rc

      integer :: status
      type(GeomAspect) :: geom_aspect

      geom_aspect = to_GeomAspect(goal_aspects, _RC)
      if (geom_aspect%is_time_dependent()) then
         ! must do time interpolation first
         aspect_ids = [ &
              CLASS_ASPECT_ID, &
              GEOM_ASPECT_ID &
           ]
      end if

      ! Othrerwise doing geom regrid first is a performance improveent.
      aspect_ids = [ &
           GEOM_ASPECT_ID, &
           CLASS_ASPECT_ID &
           ]

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(goal_aspects)
   end function get_aspect_order

   subroutine create(this, other_aspects, handle, rc)
      class(VectorBracketClassAspect), intent(inout) :: this
      type(AspectMap), intent(in) :: other_aspects
      integer, optional, intent(in) :: handle(:) 
      integer, optional, intent(out) :: rc

     integer :: status
     type(ESMF_Info) :: info

      this%payload = MAPL_FieldBundleCreate(fieldBundleType=FIELDBUNDLETYPE_VECTOR_BRACKET, _RC)
      _RETURN_UNLESS(present(handle))

      call ESMF_InfoGetFromHost(this%payload, info, _RC)
      call FieldBundleInfoSetInternal(info, spec_handle=handle, allocation_status=STATEITEM_ALLOCATION_CREATED, bracket_updated=.true.,  _RC)
      call MAPL_FieldBundleSet(this%payload, allocation_status=STATEITEM_ALLOCATION_CREATED, _RC)

      _RETURN(_SUCCESS)
   end subroutine create

   subroutine activate(this, rc)
      class(VectorBracketClassAspect), intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status

      call MAPL_FieldBundleSet(this%payload, allocation_status=STATEITEM_ALLOCATION_ACTIVE, _RC)

      _RETURN(_SUCCESS)
   end subroutine activate

   ! Tile / Grid   X  or X, Y
   subroutine allocate(this, other_aspects, rc)
      class(VectorBracketClassAspect), intent(inout) :: this
      type(AspectMap), intent(in) :: other_aspects
      integer, optional, intent(out) :: rc

      integer :: status
      integer :: i
      type(FieldClassAspect) :: tmp

      associate (n => this%bracket_size)
        
        do i = 1, n
           tmp = this%field_aspect
           call tmp%create(other_aspects, _RC)
           call tmp%allocate(other_aspects, _RC)
           call tmp%add_to_bundle(this%payload, _RC)
        end do
      end associate

      _RETURN(_SUCCESS)

   contains

      function int_to_string(i) result(s)
         character(:), allocatable :: s
         integer, intent(in) :: i
         character(len=20) :: buffer
         write(buffer, '(i0)') i
         s = trim(buffer)
      end function int_to_string

   end subroutine allocate


  subroutine destroy(this, rc)
      class(VectorBracketClassAspect), intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status
      integer :: i
      type(ESMF_Field), allocatable :: fieldList(:)

      call MAPL_FieldBundleGet(this%payload, fieldList=fieldList, _RC)
      do i = 1, size(fieldList)
         call ESMF_FieldDestroy(fieldList(i), noGarbage=.true., _RC)
      end do
      call ESMF_FieldBundleDestroy(this%payload, noGarbage=.true., _RC)

      _RETURN(_SUCCESS)
   end subroutine destroy


   subroutine connect_to_export(this, export, actual_pt, rc)
      class(VectorBracketClassAspect), intent(inout) :: this
      class(StateItemAspect), intent(in) :: export
      type(ActualConnectionPt), intent(in) :: actual_pt
      integer, optional, intent(out) :: rc


      _FAIL("VectorBracketClassAspect cannot be an import")

      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(export)
      _UNUSED_DUMMY(actual_pt)
   end subroutine connect_to_export
   

   function to_VectorBracketClassAspect_from_poly(aspect, rc) result(bracket_aspect)
      type(VectorBracketClassAspect) :: bracket_aspect
      class(StateItemAspect), intent(in) :: aspect
      integer, optional, intent(out) :: rc

      integer :: status

      select type(aspect)
      class is (VectorBracketClassAspect)
         bracket_aspect = aspect
      class default
         _FAIL('aspect is not VectorBracketClassAspect')
      end select

      _RETURN(_SUCCESS)
   end function to_VectorBracketClassAspect_from_poly

   function to_VectorBracketClassAspect_from_map(map, rc) result(bracket_aspect)
      type(VectorBracketClassAspect) :: bracket_aspect
      type(AspectMap), target, intent(in) :: map
      integer, optional, intent(out) :: rc

      integer :: status
      class(StateItemAspect), pointer :: poly

      poly => map%at(CLASS_ASPECT_ID, _RC)
      bracket_aspect = to_VectorBracketClassAspect(poly, _RC)

      _RETURN(_SUCCESS)
   end function to_VectorBracketClassAspect_from_map
   

   function make_transform(src, dst, other_aspects, rc) result(transform)
      class(ExtensionTransform), allocatable :: transform
      class(VectorBracketClassAspect), intent(in) :: src
      class(StateItemAspect), intent(in) :: dst
      type(AspectMap), target, intent(in) :: other_aspects
      integer, optional, intent(out) :: rc

      ! No arguments to constructor - it uses ESMF_Info
      ! and FieldBundle structure to determine what to do
      transform = TimeInterpolateTransform()

      _RETURN(_SUCCESS)
   end function make_transform

   ! Should only connect to FieldClassAspect and
   ! then needs a TimeInterpolateTransform
   logical function matches(src, dst)
      class(VectorBracketClassAspect), intent(in) :: src
      class(StateItemAspect), intent(in) :: dst

      matches = .false.

   end function matches

   logical function supports_conversion_general(src)
      class(VectorBracketClassAspect), intent(in) :: src
      supports_conversion_general = .true.
   end function supports_conversion_general

   ! Only can convert if import is VectorClassAspect.
   logical function supports_conversion_specific(src, dst)
      class(VectorBracketClassAspect), intent(in) :: src
      class(StateItemAspect), intent(in) :: dst

      supports_conversion_specific = .false.
      select type (dst)
      type is (VectorClassAspect)
         supports_conversion_specific = .true.
      end select

      _UNUSED_DUMMY(dst)
   end function supports_conversion_specific

   subroutine add_to_state(this, multi_state, actual_pt, rc)
      class(VectorBracketClassAspect), intent(in) :: this
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

      call multi_state%get_state(state, actual_pt%get_state_intent(), _RC)

      full_name = actual_pt%get_full_name()
      idx = index(full_name, '/', back=.true.)
      call get_substate(state, full_name(:idx-1), substate=substate, _RC)
      inner_name = full_name(idx+1:)

      alias = ESMF_NamedAlias(this%payload, name=inner_name, _RC)
      call ESMF_StateGet(substate, itemName=inner_name, itemType=itemType, _RC)
      if (itemType /= ESMF_STATEITEM_NOTFOUND) then
         call ESMF_StateGet(substate, itemName=inner_name, fieldBundle=existing_bundle, _RC)
         is_alias = mapl_FieldBundlesAreAliased(alias, existing_bundle, _RC)
         _ASSERT(is_alias, 'Different field bundles added under the same name in state.')
      else
         call ESMF_StateAdd(substate, [alias], _RC)
      end if

      _RETURN(_SUCCESS)
   end subroutine add_to_state

   subroutine get_payload(this, unusable, field, bundle, state, rc)
      class(VectorBracketClassAspect), intent(in) :: this
      class(KeywordEnforcer), optional, intent(out) :: unusable
      type(esmf_Field), optional, allocatable, intent(out) :: field
      type(esmf_FieldBundle), optional, allocatable, intent(out) :: bundle
      type(esmf_State), optional, allocatable, intent(out) :: state
      integer, optional, intent(out) :: rc

      bundle = this%payload

      _RETURN(_SUCCESS)

   end subroutine get_payload

end module mapl3g_VectorBracketClassAspect
