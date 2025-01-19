#include "MAPL_Generic.h"

module mapl3g_BracketSpec

   use mapl_ErrorHandling
   use mapl_KeywordEnforcer
   use mapl3g_FieldSpec
   use mapl3g_StateItemSpec
   use mapl3g_ActualConnectionPt
   use mapl3g_ESMF_Utilities, only: get_substate
   use mapl3g_ActualPtSpecPtrMap
   use mapl3g_MultiState
   use mapl3g_ActualPtVector
   use mapl3g_ActualConnectionPt
   use mapl3g_ExtensionAction
   use mapl3g_VerticalGrid
   use mapl3g_VerticalDimSpec
   use mapl3g_AbstractActionSpec
   use mapl3g_NullAction
   use gftl2_StringVector
   use esmf
   use nuopc

   implicit none
   private

   public :: BracketSpec
   public :: new_BracketSpec_geom

   type, extends(StateItemSpec) :: BracketSpec
      private

      type(FieldSpec) :: reference_spec
      integer, allocatable :: bracket_size ! unallocated implies mirror value in connection
      type(FieldSpec), allocatable :: field_specs(:)
      type(ESMF_FieldBundle) :: payload

   contains
      procedure :: create
      procedure :: destroy
      procedure :: allocate

      procedure :: connect_to
      procedure :: can_connect_to
      procedure :: add_to_state
      procedure :: add_to_bundle

      procedure :: set_geometry
      procedure :: write_formatted
   end type BracketSpec

   interface BracketSpec
      module procedure new_BracketSpec_geom
   end interface BracketSpec

contains

   function new_BracketSpec_geom(field_spec, bracket_size) result(bracket_spec)
      
      type(BracketSpec) :: bracket_spec
      type(FieldSpec), optional, intent(in) :: field_spec
      integer, optional, intent(in) :: bracket_size

      bracket_spec%reference_spec = field_spec
      if (present(bracket_size)) bracket_spec%bracket_size = bracket_size
   end function new_BracketSpec_geom

   subroutine create(this, rc)
      class(BracketSpec), intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status
      
      this%payload = ESMF_FieldBundleCreate(_RC)

      _RETURN(ESMF_SUCCESS)
   end subroutine create

   ! Tile / Grid   X  or X, Y
   subroutine allocate(this, rc)
      class(BracketSpec), intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status
      integer :: i
      type(ESMF_Field) :: field, alias

      do i = 1, this%bracket_size
         call this%field_specs(i)%allocate(_RC)
         field = this%field_specs(i)%get_payload()
         alias = ESMF_NamedAlias(field, name=int_to_string(i), _RC)
         call ESMF_FieldBundleAdd(this%payload, [alias], multiflag=.true., _RC)
      end do

      _RETURN(ESMF_SUCCESS)

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

      class(BracketSpec), intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status

      call destroy_component_fields(this, _RC)
      call ESMF_FieldBundleDestroy(this%payload, nogarbage=.true., _RC)

      _RETURN(ESMF_SUCCESS)

   contains

      subroutine destroy_component_fields(this, rc)
         class(BracketSpec), intent(inout) :: this
         integer, optional, intent(out) :: rc

         integer :: status
         integer :: i

         if (allocated(this%field_specs)) then
            do i = 1, this%bracket_size
               call this%field_specs(i)%destroy(_RC)
            end do
         end if

         _RETURN(_SUCCESS)
      end subroutine destroy_component_fields

   end subroutine destroy

   logical function can_connect_to(this, src_spec, rc)

      class(BracketSpec), intent(in) :: this
      class(StateItemSpec), intent(in) :: src_spec
      integer, optional, intent(out) :: rc

      select type(src_spec)
      class is (BracketSpec)
         can_connect_to = all ([ &
              this%reference_spec%can_connect_to(src_spec%reference_spec), &
              match_integer(this%bracket_size, src_spec%bracket_size) & ! allow for mirroring
              ])
      class default
         can_connect_to = .false.
      end select

      _RETURN(_SUCCESS)

   contains

      ! At least one of src/dst must have allocated a bracket size.
      ! THe other can mirror.
      logical function match_integer(dst, src) result(match)
         integer, allocatable, intent(in) :: dst, src
         
         match = allocated(dst) .or. allocated(src)
         if (allocated(dst) .and. allocated(src)) then
            match = (src == dst)
         end if
      end function match_integer

   end function can_connect_to

   subroutine connect_to(this, src_spec, actual_pt, rc)

      class(BracketSpec), intent(inout) :: this
      class(StateItemSpec), intent(inout) :: src_spec
      type(ActualConnectionPt), intent(in) :: actual_pt ! unused
      integer, optional, intent(out) :: rc


      _FAIL('BracketSpec can only be export (src).')

      _RETURN(ESMF_SUCCESS)
      _UNUSED_DUMMY(actual_pt)

   end subroutine connect_to

   subroutine add_to_state(this, multi_state, actual_pt, rc)
      class(BracketSpec), intent(in) :: this
      type(MultiState), intent(inout) :: multi_state
      type(ActualConnectionPt), intent(in) :: actual_pt
      integer, optional, intent(out) :: rc

      type(ESMF_FieldBundle) :: alias
      integer :: status
      type(ESMF_State) :: state, substate
      character(:), allocatable :: short_name

      call multi_state%get_state(state, actual_pt%get_state_intent(), _RC)
      call get_substate(state, actual_pt%get_comp_name(), substate=substate, _RC)

      short_name = actual_pt%get_esmf_name()
      alias = ESMF_NamedAlias(this%payload, name=short_name, _RC)
      call ESMF_StateAdd(substate, [alias], _RC)

      _RETURN(_SUCCESS)
   end subroutine add_to_state

   subroutine add_to_bundle(this, bundle, rc)
      class(BracketSpec), intent(in) :: this
      type(ESMF_FieldBundle), intent(inout) :: bundle
      integer, optional, intent(out) :: rc

      _FAIL("Cannot add bundle (bracket) to ESMF bundle.")
      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(bundle)
   end subroutine add_to_bundle

   subroutine set_geometry(this, geom, vertical_grid, rc)
      class(BracketSpec), intent(inout) :: this
      type(ESMF_Geom), optional, intent(in) :: geom
      class(VerticalGrid), optional, intent(in) :: vertical_grid
      integer, optional, intent(out) :: rc

      _FAIL('unimplemented')

      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(geom)
      _UNUSED_DUMMY(vertical_grid)
   end subroutine set_geometry

   subroutine write_formatted(this, unit, iotype, v_list, iostat, iomsg)
      class(BracketSpec), intent(in) :: this
      integer, intent(in) :: unit
      character(*), intent(in) :: iotype
      integer, intent(in) :: v_list(:)
      integer, intent(out) :: iostat
      character(*), intent(inout) :: iomsg

      write(unit, "(a)", iostat=iostat, iomsg=iomsg) "BracketSpec(write not implemented yet)"
   end subroutine write_formatted

end module mapl3g_BracketSpec
