#include "MAPL.h"

!------------------------------------------------------------------------------
! StateItemDescriptor: the description of a state item, kept apart from the
! ESMF handle (GraphStateItem). Holds the sparse StateItemCharacteristicMap
! (REQ-CHAR-007/008/009/010) plus the description values that are not yet
! characteristics (ungridded dims, vertical stagger, declared geom / vertical
! grid / item type / restart mode / expression).
!
! Independent of any ESMF object: a StateItemNode's descriptor is populated at
! ADVERTISE, before any handle exists. Layering: graph/ only; must not depend
! on OuterMetaComponent, StateRegistry, or VariableSpec.
!------------------------------------------------------------------------------
module mapl_StateItemDescriptor_mod
   use ESMF, only: ESMF_StateItem_Flag, ESMF_STATEITEM_FIELD
   use ESMF, only: ESMF_Geom
   use mapl_StateItemCharacteristic_mod, only: StateItemCharacteristic, StateItemCharacteristicMap
   use mapl_StateItemCharacteristicKind_mod, only: StateItemCharacteristicKind, operator(==)
   use mapl_UngriddedDims_mod, only: UngriddedDims
   use mapl_VerticalStaggerLoc_mod, only: VerticalStaggerLoc
   use mapl_VerticalGrid_mod, only: VerticalGrid
   use mapl_RestartModes_mod, only: RestartMode
   use mapl_ErrorHandling_mod
   implicit none(type, external)
   private

   public :: StateItemDescriptor

   type :: StateItemDescriptor
      private
      type(StateItemCharacteristicMap) :: characteristics
      ! ungridded_dims is always present (like VariableSpec%ungridded_dims);
      ! the other values are allocatable because "not declared" is meaningful.
      type(UngriddedDims) :: ungridded_dims
      type(VerticalStaggerLoc), allocatable :: vertical_stagger
      type(ESMF_Geom), allocatable :: declared_geom
      class(VerticalGrid), allocatable :: declared_vertical_grid
      type(ESMF_StateItem_Flag) :: declared_item_type = ESMF_STATEITEM_FIELD
      type(RestartMode), allocatable :: declared_restart_mode
      character(:), allocatable :: declared_expression
   contains
      procedure :: has_characteristic => descriptor_has_characteristic
      procedure :: get_characteristic => descriptor_get_characteristic
      procedure :: set_characteristic => descriptor_set_characteristic
      procedure :: get_characteristics => descriptor_get_characteristics
      procedure :: get_ungridded_dims => descriptor_get_ungridded_dims
      procedure :: set_ungridded_dims => descriptor_set_ungridded_dims
      procedure :: get_vertical_stagger => descriptor_get_vertical_stagger
      procedure :: set_vertical_stagger => descriptor_set_vertical_stagger
      procedure :: get_declared_geom => descriptor_get_declared_geom
      procedure :: set_declared_geom => descriptor_set_declared_geom
      procedure :: get_declared_vertical_grid => descriptor_get_declared_vertical_grid
      procedure :: set_declared_vertical_grid => descriptor_set_declared_vertical_grid
      procedure :: get_declared_item_type => descriptor_get_declared_item_type
      procedure :: set_declared_item_type => descriptor_set_declared_item_type
      procedure :: get_declared_restart_mode => descriptor_get_declared_restart_mode
      procedure :: set_declared_restart_mode => descriptor_set_declared_restart_mode
      procedure :: get_declared_expression => descriptor_get_declared_expression
      procedure :: set_declared_expression => descriptor_set_declared_expression
   end type StateItemDescriptor

contains

   ! -- characteristics (REQ-CHAR-007/008/010) --

   logical function descriptor_has_characteristic(this, kind) result(has)
      class(StateItemDescriptor), intent(in) :: this
      type(StateItemCharacteristicKind), intent(in) :: kind

      has = this%characteristics%count(kind) > 0
   end function descriptor_has_characteristic

   ! REQ-CHAR-008: an absent key means "never established" and fails
   ! explicitly rather than returning an INVALID-status placeholder. A
   ! subroutine with an intent(out) allocatable dummy, not a function returning
   ! a polymorphic allocatable: an unallocated polymorphic function result on
   ! the failure path is not reliably safe for the caller to assign.
   subroutine descriptor_get_characteristic(this, kind, characteristic, rc)
      class(StateItemDescriptor), target, intent(in) :: this
      type(StateItemCharacteristicKind), intent(in) :: kind
      class(StateItemCharacteristic), allocatable, intent(out) :: characteristic
      integer, optional, intent(out) :: rc

      class(StateItemCharacteristic), pointer :: found
      integer :: status

      found => this%characteristics%at(kind, rc=status)
      _ASSERT(status == _SUCCESS, 'StateItemDescriptor: get_characteristic - no characteristic established for this kind')
      allocate(characteristic, source=found)

      _RETURN(_SUCCESS)
   end subroutine descriptor_get_characteristic

   ! Inserts or replaces the entry under `kind`; the one explicit-replace
   ! operation. REQ-CHAR-006/009: characteristic%get_kind() MUST equal `kind`,
   ! because detection pairs two items' entries by key and assumes both dynamic
   ! types match.
   subroutine descriptor_set_characteristic(this, kind, characteristic, rc)
      class(StateItemDescriptor), intent(inout) :: this
      type(StateItemCharacteristicKind), intent(in) :: kind
      class(StateItemCharacteristic), intent(in) :: characteristic
      integer, optional, intent(out) :: rc

      integer :: n_erased

      _ASSERT(characteristic%get_kind() == kind, 'StateItemDescriptor: set_characteristic - characteristic%get_kind() does not match kind')

      n_erased = int(this%characteristics%erase(kind))
      call this%characteristics%insert(kind, characteristic)

      _RETURN(_SUCCESS)
   end subroutine descriptor_set_characteristic

   ! REQ-CHAR-009: enumerable so detection can iterate every entry. Returns a
   ! copy.
   function descriptor_get_characteristics(this) result(characteristics)
      class(StateItemDescriptor), intent(in) :: this
      type(StateItemCharacteristicMap) :: characteristics

      characteristics = this%characteristics
   end function descriptor_get_characteristics

   ! -- materialization inputs --

   function descriptor_get_ungridded_dims(this) result(ungridded_dims)
      class(StateItemDescriptor), intent(in) :: this
      type(UngriddedDims) :: ungridded_dims

      ungridded_dims = this%ungridded_dims
   end function descriptor_get_ungridded_dims

   subroutine descriptor_set_ungridded_dims(this, ungridded_dims)
      class(StateItemDescriptor), intent(inout) :: this
      type(UngriddedDims), intent(in) :: ungridded_dims

      this%ungridded_dims = ungridded_dims
   end subroutine descriptor_set_ungridded_dims

   ! A subroutine with an intent(out) allocatable dummy: a direct
   ! `x = d%get_vertical_stagger()` from an unallocated allocatable function
   ! result crashed with a NULL dereference under gfortran.
   subroutine descriptor_get_vertical_stagger(this, vertical_stagger)
      class(StateItemDescriptor), intent(in) :: this
      type(VerticalStaggerLoc), allocatable, intent(out) :: vertical_stagger

      if (allocated(this%vertical_stagger)) vertical_stagger = this%vertical_stagger
   end subroutine descriptor_get_vertical_stagger

   ! Explicit deallocate + allocate(source=) instead of `=` into an allocatable
   ! component: avoids a gfortran realloc-on-assignment memory corruption.
   subroutine descriptor_set_vertical_stagger(this, vertical_stagger)
      class(StateItemDescriptor), intent(inout) :: this
      type(VerticalStaggerLoc), intent(in) :: vertical_stagger

      if (allocated(this%vertical_stagger)) deallocate(this%vertical_stagger)
      allocate(this%vertical_stagger, source=vertical_stagger)
   end subroutine descriptor_set_vertical_stagger

   ! -- declared geom / vgrid / item type / restart mode / expression --

   subroutine descriptor_get_declared_geom(this, geom)
      class(StateItemDescriptor), intent(in) :: this
      type(ESMF_Geom), allocatable, intent(out) :: geom

      if (allocated(this%declared_geom)) geom = this%declared_geom
   end subroutine descriptor_get_declared_geom

   subroutine descriptor_set_declared_geom(this, geom)
      class(StateItemDescriptor), intent(inout) :: this
      type(ESMF_Geom), intent(in) :: geom

      if (allocated(this%declared_geom)) deallocate(this%declared_geom)
      allocate(this%declared_geom, source=geom)
   end subroutine descriptor_set_declared_geom

   subroutine descriptor_get_declared_vertical_grid(this, vertical_grid)
      class(StateItemDescriptor), intent(in) :: this
      class(VerticalGrid), allocatable, intent(out) :: vertical_grid

      if (allocated(this%declared_vertical_grid)) vertical_grid = this%declared_vertical_grid
   end subroutine descriptor_get_declared_vertical_grid

   ! allocate(source=) also avoids relying on intrinsic-assignment dynamic-type
   ! dispatch for a CLASS allocatable component.
   subroutine descriptor_set_declared_vertical_grid(this, vertical_grid)
      class(StateItemDescriptor), intent(inout) :: this
      class(VerticalGrid), intent(in) :: vertical_grid

      if (allocated(this%declared_vertical_grid)) deallocate(this%declared_vertical_grid)
      allocate(this%declared_vertical_grid, source=vertical_grid)
   end subroutine descriptor_set_declared_vertical_grid

   function descriptor_get_declared_item_type(this) result(item_type)
      class(StateItemDescriptor), intent(in) :: this
      type(ESMF_StateItem_Flag) :: item_type

      item_type = this%declared_item_type
   end function descriptor_get_declared_item_type

   subroutine descriptor_set_declared_item_type(this, item_type)
      class(StateItemDescriptor), intent(inout) :: this
      type(ESMF_StateItem_Flag), intent(in) :: item_type

      this%declared_item_type = item_type
   end subroutine descriptor_set_declared_item_type

   subroutine descriptor_get_declared_restart_mode(this, restart_mode)
      class(StateItemDescriptor), intent(in) :: this
      type(RestartMode), allocatable, intent(out) :: restart_mode

      if (allocated(this%declared_restart_mode)) restart_mode = this%declared_restart_mode
   end subroutine descriptor_get_declared_restart_mode

   subroutine descriptor_set_declared_restart_mode(this, restart_mode)
      class(StateItemDescriptor), intent(inout) :: this
      type(RestartMode), intent(in) :: restart_mode

      if (allocated(this%declared_restart_mode)) deallocate(this%declared_restart_mode)
      allocate(this%declared_restart_mode, source=restart_mode)
   end subroutine descriptor_set_declared_restart_mode

   subroutine descriptor_get_declared_expression(this, expression)
      class(StateItemDescriptor), intent(in) :: this
      character(:), allocatable, intent(out) :: expression

      if (allocated(this%declared_expression)) expression = this%declared_expression
   end subroutine descriptor_get_declared_expression

   subroutine descriptor_set_declared_expression(this, expression)
      class(StateItemDescriptor), intent(inout) :: this
      character(*), intent(in) :: expression

      if (allocated(this%declared_expression)) deallocate(this%declared_expression)
      allocate(this%declared_expression, source=expression)
   end subroutine descriptor_set_declared_expression

end module mapl_StateItemDescriptor_mod
