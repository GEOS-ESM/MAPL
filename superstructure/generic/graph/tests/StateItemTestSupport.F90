!------------------------------------------------------------------------------
! Test-only helpers for building minimal, real ESMF_Field/FieldBundle/
! State/RouteHandle objects, used by GraphStateItem's and StateItemNode's
! pFUnit suites. Not part of the graph-node-hierarchy/state-item
! capabilities' public API.
!------------------------------------------------------------------------------
module StateItemTestSupport_mod
   use ESMF, only: ESMF_Field, ESMF_FieldBundle, ESMF_State, ESMF_RouteHandle
   use ESMF, only: ESMF_Grid, ESMF_GridCreateNoPeriDim
   use ESMF, only: ESMF_FieldCreate, ESMF_FieldDestroy
   use ESMF, only: ESMF_FieldBundleCreate, ESMF_FieldBundleDestroy
   use ESMF, only: ESMF_StateCreate, ESMF_StateDestroy
   use ESMF, only: ESMF_RouteHandleCreate, ESMF_RouteHandleDestroy
   use ESMF, only: ESMF_TYPEKIND_R4
   implicit none(type, external)
   private

   public :: make_test_field
   public :: make_test_field_bundle
   public :: make_test_state
   public :: make_test_route_handle

contains

   function make_test_field(name, rc) result(field)
      character(*), intent(in) :: name
      integer, optional, intent(out) :: rc
      type(ESMF_Field) :: field

      type(ESMF_Grid) :: grid
      integer :: status

      grid = ESMF_GridCreateNoPeriDim(countsPerDeDim1=[2], countsPerDeDim2=[2], rc=status)
      if (status /= 0) then
         if (present(rc)) rc = status
         return
      end if

      field = ESMF_FieldCreate(grid, ESMF_TYPEKIND_R4, name=name, rc=status)
      if (present(rc)) rc = status
   end function make_test_field

   function make_test_field_bundle(rc) result(bundle)
      integer, optional, intent(out) :: rc
      type(ESMF_FieldBundle) :: bundle

      integer :: status

      bundle = ESMF_FieldBundleCreate(rc=status)
      if (present(rc)) rc = status
   end function make_test_field_bundle

   function make_test_state(rc) result(state)
      integer, optional, intent(out) :: rc
      type(ESMF_State) :: state

      integer :: status

      state = ESMF_StateCreate(rc=status)
      if (present(rc)) rc = status
   end function make_test_state

   function make_test_route_handle(rc) result(route_handle)
      integer, optional, intent(out) :: rc
      type(ESMF_RouteHandle) :: route_handle

      integer :: status

      route_handle = ESMF_RouteHandleCreate(rc=status)
      if (present(rc)) rc = status
   end function make_test_route_handle

end module StateItemTestSupport_mod
