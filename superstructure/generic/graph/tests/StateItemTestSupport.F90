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
   use ESMF, only: ESMF_TYPEKIND_R4, ESMF_TYPEKIND_R8, operator(==)
   use ESMF, only: ESMF_Geom, ESMF_HConfig
   use ESMF, only: ESMF_HConfigCreate, ESMF_HConfigDestroy
   use ESMF, only: ESMF_FieldGet, ESMF_FieldBundleAdd
   use ESMF, only: ESMF_KIND_R4, ESMF_KIND_R8
   use ESMF, only: ESMF_TypeKind_Flag
   use mapl_field_api, only: MAPL_FieldCreate
   use mapl_field_bundle_api, only: MAPL_FieldBundleCreate, MAPL_FieldBundleSet
   use mapl_enums_api, only: MAPL_FieldBundleType_Flag
   use mapl_geom_api, only: mapl_GeomManager, mapl_get_geom_manager
   use mapl_geom_api, only: mapl_MaplGeom
   implicit none(type, external)
   private

   public :: make_test_field
   public :: make_test_field_bundle
   public :: make_test_state
   public :: make_test_route_handle
   public :: make_latlon_geom
   public :: make_test_member
   public :: make_test_bundle

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

   ! Field on `geom` (R4 unless `typekind` says R8) holding `value`
   ! everywhere. Made by MAPL_FieldCreate so that it carries the MAPL
   ! metadata the vector rotation reads.
   function make_test_member(geom, name, value, rc, typekind) result(field)
      type(ESMF_Geom), intent(in) :: geom
      character(*), intent(in) :: name
      real, intent(in) :: value
      integer, intent(out) :: rc
      type(ESMF_TypeKind_Flag), optional, intent(in) :: typekind
      type(ESMF_Field) :: field

      real(kind=ESMF_KIND_R4), pointer :: x4(:,:)
      real(kind=ESMF_KIND_R8), pointer :: x8(:,:)
      type(ESMF_TypeKind_Flag) :: typekind_
      integer :: status

      typekind_ = ESMF_TYPEKIND_R4
      if (present(typekind)) typekind_ = typekind
      field = MAPL_FieldCreate(geom, typekind_, name=name, rc=status)
      rc = status
      if (status /= 0) return
      if (typekind_ == ESMF_TYPEKIND_R8) then
         call ESMF_FieldGet(field, farrayPtr=x8, rc=status)
         rc = status
         if (status /= 0) return
         x8 = value
      else
         call ESMF_FieldGet(field, farrayPtr=x4, rc=status)
         rc = status
         if (status /= 0) return
         x4 = value
      end if
   end function make_test_member

   ! Bundle of the given type with its geom and typekind set first and then
   ! `members` (which may be empty) added: setting the geom resets the bundle,
   ! so it cannot come after the members.
   function make_test_bundle(geom, bundle_type, members, rc, typekind) &
        result(bundle)
      type(ESMF_Geom), intent(in) :: geom
      type(MAPL_FieldBundleType_Flag), intent(in) :: bundle_type
      type(ESMF_Field), intent(in) :: members(:)
      integer, intent(out) :: rc
      type(ESMF_TypeKind_Flag), optional, intent(in) :: typekind
      type(ESMF_FieldBundle) :: bundle

      type(ESMF_TypeKind_Flag) :: typekind_
      integer :: status

      typekind_ = ESMF_TYPEKIND_R4
      if (present(typekind)) typekind_ = typekind

      bundle = MAPL_FieldBundleCreate(name='test_bundle', &
           fieldBundleType=bundle_type, rc=status)
      rc = status
      if (status /= 0) return
      call MAPL_FieldBundleSet(bundle, geom=geom, &
           typekind=typekind_, rc=status)
      rc = status
      if (status /= 0) return
      if (size(members) == 0) return
      call ESMF_FieldBundleAdd(bundle, members, multiflag=.true., &
           relaxedFlag=.true., rc=status)
      rc = status
   end function make_test_bundle

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

   ! Global lat-lon grid, dateline- and pole-centered, so longitude
   ! centers are -180 + (i-1)*360/im and latitude centers are
   ! -90 + (j-1)*180/(jm-1).
   function make_latlon_geom(im, jm, rc) result(geom)
      integer, intent(in) :: im, jm
      integer, intent(out) :: rc
      type(ESMF_Geom) :: geom

      type(mapl_GeomManager), pointer :: geom_mgr
      type(mapl_MaplGeom), pointer :: mapl_geom
      type(ESMF_HConfig) :: hconfig
      character(200) :: content
      integer :: status

      write(content, '(a,i0,a,i0,a)') '{class: latlon, im_world: ', im, &
           ', jm_world: ', jm, ', pole: PC, dateline: DC, nx: 1, ny: 1}'
      hconfig = ESMF_HConfigCreate(content=trim(content), rc=status)
      rc = status
      if (status /= 0) return

      geom_mgr => mapl_get_geom_manager()
      mapl_geom => geom_mgr%get_mapl_geom(hconfig, rc=status)
      rc = status
      if (status /= 0) return
      geom = mapl_geom%get_geom()

      call ESMF_HConfigDestroy(hconfig, rc=status)
      rc = status
   end function make_latlon_geom

end module StateItemTestSupport_mod
