#include "MAPL.h"

module mapl3g_Regridder
   use esmf
   use mapl_FieldUtils
   use mapl3g_FieldBundle_API
   use mapl3g_VectorBasisKind
   use mapl_ErrorHandlingMod
   use mapl3g_Geom_API
   use mapl3g_RegridderSpec
   use mapl3g_VectorBasis
   use iso_fortran_env, only: REAL32, REAL64
   implicit none(type,external)
   private

   public :: Regridder

   type, abstract :: Regridder
      private
      type(GeomManager), pointer :: geom_manager => null()
      type(RegridderSpec), allocatable :: spec
   contains
      procedure(I_regrid_field), deferred :: regrid_field
      procedure, non_overridable :: regrid_fieldbundle
      generic :: regrid => regrid_field
      generic :: regrid => regrid_fieldbundle

      procedure, non_overridable :: regrid_basic_bundle
      procedure, non_overridable :: regrid_vector

      procedure :: get_geom_manager => get_geom_mgr
      procedure :: set_geom_manager
      procedure, non_overridable :: set_spec
      procedure, non_overridable :: get_spec

      procedure, non_overridable :: regrid_array_1d_r4
      procedure, non_overridable :: regrid_array_1d_r8
      procedure, non_overridable :: regrid_array_2d_r4
      procedure, non_overridable :: regrid_array_2d_r8
      procedure, non_overridable :: regrid_array_3d_r4
      procedure, non_overridable :: regrid_array_3d_r8
      procedure, non_overridable :: regrid_array_4d_r4
      procedure, non_overridable :: regrid_array_4d_r8
      generic :: regrid => regrid_array_1d_r4
      generic :: regrid => regrid_array_1d_r8
      generic :: regrid => regrid_array_2d_r4
      generic :: regrid => regrid_array_2d_r8
      generic :: regrid => regrid_array_3d_r4
      generic :: regrid => regrid_array_3d_r8
      generic :: regrid => regrid_array_4d_r4
      generic :: regrid => regrid_array_4d_r8
   end type Regridder

   abstract interface
      subroutine I_regrid_field(this, f_in, f_out, rc)
         use esmf, only: ESMF_Field
         import Regridder
         class(Regridder), intent(inout) :: this
         type(ESMF_Field), intent(inout) :: f_in
         type(ESMF_Field), intent(inout) :: f_out
         integer, optional, intent(out) :: rc
      end subroutine I_regrid_field

   end interface

contains

   subroutine regrid_fieldbundle(this, fb_in, fb_out, rc)
      class(Regridder), intent(inout) :: this
      type(ESMF_FieldBundle), intent(inout) :: fb_in, fb_out
      integer, optional, intent(out) :: rc

      integer :: status
      type(FieldBundleType_Flag) :: bundleType_in, bundleType_out
      type(ESMF_FieldBundle) :: tb_in, tb_out
      type(ESMF_Field), allocatable :: field_list_in(:), field_list_out(:)

      call MAPL_FieldBundleGet(fb_in, fieldBundleType=bundleType_in, _RC)
      call MAPL_FieldBundleGet(fb_out, fieldBundleType=bundleType_out, _RC)
      _ASSERT(bundleType_out == bundleType_in, 'Bundle types must match.')

      if (bundleType_in == FIELDBUNDLETYPE_VECTOR) then
         call this%regrid_vector(fb_in, fb_out, _RC)
         _RETURN(_SUCCESS)
      else if (bundleType_in == FIELDBUNDLETYPE_VECTORBRACKET) then
         call MAPL_FieldBundleGet(fb_in, fieldList=field_list_in, _RC)
         call MAPL_FieldBundleGet(fb_out, fieldList=field_list_out, _RC)
         _ASSERT(mod(size(field_list_in), 2) == 0, 'VectorBracket must contain an even number of fields')
         _ASSERT(mod(size(field_list_out), 2) == 0, 'VectorBracket must contain an even number of fields')

         ! Get vector_basis_kind from parent bundle
         block
            type(VectorBasisKind) :: basis_kind
            integer :: i, n_pairs
            call MAPL_FieldBundleGet(fb_in, vector_basis_kind=basis_kind, _RC)

            n_pairs = size(field_list_in) / 2
            ! Loop over all vector pairs
            do i = 1, n_pairs
               tb_in = MAPL_FieldBundleCreate(fieldList=field_list_in(2*i-1:2*i), fieldBundleType=FIELDBUNDLETYPE_VECTOR, _RC)
               tb_out = MAPL_FieldBundleCreate(fieldList=field_list_out(2*i-1:2*i), fieldBundleType=FIELDBUNDLETYPE_VECTOR, _RC)
               call MAPL_FieldBundleSet(tb_in, vector_basis_kind=basis_kind, _RC)
               call MAPL_FieldBundleSet(tb_out, vector_basis_kind=basis_kind, _RC)
               call this%regrid_vector(tb_in, tb_out, _RC)
               call ESMF_FieldBundleDestroy(tb_in, noGarbage=.true., _RC)
               call ESMF_FieldBundleDestroy(tb_out, noGarbage=.true., _RC)
            end do
         end block

         _RETURN(_SUCCESS)
      end if

      call this%regrid_basic_bundle(fb_in, fb_out, _RC)

      _RETURN(_SUCCESS)
   end subroutine regrid_fieldbundle

   subroutine regrid_basic_bundle(this, fb_in, fb_out, rc)
      class(Regridder), intent(inout) :: this
      type(ESMF_FieldBundle), intent(inout) :: fb_in, fb_out
      integer, optional, intent(out) :: rc

      type(ESMF_Field), allocatable :: fieldList_in(:), fieldList_out(:)
      integer :: status
      integer :: i

      call MAPL_FieldBundleGet(fb_in, fieldList=fieldList_in, _RC)
      call MAPL_FieldBundleGet(fb_out, fieldList=fieldList_out, _RC)

      _ASSERT(size(fieldList_out) == size(fieldList_in), 'Brackets must have same size.')

      do i = 1, size(fieldList_in)
         call this%regrid(fieldList_in(i), fieldList_out(i), _RC)
      end do

      _RETURN(_SUCCESS)
   end subroutine regrid_basic_bundle

   subroutine regrid_vector(this, fb_in, fb_out, rc)
      class(Regridder), intent(inout) :: this
      type(ESMF_FieldBundle), intent(inout) :: fb_in, fb_out
      integer, optional, intent(out) :: rc

      type(ESMF_Field), allocatable :: uv_in(:), uv_out(:)
      type(ESMF_Field) :: xyz_in(3), xyz_out(3)
      integer :: status
      integer :: i
      integer :: id_in, id_out
      type(MaplGeom), pointer :: mapl_geom
      type(VectorBasis), pointer :: basis
      type(GeomManager), pointer :: geom_mgr
      type(ESMF_Geom) :: geom_in, geom_out
      type(VectorBasisKind) :: basis_kind

      call MAPL_FieldBundleGet(fb_in, fieldList=uv_in, _RC)
      call MAPL_FieldBundleGet(fb_out, fieldList=uv_out, _RC)

      _ASSERT(size(uv_in) == 2, 'TangentVector must consiste of exactly 2 fields.')
      _ASSERT(size(uv_out) == 2, 'TangentVector must consiste of exactly 2 fields.')
      
      call create_field_vector(archetype=uv_in(1), fv=xyz_in, _RC)
      call create_field_vector(archetype=uv_out(1), fv=xyz_out, _RC)
      
      ! Initialize xyz_in to avoid floating point invalid operations
      do i = 1, size(xyz_in)
         call ESMF_FieldFill(xyz_in(i), dataFillScheme='const', const1=0.0_ESMF_KIND_R8, _RC)
      end do

      geom_mgr => this%get_geom_manager()

      ! Get basis kind from input bundle and get corresponding basis
      call MAPL_FieldBundleGet(fb_in, vector_basis_kind=basis_kind, _RC)
      call ESMF_FieldGet(uv_in(1), geom=geom_in, _RC)
      id_in = MAPL_GeomGetId(geom_in, _RC)
      mapl_geom => geom_mgr%get_mapl_geom(id_in, _RC)
      basis => mapl_geom%get_basis(basis_kind, _RC)

      call FieldGEMV('N', 1., basis%elements, uv_in, 0., xyz_in, _RC)

      ! Regrid component-by-component
      do i = 1, 3 
         call this%regrid(xyz_in(i), xyz_out(i), _RC)
      end do

      ! Get basis kind from output bundle and get corresponding basis
      call MAPL_FieldBundleGet(fb_out, vector_basis_kind=basis_kind, _RC)
      call ESMF_FieldGet(uv_out(1), geom=geom_out, _RC)
      id_out = MAPL_GeomGetId(geom_out, _RC)
      mapl_geom => geom_mgr%get_mapl_geom(id_out, _RC)
      basis => mapl_geom%get_basis(basis_kind, _RC)
      call FieldGEMV('T', 1., basis%elements, xyz_out, 0., uv_out, _RC)

      call destroy_field_vector(xyz_in, _RC)
      call destroy_field_vector(xyz_out, _RC)

      _RETURN(_SUCCESS)
   end subroutine regrid_vector

   subroutine create_field_vector(archetype, fv, rc)
      type(ESMF_Field), intent(inout) :: archetype
      type(ESMF_Field), intent(out) :: fv(:)
      integer, optional, intent(out) :: rc

      integer :: i
      integer :: status

      do i = 1, size(fv)
         call FieldClone(archetype, fv(i), _RC)
      end do

      _RETURN(_SUCCESS)
   end subroutine create_field_vector

   subroutine destroy_field_vector(fv, rc)
      type(ESMF_Field), intent(inout) :: fv(:)
      integer, optional, intent(out) :: rc

      integer :: i
      integer :: status

      do i = 1, size(fv)
         call ESMF_FieldDestroy(fv(i), noGarbage=.true., _RC)
      end do

      _RETURN(_SUCCESS)
   end subroutine destroy_field_vector

   subroutine set_geom_manager(this, geom_manager)
      class(Regridder), intent(inout) :: this
      type(GeomManager), pointer, intent(in) :: geom_manager
      this%geom_manager => geom_manager
   end subroutine set_geom_manager

   function get_geom_mgr(this) result(geom_manager)
      type(GeomManager), pointer :: geom_manager
      class(Regridder), intent(in) :: this
      geom_manager => this%geom_manager
   end function get_geom_mgr

   subroutine set_spec(this, spec)
      class(Regridder), intent(inout) :: this
      type(RegridderSpec), intent(in) :: spec
      this%spec = spec
   end subroutine set_spec

   function get_spec(this) result(spec)
      class(Regridder), intent(in) :: this
      type(RegridderSpec) :: spec
      spec = this%spec
   end function get_spec

   ! Returns the number of gridded dimensions for a given geom via dimCount.
   ! Grid -> dimCount from ESMF_GridGet; Mesh, LocStream, XGrid -> 1.
   integer function get_geom_dimcount(geom, rc) result(dimcount)
      type(ESMF_Geom), intent(in) :: geom
      integer, optional, intent(out) :: rc

      integer :: status

      call ESMF_GeomGet(geom, dimCount=dimcount, _RC)

      _RETURN(_SUCCESS)
   end function get_geom_dimcount

   ! ---------------------------------------------------------------------------
   ! Helper: given geom dimCount dc and the shape of an array, allocate and
   ! fill the gridToFieldMap, ungriddedLBound, and ungriddedUBound arrays.
   ! The rank is derived from size(sizes).
   ! When dc == rank (all dims gridded) the three allocatables are left
   ! unallocated, so they appear as absent to ESMF optional arguments.
   ! ---------------------------------------------------------------------------

   pure subroutine make_fieldCreate_args(dc, sizes, gtfm, ulb, uub)
      integer, intent(in)  :: dc
      integer, intent(in)  :: sizes(:)   ! shape(q), length == rank
      integer, allocatable, intent(out) :: gtfm(:), ulb(:), uub(:)
      integer :: i, n_ug
      n_ug = size(sizes) - dc
      if (n_ug > 0) then
         gtfm = [(i, i=1, dc)]
         ulb  = [(1, i=1, n_ug)]
         uub  = [(sizes(dc+i), i=1, n_ug)]
      end if
   end subroutine make_fieldCreate_args

   ! ---------------------------------------------------------------------------
   ! Array overloads: wrap raw arrays as ESMF_Fields and delegate to regrid_field.
   ! Gridded dims always come first; any remaining dims are ungridded.
   ! The geom dimCount (from this%spec) determines how many leading dims are gridded.
   !
   ! Assertions:
   !   1-D array: geom dimCount must == 1
   !   2-D array: geom dimCount must <= 2
   !   3-D array: geom dimCount must <= 3
   !   4-D array: geom dimCount must <= 4
   ! ---------------------------------------------------------------------------

   subroutine regrid_array_1d_r4(this, q_in, q_out, rc)
      class(Regridder), intent(inout) :: this
      real(REAL32), intent(in)  :: q_in(:)
      real(REAL32), intent(out) :: q_out(:)
      integer, optional, intent(out) :: rc

      integer :: status
      integer :: dc_in, dc_out
      type(ESMF_Geom) :: geom_in, geom_out
      type(ESMF_Field) :: f_in, f_out
      real(REAL32), pointer :: p_in(:), p_out(:)

      _ASSERT(allocated(this%spec), 'set_spec must be called before regrid array overloads')
      geom_in  = this%spec%get_geom_in()
      geom_out = this%spec%get_geom_out()
      dc_in  = get_geom_dimcount(geom_in,  _RC)
      dc_out = get_geom_dimcount(geom_out, _RC)
      _ASSERT(dc_in  == 1, 'geom dimCount must be 1 for 1-D array overload')
      _ASSERT(dc_out == 1, 'geom dimCount must be 1 for 1-D array overload')

      f_in  = ESMF_FieldCreate(geom_in,  typekind=ESMF_TYPEKIND_R4, _RC)
      f_out = ESMF_FieldCreate(geom_out, typekind=ESMF_TYPEKIND_R4, _RC)

      call ESMF_FieldGet(f_in,  farrayPtr=p_in,  _RC)
      p_in = q_in
      call this%regrid_field(f_in, f_out, _RC)
      call ESMF_FieldGet(f_out, farrayPtr=p_out, _RC)
      q_out = p_out

      call ESMF_FieldDestroy(f_in,  noGarbage=.true., _RC)
      call ESMF_FieldDestroy(f_out, noGarbage=.true., _RC)

      _RETURN(_SUCCESS)
   end subroutine regrid_array_1d_r4

   subroutine regrid_array_1d_r8(this, q_in, q_out, rc)
      class(Regridder), intent(inout) :: this
      real(REAL64), intent(in)  :: q_in(:)
      real(REAL64), intent(out) :: q_out(:)
      integer, optional, intent(out) :: rc

      integer :: status
      integer :: dc_in, dc_out
      type(ESMF_Geom) :: geom_in, geom_out
      type(ESMF_Field) :: f_in, f_out
      real(REAL64), pointer :: p_in(:), p_out(:)

      _ASSERT(allocated(this%spec), 'set_spec must be called before regrid array overloads')
      geom_in  = this%spec%get_geom_in()
      geom_out = this%spec%get_geom_out()
      dc_in  = get_geom_dimcount(geom_in,  _RC)
      dc_out = get_geom_dimcount(geom_out, _RC)
      _ASSERT(dc_in  == 1, 'geom dimCount must be 1 for 1-D array overload')
      _ASSERT(dc_out == 1, 'geom dimCount must be 1 for 1-D array overload')

      f_in  = ESMF_FieldCreate(geom_in,  typekind=ESMF_TYPEKIND_R8, _RC)
      f_out = ESMF_FieldCreate(geom_out, typekind=ESMF_TYPEKIND_R8, _RC)

      call ESMF_FieldGet(f_in,  farrayPtr=p_in,  _RC)
      p_in = q_in
      call this%regrid_field(f_in, f_out, _RC)
      call ESMF_FieldGet(f_out, farrayPtr=p_out, _RC)
      q_out = p_out

      call ESMF_FieldDestroy(f_in,  noGarbage=.true., _RC)
      call ESMF_FieldDestroy(f_out, noGarbage=.true., _RC)

      _RETURN(_SUCCESS)
   end subroutine regrid_array_1d_r8

   subroutine regrid_array_2d_r4(this, q_in, q_out, rc)
      class(Regridder), intent(inout) :: this
      real(REAL32), intent(in)  :: q_in(:,:)
      real(REAL32), intent(out) :: q_out(:,:)
      integer, optional, intent(out) :: rc

      integer :: status
      integer :: dc_in, dc_out
      integer, allocatable :: gtfm_in(:), ulb_in(:), uub_in(:)
      integer, allocatable :: gtfm_out(:), ulb_out(:), uub_out(:)
      type(ESMF_Geom) :: geom_in, geom_out
      type(ESMF_Field) :: f_in, f_out
      real(REAL32), pointer :: p_in(:,:), p_out(:,:)

      _ASSERT(allocated(this%spec), 'set_spec must be called before regrid array overloads')
      geom_in  = this%spec%get_geom_in()
      geom_out = this%spec%get_geom_out()
      dc_in  = get_geom_dimcount(geom_in,  _RC)
      dc_out = get_geom_dimcount(geom_out, _RC)
      _ASSERT(dc_in  <= 2, 'geom dimCount must be <= 2 for 2-D array overload')
      _ASSERT(dc_out <= 2, 'geom dimCount must be <= 2 for 2-D array overload')

      call make_fieldCreate_args(dc_in,  shape(q_in),  gtfm_in,  ulb_in,  uub_in)
      call make_fieldCreate_args(dc_out, shape(q_out), gtfm_out, ulb_out, uub_out)

      f_in  = ESMF_FieldCreate(geom_in,  typekind=ESMF_TYPEKIND_R4, &
           gridToFieldMap=gtfm_in,  ungriddedLBound=ulb_in,  ungriddedUBound=uub_in,  _RC)
      f_out = ESMF_FieldCreate(geom_out, typekind=ESMF_TYPEKIND_R4, &
           gridToFieldMap=gtfm_out, ungriddedLBound=ulb_out, ungriddedUBound=uub_out, _RC)

      call ESMF_FieldGet(f_in,  farrayPtr=p_in,  _RC)
      p_in = q_in
      call this%regrid_field(f_in, f_out, _RC)
      call ESMF_FieldGet(f_out, farrayPtr=p_out, _RC)
      q_out = p_out

      call ESMF_FieldDestroy(f_in,  noGarbage=.true., _RC)
      call ESMF_FieldDestroy(f_out, noGarbage=.true., _RC)

      _RETURN(_SUCCESS)
   end subroutine regrid_array_2d_r4

   subroutine regrid_array_2d_r8(this, q_in, q_out, rc)
      class(Regridder), intent(inout) :: this
      real(REAL64), intent(in)  :: q_in(:,:)
      real(REAL64), intent(out) :: q_out(:,:)
      integer, optional, intent(out) :: rc

      integer :: status
      integer :: dc_in, dc_out
      integer, allocatable :: gtfm_in(:), ulb_in(:), uub_in(:)
      integer, allocatable :: gtfm_out(:), ulb_out(:), uub_out(:)
      type(ESMF_Geom) :: geom_in, geom_out
      type(ESMF_Field) :: f_in, f_out
      real(REAL64), pointer :: p_in(:,:), p_out(:,:)

      _ASSERT(allocated(this%spec), 'set_spec must be called before regrid array overloads')
      geom_in  = this%spec%get_geom_in()
      geom_out = this%spec%get_geom_out()
      dc_in  = get_geom_dimcount(geom_in,  _RC)
      dc_out = get_geom_dimcount(geom_out, _RC)
      _ASSERT(dc_in  <= 2, 'geom dimCount must be <= 2 for 2-D array overload')
      _ASSERT(dc_out <= 2, 'geom dimCount must be <= 2 for 2-D array overload')

      call make_fieldCreate_args(dc_in,  shape(q_in),  gtfm_in,  ulb_in,  uub_in)
      call make_fieldCreate_args(dc_out, shape(q_out), gtfm_out, ulb_out, uub_out)

      f_in  = ESMF_FieldCreate(geom_in,  typekind=ESMF_TYPEKIND_R8, &
           gridToFieldMap=gtfm_in,  ungriddedLBound=ulb_in,  ungriddedUBound=uub_in,  _RC)
      f_out = ESMF_FieldCreate(geom_out, typekind=ESMF_TYPEKIND_R8, &
           gridToFieldMap=gtfm_out, ungriddedLBound=ulb_out, ungriddedUBound=uub_out, _RC)

      call ESMF_FieldGet(f_in,  farrayPtr=p_in,  _RC)
      p_in = q_in
      call this%regrid_field(f_in, f_out, _RC)
      call ESMF_FieldGet(f_out, farrayPtr=p_out, _RC)
      q_out = p_out

      call ESMF_FieldDestroy(f_in,  noGarbage=.true., _RC)
      call ESMF_FieldDestroy(f_out, noGarbage=.true., _RC)

      _RETURN(_SUCCESS)
   end subroutine regrid_array_2d_r8

   subroutine regrid_array_3d_r4(this, q_in, q_out, rc)
      class(Regridder), intent(inout) :: this
      real(REAL32), intent(in)  :: q_in(:,:,:)
      real(REAL32), intent(out) :: q_out(:,:,:)
      integer, optional, intent(out) :: rc

      integer :: status
      integer :: dc_in, dc_out
      integer, allocatable :: gtfm_in(:), ulb_in(:), uub_in(:)
      integer, allocatable :: gtfm_out(:), ulb_out(:), uub_out(:)
      type(ESMF_Geom) :: geom_in, geom_out
      type(ESMF_Field) :: f_in, f_out
      real(REAL32), pointer :: p_in(:,:,:), p_out(:,:,:)

      _ASSERT(allocated(this%spec), 'set_spec must be called before regrid array overloads')
      geom_in  = this%spec%get_geom_in()
      geom_out = this%spec%get_geom_out()
      dc_in  = get_geom_dimcount(geom_in,  _RC)
      dc_out = get_geom_dimcount(geom_out, _RC)
      _ASSERT(dc_in  <= 3, 'geom dimCount must be <= 3 for 3-D array overload')
      _ASSERT(dc_out <= 3, 'geom dimCount must be <= 3 for 3-D array overload')

      call make_fieldCreate_args(dc_in,  shape(q_in),  gtfm_in,  ulb_in,  uub_in)
      call make_fieldCreate_args(dc_out, shape(q_out), gtfm_out, ulb_out, uub_out)

      f_in  = ESMF_FieldCreate(geom_in,  typekind=ESMF_TYPEKIND_R4, &
           gridToFieldMap=gtfm_in,  ungriddedLBound=ulb_in,  ungriddedUBound=uub_in,  _RC)
      f_out = ESMF_FieldCreate(geom_out, typekind=ESMF_TYPEKIND_R4, &
           gridToFieldMap=gtfm_out, ungriddedLBound=ulb_out, ungriddedUBound=uub_out, _RC)

      call ESMF_FieldGet(f_in,  farrayPtr=p_in,  _RC)
      p_in = q_in
      call this%regrid_field(f_in, f_out, _RC)
      call ESMF_FieldGet(f_out, farrayPtr=p_out, _RC)
      q_out = p_out

      call ESMF_FieldDestroy(f_in,  noGarbage=.true., _RC)
      call ESMF_FieldDestroy(f_out, noGarbage=.true., _RC)

      _RETURN(_SUCCESS)
   end subroutine regrid_array_3d_r4

   subroutine regrid_array_3d_r8(this, q_in, q_out, rc)
      class(Regridder), intent(inout) :: this
      real(REAL64), intent(in)  :: q_in(:,:,:)
      real(REAL64), intent(out) :: q_out(:,:,:)
      integer, optional, intent(out) :: rc

      integer :: status
      integer :: dc_in, dc_out
      integer, allocatable :: gtfm_in(:), ulb_in(:), uub_in(:)
      integer, allocatable :: gtfm_out(:), ulb_out(:), uub_out(:)
      type(ESMF_Geom) :: geom_in, geom_out
      type(ESMF_Field) :: f_in, f_out
      real(REAL64), pointer :: p_in(:,:,:), p_out(:,:,:)

      _ASSERT(allocated(this%spec), 'set_spec must be called before regrid array overloads')
      geom_in  = this%spec%get_geom_in()
      geom_out = this%spec%get_geom_out()
      dc_in  = get_geom_dimcount(geom_in,  _RC)
      dc_out = get_geom_dimcount(geom_out, _RC)
      _ASSERT(dc_in  <= 3, 'geom dimCount must be <= 3 for 3-D array overload')
      _ASSERT(dc_out <= 3, 'geom dimCount must be <= 3 for 3-D array overload')

      call make_fieldCreate_args(dc_in,  shape(q_in),  gtfm_in,  ulb_in,  uub_in)
      call make_fieldCreate_args(dc_out, shape(q_out), gtfm_out, ulb_out, uub_out)

      f_in  = ESMF_FieldCreate(geom_in,  typekind=ESMF_TYPEKIND_R8, &
           gridToFieldMap=gtfm_in,  ungriddedLBound=ulb_in,  ungriddedUBound=uub_in,  _RC)
      f_out = ESMF_FieldCreate(geom_out, typekind=ESMF_TYPEKIND_R8, &
           gridToFieldMap=gtfm_out, ungriddedLBound=ulb_out, ungriddedUBound=uub_out, _RC)

      call ESMF_FieldGet(f_in,  farrayPtr=p_in,  _RC)
      p_in = q_in
      call this%regrid_field(f_in, f_out, _RC)
      call ESMF_FieldGet(f_out, farrayPtr=p_out, _RC)
      q_out = p_out

      call ESMF_FieldDestroy(f_in,  noGarbage=.true., _RC)
      call ESMF_FieldDestroy(f_out, noGarbage=.true., _RC)

      _RETURN(_SUCCESS)
   end subroutine regrid_array_3d_r8

   subroutine regrid_array_4d_r4(this, q_in, q_out, rc)
      class(Regridder), intent(inout) :: this
      real(REAL32), intent(in)  :: q_in(:,:,:,:)
      real(REAL32), intent(out) :: q_out(:,:,:,:)
      integer, optional, intent(out) :: rc

      integer :: status
      integer :: dc_in, dc_out
      integer, allocatable :: gtfm_in(:), ulb_in(:), uub_in(:)
      integer, allocatable :: gtfm_out(:), ulb_out(:), uub_out(:)
      type(ESMF_Geom) :: geom_in, geom_out
      type(ESMF_Field) :: f_in, f_out
      real(REAL32), pointer :: p_in(:,:,:,:), p_out(:,:,:,:)

      _ASSERT(allocated(this%spec), 'set_spec must be called before regrid array overloads')
      geom_in  = this%spec%get_geom_in()
      geom_out = this%spec%get_geom_out()
      dc_in  = get_geom_dimcount(geom_in,  _RC)
      dc_out = get_geom_dimcount(geom_out, _RC)
      _ASSERT(dc_in  <= 4, 'geom dimCount must be <= 4 for 4-D array overload')
      _ASSERT(dc_out <= 4, 'geom dimCount must be <= 4 for 4-D array overload')

      call make_fieldCreate_args(dc_in,  shape(q_in),  gtfm_in,  ulb_in,  uub_in)
      call make_fieldCreate_args(dc_out, shape(q_out), gtfm_out, ulb_out, uub_out)

      f_in  = ESMF_FieldCreate(geom_in,  typekind=ESMF_TYPEKIND_R4, &
           gridToFieldMap=gtfm_in,  ungriddedLBound=ulb_in,  ungriddedUBound=uub_in,  _RC)
      f_out = ESMF_FieldCreate(geom_out, typekind=ESMF_TYPEKIND_R4, &
           gridToFieldMap=gtfm_out, ungriddedLBound=ulb_out, ungriddedUBound=uub_out, _RC)

      call ESMF_FieldGet(f_in,  farrayPtr=p_in,  _RC)
      p_in = q_in
      call this%regrid_field(f_in, f_out, _RC)
      call ESMF_FieldGet(f_out, farrayPtr=p_out, _RC)
      q_out = p_out

      call ESMF_FieldDestroy(f_in,  noGarbage=.true., _RC)
      call ESMF_FieldDestroy(f_out, noGarbage=.true., _RC)

      _RETURN(_SUCCESS)
   end subroutine regrid_array_4d_r4

   subroutine regrid_array_4d_r8(this, q_in, q_out, rc)
      class(Regridder), intent(inout) :: this
      real(REAL64), intent(in)  :: q_in(:,:,:,:)
      real(REAL64), intent(out) :: q_out(:,:,:,:)
      integer, optional, intent(out) :: rc

      integer :: status
      integer :: dc_in, dc_out
      integer, allocatable :: gtfm_in(:), ulb_in(:), uub_in(:)
      integer, allocatable :: gtfm_out(:), ulb_out(:), uub_out(:)
      type(ESMF_Geom) :: geom_in, geom_out
      type(ESMF_Field) :: f_in, f_out
      real(REAL64), pointer :: p_in(:,:,:,:), p_out(:,:,:,:)

      _ASSERT(allocated(this%spec), 'set_spec must be called before regrid array overloads')
      geom_in  = this%spec%get_geom_in()
      geom_out = this%spec%get_geom_out()
      dc_in  = get_geom_dimcount(geom_in,  _RC)
      dc_out = get_geom_dimcount(geom_out, _RC)
      _ASSERT(dc_in  <= 4, 'geom dimCount must be <= 4 for 4-D array overload')
      _ASSERT(dc_out <= 4, 'geom dimCount must be <= 4 for 4-D array overload')

      call make_fieldCreate_args(dc_in,  shape(q_in),  gtfm_in,  ulb_in,  uub_in)
      call make_fieldCreate_args(dc_out, shape(q_out), gtfm_out, ulb_out, uub_out)

      f_in  = ESMF_FieldCreate(geom_in,  typekind=ESMF_TYPEKIND_R8, &
           gridToFieldMap=gtfm_in,  ungriddedLBound=ulb_in,  ungriddedUBound=uub_in,  _RC)
      f_out = ESMF_FieldCreate(geom_out, typekind=ESMF_TYPEKIND_R8, &
           gridToFieldMap=gtfm_out, ungriddedLBound=ulb_out, ungriddedUBound=uub_out, _RC)

      call ESMF_FieldGet(f_in,  farrayPtr=p_in,  _RC)
      p_in = q_in
      call this%regrid_field(f_in, f_out, _RC)
      call ESMF_FieldGet(f_out, farrayPtr=p_out, _RC)
      q_out = p_out

      call ESMF_FieldDestroy(f_in,  noGarbage=.true., _RC)
      call ESMF_FieldDestroy(f_out, noGarbage=.true., _RC)

      _RETURN(_SUCCESS)
   end subroutine regrid_array_4d_r8

end module mapl3g_Regridder
      
