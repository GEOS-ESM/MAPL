#include "MAPL_Generic.h"

module MAPL_EsmfRegridderMod
   use, intrinsic :: iso_fortran_env, only: REAL32, REAL64
   use ESMF
   use mapl_RegridMethods
   use MAPL_KeywordEnforcerMod
   use mapl_ErrorHandlingMod
   use MAPL_RegridderSpec
   use MAPL_AbstractGridFactoryMod
   use MAPL_AbstractRegridderMod
   use MAPL_GridManagerMod
   use MAPL_BaseMod, only: MAPL_undef, MAPL_GridHasDE
   use MAPL_RegridderSpecRouteHandleMap
   use MAPL_MAPLGrid
   use MAPL_ConstantsMod
   use pFlogger, only: logging, Logger
   implicit none
   private

   public :: EsmfRegridder

   integer, save :: counter = 0

   ! Singleton container to avoid unnecessary duplication of
   ! ESMF Route handles
   type (RegridderSpecRouteHandleMap), save, target :: route_handles_r4
   type (RegridderSpecRouteHandleMap), save, target :: route_handles_r8

   type (RegridderSpecRouteHandleMap), save, target :: transpose_route_handles_r4
   type (RegridderSpecRouteHandleMap), save, target :: transpose_route_handles_r8

   type, extends(AbstractRegridder) :: EsmfRegridder
      integer :: regrid_method
      type (ESMF_DynamicMask) :: dynamic_mask
   contains
      procedure :: initialize_subclass
      procedure, nopass :: supports

      procedure :: regrid_scalar_2d_real32
      procedure :: regrid_scalar_2d_real64
      procedure :: regrid_scalar_3d_real32
      procedure :: regrid_scalar_3d_real64

      procedure :: regrid_vector_2d_real32
      procedure :: regrid_vector_2d_real64
      procedure :: regrid_vector_3d_real32
      procedure :: regrid_vector_3d_real64

      procedure :: transpose_regrid_scalar_2d_real32
      procedure :: transpose_regrid_scalar_3d_real32
      procedure :: transpose_regrid_vector_2d_real32
      procedure :: transpose_regrid_vector_3d_real32

      ! internal
      procedure :: do_regrid
      procedure :: create_route_handle
      procedure :: select_route_handle
      procedure :: destroy
      procedure :: destroy_route_handle

   end type EsmfRegridder

   interface EsmfRegridder
      module procedure new_EsmfRegridder
   end interface EsmfRegridder


contains


   function new_EsmfRegridder() result(regridder)
      use MAPL_BaseMod
      type (EsmfRegridder) :: regridder

      _unused_dummy(regridder)

      ! Nothing to do here
   end function new_EsmfRegridder

   logical function supports(spec, unusable, rc)
      type(RegridderSpec), intent(in) :: spec
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      _unused_dummy(unusable)

      supports = any(spec%regrid_method  == &
           [ &
           REGRID_METHOD_BILINEAR, &
           REGRID_METHOD_BILINEAR_MONOTONIC, &
           REGRID_METHOD_BILINEAR_ROTATE, &
           REGRID_METHOD_CONSERVE, &
           REGRID_METHOD_CONSERVE_MONOTONIC, &
           REGRID_METHOD_VOTE, &
           REGRID_METHOD_FRACTION, &
           REGRID_METHOD_CONSERVE_2ND, &
           REGRID_METHOD_PATCH, &
           REGRID_METHOD_NEAREST_STOD ])
!!$      supports = .true.

      _return(_success)
   end function supports


   subroutine regrid_scalar_2d_real32(this, q_in, q_out, rc)
      class (EsmfRegridder), intent(in) :: this
      real(kind=REAL32), intent(in) :: q_in(:,:)
      real(kind=REAL32), intent(out) :: q_out(:,:)
      integer, optional, intent(out) :: rc

      character(*), parameter :: Iam = 'MAPL_EsmfRegridder::regrid_scalar_2d_real32()'
      integer :: status

      type (RegridderSpec) :: spec

      real, pointer :: p_src(:,:), p_dst(:,:)
      type (ESMF_Field) :: src_field, dst_field
      logical :: HasDE

      spec = this%get_spec()

      ! TODO support other staggerings
      src_field = ESMF_FieldCreate(spec%grid_in, typekind=ESMF_TYPEKIND_R4, &
                  gridToFieldMap=[1,2],&
                  rc=status)
      HasDE = MAPL_GridHasDE(spec%grid_in,rc=status)
      _verify(status)
      if (HasDE) then
         call ESMF_FieldGet(src_field,localDE=0,farrayPtr=p_src,rc=status)
         _verify(status)
         p_src = q_in
      end if


      dst_field = ESMF_FieldCreate(spec%grid_out, typekind=ESMF_TYPEKIND_R4, &
                  gridToFieldMap=[1,2],&
                  rc=status)
      HasDE = MAPL_GridHasDE(spec%grid_out,rc=status)
      _verify(status)
      if (HasDE) then
         call ESMF_FieldGet(dst_field,localDE=0,farrayPtr=p_dst,rc=status)
         _verify(status)
         p_dst = MAPL_UNDEF
      end if

      call this%do_regrid(src_field, dst_field, rc=status)
      _verify(status)

      if (HasDE) q_out = p_dst

      call ESMF_FieldDestroy(src_field, noGarbage=.true., rc=status)
      _verify(status)
      call ESMF_FieldDestroy(dst_field, noGarbage=.true., rc=status)
      _verify(status)

      _return(ESMF_SUCCESS)

   end subroutine regrid_scalar_2d_real32

   subroutine regrid_scalar_2d_real64(this, q_in, q_out, rc)
      class (EsmfRegridder), intent(in) :: this
      real(kind=REAL64), intent(in) :: q_in(:,:)
      real(kind=REAL64), intent(out) :: q_out(:,:)
      integer, optional, intent(out) :: rc

      character(*), parameter :: Iam = 'MAPL_EsmfRegridder::regrid_scalar_2d_real32()'
      integer :: status

      type (RegridderSpec) :: spec

      real, pointer :: p_src(:,:), p_dst(:,:)
      type (ESMF_Field) :: src_field, dst_field
      logical :: HasDE

      spec = this%get_spec()

      ! TODO support other staggerings
      src_field = ESMF_FieldCreate(spec%grid_in, typekind=ESMF_TYPEKIND_R8, &
                  gridToFieldMap=[1,2],&
                  rc=status)
      HasDE = MAPL_GridHasDE(spec%grid_in,rc=status)
      _verify(status)
      if (HasDE) then
         call ESMF_FieldGet(src_field,localDE=0,farrayPtr=p_src,rc=status)
         _verify(status)
         p_src = q_in
      end if


      dst_field = ESMF_FieldCreate(spec%grid_out, typekind=ESMF_TYPEKIND_R8, &
                  gridToFieldMap=[1,2],&
                  rc=status)
      HasDE = MAPL_GridHasDE(spec%grid_out,rc=status)
      _verify(status)
      if (HasDE) then
         call ESMF_FieldGet(dst_field,localDE=0,farrayPtr=p_dst,rc=status)
         _verify(status)
         p_dst = MAPL_UNDEF
      end if

      call this%do_regrid(src_field, dst_field, rc=status)
      _verify(status)

      if (HasDE) q_out = p_dst

      call ESMF_FieldDestroy(src_field, noGarbage=.true., rc=status)
      _verify(status)
      call ESMF_FieldDestroy(dst_field, noGarbage=.true., rc=status)
      _verify(status)

      _return(ESMF_SUCCESS)

   end subroutine regrid_scalar_2d_real64

   subroutine transpose_regrid_scalar_2d_real32(this, q_in, q_out, rc)
      class (EsmfRegridder), intent(in) :: this
      real(kind=REAL32), intent(in) :: q_in(:,:)
      real(kind=REAL32), intent(out) :: q_out(:,:)
      integer, optional, intent(out) :: rc

      character(*), parameter :: Iam = 'MAPL_EsmfRegridder::transpose_regrid_scalar_2d_real32()'
      integer :: status

      type (RegridderSpec) :: spec

      real, pointer :: p_src(:,:), p_dst(:,:)
      type (ESMF_Field) :: src_field, dst_field
      logical :: HasDE

      spec = this%get_spec()
      src_field = ESMF_FieldCreate(spec%grid_out, typekind=ESMF_TYPEKIND_R4, &
                  gridToFieldMap=[1,2],&
                  rc=status)
      HasDE = MAPL_GridHasDE(spec%grid_out,rc=status)
      _verify(status)
      if (HasDE) then
         call ESMF_FieldGet(src_field,localDE=0,farrayPtr=p_src,rc=status)
         _verify(status)
         p_src = q_in
      end if

      dst_field = ESMF_FieldCreate(spec%grid_in, typekind=ESMF_TYPEKIND_R4, &
                  gridToFieldMap=[1,2],&
                  rc=status)
      HasDE = MAPL_GridHasDE(spec%grid_in,rc=status)
      _verify(status)
      if (HasDE) then
         call ESMF_FieldGet(dst_field,localDE=0,farrayPtr=p_dst,rc=status)
         _verify(status)
         p_dst = MAPL_UNDEF
      end if

      call this%do_regrid(src_field, dst_field, doTranspose=.true., rc=status)
      _verify(status)

      if (hasDE) q_out = p_dst

      call ESMF_FieldDestroy(src_field, noGarbage=.true., rc=status)
      _verify(status)
      call ESMF_FieldDestroy(dst_field, noGarbage=.true., rc=status)
      _verify(status)

      _return(ESMF_SUCCESS)

   end subroutine transpose_regrid_scalar_2d_real32

   subroutine regrid_scalar_3d_real32(this, q_in, q_out, rc)
      class (EsmfRegridder), intent(in) :: this
      real, intent(in) :: q_in(:,:,:)
      real, intent(out) :: q_out(:,:,:)
      integer, optional, intent(out) :: rc

      character(*), parameter :: Iam = 'MAPL_EsmfRegridder::regrid_scalar_3d_real32()'
      integer :: status

      type (RegridderSpec) :: spec

      real, pointer :: p_src(:,:,:), p_dst(:,:,:)
      type (ESMF_Field) :: src_field, dst_field

      integer :: kin,kout,km
      logical :: hasDE
      type(ESMF_VM) :: vm

      spec = this%get_spec()

      ! Note: tranposing km to leading index
      call ESMF_VMGetCurrent(vm,rc=status)
      _verify(status)
      HasDE = MAPL_GridHasDE(spec%grid_in,rc=status)
      _verify(status)
      if (hasDE) then
         km = size(q_in,3)
      else
         km = 0
      end if
      call ESMF_VMAllReduce(vm,sendData=km,recvData=kin,reduceflag=ESMF_REDUCE_MAX,rc=status)
      _verify(status)

      if (hasDE) then
          _assert(kin == size(q_in,3),'inconsistent array shape')
      end if
      src_field = ESMF_FieldCreate(spec%grid_in, typekind=ESMF_TYPEKIND_R4, &
                  gridToFieldMap=[2,3],ungriddedLBound=[1],ungriddedUBound=[kin],&
                  rc=status)
      _verify(status)
      if (HasDE) then
         call ESMF_FieldGet(src_field,localDE=0,farrayPtr=p_src,rc=status)
         _verify(status)
         p_src = reshape(q_in,shape(p_src), order=[2,3,1])
      end if


      HasDE = MAPL_GridHasDE(spec%grid_out,rc=status)
      _verify(status)
      if (hasDE) then
         km = size(q_out,3)
      else
         km = 0
      end if
      call ESMF_VMAllReduce(vm,sendData=km,recvData=kout,reduceflag=ESMF_REDUCE_MAX,rc=status)
      _verify(status)
      if (hasDE) then
          _assert(km == size(q_out,3),'inconsistent array shape')
      end if
      dst_field = ESMF_FieldCreate(spec%grid_out, typekind=ESMF_TYPEKIND_R4, &
                  gridToFieldMap=[2,3],ungriddedLBound=[1],ungriddedUBound=[kout],&
                  rc=status)
      _verify(status)
      if (HasDE) then
         call ESMF_FieldGet(dst_field,localDE=0,farrayPtr=p_dst,rc=status)
         _verify(status)
         p_dst = MAPL_UNDEF
      end if

      call this%do_regrid(src_field, dst_field, rc=status)
      _verify(status)

      if (HasDE) q_out = reshape(p_dst, shape(q_out), order=[3,1,2])

      call ESMF_FieldDestroy(src_field, noGarbage=.true., rc=status)
      _verify(status)
      call ESMF_FieldDestroy(dst_field, noGarbage=.true., rc=status)
      _verify(status)

      _return(ESMF_SUCCESS)

   end subroutine regrid_scalar_3d_real32

   subroutine regrid_scalar_3d_real64(this, q_in, q_out, rc)
      class (EsmfRegridder), intent(in) :: this
      real(real64), intent(in) :: q_in(:,:,:)
      real(real64), intent(out) :: q_out(:,:,:)
      integer, optional, intent(out) :: rc

      character(*), parameter :: Iam = 'MAPL_EsmfRegridder::regrid_scalar_3d_real32()'
      integer :: status

      type (RegridderSpec) :: spec

      real(real64), pointer :: p_src(:,:,:), p_dst(:,:,:)
      type (ESMF_Field) :: src_field, dst_field

      integer :: kin,kout,km
      logical :: hasDE
      type(ESMF_VM) :: vm

      spec = this%get_spec()

      ! Note: tranposing km to leading index
      call ESMF_VMGetCurrent(vm,rc=status)
      _verify(status)
      HasDE = MAPL_GridHasDE(spec%grid_in,rc=status)
      _verify(status)
      if (hasDE) then
         km = size(q_in,3)
      else
         km = 0
      end if
      call ESMF_VMAllReduce(vm,sendData=km,recvData=kin,reduceflag=ESMF_REDUCE_MAX,rc=status)
      _verify(status)

      if (hasDE) then
          _assert(kin == size(q_in,3),'inconsistent array shape')
      end if
      src_field = ESMF_FieldCreate(spec%grid_in, typekind=ESMF_TYPEKIND_R8, &
                  gridToFieldMap=[2,3],ungriddedLBound=[1],ungriddedUBound=[kin],&
                  rc=status)
      _verify(status)
      if (HasDE) then
         call ESMF_FieldGet(src_field,localDE=0,farrayPtr=p_src,rc=status)
         _verify(status)
         p_src = reshape(q_in,shape(p_src), order=[2,3,1])
      end if


      HasDE = MAPL_GridHasDE(spec%grid_out,rc=status)
      _verify(status)
      if (hasDE) then
         km = size(q_out,3)
      else
         km = 0
      end if
      call ESMF_VMAllReduce(vm,sendData=km,recvData=kout,reduceflag=ESMF_REDUCE_MAX,rc=status)
      _verify(status)
      if (hasDE) then
          _assert(km == size(q_out,3),'inconsistent array shape')
      end if
      dst_field = ESMF_FieldCreate(spec%grid_out, typekind=ESMF_TYPEKIND_R8, &
                  gridToFieldMap=[2,3],ungriddedLBound=[1],ungriddedUBound=[kout],&
                  rc=status)
      _verify(status)
      if (HasDE) then
         call ESMF_FieldGet(dst_field,localDE=0,farrayPtr=p_dst,rc=status)
         _verify(status)
         p_dst = MAPL_UNDEF
      end if

      call this%do_regrid(src_field, dst_field, rc=status)
      _verify(status)

      if (HasDE) q_out = reshape(p_dst, shape(q_out), order=[3,1,2])

      call ESMF_FieldDestroy(src_field, noGarbage=.true., rc=status)
      _verify(status)
      call ESMF_FieldDestroy(dst_field, noGarbage=.true., rc=status)
      _verify(status)

      _return(ESMF_SUCCESS)

   end subroutine regrid_scalar_3d_real64

   subroutine transpose_regrid_scalar_3d_real32(this, q_in, q_out, rc)
      class (EsmfRegridder), intent(in) :: this
      real, intent(in) :: q_in(:,:,:)
      real, intent(out) :: q_out(:,:,:)
      integer, optional, intent(out) :: rc

      character(*), parameter :: Iam = 'MAPL_EsmfRegridder::transpose_regrid_scalar_3d_real32()'
      integer :: status

      type (RegridderSpec) :: spec

      real, pointer :: p_src(:,:,:), p_dst(:,:,:)
      type (ESMF_Field) :: src_field, dst_field

      integer :: km,kin,kout
      logical :: hasDE
      type(ESMF_VM) :: vm

      call ESMF_VMGetCurrent(vm,rc=status)
      _verify(status)
      spec = this%get_spec()

      km = size(q_in,3)
      _assert(km == size(q_out,3),'inconsistent array shape')

      HasDE = MAPL_GridHasDE(spec%grid_out,rc=status)
      _verify(status)
      if (hasDE) then
         km = size(q_in,3)
      else
         km = 0
      end if
      call ESMF_VMAllReduce(vm,sendData=km,recvData=kin,reduceFlag=ESMF_REDUCE_MAX,rc=status)
      _verify(status)
      if (hasDE) then
          _assert(kin == size(q_in,3),'inconsistent array shape')
      end if
      src_field = ESMF_FieldCreate(spec%grid_out, typekind=ESMF_TYPEKIND_R4, &
                  gridToFieldMap=[2,3],ungriddedLBound=[1],ungriddedUBound=[kin],&
                  rc=status)
      if (HasDE) then
         call ESMF_FieldGet(src_field,localDE=0,farrayPtr=p_src,rc=status)
         _verify(status)
         p_src = reshape(q_in,shape(p_src), order=[2,3,1])
      end if

      HasDE = MAPL_GridHasDE(spec%grid_in,rc=status)
      _verify(status)
      if (hasDE) then
         km = size(q_out,3)
      else
         km = 0
      end if
      call ESMF_VMAllReduce(vm,sendData=km,recvData=kout,reduceFlag=ESMF_REDUCE_MAX,rc=status)
      _verify(status)
      if (hasDE) then
          _assert(kout == size(q_out,3),'inconsistent array shape')
      end if
      dst_field = ESMF_FieldCreate(spec%grid_in, typekind=ESMF_TYPEKIND_R4, &
                  gridToFieldMap=[2,3],ungriddedLBound=[1],ungriddedUBound=[kout],&
                  rc=status)
      if (HasDE) then
         call ESMF_FieldGet(dst_field,localDE=0,farrayPtr=p_dst,rc=status)
         _verify(status)
         p_dst = MAPL_UNDEF
      end if

      call this%do_regrid(src_field, dst_field, doTranspose=.true., rc=status)
      _verify(status)

      if (HasDE) q_out = reshape(p_dst, shape(q_out), order=[3,1,2])

      call ESMF_FieldDestroy(src_field, noGarbage=.true., rc=status)
      _verify(status)
      call ESMF_FieldDestroy(dst_field, noGarbage=.true., rc=status)
      _verify(status)

      _return(ESMF_SUCCESS)

   end subroutine transpose_regrid_scalar_3d_real32


   subroutine regrid_vector_2d_real32(this, u_in, v_in, u_out, v_out, rotate, rc)
      class (EsmfRegridder), intent(in) :: this
      real, intent(in) :: u_in(:,:)
      real, intent(in) :: v_in(:,:)
      real, intent(out) :: u_out(:,:)
      real, intent(out) :: v_out(:,:)
      logical, optional, intent(in) :: rotate
      integer, optional, intent(out) :: rc

      character(*), parameter :: Iam = 'MAPL_EsmfRegridder::regrid_vector_2d_real32()'
      integer :: status

      type (RegridderSpec) :: spec
      class (AbstractGridFactory), pointer :: factory
      character(len=ESMF_MAXSTR) :: grid_axis_in, grid_axis_out

      real, pointer :: p_src(:,:,:,:), p_dst(:,:,:,:)
      type (ESMF_Field) :: src_field, dst_field

      integer :: im_src, jm_src
      integer :: im_dst, jm_dst
      logical :: HasDE

      spec = this%get_spec()

      im_src = size(u_in,1)
      jm_src = size(u_in,2)
      _assert(im_src == size(v_in,1),'inconsistent array shape')
      _assert(jm_src == size(v_in,2),'inconsistent array shape')

      im_dst = size(u_out,1)
      jm_dst = size(u_out,2)
      _assert(im_dst == size(v_out,1),'inconsistent array shape')
      _assert(jm_dst == size(v_out,2),'inconsistent array shape')

      grid_axis_in = 'north-south'
      grid_axis_out = 'north-south'
      if (present(rotate)) then
         if (rotate) then
            grid_axis_in = 'xyz'
            grid_axis_out = 'grid'
         end if
      end if

      factory => grid_manager%get_factory(spec%grid_in,rc=status)
      _verify(status)

      ! TODO support other staggerings
      src_field = ESMF_FieldCreate(spec%grid_in, typekind=ESMF_TYPEKIND_R4, &
           & gridToFieldMap=[3,4], UngriddedLBound=[1,1], ungriddedUBound=[3,1], &
           & rc=status)
      _verify(status)
      hasDE = MAPL_GridHasDE(spec%grid_in,rc=status)
      _verify(status)
      if (hasDE) then
         call ESMF_FieldGet(src_field,localDE=0,farrayPtr=p_src,rc=status)
         _verify(status)
         call factory%spherical_to_cartesian(u_in, v_in, p_src, grid_axis_in, rc=status)
         _verify(status)
      end if


      dst_field = ESMF_FieldCreate(spec%grid_out, typekind=ESMF_TYPEKIND_R4, &
           & gridToFieldMap=[3,4], UngriddedLBound=[1,1], ungriddedUBound=[3,1], &
           & rc=status)
      _verify(status)
      hasDE = MAPL_GridHasDE(spec%grid_out,rc=status)
      _verify(status)
      if (hasDE) then
         call ESMF_FieldGet(dst_field,localDE=0,farrayPtr=p_dst,rc=status)
         _verify(status)
         p_dst = MAPL_UNDEF
      end if

      call this%do_regrid(src_field, dst_field, rc=status)
      _verify(status)

      factory => grid_manager%get_factory(spec%grid_out,rc=status)
      _verify(status)
      if (hasDE) then
         call factory%cartesian_to_spherical(p_dst, u_out, v_out, grid_axis_out, rc=status)
         _verify(status)
      end if

      call ESMF_FieldDestroy(src_field, noGarbage=.true., rc=status)
      _verify(status)
      call ESMF_FieldDestroy(dst_field, noGarbage=.true., rc=status)
      _verify(status)

      _return(ESMF_SUCCESS)

   end subroutine regrid_vector_2d_real32


   subroutine regrid_vector_2d_real64(this, u_in, v_in, u_out, v_out, rotate, rc)
     class (EsmfRegridder), intent(in) :: this
     real(real64), intent(in) :: u_in(:,:)
     real(real64), intent(in) :: v_in(:,:)
     real(real64), intent(out) :: u_out(:,:)
     real(real64), intent(out) :: v_out(:,:)
     logical, optional, intent(in) :: rotate
     integer, optional, intent(out) :: rc

     character(*), parameter :: Iam = 'MAPL_EsmfRegridder::regrid_vector_2d_real64()'
     integer :: status

     type (RegridderSpec) :: spec
     class (AbstractGridFactory), pointer :: factory

     real(real64), pointer :: p_src(:,:,:,:), p_dst(:,:,:,:)
     type (ESMF_Field) :: src_field, dst_field
     character(len=ESMF_MAXSTR) :: grid_axis_in, grid_axis_out

     integer :: im_src, jm_src
     integer :: im_dst, jm_dst
     logical :: HasDE

     spec = this%get_spec()

     im_src = size(u_in,1)
     jm_src = size(u_in,2)
     _assert(im_src == size(v_in,1),'inconsistent array shape')
     _assert(jm_src == size(v_in,2),'inconsistent array shape')

     im_dst = size(u_out,1)
     jm_dst = size(u_out,2)
     _assert(im_dst == size(v_out,1),'inconsistent array shape')
     _assert(jm_dst == size(v_out,2),'inconsistent array shape')

      grid_axis_in = 'north-south'
      grid_axis_out = 'north-south'
      if (present(rotate)) then
         if (rotate) then
            grid_axis_in = 'xyz'
            grid_axis_out = 'grid'
         end if
      end if

     factory => grid_manager%get_factory(spec%grid_in,rc=status)
     _verify(status)

     ! TODO support other staggerings
     src_field = ESMF_FieldCreate(spec%grid_in, typekind=ESMF_TYPEKIND_R8, &
          & gridToFieldMap=[3,4], UngriddedLBound=[1,1], ungriddedUBound=[3,1], &
          & rc=status)
     _verify(status)
     hasDE = MAPL_GridHasDE(spec%grid_in,rc=status)
     _verify(status)
     if (hasDE) then
        call ESMF_FieldGet(src_field,localDE=0,farrayPtr=p_src,rc=status)
        _verify(status)
        call factory%spherical_to_cartesian(u_in, v_in, p_src, grid_axis_in, rc=status)
        _verify(status)
     end if


     dst_field = ESMF_FieldCreate(spec%grid_out, typekind=ESMF_TYPEKIND_R8, &
          & gridToFieldMap=[3,4], UngriddedLBound=[1,1], ungriddedUBound=[3,1], &
          & rc=status)
     _verify(status)
     hasDE = MAPL_GridHasDE(spec%grid_out,rc=status)
     _verify(status)
     if (hasDE) then
        call ESMF_FieldGet(dst_field,localDE=0,farrayPtr=p_dst,rc=status)
        _verify(status)
        p_dst = MAPL_UNDEF
     end if

     call this%do_regrid(src_field, dst_field, rc=status)
     _verify(status)

     factory => grid_manager%get_factory(spec%grid_out,rc=status)
     _verify(status)
     if (hasDE) then
        call factory%cartesian_to_spherical(p_dst, u_out, v_out, grid_axis_out, rc=status)
        _verify(status)
     end if

     call ESMF_FieldDestroy(src_field, noGarbage=.true., rc=status)
     _verify(status)
     call ESMF_FieldDestroy(dst_field, noGarbage=.true., rc=status)
     _verify(status)

     _return(ESMF_SUCCESS)

   end subroutine regrid_vector_2d_real64


   subroutine transpose_regrid_vector_2d_real32(this, u_in, v_in, u_out, v_out, rotate, rc)
      class (EsmfRegridder), intent(in) :: this
      real, intent(in) :: u_in(:,:)
      real, intent(in) :: v_in(:,:)
      real, intent(out) :: u_out(:,:)
      real, intent(out) :: v_out(:,:)
      logical, optional, intent(in) :: rotate
      integer, optional, intent(out) :: rc

      character(*), parameter :: Iam = 'MAPL_EsmfRegridder::transpose_regrid_vector_2d_real32()'
      integer :: status

      type (RegridderSpec) :: spec
      class (AbstractGridFactory), pointer :: factory

      real, pointer :: p_src(:,:,:,:), p_dst(:,:,:,:)
      type (ESMF_Field) :: src_field, dst_field
      character(len=ESMF_MAXSTR) :: grid_axis_in, grid_axis_out

      integer :: im_src, jm_src
      integer :: im_dst, jm_dst
      logical :: HasDE

      spec = this%get_spec()

      im_src = size(u_in,1)
      jm_src = size(u_in,2)
      _assert(im_src == size(v_in,1),'inconsistent array shape')
      _assert(jm_src == size(v_in,2),'inconsistent array shape')

      im_dst = size(u_out,1)
      jm_dst = size(u_out,2)
      _assert(im_dst == size(v_out,1),'inconsistent array shape')
      _assert(jm_dst == size(v_out,2),'inconsistent array shape')

      grid_axis_in = 'north-south'
      grid_axis_out = 'north-south'
      if (present(rotate)) then
         if (rotate) then
            grid_axis_in = 'grid'
            grid_axis_out = 'xyz'
         end if
      end if

      factory => grid_manager%get_factory(spec%grid_out,rc=status)
      _verify(status)
      hasDE = MAPL_GridHasDE(spec%grid_out,rc=status)
      _verify(status)
      src_field = ESMF_FieldCreate(spec%grid_out, typekind = ESMF_TYPEKIND_R4, &
           & gridToFieldMap=[3,4],ungriddedLBound=[1,1],ungriddedUBound=[3,1],rc=status)
      _verify(status)
      if (hasDE) then
         call ESMF_FieldGet(src_field,localDE=0,farrayPtr=p_src,rc=status)
         _verify(status)
         call factory%spherical_to_cartesian(u_in, v_in, p_src, grid_axis_in, rc=status)
         _verify(status)
      end if


      hasDE = MAPL_GridHasDE(spec%grid_in,rc=status)
      _verify(status)
      dst_field = ESMF_FieldCreate(spec%grid_in, typekind = ESMF_TYPEKIND_R4, &
           & gridToFieldMap=[3,4],ungriddedLBound=[1,1],ungriddedUBound=[3,1],rc=status)
      _verify(status)
      if (hasDE) then
         call ESMF_FieldGet(dst_field,localDE=0,farrayPtr=p_dst,rc=status)
         _verify(status)
         p_dst = MAPL_UNDEF
      end if

      call this%do_regrid(src_field, dst_field, doTranspose=.true., rc=status)
      _verify(status)

      factory => grid_manager%get_factory(spec%grid_in,rc=status)
      _verify(status)
      if (hasDE) then
         call factory%cartesian_to_spherical(p_dst, u_out, v_out, grid_axis_out, rc=status)
         _verify(status)
      end if

      call ESMF_FieldDestroy(src_field, noGarbage=.true., rc=status)
      _verify(status)
      call ESMF_FieldDestroy(dst_field, noGarbage=.true., rc=status)
      _verify(status)

      _return(ESMF_SUCCESS)

   end subroutine transpose_regrid_vector_2d_real32


   subroutine regrid_vector_3d_real32(this, u_in, v_in, u_out, v_out, rotate, rc)
      class (EsmfRegridder), intent(in) :: this
      real, intent(in) :: u_in(:,:,:)
      real, intent(in) :: v_in(:,:,:)
      real, intent(out) :: u_out(:,:,:)
      real, intent(out) :: v_out(:,:,:)
      logical, optional, intent(in) :: rotate
      integer, optional, intent(out) :: rc

      character(*), parameter :: Iam = 'MAPL_EsmfRegridder::regrid_vector_3d_real32()'
      integer :: status

      type (RegridderSpec) :: spec
      class (AbstractGridFactory), pointer :: factory

      real, pointer :: p_src(:,:,:,:), p_dst(:,:,:,:)
      type (ESMF_Field) :: src_field, dst_field
      character(len=ESMF_MAXSTR) :: grid_axis_in, grid_axis_out

      integer :: km,kin,kout
      integer :: im_src, jm_src
      integer :: im_dst, jm_dst
      logical :: hasDE
      type(ESMF_VM) :: vm

      call ESMF_VMGetCurrent(vm,rc=status)
      _verify(status)
      spec = this%get_spec()

      km = size(u_in,3)
      _assert(km == size(v_in,3),'inconsistent array shape')
      if (size(u_out) /= 0) then
         _assert(km == size(u_out,3),'inconsistent array shape')
         _assert(km == size(v_out,3),'inconsistent array shape')
      end if

      im_src = size(u_in,1)
      jm_src = size(u_in,2)
      _assert(im_src == size(v_in,1),'inconsistent array shape')
      _assert(jm_src == size(v_in,2),'inconsistent array shape')

      im_dst = size(u_out,1)
      jm_dst = size(u_out,2)
      _assert(im_dst == size(v_out,1),'inconsistent array shape')
      _assert(jm_dst == size(v_out,2),'inconsistent array shape')

      grid_axis_in = 'north-south'
      grid_axis_out = 'north-south'
      if (present(rotate)) then
         if (rotate) then
            grid_axis_in = 'xyz'
            grid_axis_out = 'grid'
         end if
      end if

      factory => grid_manager%get_factory(spec%grid_in,rc=status)
      _verify(status)
      hasDE = MAPL_GridHasDE(spec%grid_in,rc=status)
      _verify(status)
      if (hasDE) then
         km = size(u_in,3)
      else
         km = 0
      end if
      call ESMF_VMAllReduce(vm,sendData=km,recvData=kin,reduceFlag=ESMF_REDUCE_MAX,rc=status)
      _verify(status)
      src_field = ESMF_FieldCreate(spec%grid_in, typekind = ESMF_TYPEKIND_R4, &
           & gridToFieldMap=[3,4],ungriddedLBound=[1,1],ungriddedUBound=[3,kin],rc=status)
      _verify(status)
      if (hasDE) then
         call ESMF_FieldGet(src_field,localDE=0,farrayPtr=p_src,rc=status)
         _verify(status)
         call factory%spherical_to_cartesian(u_in, v_in, p_src, grid_axis_in, rc=status)
         _verify(status)
      end if

      hasDE = MAPL_GridHasDE(spec%grid_out,rc=status)
      _verify(status)
      hasDE = MAPL_GridHasDE(spec%grid_out,rc=status)
      _verify(status)
      if (hasDE) then
         km = size(u_out,3)
      else
         km = 0
      end if
      call ESMF_VMAllReduce(vm,sendData=km,recvData=kout,reduceFlag=ESMF_REDUCE_MAX,rc=status)
      _verify(status)
      dst_field = ESMF_FieldCreate(spec%grid_out, typekind = ESMF_TYPEKIND_R4, &
           & gridToFieldMap=[3,4],ungriddedLBound=[1,1],ungriddedUBound=[3,kout],rc=status)
      _verify(status)
      if (hasDE) then
         call ESMF_FieldGet(dst_field,localDE=0,farrayPtr=p_dst,rc=status)
         _verify(status)
         p_dst = MAPL_UNDEF
      end if

      call this%do_regrid(src_field, dst_field, rc=status)
      _verify(status)

      factory => grid_manager%get_factory(spec%grid_out,rc=status)
      _verify(status)
      if (hasDE) then
         call factory%cartesian_to_spherical(p_dst, u_out, v_out, grid_axis_out,rc=status)
         _verify(status)
      end if

      call ESMF_FieldDestroy(src_field, noGarbage=.true., rc=status)
      _verify(status)
      call ESMF_FieldDestroy(dst_field, noGarbage=.true., rc=status)
      _verify(status)

      _return(ESMF_SUCCESS)

   end subroutine regrid_vector_3d_real32


   subroutine regrid_vector_3d_real64(this, u_in, v_in, u_out, v_out, rc)
     class (EsmfRegridder), intent(in) :: this
     real(real64), intent(in) :: u_in(:,:,:)
     real(real64), intent(in) :: v_in(:,:,:)
     real(real64), intent(out) :: u_out(:,:,:)
     real(real64), intent(out) :: v_out(:,:,:)
     integer, optional, intent(out) :: rc

     character(*), parameter :: Iam = 'MAPL_EsmfRegridder::regrid_vector_3d_real64()'
     integer :: status

     type (RegridderSpec) :: spec
     class (AbstractGridFactory), pointer :: factory

     real(real64), pointer :: p_src(:,:,:,:), p_dst(:,:,:,:)
     type (ESMF_Field) :: src_field, dst_field
     character(len=ESMF_MAXSTR) :: grid_axis_in, grid_axis_out

     integer :: km,kin,kout
     integer :: im_src, jm_src
     integer :: im_dst, jm_dst
     logical :: hasDE
     type(ESMF_VM) :: vm

     call ESMF_VMGetCurrent(vm,rc=status)
     _verify(status)
     spec = this%get_spec()

     km = size(u_in,3)
     _assert(km == size(v_in,3),'inconsistent array shape')
      if (size(u_out) /= 0) then
         _assert(km == size(u_out,3),'inconsistent array shape')
         _assert(km == size(v_out,3),'inconsistent array shape')
      end if

     im_src = size(u_in,1)
     jm_src = size(u_in,2)
     _assert(im_src == size(v_in,1),'inconsistent array shape')
     _assert(jm_src == size(v_in,2),'inconsistent array shape')

     im_dst = size(u_out,1)
     jm_dst = size(u_out,2)
     _assert(im_dst == size(v_out,1),'inconsistent array shape')
     _assert(jm_dst == size(v_out,2),'inconsistent array shape')

     grid_axis_in = 'north-south'
     grid_axis_out = 'north-south'

     factory => grid_manager%get_factory(spec%grid_in,rc=status)
     _verify(status)
     hasDE = MAPL_GridHasDE(spec%grid_in,rc=status)
     _verify(status)
     if (hasDE) then
        km = size(u_in,3)
     else
        km = 0
     end if
     call ESMF_VMAllReduce(vm,sendData=km,recvData=kin,reduceFlag=ESMF_REDUCE_MAX,rc=status)
     _verify(status)
     src_field = ESMF_FieldCreate(spec%grid_in, typekind = ESMF_TYPEKIND_R8, &
          & gridToFieldMap=[3,4],ungriddedLBound=[1,1],ungriddedUBound=[3,kin],rc=status)
     _verify(status)
     if (hasDE) then
        call ESMF_FieldGet(src_field,localDE=0,farrayPtr=p_src,rc=status)
        _verify(status)
        call factory%spherical_to_cartesian(u_in, v_in, p_src, grid_axis_in, rc=status)
        _verify(status)
     end if

     hasDE = MAPL_GridHasDE(spec%grid_out,rc=status)
     _verify(status)
     hasDE = MAPL_GridHasDE(spec%grid_out,rc=status)
     _verify(status)
     if (hasDE) then
        km = size(u_out,3)
     else
        km = 0
     end if
     call ESMF_VMAllReduce(vm,sendData=km,recvData=kout,reduceFlag=ESMF_REDUCE_MAX,rc=status)
     _verify(status)
     dst_field = ESMF_FieldCreate(spec%grid_out, typekind = ESMF_TYPEKIND_R8, &
          & gridToFieldMap=[3,4],ungriddedLBound=[1,1],ungriddedUBound=[3,kout],rc=status)
     _verify(status)
     if (hasDE) then
        call ESMF_FieldGet(dst_field,localDE=0,farrayPtr=p_dst,rc=status)
        _verify(status)
        p_dst = MAPL_UNDEF
     end if

     call this%do_regrid(src_field, dst_field, rc=status)
     _verify(status)

     factory => grid_manager%get_factory(spec%grid_out,rc=status)
     _verify(status)
     if (hasDE) then
        call factory%cartesian_to_spherical(p_dst, u_out, v_out, grid_axis_out,rc=status)
        _verify(status)
     end if

     call ESMF_FieldDestroy(src_field, noGarbage=.true., rc=status)
     _verify(status)
     call ESMF_FieldDestroy(dst_field, noGarbage=.true., rc=status)
     _verify(status)

     _return(ESMF_SUCCESS)

   end subroutine regrid_vector_3d_real64


   subroutine transpose_regrid_vector_3d_real32(this, u_in, v_in, u_out, v_out, rotate, rc)
      class (EsmfRegridder), intent(in) :: this
      real, intent(in) :: u_in(:,:,:)
      real, intent(in) :: v_in(:,:,:)
      real, intent(out) :: u_out(:,:,:)
      real, intent(out) :: v_out(:,:,:)
      logical, optional, intent(in) :: rotate
      integer, optional, intent(out) :: rc

      character(*), parameter :: Iam = 'MAPL_EsmfRegridder::transpose_regrid_vector_3d_real32()'
      integer :: status

      type (RegridderSpec) :: spec
      class (AbstractGridFactory), pointer :: factory

      real, pointer :: p_src(:,:,:,:), p_dst(:,:,:,:)
      type (ESMF_Field) :: src_field, dst_field

      integer :: km,kin,kout
      integer :: im_src, jm_src
      integer :: im_dst, jm_dst
      character(len=ESMF_MAXSTR) :: grid_axis_in, grid_axis_out
      logical :: hasDE
      type(ESMF_VM) :: vm

      spec = this%get_spec()
      call ESMF_VMGetCurrent(vm,rc=status)
      _verify(status)

      km = size(u_in,3)
      _assert(km == size(v_in,3),'inconsistent array shape')
      if (size(u_out) /= 0) then
         _assert(km == size(u_out,3),'inconsistent array shape')
         _assert(km == size(v_out,3),'inconsistent array shape')
      end if

      im_src = size(u_in,1)
      jm_src = size(u_in,2)
      _assert(im_src == size(v_in,1),'inconsistent array shape')
      _assert(jm_src == size(v_in,2),'inconsistent array shape')

      im_dst = size(u_out,1)
      jm_dst = size(u_out,2)
      _assert(im_dst == size(v_out,1),'inconsistent array shape')
      _assert(jm_dst == size(v_out,2),'inconsistent array shape')

      allocate(p_src(3,km,im_src,jm_src))
      allocate(p_dst(3,km,im_dst,jm_dst))

      grid_axis_in = 'north-south'
      grid_axis_out = 'north-south'
      if (present(rotate)) then
         if (rotate) then
            grid_axis_in = 'grid'
            grid_axis_out = 'xyz'
         end if
      end if

      factory => grid_manager%get_factory(spec%grid_out,rc=status)
      _verify(status)
      hasDE = MAPL_GridHasDE(spec%grid_out,rc=status)
      _verify(status)
      if (hasDE) then
         km = size(u_in,3)
      else
         km = 0
      end if
      call ESMF_VMAllReduce(vm,sendData=km,recvData=kin,reduceFlag=ESMF_REDUCE_MAX,rc=status)
      _verify(status)
      src_field = ESMF_FieldCreate(spec%grid_out, typekind = ESMF_TYPEKIND_R4, &
           & gridToFieldMap=[3,4],ungriddedLBound=[1,1],ungriddedUBound=[3,kin],rc=status)
      _verify(status)
      if (hasDE) then
         call ESMF_FieldGet(src_field,localDE=0,farrayPtr=p_src,rc=status)
         _verify(status)
         call factory%spherical_to_cartesian(u_in, v_in, p_src, grid_axis_in, rc=status)
         _verify(status)
      end if

      hasDE = MAPL_GridHasDE(spec%grid_in,rc=status)
      _verify(status)
      if (hasDE) then
         km = size(u_out,3)
      else
         km = 0
      end if
      call ESMF_VMAllReduce(vm,sendData=km,recvData=kout,reduceFlag=ESMF_REDUCE_MAX,rc=status)
      _verify(status)
      dst_field = ESMF_FieldCreate(spec%grid_in, typekind = ESMF_TYPEKIND_R4, &
           & gridToFieldMap=[3,4],ungriddedLBound=[1,1],ungriddedUBound=[3,kout],rc=status)
      _verify(status)
      if (hasDE) then
         call ESMF_FieldGet(dst_field,localDE=0,farrayPtr=p_dst,rc=status)
         _verify(status)
         p_dst = MAPL_UNDEF
      end if

      call this%do_regrid(src_field, dst_field, doTranspose=.true.,rc=status)
      _verify(status)

      factory => grid_manager%get_factory(spec%grid_in,rc=status)
      _verify(status)
      if (hasDE) then
         call factory%cartesian_to_spherical(p_dst, u_out, v_out, grid_axis_out,rc=status)
         _verify(status)
      end if

      call ESMF_FieldDestroy(src_field, noGarbage=.true., rc=status)
      _verify(status)
      call ESMF_FieldDestroy(dst_field, noGarbage=.true., rc=status)
      _verify(status)

      deallocate(p_src)
      deallocate(p_dst)

      _return(ESMF_SUCCESS)

   end subroutine transpose_regrid_vector_3d_real32


   subroutine simpleDynMaskProcV(dynamicMaskList, dynamicSrcMaskValue, &
        dynamicDstMaskValue, rc)
      type(ESMF_DynamicMaskElementR4R8R4V), pointer              :: dynamicMaskList(:)
      real(ESMF_KIND_R4),            intent(in), optional :: dynamicSrcMaskValue
      real(ESMF_KIND_R4),            intent(in), optional :: dynamicDstMaskValue
      integer,                       intent(out)          :: rc
      integer :: i, j, k, n
      real(ESMF_KIND_R4), allocatable  :: renorm(:)

      _unused_dummy(dynamicDstMaskValue)

      if (associated(dynamicMaskList)) then
         n = size(dynamicMaskList(1)%srcElement(1)%ptr)
         allocate(renorm(n))

         do i=1, size(dynamicMaskList)
            dynamicMaskList(i)%dstElement = 0.0 ! set to zero

            renorm = 0.d0 ! reset
            do j=1, size(dynamicMaskList(i)%factor)
               do k = 1, size(dynamicMaskList(i)%srcElement(j)%ptr)
                  if (.not. &
                       match(dynamicSrcMaskValue,dynamicMaskList(i)%srcElement(j)%ptr(k))) then
                     dynamicMaskList(i)%dstElement(k) = dynamicMaskList(i)%dstElement(k) &
                          + dynamicMaskList(i)%factor(j) &
                          * dynamicMaskList(i)%srcElement(j)%ptr(k)
                     renorm(k) = renorm(k) + dynamicMaskList(i)%factor(j)
                  endif
               end do
            end do
            where (renorm > 0.d0)
               dynamicMaskList(i)%dstElement = dynamicMaskList(i)%dstElement / renorm
            elsewhere
               dynamicMaskList(i)%dstElement = dynamicSrcMaskValue
            end where
         enddo
      endif
      ! return successfully
      rc = ESMF_SUCCESS
   end subroutine simpleDynMaskProcV

   subroutine monotonicDynMaskProcV(dynamicMaskList, dynamicSrcMaskValue, &
        dynamicDstMaskValue, rc)
      type(ESMF_DynamicMaskElementR4R8R4V), pointer              :: dynamicMaskList(:)
      real(ESMF_KIND_R4),            intent(in), optional :: dynamicSrcMaskValue
      real(ESMF_KIND_R4),            intent(in), optional :: dynamicDstMaskValue
      integer,                       intent(out)          :: rc
      integer :: i, j, k, n
      real(ESMF_KIND_R4), allocatable  :: renorm(:),max_input(:),min_input(:)

      _unused_dummy(dynamicDstMaskValue)

      if (associated(dynamicMaskList)) then
         n = size(dynamicMaskList(1)%srcElement(1)%ptr)
         allocate(renorm(n),max_input(n),min_input(n))

         do i=1, size(dynamicMaskList)
            dynamicMaskList(i)%dstElement = 0.0 ! set to zero

            renorm = 0.d0 ! reset
            max_input = -huge(0.0)
            min_input = huge(0.0)
            do j=1, size(dynamicMaskList(i)%factor)
               do k = 1, size(dynamicMaskList(i)%srcElement(j)%ptr)
                  if (.not. &
                       match(dynamicSrcMaskValue,dynamicMaskList(i)%srcElement(j)%ptr(k))) then
                     dynamicMaskList(i)%dstElement(k) = dynamicMaskList(i)%dstElement(k) &
                          + dynamicMaskList(i)%factor(j) &
                          * dynamicMaskList(i)%srcElement(j)%ptr(k)
                     renorm(k) = renorm(k) + dynamicMaskList(i)%factor(j)
                     if (dynamicMaskList(i)%srcElement(j)%ptr(k) > max_input(k)) max_input(k) = dynamicMaskList(i)%srcElement(j)%ptr(k)
                     if (dynamicMaskList(i)%srcElement(j)%ptr(k) < min_input(k)) min_input(k) = dynamicMaskList(i)%srcElement(j)%ptr(k)
                  endif
               end do
            end do
            where (renorm > 0.d0)
               dynamicMaskList(i)%dstElement = dynamicMaskList(i)%dstElement / renorm
            elsewhere
               dynamicMaskList(i)%dstElement = dynamicSrcMaskValue
            end where
            where (renorm > 0.d0 .and. dynamicMaskList(i)%dstElement > max_input)
               dynamicMaskList(i)%dstElement = max_input
            end where
            where (renorm > 0.d0 .and. dynamicMaskList(i)%dstElement < min_input)
               dynamicMaskList(i)%dstElement = min_input
            end where
         enddo
      endif
      ! return successfully
      rc = ESMF_SUCCESS
   end subroutine monotonicDynMaskProcV


   logical function match(missing,b)
      real(kind=REAL32), intent(in) :: missing, b
      match = (missing==b)
   end function match



   subroutine voteDynMaskProcV(dynamicMaskList, dynamicSrcMaskValue, &
        dynamicDstMaskValue, rc)
      type(ESMF_DynamicMaskElementR4R8R4V), pointer              :: dynamicMaskList(:)
      real(ESMF_KIND_R4),            intent(in), optional :: dynamicSrcMaskValue
      real(ESMF_KIND_R4),            intent(in), optional :: dynamicDstMaskValue
      integer,                       intent(out)          :: rc
      integer :: i, j, k, n
      real(ESMF_KIND_R4), allocatable  :: renorm(:)

      _unused_dummy(dynamicDstMaskValue)

      if (associated(dynamicMaskList)) then
         n = size(dynamicMaskList(1)%srcElement(1)%ptr)
         allocate(renorm(n))

         do i=1, size(dynamicMaskList)
            dynamicMaskList(i)%dstElement = 0.0 ! set to zero

            renorm = 0.d0 ! reset
            do j=1, size(dynamicMaskList(i)%factor)
               do k = 1, size(dynamicMaskList(i)%srcElement(j)%ptr)
                  if (.not. &
                       match(dynamicSrcMaskValue,dynamicMaskList(i)%srcElement(j)%ptr(k))) then
                     if (dynamicMaskList(i)%factor(j) > renorm(k)) then
                        renorm(k) = dynamicMaskList(i)%factor(j)
                        dynamicMaskList(i)%dstElement(k) = dynamicMaskList(i)%srcElement(j)%ptr(k)
                     end if
                  endif
               end do
            end do
            where (renorm > 0.d0)
            elsewhere
               dynamicMaskList(i)%dstElement = dynamicSrcMaskValue
            end where
         enddo
      endif
      ! return successfully
      rc = ESMF_SUCCESS
   end subroutine voteDynMaskProcV


   subroutine fractionDynMaskProcV(dynamicMaskList, dynamicSrcMaskValue, &
        dynamicDstMaskValue, rc)
      type(ESMF_DynamicMaskElementR4R8R4V), pointer              :: dynamicMaskList(:)
      real(ESMF_KIND_R4),            intent(in), optional :: dynamicSrcMaskValue
      real(ESMF_KIND_R4),            intent(in), optional :: dynamicDstMaskValue
      integer,                       intent(out)          :: rc
      integer :: i, j, k, n
      real(ESMF_KIND_R4), allocatable  :: renorm(:)

      _unused_dummy(dynamicDstMaskValue)

      if (associated(dynamicMaskList)) then
         n = size(dynamicMaskList(1)%srcElement(1)%ptr)
         allocate(renorm(n))

         do i=1, size(dynamicMaskList)
            dynamicMaskList(i)%dstElement = 0.0 ! set to zero

            renorm = 0.d0 ! reset
            do j=1, size(dynamicMaskList(i)%factor)
               do k = 1, size(dynamicMaskList(i)%srcElement(j)%ptr)
                  if (.not. &
                       match(dynamicSrcMaskValue,dynamicMaskList(i)%srcElement(j)%ptr(k))) then
                     if (nint(dynamicMaskList(i)%srcElement(j)%ptr(k)) == 0) then
                        dynamicMaskList(i)%dstElement(k) = dynamicMaskList(i)%dstElement(k) + &
                             & dynamicMaskList(i)%factor(j)
                        renorm(k) = renorm(k) + dynamicMaskList(i)%factor(j)
                     end if
                  endif
               end do
            end do
         enddo
      endif
      ! return successfully
      rc = ESMF_SUCCESS
   end subroutine fractionDynMaskProcV

   subroutine do_regrid(this, src_field, dst_field, unusable, doTranspose, rc)
      class (EsmfRegridder), intent(in) :: this
      type (ESMF_Field), intent(in) :: src_field
      type (ESMF_Field), intent(inout) :: dst_field
      class (KeywordEnforcer), optional, intent(in) :: unusable
      logical, optional, intent(in) :: doTranspose
      integer, optional, intent(out) :: rc

      type (ESMF_RouteHandle) :: route_handle
      type (RegridderSpec) :: spec
      type(ESMF_TypeKind_Flag) :: src_kind, dst_kind
      character(*), parameter :: Iam = 'MAPL_EsmfRegridder::do_regrid()'
      integer :: status

      _unused_dummy(unusable)

      call ESMF_FieldGet(src_field, typekind = src_kind, rc = status)
      _verify(status)
      call ESMF_FieldGet(dst_field, typekind = dst_kind, rc = status)
      _verify(status)

      _assert(src_kind == dst_kind,'inconsistent kinds')

      route_handle = this%select_route_handle(src_kind, do_transpose = doTranspose, rc = status)
      _verify(status)

      spec = this%get_spec()

      if (spec%regrid_method /= REGRID_METHOD_NEAREST_STOD) then
         call ESMF_FieldRegrid(src_field, dst_field, &
              & routeHandle=route_handle, &
              & dynamicMask=this%dynamic_mask, &
              & termorderflag=ESMF_TERMORDER_SRCSEQ, &
              & zeroregion=ESMF_REGION_SELECT, &
              & rc=status)
         _verify(status)
      else
         call ESMF_FieldRegrid(src_field, dst_field, &
              & routeHandle=route_handle, &
              & termorderflag=ESMF_TERMORDER_SRCSEQ, &
              & zeroregion=ESMF_REGION_SELECT, &
              & rc=status)
         _verify(status)
      end if

   end subroutine do_regrid

   subroutine initialize_subclass(this, unusable, rc)
     use MAPL_KeywordEnforcerMod
     use MAPL_RegridderSpec
     class (EsmfRegridder), intent(inout) :: this
     class (KeywordEnforcer), optional, intent(in) :: unusable
     integer, optional, intent(out) :: rc


     integer :: status
     character(len=*), parameter :: Iam = 'initialize_subclass'
     type (RegridderSpec) :: spec

     _unused_dummy(unusable)

     spec = this%get_spec()

     this%regrid_method = spec%regrid_method

     call this%create_route_handle(ESMF_TYPEKIND_R4, rc = status)
     _verify(status)

     ! TODO: should get missing value from source file
     select case (spec%regrid_method)
     case (REGRID_METHOD_BILINEAR, REGRID_METHOD_CONSERVE, REGRID_METHOD_CONSERVE_2ND, REGRID_METHOD_PATCH)
        call ESMF_DynamicMaskSetR4R8R4V(this%dynamic_mask, &
             & dynamicSrcMaskValue=MAPL_undef, &
             & dynamicMaskRoutine=simpleDynMaskProcV, &
             & rc=rc)
        _verify(rc)
     case (REGRID_METHOD_BILINEAR_MONOTONIC, REGRID_METHOD_CONSERVE_MONOTONIC)
        call ESMF_DynamicMaskSetR4R8R4V(this%dynamic_mask, &
             & dynamicSrcMaskValue=MAPL_undef, &
             & dynamicMaskRoutine=monotonicDynMaskProcV, &
             & handleAllElements=.true., &
             & rc=rc)
        _verify(rc)
     case (REGRID_METHOD_VOTE)
        call ESMF_DynamicMaskSetR4R8R4V(this%dynamic_mask, &
             & dynamicSrcMaskValue=MAPL_undef, &
             & dynamicMaskRoutine=voteDynMaskProcV, &
             & handleAllElements=.true., &
             & rc=rc)
        _verify(rc)
     case (REGRID_METHOD_FRACTION)
        call ESMF_DynamicMaskSetR4R8R4V(this%dynamic_mask, &
             & dynamicSrcMaskValue=MAPL_undef, &
             & dynamicMaskRoutine=fractionDynMaskProcV, &
             & handleAllElements=.true., &
             & rc=rc)
        _verify(rc)
     end select

     _return(_success)

   end subroutine initialize_subclass


   subroutine create_route_handle(this, kind, rc)
     class (EsmfRegridder), intent(in) :: this
     type(ESMF_TypeKind_Flag), intent(in) :: kind
     integer, optional, intent(out) :: rc

     integer :: status
     character(len=*), parameter :: Iam = 'create_route_handle'

     type (RegridderSpec) :: spec
     real, pointer :: src_dummy_r4(:,:), dst_dummy_r4(:,:)
     real(real64), pointer :: src_dummy_r8(:,:), dst_dummy_r8(:,:)
     type (ESMF_Field) :: src_field, dst_field

     integer :: srcTermProcessing, num_mask_values
     integer, pointer :: factorIndexList(:,:)
     integer, allocatable :: dstMaskValues(:)
     real(ESMF_KIND_R8), pointer :: factorList(:)
     type(ESMF_RouteHandle) :: dummy_rh
     type(ESMF_UnmappedAction_Flag) :: unmappedaction
     logical :: global, isPresent, has_mask, has_dstMaskValues
     type(RegridderSpecRouteHandleMap), pointer :: route_handles, transpose_route_handles
     type(ESMF_RouteHandle) :: route_handle, transpose_route_handle
     character(len=ESMF_MAXPATHLEN) :: rh_file,rh_trans_file
     logical :: rh_file_exists, file_weights, compute_transpose

     type(Logger), pointer :: lgr
     lgr => logging%get_logger('MAPL')

     if (kind == ESMF_TYPEKIND_R4) then
        route_handles => route_handles_r4
        transpose_route_handles => transpose_route_handles_r4
     else if(kind == ESMF_TYPEKIND_R8) then
        route_handles => route_handles_r8
        transpose_route_handles => transpose_route_handles_r8
     else
        _fail('unsupported type kind (must be R4 or R8)')
     end if

     unmappedaction = ESMF_UNMAPPEDACTION_ERROR

     spec = this%get_spec()

     if (route_handles%count(spec) == 0) then  ! new route_handle
        file_weights = IAND(spec%hints,REGRID_HINT_FILE_WEIGHTS) /= 0
        compute_transpose = IAND(spec%hints,REGRID_HINT_COMPUTE_TRANSPOSE) /= 0

        if (file_weights) then
           rh_file = generate_rh_name(spec%grid_in,spec%grid_out,spec%regrid_method,_rc)
           rh_trans_file = "transpose_"//rh_file
           inquire(file=rh_file,exist=rh_file_exists)
        else
           rh_file_exists = .false.
        end if
        if (rh_file_exists) then
           call lgr%info('Reading weight file: %a', trim(rh_file))
           route_handle = ESMF_RouteHandleCreate(rh_file,_rc)
           call route_handles%insert(spec, route_handle)
           if (compute_transpose) then
              transpose_route_handle = ESMF_RouteHandleCreate(rh_trans_file,_rc)
              call transpose_route_handles%insert(spec, transpose_route_handle)
           end if
        else
           src_field = ESMF_FieldCreate(spec%grid_in, typekind=kind, &
                & indexflag=ESMF_INDEX_DELOCAL, staggerloc=ESMF_STAGGERLOC_CENTER, rc=status)
           _verify(status)

           if (MAPL_GridHasDE(spec%grid_in)) then
              if (kind == ESMF_TYPEKIND_R4) then
                 call ESMF_FieldGet(src_field, localDe=0, farrayPtr=src_dummy_r4, rc=status)
                 _verify(status)
                 src_dummy_r4 = 0
              else if (kind == ESMF_TYPEKIND_R8) then
                 call ESMF_FieldGet(src_field, localDe=0, farrayPtr=src_dummy_r8, rc=status)
                 _verify(status)
                 src_dummy_r8 = 0
              end if
           end if

           dst_field = ESMF_FieldCreate(spec%grid_out, typekind=kind, &
                & indexflag=ESMF_INDEX_DELOCAL, staggerloc=ESMF_STAGGERLOC_CENTER, rc=status)
           _verify(status)
           if (MAPL_GridHasDE(spec%grid_out)) then
              if (kind == ESMF_TYPEKIND_R4) then
                 call ESMF_FieldGet(dst_field, localDe=0, farrayPtr=dst_dummy_r4, rc=status)
                 _verify(status)
                 dst_dummy_r4 = 0
              else if (kind == ESMF_TYPEKIND_R8) then
                 call ESMF_FieldGet(dst_field, localDe=0, farrayPtr=dst_dummy_r8, rc=status)
                 _verify(status)
                 dst_dummy_r8 = 0
              end if
           end if
           call ESMF_GridGetItem(spec%grid_out,itemflag=ESMF_GRIDITEM_MASK, &
           staggerloc=ESMF_STAGGERLOC_CENTER, isPresent = has_mask, _rc)
           call ESMF_AttributeGet(spec%grid_out, name=MAPL_DESTINATIONMASK, isPresent=has_dstMaskValues, _rc)
           if (has_dstMaskValues) then
              _assert(has_mask, "masking destination values when no masks is present")
              call ESMF_AttributeGet(spec%grid_out, name=MAPL_DESTINATIONMASK, itemcount=num_mask_values, _rc)
              allocate(dstMaskValues(num_mask_values), _stat)
              call ESMF_AttributeGet(spec%grid_out, name=MAPL_DESTINATIONMASK, valuelist=dstMaskValues, _rc)
           end if

           counter = counter + 1

           srcTermProcessing=0
           call ESMF_AttributeGet(spec%grid_in, name='Global',isPresent=isPresent,rc=status)
           if (isPresent) then
              call ESMF_AttributeGet(spec%grid_in, name='Global',value=global,rc=status)
              if (.not.global) unmappedaction=ESMF_UNMAPPEDACTION_IGNORE
           end if
           select case (spec%regrid_method)
           case (REGRID_METHOD_BILINEAR, REGRID_METHOD_BILINEAR_MONOTONIC)

              call ESMF_FieldRegridStore(src_field, dst_field, &
                   & regridmethod=ESMF_REGRIDMETHOD_BILINEAR, &
                   & dstMaskValues = dstMaskValues, &
                   & linetype=ESMF_LINETYPE_GREAT_CIRCLE, & ! closer to SJ Lin interpolation weights?
                   & srcTermProcessing = srcTermProcessing, &
                   & factorList=factorList, factorIndexList=factorIndexList, &
                   & routehandle=route_handle, unmappedaction=unmappedaction, rc=status)
              _verify(status)
           case (REGRID_METHOD_PATCH)

              call ESMF_FieldRegridStore(src_field, dst_field, &
                   & regridmethod=ESMF_REGRIDMETHOD_PATCH, &
                   & dstMaskValues = dstMaskValues, &
                   & linetype=ESMF_LINETYPE_GREAT_CIRCLE, & ! closer to SJ Lin interpolation weights?
                   & srcTermProcessing = srcTermProcessing, &
                   & factorList=factorList, factorIndexList=factorIndexList, &
                   & routehandle=route_handle, unmappedaction=unmappedaction, rc=status)
              _verify(status)
           case (REGRID_METHOD_CONSERVE_2ND)

              call ESMF_FieldRegridStore(src_field, dst_field, &
                   & regridmethod=ESMF_REGRIDMETHOD_CONSERVE_2ND, &
                   & dstMaskValues = dstMaskValues, &
                   & linetype=ESMF_LINETYPE_GREAT_CIRCLE, & ! closer to SJ Lin interpolation weights?
                   & srcTermProcessing = srcTermProcessing, &
                   & factorList=factorList, factorIndexList=factorIndexList, &
                   & routehandle=route_handle, unmappedaction=unmappedaction, rc=status)
              _verify(status)
           case (REGRID_METHOD_CONSERVE, REGRID_METHOD_CONSERVE_MONOTONIC, REGRID_METHOD_VOTE, REGRID_METHOD_FRACTION)

              call ESMF_FieldRegridStore(src_field, dst_field, &
                   & regridmethod=ESMF_REGRIDMETHOD_CONSERVE, &
                   & dstMaskValues = dstMaskValues, &
                   & srcTermProcessing = srcTermProcessing, &
                   & factorList=factorList, factorIndexList=factorIndexList, &
                   & routehandle=route_handle, unmappedaction=unmappedaction, rc=status)
              _verify(status)
           case (REGRID_METHOD_NEAREST_STOD)

              call ESMF_FieldRegridStore(src_field, dst_field, &
                   & regridmethod=ESMF_REGRIDMETHOD_NEAREST_STOD, &
                   & dstMaskValues = dstMaskValues, &
                   & factorList=factorList, factorIndexList=factorIndexList, &
                   & routehandle=route_handle, unmappedaction=unmappedaction, rc=status)
              _verify(status)
           case default
              _fail('unknown regrid method')
           end select
           call route_handles%insert(spec, route_handle)

           if (compute_transpose) then
              call ESMF_FieldSMMStore(src_field,dst_field,dummy_rh,transpose_route_handle, &
                  & factorList,factorIndexList,srcTermProcessing=srcTermProcessing, &
                  & rc=status)
              _verify(status)
              call transpose_route_handles%insert(spec, transpose_route_handle)
           end if

           ! Free resources
           deallocate(factorList,factorIndexList)

           call ESMF_FieldDestroy(src_field, rc=status)
           _verify(status)
           call ESMF_FieldDestroy(dst_field, rc=status)
           _verify(status)
           if (file_weights) then
              call lgr%info('Writing weight file: %a', trim(rh_file))
              call ESMF_RouteHandleWrite(route_handle,rh_file,_rc)
              if (compute_transpose) then
                 call lgr%info('Writing transpose weight file: %a', trim(rh_trans_file))
                 call ESMF_RouteHandleWrite(transpose_route_handle,rh_trans_file,_rc)
              end if
           end if
        end if
     end if

     _return(_success)
   end subroutine create_route_handle


   function select_route_handle(this, kind, do_transpose, rc) result(route_handle)
     type(ESMF_RouteHandle) :: route_handle
     class (EsmfRegridder), intent(in) :: this
     type(ESMF_TypeKind_Flag), intent(in) :: kind
     logical, optional, intent(in) :: do_transpose
     integer, optional, intent(out) :: rc
     character(*), parameter :: Iam = "select_route_handle"

     type(RegridderSpecRouteHandleMap), pointer :: route_handles, transpose_route_handles
     type (RegridderSpec) :: spec
     integer :: status
     logical :: transpose

     spec = this%get_spec()

     if (kind == ESMF_TYPEKIND_R4) then
        route_handles => route_handles_r4
        transpose_route_handles => transpose_route_handles_r4
     else if(kind == ESMF_TYPEKIND_R8) then
        route_handles => route_handles_r8
        transpose_route_handles => transpose_route_handles_r8
     else
        _fail('unsuported typekind (must be R4 or R8)')
     end if

     ! Create route-handle if none exist
     if (route_handles%count(spec) == 0) then
        call this%create_route_handle(kind, rc = status)
        _verify(status)
     end if

     ! select proper route-handle
     transpose = .false.
     if (present(do_transpose)) then
        transpose = do_transpose
     end if

     if (.not. transpose) then
        route_handle = route_handles%at(spec)
     else
        route_handle = transpose_route_handles%at(spec)
     end if

     _return(_success)

   end function select_route_handle

   subroutine destroy(this, rc)
      class(EsmfRegridder), intent(inout) :: this
      integer, optional, intent(out) :: rc
      integer :: status

      call this%destroy_route_handle(ESMF_TYPEKIND_R4, _rc)

      _return(_success)
   end subroutine destroy


   subroutine destroy_route_handle(this, kind, rc)
      class(EsmfRegridder), intent(inout) :: this
      type(ESMF_TypeKind_Flag), intent(in) :: kind
      integer, optional, intent(out) :: rc

     type (RegridderSpec) :: spec
     type(ESMF_RouteHandle) :: dummy_rh
     type(RegridderSpecRouteHandleMap), pointer :: route_handles, transpose_route_handles
     type(ESMF_RouteHandle) :: route_handle
     type(RegridderSpecRouteHandleMapIterator) :: iter
     integer :: status
     logical :: compute_transpose

     if (kind == ESMF_TYPEKIND_R4) then
        route_handles => route_handles_r4
        transpose_route_handles => transpose_route_handles_r4
     else if(kind == ESMF_TYPEKIND_R8) then
        route_handles => route_handles_r8
        transpose_route_handles => transpose_route_handles_r8
     else
        _fail('unsupported type kind (must be R4 or R8)')
     end if

     spec = this%get_spec()

     _assert(route_handles%count(spec) == 1, 'Did not find this spec in route handle table.')
     route_handle = route_handles%at(spec)
     call ESMF_RouteHandleDestroy(route_handle, noGarbage=.true.,_rc)
     iter = route_handles%find(spec)
     call route_handles%erase(iter)

     compute_transpose = IAND(spec%hints,REGRID_HINT_COMPUTE_TRANSPOSE) /= 0
     if (compute_transpose) then
        _assert(transpose_route_handles%count(spec) == 1, 'Did not find this spec in route handle table.')
        route_handle = transpose_route_handles%at(spec)
        call ESMF_RouteHandleDestroy(route_handle, noGarbage=.true., _rc)
        iter = transpose_route_handles%find(spec)
        call transpose_route_handles%erase(iter)
     end if

      _return(_success)
   end subroutine destroy_route_handle

   function generate_rh_name(grid_in,grid_out,regrid_method,rc) result(file_name)
      character(len=:), allocatable :: file_name
      type(ESMF_Grid), intent(in) :: grid_in
      type(ESMF_Grid), intent(in) :: grid_out
      integer, intent(in) :: regrid_method
      integer, intent(out), optional :: rc

      integer :: im_in, jm_in, im_out, jm_out
      integer :: nx_in, ny_in, nx_out, ny_out
      character(len=5) :: cim_in,cjm_in,cim_out,cjm_out
      character(len=5) :: cnx_in,cny_in,cnx_out,cny_out
      character(len=2) :: cmeth
      integer :: temp(3),layout(2)
      integer :: status

      call MAPL_GridGet(grid_in,GlobalCellCountPerDim=temp,layout=layout,_rc)
      im_in = temp(1)
      jm_in = temp(2)
      nx_in = layout(1)
      ny_in = layout(2)
      write(cim_in,'(I5.5)')im_in
      write(cjm_in,'(I5.5)')jm_in
      write(cnx_in,'(I5.5)')nx_in
      write(cny_in,'(I5.5)')ny_in
      call MAPL_GridGet(grid_out,GlobalCellCountPerDim=temp,layout=layout,_rc)
      im_out = temp(1)
      jm_out = temp(2)
      nx_out = layout(1)
      ny_out = layout(2)
      write(cim_out,'(I5.5)')im_out
      write(cjm_out,'(I5.5)')jm_out
      write(cnx_out,'(I5.5)')nx_out
      write(cny_out,'(I5.5)')ny_out
      write(cmeth,'(I2.2)')regrid_method
      file_name = "rh_"//cim_in//"x"//cjm_in//"_"//cnx_in//"x"//cny_in//"_"//cim_out//"x"//cjm_out//"_"//cnx_out//"x"//cny_out//"_method_"//cmeth
      _return(_success)

   end function

end module MAPL_EsmfRegridderMod
