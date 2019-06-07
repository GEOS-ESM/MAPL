#define _SUCCESS      0
#define _FAILURE     1
#define _VERIFY(A)   if(  A/=0) then; if(present(rc)) rc=A; PRINT *, Iam, __LINE__; return; endif
#define _ASSERT(A)   if(.not.A) then; if(present(rc)) rc=_FAILURE; PRINT *, Iam, __LINE__; return; endif
#define _RETURN(A)   if(present(rc)) rc=A; return
#include "unused_dummy.H"

module MAPL_EsmfRegridderMod
   use, intrinsic :: iso_fortran_env, only: REAL32, REAL64
   use ESMF
   use MAPL_KeywordEnforcerMod
   use MAPL_RegridderSpecMod
   use MAPL_AbstractGridFactoryMod
   use MAPL_AbstractRegridderMod
   use MAPL_GridManagerMod
   use MAPL_BaseMod, only: MAPL_undef
   use MAPL_RegridderSpecRouteHandleMapMod
   implicit none
   private

   public :: EsmfRegridder

   integer, save :: counter = 0

   ! Singleton container to avoid unnecessary duplication of
   ! ESMF Route handles
   type (RegridderSpecRouteHandleMap), save, target :: route_handles
   type (RegridderSpecRouteHandleMap), save, target :: transpose_route_handles

   type, extends(AbstractRegridder) :: EsmfRegridder
!!$      private
      integer, public :: id = -1
      type (ESMF_RouteHandle) :: route_handle
      type (ESMF_RouteHandle) :: transpose_route_handle
      integer :: regrid_method
      type (ESMF_DynamicMask) :: dynamic_mask
   contains
      procedure :: initialize_subclass
      procedure :: regrid_scalar_2d_real32
      procedure :: regrid_scalar_3d_real32
      procedure :: regrid_vector_2d_real32
      procedure :: regrid_vector_3d_real32
      procedure :: transpose_regrid_scalar_2d_real32
      procedure :: transpose_regrid_scalar_3d_real32
      procedure :: transpose_regrid_vector_2d_real32
      procedure :: transpose_regrid_vector_3d_real32

      ! internal
      procedure :: do_regrid
 
   end type EsmfRegridder

   interface EsmfRegridder
      module procedure new_EsmfRegridder
   end interface EsmfRegridder


contains


   function new_EsmfRegridder() result(regridder)
      use MAPL_BaseMod
      type (EsmfRegridder) :: regridder

      ! Nothing to do here
   end function new_EsmfRegridder
   

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

      spec = this%get_spec()
      
!!$      allocate(p_src, source=q_in)
!!$      allocate(p_dst, source=q_out)
      allocate(p_src(size(q_in,1),size(q_in,2)))
      allocate(p_dst(size(q_out,1),size(q_out,2)))
      p_src = q_in
      
      ! TODO support other staggerings
      src_field = ESMF_FieldCreate(spec%grid_in, farrayPtr=p_src, &
           & staggerloc=ESMF_STAGGERLOC_CENTER, datacopyFlag=ESMF_DATACOPY_REFERENCE, rc=status)
      _VERIFY(status)


      p_dst = 0
      dst_field = ESMF_FieldCreate(spec%grid_out, farrayPtr=p_dst, &
           & staggerloc=ESMF_STAGGERLOC_CENTER, datacopyFlag=ESMF_DATACOPY_REFERENCE, rc=status)
      _VERIFY(status)

      call this%do_regrid(src_field, dst_field, rc=status)
      _VERIFY(status)

      q_out = p_dst

      call ESMF_FieldDestroy(src_field, rc=status)
      _VERIFY(status)
      call ESMF_FieldDestroy(dst_field, rc=status)
      _VERIFY(status)

      deallocate(p_src)
      deallocate(p_dst)
      
      _RETURN(ESMF_SUCCESS)

   end subroutine regrid_scalar_2d_real32

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

      spec = this%get_spec()
      
!!$      allocate(p_src, source=q_in)
!!$      allocate(p_dst, source=q_out)
      allocate(p_src(size(q_in,1),size(q_in,2)))
      allocate(p_dst(size(q_out,1),size(q_out,2)))
      p_src = q_in
      
      ! TODO support other staggerings
      src_field = ESMF_FieldCreate(spec%grid_out, farrayPtr=p_src, &
           & staggerloc=ESMF_STAGGERLOC_CENTER, datacopyFlag=ESMF_DATACOPY_REFERENCE, rc=status)
      _VERIFY(status)


      p_dst = 0
      dst_field = ESMF_FieldCreate(spec%grid_in, farrayPtr=p_dst, &
           & staggerloc=ESMF_STAGGERLOC_CENTER, datacopyFlag=ESMF_DATACOPY_REFERENCE, rc=status)
      _VERIFY(status)

      call this%do_regrid(src_field, dst_field, doTranspose=.true., rc=status)
      _VERIFY(status)

      q_out = p_dst

      call ESMF_FieldDestroy(src_field, rc=status)
      _VERIFY(status)
      call ESMF_FieldDestroy(dst_field, rc=status)
      _VERIFY(status)

      deallocate(p_src)
      deallocate(p_dst)
      
      _RETURN(ESMF_SUCCESS)

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

      integer :: km
      integer :: im_src, jm_src
      integer :: im_dst, jm_dst

      spec = this%get_spec()

      km = size(q_in,3)
      _ASSERT(km == size(q_out,3))

      im_src = size(q_in,1)
      jm_src = size(q_in,2)

      im_dst = size(q_out,1)
      jm_dst = size(q_out,2)

      ! Note: tranposing km to leading index
      allocate(p_src(km,im_src,jm_src))
      allocate(p_dst(km,im_dst,jm_dst))

      p_src = reshape(q_in,shape(p_src), order=[2,3,1])
      ! TODO support other staggerings
      src_field = ESMF_FieldCreate(spec%grid_in, farrayPtr=p_src, &
           & gridToFieldMap=[2,3], &
           & staggerloc=ESMF_STAGGERLOC_CENTER, datacopyFlag=ESMF_DATACOPY_REFERENCE, &
           & rc=status)
      _VERIFY(status)

      
      p_dst = 0
      dst_field = ESMF_FieldCreate(spec%grid_out, farrayPtr=p_dst, &
           & gridToFieldMap=[2,3], &
           & staggerloc=ESMF_STAGGERLOC_CENTER, datacopyFlag=ESMF_DATACOPY_REFERENCE, rc=status)
      _VERIFY(status)

      call this%do_regrid(src_field, dst_field, rc=status)
      _VERIFY(status)

      q_out = reshape(p_dst, shape(q_out), order=[3,1,2])

      call ESMF_FieldDestroy(src_field, rc=status)
      _VERIFY(status)
      call ESMF_FieldDestroy(dst_field, rc=status)
      _VERIFY(status)

      deallocate(p_src)
      deallocate(p_dst)
      
      _RETURN(ESMF_SUCCESS)

   end subroutine regrid_scalar_3d_real32

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

      integer :: km
      integer :: im_src, jm_src
      integer :: im_dst, jm_dst

      spec = this%get_spec()

      km = size(q_in,3)
      _ASSERT(km == size(q_out,3))

      im_src = size(q_in,1)
      jm_src = size(q_in,2)

      im_dst = size(q_out,1)
      jm_dst = size(q_out,2)

      ! Note: tranposing km to leading index
      allocate(p_src(km,im_src,jm_src))
      allocate(p_dst(km,im_dst,jm_dst))

      p_src = reshape(q_in,shape(p_src), order=[2,3,1])
      ! TODO support other staggerings
      src_field = ESMF_FieldCreate(spec%grid_out, farrayPtr=p_src, &
           & gridToFieldMap=[2,3], &
           & staggerloc=ESMF_STAGGERLOC_CENTER, datacopyFlag=ESMF_DATACOPY_REFERENCE, &
           & rc=status)
      _VERIFY(status)

      
      p_dst = 0
      dst_field = ESMF_FieldCreate(spec%grid_in, farrayPtr=p_dst, &
           & gridToFieldMap=[2,3], &
           & staggerloc=ESMF_STAGGERLOC_CENTER, datacopyFlag=ESMF_DATACOPY_REFERENCE, rc=status)
      _VERIFY(status)

      call this%do_regrid(src_field, dst_field, doTranspose=.true., rc=status)
      _VERIFY(status)

      q_out = reshape(p_dst, shape(q_out), order=[3,1,2])

      call ESMF_FieldDestroy(src_field, rc=status)
      _VERIFY(status)
      call ESMF_FieldDestroy(dst_field, rc=status)
      _VERIFY(status)

      deallocate(p_src)
      deallocate(p_dst)
      
      _RETURN(ESMF_SUCCESS)

   end subroutine transpose_regrid_scalar_3d_real32


   subroutine regrid_vector_2d_real32(this, u_in, v_in, u_out, v_out, rc)
      class (EsmfRegridder), intent(in) :: this
      real, intent(in) :: u_in(:,:)
      real, intent(in) :: v_in(:,:)
      real, intent(out) :: u_out(:,:)
      real, intent(out) :: v_out(:,:)
      integer, optional, intent(out) :: rc

      character(*), parameter :: Iam = 'MAPL_EsmfRegridder::regrid_vector_2d_real32()'
      integer :: status

      type (RegridderSpec) :: spec
      class (AbstractGridFactory), pointer :: factory

      real, pointer :: p_src(:,:,:,:), p_dst(:,:,:,:)
      type (ESMF_Field) :: src_field, dst_field

      integer :: im_src, jm_src
      integer :: im_dst, jm_dst

      spec = this%get_spec()

      im_src = size(u_in,1)
      jm_src = size(u_in,2)
      _ASSERT(im_src == size(v_in,1))
      _ASSERT(jm_src == size(v_in,2))

      im_dst = size(u_out,1)
      jm_dst = size(u_out,2)
      _ASSERT(im_dst == size(v_out,1))
      _ASSERT(jm_dst == size(v_out,2))

      allocate(p_src(3,1,im_src,jm_src))
      allocate(p_dst(3,1,im_dst,jm_dst))

      factory => grid_manager%get_factory(spec%grid_in,rc=status)
      _VERIFY(status)
      call factory%spherical_to_cartesian(u_in, v_in, p_src, 'north-south', rc=status)
      _VERIFY(status)
      
      ! TODO support other staggerings
      src_field = ESMF_FieldCreate(spec%grid_in, farrayPtr=p_src, &
           & gridToFieldMap=[3,4], &
           & staggerloc=ESMF_STAGGERLOC_CENTER, datacopyFlag=ESMF_DATACOPY_REFERENCE, &
           & rc=status)
      _VERIFY(status)

      
      p_dst = 0
      dst_field = ESMF_FieldCreate(spec%grid_out, farrayPtr=p_dst, &
           & gridToFieldMap=[3,4], &
           & staggerloc=ESMF_STAGGERLOC_CENTER, datacopyFlag=ESMF_DATACOPY_REFERENCE, rc=status)
      _VERIFY(status)

      call this%do_regrid(src_field, dst_field, rc=status)
      _VERIFY(status)

      factory => grid_manager%get_factory(spec%grid_out,rc=status)
      _VERIFY(status)
      call factory%cartesian_to_spherical(p_dst, u_out, v_out, 'north-south', rc=status)
      _VERIFY(status)

      call ESMF_FieldDestroy(src_field, rc=status)
      _VERIFY(status)
      call ESMF_FieldDestroy(dst_field, rc=status)
      _VERIFY(status)

      deallocate(p_src)
      deallocate(p_dst)
      
      _RETURN(ESMF_SUCCESS)

   end subroutine regrid_vector_2d_real32

   subroutine transpose_regrid_vector_2d_real32(this, u_in, v_in, u_out, v_out, rc)
      class (EsmfRegridder), intent(in) :: this
      real, intent(in) :: u_in(:,:)
      real, intent(in) :: v_in(:,:)
      real, intent(out) :: u_out(:,:)
      real, intent(out) :: v_out(:,:)
      integer, optional, intent(out) :: rc

      character(*), parameter :: Iam = 'MAPL_EsmfRegridder::transpose_regrid_vector_2d_real32()'
      integer :: status

      type (RegridderSpec) :: spec
      class (AbstractGridFactory), pointer :: factory

      real, pointer :: p_src(:,:,:,:), p_dst(:,:,:,:)
      type (ESMF_Field) :: src_field, dst_field

      integer :: im_src, jm_src
      integer :: im_dst, jm_dst

      spec = this%get_spec()

      im_src = size(u_in,1)
      jm_src = size(u_in,2)
      _ASSERT(im_src == size(v_in,1))
      _ASSERT(jm_src == size(v_in,2))

      im_dst = size(u_out,1)
      jm_dst = size(u_out,2)
      _ASSERT(im_dst == size(v_out,1))
      _ASSERT(jm_dst == size(v_out,2))

      allocate(p_src(3,1,im_src,jm_src))
      allocate(p_dst(3,1,im_dst,jm_dst))

      factory => grid_manager%get_factory(spec%grid_out,rc=status)
      _VERIFY(status)
      call factory%spherical_to_cartesian(u_in, v_in, p_src, 'north-south', rc=status)
      _VERIFY(status)
      
      ! TODO support other staggerings
      src_field = ESMF_FieldCreate(spec%grid_out, farrayPtr=p_src, &
           & gridToFieldMap=[3,4], &
           & staggerloc=ESMF_STAGGERLOC_CENTER, datacopyFlag=ESMF_DATACOPY_REFERENCE, &
           & rc=status)
      _VERIFY(status)

      
      p_dst = 0
      dst_field = ESMF_FieldCreate(spec%grid_in, farrayPtr=p_dst, &
           & gridToFieldMap=[3,4], &
           & staggerloc=ESMF_STAGGERLOC_CENTER, datacopyFlag=ESMF_DATACOPY_REFERENCE, rc=status)
      _VERIFY(status)

      call this%do_regrid(src_field, dst_field, doTranspose=.true., rc=status)
      _VERIFY(status)

      factory => grid_manager%get_factory(spec%grid_in,rc=status)
      _VERIFY(status)
      call factory%cartesian_to_spherical(p_dst, u_out, v_out, 'north-south', rc=status)
      _VERIFY(status)

      call ESMF_FieldDestroy(src_field, rc=status)
      _VERIFY(status)
      call ESMF_FieldDestroy(dst_field, rc=status)
      _VERIFY(status)

      deallocate(p_src)
      deallocate(p_dst)
      
      _RETURN(ESMF_SUCCESS)

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

      integer :: km
      integer :: im_src, jm_src
      integer :: im_dst, jm_dst

      spec = this%get_spec()

      km = size(u_in,3)
      _ASSERT(km == size(v_in,3))
      _ASSERT(km == size(u_out,3))
      _ASSERT(km == size(v_out,3))

      im_src = size(u_in,1)
      jm_src = size(u_in,2)
      _ASSERT(im_src == size(v_in,1))
      _ASSERT(jm_src == size(v_in,2))

      im_dst = size(u_out,1)
      jm_dst = size(u_out,2)
      _ASSERT(im_dst == size(v_out,1))
      _ASSERT(jm_dst == size(v_out,2))

      allocate(p_src(3,km,im_src,jm_src))
      allocate(p_dst(3,km,im_dst,jm_dst))

      grid_axis_in = 'north-south'
      grid_axis_out = 'north-south'
      if (present(rotate)) then
         if (rotate) then
            grid_axis_in = 'xyz'
            grid_axis_out = 'grid' 
         end if
      end if

      factory => grid_manager%get_factory(spec%grid_in,rc=status)
      _VERIFY(status)
      call factory%spherical_to_cartesian(u_in, v_in, p_src, grid_axis_in, rc=status)
      _VERIFY(status)
      
      ! TODO support other staggerings
      src_field = ESMF_FieldCreate(spec%grid_in, farrayPtr=p_src, &
           & gridToFieldMap=[3,4], &
           & staggerloc=ESMF_STAGGERLOC_CENTER, datacopyFlag=ESMF_DATACOPY_REFERENCE, &
           & rc=status)
      _VERIFY(status)

      
      p_dst = 0
      dst_field = ESMF_FieldCreate(spec%grid_out, farrayPtr=p_dst, &
           & gridToFieldMap=[3,4], &
           & staggerloc=ESMF_STAGGERLOC_CENTER, datacopyFlag=ESMF_DATACOPY_REFERENCE, rc=status)
      _VERIFY(status)

      call this%do_regrid(src_field, dst_field, rc=status)
      _VERIFY(status)

      factory => grid_manager%get_factory(spec%grid_out,rc=status)
      _VERIFY(status)
      call factory%cartesian_to_spherical(p_dst, u_out, v_out, grid_axis_out,rc=status)
      _VERIFY(status)

      call ESMF_FieldDestroy(src_field, rc=status)
      _VERIFY(status)
      call ESMF_FieldDestroy(dst_field, rc=status)
      _VERIFY(status)

      deallocate(p_src)
      deallocate(p_dst)
      
      _RETURN(ESMF_SUCCESS)

   end subroutine regrid_vector_3d_real32

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

      integer :: km
      integer :: im_src, jm_src
      integer :: im_dst, jm_dst
      character(len=ESMF_MAXSTR) :: grid_axis_in, grid_axis_out

      spec = this%get_spec()

      km = size(u_in,3)
      _ASSERT(km == size(v_in,3))
      _ASSERT(km == size(u_out,3))
      _ASSERT(km == size(v_out,3))

      im_src = size(u_in,1)
      jm_src = size(u_in,2)
      _ASSERT(im_src == size(v_in,1))
      _ASSERT(jm_src == size(v_in,2))

      im_dst = size(u_out,1)
      jm_dst = size(u_out,2)
      _ASSERT(im_dst == size(v_out,1))
      _ASSERT(jm_dst == size(v_out,2))

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
      _VERIFY(status)
      call factory%spherical_to_cartesian(u_in, v_in, p_src, grid_axis_in, rc=status)
      _VERIFY(status)
      
      ! TODO support other staggerings
      src_field = ESMF_FieldCreate(spec%grid_out, farrayPtr=p_src, &
           & gridToFieldMap=[3,4], &
           & staggerloc=ESMF_STAGGERLOC_CENTER, datacopyFlag=ESMF_DATACOPY_REFERENCE, &
           & rc=status)
      _VERIFY(status)

      
      p_dst = 0
      dst_field = ESMF_FieldCreate(spec%grid_in, farrayPtr=p_dst, &
           & gridToFieldMap=[3,4], &
           & staggerloc=ESMF_STAGGERLOC_CENTER, datacopyFlag=ESMF_DATACOPY_REFERENCE, rc=status)
      _VERIFY(status)

      call this%do_regrid(src_field, dst_field, doTranspose=.true.,rc=status)
      _VERIFY(status)

      factory => grid_manager%get_factory(spec%grid_in,rc=status)
      _VERIFY(status)
      call factory%cartesian_to_spherical(p_dst, u_out, v_out, grid_axis_out,rc=status)
      _VERIFY(status)

      call ESMF_FieldDestroy(src_field, rc=status)
      _VERIFY(status)
      call ESMF_FieldDestroy(dst_field, rc=status)
      _VERIFY(status)

      deallocate(p_src)
      deallocate(p_dst)
      
      _RETURN(ESMF_SUCCESS)

   end subroutine transpose_regrid_vector_3d_real32


   subroutine simpleDynMaskProcV(dynamicMaskList, dynamicSrcMaskValue, &
        dynamicDstMaskValue, rc)
      type(ESMF_DynamicMaskElementR4R8R4V), pointer              :: dynamicMaskList(:)
      real(ESMF_KIND_R4),            intent(in), optional :: dynamicSrcMaskValue
      real(ESMF_KIND_R4),            intent(in), optional :: dynamicDstMaskValue
      integer,                       intent(out)          :: rc
      integer :: i, j, k, n
      real(ESMF_KIND_R4), allocatable  :: renorm(:)

      _UNUSED_DUMMY(dynamicDstMaskValue)

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

      _UNUSED_DUMMY(dynamicDstMaskValue)

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

      _UNUSED_DUMMY(dynamicDstMaskValue)

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
      character(*), parameter :: Iam = 'MAPL_EsmfRegridder::do_regrid()'
      integer :: status


      _UNUSED_DUMMY(unusable)
      route_handle = this%route_handle ! Allows "this" to be intent(in)
      if (present(doTranspose)) then
         if (doTranspose) then
            route_handle = this%transpose_route_handle
         end if
      end if
         
      call ESMF_FieldRegrid(src_field, dst_field, &
           & routeHandle=route_handle, &
           & dynamicMask=this%dynamic_mask, &
           & termorderflag=ESMF_TERMORDER_SRCSEQ, &
           & zeroregion=ESMF_REGION_SELECT, &
           & rc=status)
      _VERIFY(status)

   end subroutine do_regrid

   subroutine initialize_subclass(this, unusable, rc)
      use MAPL_KeywordEnforcerMod
      use MAPL_RegridderSpecMod
      use MAPL_BaseMod, only: MAPL_grid_interior
      class (EsmfRegridder), intent(inout) :: this
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc


      integer :: status      
      character(len=*), parameter :: Iam = 'initialize_subclass'

      type (RegridderSpec) :: spec
      real, pointer :: src_dummy(:,:), dst_dummy(:,:)
      type (ESMF_Field) :: src_field, dst_field

    type (ESMF_VM) :: vm
    integer :: pet,srcTermProcessing
    integer, pointer :: factorIndexList(:,:)
    real(ESMF_KIND_R8), pointer :: factorList(:)
    type(ESMF_RouteHandle) :: dummy_rh

    call ESMF_VMGetCurrent(vm, rc=status)
    _VERIFY(status)
    call ESMF_VMGet(vm, localPet=pet, rc=status)
    _VERIFY(status)

      _UNUSED_DUMMY(unusable)

      spec = this%get_spec()

      this%regrid_method = spec%regrid_method

      if (route_handles%count(spec) == 0) then  ! new route_handle
         src_field = ESMF_FieldCreate(spec%grid_in, typekind=ESMF_TYPEKIND_R4, &
              & indexflag=ESMF_INDEX_DELOCAL, staggerloc=ESMF_STAGGERLOC_CENTER, rc=status)
         _VERIFY(status) 
         call ESMF_FieldGet(src_field, localDe=0, farrayPtr=src_dummy, rc=status)
         _VERIFY(status)
         src_dummy = 0
         
         dst_field = ESMF_FieldCreate(spec%grid_out, typekind=ESMF_TYPEKIND_R4, &
              & indexflag=ESMF_INDEX_DELOCAL, staggerloc=ESMF_STAGGERLOC_CENTER, rc=status)
         _VERIFY(status) 
         call ESMF_FieldGet(dst_field, localDe=0, farrayPtr=dst_dummy, rc=status)
         _VERIFY(status)
         dst_dummy = 0

         counter = counter + 1
         this%id = counter

         srcTermProcessing=0
         select case (spec%regrid_method)
         case (REGRID_METHOD_BILINEAR)

            call ESMF_FieldRegridStore(src_field, dst_field, &
                 & regridmethod=ESMF_REGRIDMETHOD_BILINEAR, &
                 & linetype=ESMF_LINETYPE_GREAT_CIRCLE, & ! closer to SJ Lin interpolation weights?
                 & srcTermProcessing = srcTermProcessing, &
                 & factorList=factorList, factorIndexList=factorIndexList, &
                 & routehandle=this%route_handle, rc=status)
            _VERIFY(status)
         case (REGRID_METHOD_CONSERVE, REGRID_METHOD_VOTE, REGRID_METHOD_FRACTION)
            call ESMF_FieldRegridStore(src_field, dst_field, &
                 & regridmethod=ESMF_REGRIDMETHOD_CONSERVE, &
                 & srcTermProcessing = srcTermProcessing, &
                 & factorList=factorList, factorIndexList=factorIndexList, &
                 & routehandle=this%route_handle, rc=status)
            _VERIFY(status)
         end select
         call ESMF_FieldSMMStore(src_field,dst_field,dummy_rh,this%transpose_route_handle, &
              & factorList,factorIndexList,srcTermProcessing=srcTermProcessing, &
              & rc=status)
         _VERIFY(status)

         call route_handles%insert(spec, this%route_handle)
         call transpose_route_handles%insert(spec, this%transpose_route_handle)
         ! Free resources
         deallocate(factorList,factorIndexList)
         call ESMF_FieldDestroy(src_field, rc=status)
         _VERIFY(status)
         call ESMF_FieldDestroy(dst_field, rc=status)
         _VERIFY(status)
      else
         ! _NOT_ pointer assignment - location may float
         this%route_handle = route_handles%at(spec)
         this%transpose_route_handle = transpose_route_handles%at(spec)
      end if

      ! TODO: should get missing value from source file
      select case (spec%regrid_method)
      case (REGRID_METHOD_BILINEAR, REGRID_METHOD_CONSERVE)
         call ESMF_DynamicMaskSetR4R8R4V(this%dynamic_mask, &
              & dynamicSrcMaskValue=MAPL_undef, &
              & dynamicMaskRoutine=simpleDynMaskProcV, &
              & rc=rc)
         _VERIFY(status)
      case (REGRID_METHOD_VOTE)
         call ESMF_DynamicMaskSetR4R8R4V(this%dynamic_mask, &
              & dynamicSrcMaskValue=MAPL_undef, &
              & dynamicMaskRoutine=voteDynMaskProcV, &
              & handleAllElements=.true., &
              & rc=rc)
         _VERIFY(status)
      case (REGRID_METHOD_FRACTION)
         call ESMF_DynamicMaskSetR4R8R4V(this%dynamic_mask, &
              & dynamicSrcMaskValue=MAPL_undef, &
              & dynamicMaskRoutine=fractionDynMaskProcV, &
              & handleAllElements=.true., &
              & rc=rc)
         _VERIFY(status)
      case default
         _ASSERT(.false.)
      end select

      _RETURN(_SUCCESS)

   end subroutine initialize_subclass

end module MAPL_EsmfRegridderMod
