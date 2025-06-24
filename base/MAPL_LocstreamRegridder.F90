#include "MAPL_ErrLog.h"
#include "unused_dummy.H"

module MAPL_LocstreamRegridderMod
   use, intrinsic :: iso_fortran_env, only: REAL32, REAL64
   use ESMF
   use MAPL_KeywordEnforcerMod
   use MAPL_ErrorHandlingMod

   implicit none
   private

   public :: LocstreamRegridder

   type :: LocstreamRegridder
       type(ESMF_RouteHandle) :: route_handle
       type(ESMF_Grid) :: grid
       type(ESMF_Locstream) :: locstream
       contains
          procedure :: regrid_2d_real32
          procedure :: regrid_3d_real32
          generic :: regrid => regrid_2d_real32
          generic :: regrid => regrid_3d_real32
          procedure :: destroy => destroy_route_handle
   end type LocstreamRegridder

   interface LocstreamRegridder
      module procedure new_LocstreamRegridder
   end interface LocstreamRegridder

contains

   function new_LocstreamRegridder(grid,locstream,unusable,regrid_method,rc) result(regridder)
      type(LocstreamRegridder) :: regridder
      type(ESMF_Grid), intent(inout) :: grid
      type(ESMF_Locstream), intent(inout) :: locstream
      class (KeywordEnforcer), optional, intent(in) :: unusable
      type(ESMF_RegridMethod_Flag), optional, intent(in) :: regrid_method
      integer, optional, intent(out) :: rc

      type(ESMF_RegridMethod_Flag) :: local_regrid_method
      type(ESMF_Field) :: src_field, dst_field
      real, pointer :: pt2d(:,:), pt1d(:)
      integer :: status

      __UNUSED_DUMMY(unusable)
      if (present(regrid_method)) then
         local_regrid_method = regrid_method
      else
         local_regrid_method = ESMF_REGRIDMETHOD_NEAREST_STOD
      end if

      src_field = ESMF_FieldCreate(grid,typekind=ESMF_TYPEKIND_R4,gridToFieldMap=[1,2],rc=status)
      __VERIFY(status)
      dst_field = ESMF_FieldCreate(locstream,typekind=ESMF_TYPEKIND_R4,gridToFieldMap=[1],rc=status)
      __VERIFY(status)

      call ESMF_FieldGet(src_field, localDE=0, farrayPtr=pt2d, __RC)
      call ESMF_FieldGet(dst_field, localDE=0, farrayPtr=pt1d, __RC)
      pt2d = 0.0
      pt1d = 0.0

      call ESMF_FieldRegridStore(srcField=src_field,dstField=dst_field, &
           routeHandle=regridder%route_handle,regridmethod=local_regrid_method,rc=status)
      __VERIFY(status)
      call ESMF_FieldDestroy(src_field,noGarbage=.true.,rc=status)
      __VERIFY(status)
      call ESMF_FieldDestroy(dst_field,noGarbage=.true.,rc=status)
      __VERIFY(status)
      regridder%grid=grid
      regridder%locstream=locstream

      __RETURN(__SUCCESS)
 
   end function new_LocstreamRegridder

   subroutine regrid_2d_real32(this,q_in,q_out, rc)
      class(LocstreamRegridder), intent(inout) :: this
      real(kind=REAL32), intent(in) :: q_in(:,:)
      real(kind=REAL32), intent(out) :: q_out(:)
      integer, optional, intent(out) :: rc

      integer :: status

      type(ESMF_Field) :: dst_field,src_field
      real(kind=REAL32), pointer :: p_src(:,:),p_dst(:)
    
      dst_field=ESMF_FieldCreate(this%locstream,typekind=ESMF_TYPEKIND_R4, &
           gridToFieldMap=[1],rc=status)
      __VERIFY(status)      
      src_field=ESMF_FieldCreate(this%grid,typekind=ESMF_TYPEKIND_R4, &
           gridToFieldMap=[1,2],rc=status)
      __VERIFY(status)      

      call ESMF_FieldGet(src_field,localDE=0,farrayPtr=p_src,rc=status)
      __VERIFY(status)
      call ESMF_FieldGet(dst_field,localDE=0,farrayPtr=p_dst,rc=status)
      __VERIFY(status)

      p_src=q_in
      call ESMF_FieldRegrid(src_field,dst_field,this%route_handle,rc=status)
      __VERIFY(status)
      q_out=p_dst
      call ESMF_FieldDestroy(dst_field,noGarbage=.true.,rc=status)
      __VERIFY(status)
      call ESMF_FieldDestroy(src_field,noGarbage=.true.,rc=status)
      __VERIFY(status)

   end subroutine regrid_2d_real32

   subroutine regrid_3d_real32(this,q_in,q_out, rc)
      class(LocstreamRegridder), intent(inout) :: this
      real(kind=REAL32), intent(in) :: q_in(:,:,:)
      real(kind=REAL32), intent(out) :: q_out(:,:)
      integer, optional, intent(out) :: rc

      integer :: status
      integer :: lm

      type(ESMF_Field) :: dst_field,src_field
      real(kind=REAL32), pointer :: p_src(:,:,:),p_dst(:,:)
      __ASSERT(size(q_in,3)==size(q_out,2),"Input and output arrays size inconsistent in 3D locstream regridder")

      lm = size(q_in,3)
    
      dst_field=ESMF_FieldCreate(this%locstream,typekind=ESMF_TYPEKIND_R4, &
           gridToFieldMap=[2],ungriddedLBound=[1],ungriddedUBound=[lm],rc=status)
      __VERIFY(status)      
      src_field=ESMF_FieldCreate(this%grid,typekind=ESMF_TYPEKIND_R4, &
           gridToFieldMap=[2,3],ungriddedLBound=[1],ungriddedUBound=[lm],rc=status)
      __VERIFY(status)      

      call ESMF_FieldGet(src_field,localDE=0,farrayPtr=p_src,rc=status)
      __VERIFY(status)
      call ESMF_FieldGet(dst_field,localDE=0,farrayPtr=p_dst,rc=status)
      __VERIFY(status)

      p_src= reshape(q_in,shape(p_src), order = [2,3,1])
      call ESMF_FieldRegrid(src_field,dst_field,this%route_handle,rc=status)
      __VERIFY(status)
      q_out=reshape(p_dst, shape(q_out), order=[2,1])
      call ESMF_FieldDestroy(dst_field,noGarbage=.true.,rc=status)
      __VERIFY(status)
      call ESMF_FieldDestroy(src_field,noGarbage=.true.,rc=status)
      __VERIFY(status)

   end subroutine regrid_3d_real32


   subroutine destroy_route_handle(this,rc) 
     class(LocstreamRegridder) :: this
     integer, optional, intent(out) :: rc     
     integer :: status
     
     call ESMF_RouteHandleDestroy(this%route_handle, noGarbage=.true., __RC)
     call ESMF_LocStreamDestroy (this%locstream, noGarbage=.true., __RC)
      
     __RETURN(__SUCCESS)
     
   end subroutine destroy_route_handle
   

end module MAPL_LocstreamRegridderMod
