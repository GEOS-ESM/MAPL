#include "MAPL_ErrLog.h"

submodule (mapl3g_VectorBasis) grid_get_corners_smod
   use mapl_Constants
   use esmf
   implicit none(type,external)
contains


   module subroutine grid_get_corners(grid, corners, rc)
      type(ESMF_Grid), intent(inout) :: grid
      real(kind=ESMF_KIND_R8), allocatable, intent(out) :: corners(:,:,:)
      integer, optional, intent(out) :: rc

      integer :: status
      integer :: im, jm
      real(kind=ESMF_KIND_R8), pointer :: longitudes(:,:)
      real(kind=ESMF_KIND_R8), pointer :: latitudes(:,:)
      real(kind=ESMF_KIND_R8), allocatable :: corner_lons(:,:)
      real(kind=ESMF_KIND_R8), allocatable :: corner_lats(:,:)

      call GridGetCoords(grid, longitudes, latitudes, _RC)
      im = size(longitudes,1)
      jm = size(longitudes,2)

      allocate(corner_lons(im+1,jm+1))
      allocate(corner_lats(im+1,jm+1))

      call legacy_get_corners(grid, corner_lons, corner_lats, _RC)

      allocate(corners(size(longitudes,1),size(longitudes,2),2))
      corners(:,:,1) = corner_lons
      corners(:,:,2) = corner_lats

      _RETURN(ESMF_SUCCESS)
   end subroutine grid_get_corners


   subroutine legacy_get_corners(grid, gridCornerLons, gridCornerLats, rc)
      type (ESMF_Grid), intent(INOUT) :: grid
      real(ESMF_KIND_R8), intent(INOUT) :: gridCornerLons(:,:)
      real(ESMF_KIND_R8), intent(INOUT) :: gridCornerLats(:,:)
      integer, optional, intent(  OUT) :: RC

      integer :: status

      type(ESMF_RouteHandle) :: rh
      type(ESMF_Field) :: field
      integer :: counts(3),lsz
      real(ESMF_KIND_R8), pointer :: ptr(:,:)
      real(ESMF_KIND_R8), pointer :: corner(:,:)
      integer :: im,jm,imc,jmc,idx,i,j
      logical :: hasLons,hasLats
      real(ESMF_KIND_R8), allocatable :: r8ptr(:),lons1d(:),lats1d(:)
      type(ESMF_CoordSys_Flag) :: coordSys
      type(ESMF_Info) :: infoh

      call esmf_GridGet(grid, &
           localDe=0, &
           staggerloc=ESMF_STAGGERLOC_CENTER, &
           exclusiveCount=counts, &
           _RC)
      im=counts(1)
      jm=counts(2)
      ! check if we have corners
      call ESMF_InfoGetFromHost(grid,infoh,_RC)
      hasLons = ESMF_InfoIsPresent(infoh,'GridCornerLons',_RC)
      hasLats = ESMF_InfoIsPresent(infoh,'GridCornerLats',_RC)
      if (hasLons .and. hasLats) then
         call ESMF_InfoGet(infoh,key='GridCornerLons',size=lsz,_RC)
         _ASSERT(size(gridCornerLons,1)*size(gridCornerLons,2)==lsz,"stored corner sizes to not match grid")
         call ESMF_InfoGet(infoh,key='GridCornerLats',size=lsz,_RC)
         _ASSERT(size(gridCornerLats,1)*size(gridCornerLats,2)==lsz,"stored corner sizes to not match grid")
         allocate(r8ptr(lsz),_STAT)

         call ESMF_InfoGet(infoh,key='GridCornerLons',values=r8ptr,_RC)

         idx = 0
         do j = 1, size(gridCornerLons,2)
            do i = 1, size(gridCornerLons,1)
               idx = idx+1
               gridCornerLons(i,j) = r8ptr(idx)
            end do
         end do

         call ESMF_InfoGet(infoh,key='GridCornerLats',values=r8ptr,_RC)

         idx = 0
         do j = 1, size(gridCornerLons,2)
            do i = 1, size(gridCornerLons,1)
               idx = idx+1
               gridCornerLats(i,j) = r8ptr(idx)
            end do
         end do
         deallocate(r8ptr)
      else

         call ESMF_GridGetCoord(grid,localDE=0,coordDim=1,staggerloc=ESMF_STAGGERLOC_CORNER, &
              farrayPtr=corner, _RC)
         imc=size(corner,1)
         jmc=size(corner,2)
         allocate(ptr(0:imc+1,0:jmc+1),source=0.0d0,_STAT)
         field = ESMF_FieldCreate(grid,ptr,staggerLoc=ESMF_STAGGERLOC_CORNER,totalLWidth=[1,1],totalUWidth=[1,1],_RC)
         call ESMF_FieldHaloStore(field,rh,_RC)

         ptr(1:imc,1:jmc)=corner
         call ESMF_FieldHalo(field,rh,_RC)
         gridCornerLons=ptr(1:im+1,1:jm+1)

         call ESMF_GridGetCoord(grid,localDE=0,coordDim=2,staggerloc=ESMF_STAGGERLOC_CORNER, &
              farrayPtr=corner, _RC)
         ptr(1:imc,1:jmc)=corner
         call ESMF_FieldHalo(field,rh,_RC)
         gridCornerLats=ptr(1:im+1,1:jm+1)

         deallocate(ptr)
         call ESMF_FieldDestroy(field,_RC)
         call ESMF_FieldHaloRelease(rh,_RC)

         call ESMF_GridGet(grid,coordSys=coordSys,_RC)
         if (coordSys==ESMF_COORDSYS_SPH_DEG) then
            gridCornerLons=gridCornerLons*MAPL_DEGREES_TO_RADIANS_R8
            gridCornerLats=gridCornerLats*MAPL_DEGREES_TO_RADIANS_R8
         else if (coordSys==ESMF_COORDSYS_CART) then
            _FAIL('Unsupported coordinate system:  ESMF_COORDSYS_CART')
         end if
         allocate(lons1d(size(gridCornerLons,1)*size(gridCornerLons,2)),_STAT)
         allocate(lats1d(size(gridCornerLons,1)*size(gridCornerLons,2)),_STAT)
         idx = 0
         do j=1,size(gridCornerLons,2)
            do i=1,size(gridCornerLons,1)
               idx=idx+1
               lons1d(idx)=gridCornerLons(i,j)
               lats1d(idx)=gridCornerLats(i,j)
            enddo
         enddo
         call ESMF_InfoGetFromHost(grid,infoh,_RC)
         call ESMF_InfoSet(infoh,key='GridCornerLons:',values=lons1d,_RC)
         call ESMF_InfoSet(infoh,key='GridCornerLats:',values=lats1d,_RC)
         deallocate(lons1d,lats1d)
      end if

      _RETURN(ESMF_SUCCESS)

   end subroutine Legacy_Get_Corners


end submodule grid_get_corners_smod
