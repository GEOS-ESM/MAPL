#include "MAPL_Generic.h"

module MAPL_GetLatLonCoordMod
  use, intrinsic :: iso_fortran_env, only: REAL32
  use, intrinsic :: iso_fortran_env, only: REAL64
  use mapl_ErrorHandlingMod
  use MAPL_BaseMod, only: MAPL_GridGet
  use MAPL_CommsMod
  use esmf
  implicit none
  private

  public :: MAPL_GetLatLonCoord

  interface MAPL_GetLatLonCoord
    module procedure MAPL_GetLatLonCoord_REAL64
    module procedure MAPL_GetLatLonCoord_REAL32
  end interface

contains


   subroutine MAPL_GetLatLonCoord_REAL64(grid,dim,x,rc)
      real(kind=REAL64), intent(out)  :: x(:)
      type(ESMF_Grid), intent(inout) :: grid
      integer,           intent(in) :: dim
      integer, optional, intent(out) :: rc

      real(kind=REAL64), pointer :: x_2d(:,:) => null()
      real(kind=REAL64), allocatable :: xg(:,:)
      integer :: counts(3),mypet
      type(ESMF_DistGrid) :: distgrid
      type(ESMF_DeLayout) :: layout
      type(ESMF_VM) :: vm
      integer :: status

      call ESMF_GridGetCoord (grid, coordDim=dim, localDE=0, &
               staggerloc=ESMF_STAGGERLOC_CENTER, &
               farrayPtr=x_2d, rc=status)
      _VERIFY(status)
      call MAPL_GridGet(grid,globalCellCountPerDim=counts,rc=status)
      _VERIFY(status)

      allocate(xg(counts(1),counts(2)),stat=status)
      _VERIFY(status)

      call ArrayGather(x_2d,xg,grid,rc=status)
      _VERIFY(status)
      call ESMF_GridGet(GRID, DistGrid=distgrid, rc=status)
      _VERIFY(STATUS)
      call ESMF_DistGridGet(distgrid, DELayout=layout, rc=status)
      _VERIFY(STATUS)
      call ESMF_DELayoutGet(layout,vm=vm,rc=status)
      _VERIFY(status)
      call ESMF_VMGet(vm,localPet=myPet,rc=status)
      _VERIFY(status)
      if (dim==1) then
         if(myPet==0) x = xg(:,1)
      else if (dim==2) then
         if (myPet==0) x = xg(1,:)
      else
         _FAIL('unsupported rank > 2')
      end if
      call MAPL_CommsBcast(layout,x,size(x), 0, rc=status)
      _VERIFY(status)
      deallocate(xg)

   end subroutine MAPL_GetLatLonCoord_Real64

   subroutine MAPL_GetLatLonCoord_REAL32(grid,dim,x,rc)
      real(kind=REAL32), intent(out)  :: x(:)
      type(ESMF_Grid), intent(inout) :: grid
      integer,           intent(in) :: dim
      integer, optional, intent(out) :: rc

      real(kind=REAL64), pointer :: x_2d(:,:) => null()
      real(kind=REAL32), allocatable :: xg(:,:),xl(:,:)
      integer :: counts(3),mypet
      type(ESMF_DistGrid) :: distgrid
      type(ESMF_DeLayout) :: layout
      type(ESMF_VM) :: vm
      integer :: status

      call ESMF_GridGetCoord (grid, coordDim=dim, localDE=0, &
               staggerloc=ESMF_STAGGERLOC_CENTER, &
               farrayPtr=x_2d, rc=status)
      _VERIFY(status)
      call MAPL_GridGet(grid,globalCellCountPerDim=counts,rc=status)
      _VERIFY(status)

      allocate(xg(counts(1),counts(2)),stat=status)
      _VERIFY(status)
      allocate(xl(size(x_2d,1),size(x_2d,2)),stat=status)
      _VERIFY(status)
      xl=x_2d

      call ArrayGather(xl,xg,grid,rc=status)
      _VERIFY(status)
      deallocate(xl)
      call ESMF_GridGet(GRID, DistGrid=distgrid, rc=status)
      _VERIFY(STATUS)
      call ESMF_DistGridGet(distgrid, DELayout=layout, rc=status)
      _VERIFY(STATUS)
      call ESMF_DELayoutGet(layout,vm=vm,rc=status)
      _VERIFY(status)
      call ESMF_VMGet(vm,localPet=myPet,rc=status)
      _VERIFY(status)
      if (dim==1) then
         if(myPet==0) x = xg(:,1)
      else if (dim==2) then
         if (myPet==0) x = xg(1,:)
      else
         _FAIL('unsupported rank > 2')
      end if
      call MAPL_CommsBcast(layout,x,size(x), 0, rc=status)
      _VERIFY(status)
      deallocate(xg)

   end subroutine MAPL_GetLatLonCoord_Real32

end module MAPL_GetLatLonCoordMod
