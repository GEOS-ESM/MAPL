#include "MAPL_Generic.h"

program test_locstream

use ESMF
use MAPL

implicit NONE

type(ESMF_Grid) :: grid
class(AbstractGridFactory), allocatable :: factory
integer :: mypet, seed_size, i,status
type(ESMF_VM) :: vm
real(kind=ESMF_KIND_R8), allocatable :: lons(:), lats(:)
integer, allocatable :: seeds(:)
integer :: npoints,local_count,ls_size

type(ESMF_FIeld) :: field_root, field_dist
type(ESMF_Locstream) :: ls_root, ls_dist
real, pointer :: ptr(:),ptr_loc(:)
type(ESMF_RouteHandle) :: rh

call ESMF_Initialize (LogKindFlag=ESMF_LOGKIND_NONE,vm=vm)

call ESMF_VMGet(vm,localPet=mypet)

npoints = 100

if (mypet==0) then
allocate(lons(npoints),lats(npoints))
local_count=npoints
else
allocate(lons(0),lats(0))
local_count=0
end if

call random_seed(size=seed_size)
allocate(seeds(seed_size))

call random_seed(put=seeds)

if (mypet==0) then
call random_number(lons)
call random_number(lats)
lons=lons*360.0d0
lats=lats*180.0d0
lats=lats-90.d0
lats = lats*MAPL_DEGREES_TO_RADIANS_R8
lons = lons*MAPL_DEGREES_TO_RADIANS_R8
end if

ls_root = ESMF_LocStreamCreate(localCount=local_count,coordSys=ESMF_COORDSYS_SPH_RAD)
call ESMF_LocStreamAddKey(ls_root,keyName="ESMF:Lat",farray=lats,datacopyflag=ESMF_DATACOPY_VALUE, &
      keyUnits="Radians", keyLongName="Latitude",rc=status)
call ESMF_LocStreamAddKey(ls_root,keyName="ESMF:Lon",farray=lons,datacopyflag=ESMF_DATACOPY_VALUE, &
     keyUnits="Radians", keyLongName="Longitude",rc=status)

factory=LatLonGridFactory(im_world=144,jm_world=91,lm=2,nx=2,ny=2,pole='PC',dateline='DC')
grid = factory%make_grid()
ls_dist = ESMF_LocStreamCreate(ls_root,background=grid,rc=status)
field_root = ESMF_FieldCreate(ls_root,ESMF_TYPEKIND_R4)
field_dist = ESMF_FieldCreate(ls_dist,ESMF_TYPEKIND_R4)

call ESMF_FIeldGet(field_dist,0,farrayPtr=ptr)
call ESMF_FieldGet(field_root,0,farrayPtr=ptr_loc)
if (mypet==0) then
do i=1,npoints
  ptr_loc(i)=i
enddo
end if
call ESMF_FieldRedistStore(field_root,field_dist,rh)
call ESMF_FieldRedist(field_root,field_dist,rh)

do i=1,size(ptr)
write(*,*)i,mypet,ptr(i)
enddo
call ESMF_FInalize()

end program
