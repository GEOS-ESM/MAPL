#define I_AM_MAIN
#include "MAPL_Generic.h"

module traj
  use ESMF
  use MAPL_CommsMod
  use LocStreamFactoryMod
  public :: mytraj

  type:: mytraj
     type(LocStreamFactory) :: locstream_factory
     type(ESMF_LocStream)   :: LS_rt  !  on root
     type(ESMF_LocStream)   :: LS_ds  !  on MPI processors
  end type mytraj

  interface mytraj
     module procedure traj_from_latlon
  end interface mytraj

contains
  function traj_from_latlon(rc) result(newtraj)
    integer, optional, intent(out) :: rc
    type(mytraj) :: newtraj
    integer :: status
    integer :: nx, ny
    integer (kind=ESMF_KIND_I8) :: N0, NPTS
    real*8, allocatable :: lat1(:), lon1(:)
    real*8, allocatable :: lat2(:), lon2(:)
    integer :: iseed
    integer :: mypet
    real :: pi=3.14159265857979    

    nx=500
    ny=500
    NPTS = nx*ny
    mypet=0
    
    ! LS root
    ! -------------------
    if (mapl_am_I_root()) then
       allocate (lat1(NPTS), lon1(NPTS))   ! LS 
       !
       iseed= -1
       call gen_rand_arrayR(NPTS, lat1, iseed)
       call gen_rand_arrayR(NPTS, lon1, iseed)
       !! write(6, 101)  'lat1', lat1
       lon1 = 2.d0 * pi * lon1
       lat1 = pi * lat1
       lat1 = lat1 - pi/2.d0
    else
       NPTS=0
       allocate(lat1(0), lon1(0))
    endif
    
    if (mapl_am_I_root()) then
!       write(6, 141) 'pet, lon1', mypet, lon1(:)  
!       write(6, 141) 'pet, lat1', mypet, lat1(:)
    endif


    newtraj%locstream_factory = LocStreamFactory(lon1,lat1,rc=rc)
    newtraj%LS_rt = newtraj%locstream_factory%create_locstream(rc=rc)
    newtraj%LS_ds = newtraj%locstream_factory%create_locstream(rc=rc)
    
    if (present(rc))  rc=0
    
101 format (2x, a,10(2x,f15.8))
141 format (2x, a, 2x, i8, 10(2x,f15.8))    
  end function traj_from_latlon

end module traj


program main
  use ESMF
  use MAPL
  use MAPL_ErrorHandlingMod
  use MAPL_LatLonGridFactoryMod
  use traj

  implicit none
  real, parameter :: pi=3.14159265857979

  integer :: NPTS
  integer :: nx, ny

  ! Local variables
  integer :: rc, status, finalrc, result
  
  type(ESMF_VM)                               :: vm
  type(ESMF_RouteHandle)                      :: routehandle
  integer                                     :: localrc, localPet

  type(mytraj) :: hist_traj

  ! app.
  real*8, allocatable :: lat1(:), lon1(:)
  real*8, allocatable :: lat2(:), lon2(:)  

  type(LatLonGridFactory) :: factory
  type(ESMF_Grid) :: llgrid, llgrid2
  type(LocStreamFactory) :: locstream_factory
  type(ESMF_LocStream) :: LS_rt  !  root
  
  character(len=20) :: grid_name
  integer :: N0
  integer :: iseed
  integer :: i, j, k
  integer :: mypet, npet

  integer :: im_world
  integer :: jm_world
  integer :: lm
  real    :: mem_total, mem_commit, mem_percent
  
  call ESMF_Initialize(defaultlogfilename="FieldRedistEx.Log", rc=rc)
  call ESMF_VMGetCurrent(vm, rc=rc)
  call ESMF_VMGet(vm,localPet=mypet,petCount=nPet,rc=rc)
!!  call MAPL_Initialize(rc=rc)
  
  grid_name="LL"
  im_world=2000
  jm_world=1000
  lm=3
  nx=2
  ny=2
  factory = LatLonGridFactory (grid_name=grid_name, im_world=im_world, jm_world=jm_world, &
       lm=lm, nx=nx, ny=ny, pole='PC',dateline='DC',_RC)
  llgrid = factory%make_grid(_RC)

  
  hist_traj = mytraj(_RC)
  call hist_traj%locstream_factory%destroy_locstream( hist_traj%LS_rt, _RC)
  call hist_traj%locstream_factory%destroy_locstream( hist_traj%LS_ds, _RC)

  
  do i=1, 10
     write(6,*) 'it =', i
     call MAPL_MemCommited ( mem_total, mem_commit, mem_percent, RC=STATUS )
     if (mapl_am_I_root()) write(6,1000) mem_percent

     hist_traj = mytraj(_RC)
     write(6,*) 'create 1'
     hist_traj%LS_rt = hist_traj%locstream_factory%create_locstream(_RC)
     write(6,*) 'create 2'     
     hist_traj%LS_ds = hist_traj%locstream_factory%create_locstream(grid=llgrid,_RC)

     write(6,*) 'destroy'
!     call ESMF_GridDestroy(llgrid2, rc=status)
     call hist_traj%locstream_factory%destroy_locstream( hist_traj%LS_rt, _RC)
     call hist_traj%locstream_factory%destroy_locstream( hist_traj%LS_ds, _RC)
     call ESMF_VMBarrier(vm, rc=status)     
  enddo


  call ESMF_FInalize()

101 format (2x, a,10(2x,f15.8))
141 format (2x, a, 2x, i8, 10(2x,f15.8))
1000 format(1x,'check',2x,f7.3,'%Memory Committed')
  
end program main
