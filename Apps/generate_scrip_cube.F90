#define I_AM_MAIN
#include "MAPL_Generic.h"

    program ESMF_GenerateCSGridDescription

! ESMF Framework module
    use ESMF
    use MAPL
    use mpi
    use netcdf
    use, intrinsic :: iso_fortran_env, only: REAL64,REAL32

    implicit none
    
! Local variables to be used in the Regrid method calls.
! The code creating and filling these variables is not included in the
! example documentation because those interfaces are not specific to
! Regrid.
    type(ESMF_Grid) ::  dstgrid
    integer :: rc

!EOC

    real(REAL64), parameter           :: PI = 3.14159265358979323846
    integer                           :: npets, localPet
    integer                           :: i, j, k
    type(ESMF_VM)                     :: vm
    integer                           :: IM_World, JM_World, scrip_size
    integer, parameter                :: grid_type = 0
    integer, parameter                :: KM_WORLD=1
    integer, parameter                :: NX=1
    integer, parameter                :: NY=6
    integer, parameter                :: ntiles=6
    integer, parameter                :: ndims=2
    integer                           :: N
    integer                           :: info
    integer                           :: start(2), cnt(2), hull_num, hull(4)
    integer                           :: UNIT
    real(ESMF_KIND_R8), allocatable   :: grid_global(:,:,:,:)
    real(ESMF_KIND_R8), allocatable   :: SCRIP_CenterLat(:), SCRIP_CenterLon(:)
    real(ESMF_KIND_R8), allocatable   :: SCRIP_CornerLat(:,:), SCRIP_CornerLon(:,:)
    real(ESMF_KIND_R8), allocatable   :: SCRIP_Area(:)
    real(ESMF_KIND_R8)                :: node_xy(2,4), node_xy_tmp(2,4), lon_e, lon_w
    integer                           :: cornerdim, gridsize, griddim, rankdim, mask
    integer                           :: cornerlon, cornerlat, centerlon, centerlat,cellarea
    integer, allocatable              :: IMS(:,:), JMS(:,:), sendData(:), GlobalCounts(:), recvCounts(:), recvOffsets(:)
    integer, allocatable              :: grid_imask(:)
    character(len=ESMF_MAXSTR)        :: gridname, FMT, title
    integer                           :: NPX, NPY
    integer                           :: isg, ieg
    integer                           :: jsg, jeg
    integer                           :: is, ie
    integer                           :: js, je
    integer                           :: myTile
    integer                           :: npts, tmp, mpiC
    integer                           :: IG, JG
    logical                           :: do_schmidt
    real(ESMF_KIND_R8)                :: target_lon, target_lat, stretch_fac
    type(ESMF_Config)                 :: CF
    integer                           :: status,error_code
    real(ESMF_KIND_R8), pointer :: center_lons(:,:),center_lats(:,:),corner_lons(:,:),corner_lats(:,:)

    call ESMF_Initialize(logKindFlag=ESMF_LOGKIND_NONE,rc=status)
    _VERIFY(status)

    call ESMF_VMGetGlobal(vm, rc=status)
    _VERIFY(status)

! Get number of PETs we are running with
! --------------------------------------
    call ESMF_VMGet(vm, localPet=localPet, petCount=npets, mpiCommunicator=mpiC, rc=status)
    _VERIFY(status)
    _ASSERT(npets == 6, "must run on 6 mpi processes")

    CF = ESMF_ConfigCreate(rc=status)
    _VERIFY(STATUS)
    call ESMF_ConfigLoadFile(CF,filename='GenScrip.rc',rc=status)
    _VERIFY(STATUS)
    call ESMF_ConfigGetAttribute(CF, IM_World, Label='CUBE_DIM:',rc=status)
    _VERIFY(STATUS)
    JM_WORLD = 6 * IM_WORLD
    call ESMF_ConfigGetAttribute(CF,gridname, label='gridname:',rc=status)
    _VERIFY(STATUS)
    call ESMF_ConfigGetAttribute(CF, do_schmidt, Label='DO_SCHMIDT:',Default=.false.,rc=status)
    _VERIFY(STATUS)
    call ESMF_ConfigGetAttribute(CF, stretch_fac, Label='STRETCH_FAC:',rc=status)
    if (status /=0) then 
       if (do_schmidt) then
          write(*,*)"Asking for stretch grid without supplying stretch factor"
          call MPI_Abort(mpiC,error_code,status)
       end if
    end if
    call ESMF_ConfigGetAttribute(CF, target_lon, Label='TARGET_LON:',rc=status)
    if (status /=0) then 
       if (do_schmidt) then
          write(*,*)"Asking for stretch grid without supplying target lon"
          call MPI_Abort(mpiC,error_code,status)
       end if
    end if
    call ESMF_ConfigGetAttribute(CF, target_lat, Label='TARGET_LAT:',rc=status)
    if (status /=0) then 
       if (do_schmidt) then
          write(*,*)"Asking for stretch grid without supplying target lat"
          call MPI_Abort(mpiC,error_code,status)
       end if
    end if

    allocate(ims(1,6),jms(1,6))
    do i=1,ntiles
       ims(1,i)=im_world
       jms(1,i)=im_world
    enddo

    if (do_schmidt) then
 
    else
       dstgrid = ESMF_GridCreateCubedSphere(im_world,ims,jms,name="cubed_sphere", &
       staggerLocList = [ESMF_STAGGERLOC_CENTER,ESMF_STAGGERLOC_CORNER], &
       coordSys=ESMF_COORDSYS_SPH_RAD, _RC)
    end if
    call ESMF_GridGetCoord(dstgrid, coordDim=1, localDE=0, &
        staggerloc=ESMF_STAGGERLOC_CENTER, &
        farrayPtr=center_lons, rc=status)
        _VERIFY(status)
    call ESMF_GridGetCoord(dstgrid, coordDim=2, localDE=0, &
        staggerloc=ESMF_STAGGERLOC_CENTER, &
        farrayPtr=center_lats, rc=status)
        _VERIFY(status)
    call ESMF_GridGetCoord(dstgrid, coordDim=1, localDE=0, &
        staggerloc=ESMF_STAGGERLOC_CORNER, &
        farrayPtr=corner_lons, rc=status)
        _VERIFY(status)
    call ESMF_GridGetCoord(dstgrid, coordDim=2, localDE=0, &
        staggerloc=ESMF_STAGGERLOC_CORNER, &
        farrayPtr=corner_lats, rc=status)
        _VERIFY(status)

    call MAPL_grid_interior(dstgrid, isg, ieg, jsg, jeg)
    npx = IM_WORLD
    npy = JM_WORLD
    myTile = jsg/(npy/ntiles)

    is = isg
    ie = ieg
    js = jsg - myTile*(npy/ntiles)
    je = jeg - myTile*(npy/ntiles)
    npts = (npy/ntiles)
    if (npts /= npx) then
       print*, 'Error npts /= npx', npts, npx
       status=1
    endif
    _VERIFY(status)

    allocate( grid_global(npts+1,npts+1,ndims,ntiles) )
    grid_global(:,:,1,localPet+1) = corner_lons
    grid_global(:,:,2,localPet+1) = corner_lats


    tmp = (ieg-isg+1)*(jeg-jsg+1)
    allocate(SCRIP_CenterLat(tmp),stat=status)
    _VERIFY(status)
    allocate(SCRIP_CenterLon(tmp),stat=status)
    _VERIFY(status)
    allocate(SCRIP_CornerLat(4,tmp),stat=status)
    _VERIFY(status)
    allocate(SCRIP_CornerLon(4,tmp),stat=status)
    _VERIFY(status)
    allocate(SCRIP_Area(tmp),stat=status)
    _VERIFY(status)

    n=1
    do jg=jsg,jeg
       do ig=isg,ieg
          i=ig
          j=jg-myTile*npts
          SCRIP_CenterLon(n) = center_lons(i,j)*(180._8/MAPL_PI_R8)
          SCRIP_CenterLat(n) = center_lats(i,j)*(180._8/MAPL_PI_R8)

          node_xy(1,1:4) = (/grid_global(i  ,j,1,myTile+1),grid_global(i+1,j,1,myTile+1),grid_global(i+1,j+1,1,myTile+1),grid_global(i,j+1,1,myTile+1)/)
          node_xy(2,1:4) = (/grid_global(i  ,j,2,myTile+1),grid_global(i+1,j,2,myTile+1),grid_global(i+1,j+1,2,myTile+1),grid_global(i,j+1,2,myTile+1)/)
          node_xy_tmp = node_xy

! Correct for the periodic boundary at 0/360
! ------------------------------------------
          lon_w = min( grid_global(i  ,j,1,myTile+1),grid_global(i+1,j,1,myTile+1),grid_global(i+1,j+1,1,myTile+1),grid_global(i,j+1,1,myTile+1) ) 
          lon_e = max( grid_global(i  ,j,1,myTile+1),grid_global(i+1,j,1,myTile+1),grid_global(i+1,j+1,1,myTile+1),grid_global(i,j+1,1,myTile+1) ) 
          if ( abs(lon_e - lon_w) > 1.5_8*MAPL_PI_R8 .and. (SCRIP_CenterLon(n) < MAPL_PI_R8) ) then
             where(node_xy(1,:) > MAPL_PI_R8) node_xy_tmp(1,:) = node_xy(1,:) - 2._8*MAPL_PI_R8
          elseif ( abs(lon_e - lon_w) > 1.5_8*MAPL_PI_R8 .and. (SCRIP_CenterLon(n) > MAPL_PI_R8) ) then
             where(node_xy(1,:) < MAPL_PI_R8) node_xy_tmp(1,:) = node_xy(1,:) + 2._8*MAPL_PI_R8
          endif
          call points_hull_2d(4, node_xy_tmp, hull_num, hull)
          if(ANY(hull==0)) then
             write(*,100)'Zero Hull ', grid_global(i  ,j,1,myTile+1),grid_global(i+1,j,1,myTile+1),grid_global(i+1,j+1,1,myTile+1),grid_global(i,j+1,1,myTile+1)
             write(*,100)'Zero Hull ', node_xy_tmp(1,:)
          endif
          do k=1,4
            SCRIP_CornerLon(k,n) = node_xy(1,hull(k))*(180._8/MAPL_PI_R8)
            SCRIP_CornerLat(k,n) = node_xy(2,hull(k))*(180._8/MAPL_PI_R8) 
          enddo
          SCRIP_Area(n) = get_area_great_circles(grid_global(i  ,j  ,1:2,myTile+1), grid_global(i,j+1  ,1:2,myTile+1),   &
                            grid_global(i+1,j,1:2,myTile+1), grid_global(i+1,j+1,1:2,myTile+1))
          n=n+1
       enddo
    enddo

 100     format(a,4f20.15)
 101     format(a,f20.15)
 102     format(2f20.15)
 103     format(a)

    deallocate( grid_global )
    deallocate( IMS )
    deallocate( JMS )

    scrip_size = IM_World*JM_World
    call MPI_Info_create(info, status)
    _VERIFY(status)
    call MPI_Info_set(info, "cb_buffer_size", "1048576", status)
    _VERIFY(status)

    status = nf90_create(trim(gridname), IOR(NF90_MPIIO,IOR(NF90_CLOBBER,NF90_NETCDF4)), unit, comm=mpiC, info=info)
    _VERIFY(status)

    FMT = '(A,' // ',A,' //',A)'
    write(title,trim(FMT)) 'GMAO ',trim(gridname),' Grid'
    status = nf90_put_att(UNIT, NF90_GLOBAL, 'title',trim(title))
    _VERIFY(status)
    status = nf90_put_att(UNIT, NF90_GLOBAL, 'GridDescriptionFormat', 'SCRIP')
    _VERIFY(status)
    status = NF90_DEF_DIM(UNIT, 'grid_size'  , SCRIP_size, gridsize)
    _VERIFY(status)
    status = NF90_DEF_DIM(UNIT, 'grid_corners'  , 4, cornerdim)
    _VERIFY(status)
!! Peggy Li suggested setting grid_rank=1 and grid_dims=1
!! so that ESMF will treat the grid as unstructured
! ------------------------------------------------------
    status = NF90_DEF_DIM(UNIT, 'grid_rank'  , 1, rankdim)
    _VERIFY(status)

!! Grid dimensions
!! ---------------
    status = nf90_def_var(UNIT, "grid_dims", NF90_INT, [rankdim], griddim)
    _VERIFY(status)

!! Grid mask
!! ---------
    status = nf90_def_var(UNIT, "grid_imask", NF90_INT, [gridsize], mask)
    _VERIFY(status)
    status = nf90_put_att(UNIT, mask, "units"    , "unitless")
    _VERIFY(status)

!! cell center Longitude variable
!! ------------------------------
    status = nf90_def_var(UNIT, "grid_center_lon", NF90_DOUBLE, [gridsize], centerlon)
    _VERIFY(status)
    status = nf90_put_att(UNIT, centerlon, "units"    , "degrees")
    _VERIFY(status)

!! cell center Latitude variable
!! -----------------------------
    status = nf90_def_var(UNIT, "grid_center_lat", NF90_DOUBLE,  [gridsize], centerlat)
    _VERIFY(status)
    status = nf90_put_att(UNIT, centerlat, "units"    , "degrees")
    _VERIFY(status)

!! cell corner Longitude variable
!! ------------------------------
    status = nf90_def_var(UNIT, "grid_corner_lon", NF90_DOUBLE, [cornerdim,gridsize], cornerlon)
    _VERIFY(status)
    status = nf90_put_att(UNIT, cornerlon, "units"    , "degrees")
    _VERIFY(status)

!! cell corner Latitude variable
!! -----------------------------
    status = nf90_def_var(UNIT, "grid_corner_lat", NF90_DOUBLE, [cornerdim,gridsize], cornerlat)
    _VERIFY(status)
    status = nf90_put_att(UNIT, cornerlat, "units"    ,"degrees")
    _VERIFY(status)
    status = nf90_def_var(UNIT, "grid_area", NF90_DOUBLE, [gridsize], cellarea)
    _VERIFY(status)
    status = nf90_put_att(UNIT, cellarea, "units"    ,"radians^2")
    _VERIFY(status)

    status = nf90_enddef(UNIT)
    _VERIFY(status)

    rc = NF90_PUT_VAR(UNIT, griddim, [1])

    allocate (sendData(1),GlobalCounts(npets), recvCounts(npets), recvOffsets(npets), stat=status)
    _VERIFY(status)
    sendData = tmp
    recvCounts=1
    recvOffsets=0
    do i=2, npets
      recvOffsets(i) = recvOffsets(i-1) + recvCounts(i-1)
    enddo
    call ESMF_VMGatherV(vm,sendData=sendData,sendCount=1,recvData=GlobalCounts,recvCounts=recvCounts,recvOffsets=recvOffsets,rootPet=0,rc=status)
    _VERIFY(status)
    call ESMF_VMBroadcast(vm,bcstData=GlobalCounts,count=npets,rootPet=0, rc=status)
    _VERIFY(status)

    start=1
    do i=1,localPet
       start(1) = start(1)+GlobalCounts(i)
    enddo

    cnt(1) = tmp; cnt(2)=1
    status = NF90_PUT_VAR(UNIT, centerlon, SCRIP_CenterLon, start, cnt)
    _VERIFY(status)

    status = NF90_PUT_VAR(UNIT, centerlat, SCRIP_CenterLat, start, cnt)
    _VERIFY(status)

    status = NF90_PUT_VAR(UNIT, cellarea, SCRIP_Area,start, cnt)
    _VERIFY(status)
!! Grid mask
!! ---------
    allocate(grid_imask(tmp), stat=status)
    _VERIFY(status)
    grid_imask = 1
    status = NF90_PUT_VAR(UNIT, mask, grid_imask, start, cnt)
    _VERIFY(status)
    deallocate(grid_imask)

    start(2)=start(1)
    start(1)=1
    cnt(1)=4
    cnt(2)=tmp

    status = NF90_PUT_VAR(UNIT, cornerlat, SCRIP_CornerLat, start, cnt)
    _VERIFY(STATUS)

    status = NF90_PUT_VAR(UNIT, cornerlon, SCRIP_CornerLon, start, cnt)
    _VERIFY(STATUS)

    status = NF90_CLOSE(UNIT)
    _VERIFY(STATUS)
    call ESMF_VMBarrier(vm, rc=status)

    deallocate(SCRIP_CenterLat)
    deallocate(SCRIP_CenterLon)
    deallocate(SCRIP_CornerLat)
    deallocate(SCRIP_CornerLon)
    deallocate(sendData)
    deallocate(GlobalCounts)
    deallocate(recvCounts)
    deallocate(recvOffsets)

    call ESMF_Finalize(rc=status)

    contains

subroutine angle_rad_2d ( p1, p2, p3, res )
!*****************************************************************************80
!
!! ANGLE_RAD_2D returns the angle swept out between two rays in 2D.
!
!  Discussion:
!
!    Except for the zero angle case, it should be true that
!
!      ANGLE_RAD_2D ( P1, P2, P3 ) + ANGLE_RAD_2D ( P3, P2, P1 ) = 2 * PI
!
!        P1
!        /
!       /
!      /
!     /
!    P2--------->P3
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    15 January 2005
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( REAL64 ) P1(2), P2(2), P3(2), define the rays
!    P1 - P2 and P3 - P2 which define the angle.
!
!    Output, real ( REAL64 ) ANGLE_RAD_2D, the angle swept out by the rays,
!    in radians.  0 <= ANGLE_RAD_2D < 2 * PI.  If either ray has zero
!    length, then ANGLE_RAD_2D is set to 0.
  implicit none

  integer, parameter :: dim_num = 2

  real    (REAL64), parameter :: pi = 3.141592653589793D+00
  real    (REAL64) p(dim_num)
  real    (REAL64) p1(dim_num)
  real    (REAL64) p2(dim_num)
  real    (REAL64) p3(dim_num)
  real    (REAL64) res

  p(1) = ( p3(1) - p2(1) ) * ( p1(1) - p2(1) ) &
       + ( p3(2) - p2(2) ) * ( p1(2) - p2(2) )


  p(2) = ( p3(1) - p2(1) ) * ( p1(2) - p2(2) ) &
       - ( p3(2) - p2(2) ) * ( p1(1) - p2(1) )

  if ( p(1) == 0.0D+00 .and. p(2) == 0.0D+00 ) then
    res = 0.0D+00
    return
  end if

  res = atan2 ( p(2), p(1) )

  if ( res < 0.0D+00 ) then
    res = res + 2.0D+00 * pi
  end if

  return
end

subroutine points_hull_2d ( node_num, node_xy, hull_num, hull )

!*****************************************************************************80
!
!! POINTS_HULL_2D computes the convex hull of 2D points.
!
!  Discussion:
!
!    The work involved is N*log(H), where N is the number of points, and H is
!    the number of points that are on the hull.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    12 June 2006
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer NODE_NUM, the number of nodes.
!
!    Input, real ( REAL64 ) NODE_XY(2,NODE_NUM), the coordinates of the nodes.
!
!    Output, integer  HULL_NUM, the number of nodes that lie on
!    the convex hull.
!
!    Output, integer  HULL(NODE_NUM).  Entries 1 through HULL_NUM
!    contain the indices of the nodes that form the convex hull, in order.
!
  implicit none

  integer node_num
  real    (REAL64) angle
  real    (REAL64) angle_max
  real    (REAL64) di
  real    (REAL64) dr
  integer  first
  integer  hull(node_num)
  integer  hull_num
  integer  i
  real    (REAL64) node_xy(2,node_num)
  real    (REAL64) p_xy(2)
  integer  q
  real    (REAL64) q_xy(2)
  integer  r
  real    (REAL64) r_xy(2)

  if ( node_num < 1 ) then
    hull_num = 0
    return
  end if
!
!  If NODE_NUM = 1, the hull is the point.
!
  if ( node_num == 1 ) then
    hull_num = 1
    hull(1) = 1
    return
  end if
!
!  If NODE_NUM = 2, then the convex hull is either the two distinct points,
!  or possibly a single (repeated) point.
!
  if ( node_num == 2 ) then

    if ( node_xy(1,1) /= node_xy(1,2) .or. node_xy(2,1) /= node_xy(2,2) ) then
      hull_num = 2
      hull(1) = 1
      hull(2) = 2
    else
      hull_num = 1
      hull(1) = 1
    end if

    return

  end if
!
!  Find the leftmost point and call it "Q".
!  In case of ties, take the bottom-most.
!
  q = 1
  do i = 2, node_num
    if ( node_xy(1,i) < node_xy(1,q) .or. &
       ( node_xy(1,i) == node_xy(1,q) .and. node_xy(2,i) < node_xy(2,q) ) ) then
      q = i
    end if
  end do

  q_xy(1:2) = node_xy(1:2,q)
!
!  Remember the starting point, so we know when to stop!
!
  first = q
  hull_num = 1
  hull(1) = q
!
!  For the first point, make a dummy previous point, 1 unit south,
!  and call it "P".
!
  p_xy(1) = q_xy(1)
  p_xy(2) = q_xy(2) - 1.0D+00
!
!  Now, having old point P, and current point Q, find the new point R
!  so the angle PQR is maximal.
!
!  Watch out for the possibility that the two nodes are identical.
!
  do

    r = 0
    angle_max = 0.0D+00

    do i = 1, node_num

      if ( i /= q .and. &
           ( node_xy(1,i) /= q_xy(1) .or. node_xy(2,i) /= q_xy(2) ) ) then

        call angle_rad_2d(p_xy, q_xy, node_xy(1:2,i),angle)

        if ( r == 0 .or. angle_max < angle ) then

          r = i
          r_xy(1:2) = node_xy(1:2,r)
          angle_max = angle
!
!  In case of ties, choose the nearer point.
!
        else if ( r /= 0 .and. angle == angle_max ) then

          di = ( node_xy(1,i) - q_xy(1) )**2 + ( node_xy(2,i) - q_xy(2) )**2
          dr = ( r_xy(1)      - q_xy(1) )**2 + ( r_xy(2)      - q_xy(2) )**2

          if ( di < dr ) then
            r = i
            r_xy(1:2) = node_xy(1:2,r)
            angle_max = angle
          end if

        end if

      end if

    end do
!
!  We are done when we have returned to the first point on the convex hull.
!
    if ( r == first ) then
      exit
    end if

    hull_num = hull_num + 1
    if ( node_num < hull_num ) then
      write ( *, '(a)' ) ' '
      write ( *, '(a)' ) 'POINTS_HULL_2D - Fatal error!'
      write ( *, '(a)' ) '  The algorithm has failed.'
      stop
    end if
!
!  Add point R to convex hull.
!
    hull(hull_num) = r
!
!  Set P := Q, Q := R, and prepare to search for next point R.
!
    q = r

    p_xy(1:2) = q_xy(1:2)
    q_xy(1:2) = r_xy(1:2)

  end do

  return
end

    end program ESMF_GenerateCSGridDescription
