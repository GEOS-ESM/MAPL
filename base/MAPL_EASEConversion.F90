#include "MAPL_ErrLog.h"

module MAPL_EASEConversion

  ! =====================================================================================
  ! This file is moved from
  ! GEOSgcm_GridComp/GEOSagcm_GridComp/GEOSphysics_GridComp/GEOSsurface_GridComp/Utils/Raster/makebcs/EASE_conv.F90
  !  - Fortran routines for conversion of Equal-Area Scalable Earth (EASE)
  !    grid coordinates (lat/lon <--> row/col indices)
  !    Implemented for global cylindrical ('Mxx') EASE grids only.
  !
  !    Works for EASE[v1] and EASEv2 grids.
  !
  ! -------------------------------------------------------------------------------------
  !
  ! CHANGELOG (easeV1_conv.F90):
  ! ============================
  !
  ! easeV1_conv.F90 - Fortran routines for conversion of azimuthal
  !                   equal area and equal area cylindrical grid coordinates
  !
  ! 30-Jan-1992 H.Maybee
  ! 20-Mar-1992 Ken Knowles  303-492-0644  knowles@kryos.colorado.edu
  ! 16-Dec-1993 MJ Brodzik   303-492-8263  brodzik@jokull.colorado.edu
  !              Copied from nsmconv.f, changed resolutions from
  !              40-20-10 km to 25-12.5 km
  ! 21-Dec-1993 MJ Brodzik   303-492-8263  brodzik@jokull.colorado.edu
  !              Fixed sign of Southern latitudes in MAPL_ease_inverse.
  ! 12-Sep-1994 David Hoogstrate 303-492-4116 hoogstra@jokull.colorado.edu
  ! 	       Changed grid cell size. Changed "c","f" to "l","h"
  ! 25-Oct-1994 David Hoogstrate 303-492-4116 hoogstra@jokull.colorado.edu
  ! 	       Changed row size from 587 to 586 for Mercator projection
  ! 11-May-2011 reichle: Changed "smap" to "easeV1".
  !                      Added SSM/I and AMSR-E "M25" grid.
  !                      So far ONLY for cylindrical grids.
  !                      Converted from *.f to *.F90 module
  !
  ! $Log$
  ! Revision 1.1  2011-05-11 21:58:46  rreichle
  ! Adding utilities to map between EASE grids and lat/lon coordinates.
  !
  ! Revision 1.3  1994/11/01 23:40:43  brodzik
  ! Replaced all references to 'ease' with 'smap'
  ! Replaced all references to 'smap' with 'easeV1' -- reichle
  !
  !
  ! CHANGELOG (easeV2_conv.F90):
  ! ============================
  !
  ! easeV2_conv.F90 - FORTRAN routines for converting grid coordinates
  !                   (latitude/longitude <--> row/column indices)
  !                   of the Equal Area Scalable Earth, version 2 (EASEv2) grid
  !
  !    ***** ONLY cylindrical ('M') projection implemented *****
  !
  ! Ported from Steven Chan's matlab code (smapease2inverse.m,
  ! smapease2forward.m), which has been ported from NSIDC's IDL code
  ! (wgs84_convert.pro, wgs84_inverse.pro) available from
  ! ftp://sidads.colorado.edu/pub/tools/easegrid/geolocation_tools/
  !
  ! Official references:
  !  doi:10.3390/ijgi1010032
  !  doi:10.3390/ijgi3031154 -- correction of M25 "map_scale_m" parameters!
  !
  ! 04-Apr-2013 - reichle
  ! 11-Sep-2018 - reichle, mgirotto -- added 'M25' grid parameters
  !
  !
  ! CHANGELOG (EASE_conv.F90):
  ! ==========================
  !
  ! 2022-09-13, wjiang+reichle:
  !   merged easeV1_conv.F90 and easeV2_conv.F90 into EASE_conv.F90
  !   - using different values for PI in easeV1 and easeV2 calcs as in old easeV[x]_conv.F90 modules;
  !       in contrast, LDAS_EASE_conv.F90 in GEOSldas used only a single value for PI.
  !   - bug fix in easeV2_get_params() for EASEv2/M25 (to compute s0, divide by 2.0 not by integer 2)
  !
  !
  ! ==========================================================================
  use, intrinsic :: iso_fortran_env, only: REAL64
  use mapl_ErrorHandlingMod

  implicit none

  private

  public :: MAPL_ease_convert
  public :: MAPL_ease_inverse
  public :: MAPL_ease_extent
  public :: MAPL_get_ease_gridname_by_cols

  ! =======================================================================
  !
  ! EASEv1 global constants

  ! ***NEVER*** change these constants to GEOS MAPL constants!!!!
  !
  ! These values are from the original definition of the EASE grids by NSIDC.

  ! radius of the earth (km), authalic sphere based on International datum

  real(kind=REAL64), parameter :: easeV1_RE_km                    = 6371.228

  ! scale factor for standard paralles at +/-30.00 degrees

  real(kind=REAL64), parameter :: easeV1_COS_PHI1                 = .866025403

  real(kind=REAL64), parameter :: easeV2_PI                       = 3.14159265358979323846
  real(kind=REAL64), parameter :: easeV1_PI                       = 3.141592653589793

  ! =======================================================================
  !
  ! EASEv2 global constants

  ! ***NEVER*** change these constants to GEOS MAPL constants!!!!

  ! radius of the earth (m) and map eccentricity

  real(kind=REAL64), parameter :: map_equatorial_radius_m         = 6378137.0

  real(kind=REAL64), parameter :: map_eccentricity                = 0.081819190843


  real(kind=REAL64), parameter :: e2      = map_eccentricity * map_eccentricity
  real(kind=REAL64), parameter :: e4      = e2 * e2
  real(kind=REAL64), parameter :: e6      = e2 * e4


  real(kind=REAL64), parameter :: map_reference_longitude         =   0.0  ! 'M', 'N', 'S'

  ! constants for 'N' and 'S' (azimuthal) projections

  real(kind=REAL64), parameter :: N_map_reference_latitude        =  90.0
  real(kind=REAL64), parameter :: S_map_reference_latitude        = -90.0

  ! constants for 'M' (cylindrical) projection

  real(kind=REAL64), parameter :: M_map_reference_latitude        =   0.0
  real(kind=REAL64), parameter :: M_map_second_reference_latitude =  30.0

  real(kind=REAL64), parameter :: M_sin_phi1 = sin(M_map_second_reference_latitude*easeV2_PI/180.0)
  real(kind=REAL64), parameter :: M_cos_phi1 = cos(M_map_second_reference_latitude*easeV2_PI/180.0)

  real(kind=REAL64), parameter :: M_kz = M_cos_phi1/sqrt(1.0-e2*M_sin_phi1*M_sin_phi1)


contains

  ! *******************************************************************
  !
  !   GENERIC routines (public interface)
  !
  ! *******************************************************************
  !
  !   EASELabel = *EASEv[x]_[p][yy]*    (e.g., EASEv2_M09)
  !
  !     version:     x  = {  1,  2             }
  !     projection:  p  = {  M                 }    ! only cylindrical ("M") implemented
  !     resolution:  yy = { 01, 03, 09, 25, 36 }    ! 12.5 km not yet implemented
  !
  !   Coordinate arguments for MAPL_ease_convert() and MAPL_ease_inverse():
  !
  !     |    map coords    |  0-based index   |   # grid cells   |
  !     |                  |  (real numbers!) |                  |
  !     ----------------------------------------------------------
  !     |    latitude      |        s         |       rows       |
  !     |    longitude     |        r         |       cols       |
  !
  !   Indices are 0-based and run west to east (r) and north to south (s).
  !
  ! --------------------------------------------------------------------

  subroutine MAPL_ease_convert (EASELabel, lat, lon, r, s, rc)   ! note odd/reversed order of (lat,lon) and (r,s)

    character*(*),     intent(in)  :: EASELabel
    real,              intent(in)  :: lat, lon
    real,              intent(out) :: r, s         ! r = lon index,  s = lat index
    integer, optional, intent(out) :: rc
    integer :: status
    character(3)  :: grid

    if (     index(EASELabel,'M36') /=0 ) then
       grid='M36'
    else if (index(EASELabel,'M25') /=0 ) then
       grid='M25'
    else if (index(EASELabel,'M09') /=0 ) then
       grid='M09'
    else if (index(EASELabel,'M03') /=0 ) then
       grid='M03'
    else if (index(EASELabel,'M01') /=0 ) then
       grid='M01'
    else
       _fail("MAPL_ease_convert(): unknown grid projection and resolution: "//trim(EASELabel)//"  STOPPING.")
    endif

    if(     index(EASELabel,'EASEv2') /=0) then
       call easeV2_convert(grid,lat,lon,r,s)
    else if(index(EASELabel,'EASEv1') /=0) then
       call easeV1_convert(grid,lat,lon,r,s)
    else
       _fail("MAPL_ease_convert(): unknown grid version: "//trim(EASELabel)//"  STOPPING.")
    endif

    _return(_success)

  end subroutine MAPL_ease_convert

  ! *******************************************************************

  subroutine MAPL_ease_inverse (EASELabel, r, s, lat, lon, rc)   ! note odd/reversed order of (r,s) and (lat,lon)

    ! Note: Get lat/lon of grid cell borders by using fractional indices.
    !       E.g., s=-0.5 yields northern grid cell boundary of northernmost grid cells.

    character*(*),     intent(in)  :: EASELabel
    real,              intent(in)  :: r, s         ! r = lon index,  s = lat index
    real,              intent(out) :: lat, lon
    integer, optional, intent(out) :: rc

    character(3)  :: grid
    integer       :: status

    if (     index(EASELabel,'M36') /=0 ) then
       grid='M36'
    else if (index(EASELabel,'M25') /=0 ) then
       grid='M25'
    else if (index(EASELabel,'M09') /=0 ) then
       grid='M09'
    else if (index(EASELabel,'M03') /=0 ) then
       grid='M03'
    else if (index(EASELabel,'M01') /=0 ) then
       grid='M01'
    else
       _fail("MAPL_ease_inverse(): unknown grid projection and resolution: "//trim(EASELabel)//"  STOPPING.")
    endif

    if(     index(EASELabel,'EASEv2') /=0) then
       call easeV2_inverse(grid,r,s,lat,lon, _rc)
    else if(index(EASELabel,'EASEv1') /=0) then
       call easeV1_inverse(grid,r,s,lat,lon, _rc)
    else
       _fail("MAPL_ease_inverse(): unknown grid version: "//trim(EASELabel)//"  STOPPING.")
    endif

    _return(_success)

  end subroutine MAPL_ease_inverse

  ! *******************************************************************

  subroutine MAPL_ease_extent (EASELabel, cols, rows, cell_area, ll_lon, ll_lat, ur_lon, ur_lat, rc)

    ! get commonly used EASE grid parameters

    character*(*),           intent(in)  :: EASELabel
    integer,                 intent(out) :: cols, rows  ! number of grid cells in lon and lat direction, resp.
    real,          optional, intent(out) :: cell_area   ! [m^2]
    real,          optional, intent(out) :: ll_lon      ! lon of grid cell boundary in lower left  corner
    real,          optional, intent(out) :: ll_lat      ! lat of grid cell boundary in lower left  corner
    real,          optional, intent(out) :: ur_lon      ! lon of grid cell boundary in upper right corner
    real,          optional, intent(out) :: ur_lat      ! lat of grid cell boundary in upper right corner
    integer,       optional, intent(out) :: rc
    ! ---------------------------------------------------------------------

    real(kind=REAL64)  :: map_scale_m, CELL_km, r0, s0, Rg
    real               :: tmplon
    character(3)       :: grid
    integer            :: status

    if (     index(EASELabel,'M36') /=0 ) then
       grid='M36'
    else if (index(EASELabel,'M25') /=0 ) then
       grid='M25'
    else if (index(EASELabel,'M09') /=0 ) then
       grid='M09'
    else if (index(EASELabel,'M03') /=0 ) then
       grid='M03'
    else if (index(EASELabel,'M01') /=0 ) then
       grid='M01'
    else
       _fail("MAPL_ease_extent(): unknown grid projection and resolution: "//trim(EASELabel)//"  STOPPING.")
    endif

    if(     index(EASELabel,'EASEv2') /=0) then

       call easeV2_get_params(grid, map_scale_m, cols, rows, r0, s0, _rc)

       if(present(cell_area)) cell_area = map_scale_m**2

    else if(index(EASELabel,'EASEv1') /=0) then

       call easeV1_get_params(grid, CELL_km, cols, rows, r0, s0, Rg, _rc)

       if(present(cell_area)) cell_area = CELL_km**2 * 1000. * 1000.

    else
       _fail("MAPL_ease_extent(): unknown grid version: "//trim(EASELabel)//"  STOPPING.")
    endif

    ! get lat/lon of corner grid cells
    !
    ! recall that EASE grid indexing is zero-based

    if (present(ll_lat))  call MAPL_ease_inverse(EASElabel, 0., rows-0.5, ll_lat, tmplon, _rc)
    if (present(ur_lat))  call MAPL_ease_inverse(EASElabel, 0.,     -0.5, ur_lat, tmplon, _rc)

    if (present(ll_lon))  ll_lon = -180.
    if (present(ur_lon))  ur_lon =  180.

    _return(_success)

  end subroutine MAPL_ease_extent

  ! *******************************************************************
  !
  !   EASEv1 routines (private)
  !
  ! *******************************************************************

  subroutine easeV1_convert (grid, lat, lon, r, s, rc)

    ! convert geographic coordinates (spherical earth) to
    ! azimuthal equal area or equal area cylindrical grid coordinates
    !
    ! status = easeV1_convert (grid, lat, lon, r, s)
    !
    ! input : grid - projection name '[M][xx]'
    !            where xx = approximate resolution [km]
    !               ie xx = "01", "03", "09", "36"       (SMAP)
    !               or xx = "12", "25"                   (SSM/I, AMSR-E)
    ! 	    lat, lon = geo. coords. (decimal degrees)
    !
    ! output: r, s - column, row coordinates
    !
    ! result: status = 0 indicates normal successful completion
    ! 		-1 indicates error status (point not on grid)
    !
    ! --------------------------------------------------------------------------

    character*(*),     intent(in)  :: grid
    real,              intent(in)  :: lat, lon
    real,              intent(out) :: r, s
    integer, optional, intent(out) :: rc

    ! local variables

    integer :: cols, rows, status
    real(kind=REAL64)  :: Rg, phi, lam, rho, CELL_km, r0, s0

    real(kind=REAL64), parameter :: PI = easeV1_PI
    ! ---------------------------------------------------------------------

    call easeV1_get_params( grid, CELL_km, cols, rows, r0, s0, Rg, _rc)

    phi = lat*PI/180.   ! convert from degree to radians
    lam = lon*PI/180.   ! convert from degree to radians

    if (grid(1:1).eq.'N') then
       rho = 2 * Rg * sin(PI/4. - phi/2.)
       r = r0 + rho * sin(lam)
       s = s0 + rho * cos(lam)

    else if (grid(1:1).eq.'S') then
       rho = 2 * Rg * cos(PI/4. - phi/2.)
       r = r0 + rho * sin(lam)
       s = s0 - rho * cos(lam)

    else if (grid(1:1).eq.'M') then
       r = r0 + Rg * lam * easeV1_COS_PHI1
       s = s0 - Rg * sin(phi) / easeV1_COS_PHI1

    else
       _fail('Unsupported v1 convert')
    endif

    _return(_success)

  end subroutine easeV1_convert

  ! *******************************************************************

  subroutine easeV1_inverse (grid, r, s, lat, lon, rc)

    ! convert azimuthal equal area or equal area cylindrical
    ! grid coordinates to geographic coordinates (spherical earth)
    !
    ! status = easeV1_inverse (grid, r, s, lat, lon)
    !
    ! input : grid - projection name '[M][xx]'
    !            where xx = approximate resolution [km]
    !               ie xx = "01", "03", "09", "36"       (SMAP)
    !               or xx = "12", "25"                   (SSM/I, AMSR-E)
    ! 	    r, s - column, row coordinates
    !
    ! output: lat, lon = geo. coords. (decimal degrees)
    !
    ! result: status = 0 indicates normal successful completion
    ! 		-1 indicates error status (point not on grid)
    !
    ! --------------------------------------------------------------------------

    character*(*),     intent(in)  :: grid
    real,              intent(in)  :: r, s
    real,              intent(out) :: lat, lon
    integer, optional, intent(out) :: rc
    ! local variables

    integer :: cols, rows, status
    real(kind=REAL64)    :: Rg, phi, lam, rho, CELL_km, r0, s0
    real(kind=REAL64)    :: gamma, beta, epsilon, x, y, c
    real(kind=REAL64)    :: sinphi1, cosphi1

    real(kind=REAL64), parameter :: PI = easeV1_PI

    ! ---------------------------------------------------------------------

    call easeV1_get_params( grid, CELL_km, cols, rows, r0, s0, Rg, _rc)

    x = r - r0
    y = -(s - s0)

    if ((grid(1:1).eq.'N').or.(grid(1:1).eq.'S')) then
       rho = sqrt(x*x + y*y)
       if (rho.eq.0.0) then
          if (grid(1:1).eq.'N') lat = 90.0
          if (grid(1:1).eq.'S') lat = -90.0
          lon = 0.0
       else
          if (grid(1:1).eq.'N') then
             sinphi1 = sin(PI/2.)
             cosphi1 = cos(PI/2.)
             if (y.eq.0.) then
                if (r.le.r0) lam = -PI/2.
                if (r.gt.r0) lam = PI/2.
             else
                lam = atan2(x,-y)
             endif
          else if (grid(1:1).eq.'S') then
             sinphi1 = sin(-PI/2.)
             cosphi1 = cos(-PI/2.)
             if (y.eq.0.) then
                if (r.le.r0) lam = -PI/2.
                if (r.gt.r0) lam = PI/2.
             else
                lam = atan2(x,y)
             endif
          endif
          gamma = rho/(2 * Rg)
          if (abs(gamma) .gt. 1.) return
          c = 2 * asin(gamma)
          beta = cos(c) * sinphi1 + y * sin(c) * (cosphi1/rho)
          if (abs(beta).gt.1.) return
          phi = asin(beta)
          lat = phi*180./PI   ! convert from radians to degree
          lon = lam*180./PI   ! convert from radians to degree
       endif

    else if (grid(1:1).eq.'M') then

       ! 	  allow .5 cell tolerance in arcsin function
       ! 	  so that grid coordinates which are less than .5 cells
       ! 	  above 90.00N or below 90.00S are given a lat of 90.00

       epsilon = 1 + 0.5/Rg
       beta = y*easeV1_COS_PHI1/Rg
       if (abs(beta).gt.epsilon) return
       if (beta.le.-1.) then
          phi = -PI/2.
       else if (beta.ge.1.) then
          phi = PI/2.
       else
          phi = asin(beta)
       endif
       lam = x/easeV1_COS_PHI1/Rg
       lat = phi*180./PI   ! convert from radians to degree
       lon = lam*180./PI   ! convert from radians to degree
    else
       _fail('Unsupported v1 grid')
    endif

    _return(_success)

  end subroutine easeV1_inverse

  ! *******************************************************************

  subroutine easeV1_get_params( grid, CELL_km, cols, rows, r0, s0, Rg, rc )

    implicit none

    character*(*),    intent(in)  :: grid
    real(kind=REAL64),intent(out) :: CELL_km, r0, s0, Rg
    integer,          intent(out) :: cols, rows
    integer, optional,intent(out) :: rc

    ! --------------------------------------------------------
    !
    ! r0,s0 are defined such that cells at all scales have
    ! coincident center points
    !
    !c        r0 = (cols-1)/2. * scale
    !c        s0 = (rows-1)/2. * scale
    !
    ! --------------------------------------------------------
    integer :: status

    if ((grid(1:1).eq.'N').or.(grid(1:1).eq.'S')) then

       _fail('easeV1_get_params(): polar projections not implemented yet')

    else if (grid(1:1).eq.'M') then

       if      (grid .eq. 'M36') then ! SMAP 36 km grid
          CELL_km = 36.00040279063   ! nominal cell size in kilometers
          cols = 963
          rows = 408
          r0 = 481.0
          s0 = 203.5

       else if (grid .eq. 'M25') then ! SSM/I, AMSR-E 25 km grid
          CELL_km = 25.067525         ! nominal cell size in kilometers
          cols = 1383
          rows = 586
          r0 = 691.0
          s0 = 292.5

       else if (grid .eq. 'M09') then ! SMAP  9 km grid
          CELL_km = 9.00010069766     ! nominal cell size in kilometers
          cols = 3852
          rows = 1632
          r0 = 1925.5
          s0 = 815.5

       else if (grid .eq. 'M03') then ! SMAP  3 km grid
          CELL_km = 3.00003356589     ! nominal cell size in kilometers
          cols = 11556
          rows = 4896
          r0 = 5777.5
          s0 = 2447.5

       else if (grid .eq. 'M01') then ! SMAP  1 km grid
          CELL_km = 1.00001118863     ! nominal cell size in kilometers
          cols = 34668
          rows = 14688
          r0 = 17333.5
          s0 = 7343.5

       else
          _fail( 'easeV1_get_params(): unknown resolution: ' // grid)
       endif

    else
       _fail('easeV1_get_params(): unknown projection: '// grid)
    endif

    Rg = easeV1_RE_km/CELL_km

    _return(_success)

  end subroutine easeV1_get_params


  ! *******************************************************************
  !
  !   EASEv2 routines (private)
  !
  ! *******************************************************************

  subroutine easeV2_convert (grid, lat, lon, col_ind, row_ind, rc)

    ! convert geographic coordinates (spherical earth) to
    ! azimuthal equal area or equal area cylindrical grid coordinates
    !
    ! *** NOTE order of calling arguments:  "lat-lon-lon-lat" ***
    !
    ! useage: call easeV2_convert (grid, lat, lon, r, s)
    !
    ! input : grid - projection name '[M][xx]'
    !            where xx = approximate resolution [km]
    !               ie xx = "01", "03", "09", "36"       (SMAP)
    ! 	      lat, lon = geo. coords. (decimal degrees)
    !
    ! output: col_ind, row_ind - column, row coordinates
    !
    ! --------------------------------------------------------------------------

    character*(*),     intent(in)  :: grid
    real,              intent(in)  :: lat, lon
    real,              intent(out) :: col_ind, row_ind
    integer, optional, intent(out) :: rc
    ! local variables

    integer :: cols, rows, status
    real(kind=REAL64)  :: dlon, phi, lam, map_scale_m, r0, s0, ms, x, y, sin_phi, q

    real(kind=REAL64), parameter :: PI = easeV2_PI

    real :: epsilon

    ! ---------------------------------------------------------------------

    call easeV2_get_params( grid, map_scale_m, cols, rows, r0, s0, _rc)

    epsilon = 1.e-6
    dlon = lon

    if (abs(map_reference_longitude)>epsilon) then

       dlon = lon - map_reference_longitude

    end if

    if (dlon .lt. -180.0) dlon = dlon + 360.0
    if (dlon .gt.  180.0) dlon = dlon - 360.0

    phi =  lat*PI/180.0   ! convert from degree to radians
    lam = dlon*PI/180.0   ! convert from degree to radians

    sin_phi = sin(phi)

    ms      = map_eccentricity*sin_phi

    q = (1. - e2)*                                                     &
         (                                                             &
         (sin_phi /(1. - e2*sin_phi*sin_phi))                          &
         -                                                             &
         .5/map_eccentricity*log((1.-ms)/(1.+ms))                      &
         )

    ! note: "qp" only needed for 'N' and 'S' projections

    if      (grid(1:1).eq.'M') then

       x =  map_equatorial_radius_m*M_kz*lam

       y = (map_equatorial_radius_m*q)/(2.*M_kz)

    else

       _fail('EASEv2_convert(): Polar projections not implemented yet')

    endif

    row_ind = s0 - (y/map_scale_m)
    col_ind = r0 + (x/map_scale_m)

    _return(_success)

  end subroutine easeV2_convert

  ! *******************************************************************

  subroutine easeV2_inverse (grid, r, s, lat, lon, rc)

    ! convert azimuthal equal area or equal area cylindrical
    ! grid coordinates to geographic coordinates (spherical earth)
    !
    ! *** NOTE order of calling arguments:  "lon-lat-lat-lon" ***
    !
    ! useage: call easeV1_inverse (grid, r, s, lat, lon)
    !
    ! input : grid - projection name '[M][xx]'
    !            where xx = approximate resolution [km]
    !               ie xx = "01", "03", "09", "36"       (SMAP)
    ! 	      r, s - column, row coordinates
    !
    ! output: lat, lon = geo. coords. (decimal degrees)
    !
    ! --------------------------------------------------------------------------

    character*(*),     intent(in)  :: grid
    real,              intent(in)  :: r, s
    real,              intent(out) :: lat, lon
    integer, optional, intent(out) :: rc

    ! local variables

    integer                      :: cols, rows, status
    real(kind=REAL64)            :: phi, lam, map_scale_m, r0, s0, beta, x, y, qp
    real(kind=REAL64), parameter :: PI = easeV2_PI

    ! ---------------------------------------------------------------------

    call easeV2_get_params( grid, map_scale_m, cols, rows, r0, s0, _rc)

    x =  (r - r0)*map_scale_m
    y = -(s - s0)*map_scale_m

    qp = (1. - e2)*                                                           &
         (                                                                    &
         (1./(1.-e2))                                                         &
         -                                                                    &
         .5/map_eccentricity*log((1.-map_eccentricity)/(1.+map_eccentricity)) &
         )

    if      (grid(1:1).eq.'M') then

       beta = asin(2.*y*M_kz/(map_equatorial_radius_m*qp))

       lam  = x/(map_equatorial_radius_m*M_kz)

    else

       _fail('EASEv2_inverse(): Polar projections not implemented yet')

    endif

    phi = beta                                                              &
         + ( ( e2/3.       + 31./180.*e4 + 517./ 5040.*e6 )*sin(2.*beta) )  &
         + ( (               23./360.*e4 + 251./ 3780.*e6 )*sin(4.*beta) )  &
         + ( (                             761./45360.*e6 )*sin(6.*beta) )

    lat = phi*180./PI                            ! convert from radians to degree
    lon = lam*180./PI + map_reference_longitude  ! convert from radians to degree

    if (lon .lt. -180.0) lon = lon + 360.0
    if (lon .gt.  180.0) lon = lon - 360.0

    _return(_success)

  end subroutine easeV2_inverse

  ! *******************************************************************

  subroutine easeV2_get_params( grid, map_scale_m, cols, rows, r0, s0, rc)

    implicit none

    character*(*),     intent(in)  :: grid
    real(kind=REAL64), intent(out) :: map_scale_m, r0, s0
    integer,           intent(out) :: cols, rows
    integer, optional, intent(out) :: rc

    integer :: status

    if (grid(1:1).eq.'M') then

       if      (grid .eq. 'M36') then      ! SMAP 36 km grid

          map_scale_m = 36032.220840584   ! nominal cell size in meters
          cols = 964
          rows = 406
          r0 = (cols-1)/2.0
          s0 = (rows-1)/2.0

       else if (grid .eq. 'M25') then      ! 25 km grid

          map_scale_m = 25025.2600000      ! nominal cell size in meters (see doi:10.3390/ijgi3031154)
          cols = 1388
          rows =  584
          r0 = (cols-1)/2.0
          s0 = (rows-1)/2.0

       else if (grid .eq. 'M09') then      ! SMAP  9 km grid

          map_scale_m = 9008.055210146     ! nominal cell size in meters
          cols = 3856
          rows = 1624
          r0 = (cols-1)/2.0
          s0 = (rows-1)/2.0

       else if (grid .eq. 'M03') then      ! SMAP  3 km grid

          map_scale_m = 3002.6850700487    ! nominal cell size in meters
          cols = 11568
          rows = 4872
          r0 = (cols-1)/2.0
          s0 = (rows-1)/2.0

       else if (grid .eq. 'M01') then      ! SMAP  1 km grid

          map_scale_m = 1000.89502334956   ! nominal cell size in meters
          cols = 34704
          rows = 14616
          r0 = (cols-1)/2.0
          s0 = (rows-1)/2.0

       else

          _fail('easeV2_get_params(): unknown resolution: '//grid)

       endif

    else if ((grid(1:1).eq.'N').or.(grid(1:1).eq.'S')) then

       _fail('easeV2_get_params(): Polar projections not implemented yet')

    else

       _fail('easeV2_get_params(): unknown projection: '// grid)

    endif

    _return(_success)

  end subroutine easeV2_get_params

  function MAPL_get_ease_gridname_by_cols(cols, rc) result(name)

    ! Obtain EASE grid name based on number of columns (longitudes).
    !
    ! This inverts subroutines easeV[x]_get_params(), with which consistency
    !   must be maintained manually if more EASE grids are added.

      integer,           intent(in) :: cols
      integer, optional, intent(out):: rc

      integer :: status
      character(len=:), allocatable :: name
      ! the factory should have the grid name
      select case (cols)
      case (964)
        name = 'EASEv2_M36'
      case (1388)
        name = 'EASEv2_M25'
      case (3856)
        name = 'EASEv2_M09'
      case (11568)
        name = 'EASEv2_M03'
      case (34704)
        name = 'EASEv2_M01'

      case (963)
        name = 'EASEv1_M36'
      case (1383)
        name = 'EASEv1_M25'
      case (3852)
        name = 'EASEv1_M09'
      case (11556)
        name = 'EASEv1_M03'
      case (34668)
        name = 'EASEv1_M01'
      case default
        _fail('EASEGridFactory does not support this solution')
      end select
      _return(_success)
  end function

  ! *******************************************************************

end module MAPL_EASEConversion

! =============================== EOF =================================

