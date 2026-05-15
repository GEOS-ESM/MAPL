#include "MAPL.h"

module mapl3g_EASEConversion

  ! =====================================================================================
  ! Fortran routines for conversion of Equal-Area Scalable Earth (EASE) grid coordinates
  ! (lat/lon <--> row/col indices). Implemented for global cylindrical ('Mxx') EASE grids
  ! only. Works for EASE[v1] and EASEv2 grids.
  !
  ! Originally from GEOSgcm/.../Raster/makebcs/EASE_conv.F90; merged and moved here.
  ! base/MAPL_EASEConversion.F90 is now a thin re-export wrapper of this module.
  ! =====================================================================================

  use, intrinsic :: iso_fortran_env, only: REAL64
  use mapl_ErrorHandlingMod

  implicit none
  private

  public :: ease_convert
  public :: ease_inverse
  public :: ease_extent
  public :: get_ease_gridname_by_cols

  ! =======================================================================
  ! EASEv1 global constants
  ! ***NEVER*** change these to GEOS MAPL constants -- values are from NSIDC.

  real(kind=REAL64), parameter :: easeV1_RE_km     = 6371.228
  real(kind=REAL64), parameter :: easeV1_COS_PHI1  = .866025403
  real(kind=REAL64), parameter :: easeV2_PI        = 3.14159265358979323846
  real(kind=REAL64), parameter :: easeV1_PI        = 3.141592653589793

  ! =======================================================================
  ! EASEv2 global constants
  ! ***NEVER*** change these to GEOS MAPL constants.

  real(kind=REAL64), parameter :: map_equatorial_radius_m = 6378137.0
  real(kind=REAL64), parameter :: map_eccentricity        = 0.081819190843

  real(kind=REAL64), parameter :: e2 = map_eccentricity * map_eccentricity
  real(kind=REAL64), parameter :: e4 = e2 * e2
  real(kind=REAL64), parameter :: e6 = e2 * e4

  real(kind=REAL64), parameter :: map_reference_longitude         =   0.0
  real(kind=REAL64), parameter :: N_map_reference_latitude        =  90.0
  real(kind=REAL64), parameter :: S_map_reference_latitude        = -90.0
  real(kind=REAL64), parameter :: M_map_reference_latitude        =   0.0
  real(kind=REAL64), parameter :: M_map_second_reference_latitude =  30.0

  real(kind=REAL64), parameter :: M_sin_phi1 = sin(M_map_second_reference_latitude*easeV2_PI/180.0)
  real(kind=REAL64), parameter :: M_cos_phi1 = cos(M_map_second_reference_latitude*easeV2_PI/180.0)
  real(kind=REAL64), parameter :: M_kz = M_cos_phi1/sqrt(1.0-e2*M_sin_phi1*M_sin_phi1)

contains

  subroutine ease_convert(EASELabel, lat, lon, r, s, rc)
    character*(*),     intent(in)  :: EASELabel
    real,              intent(in)  :: lat, lon
    real,              intent(out) :: r, s
    integer, optional, intent(out) :: rc
    integer :: status
    character(3) :: grid

    if      (index(EASELabel,'M36') /=0) then; grid='M36'
    else if (index(EASELabel,'M25') /=0) then; grid='M25'
    else if (index(EASELabel,'M09') /=0) then; grid='M09'
    else if (index(EASELabel,'M03') /=0) then; grid='M03'
    else if (index(EASELabel,'M01') /=0) then; grid='M01'
    else; _FAIL("ease_convert(): unknown grid projection and resolution: "//trim(EASELabel))
    endif

    if      (index(EASELabel,'EASEv2') /=0) then; call easeV2_convert(grid,lat,lon,r,s)
    else if (index(EASELabel,'EASEv1') /=0) then; call easeV1_convert(grid,lat,lon,r,s)
    else; _FAIL("ease_convert(): unknown grid version: "//trim(EASELabel))
    endif

    _RETURN(_SUCCESS)
  end subroutine ease_convert

  subroutine ease_inverse(EASELabel, r, s, lat, lon, rc)
    character*(*),     intent(in)  :: EASELabel
    real,              intent(in)  :: r, s
    real,              intent(out) :: lat, lon
    integer, optional, intent(out) :: rc
    character(3) :: grid
    integer :: status

    if      (index(EASELabel,'M36') /=0) then; grid='M36'
    else if (index(EASELabel,'M25') /=0) then; grid='M25'
    else if (index(EASELabel,'M09') /=0) then; grid='M09'
    else if (index(EASELabel,'M03') /=0) then; grid='M03'
    else if (index(EASELabel,'M01') /=0) then; grid='M01'
    else; _FAIL("ease_inverse(): unknown grid projection and resolution: "//trim(EASELabel))
    endif

    if      (index(EASELabel,'EASEv2') /=0) then; call easeV2_inverse(grid,r,s,lat,lon,_RC)
    else if (index(EASELabel,'EASEv1') /=0) then; call easeV1_inverse(grid,r,s,lat,lon,_RC)
    else; _FAIL("ease_inverse(): unknown grid version: "//trim(EASELabel))
    endif

    _RETURN(_SUCCESS)
  end subroutine ease_inverse

  subroutine ease_extent(EASELabel, cols, rows, cell_area, ll_lon, ll_lat, ur_lon, ur_lat, rc)
    character*(*),           intent(in)  :: EASELabel
    integer,                 intent(out) :: cols, rows
    real,          optional, intent(out) :: cell_area
    real,          optional, intent(out) :: ll_lon, ll_lat, ur_lon, ur_lat
    integer,       optional, intent(out) :: rc

    real(kind=REAL64) :: map_scale_m, CELL_km, r0, s0, Rg
    real :: tmplon
    character(3) :: grid
    integer :: status

    if      (index(EASELabel,'M36') /=0) then; grid='M36'
    else if (index(EASELabel,'M25') /=0) then; grid='M25'
    else if (index(EASELabel,'M09') /=0) then; grid='M09'
    else if (index(EASELabel,'M03') /=0) then; grid='M03'
    else if (index(EASELabel,'M01') /=0) then; grid='M01'
    else; _FAIL("ease_extent(): unknown grid projection and resolution: "//trim(EASELabel))
    endif

    if (index(EASELabel,'EASEv2') /=0) then
       call easeV2_get_params(grid, map_scale_m, cols, rows, r0, s0, _RC)
       if (present(cell_area)) cell_area = map_scale_m**2
    else if (index(EASELabel,'EASEv1') /=0) then
       call easeV1_get_params(grid, CELL_km, cols, rows, r0, s0, Rg, _RC)
       if (present(cell_area)) cell_area = CELL_km**2 * 1000. * 1000.
    else
       _FAIL("ease_extent(): unknown grid version: "//trim(EASELabel))
    endif

    if (present(ll_lat)) call ease_inverse(EASELabel, 0., rows-0.5, ll_lat, tmplon, _RC)
    if (present(ur_lat)) call ease_inverse(EASELabel, 0.,     -0.5, ur_lat, tmplon, _RC)
    if (present(ll_lon)) ll_lon = -180.
    if (present(ur_lon)) ur_lon =  180.

    _RETURN(_SUCCESS)
  end subroutine ease_extent

  function get_ease_gridname_by_cols(cols, rc) result(name)
    integer,           intent(in)  :: cols
    integer, optional, intent(out) :: rc
    integer :: status
    character(len=:), allocatable :: name

    select case (cols)
    case (964);   name = 'EASEv2_M36'
    case (1388);  name = 'EASEv2_M25'
    case (3856);  name = 'EASEv2_M09'
    case (11568); name = 'EASEv2_M03'
    case (34704); name = 'EASEv2_M01'
    case (963);   name = 'EASEv1_M36'
    case (1383);  name = 'EASEv1_M25'
    case (3852);  name = 'EASEv1_M09'
    case (11556); name = 'EASEv1_M03'
    case (34668); name = 'EASEv1_M01'
    case default; _FAIL('get_ease_gridname_by_cols(): unsupported column count')
    end select
    _RETURN(_SUCCESS)
  end function get_ease_gridname_by_cols

  ! =======================================================================
  ! EASEv1 private routines
  ! =======================================================================

  subroutine easeV1_convert(grid, lat, lon, r, s, rc)
    character*(*), intent(in)  :: grid
    real,          intent(in)  :: lat, lon
    real,          intent(out) :: r, s
    integer, optional, intent(out) :: rc
    integer :: cols, rows, status
    real(kind=REAL64) :: Rg, phi, lam, rho, CELL_km, r0, s0
    real(kind=REAL64), parameter :: PI = easeV1_PI

    call easeV1_get_params(grid, CELL_km, cols, rows, r0, s0, Rg, _RC)
    phi = lat*PI/180.
    lam = lon*PI/180.

    if (grid(1:1).eq.'M') then
       r = r0 + Rg * lam * easeV1_COS_PHI1
       s = s0 - Rg * sin(phi) / easeV1_COS_PHI1
    else
       _FAIL('easeV1_convert(): unsupported projection: '//grid)
    endif
    _RETURN(_SUCCESS)
  end subroutine easeV1_convert

  subroutine easeV1_inverse(grid, r, s, lat, lon, rc)
    character*(*), intent(in)  :: grid
    real,          intent(in)  :: r, s
    real,          intent(out) :: lat, lon
    integer, optional, intent(out) :: rc
    integer :: cols, rows, status
    real(kind=REAL64) :: Rg, phi, lam, rho, CELL_km, r0, s0
    real(kind=REAL64) :: gamma, beta, epsilon, x, y, c
    real(kind=REAL64) :: sinphi1, cosphi1
    real(kind=REAL64), parameter :: PI = easeV1_PI

    call easeV1_get_params(grid, CELL_km, cols, rows, r0, s0, Rg, _RC)
    x = r - r0
    y = -(s - s0)

    if ((grid(1:1).eq.'N').or.(grid(1:1).eq.'S')) then
       rho = sqrt(x*x + y*y)
       if (rho.eq.0.0) then
          if (grid(1:1).eq.'N') lat =  90.0
          if (grid(1:1).eq.'S') lat = -90.0
          lon = 0.0
       else
          if (grid(1:1).eq.'N') then
             sinphi1 = sin(PI/2.); cosphi1 = cos(PI/2.)
             if (y.eq.0.) then
                if (r.le.r0) lam = -PI/2.
                if (r.gt.r0) lam =  PI/2.
             else
                lam = atan2(x,-y)
             endif
          else if (grid(1:1).eq.'S') then
             sinphi1 = sin(-PI/2.); cosphi1 = cos(-PI/2.)
             if (y.eq.0.) then
                if (r.le.r0) lam = -PI/2.
                if (r.gt.r0) lam =  PI/2.
             else
                lam = atan2(x,y)
             endif
          endif
          gamma = rho/(2*Rg)
          if (abs(gamma).gt.1.) return
          c = 2*asin(gamma)
          beta = cos(c)*sinphi1 + y*sin(c)*(cosphi1/rho)
          if (abs(beta).gt.1.) return
          phi = asin(beta)
          lat = phi*180./PI
          lon = lam*180./PI
       endif
    else if (grid(1:1).eq.'M') then
       epsilon = 1 + 0.5/Rg
       beta = y*easeV1_COS_PHI1/Rg
       if (abs(beta).gt.epsilon) return
       if (beta.le.-1.) then; phi = -PI/2.
       else if (beta.ge.1.) then; phi = PI/2.
       else; phi = asin(beta)
       endif
       lam = x/easeV1_COS_PHI1/Rg
       lat = phi*180./PI
       lon = lam*180./PI
    else
       _FAIL('easeV1_inverse(): unsupported projection: '//grid)
    endif
    _RETURN(_SUCCESS)
  end subroutine easeV1_inverse

  subroutine easeV1_get_params(grid, CELL_km, cols, rows, r0, s0, Rg, rc)
    character*(*),     intent(in)  :: grid
    real(kind=REAL64), intent(out) :: CELL_km, r0, s0, Rg
    integer,           intent(out) :: cols, rows
    integer, optional, intent(out) :: rc
    integer :: status

    if (grid(1:1).eq.'M') then
       select case (grid)
       case ('M36'); CELL_km=36.00040279063;   cols=963;   rows=408;  r0=481.0;   s0=203.5
       case ('M25'); CELL_km=25.067525;         cols=1383;  rows=586;  r0=691.0;   s0=292.5
       case ('M09'); CELL_km=9.00010069766;     cols=3852;  rows=1632; r0=1925.5;  s0=815.5
       case ('M03'); CELL_km=3.00003356589;     cols=11556; rows=4896; r0=5777.5;  s0=2447.5
       case ('M01'); CELL_km=1.00001118863;     cols=34668; rows=14688;r0=17333.5; s0=7343.5
       case default; _FAIL('easeV1_get_params(): unknown resolution: '//grid)
       end select
    else
       _FAIL('easeV1_get_params(): unsupported projection: '//grid)
    endif
    Rg = easeV1_RE_km/CELL_km
    _RETURN(_SUCCESS)
  end subroutine easeV1_get_params

  ! =======================================================================
  ! EASEv2 private routines
  ! =======================================================================

  subroutine easeV2_convert(grid, lat, lon, col_ind, row_ind, rc)
    character*(*), intent(in)  :: grid
    real,          intent(in)  :: lat, lon
    real,          intent(out) :: col_ind, row_ind
    integer, optional, intent(out) :: rc
    integer :: cols, rows, status
    real(kind=REAL64) :: dlon, phi, lam, map_scale_m, r0, s0, ms, x, y, sin_phi, q
    real(kind=REAL64), parameter :: PI = easeV2_PI
    real :: epsilon

    call easeV2_get_params(grid, map_scale_m, cols, rows, r0, s0, _RC)
    epsilon = 1.e-6
    dlon = lon
    if (abs(map_reference_longitude)>epsilon) dlon = lon - map_reference_longitude
    if (dlon.lt.-180.0) dlon = dlon + 360.0
    if (dlon.gt. 180.0) dlon = dlon - 360.0

    phi = lat*PI/180.0
    lam = dlon*PI/180.0
    sin_phi = sin(phi)
    ms = map_eccentricity*sin_phi
    q = (1.-e2)*( (sin_phi/(1.-e2*sin_phi*sin_phi)) - .5/map_eccentricity*log((1.-ms)/(1.+ms)) )

    if (grid(1:1).eq.'M') then
       x =  map_equatorial_radius_m*M_kz*lam
       y = (map_equatorial_radius_m*q)/(2.*M_kz)
    else
       _FAIL('easeV2_convert(): polar projections not implemented yet')
    endif

    row_ind = s0 - (y/map_scale_m)
    col_ind = r0 + (x/map_scale_m)
    _RETURN(_SUCCESS)
  end subroutine easeV2_convert

  subroutine easeV2_inverse(grid, r, s, lat, lon, rc)
    character*(*), intent(in)  :: grid
    real,          intent(in)  :: r, s
    real,          intent(out) :: lat, lon
    integer, optional, intent(out) :: rc
    integer :: cols, rows, status
    real(kind=REAL64) :: phi, lam, map_scale_m, r0, s0, beta, x, y, qp
    real(kind=REAL64), parameter :: PI = easeV2_PI

    call easeV2_get_params(grid, map_scale_m, cols, rows, r0, s0, _RC)
    x =  (r-r0)*map_scale_m
    y = -(s-s0)*map_scale_m

    qp = (1.-e2)*( (1./(1.-e2)) - .5/map_eccentricity*log((1.-map_eccentricity)/(1.+map_eccentricity)) )

    if (grid(1:1).eq.'M') then
       beta = asin(2.*y*M_kz/(map_equatorial_radius_m*qp))
       lam  = x/(map_equatorial_radius_m*M_kz)
    else
       _FAIL('easeV2_inverse(): polar projections not implemented yet')
    endif

    phi = beta &
         + ((e2/3.       + 31./180.*e4 + 517./ 5040.*e6)*sin(2.*beta)) &
         + ((              23./360.*e4 + 251./ 3780.*e6)*sin(4.*beta)) &
         + ((                            761./45360.*e6)*sin(6.*beta))

    lat = phi*180./PI
    lon = lam*180./PI + map_reference_longitude
    if (lon.lt.-180.0) lon = lon + 360.0
    if (lon.gt. 180.0) lon = lon - 360.0
    _RETURN(_SUCCESS)
  end subroutine easeV2_inverse

  subroutine easeV2_get_params(grid, map_scale_m, cols, rows, r0, s0, rc)
    character*(*),     intent(in)  :: grid
    real(kind=REAL64), intent(out) :: map_scale_m, r0, s0
    integer,           intent(out) :: cols, rows
    integer, optional, intent(out) :: rc
    integer :: status

    if (grid(1:1).eq.'M') then
       select case (grid)
       case ('M36'); map_scale_m=36032.220840584;  cols=964;   rows=406;  r0=(cols-1)/2.0; s0=(rows-1)/2.0
       case ('M25'); map_scale_m=25025.2600000;     cols=1388;  rows=584;  r0=(cols-1)/2.0; s0=(rows-1)/2.0
       case ('M09'); map_scale_m=9008.055210146;    cols=3856;  rows=1624; r0=(cols-1)/2.0; s0=(rows-1)/2.0
       case ('M03'); map_scale_m=3002.6850700487;   cols=11568; rows=4872; r0=(cols-1)/2.0; s0=(rows-1)/2.0
       case ('M01'); map_scale_m=1000.89502334956;  cols=34704; rows=14616;r0=(cols-1)/2.0; s0=(rows-1)/2.0
       case default; _FAIL('easeV2_get_params(): unknown resolution: '//grid)
       end select
    else if ((grid(1:1).eq.'N').or.(grid(1:1).eq.'S')) then
       _FAIL('easeV2_get_params(): polar projections not implemented yet')
    else
       _FAIL('easeV2_get_params(): unknown projection: '//grid)
    endif
    _RETURN(_SUCCESS)
  end subroutine easeV2_get_params

end module mapl3g_EASEConversion
