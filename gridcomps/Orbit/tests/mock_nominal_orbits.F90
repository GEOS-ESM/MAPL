!------------------------------------------------------------------------------
!               Global Modeling and Assimilation Office (GMAO)                !
!                    Goddard Earth Observing System (GEOS)                    !
!                                 MAPL Component                              !
!------------------------------------------------------------------------------
!
!> Mock implementation of MAPL_NominalOrbitsMod for testing purposes.
!> Provides deterministic, predictable orbit patterns for unit testing.
!
module MAPL_NominalOrbitsMod

   implicit none
   private

   public :: orbits_track
   public :: orbits_swath
   public :: dp

   integer, parameter :: dp = selected_real_kind(15, 307)

contains

   !---------------------------------------------------------------------------
   !> Generate orbit track (nadir points only, no swath)
   !> Supports several predefined test patterns based on satellite name
   subroutine orbits_track(tlons, tlats, sat_name, nymd, nhms, dt, rc)
      real(dp), pointer, intent(out) :: tlons(:)   ! longitude array (degrees)
      real(dp), pointer, intent(out) :: tlats(:)   ! latitude array (degrees)
      character(len=*), intent(in)   :: sat_name   ! satellite name
      integer, intent(in)            :: nymd(2)    ! begin/end date YYYYMMDD
      integer, intent(in)            :: nhms(2)    ! begin/end time HHMMSS
      integer, intent(in)            :: dt         ! time step (seconds)
      integer, optional, intent(out) :: rc         ! return code

      integer :: nobs, i
      real(dp) :: lon, lat
      
      if (present(rc)) rc = 0

      ! Determine number of observations (simplified: 10 points per pattern)
      nobs = 10

      allocate(tlons(nobs), tlats(nobs))

      select case (trim(sat_name))
         
      case ('EQUATORIAL')
         ! Equatorial track: lat = 0, lon varies 0 -> 350 (degrees)
         do i = 1, nobs
            tlons(i) = real(i-1, dp) * 350.0_dp / real(nobs-1, dp)
            tlats(i) = 0.0_dp
         end do

      case ('POLAR')
         ! Polar orbit: crosses dateline, lat varies -85 -> +85 (degrees)
         do i = 1, nobs
            lon = 180.0_dp + real(i-1, dp) * 20.0_dp / real(nobs-1, dp)
            ! Wrap around dateline
            if (lon > 180.0_dp) lon = lon - 360.0_dp
            tlons(i) = lon
            tlats(i) = -85.0_dp + real(i-1, dp) * 170.0_dp / real(nobs-1, dp)
         end do

      case ('DIAGONAL')
         ! Diagonal track: both lat and lon vary (degrees)
         do i = 1, nobs
            tlons(i) = real(i-1, dp) * 180.0_dp / real(nobs-1, dp)
            tlats(i) = -45.0_dp + real(i-1, dp) * 90.0_dp / real(nobs-1, dp)
         end do

      case ('DATELINE_CROSSING')
         ! Crosses dateline: lon transitions 170 -> -170 (degrees)
         do i = 1, nobs
            lon = 170.0_dp + real(i-1, dp) * 20.0_dp / real(nobs-1, dp)
            if (lon > 180.0_dp) lon = lon - 360.0_dp
            tlons(i) = lon
            tlats(i) = 0.0_dp
         end do

      case ('PRIME_MERIDIAN')
         ! Crosses prime meridian: lon transitions -10 -> +10 (degrees)
         do i = 1, nobs
            tlons(i) = -10.0_dp + real(i-1, dp) * 20.0_dp / real(nobs-1, dp)
            tlats(i) = 30.0_dp
         end do

      case ('ARCTIC')
         ! Arctic region: high northern latitudes (degrees)
         do i = 1, nobs
            tlons(i) = real(i-1, dp) * 360.0_dp / real(nobs-1, dp)
            tlats(i) = 86.0_dp
         end do

      case ('ANTARCTIC')
         ! Antarctic region: high southern latitudes (degrees)
         do i = 1, nobs
            tlons(i) = real(i-1, dp) * 360.0_dp / real(nobs-1, dp)
            tlats(i) = -87.0_dp
         end do

      case default
         ! Default: simple equatorial track (degrees)
         do i = 1, nobs
            tlons(i) = real(i-1, dp) * 360.0_dp / real(nobs-1, dp)
            tlats(i) = 0.0_dp
         end do

      end select

   end subroutine orbits_track

   !---------------------------------------------------------------------------
   !> Generate orbit swath (3-point cross-track: left, center, right)
   !> slons(1,:) = left edge, slons(2,:) = nadir, slons(3,:) = right edge
   subroutine orbits_swath(slons, slats, sat_name, nymd, nhms, dt, swath_width, rc, wrap)
      real(dp), pointer, intent(out) :: slons(:,:)  ! longitude array (3, nobs)
      real(dp), pointer, intent(out) :: slats(:,:)  ! latitude array (3, nobs)
      character(len=*), intent(in)   :: sat_name    ! satellite name
      integer, intent(in)            :: nymd(2)     ! begin/end date YYYYMMDD
      integer, intent(in)            :: nhms(2)     ! begin/end time HHMMSS
      integer, intent(in)            :: dt          ! time step (seconds)
      real(dp), intent(in)           :: swath_width(2) ! left/right swath width (km)
      integer, optional, intent(out) :: rc          ! return code
      logical, optional, intent(in)  :: wrap        ! wrap longitude

      integer :: nobs, i
      real(dp), pointer :: tlons(:), tlats(:)
      real(dp) :: swath_deg, lat_factor
      
      if (present(rc)) rc = 0

      ! Get nadir track first
      call orbits_track(tlons, tlats, sat_name, nymd, nhms, dt, rc)
      nobs = size(tlons)

      allocate(slons(3, nobs), slats(3, nobs))

      ! Approximate swath width in degrees (simplified: ~111 km per degree at equator)
      ! Adjust for latitude
      do i = 1, nobs
         lat_factor = cos(tlats(i) * 3.14159265358979_dp / 180.0_dp)
         if (abs(lat_factor) < 0.01_dp) lat_factor = 0.01_dp ! Avoid division by zero
         
         swath_deg = swath_width(1) / 111.0_dp / lat_factor

         ! Left edge
         slons(1, i) = tlons(i) - swath_deg
         slats(1, i) = tlats(i)

         ! Center (nadir)
         slons(2, i) = tlons(i)
         slats(2, i) = tlats(i)

         ! Right edge
         slons(3, i) = tlons(i) + swath_deg
         slats(3, i) = tlats(i)

         ! Wrap longitude if requested
         if (present(wrap)) then
            if (wrap) then
               if (slons(1, i) < -180.0_dp) slons(1, i) = slons(1, i) + 360.0_dp
               if (slons(1, i) > 180.0_dp) slons(1, i) = slons(1, i) - 360.0_dp
               if (slons(3, i) < -180.0_dp) slons(3, i) = slons(3, i) + 360.0_dp
               if (slons(3, i) > 180.0_dp) slons(3, i) = slons(3, i) - 360.0_dp
            end if
         end if
      end do

      deallocate(tlons, tlats)

   end subroutine orbits_swath

end module MAPL_NominalOrbitsMod
