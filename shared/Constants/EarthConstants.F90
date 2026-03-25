module MAPL_EarthConstants

   use, intrinsic :: iso_fortran_env, only: REAL64, REAL32
   use MAPL_MathConstants, only: MAPL_PI_R8, MAPL_PI, &
                                  MAPL_RADIANS_TO_DEGREES, MAPL_DEGREES_TO_RADIANS_R8

   implicit none

   ! Earth geometric and orbital constants.
   ! These are Earth-specific and would differ for other planets.

   real(kind=REAL64), parameter :: MAPL_PSDRY                    = 98305.0_REAL64  ! mean dry surface pressure          Pa
   real,              parameter :: MAPL_SECONDS_PER_SIDEREAL_DAY = 86164.0         ! length of Earth sidereal day        s
   real,              parameter :: MAPL_GRAV                     = 9.80665         ! mean surface gravitational accel    m/s^2
   real,              parameter :: MAPL_RADIUS                   = 6371.0E3        ! mean radius of Earth                m
   real(kind=REAL64), parameter :: MAPL_OMEGA_R8                 = &
        2.0_REAL64 * MAPL_PI_R8 / MAPL_SECONDS_PER_SIDEREAL_DAY                   ! Earth rotation rate (REAL64)        1/s
   real(kind=REAL32), parameter :: MAPL_OMEGA                    = &
        2.0 * MAPL_PI / MAPL_SECONDS_PER_SIDEREAL_DAY                             ! Earth rotation rate (REAL32)        1/s
   real(kind=REAL64), parameter :: MAPL_EARTH_ECCENTRICITY       = &
        8.181919084262200d-2                                                        ! WGS84 first eccentricity            --
   real(kind=REAL64), parameter :: MAPL_EARTH_SEMIMAJOR_AXIS     = 6378137.0_REAL64 ! WGS84 semi-major axis              m
   real(kind=REAL64), parameter :: MAPL_KM_PER_DEG               = &
        (1.0_REAL64 / (MAPL_RADIUS / 1000.0_REAL64)) * MAPL_RADIANS_TO_DEGREES    ! km per degree of arc                km/deg
   real(kind=REAL64), parameter :: MAPL_DEG_PER_KM               = &
        (MAPL_RADIUS / 1000.0_REAL64) * MAPL_DEGREES_TO_RADIANS_R8                ! degrees per km of arc               deg/km

end module MAPL_EarthConstants
