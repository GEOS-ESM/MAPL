module MAPL_EarthConstants

   use, intrinsic :: iso_fortran_env, only: REAL64, REAL32
   use MAPL_MathConstants, only: MAPL_PI_R8, MAPL_PI, &
                                  MAPL_RADIANS_TO_DEGREES, MAPL_DEGREES_TO_RADIANS_R8

   implicit none

   ! Earth geometric and orbital constants.
   ! Names are planet-independent: client code using a future MAPL_MarsConstants
   ! module will reference the same names without modification.

   real(kind=REAL64), parameter :: MAPL_MEAN_DRY_SURFACE_PRESSURE = 98305.0_REAL64  ! Pa
   real,              parameter :: MAPL_SECONDS_PER_SIDEREAL_DAY  = 86164.0          ! s
   real,              parameter :: MAPL_GRAVITY                   = 9.80665          ! mean surface gravitational acceleration  m/s^2
   real(kind=REAL64), parameter :: MAPL_RADIUS                    = 6371.0E3_REAL64  ! mean radius  m
   real(kind=REAL64), parameter :: MAPL_ROTATION_RATE_R8          = &
        2.0_REAL64 * MAPL_PI_R8 / MAPL_SECONDS_PER_SIDEREAL_DAY                     ! rotation rate (REAL64)  1/s
   real(kind=REAL32), parameter :: MAPL_ROTATION_RATE             = &
        real(MAPL_ROTATION_RATE_R8, REAL32)                                          ! rotation rate (REAL32)  1/s
   real(kind=REAL64), parameter :: MAPL_ECCENTRICITY              = &
        8.181919084262200d-2                                                          ! WGS84 first eccentricity  --
   real(kind=REAL64), parameter :: MAPL_SEMIMAJOR_AXIS            = 6378137.0_REAL64 ! WGS84 semi-major axis  m
   real(kind=REAL64), parameter :: MAPL_KM_PER_DEGREE             = &
        (1.0_REAL64 / (MAPL_RADIUS / 1000.0_REAL64)) * MAPL_RADIANS_TO_DEGREES      ! km per degree of arc  km/deg
   real(kind=REAL64), parameter :: MAPL_DEGREE_PER_KM             = &
        (MAPL_RADIUS / 1000.0_REAL64) * MAPL_DEGREES_TO_RADIANS_R8                  ! degrees per km of arc  deg/km

   ! Deprecated aliases
   real(kind=REAL64), parameter :: MAPL_PSDRY              = MAPL_MEAN_DRY_SURFACE_PRESSURE ! DEPRECATED
   real,              parameter :: MAPL_GRAV               = MAPL_GRAVITY                   ! DEPRECATED
   real(kind=REAL64), parameter :: MAPL_OMEGA_R8           = MAPL_ROTATION_RATE_R8          ! DEPRECATED
   real(kind=REAL32), parameter :: MAPL_OMEGA              = MAPL_ROTATION_RATE             ! DEPRECATED
   real(kind=REAL64), parameter :: MAPL_EARTH_ECCENTRICITY = MAPL_ECCENTRICITY              ! DEPRECATED
   real(kind=REAL64), parameter :: MAPL_EARTH_SEMIMAJOR_AXIS = MAPL_SEMIMAJOR_AXIS          ! DEPRECATED
   real(kind=REAL64), parameter :: MAPL_KM_PER_DEG         = MAPL_KM_PER_DEGREE            ! DEPRECATED
   real(kind=REAL64), parameter :: MAPL_DEG_PER_KM         = MAPL_DEGREE_PER_KM            ! DEPRECATED

end module MAPL_EarthConstants
