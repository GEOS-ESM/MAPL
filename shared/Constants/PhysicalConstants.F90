module MAPL_PhysicalConstantsMod

   use, intrinsic :: iso_fortran_env, only: REAL64, REAL32
   use MAPL_MathConstantsMod, only: MAPL_PI_R8, MAPL_PI, MAPL_RADIANS_TO_DEGREES, MAPL_DEGREES_TO_RADIANS_R8
   implicit none

!=============================================================================
!BOP

! !MODULE: -- A container module for MAPL physical constants

! !PUBLIC VARIABLES:

   ! Universal Constants
#if defined(CODATA_2018_CONSTANTS)
   real, parameter              :: MAPL_STFBOL                    = 5.670374419E-8                               ! W/(m^2 K^4)
   real, parameter              :: MAPL_AVOGAD                    = 6.02214076E26                                ! 1/kmol
   real, parameter              :: MAPL_RUNIV                     = 8314.462618                                  ! J/(Kmole K)
#else
   real, parameter              :: MAPL_STFBOL                    = 5.6734E-8                                    ! W/(m^2 K^4)
   real, parameter              :: MAPL_AVOGAD                    = 6.023E26                                     ! 1/kmol
   real, parameter              :: MAPL_RUNIV                     = 8314.47                                      ! J/(Kmole K)
#endif
   
   ! Earth Constants
   real(kind=REAL64), parameter :: MAPL_PSDRY                     = 98305.0_REAL64                                ! Pa
   real, parameter              :: MAPL_SECONDS_PER_SIDEREAL_DAY  = 86164.0                                       ! s
   real, parameter              :: MAPL_GRAV                      = 9.80665                                       ! m^2/s
   real, parameter              :: MAPL_RADIUS                    = 6371.0E3                                      ! m
   real(kind=REAL64), parameter :: MAPL_OMEGA_R8                  = 2.0*MAPL_PI_R8/MAPL_SECONDS_PER_SIDEREAL_DAY  ! 1/s
   real(kind=REAL32), parameter :: MAPL_OMEGA                     = 2.0*MAPL_PI/MAPL_SECONDS_PER_SIDEREAL_DAY     ! 1/s
   real(kind=REAL64), parameter :: MAPL_EARTH_ECCENTRICITY        = 8.1819190842622d-2                            ! --
   real(kind=REAL64), parameter :: MAPL_EARTH_SEMIMAJOR_AXIS      = 6378137                                       ! m
   real(kind=REAL64), parameter :: MAPL_KM_PER_DEG                    = (1.0/(MAPL_RADIUS/1000.)) * MAPL_RADIANS_TO_DEGREES
   real(kind=REAL64), parameter :: MAPL_DEG_PER_KM                    = (MAPL_RADIUS/1000.) * MAPL_DEGREES_TO_RADIANS_R8


   ! Physical properties
   real, parameter              :: MAPL_H2OMW                     =  18.015                                       ! kg/Kmole
   real, parameter              :: MAPL_O3MW                      = 47.9982                                       ! kg/Kmole
   real, parameter              :: MAPL_LATENT_HEAT_VAPORIZATION  = 2.4665E6                                      ! J/kg @15C @1atm
   real, parameter              :: MAPL_ALHL                      = MAPL_LATENT_HEAT_VAPORIZATION                 ! J/kg 
   real, parameter              :: MAPL_LATENT_HEAT_FUSION        = 3.3370E5                                      ! J/kg @1atm
   real, parameter              :: MAPL_ALHF                      = MAPL_LATENT_HEAT_FUSION                       ! J/kg
   real, parameter              :: MAPL_LATENT_HEAT_SUBLIMATION   = MAPL_ALHL+MAPL_ALHF                           ! J/kg
   real, parameter              :: MAPL_ALHS                      = MAPL_LATENT_HEAT_SUBLIMATION                  ! J/kg
  
   ! Earth Specific Chemistry and Thermodynamic Constants
   real, parameter              :: MAPL_AIRMW                     =  28.965                                       ! kg/Kmole
   real, parameter              :: MAPL_RDRY                      = MAPL_RUNIV/MAPL_AIRMW                         ! J/(kg K)
   real, parameter              :: MAPL_CPDRY                     = 3.5*MAPL_RDRY                                 ! J/(kg K)
   real, parameter              :: MAPL_CVDRY                     = MAPL_CPDRY-MAPL_RDRY                          ! J/(kg K)
   real, parameter              :: MAPL_RVAP                      = MAPL_RUNIV/MAPL_H2OMW                         ! J/(kg K)
   real, parameter              :: MAPL_CPVAP                     = 4.*MAPL_RVAP                                  ! J/(kg K)
   real, parameter              :: MAPL_CVVAP                     = MAPL_CPVAP-MAPL_RVAP                          ! J/(kg K)
   real, parameter              :: MAPL_KAPPA                     = MAPL_RDRY/MAPL_CPDRY                          ! (2.0/7.0)
   real, parameter              :: MAPL_EPSILON                   = MAPL_H2OMW/MAPL_AIRMW                         ! --
   real, parameter              :: MAPL_DELTAP                    = MAPL_CPVAP/MAPL_CPDRY                         ! --
   real, parameter              :: MAPL_DELTAV                    = MAPL_CVVAP/MAPL_CVDRY                         ! --
   real, parameter              :: MAPL_GAMMAD                    = MAPL_CPDRY/MAPL_CVDRY                         ! --
   real, parameter              :: MAPL_RGAS                      = MAPL_RDRY                                     ! J/(kg K) (DEPRECATED)
   real, parameter              :: MAPL_CP                        = MAPL_RGAS/MAPL_KAPPA                          ! J/(kg K) (DEPRECATED)
   real, parameter              :: MAPL_VIREPS                    = 1.0/MAPL_EPSILON-1.0                          ! (DEPRECATED)
   real, parameter              :: MAPL_P00                       = 100000.0                                      ! Pa
   real, parameter              :: MAPL_CAPICE                    = 2000.                                         ! J/(K kg)
   real, parameter              :: MAPL_CAPWTR                    = 4218.                                         ! J/(K kg)
   real, parameter              :: MAPL_RHOWTR                    = 1000.                                         ! kg/m^3
   real, parameter              :: MAPL_NUAIR                     = 1.533E-5                                      ! m^2/S (@ 18C)
   real, parameter              :: MAPL_TICE                      = 273.16                                        ! K
   real, parameter              :: MAPL_SRFPRS                    = 98470                                         ! Pa
   real, parameter              :: MAPL_KARMAN                    = 0.40                                          ! --
   real, parameter              :: MAPL_USMIN                     = 1.00                                          ! m/s
   real, parameter              :: MAPL_RHO_SEAWATER              = 1026.0                                        ! sea water density [kg/m^3]
   real, parameter              :: MAPL_RHO_SEAICE                = 917.0                                         ! sea ice   density [kg/m^3]
   real, parameter              :: MAPL_RHO_SNOW                  = 330.0                                         ! snow density      [kg/m^3]
   real, parameter              :: MAPL_CELSIUS_TO_KELVIN         = 273.15                                        ! K

!EOP

end module MAPL_PhysicalConstantsMod
