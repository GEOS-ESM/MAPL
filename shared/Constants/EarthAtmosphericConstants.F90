module MAPL_EarthAtmosphericConstants

   use MAPL_PhysicalConstants, only: MAPL_RUNIV

   implicit none

   ! Earth atmospheric and thermodynamic constants.
   ! These depend on the composition of Earth's atmosphere and the properties
   ! of Earth-surface water/ice.  They would differ for other planets.

   ! Molecular weights  [kg/Kmole]
   real, parameter :: MAPL_AIRMW  = 28.965   ! mean molecular weight of dry air
   real, parameter :: MAPL_H2OMW  = 18.015   ! molecular weight of water vapour
   real, parameter :: MAPL_O3MW   = 47.9982  ! molecular weight of ozone

   ! Gas constants derived from molecular weights  [J/(kg K)]
   real, parameter :: MAPL_RDRY   = MAPL_RUNIV / MAPL_AIRMW   ! gas constant for dry air
   real, parameter :: MAPL_RVAP   = MAPL_RUNIV / MAPL_H2OMW   ! gas constant for water vapour

   ! Heat capacities  [J/(kg K)]
   real, parameter :: MAPL_CPDRY  = 3.5 * MAPL_RDRY            ! specific heat of dry air at constant pressure
   real, parameter :: MAPL_CVDRY  = MAPL_CPDRY - MAPL_RDRY     ! specific heat of dry air at constant volume
   real, parameter :: MAPL_CPVAP  = 4.0 * MAPL_RVAP            ! specific heat of water vapour at constant pressure
   real, parameter :: MAPL_CVVAP  = MAPL_CPVAP - MAPL_RVAP     ! specific heat of water vapour at constant volume

   ! Dimensionless thermodynamic ratios
   real, parameter :: MAPL_KAPPA   = MAPL_RDRY  / MAPL_CPDRY   ! Poisson exponent (= 2/7 for diatomic ideal gas)
   real, parameter :: MAPL_EPSILON = MAPL_H2OMW / MAPL_AIRMW   ! ratio of molecular weights
   real, parameter :: MAPL_DELTAP  = MAPL_CPVAP / MAPL_CPDRY   ! Cp(vapour)/Cp(dry)
   real, parameter :: MAPL_DELTAV  = MAPL_CVVAP / MAPL_CVDRY   ! Cv(vapour)/Cv(dry)
   real, parameter :: MAPL_GAMMAD  = MAPL_CPDRY / MAPL_CVDRY   ! adiabatic index for dry air

   ! Deprecated aliases (kept for backwards compatibility)
   real, parameter :: MAPL_RGAS   = MAPL_RDRY                   ! (DEPRECATED: use MAPL_RDRY)
   real, parameter :: MAPL_CP     = MAPL_RGAS / MAPL_KAPPA      ! (DEPRECATED: use MAPL_CPDRY)
   real, parameter :: MAPL_VIREPS = 1.0 / MAPL_EPSILON - 1.0    ! (DEPRECATED)

   ! Latent heats  [J/kg]
   real, parameter :: MAPL_LATENT_HEAT_VAPORIZATION  = 2.4665E6                                  ! @ 15 C, 1 atm
   real, parameter :: MAPL_ALHL                       = MAPL_LATENT_HEAT_VAPORIZATION             ! alias
   real, parameter :: MAPL_LATENT_HEAT_FUSION         = 3.3370E5                                  ! @ 1 atm
   real, parameter :: MAPL_ALHF                       = MAPL_LATENT_HEAT_FUSION                   ! alias
   real, parameter :: MAPL_LATENT_HEAT_SUBLIMATION    = MAPL_ALHL + MAPL_ALHF
   real, parameter :: MAPL_ALHS                       = MAPL_LATENT_HEAT_SUBLIMATION              ! alias

   ! Reference pressures and temperatures
   real, parameter :: MAPL_P00              = 100000.0  ! reference pressure                       Pa
   real, parameter :: MAPL_SRFPRS          = 98470.0    ! mean surface pressure                    Pa
   real, parameter :: MAPL_TICE            = 273.16     ! freezing point of water                  K
   real, parameter :: MAPL_CELSIUS_TO_KELVIN = 273.15   ! 0 C in Kelvin                            K

   ! Thermal and density properties of water and ice
   real, parameter :: MAPL_CAPICE    = 2000.   ! specific heat of ice                 J/(K kg)
   real, parameter :: MAPL_CAPWTR    = 4218.   ! specific heat of liquid water         J/(K kg)
   real, parameter :: MAPL_RHOWTR    = 1000.   ! density of liquid water               kg/m^3
   real, parameter :: MAPL_RHO_SEAWATER = 1026.0  ! density of sea water              kg/m^3
   real, parameter :: MAPL_RHO_SEAICE   = 917.0   ! density of sea ice                kg/m^3
   real, parameter :: MAPL_RHO_SNOW     = 330.0   ! density of snow                   kg/m^3

   ! Atmospheric boundary-layer parameters
   real, parameter :: MAPL_NUAIR  = 1.533E-5  ! kinematic viscosity of air @ 18 C    m^2/s
   real, parameter :: MAPL_KARMAN = 0.40      ! von Karman constant                  --
   real, parameter :: MAPL_USMIN  = 1.00      ! minimum surface wind speed            m/s

end module MAPL_EarthAtmosphericConstants
