module MAPL_EarthAtmosphericConstants

   use MAPL_PhysicalConstants, only: MAPL_UNIVERSAL_GAS_CONSTANT

   implicit none

   ! Earth atmospheric and thermodynamic constants.
   ! Names are planet-independent: client code using a future MAPL_MarsAtmosphericConstants
   ! module will reference the same names without modification.

   ! Molecular weights  [kg/Kmole]
   real, parameter :: MAPL_MOLECULAR_WEIGHT_DRY_AIR = 28.965   ! mean molecular weight of dry air
   real, parameter :: MAPL_MOLECULAR_WEIGHT_WATER    = 18.015   ! molecular weight of water vapour
   real, parameter :: MAPL_MOLECULAR_WEIGHT_OZONE    = 47.9982  ! molecular weight of ozone

   ! Gas constants  [J/(kg K)]
   real, parameter :: MAPL_GAS_CONSTANT_DRY_AIR    = MAPL_UNIVERSAL_GAS_CONSTANT / MAPL_MOLECULAR_WEIGHT_DRY_AIR
   real, parameter :: MAPL_GAS_CONSTANT_WATER_VAPOR = MAPL_UNIVERSAL_GAS_CONSTANT / MAPL_MOLECULAR_WEIGHT_WATER

   ! Specific heats  [J/(kg K)]
   real, parameter :: MAPL_SPECIFIC_HEAT_DRY_AIR_CONST_PRESSURE    = 3.5 * MAPL_GAS_CONSTANT_DRY_AIR
   real, parameter :: MAPL_SPECIFIC_HEAT_DRY_AIR_CONST_VOLUME       = &
        MAPL_SPECIFIC_HEAT_DRY_AIR_CONST_PRESSURE - MAPL_GAS_CONSTANT_DRY_AIR
   real, parameter :: MAPL_SPECIFIC_HEAT_WATER_VAPOR_CONST_PRESSURE = 4.0 * MAPL_GAS_CONSTANT_WATER_VAPOR
   real, parameter :: MAPL_SPECIFIC_HEAT_WATER_VAPOR_CONST_VOLUME   = &
        MAPL_SPECIFIC_HEAT_WATER_VAPOR_CONST_PRESSURE - MAPL_GAS_CONSTANT_WATER_VAPOR

   ! Dimensionless thermodynamic ratios
   real, parameter :: MAPL_KAPPA                = &
        MAPL_GAS_CONSTANT_DRY_AIR / MAPL_SPECIFIC_HEAT_DRY_AIR_CONST_PRESSURE  ! Poisson exponent (~2/7)
   real, parameter :: MAPL_MOLECULAR_WEIGHT_RATIO = &
        MAPL_MOLECULAR_WEIGHT_WATER / MAPL_MOLECULAR_WEIGHT_DRY_AIR             ! Mw/Md
   real, parameter :: MAPL_CP_VAPOR_OVER_DRY_AIR  = &
        MAPL_SPECIFIC_HEAT_WATER_VAPOR_CONST_PRESSURE / MAPL_SPECIFIC_HEAT_DRY_AIR_CONST_PRESSURE
   real, parameter :: MAPL_CV_VAPOR_OVER_DRY_AIR  = &
        MAPL_SPECIFIC_HEAT_WATER_VAPOR_CONST_VOLUME / MAPL_SPECIFIC_HEAT_DRY_AIR_CONST_VOLUME
   real, parameter :: MAPL_ADIABATIC_INDEX        = &
        MAPL_SPECIFIC_HEAT_DRY_AIR_CONST_PRESSURE / MAPL_SPECIFIC_HEAT_DRY_AIR_CONST_VOLUME

   ! Latent heats  [J/kg]
   real, parameter :: MAPL_LATENT_HEAT_VAPORIZATION = 2.4665E6                                    ! @ 15 C, 1 atm
   real, parameter :: MAPL_LATENT_HEAT_FUSION        = 3.3370E5                                    ! @ 1 atm
   real, parameter :: MAPL_LATENT_HEAT_SUBLIMATION   = MAPL_LATENT_HEAT_VAPORIZATION + MAPL_LATENT_HEAT_FUSION

   ! Reference pressures and temperatures
   real, parameter :: MAPL_REFERENCE_PRESSURE  = 100000.0  ! reference pressure              Pa
   real, parameter :: MAPL_MEAN_SURFACE_PRESSURE = 98470.0 ! mean surface pressure           Pa
   real, parameter :: MAPL_FREEZING_POINT       = 273.16   ! freezing point of water         K
   real, parameter :: MAPL_CELSIUS_TO_KELVIN    = 273.15   ! 0 C in Kelvin                   K

   ! Thermal and density properties of water and ice
   real, parameter :: MAPL_SPECIFIC_HEAT_ICE    = 2000.    ! specific heat of ice            J/(K kg)
   real, parameter :: MAPL_SPECIFIC_HEAT_WATER  = 4218.    ! specific heat of liquid water   J/(K kg)
   real, parameter :: MAPL_DENSITY_LIQUID_WATER = 1000.    ! density of liquid water         kg/m^3
   real, parameter :: MAPL_DENSITY_SEAWATER     = 1026.0   ! density of sea water            kg/m^3
   real, parameter :: MAPL_DENSITY_SEAICE       = 917.0    ! density of sea ice              kg/m^3
   real, parameter :: MAPL_DENSITY_SNOW         = 330.0    ! density of snow                 kg/m^3

   ! Atmospheric boundary-layer parameters
   real, parameter :: MAPL_KINEMATIC_VISCOSITY_AIR = 1.533E-5 ! kinematic viscosity @ 18 C   m^2/s
   real, parameter :: MAPL_VON_KARMAN              = 0.40     ! von Karman constant           --
   real, parameter :: MAPL_MINIMUM_WIND_SPEED      = 1.00     ! minimum surface wind speed    m/s

   ! Deprecated aliases
   real, parameter :: MAPL_AIRMW   = MAPL_MOLECULAR_WEIGHT_DRY_AIR                    ! DEPRECATED
   real, parameter :: MAPL_H2OMW   = MAPL_MOLECULAR_WEIGHT_WATER                      ! DEPRECATED
   real, parameter :: MAPL_O3MW    = MAPL_MOLECULAR_WEIGHT_OZONE                      ! DEPRECATED
   real, parameter :: MAPL_RDRY    = MAPL_GAS_CONSTANT_DRY_AIR                        ! DEPRECATED
   real, parameter :: MAPL_RVAP    = MAPL_GAS_CONSTANT_WATER_VAPOR                    ! DEPRECATED
   real, parameter :: MAPL_CPDRY   = MAPL_SPECIFIC_HEAT_DRY_AIR_CONST_PRESSURE        ! DEPRECATED
   real, parameter :: MAPL_CVDRY   = MAPL_SPECIFIC_HEAT_DRY_AIR_CONST_VOLUME          ! DEPRECATED
   real, parameter :: MAPL_CPVAP   = MAPL_SPECIFIC_HEAT_WATER_VAPOR_CONST_PRESSURE    ! DEPRECATED
   real, parameter :: MAPL_CVVAP   = MAPL_SPECIFIC_HEAT_WATER_VAPOR_CONST_VOLUME      ! DEPRECATED
   real, parameter :: MAPL_EPSILON = MAPL_MOLECULAR_WEIGHT_RATIO                      ! DEPRECATED
   real, parameter :: MAPL_DELTAP  = MAPL_CP_VAPOR_OVER_DRY_AIR                       ! DEPRECATED
   real, parameter :: MAPL_DELTAV  = MAPL_CV_VAPOR_OVER_DRY_AIR                       ! DEPRECATED
   real, parameter :: MAPL_GAMMAD  = MAPL_ADIABATIC_INDEX                             ! DEPRECATED
   real, parameter :: MAPL_RGAS    = MAPL_GAS_CONSTANT_DRY_AIR                        ! DEPRECATED
   real, parameter :: MAPL_CP      = MAPL_SPECIFIC_HEAT_DRY_AIR_CONST_PRESSURE        ! DEPRECATED
   real, parameter :: MAPL_VIREPS  = 1.0 / MAPL_MOLECULAR_WEIGHT_RATIO - 1.0          ! DEPRECATED
   real, parameter :: MAPL_ALHL    = MAPL_LATENT_HEAT_VAPORIZATION                    ! DEPRECATED
   real, parameter :: MAPL_ALHF    = MAPL_LATENT_HEAT_FUSION                          ! DEPRECATED
   real, parameter :: MAPL_ALHS    = MAPL_LATENT_HEAT_SUBLIMATION                     ! DEPRECATED
   real, parameter :: MAPL_P00     = MAPL_REFERENCE_PRESSURE                          ! DEPRECATED
   real, parameter :: MAPL_SRFPRS  = MAPL_MEAN_SURFACE_PRESSURE                       ! DEPRECATED
   real, parameter :: MAPL_TICE    = MAPL_FREEZING_POINT                              ! DEPRECATED
   real, parameter :: MAPL_CAPICE  = MAPL_SPECIFIC_HEAT_ICE                           ! DEPRECATED
   real, parameter :: MAPL_CAPWTR  = MAPL_SPECIFIC_HEAT_WATER                         ! DEPRECATED
   real, parameter :: MAPL_RHOWTR  = MAPL_DENSITY_LIQUID_WATER                        ! DEPRECATED
   real, parameter :: MAPL_RHO_SEAWATER = MAPL_DENSITY_SEAWATER                       ! DEPRECATED
   real, parameter :: MAPL_RHO_SEAICE   = MAPL_DENSITY_SEAICE                         ! DEPRECATED
   real, parameter :: MAPL_RHO_SNOW     = MAPL_DENSITY_SNOW                           ! DEPRECATED
   real, parameter :: MAPL_NUAIR   = MAPL_KINEMATIC_VISCOSITY_AIR                     ! DEPRECATED
   real, parameter :: MAPL_KARMAN  = MAPL_VON_KARMAN                                  ! DEPRECATED
   real, parameter :: MAPL_USMIN   = MAPL_MINIMUM_WIND_SPEED                          ! DEPRECATED

end module MAPL_EarthAtmosphericConstants
