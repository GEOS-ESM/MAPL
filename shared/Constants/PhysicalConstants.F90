module MAPL_PhysicalConstants

   implicit none

   ! Universal physical constants.  These are independent of the choice of planet.
   !
   ! CODATA 2018 note: MAPL_AVOGADRO and MAPL_UNIVERSAL_GAS_CONSTANT became exact
   ! (defined) values in the 2019 SI redefinition.  MAPL_STEFAN_BOLTZMANN is
   ! derivable from other defined constants (sigma = 2 pi^5 k^4 / 15 h^3 c^2)
   ! and is thus also exact under CODATA 2018.
   ! Build with -DUSE_CODATA_2018_CONSTANTS to use those more precise values.

#if defined(CODATA_2018_CONSTANTS)
   real, parameter :: MAPL_STEFAN_BOLTZMANN      = 5.670374419E-8  ! Stefan-Boltzmann constant  W/(m^2 K^4) [exact]
   real, parameter :: MAPL_AVOGADRO              = 6.02214076E26   ! Avogadro constant           1/kmol      [exact]
   real, parameter :: MAPL_UNIVERSAL_GAS_CONSTANT = 8314.462618    ! Universal gas constant       J/(Kmole K) [exact]
#else
   real, parameter :: MAPL_STEFAN_BOLTZMANN      = 5.6734E-8       ! Stefan-Boltzmann constant  W/(m^2 K^4)
   real, parameter :: MAPL_AVOGADRO              = 6.023E26        ! Avogadro constant           1/kmol
   real, parameter :: MAPL_UNIVERSAL_GAS_CONSTANT = 8314.47        ! Universal gas constant       J/(Kmole K)
#endif

   ! Deprecated aliases
   real, parameter :: MAPL_STFBOL = MAPL_STEFAN_BOLTZMANN       ! DEPRECATED: use MAPL_STEFAN_BOLTZMANN
   real, parameter :: MAPL_AVOGAD = MAPL_AVOGADRO               ! DEPRECATED: use MAPL_AVOGADRO
   real, parameter :: MAPL_RUNIV  = MAPL_UNIVERSAL_GAS_CONSTANT  ! DEPRECATED: use MAPL_UNIVERSAL_GAS_CONSTANT

end module MAPL_PhysicalConstants
