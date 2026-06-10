! Export umbrella for the MAPL.component library
module mapl_component_api

   use mapl_ComponentDriver_mod, only: MAPL_DriverInitializePhases => DriverInitializePhases
   use mapl_GriddedComponentDriver_mod, only: MAPL_GriddedComponentDriver => GriddedComponentDriver

   implicit none
   private

   public :: MAPL_DriverInitializePhases
   public :: MAPL_GriddedComponentDriver

end module mapl_component_api
