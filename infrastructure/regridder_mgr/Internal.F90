! Internal umbrella for the MAPL infrastructure/regridder_mgr layer.
! Aggregates leaf modules for use by other MAPL subdirectories.
module mapl_regridder_mgr_internal

   use mapl_regridder_mgr
   use mapl_RoutehandleParam_mod
   use mapl_RoutehandleSpec_mod
   use mapl_RoutehandleSpecVector_mod
   use mapl_RoutehandleVector_mod
   use mapl_DynamicMask_mod
   use mapl_RoutehandleManager_mod
   use mapl_RegridderParam_mod
   use mapl_RegridderSpec_mod
   use mapl_RegridderSpecVector_mod
   use mapl_Regridder_mod
   use mapl_RegridderVector_mod
   use mapl_NullRegridder_mod
   use mapl_EsmfRegridder_mod
   use mapl_RegridderFactory_mod
   use mapl_EsmfRegridderFactory_mod
   use mapl_RegridderFactoryVector_mod
   use mapl_RegridderManager_mod
   use mapl_RegridderMethods_mod

   implicit none

end module mapl_regridder_mgr_internal
