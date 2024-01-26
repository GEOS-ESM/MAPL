module MAPL_GridCompsMod
   use mapl_CapMod
   use mapl_CapOptionsMod
   use mapl_externalGCStorage
#ifdef USE_FLAP
   use mapl_FlapCLIMod
#endif
#ifdef USE_FARGPARSE
   use mapl_FargParseCLIMod
#endif
   implicit none
end module MAPL_GridCompsMod
