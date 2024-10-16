module mapl3g_hconfig_get

   use mapl3g_hconfig_get_private, only: MAPL_HConfigGet => get_value
   use mapl3g_hconfig_params, only: HConfigParams

   implicit none (type, external)

   public :: MAPL_HConfigGet

end module mapl3g_hconfig_get
