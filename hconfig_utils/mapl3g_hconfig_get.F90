module mapl3g_hconfig_get

   use mapl3g_hconfig_get_private, only: MAPL_HConfigGet => get_value, MAPL_HConfigKeystringFound => keystring_found

   implicit none

   public :: MAPL_HConfigGet
   public :: MAPL_HConfigKeystringFound

end module mapl3g_hconfig_get
