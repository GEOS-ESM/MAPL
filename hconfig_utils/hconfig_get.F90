module mapl3g_hconfig_get

   use mapl3g_hconfig_get_private
   use mapl3g_hconfig_params, only: HConfigParams

   implicit none

   public :: MAPL_HConfigGet

   interface MAPL_HConfigGet
      procedure :: mapl_get_value_i4
      procedure :: mapl_get_value_i8
      procedure :: mapl_get_value_r4
      procedure :: mapl_get_value_r8
      procedure :: mapl_get_value_string
      procedure :: mapl_get_value_logical
      procedure :: mapl_get_value_i4seq
      procedure :: mapl_get_value_i8seq
      procedure :: mapl_get_value_r4seq
      procedure :: mapl_get_value_r8seq
      procedure :: mapl_get_value_logicalseq
   end interface MAPL_HConfigGet

end module mapl3g_hconfig_get
