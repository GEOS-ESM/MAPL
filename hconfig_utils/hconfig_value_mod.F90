module hconfig_value_mod

   use hconfig_i4
   use hconfig_i8
   use hconfig_r4
   use hconfig_r8
   use hconfig_logical
   use hconfig_string
   use hconfig_i4seq
   use hconfig_i8seq
   use hconfig_r4seq
   use hconfig_r8seq
   use hconfig_logical_seq
   implicit none

   public :: get_hconfig_value

   interface get_hconfig_value
      ! add individual get_hconfig_ subroutines
   end interface get_hconfig_value

end module hconfig_value_mod
