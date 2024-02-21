module hconfing_value

   use hconfig_value_base
   use hconfig_i4
   use hconfig_i8
   use hconfig_r4
   use hconfig_r8
   use hconfig_logical
   use hconfig_string
   implicit none

   interface get_value
      module procedure :: get_value_i4
      module procedure :: get_value_i8
      module procedure :: get_value_r4
      module procedure :: get_value_r8
      module procedure :: get_value_logical
      module procedure :: get_value_string
   end interface get_value

end module hconfing_value
