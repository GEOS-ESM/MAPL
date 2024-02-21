use hconfig_value_base
implicit none

type, extends(HConfigValue) :: HConfigValueUT_
  TYPE_ :: value_
  TYPE_, allocatable :: default_
contains
  procedure :: set_from_hconfig => set_from_hconfig_LT_
  procedure :: set_from_default => set_from_default_LT_
  procedure :: value_equals_default => value_equals_default_LT_
  procedure :: get_valuestring => get_valuestring_LT_
end type HConfigValueUT_

interface HConfigValueUT_
  module procedure :: construct_hconfig_value_LT_
end interface HConfigValueUT_
