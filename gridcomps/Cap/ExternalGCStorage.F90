module MAPL_ExternalGCStorage
use esmf
implicit none

type t_extdata_state
  type(ESMF_State)    :: expState
  type(ESMF_GridComp) :: gc
end type t_extdata_state

type extdata_wrap
  type (t_extdata_state), pointer :: PTR
end type extdata_wrap

end module MAPL_ExternalGCStorage
