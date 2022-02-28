module MAPL_ExternalGCStorage
use esmf
implicit none

type ExternalGCStorage
  type(ESMF_State)    :: expState
  type(ESMF_GridComp) :: gc
end type ExternalGCStorage

type ExternalGCStorageWrap
  type (ExternalGCStorage), pointer :: PTR
end type ExternalGCStorageWrap

end module MAPL_ExternalGCStorage
