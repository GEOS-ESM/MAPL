module mapl3g_ExtDataConstants
implicit none
private

  integer, parameter, public    :: EXTDATA_NOT_FOUND         = 0
  integer, parameter, public    :: PRIMARY_TYPE_SCALAR = 1
  integer, parameter, public    :: PRIMARY_TYPE_VECTOR_COMP1 = 2
  integer, parameter, public    :: PRIMARY_TYPE_VECTOR_COMP2 = 3
  integer, parameter, public    :: DERIVED_TYPE      = 4
  character(len=14), parameter, public :: FILE_NOT_FOUND = "file_not_found"

end module mapl3g_ExtDataConstants
