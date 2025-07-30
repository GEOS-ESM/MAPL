module mapl3g_ExtDataConstants
implicit none
private

  integer, parameter, public    :: ExtData_Not_Found         = 0
  integer, parameter, public    :: Primary_Type_Scalar = 1
  integer, parameter, public    :: Primary_Type_Vector_comp1 = 2
  integer, parameter, public    :: Primary_Type_Vector_comp2 = 3
  integer, parameter, public    :: Derived_TYpe      = 4
  integer, parameter, public    :: time_not_found = -1
  character(len=14), parameter, public :: file_not_found = "file_not_found"

end module mapl3g_ExtDataConstants
