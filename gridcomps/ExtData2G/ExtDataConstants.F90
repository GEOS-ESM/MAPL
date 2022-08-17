module MAPL_ExtDataConstants
implicit none
private 

  public    :: ExtData_Not_Found
  public    :: Primary_Type_Scalar 
  public    :: Primary_Type_Vector_comp1 
  public    :: Primary_Type_Vector_comp2 
  public    :: Derived_TYpe  
  public    :: time_not_found 

  enum, bind(c)
     enumerator :: time_not_found = -2
     enumerator :: ExtData_Not_found
     enumerator :: primary_type_scalar
     enumerator :: primary_type_vector_comp1
     enumerator :: primary_type_vector_comp2
     enumerator :: derived_type
  end enum

  public :: extdata_update
  public :: extdata_constant_value
  public :: extdata_regridding_method
  public :: extdata_vector_partner
  public :: extdata_vector_comp
  public :: extdata_expression
  public :: extdata_set_constant

  character(len=*), parameter :: extdata_update = "update"
  character(len=*), parameter :: extdata_constant_value = "constant_value"
  character(len=*), parameter :: extdata_regridding_method = "regridding_method"
  character(len=*), parameter :: extdata_vector_partner = "vector_partner"
  character(len=*), parameter :: extdata_vector_comp = "vector_comp"
  character(len=*), parameter :: extdata_expression = "expression"
  character(len=*), parameter :: extdata_set_constant = "set_constant"

end module MAPL_ExtDataConstants
