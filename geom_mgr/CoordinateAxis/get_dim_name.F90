#include "MAPL_ErrLog.h"

submodule (mapl3g_CoordinateAxis) get_dim_name_smod
   use esmf, only: ESMF_UtilStringLowerCase
   use mapl_ErrorHandling
   use gftl2_StringVector
   use, intrinsic :: iso_fortran_env, only: REAL32, REAL64

contains
   
   module function get_dim_name(file_metadata, units, rc) result(dim_name)
      character(:), allocatable :: dim_name
      type(FileMetadata), target, intent(in) :: file_metadata
      character(*), intent(in) :: units
      integer, optional, intent(out) :: rc

      integer :: status
      type(StringVariableMap), pointer :: vars
      type(Variable), pointer :: var
      type(StringVariableMapIterator) :: iter
      type(StringVector), pointer :: dims
      character(:), allocatable :: units_lower_case
      character(:), allocatable :: units_found
      logical :: has_units
      type(Attribute), pointer :: attr
      logical :: found
      integer :: counter

      dim_name = ''
      units_lower_case = ESMF_UtilStringLowerCase(units, _RC)
      found = .false.
      counter = 0

      vars => file_metadata%get_variables(_RC)
      associate ( e => vars%ftn_end() )
        iter = vars%ftn_begin()
        do while (iter /= e)
           call iter%next()

           var => iter%second()
           has_units = var%is_attribute_present('units', _RC)
           if (.not. has_units) cycle

           attr => var%get_attribute('units', _RC)
           units_found = attr%get_string(_RC)
           units_found = ESMF_UtilStringLowerCase(units_found, _RC)
           if (units_found /= units_lower_case) cycle
           
           dims => var%get_dimensions()
           if (dims%size() /= 1) cycle

           found = .true.
           counter = counter + 1
           _ASSERT(counter == 1, 'Too many variables match requested units: ' // units)
           dim_name = dims%of(1)
           
        end do
      end associate
      _ASSERT(found, "No variable found with units: " // units//".")

      _RETURN(_SUCCESS)
   end function get_dim_name

end submodule get_dim_name_smod
