#if defined(SUCCESS)
#undef SUCCESS
#endif
#define SUCCESS 0

#if defined(FAILURE)
#undef FAILURE
#define FAILURE SUCCESS-1

#if defined(_RC)
#undef _RC
#endif
#define _RC rc=status); if(present(rc)) rc=(status)

#if defined(_VERIFY)
#undef _VERIFY
#endif
#define _VERIFY if(status /= SUCCESS) return

module FieldUnits

   use ESMF, only: Field => ESMF_Field

   use, intrinsic :: iso_fortran_env, only: r64 => real64

   implicit none

   ! type to wrap C ut_unit
   type, bind(c) :: fut_unit
   end type fut_unit

   interface fut_unit
      module procedure :: construct_fut_unit
   end interface fut_unit

   ! Do I need to keep track of pointers?
!   procedure(FieldUnitConverter), pointer :: fldunicon(:)

abstract interface

   ! conversion procedure tied to ESMF_Field instances
   subroutine FieldUnitConverter(rc)
      integer, optional, intent(out) :: rc
   end subroutine FieldUnitConverter

   ! conversion procedure from t1 to t2
   elemental subroutine ScalarConverter(t1, t2, rc)
      real(r64), intent(in) :: t1
      real(r64), intent(out) :: t2
      integer, optional, intent(out) :: rc
   end subroutine ScalarConverter

end abstract interface

contains
    
   subroutine get_field_unit_converter(e1, e2, cf, invcf, rc)
      type(Field), intent(inout) :: e1, e2
      procedure(FieldUnitConverter), pointer, intent(out) :: cf
      procedure(FieldUnitConverter), optional, pointer, intent(out) :: invcf
      integer, optional, intent(out) :: rc
      class(fut_unit) :: fu1, fu2
      integer :: status

      call get_unit(e1, fu1, _RC)
      _VERIFY
      call get_unit(e2, fu2, _RC)
      _VERIFY 

   end subroutine get_field_unit_converter

   ! conversion procedure from e1 to e2
   ! calls ScalarConverter
   ! iterates over grid
   subroutine fc1(e1, e2, fptr, rc)
      type(Field), intent(inout) :: e1
      type(Field), intent(inout) :: e2
      procedure(ScalarConverter), pointer, intent(in) :: fc
      integer, optional, intent(out) :: rc
      
   end subroutine fc1

   ! get the fu for e using get_unit_name or get_unit_symbol
   ! calls get_unit_name or get_unit_symbol to get unit name or symbol
   ! calls get_unit_by_name or get_unit_by_symbol to get unit
   subroutine get_unit(e, fu, rc)
      type(Field), intent(inout) :: e
      type(fut_unit), intent(out) :: fu
      integer, optional, intent(out) :: rc
      character(len=MAXLEN) :: unit_name, unit_symbol

      !wdb fixme deleteme Don't need both
      call get_unit_name(e, unit_name, _RC)
      _VERIFY 
      call get_unit_symbol(e, unit_symbol, _RC)
      _VERIFY 

   end subroutine get_unit

   ! get unit_name for ESMF_Field e
   ! grabs from ESMF_Field info
   subroutine get_unit_name(e, unit_name, rc)
      type(Field, intent(in) :: e
      character(len=*), intent(out) :: unit_name
      integer, optional, intent(out) :: rc
   end subroutine get_unit_name

   ! get unit_symbol for ESMF_Field e
   ! grabs from ESMF_Field info
   subroutine get_unit_symbol(e, unit_symbol, rc)
      type(Field), intent(inout) :: e
      character(len=*), intent(out) :: unit_symbol
      integer, optional, intent(out) :: rc
   end subroutine get_unit_symbol

   ! unit corresponding to unit_name: C interface
   ! gets unit using udunits2 API
   subroutine get_unit_by_name(unit_name, fu, rc)
      character(len=*), intent(in) :: unit_name
      type(fut_unit), intent(out) :: fu
      integer, optional, intent(out) :: rc
      
      error stop 'Not implemented'

   end subroutine get_unit_by_name

   ! unit corresponding to unit_symbol: C interface
   ! gets unit using udunits2 API
   subroutine get_unit_by_symbol(unit_symbol, fu, rc)
      character(len=*), intent(in) :: unit_symbol
      type(fut_unit), intent(out) :: fu
      integer, optional, intent(out) :: rc

      error stop 'Not implemented'

   end subroutine get_unit_by_symbol

   ! check if units are compatible (for the same type of quantity: length, mass, time, etc)
   ! checks using udunits2 API
   subroutine are_compatible(fu1, fu2, compatible, rc)
      class(fut_unit), intent(in) :: fu1, fu2
      logical, intent(out) :: compatible
      integer, optional, intent(out) :: rc

      error stop 'Not implemented'

   end subroutine are_compatible

   ! get a conversion function for two units
   ! scalar function
   subroutine get_scalar_unit_converter(fu1, fu2, cf, rc)
      type(ft_unit), intent(in) :: fu1, fu2
      procedure(ScalarConverter), pointer, intent(out) :: cf
      integer, optional, intent(out) :: rc

      error stop 'Not implemented'

   end subroutine get_scalar_unit_converter

 end module FieldUnits
