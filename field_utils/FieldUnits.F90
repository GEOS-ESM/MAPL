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

   use udunits2mod
!   use ESMF, only: Field => ESMF_Field
   use MockField_mod, only: Field => MockField

   use, intrinsic :: iso_fortran_env, only: R64 => real64, R32 => real32

   implicit none

   ! Do I need to keep track of pointers?
!   procedure(FieldUnitConverter), pointer :: fldunicon(:)
   integer, parameter :: ESMF_KIND_R8 = R64, ESMF_KIND_R4 = R32

   abstract interface

      ! conversion procedure from t1 to t2
      elemental subroutine ScalarConverter(t1, t2, rc)
         real(ESMF_KIND_R8), intent(in) :: t1
         real(ESMF_KIND_R8), intent(out) :: t2
         integer, optional, intent(out) :: rc
      end subroutine ScalarConverter

      ! conversion procedure from e1 to e2
      subroutine FieldConverter(e1, e1, rc)
         type(Field), intent(inout) :: e1
         type(Field), intent(inout) :: e2
         integer, optional, intent(out) :: rc
      end subroutine FieldConverter

   end abstract interface

contains
    
   subroutine get_field_unit_converter(e1, e2, cf, invcf, rc)
      type(Field), intent(inout) :: e1, e2
      procedure(FieldConverter), pointer, intent(out) :: cf
      procedure(FieldConverter), optional, pointer, intent(out) :: invcf
      integer, optional, intent(out) :: rc
      class(ut_unit) :: unit1, unit2
      integer :: status

      call get_unit(e1, unit1, _RC)
      _VERIFY
      call get_unit(e2, unit2, _RC)
      _VERIFY 

      call are_compatible(unit1, unit2, compatible, _RC)
      _VERIFY

      if(.not. compatible) then
         status = FAILURE
         if(present(rc)) rc = status
         return
      end if

      call get_scalar_unit_converter(unit1, unit1, cf, _RC)
      _VERIFY

      if(present(invcf)) then
         call get_scalar_unit_converter(unit1, unit2, invcf, _RC)
         _VERIFY
      end if

   end subroutine get_field_unit_converter

   ! get the unit e using get_unit_name or get_unit_symbol
   ! calls get_unit_name or get_unit_symbol to get unit name or symbol
   ! calls get_unit_by_name or get_unit_by_symbol to get unit
   subroutine get_unit(e, unit_, rc)
      type(Field), intent(inout) :: e
      type(ut_unit), intent(out) :: unit_
      integer, optional, intent(out) :: rc
      character(len=MAXLEN) :: unit_name, unit_symbol

      !wdb fixme deleteme Don't need both
      call get_unit_name(e, unit_name, _RC)
      _VERIFY 
      call get_unit_symbol(e, unit_symbol, _RC)
      _VERIFY 

   end subroutine get_unit

   ! get unit_name for Field e
   ! grabs from Field info
   subroutine get_unit_name(e, unit_name, rc)
      type(Field), intent(inout) :: e
      character(len=*), intent(out) :: unit_name
      integer, optional, intent(out) :: rc
   end subroutine get_unit_name

   ! get unit_symbol for Field e
   ! grabs from Field info
   subroutine get_unit_symbol(e, unit_symbol, rc)
      type(Field), intent(inout) :: e
      character(len=*), intent(out) :: unit_symbol
      integer, optional, intent(out) :: rc
   end subroutine get_unit_symbol

   ! unit corresponding to unit_name: C interface
   ! gets unit using udunits2 API
   subroutine get_unit_by_name(unit_name, unit_, rc)
      character(len=*), intent(in) :: unit_name
      class(ut_unit), intent(out) :: unit_
      integer, optional, intent(out) :: rc
      
      error stop 'Not implemented'

   end subroutine get_unit_by_name

   ! unit corresponding to unit_symbol: C interface
   ! gets unit using udunits2 API
   subroutine get_unit_by_symbol(unit_symbol, unit_, rc)
      character(len=*), intent(in) :: unit_symbol
      class(ut_unit), intent(out) :: unit_
      integer, optional, intent(out) :: rc

      error stop 'Not implemented'

   end subroutine get_unit_by_symbol

   ! check if units are compatible (for the same type of quantity: length, mass, time, etc)
   ! checks using udunits2 API
   subroutine are_compatible(unit1, unit2, compatible, rc)
      class(ut_unit), intent(in) :: unit1, unit2
      logical, intent(out) :: compatible
      integer, optional, intent(out) :: rc

      error stop 'Not implemented'

   end subroutine are_compatible

   ! get a conversion function for two units
   ! scalar function
   subroutine get_scalar_unit_converter(unit1, unit2, cf, rc)
      class(ut_unit), intent(in) :: unit1, unit2
      procedure(ScalarConverter), pointer, intent(out) :: cf
      integer, optional, intent(out) :: rc

      error stop 'Not implemented'

   end subroutine get_scalar_unit_converter

 end module FieldUnits
