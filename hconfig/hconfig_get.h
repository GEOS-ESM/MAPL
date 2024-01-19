! This include file creates a get_{type} subroutine. Here is an example of usage:

!   subroutine get_i4 & ! name must match end statement (below).
!#define TYPE_ TYPE_I4 ! This macro is type spec.
!#include "hconfig_as.h" ! This include file has a macro that uses the TYPE_ macro.
!#include "hconfig_get.h" !
!#undef TYPE_
!#undef ESMF_HCONFIG_AS
!   end subroutine get_i4

      (hconfig, value, found, message, keystring, rc)
      ! Dummy argument names are boilerplate.
      type(ESMF_HConfig), intent(inout) :: hconfig
      TYPE_, intent(inout) :: value ! TYPE SPECIFIC
      logical, intent(out) :: found
      character(len=:), allocatable, intent(inout) :: message
      character(len=*), intent(in) :: keystring
      integer, intent(out) :: rc

      character(len=:), allocatable :: typestring
      character(len=:), allocatable :: valuestring

      integer :: status
      logical :: is_defined

      found = .FALSE.
      is_defined = ESMF_HConfigIsDefined(hconfig, keystring=keystring, _RC)
      if (is_defined) then
         value = ESMF_HCONFIGAS(hconfig, keyString=keystring, _RC) ! TYPE SPECIFIC
         valuestring = make_valuestring(value)
         _ASSERT(len(valuestring) > 0, 'valuestring is empty.')
         typestring = get_typestring(value)
         _ASSERT(len(typestring) > 0, 'typestring is empty.')
         message = form_message(typestring, keystring, valuestring, valuerank=0)
         found = .TRUE.
      else
         message = ''
      end if
      
      _RETURN(_SUCCESS)
