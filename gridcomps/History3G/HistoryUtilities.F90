#include "MAPL.h"
module mapl3g_HistoryUtilities
   use mapl3
   use esmf
   use mapl3g_HistoryConstants
   !use gFTL2_StringVector
   !use gFTL2_StringSet

   implicit none(type,external)
   private

   public :: replace_delimiter
   public :: parse_item

contains

   function replace_delimiter(string, delimiter, replacement) result(replaced)
      character(len=:), allocatable :: replaced
      character(len=*), intent(in) :: string
      character(len=*), optional, intent(in) :: delimiter
      character(len=*), optional, intent(in) :: replacement
      character(len=:), allocatable :: del, rep
      integer :: i

      replaced = string
      if(len(string) == 0) return

      del = '.'
      if(present(delimiter)) del = delimiter
      if(len(del) == 0) return

      rep = '/'
      if(present(replacement)) rep = replacement
      if(len(rep) == 0) return

      i = index(replaced, del)
      if(i > 0) replaced = replaced(:(i-1))// rep // replaced((i+len(del)):)

   end function replace_delimiter

   subroutine parse_item(item, short_name, alias, name_in_comp, rc)
      type(ESMF_HConfigIter), intent(in) :: item
      character(len=:), allocatable, optional, intent(out) :: short_name
      character(len=:), allocatable, optional, intent(out) :: alias
      character(len=:), allocatable, optional, intent(out) :: name_in_comp
      integer, optional, intent(out) :: rc
      integer :: status, slash_loc
      logical :: asOK, isScalar, isMap
      character(len=:), allocatable :: temp_string
      type(ESMF_HConfig) :: value

      isScalar = ESMF_HConfigIsScalarMapKey(item, _RC)
      _ASSERT(isScalar, 'Variable list item does not have a scalar name.')
      isMap = ESMF_HConfigIsMapMapVal(item, _RC)
      _ASSERT(isMap, 'Variable list item does not have a map value.')

      if (present(alias)) then
         alias = ESMF_HConfigAsStringMapKey(item, asOkay=asOK, _RC)
         _ASSERT(asOK, 'Item name could not be processed as a String.')
      end if
      if (present(short_name)) then
         value = ESMF_HConfigCreateAtMapVal(item, _RC)
         short_name = ESMF_HConfigAsString(value, keyString=KEY_EXPRESSION, _RC)
         short_name = replace_delimiter(short_name)
      end if
      if (present(name_in_comp)) then
         value = ESMF_HConfigCreateAtMapVal(item, _RC)
         temp_string = ESMF_HConfigAsString(value, keyString=KEY_EXPRESSION, _RC)
         temp_string = replace_delimiter(short_name)
         slash_loc = index(temp_string, '/')
         name_in_comp = temp_string
         if (slash_loc .gt. 0) then
            name_in_comp = temp_string(slash_loc+1:)
         end if
      end if

      _RETURN(_SUCCESS)
   end subroutine parse_item

end module mapl3g_HistoryUtilities
