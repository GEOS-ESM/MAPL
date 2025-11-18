#include "MAPL_ErrLog.h"

submodule (mapl3g_ComponentSpecParser) to_itemtype_smod
   implicit none(type,external)

contains
   
   module function to_itemtype(attributes, rc) result(itemtype)
      type(ESMF_StateItem_Flag) :: itemtype
      type(ESMF_HConfig), target, intent(in) :: attributes
      integer, optional, intent(out) :: rc

      integer :: status
      character(:), allocatable :: subclass
      logical :: has_subclass
      logical :: has_expression

      itemtype = MAPL_STATEITEM_FIELD ! default
      has_expression = ESMF_HConfigIsDefined(attributes,keyString='expression',_RC)
      if (has_expression) then
         itemtype = MAPL_STATEITEM_EXPRESSION
      end if
      
      has_subclass = ESMF_HConfigIsDefined(attributes,keyString='class',_RC)
      _RETURN_UNLESS(has_subclass)

      subclass = ESMF_HConfigAsString(attributes, keyString='class',_RC)
      subclass = ESMF_UtilStringLowerCase(subclass)

      if (has_expression) then
         _ASSERT(subclass == 'expression', 'Subclass ' // subclass // ' does not support expressions.')
      end if

      select case (subclass)
      case ('field')
         itemtype = MAPL_STATEITEM_FIELD
      case ('expression') 
         itemtype = MAPL_STATEITEM_EXPRESSION
      case ('vector')
         itemtype = MAPL_STATEITEM_VECTOR
      case ('service')
         itemtype = MAPL_STATEITEM_SERVICE
      case ('wildcard')
         itemtype = MAPL_STATEITEM_WILDCARD
      case ('bracket')
         itemtype = MAPL_STATEITEM_BRACKET
      case ('vector_bracket')
         itemtype = MAPL_STATEITEM_VECTOR_BRACKET
      case default
         _FAIL('unknown subclass for state item: '//subclass)
      end select

      _RETURN(_SUCCESS)
   end function to_itemtype

end submodule to_itemtype_smod
