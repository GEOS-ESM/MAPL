#include "MAPL.h"

submodule (mapl_ComponentSpecParser_mod) to_itemtype_smod
   implicit none(type,external)

contains
   
   module function to_itemtype(attributes, rc) result(itemtype)
      type(ESMF_StateItem_Flag) :: itemtype
      type(ESMF_HConfig), target, intent(in) :: attributes
      integer, optional, intent(out) :: rc

      integer :: status
      character(:), allocatable :: subclass
      character(:), allocatable :: msg
      logical :: has_subclass
      logical :: has_expression
      logical :: has_members

      itemtype = MAPL_STATEITEM_FIELD ! default
      has_expression = ESMF_HConfigIsDefined(attributes,keyString='expression',_RC)
      if (has_expression) then
         itemtype = MAPL_STATEITEM_EXPRESSION
      end if

      ! `members:` (a Handler callback-State EXPORT's declared members, see
      ! parse_var_specs.F90) is the only way a plain top-level YAML item
      ! becomes a STATE itemType today; there is no `class: state`.
      has_members = ESMF_HConfigIsDefined(attributes,keyString='members',_RC)
      msg = '"members:" cannot be combined with "expression:" on the same item'
      _ASSERT(.not. (has_members .and. has_expression), msg)
      if (has_members) then
         itemtype = MAPL_STATEITEM_STATE
      end if

      has_subclass = ESMF_HConfigIsDefined(attributes,keyString='class',_RC)
      msg = '"members:" cannot be combined with "class:" on the same item'
      _ASSERT(.not. (has_members .and. has_subclass), msg)
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
      case default
         _FAIL('unknown subclass for state item: '//subclass)
      end select

      _RETURN(_SUCCESS)
   end function to_itemtype

end submodule to_itemtype_smod
