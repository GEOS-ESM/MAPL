#include "MAPL_Exceptions.h"
#include "MAPL_ErrLog.h"
module MAPL_ExtDataDerived
   use ESMF
   use yaFyaml
   use MAPL_KeywordEnforcerMod
   use MAPL_ExceptionHandling
   use gFTL_StringVector
   use MAPL_NewArthParserMod
   use MAPL_ExtDataMask
   implicit none
   private

   type, public :: ExtDataDerived
      character(:), allocatable :: expression
      character(:), allocatable :: sample_key
      contains
         procedure :: display
         procedure :: set_defaults
         procedure :: get_variables_in_expression
   end type

   interface ExtDataDerived
      module procedure new_ExtDataDerived
   end interface

contains

   function new_ExtDataDerived(config,unusable,rc) result(rule)
      class(YAML_Node), intent(in) :: config
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      type(ExtDataDerived) :: rule
      logical :: is_present
      integer :: status
      character(len=:), allocatable :: tempc
      _UNUSED_DUMMY(unusable)


      if (allocated(tempc)) deallocate(tempc)
      is_present = config%has("function")
      _ASSERT(is_present,"no expression found in derived entry") 
      call config%get(tempc,"function",rc=status)
      _VERIFY(status)
      rule%expression=tempc

      if (allocated(tempc)) deallocate(tempc)
      is_present = config%has("sample")
      if (is_present) then
         call config%get(tempc,"sample",rc=status)
         _VERIFY(status)
         rule%sample_key=tempc
      end if

      _RETURN(_SUCCESS)
   end function new_ExtDataDerived

   function get_variables_in_expression(this,rc) result(variables_in_expression)
      type(StringVector) :: variables_in_expression
      class(ExtDataDerived), intent(inout), target :: this
      integer, intent(out), optional :: rc

      integer :: status
      type(ExtDataMask), allocatable :: temp_mask

      if (index(this%expression,"mask")/=0) then
         allocate(temp_mask)
         temp_mask = ExtDataMask(this%expression)
         variables_in_expression = temp_mask%get_mask_variables(_RC) 
      else
         variables_in_expression = parser_variables_in_expression(this%expression,_RC)
      end if
      _RETURN(_SUCCESS)

   end function
      

   subroutine set_defaults(this,unusable,rc)
      class(ExtDataDerived), intent(inout), target :: this
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      this%expression=''
      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine set_defaults

   subroutine display(this)
      class(ExtDataDerived) :: this
      write(*,*)"function: ",trim(this%expression)
   end subroutine display
 
end module MAPL_ExtDataDerived

module MAPL_ExtDataDerivedMap
   use MAPL_ExtDataDerived

#include "types/key_deferredLengthString.inc"
#define _value type(ExtDataDerived)
#define _alt

#define _map ExtDataDerivedMap
#define _iterator ExtDataDerivedMapIterator

#include "templates/map.inc"

#undef _iterator
#undef _map

#undef _alt
#undef _value

end module MAPL_ExtDataDerivedMap
