#include "MAPL_Exceptions.h"
#include "MAPL_ErrLog.h"
module MAPL_ExtDataDerived
   use ESMF
   use yaFyaml
   use MAPL_KeywordEnforcerMod
   use MAPL_ExceptionHandling
   use gFTL_StringVector
   use MAPL_NewArthParserMod
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
      type(Configuration), intent(in) :: config
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

      if (index(this%expression,"mask")/=0) then
         variables_in_expression = get_mask_variables(this%expression,_RC) 
      else
         variables_in_expression = parser_variables_in_expression(this%expression,_RC)
      end if
      _RETURN(_SUCCESS)

      contains

        function get_mask_variables(funcstr,rc) result(variables_in_mask)
           type(StringVector) :: variables_in_mask
           character(len=*), intent(in) :: funcstr
           integer, intent(out), optional :: rc

           integer                         :: status
           integer                         :: i1,i2,i,ivar
           logical                         :: found,twovar
           character(len=ESMF_MAXSTR)      :: tmpstring,tmpstring1,tmpstring2,functionname

           i1 = index(Funcstr,"(")
           _ASSERT(i1 > 0,'Incorrect format for function expression: missing "("')
           functionname = adjustl(Funcstr(:i1-1))
           functionname = ESMF_UtilStringLowerCase(functionname, __RC__)
           if (trim(functionname) == "regionmask") twovar = .true.
           if (trim(functionname) == "zonemask") twovar = .false.
           if (trim(functionname) == "boxmask") twovar = .false.
           tmpstring = adjustl(Funcstr(i1+1:))
           i1 = index(tmpstring,",")
           _ASSERT(i1 > 0,'Incorrect format for function expression: missing ","')
           i2 = index(tmpstring,";")
           if (twovar) then
              tmpstring1 = adjustl(tmpstring(1:i1-1))
              tmpstring2 = adjustl(tmpstring(i1+1:i2-1))
              call variables_in_mask%push_back(tmpstring1)
              call variables_in_mask%push_back(tmpstring2)
           else
              tmpstring1 = adjustl(tmpstring(1:i1-1))
              call variables_in_mask%push_back(tmpstring1)
           end if

        end function
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
