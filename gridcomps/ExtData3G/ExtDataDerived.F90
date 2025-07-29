#include "MAPL_Exceptions.h"
#include "MAPL_ErrLog.h"
module mapl3g_ExtDataDerived
   use ESMF
   use MAPL_KeywordEnforcerMod
   use MAPL_ExceptionHandling
   use gFTL2_StringVector
   use MAPL_StateUtils
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
      type(ESMF_HConfig), intent(in) :: config
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      type(ExtDataDerived) :: rule
      logical :: is_present
      integer :: status
      character(len=:), allocatable :: tempc
      _UNUSED_DUMMY(unusable)


      if (allocated(tempc)) deallocate(tempc)
      is_present = ESMF_HConfigIsDefined(config,keyString="function",_RC)
      _ASSERT(is_present,"no expression found in derived entry")
      if (is_present) then
         tempc = ESMF_HConfigAsString(config,keyString="function",_RC)
         rule%expression=tempc
      end if

      if (allocated(tempc)) deallocate(tempc)
      is_present = ESMF_HConfigIsDefined(config,keyString="sample",_RC)
      if (is_present) then
         tempc = ESMF_HConfigAsString(config,keyString="sample",_RC)
         rule%sample_key=tempc
      end if

      _RETURN(_SUCCESS)
   end function new_ExtDataDerived

   function get_variables_in_expression(this,rc) result(variables_in_expression)
      type(StringVector) :: variables_in_expression
      class(ExtDataDerived), intent(inout), target :: this
      integer, intent(out), optional :: rc

      integer :: status
      type(StateMask), allocatable :: temp_mask

      if (index(this%expression,"mask")/=0) then
         allocate(temp_mask)
         temp_mask = StateMask(this%expression)
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

end module mapl3g_ExtDataDerived

module mapl3g_ExtDataDerivedMap
   use mapl3g_ExtDataDerived

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

end module mapl3g_ExtDataDerivedMap
