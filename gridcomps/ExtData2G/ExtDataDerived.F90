#include "MAPL_Exceptions.h"
#include "MAPL_ErrLog.h"
module MAPL_ExtDataDerived
   use yaFyaml
   use MAPL_KeywordEnforcerMod
   use MAPL_ExceptionHandling
   implicit none
   private

   type, public :: ExtDataDerived
      character(:), allocatable :: expression
      character(:), allocatable :: sample_key
      contains
         procedure :: display
         procedure :: set_defaults
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


   subroutine set_defaults(this,unusable,rc)
      class(ExtDataDerived), intent(inout), target :: this
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      _UNUSED_DUMMY(unusable)
      this%expression=''
      _RETURN(_SUCCESS)
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
