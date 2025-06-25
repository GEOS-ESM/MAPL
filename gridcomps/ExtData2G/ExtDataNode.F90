#include "MAPL_Exceptions.h"
#include "MAPL_ErrLog.h"
module MAPL_ExtDataNode
   use ESMF
   use MAPL_KeywordEnforcerMod
   use MAPL_ExceptionHandling
   use MAPL_BaseMod, only: MAPL_UNDEF
   implicit none
   private

   type, public :: ExtDataNode
      type(ESMF_Field) :: field
      type(ESMF_Time)  :: time
      character(len=ESMF_MAXPATHLEN) :: file
      integer :: time_index
      logical :: was_set = .false.
      contains
         procedure :: check_if_initialized
         procedure :: set
         procedure :: get
         procedure :: equals
         generic :: operator(==) => equals
   end type

contains

   function check_if_initialized(this,rc) result(field_initialized)
      logical :: field_initialized
      class(ExtDataNode), intent(inout) :: this
      integer, intent(out), optional :: rc
      integer :: status
      field_initialized = ESMF_FieldIsCreated(this%field,_rc)
      _return(_success)
   end function

   subroutine set(this, unusable, field, time, file, time_index, was_set, rc)
      class(ExtDataNode), intent(inout) :: this
      class(KeywordEnforcer), optional, intent(in) :: unusable
      type(ESMF_Time), optional, intent(in) :: time
      type(ESMF_Field), optional, intent(in) :: field
      character(len=*), optional, intent(in) :: file
      integer, optional, intent(in) :: time_index
      logical, optional, intent(in) :: was_set
      integer, optional, intent(out) :: rc

      _unused_dummy(unusable)
      if (present(time)) this%time = time
      if (present(field)) this%field = field
      if (present(file)) this%file = trim(file)
      if (present(time_index)) this%time_index = time_index
      if (present(was_set)) this%was_set = was_set
      _return(_success)

   end subroutine set

   subroutine get(this, unusable, field, time, file, time_index, was_set, rc)
      class(ExtDataNode), intent(inout) :: this
      class(KeywordEnforcer), optional, intent(in) :: unusable
      type(ESMF_Time), optional, intent(out) :: time
      type(ESMF_Field), optional, intent(out) :: field
      character(len=*), optional, intent(out) :: file
      integer, optional, intent(out) :: time_index
      logical, optional, intent(out) :: was_set
      integer, optional, intent(out) :: rc

      _unused_dummy(unusable)
      if (present(time)) time = this%time
      if (present(field)) field = this%field
      if (present(file)) file = trim(this%file)
      if (present(time_index)) time_index = this%time_index
      if (present(was_set)) was_set = this%was_set
      _return(_success)

   end subroutine get

   logical function equals(a,b)
      class(ExtDataNode), intent(in) :: a
      class(ExtDataNode), intent(in) :: b
 
      equals = (trim(a%file)==trim(b%file)) .and. (a%time==b%time) .and. (a%time_index==b%time_index)
   end function equals

end module MAPL_ExtDataNode
