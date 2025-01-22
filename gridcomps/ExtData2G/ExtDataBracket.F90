#include "MAPL_Exceptions.h"
#include "MAPL_ErrLog.h"
module MAPL_ExtDataBracket
   use ESMF
   use MAPL_KeywordEnforcerMod
   use MAPL_ExceptionHandling
   use MAPL_BaseMod, only: MAPL_UNDEF
   use MAPL_ExtDataNode
   use MAPL_ExtDataConstants
   use MAPL_CommsMod
   use MAPL_FieldUtils
   implicit none
   private

   public :: ExtDataBracket

   type ExtDataBracket
      type(ExtDataNode) :: left_node
      type(ExtDataNode) :: right_node
      real             :: scale_factor = 0.0
      real             :: offset = 0.0
      logical          :: disable_interpolation = .false.
      logical          :: intermittent_disable = .false.
      logical          :: new_file_right = .false.
      logical          :: new_file_left = .false.
      logical          :: exact = .false.
      contains
         procedure :: interpolate_to_time
         procedure :: time_in_bracket
         procedure :: set_parameters
         procedure :: get_parameters
         procedure :: set_node
         procedure :: get_node
         procedure :: swap_node_fields
         procedure :: reset
   end type ExtDataBracket

contains

   subroutine reset(this)
      class(ExtDataBracket), intent(inout) :: this
      this%new_file_right=.false.
      this%new_file_left =.false.
   end subroutine reset
!
   function time_in_bracket(this,time) result(in_bracket)
      class(ExtDataBracket), intent(in) :: this
      logical :: in_bracket
      type(ESMF_Time), intent(in) :: time

      in_bracket = (this%left_node%time <=time) .and. (time < this%right_node%time)

   end function time_in_bracket

   subroutine set_node(this, bracketside, unusable, field, file, time, time_index, was_set, rc)
      class(ExtDataBracket), intent(inout) :: this
      character(len=*), intent(in) :: bracketside
      class(KeywordEnforcer), optional, intent(in) :: unusable
      type(ESMF_Field), optional, intent(in) :: field
      character(len=*), optional, intent(in) :: file
      integer, optional, intent(in) :: time_index
      type(ESMF_Time), optional, intent(in) :: time
      logical, optional, intent(in) :: was_set
      integer, optional, intent(out) :: rc

      _UNUSED_DUMMY(unusable)
      if (bracketside=='L') then
         if (present(field)) this%left_node%field=field
         if (present(time)) this%left_node%time=time
         if (present(time_index)) this%left_node%time_index=time_index
         if (present(file)) this%left_node%file=file
         if (present(was_set)) this%left_node%was_set=was_set
      else if (bracketside=='R') then
         if (present(field)) this%right_node%field=field
         if (present(time)) this%right_node%time=time
         if (present(time_index)) this%right_node%time_index=time_index
         if (present(file)) this%right_node%file=file
         if (present(was_set)) this%right_node%was_set=was_set
      else
         _FAIL('wrong bracket side')
      end if
      _RETURN(_SUCCESS)

   end subroutine set_node

   subroutine get_node(this, bracketside, unusable, field, file, time, time_index, was_set, rc)
      class(ExtDataBracket), intent(inout) :: this
      character(len=*), intent(in) :: bracketside
      class(KeywordEnforcer), optional, intent(in) :: unusable
      type(ESMF_Field), optional, intent(out) :: field
      character(len=*), optional, intent(out) :: file
      integer, optional, intent(out) :: time_index
      type(ESMF_Time), optional, intent(out) :: time
      logical, optional, intent(out) :: was_set
      integer, optional, intent(out) :: rc

      _UNUSED_DUMMY(unusable)
      if (bracketside=='L') then
         if (present(field)) field=this%left_node%field
         if (present(time)) time=this%left_node%time
         if (present(time_index)) time_index=this%left_node%time_index
         if (present(file)) file=this%left_node%file
         if (present(was_set)) was_set=this%left_node%was_set
      else if (bracketside=='R') then
         if (present(field)) field=this%right_node%field
         if (present(time)) time=this%right_node%time
         if (present(time_index)) time_index=this%right_node%time_index
         if (present(file)) file=this%right_node%file
         if (present(was_set)) was_set=this%right_node%was_set
      else
         _FAIL('wrong bracket side')
      end if
      _RETURN(_SUCCESS)

   end subroutine get_node


   subroutine set_parameters(this, unusable, linear_trans, disable_interpolation, left_field, right_field, intermittent_disable, exact, rc)
      class(ExtDataBracket), intent(inout) :: this
      class(KeywordEnforcer), optional, intent(in) :: unusable
      real, optional, intent(in) :: linear_trans(2)
      logical, optional, intent(in) :: disable_interpolation
      type(ESMF_Field), optional, intent(in) :: left_field
      type(ESMF_Field), optional, intent(in) :: right_field
      logical, optional, intent(in) :: intermittent_disable
      logical, optional, intent(in) :: exact
      integer, optional, intent(out) :: rc

      _UNUSED_DUMMY(unusable)
      if (present(linear_trans)) then
         this%offset=linear_trans(1)
         this%scale_factor=linear_trans(2)
      end if
      if (present(disable_interpolation)) this%disable_interpolation = disable_interpolation
      if (present(left_field)) this%left_node%field=left_field
      if (present(right_field)) this%right_node%field=right_field
      if (present(intermittent_disable)) this%intermittent_disable = intermittent_disable
      if (present(exact)) this%exact = exact
      _RETURN(_SUCCESS)

   end subroutine set_parameters

   subroutine get_parameters(this, bracket_side, unusable, field, file, time, time_index, update, rc)
      class(ExtDataBracket), intent(inout) :: this
      character(len=*), intent(in) :: bracket_side
      class(KeywordEnforcer), optional, intent(in) :: unusable
      type(ESMF_Field), optional, intent(out) :: field
      character(len=*), optional, intent(out) :: file
      type(ESMF_Time),  optional, intent(out) :: time
      integer,          optional, intent(out) :: time_index
      logical,          optional, intent(out) :: update
      integer, optional, intent(out) :: rc

      _UNUSED_DUMMY(unusable)
      if (bracket_side == 'L') then
          if (present(field))  field = this%left_node%field
          if (present(file)) file = trim(this%left_node%file)
          if (present(time)) time = this%left_node%time
          if (present(time_index)) time_index = this%left_node%time_index
          if (present(update)) update = this%new_file_left
      else if (bracket_side == 'R') then
          if (present(field))  field = this%right_node%field
          if (present(file)) file = trim(this%right_node%file)
          if (present(time)) time = this%right_node%time
          if (present(time_index)) time_index = this%right_node%time_index
          if (present(update)) update = this%new_file_right
      else
         _FAIL('invalid bracket side!')
      end if
      _RETURN(_SUCCESS)

   end subroutine get_parameters

   subroutine interpolate_to_time(this,field,time,rc)
      class(ExtDataBracket), intent(inout) :: this
      type(ESMF_Field), intent(inout) :: field
      type(ESMF_Time), intent(in) :: time
      integer, optional, intent(out) :: rc

      type(ESMF_TimeInterval)    :: tinv1, tinv2
      real                       :: alpha
      real, pointer              :: var1d(:)     => null()
      real, pointer              :: var1d_left(:)   => null()
      real, pointer              :: var1d_right(:)   => null()
      integer :: status
      logical :: right_node_set, left_node_set
      character(len=ESMF_MAXPATHLEN) :: left_file, right_file

      right_node_set = this%right_node%check_if_initialized(_RC)
      left_node_set = this%left_node%check_if_initialized(_RC)
      call ESMF_TimePrint(this%left_node%time,options='string',preString='left bracket time: ')
      call ESMF_TimePrint(this%right_node%time,options='string',preString='right bracket time: ')

      alpha = 0.0
      if ( (.not.this%disable_interpolation) .and. (.not.this%intermittent_disable) .and. right_node_set .and. left_node_set) then
         tinv1 = time - this%left_node%time
         tinv2 = this%right_node%time - this%left_node%time
         alpha = tinv1/tinv2
      end if
      call assign_fptr(field,var1d,_RC)
      if (right_node_set) then
         call assign_fptr(this%right_node%field,var1d_right,_RC)
      end if
      if (left_node_set) then
         call assign_fptr(this%left_node%field,var1d_left,_RC)
      end if
      if ( left_node_set .and. (time == this%left_node%time .or. this%disable_interpolation)) then
         var1d = var1d_left
      else if (right_node_set .and. (time == this%right_node%time)) then
         var1d = var1d_right
      else if ( (left_node_set .and. right_node_set) .and. (.not.this%exact) ) then
         where( (var1d_left /= mapl_undef) .and. (var1d_right /= mapl_undef))
            var1d = var1d_left + alpha*(var1d_right-var1d_left)
         elsewhere
            var1d = mapl_undef
         endwhere
      end if
 
      if (this%exact .and. (.not.(time == this%left_node%time))) then
         var1d = mapl_undef
      end if

      if (this%scale_factor == 0.0 .and. this%offset /= 0.0) then
         where(var1d /= MAPL_UNDEF) var1d=var1d+this%offset
      end if
      if (this%scale_factor /= 0.0 .and. this%offset == 0.0) then
         where(var1d /= MAPL_UNDEF) var1d=var1d*this%scale_factor
      end if
      if (this%scale_factor /= 0.0 .and. this%offset /= 0.0) then
         where(var1d /= MAPL_UNDEF) var1d=var1d*this%scale_factor+this%offset
      end if
      
      _RETURN(_SUCCESS)

   end subroutine interpolate_to_time

   subroutine swap_node_fields(this,rc)
      class(ExtDataBracket), intent(inout) :: this
      integer, optional, intent(out) :: rc
      integer :: status
      real, pointer :: left_ptr(:), right_ptr(:)
      logical :: left_created, right_created

      left_created  = ESMF_FieldIsCreated(this%left_node%field,_RC)
      right_created = ESMF_FieldIsCreated(this%right_node%field,_RC)
      left_created  = ESMF_FieldIsCreated(this%left_node%field,_RC)
      if (left_created .and. right_created) then     
         call assign_fptr(this%left_node%field,left_ptr,_RC) 
         call assign_fptr(this%right_node%field,right_ptr,_RC) 
         left_ptr = right_ptr
      end if
      _RETURN(_SUCCESS)
   end subroutine swap_node_fields

end module MAPL_ExtDataBracket
