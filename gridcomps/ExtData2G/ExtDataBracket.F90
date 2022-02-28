#include "MAPL_Exceptions.h"
#include "MAPL_ErrLog.h"
module MAPL_ExtDataBracket
   use ESMF
   use MAPL_KeywordEnforcerMod
   use MAPL_ExceptionHandling
   use MAPL_BaseMod, only: MAPL_UNDEF
   use MAPL_ExtDataNode
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
      logical          :: new_file_right
      logical          :: new_file_left
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

   logical function time_in_bracket(this,time)
      class(ExtDataBracket), intent(in) :: this
      type(ESMF_Time), intent(in) :: time

      time_in_bracket = (this%left_node%time <=time) .and. (time < this%right_node%time)

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
         _ASSERT(.false.,'wrong bracket side')
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
         _ASSERT(.false.,'wrong bracket side')
      end if
      _RETURN(_SUCCESS)

   end subroutine get_node


   subroutine set_parameters(this, unusable, linear_trans, disable_interpolation, left_field, right_field, intermittent_disable, rc)
      class(ExtDataBracket), intent(inout) :: this
      class(KeywordEnforcer), optional, intent(in) :: unusable
      real, optional, intent(in) :: linear_trans(2)
      logical, optional, intent(in) :: disable_interpolation
      type(ESMF_Field), optional, intent(in) :: left_field
      type(ESMF_Field), optional, intent(in) :: right_field
      logical, optional, intent(in) :: intermittent_disable
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
         _ASSERT(.false.,'invalid bracket side!')
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
      real, pointer              :: var2d(:,:)   => null()
      real, pointer              :: var3d(:,:,:) => null()
      real, pointer              :: var2d_left(:,:)   => null()
      real, pointer              :: var2d_right(:,:)   => null()
      real, pointer              :: var3d_left(:,:,:) => null()
      real, pointer              :: var3d_right(:,:,:) => null()
      integer                    :: field_rank
      integer :: status

      call ESMF_FieldGet(field,dimCount=field_rank,__RC__)
      alpha = 0.0
      if ( (.not.this%disable_interpolation) .and. (.not.this%intermittent_disable)) then
         tinv1 = time - this%left_node%time
         tinv2 = this%right_node%time - this%left_node%time
         alpha = tinv1/tinv2
      end if
      if (field_rank==2) then
         call ESMF_FieldGet(field,localDE=0,farrayPtr=var2d,__RC__)
         call ESMF_FieldGet(this%right_node%field,localDE=0,farrayPtr=var2d_right,__RC__)
         call ESMF_FieldGet(this%left_node%field,localDE=0,farrayPtr=var2d_left,__RC__)
         if (time == this%left_node%time .or. this%disable_interpolation) then
            var2d = var2d_left
         else if (time == this%right_node%time) then
            var2d = var2d_right
         else
            where( (var2d_left /= MAPL_UNDEF) .and. (var2d_right /= MAPL_UNDEF))
               var2d = var2d_left + alpha*(var2d_right-var2d_left)
            elsewhere
               var2d = MAPL_UNDEF
            endwhere
         end if

         if (this%scale_factor == 0.0 .and. this%offset /= 0.0) then
            where(var2d /= MAPL_UNDEF) var2d=var2d+this%offset
         end if
         if (this%scale_factor /= 0.0 .and. this%offset == 0.0) then
            where(var2d /= MAPL_UNDEF) var2d=var2d*this%scale_factor
         end if
         if (this%scale_factor /= 0.0 .and. this%offset /= 0.0) then
            where(var2d /= MAPL_UNDEF) var2d=var2d*this%scale_factor+this%offset
         end if

      else if (field_rank==3) then
         call ESMF_FieldGet(field,localDE=0,farrayPtr=var3d,__RC__)
         call ESMF_FieldGet(this%right_node%field,localDE=0,farrayPtr=var3d_right,__RC__)
         call ESMF_FieldGet(this%left_node%field,localDE=0,farrayPtr=var3d_left,__RC__)
         if (time == this%left_node%time .or. this%disable_interpolation) then
            var3d = var3d_left
         else if (time == this%right_node%time) then
            var3d = var3d_right
         else
            where( (var3d_left /= MAPL_UNDEF) .and. (var3d_right /= MAPL_UNDEF))
               var3d = var3d_left + alpha*(var3d_right-var3d_left)
            elsewhere
               var3d = MAPL_UNDEF
            endwhere
         end if

         if (this%scale_factor == 0.0 .and. this%offset /= 0.0) then
            where(var3d /= MAPL_UNDEF) var3d=var3d+this%offset
         end if
         if (this%scale_factor /= 0.0 .and. this%offset == 0.0) then
            where(var3d /= MAPL_UNDEF) var3d=var3d*this%scale_factor
         end if
         if (this%scale_factor /= 0.0 .and. this%offset /= 0.0) then
            where(var3d /= MAPL_UNDEF) var3d=var3d*this%scale_factor+this%offset
         end if

      end if
      _RETURN(_SUCCESS)

   end subroutine interpolate_to_time

   subroutine swap_node_fields(this,rc)
      class(ExtDataBracket), intent(inout) :: this
      integer, optional, intent(out) :: rc
      integer :: status
      integer :: field_rank
      real, pointer :: var3d_left(:,:,:),var3d_right(:,:,:)
      real, pointer :: var2d_left(:,:),var2d_right(:,:)

      call ESMF_FieldGet(this%left_node%field,dimCount=field_rank,__RC__)
      if (field_rank == 2) then
         call ESMF_FieldGet(this%right_node%field,localDE=0,farrayPtr=var2d_right,__RC__)
         call ESMF_FieldGet(this%left_node%field,localDE=0,farrayPtr=var2d_left,__RC__)
         var2d_left = var2d_right
      else if (field_rank ==3) then
         call ESMF_FieldGet(this%right_node%field,localDE=0,farrayPtr=var3d_right,__RC__)
         call ESMF_FieldGet(this%left_node%field,localDE=0,farrayPtr=var3d_left,__RC__)
         var3d_left = var3d_right
      end if
      _RETURN(_SUCCESS)
   end subroutine swap_node_fields

end module MAPL_ExtDataBracket
