module hconfig_string

   use hconfig_value_base
   implicit none

   type, extends(HConfigValue) :: HConfigValueString
     character(len=:), pointer :: value_ptr
     character(len=:), allocatable :: default_
   contains
     procedure :: set_from_hconfig => set_from_hconfig_string
     procedure :: set_from_default => set_from_default_string
     procedure :: value_equals_default => value_equals_default_string
     procedure :: get_valuestring => get_valuestring_string
   end type HConfigValueString

   interface HConfigValueString
     module procedure :: construct_hconfig_value_string
   end interface HConfigValueString

contains

   function construct_hconfig_value_string(value, default) result(this)
      type(HConfigValueString) :: this
      character(len=*), target :: value
      class(*), optional, intent(in) :: default
      this%value_ptr => value
      if(present(default)) then
         select type(default)
         type is(character(len=*))
            this%default_ = default
         end select
      end if
      this%typestring_ = 'CH'
   end function construct_hconfig_value_string

   logical function value_equals_default_string(this) result(lval)
      class(HConfigValueString), intent(in) :: this
      lval = merge(this%value_ptr == this%default_, .FALSE., allocated(this%default_))
   end function value_equals_default_string

   subroutine set_from_hconfig_string(this)
      class(HConfigValueString), intent(inout) :: this
      integer :: status
      this%value_ptr = ESMF_HConfigAsString(this%hconfig_, keyString=this%keystring_, rc=status)
      this%last_status_ = status
   end subroutine set_from_hconfig_string

   subroutine set_from_default_string(this)
      class(HConfigValueString), intent(inout) :: this
      this%value_ptr = this%default_
   end subroutine set_from_default_string

   subroutine get_valuestring_string(this, string)
      character(len=*), parameter :: FMT = '(A)'
      class(HConfigValueString), intent(inout) :: this
      character(len=:), allocatable, intent(out) :: string
      integer :: ios
      character(len=32) :: raw
      write(raw, fmt=FMT, iostat=ios) this%value_ptr
      this%last_status_ = ios
      if(ios == 0) string = trim(adjustl(raw))
   end subroutine get_valuestring_string

end module hconfig_string
