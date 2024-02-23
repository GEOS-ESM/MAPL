module hconfig_i4

   use hconfig_value_base
   implicit none

   type, extends(HConfigValue) :: HConfigValueI4
     integer(ESMF_KIND_I4), pointer :: value_ptr
     integer(ESMF_KIND_I4), allocatable :: default_
   contains
     procedure :: set_from_hconfig => set_from_hconfig_i4
     procedure :: set_from_default => set_from_default_i4
     procedure :: value_equals_default => value_equals_default_i4
     procedure :: get_valuestring => get_valuestring_i4
   end type HConfigValueI4

   interface HConfigValueI4
     module procedure :: construct_hconfig_value_i4
   end interface HConfigValueI4

contains

   function construct_hconfig_value_i4(value, default) result(this)
      type(HConfigValueI4) :: this
      integer(ESMF_KIND_I4), target :: value
      class(*), optional, intent(in) :: default
      this%value_ptr => value
      this%has_default_ = present(default)
      if(this%has_default_) then
         select type(default)
         type is(integer(ESMF_KIND_I4))
            this%default_ = default
         end select
      end if
      this%typestring_ = 'I4'
   end function construct_hconfig_value_i4

   logical function value_equals_default_i4(this) result(lval)
      class(HConfigValueI4), intent(in) :: this
      lval = this%has_default_
      if(lval) lval = (this%value_ptr == this%default_)
   end function value_equals_default_i4

   subroutine set_from_hconfig_i4(this)
      class(HConfigValueI4), intent(inout) :: this
      integer :: status
      this%value_ptr = ESMF_HConfigAsI4(this%hconfig_, keyString=this%keystring_, rc=status)
      this%last_status_ = status
   end subroutine set_from_hconfig_i4

   subroutine set_from_default_i4(this)
      class(HConfigValueI4), intent(inout) :: this
      this%value_ptr = this%default_
   end subroutine set_from_default_i4

   subroutine get_valuestring_i4(this, string)
      character(len=*), parameter :: FMT = '(I12)'
      class(HConfigValueI4), intent(inout) :: this
      character(len=:), allocatable, intent(out) :: string
      integer :: ios
      character(len=32) :: raw
      write(raw, fmt=FMT, iostat=ios) this%value_ptr
      this%last_status_ = ios
      if(ios == 0) string = trim(adjustl(raw))
   end subroutine get_valuestring_i4

end module hconfig_i4
