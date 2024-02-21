module hconfig_i8

   use hconfig_value_base
   implicit none

   type, extends(HConfigValue) :: HConfigValueI8
     integer(ESMF_KIND_I8), pointer :: value_ptr
     integer(ESMF_KIND_I8), allocatable :: default_
   contains
     procedure :: set_from_hconfig => set_from_hconfig_i8
     procedure :: set_from_default => set_from_default_i8
     procedure :: value_equals_default => value_equals_default_i8
     procedure :: get_valuestring => get_valuestring_i8
   end type HConfigValueI8

   interface HConfigValueI8
     module procedure :: construct_hconfig_value_i8
   end interface HConfigValueI8

contains

   function construct_hconfig_value_i8(value, default) result(this)
      type(HConfigValueI8) :: this
      integer(ESMF_KIND_I8), target :: value
      class(*), optional, intent(in) :: default
      this%value_ptr => value
      if(present(default)) then
         select type(default)
         type is(integer(ESMF_KIND_I8))
            this%default_ = default
         end select
      end if
      this%typestring_ = 'I8'
   end function construct_hconfig_value_i8

   logical function value_equals_default_i8(this) result(lval)
      class(HConfigValueI8), intent(in) :: this
      lval = merge(this%value_ptr == this%default_, .FALSE., allocated(this%default_))
   end function value_equals_default_i8

   subroutine set_from_hconfig_i8(this)
      class(HConfigValueI8), intent(inout) :: this
      integer :: status
      this%value_ptr = ESMF_HConfigAsI8(this%hconfig_, keyString=this%keystring_, rc=status)
      this%last_status_ = status
   end subroutine set_from_hconfig_i8

   subroutine set_from_default_i8(this)
      class(HConfigValueI8), intent(inout) :: this
      this%value_ptr = this%default_
   end subroutine set_from_default_i8

   subroutine get_valuestring_i8(this, string)
      character(len=*), parameter :: FMT = '(I22)'
      class(HConfigValueI8), intent(inout) :: this
      character(len=:), allocatable, intent(out) :: string
      integer :: ios
      character(len=32) :: raw
      write(raw, fmt=FMT, iostat=ios) this%value_ptr
      this%last_status_ = ios
      if(ios == 0) string = trim(adjustl(raw))
   end subroutine get_valuestring_i8

end module hconfig_i8
