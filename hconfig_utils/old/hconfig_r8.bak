module hconfig_r8

   use hconfig_value_base
   implicit none

   type, extends(HConfigValue) :: HConfigValueR8
     real(ESMF_KIND_R8), pointer :: value_ptr
     real(ESMF_KIND_R8), allocatable :: default_
   contains
     procedure :: set_from_hconfig => set_from_hconfig_r8
     procedure :: set_from_default => set_from_default_r8
     procedure :: value_equals_default => value_equals_default_r8
     procedure :: get_valuestring => get_valuestring_r8
   end type HConfigValueR8

   interface HConfigValueR8
     module procedure :: construct_hconfig_value_r8
   end interface HConfigValueR8

contains

   function construct_hconfig_value_r8(value, default) result(this)
      type(HConfigValueR8) :: this
      real(ESMF_KIND_R8), target :: value
      class(*), optional, intent(in) :: default
      this%value_ptr => value
      if(present(default)) then
         select type(default)
         type is(real(ESMF_KIND_R8))
            this%default_ = default
         end select
      end if
      this%typestring_ = 'R8'
   end function construct_hconfig_value_r8

   logical function value_equals_default_r8(this) result(lval)
      class(HConfigValueR8), intent(in) :: this
      lval = merge(this%value_ptr == this%default_, .FALSE., allocated(this%default_))
   end function value_equals_default_r8

   subroutine set_from_hconfig_r8(this)
      class(HConfigValueR8), intent(inout) :: this
      integer :: status
      this%value_ptr = ESMF_HConfigAsR8(this%hconfig_, keyString=this%keystring_, rc=status)
      this%last_status_ = status
   end subroutine set_from_hconfig_r8

   subroutine set_from_default_r8(this)
      class(HConfigValueR8), intent(inout) :: this
      this%value_ptr = this%default_
   end subroutine set_from_default_r8

   subroutine get_valuestring_r8(this, string)
      character(len=*), parameter :: FMT = '(G24.16)'
      class(HConfigValueR8), intent(inout) :: this
      character(len=:), allocatable, intent(out) :: string
      integer :: ios
      character(len=32) :: raw
      write(raw, fmt=FMT, iostat=ios) this%value_ptr
      this%last_status_ = ios
      if(ios == 0) string = trim(adjustl(raw))
   end subroutine get_valuestring_r8

end module hconfig_r8
