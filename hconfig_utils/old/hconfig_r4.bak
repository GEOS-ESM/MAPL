module hconfig_r4

   use hconfig_value_base
   implicit none

   type, extends(HConfigValue) :: HConfigValueR4
     real(ESMF_KIND_R4), pointer :: value_ptr
     real(ESMF_KIND_R4), allocatable :: default_
   contains
     procedure :: set_from_hconfig => set_from_hconfig_r4
     procedure :: set_from_default => set_from_default_r4
     procedure :: value_equals_default => value_equals_default_r4
     procedure :: get_valuestring => get_valuestring_r4
   end type HConfigValueR4

   interface HConfigValueR4
     module procedure :: construct_hconfig_value_r4
   end interface HConfigValueR4

contains

   function construct_hconfig_value_r4(value, default) result(this)
      type(HConfigValueR4) :: this
      real(ESMF_KIND_R4), target :: value
      class(*), optional, intent(in) :: default
      this%value_ptr => value
      if(present(default)) then
         select type(default)
         type is(real(ESMF_KIND_R4))
            this%default_ = default
         end select
      end if
      this%typestring_ = 'R4'
   end function construct_hconfig_value_r4

   logical function value_equals_default_r4(this) result(lval)
      class(HConfigValueR4), intent(in) :: this
      lval = merge(this%value_ptr == this%default_, .FALSE., allocated(this%default_))
   end function value_equals_default_r4

   subroutine set_from_hconfig_r4(this)
      class(HConfigValueR4), intent(inout) :: this
      integer :: status
      this%value_ptr = ESMF_HConfigAsR4(this%hconfig_, keyString=this%keystring_, rc=status)
      this%last_status_ = status
   end subroutine set_from_hconfig_r4

   subroutine set_from_default_r4(this)
      class(HConfigValueR4), intent(inout) :: this
      this%value_ptr = this%default_
   end subroutine set_from_default_r4

   subroutine get_valuestring_r4(this, string)
      character(len=*), parameter :: FMT = '(G17.8)'
      class(HConfigValueR4), intent(inout) :: this
      character(len=:), allocatable, intent(out) :: string
      integer :: ios
      character(len=32) :: raw
      write(raw, fmt=FMT, iostat=ios) this%value_ptr
      this%last_status_ = ios
      if(ios == 0) string = trim(adjustl(raw))
   end subroutine get_valuestring_r4

end module hconfig_r4
