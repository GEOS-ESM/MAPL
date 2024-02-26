module hconfig_LT_

   use hconfig_value_base
   implicit none

   type, extends(HConfigValue) :: HConfigValueUT_
     TYPE_ :: value_
     TYPE_, allocatable :: default_
   contains
     procedure :: set_from_hconfig => set_from_hconfig_LT_
     procedure :: set_from_default => set_from_default_LT_
     procedure :: value_equals_default => value_equals_default_LT_
     procedure :: get_valuestring => get_valuestring_LT_
   end type HConfigValueUT_

   interface HConfigValueUT_
     module procedure :: construct_hconfig_value_LT_
   end interface HConfigValueUT_

contains

   function construct_hconfig_value_LT_(default) result(this)
      type(HConfigValueUT_) :: this
      class(*), optional, intent(in) :: default
      if(present(default)) then
         select type(default)
         type is(TYPE_)
            this%default_ = default
         end select
      end if
      this%typestring_ = TYPESTRING_
   end function construct_hconfig_value_LT_

   logical function value_equals_default_LT_(this) result(lval)
      class(HConfigValueUT_), intent(in) :: this
      lval = merge(this%value_ == this%default_, .FALSE., allocated(this%default_))
   end function value_equals_default_LT_

   subroutine set_from_hconfig_LT_(this)
      class(HConfigValueUT_), intent(inout) :: this
      integer :: status
      this%value_ = ESMF_HConfigAsUT_(this%hconfig_, keyString=this%keystring_, rc=status)
      this%last_status_ = status
   end subroutine set_from_hconfig_LT_

   subroutine set_from_default_LT_(this)
      class(HConfigValueUT_), intent(inout) :: this
      this%value_ = this%default_
   end subroutine set_from_default_LT_

   subroutine get_valuestring_LT_(this, string)
      character(len=*), parameter :: FMT = FMT_
      class(HConfigValueUT_), intent(inout) :: this
      character(len=:), allocatable, intent(out) :: string
      integer :: ios
      character(len=32) :: raw
      write(raw, fmt=FMT, iostat=ios) this%value_
      this%last_status_ = ios
      if(ios == 0) string = trim(adjustl(raw))
   end subroutine get_valuestring_LT_

   function get_value_LT_(this) result(value)
      TYPE_ :: value
      class(HConfigValueUT_), intent(in) :: this
      value = this%value_
   end function get_value_LT_

end module hconfig_LT_
