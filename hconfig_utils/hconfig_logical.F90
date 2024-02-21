module hconfig_logical

   use hconfig_value_base
   implicit none

   type, extends(HConfigValue) :: HConfigValueLogical
     logical, pointer :: value_ptr
     logical, allocatable :: default_
   contains
     procedure :: set_from_hconfig => set_from_hconfig_logical
     procedure :: set_from_default => set_from_default_logical
     procedure :: value_equals_default => value_equals_default_logical
     procedure :: get_valuestring => get_valuestring_logical
   end type HConfigValueLogical

   interface HConfigValueLogical
     module procedure :: construct_hconfig_value_logical
   end interface HConfigValueLogical

contains

   function construct_hconfig_value_logical(value, default) result(this)
      type(HConfigValueLogical) :: this
      logical, target :: value
      class(*), optional, intent(in) :: default
      this%value_ptr => value
      if(present(default)) then
         select type(default)
         type is(logical)
            this%default_ = default
         end select
      end if
      this%typestring_ = 'L'
   end function construct_hconfig_value_logical

   logical function value_equals_default_logical(this) result(lval)
      class(HConfigValueLogical), intent(in) :: this
      lval = merge(this%value_ptr .eqv. this%default_, .FALSE., allocated(this%default_))
   end function value_equals_default_logical

   subroutine set_from_hconfig_logical(this)
      class(HConfigValueLogical), intent(inout) :: this
      integer :: status
      this%value_ptr = ESMF_HConfigAsLogical(this%hconfig_, keyString=this%keystring_, rc=status)
      this%last_status_ = status
   end subroutine set_from_hconfig_logical

   subroutine set_from_default_logical(this)
      class(HConfigValueLogical), intent(inout) :: this
      this%value_ptr = this%default_
   end subroutine set_from_default_logical

   subroutine get_valuestring_logical(this, string)
      character(len=*), parameter :: FMT = '(L1)'
      class(HConfigValueLogical), intent(inout) :: this
      character(len=:), allocatable, intent(out) :: string
      integer :: ios
      character(len=32) :: raw
      write(raw, fmt=FMT, iostat=ios) this%value_ptr
      this%last_status_ = ios
      if(ios == 0) string = trim(adjustl(raw))
   end subroutine get_valuestring_logical

end module hconfig_logical
