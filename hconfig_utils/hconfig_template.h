
   use hconfig_value_base
   implicit none

   type, extends(HConfigValue) :: DTYPE
     VTYPE, pointer :: value_ptr
     VTYPE, allocatable :: default_
   contains
     module procedure :: set_from_hconfig => set_from_hconfig_UCTYPE
     module procedure :: set_from_default => set_from_default_UCTYPE
     module procedure :: value_equals_default => value_equals_default_UCTYPE
     module procedure :: get_valuestring => get_valuestring_UCTYPE
   end type DTYPE

   interface DTYPE
     module procedure :: construct_hconfig_value_UCTYPE
   end interface DTYPE

contains

   function construct_hconfig_value_UCTYPE(value, default) result(this)
      type(DTYPE) :: this
      VTYPE, target :: value
      class(*), optional, intent(in) :: default
      this%value_ptr => value
      if(present(default)) then
         select type(default)
         type is(VTYPE)
            this%default_ = default
         end select
      end if
      this%typestring_ = TYPESTR
   end function construct_hconfig_value_UCTYPE

   logical function value_equals_default_UCTYPE(this) result(lval)
      class(DTYPE), intent(in) :: this
      lval = merge(this%value_ptr == this%default_, .FALSE., allocated(this%default_))
   end function value_equals_default_UCTYPE

   subroutine set_from_hconfig_UCTYPE(this)
      class(DTYPE), intent(inout) :: this
      integer :: status
      this%value_ptr = ESMF_HConfigAsUCTYPE(this%hconfig_, keyString=this%keystring_, rc=status)
      this%last_status_ = status
   end subroutine set_from_hconfig_UCTYPE

   subroutine set_from_default_UCTYPE(this)
      class(DTYPE), intent(inout) :: this
      this%value_ptr = this%default_
   end subroutine set_from_default_UCTYPE

   subroutine get_valuestring_UCTYPE(this, string)
      character(len=*), parameter :: FMT = TFMT
      class(DTYPE), intent(inout) :: this
      character(len=:), allocatable, intent(out) :: string
      integer :: ios
      character(len=32) :: raw
      write(raw, fmt=FMT, iostat=ios) this%value_ptr
      this%last_status_ = ios
      if(ios == 0) string = trim(adjustl(raw))
   end subroutine get_valuestring_UCTYPE

