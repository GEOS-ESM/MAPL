function construct_hconfig_value_LT_(default) result(this)
   type(HConfigValueUT_) :: this
   class(*), optional, intent(in) :: default
   if(present(default)) then
      select type(default)
      type is(TYPE_)
         this%default_ = default
      end select type
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
   class(HConfigValueUT_), intent(inout) :: this
   character(len=:), allocatable, intent(out) :: string
   integer :: ios
   write(string, fmt=FMT_, iostat=ios) this%value_
   this%last_status_ = ios
end subroutine get_valuestring_LT_

function get_value_LT_(this) result(value)
   TYPE_ :: value
   class(HConfigValueUT_), intent(in) :: this
   value = this%value_
end function get_value_LT_
