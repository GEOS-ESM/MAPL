if (label_is_present) then 

#if defined(IS_ARRAY)
   call ESMF_ConfigGetAttribute(config, valuelist = VALUE_, count = count, label = actual_label, _rc)
#else
   call ESMF_ConfigGetAttribute(config, VALUE_, label = actual_label, _rc) 
#endif
   value_is_set = .TRUE.
else 
   select type(default) 
   type is(TYPE_) 
      VALUE_ = default 
      value_is_set = .TRUE.
   class default 
   ! FIXME wdb Maybe different or no macro?
      _fail(MISMATCH_MESSAGE) 
   end select 
   value_is_default = .TRUE. 
end if 
call set_do_print(actual_label, do_print)

! vim:ft=fortran
