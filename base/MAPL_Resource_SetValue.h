if (label_is_present) then 

#if defined(IS_ARRAY)
   call ESMF_ConfigGetAttribute(config, valuelist = VAL_, count = count, label = actual_label, _RC)
#else
   call ESMF_ConfigGetAttribute(config, VAL_, label = actual_label, _RC) 
#endif

else 
   select type(default) 
   type is(TYPE_) 
      VAL_ = default 
   class default 
      _FAIL(MISMATCH_MESSAGE) 
   end select 
   value_is_default = .TRUE. 
end if 
call set_do_print(actual_label, do_print)
