#include "MAPL_ErrLog.h"
module MAPL_ExtDataConfig
   use ESMF
   use PFIO
   use gFTL_StringVector
   use MAPL_KeywordEnforcerMod
   use MAPL_ExceptionHandling
   use MAPL_ExtDataFileStream
   use MAPL_ExtDataFileStreamMap
   use MAPL_ExtDataRule
   use MAPL_ExtDataRuleMap
   use MAPL_ExtDataDerived
   use MAPL_ExtDataDerivedMap
   use MAPL_ExtDataConstants
   use MAPL_ExtDataTimeSample
   use MAPL_ExtDataTimeSampleMap
   use MAPL_TimeStringConversion

   implicit none
   private

   character(len=1), parameter :: rule_sep = "+"

   type, public :: ExtDataConfig
      integer :: debug
      type(ExtDataRuleMap) :: rule_map
      type(ExtDataDerivedMap) :: derived_map
      type(ExtDataFileStreamMap) :: file_stream_map
      type(ExtDataTimeSampleMap) :: sample_map

      contains
         procedure :: add_new_rule
         procedure :: get_item_type
         procedure :: new_ExtDataConfig_from_yaml
         procedure :: count_rules_for_item
         procedure :: get_time_range
         procedure :: get_extra_derived_items
         procedure :: has_rule_for
   end type

contains

   recursive subroutine new_ExtDataConfig_from_yaml(ext_config,config_file,current_time,unusable,rc)
      class(ExtDataConfig), intent(inout), target :: ext_config
      character(len=*), intent(in) :: config_file
      type(ESMF_Time), intent(in) :: current_time
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      type(ESMF_HConfig) :: input_config
      type(ESMF_HConfig) :: temp_configs
      type(ESMF_HConfigIter) :: hconfigIter,hconfigIterBegin,hconfigIterEnd
      character(len=:), allocatable :: hconfig_key
      type(ESMF_HConfig) :: single_sample,single_collection,single_export,rule_map,hconfig_val

      character(len=:), allocatable :: new_key
      type(ExtDataFileStream) :: ds
      type(ExtDataDerived) :: derived
      type(ExtDataTimeSample) :: ts
      integer :: status

      type(ExtDataFileStream), pointer :: temp_ds
      type(ExtDataDerived), pointer :: temp_derived

      integer :: i,num_rules
      integer, allocatable :: sorted_rules(:)
      character(len=1) :: i_char
      logical :: file_found
      logical :: is_right_type
      character(len=:), allocatable :: sub_configs(:)

      _UNUSED_DUMMY(unusable)

      inquire(file=trim(config_file),exist=file_found)
      _ASSERT(file_found,"could not find: "//trim(config_file))

      input_config = ESMF_HConfigCreate(filename=trim(config_file),rc=status)
      _ASSERT(status==ESMF_SUCCESS,'FAILED on ESMF_HConfigCreate for '//trim(config_file))

      if (ESMF_HConfigIsDefined(input_config,keyString="subconfigs")) then
         is_right_type = ESMF_HConfigIsSequence(input_config,keyString='subconfigs',_RC)
         _ASSERT(is_right_type,"subconfig list is not a sequence")
         sub_configs = ESMF_HConfigAsStringSeq(input_config,ESMF_MAXPATHLEN,keyString='subconfigs',_RC)
         do i=1,size(sub_configs)
            call new_ExtDataConfig_from_yaml(ext_config,sub_configs(i),current_time,_RC)
         enddo
      end if

      if (ESMF_HConfigIsDefined(input_config,keyString="Samplings")) then
         temp_configs = ESMF_HConfigCreateAt(input_config,keyString="Samplings",_RC)
         hconfigIterBegin = ESMF_HConfigIterBegin(temp_configs)
         hconfigIter = hconfigIterBegin
         hconfigIterEnd = ESMF_HConfigIterEnd(temp_configs)
         do while (ESMF_HConfigIterLoop(hconfigIter,hconfigIterBegin,hconfigIterEnd))
            hconfig_key = ESMF_HConfigAsStringMapKey(hconfigIter,_RC)
            single_sample = ESMF_HConfigCreateAtMapVal(hconfigIter,_RC)
            ts = ExtDataTimeSample(single_sample,_RC)
            call ext_config%sample_map%insert(trim(hconfig_key),ts)
         enddo
         call ESMF_HConfigDestroy(temp_configs)
      end if

      if (ESMF_HConfigIsDefined(input_config,keyString="Collections")) then
         temp_configs = ESMF_HConfigCreateAt(input_config,keyString="Collections",_RC)
         hconfigIterBegin = ESMF_HConfigIterBegin(temp_configs)
         hconfigIter = hconfigIterBegin
         hconfigIterEnd = ESMF_HConfigIterEnd(temp_configs)
         do while (ESMF_HConfigIterLoop(hconfigIter,hconfigIterBegin,hconfigIterEnd))
            hconfig_key = ESMF_HConfigAsStringMapKey(hconfigIter,_RC)
            temp_ds => ext_config%file_stream_map%at(hconfig_key)
           _ASSERT(.not.associated(temp_ds),"defined duplicate named collection " // trim(hconfig_key))
            single_collection = ESMF_HConfigCreateAtMapVal(hconfigIter,_RC)
            ds = ExtDataFileStream(single_collection,current_time,_RC)
            call ext_config%file_stream_map%insert(trim(hconfig_key),ds)
         enddo
         call ESMF_HConfigDestroy(temp_configs)
      end if

      if (ESMF_HConfigIsDefined(input_config,keyString="Exports")) then
         temp_configs = ESMF_HConfigCreateAt(input_config,keyString="Exports",_RC)
         hconfigIterBegin = ESMF_HConfigIterBegin(temp_configs)
         hconfigIter = hconfigIterBegin
         hconfigIterEnd = ESMF_HConfigIterEnd(temp_configs)
         do while (ESMF_HConfigIterLoop(hconfigIter,hconfigIterBegin,hconfigIterEnd))
            hconfig_key = ESMF_HConfigAsStringMapKey(hconfigIter,_RC)
            hconfig_val = ESMF_HConfigCreateAtMapVal(hconfigIter,_RC)
            if (ESMF_HConfigIsMap(hconfig_val)) then
               call ext_config%add_new_rule(hconfig_key,hconfig_val,_RC)
            else if (ESMF_HConfigIsSequence(hconfig_val)) then
               sorted_rules = sort_rules_by_start(hconfig_val,_RC)
               num_rules = ESMF_HConfigGetSize(hconfig_val,_RC)
               do i=1,num_rules
                  rule_map = ESMF_HConfigCreateAt(hconfig_val,index=sorted_rules(i),_RC)
                  write(i_char,'(I1)')i
                  new_key = hconfig_key//rule_sep//i_char
                  call ext_config%add_new_rule(new_key,rule_map,multi_rule=.true.,_RC)
               enddo
            else
               _FAIL("Unsupported type")
            end if
         enddo
      end if

      if (ESMF_HConfigIsDefined(input_config,keyString="Derived")) then
         temp_configs = ESMF_HConfigCreateAt(input_config,keyString="Derived",_RC)
         hconfigIterBegin = ESMF_HConfigIterBegin(temp_configs)
         hconfigIter = hconfigIterBegin
         hconfigIterEnd = ESMF_HConfigIterEnd(temp_configs)
         do while (ESMF_HConfigIterLoop(hconfigIter,hconfigIterBegin,hconfigIterEnd))
            hconfig_key = ESMF_HConfigAsStringMapKey(hconfigIter,_RC)
            single_export = ESMF_HConfigCreateAtMapVal(hconfigIter,_RC)
            derived = ExtDataDerived(single_export,_RC)
            temp_derived => ext_config%derived_map%at(trim(hconfig_key))
             _ASSERT(.not.associated(temp_derived),"duplicated derived entry key")
            call ext_config%derived_map%insert(trim(hconfig_key),derived)
         end do
      end if

      if (ESMF_HConfigIsDefined(input_config,keyString="debug") )then
         ext_config%debug =  ESMF_HConfigAsI4(input_config,keyString="debug",_RC)
      end if

      _RETURN(_SUCCESS)
   end subroutine new_ExtDataConfig_from_yaml

   function count_rules_for_item(this,item_name,rc) result(number_of_rules)
      integer :: number_of_rules
      class(ExtDataConfig), target, intent(in) :: this
      character(len=*), intent(in) :: item_name
      integer, optional, intent(out) :: rc

      type(ExtDataRuleMapIterator) :: rule_iterator
      character(len=:), pointer :: key
      integer :: idx
      rule_iterator = this%rule_map%begin()
      number_of_rules = 0
      do while(rule_iterator /= this%rule_map%end())
         key => rule_iterator%key()
         idx = index(key,rule_sep)
         if (idx > 0) then
            if (trim(item_name)==key(1:idx-1)) number_of_rules = number_of_rules + 1
         else
            if (trim(item_name) == trim(key)) number_of_rules = number_of_rules + 1
         end if
         call rule_iterator%next()
      enddo

      _RETURN(_SUCCESS)
   end function count_rules_for_item

   function get_time_range(this,item_name,rc) result(time_range)
      type(ESMF_Time), allocatable :: time_range(:)
      class(ExtDataConfig), target, intent(in) :: this
      character(len=*), intent(in) :: item_name
      integer, optional, intent(out) :: rc

      type(ExtDataRuleMapIterator) :: rule_iterator
      character(len=:), pointer :: key
      type(StringVector) :: start_times
      integer :: num_rules
      type(ExtDataRule), pointer :: rule
      integer :: i,status,idx
      type(ESMF_Time) :: very_future_time

      rule_iterator = this%rule_map%begin()
      do while(rule_iterator /= this%rule_map%end())
         key => rule_iterator%key()
         idx = index(key,rule_sep)
         if (idx > 0) then
            if (key(1:idx-1) == trim(item_name)) then
               rule => rule_iterator%value()
               call start_times%push_back(rule%start_time)
            end if
         end if
         call rule_iterator%next()
      enddo

      num_rules = start_times%size()
      allocate(time_range(num_rules+1))
      do i=1,num_rules
          time_range(i) = string_to_esmf_time(start_times%at(i))
      enddo
      call ESMF_TimeSet(very_future_time,yy=2365,mm=1,dd=1,_RC)
      time_range(num_rules+1) = very_future_time

      _RETURN(_SUCCESS)
   end function get_time_range

   function sort_rules_by_start(hconfig_sequence,rc) result(sorted_index)
      integer, allocatable :: sorted_index(:)
      type(ESMF_HConfig), intent(inout) :: hconfig_sequence
      integer, optional, intent(out) :: rc

      integer :: num_rules,i,j,i_temp,imin
      logical :: found_start
      type(ESMF_HConfig) :: hconfig_dict
      character(len=:), allocatable :: start_time
      type(ESMF_Time), allocatable :: start_times(:)
      type(ESMF_Time) :: temp_time
      integer :: status

      num_rules = ESMF_HConfigGetSize(hconfig_sequence,_RC)
      allocate(start_times(num_rules))
      allocate(sorted_index(num_rules),source=[(i,i=1,num_rules)])

      do i=1,num_rules
         hconfig_dict = ESMF_HConfigCreateAt(hconfig_sequence,index=i,_RC)
         found_start = ESMF_HConfigIsDefined(hconfig_dict,keyString="starting")
         _ASSERT(found_start,"no start key in multirule export of extdata")
         start_time = ESMF_HConfigAsString(hconfig_dict,keyString="starting",_RC)
         start_times(i) = string_to_esmf_time(start_time)
      enddo

      do i=1,num_rules-1
         imin = i
         do j=i+1,num_rules
            if (start_times(j) < start_times(imin)) then
               temp_time = start_times(imin)
               start_times(imin) = start_times(i)
               start_times(i) = temp_time
               i_temp = sorted_index(imin)
               sorted_index(imin) = sorted_index(i)
               sorted_index(i) = i_temp
            end if
         enddo
      enddo
      _RETURN(_SUCCESS)
   end function sort_rules_by_start

   function get_item_type(this,item_name,unusable,rc) result(item_type)
      class(ExtDataConfig), target, intent(inout) :: this
      character(len=*), intent(in) :: item_name
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc
      integer :: item_type
      type(ExtDataRule), pointer :: rule
      type(ExtDataDerived), pointer :: derived

      type(ExtDataRuleMapIterator) :: rule_iterator
      character(len=:), pointer :: key
      character(len=:), allocatable :: found_key, stripped_name
      logical :: found_rule

      _UNUSED_DUMMY(unusable)
      item_type=ExtData_not_found

      found_rule = .false.
      rule_iterator = this%rule_map%begin()
      do while(rule_iterator /= this%rule_map%end())
         key => rule_iterator%key()
         stripped_name = strip_multi_rule(key)
         if (trim(stripped_name)==trim(item_name)) then
            found_rule = .true.
            found_key = key
            exit
         end if
         call rule_iterator%next()
      enddo

      if (found_rule) then
         rule => this%rule_map%at(found_key)
         if (associated(rule)) then
            if (allocated(rule%vector_component)) then
               if (rule%vector_component=='EW') then
                  item_type=Primary_Type_Vector_comp1
               else if (rule%vector_component=='NS') then
                  item_type=Primary_Type_Vector_comp2
               end if
            else
               item_type=Primary_Type_scalar
            end if
         end if
      end if
      derived => this%derived_map%at(trim(item_name))
      if (associated(derived)) then
         item_type=derived_type
         found_rule = .true.
      end if
      _RETURN(_SUCCESS)

   contains
      function strip_multi_rule(full_name) result(stripped_name)
         character(len=:), allocatable :: stripped_name
         character(len=*), intent(in) :: full_name
 
         integer :: plus_sign
         plus_sign = index(full_name,'+')
         if (plus_sign == 0) then
            stripped_name=full_name
         else
            stripped_name=full_name(:plus_sign-1)
         end if
      end function
   end function get_item_type

   subroutine add_new_rule(this,key,export_rule,multi_rule,rc)
      class(ExtDataConfig), intent(inout) :: this
      character(len=*), intent(in) :: key
      type(ESMF_HConfig), intent(in) :: export_rule
      logical, optional, intent(in) :: multi_rule
      integer, intent(out), optional :: rc

      integer :: semi_pos,status,rule_n_pos
      type(ExtDataRule) :: rule,ucomp,vcomp
      type(ExtDataRule), pointer :: temp_rule
      character(len=:), allocatable :: uname,vname,original_key
      logical :: usable_multi_rule
      character(len=1) :: rule_num

      if (present(multi_rule)) then
         usable_multi_rule = multi_rule
      else
         usable_multi_rule = .false.
      end if

      call rule%set_defaults(rc=status)
      _VERIFY(status)
      rule = ExtDataRule(export_rule,this%sample_map,key,multi_rule=usable_multi_rule,_RC)
      semi_pos = index(key,";")
      if (semi_pos > 0) then
         rule_n_pos = index(key,rule_sep)
         original_key = key
         if (rule_n_pos > 0) original_key = key(1:rule_n_pos-1)

         call rule%split_vector(original_key,ucomp,vcomp,rc=status)
         uname = key(1:semi_pos-1)
         vname = key(semi_pos+1:len_trim(key))

         if (rule_n_pos > 0) then
            rule_num = key(rule_n_pos+1:rule_n_pos+1)
            uname=uname//rule_sep//rule_num
         end if

         temp_rule => this%rule_map%at(trim(uname))
         _ASSERT(.not.associated(temp_rule),"duplicated export entry key: "//trim(uname))
         call this%rule_map%insert(trim(uname),ucomp)
         temp_rule => this%rule_map%at(trim(vname))
         _ASSERT(.not.associated(temp_rule),"duplicated export entry key: "//trim(vname))
         call this%rule_map%insert(trim(vname),vcomp)
      else
         temp_rule => this%rule_map%at(trim(key))
         _ASSERT(.not.associated(temp_rule),"duplicated export entry key: "//trim(key))
         call this%rule_map%insert(trim(key),rule)
      end if
      _RETURN(_SUCCESS)
   end subroutine add_new_rule

   function get_extra_derived_items(this,primary_items,derived_items,rc) result(needed_vars)
      type(StringVector) :: needed_vars
      class(ExtDataConfig), intent(inout) :: this
      type(StringVector), intent(in) :: primary_items
      type(StringVector), intent(in) :: derived_items
      integer, intent(out), optional :: rc

      integer :: status
      type(StringVectorIterator) :: string_iter
      type(ExtDataDerived), pointer :: derived_item
      type(StringVector) :: variables_in_expression
      character(len=:), pointer :: sval,derived_name
      logical :: in_primary,found_rule
      integer :: i

      if (derived_items%size() ==0) then
         _RETURN(_SUCCESS)
      end if

      string_iter = derived_items%begin()
      do while(string_iter /= derived_items%end() )
         derived_name => string_iter%get()
         derived_item => this%derived_map%at(derived_name)
         variables_in_expression = derived_item%get_variables_in_expression(_RC)
         ! now we have a stringvector of the variables involved in the expression
         ! check which of this are already in primary_items list, if any are not
         ! then we need to createa new list of needed variables and the "derived field"
         ! wence to coppy them
         do i=1,variables_in_expression%size()
            sval => variables_in_expression%at(i)
            in_primary = string_in_stringVector(sval,primary_items)
            if (.not.in_primary) then
               found_rule = this%has_rule_for(sval,_RC)
               _ASSERT(found_rule,"no rule for "//trim(sval)//" needed by "//trim(derived_name))
               call needed_vars%push_back(sval//","//derived_name)
            end if
         enddo
         call string_iter%next()
      enddo

      _RETURN(_SUCCESS)
   end function get_extra_derived_items

   function has_rule_for(this,base_name,rc) result(found_rule)
      logical :: found_rule
      class(ExtDataConfig), intent(inout) :: This
      character(len=*), intent(in) :: base_name
      integer, optional, intent(out) :: rc

      type(ExtDataRuleMapIterator) :: iter
      character(len=:), pointer :: key
      integer :: rule_sep_loc

      found_rule = .false.
      iter = this%rule_map%begin()
      do while(iter /= this%rule_map%end())
         key => iter%key()
         rule_sep_loc = index(key,rule_sep)
         if (rule_sep_loc/=0) then
            found_rule = (key(:rule_sep_loc-1) == base_name)
         else
            found_rule = (key == base_name)
         end if
         if (found_rule) exit
         call iter%next()
      enddo
      _RETURN(_SUCCESS)
   end function

end module MAPL_ExtDataConfig
