#include "MAPL_ErrLog.h"
module MAPL_ExtDataConfig
   use ESMF
   use yaFyaml
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
   use MAPL_ExtDataMask
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
   end type

contains

   recursive subroutine new_ExtDataConfig_from_yaml(ext_config,config_file,current_time,unusable,rc) 
      class(ExtDataConfig), intent(inout), target :: ext_config
      character(len=*), intent(in) :: config_file
      type(ESMF_Time), intent(in) :: current_time
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      type(Parser)              :: p
      class(YAML_Node), allocatable :: config
      class(YAML_Node), pointer :: subcfg, ds_config, rule_config, derived_config, sample_config, subconfigs, rule_map
      class(NodeIterator), allocatable :: iter
      character(len=:), pointer :: key
      character(len=:), allocatable :: new_key
      type(ExtDataFileStream) :: ds
      type(ExtDataDerived) :: derived
      type(ExtDataTimeSample) :: ts
      integer :: status

      type(ExtDataFileStream), pointer :: temp_ds
      type(ExtDataTimeSample), pointer :: temp_ts
      type(ExtDataDerived), pointer :: temp_derived

      character(len=:), pointer :: sub_file
      integer :: i,num_rules
      integer, allocatable :: sorted_rules(:)
      character(len=1) :: i_char

      _UNUSED_DUMMY(unusable)

      p = Parser('core')
      config = p%load(config_file)

      if (config%has("subconfigs")) then 
         subconfigs => config%at("subconfigs")
         _ASSERT(subconfigs%is_sequence(),'subconfigs is not a sequence')
         do i=1,subconfigs%size()
           sub_file => to_string(subconfigs%at(i))
           call new_ExtDataConfig_from_yaml(ext_config,sub_file,current_time,rc=status)
           _VERIFY(status)
         end do
      end if
         
      if (config%has("Samplings")) then
         sample_config => config%of("Samplings")
         iter = sample_config%begin()
         do while (iter /= sample_config%end())
            key => to_string(iter%first(),_RC)
            temp_ts => ext_config%sample_map%at(key)
            _ASSERT(.not.associated(temp_ts),"defined duplicate named sample key")
            subcfg => iter%second()
            ts = ExtDataTimeSample(subcfg,_RC)
            _VERIFY(status)
            call ext_config%sample_map%insert(trim(key),ts)
            call iter%next()
         enddo
      end if

      if (config%has("Collections")) then
         ds_config => config%of("Collections")
         iter = ds_config%begin()
         do while (iter /= ds_config%end())
            key => to_string(iter%first(),_RC)
            temp_ds => ext_config%file_stream_map%at(key)
            _ASSERT(.not.associated(temp_ds),"defined duplicate named collection")
            subcfg => iter%second()
            ds = ExtDataFileStream(subcfg,current_time,_RC)
            call ext_config%file_stream_map%insert(trim(key),ds)
            call iter%next()
         enddo
      end if

      if (config%has("Exports")) then
         rule_config => config%of("Exports")
         iter = rule_config%begin()
         do while (iter /= rule_config%end())
            key => to_string(iter%first(),_RC)
            subcfg => iter%second()
            if (subcfg%is_mapping()) then
               call ext_config%add_new_rule(key,subcfg,_RC)
            else if (subcfg%is_sequence()) then
               sorted_rules = sort_rules_by_start(subcfg,_RC) 
               num_rules = subcfg%size()
               do i=1,num_rules
                  rule_map => subcfg%of(sorted_rules(i))
                  write(i_char,'(I1)')i
                  new_key = key//rule_sep//i_char
                  call ext_config%add_new_rule(new_key,rule_map,multi_rule=.true.,_RC)
               enddo 
            else
               _FAIL("Exports must be sequence or map")
            end if
            call iter%next()
         enddo
      end if

      if (config%has("Derived")) then
         derived_config => config%at("Derived")
         iter = derived_config%begin()
         do while (iter /= derived_config%end())
            call derived%set_defaults(rc=status)
            _VERIFY(status)
            key => to_string(iter%first(),_RC)
            subcfg => iter%second()
            derived = ExtDataDerived(subcfg,_RC)
            temp_derived => ext_config%derived_map%at(trim(key))
             _ASSERT(.not.associated(temp_derived),"duplicated derived entry key")
            call ext_config%derived_map%insert(trim(key),derived)
            call iter%next()
         enddo
      end if

      if (config%has("debug")) then
         call config%get(ext_config%debug,"debug",rc=status)
         _VERIFY(status)
      end if

      _RETURN(_SUCCESS)
   end subroutine new_ExtDataConfig_from_yaml

   function count_rules_for_item(this,item_name,rc) result(number_of_rules)
      integer :: number_of_rules
      class(ExtDataConfig), intent(in) :: this
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
      class(ExtDataConfig), intent(in) :: this
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

   function sort_rules_by_start(yaml_sequence,rc) result(sorted_index)
      integer, allocatable :: sorted_index(:)
      class(YAML_Node), intent(inout) :: yaml_sequence
      integer, optional, intent(out) :: rc

      integer :: num_rules,i,j,i_temp,imin
      logical :: found_start
      class(YAML_Node), pointer :: yaml_dict
      character(len=:), allocatable :: start_time
      type(ESMF_Time), allocatable :: start_times(:)
      type(ESMF_Time) :: temp_time

      num_rules = yaml_sequence%size()
      allocate(start_times(num_rules))
      allocate(sorted_index(num_rules),source=[(i,i=1,num_rules)])

      do i=1,num_rules
         yaml_dict => yaml_sequence%of(i)
         found_start = yaml_dict%has("starting")
         _ASSERT(found_start,"no start key in multirule export of extdata")
         start_time = yaml_dict%of("starting")
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
      class(ExtDataConfig), intent(inout) :: this
      character(len=*), intent(in) :: item_name
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc
      integer :: item_type
      type(ExtDataRule), pointer :: rule
      type(ExtDataDerived), pointer :: derived

      type(ExtDataRuleMapIterator) :: rule_iterator
      character(len=:), pointer :: key
      character(len=:), allocatable :: found_key
      logical :: found_rule

      _UNUSED_DUMMY(unusable)
      item_type=ExtData_not_found
 
      found_rule = .false.
      rule_iterator = this%rule_map%begin()
      do while(rule_iterator /= this%rule_map%end())
         key => rule_iterator%key()
         if (index(key,trim(item_name))/=0) then
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
   end function get_item_type

   subroutine add_new_rule(this,key,export_rule,multi_rule,rc) 
      class(ExtDataConfig), intent(inout) :: this
      character(len=*), intent(in) :: key
      class(YAML_Node), intent(in) :: export_rule
      logical, optional, intent(in) :: multi_rule
      integer, intent(out), optional :: rc

      integer :: semi_pos,status
      type(ExtDataRule) :: rule,ucomp,vcomp
      type(ExtDataRule), pointer :: temp_rule
      character(len=:), allocatable :: uname,vname
      logical :: usable_multi_rule

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
         call rule%split_vector(key,ucomp,vcomp,rc=status)
         uname = key(1:semi_pos-1)
         vname = key(semi_pos+1:len_trim(key))
         temp_rule => this%rule_map%at(trim(uname))
         _ASSERT(.not.associated(temp_rule),"duplicated export entry key")
         call this%rule_map%insert(trim(uname),ucomp)
         temp_rule => this%rule_map%at(trim(vname))
         _ASSERT(.not.associated(temp_rule),"duplicated export entry key")
         call this%rule_map%insert(trim(vname),vcomp)
      else
         temp_rule => this%rule_map%at(trim(key))
         _ASSERT(.not.associated(temp_rule),"duplicated export entry key")
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
      type(ExtDataRule), pointer :: rule
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
            if (.not.string_in_string_vector(sval,primary_items)) then
               rule => this%rule_map%at(sval)
               _ASSERT(associated(rule),"no rule for "//trim(sval)//" needed by "//trim(derived_name))
               call needed_vars%push_back(sval//","//derived_name)                
            end if
         enddo
         call string_iter%next()
      enddo
      
      _RETURN(_SUCCESS)
   end function get_extra_derived_items

   function string_in_string_vector(target_string,string_vector) result(in_vector)
      logical :: in_vector
      character(len=*), intent(in) :: target_string
      type(StringVector), intent(in) :: string_vector

      type(StringVectorIterator) :: iter

      in_vector = .false.
      iter = string_vector%begin()
      do while(iter /= string_vector%end())
         if (trim(target_string) == iter%get()) in_vector = .true.
         call iter%next()
      enddo
   end function string_in_string_vector

end module MAPL_ExtDataConfig
