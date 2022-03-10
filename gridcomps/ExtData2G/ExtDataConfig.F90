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
   implicit none
   private

   type, public :: ExtDataConfig
      integer :: debug
      type(ExtDataRuleMap) :: rule_map
      type(ExtDataDerivedMap) :: derived_map
      type(ExtDataFileStreamMap) :: file_stream_map
      type(ExtDataTimeSampleMap) :: sample_map
      
      contains
         procedure :: get_item_type
         procedure :: get_debug_flag
         procedure :: new_ExtDataConfig_from_yaml
   end type

contains

   recursive subroutine new_ExtDataConfig_from_yaml(ext_config,config_file,current_time,unusable,rc) 
      class(ExtDataConfig), intent(inout), target :: ext_config
      character(len=*), intent(in) :: config_file
      type(ESMF_Time), intent(in) :: current_time
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      type(Parser)              :: p
      type(Configuration) :: config, subcfg, ds_config, rule_config, derived_config, sample_config
      type(ConfigurationIterator) :: iter
      character(len=:), allocatable :: key
      type(ExtDataFileStream) :: ds
      type(ExtDataDerived) :: derived
      type(ExtDataRule) :: rule,ucomp,vcomp
      type(ExtDataTimeSample) :: ts
      integer :: status, semi_pos
      character(len=:), allocatable :: uname,vname
      type(FileStream) :: fstream

      type(ExtDataFileStream), pointer :: temp_ds
      type(ExtDataTimeSample), pointer :: temp_ts
      type(ExtDataRule), pointer :: temp_rule
      type(ExtDataDerived), pointer :: temp_derived

      type(Configuration) :: subconfigs
      character(len=:), allocatable :: sub_file
      integer :: i

      type(ExtDataTimeSample), pointer :: ts_grr

      _UNUSED_DUMMY(unusable)

      p = Parser('core')
      fstream=FileStream(config_file)
      config = p%load(fstream)
      call fstream%close()

      if (config%has("subconfigs")) then 
         subconfigs = config%at("subconfigs")
         _ASSERT(subconfigs%is_sequence(),'subconfigs is not a sequence')
         do i=1,subconfigs%size()
           sub_file = subconfigs%of(i)
            call new_ExtDataConfig_from_yaml(ext_config,sub_file,current_time,rc=status)
            _VERIFY(status)
         end do
      end if
         
      if (config%has("Samplings")) then
         sample_config = config%of("Samplings")
         iter = sample_config%begin()
         do while (iter /= sample_config%end())
            call iter%get_key(key)
            temp_ts => ext_config%sample_map%at(key)
            _ASSERT(.not.associated(temp_ts),"defined duplicate named sample key")
            call iter%get_value(subcfg)
            ts = ExtDataTimeSample(subcfg,_RC)
            _VERIFY(status)
            call ext_config%sample_map%insert(trim(key),ts)
            call iter%next()
         enddo
      end if

      if (config%has("Collections")) then
         ds_config = config%of("Collections")
         iter = ds_config%begin()
         do while (iter /= ds_config%end())
            call iter%get_key(key)
            temp_ds => ext_config%file_stream_map%at(key)
            _ASSERT(.not.associated(temp_ds),"defined duplicate named collection")
            call iter%get_value(subcfg)
            ds = ExtDataFileStream(subcfg,current_time,_RC)
            call ext_config%file_stream_map%insert(trim(key),ds)
            call iter%next()
         enddo
      end if

      if (config%has("Exports")) then
         rule_config = config%of("Exports")
         iter = rule_config%begin()
         do while (iter /= rule_config%end())
            call rule%set_defaults(rc=status)
            _VERIFY(status)
            call iter%get_key(key)
            call iter%get_value(subcfg)
            rule = ExtDataRule(subcfg,ext_config%sample_map,key,_RC)
            semi_pos = index(key,";")
            if (semi_pos > 0) then
               call rule%split_vector(key,ucomp,vcomp,rc=status)
               uname = key(1:semi_pos-1)
               vname = key(semi_pos+1:len_trim(key))
               temp_rule => ext_config%rule_map%at(trim(uname))
               _ASSERT(.not.associated(temp_rule),"duplicated export entry key")
               call ext_config%rule_map%insert(trim(uname),ucomp)
               temp_rule => ext_config%rule_map%at(trim(vname))
               _ASSERT(.not.associated(temp_rule),"duplicated export entry key")
               call ext_config%rule_map%insert(trim(vname),vcomp)
            else
               temp_rule => ext_config%rule_map%at(trim(key))
               _ASSERT(.not.associated(temp_rule),"duplicated export entry key")
               call ext_config%rule_map%insert(trim(key),rule)
            end if
            call iter%next()
         enddo
      end if

      if (config%has("Derived")) then
         derived_config = config%at("Derived")
         iter = derived_config%begin()
         do while (iter /= derived_config%end())
            call derived%set_defaults(rc=status)
            _VERIFY(status)
            call iter%get_key(key)
            call iter%get_value(subcfg)
            derived = ExtDataDerived(subcfg,_RC)
            temp_derived => ext_config%derived_map%at(trim(uname))
             _ASSERT(.not.associated(temp_derived),"duplicated derived entry key")
            call ext_config%derived_map%insert(trim(key),derived)
            call iter%next()
         enddo
      end if

      if (config%has("debug")) then
         call config%get(ext_config%debug,"debug",rc=status)
         _VERIFY(status)
      end if
      ts_grr =>ext_config%sample_map%at('sample_0')

      _RETURN(_SUCCESS)
   end subroutine new_ExtDataConfig_from_yaml

   function get_item_type(this,item_name,unusable,rc) result(item_type)
      class(ExtDataConfig), intent(inout) :: this
      character(len=*), intent(in) :: item_name
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc
      integer :: item_type
      type(ExtDataRule), pointer :: rule
      type(ExtDataDerived), pointer :: derived

      _UNUSED_DUMMY(unusable)
      item_type=ExtData_not_found
      rule => this%rule_map%at(trim(item_name))
      if (associated(rule)) then
         if (allocated(rule%vector_component)) then
            if (rule%vector_component=='EW') then
               item_type=Primary_Type_Vector_comp2
            else if (rule%vector_component=='NS') then
               item_type=Primary_Type_Vector_comp1
            end if
         else
            item_type=Primary_Type_scalar
         end if
      end if
      derived => this%derived_map%at(trim(item_name))
      if (associated(derived)) then
         item_type=derived_type
      end if
      _RETURN(_SUCCESS)
   end function get_item_type
 
   integer function get_debug_flag(this)
      class(ExtDataConfig), intent(inout) :: this
      get_debug_flag=this%debug
   end function get_debug_flag 

end module MAPL_ExtDataConfig
