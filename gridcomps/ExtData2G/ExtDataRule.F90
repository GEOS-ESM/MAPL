#include "MAPL_ErrLog.h"
module MAPL_ExtDataRule
   use ESMF
   use MAPL_KeywordEnforcerMod
   use MAPL_ExceptionHandling
   use MAPL_TimeStringConversion
   use MAPL_ExtDataTimeSample
   use MAPL_ExtDataTimeSampleMap
   implicit none
   private

   type, public :: ExtDataRule
      character(:), allocatable :: start_time
      character(:), allocatable :: collection
      character(:), allocatable :: file_var
      character(:), allocatable :: sample_key
      real, allocatable :: linear_trans(:)
      character(:), allocatable :: regrid_method
      character(:), allocatable :: vector_partner
      character(:), allocatable :: vector_component
      character(:), allocatable :: vector_file_partner
      logical :: enable_vertical_regrid
      logical :: multi_rule
      logical :: fail_on_missing_file = .true.
      contains
         procedure :: set_defaults
         procedure :: split_vector
   end type

   interface ExtDataRule
      module procedure new_ExtDataRule
   end interface

contains

   function new_ExtDataRule(config,sample_map,key,unusable,multi_rule,rc) result(rule)
      type(ESMF_HConfig), intent(in) :: config
      character(len=*), intent(in) :: key
      type(ExtDataTimeSampleMap) :: sample_map
      class(KeywordEnforcer), optional, intent(in) :: unusable
      logical, optional, intent(in) :: multi_rule
      integer, optional, intent(out) :: rc

      type(ExtDataRule) :: rule
      logical :: collection_present, variable_present
      integer :: status
      type(ESMF_HConfig) ::config1
      character(len=:), allocatable :: tempc
      type(ExtDataTimeSample) :: ts
      logical :: usable_multi_rule
      __UNUSED_DUMMY(unusable)

      if (present(multi_rule)) then
         usable_multi_rule = multi_rule
      else
         usable_multi_rule = .false.
      end if

      if (allocated(tempc)) deallocate(tempc)
      collection_present = ESMF_HConfigIsDefined(config,keyString="collection")
      __ASSERT(collection_present,"no collection present in ExtData export")
      rule%collection = ESMF_HConfigAsString(config,keyString="collection",__RC)

      if (allocated(tempc)) deallocate(tempc)
      variable_present = ESMF_HConfigIsDefined(config,keyString="variable")
      if (index(rule%collection,"/dev/null")==0) then
         __ASSERT(variable_present,"no variable present in ExtData export")
      end if
      if (variable_present) then
         tempc = ESMF_HConfigAsString(config,keyString="variable",__RC)
         rule%file_var=tempc
      else
         rule%file_var='null'
      end if

      if (ESMF_HConfigIsDefined(config,keyString="sample")) then

         config1 = ESMF_HConfigCreateAt(config,keyString="sample",__RC)
         if (ESMF_HConfigIsMap(config1)) then
            ts = ExtDataTimeSample(config1,__RC)
            call sample_map%insert(trim(key)//"_sample",ts)
            rule%sample_key=trim(key)//"_sample"
         else
            rule%sample_key=ESMF_HConfigAsString(config1,__RC)
         end if
      else
         rule%sample_key = ""
      end if

      if (allocated(rule%linear_trans)) deallocate(rule%linear_trans)
      if (ESMF_HConfigIsDefined(config,keyString="linear_transformation")) then
         allocate(rule%linear_trans(2))
         rule%linear_trans = ESMF_HConfigAsR4Seq(config,keyString="linear_transformation",__RC)
      else
         allocate(rule%linear_trans,source=[0.0,0.0])
      end if

      if (allocated(tempc)) deallocate(tempc)
      if (ESMF_HConfigIsDefined(config,keyString="regrid")) then
         tempc = ESMF_HConfigAsString(config,keyString="regrid",__RC)
         rule%regrid_method=tempc
      else
         rule%regrid_method="BILINEAR"
      end if

      if (ESMF_HConfigIsDefined(config,keyString="starting")) then
         tempc = ESMF_HConfigAsString(config,keyString="starting",__RC)
         rule%start_time = tempc
      end if

      if (ESMF_HConfigIsDefined(config,keyString="fail_on_missing_file")) then
         rule%fail_on_missing_file = ESMF_HConfigAsLogical(config,keyString="fail_on_missing_file",__RC)
      end if

      if (ESMF_HConfigIsDefined(config,keyString="enable_vertical_regrid")) then
         rule%enable_vertical_regrid = ESMF_HConfigAsLogical(config,keyString="enable_vertical_regrid",__RC)
      else
         rule%enable_vertical_regrid = .false.
      end if

      rule%multi_rule=usable_multi_rule

      __RETURN(__SUCCESS)
   end function new_ExtDataRule

   subroutine set_defaults(this,unusable,rc)
      class(ExtDataRule), intent(inout), target :: this
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      __UNUSED_DUMMY(unusable)
      this%collection=''
      this%file_var='missing_variable'
      this%regrid_method='BILINEAR'
      __RETURN(__SUCCESS)
   end subroutine set_defaults

   subroutine split_vector(this,original_key,ucomp,vcomp,unusable,rc)
      class(ExtDataRule), intent(in) :: this
      character(len=*), intent(in) :: original_key
      type(ExtDataRule), intent(inout) :: ucomp,vcomp
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc
      integer :: semi_pos
      character(len=:),allocatable :: uname,vname

      __UNUSED_DUMMY(unusable)

      semi_pos = index(this%file_var,";")
      __ASSERT(semi_pos > 0,"vector rule does not have 2 variables in the file_var")
      uname = this%file_var(1:semi_pos-1)
      vname = this%file_var(semi_pos+1:len_trim(this%file_var))
      ucomp = this
      vcomp = this
      semi_pos = index(original_key,";")
      ucomp%vector_partner = original_key(semi_pos+1:len_trim(original_key))
      vcomp%vector_partner = original_key(1:semi_pos-1)
      ucomp%file_var = uname
      vcomp%file_var = vname
      ucomp%vector_file_partner = vname
      vcomp%vector_file_partner = uname
      ucomp%vector_component = "EW"
      vcomp%vector_component = "NS"
      __RETURN(__SUCCESS)

   end subroutine split_vector

end module MAPL_ExtDataRule

module MAPL_ExtDataRuleMap
   use MAPL_ExtDataRule

#include "types/key_deferredLengthString.inc"
#define _value type(ExtDataRule)
#define _alt

#define _map ExtDataRuleMap
#define _iterator ExtDataRuleMapIterator

#include "templates/map.inc"

#undef _iterator
#undef _map

#undef _alt
#undef _value

end module MAPL_ExtDataRuleMap
