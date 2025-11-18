#include "MAPL_ErrLog.h"
module mapl3g_ExtDataRule
   use ESMF
   use MAPL_KeywordEnforcerMod
   use MAPL_ExceptionHandling
   use MAPL_TimeStringConversion
   use mapl3g_ExtDataSample
   use mapl3g_ExtDataSampleMap
   use gFTL2_StringVector
   implicit none
   private

   type, public :: ExtDataRule
      character(:), allocatable :: start_time
      character(:), allocatable :: collection
      !character(:), allocatable :: file_var
      type(StringVector) :: file_vars
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
   end type

   interface ExtDataRule
      module procedure new_ExtDataRule
   end interface

contains

   function new_ExtDataRule(config,sample_map,key,unusable,multi_rule,rc) result(rule)
      type(ESMF_HConfig), intent(in) :: config
      character(len=*), intent(in) :: key
      type(ExtDataSampleMap) :: sample_map
      class(KeywordEnforcer), optional, intent(in) :: unusable
      logical, optional, intent(in) :: multi_rule
      integer, optional, intent(out) :: rc

      type(ExtDataRule) :: rule
      logical :: collection_present, variable_present
      integer :: status
      type(ESMF_HConfig) ::config1
      character(len=:), allocatable :: tempc
      type(ExtDataSample) :: ts
      logical :: usable_multi_rule
      _UNUSED_DUMMY(unusable)

      if (present(multi_rule)) then
         usable_multi_rule = multi_rule
      else
         usable_multi_rule = .false.
      end if

      if (allocated(tempc)) deallocate(tempc)
      collection_present = ESMF_HConfigIsDefined(config,keyString="collection")
      _ASSERT(collection_present,"no collection present in ExtData export")
      rule%collection = ESMF_HConfigAsString(config,keyString="collection",_RC)

      if (allocated(tempc)) deallocate(tempc)
      variable_present = ESMF_HConfigIsDefined(config,keyString="variable")
      if (index(rule%collection,"/dev/null")==0) then
         _ASSERT(variable_present,"no variable present in ExtData export")
      end if
      if (variable_present) then
         tempc = ESMF_HConfigAsString(config,keyString="variable",_RC)
         rule%file_vars = split_file_var(tempc)
      else
         call rule%file_vars%push_back('null')
      end if

      if (ESMF_HConfigIsDefined(config,keyString="sample")) then

         config1 = ESMF_HConfigCreateAt(config,keyString="sample",_RC)
         if (ESMF_HConfigIsMap(config1)) then
            ts = ExtDataSample(config1,_RC)
            call sample_map%insert(trim(key)//"_sample",ts)
            rule%sample_key=trim(key)//"_sample"
         else
            rule%sample_key=ESMF_HConfigAsString(config1,_RC)
         end if
      else
         rule%sample_key = ""
      end if

      if (allocated(rule%linear_trans)) deallocate(rule%linear_trans)
      if (ESMF_HConfigIsDefined(config,keyString="linear_transformation")) then
         allocate(rule%linear_trans(2))
         rule%linear_trans = ESMF_HConfigAsR4Seq(config,keyString="linear_transformation",_RC)
      else
         allocate(rule%linear_trans,source=[0.0,1.0])
      end if

      if (allocated(tempc)) deallocate(tempc)
      if (ESMF_HConfigIsDefined(config,keyString="regrid")) then
         tempc = ESMF_HConfigAsString(config,keyString="regrid",_RC)
         rule%regrid_method=tempc
      else
         rule%regrid_method="BILINEAR"
      end if

      if (ESMF_HConfigIsDefined(config,keyString="starting")) then
         tempc = ESMF_HConfigAsString(config,keyString="starting",_RC)
         rule%start_time = tempc
      end if

      if (ESMF_HConfigIsDefined(config,keyString="fail_on_missing_file")) then
         rule%fail_on_missing_file = ESMF_HConfigAsLogical(config,keyString="fail_on_missing_file",_RC)
      end if

      if (ESMF_HConfigIsDefined(config,keyString="enable_vertical_regrid")) then
         rule%enable_vertical_regrid = ESMF_HConfigAsLogical(config,keyString="enable_vertical_regrid",_RC)
      else
         rule%enable_vertical_regrid = .false.
      end if

      rule%multi_rule=usable_multi_rule

      _RETURN(_SUCCESS)
   end function new_ExtDataRule

   subroutine set_defaults(this,unusable,rc)
      class(ExtDataRule), intent(inout), target :: this
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      _UNUSED_DUMMY(unusable)
      this%collection=''
      call this%file_vars%push_back('missing_variable')
      this%regrid_method='BILINEAR'
      _RETURN(_SUCCESS)
   end subroutine set_defaults

   function split_file_var(original_string) result(file_vars)
      type(StringVector) :: file_vars
      character(len=*), intent(in) :: original_string
      integer :: semi_pos

      semi_pos = index(original_string, ';')
      if (semi_pos > 0) then
         call file_vars%push_back(original_string(1:semi_pos-1))
         call file_vars%push_back(original_string(semi_pos+1:))
      else
         call file_vars%push_back(original_string)
      end if
   end function

end module mapl3g_ExtDataRule
