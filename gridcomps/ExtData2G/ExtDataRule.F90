#include "MAPL_ErrLog.h"
module MAPL_ExtDataRule
   use yaFyaml
   use ESMF
   use MAPL_KeywordEnforcerMod
   use MAPL_ExceptionHandling
   use MAPL_TimeStringConversion
   use MAPL_ExtDataTimeSample
   use MAPL_ExtDataTimeSampleMap
   implicit none
   private

   type, public :: ExtDataRule
      character(:), allocatable :: collection
      character(:), allocatable :: file_var
      character(:), allocatable :: sample_key
      real, allocatable :: linear_trans(:)
      character(:), allocatable :: regrid_method
      character(:), allocatable :: vector_partner
      character(:), allocatable :: vector_component
      character(:), allocatable :: vector_file_partner
      contains
         procedure :: set_defaults
         procedure :: split_vector
   end type

   interface ExtDataRule
      module procedure new_ExtDataRule
   end interface

contains

   function new_ExtDataRule(config,sample_map,key,unusable,rc) result(rule)
      type(Configuration), intent(in) :: config
      character(len=*), intent(in) :: key
      type(ExtDataTimeSampleMap) :: sample_map
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      type(ExtDataRule) :: rule
      logical :: is_present
      integer :: status
      type(Configuration) ::config1
      character(len=:), allocatable :: tempc
      type(ExtDataTimeSample) :: ts
      _UNUSED_DUMMY(unusable)

      if (allocated(tempc)) deallocate(tempc)
      is_present = config%has("collection")
      _ASSERT(is_present,"no collection present in ExtData export")
      rule%collection = config%of("collection")

      if (allocated(tempc)) deallocate(tempc)
      is_present = config%has("vname")
      if (index(rule%collection,"/dev/null")==0) then
         _ASSERT(is_present,"no vname present in ExtData export")
      end if
      if (is_present) then
         tempc = config%of("vname")
         rule%file_var=tempc
      else
         _ASSERT(.false.,"no variable name in rule")
      end if

      if (config%has("sample")) then
         config1=config%at("sample")
         if (config1%is_mapping()) then
            ts = ExtDataTimeSample(config1,_RC)
            call sample_map%insert(trim(key)//"_sample",ts)
            rule%sample_key=trim(key)//"_sample"
         else if (config1%is_string()) then
            rule%sample_key=config1
         else
            _ASSERT(.false.,"sample entry unsupported")
         end if
      end if

      if (allocated(rule%linear_trans)) deallocate(rule%linear_trans)
      if (config%has("linear_transformation")) then
         call config%get(rule%linear_trans,"linear_transformation")
      else
         allocate(rule%linear_trans,source=[0.0,0.0])
      end if
    
      if (allocated(tempc)) deallocate(tempc)
      if (config%has("regrid")) then
         tempc = config%of("regrid")
         rule%regrid_method=tempc
      else 
         rule%regrid_method="BILINEAR"
      end if

      _RETURN(_SUCCESS)
   end function new_ExtDataRule

   subroutine set_defaults(this,unusable,rc)
      class(ExtDataRule), intent(inout), target :: this
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      _UNUSED_DUMMY(unusable)
      this%collection=''
      this%file_var='missing_variable'
      this%regrid_method='BILINEAR'
      _RETURN(_SUCCESS)
   end subroutine set_defaults

   subroutine split_vector(this,original_key,ucomp,vcomp,unusable,rc)
      class(ExtDataRule), intent(in) :: this
      character(len=*), intent(in) :: original_key
      type(ExtDataRule), intent(inout) :: ucomp,vcomp
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc
      integer :: semi_pos
      character(len=:),allocatable :: uname,vname
    
      _UNUSED_DUMMY(unusable)

      semi_pos = index(this%file_var,";")
      _ASSERT(semi_pos > 0,"vector rule does not have 2 variables in the file_var")
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
      _RETURN(_SUCCESS)

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
