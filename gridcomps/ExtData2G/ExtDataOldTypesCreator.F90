#include "MAPL_Exceptions.h"
#include "MAPL_ErrLog.h"

module MAPL_ExtDataOldTypesCreator
   use ESMF
   use MAPL_BaseMod
   use yafYaml
   use MAPL_KeywordEnforcerMod
   use MAPL_ExceptionHandling
   use MAPL_ExtDataTypeDef
   use MAPL_ExtDataConfig
   use MAPL_ExtDataFileStream
   use MAPL_ExtDataFileStreamMap
   use MAPL_ExtDataRule
   use MAPL_ExtDataRuleMap
   use MAPL_ExtDataDerived
   use MAPL_ExtDataDerivedMap
   use MAPL_RegridMethods
   use MAPL_ExtDataAbstractFileHandler
   use MAPL_ExtDataSimpleFileHandler
   use MAPL_ExtDataClimFileHandler
   use MAPL_ExtDataTimeSample
   use MAPL_ExtDataTimeSampleMap
   implicit none
   public :: ExtDataOldTypesCreator

   type, extends(ExtDataConfig) :: ExtDataOldTypesCreator
      private
      contains
         procedure :: fillin_primary
         procedure :: fillin_derived
   end type ExtDataOldTypesCreator

   interface ExtDataOldTypesCreator
      module procedure :: new_ExtDataOldTypesCreator
   end interface

   contains

   function new_ExtDataOldTypesCreator(config_file,current_time,unusable,rc ) result(ExtDataObj)
      character(len=*), intent(in) :: config_file
      type(ESMF_Time), intent(in) :: current_time
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      type(ExtDataOldTypesCreator) :: ExtDataObj

      integer :: status

      _UNUSED_DUMMY(unusable)
      call ExtDataObj%ExtDataConfig%new_ExtDataConfig_from_yaml(config_file,current_time,rc=status)
      _VERIFY(status)

      _RETURN(_SUCCESS)
   end function new_ExtDataOldTypesCreator

   
   subroutine fillin_primary(this,item_name,primary_item,time,clock,unusable,rc)
      class(ExtDataOldTypesCreator), intent(inout) :: this
      character(len=*), intent(in) :: item_name
      type(PrimaryExport), intent(inout) :: primary_item
      type(ESMF_Time), intent(inout) :: time
      type(ESMF_Clock), intent(inout) :: clock
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      type(ExtDataRule), pointer :: rule
      type(ExtDataFileStream),  pointer :: dataset
      type(ExtDataTimeSample), pointer :: time_sample
      type(ExtDataTimeSample), target :: default_time_sample
      type(ExtDataSimpleFileHandler) :: simple_handler
      type(ExtDataClimFileHandler) :: clim_handler
      integer :: status, semi_pos
      logical :: disable_interpolation

      _UNUSED_DUMMY(unusable)
      rule => this%rule_map%at(trim(item_name))
      time_sample => this%sample_map%at(rule%sample_key)
      
      if(.not.associated(time_sample)) then
        call default_time_sample%set_defaults()
        time_sample=>default_time_sample
      end if
      primary_item%isVector = allocated(rule%vector_partner)
      ! name and file var
      primary_item%name = trim(item_name)
      if (primary_item%isVector) then
         primary_item%vartype = MAPL_VectorField
         primary_item%vcomp1 = trim(item_name)
         primary_item%vcomp2 = trim(rule%vector_partner)
         primary_item%var = rule%file_var
         primary_item%fcomp1 = rule%file_var
         primary_item%fcomp2 = rule%vector_file_partner
         primary_item%fileVars%itemType = ItemTypeVector
         primary_item%fileVars%xname  = trim(rule%file_var)
         primary_item%fileVars%yname  = trim(rule%vector_file_partner)
      else
         primary_item%vartype = MAPL_FieldItem
         primary_item%vcomp1 = trim(item_name)
         primary_item%var = rule%file_var
         primary_item%fcomp1 = rule%file_var
         primary_item%fileVars%itemType = ItemTypeScalar
         primary_item%fileVars%xname  = trim(rule%file_var)
      end if
      
      ! regrid method
      if (trim(rule%regrid_method) == "BILINEAR") then
         primary_item%trans = REGRID_METHOD_BILINEAR
      else if (trim(rule%regrid_method) == "CONSERVE") then
         primary_item%trans = REGRID_METHOD_CONSERVE
      else if (trim(rule%regrid_method) == "VOTE") then
         primary_item%trans = REGRID_METHOD_VOTE
      else if (index(rule%regrid_method,"FRACTION;")>0) then
         semi_pos = index(rule%regrid_method,";")
         read(rule%regrid_method(semi_pos+1:),*) primary_item%fracVal
         primary_item%trans = REGRID_METHOD_FRACTION
      else 
         _ASSERT(.false.,"Invalid regridding method")
      end if

      if (trim(time_sample%extrap_outside) =="clim") then
         primary_item%cycling=.true.
      else if (trim(time_sample%extrap_outside) == "persist_closest") then
         primary_item%persist_closest=.true.
      else if (trim(time_sample%extrap_outside) == "none") then
         primary_item%cycling=.false.
         primary_item%persist_closest=.false.
      end if

      allocate(primary_item%source_time,source=time_sample%source_time)
      ! new refresh
      call primary_item%update_freq%create_from_parameters(time_sample%refresh_time, &
           time_sample%refresh_frequency, time_sample%refresh_offset, time, clock, __RC__)

      disable_interpolation =  .not.time_sample%time_interpolation 

      call primary_item%modelGridFields%comp1%set_parameters(linear_trans=rule%linear_trans,disable_interpolation=disable_interpolation)
      call primary_item%modelGridFields%comp2%set_parameters(linear_trans=rule%linear_trans,disable_interpolation=disable_interpolation)
      call primary_item%modelGridFields%auxiliary1%set_parameters(linear_trans=rule%linear_trans, disable_interpolation=disable_interpolation)
      call primary_item%modelGridFields%auxiliary2%set_parameters(linear_trans=rule%linear_trans, disable_interpolation=disable_interpolation)

      ! file_template
      primary_item%isConst = .false.
      if (index(rule%collection,"/dev/null")==0) then
         dataset => this%file_stream_map%at(trim(rule%collection))
         primary_item%file = dataset%file_template
         call dataset%detect_metadata(primary_item%file_metadata,time,get_range=(trim(time_sample%extrap_outside) /= "none"),__RC__)
      else
         primary_item%file = rule%collection
      end if

      if (index(rule%collection,'/dev/null') /= 0) then
         primary_item%isConst = .true.
         primary_item%const=rule%linear_trans(1)
      else
         if (primary_item%cycling) then
            call clim_handler%initialize(dataset,__RC__)
            allocate(primary_item%filestream,source=clim_handler)
         else
            call simple_handler%initialize(dataset,persist_closest=primary_item%persist_closest,__RC__)
            allocate(primary_item%filestream,source=simple_handler)
         end if
      end if

      _RETURN(_SUCCESS)

   end subroutine fillin_primary

   subroutine fillin_derived(this,item_name,derived_item,time,clock,unusable,rc)
      class(ExtDataOldTypesCreator), intent(inout) :: this
      character(len=*), intent(in) :: item_name
      type(DerivedExport), intent(inout) :: derived_item
      type(ESMF_Time), intent(inout) :: time
      type(ESMF_Clock), intent(inout) :: clock
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      type(ExtDataDerived), pointer :: rule
      integer :: status
      type(ExtDataTimeSample), pointer :: time_sample
      type(ExtDataTimeSample), target :: default_time_sample

      _UNUSED_DUMMY(unusable)
      rule => this%derived_map%at(trim(item_name))
      derived_item%name = trim(item_name)
      derived_item%expression = rule%expression
      time_sample => this%sample_map%at(rule%sample_key)

      if(.not.associated(time_sample)) then
        call default_time_sample%set_defaults()
        time_sample=>default_time_sample
      end if
      call derived_item%update_freq%create_from_parameters(time_sample%refresh_time, &
           time_sample%refresh_frequency, time_sample%refresh_offset, time, clock, __RC__)
      derived_item%masking=.false.
      if (index(derived_item%expression,"mask") /= 0 ) then
         derived_item%masking=.true.
      end if

      _RETURN(_SUCCESS)
 
   end subroutine fillin_derived

end module MAPL_ExtDataOldTypesCreator
