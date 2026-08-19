#include "MAPL_Exceptions.h"
#include "MAPL_ErrLog.h"

module MAPL_ExtDataOldTypesCreator
   use ESMF
   use MAPL_BaseMod
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
   use MAPL_ExtDataConstants
   use MAPL_StateUtils
   implicit none

   public :: ExtDataOldTypesCreator
   public :: new_ExtDataOldTypesCreator

   type, extends(ExtDataConfig) :: ExtDataOldTypesCreator
      private
      logical, public :: validate_file_ranges = .false.
      character(len=:), public, allocatable :: print_required_files
      type(ESMF_HConfig), public :: required_files_hconfig  ! sequence of per-dataset entries
      logical, public :: required_files_hconfig_initialized = .false.
      contains
         procedure :: fillin_primary
         procedure :: fillin_derived
   end type ExtDataOldTypesCreator

!#   interface ExtDataOldTypesCreator
!#      module procedure :: new_ExtDataOldTypesCreator
!#   end interface

   contains

      subroutine new_ExtDataOldTypesCreator(extdataobj, config_file,current_time,unusable,rc )
         type(ExtDataOldTypesCreator), target, intent(out) :: ExtDataObj
         character(len=*), intent(in) :: config_file
         type(ESMF_Time), intent(in) :: current_time
         class(KeywordEnforcer), optional, intent(in) :: unusable
         integer, optional, intent(out) :: rc


         integer :: status
         
         call ExtDataObj%ExtDataConfig%new_ExtDataConfig_from_yaml(config_file,current_time,_RC)
         
         _RETURN(_SUCCESS)
         _UNUSED_DUMMY(unusable)
      end subroutine new_ExtDataOldTypesCreator


   subroutine fillin_primary(this,item_name,base_name,primary_item,time,clock,run_range,unusable,time_range,rc)
      class(ExtDataOldTypesCreator), target, intent(inout) :: this
      character(len=*), intent(in) :: item_name
      character(len=*), intent(in) :: base_name
      type(PrimaryExport), intent(inout) :: primary_item
      type(ESMF_Time), intent(inout) :: time
      type(ESMF_Clock), intent(inout) :: clock
      type(ESMF_Time), intent(in) :: run_range(2)
      class(KeywordEnforcer), optional, intent(in) :: unusable
      type(ESMF_Time), optional, intent(in) :: time_range(2)
      integer, optional, intent(out) :: rc

      type(ExtDataRule), pointer :: rule
      type(ExtDataFileStream),  pointer :: dataset
      type(ExtDataTimeSample), pointer :: time_sample
      type(ExtDataTimeSample), target :: default_time_sample
      type(ExtDataSimpleFileHandler) :: simple_handler
      type(ExtDataClimFileHandler) :: clim_handler
      integer :: status, semi_pos
      logical :: disable_interpolation, get_range, exact, user_set_range
      type(ESMF_HConfig) :: entry_hconfig
      type(ESMF_Time) :: effective_run(2)

      _UNUSED_DUMMY(unusable)
      rule => this%rule_map%at(trim(item_name))

      if(rule%sample_key==sample_not_provided) then
         call default_time_sample%set_defaults()
         time_sample=>default_time_sample
      else
         time_sample => this%sample_map%at(rule%sample_key)
         _ASSERT(ASSOCIATED(time_sample), 'sample_key provided for '//trim(item_name)//" does not exist")
      end if
      primary_item%vartype = MAPL_FieldItem
      if (allocated(rule%vector_partner)) primary_item%vartype = MAPL_VectorField
      primary_item%name = trim(base_name)
      if (primary_item%vartype == MAPL_VectorField) then
         primary_item%vcomp1 = trim(base_name)
         primary_item%vcomp2 = trim(rule%vector_partner)
         primary_item%var = rule%file_var
         primary_item%fcomp1 = rule%file_var
         primary_item%fcomp2 = rule%vector_file_partner
         primary_item%fileVars%itemType = ItemTypeVector
         primary_item%fileVars%xname  = trim(rule%file_var)
         primary_item%fileVars%yname  = trim(rule%vector_file_partner)
      else
         primary_item%vcomp1 = trim(base_name)
         primary_item%var = rule%file_var
         primary_item%fcomp1 = rule%file_var
         primary_item%fileVars%itemType = ItemTypeScalar
         primary_item%fileVars%xname  = trim(rule%file_var)
      end if

      ! regrid method
      if (index(rule%regrid_method,"FRACTION;")>0) then
         semi_pos = index(rule%regrid_method,";")
         read(rule%regrid_method(semi_pos+1:),*) primary_item%fracVal
         primary_item%trans = REGRID_METHOD_FRACTION
      else
         primary_item%trans = regrid_method_string_to_int(rule%regrid_method)
      end if
      _ASSERT(primary_item%trans/=UNSPECIFIED_REGRID_METHOD,"improper regrid method chosen")

      if (trim(time_sample%extrap_outside) =="clim") then
         primary_item%cycling=.true.
      else if (trim(time_sample%extrap_outside) == "persist_closest") then
         primary_item%persist_closest=.true.
         primary_item%cycling=.false.
      else if (trim(time_sample%extrap_outside) == "none") then
         primary_item%cycling=.false.
         primary_item%persist_closest=.false.
      end if

      allocate(primary_item%source_time,source=time_sample%source_time)
      ! new refresh
      call primary_item%update_freq%create_from_parameters(time_sample%refresh_time, &
           time_sample%refresh_frequency, time_sample%refresh_offset, time, clock, _RC)

      disable_interpolation =  .not.time_sample%time_interpolation
      exact = time_sample%exact

      call primary_item%modelGridFields%comp1%set_parameters(linear_trans=rule%linear_trans,disable_interpolation=disable_interpolation,exact=exact)
      call primary_item%modelGridFields%comp2%set_parameters(linear_trans=rule%linear_trans,disable_interpolation=disable_interpolation,exact=exact)

      ! file_template
      primary_item%isConst = .false.
      if (index(rule%collection,"/dev/null")==0) then

         if ( ASSOCIATED(this%file_stream_map%at(trim(rule%collection))) ) then
           dataset => this%file_stream_map%at(trim(rule%collection))
         else
           _FAIL("ExtData problem with collection "//TRIM(rule%collection))
         end if

          primary_item%file_template = dataset%file_template
          get_range = trim(time_sample%extrap_outside) /= "none"
          user_set_range = allocated(dataset%valid_range)
          call dataset%detect_metadata(primary_item%file_metadata,time,rule%multi_rule,get_range=get_range,_RC)

          ! Clip run_range to this rule's active window [time_range(1), time_range(2)).
          ! For multi-rule exports this prevents validating a rule against dates when a
          ! different rule is active.  Single-rule calls omit time_range so effective_run
          ! falls back to run_range unchanged.
          if (present(time_range)) then
             if (run_range(1) > time_range(1)) then
                effective_run(1) = run_range(1)
             else
                effective_run(1) = time_range(1)
             end if
             if (run_range(2) < time_range(2)) then
                effective_run(2) = run_range(2)
             else
                effective_run(2) = time_range(2)
             end if
          else
             effective_run = run_range
          end if

          ! Skip both blocks if this rule's active window does not overlap the run.
          if (effective_run(1) < effective_run(2)) then

          if (this%validate_file_ranges .and. &
               index(dataset%file_template,'%') /= 0) then
             if (trim(time_sample%extrap_outside) == "none" .or. user_set_range) then
                call dataset%check_data_availability(effective_run, time_sample%extrap_outside, _RC)
             end if
          end if

          ! Accumulate manifest entry if print_required_files is enabled.
          if (allocated(this%print_required_files) .and. &
               index(dataset%file_template,'%') /= 0 .and. &
               (trim(time_sample%extrap_outside) == "none" .or. user_set_range)) then
             if (.not. this%required_files_hconfig_initialized) then
                this%required_files_hconfig = ESMF_HConfigCreate(content='{}', _RC)
                this%required_files_hconfig_initialized = .true.
             end if
             call dataset%get_required_files_hconfig(effective_run, time_sample%extrap_outside, entry_hconfig, _RC)
             ! Key = base_name. For multi-rule items the key already exists as a
             ! sequence; for single-rule items it does not exist yet.
             if (ESMF_HConfigIsDefined(this%required_files_hconfig, keyString=trim(base_name))) then
                ! Key exists — this is a subsequent rule; append to the sequence.
                ! If the existing value is a map (first rule was single), convert it
                ! to a sequence first.
                block
                   type(ESMF_HConfig) :: existing, new_seq, first_entry
                   existing = ESMF_HConfigCreateAt(this%required_files_hconfig, keyString=trim(base_name), _RC)
                   if (ESMF_HConfigIsMap(existing)) then
                      ! First rule was stored as a plain map — wrap it in a sequence.
                      new_seq = ESMF_HConfigCreate(content='[]', _RC)
                      first_entry = ESMF_HConfigCreate(existing, _RC)
                      call ESMF_HConfigAdd(new_seq, content=first_entry, _RC)
                      call ESMF_HConfigAdd(new_seq, content=entry_hconfig, _RC)
                      call ESMF_HConfigAdd(this%required_files_hconfig, content=new_seq, addKeyString=trim(base_name), _RC)
                      call ESMF_HConfigDestroy(new_seq, _RC)
                      call ESMF_HConfigDestroy(first_entry, _RC)
                   else
                      ! Already a sequence — just append.
                      call ESMF_HConfigAdd(existing, content=entry_hconfig, _RC)
                   end if
                   call ESMF_HConfigDestroy(existing, _RC)
                end block
             else
                ! First rule for this export — store as a plain map.
                call ESMF_HConfigAdd(this%required_files_hconfig, content=entry_hconfig, addKeyString=trim(base_name), _RC)
             end if
             call ESMF_HConfigDestroy(entry_hconfig, _RC)
          end if

          end if ! effective_run(1) < effective_run(2)
      else
         primary_item%file_template = rule%collection
      end if

      if (index(rule%collection,'/dev/null') /= 0) then
         primary_item%isConst = .true.
         primary_item%const=rule%linear_trans(1)
      else
         if (primary_item%cycling) then
            call clim_handler%initialize(dataset,_RC)
            allocate(primary_item%filestream,source=clim_handler)
         else
            call simple_handler%initialize(dataset,persist_closest=primary_item%persist_closest,_RC)
            allocate(primary_item%filestream,source=simple_handler)
         end if
      end if

      primary_item%fail_on_missing_file = rule%fail_on_missing_file
      primary_item%enable_vertical_regrid= rule%enable_vertical_regrid

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
      if (allocated(rule%sample_key)) then
         time_sample => this%sample_map%at(rule%sample_key)
      else
        call default_time_sample%set_defaults()
        time_sample=>default_time_sample
      end if
      call derived_item%update_freq%create_from_parameters(time_sample%refresh_time, &
           time_sample%refresh_frequency, time_sample%refresh_offset, time, clock, _RC)
      derived_item%masking=.false.
      if (index(derived_item%expression,"mask") /= 0 ) then
         derived_item%masking=.true.
         allocate(derived_item%mask_definition)
         derived_item%mask_definition = StateMask(derived_item%expression,_RC)
      end if

      _RETURN(_SUCCESS)

   end subroutine fillin_derived

end module MAPL_ExtDataOldTypesCreator
