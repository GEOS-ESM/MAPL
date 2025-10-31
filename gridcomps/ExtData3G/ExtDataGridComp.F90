#include "MAPL.h"

module mapl3g_ExtDataGridComp
   use generic3g
   use mapl_ErrorHandling
   use esmf
   use pfio
   use mapl3g_ExtDataGridComp_private
   use mapl3g_Geom_API
   use MAPL_FieldUtils
   use mapl3g_FieldBundle_API
   use mapl3g_ExtDataConfig
   use mapl3g_PrimaryExportVector
   use mapl3g_PrimaryExport
   use mapl3g_geomio
   use mapl3g_Geom_API
   use mapl3g_AbstractDataSetFileSelector
   use MAPL_FileMetadataUtilsMod
   use gftl2_StringStringMap
   use mapl3g_ExtDataReader
   
   implicit none(type,external)
   private

   public :: setServices

   ! Private state
   character(*), parameter :: PRIVATE_STATE = "ExtData"
   type :: ExtDataGridComp
      type(PrimaryExportVector) :: export_vector
      type(integerVector) :: rules_per_export
      type(integerVector) :: export_id_start
      logical :: has_run_mod_advert = .false.
      type(StringVector) :: active_items
      type(StringIntegerMap) :: last_item
      contains
         procedure :: get_item_index
   end type ExtDataGridComp

contains

   subroutine setServices(gridcomp, rc)
      type(ESMF_GridComp) :: gridcomp
      integer, intent(out) :: rc

      type(ESMF_HConfig) :: hconfig
      integer :: status

      call MAPL_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_INITIALIZE, modify_advertise, phase_name="GENERIC::INIT_MODIFY_ADVERTISED", _RC)
      call MAPL_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_RUN, run, phase_name='run', _RC)

      call MAPL_GridCompGet(gridcomp, hconfig=hconfig, _RC)
      
      call add_var_specs(gridcomp, hconfig, _RC)

      _SET_NAMED_PRIVATE_STATE(gridcomp, ExtDataGridComp, PRIVATE_STATE)

      _RETURN(_SUCCESS)
   end subroutine setServices

   subroutine modify_advertise(gridcomp, importState, exportState, clock, rc)
      type(ESMF_GridComp)   :: gridcomp
      type(ESMF_State)      :: importState
      type(ESMF_State)      :: exportState
      type(ESMF_Clock)      :: clock      
      integer, intent(out)  :: rc

      integer :: status

      integer :: rules_for_item, rule_counter, j, idx
      type(ExtDataConfig) :: config
      type(ESMF_Hconfig) :: hconfig
      type(ESMF_Time) :: current_time
      type(StringVectorIterator) :: iter
      character(len=:), pointer :: item_name
      character(len=ESMF_MAXSTR) :: full_name
      logical :: has_rule
      type(ExtDataGridComp), pointer :: extdata_gridcomp
      type(PrimaryExport) :: primary_export
      type(PrimaryExport), pointer :: primary_export_ptr
      class(logger), pointer :: lgr
      integer, pointer :: last_index
      type(ESMF_TimeInterval) :: time_step

      _GET_NAMED_PRIVATE_STATE(gridcomp, ExtDataGridComp, PRIVATE_STATE, extdata_gridcomp)

      if (extdata_gridcomp%has_run_mod_advert) then
         _RETURN(_SUCCESS)
      end if

      call MAPL_GridCompGet(gridcomp, logger=lgr, _RC)
      call ESMF_ClockGet(clock, currTime=current_time, timeStep=time_step, _RC)
      call MAPL_GridCompGet(gridcomp, hconfig=hconfig, _RC)
      extdata_gridcomp%active_items = get_active_items(exportState, _RC)
      call new_ExtDataConfig_from_yaml(config, hconfig, current_time,  _RC)
      rule_counter = 0
      iter = extdata_gridcomp%active_items%ftn_begin()
      do while (iter /= extdata_gridcomp%active_items%ftn_end())
         call iter%next()
         item_name => iter%of()
         has_rule = config%has_rule_for(item_name, _RC)
         _ASSERT(has_rule, 'no rule for extdata item: '//item_name)
         rules_for_item = config%count_rules_for_item(item_name, _RC)
         call extdata_gridcomp%export_id_start%push_back(rule_counter+1)
         call extdata_gridcomp%rules_per_export%push_back(rules_for_item)
    
         _ASSERT(rules_for_item > 0, 'item: '//item_name//' has no rule')
         do j=1,rules_for_item
            rule_counter = rule_counter + 1
            full_name = item_name
            if (rules_for_item > 1) write(full_name,'(A,A1,I0)')trim(item_name),rule_sep,j
            primary_export = config%make_PrimaryExport(trim(full_name), item_name, time_step, _RC)
            call extdata_gridcomp%export_vector%push_back(primary_export)
         enddo 
         idx = extdata_gridcomp%get_item_index(item_name, current_time, _RC)
         primary_export_ptr => extdata_gridcomp%export_vector%at(idx) 
         call primary_export_ptr%complete_export_spec(item_name, exportState, _RC)
         call extdata_gridcomp%last_item%insert(item_name, idx)
      end do

      call report_active_items(extdata_gridcomp%active_items, lgr)
      extdata_gridcomp%has_run_mod_advert = .true. 

      _RETURN(_SUCCESS)
   end subroutine modify_advertise

   subroutine run(gridcomp, importState, exportState, clock, rc)
      type(ESMF_GridComp)   :: gridcomp
      type(ESMF_State)      :: importState
      type(ESMF_State)      :: exportState
      type(ESMF_Clock)      :: clock      
      integer, intent(out)  :: rc

      integer :: status

      type(ExtDataGridComp), pointer :: extdata_gridcomp
      type(StringVectorIterator) :: iter
      type(PrimaryExport), pointer :: export_item
      type(ESMF_Time) :: current_time
      real :: weights(3)
      character(len=:), allocatable :: export_name
      character(len=:), pointer :: base_name
      type(ExtDataReader) :: reader
      class(logger), pointer :: lgr
      type(ESMF_FieldBundle) :: bundle 
      integer :: idx
      integer, pointer :: last_index

      call MAPL_GridCompGet(gridcomp, logger=lgr, _RC)
      _GET_NAMED_PRIVATE_STATE(gridcomp, ExtDataGridComp, PRIVATE_STATE, extdata_gridcomp)
      call ESMF_ClockGet(clock, currTime=current_time, _RC) 
      call reader%initialize_reader(_RC)
      iter = extdata_gridcomp%active_items%ftn_begin()
      do while (iter /= extdata_gridcomp%active_items%ftn_end())
         call iter%next()
         base_name => iter%of() 
         idx = extdata_gridcomp%get_item_index(base_name, current_time, _RC)
         last_index => extdata_gridcomp%last_item%of(base_name)
         export_item => extdata_gridcomp%export_vector%at(idx) 
         if (last_index /= idx) then
            last_index = idx
            call export_item%update_export_spec(base_name, exportState, _RC)
         end if 
         if (export_item%is_constant) cycle

         export_name = export_item%get_export_var_name()
         call ESMF_StateGet(exportState, export_name, bundle, _RC) 
         call MAPL_FieldBundleSet(bundle, bracket_updated=.false., _RC)
         call export_item%update_my_bracket(bundle, current_time, weights, _RC)
         call set_weights(exportState, export_name, weights, _RC)
         call export_item%append_state_to_reader(exportState, reader, lgr, _RC)
      end do
      call reader%read_items(lgr, _RC)
      call reader%destroy_reader(_RC)

      call handle_fractional_regrid(extdata_gridcomp, current_time, exportState, _RC)

      _RETURN(_SUCCESS)
   end subroutine run

  subroutine handle_fractional_regrid(extdata_internal, current_time, export_state, rc)
      type(ExtDataGridComp), intent(in) :: extdata_internal
      type(ESMF_Time), intent(in) :: current_time
      type(ESMF_State), intent(inout) :: export_state
      integer, optional, intent(out) :: rc

      type(StringVectorIterator) :: iter
      type(PrimaryExport), pointer :: export_item
      character(len=:), allocatable :: export_name
      character(len=:), pointer :: base_name
      type(ESMF_FieldBundle) :: bundle 
      integer :: idx, status

      ! this entire loop is for the handling of the fractional regrid case
      ! has to be done after everything is read
      iter = extdata_internal%active_items%ftn_begin()
      do while (iter /= extdata_internal%active_items%ftn_end())
         call iter%next()
         base_name => iter%of() 
         idx = extdata_internal%get_item_index(base_name, current_time, _RC)
         export_item => extdata_internal%export_vector%at(idx) 
         if (export_item%is_constant .or. (export_item%regridding_method /= 'FRACTION')) cycle
         export_name = export_item%get_export_var_name()
         call ESMF_StateGet(export_state, export_name, bundle, _RC) 
         call export_item%set_fraction_values_to_zero(bundle, _RC)
      enddo
      _RETURN(_SUCCESS)
  end subroutine handle_fractional_regrid

  function get_item_index(this,base_name,current_time,rc) result(item_index)
     integer :: item_index
     class(ExtDataGridComp), intent(in) :: this
     type(ESMF_Time) :: current_time
     character(len=*),intent(in) :: base_name
     integer, optional, intent(out) :: rc

     character(len=:), allocatable :: export_name
     integer :: i
     integer, pointer :: num_rules,i_start
     logical :: found
     type(PrimaryExport), pointer :: item

     found = .false.
     do i=1,this%export_vector%size()
        item => this%export_vector%at(i)
        export_name = item%get_export_var_name()
        if (export_name == base_name) then
           found = .true.
           i_start => this%export_id_start%at(i)
           num_rules => this%rules_per_export%at(i)
           exit
        end if
     enddo
     _ASSERT(found,"ExtData no item with basename '"//TRIM(base_name)//"' found")

     item_index = -1
     if (num_rules == 1) then
        item_index = i_start
     else if (num_rules > 1) then
        do i=1,num_rules
           item => this%export_vector%at(i_start+i-1)
           if (current_time >= item%start_and_end(1) .and. &
               current_time <  item%start_and_end(2)) then
              item_index = i_start + i -1
              exit
           endif
        enddo
     end if
     _ASSERT(item_index/=-1,"ExtData did not find item index for basename "//TRIM(base_name))
     _RETURN(_SUCCESS)
  end function get_item_index

end module mapl3g_ExtDataGridComp

subroutine setServices(gridcomp,rc)
   use ESMF
   use MAPL_ErrorHandlingMod
   use mapl3g_ExtDataGridComp, only: ExtData_setServices => SetServices    
   type(ESMF_GridComp)  :: gridcomp
   integer, intent(out) :: rc

   integer :: status

   call ExtData_setServices(gridcomp,_RC)
   _RETURN(_SUCCESS)

end subroutine

