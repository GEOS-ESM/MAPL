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
      
      ! ESMF has a bug, for now we will not merge hconfig until fixed
      !merged_configs = ESMF_HConfigCreate(_RC)
      ! instead pass hconfig and this will have to traverse the subconfigs for now
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

      integer :: rules_for_item
      type(StringVector) :: active_items
      type(ExtDataConfig) :: config
      type(ESMF_Hconfig) :: hconfig
      type(ESMF_Time) :: current_time
      type(StringVectorIterator) :: iter
      character(len=:), pointer :: item_name
      logical :: has_rule
      type(ExtDataGridComp), pointer :: extdata_gridcomp
      type(PrimaryExport) :: primary_export
      class(logger), pointer :: lgr

      _GET_NAMED_PRIVATE_STATE(gridcomp, ExtDataGridComp, PRIVATE_STATE, extdata_gridcomp)

      call MAPL_GridCompGet(gridcomp, logger=lgr, _RC)
      call ESMF_ClockGet(clock, currTime=current_time, _RC)
      call MAPL_GridCompGet(gridcomp, hconfig=hconfig, _RC)
      active_items = get_active_items(exportState, _RC)
      call new_ExtDataConfig_from_yaml(config, hconfig, current_time,  _RC)
      iter = active_items%ftn_begin()
      do while (iter /= active_items%ftn_end())
         call iter%next()
         item_name => iter%of()
         has_rule = config%has_rule_for(item_name, _RC)
         _ASSERT(has_rule, 'no rule for extdata item: '//item_name)
         rules_for_item = config%count_rules_for_item(item_name, _RC)
         _ASSERT(rules_for_item == 1, 'only 1 rule per item supported now')
         primary_export = config%make_PrimaryExport(item_name, _RC)
         call primary_export%complete_export_spec(item_name, exportState, _RC)
         call extdata_gridcomp%export_vector%push_back(primary_export)
      end do

      call report_active_items(extdata_gridcomp%export_vector, lgr)
      
      _RETURN(_SUCCESS)
   end subroutine modify_advertise

   ! this is just to do something now. Obviously this is not how it will look...
   subroutine run(gridcomp, importState, exportState, clock, rc)
      type(ESMF_GridComp)   :: gridcomp
      type(ESMF_State)      :: importState
      type(ESMF_State)      :: exportState
      type(ESMF_Clock)      :: clock      
      integer, intent(out)  :: rc

      integer :: status

      type(ExtDataGridComp), pointer :: extdata_gridcomp
      type(PrimaryExportVectorIterator) :: iter
      type(PrimaryExport), pointer :: export_item
      type(ESMF_Time) :: current_time
      real :: weights(3)
      character(len=:), allocatable :: export_name
      type(ExtDataReader) :: reader
      class(logger), pointer :: lgr
      type(ESMF_FieldBundle) :: bundle 

      call MAPL_GridCompGet(gridcomp, logger=lgr, _RC)
      _GET_NAMED_PRIVATE_STATE(gridcomp, ExtDataGridComp, PRIVATE_STATE, extdata_gridcomp)
      call ESMF_ClockGet(clock, currTime=current_time, _RC) 
      call reader%initialize_reader(_RC)
      iter = extdata_gridcomp%export_vector%ftn_begin()
      do while (iter /= extdata_gridcomp%export_vector%ftn_end())
         call iter%next()
         export_item => iter%of() 
         export_name = export_item%get_export_var_name()
         call ESMF_StateGet(exportState, export_name, bundle, _RC) 
         call export_item%update_my_bracket(bundle, current_time, weights, _RC)
         call set_weights(exportState, export_name, weights, _RC)
         call export_item%append_reader(exportState, reader, _RC)
      end do
      call reader%read_items(lgr, _RC)
      call reader%destroy_reader(_RC)

      _RETURN(_SUCCESS)
   end subroutine run

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

