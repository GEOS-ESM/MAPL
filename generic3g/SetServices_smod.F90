#include "MAPL_ErrLog.h"

submodule (mapl3g_OuterMetaComponent) SetServices_smod
   use esmf, only: ESMF_GridComp
   use esmf, only: ESMF_GridCompCreate
   use esmf, only: ESMF_GridCompSetEntryPoint
   use esmf, only: ESMF_METHOD_INITIALIZE
   use esmf, only: ESMF_METHOD_RUN
   use esmf, only: ESMF_METHOD_FINALIZE
   use esmf, only: ESMF_METHOD_READRESTART
   use esmf, only: ESMF_METHOD_WRITERESTART
   use esmf, only: ESMF_SUCCESS
   use gFTL2_shared, only: StringIntegerMap, StringIntegerMapIterator
   implicit none

contains

   module subroutine SetServices(gc, rc)
      type(ESMF_GridComp) :: gc
      integer, intent(out) :: rc

      integer :: status
      type(MetaComp) :: meta
      
      meta => get_meta(gc, _RC)
      call before(meta, _RC)

      if (meta%has_yaml_config()) then
         associate(config => meta%get_yaml_config())
           call meta%set_component_spec(build_component_spec(config, _RC))
         end associate
      end if

      
      user_gc = create_user_gridcomp(meta, _RC)
      call meta%run_user_setservices(user_gc, _RC)
      

      call set_entry_points(gc, phases, _RC)

      call <messy stuff>

      ...
      
      _RETURN(ESMF_SUCCESS)

   end module subroutine

   
   ! This procedure sets the gridcomp entry points for the "outer" GC.
   ! I.e., these are the "generic" wrappers around user gridcomp methods.
   subroutine set_entry_points(gc, user_methods, unusable, rc)
      type(ESMF_GridComp), intent(inout) :: gc
      type(UserMethods), intent(in) :: user_methods
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status

      call set_fixed_entry_points(gc, _RC)
      call set_run_entry_points(gc, user_methods%get_run_phases(), _RC)

      _RETURN(ESMF_SUCCESS)
      _UNUSED_DUMMY(unusable)
   contains

      subroutine set_fixed_entry_points(gc, rc)
         type(ESMF_GridComp), intent(inout) :: gc
         integer, intent(out) :: rc
         integer :: status
         call ESMF_GridCompSetEntryPoint(gc, ESMF_METHOD_INITIALIZE, initialize, _RC)
         call ESMF_GridCompSetEntryPoint(gc, ESMF_METHOD_FINALIZE, finalize, _RC)
!!$      call ESMF_GridCompSetEntryPoint(gc, ESMF_METHOD_READRESTART, read_restart, _RC)
!!$      call ESMF_GridCompSetEntryPoint(gc, ESMF_METHOD_WRITERESTART, write_restart, _RC)

         _RETURN(ESMF_SUCCESS
      end subroutine set_fixed_entry_points
      

      ! NOTE: MAPL supports multiple phases for run().
      subroutine set_run_entry_points(gc, run_phases, rc)
         type(ESMF_GridComp), intent(inout) :: gc
         type(StringIntegerMap), target, intent(in) :: run_phases
         integer, intent(out) :: rc
         
         type(StringIntegerMapIterator) :: iter
         integer :: phase_idx

         associate(b => phases%begin(), e => phases%end())
             
           iter = b
           do while (iter /= e)
              phase_idx => iter%second()
              call ESMF_GridCompSetEntryPoint(gc, ESMF_METHOD_RUN, run, phase=phase_idx, _RC)
              call iter%next()
           end do
             
         end associate

         _RETURN(ESMF_SUCCESS
      end subroutine set_run_entry_points

   end subroutine set_entry_points


   ! This should move to a separate module.
   function build_component_spec(config, rc) result(component_spec)
      type(ComponentSpec) :: component_spec

      component_spec%setservices_spec = process_setservices_spec(config%of('setservices'), _RC)
      component_spec%states_spec = process_states_spec(config%of('states'), _RC)
      component_spec%connections_spec = process_connections_spec(config%of('connections'), _RC)
      component_spec%children_spec = process_children_spec(config%of('children'), _RC)
      component_spec%grid_spec = process_grid_spec(config%of('grid', _RC)
      component_spec%services_spec = process_grid_spec(config%of('serviceservices', _RC)

   end function build_component_spec

end submodule SetServices
