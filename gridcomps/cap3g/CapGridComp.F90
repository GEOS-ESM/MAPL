module mapl3g_CapGridComp
   use mapl3g_ExtDataGridComp, only: extdata_setservices => setServices
   use mapl3g_HistoryGridComp, only: history_setservices => setServices
   implicit none
   private

   public :: setServices

   type :: CapGridComp
      character(:), allocatable :: extdata_name
      character(:), allocatable :: history_name
   end type CapGridComp

contains
   
   subroutine setServices(gridcomp, rc)
      type(ESMF_GridComp) :: gridcomp
      integer, intent(out) :: rc

      type(CapGridComp), pointer :: cap_gridcomp
      type(ESMF_HConfig) :: hconfig

      ! Set entry points
      call MAPL_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_INITIALIZE, init, phase_name=GENERIC_INIT_USER)
      call MAPL_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_RUN, run, phase_name='run', _RC)

      ! Attach private state
      _SET_NAMED_PRIVATE_STATE(gridcomp, CapGridComp, "CapGridComp", cap_gridcomp)

      call MAPL_AddChild(gridcomp, 'EXTDATA', ExtData_setServices, 'extdata.yaml', _RC)
      call MAPL_AddChild(gridcomp, 'HIST', History_setServices, 'history.yaml', _RC)

      _RETURN(_SUCCESS)
   end subroutine setServices

   subroutine init(gridcomp, importState, exportState, clock, rc)
      type(ESMF_GridComp)   :: gridcomp
      type(ESMF_State)      :: importState
      type(ESMF_State)      :: exportState
      type(ESMF_Clock)      :: clock      
      integer, intent(out)  :: rc

      integer :: status

      ! To Do:
      ! - determine run frequencey and offset (save as alarm)

      !------------------
      ! Connections:
      !------------------
      ! At the cap level, the desire is to use ExtData to complete all unsatisfied
      ! imports from the root gridcomp.  Likewise, we use the root gridcomp to
      ! satisfy all imports for history.
      !------------------
      call MAPL_ConnectAll(gridcomp, src_comp=extdata, dst_comp=root_name, _RC)
      call MAPL_ConnectAll(gridcomp, src_comp=root_name, dst_comp=history, _RC)
      
      
      _RETURN(_SUCCESS)
   end subroutine init


   subroutine run(gridcomp, importState, exportState, clock, rc)
      type(ESMF_GridComp)   :: gridcomp
      type(ESMF_State)      :: importState
      type(ESMF_State)      :: exportState
      type(ESMF_Clock)      :: clock      
      integer, intent(out)  :: rc

      integer :: status

      call MAPL_RunChild(extdata, _RC)
      call MAPL_RunChild(root_name, _RC)
      call MAPL_RunChild(history, phase_name=GENERIC_RUN_UPDATE_GEOM, _RC)
      call MAPL_RunChild(history, phase_name='run', _RC)

      _RETURN(_SUCCESS)
   end subroutine run

end module mapl3g_CapGridComp
