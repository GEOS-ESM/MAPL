#include "MAPL_Generic.h"
module mapl3g_CapGridComp
   use :: generic3g, only: MAPL_GridCompSetEntryPoint
   use :: generic3g, only: MAPL_ResourceGet
   use :: generic3g, only: MAPL_ConnectAll
   use :: generic3g, only: MAPL_GridCompGet
   use :: generic3g, only: GriddedComponentDriver
   use :: generic3g, only: MAPL_run_child
   use :: generic3g, only: MAPL_UserCompGetInternalState
   use :: generic3g, only: MAPL_UserCompSetInternalState
   use :: generic3g, only: GENERIC_INIT_USER
   use :: mapl_ErrorHandling
   use :: esmf, only: ESMF_GridComp
   use :: esmf, only: ESMF_Config
   use :: esmf, only: ESMF_HConfig
   use :: esmf, only: ESMF_State
   use :: esmf, only: ESMF_Clock
   use :: esmf, only: ESMF_METHOD_INITIALIZE
   use :: esmf, only: ESMF_METHOD_RUN
   use :: esmf, only: ESMF_SUCCESS
   implicit none

   private

   public :: setServices

   type :: CapGridComp
      character(:), allocatable :: extdata_name
      character(:), allocatable :: history_name
      character(:), allocatable :: root_name
   end type CapGridComp

   character(*), parameter :: PRIVATE_STATE = 'CapGridComp'
    
contains
   
   subroutine setServices(gridcomp, rc)
      type(ESMF_GridComp) :: gridcomp
      integer, intent(out) :: rc

      integer :: status
      type(CapGridComp), pointer :: cap
      type(ESMF_HConfig) :: hconfig
      character(:), allocatable :: extdata, history

      ! Set entry points
      call MAPL_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_INITIALIZE, init, phase_name='GENERIC::INIT_USER', _RC)
      call MAPL_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_RUN, run, phase_name='run', _RC)

      ! Attach private state
      _SET_NAMED_PRIVATE_STATE(gridcomp, CapGridComp, PRIVATE_STATE, cap)

      ! Get Names of children

      call MAPL_GridCompGet(gridcomp, hconfig, _RC)
      call MAPL_ResourceGet(hconfig, keystring='extdata_name', value=cap%extdata_name, default='EXTDATA', _RC)
      call MAPL_ResourceGet(hconfig, keystring='history_name', value=cap%history_name, default='HIST', _RC)
      call MAPL_ResourceGet(hconfig, keystring='root_name', value=cap%root_name, _RC)

      _RETURN(_SUCCESS)
   end subroutine setServices

   subroutine init(gridcomp, importState, exportState, clock, rc)
      type(ESMF_GridComp)   :: gridcomp
      type(ESMF_State)      :: importState
      type(ESMF_State)      :: exportState
      type(ESMF_Clock)      :: clock      
      integer, intent(out)  :: rc

      integer :: status
      type(CapGridComp), pointer :: cap

      ! To Do:
      ! - determine run frequencey and offset (save as alarm)


  _GET_NAMED_PRIVATE_STATE(gridcomp, CapGridComp, PRIVATE_STATE, cap)

      !------------------
      ! Connections:
      !------------------
      ! At the cap level, the desire is to use ExtData to complete all unsatisfied
      ! imports from the root gridcomp.  Likewise, we use the root gridcomp to
      ! satisfy all imports for history.
      !------------------
      call MAPL_ConnectAll(gridcomp, src_comp=cap%extdata_name, dst_comp=cap%root_name, _RC)
      call MAPL_ConnectAll(gridcomp, src_comp=cap%root_name, dst_comp=cap%history_name, _RC)
      
      _RETURN(_SUCCESS)
   end subroutine init


   subroutine run(gridcomp, importState, exportState, clock, rc)
      type(ESMF_GridComp)   :: gridcomp
      type(ESMF_State)      :: importState
      type(ESMF_State)      :: exportState
      type(ESMF_Clock)      :: clock      
      integer, intent(out)  :: rc

      integer :: status
      type(CapGridComp), pointer :: cap

      _GET_NAMED_PRIVATE_STATE(gridcomp, CapGridComp, PRIVATE_STATE, cap)

      call MAPL_run_child(gridcomp, cap%extdata_name, _RC)
      call MAPL_run_child(gridcomp, cap%root_name, _RC)
      call MAPL_run_child(gridcomp, cap%history_name, phase_name='run', _RC)

      _RETURN(_SUCCESS)
   end subroutine run

end module mapl3g_CapGridComp
