#include "MAPL_Generic.h"
module mapl3g_CapGridComp
   use :: generic3g
   use :: mapl_ErrorHandling 
   implicit none

   private

   public :: setServices

   type :: CapGridComp
      character(:), allocatable :: extdata_name
      character(:), allocatable :: history_name
      character(:), allocatable :: root_name
      logical :: run_extdata
      logical :: run_history
   end type CapGridComp

   character(*), parameter :: PRIVATE_STATE = 'CapGridComp'
    
contains
   
   subroutine setServices(gridcomp, rc)
      type(ESMF_GridComp) :: gridcomp
      integer, intent(out) :: rc

      integer :: status
      type(CapGridComp), pointer :: cap
      character(:), allocatable :: extdata, history
      type(OuterMetaComponent), pointer :: outer_meta

      ! Set entry points
      call MAPL_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_INITIALIZE, init, phase_name='GENERIC::INIT_USER', _RC)
      call MAPL_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_RUN, run, phase_name='run', _RC)

      ! Attach private state
      _SET_NAMED_PRIVATE_STATE(gridcomp, CapGridComp, PRIVATE_STATE)
      _GET_NAMED_PRIVATE_STATE(gridcomp, CapGridComp, PRIVATE_STATE, cap)

      ! Disable extdata or history
      call MAPL_GridCompGetResource(gridcomp, keystring='run_extdata', value=cap%run_extdata, default=.true., _RC)
      call MAPL_GridCompGetResource(gridcomp, keystring='run_history', value=cap%run_history, default=.true., _RC)

      ! Get Names of children
      call MAPL_GridCompGetResource(gridcomp, keystring='extdata_name', value=cap%extdata_name, default='EXTDATA', _RC)
      call MAPL_GridCompGetResource(gridcomp, keystring='root_name', value=cap%root_name, _RC)
      call MAPL_GridCompGetResource(gridcomp, keystring='history_name', value=cap%history_name, default='HIST', _RC)

      if (cap%run_extdata) then 
         call MAPL_GridCompConnectAll(gridcomp, src_comp=cap%extdata_name, dst_comp=cap%root_name, _RC)
      end if
      if (cap%run_history) then
         call MAPL_GridCompConnectAll(gridcomp, src_comp=cap%root_name, dst_comp=cap%history_name, _RC)
      end if
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

  _GET_NAMED_PRIVATE_STATE(gridcomp, CapGridComp, PRIVATE_STATE, cap)

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

      if (cap%run_extdata) then
         call MAPL_GridCompRunChild(gridcomp, cap%extdata_name, _RC)
      end if
      call MAPL_GridCompRunChild(gridcomp, cap%root_name, _RC)
      if (cap%run_history) then
         call MAPL_GridCompRunChild(gridcomp, cap%history_name, phase_name='run', _RC)
      end if

      _RETURN(_SUCCESS)
   end subroutine run

end module mapl3g_CapGridComp
