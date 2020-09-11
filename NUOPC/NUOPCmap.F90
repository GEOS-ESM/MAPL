#include "MAPL_Generic.h"
#include "NUOPC_ErrLog.h"

module NUOPCmapMod
   use ESMF
   use NUOPC
   use MAPL_Mod
   use gFTL_IntegerStringMap

   implicit none
   private

   public num_phases
   public phase_label_list

   public NUOPCmap

   integer, parameter :: num_phases = 6
   character(len=*), dimension(num_phases), parameter :: &
         phase_label_list = [&
               "IPDv05p1", &
               "IPDv05p2", &
               "IPDv05p3", &
               "IPDv05p4", &
               "IPDv05p5", &
               "IPDv05p6" &
         ]

   type :: NUOPCmap
      type(IntegerStringMap) :: NUOPC_map
   contains
      procedure :: get_phase
      procedure :: add_phase
      procedure :: read_phase
      procedure :: create_phase_map
   end type NUOPCmap

contains
   subroutine get_phase(this, index, phase_label, rc)
      class(NUOPCmap),           intent(inout) :: this
      integer,                   intent(in   ) :: index
      character(len=:), pointer, intent(  out) :: phase_label
      integer,                   intent(  out) :: rc

      rc = ESMF_SUCCESS

      phase_label => this%NUOPC_map%at(index)
      if (.not. associated(phase_label)) then
         rc = ESMF_RC_OBJ_BAD
         return
      end if
   end subroutine get_phase

   subroutine add_phase(this, index, phase_label, rc)
      class(NUOPCmap),  intent(inout) :: this
      integer,          intent(in   ) :: index
      character(len=*), intent(in   ) :: phase_label
      integer,          intent(  out) :: rc

      rc = ESMF_SUCCESS

      if (this%NUOPC_map%count(index) > 0) then
         rc = ESMF_RC_OBJ_BAD
         return
      else
         call this%NUOPC_map%insert(index, phase_label)
      end if
   end subroutine add_phase

   subroutine read_phase(this, gc, index, rc)
      class(NUOPCmap),     intent(inout) :: this
      type(ESMF_GridComp), intent(inout) :: gc
      integer,             intent(in   ) :: index
      integer,             intent(  out) :: rc

      integer :: phase_index

      rc = ESMF_SUCCESS

      call NUOPC_CompSearchPhaseMap(gc, ESMF_METHOD_INITIALIZE, &
            phaseLabel=phase_label_list(index), phaseIndex=phase_index, rc=rc)
      VERIFY_NUOPC_(rc)

      if (phase_index >= 0) then
         call this%add_phase(phase_index, phase_label_list(index), rc=rc)
         VERIFY_NUOPC_(rc)
      else
         rc = ESMF_RC_OBJ_BAD
         return
      end if
   end subroutine read_phase

   subroutine create_phase_map(this, gc, rc)
      class(NUOPCmap),     intent(inout) :: this
      type(ESMF_GridComp), intent(inout) :: gc
      integer,             intent(  out) :: rc

      integer :: i

      rc = ESMF_SUCCESS

      do i=1, num_phases
         call this%read_phase(gc, i, rc)
         VERIFY_NUOPC_(rc)
      end do
   end subroutine create_phase_map
end module NUOPCmapMod
