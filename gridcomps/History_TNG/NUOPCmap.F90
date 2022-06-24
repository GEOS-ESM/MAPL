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

   integer, parameter :: num_phases = 2
   character(len=*), dimension(num_phases), parameter :: &
         phase_label_list = [&
               "IPDv05p1", &
               "IPDv05p4"  &
         ]

   type :: NUOPCmap
      type(IntegerStringMap) :: NUOPC_map
   contains
      procedure :: add_phase
      procedure :: get_phase
      procedure :: search_phase_map
      procedure :: read_phase
      procedure :: create_phase_map
   end type NUOPCmap

contains
   subroutine add_phase(this, index, phase_label, rc)
      class(NUOPCmap),   intent(inout) :: this
      integer,           intent(in   ) :: index
      character(len=*),  intent(in   ) :: phase_label
      integer, optional, intent(  out) :: rc

      integer :: status

      if (this%NUOPC_map%count(index) > 0) then
         status = ESMF_RC_OBJ_BAD
      else
         call this%NUOPC_map%insert(index, phase_label)
         status = ESMF_SUCCESS
      end if

      if (present(rc)) rc = status
   end subroutine add_phase

   subroutine get_phase(this, index, phase_label, rc)
      class(NUOPCmap),           intent(inout) :: this
      integer,                   intent(in   ) :: index
      character(len=:), pointer, intent(  out) :: phase_label
      integer, optional,         intent(  out) :: rc

      integer :: status

      status = ESMF_SUCCESS

      phase_label => this%NUOPC_map%at(index)
      if (.not. associated(phase_label)) then
         status = ESMF_RC_OBJ_BAD
      end if

      if (present(rc)) rc = status
   end subroutine get_phase

   subroutine search_phase_map(this, gc, phase_label, phase_index, rc)
      class(NUOPCmap),     intent(inout) :: this
      type(ESMF_GridComp), intent(inout) :: gc
      character(*),        intent(in   ) :: phase_label
      integer,             intent(  out) :: phase_index
      integer, optional,   intent(  out) :: rc

      integer :: status

      call NUOPC_CompSearchPhaseMap(gc, ESMF_METHOD_INITIALIZE, &
         phaseLabel=phase_label, phaseIndex=phase_index, rc=status)
      !VERIFY_NUOPC_(status)
      rc=status

      _RETURN(_SUCCESS)
   end subroutine search_phase_map

   subroutine read_phase(this, gc, phase_label, rc)
      class(NUOPCmap),     intent(inout) :: this
      type(ESMF_GridComp), intent(inout) :: gc
      character(*),        intent(in   ) :: phase_label
      integer, optional,   intent(  out) :: rc

      integer :: status, phase_index

      call this%search_phase_map(gc, phase_label, phase_index, rc=status)
      !VERIFY_NUOPC_(status)

      if (phase_index >= 0) then
         call this%add_phase(phase_index, phase_label, rc=status)
         !VERIFY_NUOPC_(status)
      else
         status = ESMF_RC_OBJ_BAD
      end if

      if (present(rc)) rc = status
   end subroutine read_phase

   subroutine create_phase_map(this, gc, rc)
      class(NUOPCmap),                 intent(inout) :: this
      type(ESMF_GridComp),             intent(inout) :: gc
      integer, optional,               intent(  out) :: rc

      integer :: status, i

      do i=1, num_phases
         call this%read_phase(gc, phase_label_list(i), __RC__)
      end do

      _RETURN(_SUCCESS)
   end subroutine create_phase_map
end module NUOPCmapMod
