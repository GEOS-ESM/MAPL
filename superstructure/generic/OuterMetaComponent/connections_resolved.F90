#include "MAPL.h"

! Gate for graphbuilder_resolve_connections idempotency. Replaces former
! graph%is_frozen() check which became invalid when freeze moved to whole-hierarchy level.
submodule (mapl_OuterMetaComponent_mod) connections_resolved_smod
   use mapl_NodeIdSet_mod, only: NodeIdSetIterator, operator(/=)
   implicit none(type, external)

contains

   module function connections_already_resolved(this) result(resolved)
      logical :: resolved
      class(OuterMetaComponent), intent(in) :: this

      resolved = this%connections_resolved
   end function connections_already_resolved

   module subroutine mark_connections_resolved(this)
      class(OuterMetaComponent), intent(inout) :: this

      this%connections_resolved = .true.
   end subroutine mark_connections_resolved

   module function is_import_settled(this, import_node_id) result(settled)
      logical :: settled
      class(OuterMetaComponent), intent(in) :: this
      type(NodeId), intent(in) :: import_node_id

      settled = this%settled_imports%count(import_node_id) > 0
   end function is_import_settled

   module subroutine note_import_settled(this, import_node_id)
      class(OuterMetaComponent), intent(inout) :: this
      type(NodeId), intent(in) :: import_node_id

      call this%pending_settled_imports%insert(import_node_id)
   end subroutine note_import_settled

   module subroutine note_connection_waiting(this, import_node_id)
      class(OuterMetaComponent), intent(inout) :: this
      type(NodeId), intent(in) :: import_node_id

      call this%waiting_imports%insert(import_node_id)
      this%connections_waiting = .true.
   end subroutine note_connection_waiting

   module function has_waiting_connections(this) result(waiting)
      logical :: waiting
      class(OuterMetaComponent), intent(in) :: this

      waiting = this%connections_waiting
   end function has_waiting_connections

   ! Not cleared per pass: a settled import is never resolved again.
   module subroutine note_unsupported_pair(this, description)
      class(OuterMetaComponent), intent(inout) :: this
      character(*), intent(in) :: description

      call this%unsupported_pairs%push_back(description)
   end subroutine note_unsupported_pair

   module function get_unsupported_pairs(this) result(pairs)
      type(StringVector) :: pairs
      class(OuterMetaComponent), intent(in) :: this

      pairs = this%unsupported_pairs
   end function get_unsupported_pairs

   module subroutine begin_connection_pass(this)
      class(OuterMetaComponent), intent(inout) :: this

      call this%pending_settled_imports%clear()
      call this%waiting_imports%clear()
      this%connections_waiting = .false.
   end subroutine begin_connection_pass

   module subroutine end_connection_pass(this)
      class(OuterMetaComponent), target, intent(inout) :: this

      type(NodeIdSetIterator) :: iter

      associate (e => this%pending_settled_imports%ftn_end())
         iter = this%pending_settled_imports%ftn_begin()
         do while (iter /= e)
            call iter%next()
            if (this%waiting_imports%count(iter%of()) > 0) cycle
            call this%settled_imports%insert(iter%of())
         end do
      end associate
      call this%pending_settled_imports%clear()
      call this%waiting_imports%clear()
      if (.not. this%connections_waiting) this%connections_resolved = .true.
   end subroutine end_connection_pass

end submodule connections_resolved_smod
