#include "MAPL.h"

!------------------------------------------------------------------------------
! mapl_StateItemCharacteristicDetection_mod: REQ-CHAR-009's detection
! algorithm, operating on GraphStateItem.characteristics (graph/state-item)
! directly - a new, standalone algorithm, not a rewire of
! graph/extension-reuse's existing mapl_ExtensionResolution_mod
! find_mismatched_characteristics (which continues to operate on its own,
! deliberately independent Characteristic/CharacteristicMap, built from
! VariableSpec - openspec/changes/state-item-characteristics design.md
! Decision D6). Shaped similarly to that module's own function by
! deliberate convergent design (same problem, same natural algorithm), not
! by reuse - neither module uses the other.
!
! REQ-CHAR-009: iterates every characteristics-map entry on both items
! through the common StateItemCharacteristic interface only - never
! branches on Value-vs-Reference kind. A kind present on only one side is
! reported as mismatched too (nothing to compare against, so no match can
! be confirmed) - a slightly broader contract than graph/extension-reuse's
! own find_mismatched_characteristics (which only compares kinds present
! on both sides), since this module has no equivalent of that module's own
! "GraphBuilder's own advertised-item checks are a separate concern" scope
! boundary to lean on.
!------------------------------------------------------------------------------
module mapl_StateItemCharacteristicDetection_mod
   use mapl_StateItemCharacteristic_mod, only: StateItemCharacteristic, StateItemCharacteristicMap, &
                                                StateItemCharacteristicMapIterator, &
                                                operator(==), operator(/=)
   use mapl_StateItemCharacteristicKind_mod, only: StateItemCharacteristicKind
   use mapl_GraphStateItem_mod, only: GraphStateItem
   use mapl_ErrorHandling_mod
   implicit none(type, external)
   private

   public :: find_mismatched_state_item_characteristics

contains

   function find_mismatched_state_item_characteristics(item_a, item_b, rc) result(mismatched)
      type(GraphStateItem), intent(in) :: item_a
      type(GraphStateItem), intent(in) :: item_b
      integer, optional, intent(out) :: rc
      type(StateItemCharacteristicKind), allocatable :: mismatched(:)

      type(StateItemCharacteristicMap), target :: map_a, map_b
      type(StateItemCharacteristicMapIterator) :: iter
      type(StateItemCharacteristicKind) :: kind
      class(StateItemCharacteristic), pointer :: char_a, char_b
      logical :: needs_ext
      integer :: status

      allocate(mismatched(0))

      map_a = item_a%get_characteristics()
      map_b = item_b%get_characteristics()

      ! Every kind item_a declares: mismatched if item_b does not declare
      ! it at all, or if both declare it but needs_extension_for() says
      ! they differ (REQ-CHAR-009 - no branching on Value-vs-Reference
      ! kind anywhere in this loop).
      iter = map_a%ftn_begin()
      do while (iter /= map_a%ftn_end())
         call iter%next()
         kind = iter%first()
         char_a => iter%second()

         if (map_b%count(kind) == 0) then
            mismatched = [mismatched, kind]
            cycle
         end if

         char_b => map_b%at(kind, rc=status)
         needs_ext = char_a%needs_extension_for(char_b)
         if (needs_ext) then
            mismatched = [mismatched, kind]
         end if
      end do

      ! Any kind item_b declares that item_a does not: also mismatched
      ! (nothing on item_a's side to compare against) - not caught by the
      ! loop above, which only walks item_a's own entries.
      iter = map_b%ftn_begin()
      do while (iter /= map_b%ftn_end())
         call iter%next()
         kind = iter%first()
         if (map_a%count(kind) == 0) then
            mismatched = [mismatched, kind]
         end if
      end do

      _RETURN(_SUCCESS)
   end function find_mismatched_state_item_characteristics

end module mapl_StateItemCharacteristicDetection_mod
