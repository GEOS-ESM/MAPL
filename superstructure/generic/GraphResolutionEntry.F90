! openspec/changes/graphbuilder-resolution-entry: a small record type
! replacing the ad hoc `component_name // ':' // short_name [// ':' //
! reason]` string encoding `GraphBuilder.F90` previously used for its
! unresolved-import and unsupported-characteristic resolution reports.
! `reason` is set to '' for entries where no reason beyond the
! collection's own meaning ("no matching export") applies - i.e. every
! unresolved-import entry, by construction (design.md Decision 1).
module mapl_GraphResolutionEntry_mod
   implicit none(type, external)
   private

   public :: GraphResolutionEntry

   type :: GraphResolutionEntry
      character(:), allocatable :: component_name
      character(:), allocatable :: short_name
      character(:), allocatable :: reason
   end type GraphResolutionEntry

   interface GraphResolutionEntry
      module procedure new_graph_resolution_entry
   end interface GraphResolutionEntry

contains

   function new_graph_resolution_entry(component_name, short_name, reason) result(entry)
      type(GraphResolutionEntry) :: entry
      character(*), intent(in) :: component_name
      character(*), intent(in) :: short_name
      character(*), optional, intent(in) :: reason

      entry%component_name = component_name
      entry%short_name = short_name
      if (present(reason)) then
         entry%reason = reason
      else
         entry%reason = ''
      end if

   end function new_graph_resolution_entry

end module mapl_GraphResolutionEntry_mod
