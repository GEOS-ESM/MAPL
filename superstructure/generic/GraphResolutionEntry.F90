! Record type for unresolved-import and unsupported-characteristic reports.
! `reason` is empty for entries where the collection's meaning alone applies.
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
