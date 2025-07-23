#include "MAPL.h"

submodule (mapl3g_OuterMetaComponent) connect_all_smod
   use mapl3g_Connection
   use mapl3g_ConnectionPt
   use mapl3g_VirtualConnectionPt
   use mapl3g_MatchConnection
   use mapl_ErrorHandling
   implicit none(type,external)

contains

   ! ----------
   ! This is a "magic" connection that attempts to connect each
   ! unsatisfied import in dst_comp, with a corresponding export in
   ! the src_comp.  The corresponding export must have the same short
   ! name, or if the import is a wildcard connection point, the all
   ! exports with names that match the regexp of the wildcard are
   ! connected.
   ! ----------
   module subroutine connect_all(this, src_comp, dst_comp, rc)
      class(OuterMetaComponent), intent(inout) :: this
      character(*), intent(in) :: src_comp
      character(*), intent(in) :: dst_comp
      integer, optional, intent(out) :: rc

      class(Connection), allocatable :: conn

      conn = MatchConnection( &
           ConnectionPt(src_comp, VirtualConnectionPt(state_intent='export', short_name='^.*$')), &
           ConnectionPt(dst_comp, VirtualConnectionPt(state_intent='import', short_name='^.*$'))  &
           )
      call this%component_spec%add_connection(conn)

      _RETURN(_SUCCESS)
   end subroutine connect_all

end submodule connect_all_smod
