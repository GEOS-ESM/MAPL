module mapl3g_Connection
   implicit none
   private

   public :: Connection


   type, abstract :: Connection
   contains
      procedure(I_get), deferred :: get_source
      procedure(I_get), deferred :: get_destination
      procedure(I_activate), deferred :: activate
      procedure(I_connect), deferred :: connect
   end type Connection


   abstract interface

      function I_get(this) result(source)
         use mapl3g_ConnectionPt
         import Connection
         type(ConnectionPt) :: source
         class(Connection), intent(in) :: this
      end function I_get

      subroutine I_activate(this, registry, rc)
         use mapl3g_StateRegistry
         import Connection
         class(Connection), intent(in) :: this
         type(StateRegistry), target, intent(inout) :: registry
         integer, optional, intent(out) :: rc
      end subroutine I_activate

      subroutine I_connect(this, registry, rc)
         use mapl3g_StateRegistry
         import Connection
         class(Connection), intent(inout) :: this
         type(StateRegistry), target, intent(inout) :: registry
         integer, optional, intent(out) :: rc
      end subroutine I_connect

   end interface


end module mapl3g_Connection
