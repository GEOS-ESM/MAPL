module mapl_Connection_mod
   implicit none(type,external)
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
         use mapl_ConnectionPt_mod
         import Connection
         type(ConnectionPt) :: source
         class(Connection), intent(in) :: this
      end function I_get

      subroutine I_activate(this, registry, rc)
         use mapl_StateRegistry_mod
         import Connection
         class(Connection), target, intent(in) :: this
         type(StateRegistry), target, intent(inout) :: registry
         integer, optional, intent(out) :: rc
      end subroutine I_activate

      subroutine I_connect(this, registry, rc)
         use mapl_StateRegistry_mod
         import Connection
         class(Connection), target, intent(inout) :: this
         type(StateRegistry), target, intent(inout) :: registry
         integer, optional, intent(out) :: rc
      end subroutine I_connect

   end interface


end module mapl_Connection_mod
