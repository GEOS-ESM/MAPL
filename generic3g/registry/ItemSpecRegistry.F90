module mapl3g_ItemSpecRegistry
   use mapl3g_ConnectionPt
   use mapl3g_AbstractStateItemSpec
   use mapl3g_ConnPtStateItemSpecMap
   implicit none
   private
  
   public :: ItemSpecRegistry
  
   type :: ItemSpecRegistry
      private
      type(ConnPtStateItemSpecMap) :: specs_map
   contains
      procedure :: add_spec
      procedure :: get_spec
   end type ItemSpecRegistry
  
contains

   subroutine add_spec(this, conn_pt, spec)
      class(ItemSpecRegistry), intent(inout) :: this
      type(ConnectionPt), intent(in) :: conn_pt
      class(AbstractStateItemSpec), intent(in) :: spec

      call this%specs_map%insert(conn_pt, spec)
      
   end subroutine add_spec

   function get_spec(this, conn_pt) result(spec)
      class(AbstractStateItemSpec), pointer :: spec
      class(ItemSpecRegistry), intent(inout) :: this
      type(ConnectionPt), intent(in) :: conn_pt

      spec => this%specs_map%of(conn_pt)
      
   end function get_spec

end module mapl3g_ItemSpecRegistry
