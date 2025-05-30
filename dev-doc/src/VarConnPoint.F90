module MAPL_VarConnPoint
   use ESMF, only: ESMF_MAXSTR
   use oomph, only: ConnectionPoint
   implicit none
   private

   public :: VarConnPoint
   
   type VarConnPoint
      private
      type(ConnectionPoint) :: new_connection_point
      character(len=ESMF_MAXSTR)               :: short_name
      integer :: gc_id
   contains
      procedure :: get_short_name
      procedure :: get_gc_id
   end type VarConnPoint

   interface VarConnPoint
      module procedure new_VarConnPoint
   end interface VarConnPoint

contains

   function new_VarConnPoint(short_name, gc_id) result(conn_point)
      use MAPL_KeywordEnforcerMod
      type(VarConnPoint) :: conn_point
      character(*), intent(in) :: short_name
      integer, intent(in) :: gc_id

      conn_point%new_connection_point%state_item = trim(short_name)
      conn_point%gc_id = gc_id
   end function new_VarConnPoint

   function get_short_name(this) result(short_name)
      class(VarConnPoint), intent(in) :: this
      character(:), allocatable :: short_name
      
      short_name = this%new_connection_point%state_item
   end function get_short_name

   integer function get_gc_id(this) result(gc_id)
      class(VarConnPoint), intent(in) :: this
      gc_id = this%gc_id
   end function get_gc_id

   
end module MAPL_VarConnPoint
