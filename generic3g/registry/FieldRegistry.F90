#include "MAPL_Generic.h"

module mapl3g_FieldRegistry
   use mapl3g_AbstractStateItemSpec
   use mapl3g_ConnectionPoint
   use mapl3g_ConnectionSpec
   use mapl3g_ConnectionSpecVector
   use mapl3g_ItemSpecRegistry
   use mapl3g_ConnPtStateItemSpecMap
   use mapl_ErrorHandling
   implicit none
   private
  
   public :: FieldRegistry

   type :: FieldRegistry
      private
      type(ConnPtStateItemSpecMap) :: specs_map
!!$      type(ItemSpecRegistry) :: items_registry
      type(ConnectionSpecVector) :: connections

   contains
      procedure :: add_item_spec
      procedure :: get_item_spec
      procedure :: connect
      procedure :: allocate

      ! helper
      procedure :: update_specs
   end type FieldRegistry

   
  
contains

   subroutine add_item_spec(this, conn_pt, spec)
      class(FieldRegistry), intent(inout) :: this
      type(ConnectionPoint), intent(in) :: conn_pt
      class(AbstractStateItemSpec), intent(in) :: spec
      call this%specs_map%insert(conn_pt, spec)
   end subroutine add_item_spec

   function get_item_spec(this, conn_pt) result(spec)
      class(AbstractStateItemSpec), pointer :: spec
      class(FieldRegistry), intent(in) :: this
      type(ConnectionPoint), intent(in) :: conn_pt

      integer :: status

      spec => this%specs_map%at(conn_pt, rc=status) ! failure is ok; return null ptr

   end function get_item_spec


   subroutine set_active(this, connection_pt)
      class(FieldRegistry), intent(inout) :: this
      class(ConnectionPoint), intent(in) :: connection_pt

      class(AbstractStateItemSpec), pointer :: spec

      spec => this%specs_map%of(connection_pt)
      if (associated(spec)) call spec%set_active()

   end subroutine set_active


   subroutine connect(this, connection, rc)
      class(FieldRegistry), intent(inout) :: this
      type(ConnectionSpec), intent(in) :: connection
      integer, optional, intent(out) :: rc

      integer :: status

      call this%connections%push_back(connection)
      call this%update_specs(connection%source, connection%destination, _RC)
      
      _RETURN(_SUCCESS)
   end subroutine connect


   subroutine update_specs(this, src_pt, dst_pt, rc)
      class(FieldRegistry), intent(inout) :: this
      type(ConnectionPoint), intent(in) :: src_pt
      type(ConnectionPoint), intent(in) :: dst_pt
      integer, optional, intent(out) :: rc

      type(ConnectionSpec), pointer :: connection
      type(ConnectionPoint), pointer :: conn_src, conn_dst
      class(AbstractStateItemSpec), pointer :: conn_spec, src_spec
      type(ConnectionSpecVectorIterator) :: iter
      integer :: status

      src_spec => this%specs_map%of(src_pt)
      associate (e => this%connections%end())
        iter = this%connections%begin()
        do while (iter /= e)
           connection => iter%of()
           conn_src => connection%source
           conn_dst => connection%destination
           if (conn_src == dst_pt) then
              conn_spec => this%specs_map%of(conn_dst)
              call conn_spec%connect_to(src_spec, _RC)
              call iter%next()
           end if
        end do
      end associate

   end subroutine update_specs


   subroutine allocate(this, rc)
      class(FieldRegistry), intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status
      class(AbstractStateItemSpec), pointer :: spec
      type(ConnPtStateItemSpecMapIterator) :: iter

      
      associate (e => this%specs_map%end())
        iter = this%specs_map%begin()
        do while (iter /= e)
           spec => iter%second()
           if (spec%is_active()) then
              call spec%allocate(_RC)
           end if
           call iter%next()
        end do
      end associate

     _RETURN(_SUCCESS)
   end subroutine allocate

end module mapl3g_FieldRegistry
