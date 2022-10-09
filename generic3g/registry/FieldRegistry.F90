#include "MAPL_Generic.h"

module mapl3g_FieldRegistry
   use mapl3g_AbstractStateItemSpec
   use mapl3g_ConnectionPoint
   use mapl3g_ConnectionSpec
   use mapl3g_ConnectionSpecVector
   use mapl3g_ItemSpecRegistry
   use mapl3g_ConnPtStateItemPtrMap
   use mapl3g_StateItemVector
   use mapl3g_StateItemSpecPtr
   use mapl_ErrorHandling
   implicit none
   private
  
   public :: FieldRegistry

   type :: FieldRegistry
      private
      type(StateItemVector) :: specs
      type(ConnPtStateItemPtrMap) :: specs_map
      type(ConnectionSpecVector) :: connections

   contains
      procedure :: add_item_spec
      procedure :: get_item_spec
      procedure :: has_item_spec
      procedure :: add_connection
      procedure :: allocate
      procedure :: terminate_import

      ! helper
      procedure :: update_spec
      procedure :: propagate_specs
      procedure :: set_active
   end type FieldRegistry

   
  
contains

   subroutine add_item_spec(this, conn_pt, spec, rc)
      class(FieldRegistry), intent(inout) :: this
      type(ConnectionPoint), intent(in) :: conn_pt
      class(AbstractStateItemSpec), target, intent(in) :: spec
      integer, optional, intent(out) :: rc

      integer :: status
      type(StateItemSpecPtr) :: wrap


      _ASSERT(.not. this%has_item_spec(conn_pt), 'Duplicate registry entry.')

      call this%specs%push_back(spec)
      wrap = StateItemSpecPtr(this%specs%back())
      call this%specs_map%insert(conn_pt, wrap)

      if (conn_pt%is_internal()) call this%set_active(conn_pt)

      _RETURN(_SUCCESS)
   end subroutine add_item_spec

   function get_item_spec(this, conn_pt) result(spec)
      class(AbstractStateItemSpec), pointer :: spec
      class(FieldRegistry), intent(in) :: this
      type(ConnectionPoint), intent(in) :: conn_pt

      integer :: status
      type(StateItemSpecPtr), pointer :: wrap

      ! failure is ok; return null ptr
      wrap => this%specs_map%at(conn_pt, rc=status)
      if (associated(wrap)) then
         spec => wrap%ptr
      else
         spec => null()
      end if

   end function get_item_spec


   logical function has_item_spec(this, conn_pt)
      class(FieldRegistry), intent(in) :: this
      type(ConnectionPoint), intent(in) :: conn_pt
      has_item_spec = (this%specs_map%count(conn_pt) > 0)
   end function has_item_spec

   subroutine set_active(this, conn_pt)
      class(FieldRegistry), intent(inout) :: this
      class(ConnectionPoint), intent(in) :: conn_pt

      class(AbstractStateItemSpec), pointer :: spec

      spec => this%get_item_spec(conn_pt)
      if (associated(spec)) call spec%set_active()

   end subroutine set_active


   subroutine add_connection(this, connection, rc)
      class(FieldRegistry), intent(inout) :: this
      type(ConnectionSpec), intent(in) :: connection
      integer, optional, intent(out) :: rc

      integer :: status

      _ASSERT(this%has_item_spec(connection%source),'Unknown source point for connection.')
      _ASSERT(this%has_item_spec(connection%destination),'Unknown destination point for connection.')

      call this%connections%push_back(connection)
      associate(src_pt => connection%source, dst_pt => connection%destination)
        if (connection%is_sibling()) then
           print*,__FILE__,__LINE__, src_pt%short_name()
           call this%set_active(src_pt)
        end if
        call this%update_spec(src_pt, dst_pt, _RC)
        call this%propagate_specs(src_pt, dst_pt, _RC)
      end associate
      
      _RETURN(_SUCCESS)
   end subroutine add_connection


   subroutine update_spec(this, src_pt, dst_pt, rc)
      class(FieldRegistry), intent(inout) :: this
      type(ConnectionPoint), intent(in) :: src_pt
      type(ConnectionPoint), intent(in) :: dst_pt
      integer, optional, intent(out) :: rc

      integer :: status
      class(AbstractStateItemSpec), pointer :: dst_spec, src_spec
      type(StateItemSpecPtr), pointer :: dst_wrap, src_wrap

      dst_wrap => this%specs_map%of(dst_pt)
      src_wrap => this%specs_map%of(src_pt)
      dst_wrap = src_wrap
      
!!$      dst_spec => this%get_item_spec(dst_pt)
!!$      src_spec => this%get_item_spec(src_pt)
!!$      call dst_spec%connect_to(src_spec, _RC)

      _RETURN(_SUCCESS)
   end subroutine update_spec


   ! Secondary consequences of a connection
   ! Any items with new dst as a source should update
   ! to have new src as their source.
   subroutine propagate_specs(this, src_pt, dst_pt, rc)
      class(FieldRegistry), intent(inout) :: this
      type(ConnectionPoint), intent(in) :: src_pt
      type(ConnectionPoint), intent(in) :: dst_pt
      integer, optional, intent(out) :: rc

      type(ConnectionSpec), pointer :: connection
      type(ConnectionPoint), pointer :: conn_src, conn_dst
      class(AbstractStateItemSpec), pointer :: conn_spec, src_spec
      type(ConnectionSpecVectorIterator) :: iter
      integer :: status

      src_spec => this%get_item_spec(src_pt)

      associate (e => this%connections%end())
        iter = this%connections%begin()
        do while (iter /= e)
           connection => iter%of()
           conn_src => connection%source
           conn_dst => connection%destination
           if (conn_src == dst_pt) then
              call this%update_spec(src_pt, conn_dst)
!!$              conn_spec => this%get_item_spec(conn_dst)
!!$              call conn_spec%connect_to(src_spec, _RC)
           end if
           call iter%next()
        end do
      end associate

   end subroutine propagate_specs


   subroutine allocate(this, rc)
      class(FieldRegistry), intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status
      class(AbstractStateItemSpec), pointer :: spec
      class(StateItemSpecPtr), pointer :: wrap
      type(ConnPtStateItemPtrMapIterator) :: iter

      
      associate (e => this%specs_map%end())
        iter = this%specs_map%begin()
        do while (iter /= e)
           wrap => iter%second()
           _ASSERT(associated(wrap), 'internal inconsistency in FieldRegistry')
           spec => wrap%ptr
           if (spec%is_active()) then
              call spec%allocate(_RC)
           end if
           call iter%next()
        end do
      end associate

     _RETURN(_SUCCESS)
   end subroutine allocate


   subroutine terminate_import(this, conn_pt, rc)
      class(FieldRegistry), intent(inout) :: this
      type(ConnectionPoint), intent(in) :: conn_pt
      integer, optional, intent(out) :: rc

      _ASSERT(this%has_item_spec(conn_pt), 'Cannot terminate import on unregistered item.')
      _ASSERT(conn_pt%is_import(), 'Cannot terminate import on item that is not an import.')
      call this%set_active(conn_pt)

   end subroutine terminate_import

end module mapl3g_FieldRegistry
