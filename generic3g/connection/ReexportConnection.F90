#include "MAPL_Generic.h"

module mapl3g_ReexportConnection
   use mapl3g_AbstractStateItemSpec
   use mapl3g_ConnectionPt
   use mapl3g_HierarchicalRegistry, only: Connection
   use mapl3g_HierarchicalRegistry
   use mapl3g_VirtualConnectionPt
   use mapl3g_ActualConnectionPt
   use mapl3g_ActualPtVec_Map
   use mapl3g_ActualPtVector
   use mapl_KeywordEnforcer
   use mapl_ErrorHandling
   use esmf

   implicit none
   private

   public :: ReexportConnection

   type, extends(Connection) :: ReexportConnection
      private
      type(ConnectionPt) :: source
      type(ConnectionPt) :: destination
   contains

      procedure :: get_source
      procedure :: get_destination
      procedure :: connect
      procedure :: connect_export_to_export
   end type ReexportConnection

   interface ReexportConnection
      module procedure :: new_ReexportConnection
   end interface ReexportConnection

contains

   function new_ReexportConnection(source, destination) result(this)
      type(ReexportConnection) :: this
      type(ConnectionPt), intent(in) :: source
      type(ConnectionPt), intent(in) :: destination

      this%source = source
      this%destination = destination

   end function new_ReexportConnection

   function get_source(this) result(source)
      type(ConnectionPt) :: source
      class(ReexportConnection), intent(in) :: this
      source = this%source
   end function get_source

   function get_destination(this) result(destination)
      type(ConnectionPt) :: destination
      class(ReexportConnection), intent(in) :: this
      destination = this%destination
   end function get_destination

   recursive subroutine connect(this, registry, rc)
      class(ReexportConnection), intent(in) :: this
      type(HierarchicalRegistry), target, intent(inout) :: registry
      integer, optional, intent(out) :: rc

      integer :: status
      type(HierarchicalRegistry), pointer :: src_registry

      associate( src_pt => this%get_source() )
        src_registry => registry%get_subregistry(src_pt)
        _ASSERT(associated(src_registry), 'Unknown source registry')
        call this%connect_export_to_export(registry, src_registry, _RC)
      end associate
        
      _RETURN(_SUCCESS)
   end subroutine connect


   ! Non-sibling connection: just propagate pointer "up"
   subroutine connect_export_to_export(this, registry, src_registry, unusable, rc)
      class(ReexportConnection), intent(in) :: this
      type(HierarchicalRegistry), intent(inout) :: registry
      type(HierarchicalRegistry), intent(in) :: src_registry
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      type(ActualPtVectorIterator) :: iter
      class(AbstractStateItemSpec), pointer :: spec
      type(ActualConnectionPt), pointer :: src_actual_pt
      type(ActualConnectionPt), allocatable :: dst_actual_pt
      type(ActualPtVector), pointer :: actual_pts
      integer :: status
      type(VirtualConnectionPt) :: src_pt, dst_pt
      type(ConnectionPt) :: src, dst

      src = this%get_source()
      dst = this%get_destination()
      src_pt = src%v_pt
      dst_pt = dst%v_pt

      _ASSERT(.not. registry%has_item_spec(dst_pt), 'Specified virtual point already exists in this registry')
      _ASSERT(src_registry%has_item_spec(src_pt), 'Specified virtual point does not exist.')
      
      actual_pts => src_registry%get_actual_pts(src_pt)
      associate (e => actual_pts%end())
        iter = actual_pts%begin()
        do while (iter /= e)
           src_actual_pt => iter%of()
           
           if (src_actual_pt%is_internal()) then
              ! Don't encode with comp name
              dst_actual_pt = ActualConnectionPt(dst_pt)
           else
              dst_actual_pt = src_actual_pt%add_comp_name(src_registry%get_name())
           end if
           
           spec => src_registry%get_item_spec(src_actual_pt)
           _ASSERT(associated(spec), 'This should not happen.')
           call registry%link_item_spec(dst_pt, spec, dst_actual_pt, _RC)
           call iter%next()
        end do
      end associate
    
      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)

   contains

      function str_replace(buffer, pattern, replacement) result(new_str)
         character(:), allocatable :: new_str
         character(*), intent(in) :: buffer
         character(*), intent(in) :: pattern
         character(*), intent(in) :: replacement

         integer :: idx

         idx = scan(buffer, pattern)
         new_str = buffer(:idx-1) // replacement // buffer(idx+len(pattern):)
      end function str_replace

   end subroutine connect_export_to_export

 end module mapl3g_ReexportConnection

