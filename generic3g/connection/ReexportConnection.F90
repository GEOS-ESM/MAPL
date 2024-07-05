#include "MAPL_Generic.h"

module mapl3g_ReexportConnection
   use mapl3g_StateItemSpec
   use mapl3g_ConnectionPt
   use mapl3g_HierarchicalRegistry, only: Connection
   use mapl3g_HierarchicalRegistry
   use mapl3g_Registry
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
      procedure :: connect_old
      procedure :: connect_export_to_export_old
      procedure :: connect_new
      procedure :: connect_export_to_export_new
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

   recursive subroutine connect_old(this, registry, rc)
      class(ReexportConnection), intent(in) :: this
      type(HierarchicalRegistry), target, intent(inout) :: registry
      integer, optional, intent(out) :: rc

      integer :: status
      type(HierarchicalRegistry), pointer :: src_registry
      type(ConnectionPt) :: src_pt

      src_pt = this%get_source()
      src_registry => registry%get_subregistry(src_pt)
      _ASSERT(associated(src_registry), 'Unknown source registry')

      call this%connect_export_to_export_old(registry, src_registry, _RC)
        
      _RETURN(_SUCCESS)
   end subroutine connect_old


   ! Non-sibling connection: just propagate pointer "up"
   subroutine connect_export_to_export_old(this, registry, src_registry, unusable, rc)
      class(ReexportConnection), intent(in) :: this
      type(HierarchicalRegistry), intent(inout) :: registry
      type(HierarchicalRegistry), intent(in) :: src_registry
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      type(ActualPtVectorIterator) :: iter
      class(StateItemSpec), pointer :: spec
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
      associate (e => actual_pts%ftn_end())
        iter = actual_pts%ftn_begin()
        do while (iter /= e)
           call iter%next()
           src_actual_pt => iter%of()
           dst_actual_pt = ActualConnectionPt(dst_pt)
           spec => src_registry%get_item_spec(src_actual_pt)
           _ASSERT(associated(spec), 'This should not happen.')
           call registry%link_item_spec(dst_pt, spec, dst_actual_pt, _RC)
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

   end subroutine connect_export_to_export_old

   recursive subroutine connect_new(this, with_registry, rc)
      class(ReexportConnection), intent(in) :: this
      type(Registry), target, intent(inout) :: with_registry
      integer, optional, intent(out) :: rc

      integer :: status
      type(Registry), pointer :: src_registry
      type(ConnectionPt) :: src_pt

      src_pt = this%get_source()
      src_registry => with_registry%get_subregistry(src_pt)
      _ASSERT(associated(src_registry), 'Unknown source registry')

      call this%connect_export_to_export_new(with_registry, src_registry, _RC)
        
      _RETURN(_SUCCESS)
   end subroutine connect_new

   ! Non-sibling connection: just propagate pointer "up"
   subroutine connect_export_to_export_new(this, dst_registry, src_registry, unusable, rc)
      use mapl3g_ExtensionFamily
      class(ReexportConnection), intent(in) :: this
      type(Registry), intent(inout) :: dst_registry
      type(Registry), intent(in) :: src_registry
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      type(ActualPtVectorIterator) :: iter
      class(StateItemSpec), pointer :: spec
      type(ActualConnectionPt), pointer :: src_actual_pt
      type(ActualConnectionPt), allocatable :: dst_actual_pt
      type(ActualPtVector), pointer :: actual_pts
      integer :: status
      type(VirtualConnectionPt) :: src_pt, dst_pt
      type(ConnectionPt) :: src, dst
      type(ExtensionFamily), pointer :: family

      src = this%get_source()
      dst = this%get_destination()
      src_pt = src%v_pt
      dst_pt = dst%v_pt

      _ASSERT(.not. dst_registry%has_virtual_pt(dst_pt), 'Specified virtual point already exists in this registry')
      _ASSERT(src_registry%has_virtual_pt(src_pt), 'Specified virtual point does not exist.')

      call dst_registry%add_virtual_pt(src_pt, _RC)
      ! get the pointer in dst
      family => dst_registry%get_extension_family(src_pt)
      ! copy from src
      family = src_registry%get_extension_family(src_pt)
    
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

   end subroutine connect_export_to_export_new

  end module mapl3g_ReexportConnection

