#include "MAPL.h"

module mapl3g_ReexportConnection
   use mapl3g_StateItemSpec
   use mapl3g_ExtensionFamily
   use mapl3g_Connection
   use mapl3g_ConnectionPt
   use mapl3g_StateRegistry
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
      procedure :: activate
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

   ! No-op: reexports are always active
   recursive subroutine activate(this, registry, rc)
      class(ReexportConnection), intent(in) :: this
      type(StateRegistry), target, intent(inout) :: registry
      integer, optional, intent(out) :: rc

      integer :: status
      type(StateRegistry), pointer :: src_registry
      type(ConnectionPt) :: src_pt
        
      src_pt = this%get_source()
      src_registry => registry%get_subregistry(src_pt)
      _ASSERT(associated(src_registry), 'Unknown source registry')

      call this%connect_export_to_export(registry, src_registry, _RC)

      _RETURN(_SUCCESS)
   end subroutine activate

   recursive subroutine connect(this, registry, rc)
      class(ReexportConnection), intent(inout) :: this
      type(StateRegistry), target, intent(inout) :: registry
      integer, optional, intent(out) :: rc

      ! no-op
        
      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(registry)
   end subroutine connect

   ! Non-sibling connection: just propagate pointer "up"
   subroutine connect_export_to_export(this, dst_registry, src_registry, unusable, rc)
      use mapl3g_ExtensionFamily
      class(ReexportConnection), intent(in) :: this
      type(StateRegistry), intent(inout) :: dst_registry
      type(StateRegistry), intent(in) :: src_registry
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

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

      family => src_registry%get_extension_family(src_pt, _RC)
      call dst_registry%add_family(dst_pt, family, _RC)
    
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

