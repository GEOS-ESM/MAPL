#include "MAPL_Generic.h"

module mapl3g_SimpleConnection
   use mapl3g_AbstractStateItemSpec
   use mapl3g_ConnectionPt
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

   public :: SimpleConnection

   type, extends(Connection) :: SimpleConnection
      private
      type(ConnectionPt) :: source
      type(ConnectionPt) :: destination
   contains
      procedure :: get_source
      procedure :: get_destination
      procedure :: connect
      procedure :: connect_sibling
   end type SimpleConnection

   interface SimpleConnection
      module procedure :: new_SimpleConnection
   end interface SimpleConnection

contains

   function new_SimpleConnection(source, destination) result(this)
      type(SimpleConnection) :: this
      type(ConnectionPt), intent(in) :: source
      type(ConnectionPt), intent(in) :: destination

      this%source = source
      this%destination = destination

   end function new_SimpleConnection

   function get_source(this) result(source)
      type(ConnectionPt) :: source
      class(SimpleConnection), intent(in) :: this
      source = this%source
   end function get_source

   function get_destination(this) result(destination)
      type(ConnectionPt) :: destination
      class(SimpleConnection), intent(in) :: this
      destination = this%destination
   end function get_destination

   recursive subroutine connect(this, registry, rc)
      class(SimpleConnection), intent(in) :: this
      type(HierarchicalRegistry), target, intent(inout) :: registry
      integer, optional, intent(out) :: rc

      type(HierarchicalRegistry), pointer :: src_registry, dst_registry
      integer :: status
      type(VirtualConnectionPt) :: s_v_pt
      type(VirtualConnectionPt), pointer :: d_v_pt
      type(ConnectionPt) :: s_pt,d_pt
      type(ActualPtVec_MapIterator) :: iter
      type(ConnectionPt) :: src_pt, dst_pt

      src_pt = this%get_source()
      dst_pt = this%get_destination()

      dst_registry => registry%get_subregistry(dst_pt)
      src_registry => registry%get_subregistry(src_pt)
        
      _ASSERT(associated(src_registry), 'Unknown source registry')
      _ASSERT(associated(dst_registry), 'Unknown destination registry')
        
      call this%connect_sibling(dst_registry, src_registry, _RC)
        
      _RETURN(_SUCCESS)
   end subroutine connect

   subroutine connect_sibling(this, dst_registry, src_registry, unusable, rc)
      class(SimpleConnection), intent(in) :: this
      type(HierarchicalRegistry), target, intent(in) :: dst_registry
      type(HierarchicalRegistry), target, intent(inout) :: src_registry
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      type(StateItemSpecPtr), allocatable :: export_specs(:), import_specs(:)
      class(AbstractStateItemSpec), pointer :: export_spec, import_spec
      integer :: i, j
      logical :: satisfied
      integer :: status
      type(ConnectionPt) :: src_pt, dst_pt

      src_pt = this%get_source()
      dst_pt = this%get_destination()

      import_specs = dst_registry%get_actual_pt_SpecPtrs(dst_pt%v_pt, _RC)
      export_specs = src_registry%get_actual_pt_SpecPtrs(src_pt%v_pt, _RC)
          
      do i = 1, size(import_specs)
         import_spec => import_specs(i)%ptr
         satisfied = .false.
         
         find_source: do j = 1, size(export_specs)
            export_spec => export_specs(j)%ptr

            if (.not. import_spec%can_connect_to(export_spec)) cycle

            call export_spec%set_active()
            call import_spec%set_active()
               
            if (import_spec%requires_extension(export_spec)) then
               _HERE, 'This logic should be fixed.  It bypasses connect_to() method.'
               call src_registry%extend(src_pt%v_pt, import_spec, _RC)
            else
               call import_spec%connect_to(export_spec, _RC)
            end if
            
            satisfied = .true.
            exit find_source
         end do find_source
         
         _ASSERT(satisfied,'no matching actual export spec found')
      end do

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine connect_sibling

 end module mapl3g_SimpleConnection
