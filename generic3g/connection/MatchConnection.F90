#include "MAPL_Generic.h"

module mapl3g_MatchConnection
   use mapl3g_AbstractStateItemSpec
   use mapl3g_ConnectionPt
   use mapl3g_HierarchicalRegistry, only: Connection
   use mapl3g_HierarchicalRegistry
   use mapl3g_SimpleConnection
   use mapl3g_VirtualConnectionPt
   use mapl3g_VirtualConnectionPtVector
   use mapl3g_ActualConnectionPt
   use mapl3g_ActualPtVec_Map
   use mapl3g_ActualPtVector
   use mapl3g_AbstractStateItemSpec
   use mapl_KeywordEnforcer
   use mapl_ErrorHandling
   use esmf

   implicit none
   private

   public :: MatchConnection

   type, extends(Connection) :: MatchConnection
      private
      type(ConnectionPt) :: source
      type(ConnectionPt) :: destination
   contains
      procedure :: get_source
      procedure :: get_destination
      procedure :: connect
   end type MatchConnection

   interface MatchConnection
      module procedure :: new_MatchConnection
   end interface MatchConnection

contains

   function new_MatchConnection(source, destination) result(this)
      type(MatchConnection) :: this
      type(ConnectionPt), intent(in) :: source
      type(ConnectionPt), intent(in) :: destination

      this%source = source
      this%destination = destination

   end function new_MatchConnection

   function get_source(this) result(source)
      type(ConnectionPt) :: source
      class(MatchConnection), intent(in) :: this
      source = this%source
   end function get_source

   function get_destination(this) result(destination)
      type(ConnectionPt) :: destination
      class(MatchConnection), intent(in) :: this
      destination = this%destination
   end function get_destination

   recursive subroutine connect(this, registry, rc)
      class(MatchConnection), intent(in) :: this
      type(HierarchicalRegistry), target, intent(inout) :: registry
      integer, optional, intent(out) :: rc

      integer :: status

      type(ConnectionPt) :: src_pt, dst_pt
      type(HierarchicalRegistry), pointer :: src_registry, dst_registry
      type(VirtualConnectionPtVector) :: src_v_pts, dst_v_pts
      type(VirtualConnectionPt), pointer :: dst_pattern, src_v_pt
      type(VirtualConnectionPt) :: src_pattern, dst_v_pt
      type(StateItemSpecPtr), allocatable :: dst_specs(:)
      integer :: i, j
      type(ConnectionPt) :: s_pt, d_pt

      src_pt = this%get_source()
      dst_pt = this%get_destination()

      src_registry => registry%get_subregistry(src_pt)
      dst_registry => registry%get_subregistry(dst_pt)

      dst_v_pts = dst_registry%filter(dst_pt%v_pt)

      do i = 1, dst_v_pts%size()
         dst_pattern => dst_v_pts%of(i)
         src_pattern = VirtualConnectionPt(ESMF_STATEINTENT_IMPORT, &
              '^'//dst_pattern%get_esmf_name()//'$', comp_name=dst_pattern%get_comp_name())
         dst_specs = dst_registry%get_actual_pt_SpecPtrs(dst_pattern, _RC)

         src_pattern = VirtualConnectionPt(ESMF_STATEINTENT_EXPORT, &
              dst_pattern%get_esmf_name(), comp_name=dst_pattern%get_comp_name())

         src_v_pts = src_registry%filter(src_pattern)
         do j = 1, src_v_pts%size()
            src_v_pt => src_v_pts%of(j)

            dst_v_pt = VirtualConnectionPt(ESMF_STATEINTENT_IMPORT, &
                 src_v_pt%get_esmf_name(), comp_name=src_v_pt%get_comp_name())

            s_pt = ConnectionPt(src_pt%component_name, src_v_pt)
            d_pt = ConnectionPt(dst_pt%component_name, dst_pattern)

            call registry%add_connection(SimpleConnection(s_pt, d_pt), _RC)

         end do
      end do

      _RETURN(_SUCCESS)
   end subroutine connect


end module mapl3g_MatchConnection
