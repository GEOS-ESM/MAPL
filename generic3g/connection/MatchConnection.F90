#include "MAPL_Generic.h"

module mapl3g_MatchConnection
   use mapl3g_AbstractStateItemSpec
   use mapl3g_ConnectionPt
   use mapl3g_HierarchicalRegistry, only: Connection
   use mapl3g_HierarchicalRegistry
   use mapl3g_SimpleConnection
   use mapl3g_VirtualConnectionPt
   use mapl3g_ActualConnectionPt
   use mapl3g_ActualPtVec_Map
   use mapl3g_ActualPtVector
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
!!$      procedure :: is_export_to_import
!!$      procedure :: is_export_to_export
!!$      procedure :: is_valid
!!$      procedure :: is_sibling

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

      type(HierarchicalRegistry), pointer :: src_registry, dst_registry
      integer :: status
      type(VirtualConnectionPt) :: s_v_pt
      type(VirtualConnectionPt), pointer :: d_v_pt
      type(ConnectionPt) :: s_pt,d_pt
      type(ActualPtVec_MapIterator) :: iter

      associate( &
           src_pt => this%get_source(), &
           dst_pt => this%get_destination() &
           )
        dst_registry => registry%get_subregistry(dst_pt)
        ! TODO: Move this into a separate procedure, or introduce
        ! a 2nd type of connection
        if (dst_pt%get_esmf_name() == '*') then
           associate (range => dst_registry%get_range())
             iter = range(1)
             do while (iter /= range(2))
                d_v_pt => iter%first()
                if (d_v_pt%get_state_intent() /= 'import') cycle
                s_v_pt = VirtualConnectionPt(ESMF_STATEINTENT_EXPORT, &
                     d_v_pt%get_esmf_name(), &
                     comp_name=d_v_pt%get_comp_name())
                s_pt = ConnectionPt(src_pt%component_name, s_v_pt)
                d_pt = ConnectionPt(dst_pt%component_name, d_v_pt)
                call registry%add_connection(SimpleConnection(s_pt, d_pt), _RC)
                call iter%next()
             end do
           end associate
           _RETURN(_SUCCESS)
        end if
        
      end associate
      
      _RETURN(_SUCCESS)
   end subroutine connect


 end module mapl3g_MatchConnection
