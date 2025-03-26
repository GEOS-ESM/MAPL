#include "MAPL_Generic.h"

module mapl3g_SimpleConnection
   use mapl3g_StateItemSpec
   use mapl3g_Connection
   use mapl3g_ConnectionPt
   use mapl3g_StateRegistry
   use mapl3g_VirtualConnectionPt
   use mapl3g_ActualConnectionPt
   use mapl3g_ActualPtVec_Map
   use mapl3g_ActualPtVector
   use mapl3g_GriddedComponentDriver
   use mapl3g_StateItemExtension
   use mapl3g_StateItemExtensionVector
   use mapl3g_StateItemExtensionPtrVector
   use mapl3g_MultiState
   use mapl_KeywordEnforcer
   use mapl_ErrorHandling
   use gFTL2_StringVector, only: StringVector
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
      procedure :: activate
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

   recursive subroutine activate(this, registry, rc)
      class(SimpleConnection), intent(in) :: this
      type(StateRegistry), target, intent(inout) :: registry
      integer, optional, intent(out) :: rc

      type(StateRegistry), pointer :: src_registry, dst_registry
      type(ConnectionPt) :: src_pt, dst_pt
      type(StateItemExtensionPtr), target, allocatable :: src_extensions(:), dst_extensions(:)
      type(StateItemExtension), pointer :: src_extension, dst_extension
      type(StateItemSpec), pointer :: spec
      integer :: i
      integer :: status

      src_pt = this%get_source()
      dst_pt = this%get_destination()

      dst_registry => registry%get_subregistry(dst_pt)
      src_registry => registry%get_subregistry(src_pt)
        
      _ASSERT(associated(src_registry), 'Unknown source registry')
      _ASSERT(associated(dst_registry), 'Unknown destination registry')
        
      _ASSERT(dst_registry%has_virtual_pt(dst_pt%v_pt), "connection to unknown src_pt")
      dst_extensions = dst_registry%get_extensions(dst_pt%v_pt, _RC)
      _ASSERT(src_registry%has_virtual_pt(src_pt%v_pt), "connection to unknown src_pt")
      src_extensions = src_registry%get_extensions(src_pt%v_pt, _RC)

      do i = 1, size(dst_extensions)
         dst_extension => dst_extensions(i)%ptr
         spec => dst_extension%get_spec()
         call spec%set_active()
         call spec%set_allocated()
      end do

      do i = 1, size(src_extensions)
         src_extension => src_extensions(i)%ptr
         spec => src_extension%get_spec()
         call spec%set_active()
         call activate_dependencies(src_extension, src_registry, _RC)
      end do
        
      _RETURN(_SUCCESS)
   end subroutine activate


   recursive subroutine connect(this, registry, rc)
      class(SimpleConnection), intent(in) :: this
      type(StateRegistry), target, intent(inout) :: registry
      integer, optional, intent(out) :: rc

      type(StateRegistry), pointer :: src_registry, dst_registry
      type(ConnectionPt) :: src_pt, dst_pt
      integer :: status

      src_pt = this%get_source()
      dst_pt = this%get_destination()

      dst_registry => registry%get_subregistry(dst_pt)
      src_registry => registry%get_subregistry(src_pt)
        
      _ASSERT(associated(src_registry), 'Unknown source registry')
      _ASSERT(associated(dst_registry), 'Unknown destination registry')

      call this%connect_sibling(dst_registry, src_registry, _RC)
        
      _RETURN(_SUCCESS)
   end subroutine connect


   recursive subroutine connect_sibling(this, dst_registry, src_registry, unusable, rc)
      class(SimpleConnection), intent(in) :: this
      type(StateRegistry), target, intent(inout) :: dst_registry
      type(StateRegistry), target, intent(inout) :: src_registry
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc


      type(StateItemExtensionPtr), target, allocatable :: dst_extensions(:)
      type(StateItemExtension), pointer :: dst_extension
      type(StateItemSpec), pointer :: dst_spec
      integer :: i
      integer :: status
      type(ConnectionPt) :: src_pt, dst_pt
      type(StateItemExtension), pointer :: last_extension
      type(StateItemExtension), pointer :: new_extension
      type(StateItemSpec), pointer :: new_spec
      type(ActualConnectionPt) :: effective_pt
      type(GriddedComponentDriver), pointer :: coupler
      type(ActualConnectionPt) :: a_pt
      type(MultiState) :: coupler_states

      src_pt = this%get_source()
      dst_pt = this%get_destination()

      dst_extensions = dst_registry%get_extensions(dst_pt%v_pt, _RC)

      do i = 1, size(dst_extensions)
         dst_extension => dst_extensions(i)%ptr
         dst_spec => dst_extension%get_spec()

         new_extension => src_registry%extend(src_pt%v_pt, dst_spec, _RC)

         ! In the case of wildcard specs, we need to pass an actual_pt to
         ! the dst_spec to support multiple matches.  A bit of a kludge.
         effective_pt = ActualConnectionPt(VirtualConnectionPt(ESMF_STATEINTENT_IMPORT, &
              src_pt%v_pt%get_comp_name()//'/'//src_pt%v_pt%get_esmf_name()))
         new_spec => new_extension%get_spec()

         call dst_spec%connect(new_spec, effective_pt, _RC)
            
         if (new_extension%has_producer()) then
            call dst_extension%set_producer(new_extension%get_producer(), _RC)
         end if
      end do
         
      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine connect_sibling

   ! This activates _within_ the user gridcomp.   Some exports may require
   ! other exports to be computed even when no external connection is made to those
   ! exports.
   subroutine activate_dependencies(extension, registry, rc)
      type(StateItemExtension), target, intent(in) :: extension
      type(StateRegistry), target, intent(in) :: registry
      integer, optional, intent(out) :: rc

      integer :: status
      integer :: i
      type(StringVector) :: dependencies
      class(StateItemExtension), pointer :: dep_extension
      type(StateItemSpec), pointer :: spec
      type(StateItemSpec), pointer :: dep_spec

      spec => extension%get_spec()
      dependencies = spec%get_raw_dependencies()
      do i = 1, dependencies%size()
         associate (v_pt => VirtualConnectionPt(state_intent='export', short_name=dependencies%of(i)) )
           dep_extension => registry%get_primary_extension(v_pt, _RC)
         end associate
         dep_spec => dep_extension%get_spec()
         call dep_spec%set_active()
      end do

      _RETURN(_SUCCESS)
   end subroutine activate_dependencies


end module mapl3g_SimpleConnection

