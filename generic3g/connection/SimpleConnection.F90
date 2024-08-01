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
      type(StateRegistry), target, intent(inout) :: registry
      integer, optional, intent(out) :: rc

      type(StateRegistry), pointer :: src_registry, dst_registry
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


   recursive subroutine connect_sibling(this, dst_registry, src_registry, unusable, rc)
      class(SimpleConnection), intent(in) :: this
      type(StateRegistry), target, intent(inout) :: dst_registry
      type(StateRegistry), target, intent(inout) :: src_registry
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc


      type(StateItemExtensionPtr), target, allocatable :: src_extensions(:), dst_extensions(:)
      type(StateItemExtension), pointer :: src_extension, dst_extension
      class(StateItemSpec), pointer :: src_spec, dst_spec
      integer :: i, j
      integer :: status
      type(ConnectionPt) :: src_pt, dst_pt
      integer :: i_extension
      integer :: cost, lowest_cost
      type(StateItemExtension), pointer :: best_extension
      type(StateItemExtension), pointer :: last_extension
      type(StateItemExtension) :: extension
      type(StateItemExtension), pointer :: new_extension
      class(StateItemSpec), pointer :: last_spec
      class(StateItemSpec), pointer :: best_spec
      type(ActualConnectionPt) :: effective_pt

      type(GriddedComponentDriver), pointer :: coupler
      type(ActualPtVector), pointer :: src_actual_pts
      type(ActualConnectionPt), pointer :: best_pt

      src_pt = this%get_source()
      dst_pt = this%get_destination()

      dst_extensions = dst_registry%get_extensions(dst_pt%v_pt, _RC)
      src_extensions = src_registry%get_extensions(src_pt%v_pt, _RC)

      do i = 1, size(dst_extensions)
         dst_extension => dst_extensions(i)%ptr
         dst_spec => dst_extension%get_spec()

         ! Connection is transitive -- if any src_specs can connect, all can connect.
         ! So we can just check this property on the 1st item.
         src_extension => src_extensions(1)%ptr
         src_spec => src_extension%get_spec()
         _ASSERT(dst_spec%can_connect_to(src_spec), "impossible connection")

         call find_closest_extension(dst_extension, src_extensions, closest_extension=best_extension, lowest_cost=lowest_cost, _RC)
         best_spec => best_extension%get_spec()
         call best_spec%set_active()
         call activate_dependencies(best_extension, src_registry, _RC)

         last_extension => best_extension

         do i_extension = 1, lowest_cost
            extension = last_extension%make_extension(dst_spec, _RC)
            new_extension => src_registry%add_extension(src_pt%v_pt, extension, _RC)
            coupler => new_extension%get_producer()
            call last_extension%add_consumer(coupler)
            last_extension => new_extension
         end do

         ! In the case of wildcard specs, we need to pass an actual_pt to
         ! the dst_spec to support multiple matches.  A bit of a kludge.
         effective_pt = ActualConnectionPt(VirtualConnectionPt(ESMF_STATEINTENT_IMPORT, &
              src_pt%v_pt%get_comp_name()//'/'//src_pt%v_pt%get_esmf_name()))
         last_spec => last_extension%get_spec()
         call dst_spec%connect_to(last_spec, effective_pt, _RC)
         call dst_spec%set_active()
            
      end do
         
      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine connect_sibling

   ! This activates _within_ the user gridcomp.   Some exports may require
   ! other exports to be computed even when no external connection is made to those
   ! exports.
   subroutine activate_dependencies(extension, registry, rc)
      type(StateItemExtension), intent(in) :: extension
      type(StateRegistry), target, intent(in) :: registry
      integer, optional, intent(out) :: rc

      integer :: status
      integer :: i
      type(StringVector) :: dependencies
      class(StateItemExtension), pointer :: dep_extension
      class(StateItemSpec), pointer :: spec
      class(StateItemSpec), pointer :: dep_spec

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

   subroutine find_closest_extension(goal_extension, candidate_extensions, closest_extension, lowest_cost, rc)
      type(StateItemExtension), intent(in) :: goal_extension
      type(StateItemExtensionPtr), target, intent(in) :: candidate_extensions(:)
      type(StateItemExtension), pointer :: closest_extension
      integer, intent(out) :: lowest_cost
      integer, optional, intent(out) :: rc

      integer :: status
      type(StateItemExtension), pointer :: extension
      class(StateItemSpec), pointer :: spec
      class(StateItemSpec), pointer :: goal_spec
      integer :: cost
      integer :: j
      
      _ASSERT(size(candidate_extensions) > 0, 'no candidates found')

      goal_spec => goal_extension%get_spec()
      closest_extension => candidate_extensions(1)%ptr
      spec => closest_extension%get_spec()
      lowest_cost = goal_spec%extension_cost(spec, _RC)
      do j = 2, size(candidate_extensions)
         if (lowest_cost == 0) exit

         extension => candidate_extensions(j)%ptr
         spec => closest_extension%get_spec()
         cost = goal_spec%extension_cost(spec)
         if (cost < lowest_cost) then
            lowest_cost = cost
            closest_extension => extension
         end if

      end do

   end subroutine find_closest_extension

end module mapl3g_SimpleConnection

