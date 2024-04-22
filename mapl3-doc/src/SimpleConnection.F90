#include "MAPL_Generic.h"

module mapl3g_SimpleConnection
   use mapl3g_StateItemSpec
   use mapl3g_ConnectionPt
   use mapl3g_HierarchicalRegistry
   use mapl3g_VirtualConnectionPt
   use mapl3g_ActualConnectionPt
   use mapl3g_ActualPtVec_Map
   use mapl3g_ActualPtVector
   use mapl3g_GriddedComponentDriver
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

   recursive subroutine connect_sibling(this, dst_registry, src_registry, unusable, rc)
      class(SimpleConnection), intent(in) :: this
      type(HierarchicalRegistry), target, intent(inout) :: dst_registry
      type(HierarchicalRegistry), target, intent(inout) :: src_registry
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      type(StateItemSpecPtr), target, allocatable :: src_specs(:), dst_specs(:)
      class(StateItemSpec), pointer :: src_spec, dst_spec
      integer :: i, j
      integer :: status
      type(ConnectionPt) :: src_pt, dst_pt
      integer :: i_extension
      integer :: cost, lowest_cost
      class(StateItemSpec), pointer :: best_spec
      class(StateItemSpec), pointer :: last_spec
      class(StateItemSpec), target, allocatable :: old_spec
      class(StateItemSpec), allocatable, target :: new_spec
      type(ActualConnectionPt) :: effective_pt
      type(ActualConnectionPt) :: extension_pt


      type(GriddedComponentDriver), pointer :: source_coupler
      type(ActualPtVector), pointer :: src_actual_pts
      type(ActualConnectionPt), pointer :: best_pt
      

      src_pt = this%get_source()
      dst_pt = this%get_destination()

      dst_specs = dst_registry%get_actual_pt_SpecPtrs(dst_pt%v_pt, _RC)
      src_specs = src_registry%get_actual_pt_SpecPtrs(src_pt%v_pt, _RC)

      src_actual_pts => src_registry%get_actual_pts(src_pt%v_pt)
      _ASSERT(src_actual_pts%size() > 0, 'Empty virtual point?  This should not happen.')

      do i = 1, size(dst_specs)
         dst_spec => dst_specs(i)%ptr

         ! Connection is transitive -- if any src_specs can connect, all can connect.
         ! So we can just check this property on the 1st item.
         src_spec => src_specs(1)%ptr
         _ASSERT(dst_spec%can_connect_to(src_spec), "impossible connection")

         call find_closest_spec(dst_spec, src_specs, src_actual_pts, closest_spec=best_spec, closest_pt=best_pt, lowest_cost=lowest_cost, _RC)
         call best_spec%set_active()
         call activate_dependencies(best_spec, src_registry, _RC)

         ! Now build out sequence of extensions that form a chain to
         ! dst_spec.  This includes creating couplers (handled inside
         ! registry.)
         last_spec => best_spec
         old_spec = best_spec
         source_coupler => null()
         do i_extension = 1, lowest_cost
            new_spec = old_spec%make_extension(dst_spec, _RC)
            call new_spec%set_active()
            extension_pt = src_registry%extend(src_pt%v_pt, old_spec, new_spec, source_coupler=source_coupler, _RC)
            source_coupler => src_registry%get_export_coupler(extension_pt)
            ! ifort 2021.6 does something odd with the following move_alloc
!!$            call move_alloc(from=new_spec, to=old_spec)
            deallocate(old_spec)
            allocate(old_spec, source=new_spec)
            deallocate(new_spec)

            last_spec => old_spec
         end do

         call dst_spec%set_active()

         ! If couplers were needed, then the final coupler must also be
         ! referenced in the dst registry so that gridcomps can do update()
         ! requests.
         if (lowest_cost >= 1) then
            call dst_registry%add_import_coupler(ActualConnectionPt(dst_pt%v_pt), source_coupler)
         end if

         ! In the case of wildcard specs, we need to pass an actual_pt to
         ! the dst_spec to support multiple matches.  A bit of a kludge.
         effective_pt = ActualConnectionPt(VirtualConnectionPt(ESMF_STATEINTENT_IMPORT, &
              src_pt%v_pt%get_esmf_name(), comp_name=src_pt%v_pt%get_comp_name()))
         effective_pt = ActualConnectionPt(VirtualConnectionPt(ESMF_STATEINTENT_IMPORT, &
              src_pt%v_pt%get_comp_name()//'/'//src_pt%v_pt%get_esmf_name()))
         call dst_spec%connect_to(last_spec, effective_pt, _RC)
         call dst_spec%set_active()
            
      end do
         
      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine connect_sibling

   subroutine activate_dependencies(spec, registry, rc)
      class(StateItemSpec), intent(in) :: spec
      type(HierarchicalRegistry), target, intent(in) :: registry
      integer, optional, intent(out) :: rc

      integer :: status
      integer :: i
      type(ActualPtVector) :: dependencies
      class(StateItemSpec), pointer :: dep_spec

      dependencies = spec%get_dependencies()
      do i = 1, dependencies%size()
         dep_spec => registry%get_item_spec(dependencies%of(i), _RC)
         call dep_spec%set_active()
      end do

      _RETURN(_SUCCESS)
   end subroutine activate_dependencies

   subroutine find_closest_spec(goal_spec, candidate_specs, candidate_pts, closest_spec, closest_pt, lowest_cost, rc)
      class(StateItemSpec), intent(in) :: goal_spec
      type(StateItemSpecPtr), target, intent(in) :: candidate_specs(:)
      type(ActualPtVector), target, intent(in) :: candidate_pts
      class(StateItemSpec), pointer :: closest_Spec
      type(ActualConnectionPt), pointer :: closest_pt
      integer, intent(out) :: lowest_cost
      integer, optional, intent(out) :: rc

      integer :: status
      class(StateItemSpec), pointer :: spec
      integer :: cost
      integer :: j
      
      _ASSERT(size(candidate_specs) > 0, 'no candidates found')

      closest_spec => candidate_specs(1)%ptr
      closest_pt => candidate_pts%of(1)
      lowest_cost = goal_spec%extension_cost(closest_spec, _RC)
      do j = 2, size(candidate_specs)
         if (lowest_cost == 0) exit

         spec => candidate_specs(j)%ptr
         cost = goal_spec%extension_cost(spec)
         if (cost < lowest_cost) then
            lowest_cost = cost
            closest_spec => spec
            closest_pt => candidate_pts%of(j)
         end if

      end do

   end subroutine find_closest_spec

end module mapl3g_SimpleConnection
