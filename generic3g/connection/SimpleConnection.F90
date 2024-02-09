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
      type(HierarchicalRegistry), target, intent(inout) :: dst_registry
      type(HierarchicalRegistry), target, intent(inout) :: src_registry
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      type(StateItemSpecPtr), allocatable :: src_specs(:), dst_specs(:)
      class(StateItemSpec), pointer :: src_spec, dst_spec
      integer :: i, j
      integer :: status
      type(ConnectionPt) :: src_pt, dst_pt
      integer :: i_extension
      integer :: cost, lowest_cost
      class(StateItemSpec), pointer :: best_spec
      class(StateItemSpec), pointer :: old_spec
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

         ! Loop through possible specific exports to find best match.
         best_spec => src_specs(1)%ptr
         best_pt => src_actual_pts%of(1)
         lowest_cost = dst_spec%extension_cost(best_spec, _RC)
         find_best_src_spec: do j = 2, size(src_specs)
            if (lowest_cost == 0) exit

            src_spec => src_specs(j)%ptr
            cost = dst_spec%extension_cost(src_spec)
            if (cost < lowest_cost) then
               lowest_cost = cost
               best_spec => src_spec
               best_pt => src_actual_pts%of(j)
            end if

         end do find_best_src_spec

         call best_spec%set_active()

         ! Now build out sequence of extensions that form a chain to
         ! dst_spec.  This includes creating couplers (handled inside
         ! registry.)
         old_spec => best_spec
         source_coupler => null()
         do i_extension = 1, lowest_cost
            new_spec = old_spec%make_extension(dst_spec, _RC)
            call new_spec%set_active()
            extension_pt = src_registry%extend(src_pt%v_pt, old_spec, new_spec, source_coupler=source_coupler, _RC)
            source_coupler => src_registry%get_export_coupler(extension_pt)
            old_spec => new_spec
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
         call dst_spec%connect_to(old_spec, effective_pt, _RC)
         call dst_spec%set_active()
            
      end do
         
      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine connect_sibling

 end module mapl3g_SimpleConnection
