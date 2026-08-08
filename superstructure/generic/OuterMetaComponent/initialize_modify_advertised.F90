#include "MAPL.h"

submodule (mapl_OuterMetaComponent_mod) initialize_modify_advertised_smod
   use mapl_enums_api, only: MAPL_GENERIC_INIT_MODIFY_ADVERTISED, MAPL_STATEITEM_ALLOCATION_CONNECTED
   use mapl_MultiState_mod
   use mapl_Connection_mod
   use mapl_ConnectionVector_mod, only: ConnectionVectorIterator
   use mapl_ConnectionVector_mod, only: operator(/=)
   use mapl_ConnectionPt_mod
   use mapl_VirtualConnectionPt_mod
   use mapl_StateItemSpec_mod
   use mapl_StateItemAspect_mod
   use mapl_AspectId_mod, only: CLASS_ASPECT_ID
   use mapl_GeometryClassAspect_mod
   use mapl_GeometrySpec_mod, only: GEOMETRY_FROM_PARENT, GEOMETRY_FROM_CHILD
   use mapl_StateItemAllocation_mod, only: StateItemAllocation, operator(/=)
   use mapl_InternalConstants_mod, only: MAPL_FRAMEWORK_NAMESPACE
   use esmf, only: ESMF_Geom, ESMF_GridComp, ESMF_STATEINTENT_IMPORT
   use mapl_ErrorHandling_mod
   implicit none(type,external)

contains

   module recursive subroutine initialize_modify_advertised(this, importState, exportState, clock, unusable, rc)
      class(OuterMetaComponent), target, intent(inout) :: this
      ! optional arguments
      type(ESMF_State) :: importState
      type(ESMF_State) :: exportState
      type(ESMF_Clock) :: clock
      class(KE), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status
      logical :: geometry_ready
      character(*), parameter :: PHASE_NAME = 'GENERIC::INIT_MODIFY_ADVERTISED'
      type(MultiState) :: user_states

      user_states = this%user_gc_driver%get_states()
      call this%registry%add_to_states(user_states, mode='user', _RC)

      call process_framework_geometry_connections(this, _RC)
      call this%run_custom(ESMF_METHOD_INITIALIZE, PHASE_NAME, _RC)
      call recurse(this, phase_idx=MAPL_GENERIC_INIT_MODIFY_ADVERTISED, _RC)

      call resolve_framework_geometry(this, geometry_ready, _RC)
      call process_connections(this, skip_framework=.not. geometry_ready, _RC)
      call this%registry%propagate_exports(_RC)
      _RETURN_UNLESS(geometry_ready)

      _RETURN(_SUCCESS)

      _UNUSED_DUMMY(unusable)
      _UNUSED_DUMMY(importState)
      _UNUSED_DUMMY(exportState)
      _UNUSED_DUMMY(clock)
   end subroutine initialize_modify_advertised

   subroutine process_connections(this, skip_framework, rc)
      class(OuterMetaComponent), target, intent(inout) :: this
      logical, optional, intent(in) :: skip_framework
      integer, optional, intent(out) :: rc

      integer :: status
      type(ConnectionVectorIterator) :: iter
      class(Connection), pointer :: c
      type(ConnectionPt) :: source, destination
      logical :: skip

      skip = .false.
      if (present(skip_framework)) skip = skip_framework

      associate (e => this%component_spec%connections%end())
         iter = this%component_spec%connections%begin()
         do while (iter /= e)
            c => iter%of()
            if (skip) then
               source = c%get_source()
               destination = c%get_destination()
               if (source%get_esmf_name() == MAPL_FRAMEWORK_NAMESPACE // 'geom_out' .and. &
                    destination%get_esmf_name() == MAPL_FRAMEWORK_NAMESPACE // 'geom_in') then
                  call iter%next()
                  cycle
               end if
            end if
            call c%connect(this%registry, _RC)
           call iter%next()
        end do
      end associate

      _RETURN(_SUCCESS)
   end subroutine process_connections

   subroutine process_framework_geometry_connections(this, rc)
      class(OuterMetaComponent), target, intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status
      type(ConnectionVectorIterator) :: iter
      class(Connection), pointer :: framework_conn
      type(ConnectionPt) :: source, destination

      associate (e => this%component_spec%connections%end())
         iter = this%component_spec%connections%begin()
         do while (iter /= e)
            framework_conn => iter%of()
            source = framework_conn%get_source()
            destination = framework_conn%get_destination()
            if (source%get_esmf_name() == MAPL_FRAMEWORK_NAMESPACE // 'geom_out' .and. &
                 destination%get_esmf_name() == MAPL_FRAMEWORK_NAMESPACE // 'geom_in') then
               call framework_conn%connect(this%registry, _RC)
            end if
            call iter%next()
         end do
      end associate

      _RETURN(_SUCCESS)
   end subroutine process_framework_geometry_connections

   subroutine resolve_framework_geometry(this, ready, rc)
      class(OuterMetaComponent), target, intent(inout) :: this
      logical, intent(out) :: ready
      integer, optional, intent(out) :: rc

      type(StateItemSpec), pointer :: primary
      type(StateItemAllocation) :: allocation_status
      class(StateItemAspect), pointer :: aspect
      type(GeometryClassAspect) :: geometry_aspect
      type(ESMF_Geom), allocatable :: geom
      class(VerticalGrid), pointer :: vertical_grid
      class(GriddedComponentDriver), pointer :: provider
      type(ESMF_GridComp) :: provider_gc
      type(OuterMetaComponent), pointer :: provider_meta
      integer :: status

      ready = .true.
      select case (this%component_spec%geometry_spec%kind)
      case (GEOMETRY_FROM_PARENT)
         primary => this%registry%get_primary_spec( &
              VirtualConnectionPt(ESMF_STATEINTENT_IMPORT, MAPL_FRAMEWORK_NAMESPACE // 'geom_in'), _RC)
         allocation_status = primary%get_allocation_status(_RC)
         if (allocation_status /= MAPL_STATEITEM_ALLOCATION_CONNECTED) then
            ready = .false.
            _RETURN(_SUCCESS)
         end if
         aspect => primary%get_aspect(CLASS_ASPECT_ID, _RC)
         geometry_aspect = to_GeometryClassAspect(aspect, _RC)
         geom = geometry_aspect%get_geom(_RC)
         if (.not. allocated(geom)) then
            ready = .false.
            _RETURN(_SUCCESS)
         end if
         this%geom = geom
         vertical_grid => geometry_aspect%get_vertical_grid(_RC)
         if (associated(vertical_grid)) this%vertical_grid = vertical_grid

      case (GEOMETRY_FROM_CHILD)
         provider => this%children%at(this%component_spec%geometry_spec%provider, _RC)
         provider_gc = provider%get_gridcomp()
         provider_meta => get_outer_meta(provider_gc, _RC)
         if (.not. provider_meta%has_geom()) then
            ready = .false.
            _RETURN(_SUCCESS)
         end if
         geom = provider_meta%get_geom(_RC)
         if (.not. allocated(geom)) then
            ready = .false.
            _RETURN(_SUCCESS)
         end if
         this%geom = geom
         vertical_grid => provider_meta%get_vertical_grid()
         if (associated(vertical_grid)) this%vertical_grid = vertical_grid
      end select

      _RETURN(_SUCCESS)
   end subroutine resolve_framework_geometry

end submodule initialize_modify_advertised_smod
