#include "MAPL_Generic.h"

submodule (mapl3g_OuterMetaComponent) initialize_advertise_smod
   use mapl3g_GenericPhases, only: GENERIC_INIT_ADVERTISE
   use mapl3g_VirtualConnectionPt
   use mapl3g_StateItem
   use mapl3g_VariableSpec
   use mapl3g_VariableSpecVector, only: VariableSpecVectorIterator
   use mapl3g_make_ItemSpec, only: make_ItemSpec
   use esmf, only: operator(==)
   use mapl3g_Connection
   use mapl3g_ConnectionVector, only: ConnectionVectorIterator
   use mapl3g_ConnectionVector, only: operator(/=)
   use mapl3g_VariableSpecVector, only: operator(/=)
   use mapl3g_geom_mgr
   use mapl3g_GeometrySpec
   use mapl3g_StateItemSpec
   use mapl_ErrorHandling
   implicit none (type, external)


contains

   module recursive subroutine initialize_advertise(this, unusable, rc)
      class(OuterMetaComponent), target, intent(inout) :: this
      ! optional arguments
      class(KE), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status
      class(GriddedComponentDriver), pointer :: provider
      type(ESMF_GridComp) :: provider_gc
      type(OuterMetaComponent), pointer :: provider_meta
      type(MaplGeom), pointer :: mapl_geom
      type(GeomManager), pointer :: geom_mgr
      character(*), parameter :: PHASE_NAME = 'GENERIC::INIT_ADVERTISE'

      call this%run_custom(ESMF_METHOD_INITIALIZE, PHASE_NAME, _RC)
      call self_advertise(this, _RC)

      associate (geometry_spec => this%component_spec%geometry_spec)
        if (allocated(geometry_spec%geom_spec)) then
           geom_mgr => get_geom_manager()
           mapl_geom => geom_mgr%get_mapl_geom(geometry_spec%geom_spec, _RC)
           this%geom = mapl_geom%get_geom()
        end if
        if (allocated(geometry_spec%vertical_grid)) then
           this%vertical_grid = geometry_spec%vertical_grid
        end if
      end associate

      call recurse(this, phase_idx=GENERIC_INIT_ADVERTISE, _RC)

      associate (geometry_spec => this%component_spec%geometry_spec)
        if (geometry_spec%kind == GEOMETRY_FROM_CHILD) then
           provider => this%children%at(geometry_spec%provider, _RC)
           provider_gc = provider%get_gridcomp()
           provider_meta => get_outer_meta(provider_gc, _RC)
           _ASSERT(allocated(provider_meta%geom), 'Specified child does not provide a geom.')
           this%geom = provider_meta%geom
           this%vertical_grid = provider_meta%vertical_grid
        end if
      end associate

      call process_connections(this, _RC)
      call this%registry%propagate_unsatisfied_imports(_RC)
      call this%registry%propagate_exports(_RC)

      _RETURN(ESMF_SUCCESS)
      _UNUSED_DUMMY(unusable)
  end subroutine initialize_advertise

      subroutine self_advertise(this, unusable, rc)
         class(OuterMetaComponent), target, intent(inout) :: this
         class(KE), optional, intent(in) :: unusable
         integer, optional, intent(out) :: rc

         integer :: status
         type(VariableSpecVectorIterator) :: iter
         type(VariableSpec), pointer :: var_spec

         associate (e => this%component_spec%var_specs%end())
           iter = this%component_spec%var_specs%begin()
           do while (iter /= e)
              var_spec => iter%of()
              call advertise_variable (var_spec, this%registry, this%component_spec%activate_all_exports, _RC)
              call iter%next()
           end do
         end associate

         _RETURN(_SUCCESS)
         _UNUSED_DUMMY(unusable)
      end subroutine self_advertise


      subroutine advertise_variable(var_spec, registry, activate_all_exports, unusable, rc)
         type(VariableSpec), intent(in) :: var_spec
         type(StateRegistry), target, intent(inout) :: registry
         logical, intent(in) :: activate_all_exports
         class(KE), optional, intent(in) :: unusable
         integer, optional, intent(out) :: rc

         integer :: status
         type(StateItemSpec) :: item_spec
         type(VirtualConnectionPt) :: virtual_pt

         _ASSERT(var_spec%itemtype /= MAPL_STATEITEM_UNKNOWN, 'Invalid type id in variable spec <'//var_spec%short_name//'>.')

         item_spec = make_ItemSpec(var_spec, registry, _RC)
         call item_spec%create(_RC)

         if (activate_all_exports) then
            if (var_spec%state_intent == ESMF_STATEINTENT_EXPORT) then
               call item_spec%set_active()
            end if
         end if
               
         if (var_spec%state_intent == ESMF_STATEINTENT_INTERNAL) then
            call item_spec%set_active()
         end if

         virtual_pt = var_spec%make_virtualPt()
         call registry%add_primary_spec(virtual_pt, item_spec)


         _RETURN(_SUCCESS)
         _UNUSED_DUMMY(unusable)
      end subroutine advertise_variable

      subroutine process_connections(this, rc)
        class(OuterMetaComponent), target, intent(inout) :: this
        integer, optional, intent(out) :: rc

        integer :: status
        type(ConnectionVectorIterator) :: iter
        class(Connection), pointer :: c

        associate (e => this%component_spec%connections%end())
          iter = this%component_spec%connections%begin()
          do while (iter /= e)
             c => iter%of()
             call c%activate(this%registry, _RC)
             call iter%next()
          end do
        end associate

        _RETURN(_SUCCESS)
     end subroutine process_connections


end submodule initialize_advertise_smod
