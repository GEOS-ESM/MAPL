#include "MAPL_Generic.h"

submodule (mapl3g_OuterMetaComponent) initialize_advertise_smod
   implicit none

contains

   module recursive subroutine initialize_advertise(this, unusable, rc)
      class(OuterMetaComponent), intent(inout) :: this
      ! optional arguments
      class(KE), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status
      character(*), parameter :: PHASE_NAME = 'GENERIC::INIT_ADVERTISE'

      call apply_to_children(this, set_child_geom, _RC)


      call this%run_custom(ESMF_METHOD_INITIALIZE, PHASE_NAME, _RC)
      call self_advertise(this, _RC)
      call recurse(this, phase_idx=GENERIC_INIT_ADVERTISE, _RC)

      call process_connections(this, _RC)
      call this%registry%propagate_unsatisfied_imports(_RC)
      call this%registry%propagate_exports(_RC)

      _RETURN(ESMF_SUCCESS)
      _UNUSED_DUMMY(unusable)
   contains

      subroutine set_child_geom(this, child_meta, rc)
         class(OuterMetaComponent), target, intent(inout) :: this
         type(OuterMetaComponent), target, intent(inout) ::  child_meta
         integer, optional, intent(out) :: rc

         integer :: status

         associate(kind => child_meta%component_spec%geometry_spec%kind)
           _RETURN_IF(kind /= GEOMETRY_FROM_PARENT)

           if (allocated(this%geom)) then
              call child_meta%set_geom(this%geom)
           end if
           if (allocated(this%vertical_grid)) then
              call child_meta%set_vertical_grid(this%vertical_grid)
           end if
         end associate

         _RETURN(ESMF_SUCCESS)
      end subroutine set_child_geom

      subroutine self_advertise(this, unusable, rc)
         class(OuterMetaComponent), intent(inout) :: this
         class(KE), optional, intent(in) :: unusable
         integer, optional, intent(out) :: rc

         integer :: status
         type(VariableSpecVectorIterator) :: iter
         type(VariableSpec), pointer :: var_spec

!#         if (this%component_spec%var_specs%size() > 0) then
!#            _ASSERT(allocated(this%geom),'Component must define a geom to advertise variables.')
!#         end if
         associate (e => this%component_spec%var_specs%end())
           iter = this%component_spec%var_specs%begin()
           do while (iter /= e)
              var_spec => iter%of()
              call advertise_variable (var_spec, this%registry, this%geom, this%vertical_grid,  _RC)
              call iter%next()
           end do
         end associate

         _RETURN(_SUCCESS)
         _UNUSED_DUMMY(unusable)
      end subroutine self_advertise


      subroutine advertise_variable(var_spec, registry, geom, vertical_grid, unusable, rc)
         type(VariableSpec), intent(in) :: var_spec
         type(StateRegistry), intent(inout) :: registry
         type(ESMF_Geom), optional, intent(in) :: geom
         class(VerticalGrid), optional, intent(in) :: vertical_grid
         class(KE), optional, intent(in) :: unusable
         integer, optional, intent(out) :: rc

         integer :: status
         class(StateItemSpec), allocatable :: item_spec
         type(VirtualConnectionPt) :: virtual_pt
         integer :: i

         _ASSERT(var_spec%itemtype /= MAPL_STATEITEM_UNKNOWN, 'Invalid type id in variable spec <'//var_spec%short_name//'>.')

         allocate(item_spec, source=var_spec%make_ItemSpec(geom, vertical_grid, registry, rc=status)); _VERIFY(status)
         call item_spec%create(_RC)

         virtual_pt = var_spec%make_virtualPt()
         call registry%add_primary_spec(virtual_pt, item_spec)

         _RETURN(_SUCCESS)
         _UNUSED_DUMMY(unusable)
      end subroutine advertise_variable


      subroutine process_connections(this, rc)
        class(OuterMetaComponent), intent(inout) :: this
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
  end subroutine initialize_advertise

end submodule initialize_advertise_smod
