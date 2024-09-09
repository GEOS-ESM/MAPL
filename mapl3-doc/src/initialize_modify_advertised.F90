#include "MAPL_Generic.h"

submodule (mapl3g_OuterMetaComponent) initialize_modify_advertised_smod
   implicit none

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
      character(*), parameter :: PHASE_NAME = 'GENERIC::INIT_MODIFY_ADVERTISED'

      call apply_to_children(this, set_child_geom, _RC)
      call this%run_custom(ESMF_METHOD_INITIALIZE, PHASE_NAME, _RC)
      call recurse(this, phase_idx=GENERIC_INIT_MODIFY_ADVERTISED, _RC)

      call self_advertise(this, _RC)
      call process_connections(this, _RC)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
      _UNUSED_DUMMY(importState)
      _UNUSED_DUMMY(exportState)
      _UNUSED_DUMMY(clock)
   contains

      subroutine set_child_geom(this, child_meta, rc)
         class(OuterMetaComponent), target, intent(inout) :: this
         type(OuterMetaComponent), target, intent(inout) ::  child_meta
         integer, optional, intent(out) :: rc
         
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

   end subroutine initialize_modify_advertised
   

   subroutine self_advertise(this, unusable, rc)
      class(OuterMetaComponent), target, intent(inout) :: this
      class(KE), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc
      
      integer :: status

      call this%registry%set_blanket_geometry(this%geom, this%vertical_grid, _RC)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine self_advertise

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
           call c%connect(this%registry, _RC)
           call iter%next()
        end do
      end associate

      _RETURN(_SUCCESS)
   end subroutine process_connections

end submodule initialize_modify_advertised_smod
