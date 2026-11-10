#include "MAPL.h"

submodule (mapl_OuterMetaComponent_mod) add_child_by_spec_smod

   use mapl_ChildSpec_mod
   use mapl_ErrorHandling_mod
   use esmf

   implicit none(type,external)

contains

   ! Thin wrapper over create_child: owns the duplicate check, the child map
   ! and the logging. The construction itself is shared with other parent
   ! meta types.
   module recursive subroutine add_child_by_spec(this, child_name, child_spec, rc)
      class(OuterMetaComponent), target, intent(inout) :: this
      character(*), intent(in) :: child_name
      type(ChildSpec), intent(inout) :: child_spec
      integer, optional, intent(out) :: rc

      integer :: status
      type(GriddedComponentDriver) :: child_driver
      class(Logger), pointer :: lgr
      character(:), allocatable :: this_name

      _ASSERT(this%children%count(child_name) == 0, 'duplicate child name: <'//child_name//'>.')

      call create_child(child_name, child_spec, this%hconfig, this%registry, &
           this%user_offset, child_driver, _RC)

      call this%children%insert(child_name, child_driver)

      lgr => this%get_logger()
      this_name = this%get_name() ! workaround for gfortran
      call lgr%debug('%a added child <%a~>', this_name, child_name, _RC)

      _RETURN(_SUCCESS)

   end subroutine add_child_by_spec

end submodule add_child_by_spec_smod
