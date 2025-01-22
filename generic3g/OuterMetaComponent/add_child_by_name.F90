#include "MAPL_Generic.h"

submodule (mapl3g_OuterMetaComponent) add_child_by_name_smod
   use mapl3g_ComponentSpecParser
   use mapl3g_ChildSpec
   use mapl3g_ChildSpecMap
   use mapl3g_GenericGridComp
   use mapl3g_Validation
   use mapl3g_Multistate
   use mapl_ErrorHandling
   implicit none

contains

   module recursive subroutine add_child_by_name(this, child_name, setservices, hconfig, rc)
      class(OuterMetaComponent), target, intent(inout) :: this
      character(len=*), intent(in) :: child_name
      class(AbstractUserSetServices), intent(in) :: setservices
      type(ESMF_Hconfig), intent(in) :: hconfig
      integer, optional, intent(out) :: rc

      integer :: status
      type(GriddedComponentDriver) :: child_gc_driver
      type(ESMF_GridComp) :: child_gc
      type(ESMF_Clock) :: clock, child_clock
      type(GriddedComponentDriver), pointer :: child
      type(OuterMetaComponent), pointer :: child_meta
      type(ESMF_GridComp) :: child_outer_gc

      _ASSERT(is_valid_name(child_name), 'Child name <' // child_name //'> does not conform to GEOS standards.')

      ! By default, children run with the same timestep as their
      ! parent.  This can be overridden by the MAPL generic layer
      ! which will check for `timestep` in the MAPL section of the
      ! resource file.
      clock = this%user_gc_driver%get_clock()
      child_clock = ESMF_ClockCreate(clock, _RC)
      call ESMF_ClockSet(child_clock, name=this%get_name()//'_outer', _RC)
      child_gc = create_grid_comp(child_name, setservices, hconfig, child_clock, _RC)

      child_gc_driver = GriddedComponentDriver(child_gc, child_clock, MultiState())

      _ASSERT(this%children%count(child_name) == 0, 'duplicate child name: <'//child_name//'>.')
      call this%children%insert(child_name, child_gc_driver)

      ! add subregistry
      child => this%children%of(child_name)
      child_outer_gc = child%get_gridcomp()
      child_meta => get_outer_meta(child_outer_gc, _RC)
      call this%registry%add_subregistry(child_meta%get_registry())

      _RETURN(ESMF_SUCCESS)
   end subroutine add_child_by_name


end submodule add_child_by_name_smod
