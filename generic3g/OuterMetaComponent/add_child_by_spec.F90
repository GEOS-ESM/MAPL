#include "MAPL.h"

submodule (mapl3g_OuterMetaComponent) add_child_by_spec_smod

   use mapl3g_ComponentSpecParser
   use mapl3g_GenericGridComp
   use mapl3g_ChildSpec
   use mapl3g_ChildSpecMap
   use mapl3g_GenericGridComp
   use mapl3g_Validation
   use mapl3g_Multistate
   use mapl3g_HConfigUtilities, only: merge_hconfig
   use mapl_ErrorHandling
   use esmf

   implicit none(type,external)

contains

   module recursive subroutine add_child_by_spec(this, child_name, child_spec, rc)
      class(OuterMetaComponent), target, intent(inout) :: this
      character(*), intent(in) :: child_name
#if defined(ESMF_HCONFIGSET_HAS_INTENT_INOUT)
      type(ChildSpec), intent(inout) :: child_spec
#else
      type(ChildSpec), intent(in) :: child_spec
#endif
      integer, optional, intent(out) :: rc

      integer :: status
      type(GriddedComponentDriver) :: child_driver
      type(ESMF_GridComp) :: child_outer_gc
      type(OuterMetaComponent), pointer :: child_meta
      type(ESMF_HConfig) :: total_hconfig
      class(Logger), pointer :: lgr
      character(:), allocatable :: this_name

      _ASSERT(is_valid_name(child_name), 'Child name <' // child_name //'> does not conform to GEOS standards.')
      _ASSERT(this%children%count(child_name) == 0, 'duplicate child name: <'//child_name//'>.')

      total_hconfig = merge_hconfig(this%hconfig, child_spec%hconfig, _RC)
      child_outer_gc = MAPL_GridCompCreate(child_name, child_spec%user_setservices, total_hconfig, _RC)

      ! Meta stuff
      child_meta => get_outer_meta(child_outer_gc, _RC)
      call this%registry%add_subregistry(child_meta%get_registry())

      if (allocated(child_spec%timeStep)) child_meta%user_timeStep = child_spec%timeStep

      child_meta%user_offset = this%user_offset + child_spec%offset

      child_driver = GriddedComponentDriver(child_outer_gc)
      call this%children%insert(child_name, child_driver)

      lgr => this%get_logger()
      this_name = this%get_name() ! workaround for gfortran
      call lgr%debug('%a added child <%a~>', this_name, child_name, _RC)

      _RETURN(_SUCCESS)
   end subroutine add_child_by_spec

end submodule add_child_by_spec_smod
