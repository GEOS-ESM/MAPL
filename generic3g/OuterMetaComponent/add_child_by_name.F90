#include "MAPL_Generic.h"

submodule (mapl3g_OuterMetaComponent) add_child_by_name_smod
   use esmf
   use gFTL2_StringVector
   use mapl3g_ComponentSpecParser
   use mapl3g_HierarchicalRegistry
   use mapl3g_ChildSpec
   use mapl3g_ChildSpecMap
   use mapl3g_GenericGridComp
   ! Kludge to work around Intel 2021 namespace bug that exposes
   ! private names from other modules in unrelated submodules.
   ! Report filed 2022-03-14 (T. Clune)
   use mapl_keywordenforcer, only: KE => KeywordEnforcer
   implicit none

contains

   module recursive subroutine add_child_by_name(this, child_name, setservices, hconfig, rc)
      use mapl3g_GenericGridComp, only: generic_setservices => setservices
      class(OuterMetaComponent), intent(inout) :: this
      character(len=*), intent(in) :: child_name
      class(AbstractUserSetServices), intent(in) :: setservices
      type(ESMF_Hconfig), intent(in) :: hconfig
      integer, optional, intent(out) :: rc

      integer :: status
      type(GriddedComponentDriver) :: child_gc_driver
      type(ESMF_GridComp) :: child_gc
      type(ESMF_Clock) :: clock, child_clock

      _ASSERT(is_valid_name(child_name), 'Child name <' // child_name //'> does not conform to GEOS standards.')

      clock = this%user_gc_driver%get_clock()
      child_clock = ESMF_ClockCreate(clock, _RC)
      child_gc = create_grid_comp(child_name, setservices, hconfig, clock, _RC)

      child_gc_driver = GriddedComponentDriver(child_gc, child_clock, MultiState())

      _ASSERT(this%children%count(child_name) == 0, 'duplicate child name: <'//child_name//'>.')
      call this%children%insert(child_name, child_gc_driver)

      _RETURN(ESMF_SUCCESS)
   end subroutine add_child_by_name


end submodule add_child_by_name_smod
