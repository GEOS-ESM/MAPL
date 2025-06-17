#include "MAPL_Generic.h"

submodule (mapl3g_OuterMetaComponent) add_child_by_spec_smod
   use mapl3g_ComponentSpecParser
   use mapl3g_GenericGridComp
   use mapl3g_ChildSpec
   use mapl3g_ChildSpecMap
   use mapl3g_GenericGridComp
   use mapl3g_Validation
   use mapl3g_Multistate
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
      call lgr%debug('%a added child <%a~>', this%get_name(), child_name, _RC)

      _RETURN(_SUCCESS)
   end subroutine add_child_by_spec

   ! Merge two hconfigs
   ! 1) Do not include parent `mapl` section
   ! 2) Duplicate keys defer to those of the child
   function merge_hconfig(parent_hconfig, child_hconfig, rc) result(total_hconfig)
      type(ESMF_HConfig) :: total_hconfig
      type(ESMF_HConfig), intent(in) :: parent_hconfig
#if defined(ESMF_HCONFIGSET_HAS_INTENT_INOUT)
      type(ESMF_HConfig), intent(inout) :: child_hconfig
#else
      type(ESMF_HConfig), intent(in) :: child_hconfig
#endif
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_HConfigIter) :: iter_begin, iter_end, iter
      character(:), allocatable :: key
      type(ESMF_HConfig) :: val
      logical :: duplicate_key

      _ASSERT(ESMF_HConfigIsMap(parent_hconfig), 'parent hconfig must be a mapping.')
      _ASSERT(ESMF_HConfigIsMap(child_hconfig), 'childhconfig must be a mapping.')

      total_hconfig = ESMF_HConfigCreate(child_hconfig, _RC)

      iter_begin = ESMF_HConfigIterBegin(parent_hconfig, rc=rc)
      iter_end = ESMF_HConfigIterEnd(parent_hconfig, rc=rc)
      iter = iter_begin
      do while (ESMF_HConfigIterLoop(iter, iter_begin, iter_end, rc=status))
         _VERIFY(status)

         ! ignore mapl section
         key = ESMF_HConfigAsStringMapKey(iter, rc=rc)
         if (key == MAPL_SECTION) cycle

         ! ignore duplicate key
         duplicate_key = ESMF_HConfigIsDefined(child_hconfig, keystring=key, _RC)
         if (duplicate_key) cycle

         val = ESMF_HConfigCreateAtMapVal(iter, _RC)
         call ESMF_HConfigSet(child_hconfig, keystring=key, content=val, _RC)
      end do

      _RETURN(_SUCCESS)
   end function merge_hconfig

end submodule add_child_by_spec_smod
