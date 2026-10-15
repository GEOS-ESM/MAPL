#include "MAPL.h"

submodule (mapl_OuterMetaComponent_mod) add_child_by_spec_smod

   use mapl_ComponentSpecParser_mod
   use mapl_Generic_api, only: MAPL_GridCompCreate
   use mapl_ChildSpec_mod
   use mapl_ChildSpecMap_mod
   use mapl_Validation_mod
   use mapl_MultiState_mod
   use mapl_HConfigUtilities_mod, only: merge_hconfig
   use mapl_HierarchyResources_mod, only: HierarchyResources
   use mapl_HierarchyRegistry_mod, only: get_hierarchy_resources
   use mapl_ErrorHandling_mod
   use esmf

   implicit none(type,external)

contains

   module recursive subroutine add_child_by_spec(this, child_name, child_spec, rc)
      class(OuterMetaComponent), target, intent(inout) :: this
      character(*), intent(in) :: child_name
      type(ChildSpec), intent(inout) :: child_spec
      integer, optional, intent(out) :: rc

      integer :: status
      type(GriddedComponentDriver) :: child_driver
      type(ESMF_GridComp) :: child_outer_gc
      type(ESMF_GridComp) :: child_user_gc
      type(OuterMetaComponent), pointer :: child_meta
      type(ESMF_HConfig) :: total_hconfig
      type(ESMF_Context_Flag) :: contextFlag
      class(Logger), pointer :: lgr
      character(:), allocatable :: this_name
      type(HierarchyResources), pointer :: resources

      _ASSERT(is_valid_name(child_name), 'Child name <' // child_name //'> does not conform to GEOS standards.')
      _ASSERT(this%children%count(child_name) == 0, 'duplicate child name: <'//child_name//'>.')
      ! openspec/changes/hierarchy-wide-component-graph, graph/component-
      ! hierarchy delta spec "Component names are unique across the
      ! entire hierarchy": sibling-level uniqueness alone (the assert
      ! above) is not sufficient - a name must not collide with any
      ! other component's name anywhere in this hierarchy, including a
      ! cousin several levels removed. The sibling check above still
      ! runs first, for a more specific error message in that common
      ! case.
      resources => get_hierarchy_resources(this%hierarchy_id)
      _ASSERT(resources%component_names%count(child_name) == 0, 'duplicate component name elsewhere in the hierarchy: <'//child_name//'>.')

      total_hconfig = merge_hconfig(this%hconfig, child_spec%hconfig, _RC)
      if (allocated(child_spec%user_setservices)) then
         ! openspec/changes/hierarchy-wide-component-graph, design.md D1:
         ! this child is constructed through the same public
         ! MAPL_GridCompCreate entry point ordinary user code calls to
         ! create a brand-new hierarchy's own root - passing this%
         ! hierarchy_id explicitly here is what keeps it in THIS
         ! hierarchy rather than letting it request a fresh one of its
         ! own (create_grid_comp_primary's own default when the
         ! argument is absent).
         child_outer_gc = MAPL_GridCompCreate(child_name, child_spec%user_setservices, total_hconfig, &
              hierarchy_id=this%hierarchy_id, rc=status)
         _VERIFY(status)
      else
         contextFlag = ESMF_CONTEXT_PARENT_VM
         child_outer_gc = ESMF_GridCompCreate(name='[' // trim(child_name) // ']', contextFlag=contextFlag, _RC)
         call set_is_generic(child_outer_gc, .true., _RC)

         child_user_gc = ESMF_GridCompCreate(name=child_name, contextFlag=contextFlag, _RC)
         call set_is_generic(child_user_gc, .false., _RC)

         call attach_outer_meta(child_outer_gc, _RC)
         child_meta => get_outer_meta(child_outer_gc, _RC)
         child_driver = GriddedComponentDriver(child_user_gc)
#ifndef __GFORTRAN__
         child_meta = OuterMetaComponent(child_outer_gc, child_driver, hconfig=total_hconfig)
#else
         call ridiculous(child_meta, OuterMetaComponent(child_outer_gc, child_driver, hconfig=total_hconfig))
#endif
         ! design.md D1: pass this%hierarchy_id down explicitly, so the
         ! child's own local_graph shares this component's hierarchy
         ! rather than requesting a fresh one of its own.
         call child_meta%init_meta(hierarchy_id=this%hierarchy_id, rc=status)
         _VERIFY(status)
      end if

      ! Meta stuff
      child_meta => get_outer_meta(child_outer_gc, _RC)
      call this%registry%add_subregistry(child_meta%get_registry())

      if (allocated(child_spec%timeStep)) child_meta%user_timeStep = child_spec%timeStep

      child_meta%user_offset = this%user_offset + child_spec%offset

      child_driver = GriddedComponentDriver(child_outer_gc)
      call this%children%insert(child_name, child_driver)
      ! Record this name in the hierarchy-wide registry (graph/
      ! component-hierarchy delta spec) only once the child is fully
      ! and successfully added - matching the sibling-level
      ! this%children%insert() above, not inserted speculatively before
      ! construction could still fail.
      call resources%component_names%insert(child_name)

      lgr => this%get_logger()
      this_name = this%get_name() ! workaround for gfortran
      call lgr%debug('%a added child <%a~>', this_name, child_name, _RC)

      _RETURN(_SUCCESS)

   contains

#ifdef __GFORTRAN__

      subroutine ridiculous(a, b)
         type(OuterMetaComponent), intent(out) :: a
         type(OuterMetaComponent), intent(in) :: b

         a = b
      end subroutine ridiculous
#endif

      subroutine set_is_generic(gridcomp, flag, rc)
         type(ESMF_GridComp), intent(inout) :: gridcomp
         logical, intent(in) :: flag
         integer, optional, intent(out) :: rc

         integer :: status
         type(ESMF_Info) :: info

         call ESMF_InfoGetFromHost(gridcomp, info, _RC)
         call ESMF_InfoSet(info, key='MAPL/GRIDCOMP_IS_GENERIC', value=flag, _RC)

         _RETURN(_SUCCESS)
      end subroutine set_is_generic
    end subroutine add_child_by_spec

end submodule add_child_by_spec_smod
