#include "MAPL.h"

submodule (mapl_OuterMetaComponent_mod) create_child_smod

   use mapl_ComponentSpecParser_mod
   use mapl_Generic_api, only: MAPL_GridCompCreate
   use mapl_ChildSpec_mod
   use mapl_ChildSpecMap_mod
   use mapl_Validation_mod
   use mapl_MultiState_mod
   use mapl_HConfigUtilities_mod, only: merge_hconfig
   use mapl_ErrorHandling_mod
   use esmf

   implicit none(type,external)

contains

   ! Creates a child component and configures it from the parent-supplied
   ! values: merged hconfig, registry linkage, timestep and offset. The caller
   ! owns the parent's child map and inserts child_driver itself.
   module recursive subroutine create_child(child_name, child_spec, &
        parent_hconfig, parent_registry, parent_offset, child_driver, rc)
      character(*), intent(in) :: child_name
      type(ChildSpec), intent(inout) :: child_spec
      type(ESMF_HConfig), intent(in) :: parent_hconfig
      type(StateRegistry), intent(inout) :: parent_registry
      type(ESMF_TimeInterval), intent(in) :: parent_offset
      type(GriddedComponentDriver), intent(out) :: child_driver
      integer, optional, intent(out) :: rc

      integer :: status
      type(GriddedComponentDriver) :: user_gc_driver
      type(ESMF_GridComp) :: child_outer_gc
      type(ESMF_GridComp) :: child_user_gc
      type(OuterMetaComponent), pointer :: child_meta
      type(ESMF_HConfig) :: total_hconfig
      type(ESMF_Context_Flag) :: contextFlag

      _ASSERT(is_valid_name(child_name), 'Child name <' // child_name //'> does not conform to GEOS standards.')

      total_hconfig = merge_hconfig(parent_hconfig, child_spec%hconfig, _RC)
      if (allocated(child_spec%user_setservices)) then
         child_outer_gc = MAPL_GridCompCreate(child_name, child_spec%user_setservices, total_hconfig, _RC)
      else
         contextFlag = ESMF_CONTEXT_PARENT_VM
         child_outer_gc = ESMF_GridCompCreate(name='[' // trim(child_name) // ']', contextFlag=contextFlag, _RC)
         call set_is_generic(child_outer_gc, .true., _RC)

         child_user_gc = ESMF_GridCompCreate(name=child_name, contextFlag=contextFlag, _RC)
         call set_is_generic(child_user_gc, .false., _RC)

         call attach_outer_meta(child_outer_gc, _RC)
         child_meta => get_outer_meta(child_outer_gc, _RC)
         user_gc_driver = GriddedComponentDriver(child_user_gc)
#ifndef __GFORTRAN__
         child_meta = OuterMetaComponent(child_outer_gc, user_gc_driver, hconfig=total_hconfig)
#else
         call ridiculous(child_meta, OuterMetaComponent(child_outer_gc, user_gc_driver, hconfig=total_hconfig))
#endif
         call child_meta%init_meta(_RC)
      end if

      ! Meta stuff
      child_meta => get_outer_meta(child_outer_gc, _RC)
      call parent_registry%add_subregistry(child_meta%get_registry())

      if (allocated(child_spec%timeStep)) child_meta%user_timeStep = child_spec%timeStep

      child_meta%user_offset = parent_offset + child_spec%offset

      child_driver = GriddedComponentDriver(child_outer_gc)

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
   end subroutine create_child

end submodule create_child_smod
