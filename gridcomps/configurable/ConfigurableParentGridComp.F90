#include "MAPL_Generic.h"

module ConfigurableParentGridComp
   use generic3g
   use mapl_ErrorHandling
   use pFlogger, only: logger
   use esmf
   implicit none (type, external)
   private

   public :: setServices

contains

   subroutine setServices(gridcomp, rc)
      use mapl3g_BasicVerticalGrid
      type(ESMF_GridComp) :: gridcomp
      integer, intent(out) :: rc

      type(ESMF_HConfig) :: hconfig, collections_config, child_hconfig
      character(len=:), allocatable :: child_name, collection_name
      type(ESMF_HConfigIter) :: iter, iter_begin, iter_end
      logical :: has_active_collections
      class(logger), pointer :: lgr
      integer :: num_collections, status
      type(BasicVerticalGrid) :: vertical_grid
      type(ESMF_GridComp) outer_gridcomp
      type(OuterMetaComponent), pointer :: outer_meta

      call MAPL_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_INITIALIZE, init, _RC)
      call MAPL_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_RUN, run, phase_name='run', _RC)

      outer_meta => get_outer_meta_from_inner_gc(gridcomp,_RC)
      vertical_grid = BasicVerticalGrid(4)
      call outer_meta%set_vertical_grid(vertical_grid)


      _RETURN(_SUCCESS)
   end subroutine setServices

   subroutine init(gridcomp, importState, exportState, clock, rc)
      type(ESMF_GridComp)   :: gridcomp
      type(ESMF_State)      :: importState
      type(ESMF_State)      :: exportState
      type(ESMF_Clock)      :: clock
      integer, intent(out)  :: rc

      integer :: status

      _RETURN(_SUCCESS)
   end subroutine init

   subroutine run(gridcomp, importState, exportState, clock, rc)
      !use mapl3g_MultiState
      type(ESMF_GridComp)   :: gridcomp
      type(ESMF_State)      :: importState
      type(ESMF_State)      :: exportState
      type(ESMF_Clock)      :: clock
      integer, intent(out)  :: rc

      integer :: status
      character(len=ESMF_MAXSTR) :: gc_name
      call ESMF_GridCompGet(gridcomp, name=gc_name, _RC)
      print*,'running ',trim(gc_name)
      call MAPL_RunChildren(gridcomp, phase_name='run', _RC)
      _RETURN(_SUCCESS)
   end subroutine run

end module ConfigurableParentGridComp

subroutine setServices(gridcomp,rc)
   use ESMF
   use MAPL_ErrorHandlingMod
   use ConfigurableParentGridComp, only: ConfigurableParent_setServices => SetServices
   type(ESMF_GridComp)  :: gridcomp
   integer, intent(out) :: rc

   integer :: status

   call ConfigurableParent_setServices(gridcomp,_RC)
   _RETURN(_SUCCESS)

end subroutine

