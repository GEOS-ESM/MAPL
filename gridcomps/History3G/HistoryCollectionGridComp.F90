#include "MAPL_Generic.h"

module mapl3g_HistoryCollectionGridComp
   use mapl_ErrorHandlingMod
   use generic3g
   use mapl3g_esmf_utilities
   use mapl3g_HistoryCollectionGridComp_private
   use esmf
   implicit none
   private

   public :: setServices

   ! Private state
   type :: HistoryCollectionGridComp
!#      class(Client), pointer :: client
   end type HistoryCollectionGridComp


contains

   subroutine setServices(gridcomp, rc)
      type(ESMF_GridComp) :: gridcomp
      integer, intent(out) :: rc

      type(HistoryCollectionGridComp), pointer :: collection_gridcomp
      type(ESMF_HConfig) :: hconfig
      character(*), parameter :: PRIVATE_STATE = "HistoryCollectionGridComp"
      integer :: status

      type(VerticalGeom) :: vertical_geom
      type(OuterMetaComponent), pointer :: outer_meta

      ! Set entry points
      call MAPL_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_INITIALIZE, init_geom, phase_name='GENERIC::INIT_ADVERTISE_GEOM', _RC)
      call MAPL_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_INITIALIZE, init, _RC)
      call MAPL_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_INITIALIZE, init_geom, phase_name='GENERIC_RUN_UPDATE_GEOM', _RC)
      call MAPL_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_RUN, run, phase_name='run', _RC)

      ! Attach private state
      _SET_NAMED_PRIVATE_STATE(gridcomp, HistoryCollectionGridComp, PRIVATE_STATE, collection_gridcomp)

      outer_meta => get_outer_meta_from_inner_gc(gridcomp,_RC)
      vertical_geom = VerticalGeom(4)
      call outer_meta%set_vertical_geom(vertical_geom)
      call MAPL_GridCompGet(gridcomp, hconfig=hconfig, _RC)
      call make_import_state(gridcomp,hconfig,_RC)

      _RETURN(_SUCCESS)
   end subroutine setServices

   subroutine init(gridcomp, importState, exportState, clock, rc)
      type(ESMF_GridComp)   :: gridcomp
      type(ESMF_State)      :: importState
      type(ESMF_State)      :: exportState
      type(ESMF_Clock)      :: clock
      integer, intent(out)  :: rc

      integer :: status

      ! To Do:
      ! - determine run frequencey and offset (save as alarm)
     
      _RETURN(_SUCCESS)
   end subroutine init


   subroutine init_geom(gridcomp, importState, exportState, clock, rc)
      type(ESMF_GridComp)   :: gridcomp
      type(ESMF_State)      :: importState
      type(ESMF_State)      :: exportState
      type(ESMF_Clock)      :: clock
      integer, intent(out)  :: rc

      integer :: status
      type(ESMF_HConfig) :: hconfig
      type(ESMF_Geom) :: geom

      call MAPL_GridCompGet(gridcomp, hconfig=hconfig, _RC)
      geom = make_geom(hconfig)
      call MAPL_GridCompSetGeom(gridcomp, geom, _RC)

      _RETURN(_SUCCESS)
   end subroutine init_geom

   subroutine run(gridcomp, importState, exportState, clock, rc)
      type(ESMF_GridComp)   :: gridcomp
      type(ESMF_State)      :: importState
      type(ESMF_State)      :: exportState
      type(ESMF_Clock)      :: clock
      integer, intent(out)  :: rc

      integer :: status
      real(kind=ESMF_KIND_R4), pointer :: ptr(:,:)
      type(ESMF_Field) :: field

      _RETURN(_SUCCESS)
   end subroutine run

end module mapl3g_HistoryCollectionGridComp
