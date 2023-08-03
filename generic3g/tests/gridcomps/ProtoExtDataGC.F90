#include "MAPL_ErrLog.h"

! See external setservices() procedure at end of file


module ProtoExtDataGC
   use mapl_ErrorHandling
   use mapl3g_OuterMetaComponent
   use mapl3g_GenericConfig
   use mapl3g_Generic
   use mapl3g_UserSetServices
   use mapl3g_HierarchicalRegistry
   use mapl3g_VirtualConnectionPt
   use mapl3g_ActualConnectionPt
   use mapl3g_ConnectionPt
   use mapl3g_SimpleConnection
   use mapl3g_AbstractStateItemSpec
   use esmf
   use pFlogger
   implicit none
   private

   public :: setservices
   
contains

   subroutine setservices(gc, rc)
      use mapl3g_Generic, only: MAPL_GridCompSetEntryPoint
      type(ESMF_GridComp) :: gc
      integer, intent(out) :: rc

      integer :: status

      call MAPL_GridCompSetEntryPoint(gc, ESMF_METHOD_RUN, run, _RC)
      call MAPL_GridCompSetEntryPoint(gc, ESMF_METHOD_INITIALIZE, init_post_advertise, phase_name='GENERIC::INIT_POST_ADVERTISE', _RC)

      _RETURN(ESMF_SUCCESS)
   end subroutine setservices

   
   subroutine init_post_advertise(gc, importState, exportState, clock, rc)
      type(ESMF_GridComp) :: gc
      type(ESMF_State) :: importState
      type(ESMF_State) :: exportState
      type(ESMF_Clock) :: clock
      integer, intent(out) :: rc

      type(OuterMetaComponent), pointer :: outer_meta
      integer :: status
      type(VirtualConnectionPt) :: export_v_pt, import_v_pt
      type(ActualConnectionPt) :: a_pt
      type(ConnectionPt) :: s_pt, d_pt
      type(SimpleConnection) :: conn
      type(HierarchicalRegistry), pointer :: registry
      class(AbstractStateItemSpec), pointer :: export_spec
      class(AbstractStateItemSpec), pointer :: import_spec
      class(Logger), pointer :: lgr

      outer_meta => get_outer_meta_from_inner_gc(gc, _RC)
      registry => outer_meta%get_registry()

      lgr => logging%get_logger('MAPL.generic3g.ProtoExtDataGC')
      call lgr%warning('Names are hardwired - should derive from config.')
      export_v_pt = VirtualConnectionPt(ESMF_STATEINTENT_EXPORT, 'E1')
      import_v_pt = VirtualConnectionPt(ESMF_STATEINTENT_IMPORT, 'E1')
      a_pt = ActualConnectionPt(export_v_pt)
      export_spec => registry%get_item_spec(a_pt, _RC)

      allocate(import_spec, source=export_spec)
!!$      import_spec = export_spec
      ! Need new payload ... (but maybe not as it will get tossed at connect() anyway.)
      call import_spec%create([StateItemSpecPtr::], _RC)
      call registry%add_item_spec(import_v_pt, import_spec)

      ! And now connect
      export_v_pt = VirtualConnectionPt(ESMF_STATEINTENT_EXPORT, 'E1')
      s_pt = ConnectionPt('collection_1', export_v_pt)
      d_pt = ConnectionPt('<self>', import_v_pt)
      conn = SimpleConnection(source=s_pt, destination=d_pt)

      call registry%add_connection(conn, _RC)

      _RETURN(ESMF_SUCCESS)
   end subroutine init_post_advertise


   subroutine run(gc, importState, exportState, clock, rc)
      type(ESMF_GridComp) :: gc
      type(ESMF_State) :: importState
      type(ESMF_State) :: exportState
      type(ESMF_Clock) :: clock
      integer, intent(out) :: rc

      type(OuterMetaComponent), pointer :: outer_meta
      integer :: status

      outer_meta => get_outer_meta_from_inner_gc(gc, _RC)
      call outer_meta%run_children(clock, _RC)
      
      _RETURN(ESMF_SUCCESS)
   end subroutine run
   
   subroutine init(gc, importState, exportState, clock, rc)
      type(ESMF_GridComp) :: gc
      type(ESMF_State) :: importState
      type(ESMF_State) :: exportState
      type(ESMF_Clock) :: clock
      integer, intent(out) :: rc

      
      _RETURN(ESMF_SUCCESS)
   end subroutine init

end module ProtoExtDataGC

subroutine setServices(gc, rc)
   use esmf, only: ESMF_GridComp
   use esmf, only: ESMF_SUCCESS
   use mapl_ErrorHandling
   use ProtoExtDataGC, only: inner_setservices => setservices
   type(ESMF_GridComp) :: gc
   integer, intent(out) :: rc

   integer :: status

   call inner_setservices(gc, _RC)

   _RETURN(ESMF_SUCCESS)
end subroutine setServices
