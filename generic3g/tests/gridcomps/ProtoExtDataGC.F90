#include "MAPL_ErrLog.h"

! See external setservices() procedure at end of file


module ProtoExtDataGC
   use mapl_ErrorHandling
   use mapl3g_OuterMetaComponent
   use mapl3g_Generic
   use mapl3g_UserSetServices
   use mapl3g_StateRegistry, only: StateRegistry
   use mapl3g_VirtualConnectionPt
   use mapl3g_ActualConnectionPt
   use mapl3g_ConnectionPt
   use mapl3g_SimpleConnection
   use mapl3g_StateItemSpec
   use mapl3g_StateItemExtension
   use mapl3g_ESMF_Subset
   use MAPL_FieldUtils
   use esmf, only: ESMF_StateGet, ESMF_FieldGet

   implicit none (type, external)
   private

   public :: setservices
   
contains

   subroutine setservices(gc, rc)
      use mapl3g_Generic, only: MAPL_GridCompSetEntryPoint
      type(ESMF_GridComp) :: gc
      integer, intent(out) :: rc

      integer :: status

      call MAPL_GridCompSetEntryPoint(gc, ESMF_METHOD_RUN, run, phase_name="run", _RC)
      call MAPL_GridCompSetEntryPoint(gc, ESMF_METHOD_INITIALIZE, init_modify_advertised, phase_name='GENERIC::INIT_MODIFY_ADVERTISED', _RC)

      _RETURN(ESMF_SUCCESS)
   end subroutine setservices

   
   subroutine init_modify_advertised(gc, importState, exportState, clock, rc)
      type(ESMF_GridComp) :: gc
      type(ESMF_State) :: importState
      type(ESMF_State) :: exportState
      type(ESMF_Clock) :: clock
      integer, intent(out) :: rc

      call step_A(gc, importState, exportState, clock, _RC)
      call step_B(gc, importState, exportState, clock, _RC)
      _RETURN(_SUCCESS)
   end subroutine init_modify_advertised

   subroutine step_A(gc, importState, exportState, cloc, rc)
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
      type(StateRegistry), pointer :: registry, collection_registry
      class(StateItemSpec), pointer :: export_spec
      class(StateItemSpec), pointer :: import_spec
      type(ESMF_HConfig) :: hconfig, states_spec, state_spec, mapl_config
      type(ESMF_HConfigIter) :: iter,e,b
      character(:), allocatable :: var_name
      type(StateItemExtension), pointer :: primary
      type(StateItemExtensionPtr), target, allocatable :: extensions(:)

      call MAPL_GridCompGet(gc, hconfig=hconfig, _RC)
      call MAPL_GridCompGetRegistry(gc, registry, _RC)

      ! We would do this quite differently in an actual ExtData implementation.
      ! Here we are using information from the generic spec.
      mapl_config = ESMF_HConfigCreateAt(hconfig, keystring='mapl', _RC)

      if (ESMF_HConfigIsDefined(mapl_config, keystring='states')) then
         states_spec = ESMF_HConfigCreateAt(mapl_config, keystring='states')
         if (ESMF_HConfigIsDefined(states_spec, keystring='export')) then
            state_spec = ESMF_HConfigCreateAt(states_spec, keystring='export')

            b = ESMF_HConfigIterBegin(state_spec)
            e = ESMF_HConfigIterEnd(state_spec) 
            iter = ESMF_HConfigIterBegin(state_spec)
            do while (ESMF_HConfigIterLoop(iter,b,e))
               var_name = ESMF_HConfigAsStringMapKey(iter,_RC)
               export_v_pt = VirtualConnectionPt(ESMF_STATEINTENT_EXPORT, var_name)
               import_v_pt = VirtualConnectionPt(ESMF_STATEINTENT_IMPORT, var_name)
               a_pt = ActualConnectionPt(export_v_pt)
               primary => registry%get_primary_extension(export_v_pt, _RC)
               export_spec => primary%get_spec()

               s_pt = ConnectionPt('collection_1', export_v_pt)
               collection_registry => registry%get_subregistry(s_pt, _RC)
               extensions = collection_registry%get_extensions(export_v_pt, _RC)
               export_spec => extensions(1)%ptr%get_spec()
               call export_spec%activate(_RC)
                 
            end do

         end if
      end if

      call ESMF_HConfigDestroy(mapl_config, _RC)


      _RETURN(ESMF_SUCCESS)
   end subroutine step_A

   subroutine step_B(gc, importState, exportState, clock, rc)
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
      type(StateRegistry), pointer :: registry
      class(StateItemSpec), pointer :: export_spec
      class(StateItemSpec), pointer :: import_spec
      type(ESMF_HConfig) :: hconfig, states_spec, state_spec, mapl_config
      type(ESMF_HConfigIter) :: iter,e,b
      character(:), allocatable :: var_name
      type(StateItemExtension), pointer :: primary

      call MAPL_GridCompGet(gc, hconfig=hconfig, _RC)
      call MAPL_GridCompGetRegistry(gc, registry, _RC)

      ! We would do this quite differently in an actual ExtData implementation.
      ! Here we are using information from the generic spec.
      mapl_config = ESMF_HConfigCreateAt(hconfig, keystring='mapl', _RC)
         
      if (ESMF_HConfigIsDefined(mapl_config, keystring='states')) then
         states_spec = ESMF_HConfigCreateAt(mapl_config, keystring='states')
         if (ESMF_HConfigIsDefined(states_spec, keystring='export')) then
            state_spec = ESMF_HConfigCreateAt(states_spec, keystring='export')

            b = ESMF_HConfigIterBegin(state_spec)
            e = ESMF_HConfigIterEnd(state_spec) 
            iter = ESMF_HConfigIterBegin(state_spec)
            do while (ESMF_HConfigIterLoop(iter,b,e))
               var_name = ESMF_HConfigAsStringMapKey(iter,_RC)
               export_v_pt = VirtualConnectionPt(ESMF_STATEINTENT_EXPORT, var_name)
               import_v_pt = VirtualConnectionPt(ESMF_STATEINTENT_IMPORT, var_name)
               a_pt = ActualConnectionPt(export_v_pt)
               primary => registry%get_primary_extension(export_v_pt, _RC)
               export_spec => primary%get_spec()


               allocate(import_spec, source=export_spec)

               call import_spec%create(_RC)
               call registry%add_primary_spec(import_v_pt, import_spec)

              ! And now connect
               export_v_pt = VirtualConnectionPt(ESMF_STATEINTENT_EXPORT, var_name)
               
               s_pt = ConnectionPt('collection_1', export_v_pt)
               d_pt = ConnectionPt('<self>', import_v_pt)
               conn = SimpleConnection(source=s_pt, destination=d_pt)
               call conn%connect(registry, _RC)
            end do
         end if
      end if

      call ESMF_HConfigDestroy(mapl_config, _RC)
      _RETURN(ESMF_SUCCESS)
   end subroutine step_B


   subroutine run(gc, importState, exportState, clock, rc)
      type(ESMF_GridComp) :: gc
      type(ESMF_State) :: importState
      type(ESMF_State) :: exportState
      type(ESMF_Clock) :: clock
      integer, intent(out) :: rc

      type(OuterMetaComponent), pointer :: outer_meta
      type(ESMF_Field) :: f_in, f_out
      character(:), allocatable :: var_name
      type(ESMF_HConfigIter) :: iter,e,b
      type(ESMF_HConfig) :: hconfig, states_spec, state_spec, mapl_config
      integer :: status


     call MAPL_GridCompGet(gc, hconfig=hconfig, _RC)
     call MAPL_GridCompGetOuterMeta(gc, outer_meta, _RC)
     call outer_meta%run_children(_RC)

     mapl_config = ESMF_HConfigCreateAt(hconfig, keystring='mapl', _RC)
     if (ESMF_HConfigIsDefined(mapl_config, keystring='states')) then
        states_spec = ESMF_HConfigCreateAt(mapl_config, keystring='states')
        if (ESMF_HConfigIsDefined(states_spec, keystring='export')) then
            state_spec = ESMF_HConfigCreateAt(states_spec, keystring='export')
            b = ESMF_HConfigIterBegin(state_spec)
            e = ESMF_HConfigIterEnd(state_spec) 
            iter = ESMF_HConfigIterBegin(state_spec)
            do while (ESMF_HConfigIterLoop(iter,b,e))
               var_name = ESMF_HConfigAsStringMapKey(iter,_RC)

               call ESMF_StateGet(importState, itemName=var_name, field=f_in, _RC)
               call ESMF_StateGet(exportState, itemName=var_name, field=f_out, _RC)

               call FieldCopy(f_in, f_out, _RC)

            end do
         end if
      end if


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
