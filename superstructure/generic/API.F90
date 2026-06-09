! Export umbrella for the MAPL superstructure/generic layer.
! Public API exposed to external consumers.
module mapl_generic_api

   use mapl_UserSetServices_mod, only: user_setservices, AbstractUserSetServices, DSOSetServices

   use mapl_OpenMP_Support_mod, only: mapl_find_bounds => find_bounds
   use mapl_OpenMP_Support_mod, only: mapl_get_num_threads => get_num_threads
   use mapl_OpenMP_Support_mod, only: mapl_get_current_thread => get_current_thread

   use mapl_Generic_mod, &
       mapl_GridCompAddVarSpec => GridCompAddVarSpec, &
       mapl_GridCompAddSpec => GridCompAddSpec, &
       mapl_GridCompAdvertiseVariable => GridCompAdvertiseVariable, &
       mapl_GridCompGet => GridCompGet, &
       mapl_GridCompSet => GridCompSet, &
       mapl_GridCompSetEntryPoint => GridCompSetEntryPoint, &
       mapl_GridCompAddChild => GridCompAddChild, &
       mapl_GridCompGetChildName => GridCompGetChildName, &
       mapl_GridCompRunChild => GridCompRunChild, &
       mapl_GridCompRunChildren => GridCompRunChildren, &
       mapl_GridCompGetInternalState => GridCompGetInternalState, &
       mapl_GridCompSetGeometry => GridCompSetGeometry, &
       mapl_GridcompGetResource => GridCompGetResource, &
       mapl_ClockGet => ClockGet, &
       mapl_GridCompSetGeom => GridCompSetGeom, &
       mapl_GridCompSetVerticalGrid => GridCompSetVerticalGrid, &
       mapl_GridCompAddConnection => GridCompAddConnection, &
       mapl_GridCompAddConnectivity => GridCompAddConnectivity, &
       mapl_GridCompReexport => GridCompReexport, &
       mapl_GridCompConnectAll => GridCompConnectAll, &
       mapl_GridCompTimerStart => GridCompTimerStart, &
       mapl_GridCompTimerStop => GridCompTimerStop, &
       mapl_GridCompGetCheckpointDir => GridCompGetCheckpointDir, &
   !    mapl_STATEITEM_STATE => STATEITEM_STATE, &
   !    mapl_STATEITEM_FIELDBUNDLE => STATEITEM_FIELDBUNDLE, &
   !    mapl_STATEITEM_SERVICE => STATEITEM_SERVICE, &
   !    mapl_STATEITEM_VECTOR => STATEITEM_VECTOR, &
   !    mapl_UserCompGetInternalState => UserCompGetInternalState, &
   !    MAPL_UserCompSetInternalState => UserCompSetInternalState, &
       MAPL_GridCompGetOuterMeta => GridCompGetOuterMeta, &
       MAPL_GridCompGetRegistry => GridCompGetRegistry, &
       MAPL_GridCompIsGeneric => GridCompIsGeneric, &
       MAPL_GridCompIsUser => GridCompIsUser

   use mapl_GenericGridComp_mod,  &
       mapl_GridCompCreate => GridCompCreate, &
       mapl_GenericSetServices => GenericSetServices

   use mapl_VariableSpec_mod
   use mapl_ComponentSpec_mod
   use mapl_ChildSpec_mod
   use mapl_RestartHandler_mod

   implicit none
   private

   ! These should be available to users
   public :: mapl_GridCompAddVarSpec
   public :: mapl_GridCompAddSpec
   public :: mapl_GridCompAdvertiseVariable

   public :: mapl_GridCompGet
   public :: mapl_GridCompSet
   public :: mapl_GridCompSetEntryPoint

   public :: mapl_GridCompAddChild
   public :: mapl_GridCompGetChildName
   public :: mapl_GridCompRunChild
   public :: mapl_GridCompRunChildren

   public :: mapl_GridCompGetInternalState

   public :: mapl_GridCompSetGeometry

   public :: mapl_GridcompGetResource

   public :: mapl_ClockGet

   ! Accessors
!!$   public :: mapl_GetOrbit
!!$   public :: mapl_GetCoordinates
!!$   public :: mapl_GetLayout

   public :: mapl_GridCompSetGeom
   public :: mapl_GridCompSetVerticalGrid

   ! Connections
   public :: mapl_GridCompAddConnection
   public :: mapl_GridCompAddConnectivity  ! Legacy name - temporary backward compatibility
   public :: mapl_GridCompReexport
   public :: mapl_GridCompConnectAll

   ! Timers
   public :: mapl_GridCompTimerStart
   public :: mapl_GridCompTimerStop

   ! Checkpoint directory
   public :: mapl_GridCompGetCheckpointDir

   ! Spec types
   public :: mapl_STATEITEM_STATE, mapl_STATEITEM_FIELDBUNDLE
   public :: mapl_STATEITEM_SERVICE, mapl_STATEITEM_VECTOR

   public :: mapl_UserCompGetInternalState, MAPL_UserCompSetInternalState

   public :: user_setservices
   public :: AbstractUserSetServices
   public :: DSOSetServices

   public :: mapl_find_bounds
   public :: mapl_get_num_threads
   public :: mapl_get_current_thread

   public :: mapl_GridCompCreate
   public :: mapl_GenericSetServices

   public :: VariableSpec
   public :: make_VariableSpec

   public :: ChildSpec

   public :: CheckpointControls
   public :: RestartHandler
end module mapl_generic_api
