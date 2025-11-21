! This module provides a limited subset of ESMF types, parameters, and
! procedures.  The intent is to enforce MAPL GridComps to use MAPL
! wrappers when appropriate.  Compliant MAPL components should not do
! 'USE ESMF', but instead should have 'USE mapl3g_ESMF_SUBSET'.

module mapl3g_ESMF_Subset

   ! Note: items should be listed in alphabetic order for easy human search.
   ! types

   use:: esmf, only: &
        ESMF_VM, &
        ESMF_Clock, &
        ESMF_Alarm, &
        ESMF_Time, &
        ESMF_TimeInterval, &
        ESMF_Config, &
        ESMF_Geom, &
        ESMF_Grid, &
        ESMF_Mesh, &
        ESMF_LocStream, &
        ESMF_Xgrid, &
        ESMF_Field, &
        ESMF_FieldBundle, &
        ESMF_State, &
        ESMF_HConfig, &
        ESMF_HConfigIter, &
        ESMF_GridComp, &
        ESMF_Info, &
        ESMF_State


   ! parameters
   use:: esmf, only: &
        ESMF_FAILURE, &
        ESMF_METHOD_FINALIZE, &
        ESMF_METHOD_INITIALIZE, &
        ESMF_METHOD_RUN, &
        ESMF_STATEINTENT_EXPORT, &
        ESMF_STATEINTENT_IMPORT, &
        ESMF_SUCCESS, &
        ESMF_CALKIND_GREGORIAN
        
   ! procedures
   use :: esmf, only: &
        ESMF_TimePrint, &
        ESMF_TimeSet, &
        ESMF_CalendarSetDefault, &
        ESMF_HConfigAsStringMapKey, &
        ESMF_HConfigAsString, &
        ESMF_HConfigCreate, &
        ESMF_HConfigCreateAt, &
        ESMF_HConfigDestroy, &
        ESMF_HConfigIsDefined, &
        ESMF_HConfigIterBegin, &
        ESMF_HConfigIterEnd, &
        ESMF_HConfigIterLoop, &
        ESMF_HConfigGetSize, &
        ESMF_VMGet, &
        ESMF_VMGetCurrent, &
        ESMF_ClockCreate, &
        ESMF_ClockGet, &
        operator(+), &
        operator(-), &
        operator(/), &
        operator(*), &
        operator(==), &
        operator(/=), &
        operator(<), &
        operator(<=), &
        operator(>), &
        operator(>=)

    use :: esmf, only: &
         ESMF_InfoGetFromHost, &
         ESMF_InfoGet, &
         ESMF_InfoIsSet

   implicit none
   
end module mapl3g_ESMF_Subset
