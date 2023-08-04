! This module provides a limited subset of ESMF types, parameters, and
! procedures.  The intent is to enforce MAPL GridComps to use MAPL
! wrappers when appropriate.  Compliant MAPL components should not do
! 'USE ESMF', but instead should have 'USE mapl3g_ESMF_SUBSET'.

module mapl3g_ESMF_Subset

   ! Note: items should be listed in alphabetic order for easy human search.
   ! types

   use:: esmf, only: &
        ESMF_Clock, &
        ESMF_Config, &
        ESMF_Field, &
        ESMF_HConfig, &
        ESMF_GridComp, &
        ESMF_State

   ! parameters
   use:: esmf, only: &
        ESMF_FAILURE, &
        ESMF_METHOD_FINALIZE, &
        ESMF_METHOD_INITIALIZE, &
        ESMF_METHOD_RUN, &
        ESMF_STATEINTENT_EXPORT, &
        ESMF_STATEINTENT_IMPORT, &
        ESMF_SUCCESS
        
   implicit none

   
end module mapl3g_ESMF_Subset
