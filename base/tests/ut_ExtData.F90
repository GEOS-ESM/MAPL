!------------------------------------------------------------------------------
!               Global Modeling and Assimilation Office (GMAO)                !
!                    Goddard Earth Observing System (GEOS)                    !
!                                 MAPL Component                              !
!------------------------------------------------------------------------------
!
#include "MAPL_Generic.h"
!
!>
!### MODULE: `module_name`
!
! Author: GMAO SI-Team
!
! `RUTMod` - Implements Interface to a minimalistic MAPL GC that
! serves as a parent of the MAPL_ExtData GC
!
! `RUT` Root Utility Test is a minimalistic parent grid component that creates 
! ExtData grid component.
!
!#### History
!- 29Sep2011  Anton Darmenov  Implemented as part of the ExtData utility test.
!```
! ut_ExtaData.x - Simple ESMF/MAPL example demonstrating 
!                 MAPL_ExtDataGridComp
!
!                       ---
!
! In addition to MAPL_ExtDataGridComp.rc also needs:
!    AGCM.rc 
!    CAP.rc
!    HISTORY.rc
!
! Sample files:
!    # ---- AGCM.rc ---------------
!    NX: 2
!    NY: 4
!    GRIDNAME: PC144x91-DC
!    LM: 72
!    # ----------------------------
!
!    # --- CAP.rc -----------------
!    MAPLROOT_COMPNAME: RUT
!            ROOT_NAME: RUT
!
!    ROOT_CF: AGCM.rc
!    HIST_CF: HISTORY.rc
!    
!    BEG_DATE:     18910301 000000
!    END_DATE:     20070630 210000
!    JOB_SGMT:     00000030 000000
!    NUM_SGMT:     1
!    HEARTBEAT_DT:       1800
!    # ----------------------------
!```
!
   MODULE RUTMod
!
! !USES:
!
   USE ESMF_Mod
   USE MAPLBase_Mod

   use MAPL_ExtDataGridCompMod, only: ExtData_SetServices => SetServices


   IMPLICIT NONE
   PRIVATE
!
! !PUBLIC MEMBER FUNCTIONS:

   PUBLIC SetServices

   integer :: ExtData
!
!-------------------------------------------------------------------------

CONTAINS

!-------------------------------------------------------------------------
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 610.1     !
!-------------------------------------------------------------------------
!>
! `SetServices` --- Sets IRF services for the RUT
!
! Sets Initialize, Run and Finalize services.
!
!#### History
!- 29Sep2011  Anton Darmenov   Cloned from the ExtData GC code
!
   SUBROUTINE SetServices ( GC, RC )

! !ARGUMENTS:

    type(ESMF_GridComp), intent(INOUT) :: GC  !! gridded component
    integer, optional                  :: RC  !! return code

!
!-------------------------------------------------------------------------

                            __Iam__('SetServices')

!   Local derived type aliases
    type(ESMF_Config)          :: CF
    character(len=ESMF_MAXSTR) :: comp_name

!                              ------------

!   Get my name and set-up traceback handle
!   ---------------------------------------
    call ESMF_GridCompGet( GC, name=comp_name, _rc )
    Iam = trim(comp_name) // '::' // trim(Iam)

!   Greetings
!   ---------
    if (MAPL_am_I_root()) then
         print *, trim(Iam)//': ACTIVE'
         print *
    end if

!   Set the Initialize, Run, Finalize entry points
!   ----------------------------------------------
    call MAPL_GridCompSetEntryPoint ( GC, ESMF_SETINIT,  Initialize_, _rc )
    call MAPL_GridCompSetEntryPoint ( GC, ESMF_SETRUN,   Run_,        _rc )
    call MAPL_GridCompSetEntryPoint ( GC, ESMF_SETFINAL, Finalize_,   _rc )

!   Add the ExtData as a child
!   --------------------------
    ExtData = MAPL_AddChild ( GC, NAME='ExtData', SS=ExtData_SetServices, _rc )

!   Generic Set Services
!   --------------------
    call MAPL_GenericSetServices ( GC, _rc )

!   All done
!   --------

    _return(ESMF_SUCCESS)

  END SUBROUTINE SetServices


!-------------------------------------------------------------------------
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 610.1     !
!-------------------------------------------------------------------------
!>
! `Initialize_` --- Initialize RUT
!
! This is a simple ESMF wrapper.
!
!#### History
!- 29Sep2011  Anton Darmenov   Cloned from the ExtData GC code
!
   SUBROUTINE Initialize_ ( GC, IMPORT, EXPORT, CLOCK, rc )

! !USES:

   implicit NONE

! !INPUT PARAMETERS:

   type(ESMF_Clock),  intent(inout) :: CLOCK     !! The clock

! !OUTPUT PARAMETERS:

   type(ESMF_GridComp), intent(inout) :: GC      !! Grid Component
   type(ESMF_State), intent(inout) :: IMPORT     !! Import State
   type(ESMF_State), intent(inout) :: EXPORT     !! Export State
   integer, intent(out)            :: rc         !! Error return code:    
                                                 !!  0 - all is well    
                                                 !!  1 -     

!
!-------------------------------------------------------------------------

                              __Iam__('Initialize_')

   type(ESMF_Grid)             :: GRID        ! Grid
   type(ESMF_Config)           :: CF          ! Universal Config 

   character(len=ESMF_MAXSTR)     :: comp_name

  
!  Get my name and set-up traceback handle
!  ---------------------------------------
   call ESMF_GridCompGet( GC, name=comp_name, config=CF, _rc )
   Iam = trim(comp_name) // '::' // trim(Iam)

!  Create grid for this GC
!  ------------------------
   call MAPL_GridCreate  (GC, _rc )
   call ESMF_GridCompGet (GC, grid=GRID, _rc)

!  Initialize MAPL Generic
!  -----------------------
   call MAPL_GenericInitialize ( GC, IMPORT, EXPORT, clock,  _rc )

!  All done
!  --------
   _return(ESMF_SUCCESS)

   END SUBROUTINE Initialize_


!-------------------------------------------------------------------------
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 610.1     !
!-------------------------------------------------------------------------
!>
! `Run_` --- Runs RUT
!
! This is a simple ESMF wrapper.
!
!#### History
!- 29Sep2011  Anton Darmenov   Cloned from the ExtData GC code
!

   SUBROUTINE Run_ ( GC, IMPORT, EXPORT, CLOCK, rc )

! !USES:

  implicit NONE

! !INPUT PARAMETERS:

   type(ESMF_Clock),  intent(inout) :: CLOCK     !! The clock

! !OUTPUT PARAMETERS:

   type(ESMF_GridComp), intent(inout)  :: GC     !! Grid Component   
   type(ESMF_State), intent(inout) :: IMPORT     !! Import State   
   type(ESMF_State), intent(inout) :: EXPORT     !! Export State   
   integer, intent(out) ::  rc                   !! Error return code:   
                                                 !!  0 - all is well   
                                                 !!  1 -    

!
!-------------------------------------------------------------------------

                              __Iam__('Run_')


   character(len=ESMF_MAXSTR)    :: comp_name


!  Get my name and set-up traceback handle
!  ---------------------------------------
   call ESMF_GridCompGet( GC, name=comp_name, _rc )
   Iam = trim(comp_name) // '::' // trim(Iam)

!   Call Run for every Child
!   -------------------------
    call MAPL_GenericRunChildren ( GC, IMPORT, EXPORT, CLOCK,  _rc)


!  All done
!  --------
   _return(ESMF_SUCCESS)

   END SUBROUTINE Run_

!-------------------------------------------------------------------------
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 610.1     !
!-------------------------------------------------------------------------
!>
! `Finalize_` --- Finalize RUT
!
! This is a simple ESMF wrapper.
!
!#### History
!- 29Sep2011  Anton Darmenov   Cloned from the ExtData GC code
!
   SUBROUTINE Finalize_ ( GC, IMPORT, EXPORT, CLOCK, rc )

! !USES:

  implicit NONE

! !INPUT PARAMETERS:

   type(ESMF_Clock),  intent(inout) :: CLOCK      !! The clock

! !OUTPUT PARAMETERS:

   type(ESMF_GridComp), intent(inout)  :: GC     !! Grid Component
   type(ESMF_State), intent(inout) :: IMPORT     !! Import State
   type(ESMF_State), intent(inout) :: EXPORT     !! Export State
   integer, intent(out) ::  rc                   !! Error return code:    
                                                 !!  0 - all is well    
                                                 !!  1 -     

!
!-------------------------------------------------------------------------

                              __Iam__('Finalize_')
   

   character(len=ESMF_MAXSTR)  :: comp_name

!  Get my name and set-up traceback handle
!  ---------------------------------------
   call ESMF_GridCompGet( GC, name=comp_name, _rc )
   Iam = trim(comp_name) // trim(Iam)

!  Finalize MAPL Generic
!  ---------------------
   call MAPL_GenericFinalize ( GC, IMPORT, EXPORT, CLOCK,  _rc )

!  All done
!  --------
   _return(ESMF_SUCCESS)

 end SUBROUTINE Finalize_

end module RUTMod


! ----------------------------------------------------------------------


Program ut_ExtData
   
   use MAPLBase_Mod

   use RUTMod,                  only: ROOT_SetServices    => SetServices
   use MAPL_ExtDataGridCompMod, only: ExtData_SetServices => SetServices

   implicit NONE

   integer :: STATUS
   logical :: am_I_root

   character(len=*), parameter :: Iam = 'ut_ExtData'

!                             -----
   
    call MAPL_CAP(ROOT_SetServices, AmIRoot = am_I_root, rc=STATUS)

    call exit(STATUS)

end program ut_ExtData
