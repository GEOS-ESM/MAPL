#include "MAPL.h"
#include "MAPL_Exceptions.h"
!------------------------------------------------------------------------------
!>
!### MODULE: `ACG_GridComp`
!
! This module is created to show how to automatically regenerate code segments
! for the registration and access of ESMF states member variables.
! It is not meant to be executed in an application but only to be compiled.
!
module ACG_GridComp

  use ESMF
  use MAPL

  implicit none
  private

  public SetServices

!------------------------------------------------------------------------------
  contains
!------------------------------------------------------------------------------
!> 
! `SetServices` uses MAPL_GenericSetServices, which sets
! the Initialize and Finalize services to generic versions. 
! It also allocates our instance of a generic state and puts it in the
! gridded component (GC). Here we only set the run method and
! declare the data services.
!
  subroutine SetServices(GC,rc)

     type(ESMF_GridComp), intent(inout) :: GC !! gridded component
     integer, optional                  :: rc !! return code

     integer :: status

     call MAPL_GridCompSetEntryPoint ( gc, ESMF_METHOD_INITIALIZE,  initialize, _RC)
     call MAPL_GridCompSetEntryPoint ( gc, ESMF_METHOD_RUN,  run, _RC)

#include "ACG_Export___.h"
#include "ACG_Import___.h"

!   Set generic services
!   ----------------------------------
     call MAPL_GenericSetServices(GC, _RC)

     _RETURN(_SUCCESS)

  end subroutine SetServices

!------------------------------------------------------------------------------
!> 
! `initialize` is meant to initialize the `ACG` gridded component. 
! It primarily creates its exports.
!
  subroutine initialize(GC, import, export, clock, rc)

     type (ESMF_GridComp), intent(inout) :: GC     !! Gridded component
     type (ESMF_State),    intent(inout) :: import !! Import state
     type (ESMF_State),    intent(inout) :: export !! Export state
     type (ESMF_Clock),    intent(inout) :: clock  !! The clock
     integer, optional,    intent(  out) :: RC     !! Error code
!
! Locals
     integer :: status

     call MAPL_GridCreate(GC, _RC)

!   Call Generic Initialize
!   ----------------------------------------
     call MAPL_GenericInitialize(GC, import, export, clock, _RC)

     _RETURN(_SUCCESS)

  end subroutine initialize

!------------------------------------------------------------------------------
!> 
! `run` is the Run method for `ACG`.
!
  subroutine run(GC, import, export, clock, rc)

     type (ESMF_GridComp), intent(inout) :: GC     !! Gridded component
     type (ESMF_State),    intent(inout) :: import !! Import state
     type (ESMF_State),    intent(inout) :: export !! Export state
     type (ESMF_Clock),    intent(inout) :: clock  !! The clock
     integer, optional,    intent(  out) :: RC     !! Error code
!
! Locals
     type (MAPL_MetaComp),      pointer  :: MAPL
     integer :: status

#include "ACG_DeclarePointer___.h"

!****************************************************************************
! Begin...

     !   Get my internal MAPL_Generic state
     !   -----------------------------------
     call MAPL_GetObjectFromGC ( GC, MAPL, _RC)

#include "ACG_GetPointer___.h"


     _RETURN(_SUCCESS)

     _UNUSED_DUMMY(import)
     _UNUSED_DUMMY(clock)

  end subroutine run

end module ACG_GridComp
