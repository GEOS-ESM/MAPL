#define I_AM_MAIN
#include "MAPL.h"

   Program Regrid_Util

   use mpi   
   use esmf
   use regrid_util_support_mod
   use mapl3
   use mapl3g_ReaderWriterCouplerGridComp, only: ReaderWriter_setServices => setServices
   use mapl3g_HConfigUtilities
   implicit NONE

   integer :: status

   call main(_RC)
CONTAINS

#undef I_AM_MAIN
   subroutine main(rc)
   use mapl3g_GenericGridComp, only: generic_SetServices => setServices
   integer, optional, intent(out) :: rc

   type(regrid_support), target :: support
   integer :: status, user_status
   type(GriddedComponentDriver) :: driver
   type(ESMF_GridComp) :: readerwriter_gridcomp
   type(ESMF_HConfig) :: hconfig
   type(ESMF_Clock) :: clock
   type(ESMF_Time) :: time
   type(ESMF_TimeInterval) :: time_interval

   call MAPL_Initialize()
   call support%process_command_line(_RC)

   hconfig = ESMF_HConfigCreate(_RC)
   call add_string_vector_to_hconfig(hconfig, "input_files", support%filenames, _RC)
   call add_string_vector_to_hconfig(hconfig, "output_files", support%filenames, _RC)

   readerwriter_gridcomp = mapl_GridCompCreate('readerwritercoupler_gc',user_setservices(ReaderWriter_setServices), hconfig, _RC)
   call ESMF_GridCompSetServices(ReaderWriter_gridcomp, generic_setServices, _USERRC)
   call ESMF_TimeIntervalSet(time_interval, m=30, _RC)
   call ESMF_TimeSet(time, yy=2004, mm=01, dd=01, h=0, m=0, s=0, _RC)
   clock = ESMF_ClockCreate(timeStep=time_interval, startTime=time, _RC)
   driver = GriddedComponentDriver(ReaderWriter_gridcomp, clock=clock)
   call mapl_DriverInitializePhases(driver, phases=GENERIC_INIT_PHASE_SEQUENCE, _RC)
   call driver%run(phase_idx=GENERIC_RUN_USER, _RC)
   call driver%finalize(_RC)
   call MAPL_Finalize()
   _RETURN(_SUCCESS)

   end subroutine main

   end program Regrid_Util
