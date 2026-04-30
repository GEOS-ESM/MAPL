#include "MAPL.h"

module MAPL_PythonBridge

   ! -----------------------------
   ! Generic bridge mechanism to pull MAPL down to the python level
   !
   ! WARNING: All functional code should be #ifdef with PYTHONBRIDGE_INTEGRATION
   !          in order to keep the functionality as a deliberate opt-in
   !
   ! -----------------------------
   use ESMF
   use MAPL
#ifdef PYTHONBRIDGE_INTEGRATION
   use mapl_fortran_python_bridge, only: mapl_fortran_python_bridge_global_initialize
   use mapl_fortran_python_bridge, only: mapl_fortran_python_bridge_user_init
   use mapl_fortran_python_bridge, only: mapl_fortran_python_bridge_user_run
   use mapl_fortran_python_bridge, only: mapl_fortran_python_bridge_user_run_with_internal
   use mapl_fortran_python_bridge, only: mapl_fortran_python_bridge_user_finalize
   use mapl_fortran_python_bridge, only: pygeosbridge_name_buffer
   use ieee_exceptions, only: ieee_get_halting_mode, ieee_set_halting_mode
   use ieee_exceptions, only: ieee_overflow, ieee_invalid, ieee_support_halting
   use iso_c_binding, only: c_loc, C_NULL_CHAR
#endif

   implicit none
   private

   public initialize_python_bridge
   public MAPL_pybridge_gcrun
   public MAPL_pybridge_gcrun_with_internal
   public MAPL_pybridge_gcinit
   public MAPL_pybridge_gcfinalize

contains

   subroutine initialize_python_bridge(im, jm, lm)
      ! -----------------------------
      ! Initialize the underlying python tools with a grid size.
      ! It will spin a central CFFI-handled python interpreter
      ! It also gives us a hook to do pre-load of common packages (numpy, tf, etc.)
      !
      ! TODO | Dev Note : this works for a single GRID. A much powerful system
      !                   would not get the dimensions upfront but have them part
      !                   of the backward fetch from python to MAPL
      ! -----------------------------
      integer, intent(in), value :: im, jm, lm

      character(len=ESMF_MAXSTR) :: IAm
      integer :: STATUS
      integer :: RC

#ifdef PYTHONBRIDGE_INTEGRATION
      logical :: halting_mode_overflow(5)
      logical :: halting_mode_invalid(5)
      logical :: set_halting_allowed

      ! Spin the interface - we have to deactivate the ieee error
      ! to be able to load numpy, scipy and other numpy packages
      ! that generate an overflow or invalid operation during init

      set_halting_allowed = ieee_support_halting(ieee_overflow) .and. &
         ieee_support_halting(ieee_invalid)

      if (set_halting_allowed) then
         call ieee_get_halting_mode(ieee_overflow, halting_mode_overflow)
         call ieee_set_halting_mode(ieee_overflow, .false.)
         call ieee_get_halting_mode(ieee_invalid, halting_mode_invalid)
         call ieee_set_halting_mode(ieee_invalid, .false.)
      end if
      call mapl_fortran_python_bridge_global_initialize(im, jm, lm)
      if (set_halting_allowed) then
         call ieee_set_halting_mode(ieee_overflow, halting_mode_overflow)
         call ieee_set_halting_mode(ieee_invalid, halting_mode_invalid)
      end if
#endif

   end subroutine initialize_python_bridge

   subroutine MAPL_pybridge_gcinit(pypkg_name, mapl, import, export)
      ! -----------------------------
      ! Call the python integration by marhshalling Fortran object/memory
      ! into C compatible memory
      ! Will trigger the `GEOSInterfaceCode.init` python function on the user code
      ! -----------------------------
      character(len=*), intent(in) :: pypkg_name
      type(ESMF_GridComp), intent(inout), target :: mapl ! MAPL state
      type(ESMF_State), intent(inout), target :: import ! Import state
      type(ESMF_State), intent(inout), target :: export ! Export state

#ifdef PYTHONBRIDGE_INTEGRATION
      pygeosbridge_name_buffer = pypkg_name // C_NULL_CHAR
      call mapl_fortran_python_bridge_user_init( &
           c_loc(pygeosbridge_name_buffer), &
           c_loc(mapl), c_loc(import), c_loc(export))
#endif
   end subroutine MAPL_pybridge_gcinit

   subroutine MAPL_pybridge_gcrun(pypkg_name, mapl, import, export)
      ! -----------------------------
      ! Call the python integration by marhshalling Fortran object/memory
      ! into C compatible memory
      ! Will trigger the `GEOSInterfaceCode.run` python function on the user code
      ! -----------------------------
      character(len=*), intent(in) :: pypkg_name
      type(ESMF_GridComp), intent(inout), target :: mapl ! MAPL state
      type(ESMF_State), intent(inout), target :: import ! Import state
      type(ESMF_State), intent(inout), target :: export ! Export state

#ifdef PYTHONBRIDGE_INTEGRATION
      pygeosbridge_name_buffer = pypkg_name // C_NULL_CHAR
      call mapl_fortran_python_bridge_user_run( &
           c_loc(pygeosbridge_name_buffer), &
           c_loc(mapl), c_loc(import), c_loc(export))
#endif
   end subroutine MAPL_pybridge_gcrun

   subroutine MAPL_pybridge_gcrun_with_internal(pypkg_name, mapl, import, export, internal)
      ! -----------------------------
      ! Call the python integration by marhshalling Fortran object/memory
      ! into C compatible memory. Variation of `gcrun` with an INTERNAL state
      ! Will trigger the `GEOSInterfaceCode.run_with_internal` python function on the user code
      ! -----------------------------
      character(len=*), intent(in) :: pypkg_name
      type(ESMF_GridComp), intent(inout), target :: mapl ! MAPL state
      type(ESMF_State), intent(inout), target :: import ! Import state
      type(ESMF_State), intent(inout), target :: export ! Export state
      type(ESMF_State), intent(inout), target :: internal ! Internal state

#ifdef PYTHONBRIDGE_INTEGRATION
      pygeosbridge_name_buffer = pypkg_name // C_NULL_CHAR
      call mapl_fortran_python_bridge_user_run_with_internal( &
           c_loc(pygeosbridge_name_buffer), &
           c_loc(mapl), c_loc(import), c_loc(export), c_loc(internal))
#endif
   end subroutine MAPL_pybridge_gcrun_with_internal

   subroutine MAPL_pybridge_gcfinalize(pypkg_name, mapl, import, export)
      ! -----------------------------
      ! Call the python integration by marhshalling Fortran object/memory
      ! into C compatible memory.
      ! Will trigger the `GEOSInterfaceCode.finalize` python function on the user code
      ! -----------------------------
      character(len=*), intent(in) :: pypkg_name
      type(ESMF_GridComp), intent(inout), target :: mapl ! MAPL state
      type(ESMF_State), intent(inout), target :: import ! Import state
      type(ESMF_State), intent(inout), target :: export ! Export state

#ifdef PYTHONBRIDGE_INTEGRATION
      pygeosbridge_name_buffer = pypkg_name // C_NULL_CHAR
      call mapl_fortran_python_bridge_user_finalize( &
           c_loc(pygeosbridge_name_buffer), &
           c_loc(mapl), c_loc(import), c_loc(export))
#endif
   end subroutine MAPL_pybridge_gcfinalize

end module MAPL_PythonBridge
