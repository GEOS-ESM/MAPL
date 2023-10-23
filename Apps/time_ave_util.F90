#define I_AM_MAIN
#include "MAPL_Generic.h"

program  read_OBS_PLATFORM

   use ESMF
   use MAPL
   use MPI
   use, intrinsic :: iso_fortran_env, only: int32, int64, int16, real32, real64
   use ieee_arithmetic, only: isnan => ieee_is_nan

   implicit none

   integer  comm,myid,npes,ierror
   integer  imglobal
   integer  jmglobal
   logical  root

   type(ESMF_Config), intent(in)          :: config
    integer, intent(in)                    :: nlist
    character(len=ESMF_MAXSTR), intent(in) :: collections(nlist)
    integer, intent(inout), optional :: rc

    integer n, unitr, unitw
    logical :: match, contLine, con3, count


    
    ! -- note: work on HEAD node
    !
    call ESMF_ConfigGetAttribute(config, value=HIST_CF, &
         label="HIST_CF:", default="HIST.rc", _RC )
    unitr = GETFILE(HIST_CF, FORM='formatted', _RC)


 end program read_OBS_PLATFORM
