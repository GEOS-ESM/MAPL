!------------------------------------------------------------------------------
!               Global Modeling and Assimilation Office (GMAO)                !
!                    Goddard Earth Observing System (GEOS)                    !
!                                 MAPL Component                              !
!------------------------------------------------------------------------------
!
#include "MAPL_Generic.h"
#include "unused_dummy.H"
!
!>
!### MODULE: `MAPL_HistoryGridCompMod`
!
! Author: GMAO SI-Team
!
! `MAPL_HistoryGridCompMod` contains the `Initialize`, `Run` and `Finalize` methods for `History`.
! The three methods are called at the level of CAP.
!
  SUBMODULE (MAPL_HistoryGridCompMod) get_acc_offset_smod
!
! !USES:
!
  implicit none

contains


  MODULE function get_acc_offset(current_time,ref_time,rc) result(acc_offset)

     integer :: acc_offset
     type(ESMF_Time), intent(in) :: current_time
     integer, intent(in) :: ref_time
     integer, optional, intent(out) :: rc

     integer :: status
     integer :: hour,minute,second,year,month,day,diff_sec
     type(ESMF_Time) :: new_time
     type(ESMF_TimeInterval) :: t_int

     call ESMF_TimeGet(current_time,yy=year,mm=month,dd=day,h=hour,m=minute,s=second,_RC)
     call MAPL_UnpackTime(ref_time,hour,minute,second)
     call ESMF_TimeSet(new_time,yy=year,mm=month,dd=day,h=hour,m=minute,s=second,_RC)
     t_int = new_time - current_time

     call ESMF_TimeIntervalGet(t_int,s=diff_sec,_RC)
     if (diff_sec == 0) then
        acc_offset = 0
     else if (diff_sec > 0) then
        acc_offset = diff_sec - 86400
     else if (diff_sec < 0) then
        acc_offset = diff_sec
     end if
     _RETURN(_SUCCESS)
  end function

END SUBMODULE
