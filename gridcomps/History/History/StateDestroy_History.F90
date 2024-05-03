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
  SUBMODULE (MAPL_HistoryGridCompMod) StateDestroy_smod
!
! !USES:
!
  implicit none

contains


  MODULE SUBROUTINE MAPL_StateDestroy(State, RC)

    type(ESMF_State), intent(inout) :: state
    integer, optional,intent(  out) :: rc

! Local variables:
    integer                    :: STATUS

    type(ESMF_Field)                      :: field
    type(ESMF_FieldBundle)                :: bundle
    type (ESMF_StateItem_Flag),  pointer  :: itemTypeList(:)
    character(len=ESMF_MAXSTR ), pointer  :: itemNameList(:)

    integer                               :: I, J, N, NF

    call ESMF_StateGet(state, ITEMCOUNT=N,  _RC)

    allocate(itemNameList(N), _STAT)
    allocate(itemtypeList(N), _STAT)

    call ESMF_StateGet(state,ITEMNAMELIST=itemNamelist,ITEMTYPELIST=itemtypeList,_RC)

    do I=1,N
       if(itemtypeList(I)==ESMF_STATEITEM_FIELD) then
          call ESMF_StateGet(state,itemNameList(I),FIELD,_RC)
          call ESMF_FieldDestroy(FIELD, _RC)
       else if(itemtypeList(I)==ESMF_STATEITEM_FieldBundle) then
          call ESMF_StateGet(state,itemNameList(I), BUNDLE, _RC)
          call ESMF_FieldBundleGet(BUNDLE,FieldCount=NF, _RC)
          DO J=1,NF
             call ESMF_FieldBundleGet(BUNDLE, J, FIELD, _RC)
             call ESMF_FieldDestroy(field, _RC)
          END DO
          call ESMF_FieldBundleDestroy(BUNDLE, _RC)
       else if(itemtypeList(I)==ESMF_STATEITEM_State) then
!ALT we ingore nested states for now, they will get destroyed by their GC
       end if
    end do
    call ESMF_StateDestroy(STATE, _RC)

    deallocate(itemNameList, _STAT)
    deallocate(itemtypeList, _STAT)

    _RETURN(ESMF_SUCCESS)
  end subroutine MAPL_StateDestroy

END SUBMODULE
