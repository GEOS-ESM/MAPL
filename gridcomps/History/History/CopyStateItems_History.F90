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
  SUBMODULE (MAPL_HistoryGridCompMod) CopyStateItems_smod
!
! !USES:
!
  implicit none

contains


  MODULE SUBROUTINE CopyStateItems(src, dst, rc)
    type(ESMF_State), intent(in) :: src
    type(ESMF_State), intent(inout) :: dst
    integer, optional, intent(out) :: rc

! local vars
    type (ESMF_StateItem_Flag), pointer  :: itemTypes(:)
    character(len=ESMF_MAXSTR ), pointer :: itemNames(:)
    integer :: status
    integer :: n, itemCount
    type(ESMF_Field) :: field(1)
    type(ESMF_FieldBundle) :: bundle(1)

    call ESMF_StateGet(src,  itemCount=itemCount, _RC)

    allocate(itemnames(itemcount), _STAT)
    allocate(itemtypes(itemcount), _STAT)

    call ESMF_StateGet(src, itemNameList=itemNames, &
                       itemTypeList=itemTypes, _RC)

    do n=1,itemCount
       if(itemTypes(n)==ESMF_STATEITEM_FIELD) then
          call ESMF_StateGet(src, itemNames(n), field(1), _RC)
          call ESMF_StateAdd(dst, field, _RC)
       else if(itemTypes(n)==ESMF_STATEITEM_FieldBundle) then
          call ESMF_StateGet(src, itemNames(n), bundle(1), _RC)
          call ESMF_StateAdd(dst, bundle, _RC)
       end if
    end do

    deallocate(itemTypes)
    deallocate(itemNames)

    _RETURN(ESMF_SUCCESS)
  end subroutine CopyStateItems

END SUBMODULE
