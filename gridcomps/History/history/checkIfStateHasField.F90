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
  SUBMODULE (MAPL_HistoryGridCompMod) checkIfStateHasField
!
! !USES:
!
  implicit none

contains


  MODULE SUBROUTINE checkIfStateHasField(state, input_fieldName, hasField, rc)

    type(ESMF_State), intent(in) :: state ! export state
    character(len=*), intent(in) :: input_fieldName
    logical, intent(out)         :: hasField
    integer, intent(out), optional :: rc ! Error code:

    integer :: n, i, status, p_index
    character (len=ESMF_MAXSTR), allocatable  :: itemNameList(:)
    type(ESMF_StateItem_Flag),   allocatable  :: itemTypeList(:)
    character(len=:),allocatable :: field_name,bundle_name
    logical :: is_bundle,isPresent
    type(ESMF_FieldBundle) :: bundle

    call ESMF_StateGet(state, itemcount=n,  _RC)

    allocate(itemNameList(n), _STAT)
    allocate(itemTypeList(n), _STAT)
    call ESMF_StateGet(state,itemnamelist=itemNamelist,itemtypelist=itemTypeList,_RC)
    p_index = index(input_fieldName,"%")
    if (p_index/=0) then
       is_bundle = .true.
       bundle_name = input_fieldName(1:p_index-1)
       field_name = input_fieldName(p_index+1:)
    else
       is_bundle = .false.
       field_name = input_fieldName
    end if

    hasField = .false.
    if (is_bundle) then
      do I=1,N
         if(itemTypeList(I)/=ESMF_STATEITEM_FIELDBUNDLE) cycle
         if(itemNameList(I)==bundle_name) then
            call ESMF_StateGet(state,bundle_name,bundle,_RC)
            call ESMF_FieldBundleGet(bundle,field_name,isPresent=isPresent,_RC)
            if (isPresent) then
               hasField = .true.
               exit
            end if
         end if
      end do

    else
      do I=1,N
         if(itemTypeList(I)/=ESMF_STATEITEM_FIELD) cycle
         if(itemNameList(I)==field_name) then
            hasField = .true.
            exit
         end if
      end do
    end if
    deallocate(itemNameList, _STAT)
    deallocate(itemTypeList, _STAT)

    _RETURN(ESMF_SUCCESS)
  end subroutine checkIfStateHasField

END SUBMODULE
