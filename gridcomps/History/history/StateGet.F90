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
  SUBMODULE (MAPL_HistoryGridCompMod) StateGet
!
! !USES:
!
  implicit none

contains


  MODULE SUBROUTINE MAPL_StateGet(state,name,field,force_field,rc)

    type(ESMF_State), intent(in) :: state
    character(len=*), intent(in) :: name
    type(ESMF_Field), intent(inout) :: field
    logical, optional, intent(in) :: force_field
    integer, optional, intent(out  ) :: rc

    integer :: status
    character(len=ESMF_MAXSTR) :: bundlename, fieldname
    type(ESMF_FieldBundle) :: bundle
    logical :: local_force_field
    integer :: i

    if (present(force_field)) then
       local_force_field = force_field
    else
       local_force_field = .false.
    end if
    i = 0
    if (.not.local_force_field) i = index(name,"%")
    if (i.ne.0) then
        bundlename = name(:i-1)
        fieldname = name(i+1:)
        call ESMF_StateGet(state,trim(bundlename),bundle,rc=status)
        _ASSERT(status==ESMF_SUCCESS,'Bundle '//trim(bundlename)//' not found')
        call ESMF_FieldBundleGet(bundle,trim(fieldname),field=field,rc=status)
        _ASSERT(status==ESMF_SUCCESS,'Field '//trim(fieldname)//' not found')
    else
       call ESMF_StateGet(state,trim(name),field,rc=status)
        _ASSERT(status==ESMF_SUCCESS,'Field '//trim(name)//' not found')
        _VERIFY(STATUS)
    end if

    _RETURN(ESMF_SUCCESS)

  end subroutine MAPL_StateGet

END SUBMODULE
