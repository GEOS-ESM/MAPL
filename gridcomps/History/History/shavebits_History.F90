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
  SUBMODULE (MAPL_HistoryGridCompMod) shavebits_smod
!
! !USES:
!
  implicit none

contains


  MODULE SUBROUTINE shavebits( state, list, rc)

    type(ESMF_state), intent(inout) :: state
    type (HistoryCollection), intent(in) :: list
    integer, optional, intent(out):: rc

    integer :: m, fieldRank, status
    type(ESMF_Field) :: field
    real, pointer :: ptr1d(:), ptr2d(:,:), ptr3d(:,:,:)
    type(ESMF_VM) :: vm
    integer :: comm

    call ESMF_VMGetCurrent(vm,_RC)
    call ESMF_VMGet(vm,mpiCommunicator=comm,_RC)

    do m=1,list%field_set%nfields
       call ESMF_StateGet(state, trim(list%field_set%fields(3,m)),field,_RC )
       call ESMF_FieldGet(field, rank=fieldRank,_RC)
       if (fieldRank ==1) then
          call ESMF_FieldGet(field, farrayptr=ptr1d, _RC)
          call DownBit(ptr1d,ptr1d,list%nbits_to_keep,undef=MAPL_undef,mpi_comm=comm,_RC)
       elseif (fieldRank ==2) then
          call ESMF_FieldGet(field, farrayptr=ptr2d, _RC)
          call DownBit(ptr2d,ptr2d,list%nbits_to_keep,undef=MAPL_undef,mpi_comm=comm,_RC)
       elseif (fieldRank ==3) then
          call ESMF_FieldGet(field, farrayptr=ptr3d, _RC)
          call DownBit(ptr3d,ptr3d,list%nbits_to_keep,undef=MAPL_undef,mpi_comm=comm,_RC)
       else
          _FAIL('The field rank is not implmented')
       endif
    enddo

    _RETURN(ESMF_SUCCESS)

  end subroutine

END SUBMODULE
