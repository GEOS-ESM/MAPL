! $Id$

#ifdef NAME_
#undef NAME_
#endif

#ifdef NAMESTR_
#undef NAMESTR_
#endif

#define NAME_ MAPL_CommsSend_
#define NAMESTR_ 'MAPL_CommsSend_'

#include "overload.macro"

  subroutine SUB_ ( layout, DATA, COUNT, DEST, RC)

    type (ESMF_DELayout)                         :: layout
#if (VARTYPE_ > 0)
    TYPE_(kind=EKIND_), intent(INOUT)            :: data DIMENSIONS_
#else
    character(LEN=*),   intent(INOUT)            :: data DIMENSIONS_
#endif

    integer,            intent(in   )            :: COUNT
    integer,            intent(in   )            :: DEST
    integer         ,   intent(  out),  optional :: RC

    character(len=ESMF_MAXSTR), parameter :: IAM='MAPL_Send'
    integer                               :: status
    integer                               :: ierr
    integer                               :: comm
    type(ESMF_VM)                         :: vm

    call ESMF_DELayoutGet(layout, vm=vm, rc=status)
    _VERIFY(STATUS)

    call ESMF_VMGet(vm, mpiCommunicator=COMM, rc=status)
    _VERIFY(STATUS)

    call MPI_Send(DATA, count, MPITYPE_, dest, msg_tag, COMM, IERR)
    _VERIFY(IERR)

    _RETURN(ESMF_SUCCESS)
  END SUBROUTINE SUB_

#undef NAME_
#undef NAMESTR_

#undef DIMENSIONS_
#undef RANK_
#undef RANKSTR_
#undef VARTYPE_


