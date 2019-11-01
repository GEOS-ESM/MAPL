! $Id$

#ifdef NAME_
#undef NAME_
#endif

#ifdef NAMESTR_
#undef NAMESTR_
#endif

#define NAME_ MAPL_CommsAllGatherV_
#define NAMESTR_ 'MAPL_CommsAllGatherV_'

#include "overload.macro"

  subroutine SUB_ (layout, sendbuf, sendcnt, &
                   recvbuf, recvcnts, displs, RC)
    type (ESMF_DELayout)                         :: layout
    TYPE_(kind=EKIND_), intent(IN   )            :: sendbuf DIMENSIONS_
    integer,            intent(IN   )            :: sendcnt
    TYPE_(kind=EKIND_), intent(INOUT)            :: recvbuf DIMENSIONS_
    integer,            intent(IN   )            :: recvcnts(:)
    integer,            intent(IN   )            :: displs(:)
    integer         ,   intent(  OUT),  optional :: RC

    character(len=ESMF_MAXSTR), parameter :: IAM='MAPL_CommsAllGatherV'
    integer                               :: status
    integer                               :: comm
    type(ESMF_VM)                         :: vm

    call ESMF_DELayoutGet(layout, vm=vm, rc=status)
    _VERIFY(STATUS)

    call ESMF_VMGet(vm, mpiCommunicator=COMM, rc=status)
    _VERIFY(STATUS)

    call mpi_allgatherv ( sendbuf, sendcnt,  MPITYPE_, &
                          recvbuf, recvcnts, displs, MPITYPE_, &
                          comm, status )
    _VERIFY(STATUS)


    _RETURN(ESMF_SUCCESS)
  END SUBROUTINE SUB_

#undef NAME_
#undef NAMESTR_

#undef DIMENSIONS_
#undef RANK_
#undef RANKSTR_
#undef VARTYPE_

