#ifdef NAME_
#undef NAME_
#endif

#ifdef NAMESTR_
#undef NAMESTR_
#endif

#define NAME_ comms_allgather_
#define NAMESTR_ 'comms_allgather_'

#include "overload.macro"

subroutine SUB_ (vm, sendbuf, sendcnt, recvbuf, recvcnt, rc)
   type(ESMF_VM), intent(in) :: vm
   TYPE_, intent(in) :: sendbuf DIMENSIONS_
   integer, intent(in) :: sendcnt
   TYPE_, intent(inout) :: recvbuf DIMENSIONS_
   integer, intent(in) :: recvcnt
   integer, intent(out), optional :: rc

   integer :: comm, status

   call ESMF_VMGet(vm, mpiCommunicator=comm, _RC)

   call mpi_allgather( &
        sendbuf, sendcnt, MPITYPE_, &
        recvbuf, recvcnt, MPITYPE_, &
        comm, status)
   _VERIFY(STATUS)

   _RETURN(_SUCCESS)
end subroutine SUB_

#undef NAME_
#undef NAMESTR_

#undef DIMENSIONS_
#undef RANK_
#undef RANKSTR_
#undef VARTYPE_
