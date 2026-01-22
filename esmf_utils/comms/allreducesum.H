#ifdef NAME_
#undef NAME_
#endif

#ifdef NAMESTR_
#undef NAMESTr_
#endif

#define NAME_ comms_allreduce_sum_
#define NAMESTR_ 'comms_allreduce_sum_'

#include "overload.macro"

subroutine SUB_ (vm, sendbuf, recvbuf, cnt, rc)
   type(ESMF_VM), intent(in) :: vm
   TYPE_(kind=EKIND_), intent(in) :: sendbuf DIMENSIONS_
   TYPE_(kind=EKIND_), intent(inout) :: recvbuf DIMENSIONS_
   integer, intent(in) :: cnt
   integer, intent(out), optional :: rc

   integer :: comm, status

   call ESMF_VMGet(vm, mpiCommunicator=comm, _RC)
   call mpi_allreduce(sendbuf, recvbuf, cnt, MPITYPE_, MPI_SUM, comm, status)
   _VERIFY(STATUS)

   _RETURN(_SUCCESS)
end subroutine SUB_

#undef NAME_
#undef NAMESTR_

#undef DIMENSIONS_
#undef RANK_
#undef RANKSTR_
#undef VARTYPE_

