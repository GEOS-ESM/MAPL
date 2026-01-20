#ifdef NAME_
#undef NAME_
#endif

#ifdef NAMESTR_
#undef NAMESTR_
#endif

#define NAME_ CommsGatherV_
#define NAMESTR_ 'CommsGatherV_'

#include "overload.macro"

subroutine SUB_ (layout, sendbuf, sendcnt, recvbuf, recvcnts, displs, root, rc)
   type(ESMF_DELayout) :: layout
   TYPE_(kind=EKIND_), intent(in) :: sendbuf DIMENSIONS_
   integer, intent(in) :: sendcnt
   TYPE_(kind=EKIND_), intent(out) :: recvbuf(:)
   integer, intent(in) :: recvcnts(:)
   integer, intent(in) :: displs(:)
   integer, intent(in) :: root
   integer, intent(out), optional :: rc

   type(ESMF_VM) :: vm
   integer :: comm, status

   call ESMF_DELayoutGet(layout, vm=vm, _RC)
   call ESMF_VMGet(vm, mpiCommunicator=COMM, _RC)

   call mpi_gatherv(sendbuf, sendcnt,  MPITYPE_, recvbuf, recvcnts, displs, MPITYPE_, root, comm, status)
   _VERIFY(STATUS)

#ifdef sysLinux
   call mpi_barrier(comm, status)
   _VERIFY(STATUS)
#endif

   _RETURN(_SUCCESS)
end subroutine SUB_

#undef NAME_
#undef NAMESTR_

#undef DIMENSIONS_
#undef RANK_
#undef RANKSTR_
#undef VARTYPE_
