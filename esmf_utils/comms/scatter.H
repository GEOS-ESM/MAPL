#ifdef NAME_
#undef NAME_
#endif

#ifdef NAMESTR_
#undef NAMESTR_
#endif

#define NAME_ comms_scatterv_
#define NAMESTR_ 'comms_scatterv_'

#include "overload.macro"

subroutine SUB_ (layout, sendbuf, sendcnts, displs, recvbuf, recvcnt, root, rc)
   type(ESMF_DELayout), intent(in) :: layout
   TYPE_(kind=EKIND_), intent(in) :: sendbuf(:)
   integer, intent(in) :: sendcnts(:)
   TYPE_(kind=EKIND_), intent(out) :: recvbuf DIMENSIONS_
   integer, intent(in) :: recvcnt
   integer, intent(in) :: displs(:)
   integer, intent(in) :: root
   integer, intent(out), optional :: RC

   integer                               :: status
   type(ESMF_VM)                         :: vm
   integer                               :: comm

   call ESMF_DELayoutGet(layout, vm=vm, _RC)
#if 1
   call ESMF_VMGet(vm, mpiCommunicator = comm, _RC)
   call mpi_scatterv( &
        sendbuf, sendcnts, displs,  MPITYPE_, &
        recvbuf, recvcnt, MPITYPE_, &
        root, comm, status)
   _VERIFY(STATUS)
#else
   call ESMF_VMScatterv( &
        vm, sendbuf, sendcnts, displs, &
        recvbuf, recvcnt, &
        root, status)
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
