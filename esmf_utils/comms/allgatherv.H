#ifdef NAME_
#undef NAME_
#endif

#ifdef NAMESTR_
#undef NAMESTR_
#endif

#define NAME_ comms_allgatherv_
#define NAMESTR_ 'comms_allgatherv_'

#include "overload.macro"

subroutine SUB_ (layout, sendbuf, sendcnt, recvbuf, recvcnts, displs, rc)
   type(ESMF_DELayout) :: layout
   TYPE_(kind=EKIND_), intent(in) :: sendbuf DIMENSIONS_
   integer, intent(in) :: sendcnt
   TYPE_(kind=EKIND_), intent(inout) :: recvbuf DIMENSIONS_
   integer, intent(in) :: recvcnts(:)
   integer, intent(in) :: displs(:)
   integer, intent(out), optional :: rc

   type(ESMF_VM) :: vm
   integer :: comm, status

   call ESMF_DELayoutGet(layout, vm=vm, _RC)
   call ESMF_VMGet(vm, mpiCommunicator=comm, _RC)
   call mpi_allgatherv( &
        sendbuf, sendcnt,  MPITYPE_, &
        recvbuf, recvcnts, displs, MPITYPE_, &
        comm, status )
   _VERIFY(STATUS)

   _RETURN(_SUCCESS)
end subroutine SUB_

#undef NAME_
#undef NAMESTR_

#undef DIMENSIONS_
#undef RANK_
#undef RANKSTR_
#undef VARTYPE_
