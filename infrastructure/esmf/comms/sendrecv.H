#ifdef NAME_
#undef NAME_
#endif

#ifdef NAMESTR_
#undef NAMESTR_
#endif

#define NAME_ comms_sendrecv_
#define NAMESTR_ 'comms_sendrecv_'

#include "overload.macro"

subroutine SUB_ (layout, sendbuf, sendcount, dest, recvbuf, recvcount, source, rc)

   type(ESMF_DELayout), intent(in) :: layout
#if (VARTYPE_ > 0)
   TYPE_(kind=EKIND_), intent(inout) :: sendbuf DIMENSIONS_
   TYPE_(kind=EKIND_), intent(inout) :: recvbuf DIMENSIONS_
#else
   character(LEN=*), intent(inout) :: sendbuf DIMENSIONS_
   character(LEN=*), intent(inout) :: recvbuf DIMENSIONS_
#endif
   integer, intent(in) :: sendcount
   integer, intent(in) :: recvcount
   integer, intent(in) :: dest
   integer, intent(in) :: source
   integer, intent(out), optional :: rc

   type(ESMF_VM) :: vm
   integer :: comm, mpstatus(MPI_STATUS_SIZE), ierr, status

   call ESMF_DELayoutGet(layout, vm=vm, _RC)
   call ESMF_VMGet(vm, mpiCommunicator=comm, _RC)
   call MPI_SendRecv( &
        sendbuf, sendcount, MPITYPE_, dest,   msg_tag, &
        recvbuf, recvcount, MPITYPE_, source, msg_tag, &
        comm, mpstatus, ierr)
   _VERIFY(IERR)
  
   _RETURN(_SUCCESS)
end subroutine SUB_

#undef NAME_
#undef NAMESTR_

#undef DIMENSIONS_
#undef RANK_
#undef RANKSTR_
#undef VARTYPE_
