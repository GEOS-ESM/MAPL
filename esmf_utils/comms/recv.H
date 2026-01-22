#ifdef NAME_
#undef NAME_
#endif

#ifdef NAMESTR_
#undef NAMESTR_
#endif

#define NAME_ comms_recv_
#define NAMESTR_ 'comms_recv_'

#include "overload.macro"

subroutine SUB_ (layout, data, count, source, rc)
   type(ESMF_DELayout), intent(in) :: layout
#if (VARTYPE_ > 0)
   TYPE_(kind=EKIND_), intent(inout) :: data DIMENSIONS_
#else
   character(len=*), intent(inout) :: data DIMENSIONS_
#endif
   integer, intent(in) :: count
   integer, intent(in) :: source
   integer, intent(out), optional :: rc

   type(ESMF_VM) :: vm
   integer :: comm, mpstatus(MPI_STATUS_SIZE), ierr, status

   call ESMF_DELayoutGet(layout, vm=vm, _RC)
   call ESMF_VMGet(vm, mpiCommunicator=comm, _RC)
   call MPI_Recv(data, count, MPITYPE_, source, MSG_TAG, comm, mpstatus, ierr)
   _VERIFY(IERR)

   _RETURN(_SUCCESS)
end subroutine SUB_

#undef NAME_
#undef NAMESTR_

#undef DIMENSIONS_
#undef RANK_
#undef RANKSTR_
#undef VARTYPE_
