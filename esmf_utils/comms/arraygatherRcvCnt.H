#ifdef NAME_
#undef NAME_
#endif

#ifdef NAMESTR_
#undef NAMESTR_
#endif

#define NAME_ array_gather_rcv_cnt_
#define NAMESTR_ 'array_gather_rcv_cnt_'

#include "overload.macro"

subroutine SUB_(local_array, global_array, recv_counts, vm, dst_pe, rc)
   TYPE_(kind=EKIND_), intent(in) :: local_array DIMENSIONS_
   TYPE_(kind=EKIND_), intent(out) :: global_array DIMENSIONS_
   integer :: recv_counts(:)
   type(ESMF_VM) :: vm
   integer, optional, intent(in) :: dst_pe
   integer, optional, intent(out) :: rc

   ! Local variables
   integer, allocatable :: displs(:)
   integer :: num_des, send_count, i
   integer :: de_id ! index of my PE
   integer :: dst_de
   integer :: status

   ! This version works only for 1D arrays!
   _ASSERT(RANK_ == 1, 'only rank 1 is supported')

   dst_de = ROOT_PROCESS_ID
   if(present(dst_pe)) dst_de = dst_pe

   call ESMF_VMGet(vm, localPet=de_id, petCount=num_des, _RC)
   _ASSERT(size(recv_counts) == num_des, 'recv_counts must match num_des')
   allocate(displs(0:num_des), _STAT)

   displs(0) = 0
   do i = 1, num_des
      displs(i) = displs(i-1) + recv_counts(i)
   enddo

   ! Count I will send
   send_count = recv_counts(de_id+1)

   call ESMF_VMGatherV(vm, local_array, send_count, global_array, recv_counts, displs, dst_de, _RC)

   deallocate(displs, _STAT)

   _RETURN(_SUCCESS)
end subroutine SUB_

#undef NAME_
#undef NAMESTR_

#undef DIMENSIONS_
#undef RANK_
#undef RANKSTR_
#undef VARTYPE_
