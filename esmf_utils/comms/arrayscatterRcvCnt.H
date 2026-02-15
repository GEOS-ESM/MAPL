#ifdef NAME_
#undef NAME_
#endif

#ifdef NAMESTR_
#undef NAMESTR_
#endif

#define NAME_ array_scatter_rcv_cnt_
#define NAMESTR_ array_scatter_rcv_cnt_

#include "overload.macro"

subroutine SUB_(local_array, global_array, send_counts, vm, src_pe, rc)
   TYPE_(kind=EKIND_), intent(out) :: local_array  DIMENSIONS_
   TYPE_(kind=EKIND_), target, intent(in) :: global_array DIMENSIONS_
   integer  :: send_counts(:)
   type(ESMF_VM) :: vm
   integer, optional, intent(in) :: src_pe
   integer, optional, intent(out) :: rc

   ! Local variables
   integer, allocatable :: displs(:)
   integer :: status, num_des, recv_count, i
   integer :: de_id ! index of my PE
   integer :: src_de

   ! This version works only for 1D arrays!
   _ASSERT(RANK_ == 1, 'only rank = 1 is supported')

   src_de = ROOT_PROCESS_ID
   if(present(src_pe)) src_de = src_pe

   call ESMF_VMGet(vm, localPet=de_id, petCount=num_des, _RC)
   _ASSERT(size(send_counts) == num_des, 'send_counts must match num_des')
   allocate (displs(0:num_des), _STAT)

   displs(0) = 0
   do i = 1, num_des
      displs(i) = displs(i-1) + send_counts(i)
   enddo

   ! Count I will recieve
   recv_count = send_counts(de_id+1)

   call ESMF_VMScatterV(vm, global_array, send_counts, displs, local_array, recv_count, src_de, _RC)

   deallocate(displs, _STAT)

   _RETURN(_SUCCESS)
end subroutine SUB_

#undef NAME_
#undef NAMESTR_

#undef DIMENSIONS_
#undef RANK_

#undef RANKSTR_
#undef VARTYPE_
