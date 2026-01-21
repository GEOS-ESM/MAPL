#ifdef NAME_
#undef NAME_
#endif

#ifdef NAMESTR_
#undef NAMESTR_
#endif

#define NAME_ ArrayGather_
#define NAMESTR_ 'ArrayGather_'

#include "overload.macro"

subroutine SUB_(local_array, global_array, grid, mask, depe, hw, rc)
   TYPE_(kind=EKIND_), intent(in) :: local_array DIMENSIONS_
   TYPE_(kind=EKIND_), intent(out) :: global_array DIMENSIONS_
   type(ESMF_Grid), intent(in) :: grid
   integer, optional, intent(in) :: mask(:)
   integer, optional, intent(in) :: depe
   integer, optional, intent(in) :: hw
   integer, optional, intent(out) :: rc

   ! Local variables
   integer :: status

   type(ESMF_DELayout) :: layout
   type(ESMF_DistGrid) :: distGrid
   type(ESMF_VM) :: vm
   integer, allocatable :: AL(:,:), AU(:,:)
   integer, allocatable :: recvcounts(:), displs(:), kk(:)
   integer :: nDEs, sendcount
   integer :: I, J, K, II
#if (RANK_ != 1)
   integer :: LX, JJ
#endif
   integer :: de, deId
   integer :: I1, IN, ibeg,iend
   integer :: gridRank
#if (RANK_ > 1)
   integer :: J1, JN, jbeg,jend
   integer :: jsz
#endif
   integer :: ISZ
   integer :: destPE, myhw
   TYPE_(kind=EKIND_), allocatable :: var(:)

   ! Works only on 1D and 2D arrays
   ! Note: for tile variables the gridRank is 1
   ! and the case RANK_=2 needs additional attention

   _ASSERT(RANK_ <= 2, 'rank must be <= 2')

   if(present(depe)) then
      destPE = depe
   else
      destPE = ROOT_PROCESS_ID
   end if

   if(present(hw)) then
      myhw = hw
   else
      myhw = 0
   end if

   call ESMF_GridGet(GRID, dimCount=gridRank, _RC)
   call ESMF_GridGet(GRID, distGrid=distGrid, _RC)
   call ESMF_DistGridGet(distGRID, delayout=layout, _RC);_VERIFY(STATUS)
   call ESMF_DELayoutGet(layout, vm=vm, _RC)
   call ESMF_VmGet(vm, localPet=deId, petCount=nDEs, _RC)

   allocate (AL(gridRank,0:nDEs-1), _STAT)
   allocate (AU(gridRank,0:nDEs-1), _STAT)

   call DistGridGet(distgrid, min_index=AL, max_index=AU, _RC)

   allocate (recvcounts(nDEs), displs(0:nDEs), _STAT)

   if (deId == destPE) then
      allocate(VAR(0:size(GLOBAL_ARRAY)-1), _STAT)
   else
      allocate(VAR(0), _STAT)
   end if

   displs(0) = 0
#if (RANK_ > 1)
   if (gridRank == 1) then
      J1 = lbound(local_array,2)
      JN = ubound(local_array,2)
   endif
#endif
   do I = 1,nDEs
      J = I - 1
      de = J
      I1 = AL(1,J)
      IN = AU(1,J)
#if (RANK_ > 1)
      if (gridRank > 1) then
         J1 = AL(2,J)
         JN = AU(2,J)
      end if
      recvcounts(I) = (IN - I1 + 1) * (JN - J1 + 1)
#else
      recvcounts(I) = (IN - I1 + 1)
#endif
      if (de == deId) then
         sendcount = recvcounts(I)      ! Count I will send
         ibeg = 1+myhw
         iend = IN-I1+1+myhw
#if (RANK_ > 1)
         jbeg = 1+myhw
         jend = JN-J1+1+myhw
#endif
      endif
      displs(I) = displs(J) + recvcounts(I)
   enddo

   if (present(mask) .or. myHW == 0) then
      call CommsGatherV(layout, local_array, sendcount, var, recvcounts, displs, destPE, status)
   else
#if (RANK_ > 1)
      call CommsGatherV(layout, local_array(ibeg:iend,jbeg:jend), &
           sendcount, var, recvcounts, displs, destPE, &
           status)
#else
      call CommsGatherV(layout, local_array(ibeg:iend), sendcount, var, recvcounts, displs, destPE, status)
#endif
   end if
   _VERIFY(STATUS)

   if (deId == destPE) then
      if (present(mask)) then
         ISZ = size(mask)

#if (RANK_ == 2)
         JSZ = size(GLOBAL_ARRAY,2)
#endif

         allocate(KK (0:nDEs-1        ), _STAT)
         KK = DISPLS(0:nDEs-1)

         do I=1,ISZ
            K = MASK(I)
            II = KK(K)
#if (RANK_ == 1)
            GLOBAL_ARRAY(I) = VAR(II)
#else
            LX = AU(1,K) - AL(1,K) + 1
            do J=1,JSZ
               GLOBAL_ARRAY(I,J) = VAR(II+LX*(J-1))
            end do
#endif
            KK(MASK(I)) = KK(MASK(I)) + 1
         end do

         deallocate(KK, _STAT)
      else
#if (RANK_ == 1)
         global_array = var ! ALT: I am not sure if this is correct
#else
         do I = 0,nDEs-1
            I1 = AL(1,I)
            IN = AU(1,I)
            J1 = AL(2,I)
            JN = AU(2,I)

            K = displs(I)
            do JJ=J1,JN
               do II=I1,IN
                  global_array(II,JJ) = var(K)
                  K = K+1
               end do
            end do
         end do
#endif
      end if ! if (present(mask))
   end if

   deallocate(VAR, _STAT)
   deallocate(recvcounts, displs, AU, AL, _STAT)

   call ESMF_VmBarrier(vm, _RC)

   _RETURN(_SUCCESS)
end subroutine SUB_

#undef NAME_
#undef NAMESTR_

#undef DIMENSIONS_
#undef RANK_
#undef RANKSTR_
#undef VARTYPE_
