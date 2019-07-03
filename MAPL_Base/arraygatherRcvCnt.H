! $Id$

#ifdef NAME_
#undef NAME_
#endif

#ifdef NAMESTR_
#undef NAMESTR_
#endif

#define NAME_ ArrayGatherRcvCnt_
#define NAMESTR_ 'ArrayGatherRcvCnt_'

#include "overload.macro"

  subroutine SUB_(local_array, global_array, recvCounts, vm, dstPe, rc)
    TYPE_(kind=EKIND_), intent(IN   )            :: local_array DIMENSIONS_
    TYPE_(kind=EKIND_), intent(  OUT)            :: global_array DIMENSIONS_
    integer                                      :: recvcounts(:)
    type(ESMF_VM)                                :: vm
    integer, optional,  intent(IN   )            :: dstPe
    integer, optional, intent(OUT)               :: rc
    
! Local variables

    integer                               :: status
    character(len=ESMF_MAXSTR)            :: IAm='ArrayGatherRcvCnt'

    integer, allocatable, dimension(:)    :: displs
    integer                               :: nDEs
    integer                               :: sendcount
    
    integer                               :: I
    integer                               :: IM1 ! 'I minus 1'
    integer                               :: deId ! index of my PE
    integer                               :: dstDE
    
! This version works only for 1D arrays! 

    _ASSERT(RANK_ == 1, 'only rank 1 is supported')

    if(present(dstPe)) then
      dstDE = dstPe 
    else
      dstDE = MAPL_Root
    end if

    call ESMF_VMGet(vm, localPet=deId, petCount=ndes, rc=status)
    _VERIFY(STATUS)


    _ASSERT(size(recvCounts) == nDEs, 'recvcounts must match nDEs')

    allocate (displs(0:nDEs), stat=status)
    _VERIFY(STATUS)

    displs(0) = 0
    do I = 1,nDEs
       IM1 = I - 1
       displs(I) = displs(IM1) + recvcounts(I)
    enddo

    ! Count I will send
    sendcount = recvcounts(deId+1)

    call ESMF_VMGatherV(vm, local_array, sendcount, &
                        global_array, recvcounts, displs, dstDE, rc=status)
    _VERIFY(STATUS)

    deallocate(displs, stat=status)
    _VERIFY(STATUS)

    _RETURN(ESMF_SUCCESS)
  end subroutine SUB_

#undef NAME_
#undef NAMESTR_

#undef DIMENSIONS_
#undef RANK_
#undef RANKSTR_
#undef VARTYPE_
