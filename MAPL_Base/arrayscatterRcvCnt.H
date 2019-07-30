! $Id$

#ifdef NAME_
#undef NAME_
#endif

#ifdef NAMESTR_
#undef NAMESTR_
#endif

#define NAME_ ArrayScatterRcvCnt_
#define NAMESTR_ ArrayScatterRcvCnt_

#include "overload.macro"

  subroutine SUB_(local_array, global_array, sendCounts, vm, srcPe, rc)
    TYPE_(kind=EKIND_),         intent(  OUT) :: local_array  DIMENSIONS_
    TYPE_(kind=EKIND_), target, intent(IN   ) :: global_array DIMENSIONS_
    integer                                   :: sendcounts(:)
    type(ESMF_VM)                             :: vm
    integer, optional,  intent(IN   )         :: srcPe
    integer, optional, intent(   OUT)         :: rc
    
! Local variables

    integer                               :: status
    character(len=ESMF_MAXSTR)            :: IAm='ArrayScatterRcvCnt'

    integer, allocatable, dimension(:)    :: displs
    integer                               :: nDEs
    integer                               :: recvcount
    
    integer                               :: I
    integer                               :: IM1 ! 'I minus 1'
    integer                               :: deId ! index of my PE
    integer                               :: srcDE
    
! This version works only for 1D arrays! 

    _ASSERT(RANK_ == 1, 'only rank = 1 is supported')

    if(present(srcPe)) then
      srcDE = srcPe 
    else
      srcDE = MAPL_Root
    end if

    call ESMF_VMGet(vm, localPet=deId, petCount=ndes, rc=status)
    _VERIFY(STATUS)

    _ASSERT(size(sendCounts) == nDEs, 'sendcounts must match nDEs')

    allocate (displs(0:nDEs), stat=status)
    _VERIFY(STATUS)

    displs(0) = 0
    do I = 1,nDEs
       IM1 = I - 1
       displs(I) = displs(IM1) + sendcounts(I)
    enddo

! Count I will recieve

    recvcount = sendcounts(deId+1)

    call ESMF_VMScatterV(vm, global_array, sendcounts, displs, &
                         local_array, recvcount, srcDE, rc=status)
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
