! $Id$

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
    TYPE_(kind=EKIND_), intent(IN   )            :: local_array DIMENSIONS_
    TYPE_(kind=EKIND_), intent(  OUT)            :: global_array DIMENSIONS_
    type (ESMF_Grid)      :: grid
    integer, optional,  intent(IN   )   :: mask(:)
    integer, optional,  intent(IN   )   :: depe
    integer, optional,  intent(IN   )   :: hw
    integer, optional, intent(OUT)     :: rc
    
! Local variables

    integer                               :: status
    character(len=ESMF_MAXSTR)            :: IAm='ArrayGather'

    type (ESMF_DELayout)  :: layout
    type (ESMF_DistGrid)  :: distGrid
    integer,               allocatable    :: AL(:,:)
    integer,               allocatable    :: AU(:,:)
    integer, allocatable, dimension(:)            :: recvcounts, displs, kk
    integer                                       :: nDEs
    integer                                       :: sendcount
    
    integer                                       :: I, J, K, II
#if (RANK_ != 1)
    integer                                       :: LX, JJ
#endif
    integer                                       :: de, deId
    integer                                       :: I1, IN
    integer                                       :: ibeg,iend
    integer                                       :: gridRank
#if (RANK_ > 1)
    integer                                       :: J1, JN
    integer                                       :: jbeg,jend
#endif
    integer                                       :: ISZ, JSZ
    integer                                       :: destPE, myhw
    TYPE_(kind=EKIND_), allocatable               :: var(:)
    type(ESMF_VM) :: vm
    
! Works only on 1D and 2D arrays
! Note: for tile variables the gridRank is 1 
! and the case RANK_=2 needs additional attention 

    _ASSERT(RANK_ <= 2, 'rank must be <= 2')

    if(present(depe)) then
      destPE = depe 
    else
      destPE = MAPL_Root
    end if

    if(present(hw)) then
      myhw = hw
    else
      myhw = 0
    end if

    call ESMF_GridGet    (GRID,   dimCount=gridRank, rc=STATUS);_VERIFY(STATUS)
    call ESMF_GridGet    (GRID,   distGrid=distGrid, rc=STATUS);_VERIFY(STATUS)
    call ESMF_DistGridGet(distGRID, delayout=layout, rc=STATUS);_VERIFY(STATUS)
    call ESMF_DELayoutGet(layout, vm=vm, rc=status);_VERIFY(STATUS)
    call ESMF_VmGet(vm, localPet=deId, petCount=nDEs, rc=status);_VERIFY(STATUS)

    allocate (AL(gridRank,0:nDEs-1),  stat=status)
    _VERIFY(STATUS)
    allocate (AU(gridRank,0:nDEs-1),  stat=status)
    _VERIFY(STATUS)

    call MAPL_DistGridGet(distgrid, &
         minIndex=AL, maxIndex=AU, rc=status)
    _VERIFY(STATUS)

    allocate (recvcounts(nDEs), displs(0:nDEs), stat=status)
    _VERIFY(STATUS)

    if (deId == destPE) then
       allocate(VAR(0:size(GLOBAL_ARRAY)-1), stat=status)
       _VERIFY(STATUS)
    else
       allocate(VAR(0), stat=status)
       _VERIFY(STATUS)
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
       call MAPL_CommsGatherV(layout, local_array, sendcount, &
                              var, recvcounts, displs, destPE, status)
    else
#if (RANK_ > 1)
       call MAPL_CommsGatherV(layout, local_array(ibeg:iend,jbeg:jend), &
                              sendcount, var, recvcounts, displs, destPE, &
                              status)
#else
       call MAPL_CommsGatherV(layout, local_array(ibeg:iend), sendcount, &
                              var, recvcounts, displs, destPE, status)
#endif
    end if
    _VERIFY(STATUS)

    if (deId == destPE) then
       if (present(mask)) then
          ISZ = size(mask)

#if (RANK_ == 2)
          JSZ = size(GLOBAL_ARRAY,2)
#else
          JSZ = 1
#endif

          allocate(KK (0:nDEs-1        ), stat=status)
          _VERIFY(STATUS)
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

          deallocate(KK, stat=status)
          _VERIFY(STATUS)
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

    deallocate(VAR, stat=status)
    _VERIFY(STATUS)
    deallocate(recvcounts, displs, AU, AL, stat=status)
    _VERIFY(STATUS)

    call ESMF_VmBarrier(vm, rc=status)
    _VERIFY(STATUS)
    _RETURN(ESMF_SUCCESS)
  end subroutine SUB_

#undef NAME_
#undef NAMESTR_

#undef DIMENSIONS_
#undef RANK_
#undef RANKSTR_
#undef VARTYPE_
