
#ifdef NAME_
#undef NAME_
#endif

#ifdef NAMESTR_
#undef NAMESTR_
#endif

#define NAME_ ArrayScatter_
#define NAMESTR_ 'ArrayScatter_'

#include "overload.macro"

  subroutine SUB_(local_array, global_array, grid, mask, depe, hw, rc)

! Mask is really a permutation on the first dimension

    TYPE_(kind=EKIND_),         intent(  OUT) :: local_array  DIMENSIONS_
    TYPE_(kind=EKIND_), target, intent(IN   ) :: global_array DIMENSIONS_
    type (ESMF_Grid)                          :: grid
    integer, optional,          intent(IN   ) :: mask(:)
    integer, optional,          intent(IN   ) :: depe
    integer, optional,          intent(IN   ) :: hw
    integer, optional,          intent(  OUT) :: rc

! Local variables

    integer                               :: status

    TYPE_(kind=EKIND_),    pointer        :: myglob DIMENSIONS_ => null()
    TYPE_(kind=EKIND_),    pointer        :: VAR(:)
    type (ESMF_DistGrid)                  :: distGrid
    type(ESMF_DELayout)                   :: LAYOUT
    integer,               allocatable    :: AL(:,:)
    integer,               allocatable    :: AU(:,:)
    integer, dimension(:), allocatable    :: SENDCOUNTS, DISPLS, KK
    integer                               :: nDEs
    integer                               :: recvcount
    integer                               :: II, deId
    integer                               :: I, K
#if (RANK_ == 2)
    integer                               :: J, JJ
#endif
#if (RANK_ != 1)
    integer                               :: I1, IN, J1, JN
#endif
    integer                               :: gridRank
    integer                               :: LX
#if (RANK_ != 1)
    integer                               :: LY
#endif
    integer                               :: srcPE
    integer                               :: MYHW, ISZ, JSZ
    logical                               :: alloc_var
    type(ESMF_VM)                         :: vm

! Works only on 1D and 2D arrays
! Note: for tile variables the gridRank is 1
! and the case RANK_=2 needs additional attention

    _ASSERT(RANK_ <= 2, 'rank must be <= 2')

! Optional change of source PE. Default=MAPL_Root

    if(present(depe)) then
       srcPE = depe
    else
       srcPE = MAPL_Root
    end if

! Optional single halo width

    if(present(hw)) then
       myhw = hw
    else
       myhw = 0
    end if

! Some halo limitations

    if(myhw > 0) then
       _ASSERT(RANK_ == 2, 'no halo on 1d')
       _ASSERT(.not.present(MASK), 'No halo allowed if 1st dim is permutted')
    end if

! Initialize
    alloc_var=.false.

! Get grid and layout information

    call ESMF_GridGet    (GRID,   dimCount=gridRank, rc=STATUS);_VERIFY(STATUS)
    call ESMF_GridGet    (GRID,   distGrid=distGrid, rc=STATUS);_VERIFY(STATUS)
    call ESMF_DistGridGet(distGRID, delayout=layout, rc=STATUS);_VERIFY(STATUS)
    call ESMF_DELayoutGet(layout, vm=vm, rc=status);_VERIFY(STATUS)
    call ESMF_VmGet(vm, localPet=deId, petCount=nDEs, rc=status);_VERIFY(STATUS)

    allocate (AL(gridRank,0:nDEs-1),  stat=status)
    _VERIFY(STATUS)
    allocate (AU(gridRank,0:nDEs-1),  stat=status)
    _VERIFY(STATUS)
    allocate (sendcounts(0:nDEs-1), stat=status)
    _VERIFY(STATUS)
    call MAPL_DistGridGet(distgrid, &
         minIndex=AL, maxIndex=AU, rc=status)
    _VERIFY(STATUS)

! Compute count to be sent to each PE

    if(present(mask)) then
       ISZ = size(mask)

#if (RANK_ == 2)
       JSZ = size(GLOBAL_ARRAY,2)
#else
       JSZ = 1
#endif

       sendcounts = 0
       do II = 1,ISZ
          sendcounts(mask(ii)) = sendcounts(mask(ii)) + 1
       enddo
       sendcounts = sendcounts*JSZ

    else
       do I = 0,nDEs-1
          LX = AU(1,I) - AL(1,I) + 1 + 2*MYHW
#if (RANK_ == 1)
          sendcounts(I) = LX
#else
          LY = AU(2,I) - AL(2,I) + 1 + 2*MYHW
          sendcounts(I) = LX*LY
#endif
       end do
    end if

! Count I will recieve

    recvcount = sendcounts(deId)

! Put VAR together at the srcPE

    if (deId == srcPE) then

       allocate(DISPLS(0:nDEs          ), stat=status)
       _VERIFY(STATUS)

! Compute displacements into the VAR vector

       displs(0) = 0
       do I = 1,nDEs
          displs(I) = displs(I-1) + sendcounts(I-1)
       end do
!ALT       _ASSERT(displs(nDEs) == (ISZ+2*myhw)*(JSZ+2*myhw),'needs informative message')

! If there is a halo, make a haloed copy of the global array.
!   otherwise just copy the pointer.

       myglob => global_array

#if (RANK_ == 2)
       if (myhw > 0) then
          ISZ = size(GLOBAL_ARRAY,1)
          JSZ = size(GLOBAL_ARRAY,2)
          allocate(myglob(1-myhw:isz+myhw,1-myhw:jsz+myhw), stat=status)
          _VERIFY(STATUS)
          myglob(1:isz,1:jsz) = GLOBAL_ARRAY

! Fill the halo (I is cyclic)

          do j=1,myhw
             myglob(1  -j,:) = myglob(isz-j+1,:)
             myglob(isz+j,:) = myglob(    j  ,:)
             myglob(:,1  -j) = MAPL_Undef
             myglob(:,jsz+j) = MAPL_Undef
          enddo
       endif
#endif

! Fill the VAR vector

       if (present(mask)) then
          alloc_var = .true.
          allocate(VAR(0:displs(nDEs)-1), stat=status)
          _VERIFY(STATUS)
          allocate(KK (0:nDEs-1        ), stat=status)
          _VERIFY(STATUS)
          KK = DISPLS(0:nDEs-1)

          do I=1,ISZ
             K = MASK(I)
             II = KK(K)
#if (RANK_ == 1)
             VAR(II) = MYGLOB(I)
#else
             LX = AU(1,K) - AL(1,K) + 1
             do J=1,JSZ
                VAR(II+LX*(J-1)) = MYGLOB(I,J)
             end do
#endif
             KK(MASK(I)) = KK(MASK(I)) + 1
          end do

          deallocate(KK, stat=status)
          _VERIFY(STATUS)

       else

#if (RANK_ == 1)
          alloc_var = .false.
          var => myglob
#else
          alloc_var = .true.
          allocate(VAR(0:displs(nDEs)-1), stat=status)
          _VERIFY(STATUS)

          if (gridRank == 1) then
             J1 = lbound(local_array,2)
             JN = ubound(local_array,2)
          endif
          do I = 0,nDEs-1
             I1 = AL(1,I) - myhw
             IN = AU(1,I) + myhw
             if (gridRank > 1) then
                J1 = AL(2,I) - myhw
                JN = AU(2,I) + myhw
             end if

             K = displs(I)
             do JJ=J1,JN
                do II=I1,IN
                   var(K) = MYglob(II,JJ)
                   K = K+1
                end do
             end do
          end do
#endif

       endif !  present(mask)

       if (myhw > 0) then
          deallocate(myglob, stat=status)
          _VERIFY(STATUS)
       end if

     else
        alloc_var = .true.
        allocate(var(0:1), stat=status)
        _VERIFY(STATUS)
        allocate(DISPLS(0:nDEs), stat=status)
        _VERIFY(STATUS)
     end if !  I am srcPEa


! Do the communications

    call MAPL_CommsScatterV(layout, var, sendcounts, displs, &
                            local_array, recvcount, srcPE, status)
    _VERIFY(STATUS)

! Clean-up

    deallocate(displs, stat=status)
    _VERIFY(STATUS)
    if(alloc_var) then
       deallocate(VAR, stat=status)
       _VERIFY(STATUS)
    end if

    deallocate(sendcounts, stat=status)
    _VERIFY(STATUS)
    deallocate(AU,         stat=status)
    _VERIFY(STATUS)
    deallocate(AL,         stat=status)
    _VERIFY(STATUS)

! All done

    _RETURN(ESMF_SUCCESS)
  end subroutine SUB_

#undef NAME_
#undef NAMESTR_

#undef DIMENSIONS_
#undef RANK_
#undef RANKSTR_
#undef VARTYPE_
