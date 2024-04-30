!------------------------------------------------------------------------------
!               Global Modeling and Assimilation Office (GMAO)                !
!                    Goddard Earth Observing System (GEOS)                    !
!                                 MAPL Component                              !
!------------------------------------------------------------------------------
!
#include "MAPL_Generic.h"
#include "unused_dummy.H"
!
!>
!### MODULE: `MAPL_HistoryGridCompMod`
!
! Author: GMAO SI-Team
!
! `MAPL_HistoryGridCompMod` contains the `Initialize`, `Run` and `Finalize` methods for `History`.
! The three methods are called at the level of CAP.
!
  SUBMODULE (MAPL_HistoryGridCompMod) RegridTransform
!
! !USES:
!
  USE ESMF

  implicit none

contains


  MODULE SUBROUTINE RegridTransform(STATE_IN, XFORM, STATE_OUT, LS_IN, LS_OUT, NTILES_IN, NTILES_OUT, RC)

   intrinsic :: size

    type (ESMF_State)        , intent(IN   ) :: STATE_IN
    type (ESMF_State)        , intent(INOUT) :: STATE_OUT
    type(MAPL_LocStreamXform), intent(IN   ) :: XFORM
    type(MAPL_LocStream)     , intent(IN   ) :: LS_IN, LS_OUT
    integer                  , intent(IN   ) :: NTILES_IN, NTILES_OUT
    integer, optional        , intent(  OUT) :: RC

    integer                    :: STATUS

    integer                         :: L, LM
    integer                         :: LL, LU
    integer                         :: I
    integer                         :: rank_in
    integer                         :: rank_out
    integer                         :: itemcount, itemcount_in, itemcount_out
    real, allocatable, dimension(:) :: tile_in, tile_out
    real, pointer                   :: ptr2d_in(:,:)
    real, pointer                   :: ptr2d_out(:,:)
    real, pointer                   :: ptr3d_in(:,:,:)
    real, pointer                   :: ptr3d_out(:,:,:)
    type(ESMF_Array)                :: array_in
    type(ESMF_Array)                :: array_out
    type(ESMF_Field)                :: field
    type (ESMF_StateItem_Flag), pointer  :: ITEMTYPES_IN(:), ITEMTYPES_OUT(:)
    character(len=ESMF_MAXSTR ), pointer :: ITEMNAMES_IN(:), ITEMNAMES_OUT(:)

    allocate(tile_in (ntiles_in ), _STAT)
    allocate(tile_out(ntiles_out), _STAT)


    call ESMF_StateGet(STATE_IN,  ITEMCOUNT=ITEMCOUNT_IN,  _RC)
    call ESMF_StateGet(STATE_OUT, ITEMCOUNT=ITEMCOUNT_OUT, _RC)

    _ASSERT(ITEMCOUNT_IN == ITEMCOUNT_OUT,'needs informative message')

    ITEMCOUNT = ITEMCOUNT_IN
    _ASSERT(ITEMCOUNT>0,'needs informative message')

    allocate(ITEMNAMES_IN(ITEMCOUNT),_STAT)
    allocate(ITEMTYPES_IN(ITEMCOUNT),_STAT)

    call ESMF_StateGet(STATE_IN, ITEMNAMELIST=ITEMNAMES_IN, &
                       ITEMTYPELIST=ITEMTYPES_IN, _RC)

    allocate(ITEMNAMES_OUT(ITEMCOUNT),_STAT)
    allocate(ITEMTYPES_OUT(ITEMCOUNT),_STAT)

    call ESMF_StateGet(STATE_OUT, ITEMNAMELIST=ITEMNAMES_OUT, &
                       ITEMTYPELIST=ITEMTYPES_OUT, _RC)

    DO I=1, ITEMCOUNT
       _ASSERT(ITEMTYPES_IN (I) == ESMF_StateItem_Field,'needs informative message')
       _ASSERT(ITEMTYPES_OUT(I) == ESMF_StateItem_Field,'needs informative message')

       call ESMF_StateGet(STATE_IN , ITEMNAMES_IN (i), field, _RC)
       call ESMF_FieldGet(field, Array=array_in , _RC)
       call ESMF_StateGet(STATE_OUT, ITEMNAMES_OUT(i), field, _RC)
       call ESMF_FieldGet(field, Array=array_out, _RC)

       call ESMF_ArrayGet(array_in , rank=rank_in , _RC)
       call ESMF_ArrayGet(array_out, rank=rank_out, _RC)
       _ASSERT(rank_in == rank_out,'needs informative message')
       _ASSERT(rank_in >=2, 'Rank is less than 2')
       _ASSERT(rank_in <= 3,'Rank is greater than 3')

       if (rank_in == 2) then
          LM = 1
          LL = 1
          LU = 1
          call ESMF_ArrayGet(array_in , localDE=0, farrayptr=ptr2d_in , _RC)
          call ESMF_ArrayGet(array_out, localDE=0, farrayptr=ptr2d_out, _RC)
       else
          call ESMF_ArrayGet(array_in , localDE=0, farrayptr=ptr3d_in , _RC)
          call ESMF_ArrayGet(array_out, localDE=0, farrayptr=ptr3d_out, _RC)
          LM = size(ptr3d_in,3)
          LL = lbound(ptr3d_in,3)
          LU = ubound(ptr3d_in,3)
          _ASSERT(size(ptr3d_out,3) == LM,'needs informative message')
          _ASSERT(lbound(ptr3d_out,3) == LL,'needs informative message')
          _ASSERT(ubound(ptr3d_out,3) == LU,'needs informative message')
       end if

       DO L=LL,LU
          if (rank_in == 3) then
             ptr2d_in  => ptr3d_in (:,:,L)
             ptr2d_out => ptr3d_out(:,:,L)
          end if

          call MAPL_LocStreamTransform(LS_IN, TILE_IN, PTR2d_IN, _RC)

          call MAPL_LocStreamTransform( tile_out, XFORM, tile_in, _RC )

          call MAPL_LocStreamTransform(LS_OUT, PTR2d_OUT, TILE_OUT, _RC)

       ENDDO

    ENDDO

    deallocate(itemtypes_out)
    deallocate(itemnames_out)
    deallocate(itemtypes_in)
    deallocate(itemnames_in)
    deallocate(tile_out)
    deallocate(tile_in )

    _RETURN(ESMF_SUCCESS)
  end subroutine RegridTransform

END SUBMODULE
