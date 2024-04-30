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
  SUBMODULE (MAPL_HistoryGridCompMod) RegridTransformT2G2G
!
! !USES:
!
  implicit none

contains


  MODULE SUBROUTINE RegridTransformT2G2G(STATE_IN, XFORM, XFORMntv, STATE_OUT, LS_IN, LS_OUT, LS_NTV, NTILES_IN, NTILES_OUT, RC)

   intrinsic :: size

    type (ESMF_State)        , intent(IN   ) :: STATE_IN
    type (ESMF_State)        , intent(INOUT) :: STATE_OUT
    type(MAPL_LocStreamXform), intent(IN   ) :: XFORM, XFORMntv
    type(MAPL_LocStream)     , intent(IN   ) :: LS_IN, LS_OUT, LS_NTV
    integer                  , intent(IN   ) :: NTILES_IN, NTILES_OUT
    integer, optional        , intent(  OUT) :: RC

    integer                    :: STATUS

    integer                         :: L, LM, K, KM
    integer                         :: I
    integer                         :: rank_in
    integer                         :: rank_out
    integer                         :: itemcount, itemcount_in, itemcount_out
    integer                         :: sizett
    real, pointer                   :: tile1d(:) => null()
    real, pointer                   :: tt(:)
    real, pointer                   :: tt_in(:)
    real, pointer                   :: G2d_in(:,:)
    real, pointer                   :: ptr1d_in(:)
    real, pointer                   :: ptr2d_in(:,:)
    real, pointer                   :: ptr3d_in(:,:,:)
    real(kind=REAL64), pointer                 :: p1dr8_in(:)
    real(kind=REAL64), pointer                 :: p2dr8_in(:,:)
    real(kind=REAL64), pointer                 :: p3dr8_in(:,:,:)
    real, pointer                   :: ptr2d_out(:,:)
    real, pointer                   :: ptr3d_out(:,:,:)
    real, pointer                   :: ptr4d_out(:,:,:,:)
    real, pointer                   :: tile_in(:)
    real, pointer                   :: tile_out(:)
    real, pointer                   :: out2d(:,:)
    type(ESMF_Array)                :: array_in
    type(ESMF_Array)                :: array_out
    type(ESMF_Field)                :: field
    type(ESMF_Grid)                 :: grid
    type(ESMF_TypeKind_Flag)        :: tk
    integer                         :: counts(3)
    type (ESMF_StateItem_Flag), pointer  :: ITEMTYPES_IN(:), ITEMTYPES_OUT(:)
    character(len=ESMF_MAXSTR ), pointer :: ITEMNAMES_IN(:), ITEMNAMES_OUT(:)

    allocate(tt_in (ntiles_in ), _STAT)
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

    call MAPL_LocStreamGet(LS_NTV, ATTACHEDGRID=GRID, _RC)
    call MAPL_GridGet(grid, localCellCountPerDim=COUNTS, _RC)
    allocate(G2d_in(COUNTS(1),COUNTS(2)), _STAT)

    call MAPL_LocStreamGet(LS_ntv, NT_LOCAL = sizett, _RC)
    allocate(tt(sizett), _STAT)

    DO I=1, ITEMCOUNT
       _ASSERT(ITEMTYPES_IN (I) == ESMF_StateItem_Field,'needs informative message')
       _ASSERT(ITEMTYPES_OUT(I) == ESMF_StateItem_Field,'needs informative message')

       call ESMF_StateGet(STATE_IN , ITEMNAMES_IN (i), field, _RC)
       call ESMF_FieldGet(field, Array=array_in , _RC)
       call ESMF_StateGet(STATE_OUT, ITEMNAMES_OUT(i), field, _RC)
       call ESMF_FieldGet(field, Array=array_out, _RC)

       call ESMF_ArrayGet(array_in , rank=rank_in , typekind=tk, _RC)
       call ESMF_ArrayGet(array_out, rank=rank_out, _RC)

       _ASSERT(rank_in+1 == rank_out,'needs informative message')
       _ASSERT(rank_in >=1, 'Rank is less than 1')
       _ASSERT(rank_in <= 3,'Rank is greater than 3')

       KM = 1
       if (rank_in == 1) then
          if (tk == ESMF_TypeKind_R4) then
             call ESMF_ArrayGet(array_in , localDE=0, farrayptr=ptr1d_in , _RC)
             tile_in => ptr1d_in
          else if (tk == ESMF_TypeKind_R8) then
             call ESMF_ArrayGet(array_in , localDE=0, farrayptr=p1dr8_in , _RC)
             if (.not. associated(tile1d)) then
                allocate(tile1d(size(p1dr8_in)), _STAT)
             end if
             tile1d = p1dr8_in
             tile_in => tile1d
          end if

          call ESMF_ArrayGet(array_out, localDE=0, farrayptr=ptr2d_out, _RC)
          out2d   => ptr2d_out
          LM = 1
       else if (rank_in == 2) then
          if (tk == ESMF_TypeKind_R4) then
             call ESMF_ArrayGet(array_in , localDE=0, farrayptr=ptr2d_in , _RC)
          else if (tk == ESMF_TypeKind_R8) then
             call ESMF_ArrayGet(array_in , localDE=0, farrayptr=p2dr8_in , _RC)
             if (.not. associated(tile1d)) then
                allocate(tile1d(size(p2dr8_in,1)), _STAT)
             end if
          end if

          call ESMF_ArrayGet(array_out, localDE=0, farrayptr=ptr3d_out, _RC)
          LM = size(ptr3d_out,3)
       else if (rank_in == 3) then
          if (tk == ESMF_TypeKind_R4) then
             call ESMF_ArrayGet(array_in , localDE=0, farrayptr=ptr3d_in , _RC)
          else if (tk == ESMF_TypeKind_R8) then
             call ESMF_ArrayGet(array_in , localDE=0, farrayptr=p3dr8_in , _RC)
             if (.not. associated(tile1d)) then
                allocate(tile1d(size(p3dr8_in,1)), _STAT)
             end if
          end if

          call ESMF_ArrayGet(array_out, localDE=0, farrayptr=ptr4d_out, _RC)
          LM = size(ptr4d_out,3)
          KM = size(ptr4d_out,4)
       else
          _RETURN(ESMF_FAILURE)
       end if

       DO K=1,KM
          DO L=1,LM
             if (rank_out == 3) then
                if (tk == ESMF_TypeKind_R4) then
                   tile_in  => ptr2d_in (:,L)
                else if (tk == ESMF_TypeKind_R8) then
                   tile1d = p2dr8_in(:,L)
                   tile_in => tile1d
                end if
                out2d    => ptr3d_out(:,:,L)
             else if (rank_out == 4) then
                if (tk == ESMF_TypeKind_R4) then
                   tile_in  => ptr3d_in (:,L,K)
                else if (tk == ESMF_TypeKind_R8) then
                   tile1d = p3dr8_in(:,L,K)
                   tile_in => tile1d
                end if
                out2d    => ptr4d_out(:,:,L,K)
             end if

             ! T2T
             call MAPL_LocStreamTransform( tt, XFORMntv, tile_in, _RC )
             ! T2G
             call MAPL_LocStreamTransform(LS_NTV, G2d_IN, tt, _RC)

             ! G2T
             call MAPL_LocStreamTransform(LS_IN, TT_IN, G2d_IN, _RC)
             ! T2T
             call MAPL_LocStreamTransform( tile_out, XFORM, tt_in, _RC )
             ! T2G
             call MAPL_LocStreamTransform(LS_OUT, PTR2d_OUT, TILE_OUT, _RC)

          ENDDO
       END DO

    ENDDO

    deallocate(G2d_in)
    deallocate(itemtypes_out)
    deallocate(itemnames_out)
    deallocate(itemtypes_in)
    deallocate(itemnames_in)
    deallocate(tile_out)
    deallocate(tt_in )
    deallocate(tt )
    if (associated(tile1d)) deallocate(tile1d)

    _RETURN(ESMF_SUCCESS)
  end subroutine RegridTransformT2G2G

END SUBMODULE
