
#include "MAPL_Exceptions.h"
#define DEALOC_(A) if(associated(A))then;if(MAPL_ShmInitialized)then;call MAPL_SyncSharedMemory(rc=STATUS);call MAPL_DeAllocNodeArray(A,rc=STATUS);else;deallocate(A,stat=STATUS);endif;_VERIFY(STATUS);NULLIFY(A);endif

!BOP

! !MODULE: NCIOMod -- A Module to do NetCDF I/O


! !INTERFACE:

module NCIOMod

  use FileIOSharedMod, only: ArrDescr, ArrDescrSet, WRITE_PARALLEL, MAPL_TileMaskGet
  use FileIOSharedMod, only: ArrayScatterShm
  use ESMF
  use MAPL_BaseMod
  use MAPL_CommsMod
  use MAPL_SortMod
  !use MAPL_RangeMod
  use MAPL_ShmemMod
  use MAPL_ExceptionHandling
  use netcdf
  use pFIO
  use BinIOMod, only: GETFILE, READ_PARALLEL, FREE_FILE
  use MAPL_Constants
  !use pFIO_ClientManagerMod
  use gFTL_StringIntegerMap
  use gFTL_StringVector
  use, intrinsic :: ISO_C_BINDING
  use, intrinsic :: iso_fortran_env
  use mpi
  implicit none
  private

  public MAPL_IOChangeRes
  public MAPL_IOCountNonDimVars
  public MAPL_IOGetNonDimVars
  public MAPL_IOCountLevels
  public MAPL_IOGetTime
  public MAPL_NCIOParseTimeUnits
  public MAPL_VarRead
  public MAPL_VarWrite
  public get_fname_by_rank
  public MAPL_NCIOGetFileType
  public MAPL_VarReadNCPar
  public MAPL_VarWriteNCPar
  public MAPL_ReadTilingNC4
  public MAPL_WriteTilingNC4
  public MAPL_ReadTilingASCII

  interface MAPL_VarReadNCPar
     module procedure MAPL_StateVarReadNCPar
     module procedure MAPL_BundleReadNCPar
     module procedure MAPL_ArrayReadNCpar_1d
     module procedure MAPL_ArrayReadNCpar_2d
     module procedure MAPL_ArrayReadNCpar_3d
  end interface

  interface MAPL_VarWriteNCPar
     module procedure MAPL_StateVarWriteNCPar
     module procedure MAPL_BundleWriteNCPar
  end interface

  interface MAPL_VarRead
     module procedure MAPL_VarReadNCpar_R4_1d
     module procedure MAPL_VarReadNCpar_R4_2d
     module procedure MAPL_VarReadNCpar_R4_3d
     module procedure MAPL_VarReadNCpar_R8_1d
     module procedure MAPL_VarReadNCpar_R8_2d
     module procedure MAPL_VarReadNCpar_R8_3d
  end interface


  interface MAPL_VarWrite
     module procedure MAPL_VarWriteNCpar_R4_1d
     module procedure MAPL_VarWriteNCpar_R4_2d
     module procedure MAPL_VarWriteNCpar_R4_3d
     module procedure MAPL_VarWriteNCpar_R4_4d
     module procedure MAPL_VarWriteNCpar_R8_1d
     module procedure MAPL_VarWriteNCpar_R8_2d
     module procedure MAPL_VarWriteNCpar_R8_3d
     module procedure MAPL_VarWriteNCpar_R8_4d
  end interface

  integer, parameter, public :: NumGlobalVars=4
  integer, parameter, public :: NumLocalVars =4
  real(REAL64), parameter :: UNDEF_REAL64 = 1.0D15
contains


  subroutine MAPL_FieldReadNCPar(formatter,name,FIELD, ARRDES, HomePE, RC)
    type(Netcdf4_Fileformatter) , intent(IN   ) :: formatter
    character(len=*)            , intent(IN   ) :: name
    type (ESMF_Field)           , intent(INOUT) :: field
    type(ArrDescr),     optional, intent(INOUT) :: ARRDES
    integer, target,   optional , intent(IN   ) :: HomePE(:)
    integer,           optional , intent(  OUT) :: RC

! Local vars
    type (ESMF_Array)                  :: array
    type (ESMF_DELayout)               :: layout
    type (ESMF_Grid)                   :: GRID
    integer                            :: rank
    integer                            :: status
    real(KIND=ESMF_KIND_R4), pointer, dimension(:)        :: var_1d
    real(KIND=ESMF_KIND_R4), pointer, dimension(:,:)      :: var_2d
    real(KIND=ESMF_KIND_R4), pointer, dimension(:,:,:)    :: var_3d
    real(KIND=ESMF_KIND_R4), pointer, dimension(:,:,:,:)  :: var_4d

    real(KIND=ESMF_KIND_R8), pointer, dimension(:)        :: vr8_1d
    real(KIND=ESMF_KIND_R8), pointer, dimension(:,:)      :: vr8_2d
    real(KIND=ESMF_KIND_R8), pointer, dimension(:,:,:)    :: vr8_3d
    type(ESMF_TypeKind_Flag)           :: tk
    integer                            :: dims
    integer                            :: J, K, L
    integer, pointer                   :: mask(:)
    type (ESMF_DistGrid)               :: distGrid

    call ESMF_FieldGet(field, grid=grid, rc=status)
    _VERIFY(STATUS)
    call ESMF_GridGet(grid, distGrid=distGrid, rc=STATUS)
    _VERIFY(STATUS)
    call ESMF_DistGridGet(distGrid, delayout=layout, rc=STATUS)
    _VERIFY(STATUS)

    call ESMF_AttributeGet(field, name='DIMS', value=DIMS, rc=status)
    _VERIFY(STATUS)
    if (DIMS == MAPL_DimsTileOnly .or. DIMS == MAPL_DimsTileTile) then
       if(present(HomePE)) then
          mask => HomePE
       else
          call MAPL_TileMaskGet(grid, mask, rc=status)
          _VERIFY(STATUS)
       endif
    end if

    call ESMF_FieldGet(field, Array=array, rc=status)
    _VERIFY(STATUS)
    call ESMF_ArrayGet(array, typekind=tk, rank=rank, rc=status)
    _VERIFY(STATUS)

    if (rank == 1) then
       if (tk == ESMF_TYPEKIND_R4) then
          call ESMF_ArrayGet(array, localDE=0, farrayptr=var_1d, rc=status)
          _VERIFY(STATUS)
          if (associated(var_1d)) then
             if (DIMS == MAPL_DimsTileOnly .or. DIMS == MAPL_DimsTileTile) then
                call MAPL_VarRead(formatter, name, var_1d, layout=layout, arrdes=arrdes, mask=mask, rc=status)
                _VERIFY(STATUS)
             else if (DIMS == MAPL_DimsVertOnly .or. DIMS==MAPL_DimsNone) then
                call MAPL_VarRead(formatter, name, var_1d, layout=layout, arrdes=arrdes, rc=status)
                _VERIFY(STATUS)
             else
                _RETURN(ESMF_FAILURE)
             endif
          end if
       else
          call ESMF_ArrayGet(array, localDE=0, farrayptr=vr8_1d, rc=status)
          _VERIFY(STATUS)
          if (associated(vr8_1d)) then
             if (DIMS == MAPL_DimsTileOnly .or. DIMS == MAPL_DimsTileTile) then
                call MAPL_VarRead(formatter, name, vr8_1d, layout=layout, arrdes=arrdes, mask=mask, rc=status)
                _VERIFY(STATUS)
             else if (DIMS == MAPL_DimsVertOnly .or. DIMS==MAPL_DimsNone) then
                call MAPL_VarRead(formatter, name, vr8_1d, layout=layout, arrdes=arrdes, rc=status)
                _VERIFY(STATUS)
             else
                _RETURN(ESMF_FAILURE)
             endif
          end if
       end if
    else if (rank == 2) then
       if (tk == ESMF_TYPEKIND_R4) then
          call ESMF_ArrayGet(array, localDE=0, farrayptr=var_2d, rc=status)
          _VERIFY(STATUS)
          if (associated(var_2d)) then !ALT: temp kludge
             if (DIMS == MAPL_DimsTileOnly) then
                do J = 1,size(var_2d,2)
                   call MAPL_VarRead(formatter, name, var_2d(:,J), layout=layout, arrdes=arrdes, mask=mask, offset1=j, rc=status)
                   _VERIFY(STATUS)
                end do
             else if (DIMS == MAPL_DimsTileTile) then
                do j=1,size(var_2d,2)
                   call MAPL_VarRead(formatter, name, var_2d(:,J), layout=layout, arrdes=arrdes, mask=mask, offset1=j, rc=status)
                   _VERIFY(STATUS)
                enddo
             else
                call MAPL_VarRead(formatter, name, var_2d, arrdes=arrdes, rc=status)
                _VERIFY(STATUS)
             end if
          end if
       else
          call ESMF_ArrayGet(array, localDE=0, farrayptr=vr8_2d, rc=status)
          _VERIFY(STATUS)
          if (associated(vr8_2d)) then !ALT: temp kludge
             if (DIMS == MAPL_DimsTileOnly) then
                do J = 1,size(vr8_2d,2)
                   call MAPL_VarRead(formatter, name, vr8_2d(:,J), layout=layout, arrdes=arrdes, mask=mask, offset1=j, rc=status)
                end do
             else if (DIMS == MAPL_DimsTileTile) then
                do j=1,size(vr8_2d,2)
                   call MAPL_VarRead(formatter, name, vr8_2d(:,J), layout=layout, arrdes=arrdes, mask=mask, offset1=j, rc=status)
                   _VERIFY(STATUS)
                enddo
             else
                call MAPL_VarRead(formatter, name, vr8_2d, arrdes=arrdes, rc=status)
                _VERIFY(STATUS)
             end if
          end if
       endif
    else if (rank == 3) then
       if (tk == ESMF_TYPEKIND_R4) then
          call ESMF_ArrayGet(array, localDE=0, farrayptr=var_3d, rc=status)
          _VERIFY(STATUS)
          if (associated(var_3d)) then !ALT: temp kludge
             if (DIMS == MAPL_DimsTileOnly) then
                do J = 1,size(var_3d,2)
                   do K = 1,size(var_3d,3)
                      call MAPL_VarRead(formatter, name, var_3d(:,J,K), layout=layout, arrdes=arrdes, mask=mask, offset1=j, &
                           & offset2=k, rc=status)
                   end do
                end do
             else
                call MAPL_VarRead(formatter, name, var_3d, arrdes=arrdes, rc=status)
             end if
          end if
       else
          call ESMF_ArrayGet(array, localDE=0, farrayptr=vr8_3d, rc=status)
          _VERIFY(STATUS)
          if (associated(vr8_3d)) then !ALT: temp kludge
             if (DIMS == MAPL_DimsTileOnly) then
                do J = 1,size(vr8_3d,2)
                   do K = 1,size(vr8_3d,3)
                      call MAPL_VarRead(formatter, name, vr8_3d(:,J,K), layout=layout, arrdes=arrdes, mask=mask, &
                           & offset1=j, offset2=k, rc=status)
                   end do
                end do
             else
                call MAPL_VarRead(formatter, name, vr8_3d, arrdes=arrdes, rc=status)
             end if
          end if
       endif

    else if (rank == 4) then
       if (tk == ESMF_TYPEKIND_R4) then
          call ESMF_ArrayGet(array, localDE=0, farrayptr=var_4d, rc=status)
          _VERIFY(STATUS)
          if (.not.associated(var_4d)) then
             _FAIL( "Cannot read unassociated variable")
          end if

          do L = 1,size(var_4d,3)
             do K = 1,size(var_4d,4)
                call MAPL_VarRead(formatter, name, var_4d(:,:,L,K), &
                     arrdes=arrdes, lev=l, &
                     & offset2=k, rc=status)
                _VERIFY(status)
             end do
          end do
       else
          _FAIL( "ERROR: unsupported RANK/KIND")
       endif
    else
       _FAIL( "ERROR: unsupported RANK")
    endif
    _VERIFY(STATUS)

    if (DIMS == MAPL_DimsTileOnly .or. DIMS == MAPL_DimsTileTile) then
       if(.not.present(HomePE)) then
          DEALOC_(mask)
       end if
    end if

    _RETURN(ESMF_SUCCESS)
  end subroutine MAPL_FieldReadNCPar


!---------------------------
! Write routines
!---------------------------

  subroutine MAPL_FieldWriteNCPar(formatter, name, FIELD, ARRDES, HomePE, oClients, RC)
    type(Netcdf4_fileformatter) , intent(IN   ) :: formatter
    character(len=*)            , intent(IN   ) :: name
    type (ESMF_Field)           , intent(INOUT) :: field  !ALT: intent(in)
    type(ArrDescr)              , intent(INOUT) :: ARRDES
    integer, target,   optional , intent(IN   ) :: HomePE(:)
    type (ClientManager), optional, intent(inout)  :: oClients
    integer,           optional , intent(  OUT) :: RC

! Local vars
    type (ESMF_Array)                  :: array
    type (ESMF_DELayout)               :: layout
    type (ESMF_Grid)                   :: GRID
    integer                            :: rank
    integer                            :: status
    integer                            :: DIMS
    real(KIND=ESMF_KIND_R4), pointer, dimension(:)        :: var_1d
    real(KIND=ESMF_KIND_R4), pointer, dimension(:,:)      :: var_2d
    real(KIND=ESMF_KIND_R4), pointer, dimension(:,:,:)    :: var_3d
    real(KIND=ESMF_KIND_R4), pointer, dimension(:,:,:,:)  :: var_4d

    real(KIND=ESMF_KIND_R4), pointer, dimension(:)        :: gvar_1d
    real(KIND=ESMF_KIND_R4), pointer, dimension(:,:)      :: gvar_2d
    real(KIND=ESMF_KIND_R4), pointer, dimension(:,:,:)    :: gvar_3d

    real(KIND=ESMF_KIND_R8), pointer, dimension(:)        :: vr8_1d
    real(KIND=ESMF_KIND_R8), pointer, dimension(:,:)      :: vr8_2d
    real(KIND=ESMF_KIND_R8), pointer, dimension(:,:,:)    :: vr8_3d
    real(KIND=ESMF_KIND_R8), pointer, dimension(:,:,:,:)  :: vr8_4d

    real(KIND=ESMF_KIND_R8), pointer, dimension(:)        :: gvr8_1d
    real(KIND=ESMF_KIND_R8), pointer, dimension(:,:)      :: gvr8_2d
    real(KIND=ESMF_KIND_R8), pointer, dimension(:,:,:)    :: gvr8_3d

    type(ESMF_TypeKind_Flag)           :: tk
    integer, pointer                   :: mask(:)
    integer                            :: J,K
    type (ESMF_DistGrid)               :: distGrid
    type (LocalMemReference) :: lMemRef
    type (LocalMemReference), allocatable :: lMemRef_vec(:)
    integer :: size_1d
    logical :: have_oclients
    character(len=:), allocatable :: fname_by_writer

    call ESMF_FieldGet(field, grid=grid, rc=status)
    _VERIFY(STATUS)
    call ESMF_GridGet(grid, distGrid=distGrid, rc=STATUS)
    _VERIFY(STATUS)
    call ESMF_DistGridGet(distGrid, delayout=layout, rc=STATUS)
    _VERIFY(STATUS)

    have_oclients = present(oClients)


    call ESMF_AttributeGet(field, name='DIMS', value=DIMS, rc=status)
    _VERIFY(STATUS)
    if (DIMS == MAPL_DimsTileOnly .or. DIMS == MAPL_DimsTileTile) then
       if(present(HomePE)) then
          mask => HomePE
       else
          call MAPL_TileMaskGet(grid, mask, rc=status)
          _VERIFY(STATUS)
       endif
    end if

    call ESMF_FieldGet(field, Array=array, rc=status)
    _VERIFY(STATUS)
    call ESMF_ArrayGet(array, typekind=tk, rank=rank, rc=status)
    _VERIFY(STATUS)
    call ESMF_AttributeGet(field, name='DIMS', value=DIMS, rc=status)
    if (rank == 1) then
       if (tk == ESMF_TYPEKIND_R4) then
          call ESMF_ArrayGet(array, localDE=0, farrayptr=var_1d, rc=status)
          _VERIFY(STATUS)
          if (associated(var_1d)) then

             if (DIMS == MAPL_DimsTileOnly .or. DIMS == MAPL_DimsTileTile) then
                size_1d = arrdes%im_world
             else if (DIMS == MAPL_DimsVertOnly .or. DIMS==MAPL_DimsNone) then
                size_1d = size(var_1d,1)
             endif

             if (have_oclients) then
                if( MAPL_AM_I_ROOT())  then
                   lMemRef = LocalMemReference(pFIO_REAL32,[size_1d])
                   call c_f_pointer(lMemRef%base_address, gvar_1d, shape=[size_1d])
                   if (DIMS == MAPL_DimsVertOnly .or. DIMS==MAPL_DimsNone) gvar_1d = var_1d
                else
                   lMemRef = LocalMemReference(pFIO_REAL32,[0])
                   call c_f_pointer(lMemRef%base_address, gvar_1d, shape=[0])
                endif
                if (DIMS == MAPL_DimsTileOnly .or. DIMS == MAPL_DimsTileTile) then
                   call ArrayGather(var_1d, gvar_1d, grid, mask=mask, rc=status)
                endif
                if (dims == MAPL_DimsVertOnly .and. arrdes%split_checkpoint) then
                   allocate(lMemRef_vec(arrdes%num_writers))
                   do j=1,arrdes%num_writers
                      fname_by_writer = get_fname_by_rank(trim(arrdes%filename),j-1)
                      if (mapl_am_i_root()) then
                         lMemRef_vec(j) = LocalMemReference(pFIO_REAL32,[size_1d])
                         call c_f_pointer(lMemRef_vec(j)%base_address, gvar_1d, shape=[size_1d])
                         gvar_1d = var_1d
                      else
                         lMemRef_vec(j) = LocalMemReference(pFIO_REAL32,[0])
                         call c_f_pointer(lMemRef_vec(j)%base_address, gvar_1d, shape=[0])
                      end if
                      call oClients%collective_stage_data(arrdes%collection_id(j), trim(fname_by_writer), name, lMemRef_vec(j), start=[1], &
                                   global_start=[1], global_count=[size_1d])
                   enddo
                else
                   call oClients%collective_stage_data(arrdes%collection_id(1), trim(arrdes%filename), name, lMemRef, start=[1], &
                                global_start=[1], global_count=[size_1d])
                end if
             else

                if (DIMS == MAPL_DimsTileOnly .or. DIMS == MAPL_DimsTileTile) then
                   call MAPL_VarWrite(formatter, name, var_1d, layout=layout, arrdes=arrdes, mask=mask, rc=status)
                else if (DIMS == MAPL_DimsVertOnly .or. DIMS==MAPL_DimsNone) then
                   call MAPL_VarWrite(formatter, name, var_1d, layout=layout, arrdes=arrdes, rc=status)
                else
                   _RETURN(ESMF_FAILURE)
                end if

             endif
          else
             _FAIL( "Cannot write unassociated var-1d")
          end if
       else
          call ESMF_ArrayGet(array, localDE=0, farrayptr=vr8_1d, rc=status)
          _VERIFY(STATUS)
          if (associated(vr8_1d)) then

             if (DIMS == MAPL_DimsTileOnly .or. DIMS == MAPL_DimsTileTile) then
                size_1d = arrdes%im_world
             else if (DIMS == MAPL_DimsVertOnly .or. DIMS==MAPL_DimsNone) then
                size_1d = size(vr8_1d,1)
             endif

             if (have_oclients) then
                if(MAPL_AM_I_ROOT()) then
                   lMemRef = LocalMemReference(pFIO_REAL64,[size_1d])
                   call c_f_pointer(lMemRef%base_address, gvr8_1d, shape=[size_1d])
                   if (DIMS == MAPL_DimsVertOnly .or. DIMS==MAPL_DimsNone) gvr8_1d = vr8_1d
                else
                   lMemRef = LocalMemReference(pFIO_REAL64,[0])
                   call c_f_pointer(lMemRef%base_address, gvr8_1d, shape=[0])
                endif

                if (DIMS == MAPL_DimsTileOnly .or. DIMS == MAPL_DimsTileTile) then
                   call ArrayGather(vr8_1d, gvr8_1d, grid, mask=mask, rc=status)
                endif
                if (dims == MAPL_DimsVertOnly .and. arrdes%split_checkpoint) then
                   allocate(lMemRef_vec(arrdes%num_writers))
                   do j=1,arrdes%num_writers
                      fname_by_writer = get_fname_by_rank(trim(arrdes%filename),j-1)
                      if (mapl_am_i_root()) then
                         lMemRef_vec(j) = LocalMemReference(pFIO_REAL64,[size_1d])
                         call c_f_pointer(lMemRef_vec(j)%base_address, gvr8_1d, shape=[size_1d])
                         gvr8_1d = vr8_1d
                      else
                         lMemRef_vec(j) = LocalMemReference(pFIO_REAL64,[0])
                         call c_f_pointer(lMemRef_vec(j)%base_address, gvr8_1d, shape=[0])
                      end if
                      call oClients%collective_stage_data(arrdes%collection_id(j), trim(fname_by_writer), name, lMemRef_vec(j), start=[1], &
                                   global_start=[1], global_count=[size_1d])
                   enddo
                else
                   call oClients%collective_stage_data(arrdes%collection_id(1), trim(arrdes%filename), name, lMemRef, start=[1], &
                                global_start=[1], global_count=[size_1d])
                end if

             else

                if (DIMS == MAPL_DimsTileOnly .or. DIMS == MAPL_DimsTileTile) then
                   call MAPL_VarWrite(formatter, name, vr8_1d, layout=layout, arrdes=arrdes, mask=mask, rc=status)
                else if (DIMS == MAPL_DimsVertOnly .or. DIMS==MAPL_DimsNone) then
                   call MAPL_VarWrite(formatter, name, vr8_1d, layout=layout, arrdes=arrdes, rc=status)
                else
                   _RETURN(ESMF_FAILURE)
                end if

             endif
          else
             _FAIL( "Cannot write unassociated var8-1d")
          end if
       endif
    else if (rank == 2) then
       if (tk == ESMF_TYPEKIND_R4) then
          call ESMF_ArrayGet(array, localDE=0, farrayptr=var_2d, rc=status)
          _VERIFY(STATUS)
          if (associated(var_2d)) then !ALT: temp kludge
             if (DIMS == MAPL_DimsTileOnly .or. DIMS == MAPL_DimsTileTile) then

                if (have_oclients) then
                   if(MAPL_AM_I_ROOT()) then
                      lMemRef = LocalMemReference(pFIO_REAL32,[arrdes%im_world, size(var_2d,2)])
                      call c_f_pointer(lMemRef%base_address, gvar_2d, shape=[arrdes%im_world, size(var_2d,2)])
                   else
                      lMemRef = LocalMemReference(pFIO_REAL32,[0,size(var_2d,2)])
                      call c_f_pointer(lMemRef%base_address, gvar_2d, shape=[0, size(var_2d,2)])
                   endif
                   do J = 1,size(var_2d,2)
                      call ArrayGather(var_2d(:,J), gvar_2d(:,J), grid, mask=mask, rc=status)
                   enddo
                   call oClients%collective_stage_data(arrdes%collection_id(1), trim(arrdes%filename), name, lMemRef, start=[1,1], &
                                global_start=[1,1], global_count=[arrdes%im_world,size(var_2d,2)])

                else

                   do J = 1,size(var_2d,2)
                      call MAPL_VarWrite(formatter, name, var_2d(:,J), layout=layout, arrdes=arrdes, mask=mask, offset1=j, rc=status)
                   end do

                endif

             else
               call MAPL_VarWrite(formatter, name, var_2d, arrdes=arrdes, oClients=oClients, rc=status)
             endif ! dims
          else
             _FAIL( "Cannot write unassociated var-2d")
          endif ! associated
       else
          call ESMF_ArrayGet(array, localDE=0, farrayptr=vr8_2d, rc=status)
          _VERIFY(STATUS)
          if (associated(vr8_2d)) then !ALT: temp kludge
             if (DIMS == MAPL_DimsTileOnly .or. DIMS == MAPL_DimsTileTile) then

                if (have_oclients) then
                   if( MAPL_AM_I_ROOT() ) then
                      lMemRef = LocalMemReference(pFIO_REAL64,[arrdes%im_world,size(vr8_2d,2)])
                      call c_f_pointer(lMemRef%base_address, gvr8_2d, shape=[arrdes%im_world,size(vr8_2d,2)])
                   else
                      lMemRef = LocalMemReference(pFIO_REAL64,[0,size(vr8_2d,2)])
                      call c_f_pointer(lMemRef%base_address, gvr8_2d, shape=[0,size(vr8_2d,2)])
                   endif
                   do J = 1,size(vr8_2d,2)
                      call ArrayGather(vr8_2d(:,J), gvr8_2d(:,J), grid, mask=mask, rc=status)
                   enddo
                   call oClients%collective_stage_data(arrdes%collection_id(1), trim(arrdes%filename), name, lMemRef, start=[1,1], &
                                 global_start=[1,1], global_count=[arrdes%im_world,size(vr8_2d,2)])
                else

                   do J = 1,size(vr8_2d,2)
                      call MAPL_VarWrite(formatter, name, vr8_2d(:,J), layout=layout, arrdes=arrdes, mask=mask, offset1=j, rc=status)
                   end do

                endif

             else
                call MAPL_VarWrite(formatter, name, vr8_2d, arrdes=arrdes, oClients=oClients, rc=status)
             end if
          else
             _FAIL( "Cannot write unassociated var8-2d")
          end if
       endif
    else if (rank == 3) then
       if (tk == ESMF_TYPEKIND_R4) then
          call ESMF_ArrayGet(array, localDE=0, farrayptr=var_3d, rc=status)
          _VERIFY(STATUS)
          if (associated(var_3d)) then !ALT: temp kludge
             if (DIMS == MAPL_DimsTileOnly) then

                if (have_oclients) then
                   if( MAPL_Am_I_Root() ) then
                      lMemRef = LocalMemReference(pFIO_REAL32,[arrdes%im_world, size(var_3d,2), size(var_3d,3)])
                      call c_f_pointer(lMemRef%base_address, gvar_3d, shape=[arrdes%im_world, size(var_3d,2), size(var_3d,3)])
                   else
                      lMemRef = LocalMemReference(pFIO_REAL32,[0,size(var_3d,2), size(var_3d,3)])
                      call c_f_pointer(lMemRef%base_address, gvar_3d, shape=[0, size(var_3d,2), size(var_3d,3)])
                   endif
                   do K = 1, size(var_3d,3)
                      do J = 1,size(var_3d,2)
                         call ArrayGather(var_3d(:,J,K), gvar_3d(:,J,K), grid, mask=mask, rc=status)
                      enddo
                   enddo

                   call oClients%collective_stage_data(arrdes%collection_id(1), trim(arrdes%filename), name, lMemRef, start=[1,1,1], &
                                 global_start=[1,1,1], global_count=[arrdes%im_world,size(var_3d,2),size(var_3d,3)])
                else

                   do J = 1,size(var_3d,2)
                      do K = 1,size(var_3d,3)
                         call MAPL_VarWrite(formatter, name, var_3d(:,J,K), layout=layout, arrdes=arrdes, mask=mask, &
                           & offset1=j, offset2=k, rc=status)
                      end do
                   end do

                endif

             else
                call MAPL_VarWrite(formatter, name, var_3d, arrdes=arrdes, oClients=oClients, rc=status)
             endif
          else
             _FAIL( "Cannot write unassociated var-3d")
          end if
       else
          call ESMF_ArrayGet(array, localDE=0, farrayptr=vr8_3d, rc=status)
          _VERIFY(STATUS)
          if (associated(vr8_3d)) then !ALT: temp kludge
             if (DIMS == MAPL_DimsTileOnly) then

                if (have_oclients) then
                   if( MAPL_Am_I_Root() ) then
                      lMemRef = LocalMemReference(pFIO_REAL64,[arrdes%im_world,size(vr8_3d,2), size(vr8_3d,3)])
                      call c_f_pointer(lMemRef%base_address, gvr8_3d, shape=[arrdes%im_world,size(vr8_3d,2), size(vr8_3d,3)])
                   else
                      lMemRef = LocalMemReference(pFIO_REAL64,[0,size(vr8_3d,2), size(vr8_3d,3)])
                      call c_f_pointer(lMemRef%base_address, gvr8_3d, shape=[0,size(vr8_3d,2), size(vr8_3d,3)])
                   endif
                   do K = 1, size(vr8_3d,3)
                      do J = 1, size(vr8_3d,2)
                         call ArrayGather(vr8_3d(:,J,K), gvr8_3d(:,J,K), grid, mask=mask, rc=status)
                      enddo
                   enddo
                   call oClients%collective_stage_data(arrdes%collection_id(1), trim(arrdes%filename), name, lMemRef, start=[1,1,1], &
                                 global_start=[1,1,1], global_count=[arrdes%im_world, size(vr8_3d,2), size(vr8_3d,3)])
                else

                   do J = 1,size(vr8_3d,2)
                      do K = 1,size(vr8_3d,3)
                         call MAPL_VarWrite(formatter, name, vr8_3d(:,J,K), layout=layout, arrdes=arrdes, mask=mask, &
                           & offset1=j, offset2=k, rc=status)
                      end do
                   end do

                endif

             else
                call MAPL_VarWrite(formatter, name, vr8_3d, arrdes=arrdes, oClients=oClients, rc=status)
             end if
          else
             _FAIL( "Cannot write unassociated var8-3d")
          end if
       endif
    else if (rank == 4) then
       if (DIMS == MAPL_DimsTileOnly .or. DIMS == MAPL_DimsTileTile) then
          _FAIL( "Unsupported tile/ungrid variable")
       end if
       if (tk == ESMF_TYPEKIND_R4) then
          call ESMF_ArrayGet(array, localDE=0, farrayptr=var_4d, rc=status)
          _VERIFY(STATUS)
          if (.not.associated(var_4d)) then
             _FAIL( "Cannot write unassociated vars")
          end if
          call MAPL_VarWrite(formatter, name, var_4d, arrdes=arrdes, oClients=oClients, rc=status)
       else
          call ESMF_ArrayGet(array, localDE=0, farrayptr=vr8_4d, rc=status)
          _VERIFY(STATUS)
          if (.not.associated(vr8_4d)) then
             _FAIL( "Cannot write unassociated vars")
          end if
          call MAPL_VarWrite(formatter, name, vr8_4d, arrdes=arrdes, oClients=oClients, rc=status)
       endif
    else
       print *, "ERROR: unsupported RANK"
       _RETURN(ESMF_FAILURE)
    endif
    _VERIFY(STATUS)

    if (DIMS == MAPL_DimsTileOnly .or. DIMS == MAPL_DimsTileTile) then
       if(.not.present(HomePE)) then
          DEALOC_(mask)
       end if
    end if

    _RETURN(ESMF_SUCCESS)
  end subroutine MAPL_FieldWriteNCPar

!---------------------------
  subroutine MAPL_VarWriteNCpar_R4_4d(formatter, name, A, ARRDES, oClients, RC)

    type(Netcdf4_Fileformatter) , intent(IN   ) :: formatter
    character(len=*)            , intent(IN   ) :: name
    real(kind=ESMF_KIND_R4)     , intent(IN   ) :: A(:,:,:,:)
    type(ArrDescr), optional    , intent(INOUT) :: ARRDES
    type (ClientManager), optional, intent(inout)  :: oClients
    integer,           optional , intent(  OUT) :: RC

    integer                               :: status
    integer :: K, L
    integer ::  i1, j1, in, jn,  global_dim(3), dim3, dim4,i
    type(ArrayReference)     :: ref
    integer :: start_bound,end_bound,counts_per_writer
    logical :: in_bounds
    real(kind=ESMF_KIND_R4), pointer :: a_temp(:,:,:,:)
    character(len=:), allocatable :: writer_filename

    if (present(arrdes)) then
       if (present(oClients)) then
          if (arrdes%split_checkpoint) then
             call MAPL_GridGet(arrdes%grid,globalCellCountPerDim=global_dim,rc=status)
              _VERIFY(status)
             call MAPL_Grid_interior(arrdes%grid,i1,in,j1,jn)
             _ASSERT( i1 == arrdes%I1(arrdes%NX0), "interior starting i1 not match")
             _ASSERT( j1 == arrdes%j1(arrdes%NY0), "interior starting j1 not match")
             _ASSERT( size(a,1) == in-i1+1, "size not match")
             _ASSERT( size(a,2) == jn-j1+1, "size not match")
             counts_per_writer = global_dim(2)/arrdes%num_writers
             allocate(a_temp(0,0,0,0))
             do i=1,arrdes%num_writers
                start_bound = (i-1)*counts_per_writer+1
                end_bound   = i*counts_per_writer
                in_bounds = (j1 .ge. start_bound) .and. (jn .le. end_bound)
                dim3 = size(a,3)
                dim4 = size(a,4)
                if (in_bounds) then
                   ref = ArrayReference(A)
                else
                   ref = ArrayReference(a_temp)
                end if
                writer_filename = get_fname_by_rank(trim(arrdes%filename),i-1)
                call oClients%collective_stage_data(arrdes%collection_id(i),trim(writer_filename),trim(name), &
                            ref,start=[i1,j1-(i-1)*counts_per_writer,1,1], &
                            global_start=[1,1,1,1], global_count=[global_dim(1),global_dim(2)/arrdes%num_writers,dim3,dim4])
             enddo
             _RETURN(_SUCCESS)
          else
             call MAPL_GridGet(arrdes%grid,globalCellCountPerDim=global_dim,rc=status)
              _VERIFY(status)
             call MAPL_Grid_interior(arrdes%grid,i1,in,j1,jn)
             _ASSERT( i1 == arrdes%I1(arrdes%NX0), "interior starting i1 not match")
             _ASSERT( j1 == arrdes%j1(arrdes%NY0), "interior starting j1 not match")

             ref = ArrayReference(A)
             _ASSERT( size(a,1) == in-i1+1, "size not match")
             _ASSERT( size(a,2) == jn-j1+1, "size not match")
             call oClients%collective_stage_data(arrdes%collection_id(1),trim(arrdes%filename),trim(name), &
                         ref,start=[i1,j1,1,1], &
                         global_start=[1,1,1,1], global_count=[global_dim(1),global_dim(2),size(a,3),size(a,4)])
             _RETURN(_SUCCESS)
          end if
       else
          do K = 1,size(A,4)
             do L = 1,size(A,3)
                call MAPL_VarWrite(formatter, name, A(:,:,L,K), arrdes=arrdes, &
                     & oClients=oClients, lev=l, offset2=k, rc=status)
                _VERIFY(status)
             end do
          end do
       end if
    else
       do K = 1,size(A,4)
          do L = 1,size(A,3)
             call MAPL_VarWrite(formatter, name, A(:,:,L,K), &
                  & oClients=oClients, lev=l, offset2=k, rc=status)
             _VERIFY(status)
          end do
       enddo
    endif

    _RETURN(ESMF_SUCCESS)

  end subroutine MAPL_VarWriteNCpar_R4_4d
!---------------------------
  subroutine MAPL_VarWriteNCpar_R8_4d(formatter, name, A, ARRDES, oClients, RC)

    type(Netcdf4_Fileformatter) , intent(IN   ) :: formatter
    character(len=*)            , intent(IN   ) :: name
    real(kind=ESMF_KIND_R8)     , intent(IN   ) :: A(:,:,:,:)
    type(ArrDescr)              , intent(INOUT) :: ARRDES
    type (ClientManager), optional, intent(inout)  :: oClients
    integer,           optional , intent(  OUT) :: RC

    integer                               :: status

    integer :: K, L
    integer ::  i1, j1, in, jn,  global_dim(3), dim3, dim4, i
    type(ArrayReference)     :: ref
    integer :: start_bound,end_bound,counts_per_writer
    logical :: in_bounds
    real(kind=ESMF_KIND_R8), pointer :: a_temp(:,:,:,:)
    character(len=:), allocatable :: writer_filename

    if (present(oClients)) then

       if (arrdes%split_checkpoint) then
          call MAPL_GridGet(arrdes%grid,globalCellCountPerDim=global_dim,rc=status)
           _VERIFY(status)
          call MAPL_Grid_interior(arrdes%grid,i1,in,j1,jn)
          _ASSERT( i1 == arrdes%I1(arrdes%NX0), "interior starting i1 not match")
          _ASSERT( j1 == arrdes%j1(arrdes%NY0), "interior starting j1 not match")
          _ASSERT( size(a,1) == in-i1+1, "size not match")
          _ASSERT( size(a,2) == jn-j1+1, "size not match")
          counts_per_writer = global_dim(2)/arrdes%num_writers
          allocate(a_temp(0,0,0,0))
          do i=1,arrdes%num_writers
             start_bound = (i-1)*counts_per_writer+1
             end_bound   = i*counts_per_writer
             in_bounds = (j1 .ge. start_bound) .and. (jn .le. end_bound)
             dim3 = size(a,3)
             dim4 = size(a,4)
             if (in_bounds) then
                ref = ArrayReference(A)
             else
                ref = ArrayReference(a_temp)
             end if
             writer_filename = get_fname_by_rank(trim(arrdes%filename),i-1)
             call oClients%collective_stage_data(arrdes%collection_id(i),trim(writer_filename),trim(name), &
                         ref,start=[i1,j1-(i-1)*counts_per_writer,1,1], &
                         global_start=[1,1,1,1], global_count=[global_dim(1),global_dim(2)/arrdes%num_writers,dim3,dim4])
          enddo
          _RETURN(_SUCCESS)
       else
          call MAPL_GridGet(arrdes%grid,globalCellCountPerDim=global_dim,rc=status)
           _VERIFY(status)
          call MAPL_Grid_interior(arrdes%grid,i1,in,j1,jn)
          _ASSERT( i1 == arrdes%I1(arrdes%NX0), "interior starting i1 not match")
          _ASSERT( j1 == arrdes%j1(arrdes%NY0), "interior starting j1 not match")

          ref = ArrayReference(A)
          _ASSERT( size(a,1) == in-i1+1, "size not match")
          _ASSERT( size(a,2) == jn-j1+1, "size not match")
          call oClients%collective_stage_data(arrdes%collection_id(1),trim(arrdes%filename),trim(name), &
                      ref,start=[i1,j1,1,1], &
                      global_start=[1,1,1,1], global_count=[global_dim(1),global_dim(2),size(a,3),size(a,4)])
          _RETURN(_SUCCESS)
      end if
    else
       do K = 1,size(A,4)
          do L = 1,size(A,3)
             call MAPL_VarWrite(formatter, name, A(:,:,L,K), arrdes=arrdes, &
                  & oClients=oClients, lev=l, offset2=k, rc=status)
             _VERIFY(status)
          end do
       end do
    end if
    _RETURN(ESMF_SUCCESS)

  end subroutine MAPL_VarWriteNCpar_R8_4d
!---------------------------

  subroutine MAPL_VarWriteNCpar_R4_3d(formatter, name, A, ARRDES, oClients, RC)

    type(Netcdf4_Fileformatter)           , intent(IN   ) :: formatter
    character(len=*)            , intent(IN   ) :: name
    real(kind=ESMF_KIND_R4)     , intent(IN   ) :: A(:,:,:)
    type(ArrDescr), optional    , intent(INOUT) :: ARRDES
    type (ClientManager), optional, intent(inout)  :: oClients
    integer,           optional , intent(  OUT) :: RC

    integer                               :: status
    integer :: l
    integer ::  i1, j1, in, jn,  global_dim(3), dim3, i, j1p
    type(ArrayReference)     :: ref
    integer :: start_bound,end_bound,counts_per_writer
    logical :: in_bounds
    real(kind=ESMF_KIND_R4), pointer :: a_temp(:,:,:)
    character(len=:), allocatable :: writer_filename

    type(ESMF_VM) :: vm
    integer :: mypet
    call ESMF_VMGetCurrent(vm)
    call ESMF_VMGet(vm,localPet=mypet)

    if (present(arrdes)) then
       if (present(oclients)) then
          if (arrdes%split_checkpoint) then
             call MAPL_GridGet(arrdes%grid,globalCellCountPerDim=global_dim,rc=status)
              _VERIFY(status)
             call MAPL_Grid_interior(arrdes%grid,i1,in,j1,jn)

             _ASSERT( i1 == arrdes%I1(arrdes%NX0), "interior starting i1 not match")
             _ASSERT( j1 == arrdes%j1(arrdes%NY0), "interior starting j1 not match")
             _ASSERT( size(a,1) == in-i1+1, "size not match")
             _ASSERT( size(a,2) == jn-j1+1, "size not match")
             counts_per_writer = global_dim(2)/arrdes%num_writers
             allocate(a_temp(0,0,0))
             do i=1,arrdes%num_writers
                start_bound = (i-1)*counts_per_writer+1
                end_bound   = i*counts_per_writer
                in_bounds = (j1 .ge. start_bound) .and. (jn .le. end_bound)
                dim3 = size(a,3)
                if (in_bounds) then
                   ref = ArrayReference(A)
                   j1p = j1-(i-1)*counts_per_writer
                else
                   ref = ArrayReference(a_temp)
                   j1p = 1
                end if
                writer_filename = get_fname_by_rank(trim(arrdes%filename),i-1)
                call oClients%collective_stage_data(arrdes%collection_id(i),trim(writer_filename),trim(name), &
                            ref,start=[i1,j1p,1], &
                            global_start=[1,1,1], global_count=[global_dim(1),global_dim(2)/arrdes%num_writers,dim3])
             enddo
             _RETURN(_SUCCESS)
          else
             call MAPL_GridGet(arrdes%grid,globalCellCountPerDim=global_dim,rc=status)
              _VERIFY(status)
             call MAPL_Grid_interior(arrdes%grid,i1,in,j1,jn)
             _ASSERT( i1 == arrdes%I1(arrdes%NX0), "interior starting i1 not match")
             _ASSERT( j1 == arrdes%j1(arrdes%NY0), "interior starting j1 not match")

             ref = ArrayReference(A)
             _ASSERT( size(a,1) == in-i1+1, "size not match")
             _ASSERT( size(a,2) == jn-j1+1, "size not match")
             call oClients%collective_stage_data(arrdes%collection_id(1),trim(arrdes%filename),trim(name), &
                         ref,start=[i1,j1,1], &
                         global_start=[1,1,1], global_count=[global_dim(1),global_dim(2),size(a,3)])
             _RETURN(_SUCCESS)
          end if

       else
          do l=1,size(a,3)
             call MAPL_VarWrite(formatter,name,A(:,:,l), arrdes=arrdes,lev=l, rc=status)
             _VERIFY(status)
          enddo
       endif
    else
       do l=1,size(a,3)
          call MAPL_VarWrite(formatter,name,A(:,:,l), lev=l, rc=status)
          _VERIFY(status)
       enddo
    endif
    _RETURN(ESMF_SUCCESS)
  end subroutine MAPL_VarWriteNCpar_R4_3d

!---------------------------

  subroutine MAPL_VarReadNCpar_R4_3d(formatter, name, A, ARRDES, RC)

    type (Netcdf4_Fileformatter)          , intent(IN   ) :: formatter
    character(len=*)            , intent(IN   ) :: name
    real(kind=ESMF_KIND_R4)     , intent(INOUT) :: A(:,:,:)
    type(ArrDescr), optional    , intent(INOUT) :: ARRDES
    integer,           optional , intent(  OUT) :: RC

    integer                               :: status
    integer :: l

    do l=1,size(a,3)
       call MAPL_VarRead(formatter,name,A(:,:,l), arrdes=arrdes, lev=l, rc=status)
       _VERIFY(status)
    enddo

    _RETURN(ESMF_SUCCESS)
  end subroutine MAPL_VarReadNCpar_R4_3d

!---------------------------

  subroutine MAPL_VarWriteNCpar_R8_3d(formatter, name, A, ARRDES, oClients, RC)

    type (Netcdf4_Fileformatter), intent(IN   ) :: formatter
    character(len=*)            , intent(IN   ) :: name
    real(kind=ESMF_KIND_R8)     , intent(IN   ) :: A(:,:,:)
    type(ArrDescr)              , intent(INOUT) :: ARRDES
    type (ClientManager), optional, intent(inout)  :: oClients
    integer,           optional , intent(  OUT) :: RC

    integer                               :: status

    integer :: l

    integer ::  i1, j1, in, jn,  global_dim(3), dim3, i
    type(ArrayReference)     :: ref
    integer :: start_bound,end_bound,counts_per_writer
    logical :: in_bounds
    real(kind=ESMF_KIND_R8), pointer :: a_temp(:,:,:)
    character(len=:), allocatable :: writer_filename
    if (present(oclients)) then
          if (arrdes%split_checkpoint) then
             call MAPL_GridGet(arrdes%grid,globalCellCountPerDim=global_dim,rc=status)
              _VERIFY(status)
             call MAPL_Grid_interior(arrdes%grid,i1,in,j1,jn)
             _ASSERT( i1 == arrdes%I1(arrdes%NX0), "interior starting i1 not match")
             _ASSERT( j1 == arrdes%j1(arrdes%NY0), "interior starting j1 not match")
             _ASSERT( size(a,1) == in-i1+1, "size not match")
             _ASSERT( size(a,2) == jn-j1+1, "size not match")
             counts_per_writer = global_dim(2)/arrdes%num_writers
             allocate(a_temp(0,0,0))
             do i=1,arrdes%num_writers
                start_bound = (i-1)*counts_per_writer+1
                end_bound   = i*counts_per_writer
                in_bounds = (j1 .ge. start_bound) .and. (jn .le. end_bound)
                dim3 = size(a,3)
                if (in_bounds) then
                   ref = ArrayReference(A)
                else
                   ref = ArrayReference(a_temp)
                end if
                writer_filename = get_fname_by_rank(trim(arrdes%filename),i-1)
                call oClients%collective_stage_data(arrdes%collection_id(i),trim(writer_filename),trim(name), &
                            ref,start=[i1,j1-(i-1)*counts_per_writer,1], &
                            global_start=[1,1,1], global_count=[global_dim(1),global_dim(2)/arrdes%num_writers,dim3])
             enddo
             _RETURN(_SUCCESS)
          else
             call MAPL_GridGet(arrdes%grid,globalCellCountPerDim=global_dim,rc=status)
              _VERIFY(status)
             call MAPL_Grid_interior(arrdes%grid,i1,in,j1,jn)
             _ASSERT( i1 == arrdes%I1(arrdes%NX0), "interior starting i1 not match")
             _ASSERT( j1 == arrdes%j1(arrdes%NY0), "interior starting j1 not match")

             ref = ArrayReference(A)
             _ASSERT( size(a,1) == in-i1+1, "size not match")
             _ASSERT( size(a,2) == jn-j1+1, "size not match")
             call oClients%collective_stage_data(arrdes%collection_id(1),trim(arrdes%filename),trim(name), &
                         ref,start=[i1,j1,1], &
                         global_start=[1,1,1], global_count=[global_dim(1),global_dim(2),size(a,3)])
             _RETURN(_SUCCESS)
          end if
    else
       do l=1,size(a,3)
          call MAPL_VarWrite(formatter,name,A(:,:,l),arrdes,lev=l, rc=status)
          _VERIFY(status)
       enddo
    end if
    _RETURN(ESMF_SUCCESS)

  end subroutine MAPL_VarWriteNCpar_R8_3d

!---------------------------

  subroutine MAPL_VarReadNCpar_R8_3d(formatter, name, A, ARRDES, RC)

    type(Netcdf4_Fileformatter)           , intent(IN   ) :: formatter
    character(len=*)            , intent(IN   ) :: name
    real(kind=ESMF_KIND_R8)     , intent(INOUT) :: A(:,:,:)
    type(ArrDescr)              , intent(INOUT) :: ARRDES
    integer,           optional , intent(  OUT) :: RC

    integer                               :: status
    integer :: l

    do l=1,size(a,3)
       call MAPL_VarRead(formatter,name,A(:,:,l),arrdes,lev=l, rc=status)
       _VERIFY(status)
    enddo

    _RETURN(ESMF_SUCCESS)
  end subroutine MAPL_VarReadNCpar_R8_3d

!---------------------------

  subroutine MAPL_VarWriteNCpar_R4_2d(formatter, name, A, ARRDES, lev, offset2, oClients, RC)

    type(Netcdf4_Fileformatter)           , intent(IN   ) :: formatter
    character(len=*)            , intent(IN   ) :: name
    real(kind=ESMF_KIND_R4)     , intent(IN   ) :: A(:,:)
    type(ArrDescr),    optional , intent(INOUT) :: ARRDES
    integer,           optional , intent(IN   ) :: lev
    integer,           optional , intent(IN   ) :: offset2
    type (ClientManager), optional, intent(inout) :: oClients
    integer,           optional , intent(  OUT) :: RC

! Local variables
    real(kind=ESMF_KIND_R4),  allocatable :: VAR(:,:)
    integer                               :: IM_WORLD
    integer                               :: status

    real(kind=ESMF_KIND_R4),  allocatable :: recvbuf(:)
    integer                               :: I,J,N,K,L,myrow,myiorank,ndes_x
    integer                               :: start(4), cnt(4)
    integer                               :: jsize, jprev, num_io_rows
    integer, allocatable                  :: recvcounts(:), displs(:)

    type (ArrayReference) :: ref
    integer ::  i1, j1, in, jn,  global_dim(3), jp1
    integer :: start_bound,end_bound,counts_per_writer
    logical :: in_bounds
    real(kind=ESMF_KIND_R4), pointer :: a_temp(:,:)
    character(len=:), allocatable :: writer_filename

    if (present(arrdes)) then
       if(present(oClients)) then
          if (arrdes%split_checkpoint) then
             call MAPL_GridGet(arrdes%grid,globalCellCountPerDim=global_dim,rc=status)
              _VERIFY(status)
             call MAPL_Grid_interior(arrdes%grid,i1,in,j1,jn)
             _ASSERT( i1 == arrdes%I1(arrdes%NX0), "interior starting i1 not match")
             _ASSERT( j1 == arrdes%j1(arrdes%NY0), "interior starting j1 not match")
             _ASSERT( size(a,1) == in-i1+1, "size not match")
             _ASSERT( size(a,2) == jn-j1+1, "size not match")
             counts_per_writer = global_dim(2)/arrdes%num_writers
             allocate(a_temp(0,0))
             do i=1,arrdes%num_writers
                start_bound = (i-1)*counts_per_writer+1
                end_bound   = i*counts_per_writer
                in_bounds = (j1 .ge. start_bound) .and. (jn .le. end_bound)
                if (in_bounds) then
                   ref = ArrayReference(A)
                   jp1 = j1 - (i-1)*counts_per_writer
                else
                   ref = ArrayReference(a_temp)
                   jp1 = 1
                end if
                writer_filename = get_fname_by_rank(trim(arrdes%filename),i-1)
                call oClients%collective_stage_data(arrdes%collection_id(i),trim(writer_filename),trim(name), &
                            ref,start=[i1,jp1], &
                            global_start=[1,1], global_count=[global_dim(1),global_dim(2)/arrdes%num_writers])
             enddo
             _RETURN(_SUCCESS)
          else
             call MAPL_GridGet(arrdes%grid,globalCellCountPerDim=global_dim,rc=status)
              _VERIFY(status)
             call MAPL_Grid_interior(arrdes%grid,i1,in,j1,jn)
             _ASSERT( i1 == arrdes%I1(arrdes%NX0), "interior starting i1 not match")
             _ASSERT( j1 == arrdes%j1(arrdes%NY0), "interior starting j1 not match")

             ref = ArrayReference(A)
             _ASSERT( size(a,1) == in-i1+1, "size not match")
             _ASSERT( size(a,2) == jn-j1+1, "size not match")
             call oClients%collective_stage_data(arrdes%collection_id(1),trim(arrdes%filename),trim(name), &
                         ref,start=[i1,j1], &
                         global_start=[1,1], global_count=[global_dim(1),global_dim(2)])
             _RETURN(_SUCCESS)
          end if
       end if
    endif


    if (present(arrdes)) then

       IM_WORLD = arrdes%im_world

       ndes_x = size(arrdes%in)

       call mpi_comm_rank(arrdes%ycomm,myrow,status)
       _VERIFY(STATUS)
       call mpi_comm_rank(arrdes%iogathercomm,myiorank,status)
       _VERIFY(STATUS)
       call mpi_comm_size(arrdes%iogathercomm,num_io_rows,status)
       _VERIFY(STATUS)
       num_io_rows=num_io_rows/ndes_x

       allocate (recvcounts(ndes_x*num_io_rows), displs(ndes_x*num_io_rows), stat=status)
       _VERIFY(STATUS)

       if(myiorank==0) then
          do j=1,num_io_rows
             jsize = arrdes%jn(myrow+j) - arrdes%j1(myrow+j) + 1
             recvcounts((j-1)*ndes_x+1:(j-1)*ndes_x+ndes_x) = ( arrdes%IN -  arrdes%I1 + 1) * jsize
          enddo

          displs(1) = 0
          do i=2,ndes_x*num_io_rows
             displs(i) = displs(i-1) + recvcounts(i-1)
          enddo

          jsize = 0
          do j=1,num_io_rows
             jsize=jsize + (arrdes%jn(myrow+j) - arrdes%j1(myrow+j) + 1)
          enddo
          allocate(VAR(IM_WORLD,jsize), stat=status)
          _VERIFY(STATUS)
          allocate(recvbuf(IM_WORLD*jsize), stat=status)
          _VERIFY(STATUS)
       end if

       if(myiorank/=0) then
          allocate(recvbuf(0), stat=status)
          _VERIFY(STATUS)
       endif

       call mpi_gatherv( a, size(a), MPI_REAL, recvbuf, recvcounts, displs, MPI_REAL, &
                      0, arrdes%iogathercomm, status )
       _VERIFY(STATUS)

       if(myiorank==0) then

          jprev = 0
          k=1
          do l=1,num_io_rows
            jsize = arrdes%jn(myrow+l) - arrdes%j1(myrow+l) + 1
            do n=1,ndes_x
              do j=1,jsize
                do i=arrdes%i1(n),arrdes%in(n)
                  VAR(i,jprev+j) = recvbuf(k)
                  k=k+1
                end do
              end do
            end do
            jprev = jprev + jsize
          end do
          jsize=jprev

          start(1) = 1
          start(2) = arrdes%j1(myrow+1)
          start(3) = 1
          if (present(lev)) start(3)=lev
          start(4) = 1
          if (present(offset2)) start(4) = offset2
          cnt(1) = IM_WORLD
          cnt(2) = jsize
          cnt(3) = 1
          cnt(4) = 1

          if(arrdes%split_checkpoint) then
             start(2) = 1
          endif

          call formatter%put_var(trim(name),VAR,start=start,count=cnt,rc=status)
          if(status /= NF90_NOERR) then
             print*,'Error writing variable ',status
             print*, NF90_STRERROR(status)
             _VERIFY(STATUS)
          endif
          deallocate(VAR, stat=status)
          _VERIFY(STATUS)

       endif ! myiorank

       deallocate(recvbuf, stat=status)
       _VERIFY(STATUS)
       deallocate (recvcounts, displs, stat=status)
       _VERIFY(STATUS)

    else

          start(1) = 1
          start(2) = 1
          start(3) = 1
          if (present(lev)) start(3)=lev
          start(4) = 1
          if (present(offset2)) start(4) = offset2
          cnt(1) = size(a,1)
          cnt(2) = size(a,2)
          cnt(3) = 1
          cnt(4) = 1

          call formatter%put_var(trim(name),A,start=start,count=cnt,rc=status)
          if(status /= NF90_NOERR) then
             print*,'Error writing variable ',status
             print*, NF90_STRERROR(status)
             _VERIFY(STATUS)
          endif

    end if

    _RETURN(ESMF_SUCCESS)
  end subroutine MAPL_VarWriteNCpar_R4_2d

!---------------------------

  subroutine MAPL_VarReadNCpar_R4_2d(formatter, name, A, ARRDES, lev, offset2, RC)

    type(Netcdf4_Fileformatter)           , intent(IN   ) :: formatter
    character(len=*)            , intent(IN   ) :: name
    real(kind=ESMF_KIND_R4)     , intent(INOUT) :: A(:,:)
    type(ArrDescr), optional    , intent(INOUT) :: ARRDES
    integer, optional           , intent(IN   ) :: lev
    integer, optional           , intent(IN   ) :: offset2
    integer,           optional , intent(  OUT) :: RC

! Local variables
    real(kind=ESMF_KIND_R4),  allocatable :: VAR(:,:)
    integer                               :: IM_WORLD
    integer                               :: JM_WORLD
    integer                               :: status

    real(kind=ESMF_KIND_R4),  allocatable :: buf(:)
    integer                               :: I,J,N,K,L,myrow,myiorank,ndes_x
    integer                               :: start(4), cnt(4)
    integer                               :: jsize, jprev, num_io_rows
    integer, allocatable                  :: sendcounts(:), displs(:)

    if (present(arrdes) ) then

       IM_WORLD = arrdes%im_world
       JM_WORLD = arrdes%jm_world

       ndes_x = size(arrdes%in)
       call mpi_comm_rank(arrdes%ycomm,myrow,status)
       _VERIFY(STATUS)
       call mpi_comm_rank(arrdes%ioscattercomm,myiorank,status)
       _VERIFY(STATUS)
       call mpi_comm_size(arrdes%ioscattercomm,num_io_rows,status)
       _VERIFY(STATUS)
       num_io_rows=num_io_rows/ndes_x

       allocate (sendcounts(ndes_x*num_io_rows), displs(ndes_x*num_io_rows), stat=status)
       _VERIFY(STATUS)

       if(myiorank==0) then
          do j=1,num_io_rows
             jsize = arrdes%jn(myrow+j) - arrdes%j1(myrow+j) + 1
             sendcounts((j-1)*ndes_x+1:(j-1)*ndes_x+ndes_x) = ( arrdes%IN -  arrdes%I1 + 1) * jsize
          enddo

          displs(1) = 0
          do i=2,ndes_x*num_io_rows
             displs(i) = displs(i-1) + sendcounts(i-1)
          enddo

          jsize = 0
          do j=1,num_io_rows
             jsize=jsize + (arrdes%jn(myrow+j) - arrdes%j1(myrow+j) + 1)
          enddo
          allocate(VAR(IM_WORLD,jsize), stat=status)
          _VERIFY(STATUS)
          allocate(buf(IM_WORLD*jsize), stat=status)
          _VERIFY(STATUS)

          start(1) = 1
          if (arrdes%split_restart) then
             start(2) = 1
          else
             start(2) = arrdes%j1(myrow+1)
          end if
          start(3) = 1
          if (present(lev)) start(3) = lev
          start(4) = 1
          if (present(offset2)) start(4) = offset2
          cnt(1) = IM_WORLD
          cnt(2) = jsize
          cnt(3) = 1
          cnt(4) = 1

          if(arrdes%split_restart) then
             start(2) = 1
          endif

          call formatter%get_var(trim(name),VAR,start=start,count=cnt,rc=status)
          if(status /= NF90_NOERR) then
             print*,'Error reading variable ',status
             print*, NF90_STRERROR(status)
             _VERIFY(STATUS)
          endif

          jprev = 0
          k=1
          do l=1,num_io_rows
             jsize = arrdes%jn(myrow+l) - arrdes%j1(myrow+l) + 1
             do n=1,ndes_x
               do j=1,jsize
                 do i=arrdes%i1(n),arrdes%in(n)
                   buf(k) = VAR(i,jprev+j)
                   k=k+1
                 end do
               end do
             end do
             jprev = jprev + jsize
          end do

          deallocate(VAR, stat=status)
          _VERIFY(STATUS)
       end if ! myiorank

       if(myiorank/=0) then
          allocate(buf(0), stat=status)
          _VERIFY(STATUS)
       endif

       call mpi_scatterv( buf, sendcounts, displs, MPI_REAL, &
               a,  size(a),  MPI_REAL, &
               0, arrdes%ioscattercomm, status )
       _VERIFY(STATUS)

       deallocate(buf, stat=status)
       _VERIFY(STATUS)
       deallocate (sendcounts, displs, stat=status)
       _VERIFY(STATUS)

    else

       start(1) = 1
       start(2) = 1
       start(3) = 1
       if (present(lev) ) start(3)=lev
       start(4) = 1
       if (present(offset2)) start(4) = offset2
       cnt(1) = size(a,1)
       cnt(2) = size(a,2)
       cnt(3) = 1
       cnt(4) = 1

       call formatter%get_var(trim(name),A,start=start,count=cnt,rc=status)
       if(status /= NF90_NOERR) then
          print*,'Error reading variable ',status
          print*, NF90_STRERROR(status)
          _VERIFY(STATUS)
       endif

    end if

    _RETURN(ESMF_SUCCESS)
  end subroutine MAPL_VarReadNCpar_R4_2d

!---------------------------

  subroutine MAPL_VarWriteNCpar_R4_1d(formatter, name, A, layout, ARRDES, MASK, offset1, offset2, RC)

    type(Netcdf4_Fileformatter)           , intent(IN   ) :: formatter
    character(len=*)            , intent(IN   ) :: name
    real(kind=ESMF_KIND_R4)     , intent(IN   ) :: A(:)
    type (ESMF_DELayout), optional, intent(IN   ) :: layout
    type(ArrDescr), optional, intent(INOUT) :: ARRDES
    integer,           optional , intent(IN   ) :: MASK(:)
    integer,           optional,  intent(IN   ) :: offset1
    integer,           optional,  intent(IN   ) :: offset2
    integer,           optional , intent(  OUT) :: RC

! Local variables
    real(kind=ESMF_KIND_R4),  allocatable :: VAR(:)
    real(kind=ESMF_KIND_R4),  allocatable :: GVAR(:)
    integer                               :: IM_WORLD
    integer                               :: status
    character(len=ESMF_MAXSTR)            :: IAm='MAPL_VarWriteNCpar_R4_1d'

    integer, allocatable                  :: msk(:), recvcounts(:), displs(:)
    integer                               :: nwrts, mype,  npes, sendcount
    integer                               :: mypeWr, io_rank
    integer                               :: Rsize, first, last
    integer(KIND=MPI_OFFSET_KIND)         :: offset
    integer                               :: i, k, n
    integer                               :: ii
    real(kind=ESMF_KIND_R4)               :: dummy
    integer                               :: group, newgroup
    integer                               :: thiscomm
    integer                               :: nactive
    integer                               :: ntransl
    integer, allocatable                  :: pes(:)
    integer, allocatable                  :: inv_pes(:)
    integer, allocatable                  :: r2g(:)
    integer, allocatable                  :: rpes(:)
    integer, allocatable                  :: activeranks(:)
    integer, allocatable                  :: activerecvcounts(:)
    integer                               :: start(4), cnt(4)

    if(present(mask) .and. present(layout) .and. present(arrdes) ) then

       IM_WORLD = arrdes%im_world

       call mpi_comm_size(arrdes%iogathercomm,npes ,status)
       _VERIFY(STATUS)
       if(arrdes%writers_comm /= MPI_COMM_NULL) then
          call mpi_comm_rank(arrdes%writers_comm,mypeWr ,status)
          _VERIFY(STATUS)
          call mpi_comm_size(arrdes%writers_comm,nwrts,status)
          _VERIFY(STATUS)
       else
          mypeWr = -1
       endif
       call MAPL_CommsBcast(layout, nwrts, 1, 0, rc = status)

       Rsize = im_world/nwrts + 1
       first = mypeWr*Rsize + 1
       if(mypeWr >=  mod(im_world,nwrts)) then
          Rsize = Rsize - 1
          first = first - (mypeWr-mod(im_world,nwrts))
       endif
       last  = first + Rsize - 1

#ifdef DEBUG_MPIIO
        if (mypeWr <= nwrts-1) write(*,'(5i)') mypeWr, IM_WORLD, first, last, Rsize
#endif

       if(arrdes%writers_comm /= MPI_COMM_NULL) then
          allocate(GVAR(Rsize), _STAT)
          allocate(VAR(Rsize), msk(Rsize), _STAT)
       else
          allocate(VAR(0), msk(0), _STAT)
       end if
       allocate (recvcounts(0:npes-1), stat=status)
       _VERIFY(STATUS)
       allocate (r2g(0:nwrts-1), stat=status)
       _VERIFY(STATUS)
       allocate(inv_pes(0:npes-1),stat=status)
       _VERIFY(STATUS)

       call mpi_comm_rank(arrdes%iogathercomm,mype ,status)
       _VERIFY(STATUS)

       call MPI_COMM_GROUP (arrdes%iogathercomm, GROUP, STATUS)
       _VERIFY(STATUS)

#if 1
       if (arrdes%writers_comm /= MPI_COMM_NULL) then
          allocate(rpes(0:nwrts-1), stat=status)
          _VERIFY(STATUS)

          call MPI_COMM_GROUP (arrdes%writers_comm, NEWGROUP, STATUS)
          _VERIFY(STATUS)
          do n=0,nwrts-1
             rpes(n) = n
          end do
          call MPI_Group_translate_ranks(newgroup, nwrts, rpes, group, r2g, status)
          _VERIFY(STATUS)
          call MPI_GROUP_FREE (NEWGROUP, STATUS)
          _VERIFY(STATUS)
          deallocate(rpes)
       end if
       call MAPL_CommsBcast(layout, r2g, nwrts, 0, rc = status)

#else
       do n=0,nrdrs-1
          r2g(n) = (npes/nrdrs)*n
       end do
#endif
       offset = 1

       do n=0,nwrts-1

          Rsize = im_world/nwrts + 1
          first = n*Rsize + 1
          if(n >=  mod(im_world,nwrts)) then
             Rsize = Rsize - 1
             first = first - (n-mod(im_world,nwrts))
          endif
          last  = first + Rsize - 1

          recvcounts = 0
          do i=first,last
             recvcounts(mask(i)) = recvcounts(mask(i)) + 1
          enddo

          ! Writer "n" must be included in the mpi group + evevybody that need the data
          nactive = count(recvcounts > 0)
          if (recvcounts(r2g(n)) == 0) then
             nactive = nactive + 1
          end if
          allocate (activeranks(0:nactive-1), activerecvcounts(0:nactive-1), stat=status)
          _VERIFY(STATUS)
          allocate(pes(0:nactive-1), stat=status)
          _VERIFY(STATUS)
          allocate (displs(0:nactive), stat=status)
          _VERIFY(STATUS)
          k = 0
          do i=0, npes-1
             if (recvcounts(i) > 0) then
                pes(k) = i
                k = k+1
             end if
          enddo
          if (k /= nactive) then
             k = k+1
             _ASSERT(k == nactive, 'inconsistent nactive')
             _ASSERT(recvcounts(r2g(n)) == 0, 'recvcounts must be 0')
             pes(nactive-1) = r2g(n)
          end if
          call MPI_GROUP_INCL (GROUP, nactive, PES, newgroup, STATUS)
          _VERIFY(STATUS)
          call MPI_COMM_CREATE(arrdes%iogathercomm, newgroup, thiscomm, STATUS)
          _VERIFY(STATUS)
          call MPI_Group_translate_ranks(group, nactive, pes, newgroup, activeranks, status)
          _VERIFY(STATUS)
          call MPI_GROUP_FREE (NEWGROUP, STATUS)
          _VERIFY(STATUS)
          inv_pes = -1 ! initialized to invalid
          do i=0,nactive-1
             inv_pes(pes(i)) = i
          end do

          if (thiscomm /= MPI_COMM_NULL) then
             activerecvcounts = 0
             do i=0,nactive-1
                activerecvcounts(activeranks(i)) = recvcounts(pes(i))
                if (pes(i) == r2g(n)) ntransl = activeranks(i)
             end do
             displs(0) = 0
             do i=1,nactive
                displs(i) = displs(i-1) + activerecvcounts(i-1)
             enddo

             sendcount = recvcounts(mype)

             if (sendcount == 0) then
                call MPI_GATHERV( dummy, sendcount, MPI_REAL, &
                                  var,   activerecvcounts, displs, MPI_REAL, &
                                  ntransl, thiscomm, status )
             else
                call MPI_GATHERV( a(offset), sendcount, MPI_REAL, &
                                  var, activerecvcounts, displs, MPI_REAL, &
                                  ntransl, thiscomm, status )
             endif
             _VERIFY(STATUS)
             call MPI_Comm_Free(thiscomm, status)
             _VERIFY(STATUS)

             if(n==mypeWr) then
                msk = mask(first:last)

                do I=1,Rsize
                   K = inv_pes(MSK(I))
                   II = displs(K)+1 ! var is 1-based
                   GVAR(I) = VAR(II)
                   displs(K) = displs(K) + 1
                end do
             endif
             offset = offset + sendcount
          end if
          deallocate (displs)
          deallocate(pes)
          deallocate (activerecvcounts, activeranks)

       enddo
       if(arrdes%writers_comm /= MPI_COMM_NULL) then

          Rsize = im_world/nwrts + 1
          first = mypeWr*Rsize + 1
          if(mypeWr >=  mod(im_world,nwrts)) then
             Rsize = Rsize - 1
             first = first - (mypeWr-mod(im_world,nwrts))
          endif
          last  = first + Rsize - 1

          _ASSERT( (lbound(mask,1) <= first), 'out of bounds' )
          _ASSERT( (ubound(mask,1) >= last ), 'out of bounds' )
! lon, lat, lev, time
          start(1) = first
          start(2) = 1
          start(3) = 1
          if (present(offset1)) start(2) = offset1
          if (present(offset2)) start(3) = offset2
          start(4) = 1
          cnt(1) = Rsize
          cnt(2) = 1
          cnt(3) = 1
          cnt(4) = 1
!          print*,'start values are ',start
!          print*,'count values are ',cnt

          call formatter%put_var(trim(name),gvar,start=start,count=cnt,rc=status)
          if(status /= NF90_NOERR) then
             print*,'Error writing variable ', status
             print*, NF90_STRERROR(status)
             _VERIFY(STATUS)
          endif
       endif

       call MPI_GROUP_FREE (GROUP, STATUS)
       _VERIFY(STATUS)
       deallocate(var,msk)
       deallocate (inv_pes)
       deallocate (r2g)
       deallocate(recvcounts)
       if(arrdes%writers_comm /= MPI_COMM_NULL) then
          deallocate(gvar)
       end if

    else

! Comments
! This routine is used to write PREF to moist_import_checkpoint

       start(1) = 1
       start(2) = 1
       start(3) = 1
       if (present(offset1)) start(2) = offset1
       if (present(offset2)) start(3) = offset2
       start(4) = 1
       cnt(1) = size(a)
       cnt(2) = 1
       cnt(3) = 1
       cnt(4) = 1

       if (present(arrdes)) then

          if (arrdes%writers_comm/=MPI_COMM_NULL) then

             call MPI_COMM_RANK(arrdes%writers_comm, io_rank, STATUS)
             _VERIFY(STATUS)

             if (io_rank == 0 .or. arrdes%split_checkpoint) then
                call formatter%put_var(trim(name),A,start=start,count=cnt,rc=status)
                if(status /= NF90_NOERR) then
                   print*,trim(IAm),'Error writing variable ',status
                   print*, NF90_STRERROR(status)
                   _VERIFY(STATUS)
                endif
             endif ! io_rank
          endif
       else ! not present(arrdes)
          ! WY notes : it doesnot seem to get this branch
          call formatter%put_var(trim(name),A,start=start,count=cnt,rc=status)
          if(status /= NF90_NOERR) then
             print*,trim(IAm),' :Error writing variable: '// trim(name)
             print*, NF90_STRERROR(status)
             _VERIFY(STATUS)
          endif

       end if

    end if

    _RETURN(ESMF_SUCCESS)
  end subroutine MAPL_VarWriteNCpar_R4_1d

  subroutine MAPL_VarWriteNCpar_R8_1d(formatter, name, A, layout, ARRDES, MASK, offset1, offset2, RC)

    type(Netcdf4_Fileformatter)           , intent(IN   ) :: formatter
    character(len=*)            , intent(IN   ) :: name
    real(kind=ESMF_KIND_R8)     , intent(IN   ) :: A(:)
    type (ESMF_DELayout), optional, intent(IN   ) :: layout
    type(ArrDescr), optional, intent(INOUT) :: ARRDES
    integer,           optional , intent(IN   ) :: MASK(:)
    integer,           optional,  intent(IN   ) :: offset1
    integer,           optional,  intent(IN   ) :: offset2
    integer,           optional , intent(  OUT) :: RC

! Local variables
    real(kind=ESMF_KIND_R8),  allocatable :: VAR(:)
    real(kind=ESMF_KIND_R8),  allocatable :: GVAR(:)
    integer                               :: IM_WORLD
    integer                               :: status
    character(len=ESMF_MAXSTR)            :: IAm='MAPL_VarWriteNCpar_R8_1d'

    integer, allocatable                  :: msk(:), recvcounts(:), displs(:)
    integer                               :: nwrts, mype,  npes, sendcount
    integer                               :: mypeWr, io_rank
    integer                               :: Rsize, first, last
    integer(KIND=MPI_OFFSET_KIND)         :: offset
    integer                               :: i, k, n
    integer                               :: ii
    real(kind=ESMF_KIND_R8)               :: dummy
    integer                               :: group, newgroup
    integer                               :: thiscomm
    integer                               :: nactive
    integer                               :: ntransl
    integer, allocatable                  :: pes(:)
    integer, allocatable                  :: inv_pes(:)
    integer, allocatable                  :: r2g(:)
    integer, allocatable                  :: rpes(:)
    integer, allocatable                  :: activeranks(:)
    integer, allocatable                  :: activerecvcounts(:)
    integer                               :: start(4), cnt(4)


    if(present(mask) .and. present(layout) .and. present(arrdes) ) then

       IM_WORLD = arrdes%im_world

       call mpi_comm_size(arrdes%iogathercomm,npes ,status)
       _VERIFY(STATUS)
       if(arrdes%writers_comm /= MPI_COMM_NULL) then
          call mpi_comm_rank(arrdes%writers_comm,mypeWr ,status)
          _VERIFY(STATUS)
          call mpi_comm_size(arrdes%writers_comm,nwrts,status)
          _VERIFY(STATUS)
       else
          mypeWr = -1
       endif
       call MAPL_CommsBcast(layout, nwrts, 1, 0, rc = status)

       Rsize = im_world/nwrts + 1
       first = mypeWr*Rsize + 1
       if(mypeWr >=  mod(im_world,nwrts)) then
          Rsize = Rsize - 1
          first = first - (mypeWr-mod(im_world,nwrts))
       endif
       last  = first + Rsize - 1

#ifdef DEBUG_MPIIO
        if (mypeWr <= nwrts-1) write(*,'(5i)') mypeWr, IM_WORLD, first, last, Rsize
#endif

       if(arrdes%writers_comm /= MPI_COMM_NULL) then
          allocate(GVAR(Rsize), stat=status)
          _VERIFY(STATUS)
       end if
       allocate(VAR(Rsize), stat=status)
       _VERIFY(STATUS)
       allocate(msk(Rsize), stat=status)
       _VERIFY(STATUS)
       allocate (recvcounts(0:npes-1), stat=status)
       _VERIFY(STATUS)
       allocate (r2g(0:nwrts-1), stat=status)
       _VERIFY(STATUS)
       allocate(inv_pes(0:npes-1),stat=status)
       _VERIFY(STATUS)

       call mpi_comm_rank(arrdes%iogathercomm,mype ,status)
       _VERIFY(STATUS)

       call MPI_COMM_GROUP (arrdes%iogathercomm, GROUP, STATUS)
       _VERIFY(STATUS)

#if 1
       if (arrdes%writers_comm /= MPI_COMM_NULL) then
          allocate(rpes(0:nwrts-1), stat=status)
          _VERIFY(STATUS)

          call MPI_COMM_GROUP (arrdes%writers_comm, NEWGROUP, STATUS)
          _VERIFY(STATUS)
          do n=0,nwrts-1
             rpes(n) = n
          end do
          call MPI_Group_translate_ranks(newgroup, nwrts, rpes, group, r2g, status)
          _VERIFY(STATUS)
          call MPI_GROUP_FREE (NEWGROUP, STATUS)
          _VERIFY(STATUS)
          deallocate(rpes)
       end if
       call MAPL_CommsBcast(layout, r2g, nwrts, 0, rc = status)

#else
       do n=0,nrdrs-1
          r2g(n) = (npes/nrdrs)*n
       end do
#endif
       offset = 1

       do n=0,nwrts-1

          Rsize = im_world/nwrts + 1
          first = n*Rsize + 1
          if(n >=  mod(im_world,nwrts)) then
             Rsize = Rsize - 1
             first = first - (n-mod(im_world,nwrts))
          endif
          last  = first + Rsize - 1

          recvcounts = 0
          do i=first,last
             recvcounts(mask(i)) = recvcounts(mask(i)) + 1
          enddo

          ! Writer "n" must be included in the mpi group + evevybody that need the data
          nactive = count(recvcounts > 0)
          if (recvcounts(r2g(n)) == 0) then
             nactive = nactive + 1
          end if
          allocate (activeranks(0:nactive-1), activerecvcounts(0:nactive-1), stat=status)
          _VERIFY(STATUS)
          allocate(pes(0:nactive-1), stat=status)
          _VERIFY(STATUS)
          allocate (displs(0:nactive), stat=status)
          _VERIFY(STATUS)
          k = 0
          do i=0, npes-1
             if (recvcounts(i) > 0) then
                pes(k) = i
                k = k+1
             end if
          enddo
          if (k /= nactive) then
             k = k+1
             _ASSERT(k == nactive, 'inconsistent nactive')
             _ASSERT(recvcounts(r2g(n)) == 0, 'recvcounts should be 0')
             pes(nactive-1) = r2g(n)
          end if
          call MPI_GROUP_INCL (GROUP, nactive, PES, newgroup, STATUS)
          _VERIFY(STATUS)
          call MPI_COMM_CREATE(arrdes%iogathercomm, newgroup, thiscomm, STATUS)
          _VERIFY(STATUS)
          call MPI_Group_translate_ranks(group, nactive, pes, newgroup, activeranks, status)
          _VERIFY(STATUS)
          call MPI_GROUP_FREE (NEWGROUP, STATUS)
          _VERIFY(STATUS)
          inv_pes = -1 ! initialized to invalid
          do i=0,nactive-1
             inv_pes(pes(i)) = i
          end do

          if (thiscomm /= MPI_COMM_NULL) then
             activerecvcounts = 0
             do i=0,nactive-1
                activerecvcounts(activeranks(i)) = recvcounts(pes(i))
                if (pes(i) == r2g(n)) ntransl = activeranks(i)
             end do
             displs(0) = 0
             do i=1,nactive
                displs(i) = displs(i-1) + activerecvcounts(i-1)
             enddo

             sendcount = recvcounts(mype)

             if (sendcount == 0) then
                call MPI_GATHERV( dummy, sendcount, MPI_DOUBLE_PRECISION, &
                                  var,   activerecvcounts, displs, MPI_DOUBLE_PRECISION, &
                                  ntransl, thiscomm, status )
             else
                call MPI_GATHERV( a(offset), sendcount, MPI_DOUBLE_PRECISION, &
                                  var, activerecvcounts, displs, MPI_DOUBLE_PRECISION, &
                                  ntransl, thiscomm, status )
             endif
             _VERIFY(STATUS)
             call MPI_Comm_Free(thiscomm, status)
             _VERIFY(STATUS)

             if(n==mypeWr) then
                msk = mask(first:last)

                do I=1,Rsize
                   K = inv_pes(MSK(I))
                   II = displs(K)+1 ! var is 1-based
                   GVAR(I) = VAR(II)
                   displs(K) = displs(K) + 1
                end do
             endif
             offset = offset + sendcount
          end if
          deallocate (displs)
          deallocate(pes)
          deallocate (activerecvcounts, activeranks)

       enddo
       if(arrdes%writers_comm /= MPI_COMM_NULL) then

          Rsize = im_world/nwrts + 1
          first = mypeWr*Rsize + 1
          if(mypeWr >=  mod(im_world,nwrts)) then
             Rsize = Rsize - 1
             first = first - (mypeWr-mod(im_world,nwrts))
          endif
          last  = first + Rsize - 1

          _ASSERT( (lbound(mask,1) <= first), 'out of bounds' )
          _ASSERT( (ubound(mask,1) >= last ), 'out of bounds' )
! lon, lat, lev, time
          start(1) = first
          start(2) = 1
          start(3) = 1
          if (present(offset1)) start(2) = offset1
          if (present(offset2)) start(3) = offset2
          start(4) = 1
          cnt(1) = Rsize
          cnt(2) = 1
          cnt(3) = 1
          cnt(4) = 1
!          print*,'start values are ',start
!          print*,'count values are ',cnt

          call formatter%put_var(trim(name),gvar,start=start,count=cnt,rc=status)
          if(status /= NF90_NOERR) then
             print*,'Error writing variable ', status
             print*, NF90_STRERROR(status)
             _VERIFY(STATUS)
          endif
       endif

       call MPI_GROUP_FREE (GROUP, STATUS)
       _VERIFY(STATUS)
       deallocate(var,msk)
       deallocate (inv_pes)
       deallocate (r2g)
       deallocate(recvcounts)
       if(arrdes%writers_comm /= MPI_COMM_NULL) then
          deallocate(gvar)
       end if

    else

! Comments
! This routine is used to write PREF to moist_import_checkpoint

       start(1) = 1
       start(2) = 1
       start(3) = 1
       if (present(offset1)) start(2) = offset1
       if (present(offset2)) start(3) = offset2
       start(4) = 1
       cnt(1) = size(a)
       cnt(2) = 1
       cnt(3) = 1
       cnt(4) = 1

       if (present(arrdes)) then

          if (arrdes%writers_comm/=MPI_COMM_NULL) then

             call MPI_COMM_RANK(arrdes%writers_comm, io_rank, STATUS)
             _VERIFY(STATUS)

             if (io_rank == 0 .or. arrdes%split_checkpoint) then
                call formatter%put_var(trim(name),A,start=start,count=cnt,rc=status)
                if(status /= NF90_NOERR) then
                   print*,trim(IAm),'Error writing variable ',status
                   print*, NF90_STRERROR(status)
                   _VERIFY(STATUS)
                endif
             endif ! io_rank
           endif

       else
          !WJ notes : not here
          call formatter%put_var(trim(name),A,start=start,count=cnt,rc=status)
          if(status /= NF90_NOERR) then
             print*,trim(IAm),'Error writing variable ',status
             print*, NF90_STRERROR(status)
             _VERIFY(STATUS)
          endif

       end if

    end if

    _RETURN(ESMF_SUCCESS)
  end subroutine MAPL_VarWriteNCpar_R8_1d

!----------------------------------------------------------------------------

  subroutine MAPL_VarReadNCpar_R4_1d(formatter, name, A, layout, ARRDES, MASK, offset1, offset2, RC)

    type(Netcdf4_Fileformatter)             , intent(in   ) :: formatter
    character(len=*)              , intent(in   ) :: name
    real(kind=ESMF_KIND_R4)       , intent(inOUT) :: A(:)
    type (ESMF_DELayout), optional, intent(IN   ) :: layout
    type(ArrDescr), optional,  intent(INOUT) :: ARRDES
    integer,           optional   , intent(IN   ) :: MASK(:)
    integer,           optional,    intent(IN   ) :: offset1
    integer,           optional,    intent(IN   ) :: offset2
    integer,           optional   , intent(  OUT) :: RC

! Local variables
    real(kind=ESMF_KIND_R4),  allocatable :: VAR(:)
    real(kind=ESMF_KIND_R4),  pointer     :: VR(:)=>null()
    integer                               :: IM_WORLD
    integer                               :: status
    character(len=ESMF_MAXSTR)            :: IAm='MAPL_VarReadNCpar_R4_1d'
    integer, allocatable                  :: msk(:), sendcounts(:), displs(:)
    integer, allocatable                  :: idx(:)
    integer                               :: nrdrs, mype,  npes, recvcount
    integer                               :: mypeRd
    integer                               :: Rsize, first, last
    integer(KIND=MPI_OFFSET_KIND)         :: offset
    integer                               :: i, k, n, i1, in
    real(kind=ESMF_KIND_R4)               :: dummy
    integer                               :: group, newgroup
    integer                               :: thiscomm
    integer                               :: nactive
    integer                               :: ntransl
    integer, allocatable                  :: pes(:)
    integer, allocatable                  :: r2g(:)
    integer, allocatable                  :: rpes(:)
    integer, allocatable                  :: activeranks(:)
    integer, allocatable                  :: activesendcounts(:)
    integer                               :: start(4), cnt(4)
    logical                               :: amIRoot

    if(present(mask) .and. present(layout) .and. present(arrdes) ) then

       IM_WORLD = arrdes%im_world
#ifdef USE_MAPL_ORIGINAL_TILE_HANDLING
       call mpi_comm_size(arrdes%ioscattercomm,npes ,status)
       _VERIFY(STATUS)
       if(arrdes%readers_comm /= MPI_COMM_NULL) then
          call mpi_comm_rank(arrdes%readers_comm,mypeRd ,status)
          _VERIFY(STATUS)
          call mpi_comm_size(arrdes%readers_comm,nrdrs,status)
          _VERIFY(STATUS)
       else
          mypeRd = -1
       endif

       call MAPL_CommsBcast(layout, nrdrs, 1, 0, rc = status)
       _VERIFY(STATUS)
       Rsize = im_world/nrdrs + 1
       first = mypeRd*Rsize + 1
       if(mypeRd >=  mod(im_world,nrdrs)) then
          Rsize = Rsize - 1
          first = first - (mypeRd-mod(im_world,nrdrs))
       endif
       last  = first + Rsize - 1

#ifdef DEBUG_MPIIO
        if (mypeRd <= nrdrs-1) write(*,'(5i)') mypeRd, IM_WORLD, first, last, Rsize
#endif

       allocate(VAR(Rsize), stat=status)
       _VERIFY(STATUS)
       allocate(msk(Rsize), stat=status)
       _VERIFY(STATUS)
       allocate (sendcounts(0:npes-1), stat=status)
       _VERIFY(STATUS)
       allocate (r2g(0:nrdrs-1), stat=status)
       _VERIFY(STATUS)

       if(arrdes%readers_comm /= MPI_COMM_NULL) then
          start(1) = first
          start(2) = 1
          start(3) = 1
          if ( present(offset1) ) start(2) = offset1
          if ( present(offset2) ) start(3) = offset2
          start(4) = 1
          cnt(1) = Rsize
          cnt(2) = 1
          cnt(3) = 1
          cnt(4) = 1
!          print*,'start values are ',start
!          print*,'count values are ',count

          call formatter%get_var(trim(name),var,start=start,count=cnt,rc=status)
          if(status /= NF90_NOERR) then
             print*,'Error reading variable ',status
             print*, NF90_STRERROR(status)
             _VERIFY(STATUS)
          endif

          _ASSERT( (lbound(mask,1) <= first), 'out of bounds' )
          _ASSERT( (ubound(mask,1) >= last ), 'out of bounds' )
          msk = mask(first:last)

          allocate(idx(Rsize), stat=status)
          _VERIFY(STATUS)

          do i=1,Rsize
             idx(i) = i
          enddo
          msk = mask(first:last)
          call MAPL_Sort(msk,idx)
          msk = mask(first:last)
          call MAPL_Sort(msk,var)
       endif

       call mpi_comm_rank(arrdes%ioscattercomm,mype ,status)
       _VERIFY(STATUS)

       call MPI_COMM_GROUP (arrdes%ioscattercomm, GROUP, STATUS)
       _VERIFY(STATUS)

#if 1
       if (arrdes%readers_comm /= MPI_COMM_NULL) then
          allocate(rpes(0:nrdrs-1), stat=status)
          _VERIFY(STATUS)

          call MPI_COMM_GROUP (arrdes%readers_comm, NEWGROUP, STATUS)
          _VERIFY(STATUS)
          do n=0,nrdrs-1
             rpes(n) = n
          end do
          call MPI_Group_translate_ranks(newgroup, nrdrs, rpes, group, r2g, status)
          _VERIFY(STATUS)
          call MPI_GROUP_FREE (NEWGROUP, STATUS)
          _VERIFY(STATUS)
          deallocate(rpes)
       end if
       call MAPL_CommsBcast(layout, r2g, nrdrs, 0, rc = status)
       _VERIFY(STATUS)

#else
       do n=0,nrdrs-1
          r2g(n) = (npes/nrdrs)*n
       end do
#endif

       offset = 1

       do n=0,nrdrs-1

          Rsize = im_world/nrdrs + 1
          first = n*Rsize + 1
          if(n >=  mod(im_world,nrdrs)) then
             Rsize = Rsize - 1
             first = first - (n-mod(im_world,nrdrs))
          endif
          last  = first + Rsize - 1

          sendcounts = 0
          do i=first,last
             sendcounts(mask(i)) = sendcounts(mask(i)) + 1
          enddo

          ! Reader "n" must be included in the mpi group + evevybody that need the data
          nactive = count(sendcounts > 0)
          if (sendcounts(r2g(n)) == 0) then
             nactive = nactive + 1
          end if
          allocate (activeranks(0:nactive-1), activesendcounts(0:nactive-1), stat=status)
          _VERIFY(STATUS)
          allocate(pes(0:nactive-1), stat=status)
          _VERIFY(STATUS)
          allocate (displs(0:nactive), stat=status)
          _VERIFY(STATUS)
          k = 0
          do i=0, npes-1
             if (sendcounts(i) > 0) then
                pes(k) = i
                k = k+1
             end if
          enddo
          if (k /= nactive) then
             k = k+1
             _ASSERT(k == nactive, 'inconsistent nactive')
             _ASSERT(sendcounts(r2g(n)) == 0, 'sendcounts should be 0')
             pes(nactive-1) = r2g(n)
          end if
          call MPI_GROUP_INCL (GROUP, nactive, PES, newgroup, STATUS)
          _VERIFY(STATUS)
          call MPI_COMM_CREATE(arrdes%ioscattercomm, newgroup, thiscomm, STATUS)
          _VERIFY(STATUS)
          call MPI_Group_translate_ranks(group, nactive, pes, newgroup, activeranks, status)
          _VERIFY(STATUS)
          call MPI_GROUP_FREE (NEWGROUP, STATUS)
          _VERIFY(STATUS)

          if (thiscomm /= MPI_COMM_NULL) then
             activesendcounts = 0
             do i=0,nactive-1
                activesendcounts(activeranks(i)) = sendcounts(pes(i))
                if (pes(i) == r2g(n)) ntransl = activeranks(i)
             end do
             displs(0) = 0
             do i=1,nactive
                displs(i) = displs(i-1) + activesendcounts(i-1)
             enddo

             if(n==mypeRd) then
                do i=0,nactive-1
                   if(activesendcounts(i)>0) then
                      i1 = displs(i  ) + 1
                      in = displs(i+1)
                      call MAPL_Sort(idx(i1:in),var(i1:in))
                   endif
                end do
             endif

             recvcount = sendcounts(mype)

             if (recvcount == 0) then
                call MPI_SCATTERV( var, activesendcounts, displs, MPI_REAL, &
                                   dummy,   recvcount,  MPI_REAL, &
                                   ntransl, thiscomm,    status )
             else
                call MPI_SCATTERV( var, activesendcounts, displs, MPI_REAL, &
                                   a(offset),   recvcount,  MPI_REAL, &
                                   ntransl, thiscomm,    status )
             endif
             _VERIFY(STATUS)
             call MPI_Comm_Free(thiscomm, status)
             _VERIFY(STATUS)
             offset = offset + recvcount
          end if
          deallocate (displs)
          deallocate(pes)
          deallocate (activesendcounts, activeranks)

       enddo

       call MPI_GROUP_FREE (GROUP, STATUS)
       _VERIFY(STATUS)
       deallocate(var,msk)
       deallocate (r2g)
       deallocate(sendcounts)
       if(arrdes%readers_comm /= MPI_COMM_NULL) then
          deallocate(idx)
       end if
#else
!if USE_MAPL_ORIGINAL_TILE_HANDLING

       amIRoot = MAPL_am_i_root(layout)
       if (.not. MAPL_ShmInitialized) then
          if (amIRoot) then
             allocate(VR(IM_WORLD), stat=status)
             _VERIFY(STATUS)
          else
             allocate(VR(0), stat=status)
             _VERIFY(STATUS)
          end if
       else
          call MAPL_AllocNodeArray(vr,[IM_WORLD],_RC)
       end if

       if (amIRoot) then
          start(1) = 1
          start(2) = 1
          start(3) = 1
          if ( present(offset1) ) start(2) = offset1
          if ( present(offset2) ) start(3) = offset2
          start(4) = 1
          cnt(1) = im_world
          cnt(2) = 1
          cnt(3) = 1
          cnt(4) = 1

          call formatter%get_var(trim(name),vr,start=start,count=cnt,rc=status)
          if(status /= NF90_NOERR) then
             print*,'Error reading variable ',status
             print*, NF90_STRERROR(status)
             _VERIFY(STATUS)
          endif
       end if

       if (.not. MAPL_ShmInitialized) then
          call ArrayScatter(A, VR, arrdes%grid, mask=mask, rc=status)
          _VERIFY(STATUS)

          deallocate(VR)
       else
          call ArrayScatterShm(A, VR, arrdes%grid, mask=mask, rc=status)
          _VERIFY(STATUS)
          call MAPL_DeAllocNodeArray(VR,rc=STATUS)
          _VERIFY(STATUS)
       end if
#endif

    else

       start(1) = 1
       start(2) = 1
       start(3) = 1
       if ( present(offset1) ) start(2) = offset1
       if ( present(offset2) ) start(3) = offset2
       start(4) = 1
       cnt(1) = size(a)
       cnt(2) = 1
       cnt(3) = 1
       cnt(4) = 1

       if (present(layout) ) then
          if (MAPL_am_i_root(layout)) then
             call formatter%get_var(trim(name),A,start=start,count=cnt,rc=status)
             if(status /= NF90_NOERR) then
                print*,trim(IAm),'Error reading variable ',status
                print*, NF90_STRERROR(status)
                _VERIFY(STATUS)
             endif
          endif
          call MAPL_CommsBcast(layout, A, size(A), MAPL_Root, status)
          _VERIFY(STATUS)
       else
          call formatter%get_var(trim(name),A,start=start,count=cnt,rc=status)
          if(status /= NF90_NOERR) then
             print*,trim(IAm),'Error reading variable ',status
             print*, NF90_STRERROR(status)
             _VERIFY(STATUS)
          endif
       end if

    end if

    _RETURN(ESMF_SUCCESS)
  end subroutine MAPL_VarReadNCpar_R4_1d

  subroutine MAPL_VarReadNCpar_R8_1d(formatter, name, A, layout, ARRDES, MASK, offset1, offset2, RC)

    type(Netcdf4_Fileformatter)             , intent(IN   ) :: formatter
    character(len=*)              , intent(IN   ) :: name
    real(kind=ESMF_KIND_R8)       , intent(  OUT) :: A(:)
    type (ESMF_DELayout), optional, intent(IN   ) :: layout
    type(ArrDescr), optional,  intent(INOUT) :: ARRDES
    integer,           optional   , intent(IN   ) :: MASK(:)
    integer,           optional,    intent(IN   ) :: offset1
    integer,           optional,    intent(IN   ) :: offset2
    integer,           optional   , intent(  OUT) :: RC

! Local variables
    real(kind=ESMF_KIND_R8),  allocatable :: VAR(:)
    integer                               :: IM_WORLD
    integer                               :: status
    character(len=ESMF_MAXSTR)            :: IAm='MAPL_VarReadNCpar_R8_1d'
    integer, allocatable                  :: msk(:), sendcounts(:), displs(:)
    integer, allocatable                  :: idx(:)
    integer                               :: nrdrs, mype,  npes, recvcount
    integer                               :: mypeRd
    integer                               :: Rsize, first, last
    integer(KIND=MPI_OFFSET_KIND)         :: offset
    integer                               :: i, k, n, i1, in
    real(kind=ESMF_KIND_R8)               :: dummy
    integer                               :: group, newgroup
    integer                               :: thiscomm
    integer                               :: nactive
    integer                               :: ntransl
    integer, allocatable                  :: pes(:)
    integer, allocatable                  :: r2g(:)
    integer, allocatable                  :: rpes(:)
    integer, allocatable                  :: activeranks(:)
    integer, allocatable                  :: activesendcounts(:)
    integer                               :: start(4), cnt(4)

    if(present(mask) .and. present(layout) .and. present(arrdes) ) then

       IM_WORLD = arrdes%im_world

       call mpi_comm_size(arrdes%ioscattercomm,npes ,status)
       _VERIFY(STATUS)
       if(arrdes%readers_comm /= MPI_COMM_NULL) then
          call mpi_comm_rank(arrdes%readers_comm,mypeRd ,status)
          _VERIFY(STATUS)
          call mpi_comm_size(arrdes%readers_comm,nrdrs,status)
          _VERIFY(STATUS)
       else
          mypeRd = -1
       endif

       call MAPL_CommsBcast(layout, nrdrs, 1, 0, rc = status)
       _VERIFY(STATUS)
       Rsize = im_world/nrdrs + 1
       first = mypeRd*Rsize + 1
       if(mypeRd >=  mod(im_world,nrdrs)) then
          Rsize = Rsize - 1
          first = first - (mypeRd-mod(im_world,nrdrs))
       endif
       last  = first + Rsize - 1

#ifdef DEBUG_MPIIO
        if (mypeRd <= nrdrs-1) write(*,'(5i)') mypeRd, IM_WORLD, first, last, Rsize
#endif

       allocate(VAR(Rsize), stat=status)
       _VERIFY(STATUS)
       allocate(msk(Rsize), stat=status)
       _VERIFY(STATUS)
       allocate (sendcounts(0:npes-1), stat=status)
       _VERIFY(STATUS)
       allocate (r2g(0:nrdrs-1), stat=status)
       _VERIFY(STATUS)

       if(arrdes%readers_comm /= MPI_COMM_NULL) then
          start(1) = first
          start(2) = 1
          start(3) = 1
          if ( present(offset1) ) start(2) = offset1
          if ( present(offset2) ) start(3) = offset2
          start(4) = 1
          cnt(1) = Rsize
          cnt(2) = 1
          cnt(3) = 1
          cnt(4) = 1
!          print*,'start values are ',start
!          print*,'count values are ',count

          call formatter%get_var(trim(name),VAR,start=start,count=cnt,rc=status)
          if(status /= NF90_NOERR) then
             print*,'Error reading variable ',status
             print*, NF90_STRERROR(status)
             _VERIFY(STATUS)
          endif

          _ASSERT( (lbound(mask,1) <= first), 'out of bounds' )
          _ASSERT( (ubound(mask,1) >= last ), 'out of bounds' )
          msk = mask(first:last)

          allocate(idx(Rsize), stat=status)
          _VERIFY(STATUS)

          do i=1,Rsize
             idx(i) = i
          enddo
          msk = mask(first:last)
          call MAPL_Sort(msk,idx)
          msk = mask(first:last)
          call MAPL_Sort(msk,var)
       endif

       call mpi_comm_rank(arrdes%ioscattercomm,mype ,status)
       _VERIFY(STATUS)

       call MPI_COMM_GROUP (arrdes%ioscattercomm, GROUP, STATUS)
       _VERIFY(STATUS)

#if 1
       if (arrdes%readers_comm /= MPI_COMM_NULL) then
          allocate(rpes(0:nrdrs-1), stat=status)
          _VERIFY(STATUS)

          call MPI_COMM_GROUP (arrdes%readers_comm, NEWGROUP, STATUS)
          _VERIFY(STATUS)
          do n=0,nrdrs-1
             rpes(n) = n
          end do
          call MPI_Group_translate_ranks(newgroup, nrdrs, rpes, group, r2g, status)
          _VERIFY(STATUS)
          call MPI_GROUP_FREE (NEWGROUP, STATUS)
          _VERIFY(STATUS)
          deallocate(rpes)
       end if
       call MAPL_CommsBcast(layout, r2g, nrdrs, 0, rc = status)
       _VERIFY(STATUS)

#else
       do n=0,nrdrs-1
          r2g(n) = (npes/nrdrs)*n
       end do
#endif

       offset = 1

       do n=0,nrdrs-1

          Rsize = im_world/nrdrs + 1
          first = n*Rsize + 1
          if(n >=  mod(im_world,nrdrs)) then
             Rsize = Rsize - 1
             first = first - (n-mod(im_world,nrdrs))
          endif
          last  = first + Rsize - 1

          sendcounts = 0
          do i=first,last
             sendcounts(mask(i)) = sendcounts(mask(i)) + 1
          enddo

          ! Reader "n" must be included in the mpi group + evevybody that need the data
          nactive = count(sendcounts > 0)
          if (sendcounts(r2g(n)) == 0) then
             nactive = nactive + 1
          end if
          allocate (activeranks(0:nactive-1), activesendcounts(0:nactive-1), stat=status)
          _VERIFY(STATUS)
          allocate(pes(0:nactive-1), stat=status)
          _VERIFY(STATUS)
          allocate (displs(0:nactive), stat=status)
          _VERIFY(STATUS)
          k = 0
          do i=0, npes-1
             if (sendcounts(i) > 0) then
                pes(k) = i
                k = k+1
             end if
          enddo
          if (k /= nactive) then
             k = k+1
             _ASSERT(k == nactive, 'inconsistent nactive')
             _ASSERT(sendcounts(r2g(n)) == 0, 'sendcounts should be 0')
             pes(nactive-1) = r2g(n)
          end if
          call MPI_GROUP_INCL (GROUP, nactive, PES, newgroup, STATUS)
          _VERIFY(STATUS)
          call MPI_COMM_CREATE(arrdes%ioscattercomm, newgroup, thiscomm, STATUS)
          _VERIFY(STATUS)
          call MPI_Group_translate_ranks(group, nactive, pes, newgroup, activeranks, status)
          _VERIFY(STATUS)
          call MPI_GROUP_FREE (NEWGROUP, STATUS)
          _VERIFY(STATUS)

          if (thiscomm /= MPI_COMM_NULL) then
             activesendcounts = 0
             do i=0,nactive-1
                activesendcounts(activeranks(i)) = sendcounts(pes(i))
                if (pes(i) == r2g(n)) ntransl = activeranks(i)
             end do
             displs(0) = 0
             do i=1,nactive
                displs(i) = displs(i-1) + activesendcounts(i-1)
             enddo

             if(n==mypeRd) then
                do i=0,nactive-1
                   if(activesendcounts(i)>0) then
                      i1 = displs(i  ) + 1
                      in = displs(i+1)
                      call MAPL_Sort(idx(i1:in),var(i1:in))
                   endif
                end do
             endif

             recvcount = sendcounts(mype)

             if (recvcount == 0) then
                call MPI_SCATTERV( var, activesendcounts, displs, MPI_DOUBLE_PRECISION, &
                                   dummy,   recvcount,  MPI_DOUBLE_PRECISION, &
                                   ntransl, thiscomm,    status )
             else
                call MPI_SCATTERV( var, activesendcounts, displs, MPI_DOUBLE_PRECISION, &
                                   a(offset),   recvcount,  MPI_DOUBLE_PRECISION, &
                                   ntransl, thiscomm,    status )
             endif
             _VERIFY(STATUS)
             call MPI_Comm_Free(thiscomm, status)
             _VERIFY(STATUS)
             offset = offset + recvcount
          end if
          deallocate (displs)
          deallocate(pes)
          deallocate (activesendcounts, activeranks)

       enddo

       call MPI_GROUP_FREE (GROUP, STATUS)
       _VERIFY(STATUS)
       deallocate(var,msk)
       deallocate (r2g)
       deallocate(sendcounts)
       if(arrdes%readers_comm /= MPI_COMM_NULL) then
          deallocate(idx)
       end if

    else

       start(1) = 1
       start(2) = 1
       start(3) = 1
       if ( present(offset1) ) start(2) = offset1
       if ( present(offset2) ) start(3) = offset2
       start(4) = 1
       cnt(1) = size(a)
       cnt(2) = 1
       cnt(3) = 1
       cnt(4) = 1
       if (present(layout) ) then
          if (MAPL_am_i_root(layout)) then
             call formatter%get_var(trim(name),A,start=start,count=cnt,rc=status)
             if(status /= NF90_NOERR) then
                print*,trim(IAm),'Error reading variable ',status
                print*, NF90_STRERROR(status)
                _VERIFY(STATUS)
             endif
          endif
          call MAPL_CommsBcast(layout, A, size(A), MAPL_Root, status)
          _VERIFY(STATUS)
       else
          call formatter%get_var(trim(name),A,start=start,count=cnt,rc=status)
          if(status /= NF90_NOERR) then
             print*,trim(IAm),'Error reading variable ',status
             print*, NF90_STRERROR(status)
             _VERIFY(STATUS)
          endif
       end if

    end if

    _RETURN(ESMF_SUCCESS)
  end subroutine MAPL_VarReadNCpar_R8_1d

!---------------------------

  subroutine MAPL_VarWriteNCpar_R8_2d(formatter, name, A, ARRDES, lev, offset2, oClients, RC)

    type(Netcdf4_Fileformatter)           , intent(IN   ) :: formatter
    character(len=*)            , intent(IN   ) :: name
    real(kind=ESMF_KIND_R8)     , intent(IN   ) :: A(:,:)
    type(ArrDescr),    optional , intent(INOUT) :: ARRDES
    integer,           optional , intent(IN   ) :: lev
    integer,           optional , intent(IN   ) :: offset2
    type (ClientManager), optional, intent(inout) :: oClients
    integer,           optional , intent(  OUT) :: RC

! Local variables
    real(kind=ESMF_KIND_R8),  allocatable :: VAR(:,:)
    integer                               :: IM_WORLD
    integer                               :: JM_WORLD
    integer                               :: status

    real(kind=ESMF_KIND_R8),  allocatable :: recvbuf(:)
    integer                               :: I,J,N,K,L,myrow,myiorank,ndes_x
    integer                               :: start(4), cnt(4)
    integer                               :: jsize, jprev, num_io_rows
    integer, allocatable                  :: recvcounts(:), displs(:)
    type (ArrayReference) :: ref
    integer ::  i1, j1, in, jn,  global_dim(3)
    integer :: start_bound,end_bound,counts_per_writer
    logical :: in_bounds
    real(kind=ESMF_KIND_R8), pointer :: a_temp(:,:)
    character(len=:), allocatable :: writer_filename

    if (present(arrdes)) then
       if(present(oClients)) then
          if (arrdes%split_checkpoint) then
             call MAPL_GridGet(arrdes%grid,globalCellCountPerDim=global_dim,rc=status)
              _VERIFY(status)
             call MAPL_Grid_interior(arrdes%grid,i1,in,j1,jn)
             _ASSERT( i1 == arrdes%I1(arrdes%NX0), "interior starting i1 not match")
             _ASSERT( j1 == arrdes%j1(arrdes%NY0), "interior starting j1 not match")
             _ASSERT( size(a,1) == in-i1+1, "size not match")
             _ASSERT( size(a,2) == jn-j1+1, "size not match")
             counts_per_writer = global_dim(2)/arrdes%num_writers
             allocate(a_temp(0,0))
             do i=1,arrdes%num_writers
                start_bound = (i-1)*counts_per_writer+1
                end_bound   = i*counts_per_writer
                in_bounds = (j1 .ge. start_bound) .and. (jn .le. end_bound)
                if (in_bounds) then
                   ref = ArrayReference(A)
                else
                   ref = ArrayReference(a_temp)
                end if
                writer_filename = get_fname_by_rank(trim(arrdes%filename),i-1)
                call oClients%collective_stage_data(arrdes%collection_id(i),trim(writer_filename),trim(name), &
                            ref,start=[i1,j1-(i-1)*counts_per_writer], &
                            global_start=[1,1], global_count=[global_dim(1),global_dim(2)/arrdes%num_writers])
             enddo
             _RETURN(_SUCCESS)
          else
             call MAPL_GridGet(arrdes%grid,globalCellCountPerDim=global_dim,rc=status)
              _VERIFY(status)
             call MAPL_Grid_interior(arrdes%grid,i1,in,j1,jn)
             _ASSERT( i1 == arrdes%I1(arrdes%NX0), "interior starting i1 not match")
             _ASSERT( j1 == arrdes%j1(arrdes%NY0), "interior starting j1 not match")

             ref = ArrayReference(A)
             _ASSERT( size(a,1) == in-i1+1, "size not match")
             _ASSERT( size(a,2) == jn-j1+1, "size not match")
             call oClients%collective_stage_data(arrdes%collection_id(1),trim(arrdes%filename),trim(name), &
                         ref,start=[i1,j1], &
                         global_start=[1,1], global_count=[global_dim(1),global_dim(2)])
             _RETURN(_SUCCESS)
          end if
       end if
    endif
    if (present(arrdes)) then
       if(present(oClients)) then
          call MAPL_GridGet(arrdes%grid,globalCellCountPerDim=global_dim,rc=status)
           _VERIFY(status)
          call MAPL_Grid_interior(arrdes%grid,i1,in,j1,jn)
          _ASSERT( i1 == arrdes%I1(arrdes%NX0), "interior starting not match")
          _ASSERT( j1 == arrdes%j1(arrdes%NY0), "interior starting not match")
          ref = ArrayReference(A)
          _ASSERT( size(a,1) == in-i1+1, "size not match")
          _ASSERT( size(a,2) == jn-j1+1, "size not match")
          call oClients%collective_stage_data(arrdes%collection_id(1),trim(arrdes%filename),trim(name), &
                      ref,start=[i1,j1], &
                      global_start=[1,1], global_count=[global_dim(1),global_dim(2)])
          _RETURN(_SUCCESS)
       endif
    endif

    if (present(arrdes)) then

       IM_WORLD = arrdes%im_world
       JM_WORLD = arrdes%jm_world
       ndes_x = size(arrdes%in)

       call mpi_comm_rank(arrdes%ycomm,myrow,status)
       _VERIFY(STATUS)
       call mpi_comm_rank(arrdes%iogathercomm,myiorank,status)
       _VERIFY(STATUS)
       call mpi_comm_size(arrdes%iogathercomm,num_io_rows,status)
       _VERIFY(STATUS)
       num_io_rows=num_io_rows/ndes_x

       allocate (recvcounts(ndes_x*num_io_rows), displs(ndes_x*num_io_rows), stat=status)
       _VERIFY(STATUS)

       if(myiorank==0) then
          do j=1,num_io_rows
             jsize = arrdes%jn(myrow+j) - arrdes%j1(myrow+j) + 1
             recvcounts((j-1)*ndes_x+1:(j-1)*ndes_x+ndes_x) = ( arrdes%IN -  arrdes%I1 + 1) * jsize
          enddo

          displs(1) = 0
          do i=2,ndes_x*num_io_rows
             displs(i) = displs(i-1) + recvcounts(i-1)
          enddo

          jsize = 0
          do j=1,num_io_rows
             jsize=jsize + (arrdes%jn(myrow+j) - arrdes%j1(myrow+j) + 1)
          enddo
          allocate(VAR(IM_WORLD,jsize), stat=status)
          _VERIFY(STATUS)
          allocate(recvbuf(IM_WORLD*jsize), stat=status)
          _VERIFY(STATUS)
       end if

       if(myiorank/=0) then
          allocate(recvbuf(0), stat=status)
          _VERIFY(STATUS)
       endif

       call mpi_gatherv( a, size(a), MPI_DOUBLE_PRECISION, recvbuf, recvcounts, displs, &
                         MPI_DOUBLE_PRECISION, 0, arrdes%iogathercomm, status )
       _VERIFY(STATUS)

       if(myiorank==0) then

          jprev = 0
          k=1
          do l=1,num_io_rows
            jsize = arrdes%jn(myrow+l) - arrdes%j1(myrow+l) + 1
            do n=1,ndes_x
              do j=1,jsize
                do i=arrdes%i1(n),arrdes%in(n)
                  VAR(i,jprev+j) = recvbuf(k)
                  k=k+1
                end do
              end do
            end do
            jprev = jprev + jsize
          end do
          jsize=jprev

   ! lon, lat, lev, time
          start(1) = 1
          start(2) = arrdes%j1(myrow+1)
          start(3) = 1
          if (present(lev)) start(3) = lev
          start(4) = 1
          if (present(offset2)) start(4) = offset2
          cnt(1) = IM_WORLD
          cnt(2) = jsize
          cnt(3) = 1
          cnt(4) = 1

          if(arrdes%split_checkpoint) then
             start(2) = 1
          endif

          call formatter%put_var(trim(name),VAR,start=start,count=cnt,rc=status)
          if(status /= NF90_NOERR) then
             print*,'Error writing variable ',status
             print*, NF90_STRERROR(status)
             _VERIFY(STATUS)
          endif
          deallocate(VAR, stat=status)
          _VERIFY(STATUS)

       endif ! myiorank

       deallocate(recvbuf, stat=status)
       _VERIFY(STATUS)
       deallocate (recvcounts, displs, stat=status)
       _VERIFY(STATUS)

    else

       start(1) = 1
       start(2) = 1
       start(3) = 1
       if (present(lev)) start(3) = lev
       start(4) = 1
       cnt(1) = size(a,1)
       cnt(2) = size(a,2)
       cnt(3) = 1
       cnt(4) = 1

       call formatter%put_var(trim(name),A,start=start,count=cnt,rc=status)
       if(status /= NF90_NOERR) then
          print*,'Error writing variable ',status
          print*, NF90_STRERROR(status)
          _VERIFY(STATUS)
       endif

    end if

    _RETURN(ESMF_SUCCESS)
  end subroutine MAPL_VarWriteNCpar_R8_2d

!---------------------------

  subroutine MAPL_VarReadNCpar_R8_2d(formatter, name, A, ARRDES, lev, offset2, RC)

    type(Netcdf4_Fileformatter)           , intent(IN   ) :: formatter
    character(len=*)            , intent(IN   ) :: name
    real(kind=ESMF_KIND_R8)     , intent(INOUT) :: A(:,:)
    type(ArrDescr),    optional , intent(INOUT) :: ARRDES
    integer,           optional , intent(IN   ) :: lev
    integer,           optional , intent(IN   ) :: offset2
    integer,           optional , intent(  OUT) :: RC

! Local variables
    real(kind=ESMF_KIND_R8),  allocatable :: VAR(:,:)
    integer                               :: IM_WORLD
    integer                               :: JM_WORLD
    integer                               :: status

    real(kind=ESMF_KIND_R8),  allocatable :: buf(:)
    integer                               :: I,J,N,K,L,myrow,myiorank,ndes_x
    integer                               :: start(4), cnt(4)
    integer                               :: jsize, jprev, num_io_rows
    integer, allocatable                  :: sendcounts(:), displs(:)

    if (present(arrdes)) then

       ndes_x = size(arrdes%in)
       IM_WORLD = arrdes%im_world
       JM_WORLD = arrdes%jm_world

       call mpi_comm_rank(arrdes%ycomm,myrow,status)
       _VERIFY(STATUS)
       call mpi_comm_rank(arrdes%ioscattercomm,myiorank,status)
       _VERIFY(STATUS)
       call mpi_comm_size(arrdes%ioscattercomm,num_io_rows,status)
       _VERIFY(STATUS)
       num_io_rows=num_io_rows/ndes_x

       allocate (sendcounts(ndes_x*num_io_rows), displs(ndes_x*num_io_rows), stat=status)
       _VERIFY(STATUS)

       if(myiorank==0) then
          do j=1,num_io_rows
             jsize = arrdes%jn(myrow+j) - arrdes%j1(myrow+j) + 1
             sendcounts((j-1)*ndes_x+1:(j-1)*ndes_x+ndes_x) = ( arrdes%IN -  arrdes%I1 + 1) * jsize
          enddo

          displs(1) = 0
          do i=2,ndes_x*num_io_rows
             displs(i) = displs(i-1) + sendcounts(i-1)
          enddo

          jsize = 0
          do j=1,num_io_rows
             jsize=jsize + (arrdes%jn(myrow+j) - arrdes%j1(myrow+j) + 1)
          enddo
          allocate(VAR(IM_WORLD,jsize), stat=status)
          _VERIFY(STATUS)
          allocate(buf(IM_WORLD*jsize), stat=status)
          _VERIFY(STATUS)

          start(1) = 1
          if (arrdes%split_restart) then
             start(2) = 1
          else
             start(2) = arrdes%j1(myrow+1)
          end if
          start(3) = 1
          if (present(lev)) start(3)=lev
          start(4) = 1
          if (present(offset2)) start(4) = offset2
          cnt(1) = IM_WORLD
          cnt(2) = jsize
          cnt(3) = 1
          cnt(4) = 1

          if(arrdes%split_restart) then
             start(2) = 1
          endif

          call formatter%get_var(trim(name),VAR,start=start,count=cnt,rc=status)
          if(status /= NF90_NOERR) then
                  print*,'Error reading variable ',status
                  print*, NF90_STRERROR(status)
                  _VERIFY(STATUS)
          endif

          jprev = 0
          k=1
          do l=1,num_io_rows
             jsize = arrdes%jn(myrow+l) - arrdes%j1(myrow+l) + 1
             do n=1,ndes_x
               do j=1,jsize
                 do i=arrdes%i1(n),arrdes%in(n)
                   buf(k) = VAR(i,jprev+j)
                   k=k+1
                 end do
               end do
             end do
             jprev = jprev + jsize
          end do

          deallocate(VAR, stat=status)
          _VERIFY(STATUS)
       end if ! myiorank

       if(myiorank/=0) then
          allocate(buf(0), stat=status)
          _VERIFY(STATUS)
       endif

       call mpi_scatterv( buf, sendcounts, displs, MPI_DOUBLE_PRECISION, &
                 a,  size(a),  MPI_DOUBLE_PRECISION, &
                 0, arrdes%ioscattercomm, status )
       _VERIFY(STATUS)

       deallocate(buf, stat=status)
       _VERIFY(STATUS)
       deallocate (sendcounts, displs, stat=status)
       _VERIFY(STATUS)

    else

       start(1) = 1
       start(2) = 1
       start(3) = 1
       if (present(lev) ) start(3) = lev
       start(4) = 1
       if (present(offset2)) start(4) = offset2
       cnt(1) = size(a,1)
       cnt(2) = size(a,2)
       cnt(3) = 1
       cnt(4) = 1

       call formatter%get_var(trim(name),A,start=start,count=cnt,rc=status)
       if(status /= NF90_NOERR) then
               print*,'Error reading variable ',status
               print*, NF90_STRERROR(status)
               _VERIFY(STATUS)
       endif

    endif

  _RETURN(ESMF_SUCCESS)
  end subroutine MAPL_VarReadNCpar_R8_2d

!---------------------------



  subroutine MAPL_BundleReadNCPar(Bundle, arrdes, filename, rc)
    type(ESMF_FieldBundle), intent(inout)   :: Bundle
    type(ArrDescr), intent(inout)           :: arrdes
    character(len=*),   intent(in   )       :: filename
    integer, optional, intent(out)          :: rc


    integer                            :: nVars
    integer                            :: l
    type(ESMF_Field)                   :: field
    character(len=ESMF_MAXSTR)         :: FieldName
    integer                            :: info

    integer                            :: STATUS

    integer                            :: ind
    type(ESMF_Grid)                    :: grid

    integer                            :: MAPL_DIMS, reader_rank
    integer, pointer                   :: MASK(:) => null()
    type(Netcdf4_Fileformatter)        :: formatter
    type(FileMetaData)                 :: metadata
    character(len=:), allocatable      :: fname_by_rank
    logical :: grid_file_match,flip, restore_export, isPresent
    type(ESMF_VM) :: vm
    integer :: comm

    call ESMF_FieldBundleGet(Bundle,FieldCount=nVars,rc=STATUS)
    _VERIFY(STATUS)

    !open the file for parallel reading
    if (arrdes%readers_comm/=MPI_COMM_NULL) then
       call MPI_Info_create(info,STATUS)
       _VERIFY(STATUS)
       call MPI_Info_set(info,"romio_cb_read", trim(arrdes%romio_cb_read),STATUS)
       _VERIFY(STATUS)
       call MPI_Info_set(info,"cb_buffer_size", trim(arrdes%cb_buffer_size),STATUS)
       _VERIFY(STATUS)
       if (arrdes%num_readers == 1) then
          call formatter%open(filename,pFIO_READ,rc=status)
          _VERIFY(STATUS)
       else
          if(arrdes%split_restart .and. .not. arrdes%tile) then

             call MPI_COMM_RANK(arrdes%readers_comm,reader_rank,status)
             _VERIFY(STATUS)
             fname_by_rank = get_fname_by_rank(trim(filename),reader_rank)
             call formatter%open(trim(fname_by_rank),pFIO_READ,rc=status)
             _VERIFY(STATUS)
          else
             call formatter%open(filename,pFIO_READ,comm=arrdes%readers_comm,info=info,rc=status)
             _VERIFY(STATUS)
          endif
       end if
       metadata=formatter%read(rc=status)
       _VERIFY(status)
       call ESMF_FieldBundleGet(bundle,grid=grid,rc=status)
       _VERIFY(status)
       grid_file_match=compare_grid_file(metadata,grid,rc=status)
       _VERIFY(status)
       flip = check_flip(metadata,rc=status)
       _VERIFY(status)

       !_ASSERT(grid_file_match,"File grid dimensions in "//trim(filename)//" do not match grid")
    endif
    call ESMF_VMGetCurrent(vm,rc=status)
    _VERIFY(status)
    call ESMF_VMGet(vm,mpiCommunicator=comm,rc=status)
    _VERIFY(status)
    call MPI_BCast(flip,1,MPI_LOGICAL,0,comm,status)
    _VERIFY(status)

    do l=1,nVars
      call ESMF_FieldBundleGet(bundle, fieldIndex=l, field=field, rc=status)
      _VERIFY(STATUS)
      call ESMF_FieldGet(field,name=FieldName,rc=status)
      _VERIFY(STATUS)
! Check for old style aerosol names
      ind= index(FieldName, '::')
      if (ind> 0) then
        FieldName = trim(FieldName(ind+2:))
      end if

      if(.not.associated(MASK)) then
         call ESMF_AttributeGet(field, name='DIMS', value=MAPL_DIMS, rc=status)
         _VERIFY(STATUS)
         if (MAPL_DIMS == MAPL_DimsTileOnly .or. MAPL_DIMS == MAPL_DimsTileTile) then
            call ESMF_FieldGet   (field, grid=grid, rc=status)
            _VERIFY(STATUS)
            call MAPL_TileMaskGet(grid,  mask, rc=status)
            _VERIFY(STATUS)
!@         else
!@            allocate(Mask(1))
         endif
      endif

      restore_export = .false.
      call ESMF_AttributeGet(bundle, name='MAPL_RestoreExport', isPresent=isPresent, _RC)
      if (isPresent) then
         call ESMF_AttributeGet(bundle, name='MAPL_RestoreExport', value=restore_export, _RC)
      end if
      if (restore_export) then
         call MAPL_AllocateCoupling(field, _RC)
      end if

      call MAPL_FieldReadNCPar(formatter, FieldName, field, arrdes=arrdes, HomePE=mask, rc=status)
      _VERIFY(STATUS)
      if (flip) then
          call flip_field(field,rc=status)
          _VERIFY(status)
      end if

    enddo

    if(associated(MASK)) then
       DEALOC_(MASK)
    end if

    if (arrdes%readers_comm/=MPI_COMM_NULL) then
       call formatter%close()
       _VERIFY(STATUS)
       call MPI_Info_free(info, status)
       _VERIFY(STATUS)
    end if

    _RETURN(ESMF_SUCCESS)

  end subroutine MAPL_BundleReadNCPar

  function compare_grid_file(metadata,grid,rc) result(match)
     type(FileMetaData), intent(in) :: metadata
     type(ESMF_Grid), intent(in) :: grid
     integer, optional, intent(out) :: rc

     integer :: status
     logical :: match

     integer :: file_lev_size, file_lat_size, file_lon_size, file_tile_size
     integer :: grid_dims(3)

     match = .false.
     call MAPL_GridGet(grid,globalCellCountPerDim=grid_dims,rc=status)
     _VERIFY(status)
     file_lon_size = metadata%get_dimension("lon")
     file_lat_size = metadata%get_dimension("lat")
     if (metadata%has_attribute("Split_Cubed_Sphere")) file_lat_size = file_lat_size*6
     file_lev_size = metadata%get_dimension("lev")
     file_tile_size = metadata%get_dimension("tile")

     if (file_tile_size > 0) then
        match = (file_tile_size == grid_dims(1))
     else
        if (file_lev_size > 0) then

            match = (file_lon_size == grid_dims(1)) .and. (file_lat_size == grid_dims(2)) &
                    .and. (file_lev_size==grid_dims(3))
        else
            match = (file_lon_size == grid_dims(1)) .and. (file_lat_size == grid_dims(2))
        end if
     end if
     _RETURN(_SUCCESS)
  end function compare_grid_file

  subroutine MAPL_StateVarReadNCPar(filename, STATE, arrdes, bootstrapable, NAME, RC)
    character(len=*)            , intent(IN   ) :: filename
    type (ESMF_State)           , intent(INOUT) :: STATE
    type(ArrDescr)              , intent(INOUT) :: ARRDES
    logical                     , intent(IN   ) :: bootstrapable
    character(len=*),   optional, intent(IN   ) :: NAME
    integer,            optional, intent(  OUT) :: RC

! Local vars
    type (ESMF_FieldBundle)              :: bundle
    type (ESMF_Field)                    :: field
    integer                              :: status
    integer                              :: I, K
    integer                              :: J, ITEMCOUNT
    type (ESMF_StateItem_Flag), pointer  :: ITEMTYPES(:)
    character(len=ESMF_MAXSTR ), pointer :: ITEMNAMES(:)
    logical, pointer                     :: DOIT(:)

    integer                            :: ind
    logical                            :: skipReading
    integer                            :: RST
    character(len=ESMF_MAXSTR)         :: FieldName, BundleName

    type (ESMF_Field)                  :: new_field
    type (ESMF_FieldBundle)            :: bundle_read
    integer                            :: nBundle
    logical                            :: tile

    integer                            :: nVarFile, ncid
    character(len=ESMF_MAXSTR), allocatable :: VarNamesFile(:)
    type(ESMF_VM)                      :: VM
    logical                            :: foundInFile
    integer                            :: dna
    logical                            :: bootstrapable_
    logical                            :: isPresent
    logical                            :: is_test_framework, restore_export
    character(len=:), allocatable      :: fname_by_face
    ! get a list of variables in the file so we can skip if the
    ! variable in the state is not in the file and it is bootstrapable
    ! will just let root do this since everybody will need it
    ! and avoid complications with doing later on when only readers_comm has opened file

    call ESMF_VMGetCurrent(VM,rc=status)
    _VERIFY(STATUS)

    if (MAPL_AM_I_Root()) then
       if(arrdes%split_restart) then
          fname_by_face = get_fname_by_rank(filename, 1)
          status = NF90_OPEN(trim(fname_by_face),NF90_NOWRITE, ncid) ! just pick one
          _VERIFY(STATUS)
       else
          status = NF90_OPEN(trim(filename),NF90_NOWRITE, ncid)
          _VERIFY(STATUS)
       endif
       status = NF90_INQUIRE(ncid, nVariables=nVarFile)
       _VERIFY(STATUS)
    end if

    call MAPL_CommsBcast(vm, nVarFile, n=1, ROOT=MAPL_Root, rc=status)
    _VERIFY(STATUS)
    allocate(VarNamesFile(nVarFile),stat=status)
    _VERIFY(STATUS)

    if (MAPL_AM_I_Root()) then
       do i=1,nVarFile
          status = NF90_INQUIRE_VARIABLE(ncid, i, VarNamesFile(i))
          _VERIFY(STATUS)
       end do
       status = NF90_CLOSE(ncid)
       _VERIFY(STATUS)
    end if

    do i=1,nVarFile
       call MAPL_CommsBcast(vm, VarNamesFile(i), N=ESMF_MAXSTR, ROOT=MAPL_Root, rc=status)
       _VERIFY(STATUS)
    end do

    call ESMF_StateGet(STATE,ITEMCOUNT=ITEMCOUNT,RC=STATUS)
    _VERIFY(STATUS)

    _ASSERT(ITEMCOUNT>0, 'itemcount should be > 0')

    allocate(ITEMNAMES(ITEMCOUNT),STAT=STATUS)
    _VERIFY(STATUS)
    allocate(ITEMTYPES(ITEMCOUNT),STAT=STATUS)
    _VERIFY(STATUS)
    allocate(     DOIT(ITEMCOUNT),STAT=STATUS)
    _VERIFY(STATUS)

    call ESMF_StateGet(STATE,ITEMNAMELIST=ITEMNAMES,&
                       ITEMTYPELIST=ITEMTYPES,RC=STATUS)
    _VERIFY(STATUS)

    if(present(NAME)) then
       DOIT = ITEMNAMES==NAME
       _ASSERT(count(DOIT)/=0, 'count(DOIT) should not be 0')
    else
       DOIT = .true.
    endif

    bundle_read = ESMF_FieldBundleCreate(rc=STATUS)
    _VERIFY(STATUS)
    call ESMF_FieldBundleSet(bundle_read,grid=arrdes%grid,rc=STATUS)
    _VERIFY(STATUS)

    do I = 1, ITEMCOUNT

       if (DOIT(I)) then


          if (ITEMTYPES(I) == ESMF_StateItem_FieldBundle) then
             call ESMF_StateGet(state, itemnames(i), bundle, rc=status)
             _VERIFY(STATUS)

             skipReading = .false.
             call ESMF_AttributeGet(bundle, name='RESTART', isPresent=isPresent, rc=status)
             _VERIFY(STATUS)
             if (isPresent) then
                call ESMF_AttributeGet(bundle, name='RESTART', value=RST, rc=status)
                _VERIFY(STATUS)
             else
                RST = MAPL_RestartOptional
             end if
             skipReading = (RST == MAPL_RestartSkip .or.   &
                            RST == MAPL_RestartSkipInitial)

             call ESMF_AttributeGet(state, name='MAPL_TestFramework', isPresent=isPresent, _RC)
             if (isPresent) then
                call ESMF_AttributeGet(state, name='MAPL_TestFramework', value=is_test_framework, _RC)
                if (is_test_framework) skipReading = .false.
             end if

             if (skipReading) cycle
             bootstrapable_ = bootstrapable .and. (RST == MAPL_RestartOptional)

             call ESMF_FieldBundleGet(bundle, fieldCount=nBundle, rc=STATUS)
             _VERIFY(STATUS)
             call ESMF_FieldBundleGet(bundle, name=BundleName, rc=status)
             _VERIFY(STATUS)
             DO J = 1,nBundle
               call ESMF_FieldBundleGet(bundle, fieldIndex=J, field=field, rc=status)
               _VERIFY(STATUS)
               call ESMF_FieldGet(field,name=FieldName,rc=status)
               _VERIFY(STATUS)

               skipReading = .false.
               call ESMF_AttributeGet(field, name='RESTART', isPresent=isPresent, rc=status)
               _VERIFY(STATUS)
               if (isPresent) then
                  call ESMF_AttributeGet(field, name='RESTART', value=RST, rc=status)
                  _VERIFY(STATUS)
               else
                  RST = MAPL_RestartOptional
               end if
               skipReading = (RST == MAPL_RestartSkip)

               call ESMF_AttributeGet(state, name='MAPL_TestFramework', isPresent=isPresent, _RC)
               if (isPresent) then
                  call ESMF_AttributeGet(state, name='MAPL_TestFramework', value=is_test_framework, _RC)
                  if (is_test_framework) skipReading = .false.
               end if

               if (skipReading) cycle

               ind= index(FieldName, '::')
               if (ind> 0) then
                 FieldName = trim(FieldName(ind+2:))
               end if

               ! Tack on BundleName to distiguish duplicate FieldNames in different Bundles (PCHEM for instance)
               FieldName = trim(BundleName) //'_'// trim(FieldName)

               ! now check if the fieldname is in the list of available fields
               ! -------------------------------------------------------------
               foundInFile = .false.
               do k=1,nVarFile
                  if (trim(FieldName) == trim(VarNamesFile(k))) then
                     FoundInFile = .true.
                     exit
                  end if
               end do

               if (foundInFile) then
                  new_field = MAPL_FieldCreate(Field,FieldName,rc=status)
                  _VERIFY(STATUS)
                  call MAPL_FieldBundleAdd(bundle_read,new_field,rc=status)
                  _VERIFY(STATUS)
               else
                  if (bootStrapable_ .and. (RST == MAPL_RestartOptional)) then
                     call WRITE_PARALLEL("  Bootstrapping Variable: "//trim(FieldName)//" in "//trim(filename))
                     call ESMF_AttributeSet ( field, name='RESTART', &
                             value=MAPL_RestartBootstrap, rc=status)
                  else
                     restore_export = .false.
                     call ESMF_AttributeGet(state, name='MAPL_RestoreExport', isPresent=isPresent, _RC)
                     if (isPresent) then
                        call ESMF_AttributeGet(state, name='MAPL_RestoreExport', value=restore_export, _RC)
                     end if
                     if (restore_export) then
                        if (mapl_am_i_root()) print*, trim(fieldName), " not found in ", trim(filename), ". Skipping reading..."
                     else
                        _FAIL( "  Could not find field "//trim(FieldName)//" in "//trim(filename))
                     end if
                  end if
               end if

             ENDDO
          else if (ITEMTYPES(I) == ESMF_StateItem_Field) then
             call ESMF_StateGet(state, itemnames(i), field, rc=status)
             _VERIFY(STATUS)
             FieldName = trim(itemnames(i))

               ind= index(FieldName, '::')
               if (ind> 0) then
                 FieldName = trim(FieldName(ind+2:))
               end if

             skipReading = .false.
             call ESMF_AttributeGet(field, name='RESTART', isPresent=isPresent, rc=status)
             _VERIFY(STATUS)
             if (isPresent) then
                call ESMF_AttributeGet(field, name='RESTART', value=RST, rc=status)
                _VERIFY(STATUS)
             else
                RST = MAPL_RestartOptional
             end if
             skipReading = (RST == MAPL_RestartSkip)

             call ESMF_AttributeGet(state, name='MAPL_TestFramework', isPresent=isPresent, _RC)
             if (isPresent) then
                call ESMF_AttributeGet(state, name='MAPL_TestFramework', value=is_test_framework, _RC)
                if (is_test_framework) skipReading = .false.
             end if

             if (skipReading) cycle
             call ESMF_AttributeGet(field, name='doNotAllocate', isPresent=isPresent, rc=status)
             _VERIFY(STATUS)
             if (isPresent) then
                call ESMF_AttributeGet(field, name='doNotAllocate', value=DNA, rc=status)
                _VERIFY(STATUS)
                skipReading = (DNA /= 0)
             end if

             call ESMF_AttributeGet(state, name='MAPL_TestFramework', isPresent=isPresent, _RC)
             if (isPresent) then
                call ESMF_AttributeGet(state, name='MAPL_TestFramework', value=is_test_framework, _RC)
                if (is_test_framework) skipReading = .false.
             end if

             if (skipReading) cycle

             ! now check if the field is in the list of available fields
             ! ---------------------------------------------------------
             foundInFile = .false.
             do k=1,nVarFile
                if (trim(Fieldname) == trim(VarNamesFile(k))) then
                   FoundInFile = .true.
                   exit
                end if
             end do

             if (foundInFile) then
                call MAPL_FieldBundleAdd(bundle_read,field,rc=status)
                _VERIFY(STATUS)
             else
                if (bootStrapable .and. (RST == MAPL_RestartOptional)) then
                    call WRITE_PARALLEL("  Bootstrapping Variable: "//trim(FieldName)//" in "//trim(filename))
                    call ESMF_AttributeSet ( field, name='RESTART', &
                            value=MAPL_RestartBootstrap, rc=status)
                else
                   restore_export = .false.
                   call ESMF_AttributeGet(state, name='MAPL_RestoreExport', isPresent=isPresent, _RC)
                   if (isPresent) then
                      call ESMF_AttributeGet(state, name='MAPL_RestoreExport', value=restore_export, _RC)
                   end if
                   if (restore_export) then
                      if (mapl_am_i_root()) print*, trim(fieldName), " not found in ", trim(filename), ". Skipping reading..."
                   else
                      _FAIL( "  Could not find field "//trim(FieldName)//" in "//trim(filename))
                   end if
                end if
             end if

          end if

       end if

    end do

    tile = arrdes%tile

    call ESMF_AttributeGet(state, name='MAPL_RestoreExport', isPresent=isPresent, _RC)
    if (isPresent) then
       call ESMF_AttributeGet(state, name='MAPL_RestoreExport', value=restore_export, _RC)
       call ESMF_AttributeSet(bundle_read, name="MAPL_RestoreExport", value=restore_export, _RC)
    end if
    call MAPL_VarReadNCPar(Bundle_Read, arrdes, filename, rc=status)
    _VERIFY(STATUS)

    deallocate(ITEMNAMES)
    deallocate(ITEMTYPES)
    deallocate(     DOIT)
    deallocate(VarNamesFile)

    _RETURN(ESMF_SUCCESS)

  end subroutine MAPL_StateVarReadNCPar

  subroutine MAPL_ArrayReadNCpar_1d(varn,filename,farrayPtr,arrDes,rc)
  character(len=*),      intent(IN   )  :: varn
  character(len=*),      intent(IN   )  :: filename
  real, pointer                         :: farrayPtr(:)
  type(arrDescr),        intent(INOUT)  :: arrDes
  integer, optional,     intent(OUT  )  :: rc

  character(len=*), parameter           :: Iam="MAPL_ArrayReadNCpar_1d"
  integer                               :: status
  type(ESMF_Field)                      :: field
  type(ESMF_FieldBundle)                :: bundle

  FIELD = ESMF_FieldCreate(grid=arrDes%grid, datacopyflag=ESMF_DATACOPY_VALUE, &
         farrayPtr=farrayPtr, name=trim(varn), RC=STATUS)
  _VERIFY(STATUS)
  if (arrDes%tile) then
     call ESMF_AttributeSet(field,name='DIMS',value=MAPL_DimsTileOnly,rc=status)
     _VERIFY(STATUS)
  endif
  BUNDLE =  ESMF_FieldBundleCreate ( name=Iam, rc=STATUS )
  _VERIFY(STATUS)
  call ESMF_FieldBundleSet ( bundle, grid=arrDes%grid, rc=STATUS )
  _VERIFY(STATUS)
  call MAPL_FieldBundleAdd(BUNDLE, FIELD, rc=STATUS)
  _VERIFY(STATUS)

  call MAPL_VarReadNCPar(Bundle, arrdes, filename, rc=status)
  _VERIFY(STATUS)

  call ESMF_FieldBundleDestroy(bundle,rc=status)
  _VERIFY(STATUS)
  call ESMF_FieldDestroy(field,rc=status)
  _VERIFY(STATUS)

  _RETURN(ESMF_SUCCESS)
  end subroutine MAPL_ArrayReadNCpar_1d

  subroutine MAPL_ArrayReadNCpar_2d(varn,filename,farrayPtr,arrDes,rc)
  character(len=*),      intent(IN   )  :: varn
  character(len=*),      intent(IN   )  :: filename
  real, pointer                         :: farrayPtr(:,:)
  type(arrDescr),        intent(INOUT)  :: arrDes
  integer, optional,     intent(OUT  )  :: rc

  character(len=*), parameter           :: Iam="MAPL_ArrayReadNCpar_2d"
  integer                               :: status
  type(ESMF_Field)                      :: field
  type(ESMF_FieldBundle)                :: bundle

  FIELD = ESMF_FieldCreate(grid=arrDes%grid, datacopyflag=ESMF_DATACOPY_VALUE, &
         farrayPtr=farrayPtr, name=trim(varn), RC=STATUS)
  _VERIFY(STATUS)
  if (arrDes%tile) then
     call ESMF_AttributeSet(field,name='DIMS',value=MAPL_DimsTileTile,rc=status)
     _VERIFY(STATUS)
  else
     call ESMF_AttributeSet(field,name='DIMS',value=MAPL_DimsHorzOnly,rc=status)
     _VERIFY(STATUS)
  endif
  BUNDLE =  ESMF_FieldBundleCreate ( name=Iam, rc=STATUS )
  _VERIFY(STATUS)
  call ESMF_FieldBundleSet ( bundle, grid=arrDes%grid, rc=STATUS )
  _VERIFY(STATUS)
  call MAPL_FieldBundleAdd(BUNDLE, FIELD, rc=STATUS)
  _VERIFY(STATUS)

  call MAPL_VarReadNCPar(Bundle, arrdes, filename, rc=status)
  _VERIFY(STATUS)

  call ESMF_FieldBundleDestroy(bundle,rc=status)
  _VERIFY(STATUS)
  call ESMF_FieldDestroy(field,rc=status)
  _VERIFY(STATUS)

  _RETURN(ESMF_SUCCESS)
  end subroutine MAPL_ArrayReadNCpar_2d

  subroutine MAPL_ArrayReadNCpar_3d(varn,filename,farrayPtr,arrDes,rc)
  character(len=*),      intent(IN   )  :: varn
  character(len=*),      intent(IN   )  :: filename
  real, pointer                         :: farrayPtr(:,:,:)
  type(arrDescr),        intent(INOUT)  :: arrDes
  integer, optional,     intent(OUT  )  :: rc

  character(len=*), parameter           :: Iam="MAPL_ArrayReadNCpar_3d"
  integer                               :: status
  type(ESMF_Field)                      :: field
  type(ESMF_FieldBundle)                :: bundle

  FIELD = ESMF_FieldCreate(grid=arrDes%grid, datacopyflag=ESMF_DATACOPY_VALUE, &
         farrayPtr=farrayPtr, name=trim(varn), RC=STATUS)
  _VERIFY(STATUS)
  call ESMF_AttributeSet(field,name='DIMS',value=MAPL_DimsHorzVert,rc=status)
  _VERIFY(STATUS)
  BUNDLE =  ESMF_FieldBundleCreate ( name=Iam, rc=STATUS )
  _VERIFY(STATUS)
  call ESMF_FieldBundleSet ( bundle, grid=arrDes%grid, rc=STATUS )
  _VERIFY(STATUS)
  call MAPL_FieldBundleAdd(BUNDLE, FIELD, rc=STATUS)
  _VERIFY(STATUS)

  call MAPL_VarReadNCPar(Bundle, arrdes, filename, rc=status)
  _VERIFY(STATUS)

  call ESMF_FieldBundleDestroy(bundle,rc=status)
  _VERIFY(STATUS)
  call ESMF_FieldDestroy(field,rc=status)
  _VERIFY(STATUS)

  _RETURN(ESMF_SUCCESS)
  end subroutine MAPL_ArrayReadNCpar_3d

  subroutine MAPL_BundleWriteNCPar(Bundle, arrdes, CLOCK, filename, clobber, oClients, rc)
    type(ESMF_FieldBundle), intent(inout)   :: Bundle
    type(ArrDescr), intent(inout)           :: arrdes
    type(ESMF_Clock), intent(in)            :: CLOCK
    character(len=*), intent(in  )         :: filename
    logical, intent(in)                    :: clobber
    type (ClientManager), optional, intent(inout) :: oClients
    integer, optional, intent(out)          :: rc


    integer                            :: nVars, ndims
    integer                            :: i,j,l
    type(ESMF_Field)                   :: field
    type(ESMF_Array)                   :: array
    type(ESMF_Grid)                    :: grid
    character(len=ESMF_MAXSTR)         :: FieldName
    type(ESMF_Time)                       :: currentTime
    character(len=ESMF_MAXSTR)            :: TimeString, TimeUnits

    type(ESMF_TypeKind_Flag)              :: tk
    integer                               :: ind
    logical                               :: Have_HorzOnly, Have_HorzVert, Have_VertOnly, Have_TileOnly
    logical                               :: Have_TileTile, Have_VLocationCenter, Have_VLocationEdge
    real(KIND=REAL64),  allocatable :: lon(:), lat(:), lev(:), edges(:)
    integer, allocatable                  :: LOCATION(:), DIMS(:), UNGRID_DIMS(:,:)
    integer, allocatable                  :: UNIQUE_UNGRID_DIMS(:), ungriddim(:)
    real(KIND=REAL64)                     :: x0,x1
    integer                               :: arrayRank, KM_WORLD, DataType
    integer                               :: ungrid_dim_max_size, n_unique_ungrid_dims
    character(len=ESMF_MAXSTR)            :: ungrid_dim_name
    character(len=ESMF_MAXSTR), allocatable :: unique_ungrid_dim_name(:)
    character(len=ESMF_MAXSTR)            :: myUngridDimName1, myUngridDimName2
    character(len=ESMF_MAXSTR)            :: BundleName
    real(KIND=ESMF_KIND_R4), pointer, dimension(:,:,:,:):: var_4d => null()
    real(KIND=ESMF_KIND_R8), pointer, dimension(:,:,:,:):: var8_4d => null()
    real(KIND=ESMF_KIND_R4), pointer, dimension(:,:,:)  :: var_3d => null()
    real(KIND=ESMF_KIND_R8), pointer, dimension(:,:,:)  :: var8_3d => null()
    real(KIND=ESMF_KIND_R4), pointer, dimension(:,:)    :: var_2d => null()
    real(KIND=ESMF_KIND_R8), pointer, dimension(:,:)    :: var8_2d => null()
    real(KIND=ESMF_KIND_R4), pointer, dimension(:)      :: var_1d => null()
    real(KIND=ESMF_KIND_R8), pointer, dimension(:)      :: var8_1d => null()
    character(len=ESMF_MAXSTR )           :: LONG_NAME, UNITS
    character(100) :: buffer
    integer                               :: info

    integer                               :: MAPL_DIMS
    integer                               :: JM_WORLD
    integer, pointer                      :: MASK(:) => null()
    logical                               :: isCubed
    logical                               :: found
    logical                               :: isPresent

    type(Netcdf4_Fileformatter)           :: formatter
    type(FileMetadata) :: cf
    class (Variable), allocatable :: var
    class(*), allocatable :: coordinate_data(:)
    integer :: pfDataType, writer_rank
    character(len=:), allocatable         :: fname_by_writer

    integer                            :: STATUS
    type (StringIntegerMap), save      :: RstCollections
    type (StringIntegerMapIterator)    :: iter
    type (StringVariableMap) :: var_map
    logical :: have_target_lon, have_target_lat, have_stretch_factor
    real :: target_lon, target_lat, stretch_factor
    logical :: is_stretched
    character(len=ESMF_MAXSTR) :: positive
    type(StringVector) :: flip_vars
    type(ESMF_Field) :: lons_field, lats_field
    logical :: isGridCapture, have_oclients
    real(kind=ESMF_KIND_R8), pointer :: grid_lons(:,:), grid_lats(:,:), lons_field_ptr(:,:), lats_field_ptr(:,:)
    integer :: pfio_mode

    have_oclients = present(oClients)

    call ESMF_FieldBundleGet(Bundle,FieldCount=nVars, name=BundleName, rc=STATUS)
    _VERIFY(STATUS)

    call ESMF_AttributeGet(arrdes%grid,name="TARGET_LON",isPresent=have_target_lon,rc=status)
    _VERIFY(status)
    call ESMF_AttributeGet(arrdes%grid,name="TARGET_LAT",isPresent=have_target_lat,rc=status)
    _VERIFY(status)
    call ESMF_AttributeGet(arrdes%grid,name="STRETCH_FACTOR",isPresent=have_stretch_factor,rc=status)
    _VERIFY(status)
    if (have_target_lon .and. have_target_lat .and. have_stretch_factor) then
       is_stretched = .true.
       call ESMF_AttributeGet(arrdes%grid,name="TARGET_LON",value=target_lon,rc=status)
       _VERIFY(status)
       call ESMF_AttributeGet(arrdes%grid,name="TARGET_LAT",value=target_lat,rc=status)
       _VERIFY(status)
       call ESMF_AttributeGet(arrdes%grid,name="STRETCH_FACTOR",value=stretch_factor,rc=status)
       _VERIFY(status)
    else
       is_stretched = .false.
    end if


    ! verify that file is compatible with fields in bundle we are reading

    if (nVars == 0) then
       _FAIL( "The bundle you are trying to write is empty")
    endif

    ! first we need to prep the netcdf file for writing
    allocate(LOCATION(nVars), stat=STATUS)
    _VERIFY(STATUS)
    allocate(DIMS(nVars), stat=STATUS)
    _VERIFY(STATUS)

    allocate(UNGRID_DIMS(nVars,2),stat=STATUS)
    _VERIFY(STATUS)
    UNGRID_DIMS = 0

    ! now determine the dimensionality and vertical structure of each field
    JM_WORLD=1
    DO I = 1, nVars

       call ESMF_FieldBundleGet(Bundle,fieldIndex=I, field=field, rc=status)
       _VERIFY(STATUS)
       call ESMF_AttributeGet(field, NAME='DIMS', VALUE=DIMS(I), rc=status)
       _VERIFY(STATUS)
       call ESMF_AttributeGet(field, NAME='VLOCATION', VALUE=LOCATION(I), rc=status)
       _VERIFY(STATUS)

       ! now check if we have an ungridded dimension
       call ESMF_FieldGet(field,array=array,rc=status)
       _VERIFY(STATUS)
       call ESMF_ArrayGet(array, typekind=tk, rank=arrayRank,  RC=STATUS)
       _VERIFY(STATUS)
       if (arrayRank == 3 .and. DIMS(I) == MAPL_DimsHorzOnly) then
          if (tk == ESMF_TYPEKIND_R4) then
             call ESMF_ArrayGet(array, localDE=0, farrayptr=var_3d, rc=status)
             _VERIFY(STATUS)
             UNGRID_DIMS(I,1) = size(var_3d,3)
          elseif (tk == ESMF_TYPEKIND_R8) then
             call ESMF_ArrayGet(array, localDE=0, farrayptr=var8_3d, rc=status)
             _VERIFY(STATUS)
             UNGRID_DIMS(I,1) = size(var8_3d,3)
          endif
       else if (arrayRank == 2 .and. DIMS(I) == MAPL_DimsTileOnly) then
          if (tk == ESMF_TYPEKIND_R4) then
             call ESMF_ArrayGet(array, localDE=0, farrayptr=var_2d, rc=status)
             _VERIFY(STATUS)
             UNGRID_DIMS(I,1) = size(var_2d,2)
          elseif (tk == ESMF_TYPEKIND_R8) then
             call ESMF_ArrayGet(array, localDE=0, farrayptr=var8_2d, rc=status)
             _VERIFY(STATUS)
             UNGRID_DIMS(I,1) = size(var8_2d,2)
          endif
       else if (arrayRank == 2 .and. DIMS(I) == MAPL_DimsTileTile) then
          if (tk == ESMF_TYPEKIND_R4) then
             call ESMF_ArrayGet(array, localDE=0, farrayptr=var_2d, rc=status)
             _VERIFY(STATUS)
             JM_WORLD = max(JM_WORLD,size(var_2d,2))
          elseif (tk == ESMF_TYPEKIND_R8) then
             call ESMF_ArrayGet(array, localDE=0, farrayptr=var8_2d, rc=status)
             _VERIFY(STATUS)
             JM_WORLD = max(JM_WORLD,size(var_2d,2))
          endif
       else if (arrayRank == 1 .and. DIMS(I) == MAPL_DimsNone) then
          if (tk == ESMF_TYPEKIND_R4) then
             call ESMF_ArrayGet(array, localDE=0, farrayptr=var_1d, rc=status)
             _VERIFY(STATUS)
             UNGRID_DIMS(I,1) = size(var_1d)
          elseif (tk == ESMF_TYPEKIND_R8) then
             call ESMF_ArrayGet(array, localDE=0, farrayptr=var8_1d, rc=status)
             _VERIFY(STATUS)
             UNGRID_DIMS(I,1) = size(var8_1d)
          endif
       else if (arrayRank == 3 .and. DIMS(I) == MAPL_DimsTileOnly) then
          if (tk == ESMF_TYPEKIND_R4) then
             call ESMF_ArrayGet(array, localDE=0, farrayptr=var_3d, rc=status)
             _VERIFY(STATUS)
             UNGRID_DIMS(I,1) = size(var_3d,2)
             UNGRID_DIMS(I,2) = size(var_3d,3)
          elseif (tk == ESMF_TYPEKIND_R8) then
             call ESMF_ArrayGet(array, localDE=0, farrayptr=var8_3d, rc=status)
             _VERIFY(STATUS)
             UNGRID_DIMS(I,1) = size(var8_3d,2)
             UNGRID_DIMS(I,2) = size(var8_3d,3)
          endif
       else if (arrayRank == 4) then
          if (tk == ESMF_TYPEKIND_R4) then
             call ESMF_ArrayGet(array, localDE=0, farrayptr=var_4d, rc=status)
             _VERIFY(STATUS)
             if (DIMS(I) == MAPL_DimsHorzVert) then
                UNGRID_DIMS(I,1) = size(var_4d,4)
             else if (DIMS(I) == MAPL_DimsHorzOnly) then
                UNGRID_DIMS(I,1) = size(var_4d,3)
                UNGRID_DIMS(I,2) = size(var_4d,4)
             else
                _FAIL( "Unsupported DIMS type")
             end if
          elseif (tk == ESMF_TYPEKIND_R8) then
             call ESMF_ArrayGet(array, localDE=0, farrayptr=var8_4d, rc=status)
             _VERIFY(STATUS)
             if (DIMS(I) == MAPL_DimsHorzVert) then
                UNGRID_DIMS(I,1) = size(var8_4d,4)
             else if (DIMS(I) == MAPL_DimsHorzOnly) then
                UNGRID_DIMS(I,1) = size(var8_4d,3)
                UNGRID_DIMS(I,2) = size(var8_4d,4)
             else
                _FAIL( "Unsupported DIMS type")
             end if
          else
             _FAIL( "Unsupported type/rank")
          endif
       endif

    ENDDO

    Have_HorzOnly = any(DIMS==MAPL_DimsHorzOnly)
    Have_HorzVert = any(DIMS==MAPL_DimsHorzVert)
    Have_VertOnly = any(DIMS==MAPL_DimsVertOnly)
    Have_TileOnly = any(DIMS==MAPL_DimsTileOnly)
    Have_TileTile = any(DIMS==MAPL_DimsTileTile)
    Have_VLocationCenter = any(LOCATION==MAPL_VLocationCenter)
    Have_VLocationEdge   = any(LOCATION==MAPL_VLocationEdge)

    ungrid_dim_max_size = maxval(UNGRID_DIMS)

    n_unique_ungrid_dims = 0
    if (ungrid_dim_max_size /= 0) then

       n_unique_ungrid_dims = 0
       do i = 1,ungrid_dim_max_size
          if (any(ungrid_dims == i)) n_unique_ungrid_dims = n_unique_ungrid_dims + 1
       end do

       allocate(unique_ungrid_dims(n_unique_ungrid_dims),stat=status)
       _VERIFY(STATUS)
       allocate(unique_ungrid_dim_name(n_unique_ungrid_dims),stat=status)
       _VERIFY(STATUS)
       allocate(ungriddim(n_unique_ungrid_dims),stat=status)
       _VERIFY(STATUS)

       n_unique_ungrid_dims = 0
       do i = 1,ungrid_dim_max_size
          if (any(ungrid_dims == i)) then
             n_unique_ungrid_dims = n_unique_ungrid_dims + 1
             unique_ungrid_dims(n_unique_ungrid_dims) = i
          end if
       end do

    endif

    deallocate(DIMS)
    deallocate(LOCATION)

    if (Have_TileTile) then
       call ArrDescrSet(arrdes, JM_WORLD=JM_WORLD)
    end if

    call ESMF_AttributeGet(bundle,"POSITIVE",positive,rc=status)
    _VERIFY(status)
    ! count dimensions for NCIO
    ndims = 0
    if (Have_HorzVert .or. Have_HorzOnly) ndims = ndims + 2
    if (Have_VLocationCenter) ndims = ndims + 1
    if (Have_VLocationEdge) ndims = ndims + 1
    if (Have_TileOnly .or. Have_TileTile) then
        ndims = ndims + 1
        if (Have_TileTile) ndims = ndims + 1
    end if
    ndims = ndims + n_unique_ungrid_dims
    ! add 1 for time
    ndims = ndims + 1

    !WJ note: if arrdes%write_restart_by_oserver is true, all processors will participate
    !if (arrdes%writers_comm/=MPI_COMM_NULL .or. have_oclients ) then !bmaa

       ! Create dimensions as needed
       if (Have_HorzVert .or. Have_HorzOnly) then

          if (arrdes%IM_WORLD*6 == arrdes%JM_WORLD) then
             isCubed = .true.
             x0=1.0d0
             x1=dble(arrdes%IM_WORLD)
             if (is_stretched) then
                call cf%add_attribute('TARGET_LON',target_lon)
                call cf%add_attribute('TARGET_LAT',target_lat)
                call cf%add_attribute('STRETCH_FACTOR',stretch_factor)
             end if
          else
             isCubed = .false.
             x0=-180.0d0
             x1=180.0d0-360.d0/dble(arrdes%IM_WORLD)
          endif
          lon = MAPL_Range(x0,x1,arrdes%IM_WORLD)

          call cf%add_dimension('lon',arrdes%im_world,rc=status)
          _VERIFY(status)
          allocate(coordinate_data,source=lon)
          allocate(var,source=CoordinateVariable(Variable(type=pFIO_REAL64,dimensions='lon'),coordinate_data))
          call var%add_attribute('units','degrees_east')
          call var%add_attribute('long_name','Longitude')
          call cf%add_variable('lon',var,rc=status)
          _VERIFY(status)
          deallocate(var,coordinate_data)

          if (isCubed) then
             x0=1.0d0
             if (arrdes%split_checkpoint) then
                x1 = dble(arrdes%jm_world/arrdes%num_writers)
             else
                x1=dble(arrdes%JM_WORLD)
             end if
          else
             if (arrdes%jm_world==1) then
                x0=0.0
                x1=0.0
             else
                x0=-90.0d0
                x1=90.0d0
             end if
          endif
          if (arrdes%split_checkpoint) then
             lat = MAPL_Range(x0,x1,arrdes%JM_WORLD/arrdes%num_writers)
             call cf%add_dimension('lat',arrdes%jm_world/arrdes%num_writers,rc=status)
             _VERIFY(status)
             allocate(coordinate_data,source=lat)
             allocate(var,source=CoordinateVariable(Variable(type=pFIO_REAL64,dimensions='lat'),coordinate_data))
          else
             lat = MAPL_Range(x0,x1,arrdes%JM_WORLD)
             call cf%add_dimension('lat',arrdes%jm_world,rc=status)
             _VERIFY(status)
             allocate(coordinate_data,source=lat)
             allocate(var,source=CoordinateVariable(Variable(type=pFIO_REAL64,dimensions='lat'),coordinate_data))
          endif

          call var%add_attribute('units','degrees_north')
          call var%add_attribute('long_name','Latitude')
          call cf%add_variable('lat',var,rc=status)
          _VERIFY(status)
          deallocate(var,coordinate_data)

       endif

       if (Have_HorzVert .or. Have_VertOnly) then
          if (Have_VLocationCenter) then
             ! Level variable
             KM_World = arrdes%lm_World
             allocate(lev(KM_WORLD))
             lev = (/(L, L=1,KM_WORLD)/)

             call cf%add_dimension('lev',km_world,rc=status)
             _VERIFY(status)
             allocate(coordinate_data,source=lev)
             allocate(var,source=CoordinateVariable(Variable(type=pFIO_REAL64,dimensions='lev'),coordinate_data))
             call var%add_attribute('units','layer')
             call var%add_attribute('long_name','sigma at layer midpoints')
             call var%add_attribute('standard_name','atmosphere_hybrid_sigma_pressure_coordinate')
             call var%add_attribute('positive',trim(positive))
             call var%add_attribute('coordinate','eta')
             call var%add_attribute('formulaTerms','ap: ak b: bk ps: ps p0: p00')
             call cf%add_variable('lev',var,rc=status)
             _VERIFY(status)
             deallocate(var,coordinate_data)

             deallocate(lev)
          endif
          if (Have_VLocationEdge) then
             ! Edges variable
             KM_World = arrdes%lm_World
             allocate(edges(KM_WORLD+1))
             edges = (/(L, L=1,KM_WORLD+1)/)

             call cf%add_dimension('edge',km_world+1,rc=status)
             _VERIFY(status)
             allocate(coordinate_data,source=edges)
             allocate(var,source=CoordinateVariable(Variable(type=pFIO_REAL64,dimensions='edge'),coordinate_data))
             call var%add_attribute('units','level')
             call var%add_attribute('long_name','sigma at layer edges')
             call var%add_attribute('standard_name','atmosphere_hybrid_sigma_pressure_coordinate')
             call var%add_attribute('positive',trim(positive))
             call var%add_attribute('coordinate','eta')
             call var%add_attribute('formulaTerms','ap: ak b: bk ps: ps p0: p00')
             call cf%add_variable('edge',var,rc=status)
             _VERIFY(status)
             deallocate(var,coordinate_data)

             deallocate(edges)
          endif
       endif

       if (Have_TileOnly .or. Have_TileTile) then
          call cf%add_dimension('tile',arrdes%im_world,rc=status)
          _VERIFY(status)
          if(Have_TileTile) then
            call cf%add_dimension('subtile',arrdes%jm_world,rc=status)
            _VERIFY(status)
          endif
       endif

       if (ungrid_dim_max_size /=0) then
          do i=1,n_unique_ungrid_dims
             if (i < 10) then
                write(ungrid_dim_name, '(A11,I1)')"unknown_dim",i
             else if (i > 9 .and. i < 100) then
                write(ungrid_dim_name, '(A11,I2)')"unknown_dim",i
             else if (i > 99 .and. i < 1000) then
                write(ungrid_dim_name, '(A11,I3)')"unknown_dim",i
             end if
             unique_ungrid_dim_name(i)=ungrid_dim_name
             call cf%add_dimension(trim(ungrid_dim_name),unique_ungrid_dims(i),rc=status)
             _VERIFY(status)
          end do
       endif

       ! Time variable
       call ESMF_ClockGet ( clock,  currTime=CurrentTime ,rc=STATUS )
       _VERIFY(STATUS)
       call ESMF_TimeGet  ( CurrentTime, timeString=TimeString, rc=status )
       _VERIFY(STATUS)


       TimeUnits = "minutes since "//timestring( 1: 10)//" "//timestring(12:19)

       call cf%add_dimension('time',1,rc=status)
       _VERIFY(status)
       allocate(coordinate_data,source=(/0.d0/))
       allocate(var,source=CoordinateVariable(Variable(type=pFIO_REAL64,dimensions='time'),coordinate_data))
       call var%add_attribute('units',trim(timeUnits))
       call cf%add_variable('time',var,rc=status)
       _VERIFY(status)
       call var_map%insert('time', var)
       deallocate(var,coordinate_data)

       allocate(DIMS(1), stat=STATUS)
       _VERIFY(STATUS)
       allocate(LOCATION(1), stat=STATUS)
       _VERIFY(STATUS)

       do i=1,nVars
          call ESMF_FieldBundleGet(Bundle,fieldIndex=I, field=field, rc=status)
          _VERIFY(STATUS)
          call ESMF_AttributeGet(FIELD, NAME='LONG_NAME'   , VALUE=LONG_NAME , rc=status)
          _VERIFY(STATUS)
          call ESMF_AttributeGet(FIELD, NAME='UNITS'       , VALUE=UNITS     , rc=status)
          _VERIFY(STATUS)
          call ESMF_AttributeGet(field, NAME='DIMS'        , VALUE=DIMS(1)      , rc=status)
          _VERIFY(STATUS)
          call ESMF_AttributeGet(field, NAME="VLOCATION" , isPresent=isPresent, RC=STATUS)
          _VERIFY(STATUS)
          if ( isPresent ) then
             call ESMF_AttributeGet(field, NAME="VLOCATION" , VALUE=LOCATION(1)  , RC=STATUS)
             _VERIFY(STATUS)
          else
             LOCATION(1) = MAPL_VLocationNone
          end if
          call ESMF_FieldGet    (FIELD, ARRAY=array, name=FieldName,  RC=STATUS)
          _VERIFY(STATUS)
          ! Check for old style aerosol names
          ind= index(FieldName, '::')
          if (ind> 0) then
             FieldName = trim(FieldName(ind+2:))
          end if
          ! Extract some info from the array and define variables accordingly
          call ESMF_ArrayGet    (array, typekind=tk, rank=arrayRank,  RC=STATUS)
          _VERIFY(STATUS)
   !ALT                if (tk .eq. ESMF_TYPEKIND_I1) DataType = NF90_BYTE
   !ALT                if (tk .eq. ESMF_TYPEKIND_I2) DataType = NF90_SHORT
          if (tk .eq. ESMF_TYPEKIND_I4) DataType = NF90_INT
          if (tk .eq. ESMF_TYPEKIND_R4) DataType = NF90_FLOAT
          if (tk .eq. ESMF_TYPEKIND_R8) DataType = NF90_DOUBLE
          if (tk .eq. ESMF_TYPEKIND_I4) pfDataType = pFIO_INT32
          if (tk .eq. ESMF_TYPEKIND_R4) pfDataType = pFIO_REAL32
          if (tk .eq. ESMF_TYPEKIND_R8) pfDataType = pFIO_REAL64

          if (arrayRank == 1) then
             if (DIMS(1)==MAPL_DimsVertOnly) then
                if (LOCATION(1) == MAPL_VLocationCenter) then
                   call add_fvar(cf,trim(fieldname),pfDataType,'lev',units,long_name,rc=status)
                   _VERIFY(status)
                elseif(LOCATION(1) == MAPL_VLocationEdge) then
                   call add_fvar(cf,trim(fieldname),pfDataType,'edge',units,long_name,rc=status)
                   _VERIFY(status)
                else
                   _FAIL( 'ERROR: LOCATION not recognized for rank 1')
                endif
             elseif(DIMS(1)==MAPL_DimsTileOnly) then
                call add_fvar(cf,trim(fieldname),pfDataType,'tile',units,long_name,rc=status)
                _VERIFY(status)
             elseif(DIMS(1)==MAPL_DimsNone) then
                found = .false.
                do j=1,n_unique_ungrid_dims
                   if (ungrid_dims(i,1) == unique_ungrid_dims(j) ) then
                      myUngridDimName1 = trim(unique_ungrid_dim_name(j))
                      found = .true.
                      exit
                   end if
                end do
                _ASSERT(found, 'search failed')
                call add_fvar(cf,trim(fieldname),pfDataType,myUngridDimName1,units,long_name,rc=status)
                _VERIFY(status)
             else
                _FAIL( 'unsupported Dims case')
             endif
          else if(arrayRank == 2) then
             if (DIMS(1)==MAPL_DimsHorzOnly) then
                call add_fvar(cf,trim(fieldname),pfDataType,'lon,lat',units,long_name,rc=status)
                _VERIFY(status)
             else if(DIMS(1)==MAPL_DimsTileTile) then
                call add_fvar(cf,trim(fieldname),pfDataType,'tile,subtile',units,long_name,rc=status)
                _VERIFY(status)
             elseif(DIMS(1)==MAPL_DimsTileOnly) then
                do j=1,n_unique_ungrid_dims
                   if (ungrid_dims(i,1) == unique_ungrid_dims(j) ) then
                      myUngridDimName1 = trim(unique_ungrid_dim_name(j))
                      exit
                   end if
                end do
                call add_fvar(cf,trim(fieldname),pfDataType,'tile,'//myUngridDimName1,units,long_name,rc=status)
                _VERIFY(status)
             else
                write(buffer,*)'ERROR: DIMS not recognized for rank 2 variable ',trim(FieldName), DIMS(1)
                _FAIL( trim(buffer))
             endif

          else if(arrayRank == 3) then
             if (DIMS(1)==MAPL_DimsHorzVert) then
                if (LOCATION(1) == MAPL_VLocationCenter) then
                   call flip_vars%push_back(trim(filename))
                   call add_fvar(cf,trim(fieldname),pfDataType,'lon,lat,lev',units,long_name,rc=status)
                   _VERIFY(status)
                else if(LOCATION(1) == MAPL_VLocationEdge) then
                   call flip_vars%push_back(trim(filename))
                   call add_fvar(cf,trim(fieldname),pfDataType,'lon,lat,edge',units,long_name,rc=status)
                   _VERIFY(status)
                else
                   _FAIL( 'ERROR: LOCATION not recognized for rank 3')
                endif
             else if(DIMS(1)==MAPL_DimsHorzOnly) then
                do j=1,n_unique_ungrid_dims
                   if (ungrid_dims(i,1) == unique_ungrid_dims(j) ) then
                      myUngridDimName1 = trim(unique_ungrid_dim_name(j))
                      exit
                   end if
                end do
                call add_fvar(cf,trim(fieldname),pfDataType,'lon,lat,'//myUngridDimName1,units,long_name,rc=status)
                _VERIFY(status)
             else if (DIMS(1)==MAPL_DimsTileOnly) then
                do j=1,n_unique_ungrid_dims
                   if (ungrid_dims(i,1) == unique_ungrid_dims(j) ) then
                      myUngridDimName1 = trim(unique_ungrid_dim_name(j))
                      exit
                   end if
                end do
                do j=1,n_unique_ungrid_dims
                   if (ungrid_dims(i,2) == unique_ungrid_dims(j) ) then
                      myUngridDimName2 = trim(unique_ungrid_dim_name(j))
                      exit
                   end if
                end do
                call add_fvar(cf,trim(fieldname),pfDataType,'tile,'//myUngridDimName1//','//myUngridDimName2,units,long_name,rc=status)
                _VERIFY(status)
             else if(DIMS(1)/=MAPL_DimsHorzVert .and. DIMS(1)/=MAPL_DimsHorzOnly) then
                _FAIL( 'ERROR: What else could it be')
             endif
          else if(arrayRank == 4) then
             if (DIMS(1)==MAPL_DimsHorzVert) then
                do j=1,n_unique_ungrid_dims
                   if (ungrid_dims(i,1) == unique_ungrid_dims(j) ) then
                      myUngridDimName1 = trim(unique_ungrid_dim_name(j))
                      exit
                   end if
                end do
                if (LOCATION(1) == MAPL_VLocationCenter) then
                   call add_fvar(cf,trim(fieldname),pfDataType,'lon,lat,lev,'//myUngridDimName1,units,long_name,rc=status)
                   _VERIFY(status)
                else if(LOCATION(1) == MAPL_VLocationEdge) then
                   call add_fvar(cf,trim(fieldname),pfDataType,'lon,lat,edge,'//myUngridDimName1,units,long_name,rc=status)
                   _VERIFY(status)
                else
                   _FAIL( 'ERROR: LOCATION not recognized for rank 4')
                endif
             else if(DIMS(1)==MAPL_DimsHorzOnly) then
                do j=1,n_unique_ungrid_dims
                   if (ungrid_dims(i,1) == unique_ungrid_dims(j) ) then
                      myUngridDimName1 = trim(unique_ungrid_dim_name(j))
                      exit
                   end if
                end do
                do j=1,n_unique_ungrid_dims
                   if (ungrid_dims(i,2) == unique_ungrid_dims(j) ) then
                      myUngridDimName2 = trim(unique_ungrid_dim_name(j))
                      exit
                   end if
                end do
                call add_fvar(cf,trim(fieldname),pfDataType,'lon,lat,'//myUngridDimName1//','//myUngridDimName2,units,long_name,rc=status)
                _VERIFY(status)
             else if (DIMS(1)==MAPL_DimsTileOnly .or. &
                  DIMS(1)==MAPL_DimsTileTile) then
                _FAIL( 'ERROR: tiles with 2 or more UNGRIDDED dims not supported')
             else
                _FAIL( 'ERROR: What else could it be')
             endif
          else
             write(buffer,*) 'ERROR: arrayRank ',arrayRank, ' not supported'
             _FAIL( trim(buffer))
          endif

       enddo

       call ESMF_AttributeGet(bundle, name='MAPL_GridCapture', isPresent=isPresent, _RC)
       if (isPresent) then
          call ESMF_AttributeGet(bundle, name='MAPL_GridCapture', value=isGridCapture, _RC)
       else
          isGridCapture = .false.
       end if

       if (isGridCapture) then
          call add_fvar(cf, 'lons', pFIO_REAL64, 'lon,lat,', 'degrees east', 'lons', _RC)
          call add_fvar(cf, 'lats', pFIO_REAL64, 'lon,lat,', 'degrees north', 'lats', _RC)
       end if

       if (ungrid_dim_max_size /= 0) then
          deallocate(unique_ungrid_dims)
          deallocate(ungriddim)
       end if
       deallocate(ungrid_dims)

       call MPI_Info_create(info,STATUS)
       _VERIFY(STATUS)
       call MPI_Info_set(info,"romio_cb_write", trim(arrdes%romio_cb_write),STATUS)
       _VERIFY(STATUS)
       call MPI_Info_set(info,"cb_buffer_size", trim(arrdes%cb_buffer_size),STATUS)
       _VERIFY(STATUS)

!   now write the files

    if (have_oclients) then
       call oClients%set_optimal_server(1)
       if (arrdes%split_checkpoint) then
          if (.not.allocated(arrdes%collection_id)) allocate(arrdes%collection_id(arrdes%num_writers))
          do i=1,arrdes%num_writers
             fname_by_writer = get_fname_by_rank(trim(filename),i-1)
             iter = RstCollections%find(trim(fname_by_writer))
             if (iter == RstCollections%end()) then
                call cf%add_attribute("Split_Cubed_Sphere", i, _RC)
                arrdes%collection_id(i) = oClients%add_hist_collection(cf)
                call RstCollections%insert(trim(fname_by_writer), arrdes%collection_id(i))
             else
                arrdes%collection_id(i) = iter%value()
                call oClients%modify_metadata(arrdes%collection_id(i), var_map = var_map, rc=status)
                _VERIFY(status)
             endif
             arrdes%filename = trim(filename)
          enddo
       else
          if (.not.allocated(arrdes%collection_id)) allocate(arrdes%collection_id(1))
          iter = RstCollections%find(trim(BundleName))
          if (iter == RstCollections%end()) then
             arrdes%collection_id(1) = oClients%add_hist_collection(cf)
             call RstCollections%insert(trim(BundleName), arrdes%collection_id(1))
          else
             arrdes%collection_id(1) = iter%value()
             call oClients%modify_metadata(arrdes%collection_id(1), var_map = var_map, rc=status)
             _VERIFY(status)
          endif
          arrdes%filename = trim(filename)
       end if

    else

       pfio_mode = PFIO_NOCLOBBER 
       if (clobber) pfio_mode = PFIO_CLOBBER
       if (arrdes%writers_comm /= mpi_comm_null) then
          if (arrdes%num_writers == 1) then
             call formatter%create(trim(filename), mode=pfio_mode, rc=status)
             _VERIFY(status)
             call formatter%write(cf,rc=status)
             _VERIFY(STATUS)
          else
             if (arrdes%split_checkpoint) then
                call mpi_comm_rank(arrdes%writers_comm,writer_rank,status)
                _VERIFY(STATUS)
                fname_by_writer = get_fname_by_rank(trim(filename),writer_rank)
                call formatter%create(trim(fname_by_writer),rc=status)
                _VERIFY(status)
                call cf%add_attribute("Split_Cubed_Sphere", writer_rank, _RC)
             else
                call formatter%create_par(trim(filename),mode=pfio_mode,comm=arrdes%writers_comm,info=info,rc=status)
                _VERIFY(status)
             endif
             call formatter%write(cf,rc=status)
             _VERIFY(STATUS)
          end if
       end if
    endif ! write_restart_by_oserver

    do l=1,nVars
       call ESMF_FieldBundleGet(bundle, fieldIndex=l, field=field, rc=status)
       _VERIFY(STATUS)
       call ESMF_FieldGet(field,name=FieldName,rc=status)
       _VERIFY(STATUS)
       ! Check for old style aerosol names
       ind= index(FieldName, '::')
       if (ind> 0) then
          FieldName = trim(FieldName(ind+2:))
       end if

       if (.not.associated(MASK)) then
          call ESMF_AttributeGet(field, name='DIMS', value=MAPL_DIMS, rc=status)
          _VERIFY(STATUS)
          if (MAPL_DIMS == MAPL_DimsTileOnly .or. MAPL_DIMS == MAPL_DimsTileTile) then
             call ESMF_FieldGet   (field, grid=grid, rc=status)
             _VERIFY(STATUS)
             call MAPL_TileMaskGet(grid,  mask, rc=status)
             _VERIFY(STATUS)
          endif
       endif

       call MAPL_FieldWriteNCPar(formatter, fieldName, field, arrdes, HomePE=mask, oClients=oClients, rc=status)
       _VERIFY(STATUS)

       call ESMF_AttributeGet(field,name="FLIPPED",isPresent=isPresent,rc=status)
       if (isPresent) then
         call ESMF_AttributeGet(field,name="FLIPPED",value=fieldName,rc=status)
         if (status == _SUCCESS) then
            call ESMF_FieldDestroy(field,noGarbage=.true.,rc=status)
            _VERIFY(status)
         end if
       end if

    enddo

    call ESMF_AttributeGet(bundle, name='MAPL_GridCapture', isPresent=isPresent, _RC)
    if (isPresent) then
       call ESMF_AttributeGet(bundle, name='MAPL_GridCapture', value=isGridCapture, _RC)
    else
       isGridCapture = .false.
    end if

    if (isGridCapture) then
       call ESMF_GridGet(arrdes%grid, name=fieldname, _RC)
       lons_field = ESMF_FieldCreate(grid=arrdes%grid, typekind=ESMF_TYPEKIND_R8, name='lons', _RC)
       lats_field = ESMF_FieldCreate(grid=arrdes%grid, typekind=ESMF_TYPEKIND_R8, name='lats', _RC)

       call ESMF_GridGetCoord(grid=arrdes%grid, localDE=0, coordDim=1, &
           staggerloc=ESMF_STAGGERLOC_CENTER, farrayPtr=grid_lons, _RC)
       call ESMF_GridGetCoord(grid=arrdes%grid, localDE=0, coordDim=2, &
           staggerloc=ESMF_STAGGERLOC_CENTER, farrayPtr=grid_lats, _RC)

       call ESMF_FieldGet(lons_field, farrayPtr=lons_field_ptr, _RC)
       call ESMF_FieldGet(lats_field, farrayPtr=lats_field_ptr, _RC)

       lons_field_ptr = grid_lons
       lats_field_ptr = grid_lats

       call ESMF_AttributeSet(lons_field, name="DIMS", value=MAPL_DimsHorzOnly, _RC)
       call ESMF_AttributeSet(lats_field, name="DIMS", value=MAPL_DimsHorzOnly, _RC)

       call MAPL_FieldWriteNCPar(formatter, 'lons', lons_field, arrdes, HomePE=mask, oClients=oClients, rc=status)
       call MAPL_FieldWriteNCPar(formatter, 'lats', lats_field, arrdes, HomePE=mask, oClients=oClients, rc=status)
    end if

    if (have_oclients) then
       call oClients%done_collective_stage(_RC)
       call oClients%post_wait()
       call MPI_Info_free(info, status)
       _VERIFY(STATUS)
    elseif (arrdes%writers_comm/=MPI_COMM_NULL) then
       call formatter%close(rc=status)
       _VERIFY(STATUS)
       call MPI_Info_free(info, status)
       _VERIFY(STATUS)
    end if

    if(associated(MASK)) then
       DEALOC_(MASK)
    end if


    _RETURN(ESMF_SUCCESS)

    contains

    subroutine add_fvar(cf,vname,vtype,dims,units,long_name,rc)
       type(FileMetadata), intent(inout) :: cf
       integer, intent(in) :: vtype
       character(len=*), intent(in) :: vname
       character(len=*), intent(in) :: dims
       character(len=*), intent(in) :: units
       character(len=*), intent(in) :: long_name
       integer, optional, intent(out) :: rc

       integer :: status
       type(Variable) :: fvar

       fvar = Variable(type=vtype,dimensions=dims)
       call fvar%add_attribute('units',trim(units))
       call fvar%add_attribute('long_name',trim(long_name))
       call cf%add_variable(trim(vname),fvar,rc=status)
       _VERIFY(status)

       end subroutine add_fvar

  end subroutine MAPL_BundleWriteNCPar

  subroutine MAPL_StateVarWriteNCPar(filename, STATE, ARRDES, CLOCK, NAME, forceWriteNoRestart, clobber, oClients, RC)
    character(len=*)            , intent(IN   ) :: filename
    type (ESMF_State)           , intent(IN   ) :: STATE
    type(ArrDescr)              , intent(INOUT) :: ARRDES
    type(ESMF_Clock)            , intent(IN   ) :: CLOCK
    character(len=*),   optional, intent(IN   ) :: NAME
    logical,            optional, intent(IN   ) :: forceWriteNoRestart
    logical,            optional, intent(in  ) :: clobber
    type (ClientManager), optional, intent(inout) :: oClients
    integer,            optional, intent(  OUT) :: RC

! Local vars
    type (ESMF_FieldBundle)              :: bundle
    type (ESMF_Field)                    :: field
    integer                              :: status
    integer                              :: I, J, ITEMCOUNT
    type (ESMF_StateItem_Flag), pointer  :: ITEMTYPES(:)
    character(len=ESMF_MAXSTR ), pointer :: ITEMNAMES(:)
    logical, pointer                     :: DOIT(:)
    logical                              :: skipWriting
    integer                              :: RST, dna
    character(len=ESMF_MAXSTR)           :: FieldName,BundleName,StateName
    logical                              :: forceWriteNoRestart_

    type (ESMF_Field)                  :: new_field, added_field
    type (ESMF_FieldBundle)            :: bundle_write
    integer                            :: nBundle
    logical                            :: isPresent
    character(len=ESMF_MAXSTR)         :: positive
    logical                            :: flip
    logical                            :: is_test_framework, isGridCapture
    integer :: fieldIsValid
    type(ESMF_Array) :: array
    logical :: local_clobber

    call ESMF_StateGet(STATE,ITEMCOUNT=ITEMCOUNT,RC=STATUS)
    _VERIFY(STATUS)

    _ASSERT(ITEMCOUNT>0, 'itemcount must be > 0')

    allocate(ITEMNAMES(ITEMCOUNT),STAT=STATUS)
    _VERIFY(STATUS)
    allocate(ITEMTYPES(ITEMCOUNT),STAT=STATUS)
    _VERIFY(STATUS)
    allocate(DOIT     (ITEMCOUNT),STAT=STATUS)
    _VERIFY(STATUS)

    call ESMF_StateGet(STATE,ITEMNAMELIST=ITEMNAMES,ITEMTYPELIST=ITEMTYPES,RC=STATUS)
    _VERIFY(STATUS)
    call ESMF_StateGet(STATE,name=StateName,RC=STATUS)
    _VERIFY(STATUS)

    forceWriteNoRestart_ = .false.
    if(present(forceWriteNoRestart)) forceWriteNoRestart_ = forceWriteNoRestart
    local_clobber = .false.
    if (present(clobber)) local_clobber = clobber

    if(present(NAME)) then
       DOIT = ITEMNAMES==NAME
       _ASSERT(count(DOIT)/=0, 'count(DOIT) must not be 0')
    else
       DOIT = .true.
    endif

    bundle_write = ESMF_FieldBundleCreate(name=trim(StateName),rc=STATUS)
    _VERIFY(STATUS)
    call ESMF_FieldBundleSet(bundle_write,grid=arrdes%grid,rc=STATUS)
    _VERIFY(STATUS)

    call ESMF_AttributeGet(state,name="POSITIVE",value=positive,rc=status)
    _VERIFY(status)
    call ESMF_AttributeSet(bundle_write,name="POSITIVE",value=positive,rc=status)
    _VERIFY(status)
    flip = trim(positive)=="up"

    DO I = 1, ITEMCOUNT


       IF (DOIT     (I)) then

          IF (ITEMTYPES(I) == ESMF_StateItem_FieldBundle) then
             call ESMF_StateGet(state, itemnames(i), bundle, rc=status)
             _VERIFY(STATUS)
             skipWriting = .false.
             if (.not. forceWriteNoRestart_) then
                call ESMF_AttributeGet(bundle, name='RESTART', isPresent=isPresent, rc=status)
                _VERIFY(STATUS)
                if (isPresent) then
                   call ESMF_AttributeGet(bundle, name='RESTART', value=RST, rc=status)
                   _VERIFY(STATUS)
                   skipWriting = (RST == MAPL_RestartSkip)
                end if
             else
                skipWriting = .true.
             end if

             call ESMF_AttributeGet(state, name='MAPL_TestFramework', isPresent=isPresent, _RC)
             if (isPresent) then
                call ESMF_AttributeGet(state, name='MAPL_TestFramework', value=is_test_framework, _RC)
                if (is_test_framework) skipWriting = .false.
             end if

             if (skipWriting) cycle
             call ESMF_FieldBundleGet(bundle, fieldCount=nBundle, rc=STATUS)
             _VERIFY(STATUS)
             call ESMF_FieldBundleGet(bundle, name=BundleName, rc=status)
             _VERIFY(STATUS)
             DO J = 1,nBundle
               call ESMF_FieldBundleGet(bundle, fieldIndex=J, field=field, rc=status)
               _VERIFY(STATUS)
               call ESMF_FieldGet(field,name=FieldName,rc=status)
               _VERIFY(STATUS)
               ! Tack on BundleName to distiguish duplicate FieldNames in different Bundles (PCHEM for instance)
               FieldName = trim(BundleName) //'_'// trim(FieldName)
               new_field = MAPL_FieldCreate(Field,FieldName,rc=status)
               _VERIFY(STATUS)
               call MAPL_FieldBundleAdd(bundle_write,new_field,rc=status)
               _VERIFY(STATUS)
             ENDDO

          ELSE IF (ITEMTYPES(I) == ESMF_StateItem_Field) THEN
             call ESMF_StateGet(state, itemnames(i), field, rc=status)
             _VERIFY(STATUS)
             call ESMF_FieldGet(field,array=array,rc=FieldIsValid)

             if (fieldIsValid == 0) then

                skipWriting = .false.
                if (.not. forceWriteNoRestart_) then
                   call ESMF_AttributeGet(field, name='RESTART', isPresent=isPresent, rc=status)
                   _VERIFY(STATUS)
                   if (isPresent) then
                      call ESMF_AttributeGet(field, name='RESTART', value=RST, rc=status)
                      _VERIFY(STATUS)
                      skipWriting = (RST == MAPL_RestartSkip)
                   end if
                else
                   skipWriting = .true.
                end if

                call ESMF_AttributeGet(state, name='MAPL_TestFramework', isPresent=isPresent, _RC)
                if (isPresent) then
                   call ESMF_AttributeGet(state, name='MAPL_TestFramework', value=is_test_framework, _RC)
                   if (is_test_framework) skipWriting = .false.
                end if

                if (skipWriting) cycle

                call ESMF_AttributeGet(field, name='doNotAllocate', isPresent=isPresent, rc=status)
                _VERIFY(STATUS)
                if (isPresent) then
                   call ESMF_AttributeGet(field, name='doNotAllocate', value=dna, rc=status)
                   _VERIFY(STATUS)
                   skipWriting = (dna /= 0)
                endif

                call ESMF_AttributeGet(state, name='MAPL_TestFramework', isPresent=isPresent, _RC)
                if (isPresent) then
                   call ESMF_AttributeGet(state, name='MAPL_TestFramework', value=is_test_framework, _RC)
                   if (is_test_framework) skipWriting = .false.
                end if

                if (skipWriting) cycle

                if (flip) then
                   added_field = create_flipped_field(field,rc=status)
                   _VERIFY(status)
                else
                   added_field = field
                end if
                call MAPL_FieldBundleAdd(bundle_write,added_field,rc=status)
                _VERIFY(STATUS)
             end if

          end IF
       END IF

    END DO

    deallocate(ITEMNAMES)
    deallocate(ITEMTYPES)
    deallocate(DOIT     )

    call ESMF_AttributeGet(state, name='MAPL_GridCapture', isPresent=isPresent, _RC)
    if (isPresent) then
       call ESMF_AttributeGet(state, name='MAPL_GridCapture', value=isGridCapture, _RC)
       call ESMF_AttributeSet(bundle_write, name="MAPL_GridCapture", value=isGridCapture, _RC)
    end if

    call MAPL_BundleWriteNCPar(Bundle_Write, arrdes, CLOCK, filename, clobber=local_clobber, oClients=oClients, rc=status)
    _VERIFY(STATUS)

    _RETURN(ESMF_SUCCESS)

  end subroutine MAPL_StateVarWriteNCPar

  subroutine MAPL_NCIOGetFileType(filename,filetype,rc)
   ! filetype = 0, hdf5
   ! filetype = 1, ascii
   ! filetype = 2, binary or unknown
   ! filetype = -1, unknown

   implicit none
   ! Arguments
   !----------
   character(len=*),  intent(IN   ) :: filename
   integer,           intent(INOUT) :: filetype
   integer, optional, intent(  OUT) :: RC

   ! ErrLog variables
   !-----------------

   integer                      :: STATUS

   character(len=1)             :: word(4)
   character(len=1)             :: TwoWords(8)
   integer, parameter           :: hdf5(8) = (/137, 72, 68, 70, 13, 10, 26, 10 /)
   integer                      :: irec
   integer                      :: unit
   integer                      :: i, cwrd
   logical                      :: typehdf5, isascii
   character(len=12)            :: fmt

   INQUIRE(IOLENGTH=IREC) WORD
   open (NEWUNIT=UNIT, FILE=FILENAME, FORM='unformatted', ACCESS='DIRECT', RECL=IREC, IOSTAT=status)
   _VERIFY(STATUS)

! Read first 8 characters and compare with HDF5 signature
   read (UNIT, REC=1, ERR=100) TwoWords(1:4)
   read (UNIT, REC=2, ERR=100) TwoWords(5:8)
   close(UNIT)

   filetype = MAPL_FILETYPE_UNK ! Unknown

   typehdf5 = .true.
   do i = 1, 8
      if (iachar(TwoWords(i)) /= hdf5(i)) then
         typehdf5 = .false.
         exit
      end if
   end do
   if (typehdf5) then
      filetype = MAPL_FILETYPE_NC4
      _RETURN(ESMF_SUCCESS)
   end if

   isascii = .true.
   do i = 1, 4
       if (iachar(TwoWords(i)) < 7) then
          isascii = .false.
          exit
       end if
   end do
   if (isascii) then
      filetype = MAPL_FILETYPE_TXT
      _RETURN(ESMF_SUCCESS)
   endif

   filetype = MAPL_FILETYPE_BIN
   _RETURN(ESMF_SUCCESS)

100   continue
   _RETURN(ESMF_FAILURE)

  end subroutine MAPL_NCIOGetFileType

  subroutine MAPL_IOChangeRes(cfIn,cfOut,dimNames,dimSizes,rc)
  type(FileMetadata), intent(inout) :: cfIn
  type(Filemetadata), intent(inout) :: cfOut
  character(len=*) :: dimNames(:)
  integer, intent(in) :: dimSizes(:)
  integer, intent(out), optional :: rc

  integer :: status
  type(StringIntegerMap) :: newDims
  integer :: i

  do i=1,size(dimNames)
     call newDims%insert(trim(dimNames(i)),dimSizes(i))
  enddo

  cfOut = cfIn
  call modify_grid_dimensions(rc=status)
  _VERIFY(status)
  call modify_coordinate_vars(rc=status)

  _RETURN(ESMF_SUCCESS)

  contains

      subroutine modify_grid_dimensions(rc)
         integer, optional, intent(out) :: rc
         integer :: status
         type(StringIntegerMap), pointer :: dims
         type(StringIntegerMapIterator) :: iter
         character(len=:), pointer :: name
         integer, pointer :: newExtent => null()

         dims => cfIn%get_dimensions()

         iter = dims%begin()
         do while (iter /= dims%end())
            name => iter%key()
            newExtent => newDims%at(trim(name))
            if (associated(newExtent)) then
               call cfOut%modify_dimension(trim(name),newExtent,rc=status)
               nullify(newExtent)
            end if
            call iter%next()
         enddo

         _RETURN(ESMF_SUCCESS)

      end subroutine modify_grid_dimensions

      subroutine modify_coordinate_vars(rc)
         integer, optional, intent(out) :: rc
         integer :: status
         type(StringVariableMap), pointer :: vars
         type(StringVariableMapIterator) :: iter
         type(CoordinateVariable), pointer :: cvar
         character(len=:), pointer :: name
         real(kind=REAL32) :: r32_x1,r32_x0
         real(kind=REAL64) :: r64_x1,r64_x0
         real(kind=REAL32), allocatable :: var32(:)
         real(kind=REAL64), allocatable :: var64(:)
         integer, pointer :: newExtent => null()
         class(*), pointer :: dim_var_values(:)
         class(*), allocatable :: coordinate_data(:)

         vars => cfIn%get_variables()

         iter = vars%begin()
         do while (iter /= vars%end())
            name => iter%key()
            newExtent => newDims%at(trim(name))
            if (associated(newExtent)) then
               cvar => cfOut%get_coordinate_variable(trim(name),rc=status)
               if (status==ESMF_SUCCESS) then
                  dim_var_values => cvar%get_coordinate_data()
                  select type(q => dim_var_values)
                  type is (real(REAL32))
                     r32_x0=1.0d0
                     r32_x1=dble(newExtent)
                     var32 = MAPL_Range(r32_x0,r32_x1,newExtent)
                     allocate(coordinate_data,source=var32)
                     call cvar%replace_coordinate_data(coordinate_data)
                     deallocate(coordinate_data,var32)
                  type is (real(REAL64))
                     r64_x0=1.0d0
                     r64_x1=dble(newExtent)
                     var64 = MAPL_Range(r64_x0,r64_x1,newExtent)
                     allocate(coordinate_data,source=var64)
                     call cvar%replace_coordinate_data(coordinate_data)
                     deallocate(coordinate_data,var64)
                  class default
                     status = ESMF_FAILURE
                  end select

               end if

               nullify(newExtent)
            end if
           call iter%next()
         enddo

         _RETURN(ESMF_SUCCESS)

      end subroutine modify_coordinate_vars

  end subroutine MAPL_IOChangeRes

  subroutine MAPL_IOCountNonDimVars(cf,nvars,rc)
  type(FileMetadata), intent(inout) :: cf
  integer, intent(out) :: nvars
  integer, intent(out), optional :: rc

  type(StringVariableMap), pointer :: vars
  type(StringVariableMapIterator) :: iter
  type(StringIntegerMap), pointer :: dims
  integer, pointer :: dimsize => null()
  character(len=:), pointer :: name

  nvars = 0
  dims => cf%get_dimensions()
  vars => cf%get_variables()
  iter = vars%begin()
  do while(iter/=vars%end())

     name =>  iter%key()
     dimsize => dims%at(trim(name))
     if (.not.associated(dimsize)) nvars=nvars+1
     if (associated(dimsize)) nullify(dimsize)

     call iter%next()
  end do

  _RETURN(ESMF_SUCCESS)

  end subroutine MAPL_IOCountNonDimVars

  function MAPL_IOGetNonDimVars(cf,rc) result(nondim_vars)
  type(FileMetadata), intent(inout) :: cf
  integer, intent(out), optional :: rc

  type(StringVector) :: nondim_vars
  type(StringVariableMap), pointer :: vars
  type(StringVariableMapIterator) :: iter
  type(StringIntegerMap), pointer :: dims
  integer, pointer :: dimsize => null()
  character(len=:), pointer :: name

  dims => cf%get_dimensions()
  vars => cf%get_variables()
  iter = vars%begin()
  do while(iter/=vars%end())

     name =>  iter%key()
     dimsize => dims%at(trim(name))
     if (.not.associated(dimsize)) call nondim_vars%push_back(trim(name))
     if (associated(dimsize)) nullify(dimsize)

     call iter%next()
  end do

  _RETURN(ESMF_SUCCESS)

  end function MAPL_IOGetNonDimVars

  subroutine MAPL_IOCountLevels(cf,nlev,rc)
  type(FileMetadata), intent(inout) :: cf
  integer, intent(out) :: nlev
  integer, intent(out), optional :: rc

  integer :: status
  type(StringVariableMap), pointer :: vars
  type(StringVariableMapIterator) :: iter
  type(StringIntegerMap), pointer :: dims
  integer, pointer :: dimsize => null()
  character(len=:), pointer :: name
  type(StringVector), pointer :: vdims
  type(Variable), pointer :: var
  integer :: levsize

  nlev = 0
  dims => cf%get_dimensions()
  vars => cf%get_variables()
  iter = vars%begin()
  do while(iter/=vars%end())

     name => iter%key()
     var => iter%value()
     dimsize => dims%at(trim(name))
     if (.not.associated(dimsize)) then
        vdims => var%get_dimensions()
        if (vdims%get_index('lev') /=0) then
           levsize = cf%get_dimension('lev',rc=status)
           _VERIFY(status)
           nlev=nlev+levsize
        else if (vdims%get_index('edge') /=0) then
           levsize = cf%get_dimension('edge',rc=status)
           _VERIFY(status)
           nlev=nlev+levsize
        else
           nlev=nlev+1
        end if
     end if
     if (associated(dimsize)) nullify(dimsize)

     call iter%next()
  end do

  _RETURN(ESMF_SUCCESS)

  end subroutine MAPL_IOCountLevels

  subroutine MAPL_IOGetTime(cf,nymd,nhms,rc)
  type(FileMetadata), intent(inout) :: cf
  integer, intent(out) :: nymd,nhms
  integer, intent(out), optional :: rc

  integer :: status

  class(Variable), pointer :: var
  type(Attribute), pointer :: attr
  class(*), pointer :: units
  integer :: year,month,day,hour,min,sec

  var => cf%get_variable('time',rc=status)
  _VERIFY(status)
  attr => var%get_attribute('units')
  units => attr%get_value()
  select type(units)
  type is (character(*))
     call MAPL_NCIOParseTimeUnits(units,year,month,day,hour,min,sec,status)
  class default
     _FAIL( 'unsupported subclass for units')
  end select
  nymd = year*10000 + month*100 + day
  nhms = hour*10000 + min*100   + sec

  _RETURN(ESMF_SUCCESS)

  end subroutine MAPL_IOGetTime

      subroutine MAPL_NCIOParseTimeUnits ( TimeUnits, year, month, day, hour, min, sec, rc )

      implicit none
!
! !INPUT PARAMETERS:
!
      character(len=*) TimeUnits      ! Units metadata string from the Time coord var
!
! !OUTPUT PARAMETERS:
!
      integer        year               ! 4-digit year
      integer        month              ! month
      integer        day                ! day
      integer        hour               ! hour
      integer        min                ! minute
      integer        sec                ! second
      integer        rc                 ! return code
                                        !  0 = no error
                                        ! -1 = problem parsing string

      integer ypos(2), mpos(2), dpos(2), hpos(2), spos(2)
      integer strlen
      integer firstdash, lastdash
      integer firstcolon, lastcolon
      integer lastspace
      strlen = LEN_TRIM (TimeUnits)

      firstdash = index(TimeUnits, '-')
      lastdash  = index(TimeUnits, '-', BACK=.TRUE.)

      if (firstdash .LE. 0 .OR. lastdash .LE. 0) then
        rc = -1
        return
      endif

      ypos(2) = firstdash - 1
      mpos(1) = firstdash + 1
      ypos(1) = ypos(2) - 3

      mpos(2) = lastdash - 1
      dpos(1) = lastdash + 1
      dpos(2) = dpos(1) + 1

      read ( TimeUnits(ypos(1):ypos(2)), * ) year
      read ( TimeUnits(mpos(1):mpos(2)), * ) month
      read ( TimeUnits(dpos(1):dpos(2)), * ) day

      firstcolon = index(TimeUnits, ':')
      if (firstcolon .LE. 0) then

        ! If no colons, check for hour.

        ! Logic below assumes a null character or something else is after the hour
        ! if we do not find a null character add one so that it correctly parses time
        if (TimeUnits(strlen:strlen) /= char(0)) then
           TimeUnits = trim(TimeUnits)//char(0)
           strlen=len_trim(TimeUnits)
        endif
        lastspace = index(TRIM(TimeUnits), ' ', BACK=.TRUE.)
        if ((strlen-lastspace).eq.2 .or. (strlen-lastspace).eq.3) then
          hpos(1) = lastspace+1
          hpos(2) = strlen-1
          read (TimeUnits(hpos(1):hpos(2)), * ) hour
          min  = 0
          sec  = 0
        else
          print *, 'ParseTimeUnits: Assuming a starting time of 00z'
          hour = 0
          min  = 0
          sec  = 0
        endif

      else
        hpos(1) = firstcolon - 2
        hpos(2) = firstcolon - 1
        lastcolon =  index(TimeUnits, ':', BACK=.TRUE.)
        if ( lastcolon .EQ. firstcolon ) then
          mpos(1) = firstcolon + 1
          mpos(2) = firstcolon + 2
          read (TimeUnits(hpos(1):hpos(2)), * ) hour
          read (TimeUnits(mpos(1):mpos(2)), * ) min
          sec = 0
        else
          mpos(1) = firstcolon + 1
          mpos(2) = lastcolon - 1
          spos(1) = lastcolon + 1
          spos(2) = lastcolon + 2
          read (TimeUnits(hpos(1):hpos(2)), * ) hour
          read (TimeUnits(mpos(1):mpos(2)), * ) min
          read (TimeUnits(spos(1):spos(2)), * ) sec
        endif
      endif

      rc = 0
      return
      end subroutine MAPL_NCIOParseTimeUnits

   ! WJ notes: To avoid changing gcm_run.j script, insert "_split_x_", not append
   function get_fname_by_rank(fname, rank) result(name)
     character(len=:), allocatable :: name
     character(len=*), intent(in) :: fname
     integer, intent(in) :: rank
     integer :: i

     name = trim(fname)//"_"//i_to_string(rank)

   end function get_fname_by_rank

   function check_flip(metadata,rc) result(flip)
      type(FileMetadata), intent(inout) :: metadata
      integer, optional, intent(out) :: rc
      character(len=:), pointer :: positive
      type(CoordinateVariable), pointer :: var
      type (StringVariableMap), pointer :: vars
      type (StringVariableMapIterator) :: var_iter
      character(len=:), pointer :: var_name
      logical :: isPresent
      logical :: flip
      type(Attribute), pointer :: attr => null()
      class(*), pointer :: vpos

      flip = .false.
      vars => metadata%get_variables()
      var_iter = vars%begin()
      do while(var_iter /=vars%end())
         var_name => var_iter%key()
         var => metadata%get_coordinate_variable(trim(var_name))
         if (associated(var)) then
            if (index(var_name,'lev') .ne. 0 .or. index(var_name,'edge') .ne. 0) then
               isPresent = var%is_attribute_present('positive')
               if (isPresent) then
                  attr => var%get_attribute('positive')
                  _ASSERT(associated(attr),"restart file leve dim has no positive attribute")
                  vpos => attr%get_value()
                  select type(vpos)
                  type is (character(*))
                     positive => vpos
                  class default
                     _FAIL('units must be string')
                  end select
               else
                  positive => null()
               end if
               if (associated(positive)) then
                  flip = (trim(positive) == "up")
                  _RETURN(_SUCCESS)
               end if
            end if
         end if
         call var_iter%next()
      enddo
      _RETURN(_SUCCESS)
   end function check_flip

   subroutine flip_field(field,rc)
      type(ESMF_Field), intent(inout) :: field
      integer, intent(out), optional :: rc

      integer :: status,rank
      real(KIND=ESMF_KIND_R4), pointer :: ptr_r4(:,:,:)
      real(KIND=ESMF_KIND_R8), pointer :: ptr_r8(:,:,:)
      real(KIND=ESMF_KIND_R4), allocatable :: alloc_r4(:,:,:)
      real(KIND=ESMF_KIND_R8), allocatable :: alloc_r8(:,:,:)
      type(ESMF_TypeKind_Flag) :: tk
      integer :: vloc,i,lb,ub,ii

      call ESMF_FieldGet(field,rank=rank,typeKind=tk,rc=status)
      _VERIFY(status)
      if (rank/=3) then
         _RETURN(_SUCCESS)
      else
         call ESMF_AttributeGet(field,name="VLOCATION",value=vloc,rc=status)
         _VERIFY(status)
         if (vloc==MAPL_VLocationCenter .or. vloc==MAPL_VLocationEdge) then
            if (tk == ESMF_TYPEKIND_R4) then
               call ESMF_FieldGet(field,farrayPtr=ptr_r4,rc=status)
               _VERIFY(status)
               allocate(alloc_r4,source=ptr_r4)
               lb = lbound(ptr_r4,dim=3)
               ub = ubound(ptr_r4,dim=3)
               ii=0
               do i=lb,ub
                  ptr_r4(:,:,i)=alloc_r4(:,:,ub-ii)
                  ii=ii+1
               enddo
            else if (tk == ESMF_TYPEKIND_R8) then
               call ESMF_FieldGet(field,farrayPtr=ptr_r8,rc=status)
               _VERIFY(status)
               allocate(alloc_r8,source=ptr_r8)
               lb = lbound(ptr_r8,dim=3)
               ub = ubound(ptr_r8,dim=3)
               ii=0
               do i=lb,ub
                  ptr_r8(:,:,i)=alloc_r8(:,:,ub-ii)
                  ii=ii+1
               enddo
            end if
         end if
      end if
      _RETURN(_SUCCESS)
   end subroutine flip_field

   function create_flipped_field(field,rc) result(flipped_field)
      type(ESMF_Field), intent(inout) :: field
      integer, intent(out), optional :: rc

      type(ESMF_Field) :: flipped_field
      integer :: status,rank
      character(len=ESMF_MAXSTR) :: fname
      integer :: vloc,lb(1),ub(1)
      type(ESMF_Grid) :: grid
      type(ESMF_TYPEKIND_FLAG) :: tk
      real(KIND=ESMF_KIND_R4), pointer :: ptr_r4_in(:,:,:),ptr_r4_out(:,:,:)
      real(KIND=ESMF_KIND_R8), pointer :: ptr_r8_in(:,:,:),ptr_r8_out(:,:,:)


      call ESMF_FieldGet(field,rank=rank,name=fname,rc=status)
      _VERIFY(status)
      if (rank==3) then
         call ESMF_AttributeGet(field,name="VLOCATION",value=vloc,rc=status)
         _VERIFY(status)
         if (vloc==MAPL_VLocationCenter .or. vloc==MAPL_VLocationEdge) then
            call ESMF_FieldGet(Field,grid=grid,ungriddedLbound=lb,ungriddedUBound=ub,typekind=tk,rc=status)
            _VERIFY(status)
            flipped_field = ESMF_FieldCreate(grid,tk,name=trim(fname),ungriddedLBound=lb,ungriddedUBound=ub,rc=status)
            _VERIFY(status)
            call MAPL_FieldCopyAttributes(field_in=field,field_out=flipped_field,rc=status)
            _VERIFY(status)
            if (tk==ESMF_TYPEKIND_R4) then
               call ESMF_FieldGet(field,farrayptr=ptr_r4_in,rc=status)
               _VERIFY(status)
               call ESMF_FieldGet(flipped_field,farrayptr=ptr_r4_out,rc=status)
               _VERIFY(status)
               ptr_r4_out=ptr_r4_in
            else if (tk==ESMF_TYPEKIND_R8) then
               call ESMF_FieldGet(field,farrayptr=ptr_r8_in,rc=status)
               _VERIFY(status)
               call ESMF_FieldGet(flipped_field,farrayptr=ptr_r8_out,rc=status)
               _VERIFY(status)
               ptr_r8_out=ptr_r8_in
            end if
            call flip_field(flipped_field,rc=status)
            _VERIFY(status)
            call ESMF_AttributeSet(flipped_field,"FLIPPED","flipped",rc=status)
            _VERIFY(status)
         else
            flipped_field=field
         end if
      else
         flipped_field=field
      end if
      _RETURN(_SUCCESS)
   end function create_flipped_field

   subroutine MAPL_ReadTilingNC4(File, GridName, im, jm, nx, ny, n_Grids, n_tiles, iTable, rTable, N_PfafCat, AVR,rc)
      character(*),                             intent(IN)  :: File
      character(*), optional,                   intent(out) :: GridName(:)
      integer,      optional,                   intent(out) :: IM(:), JM(:)
      integer,      optional,                   intent(out) :: nx, ny, n_Grids, n_tiles
      integer,      optional, allocatable,      intent(out) :: iTable(:,:)
      real(kind=REAL64), optional, allocatable, intent(out) :: rTable(:,:)
      integer,      optional,              intent(out) :: N_PfafCat
      real,         optional, pointer,     intent(out) :: AVR(:,:)      ! used by GEOSgcm
      integer,      optional,              intent(out) :: rc

      type (Attribute), pointer     :: ref
      character(len=:), allocatable :: attr
      type (NetCDF4_FileFormatter)  :: formatter
      type (FileMetadata)           :: meta
      character(len=4)              :: ocn_str
      integer                       :: ng, ntile, status, ll
      class(*), pointer             :: attr_val(:)
      class(*), pointer             :: char_val
      integer, allocatable          :: tmp_int(:)
      real(kind=REAL64),allocatable :: fr(:)

      integer            :: NumCol
      integer,      allocatable :: iTable_(:,:)
      real(kind=REAL64), allocatable :: rTable_(:,:)

      call formatter%open(File, pFIO_READ, rc=status)
      meta = formatter%read(rc=status)

      ref => meta%get_attribute('N_Grids')
      attr_val => ref%get_values()
      select type(attr_val)
      type is (integer(INT32))
        ng = attr_val(1)
      endselect

      if (present(n_Grids)) then
        n_Grids = ng
      endif

      ntile = meta%get_dimension('tile')
      if (present(n_tiles)) then
         n_tiles = ntile
      endif

      if (present(nx)) then
         ref => meta%get_attribute('raster_nx')
         attr_val => ref%get_values()
         select type(attr_val)
         type is (integer(INT32))
            nx = attr_val(1)
         endselect
      endif
      if (present(ny)) then
         ref => meta%get_attribute('raster_ny')
         attr_val => ref%get_values()
         select type (attr_val)
         type is (integer(INT32))
            ny = attr_val(1)
         endselect
      endif

      if (present(N_PfafCat)) then
         ref => meta%get_attribute('N_PfafCat')
         attr_val => ref%get_values()
         select type (attr_val)
         type is (integer(INT32))
            N_PfafCat = attr_val(1)
         endselect
      endif

      do ll = 1, ng
        if (ll == 1) then
          ocn_str = ''
        else
          ocn_str = '_ocn'
        endif

        if (present(GridName)) then
           attr = 'Grid'//trim(ocn_str)//'_Name'
           ref =>meta%get_attribute(attr)
           char_val => ref%get_value()
           select type(char_val)
           type is(character(*))
              GridName(ll) = char_val
           class default
              print('unsupported subclass (not string) of attribute named '//attr)
           end select
        endif
        if (present(IM)) then
           attr = 'IM'//trim(ocn_str)
           ref =>meta%get_attribute(attr)
           attr_val => ref%get_values()
           select type(attr_val)
           type is( integer(INT32))
              IM(ll) = attr_val(1)
           end select
        endif
        if (present(JM)) then
           attr = 'JM'//trim(ocn_str)
           ref =>meta%get_attribute(attr)
           attr_val => ref%get_values()
           select type(attr_val)
           type is(integer(INT32))
              JM(ll) = attr_val(1)
           end select
        endif
      enddo

      if (present(iTable) .or. present(AVR) ) then
        allocate(iTable_(ntile,0:7))
        allocate(tmp_int(ntile))
        call formatter%get_var('typ', iTable_(:,0))
        do ll = 1, ng
          if (ll == 1) then
            ocn_str = ''
          else
            ocn_str = '_ocn'
          endif

          call formatter%get_var('i_indg'    //trim(ocn_str), tmp_int, rc=status)
          iTable_(:,ll*2) = tmp_int
          call formatter%get_var('j_indg'    //trim(ocn_str), tmp_int, rc=status)
          iTable_(:,ll*2+1) = tmp_int
          call formatter%get_var('dummy_index'//trim(ocn_str), tmp_int, rc=status)
          if ( ng == 1) then
            iTable_(:,4) = tmp_int ! for ease, it is pfaf
            ! set this 7th column to 1. This is to reproduce a potential bug
            ! when it is ease grid and mask file is not GEOS5_10arcsec_mask
            iTable_(:,7) = 1
          else
            iTable_(:,5+ll) = tmp_int
          endif
        enddo
        call formatter%get_var('pfaf_index', tmp_int, rc=status)
        if (ng == 2) then
           where (iTable_(:,0) == 100)
             iTable_(:,4) = tmp_int
           endwhere
        endif
      endif

      if (present(rTable) .or. present(AVR) ) then
        allocate(rTable_(ntile,10))
        call formatter%get_var('com_lon', rTable_(:,1),   rc=status)
        call formatter%get_var('com_lat', rTable_(:,2),   rc=status)
        call formatter%get_var('area',    rTable_(:,3),   rc=status)
        do ll = 1, ng
          if (ll == 1) then
            ocn_str = ''
          else
            ocn_str = '_ocn'
          endif
          call formatter%get_var('frac_cell' //trim(ocn_str), rTable_(:,3+ll), rc=status)
        enddo
        call formatter%get_var('min_lon', rTable_(:, 6), rc=status)
        call formatter%get_var('max_lon', rTable_(:, 7), rc=status)
        call formatter%get_var('min_lat', rTable_(:, 8), rc=status)
        call formatter%get_var('max_lat', rTable_(:, 9), rc=status)
        call formatter%get_var('elev',    rTable_(:,10), rc=status)
      endif
      if (present(AVR)) then
        ! In GEOSgcm, it already assumes ng = 2, so NumCol = 10
         NumCol = NumGlobalVars+NumLocalVars*ng
         allocate(AVR(ntile, NumCol))
         AVR(:, 1) = iTable_(:,0)
         ! for EASE grid, the second collum is replaced by the area
         AVR(:, 2) = rTable_(:,3)
         AVR(:, 3) = rTable_(:,1)
         AVR(:, 4) = rTable_(:,2)

         AVR(:, 5) = iTable_(:,2)
         AVR(:, 6) = iTable_(:,3)
         AVR(:, 7) = rTable_(:,4)
        if (ng == 1) then
          AVR(:,8) = iTable_(:,4)
        else
          AVR(:, 8)  = iTable_(:,6)

          AVR(:, 9)  = iTable_(:,4)
          AVR(:, 10) = iTable_(:,5)
          AVR(:, 11) = rTable_(:,5)
          AVR(:, 12) = iTable_(:,7)
        endif
      endif

      if (present(iTable)) then
        call move_alloc(iTable_, iTable)
      endif

      if (present(rTable)) then
        call move_alloc(rTable_, rTable)
        do ll = 1, ng
           where ( rTable(:,3+ll) /=0.0 ) rTable(:,3+ll) = rTable(:,3)/rTable(:,3+ll)
        enddo
      endif
      _RETURN(ESMF_SUCCESS)
   end subroutine MAPL_ReadTilingNC4

   subroutine MAPL_WriteTilingNC4(File, GridName, im, jm, nx, ny, iTable, rTable, N_PfafCat, rc)
   
     character(*),      intent(IN) :: File
     character(*),      intent(IN) :: GridName(:)
     integer,           intent(IN) :: IM(:), JM(:)
     integer,           intent(IN) :: nx, ny
     integer,           intent(IN) :: iTable(:,0:)
     real(REAL64),      intent(IN) :: rTable(:,:)
     integer, optional, intent(in) :: N_PfafCat
     integer, optional, intent(out):: rc
   
     integer                       :: k, ll, ng, ip, status, n_pfafcat_
   
     character(len=:), allocatable :: attr
     type (Variable)               :: v
     type (NetCDF4_FileFormatter)  :: formatter
     character(len=4)              :: ocn_str
     type (FileMetadata)           :: metadata
     integer,          allocatable :: II(:), JJ(:), KK(:), pfaf(:)
     real(REAL64),     allocatable :: fr(:)
     logical                       :: EASE
     integer, parameter            :: deflate_level = 1
     integer, parameter            :: SRTM_maxcat = 291284
 
     ng  = size(GridName)
     ip  = size(iTable,1)
   
     EASE = .false.
     if (index(GridName(1), 'EASE') /=0) EASE = .true.
     ! number of Pfafstetter catchments defined in underlying raster file
   
     n_pfafcat_ = SRTM_maxcat
   
     if (present(N_PfafCat)) n_pfafcat_ = N_PfafCat
   
     call metadata%add_dimension('tile', ip)
   
     ! -------------------------------------------------------------------
     !  
     ! create nc4 variables and write metadata
   
     do ll = 1, ng
       if (ll == 1) then
         ocn_str = ''
       else
         ocn_str = '_ocn'
       endif
   
       attr = 'Grid'//trim(ocn_str)//'_Name'
       call metadata%add_attribute( attr, trim(GridName(ll)))
       attr = 'IM'//trim(ocn_str)
       call metadata%add_attribute( attr, IM(ll))
       attr = 'JM'//trim(ocn_str)
       call metadata%add_attribute( attr, JM(ll))
     enddo
   
     attr = 'raster_nx'
     call metadata%add_attribute( attr, nx)
     attr = 'raster_ny'
     call metadata%add_attribute( attr, ny)
     attr = 'N_PfafCat'
     call metadata%add_attribute( attr, n_pfafcat_)
     attr = 'N_Grids'
     call metadata%add_attribute( attr, ng)
   
     v = Variable(type=PFIO_INT32, dimensions='tile')
     call v%add_attribute('units', '1')
     call v%add_attribute('long_name', 'tile_type')
     call v%set_deflation(DEFLATE_LEVEL)
     call metadata%add_variable('typ', v)
   
     v = Variable(type=PFIO_REAL64, dimensions='tile')
     call v%add_attribute('units', 'radian2')
     call v%add_attribute('long_name', 'tile_area')
     call v%add_attribute("missing_value", UNDEF_REAL64)
     call v%add_attribute("_FillValue", UNDEF_REAL64)
     call v%set_deflation(DEFLATE_LEVEL)
     call metadata%add_variable('area', v)
   
     v = Variable(type=PFIO_REAL64, dimensions='tile')
     call v%add_attribute('units', 'degree')
     call v%add_attribute('long_name', 'tile_center_of_mass_longitude')
     call v%add_attribute("missing_value", UNDEF_REAL64)
     call v%add_attribute("_FillValue", UNDEF_REAL64)
     call v%set_deflation(DEFLATE_LEVEL)
     call metadata%add_variable('com_lon', v)
   
     v = Variable(type=PFIO_REAL64, dimensions='tile')
     call v%add_attribute('units', 'degree')
     call v%add_attribute('long_name', 'tile_center_of_mass_latitude')
     call v%add_attribute("missing_value", UNDEF_REAL64)
     call v%add_attribute("_FillValue", UNDEF_REAL64)
     call v%set_deflation(DEFLATE_LEVEL)
     call metadata%add_variable('com_lat', v)
   
     do ll = 1, ng
        if (ll == 1) then
           ocn_str = ''
        else
           ocn_str = '_ocn'
        endif
   
        v = Variable(type=PFIO_INT32, dimensions='tile')
        call v%add_attribute('units', '1')
        call v%add_attribute('long_name', 'GRID'//trim(ocn_str)//'_i_index_of_tile_in_global_grid')
        call v%add_attribute("missing_value",   MAPL_UNDEFINED_INTEGER)
        call v%set_deflation(DEFLATE_LEVEL)
        call metadata%add_variable('i_indg'//trim(ocn_str), v)
   
        v = Variable(type=PFIO_INT32, dimensions='tile')
        call v%add_attribute('units', '1')
        call v%add_attribute('long_name', 'GRID'//trim(ocn_str)//'_j_index_of_tile_in_global_grid')
        call v%set_deflation(DEFLATE_LEVEL)
        call v%add_attribute("missing_value",   MAPL_UNDEFINED_INTEGER)
        call metadata%add_variable('j_indg'//trim(ocn_str), v)
   
        v = Variable(type=PFIO_REAL64, dimensions='tile')
        call v%add_attribute('units', '1')
        call v%add_attribute('long_name', 'GRID'//trim(ocn_str)//'_area_fraction_of_tile_in_grid_cell')
        call v%add_attribute("missing_value", UNDEF_REAL64)
        call v%add_attribute("_FillValue",    UNDEF_REAL64)
        call v%set_deflation(DEFLATE_LEVEL)
        call metadata%add_variable('frac_cell'//trim(ocn_str), v)
   
        v = Variable(type=PFIO_INT32, dimensions='tile')
        call v%add_attribute('units', '1')
        call v%add_attribute('long_name', 'internal_dummy_index_of_tile')
        call v%add_attribute("missing_value",  MAPL_UNDEFINED_INTEGER)
        call v%set_deflation(DEFLATE_LEVEL)
        call metadata%add_variable('dummy_index'//trim(ocn_str), v)
     enddo
   
     v = Variable(type=PFIO_INT32, dimensions='tile')
     call v%add_attribute('units', '1')
     call v%add_attribute('long_name', 'Pfafstetter_index_of_tile')
     call v%add_attribute("missing_value",   MAPL_UNDEFINED_INTEGER)
     call v%set_deflation(DEFLATE_LEVEL)
     call metadata%add_variable('pfaf_index', v)
   
     v = Variable(type=PFIO_REAL64, dimensions='tile')
     call v%add_attribute('units', 'degree')
     call v%add_attribute('long_name', 'tile_minimum_longitude')
     call v%add_attribute("missing_value", UNDEF_REAL64)
     call v%set_deflation(DEFLATE_LEVEL)
     call metadata%add_variable('min_lon', v)
   
     v = Variable(type=PFIO_REAL64, dimensions='tile')
     call v%add_attribute('units', 'degree')
     call v%add_attribute('long_name', 'tile_maximum_longitude')
     call v%add_attribute("missing_value", UNDEF_REAL64)
     call v%set_deflation(DEFLATE_LEVEL)
     call metadata%add_variable('max_lon', v)
   
     v = Variable(type=PFIO_REAL64, dimensions='tile')
     call v%add_attribute('units', 'degree')
     call v%add_attribute('long_name', 'tile_minimum_latitude')
     call v%add_attribute("missing_value", UNDEF_REAL64)
     call v%set_deflation(DEFLATE_LEVEL)
     call metadata%add_variable('min_lat', v)
   
     v = Variable(type=PFIO_REAL64, dimensions='tile')
     call v%add_attribute('units', 'degree')
     call v%add_attribute('long_name', 'tile_maximum_latitude')
     call v%add_attribute("missing_value", UNDEF_REAL64)
     call v%set_deflation(DEFLATE_LEVEL)
     call metadata%add_variable('max_lat', v)
   
     v = Variable(type=PFIO_REAL64, dimensions='tile')
     call v%add_attribute('units', 'm')
     call v%add_attribute('long_name', 'tile_mean_elevation')
     call v%add_attribute("missing_value", UNDEF_REAL64)
     call v%set_deflation(DEFLATE_LEVEL)
     call metadata%add_variable('elev', v)
   
     ! -------------------------------------------------------------------
     !  
     ! write data into nc4 file
   
     call formatter%create(File, mode=PFIO_NOCLOBBER, rc=status)
     call formatter%write(metadata,                   rc=status)
     call formatter%put_var('typ',     iTable(:,0),   rc=status)
     call formatter%put_var('area',    rTable(:,3),   rc=status)
     call formatter%put_var('com_lon', rTable(:,1),   rc=status)
     call formatter%put_var('com_lat', rTable(:,2),   rc=status)
   
     allocate(fr(ip), pfaf(ip))
     fr = UNDEF_REAL64
   
     do ll = 1, ng
        if (ng == 1) then
           if (EASE) then
              KK   = iTable(:,4)
              pfaf = KK
           else
              KK =[(k, k=1,ip)]
           endif
        else
           KK = iTable(:,5+ll)
        endif
   
        II = iTable(:,ll*2    )
        JJ = iTable(:,ll*2 + 1)
   
        where( rTable(:,3+ll) /=0.0)
           fr = rTable(:,3)/rTable(:,3+ll)
        endwhere
   
        if (ll == 1) then
          ocn_str=''
        else
          ocn_str='_ocn'
        endif
   
        if (ll == 2) then
          pfaf = MAPL_UNDEFINED_INTEGER
          where (iTable(:,0) == 100)
            pfaf = II
          endwhere
          where (iTable(:,0) == 19)
            pfaf = 190000000
          endwhere
          where (iTable(:,0) == 20)
            pfaf = 200000000
          endwhere
   
          where (iTable(:,0) /=0 )
            II = MAPL_UNDEFINED_INTEGER
            JJ = MAPL_UNDEFINED_INTEGER
            fr = UNDEF_REAL64
          endwhere
        endif
   
        call formatter%put_var('i_indg'    //trim(ocn_str), II, rc=status)
        call formatter%put_var('j_indg'    //trim(ocn_str), JJ, rc=status)
        call formatter%put_var('frac_cell' //trim(ocn_str), fr, rc=status)
        call formatter%put_var('dummy_index'//trim(ocn_str), KK, rc=status)
   
        if (EASE .or. ll == 2) call formatter%put_var('pfaf_index', pfaf, rc=status)
     enddo
   
     call formatter%put_var('min_lon', rTable(:, 6), rc=status)
     call formatter%put_var('max_lon', rTable(:, 7), rc=status)
     call formatter%put_var('min_lat', rTable(:, 8), rc=status)
     call formatter%put_var('max_lat', rTable(:, 9), rc=status)
     call formatter%put_var('elev',    rTable(:,10), rc=status)
   
     call formatter%close(rc=status)
   
     if (present(rc)) rc = status
   
   end subroutine MAPL_WriteTilingNC4

   subroutine MAPL_ReadTilingASCII(layout, FileName, GridName, NT, im, jm, n_Grids, N_PfafCat, AVR,rc)
      type(ESMF_DELayout), intent(IN)  :: LAYOUT
      character(*),        intent(IN)  :: FileName
      character(*),        intent(out) :: GridName(:)
      integer,             intent(out) :: NT
      integer,             intent(out) :: IM(:), JM(:)
      integer,             intent(out) :: n_Grids
      integer,             intent(out) :: N_PfafCat
      real,  pointer,      intent(out) :: AVR(:,:)      ! used by GEOSgcm
      integer, optional,   intent(out) :: rc

      integer :: unit, status, hdr(2), N
      real, pointer :: AVR_Transpose(:,:)

      UNIT = GETFILE(FILENAME, form='FORMATTED', RC=status)
      _VERIFY(STATUS)

! Total number of tiles in exchange grid
!---------------------------------------

      call READ_PARALLEL(layout, hdr, UNIT=UNIT, rc=status)
       _VERIFY(STATUS)
       NT        = hdr(1)
       N_PfafCat = hdr(2)
       
      call READ_PARALLEL(layout, N_GRIDS, unit=UNIT, rc=status)
      _VERIFY(STATUS)

      do N = 1, N_GRIDS
         call READ_PARALLEL(layout, GridName(N), unit=UNIT, rc=status)
         _VERIFY(STATUS)
         call READ_PARALLEL(layout, IM(N), unit=UNIT, rc=status)
         _VERIFY(STATUS)
         call READ_PARALLEL(layout, JM(N), unit=UNIT, rc=status)
         _VERIFY(STATUS)
      enddo

      if(index(GridNAME(1),'EASE') /=0 ) then
          allocate(AVR(NT,9), STAT=STATUS) ! 9 columns for EASE grid
          _VERIFY(STATUS)
          allocate(AVR_transpose(9,NT))
      else
         allocate(AVR(NT,NumGlobalVars+NumLocalVars*N_GRIDS), STAT=STATUS)
         _VERIFY(STATUS)
         allocate(AVR_transpose(NumGlobalVars+NumLocalVars*N_GRIDS,NT), STAT=STATUS)
         _VERIFY(STATUS)
      endif

      call READ_PARALLEL(layout, AVR_transpose(:,:), unit=UNIT, rc=status)
      AVR = transpose(AVR_transpose)
      deallocate(AVR_transpose)
      call FREE_FILE(UNIT)

      _RETURN(ESMF_SUCCESS)

   end subroutine MAPL_ReadTilingASCII

end module NCIOMod
