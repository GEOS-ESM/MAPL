#include "MAPL_ErrLog.h"
#define DEALOC_(A) if(associated(A))then;if(MAPL_ShmInitialized)then;call MAPL_SyncSharedMemory(rc=STATUS);call MAPL_DeAllocNodeArray(A,rc=STATUS);else;deallocate(A,stat=STATUS);endif;_VERIFY(STATUS);NULLIFY(A);endif

!BOP

! !MODULE: BinIO -- A Module to do I/O (ASCII+binary) until ESMF fully supports it


! !INTERFACE:

module  BinIOMod

  use FileIOSharedMod, only: ArrDescr, MAPL_TileMaskGet, WRITE_PARALLEL, alloc_, dealloc_
  use FileIOSharedMod, only: STD_OUT_UNIT_NUMBER, LAST_UNIT, TAKEN, MTAKEN, mname
  use FileIOSharedMod, only: not_allocated, r4_2, r4_1, r8_2, r8_1, i4_2, i4_1
  use FileIOSharedMod, only: MEM_UNITS, munit, REC
  use ESMF
  use MAPL_BaseMod
  use MAPL_SortMod
  use MAPL_CommsMod
  use MAPL_ShmemMod
  use MAPL_ExceptionHandling
  use, intrinsic :: ISO_C_BINDING
  use, intrinsic :: iso_fortran_env
  implicit none
  private


  ! public routines
  public READ_PARALLEL
  public GETFILEUNIT
  public GETFILE
  public FREE_FILE
  public MAPL_VarRead
  public MAPL_VarWrite
  public MAPL_Skip
  public MAPL_Backspace
  public MAPL_Rewind
  public MAPL_ClimUpdate 
  public MAPL_DestroyFile
  public MAPL_MemFileInquire

  include "mpif.h"

!#define TIME_MPIIO
#ifdef TIME_MPIIO
  real(kind=ESMF_KIND_R8), save :: peak_ioread_bandwidth=0
  real(kind=ESMF_KIND_R8), save :: mean_ioread_bandwidth=0
  real(kind=ESMF_KIND_R8), save :: ioread_counter=0
  real(kind=ESMF_KIND_R8), save :: peak_iowrite_bandwidth=0
  real(kind=ESMF_KIND_R8), save :: mean_iowrite_bandwidth=0
  real(kind=ESMF_KIND_R8), save :: iowrite_counter=0
#endif
  integer, parameter :: UNDEF = 999


  interface READ_PARALLEL
     module procedure READ_PARALLEL_STRING_0
     module procedure READ_PARALLEL_I4_0
     module procedure READ_PARALLEL_I4_1
     module procedure READ_PARALLEL_I4_2
     module procedure READ_PARALLEL_R4_0
     module procedure READ_PARALLEL_R4_1
     module procedure READ_PARALLEL_R4_2
     module procedure READ_PARALLEL_R8_0
     module procedure READ_PARALLEL_R8_1
     module procedure READ_PARALLEL_R8_2
  end interface

  interface ArrayScatterShm
     module procedure ArrayScatterShmR4D1
  end interface ArrayScatterShm

  interface MAPL_MemFileInquire
     module procedure InqFileMem
  end interface

  interface MAPL_VarRead
     !Binary procedures
     module procedure MAPL_StateVarRead
     module procedure MAPL_BundleRead
     module procedure MAPL_FieldRead
     module procedure MAPL_VarRead_R4_1D
     module procedure MAPL_VarRead_R4_2D
     module procedure MAPL_VarRead_R4_3d
     module procedure MAPL_VarRead_R4_4D
     module procedure MAPL_VarRead_R8_1D
     module procedure MAPL_VarRead_R8_2D
     module procedure MAPL_VarRead_R8_3D
     module procedure MAPL_VarRead_R8_4D
  end interface

  interface MAPL_VarWrite
     !Binary procedures
     module procedure MAPL_StateVarWrite
     module procedure MAPL_BundleWrite
     module procedure MAPL_FieldWrite
     module procedure MAPL_VarWrite_I4_1D
     module procedure MAPL_VarWrite_R4_1D
     module procedure MAPL_VarWrite_R4_2d
     module procedure MAPL_VarWrite_R4_3D
     module procedure MAPL_VarWrite_R4_4D
     module procedure MAPL_VarWrite_R8_1D
     module procedure MAPL_VarWrite_R8_2D
     module procedure MAPL_VarWrite_R8_3D
     module procedure MAPL_VarWrite_R8_4D
  end interface



  contains

!-READS --------------------

! Rank 0
!---------------------------
#define RANK_ 0
#define VARTYPE_ 0
#include "read_parallel.H"

!---------------------------
#define RANK_ 0
#define VARTYPE_ 1
#include "read_parallel.H"

!---------------------------
#define RANK_ 0
#define VARTYPE_ 3
#include "read_parallel.H"

!---------------------------
#define RANK_ 0
#define VARTYPE_ 4
#include "read_parallel.H"

! Rank 1
!---------------------------
#define RANK_ 1
#define VARTYPE_ 1
#include "read_parallel.H"

!---------------------------
#define RANK_ 1
#define VARTYPE_ 3
#include "read_parallel.H"

!---------------------------
#define RANK_ 1
#define VARTYPE_ 4
#include "read_parallel.H"

! Rank 2
!---------------------------
#define RANK_ 2
#define VARTYPE_ 1
#include "read_parallel.H"

!---------------------------
#define RANK_ 2
#define VARTYPE_ 3
#include "read_parallel.H"

!---------------------------
#define RANK_ 2
#define VARTYPE_ 4
#include "read_parallel.H"

  LOGICAL FUNCTION INQFILEMEM(name)
    IMPLICIT NONE
    character(LEN=*), intent(in   )           :: Name

    integer :: i
    logical :: found

    found = .false.
    do i = 3, last_unit
       if(name==Mname(i)) then
          found = .true.
          exit
       end if
    end do
    InqFileMem = found
    return
  end FUNCTION INQFILEMEM

  INTEGER FUNCTION GETFILEUNIT(name,  RC )
    IMPLICIT NONE
    character(LEN=*), intent(in   )           :: Name
    integer         , intent(  out), OPTIONAL :: RC
     
    integer :: i
    logical :: found
     
    found = .false.
    do i = 2, last_unit
       if(name==Mname(i)) then
          found = .true.
          exit
       end if
    end do
     
    if (.not. found) then
       do i = 2,last_unit
          if(.not.MTAKEN(i)) then
             found = .true.
             exit
          endif
       enddo
    end if

    if (.not. found) then
       if(present(rc)) rc = 1
       return
    endif
     
    mname(i)   = name 
    mtaken(i)  = .true.
    getfileunit = i
     
    if(present(rc)) rc = 0
    return
  end function getfileunit


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  SUBROUTINE FREE_FILE(UNIT, RC)
    implicit none
    integer         , intent(out), OPTIONAL :: RC

    integer :: UNIT

    if(UNIT < 0) then

      _ASSERT(-UNIT<=LAST_UNIT, 'illegal io unit')
      _ASSERT(MTAKEN(-UNIT), 'illegal io unit')
      MEM_units(-unit)%PREVREC=0

    ELSE

    if (UNIT == STD_OUT_UNIT_NUMBER) return
    if (UNIT /= UNDEF) then
       close(UNIT)

       IF (UNIT.LT.1 .OR. UNIT.GT.LAST_UNIT) THEN
          WRITE (0,*) ' BAD UNIT NUMBER  ZFILCLR  UNIT = ', UNIT
          _RETURN(ESMF_FAILURE)
       ELSE
          TAKEN(UNIT) = .FALSE.
          MTAKEN(UNIT) = .FALSE. 
       ENDIF
    end if

    END IF

    _RETURN(ESMF_SUCCESS)
  END SUBROUTINE FREE_FILE

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  subroutine MAPL_DestroyFile(unit,  RC )
    IMPLICIT NONE
    integer         , intent(in   )           :: unit
    integer         , intent(  out), OPTIONAL :: RC

    integer :: i,k

!ALT: Currently, this is NOP except for RAM files
    if (unit < 0) then
       ! this is RAM "file", do it!
       i = -unit
       if (associated(mem_units(i)%records)) then
          do k=1,size(mem_units(i)%records)
             call dealloc_(mem_units(i)%records(k))
          end do
          deallocate(mem_units(i)%records)
          nullify(mem_units(i)%records)
       end if
       mtaken(i)  = .false.
       mname(i) = ''
    end if

    if(present(rc)) rc = 0
    return
  end subroutine MAPL_DestroyFile
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine MAPL_StateVarRead(UNIT, STATE, NAME, arrdes, bootstrapable, RC)
    integer                     , intent(IN   ) :: UNIT
    type (ESMF_State)           , intent(INOUT) :: STATE
    character(len=*),   optional, intent(IN   ) :: NAME
    type(ArrDescr),     optional, intent(INOUT) :: ARRDES
    logical,            optional, intent(IN   ) :: bootstrapable
    integer,            optional, intent(  OUT) :: RC

! Local vars
    type (ESMF_FieldBundle)              :: bundle
    type (ESMF_Field)                    :: field
    type (ESMF_Grid)                     :: grid
    integer                              :: status
    integer                              :: I
    integer                              :: ITEMCOUNT
    type (ESMF_StateItem_Flag), pointer  :: ITEMTYPES(:)
    character(len=ESMF_MAXSTR ), pointer :: ITEMNAMES(:)
    logical, pointer                     :: DOIT(:)
    integer                              :: DIMS
    integer, pointer                     :: MASK(:) => null()
    
    logical                            :: skipReading
    integer                            :: RST
    integer                            :: dna
    logical                            :: ignoreEOF
    logical                            :: bootstrapable_
    logical                            :: isPresent

    integer, allocatable :: orderlist(:)
    integer :: jj
    character(len=ESMF_MAXSTR)           :: attrName
    character(len=ESMF_MAXSTR), allocatable :: currList(:)
    integer                                 :: natt
    
    call ESMF_StateGet(STATE,ITEMCOUNT=ITEMCOUNT,RC=STATUS)
    _VERIFY(STATUS)

    _ASSERT(ITEMCOUNT>0, 'itemcount must be > 0')

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
       _ASSERT(count(DOIT)/=0, 'cont(doit) must be > 0')
    else
       DOIT = .true.
    endif

    attrName = MAPL_StateItemOrderList
    call ESMF_AttributeGet(state, NAME=attrName, itemcount=natt, RC=STATUS)
    _VERIFY(STATUS)

    _ASSERT(natt > 0, 'natt not > 0')
    allocate(orderlist(natt), stat=status)
    _VERIFY(STATUS)
    allocate(currList(natt), stat=status)
    _VERIFY(STATUS)

    ! get the current list
    call ESMF_AttributeGet(state, NAME=attrName, VALUELIST=currList, rc=status)
    _VERIFY(STATUS)

    orderList = -1 ! not found
    do i = 1, natt
       ! search loop
       do jj = 1, ITEMCOUNT
          if (itemNames(jj) == currList(i)) then
             orderList(i) = jj
             exit
          end if
       end do
    end do

    deallocate(currList)

    if (present(bootstrapable)) then
       bootstrapable_ = bootstrapable
    else
       bootstrapable_ = .false.
    end if

    do JJ = 1, natt

       I = ORDERLIST(JJ)
       if (DOIT(I)) then


#ifdef TIME_MPIIO
    call write_parallel(itemnames(i))
#endif

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
             skipReading = (RST == MAPL_RestartSkip)
             if (skipReading) cycle

             if (RST == MAPL_RestartRequired) then
                bootstrapable_ = .true.
             end if
             call MAPL_BundleRead(unit, bundle, arrdes=arrdes, &
                  bootstrapable=bootstrapable_, rc=status)
             _VERIFY(STATUS)

          else if (ITEMTYPES(I) == ESMF_StateItem_Field) then
             call ESMF_StateGet(state, itemnames(i), field, rc=status)
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

             if (skipReading) cycle
             call ESMF_AttributeGet(field, name='doNotAllocate', isPresent=isPresent, rc=status)
             _VERIFY(STATUS)
             if (isPresent) then
                call ESMF_AttributeGet(field, name='doNotAllocate', value=dna, rc=status)
                _VERIFY(STATUS)
                skipReading = (dna /= 0)
             end if
             if (skipReading) cycle

             ignoreEOF = .false.
             if (bootstrapable_ .and. (RST == MAPL_RestartOptional)) then
                ignoreEOF = .true.
             end if

             if(.not.associated(MASK)) then
                call ESMF_AttributeGet(field, name='DIMS', value=DIMS, rc=status)
                _VERIFY(STATUS)
                if (DIMS == MAPL_DimsTileOnly .or. DIMS == MAPL_DimsTileTile) then
                   call ESMF_FieldGet   (field, grid=grid, rc=status)
                   _VERIFY(STATUS)
                   call MAPL_TileMaskGet(grid,  mask, rc=status)
                   _VERIFY(STATUS)
!@                else
!@                   allocate(Mask(1))
                endif
             endif
      
             call MAPL_FieldRead(unit, field, arrdes=arrdes, HomePE=Mask, ignoreEOF=ignoreEOF, rc=status)
             _VERIFY(STATUS)

!ALT          else
!ALT             _ASSERT(.false.,'failed mapl_statevarread')

          end if

       end if

    end do

    deallocate(orderlist)
    deallocate(ITEMNAMES)
    deallocate(ITEMTYPES)
    deallocate(     DOIT)
    if(associated(MASK)) then
       DEALOC_(MASK)
    end if

    _RETURN(ESMF_SUCCESS)
  end subroutine MAPL_StateVarRead
!---------------------------


  subroutine MAPL_BundleRead(UNIT,BUNDLE, ARRDES, BOOTSTRAPABLE, RC)
    integer                     , intent(IN   ) :: UNIT
    type (ESMF_FieldBundle)     , intent(INOUT) :: BUNDLE
    type(ArrDescr),    optional , intent(INOUT) :: ARRDES
    logical,           optional , intent(IN   ) :: BOOTSTRAPABLE
    integer,           optional , intent(  OUT) :: RC

    integer                            :: status
    integer                            :: J, N, fieldCount
    type (ESMF_Field)                  :: field
    character(len=ESMF_MAXSTR),allocatable  :: nameList(:)
    character(len=ESMF_MAXSTR)              :: BundleName
    integer                            :: RST
    logical                            :: ignoreEOF
    logical                            :: skipReading
    logical                            :: bootstrapable_
    logical                            :: isPresent

    call ESMF_FieldBundleGet(bundle, fieldCount=N, name=BundleName, rc=STATUS)
    _VERIFY(STATUS)
    allocate(namelist(N), stat=status)
    _VERIFY(STATUS)
    call ESMF_FieldBundleGet(bundle, fieldNameList=nameList, fieldCount=FieldCount,  rc=STATUS)
    _VERIFY(STATUS)
    _ASSERT(N==fieldCount, 'inconsistent fieldCount')

    if (present(bootstrapable)) then
       bootstrapable_ = bootstrapable
    else
       bootstrapable_ = .false.
    end if

    do J = 1, N
       call MAPL_FieldBundleGet(bundle, fieldIndex=J, field=field, rc=status)
       _VERIFY(STATUS)

       call ESMF_AttributeGet(field, name='RESTART', isPresent=isPresent, rc=status)
       _VERIFY(STATUS)
       if (isPresent) then
          call ESMF_AttributeGet(field, name='RESTART', value=RST, rc=status)
          _VERIFY(STATUS)
       else
          RST = MAPL_RestartOptional
       end if
       skipReading = (RST == MAPL_RestartSkip)
       if (skipReading) cycle
       
       ignoreEOF=.false.
       if (bootstrapable_ .and. (RST == MAPL_RestartOptional)) then
          ignoreEOF = .true.
       end if
       
       call MAPL_FieldRead(unit, field, arrdes=ARRDES,  ignoreEOF=ignoreEOF, rc=status)
       _VERIFY(STATUS)

    end do

    _RETURN(ESMF_SUCCESS)
  end subroutine MAPL_BundleRead


  subroutine MAPL_FieldRead(UNIT,FIELD, ARRDES, HomePE, ignoreEOF, RC)
    integer                     , intent(IN   ) :: UNIT
    type (ESMF_Field)           , intent(INOUT) :: field
    type(ArrDescr),     optional, intent(INOUT) :: ARRDES
    integer, target,   optional , intent(IN   ) :: HomePE(:)
    logical,           optional , intent(IN   ) :: ignoreEOF
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
    real(KIND=ESMF_KIND_R8), pointer, dimension(:,:,:,:)  :: vr8_4d
    type(ESMF_TypeKind_Flag)           :: tk
    character(len=ESMF_MAXSTR)         :: FORMATTED
    integer                            :: dims
    integer                            :: J, K
    integer, pointer                   :: mask(:)
    type (ESMF_DistGrid)               :: distGrid
    integer                            :: stat
    logical                            :: ignoreEOF_

    if (unit < 0 .or. present(arrdes)) then
       FORMATTED = "NO"
    else
       inquire(unit=UNIT, formatted=FORMATTED)
    end if

    if (present(ignoreEOF)) then
       ignoreEOF_ = ignoreEOF
    else
       ignoreEOF_ = .false.
    end if

    call ESMF_FieldGet(field, grid=grid, rc=status)
    _VERIFY(STATUS)
    call ESMF_GridGet(grid, distGrid=distGrid, rc=STATUS)
    _VERIFY(STATUS)
    call ESMF_DistGridGet(distGrid, delayout=layout, rc=STATUS)
    _VERIFY(STATUS)

    if (ignoreEOF_ .and. (unit > 0)) then
       ! test for end-of-file by 
       ! making a blank read followed by backspace

       if (MAPL_am_i_root(layout)) then
          read (UNIT, IOSTAT=status)
       end if
       call MAPL_CommsBcast(layout, status, n=1, ROOT=MAPL_Root, rc=stat)
       _VERIFY(STAT)

       if (status == IOSTAT_END) then
          _RETURN(ESMF_SUCCESS)
       end if
       _VERIFY(STATUS)

       call MAPL_Backspace(UNIT, layout, rc=status)
       _VERIFY(STATUS)
    end if

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
                call MAPL_VarRead(unit, grid, var_1d, arrdes=arrdes, mask=mask, rc=status)
                _VERIFY(STATUS)
             else if (DIMS == MAPL_DimsVertOnly .or. DIMS==MAPL_DimsNone) then
                call READ_PARALLEL(layout, var_1d, unit, arrdes=arrdes, rc=status)
             else 
                _RETURN(ESMF_FAILURE)
             endif
          end if
       else
          call ESMF_ArrayGet(array, localDE=0, farrayptr=vr8_1d, rc=status)
          _VERIFY(STATUS)
          if (associated(vr8_1d)) then
             if (DIMS == MAPL_DimsTileOnly .or. DIMS == MAPL_DimsTileTile) then
                call MAPL_VarRead(unit, grid, vr8_1d, arrdes=arrdes, mask=mask, rc=status)
             else if (DIMS == MAPL_DimsVertOnly .or. DIMS==MAPL_DimsNone) then
                call READ_PARALLEL(layout, vr8_1d, unit, arrdes=arrdes, rc=status)
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
             if (FORMATTED=="YES") THEN
                call READ_PARALLEL(layout, &
                     var_2d(lbound(var_2d,1),:), unit, rc=status)
             else
                if (DIMS == MAPL_DimsTileOnly) then
                   do J = 1,size(var_2d,2)
                      call MAPL_VarRead(unit, grid, var_2d(:,J), arrdes=arrdes, mask=mask, rc=status)
                   end do
                else if (DIMS == MAPL_DimsTileTile) then
                   call MAPL_VarRead(unit, grid, var_2d, arrdes=arrdes, mask=mask, rc=status)
                else
                   call MAPL_VarRead(unit, grid, var_2d, arrdes=arrdes, rc=status)
                end if
             end if
          end if
       else
          call ESMF_ArrayGet(array, localDE=0, farrayptr=vr8_2d, rc=status)
          _VERIFY(STATUS)
          if (associated(vr8_2d)) then !ALT: temp kludge
             if (FORMATTED=="YES") THEN
                call READ_PARALLEL(layout, &
                     vr8_2d(lbound(vr8_2d,1),:), unit, rc=status)
             else
                if (DIMS == MAPL_DimsTileOnly) then
                   do J = 1,size(vr8_2d,2)
                      call MAPL_VarRead(unit, grid, vr8_2d(:,J), arrdes=arrdes, mask=mask, rc=status)
                   end do
                else if (DIMS == MAPL_DimsTileTile) then
                   call MAPL_VarRead(unit, grid, vr8_2d, arrdes=arrdes, mask=mask, rc=status)
                else
                   call MAPL_VarRead(unit, grid, vr8_2d, arrdes=arrdes, rc=status)
                end if
             end if
          end if
       endif
    else if (rank == 3) then
       if (tk == ESMF_TYPEKIND_R4) then
          call ESMF_ArrayGet(array, localDE=0, farrayptr=var_3d, rc=status)
          _VERIFY(STATUS)
          if (associated(var_3d)) then !ALT: temp kludge
             if (FORMATTED=="YES") THEN
                call READ_PARALLEL(layout, &
                     var_3d(lbound(var_3d,1),lbound(var_3d,2),:), unit)
             else
                if (DIMS == MAPL_DimsTileOnly) then
                   do J = 1,size(var_3d,2)
                      do K = 1,size(var_3d,3)
                         call MAPL_VarRead(unit, grid, var_3d(:,J,K), arrdes=arrdes, mask=mask, rc=status)
                      end do
                   end do
                else
                   call MAPL_VarRead(unit, grid, var_3d, arrdes=arrdes, rc=status)
                end if
             endif
          end if
       else
          call ESMF_ArrayGet(array, localDE=0, farrayptr=vr8_3d, rc=status)
          _VERIFY(STATUS)
          if (associated(vr8_3d)) then !ALT: temp kludge
             if (FORMATTED=="YES") THEN
                call READ_PARALLEL(layout, &
                     vr8_3d(lbound(vr8_3d,1),lbound(vr8_3d,2),:), unit)
             else
                if (DIMS == MAPL_DimsTileOnly) then
                   do J = 1,size(vr8_3d,2)
                      do K = 1,size(vr8_3d,3)
                         call MAPL_VarRead(unit, grid, vr8_3d(:,J,K), arrdes=arrdes, mask=mask, rc=status)
                      end do
                   end do
                else
                   call MAPL_VarRead(unit, grid, vr8_3d, arrdes=arrdes, rc=status)
                end if
             endif
          end if
       endif
    else if (rank == 4) then
       if (tk == ESMF_TYPEKIND_R4) then
          call ESMF_ArrayGet(array, localDE=0, farrayptr=var_4d, rc=status)
          _VERIFY(STATUS)
          call MAPL_VarRead(unit, grid, var_4d, rc=status)
       else
          call ESMF_ArrayGet(array, localDE=0, farrayptr=vr8_4d, rc=status)
          _VERIFY(STATUS)
          call MAPL_VarRead(unit, grid, vr8_4d, rc=status)
       end if
    else
       _ASSERT(.false., "ERROR: unsupported RANK")
    endif
    _VERIFY(STATUS)

    if (DIMS == MAPL_DimsTileOnly .or. DIMS == MAPL_DimsTileTile) then
       if(.not.present(HomePE)) then
          DEALOC_(mask)
       end if
    end if

    _RETURN(ESMF_SUCCESS)
  end subroutine MAPL_FieldRead

!---------------------------

  subroutine MAPL_VarRead_R4_1d(UNIT, GRID, A, MASK, arrdes, RC)

    integer                     , intent(IN   ) :: UNIT
    type (ESMF_Grid)            , intent(INout) :: GRID
    real(kind=ESMF_KIND_R4)     , intent(  OUT) :: A(:)
    integer,           optional , intent(IN   ) :: MASK(:)
    type(ArrDescr),     optional, intent(INOUT) :: ARRDES
    integer,           optional , intent(  OUT) :: RC

! Local variables
    real(kind=ESMF_KIND_R4),  pointer     :: VAR(:)
    integer                               :: IM_WORLD
    integer                               :: status
    integer                               :: DIMS(ESMF_MAXGRIDDIM)
    type (ESMF_DELayout)                  :: layout
    type (ESMF_DistGrid)                  :: distgrid
    integer, allocatable                  :: msk(:), sendcounts(:), displs(:)
    integer, allocatable                  :: idx(:)
    integer                               :: nrdrs, mype,  npes, recvcount
    integer                               :: mypeRd
    integer                               :: Rsize, first, last
    integer(KIND=MPI_OFFSET_KIND)         :: offset
    integer(KIND=MPI_OFFSET_KIND)         :: loffset
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

    integer :: numread, mpistatus(MPI_STATUS_SIZE)
    integer :: cnt
    logical :: amIRoot

    if(present(arrdes)) then
       _ASSERT(present(mask), 'mask must be present if arrdes is present')

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
       call ESMF_GridGet(grid, distGrid=distGrid, rc=STATUS)
       _VERIFY(STATUS)
       call ESMF_DistGridGet(distGrid, delayout=layout, rc=STATUS)
       _VERIFY(STATUS)
       call MAPL_CommsBcast(layout, nrdrs, 1, 0, rc = status)

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
          if(arrdes%offset<=0) then
             offset = 4 
          else
             offset = arrdes%offset 
          endif

          loffset = offset + (first-1)*4
          cnt = Rsize
          call MPI_FILE_READ_AT_ALL(UNIT, loffset, VAR, cnt, MPI_REAL, mpistatus, STATUS)
          _VERIFY(STATUS)
          call MPI_GET_COUNT( mpistatus, MPI_REAL, numread, STATUS )
          _VERIFY(STATUS)
          _ASSERT(cnt == numread, 'inconsistent numread')
#ifdef DEBUG_MPIIO
          write(*,'(3i,1f)') IM_WORLD, loffset, numread, VAR(1)
#endif

          _ASSERT( (lbound(mask,1) <= first), 'location not in bounds')
          _ASSERT( (ubound(mask,1) >= last ), 'location not in bounds')
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

          arrdes%offset = offset + IM_WORLD*4 + 8
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

       call MPI_Barrier(arrdes%ioscattercomm, status)
       _VERIFY(STATUS)
       call MPI_GROUP_FREE (GROUP, STATUS)
       _VERIFY(STATUS)
       deallocate(var,msk)
       deallocate (r2g)
       deallocate(sendcounts)
       if(arrdes%readers_comm /= MPI_COMM_NULL) then
          deallocate(idx)
       end if

    elseif(unit < 0) then

       _ASSERT(-UNIT<=LAST_UNIT, 'illegal unit')
       munit => MEM_units(-unit)
       munit%prevrec = munit%prevrec + 1
       _ASSERT(associated(munit%Records(munit%prevrec)%R4_1), 'unassociated pointer')
       _ASSERT(size(A)==size(munit%Records(munit%prevrec)%R4_1), 'inconsistent array size')
       A = munit%Records(munit%prevrec)%R4_1

    else

       call MAPL_GridGet(grid, globalCellCountPerDim=DIMS, RC=STATUS)
       _VERIFY(STATUS)

       call ESMF_GridGet(grid, distGrid=distGrid, rc=STATUS)
       _VERIFY(STATUS)
       call ESMF_DistGridGet(distGrid, delayout=layout, rc=STATUS)
       _VERIFY(STATUS)

       amIRoot = MAPL_am_i_root(layout)
       IM_WORLD = DIMS(1)

       if (.not. MAPL_ShmInitialized) then
          if (amIRoot) then
             allocate(VAR(IM_WORLD), stat=status)
             _VERIFY(STATUS)
          else
             allocate(VAR(0), stat=status)
             _VERIFY(STATUS)
          end if
       else
          call MAPL_AllocNodeArray(var,(/IM_WORLD/),rc=STATUS)
          _VERIFY(STATUS)
       end if

       if (amIRoot) then
          read (UNIT, IOSTAT=status) VAR
          _VERIFY(STATUS)
       end if

       if (.not. MAPL_ShmInitialized) then
          call ArrayScatter(A, VAR, grid, mask=mask, rc=status)
          _VERIFY(STATUS)
    
          deallocate(VAR)
       else
          call ArrayScatterShm(A, VAR, grid, mask=mask, rc=status)
          _VERIFY(STATUS)
          call MAPL_DeAllocNodeArray(VAR,rc=STATUS)
          _VERIFY(STATUS)
       end if
    end if
    
    _RETURN(ESMF_SUCCESS)
  end subroutine MAPL_VarRead_R4_1d

!---------------------------

  subroutine MAPL_VarRead_R4_2d(UNIT, GRID, A, MASK, arrdes, RC)

    integer                     , intent(IN   ) :: UNIT
    type (ESMF_Grid)            , intent(INout) :: GRID
    real(kind=ESMF_KIND_R4)     , intent(  OUT) :: A(:,:)
    integer,           optional , intent(IN   ) :: MASK(:)
    type(ArrDescr),     optional, intent(INOUT) :: ARRDES
    integer,           optional , intent(  OUT) :: RC

! Local variables
    real(kind=ESMF_KIND_R4),  allocatable :: VAR(:,:)
    real(kind=ESMF_KIND_R4),  pointer     :: VAR1d(:)
    integer                               :: IM_WORLD
    integer                               :: JM_WORLD
    integer                               :: status
    integer                               :: gridRank
    integer                               :: DIMS(ESMF_MAXGRIDDIM)
    type (ESMF_DELayout)                  :: layout
    type (ESMF_DistGrid)                  :: distGRID

    real(kind=ESMF_KIND_R4),  allocatable :: buf(:)
    integer                               :: I,J,N,K,L,myrow,myiorank,ndes_x
    integer(kind=MPI_OFFSET_KIND)         :: offset
    integer                               :: jsize, jprev, num_io_rows
    integer, allocatable                  :: sendcounts(:), displs(:)

    integer :: numread, mpistatus(MPI_STATUS_SIZE)
    integer :: cnt
    logical :: am_i_root

#ifdef TIME_MPIIO
    real(kind=ESMF_KIND_R8) :: itime_beg, itime_end, bwidth
#endif

#ifdef TIME_MPIIO
  call MPI_BARRIER(MPI_COMM_WORLD,STATUS)
  _VERIFY(STATUS)
  itime_beg = MPI_Wtime(STATUS)
  _VERIFY(STATUS)
#endif

    if(present(arrdes)) then

       if(present(mask)) then
          JM_WORLD = size(A,2)

!          arrdes%offset = 0

          do j=1,jm_world
             call MAPL_VarRead(Unit, Grid, a(:,j), mask, arrdes, rc=status)
             arrdes%offset = arrdes%offset - 8
          enddo

          arrdes%offset = arrdes%offset + 8

       else

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

          if(arrdes%offset<=0) then
             offset = 4 
          else
             offset = arrdes%offset 
          endif

          offset = offset + (arrdes%j1(myrow+1)-1)*IM_WORLD*4
          cnt = IM_WORLD*jsize
          call MPI_FILE_READ_AT_ALL(UNIT, offset, VAR, cnt, MPI_REAL, mpistatus, STATUS)
          _VERIFY(STATUS)
          call MPI_GET_COUNT( mpistatus, MPI_REAL, numread, STATUS ) 
          _VERIFY(STATUS)
          _ASSERT(cnt == numread, 'inconsistent numread')
          offset = offset - (arrdes%j1(myrow+1)-1)*IM_WORLD*4

          arrdes%offset = offset + IM_WORLD*JM_WORLD*4 + 8

#ifdef DEBUG_MPIIO
          print*, offset, numread, IM_WORLD*jsize, VAR(1,1)
#endif

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

       end if

!DSK avoid "Attempt to fetch from allocatable variable BUF when it is not allocated"
       if(myiorank/=0) then
          allocate(buf(0), stat=status)
          _VERIFY(STATUS)
       endif

       call mpi_scatterv( buf, sendcounts, displs, MPI_REAL, &
            a,  size(a),  MPI_REAL, &
            0, arrdes%ioscattercomm, status )
       _VERIFY(STATUS)

       if(myiorank==0) then 
          deallocate(VAR, stat=status)
          _VERIFY(STATUS)
!          deallocate(buf, stat=status)
!          _VERIFY(STATUS)
       endif
       deallocate(buf, stat=status)
       _VERIFY(STATUS)
       deallocate (sendcounts, displs, stat=status)
       _VERIFY(STATUS)

       end if

    elseif(unit < 0) then

      _ASSERT(-UNIT<=LAST_UNIT, 'illegal unit')
      munit => MEM_units(-unit)
      munit%prevrec = munit%prevrec + 1
      _ASSERT(associated(munit%Records(munit%prevrec)%R4_2), 'pointer not associated')
      _ASSERT(size(A)==size(munit%Records(munit%prevrec)%R4_2), 'inconsistent array size')
      A = munit%Records(munit%prevrec)%R4_2

    else

    call ESMF_GridGet(GRID, dimCount=gridRank, rc=STATUS)
    _VERIFY(STATUS)
    call MAPL_GridGet(GRID, globalCellCountPerDim=DIMS, RC=STATUS)
    _VERIFY(STATUS)

    IM_WORLD = DIMS(1)
    JM_WORLD = DIMS(2)
    if (present(MASK)) JM_WORLD=size(A,2)

    call ESMF_GridGet(grid, distGrid=distGrid, rc=status)
    _VERIFY(STATUS)
    call ESMF_DistGridGet(distGrid, delayout=layout, rc=status)
    _VERIFY(STATUS)

    am_i_root = MAPL_am_i_root(layout)
    if (am_i_root) then
       allocate(VAR(IM_WORLD,JM_WORLD), stat=status)
       _VERIFY(STATUS)
       read (UNIT, IOSTAT=status) VAR
       _VERIFY(STATUS)
    else
       allocate(VAR(0,JM_WORLD), stat=status)
       _VERIFY(STATUS)
    end if

    if (MAPL_ShmInitialized .and. present(mask)) then
       call MAPL_AllocNodeArray(var1d,(/IM_WORLD/),rc=STATUS)
       _VERIFY(STATUS)
       do j=1,JM_WORLD
          if (am_i_root) then
             var1d = var(:,j)
          end if
          call ArrayScatterShm(A(:,j), VAR1d, grid, mask=mask, rc=status)
          _VERIFY(STATUS)
       end do
       call MAPL_DeAllocNodeArray(VAR1d,rc=STATUS)
       _VERIFY(STATUS)

    else
       call ArrayScatter(A, VAR, grid, mask=mask, rc=status)
       _VERIFY(STATUS)
    end if

    deallocate(VAR)
    _VERIFY(STATUS)

    END IF

#ifdef TIME_MPIIO
  call MPI_BARRIER(MPI_COMM_WORLD,STATUS)
  _VERIFY(STATUS)
  itime_end = MPI_Wtime(STATUS)
  _VERIFY(STATUS)
  bwidth = REAL(IM_WORLD*JM_WORLD*4/1024.0/1024.0,kind=8)
  bwidth = bwidth/(itime_end-itime_beg)
  if (bwidth > peak_ioread_bandwidth) peak_ioread_bandwidth = bwidth
  mean_ioread_bandwidth = (mean_ioread_bandwidth + bwidth)
  ioread_counter=ioread_counter+1
  if (mod(ioread_counter,72.d0)==0) then
  if (MAPL_AM_I_Root()) write(*,'(a64,3es11.3)') 'MPIIO Read Bandwidth (MB per second): ', peak_ioread_bandwidth, bwidth, mean_ioread_bandwidth/ioread_counter
  endif
#endif
    
    _RETURN(ESMF_SUCCESS)
  end subroutine MAPL_VarRead_R4_2d

!---------------------------
  subroutine MAPL_VarRead_R4_3d(UNIT, GRID, A, Arrdes, RC)

    integer                     , intent(IN   ) :: UNIT
    type (ESMF_Grid)            , intent(INout) :: GRID
    real(kind=ESMF_KIND_R4)     , intent(  OUT) :: A(:,:,:)
    type(ArrDescr),     optional, intent(INOUT) :: ARRDES
    integer,           optional , intent(  OUT) :: RC

! Local variables

    integer                               :: status

    integer :: L

    do L = 1, size(A,3)
       call MAPL_VarRead(UNIT, GRID, A(:,:,L), ARRDES=arrdes, rc=status)
       _VERIFY(STATUS)
    end do

    _RETURN(ESMF_SUCCESS)
  end subroutine MAPL_VarRead_R4_3d
  
!---------------------------
  subroutine MAPL_VarRead_R4_4d(UNIT, GRID, A, Arrdes, RC)

    integer                     , intent(IN   ) :: UNIT
    type (ESMF_Grid)            , intent(INout) :: GRID
    real(kind=ESMF_KIND_R4)     , intent(  OUT) :: A(:,:,:,:)
    type(ArrDescr),     optional, intent(INOUT) :: ARRDES
    integer,           optional , intent(  OUT) :: RC

! Local variables

    integer                               :: status

    integer :: L

    do L = 1, size(A,4)
       call MAPL_VarRead(UNIT, GRID, A(:,:,:,L), ARRDES=arrdes, rc=status)
       _VERIFY(STATUS)
    end do

    _RETURN(ESMF_SUCCESS)
  end subroutine MAPL_VarRead_R4_4d
  
!---------------------------
  subroutine MAPL_VarRead_R8_1d(UNIT, GRID, A, MASK, arrdes, RC)

    integer                     , intent(IN   ) :: UNIT
    type (ESMF_Grid)            , intent(INout) :: GRID
    real(kind=ESMF_KIND_R8)     , intent(  OUT) :: A(:)
    integer,           optional , intent(IN   ) :: MASK(:)
    type(ArrDescr),     optional, intent(INOUT) :: ARRDES
    integer,           optional , intent(  OUT) :: RC

! Local variables
    real(kind=ESMF_KIND_R8),  allocatable :: VAR(:)
    integer                               :: IM_WORLD
    integer                               :: status
    integer                               :: DIMS(ESMF_MAXGRIDDIM)
    type (ESMF_DELayout)                  :: layout
    type (ESMF_DistGrid)                  :: distGRID
    integer, allocatable                  :: msk(:), sendcounts(:), displs(:)
    integer, allocatable                  :: idx(:)
    integer                               :: nrdrs, mype,  npes, recvcount
    integer                               :: mypeRd
    integer                               :: Rsize, first, last
    integer(KIND=MPI_OFFSET_KIND)         :: offset
    integer(KIND=MPI_OFFSET_KIND)         :: loffset
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

    integer :: numread, mpistatus(MPI_STATUS_SIZE)
    integer :: cnt

    if(present(arrdes)) then
       _ASSERT(present(mask), 'mask must be present if arrdes is present')

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
       call ESMF_GridGet(grid, distGrid=distGrid, rc=STATUS)
       _VERIFY(STATUS)
       call ESMF_DistGridGet(distGrid, delayout=layout, rc=STATUS)
       _VERIFY(STATUS)
       call MAPL_CommsBcast(layout, nrdrs, 1, 0, rc = status)

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
          if(arrdes%offset<=0) then
             offset = 4 
          else
             offset = arrdes%offset 
          endif

          loffset = offset + (first-1)*8
          cnt = Rsize
          call MPI_FILE_READ_AT_ALL(UNIT, loffset, VAR, cnt, &
               MPI_DOUBLE_PRECISION, mpistatus, STATUS)
          _VERIFY(STATUS)
          call MPI_GET_COUNT( mpistatus, MPI_DOUBLE_PRECISION, numread, STATUS )
          _VERIFY(STATUS)
          _ASSERT(cnt == numread, 'inconsistent numread')
#ifdef DEBUG_MPIIO
          write(*,'(3i,1f)') IM_WORLD, loffset, numread, VAR(1)
#endif

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

          arrdes%offset = offset + IM_WORLD*8 + 8
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
                call MPI_SCATTERV( var, activesendcounts, displs, &
                                   MPI_DOUBLE_PRECISION, &
                                   dummy,   recvcount,  MPI_DOUBLE_PRECISION, &
                                   ntransl, thiscomm,    status )
             else
                call MPI_SCATTERV( var, activesendcounts, displs, &
                                   MPI_DOUBLE_PRECISION, &
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

    elseif(unit < 0) then

       _ASSERT(-UNIT<=LAST_UNIT, 'illegal unit')
       munit => MEM_units(-unit)
       munit%prevrec = munit%prevrec + 1
       _ASSERT(associated(munit%Records(munit%prevrec)%R8_1), 'pointer not associated')
       _ASSERT(size(A)==size(munit%Records(munit%prevrec)%R8_1), 'inconsistent array size')
       A = munit%Records(munit%prevrec)%R8_1

    else


    call MAPL_GridGet(GRID, globalCellCountPerDim=DIMS, RC=STATUS)
    _VERIFY(STATUS)

    IM_WORLD = DIMS(1)

    allocate(VAR(IM_WORLD), stat=status)
    _VERIFY(STATUS)
    call ESMF_GridGet(grid, distGrid=distGrid, rc=status)
    _VERIFY(STATUS)
    call ESMF_DistGridGet(distGrid, delayout=layout, rc=status)
    _VERIFY(STATUS)

    if (MAPL_am_i_root(layout)) then
       read (UNIT, IOSTAT=status) VAR
       _VERIFY(STATUS)
    end if

    call ArrayScatter(A, VAR, grid, mask=mask, rc=status)
    _VERIFY(STATUS)
    
    deallocate(VAR)

    end if
    
    _RETURN(ESMF_SUCCESS)
  end subroutine MAPL_VarRead_R8_1d

!---------------------------


  subroutine MAPL_VarRead_R8_2d(UNIT, GRID, A, MASK, arrdes, RC)

    integer                     , intent(IN   ) :: UNIT
    type (ESMF_Grid)            , intent(INout) :: GRID
    real(kind=ESMF_KIND_R8)     , intent(  OUT) :: A(:,:)
    integer,           optional , intent(IN   ) :: MASK(:)
    type(ArrDescr),     optional, intent(INOUT) :: ARRDES
    integer,           optional , intent(  OUT) :: RC

! Local variables
    real(kind=ESMF_KIND_R8),  allocatable :: VAR(:,:)
    integer                               :: IM_WORLD
    integer                               :: JM_WORLD
    integer                               :: status
    integer                               :: DIMS(ESMF_MAXGRIDDIM)
    integer                               :: gridRank
    type (ESMF_DELayout)                  :: layout
    type (ESMF_DistGrid)                  :: distGRID

    real(kind=ESMF_KIND_R8),  allocatable :: buf(:)
    integer                               :: I,J,N,K,L,myrow,myiorank,ndes_x
    integer(kind=MPI_OFFSET_KIND)         :: offset
    integer                               :: jsize, jprev
    integer                               :: num_io_rows
    integer, allocatable                  :: sendcounts(:), displs(:)


    integer :: numread, mpistatus(MPI_STATUS_SIZE)
    integer :: cnt

#ifdef TIME_MPIIO
    real(kind=ESMF_KIND_R8) :: itime_beg, itime_end, bwidth
#endif
#ifdef TIME_MPIIO
  call MPI_BARRIER(MPI_COMM_WORLD,STATUS)
  _VERIFY(STATUS)
  itime_beg = MPI_Wtime(STATUS)
  _VERIFY(STATUS)
#endif

    if(present(arrdes)) then

       if(present(mask)) then
          JM_WORLD = size(A,2)

          arrdes%offset = 0

          do j=1,jm_world
             call MAPL_VarRead(Unit, Grid, a(:,j), mask, arrdes, rc=status)
             arrdes%offset = arrdes%offset - 8
          enddo

          arrdes%offset = arrdes%offset + 8
       else

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

          if(arrdes%offset<=0) then
             offset = 4
          else
             offset = arrdes%offset
          endif

          offset = offset + (arrdes%j1(myrow+1)-1)*IM_WORLD*8
          cnt = IM_WORLD*jsize
          call MPI_FILE_READ_AT_ALL(UNIT, offset, VAR, cnt, MPI_DOUBLE_PRECISION, mpistatus, STATUS)
          _VERIFY(STATUS)
          call MPI_GET_COUNT( mpistatus, MPI_DOUBLE_PRECISION, numread, STATUS )
          _VERIFY(STATUS)
          _ASSERT(cnt == numread, 'inconsistent numread')
          offset = offset - (arrdes%j1(myrow+1)-1)*IM_WORLD*8

          arrdes%offset = offset + IM_WORLD*JM_WORLD*8 + 8

#ifdef DEBUG_MPIIO
         print*, offset, numread, VAR(1,1)
#endif

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

       end if

!DSK avoid "Attempt to fetch from allocatable variable BUF when it is not allocated"
       if(myiorank/=0) then
          allocate(buf(0), stat=status)
          _VERIFY(STATUS)
       endif

       call mpi_scatterv( buf, sendcounts, displs, MPI_DOUBLE_PRECISION, &
                          a,  size(a),  MPI_DOUBLE_PRECISION, &
                          0, arrdes%ioscattercomm, status )
       _VERIFY(STATUS)

       if(myiorank==0) then
          deallocate(VAR, stat=status)
          _VERIFY(STATUS)
!          deallocate(buf, stat=status)
!          _VERIFY(STATUS)
       endif
       deallocate(buf, stat=status)
       _VERIFY(STATUS)
       deallocate (sendcounts, displs, stat=status)
       _VERIFY(STATUS)

       endif

    elseif(unit < 0) then

      _ASSERT(-UNIT<=LAST_UNIT, 'illegal unit')
      munit => MEM_units(-unit)
      munit%prevrec = munit%prevrec + 1
      _ASSERT(associated(munit%Records(munit%prevrec)%R8_2), 'pointer not associated')
      _ASSERT(size(A)==size(munit%Records(munit%prevrec)%R8_2), 'array size mismatch')
      A = munit%Records(munit%prevrec)%R8_2

    else


    call ESMF_GridGet(GRID, dimCount=gridRank, rc=STATUS)
    _VERIFY(STATUS)
    call MAPL_GridGet(GRID, globalCellCountPerDim=DIMS, RC=STATUS)
    _VERIFY(STATUS)

    IM_WORLD = DIMS(1)
    JM_WORLD = DIMS(2)
    if(present(MASK)) JM_WORLD=size(A,2)


    allocate(VAR(IM_WORLD,JM_WORLD), stat=status)
    _VERIFY(STATUS)

    call ESMF_GridGet(grid, distGrid=distGrid, rc=status)
    _VERIFY(STATUS)
    call ESMF_DistGridGet(distGrid, delayout=layout, rc=status)
    _VERIFY(STATUS)

    if (MAPL_am_i_root(layout)) then
       read (UNIT, IOSTAT=status) VAR
       _VERIFY(STATUS)
    end if
    call ArrayScatter(A, VAR, grid, mask=mask, rc=status)
    _VERIFY(STATUS)
    
    deallocate(VAR)

    END IF

#ifdef TIME_MPIIO
  call MPI_BARRIER(MPI_COMM_WORLD,STATUS)
  _VERIFY(STATUS)
  itime_end = MPI_Wtime(STATUS)
  _VERIFY(STATUS) 
  bwidth = REAL(IM_WORLD*JM_WORLD*8/1024.0/1024.0,kind=8)
  bwidth = bwidth/(itime_end-itime_beg)
  if (bwidth > peak_ioread_bandwidth) peak_ioread_bandwidth = bwidth
  mean_ioread_bandwidth = (mean_ioread_bandwidth + bwidth)
  ioread_counter=ioread_counter+1
  if (mod(ioread_counter,72.d0)==0) then
  if (MAPL_AM_I_Root()) write(*,'(a64,3es11.3)') 'MPIIO Read Bandwidth (MB per second): ', peak_ioread_bandwidth, bwidth, mean_ioread_bandwidth/ioread_counter
  endif
#endif 
    
    _RETURN(ESMF_SUCCESS)
  end subroutine MAPL_VarRead_R8_2d

!---------------------------
  subroutine MAPL_VarRead_R8_3d(UNIT, GRID, A, arrdes, RC)

    integer                     , intent(IN   ) :: UNIT
    type (ESMF_Grid)            , intent(INout) :: GRID
    real(kind=ESMF_KIND_R8)     , intent(  OUT) :: A(:,:,:)
    type(ArrDescr),     optional, intent(INOUT) :: ARRDES
    integer,           optional , intent(  OUT) :: RC

! Local variables

    integer                               :: status

    integer :: L

    do L = 1, size(A,3)
       call MAPL_VarRead(UNIT, GRID, A(:,:,L), ARRDES=arrdes, rc=status)
       _VERIFY(STATUS)
    end do

    _RETURN(ESMF_SUCCESS)
  end subroutine MAPL_VarRead_R8_3d
  
!---------------------------
  subroutine MAPL_VarRead_R8_4d(UNIT, GRID, A, arrdes, RC)

    integer                     , intent(IN   ) :: UNIT
    type (ESMF_Grid)            , intent(INout) :: GRID
    real(kind=ESMF_KIND_R8)     , intent(  OUT) :: A(:,:,:,:)
    type(ArrDescr),     optional, intent(INOUT) :: ARRDES
    integer,           optional , intent(  OUT) :: RC

! Local variables

    integer                               :: status

    integer :: L

    do L = 1, size(A,4)
       call MAPL_VarRead(UNIT, GRID, A(:,:,:,L), ARRDES=arrdes, rc=status)
       _VERIFY(STATUS)
    end do

    _RETURN(ESMF_SUCCESS)
  end subroutine MAPL_VarRead_R8_4d
  
!---------------------------
! Write routines
!---------------------------

  subroutine MAPL_StateVarWrite(UNIT, STATE, NAME, ARRDES, forceWriteNoRestart, RC)
    integer                     , intent(IN   ) :: UNIT
    type (ESMF_State)           , intent(INout) :: STATE
    character(len=*),   optional, intent(IN   ) :: NAME
    type(ArrDescr),     optional, intent(INOUT) :: ARRDES
    logical,            optional, intent(IN   ) :: forceWriteNoRestart
    integer,            optional, intent(  OUT) :: RC

! Local vars
    type (ESMF_FieldBundle)              :: bundle
    type (ESMF_Field)                    :: field
    type (ESMF_Grid)                     :: grid
    integer                              :: status
    integer                              :: I, ITEMCOUNT
    type (ESMF_StateItem_Flag), pointer  :: ITEMTYPES(:)
    character(len=ESMF_MAXSTR ), pointer :: ITEMNAMES(:)
    logical, pointer                     :: DOIT(:)
    logical                              :: skipWriting
    integer                              :: RST, dna
    logical                              :: forceWriteNoRestart_
    integer                              :: DIMS
    integer, pointer                     :: MASK(:) => null()
    
    integer, allocatable :: orderlist(:)
    integer :: jj
    character(len=ESMF_MAXSTR)           :: attrName
    character(len=ESMF_MAXSTR), allocatable :: currList(:)
    integer                                 :: natt
    logical                                 :: isPresent

    call ESMF_StateGet(STATE,ITEMCOUNT=ITEMCOUNT,RC=STATUS)
    _VERIFY(STATUS)

    _ASSERT(ITEMCOUNT>0, 'itemcount must be > 0')

    allocate(ITEMNAMES(ITEMCOUNT),STAT=STATUS)
    _VERIFY(STATUS)
    allocate(ITEMTYPES(ITEMCOUNT),STAT=STATUS)
    _VERIFY(STATUS)
    allocate(DOIT     (ITEMCOUNT),STAT=STATUS)
    _VERIFY(STATUS)

    call ESMF_StateGet(STATE,ITEMNAMELIST=ITEMNAMES,itemTypeList=ITEMTYPES,RC=STATUS)
    _VERIFY(STATUS)

    forceWriteNoRestart_ = .false.
    if(present(forceWriteNoRestart)) then
       forceWriteNoRestart_ = forceWriteNoRestart
    endif

    if(present(NAME)) then
       DOIT = ITEMNAMES==NAME
       _ASSERT(count(DOIT)/=0, 'count(doit) should not be 0')
    else
       DOIT = .true.
    endif

    attrName = MAPL_StateItemOrderList
    call ESMF_AttributeGet(state, NAME=attrName, itemcount=natt, RC=STATUS)
    _VERIFY(STATUS)

    _ASSERT(natt > 0, 'natt not > 0')
    allocate(orderlist(natt), stat=status)
    _VERIFY(STATUS)
    allocate(currList(natt), stat=status)
    _VERIFY(STATUS)

    ! get the current list
    call ESMF_AttributeGet(state, NAME=attrName, VALUELIST=currList, rc=status)
    _VERIFY(STATUS)

    orderList = -1 ! not found
    do i = 1, natt
       ! search loop
       do jj = 1, ITEMCOUNT
          if (itemNames(jj) == currList(i)) then
             orderList(i) = jj
             exit
          end if
       end do
    end do

    deallocate(currList)

    do JJ = 1, natt
       I = ORDERLIST(JJ)
    
       IF (DOIT     (I)) then

#ifdef TIME_MPIIO
    call write_parallel(itemnames(i))
#endif

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
             end if
             if (skipWriting) cycle

             call MAPL_BundleWrite(unit, bundle, arrdes=arrdes, rc=status)
             _VERIFY(STATUS)

          ELSE IF (ITEMTYPES(I) == ESMF_StateItem_Field) THEN
             call ESMF_StateGet(state, itemnames(i), field, rc=status)
             _VERIFY(STATUS)

             skipWriting = .false.
             if (.not. forceWriteNoRestart_) then
                call ESMF_AttributeGet(field, name='RESTART', isPresent=isPresent, rc=status)
                _VERIFY(STATUS)
                if (isPresent) then
                   call ESMF_AttributeGet(field, name='RESTART', value=RST, rc=status)
                   _VERIFY(STATUS)
                   skipWriting = (RST == MAPL_RestartSkip)
                end if
             end if
             if (skipWriting) cycle

             call ESMF_AttributeGet(field, name='doNotAllocate', isPresent=isPresent, rc=status)
             _VERIFY(STATUS)
             if (isPresent) then
                call ESMF_AttributeGet(field, name='doNotAllocate', value=dna, rc=status)
                _VERIFY(STATUS)
                skipWriting = (dna /= 0)
             endif
             if (skipWriting) cycle

             if(.not.associated(MASK)) then
                call ESMF_AttributeGet(field, name='DIMS', value=DIMS, rc=status)
                _VERIFY(STATUS)
                if (DIMS == MAPL_DimsTileOnly .or. DIMS == MAPL_DimsTileTile) then
                   call ESMF_FieldGet   (field, grid=grid, rc=status)
                   _VERIFY(STATUS)
                   call MAPL_TileMaskGet(grid,  mask, rc=status)
                   _VERIFY(STATUS)
                endif
             endif

             call MAPL_FieldWrite(unit, field, arrdes=arrdes, HomePE=mask, rc=status)
             _VERIFY(STATUS)

          end IF
       END IF

    END DO

    deallocate(orderlist)
    deallocate(ITEMNAMES)
    deallocate(ITEMTYPES)
    deallocate(DOIT     )
    if(associated(MASK)) then
       DEALOC_(MASK)
    end if

    _RETURN(ESMF_SUCCESS)
  end subroutine MAPL_StateVarWrite
!---------------------------


  subroutine MAPL_BundleWrite(UNIT,BUNDLE, ARRDES, RC)
    integer                     , intent(IN   ) :: UNIT
    type (ESMF_FieldBundle)          , intent(INOUT) :: BUNDLE
    type(ArrDescr),    optional , intent(INOUT) :: ARRDES
    integer,           optional , intent(  OUT) :: RC


    integer                                 :: status
    integer                                 :: J, N, fieldCount
    type (ESMF_Field)                       :: field
    character(len=ESMF_MAXSTR),allocatable  :: nameList(:)
    character(len=ESMF_MAXSTR)              :: BundleName

    call ESMF_FieldBundleGet(bundle, fieldCount=N, name=BundleName, rc=STATUS)
    _VERIFY(STATUS)
    allocate(namelist(N), stat=status)
    _VERIFY(STATUS)
    call ESMF_FieldBundleGet(bundle, fieldNameList=nameList, fieldCount=FieldCount, rc=STATUS)
    _VERIFY(STATUS)
    _ASSERT(N==fieldCount, 'inconsistent fieldcount')

    DO J = 1, N
       call MAPL_FieldBundleGet(bundle, fieldIndex=J, field=field, rc=status)
       _VERIFY(STATUS)

       call MAPL_FieldWrite(unit, field, arrdes=ARRDES, rc=status)
       _VERIFY(STATUS)

    END DO

    deallocate(nameList)

    _RETURN(ESMF_SUCCESS)
  end subroutine MAPL_BundleWrite

!---------------------------

  subroutine MAPL_FieldWrite(UNIT,FIELD, ARRDES, HomePE, RC)
    integer                     , intent(IN   ) :: UNIT
    type (ESMF_Field)           , intent(INOUT) :: field  !ALT: intent(in)
    type(ArrDescr),    optional , intent(INOUT) :: ARRDES
    integer, target,   optional , intent(IN   ) :: HomePE(:)
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

    real(KIND=ESMF_KIND_R8), pointer, dimension(:)        :: vr8_1d
    real(KIND=ESMF_KIND_R8), pointer, dimension(:,:)      :: vr8_2d
    real(KIND=ESMF_KIND_R8), pointer, dimension(:,:,:)    :: vr8_3d
    real(KIND=ESMF_KIND_R8), pointer, dimension(:,:,:,:)  :: vr8_4d
    type(ESMF_TypeKind_Flag)           :: tk
    integer, pointer                   :: mask(:) => NULL()
    character(len=ESMF_MAXSTR)         :: FORMATTED
    integer                            :: J,K
    type (ESMF_DistGrid)               :: distGrid
    
    if (unit < 0 .or. present(arrdes)) then
       FORMATTED = "NO"
    else
       inquire(unit=UNIT, formatted=FORMATTED)
    end if

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
                call MAPL_VarWrite(unit, grid, var_1d, arrdes=arrdes, mask=mask, rc=status)
             else if (DIMS == MAPL_DimsVertOnly .or. DIMS==MAPL_DimsNone) then
                call WRITE_PARALLEL(var_1d, unit, arrdes=arrdes, rc=status)
             else
                _RETURN(ESMF_FAILURE)
             end if
          end if
       else
          call ESMF_ArrayGet(array, localDE=0, farrayptr=vr8_1d, rc=status)
          _VERIFY(STATUS)
          if (associated(vr8_1d)) then
             if (DIMS == MAPL_DimsTileOnly .or. DIMS == MAPL_DimsTileTile) then
                call MAPL_VarWrite(unit, grid, vr8_1d, arrdes=arrdes, mask=mask, rc=status)
             else if (DIMS == MAPL_DimsVertOnly .or. DIMS==MAPL_DimsNone) then
                call WRITE_PARALLEL(vr8_1d, unit, arrdes=arrdes, rc=status)
             else
                _RETURN(ESMF_FAILURE)
             end if
          end if
       endif
    else if (rank == 2) then
       if (tk == ESMF_TYPEKIND_R4) then
          call ESMF_ArrayGet(array, localDE=0, farrayptr=var_2d, rc=status)
          _VERIFY(STATUS)
          if (associated(var_2d)) then !ALT: temp kludge
             if (FORMATTED=="YES") THEN
                call WRITE_PARALLEL( &
                     var_2d(lbound(var_2d,1),:), unit, rc=status)
             else
                if (DIMS == MAPL_DimsTileOnly) then
                   do J = 1,size(var_2d,2)
                      call MAPL_VarWrite(unit, grid, var_2d(:,J), arrdes=arrdes, mask=mask, rc=status)
                   end do
                else if (DIMS == MAPL_DimsTileTile) then
                   call MAPL_VarWrite(unit, grid, var_2d, arrdes=arrdes, mask=mask, rc=status)
                else
                   call MAPL_VarWrite(unit, grid, var_2d, arrdes=arrdes, rc=status)
                end if
             end if
          end if
       else
          call ESMF_ArrayGet(array, localDE=0, farrayptr=vr8_2d, rc=status)
          _VERIFY(STATUS)
          if (associated(vr8_2d)) then !ALT: temp kludge
             if (FORMATTED=="YES") THEN
                call WRITE_PARALLEL( &
                     vr8_2d(lbound(vr8_2d,1),:), unit, rc=status)
             else
                if (DIMS == MAPL_DimsTileOnly) then
                   do J = 1,size(vr8_2d,2)
                      call MAPL_VarWrite(unit, grid, vr8_2d(:,J), arrdes=arrdes, mask=mask, rc=status)
                   end do
                else if (DIMS == MAPL_DimsTileTile) then
                   call MAPL_VarWrite(unit, grid, vr8_2d, mask=mask, rc=status)
                else
                   call MAPL_VarWrite(unit, grid, vr8_2d, arrdes=arrdes, rc=status)
                end if
             end if
          end if
       endif
    else if (rank == 3) then
       if (tk == ESMF_TYPEKIND_R4) then
          call ESMF_ArrayGet(array, localDE=0, farrayptr=var_3d, rc=status)
          _VERIFY(STATUS)
          if (associated(var_3d)) then !ALT: temp kludge
             if (FORMATTED=="YES") THEN
                call WRITE_PARALLEL( &
                     var_3d(lbound(var_3d,1),lbound(var_3d,2),:), unit)
             else
                if (DIMS == MAPL_DimsTileOnly) then
                   do J = 1,size(var_3d,2)
                      do K = 1,size(var_3d,3)
                         call MAPL_VarWrite(unit, grid, var_3d(:,J,K), arrdes=arrdes, mask=mask, rc=status)
                      end do
                   end do
                else
                   call MAPL_VarWrite(unit, grid, var_3d, arrdes=arrdes, rc=status)
                endif
             endif
          end if
       else
          call ESMF_ArrayGet(array, localDE=0, farrayptr=vr8_3d, rc=status)
          _VERIFY(STATUS)
          if (associated(vr8_3d)) then !ALT: temp kludge
             if (FORMATTED=="YES") THEN
                call WRITE_PARALLEL( &
                     vr8_3d(lbound(vr8_3d,1),lbound(vr8_3d,2),:), unit)
             else
                if (DIMS == MAPL_DimsTileOnly) then
                   do J = 1,size(vr8_3d,2)
                      do K = 1,size(vr8_3d,3)
                         call MAPL_VarWrite(unit, grid, vr8_3d(:,J,K), arrdes=arrdes, mask=mask, rc=status)
                      end do
                   end do
                else
                   call MAPL_VarWrite(unit, grid, vr8_3d, arrdes=arrdes, rc=status)
                end if
             endif
          end if
       endif
    else if (rank == 4) then
       if (tk == ESMF_TYPEKIND_R4) then
          call ESMF_ArrayGet(array, localDE=0, farrayptr=var_4d, rc=status)
          _VERIFY(STATUS)
          call MAPL_VarWrite(unit, grid, var_4d, rc=status)
       else
          call ESMF_ArrayGet(array, localDE=0, farrayptr=vr8_4d, rc=status)
          _VERIFY(STATUS)
          call MAPL_VarWrite(unit, grid, vr8_4d, rc=status)
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
  end subroutine MAPL_FieldWrite


!---------------------------
  subroutine MAPL_VarWrite_I4_1d(UNIT, GRID, A, MASK, RC)

    integer                     , intent(IN   ) :: UNIT
    type (ESMF_Grid)            , intent(INout) :: GRID
    integer(kind=ESMF_KIND_I4)  , intent(IN   ) :: A(:)
    integer,           optional , intent(IN   ) :: MASK(:)
    integer,           optional , intent(  OUT) :: RC

! Local variables
    integer(kind=ESMF_KIND_I4),  allocatable :: VAR(:)
    integer                               :: IM_WORLD
    integer                               :: status
    integer                               :: DIMS(ESMF_MAXGRIDDIM)
    type (ESMF_DELayout)                  :: layout
    type (ESMF_DistGrid)                  :: distGrid

    if(unit < 0) then

      munit => MEM_units(-unit)
      munit%prevrec = munit%prevrec + 1
      if(.not.associated(munit%Records)) then
         allocate(munit%Records(16),stat=status)
         _VERIFY(STATUS)
      elseif(size(munit%Records)< munit%prevrec) then
         allocate(REC(munit%prevrec*2),stat=status)
         _VERIFY(STATUS)
         REC(:munit%prevrec-1) = munit%Records
         deallocate(munit%Records)
         munit%Records => REC
      endif
      call alloc_(munit%Records(munit%prevrec),i4_1,size(A),rc=status)	
      _VERIFY(STATUS)
      munit%Records(munit%prevrec)%I4_1  = A

    else

    call MAPL_GridGet(GRID, globalCellCountPerDim=DIMS, RC=STATUS)
    _VERIFY(STATUS)

    IM_WORLD = DIMS(1)

    allocate(VAR(IM_WORLD), stat=status)
    _VERIFY(STATUS)

    call ESMF_GridGet(grid, distGrid=distGrid, rc=STATUS)
    _VERIFY(STATUS)
    call ESMF_DistGridGet(distGrid, delayout=layout, rc=STATUS)
    _VERIFY(STATUS)

    call ArrayGather(A, VAR, grid, mask=mask, rc=status)
    _VERIFY(STATUS)
    if (MAPL_am_i_root(layout)) then
       write (UNIT, IOSTAT=status) VAR
       _VERIFY(STATUS)
    end if
    
    deallocate(VAR)

    endif

    _RETURN(ESMF_SUCCESS)
  end subroutine MAPL_VarWrite_I4_1d

!---------------------------
  subroutine MAPL_VarWrite_R4_1d(UNIT, GRID, A, MASK, arrdes, writeFCtrl, RC)

    integer                     , intent(IN   ) :: UNIT
    type (ESMF_Grid)            , intent(INout) :: GRID
    real(kind=ESMF_KIND_R4)     , intent(IN   ) :: A(:)
    integer,           optional , intent(IN   ) :: MASK(:)
    type(ArrDescr),     optional, intent(INOUT) :: ARRDES
    logical,           optional , intent(IN   ) :: writeFCtrl ! if not present default is .true.
    integer,           optional , intent(  OUT) :: RC

! Local variables
    real(kind=ESMF_KIND_R4),  allocatable :: VAR(:)
    real(kind=ESMF_KIND_R4),  allocatable :: GVAR(:)
    integer                               :: IM_WORLD
    integer                               :: status
    integer                               :: DIMS(ESMF_MAXGRIDDIM)
    type (ESMF_DELayout)                  :: layout
    type (ESMF_DistGrid)                  :: distGrid

    integer, allocatable                  :: msk(:), recvcounts(:), displs(:)
    integer                               :: nwrts, mype,  npes, sendcount
    integer                               :: mypeWr
    integer                               :: Rsize, first, last
    integer(KIND=MPI_OFFSET_KIND)         :: offset
    integer(KIND=MPI_OFFSET_KIND)         :: loffset
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
    integer                               :: recl
    logical                               :: useWriteFCtrl 

    integer :: mpistatus(MPI_STATUS_SIZE)
    logical :: amIRoot

    if(present(writeFCtrl)) then
       useWriteFCtrl = writeFCtrl 
    else
       useWriteFCtrl = .true.
    end if

    if(present(arrdes)) then
       _ASSERT(present(mask), 'mask must be present if arrdes is present')

       IM_WORLD = arrdes%im_world
       recl = IM_WORLD*4

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
       call ESMF_GridGet(grid, distGrid=distGrid, rc=STATUS)
       _VERIFY(STATUS)
       call ESMF_DistGridGet(distGrid, delayout=layout, rc=STATUS)
       _VERIFY(STATUS)
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
          if(arrdes%offset<=0) then
             offset = 4 
          else
             offset = arrdes%offset 
          endif
          if(useWriteFCtrl .and. mypeWr==0) then
             call MPI_FILE_SEEK(UNIT, offset-4, MPI_SEEK_SET, STATUS)
             _VERIFY(STATUS)
             call MPI_FILE_WRITE(UNIT, recl, 1, MPI_INTEGER, MPI_STATUS_IGNORE, STATUS)
             _VERIFY(STATUS)
          endif

          Rsize = im_world/nwrts + 1
          first = mypeWr*Rsize + 1
          if(mypeWr >=  mod(im_world,nwrts)) then
             Rsize = Rsize - 1
             first = first - (mypeWr-mod(im_world,nwrts))
          endif
          last  = first + Rsize - 1

          _ASSERT( (lbound(mask,1) <= first), 'out of bounds' )
          _ASSERT( (ubound(mask,1) >= last ), 'out of bounds' )

          loffset = offset + (first-1)*4
          call MPI_FILE_WRITE_AT_ALL(UNIT, loffset, GVAR, Rsize, MPI_REAL, mpistatus, STATUS)
          _VERIFY(STATUS)

#ifdef DEBUG_MPIIO
          call MPI_GET_COUNT( mpistatus, MPI_REAL, numwrite, STATUS )
          _VERIFY(STATUS)
          write(*,'(4i,1f)') IM_WORLD, loffset, numwrite, GVAR(1)
#endif

          if(useWriteFCtrl .and. mypeWr==0) then
             call MPI_FILE_SEEK(UNIT, offset+recl, MPI_SEEK_SET, STATUS)
             _VERIFY(STATUS)
             call MPI_FILE_WRITE(UNIT, recl, 1, MPI_INTEGER, MPI_STATUS_IGNORE, STATUS)
             _VERIFY(STATUS)
          endif
          arrdes%offset = offset + recl + 8
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

    elseif(unit < 0) then

      munit => MEM_units(-unit)
      munit%prevrec = munit%prevrec + 1
      if(.not.associated(munit%Records)) then
         allocate(munit%Records(16),stat=status)
         _VERIFY(STATUS)
      elseif(size(munit%Records)< munit%prevrec) then
         allocate(REC(munit%prevrec*2),stat=status)
         _VERIFY(STATUS)
         REC(:munit%prevrec-1) = munit%Records
         deallocate(munit%Records)
         munit%Records => REC
      endif
      call alloc_(munit%Records(munit%prevrec),R4_1,size(A),rc=status)	
      _VERIFY(STATUS)
      munit%Records(munit%prevrec)%R4_1  = A

    else

    call MAPL_GridGet(GRID, globalCellCountPerDim=DIMS, RC=STATUS)
    _VERIFY(STATUS)

    IM_WORLD = DIMS(1)

    call ESMF_GridGet(grid, distGrid=distGrid, rc=STATUS)
    _VERIFY(STATUS)
    call ESMF_DistGridGet(distGrid, delayout=layout, rc=STATUS)
    _VERIFY(STATUS)

    amIRoot = MAPL_am_i_root(layout)

    if (amIRoot) then
       allocate(VAR(IM_WORLD), stat=status)
       _VERIFY(STATUS)
    else
       allocate(VAR(0), stat=status)
       _VERIFY(STATUS)
    end if

    call ArrayGather(A, VAR, grid, mask=mask, rc=status)
    _VERIFY(STATUS)
    if (MAPL_am_i_root(layout)) then
       write (UNIT, IOSTAT=status) VAR
       _VERIFY(STATUS)
    end if
    
    deallocate(VAR)

    end if
    
    _RETURN(ESMF_SUCCESS)
  end subroutine MAPL_VarWrite_R4_1d

!---------------------------
!---------------------------

  subroutine MAPL_VarWrite_R4_2d(UNIT, GRID, A, MASK, ARRDES, RC)

    integer                     , intent(IN   ) :: UNIT
    type (ESMF_Grid)            , intent(INout) :: GRID
    real(kind=ESMF_KIND_R4)     , intent(IN   ) :: A(:,:)
    integer,           optional , intent(IN   ) :: MASK(:)
    type(ArrDescr),     optional, intent(INOUT) :: ARRDES
    integer,           optional , intent(  OUT) :: RC

! Local variables
    real(kind=ESMF_KIND_R4),  allocatable :: VAR(:,:)
    integer                               :: IM_WORLD
    integer                               :: JM_WORLD
    integer                               :: status
    integer                               :: DIMS(ESMF_MAXGRIDDIM)
    integer                               :: gridRank
    type (ESMF_DELayout)                  :: layout
    type (ESMF_DistGrid)                  :: distGrid

    real(kind=ESMF_KIND_R4),  allocatable :: buf(:)
    integer                               :: I,J,N,K,L,myrow,myiorank,ndes_x
    integer(kind=MPI_OFFSET_KIND)         :: offset
    integer                               :: jsize, jprev, num_io_rows
    integer, allocatable                  :: sendcounts(:), displs(:)


    integer                               :: mypeWr
    integer                               :: recl
    integer                               :: mpistatus(MPI_STATUS_SIZE)
    logical                               :: amIRoot

#ifdef TIME_MPIIO
    real(kind=ESMF_KIND_R8) :: itime_beg, itime_end, bwidth
#endif

#ifdef TIME_MPIIO
    call MPI_BARRIER(MPI_COMM_WORLD,STATUS)
    _VERIFY(STATUS)
    itime_beg = MPI_Wtime(STATUS)
    _VERIFY(STATUS)

#endif

    if(present(arrdes)) then
       IM_WORLD = arrdes%im_world
       JM_WORLD = arrdes%jm_world

       mypeWr = -1 !mark it invalid
       if(arrdes%writers_comm /= MPI_COMM_NULL) then
          call mpi_comm_rank(arrdes%writers_comm,mypeWr ,status)
          _VERIFY(STATUS)
       end if

       if(present(mask)) then
          JM_WORLD=size(a,2)
       
!          arrdes%offset = 0

! write Fortran control
          if(arrdes%writers_comm /= MPI_COMM_NULL) then
             if(arrdes%offset<=0) then
                offset = 4 
             else
                offset = arrdes%offset 
             endif

             recl = IM_WORLD*JM_WORLD*4

             if(mypeWr==0) then
                call MPI_FILE_SEEK(UNIT, offset-4, MPI_SEEK_SET, STATUS)
                _VERIFY(STATUS)
                call MPI_FILE_WRITE(UNIT, recl, 1, MPI_INTEGER, MPI_STATUS_IGNORE, STATUS)
                _VERIFY(STATUS)
             endif
          end if

          do j=1,jm_world
             call MAPL_VarWrite(Unit, Grid, a(:,j), mask, arrdes, writeFCtrl=.false., rc=status)
             arrdes%offset = arrdes%offset - 8
          enddo

          arrdes%offset = arrdes%offset + 8

! write Fortran control
          if(arrdes%writers_comm /= MPI_COMM_NULL) then
             if(mypeWr==0) then
                call MPI_FILE_SEEK(UNIT, offset+recl, MPI_SEEK_SET, STATUS)
                _VERIFY(STATUS)
                call MPI_FILE_WRITE(UNIT, recl, 1, MPI_INTEGER, MPI_STATUS_IGNORE, STATUS)
                _VERIFY(STATUS)
             endif
          end if

       else

       ndes_x = size(arrdes%in)

       call mpi_comm_rank(arrdes%ycomm,myrow,status)
       _VERIFY(STATUS)
       call mpi_comm_rank(arrdes%iogathercomm,myiorank,status)
       _VERIFY(STATUS)
       call mpi_comm_size(arrdes%iogathercomm,num_io_rows,status)
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
       end if

!DSK avoid "Attempt to fetch from allocatable variable BUF when it is not allocated"
       if(myiorank/=0) then
          allocate(buf(0), stat=status)
          _VERIFY(STATUS)
       endif

       call mpi_gatherv( a, size(a), MPI_REAL, buf, sendcounts, displs, MPI_REAL, &
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
                      VAR(i,jprev+j) = buf(k)
                      k=k+1
                   end do
                end do
             end do
             jprev = jprev + jsize
          end do
          jsize=jprev

          if(arrdes%offset<=0) then
             offset = 0
          else
             offset = arrdes%offset
          endif

          recl = IM_WORLD*JM_WORLD*4
          if (mypeWr==0) then
#ifdef DEBUG_MPIIO
                print*, offset, recl, offset + IM_WORLD*JM_WORLD*4 + 8
#endif
             call MPI_FILE_SEEK(UNIT, offset, MPI_SEEK_SET, STATUS)
             _VERIFY(STATUS)
             call MPI_FILE_WRITE(UNIT, recl, 1, MPI_INTEGER, MPI_STATUS_IGNORE, STATUS)
             _VERIFY(STATUS)
          endif
          offset = offset + 4

          offset = offset + (arrdes%j1(myrow+1)-1)*IM_WORLD*4
          call MPI_FILE_WRITE_AT_ALL(UNIT, offset, VAR, IM_WORLD*jsize, MPI_REAL, mpistatus, STATUS)
          _VERIFY(STATUS)
          offset = offset - (arrdes%j1(myrow+1)-1)*IM_WORLD*4

          offset = offset + IM_WORLD*JM_WORLD*4
          if (mypeWr==0) then
             call MPI_FILE_SEEK(UNIT, offset, MPI_SEEK_SET, STATUS)
             _VERIFY(STATUS)
             call MPI_FILE_WRITE(UNIT, recl, 1, MPI_INTEGER, MPI_STATUS_IGNORE, STATUS)
             _VERIFY(STATUS)
          endif

          arrdes%offset = offset + 4

       end if

       if(myiorank==0) then
          deallocate(VAR, stat=status)
          _VERIFY(STATUS)
!          deallocate(buf, stat=status)
!          _VERIFY(STATUS)
       endif
       deallocate(buf, stat=status)
       _VERIFY(STATUS)
       deallocate (sendcounts, displs, stat=status)
       _VERIFY(STATUS)
    endif

    elseif(unit < 0) then

      munit => MEM_units(-unit)
      munit%prevrec = munit%prevrec + 1
      if(.not.associated(munit%Records)) then
         allocate(munit%Records(16),stat=status)
         _VERIFY(STATUS)
      elseif(size(munit%Records)< munit%prevrec) then
         allocate(REC(munit%prevrec*2),stat=status)
         _VERIFY(STATUS)
         REC(:munit%prevrec-1) = munit%Records
         deallocate(munit%Records)
         munit%Records => REC
      endif
      call alloc_(munit%Records(munit%prevrec),r4_2,size(A,1),size(a,2),rc=status)	
      _VERIFY(STATUS)
      munit%Records(munit%prevrec)%R4_2  = A

    else

      call ESMF_GridGet(GRID, dimCount=gridRank, rc=STATUS)
      _VERIFY(STATUS)
      call MAPL_GridGet(GRID, globalCellCountPerDim=DIMS, RC=STATUS)
      _VERIFY(STATUS)

      IM_WORLD = DIMS(1)
      JM_WORLD = DIMS(2)
      if(present(MASK)) JM_WORLD=size(a,2)

      call ESMF_GridGet(grid, distGrid=distGrid, rc=STATUS)
      _VERIFY(STATUS)
      call ESMF_DistGridGet(distGrid, delayout=layout, rc=STATUS)
      _VERIFY(STATUS)

      amIRoot = MAPL_am_i_root(layout)
      if (amIRoot) then
         allocate(VAR(IM_WORLD,JM_WORLD), stat=status)
         _VERIFY(STATUS)
      else
         allocate(VAR(0,JM_WORLD), stat=status)
         _VERIFY(STATUS)
      end if

      call ArrayGather(A, VAR, grid, mask=mask, rc=status)
      _VERIFY(STATUS)
      if (amIRoot) then

         write (UNIT, IOSTAT=status) VAR
         _VERIFY(STATUS)
      end if
    
      deallocate(VAR)

   end if

#ifdef TIME_MPIIO
  call MPI_BARRIER(MPI_COMM_WORLD,STATUS)
  _VERIFY(STATUS)
  itime_end = MPI_Wtime(STATUS)
  _VERIFY(STATUS)
  bwidth = REAL(IM_WORLD*JM_WORLD*4/1024.0/1024.0,kind=8)
  bwidth = bwidth/(itime_end-itime_beg)
  if (bwidth > peak_iowrite_bandwidth) peak_iowrite_bandwidth = bwidth
  mean_iowrite_bandwidth = (mean_iowrite_bandwidth + bwidth)
  iowrite_counter=iowrite_counter+1
  if (mod(iowrite_counter,72.d0)==0) then
    if (MAPL_AM_I_Root()) write(*,'(a64,3es11.3)') 'MPIIO Write Bandwidth (MB per second): ', peak_iowrite_bandwidth, bwidth, mean_iowrite_bandwidth/iowrite_counter
  endif
#endif 
    
    _RETURN(ESMF_SUCCESS)
  end subroutine MAPL_VarWrite_R4_2d

!---------------------------
  subroutine MAPL_VarWrite_R4_3d(UNIT, GRID, A, ARRDES, RC)

    integer                     , intent(IN   ) :: UNIT
    type (ESMF_Grid)            , intent(INout) :: GRID
    real(kind=ESMF_KIND_R4)     , intent(IN   ) :: A(:,:,:)
    type(ArrDescr),     optional, intent(INOUT) :: ARRDES
    integer,           optional , intent(  OUT) :: RC

! Local variables

    integer                               :: status

    integer :: L

    do L = 1, size(A,3)
       call MAPL_VarWrite(UNIT, GRID, A(:,:,L), ARRDES=ARRDES, rc=status)
       _VERIFY(STATUS)
    end do

    _RETURN(ESMF_SUCCESS)
  end subroutine MAPL_VarWrite_R4_3d
  
!---------------------------
  subroutine MAPL_VarWrite_R4_4d(UNIT, GRID, A, ARRDES, RC)

    integer                     , intent(IN   ) :: UNIT
    type (ESMF_Grid)            , intent(INout) :: GRID
    real(kind=ESMF_KIND_R4)     , intent(IN   ) :: A(:,:,:,:)
    type(ArrDescr),     optional, intent(INOUT) :: ARRDES
    integer,           optional , intent(  OUT) :: RC

! Local variables

    integer                               :: status

    integer :: L

    do L = 1, size(A,4)
       call MAPL_VarWrite(UNIT, GRID, A(:,:,:,L), ARRDES=ARRDES, rc=status)
       _VERIFY(STATUS)
    end do

    _RETURN(ESMF_SUCCESS)
  end subroutine MAPL_VarWrite_R4_4d
  
!---------------------------
  subroutine MAPL_VarWrite_R8_1d(UNIT, GRID, A, MASK, arrdes, writeFCtrl, RC)

    integer                     , intent(IN   ) :: UNIT
    type (ESMF_Grid)            , intent(INout) :: GRID
    real(kind=ESMF_KIND_R8)     , intent(IN   ) :: A(:)
    integer,           optional , intent(IN   ) :: MASK(:)
    type(ArrDescr),     optional, intent(INOUT) :: ARRDES
    logical,           optional , intent(IN   ) :: writeFCtrl ! if not present default is .true.
    integer,           optional , intent(  OUT) :: RC

! Local variables
    real(kind=ESMF_KIND_R8),  allocatable :: VAR(:)
    real(kind=ESMF_KIND_R8),  allocatable :: GVAR(:)
    integer                               :: IM_WORLD
    integer                               :: status
    integer                               :: DIMS(ESMF_MAXGRIDDIM)
    type (ESMF_DELayout)                  :: layout
    type (ESMF_DistGrid)                  :: distGrid

    integer, allocatable                  :: msk(:), recvcounts(:), displs(:)
    integer                               :: nwrts, mype,  npes, sendcount
    integer                               :: mypeWr
    integer                               :: Rsize, first, last
    integer(KIND=MPI_OFFSET_KIND)         :: offset
    integer(KIND=MPI_OFFSET_KIND)         :: loffset
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
    integer                               :: recl
    logical                               :: useWriteFCtrl 

    integer :: mpistatus(MPI_STATUS_SIZE)

    if(present(writeFCtrl)) then
       useWriteFCtrl = writeFCtrl 
    else
       useWriteFCtrl = .true.
    end if

    if(present(arrdes)) then
       _ASSERT(present(mask), 'mask must be present if arrdes is present')

       IM_WORLD = arrdes%im_world
       recl = IM_WORLD*8

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
       call ESMF_GridGet(grid, distGrid=distGrid, rc=STATUS)
       _VERIFY(STATUS)
       call ESMF_DistGridGet(distGrid, delayout=layout, rc=STATUS)
       _VERIFY(STATUS)
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
          if(arrdes%offset<=0) then
             offset = 4 
          else
             offset = arrdes%offset 
          endif
          if(useWriteFCtrl .and. mypeWr==0) then
             call MPI_FILE_SEEK(UNIT, offset-4, MPI_SEEK_SET, STATUS)
             _VERIFY(STATUS)
             call MPI_FILE_WRITE(UNIT, recl, 1, MPI_INTEGER, MPI_STATUS_IGNORE, STATUS)
             _VERIFY(STATUS)
          endif

          Rsize = im_world/nwrts + 1
          first = mypeWr*Rsize + 1
          if(mypeWr >=  mod(im_world,nwrts)) then
             Rsize = Rsize - 1
             first = first - (mypeWr-mod(im_world,nwrts))
          endif
          last  = first + Rsize - 1

          _ASSERT( (lbound(mask,1) <= first), 'out of bounds')
          _ASSERT( (ubound(mask,1) >= last ), 'out of bounds' )

          loffset = offset + (first-1)*8
          call MPI_FILE_WRITE_AT_ALL(UNIT, loffset, GVAR, Rsize, MPI_DOUBLE_PRECISION, mpistatus, STATUS)
          _VERIFY(STATUS)

#ifdef DEBUG_MPIIO
          call MPI_GET_COUNT( mpistatus, MPI_DOUBLE_PRECISION, numwrite, STATUS )
          _VERIFY(STATUS)
          write(*,'(4i,1f)') IM_WORLD, loffset, numwrite, GVAR(1)
#endif

          if(useWriteFCtrl .and. mypeWr==0) then
             call MPI_FILE_SEEK(UNIT, offset+recl, MPI_SEEK_SET, STATUS)
             _VERIFY(STATUS)
             call MPI_FILE_WRITE(UNIT, recl, 1, MPI_INTEGER, MPI_STATUS_IGNORE, STATUS)
             _VERIFY(STATUS)
          endif
          arrdes%offset = offset + recl + 8
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

    elseif(unit < 0) then

      munit => MEM_units(-unit)
      munit%prevrec = munit%prevrec + 1
      if(.not.associated(munit%Records)) then
         allocate(munit%Records(16),stat=status)
         _VERIFY(STATUS)
      elseif(size(munit%Records)< munit%prevrec) then
         allocate(REC(munit%prevrec*2),stat=status)
         _VERIFY(STATUS)
         REC(:munit%prevrec-1) = munit%Records
         deallocate(munit%Records)
         munit%Records => REC
      endif
      call alloc_(munit%Records(munit%prevrec),R8_1,size(A),rc=status)	
      _VERIFY(STATUS)
      munit%Records(munit%prevrec)%R8_1  = A

    else

    call MAPL_GridGet(GRID, globalCellCountPerDim=DIMS, RC=STATUS)
    _VERIFY(STATUS)

    IM_WORLD = DIMS(1)

    allocate(VAR(IM_WORLD), stat=status)
    _VERIFY(STATUS)

    call ESMF_GridGet(grid, distGrid=distGrid, rc=STATUS)
    _VERIFY(STATUS)
    call ESMF_DistGridGet(distGrid, delayout=layout, rc=STATUS)
    _VERIFY(STATUS)

    call ArrayGather(A, VAR, grid, mask=mask, rc=status)
    _VERIFY(STATUS)
    if (MAPL_am_i_root(layout)) then
       write (UNIT, IOSTAT=status) VAR
       _VERIFY(STATUS)
    end if
    
    deallocate(VAR)

    end if
    
    _RETURN(ESMF_SUCCESS)
  end subroutine MAPL_VarWrite_R8_1d

!---------------------------

  subroutine MAPL_VarWrite_R8_2d(UNIT, GRID, A, MASK, ARRDES, RC)

    integer                     , intent(IN   ) :: UNIT
    type (ESMF_Grid)            , intent(INout) :: GRID
    real(kind=ESMF_KIND_R8)     , intent(IN   ) :: A(:,:)
    integer,           optional , intent(IN   ) :: MASK(:)
    type(ArrDescr),     optional, intent(INOUT) :: ARRDES
    integer,           optional , intent(  OUT) :: RC

! Local variables
    real(kind=ESMF_KIND_R8),  allocatable :: VAR(:,:)
    integer                               :: IM_WORLD
    integer                               :: JM_WORLD
    integer                               :: status
    integer                               :: DIMS(ESMF_MAXGRIDDIM)
    integer                               :: gridRank
    type (ESMF_DELayout)                  :: layout
    type (ESMF_DistGrid)                  :: distGrid

    real(kind=ESMF_KIND_R8),  allocatable :: buf(:)
    integer                               :: I,J,N,K,L,myrow,myiorank,ndes_x
    integer(kind=MPI_OFFSET_KIND)         :: offset
    integer                               :: jsize, jprev, num_io_rows
    integer, allocatable                  :: sendcounts(:), displs(:)

    integer                               :: mypeWr
    integer                               :: recl
    integer                               :: mpistatus(MPI_STATUS_SIZE)

#ifdef TIME_MPIIO
    real(kind=ESMF_KIND_R8) :: itime_beg, itime_end, bwidth
#endif

#ifdef TIME_MPIIO
  call MPI_BARRIER(MPI_COMM_WORLD,STATUS)
  _VERIFY(STATUS)
  itime_beg = MPI_Wtime(STATUS)
  _VERIFY(STATUS)
#endif

    if(present(arrdes)) then
       IM_WORLD = arrdes%im_world
       JM_WORLD = arrdes%jm_world

       mypeWr = -1 !mark it invalid
       if(arrdes%writers_comm /= MPI_COMM_NULL) then
          call mpi_comm_rank(arrdes%writers_comm,mypeWr ,status)
          _VERIFY(STATUS)
       end if

       if(present(mask)) then
          _ASSERT(JM_WORLD==size(A,2), 'inconsistent array shape')

!          arrdes%offset = 0

! write Fortran control
          if(arrdes%writers_comm /= MPI_COMM_NULL) then
             if(arrdes%offset<=0) then
                offset = 4 
             else
                offset = arrdes%offset 
             endif

             recl = IM_WORLD*JM_WORLD*8

             if(mypeWr==0) then
                call MPI_FILE_SEEK(UNIT, offset-4, MPI_SEEK_SET, STATUS)
                _VERIFY(STATUS)
                call MPI_FILE_WRITE(UNIT, recl, 1, MPI_INTEGER, MPI_STATUS_IGNORE, STATUS)
                _VERIFY(STATUS)
             endif
          end if

          do j=1,jm_world
             call MAPL_VarWrite(Unit, Grid, a(:,j), mask, arrdes, writeFCtrl=.false., rc=status)
             arrdes%offset = arrdes%offset - 8
          enddo

          arrdes%offset = arrdes%offset + 8

! write Fortran control
          if(arrdes%writers_comm /= MPI_COMM_NULL) then
             if(mypeWr==0) then
                call MPI_FILE_SEEK(UNIT, offset+recl, MPI_SEEK_SET, STATUS)
                _VERIFY(STATUS)
                call MPI_FILE_WRITE(UNIT, recl, 1, MPI_INTEGER, MPI_STATUS_IGNORE, STATUS)
                _VERIFY(STATUS)
             endif
          end if

       else

       ndes_x = size(arrdes%in)

       call mpi_comm_rank(arrdes%ycomm,myrow,status)
       _VERIFY(STATUS)
       call mpi_comm_rank(arrdes%iogathercomm,myiorank,status)
       _VERIFY(STATUS)
       call mpi_comm_size(arrdes%iogathercomm,num_io_rows,status)
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
       end if

!DSK avoid "Attempt to fetch from allocatable variable BUF when it is not allocated"
       if(myiorank/=0) then
          allocate(buf(0), stat=status)
          _VERIFY(STATUS)
       endif

       call mpi_gatherv( a, size(a), MPI_DOUBLE_PRECISION, buf, sendcounts, displs, MPI_DOUBLE_PRECISION, &
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
                      VAR(i,jprev+j) = buf(k)
                      k=k+1
                   end do
                end do
             end do
             jprev = jprev + jsize
          end do
          jsize=jprev

          if(arrdes%offset<=0) then
             offset = 0
          else
             offset = arrdes%offset
          endif

          recl = IM_WORLD*JM_WORLD*8
          if (mypeWr==0) then
#ifdef DEBUG_MPIIO
        print*, offset, recl, offset + IM_WORLD*JM_WORLD*8 + 8
#endif
             call MPI_FILE_SEEK(UNIT, offset, MPI_SEEK_SET, STATUS)
             _VERIFY(STATUS)
             call MPI_FILE_WRITE(UNIT, recl, 1, MPI_INTEGER, MPI_STATUS_IGNORE, STATUS)
             _VERIFY(STATUS)
          endif
          offset = offset + 4

          offset = offset + (arrdes%j1(myrow+1)-1)*IM_WORLD*8
          call MPI_FILE_WRITE_AT_ALL(UNIT, offset, VAR, IM_WORLD*jsize, MPI_DOUBLE_PRECISION, mpistatus, STATUS)
          _VERIFY(STATUS)
          offset = offset - (arrdes%j1(myrow+1)-1)*IM_WORLD*8 

          offset = offset + IM_WORLD*JM_WORLD*8 
          if (mypeWr==0) then
             call MPI_FILE_SEEK(UNIT, offset, MPI_SEEK_SET, STATUS)
             _VERIFY(STATUS)
             call MPI_FILE_WRITE(UNIT, recl, 1, MPI_INTEGER, MPI_STATUS_IGNORE, STATUS)
             _VERIFY(STATUS)
          endif

          arrdes%offset = offset + 4

       end if

       if(myiorank==0) then
          deallocate(VAR, stat=status)
          _VERIFY(STATUS)
!          deallocate(buf, stat=status)
!          _VERIFY(STATUS)
       endif
       deallocate(buf, stat=status)
       _VERIFY(STATUS)
       deallocate (sendcounts, displs, stat=status)
       _VERIFY(STATUS)
    endif

    elseif(unit < 0) then

      munit => MEM_units(-unit)
      munit%prevrec = munit%prevrec + 1
      if(.not.associated(munit%Records)) then
         allocate(munit%Records(16),stat=status)
         _VERIFY(STATUS)
      elseif(size(munit%Records)< munit%prevrec) then
         allocate(REC(munit%prevrec*2),stat=status)
         _VERIFY(STATUS)
         REC(:munit%prevrec-1) = munit%Records
         deallocate(munit%Records)
         munit%Records => REC
      endif
      call alloc_(munit%Records(munit%prevrec),r8_2,size(A,1),size(a,2),rc=status)	
      _VERIFY(STATUS)
      munit%Records(munit%prevrec)%R8_2  = A

    else

      call ESMF_GridGet(GRID, dimCount=gridRank, rc=STATUS)
      _VERIFY(STATUS)
      call MAPL_GridGet(GRID, globalCellCountPerDim=DIMS, RC=STATUS)
      _VERIFY(STATUS)

      IM_WORLD = DIMS(1)
      JM_WORLD = DIMS(2)
      if (present(MASK)) JM_WORLD=size(A,2)

      allocate(VAR(IM_WORLD,JM_WORLD), stat=status)
      _VERIFY(STATUS)

      call ESMF_GridGet(grid, distGrid=distGrid, rc=STATUS)
      _VERIFY(STATUS)
      call ESMF_DistGridGet(distGrid, delayout=layout, rc=STATUS)
      _VERIFY(STATUS)

      call ArrayGather(A, VAR, grid, mask=mask, rc=status)
      _VERIFY(STATUS)
      if (MAPL_am_i_root(layout)) then

         write (UNIT, IOSTAT=status) VAR
         _VERIFY(STATUS)
      end if
    
      deallocate(VAR)

    end if

#ifdef TIME_MPIIO
  call MPI_BARRIER(MPI_COMM_WORLD,STATUS)
  _VERIFY(STATUS)
  itime_end = MPI_Wtime(STATUS)
  _VERIFY(STATUS)
  bwidth = REAL(IM_WORLD*JM_WORLD*8/1024.0/1024.0,kind=8)
  bwidth = bwidth/(itime_end-itime_beg)
  if (bwidth > peak_iowrite_bandwidth) peak_iowrite_bandwidth = bwidth
  mean_iowrite_bandwidth = (mean_iowrite_bandwidth + bwidth)
  iowrite_counter=iowrite_counter+1
  if (mod(iowrite_counter,72.d0)==0) then
  if (MAPL_AM_I_Root()) write(*,'(a64,3es11.3)') 'MPIIO Write Bandwidth (MB per second): ', peak_iowrite_bandwidth, bwidth, mean_iowrite_bandwidth/iowrite_counter
  endif
#endif 
    
    _RETURN(ESMF_SUCCESS)
  end subroutine MAPL_VarWrite_R8_2d

!---------------------------
  subroutine MAPL_VarWrite_R8_3d(UNIT, GRID, A, ARRDES, RC)

    integer                     , intent(IN   ) :: UNIT
    type (ESMF_Grid)            , intent(INout) :: GRID
    real(kind=ESMF_KIND_R8)     , intent(IN   ) :: A(:,:,:)
    type(ArrDescr),     optional, intent(INOUT) :: ARRDES
    integer,           optional , intent(  OUT) :: RC

! Local variables

    integer                               :: status

    integer :: L

    do L = 1, size(A,3)
       call MAPL_VarWrite(UNIT, GRID, A(:,:,L), ARRDES=ARRDES, rc=status)
       _VERIFY(STATUS)
    end do

    _RETURN(ESMF_SUCCESS)
  end subroutine MAPL_VarWrite_R8_3d
  
!---------------------------
  subroutine MAPL_VarWrite_R8_4d(UNIT, GRID, A, ARRDES, RC)

    integer                     , intent(IN   ) :: UNIT
    type (ESMF_Grid)            , intent(INout) :: GRID
    real(kind=ESMF_KIND_R8)     , intent(IN   ) :: A(:,:,:,:)
    type(ArrDescr),     optional, intent(INOUT) :: ARRDES
    integer,           optional , intent(  OUT) :: RC

! Local variables

    integer                               :: status

    integer :: L

    do L = 1, size(A,4)
       call MAPL_VarWrite(UNIT, GRID, A(:,:,:,L), ARRDES=ARRDES, rc=status)
       _VERIFY(STATUS)
    end do

    _RETURN(ESMF_SUCCESS)
  end subroutine MAPL_VarWrite_R8_4d
  
!---------------------------
!---------------------------
!---------------------------

!---------------------------
#define RANK_ 1
#define VARTYPE_ 3
#include "arrayscatter.H"

!---------------------------
#define RANK_ 1
#define VARTYPE_ 4
#include "arrayscatter.H"

!---------------------------
#define RANK_ 2
#define VARTYPE_ 3
#include "arrayscatter.H"

!---------------------------
#define RANK_ 2
#define VARTYPE_ 4
#include "arrayscatter.H"
!---------------------------
!---------------------------


    subroutine MAPL_ClimUpdate ( STATE, BEFORE, AFTER, &
                                 CURRENT_TIME, NAMES, FILE, RC )
        type(ESMF_State),       intent(INOUT) :: STATE
        type(ESMF_Time),        intent(  out) :: BEFORE, AFTER
        type(ESMF_Time),        intent(inout) :: CURRENT_TIME !ALT:intent(in)
        character(len=*),       intent(in   ) :: NAMES(:)
        character(len=*),       intent(in   ) :: FILE
        integer,  optional,     intent(  out) :: RC

        integer :: STATUS


        integer          :: I, M, M1, M2
        integer          :: NFLD
        integer          :: UNIT
        integer          :: DONE

        type (ESMF_Field   ), pointer :: PREV(:)
        type (ESMF_Field   ), pointer :: NEXT(:)
        type (ESMF_DELayout)          :: LAYOUT
        type (ESMF_Grid    )          :: GRID
        type (ESMF_DistGrid)          :: distGRID


    ! --------------------------------------------------------------------------
    ! Allocate the number of fileds in the file
    ! --------------------------------------------------------------------------

        NFLD = size(NAMES)
        _ASSERT(NFLD>0, 'NFLD must be > 0')

        allocate(PREV(NFLD),stat=STATUS)
        _VERIFY(STATUS)
        allocate(NEXT(NFLD),stat=STATUS)
        _VERIFY(STATUS)

    ! --------------------------------------------------------------------------
    ! get the fields from the state
    ! --------------------------------------------------------------------------

        do I=1,NFLD
           call ESMF_StateGet ( STATE, trim(NAMES(I))//'_PREV', PREV(I), RC=STATUS )
           _VERIFY(STATUS)
           call ESMF_StateGet ( STATE, trim(NAMES(I))//'_NEXT', NEXT(I), RC=STATUS )
           _VERIFY(STATUS)
        end do

        call ESMF_FieldGet(PREV(1), GRID=GRID,    RC=STATUS)
        _VERIFY(STATUS)
        call ESMF_GridGet    (GRID,   distGrid=distGrid, rc=STATUS)
        _VERIFY(STATUS)
        call ESMF_DistGridGet(distGRID, delayout=layout, rc=STATUS)
        _VERIFY(STATUS)

    ! --------------------------------------------------------------------------
    ! Find out the times of next, prev from the field attributes
    ! --------------------------------------------------------------------------

        call MAPL_FieldGetTime ( PREV(1), BEFORE, RC=STATUS )
        _VERIFY(STATUS)
        call MAPL_FieldGetTime ( NEXT(1), AFTER , RC=STATUS )
        _VERIFY(STATUS)

    ! --------------------------------------------------------------------------
    ! check to see if albedos need to be refreshed in the
    ! ESMF Internal State (prev, next need to surround
    ! the current time)
    ! --------------------------------------------------------------------------

        call ESMF_TimeGet ( BEFORE, yy=I, rc=STATUS )
        _VERIFY(STATUS)

        DONE = 0
        if( I > 0) then
           if( (BEFORE <= CURRENT_TIME) .and. (AFTER >= CURRENT_TIME)) then
              DONE = 1
           end if
        end if

        if(DONE /= 1) then

    ! --------------------------------------------------------------------------
    !  Get the midmonth times for the months before and after the current time
    ! --------------------------------------------------------------------------

           call MAPL_GetClimMonths ( CURRENT_TIME, BEFORE, AFTER,  RC=STATUS )
           _VERIFY(STATUS)

           call ESMF_TimeGet ( BEFORE, MM=M1, rc=STATUS )
           _VERIFY(STATUS)
           call ESMF_TimeGet ( AFTER , MM=M2, rc=STATUS )
           _VERIFY(STATUS)

    ! --------------------------------------------------------------------------
    !  Read the albedo climatologies from file
    ! --------------------------------------------------------------------------

           UNIT = GETFILE(FILE, form="unformatted",  RC=STATUS)
           _VERIFY(STATUS)

           DONE = 0
           do M=1,12
              if    (M==M1) then
                 do I=1,NFLD
                    call MAPL_VarRead(UNIT, PREV(I), RC=STATUS)
                    _VERIFY(STATUS)
                 end do
                 if(DONE==1) exit
                 DONE = DONE + 1
              elseif(M==M2) then
                 do I=1,NFLD
                    call MAPL_VarRead(UNIT, NEXT(I), RC=STATUS)
                    _VERIFY(STATUS)
                 end do
                 if(DONE==1) exit
                 DONE = DONE + 1
              else
                 call MAPL_Skip(UNIT,LAYOUT,COUNT=NFLD,rc=status)
                 _VERIFY(STATUS)
              end if
           end do

           call FREE_FILE ( Unit )
 
    ! --------------------------------------------------------------------------
    !  Reset the time on all fields
    ! --------------------------------------------------------------------------

           do I=1,NFLD
              call MAPL_FieldSetTime (  PREV(I), BEFORE, rc=STATUS )
              _VERIFY(STATUS)
              call MAPL_FieldSetTime (  NEXT(I), AFTER , rc=STATUS )
              _VERIFY(STATUS)
           end do
   
        endif

        deallocate(NEXT)
        deallocate(PREV)

        _RETURN(ESMF_SUCCESS)
      end subroutine MAPL_ClimUpdate


    subroutine MAPL_GetClimMonths ( CURRENT_TIME, BEFORE, AFTER, RC )
        type(ESMF_Time), intent(inout) :: CURRENT_TIME !ALT: intent(in)
        type(ESMF_Time), intent(out) :: BEFORE, AFTER
        integer,optional,intent(out) :: RC

        integer :: STATUS

        integer                 :: MonthCurr
        type(ESMF_Time        ) :: midMonth
        type(ESMF_TimeInterval) :: oneMonth

        call ESMF_TimeIntervalSet(oneMonth, MM = 1, RC=STATUS )
        _VERIFY(STATUS)
        call ESMF_TimeGet(CURRENT_TIME, midMonth=midMonth, mm=MonthCurr, RC=STATUS )
        _VERIFY(STATUS)

        if( CURRENT_TIME < midMonth ) then
           AFTER    = midMonth
           midMonth = midMonth - oneMonth
           call ESMF_TimeGet (midMonth, midMonth=BEFORE, rc=STATUS )
           _VERIFY(STATUS)
        else
           BEFORE   = midMonth
           midMonth = midMonth + oneMonth
           call ESMF_TimeGet (midMonth, midMonth=AFTER , rc=STATUS )
           _VERIFY(STATUS)
        endif

        _RETURN(ESMF_SUCCESS)
    end subroutine MAPL_GetClimMonths
    
  subroutine MAPL_Skip(UNIT, LAYOUT, COUNT, RC)

    integer                     , intent(IN   ) :: UNIT
    type (ESMF_DELayout)        , intent(IN   ) :: LAYOUT
    integer,           optional , intent(IN   ) :: COUNT
    integer,           optional , intent(  OUT) :: RC

! Local variables

    integer                               :: STATUS
    integer                               :: N, NN

    if(present(COUNT)) then
       NN=COUNT
    else
       NN=1
    endif

    if (unit < 0) then
       munit => MEM_units(-unit)
       munit%prevrec = munit%prevrec + NN
       _RETURN(ESMF_SUCCESS)
    endif

    if (MAPL_AM_I_ROOT(LAYOUT)) then

       do N=1,NN
          read (unit=UNIT, IOSTAT=status)
          _VERIFY(STATUS)
       end do
    end if
    
    _RETURN(ESMF_SUCCESS)
  end subroutine MAPL_Skip

  subroutine MAPL_Backspace(UNIT, LAYOUT, COUNT, RC)

    integer                     , intent(IN   ) :: UNIT
    type (ESMF_DELayout)        , intent(IN   ) :: LAYOUT
    integer,           optional , intent(IN   ) :: COUNT
    integer,           optional , intent(  OUT) :: RC

! Local variables

    integer                               :: STATUS
    integer                               :: N, NN

    if (MAPL_AM_I_ROOT(LAYOUT)) then
       if(present(COUNT)) then
          NN=COUNT
       else
          NN=1
       endif

       do N=1,NN
          backspace(unit=UNIT, IOSTAT=status)
          _VERIFY(STATUS)
       end do
    end if
    
    _RETURN(ESMF_SUCCESS)
  end subroutine MAPL_Backspace

  subroutine MAPL_Rewind(UNIT, LAYOUT, RC)

    integer                     , intent(IN   ) :: UNIT
    type (ESMF_DELayout)        , intent(IN   ) :: LAYOUT
    integer,           optional , intent(  OUT) :: RC

! Local variables

    integer                               :: STATUS

    if (MAPL_AM_I_ROOT(LAYOUT)) then
       rewind(unit=UNIT, IOSTAT=status)
       _VERIFY(STATUS)
    end if
    
    _RETURN(ESMF_SUCCESS)
  end subroutine MAPL_Rewind

  subroutine ArrayScatterShmR4D1(local_array, global_array, grid, mask, rc)

! Mask is really a permutation on the first dimension

    real,         intent(  OUT) :: local_array(:)
!    TYPE_(kind=EKIND_), target, intent(IN   ) :: global_array DIMENSIONS_
    real, target                :: global_array(:)
    type (ESMF_Grid)                          :: grid
    integer, optional,          intent(IN   ) :: mask(:)
    integer, optional,          intent(  OUT) :: rc
    
! Local variables

    integer                               :: status

    real,    pointer        :: myglob(:) => null()
    real,    pointer        :: VAR(:)
    type (ESMF_DistGrid)                  :: distGrid
    type(ESMF_DELayout)                   :: LAYOUT
    type (ESMF_VM)                        :: vm
    integer,               allocatable    :: AL(:,:)
    integer,               allocatable    :: AU(:,:)
    integer, dimension(:), allocatable    :: SENDCOUNTS, DISPLS
    integer                               :: KK
    integer                               :: nDEs
    integer                               :: recvcount
    integer                               :: I, K, II, deId
    integer                               :: gridRank
    integer                               :: LX
    integer                               :: srcPE
    integer                               :: ISZ
    logical                               :: alloc_var
    logical                               :: use_shmem

! Works only on 1D and 2D arrays
! Note: for tile variables the gridRank is 1 
! and the case RANK_=2 needs additional attention 

! use_shmem controls communication (bcastToNodes+local copy vs scatterv)
    use_shmem = .true.

    ! temporary Shmem restricted only to 1d and tile vars
    if (.not.present(mask)) use_shmem = .false.

! Optional change of source PE. Default=MAPL_Root

    srcPE = MAPL_Root
 
! Initialize
    alloc_var = .true.

! Get grid and layout information

    call ESMF_GridGet    (GRID,   dimCount=gridRank, rc=STATUS);_VERIFY(STATUS)
    call ESMF_GridGet    (GRID,   distGrid=distGrid, rc=STATUS);_VERIFY(STATUS)
    call ESMF_DistGridGet(distGRID, delayout=layout, rc=STATUS);_VERIFY(STATUS)
    call ESMF_DELayoutGet(layout, vm=vm, rc=status);_VERIFY(STATUS)
    call ESMF_VmGet(vm, localPet=deId, petCount=nDEs, rc=status);_VERIFY(STATUS)

    if (use_shmem) then
       srcPE = deId
    end if

    allocate (AL(gridRank,0:nDEs-1),  stat=status)
    _VERIFY(STATUS)
    allocate (AU(gridRank,0:nDEs-1),  stat=status)
    _VERIFY(STATUS)
    allocate (sendcounts(0:nDEs-1), stat=status)
    _VERIFY(STATUS)
    call MAPL_DistGridGet(distgrid, &
         minIndex=AL, maxIndex=AU, rc=status)
    _VERIFY(STATUS)

    ISZ = size(GLOBAL_ARRAY,1)

    if (use_shmem) then
       call MAPL_SyncSharedMemory(rc=STATUS)
       _VERIFY(STATUS)
       call MAPL_BroadcastToNodes(global_array, N=ISZ, ROOT=MAPL_Root, rc=status)
       _VERIFY(STATUS)
       call MAPL_SyncSharedMemory(rc=STATUS)
       _VERIFY(STATUS)
    end if

! Compute count to be sent to each PE

    if(present(mask)) then
       sendcounts = 0
       do II = 1,ISZ
          sendcounts(mask(ii)) = sendcounts(mask(ii)) + 1
       enddo
    else
       do I = 0,nDEs-1
          LX = AU(1,I) - AL(1,I) + 1
          sendcounts(I) = LX
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

       myglob => global_array
       
! Fill the VAR vector
       
       if (present(mask)) then
          allocate(VAR(displs(deId):displs(deId+1)-1), stat=status)
          _VERIFY(STATUS)
          KK = DISPLS(deId)

          do I=1,ISZ
             K = MASK(I)
             if(K == deId) then
                II = KK
                VAR(II) = MYGLOB(I)
                KK = KK + 1 
             end if
          end do

       else

          var => myglob
          alloc_var = .false.

       endif !  present(mask)

     else
        allocate(var(0:1), stat=status)
        _VERIFY(STATUS)
        allocate(DISPLS(0:nDEs), stat=status)
        _VERIFY(STATUS)
     end if !  I am srcPEa


! Do the communications
    if (use_shmem) then
       ! copy my piece from var (var is local but was filled from shared array)
       call MAPL_SyncSharedMemory(rc=STATUS)
       _VERIFY(STATUS)
       local_array = var(displs(deId):displs(deId+1)-1)
       call MAPL_SyncSharedMemory(rc=STATUS)
       _VERIFY(STATUS)
    else
       call MAPL_CommsScatterV(layout, var, sendcounts, displs, &
                               local_array, recvcount, srcPE, status)
       _VERIFY(STATUS)
    end if

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
  end subroutine ArrayScatterShmR4D1

  INTEGER FUNCTION GETFILE( NAME, DO_OPEN, FORM, ALL_PES, &
                             BLOCKSIZE, NUMBUFFERS, RC )
    IMPLICIT NONE

    character(LEN=*), intent(in   )           :: Name
    integer         , intent(in   ), OPTIONAL :: DO_OPEN
    character(LEN=*), intent(in   ), OPTIONAL :: Form
    logical         , intent(in   ), OPTIONAL :: ALL_PES
    integer         , intent(in   ), OPTIONAL :: BLOCKSIZE
    integer         , intent(in   ), OPTIONAL :: NUMBUFFERS
    integer         , intent(  out), OPTIONAL :: RC

    INTEGER I
    integer :: DO_OPEN_
    logical :: ALL_PES_
    integer          :: status

    LOGICAL FILEOPEN, UNITOPEN, FOUND

    if(INDEX(NAME,'*') /= 0) then
        getfile = getfilemem(name,rc=status)
    _VERIFY(STATUS)
        _RETURN(ESMF_SUCCESS)
    endif

    if (NAME == "stdout" .or. NAME== "STDOUT") then
       GETFILE = STD_OUT_UNIT_NUMBER
       _RETURN(ESMF_SUCCESS)
    end if

    if (.not. present(DO_OPEN)) then
       DO_OPEN_ = 1
    else
       DO_OPEN_ = DO_OPEN
    end if

    ALL_PES_ = .false.
    if (present(ALL_PES)) then
       ALL_PES_ = ALL_PES
    end if

    if (.not. MAPL_AM_I_ROOT() .and. .not. ALL_PES_) then
       GETFILE = UNDEF
       _RETURN(ESMF_SUCCESS)
    end if

!   Check if the file is already open

    INQUIRE ( FILE=NAME, NUMBER=GETFILE, OPENED=FILEOPEN )

!   If the file isnt already open THEN

    IF ( .NOT. FILEOPEN ) THEN
       I = 20
       FOUND = .FALSE.
       DO WHILE ( I.LE.LAST_UNIT .AND. .NOT.FOUND )
          IF ( .NOT. TAKEN(I) ) THEN
             TAKEN(I) = .TRUE.
             INQUIRE ( UNIT=I, OPENED=UNITOPEN )
             IF ( .NOT. UNITOPEN ) THEN

                status = 0

                if ( DO_OPEN_ .NE. 0 ) then
                   call MAPL_open(UNIT=i,FILE=Name,FORM=FORM, &
                                  BLOCKSIZE= BLOCKSIZE, NUMBUFFERS=NUMBUFFERS, RC=STATUS)
                endif

                if ( status /= 0 ) then
                   write (0,*) 'ERROR opening "',trim(Name),'" using GETFILE'
                   write (0,*) ' IOSTAT = ',status
                   _RETURN(ESMF_FAILURE)
                endif

                GETFILE = I
                FOUND = .TRUE.
             ENDIF
          ENDIF
          I = I + 1
       ENDDO
!
!      IF there are no available logical units THEN
!         Write an error message
!         Return Error status
!      ENDIF there are no available logical units
!  
       IF ( .NOT. FOUND ) THEN
          WRITE (0,*) ' COULD NOT FIND ANY AVAILABLE UNITS '
          _RETURN(ESMF_FAILURE)
       ENDIF

    ENDIF ! the file isnt already open 

    _RETURN(ESMF_SUCCESS)
  END FUNCTION GETFILE

  INTEGER FUNCTION GETFILEMEM(name,  RC )
    IMPLICIT NONE
    character(LEN=*), intent(in   )           :: Name
    integer         , intent(  out), OPTIONAL :: RC

    integer :: i
    logical :: found

    found = .false.
    do i = 3, last_unit
       if(name==Mname(i)) then
          found = .true.
          exit
       end if
    end do

    if (.not. found) then
       do i = 3,last_unit
          if(.not.MTAKEN(i)) then
             found = .true.
             exit
          endif
       enddo
    end if

    if (.not. found) then
       if(present(rc)) rc = 1
       return
    endif

    mname(i)   = name
    mtaken(i)  = .true.
    getfilemem = -i

    if(present(rc)) rc = 0
    return
  end function getfilemem

  subroutine MAPL_OPEN(UNIT,FILE,FORM,BLOCKSIZE, NUMBUFFERS, RC)

    implicit none
    integer         , optional, intent(out) :: RC

    integer         ,           intent(in) :: UNIT
    character(LEN=*),           intent(in) :: FILE
    character(LEN=*), optional, intent(in) :: FORM
    integer,          optional, intent(in) :: BLOCKSIZE, NUMBUFFERS
    integer          :: status

    character(LEN=ESMF_MAXSTR) :: usableFORM

    if(MAPL_AM_I_ROOT()) then
       if(.not.present(BLOCKSIZE) .and. .not.present(NUMBUFFERS)) then
          print *, "NOT using buffer I/O for file: ", trim(file)
       else
          print *, "Using buffer I/O for file: ", trim(file)
       endif
    endif

    if (present(FORM)) then
       usableFORM = FORM
    else
       usableFORM = "unformatted"
    end if

    open(UNIT,FILE=FILE,FORM=usableFORM,IOSTAT=STATUS)
    _VERIFY(STATUS)

    _RETURN(ESMF_SUCCESS)
  end subroutine MAPL_OPEN


end module BinIOMod
