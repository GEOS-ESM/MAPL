
#include "MAPL_ErrLog.h"
#define DEALOC_(A) if(associated(A))then;if(MAPL_ShmInitialized)then;call MAPL_SyncSharedMemory(rc=STATUS);call MAPL_DeAllocNodeArray(A,rc=STATUS);else;deallocate(A,stat=STATUS);endif;_VERIFY(STATUS);NULLIFY(A);endif

!BOP

! !MODULE: MAPL_IO -- A Module to do I/O (ASCII+binary) until ESMF fully supports it


! !INTERFACE:

module MAPL_IOMod

  use ESMF
  use MAPL_BaseMod
  use MAPL_CommsMod
  use MAPL_SortMod
  use MAPL_ShmemMod
  use MAPL_RangeMod
  use MAPL_ExceptionHandling
  use netcdf
  use pFIO
  use pFIO_ClientManagerMod
  use gFTL_StringIntegerMap
  use gFTL_StringVector
  use, intrinsic :: ISO_C_BINDING
  use, intrinsic :: iso_fortran_env
  implicit none
  private

  ! public types
  public MAPL_IOChangeRes
  public MAPL_IOCountNonDimVars
  public MAPL_IOGetNonDimVars
  public MAPL_IOCountLevels
  public MAPL_IOGetTime
  public MAPL_NCIOGetFileType
  public MAPL_NCIOParseTimeUnits
  ! public routines

  ! public routines
  public GETFILEUNIT
  public GETFILE
  public FREE_FILE
  public READ_PARALLEL
  public WRITE_PARALLEL
  public MAPL_VarRead
  public MAPL_VarWrite
  public MAPL_Skip
  public MAPL_Backspace
  public MAPL_Rewind
  public MAPL_ClimUpdate 
  public MAPL_DestroyFile
  public ArrDescr
  public ArrDescrSet
  public ArrDescrInit
  public get_fname_by_face
  public MAPL_TileMaskGet
  public MAPL_VarReadNCPar
  public MAPL_VarWriteNCPar
  public MAPL_MemFileInquire

! Interfaces:
! -----------

  interface WRITE_PARALLEL
     module procedure WRITE_PARALLEL_I4_0
     module procedure WRITE_PARALLEL_I4_1
     module procedure WRITE_PARALLEL_R4_0
     module procedure WRITE_PARALLEL_R4_1
     module procedure WRITE_PARALLEL_R8_0
     module procedure WRITE_PARALLEL_R8_1
     module procedure WRITE_PARALLEL_STRING_0
  end interface

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

! -----------------------------------------
  interface MAPL_VarRead
     module procedure MAPL_StateVarRead
     module procedure MAPL_BundleRead 
     module procedure MAPL_FieldRead 
     module procedure MAPL_VarRead_R4_1D
     module procedure MAPL_VarReadNCpar_R4_1d
     module procedure MAPL_VarRead_R4_2D
     module procedure MAPL_VarReadNCpar_R4_2d
     module procedure MAPL_VarRead_R4_3d
     module procedure MAPL_VarReadNCpar_R4_3d
     module procedure MAPL_VarRead_R4_4D
     module procedure MAPL_VarRead_R8_1D
     module procedure MAPL_VarReadNCpar_R8_1d
     module procedure MAPL_VarRead_R8_2D
     module procedure MAPL_VarReadNCpar_R8_2d
     module procedure MAPL_VarRead_R8_3D
     module procedure MAPL_VarReadNCpar_R8_3d
     module procedure MAPL_VarRead_R8_4D
  end interface

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

  interface MAPL_VarWrite
     module procedure MAPL_StateVarWrite
     module procedure MAPL_BundleWrite 
     module procedure MAPL_FieldWrite 
     module procedure MAPL_VarWrite_I4_1D
     module procedure MAPL_VarWrite_R4_1D
     module procedure MAPL_VarWriteNCpar_R4_1d
     module procedure MAPL_VarWrite_R4_2d
     module procedure MAPL_VarWriteNCpar_R4_2d
     module procedure MAPL_VarWrite_R4_3D
     module procedure MAPL_VarWriteNCpar_R4_3d
     module procedure MAPL_VarWrite_R4_4D
     module procedure MAPL_VarWriteNCpar_R4_4d
     module procedure MAPL_VarWrite_R8_1D
     module procedure MAPL_VarWriteNCpar_R8_1d
     module procedure MAPL_VarWrite_R8_2D
     module procedure MAPL_VarWriteNCpar_R8_2d
     module procedure MAPL_VarWrite_R8_3D
     module procedure MAPL_VarWriteNCpar_R8_3d
     module procedure MAPL_VarWrite_R8_4D
     module procedure MAPL_VarWriteNCpar_R8_4d
  end interface

  interface ArrayScatterShm
     module procedure ArrayScatterShmR4D1
  end interface ArrayScatterShm

  interface MAPL_MemFileInquire
     module procedure InqFileMem
  end interface

  include "mpif.h"
  include "netcdf.inc"

! Global vars:
! ------------

  integer, parameter :: STD_OUT_UNIT_NUMBER = 6
  integer, parameter :: LAST_UNIT = 999
  integer, parameter :: UNDEF = 999
  logical, save      :: TAKEN(LAST_UNIT)=.FALSE.
  logical, save      :: MTAKEN(LAST_UNIT)=.FALSE.
  character(len=ESMF_MAXSTR), save  :: mname(LAST_UNIT)

  integer, parameter :: not_allocated = 0
  integer, parameter :: r4_2 = 1
  integer, parameter :: r4_1 = 2
  integer, parameter :: r8_2 = 3
  integer, parameter :: r8_1 = 4
  integer, parameter :: i4_2 = 5
  integer, parameter :: i4_1 = 6

  type PTR
   integer :: allocated=not_allocated
   real(kind=ESMF_KIND_R4)   , pointer :: r4_2(:,:) => null()
   real(kind=ESMF_KIND_R4)   , pointer :: r4_1(:) => null()
   real(kind=ESMF_KIND_R4)             :: r4_0
   real(kind=ESMF_KIND_R8)   , pointer :: r8_2(:,:) => null()
   real(kind=ESMF_KIND_R8)   , pointer :: r8_1(:) => null()
   real(kind=ESMF_KIND_R8)             :: r8_0
   integer(kind=ESMF_KIND_I4), pointer :: I4_2(:,:) => null()
   integer(kind=ESMF_KIND_I4), pointer :: I4_1(:) => null()
   integer(kind=ESMF_KIND_I4)          :: I4_0
  end type PTR

  type memunit
     integer :: prevrec = 0
     type (PTR), pointer :: Records(:)=>null()
  end type MEMUNIT

  type (memunit), target, save :: MEM_UNITS(LAST_UNIT)
  type (memunit), pointer      :: munit
  type(PTR), pointer           :: REC(:)

  type ArrDescr
     integer(kind=MPI_OFFSET_KIND) :: offset
     character(len=MPI_MAX_INFO_VAL) :: romio_cb_read,cb_buffer_size,romio_cb_write
     integer :: Xcomm, Ycomm, NX0, NY0
     integer :: readers_comm, IOscattercomm
     integer :: writers_comm, IOgathercomm
     integer :: face_writers_comm
     integer :: face_readers_comm
     integer :: face_index
     logical :: write_restart_by_face = .false.
     logical :: read_restart_by_face = .false.
     integer, pointer :: i1(:), in(:), j1(:), jn(:)
     integer :: im_world, jm_world, lm_world
     type (ESMF_Grid) :: grid
     logical :: tile
     integer :: num_readers = 1
     integer :: num_writers = 1
     ! only used when writing though o_server
     logical :: write_restart_by_oserver = .false.
     integer :: collection_id = -1
     character(LEN=ESMF_MAXSTR) :: filename
  end type ArrDescr

!#define TIME_MPIIO
#ifdef TIME_MPIIO
  real(kind=ESMF_KIND_R8), save :: peak_ioread_bandwidth=0
  real(kind=ESMF_KIND_R8), save :: mean_ioread_bandwidth=0
  real(kind=ESMF_KIND_R8), save :: ioread_counter=0
  real(kind=ESMF_KIND_R8), save :: peak_iowrite_bandwidth=0
  real(kind=ESMF_KIND_R8), save :: mean_iowrite_bandwidth=0
  real(kind=ESMF_KIND_R8), save :: iowrite_counter=0
#endif

  contains

      subroutine ArrDescrInit(ArrDes,comm,im_world,jm_world,lm_world,nx,ny,num_readers,num_writers,is,ie,js,je,rc)
         type(ArrDescr), intent(INOUT) :: ArrDes
         integer, intent(in) :: comm
         integer, intent(in) :: IM_World
         integer, intent(in) :: JM_World
         integer, intent(in) :: lm_world
         integer, intent(in) :: nx
         integer, intent(in) :: ny
         integer, intent(in) :: num_readers
         integer, intent(in) :: num_writers
         integer, intent(in) :: is
         integer, intent(in) :: ie
         integer, intent(in) :: js
         integer, intent(in) :: je
         integer, optional, intent(out) :: rc

         integer :: color,myid,npes,NX0,NY0,ny_by_readers,ny_by_writers,j
         integer :: readers_comm, writers_comm, ioscattercomm,iogathercomm, xcomm,ycomm
         integer, allocatable :: i1(:),in(:),j1(:),jn(:)
         integer, allocatable :: iminw(:),imaxw(:),jminw(:),jmaxw(:)
         integer :: imincnt,jmincnt,imaxcnt,jmaxcnt,i

         integer :: status

         call MPI_Comm_Rank(comm,myid,status)
         _VERIFY(STATUS)
         call MPI_COMM_Size(comm,npes,status)
         _VERIFY(STATUS)

         allocate(iminw(npes),imaxw(npes),jminw(npes),jmaxw(npes),stat=status)
         iminw=-1
         imaxw=-1
         jminw=-1
         jmaxw=-1
         iminw(myid+1)=is
         imaxw(myid+1)=ie
         jminw(myid+1)=js
         jmaxw(myid+1)=je
         call MPI_AllReduce(MPI_IN_PLACE,iminw,npes,MPI_INTEGER,MPI_MAX,comm,status)
         call MPI_AllReduce(MPI_IN_PLACE,imaxw,npes,MPI_INTEGER,MPI_MAX,comm,status)
         call MPI_AllReduce(MPI_IN_PLACE,jminw,npes,MPI_INTEGER,MPI_MAX,comm,status)
         call MPI_AllReduce(MPI_IN_PLACE,jmaxw,npes,MPI_INTEGER,MPI_MAX,comm,status)

         call MAPL_Sort(iminw)
         call MAPL_Sort(imaxw)
         call MAPL_Sort(jminw)
         call MAPL_Sort(jmaxw)

         allocate(i1(nx),in(nx),j1(ny),jn(ny))
         i1(1) = minval(iminw)
         in(1) = minval(imaxw)
         j1(1) = minval(jminw)
         jn(1) = minval(jmaxw)
         imincnt = 1
         imaxcnt = 1
         jmincnt = 1
         jmaxcnt = 1
         do i=1,npes
            if (iminw(i) > i1(imincnt)) then
               imincnt = imincnt + 1
               i1(imincnt) = iminw(i)
            end if
            if (imaxw(i) > in(imaxcnt)) then
               imaxcnt = imaxcnt + 1
               in(imaxcnt) = imaxw(i)
            end if
            if (jminw(i) > j1(jmincnt)) then
               jmincnt = jmincnt + 1
               j1(jmincnt) = jminw(i)
            end if
            if (jmaxw(i) > jn(jmaxcnt)) then
               jmaxcnt = jmaxcnt + 1
               jn(jmincnt) = jmaxw(i)
            end if
         enddo
         deallocate(iminw,imaxw,jminw,jmaxw)

         NX0 = mod(myid,nx) + 1
         NY0 = myid/nx + 1
         color = nx0
         call MPI_Comm_Split(comm,color,myid,ycomm,status)
         _VERIFY(STATUS)
         color = ny0
         call MPI_Comm_Split(comm,color,myid,xcomm,status)
         _VERIFY(STATUS)
         ! reader communicators
         if (num_readers > ny .or. mod(ny,num_readers) /= 0) then
            _RETURN(ESMF_FAILURE)
         end if
         ny_by_readers = ny/num_readers
         if (mod(myid,nx*ny/num_readers) ==0) then
            color = 0
         else
            color = MPI_UNDEFINED
         end if
         call MPI_COMM_SPLIT(comm,color,myid,readers_comm,status)
         _VERIFY(STATUS)
         if (num_readers==ny) then
            IOscattercomm = xcomm
         else
            j = ny0 - mod(ny0-1,ny_by_readers)
            call MPI_Comm_Split(comm,j,myid,IOScattercomm,status)
            _VERIFY(STATUS)
         endif
         ! writer communicators
         if (num_writers > ny .or. mod(ny,num_writers) /= 0) then
            _RETURN(ESMF_FAILURE)
         end if
         ny_by_writers = ny/num_writers
         if (mod(myid,nx*ny/num_writers) ==0) then
            color = 0
         else
            color = MPI_UNDEFINED
         end if
         call MPI_COMM_SPLIT(comm,color,myid,writers_comm,status)
         _VERIFY(STATUS)
         if (num_writers==ny) then
            IOgathercomm = xcomm
         else
            j = ny0 - mod(ny0-1,ny_by_writers)
            call MPI_Comm_Split(comm,j,myid,IOgathercomm,status)
            _VERIFY(STATUS)
         endif

         ArrDes%im_world=im_world
         ArrDes%jm_world=jm_world
         ArrDes%lm_world=lm_world

         ArrDes%readers_comm  = readers_comm
         ArrDes%ioscattercomm = ioscattercomm
         ArrDes%writers_comm  = writers_comm
         ArrDes%iogathercomm  = iogathercomm
         ArrDes%xcomm = xcomm
         ArrDes%ycomm = ycomm

         allocate(arrdes%i1(size(i1)),stat=status)
         _VERIFY(STATUS)
         arrdes%i1=i1
         allocate(arrdes%in(size(in)),stat=status)
         _VERIFY(STATUS)
         arrdes%in=in
         allocate(arrdes%j1(size(j1)),stat=status)
         _VERIFY(STATUS)
         arrdes%j1=j1
         allocate(arrdes%jn(size(jn)),stat=status)
         _VERIFY(STATUS)
         arrdes%jn=jn

         ArrDes%NX0 = NY0
         ArrDes%NY0 = NX0

         ArrDes%offset = 0

         ArrDes%romio_cb_read  = "automatic"
         ArrDes%cb_buffer_size = "16777216"
         ArrDes%romio_cb_write = "enable"

         ArrDes%face_readers_comm = MPI_COMM_NULL
         ArrDes%face_writers_comm = MPI_COMM_NULL
         ArrDes%face_index        = 0

         ArrDes%tile = .false.

         ArrDes%filename = ''

         _RETURN(ESMF_SUCCESS)

      end subroutine ArrDescrInit


    subroutine ArrDescrSet(ArrDes, offset, &
         readers_comm, ioscattercomm, &
         writers_comm, iogathercomm, &
         i1, in, j1, jn, im_world, jm_world, lm_world)

      type(ArrDescr),                 intent(INOUT) :: ArrDes
      integer(kind=MPI_OFFSET_KIND), &
                           optional,  intent(IN   ) :: offset
      integer, optional,              intent(IN   ) :: readers_comm, ioscattercomm
      integer, optional,              intent(IN   ) :: writers_comm, iogathercomm
      integer, optional, pointer                    :: i1(:), in(:), j1(:), jn(:)
      integer, optional,              intent(IN   ) :: im_world, jm_world, lm_world

      if(present(offset  )) ArrDes%offset   = offset
      if(present(readers_comm )) ArrDes%readers_comm  = readers_comm
      if(present(ioscattercomm)) ArrDes%ioscattercomm = ioscattercomm
      if(present(writers_comm )) ArrDes%writers_comm  = writers_comm
      if(present(iogathercomm )) ArrDes%iogathercomm  = iogathercomm
      if(present(i1      )) ArrDes%i1       => i1
      if(present(in      )) ArrDes%in       => in
      if(present(j1      )) ArrDes%j1       => j1
      if(present(jn      )) ArrDes%jn       => jn
      if(present(im_world)) ArrDes%im_world = im_world
      if(present(jm_world)) ArrDes%jm_world = jm_world
      if(present(lm_world)) ArrDes%lm_world = lm_world

    end subroutine ArrDescrSet

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

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!--WRITES ------------------

!---------------------------
#define RANK_ 0
#define VARTYPE_ 1
#include "write_parallel.H"

!---------------------------
#define RANK_ 1
#define VARTYPE_ 1
#include "write_parallel.H"

!---------------------------
#define RANK_ 0
#define VARTYPE_ 3
#include "write_parallel.H"

!---------------------------
#define RANK_ 1
#define VARTYPE_ 3
#include "write_parallel.H"

!---------------------------
#define RANK_ 0
#define VARTYPE_ 4
#include "write_parallel.H"

!---------------------------
#define RANK_ 1
#define VARTYPE_ 4
#include "write_parallel.H"

!---------------------------
#define RANK_ 0
#define VARTYPE_ 0
#include "write_parallel.H"


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

!---------------------------
! Read routines
!---------------------------

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
                call MAPL_VarRead(formatter, name, var_1d, layout=layout, arrdes=arrdes, mask=mask, rc=status)
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
                do j=1,size(var_2d,2)
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
             _ASSERT(.false., "Cannot read unassociated variable")
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
          _ASSERT(.false., "ERROR: unsupported RANK/KIND")
       endif
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
  end subroutine MAPL_FieldReadNCPar


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
    integer :: size_1d
    
 
    call ESMF_FieldGet(field, grid=grid, rc=status)
    _VERIFY(STATUS)
    call ESMF_GridGet(grid, distGrid=distGrid, rc=STATUS)
    _VERIFY(STATUS)
    call ESMF_DistGridGet(distGrid, delayout=layout, rc=STATUS)
    _VERIFY(STATUS)

    if( arrdes%write_restart_by_oserver) then
      _ASSERT(present(oClients), "output server is needed")
    endif

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
                size_1d = arrdes%im_world
             else if (DIMS == MAPL_DimsVertOnly .or. DIMS==MAPL_DimsNone) then
                size_1d = size(var_1d,1)
             endif
 
             if (arrdes%write_restart_by_oserver) then
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
                call oClients%collective_stage_data(arrdes%collection_id, trim(arrdes%filename), name, lMemRef, start=[1], &
                             global_start=[1], global_count=[size_1d])
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
             _ASSERT(.false., "Cannot write unassociated var-1d")
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

             if (arrdes%write_restart_by_oserver) then
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
                call oClients%collective_stage_data(arrdes%collection_id, trim(arrdes%filename), name, lMemRef, start=[1], &
                             global_start=[1], global_count=[size_1d])

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
             _ASSERT(.false., "Cannot write unassociated var8-1d")
          end if
       endif
    else if (rank == 2) then
       if (tk == ESMF_TYPEKIND_R4) then
          call ESMF_ArrayGet(array, localDE=0, farrayptr=var_2d, rc=status)
          _VERIFY(STATUS)
          if (associated(var_2d)) then !ALT: temp kludge
             if (DIMS == MAPL_DimsTileOnly .or. DIMS == MAPL_DimsTileTile) then

                if (arrdes%write_restart_by_oserver) then
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
                   call oClients%collective_stage_data(arrdes%collection_id, trim(arrdes%filename), name, lMemRef, start=[1,1], &
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
             _ASSERT(.false., "Cannot write unassociated var-2d")
          endif ! associated 
       else
          call ESMF_ArrayGet(array, localDE=0, farrayptr=vr8_2d, rc=status)
          _VERIFY(STATUS)
          if (associated(vr8_2d)) then !ALT: temp kludge
             if (DIMS == MAPL_DimsTileOnly .or. DIMS == MAPL_DimsTileTile) then

                if (arrdes%write_restart_by_oserver) then
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
                   call oClients%collective_stage_data(arrdes%collection_id, trim(arrdes%filename), name, lMemRef, start=[1,1], &
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
             _ASSERT(.false., "Cannot write unassociated var8-2d")
          end if
       endif
    else if (rank == 3) then
       if (tk == ESMF_TYPEKIND_R4) then
          call ESMF_ArrayGet(array, localDE=0, farrayptr=var_3d, rc=status)
          _VERIFY(STATUS)
          if (associated(var_3d)) then !ALT: temp kludge
             if (DIMS == MAPL_DimsTileOnly) then

                if (arrdes%write_restart_by_oserver) then
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

                   call oClients%collective_stage_data(arrdes%collection_id, trim(arrdes%filename), name, lMemRef, start=[1,1,1], &
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
             _ASSERT(.false., "Cannot write unassociated var-3d")
          end if
       else
          call ESMF_ArrayGet(array, localDE=0, farrayptr=vr8_3d, rc=status)
          _VERIFY(STATUS)
          if (associated(vr8_3d)) then !ALT: temp kludge
             if (DIMS == MAPL_DimsTileOnly) then

                if (arrdes%write_restart_by_oserver) then
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
                   call oClients%collective_stage_data(arrdes%collection_id, trim(arrdes%filename), name, lMemRef, start=[1,1,1], &
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
             _ASSERT(.false., "Cannot write unassociated var8-3d")
          end if
       endif
    else if (rank == 4) then
       if (DIMS == MAPL_DimsTileOnly .or. DIMS == MAPL_DimsTileTile) then
          _ASSERT(.false., "Unsupported tile/ungrid variable")
       end if
       if (tk == ESMF_TYPEKIND_R4) then
          call ESMF_ArrayGet(array, localDE=0, farrayptr=var_4d, rc=status)
          _VERIFY(STATUS)
          if (.not.associated(var_4d)) then
             _ASSERT(.false., "Cannot write unassociated vars")
          end if
          call MAPL_VarWrite(formatter, name, var_4d, arrdes=arrdes, oClients=oClients, rc=status)
       else
          call ESMF_ArrayGet(array, localDE=0, farrayptr=vr8_4d, rc=status)
          _VERIFY(STATUS)
          if (.not.associated(vr8_4d)) then
             _ASSERT(.false., "Cannot write unassociated vars")
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


  subroutine alloc_(A,type,im,jm,rc)
    type (Ptr),        intent(INOUT) :: A
    integer,           intent(IN)    :: TYPE
    integer,           intent(IN)    :: IM
    integer, optional, intent(IN)    :: JM
    integer, optional, intent(out)   :: rc

    integer :: status

    call dealloc_(A,RC=STATUS)
    _VERIFY(STATUS)

    select case (type)
    case (R4_2)
       _ASSERT(present(jm), 'jm not present for 2d')
       allocate(A%r4_2(IM,JM))
    case (R4_1)
       _ASSERT(.not.present(jm), 'jm is present for 1d')
       allocate(A%r4_1(IM))
    case (R8_2) 
       _ASSERT(present(jm), 'jm not present for 2d')
       allocate(A%r8_2(IM,JM))
    case (R8_1) 
       _ASSERT(.not.present(jm),'jm is present for 1d')
       allocate(A%r8_1(IM))
    case (i4_1) 
       _ASSERT(.not.present(jm), 'jm present for 1d')
       allocate(A%I4_1(IM))
    case (i4_2) 
       _ASSERT(present(jm), 'jm not present for 2d')
       allocate(A%I4_2(IM,JM))
    case default
       _ASSERT(.false., 'unsupported tkr')
    end select

    a%allocated=type

    _RETURN(ESMF_SUCCESS)
  end subroutine alloc_


  subroutine dealloc_(A,RC)
    type (Ptr), intent(INOUT)        :: A
    integer, optional, intent(out)   :: rc


    if(a%allocated/=not_allocated) then
       select case (a%allocated)
       case (R4_2) 
          if(associated(A%r4_2)) then
             deallocate(A%r4_2)
             nullify(A%r4_2)
          end if
       case (R4_1) 
          if(associated(A%r4_1)) then
             deallocate(A%r4_1)
             nullify(A%r4_1)
          end if
       case (R8_2) 
          if(associated(A%r8_2)) then
             deallocate(A%r8_2)
             nullify(A%r8_2)
          end if
       case (R8_1) 
          if(associated(A%r8_1)) then
             deallocate(A%r8_1)
             nullify(A%r8_1)
          end if
       case (i4_1) 
          if(associated(A%i4_1)) then
             deallocate(A%i4_1)
             nullify(A%i4_1)
          end if
       case (i4_2) 
          if(associated(A%i4_2)) then
             deallocate(A%i4_2)
             nullify(A%i4_2)
          end if
       case default
          _ASSERT(.false., 'unsupported tkr')
       end select
       a%allocated=not_allocated
    end if

    _RETURN(ESMF_SUCCESS)
  end subroutine dealloc_

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
#define RANK_ 1
#define VARTYPE_ 3
#include "arraygather.H"

!---------------------------
#define RANK_ 1
#define VARTYPE_ 4
#include "arraygather.H"

!---------------------------
#define RANK_ 2
#define VARTYPE_ 3
#include "arraygather.H"

!---------------------------
#define RANK_ 2
#define VARTYPE_ 4
#include "arraygather.H"


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

  subroutine MAPL_TileMaskGet(grid, mask, rc)
    type (ESMF_Grid),             intent(INout) :: GRID
    integer, pointer                            :: mask(:)
    integer,           optional , intent(  OUT) :: RC

! Local variables

    integer                               :: STATUS
    integer, pointer                      :: tileIndex(:)
    integer                               :: gcount(2), lcount(2)
    integer                               :: gsize, lsize
    integer                               :: gridRank
    integer                               :: n
    type (ESMF_DistGrid)                  :: distGrid

    integer,               allocatable    :: AL(:,:)
    integer,               allocatable    :: AU(:,:)
    integer, allocatable, dimension(:)    :: recvcounts, displs
    integer                               :: de, deId
    integer                               :: nDEs
    integer                               :: sendcount
    
    integer                               :: I
    integer                               :: I1, IN
    integer, allocatable                  :: var(:)
    type (ESMF_DELayout)                  :: layout

    type(ESMF_VM) :: vm
    logical :: amIRoot

    call ESMF_GridGet(grid, dimCount=gridRank, distGrid=distGrid, rc=status)
    _VERIFY(STATUS)
    _ASSERT(gridRank == 1, 'gridRank must be 1')

    call MAPL_GridGet(grid, globalCellCountPerDim=gcount, &
         localCellCountPerDim=lcount, RC=STATUS)
    _VERIFY(STATUS)

    gsize = gcount(1)
    lsize = lcount(1)

    call ESMF_DistGridGet(distgrid, localDe=0, elementCount=n, rc=status)
    _ASSERT(lsize == n, ' inconsistent lsize')

    allocate(tileIndex(lsize), stat=status)
    _VERIFY(STATUS)

    call ESMF_DistGridGet(distgrid, localDe=0, seqIndexList=tileIndex, rc=status)
    _VERIFY(STATUS)

    call ESMF_DistGridGet(distGRID, delayout=layout, rc=STATUS)
    _VERIFY(STATUS)
    call ESMF_DELayoutGet(layout, vm=vm, rc=status)
    _VERIFY(STATUS)
    call ESMF_VmGet(vm, localPet=deId, petCount=nDEs, rc=status)
    _VERIFY(STATUS)

    amIRoot = MAPL_AM_I_Root(vm)

    call ESMF_VmBarrier(vm, rc=status)
    _VERIFY(STATUS)

    if (.not. MAPL_ShmInitialized) then
       allocate(mask(gsize), stat=status)
       _VERIFY(STATUS)
    else
       call MAPL_AllocNodeArray(mask,(/gsize/),rc=STATUS)
       _VERIFY(STATUS)
    end if

    allocate (AL(gridRank,0:nDEs-1),  stat=status)
    _VERIFY(STATUS)
    allocate (AU(gridRank,0:nDEs-1),  stat=status)
    _VERIFY(STATUS)

    call MAPL_DistGridGet(distgrid, &
         minIndex=AL, maxIndex=AU, rc=status)
    _VERIFY(STATUS)

    allocate (recvcounts(0:nDEs-1), displs(0:nDEs), stat=status)
    _VERIFY(STATUS)

    if (.not. MAPL_ShmInitialized .or. amIRoot) then
       allocate(VAR(0:gsize-1), stat=status)
       _VERIFY(STATUS)
    else
       allocate(VAR(0), stat=status)
       _VERIFY(STATUS)
    end if
    
    displs(0) = 0
    do I = 0,nDEs-1
       de = I
       I1 = AL(1,I)
       IN = AU(1,I)

       recvcounts(I) = (IN - I1 + 1)
       if (de == deId) then
          sendcount = recvcounts(I)      ! Count I will send
       endif

       displs(I+1) = displs(I) + recvcounts(I)
    enddo

#ifdef NEW
    _ASSERT(.false., 'unsupported code block') !ALT this section is questionable
    do I = 0,nDEs-1
       de = I
       I1 = AL(1,I)
       IN = AU(1,I)
       var(I1:IN) = -9999
       if (de == deId) then
          var(I1:IN) = tileindex
       endif
       do II=I1,IN
          mmax=var(II)
          call MAPL_CommsAllReduceMax(vm, mmax, var(II), 1, rc=status)
          _VERIFY(STATUS)
       enddo
    end do
#else
    if (MAPL_ShmInitialized) then
       call MAPL_CommsGatherV(layout, tileindex, sendcount, &
                              var, recvcounts, displs, MAPL_Root, status)
       _VERIFY(STATUS)
    else
       call MAPL_CommsAllGatherV(layout, tileindex, sendcount, &
                                 var, recvcounts, displs, status)
       _VERIFY(STATUS)
    endif
#endif

    if (.not. MAPL_ShmInitialized .or. amIRoot) then
       do I = 0,nDEs-1
          mask(displs(I)+1:displs(I+1)) = I
       end do
       call MAPL_SORT(var,MASK)
    end if

! clean up
    
    deallocate(var)
    deallocate (recvcounts, displs)
    deallocate (AU)
    deallocate (AL)
    deallocate(tileIndex)

! mask is deallocated in the caller routine
       call MAPL_BroadcastToNodes(MASK, N=gsize, ROOT=MAPL_Root, rc=status)
       _VERIFY(STATUS)

    call MAPL_SyncSharedMemory(rc=status)
    _VERIFY(STATUS)

    _RETURN(ESMF_SUCCESS)
  end subroutine MAPL_TileMaskGet

!---------------------------
  subroutine MAPL_VarWriteNCpar_R4_4d(formatter, name, A, ARRDES, oClients, RC)

    type(Netcdf4_Fileformatter) , intent(IN   ) :: formatter
    character(len=*)            , intent(IN   ) :: name
    real(kind=ESMF_KIND_R4)     , intent(IN   ) :: A(:,:,:,:)
    type(ArrDescr)              , intent(INOUT) :: ARRDES
    type (ClientManager), optional, intent(inout)  :: oClients
    integer,           optional , intent(  OUT) :: RC

    integer                               :: status
    integer :: K, L

    !    MORE HERE
    do K = 1,size(A,4)
       do L = 1,size(A,3)
          call MAPL_VarWrite(formatter, name, A(:,:,L,K), arrdes=arrdes, &
               & oClients=oClients, lev=l, offset2=k, rc=status)
          _VERIFY(status)
       end do
    end do
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

    !    MORE HERE
    do K = 1,size(A,4)
       do L = 1,size(A,3)
          call MAPL_VarWrite(formatter, name, A(:,:,L,K), arrdes=arrdes, &
               & oClients=oClients, lev=l, offset2=k, rc=status)
          _VERIFY(status)
       end do
    end do
    _RETURN(ESMF_SUCCESS)
    
    !    MORE HERE
  end subroutine MAPL_VarWriteNCpar_R8_4d
!---------------------------

  subroutine MAPL_VarWriteNCpar_R4_3d(formatter, name, A, ARRDES, oClients, RC)

    type(Netcdf4_Fileformatter)           , intent(IN   ) :: formatter
    character(len=*)            , intent(IN   ) :: name
    real(kind=ESMF_KIND_R4)     , intent(IN   ) :: A(:,:,:)
    type(ArrDescr)              , intent(INOUT) :: ARRDES
    type (ClientManager), optional, intent(inout)  :: oClients
    integer,           optional , intent(  OUT) :: RC

    integer                               :: status
    integer :: l
    integer ::  i1, j1, in, jn,  global_dim(3)
    type(ArrayReference)     :: ref

    if (arrdes%write_restart_by_oserver) then
       _ASSERT(present(oClients), "output server is needed")
       call MAPL_GridGet(arrdes%grid,globalCellCountPerDim=global_dim,rc=status)
        _VERIFY(status)
       call MAPL_Grid_interior(arrdes%grid,i1,in,j1,jn)
       _ASSERT( i1 == arrdes%I1(arrdes%NX0), "interior starting i not match")
       _ASSERT( j1 == arrdes%j1(arrdes%NY0), "interior starting j not match")
       ref = ArrayReference(A)
       _ASSERT( size(a,1) == in-i1+1, "size not match")
       _ASSERT( size(a,2) == jn-j1+1, "size not match")
       call oClients%collective_stage_data(arrdes%collection_id,trim(arrdes%filename),trim(name), &
                      ref,start=[i1,j1,1], &
                      global_start=[1,1,1], global_count=[global_dim(1),global_dim(2),size(a,3)])
       _RETURN(_SUCCESS)
    endif

    do l=1,size(a,3)
       call MAPL_VarWrite(formatter,name,A(:,:,l),arrdes,lev=l, rc=status)
       _VERIFY(status)
    enddo

    _RETURN(ESMF_SUCCESS)
  end subroutine MAPL_VarWriteNCpar_R4_3d

!---------------------------

  subroutine MAPL_VarReadNCpar_R4_3d(formatter, name, A, ARRDES, RC)
  
    type (Netcdf4_Fileformatter)          , intent(IN   ) :: formatter
    character(len=*)            , intent(IN   ) :: name
    real(kind=ESMF_KIND_R4)     , intent(INOUT) :: A(:,:,:)
    type(ArrDescr)              , intent(INOUT) :: ARRDES
    integer,           optional , intent(  OUT) :: RC

    integer                               :: status
    integer :: l

    do l=1,size(a,3)
       call MAPL_VarRead(formatter,name,A(:,:,l),arrdes,lev=l, rc=status)
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

    integer ::  i1, j1, in, jn,  global_dim(3)
    type(ArrayReference)     :: ref


    if (arrdes%write_restart_by_oserver) then
       _ASSERT(present(oClients), "outpur server is needed")
       call MAPL_GridGet(arrdes%grid,globalCellCountPerDim=global_dim,rc=status)
        _VERIFY(status)
       call MAPL_Grid_interior(arrdes%grid,i1,in,j1,jn)
       _ASSERT( i1 == arrdes%i1(arrdes%NX0), "interior starting i not match")
       _ASSERT( j1 == arrdes%j1(arrdes%NY0), "interior starting j not match")
       ref = ArrayReference(A)
       _ASSERT( size(a,1) == in-i1+1, "size not match")
       _ASSERT( size(a,2) == jn-j1+1, "size not match")
       call oClients%collective_stage_data(arrdes%collection_id,trim(arrdes%filename),trim(name), &
                      ref,start=[i1,j1,1], &
                      global_start=[1,1,1], global_count=[global_dim(1),global_dim(2),size(a,3)])
       _RETURN(_SUCCESS)
    endif

    do l=1,size(a,3)
       call MAPL_VarWrite(formatter,name,A(:,:,l),arrdes,lev=l, rc=status)
       _VERIFY(status)
    enddo

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
    integer                               :: JM_WORLD
    integer                               :: status

    real(kind=ESMF_KIND_R4),  allocatable :: recvbuf(:)
    integer                               :: I,J,N,K,L,myrow,myiorank,ndes_x
    integer                               :: start(4), cnt(4)
    integer                               :: jsize, jprev, num_io_rows
    integer, allocatable                  :: recvcounts(:), displs(:)

    logical :: AM_WRITER
    type (ArrayReference) :: ref
    integer ::  i1, j1, in, jn,  global_dim(3)

    if (present(arrdes)) then
       if(arrdes%write_restart_by_oserver) then
          _ASSERT(present(oClients), "output server is needed")
          call MAPL_GridGet(arrdes%grid,globalCellCountPerDim=global_dim,rc=status)
           _VERIFY(status)
          call MAPL_Grid_interior(arrdes%grid,i1,in,j1,jn)
          _ASSERT( i1 == arrdes%I1(arrdes%NX0), "interior starting i1 not match")
          _ASSERT( j1 == arrdes%j1(arrdes%NY0), "interior starting j1 not match")
       
          ref = ArrayReference(A)
          _ASSERT( size(a,1) == in-i1+1, "size not match")
          _ASSERT( size(a,2) == jn-j1+1, "size not match")
          call oClients%collective_stage_data(arrdes%collection_id,trim(arrdes%filename),trim(name), &
                      ref,start=[i1,j1], &
                      global_start=[1,1], global_count=[global_dim(1),global_dim(2)])
          _RETURN(_SUCCESS)
       endif
    endif

    AM_WRITER = .false.
    if (present(arrdes)) then
       if (arrdes%writers_comm/=MPI_COMM_NULL) then
          AM_WRITER = .true.
       end if
    else
       AM_WRITER = .true.
    end if

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

          if(arrdes%write_restart_by_face) then
             start(2) = start(2) - (arrdes%face_index-1)*IM_WORLD
          endif

          call formatter%put_var(trim(name),VAR,start=start,count=cnt,rc=status)
          if(status /= nf_noerr) then
             print*,'Error writing variable ',status
             print*, NF_STRERROR(status)
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
          if(status /= nf_noerr) then
             print*,'Error writing variable ',status
             print*, NF_STRERROR(status)
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

    logical :: AM_READER 

    AM_READER = .false.
    if (present(arrdes)) then
       if (arrdes%readers_comm/=MPI_COMM_NULL) then
          AM_READER = .true.
       end if
    else
       AM_READER = .true.
    end if
      
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
          start(2) = arrdes%j1(myrow+1)
          start(3) = 1
          if (present(lev)) start(3) = lev
          start(4) = 1
          if (present(offset2)) start(4) = offset2
          cnt(1) = IM_WORLD
          cnt(2) = jsize
          cnt(3) = 1
          cnt(4) = 1
    
          if(arrdes%read_restart_by_face) then
             start(2) = start(2) - (arrdes%face_index-1)*IM_WORLD
          endif

          call formatter%get_var(trim(name),VAR,start=start,count=cnt,rc=status)   
          if(status /= nf_noerr) then
             print*,'Error reading variable ',status
             print*, NF_STRERROR(status)
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
       if(status /= nf_noerr) then
          print*,'Error reading variable ',status
          print*, NF_STRERROR(status)
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

    logical :: AM_WRITER

    AM_WRITER = .false.
    if (present(arrdes)) then
       if (arrdes%writers_comm/=MPI_COMM_NULL) then
          AM_WRITER = .true.
       end if
    else
       AM_WRITER = .true.
    end if

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
          if(status /= nf_noerr) then
             print*,'Error writing variable ', status
             print*, NF_STRERROR(status)
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

             if (arrdes%write_restart_by_face) then
                call MPI_COMM_RANK(arrdes%face_writers_comm, io_rank, STATUS)
                _VERIFY(STATUS)
             else
                call MPI_COMM_RANK(arrdes%writers_comm, io_rank, STATUS)
                _VERIFY(STATUS)
             endif

             if (io_rank == 0) then
                call formatter%put_var(trim(name),A,start=start,count=cnt,rc=status)
                if(status /= nf_noerr) then
                   print*,trim(IAm),'Error writing variable ',status
                   print*, NF_STRERROR(status)
                   _VERIFY(STATUS)
                endif
             endif ! io_rank = 0
          endif ! arrdes%writers_comm/=MPI_COMM_NULL
       else ! not present(arrdes)
          ! WY notes : it doesnot seem to get this branch
          call formatter%put_var(trim(name),A,start=start,count=cnt,rc=status)
          if(status /= nf_noerr) then
             print*,trim(IAm),' :Error writing variable: '// trim(name)
             print*, NF_STRERROR(status)
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

    logical :: AM_WRITER

    AM_WRITER = .false.
    if (present(arrdes)) then
       if (arrdes%writers_comm/=MPI_COMM_NULL) then
          AM_WRITER = .true.
       end if
    else
       AM_WRITER = .true.
    end if

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
          if(status /= nf_noerr) then
             print*,'Error writing variable ', status
             print*, NF_STRERROR(status)
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

             if(arrdes%write_restart_by_face) then
                call MPI_COMM_RANK(arrdes%face_writers_comm, io_rank, STATUS)
                _VERIFY(STATUS)
             else
                call MPI_COMM_RANK(arrdes%writers_comm, io_rank, STATUS)
                _VERIFY(STATUS)
             endif

             if (io_rank == 0) then
                call formatter%put_var(trim(name),A,start=start,count=cnt,rc=status)
                if(status /= nf_noerr) then
                   print*,trim(IAm),'Error writing variable ',status
                   print*, NF_STRERROR(status)
                   _VERIFY(STATUS)
                endif
             endif ! io_rank
           endif

       else
          !WJ notes : not here
          call formatter%put_var(trim(name),A,start=start,count=cnt,rc=status)
          if(status /= nf_noerr) then
             print*,trim(IAm),'Error writing variable ',status
             print*, NF_STRERROR(status)
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

    logical :: AM_READER

    AM_READER = .false.
    if (present(arrdes)) then
       if (arrdes%readers_comm/=MPI_COMM_NULL) then
          AM_READER = .true.
       end if
    else
       AM_READER = .true.
    end if

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

          call formatter%get_var(trim(name),var,start=start,count=cnt,rc=status)
          if(status /= nf_noerr) then
             print*,'Error reading variable ',status
             print*, NF_STRERROR(status)
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
             if(status /= nf_noerr) then
                print*,trim(IAm),'Error reading variable ',status
                print*, NF_STRERROR(status)
                _VERIFY(STATUS)
             endif
          endif
          call MAPL_CommsBcast(layout, A, size(A), MAPL_Root, status)
          _VERIFY(STATUS)
       else
          call formatter%get_var(trim(name),A,start=start,count=cnt,rc=status)
          if(status /= nf_noerr) then
             print*,trim(IAm),'Error reading variable ',status
             print*, NF_STRERROR(status)
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

    logical :: AM_READER

    AM_READER = .false.
    if (present(arrdes)) then
       if (arrdes%readers_comm/=MPI_COMM_NULL) then
          AM_READER = .true.
       end if
    else
       AM_READER = .true.
    end if

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
          if(status /= nf_noerr) then
             print*,'Error reading variable ',status
             print*, NF_STRERROR(status)
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
             if(status /= nf_noerr) then
                print*,trim(IAm),'Error reading variable ',status
                print*, NF_STRERROR(status)
                _VERIFY(STATUS)
             endif
          endif
          call MAPL_CommsBcast(layout, A, size(A), MAPL_Root, status)
          _VERIFY(STATUS)
       else
          call formatter%get_var(trim(name),A,start=start,count=cnt,rc=status)
          if(status /= nf_noerr) then
             print*,trim(IAm),'Error reading variable ',status
             print*, NF_STRERROR(status)
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

    logical :: AM_WRITER
    type (ArrayReference) :: ref
    integer ::  i1, j1, in, jn,  global_dim(3)

    if (present(arrdes)) then
       if( arrdes%write_restart_by_oserver) then
          _ASSERT(present(oClients), "output server is needed")
          call MAPL_GridGet(arrdes%grid,globalCellCountPerDim=global_dim,rc=status)
           _VERIFY(status)
          call MAPL_Grid_interior(arrdes%grid,i1,in,j1,jn)
          _ASSERT( i1 == arrdes%I1(arrdes%NX0), "interior starting not match")
          _ASSERT( j1 == arrdes%j1(arrdes%NY0), "interior starting not match")
          ref = ArrayReference(A)
          _ASSERT( size(a,1) == in-i1+1, "size not match")
          _ASSERT( size(a,2) == jn-j1+1, "size not match")
          call oClients%collective_stage_data(arrdes%collection_id,trim(arrdes%filename),trim(name), &
                      ref,start=[i1,j1], &
                      global_start=[1,1], global_count=[global_dim(1),global_dim(2)])
          _RETURN(_SUCCESS)
       endif
    endif


    AM_WRITER = .false.
    if (present(arrdes)) then
       if (arrdes%writers_comm/=MPI_COMM_NULL) then
          AM_WRITER = .true.
       end if
    else
       AM_WRITER = .true.
    end if

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

          if(arrdes%write_restart_by_face) then
             start(2) = start(2) - (arrdes%face_index-1)*IM_WORLD
          endif

          call formatter%put_var(trim(name),VAR,start=start,count=cnt,rc=status)
          if(status /= nf_noerr) then
             print*,'Error writing variable ',status
             print*, NF_STRERROR(status)
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
       if(status /= nf_noerr) then
          print*,'Error writing variable ',status
          print*, NF_STRERROR(status)
          _VERIFY(STATUS)
       endif

    end if

    _RETURN(ESMF_SUCCESS)
  end subroutine MAPL_VarWriteNCpar_R8_2d

!---------------------------

  subroutine MAPL_VarReadNCpar_R8_2d(formatter, name, A, ARRDES, lev, RC)
  
    type(Netcdf4_Fileformatter)           , intent(IN   ) :: formatter
    character(len=*)            , intent(IN   ) :: name
    real(kind=ESMF_KIND_R8)     , intent(INOUT) :: A(:,:)
    type(ArrDescr),    optional , intent(INOUT) :: ARRDES
    integer,           optional , intent(IN   ) :: lev
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

    logical :: AM_READER

    AM_READER = .false.
    if (present(arrdes)) then
       if (arrdes%readers_comm/=MPI_COMM_NULL) then
          AM_READER = .true.
       end if
    else
       AM_READER = .true.
    end if

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
          start(2) = arrdes%j1(myrow+1)
          start(3) = 1
          if (present(lev)) start(3)=lev
          start(4) = 1
          cnt(1) = IM_WORLD
          cnt(2) = jsize
          cnt(3) = 1
          cnt(4) = 1
         
          if(arrdes%read_restart_by_face) then
             start(2) = start(2) - (arrdes%face_index-1)*IM_WORLD
          endif

          call formatter%get_var(trim(name),VAR,start=start,count=cnt,rc=status) 
          if(status /= nf_noerr) then
                  print*,'Error reading variable ',status
                  print*, NF_STRERROR(status)
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
       cnt(1) = size(a,1)
       cnt(2) = size(a,2)
       cnt(3) = 1
       cnt(4) = 1

       call formatter%get_var(trim(name),A,start=start,count=cnt,rc=status) 
       if(status /= nf_noerr) then
               print*,'Error reading variable ',status
               print*, NF_STRERROR(status)
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

    integer                            :: MAPL_DIMS
    integer, pointer                   :: MASK(:) => null()
    type(Netcdf4_Fileformatter)        :: formatter
    type(FileMetaData)                 :: metadata
    character(len=:), allocatable      :: fname_by_face
    logical :: grid_file_match

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
          if(arrdes%read_restart_by_face) then
             fname_by_face = get_fname_by_face(trim(filename),arrdes%face_index)
             call formatter%open(trim(fname_by_face),pFIO_READ,comm=arrdes%face_readers_comm,info=info,rc=status)
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
       _ASSERT(grid_file_match,"File grid dimensions in "//trim(filename)//" do not match grid")
    endif

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

      call MAPL_FieldReadNCPar(formatter, FieldName, field, arrdes=arrdes, HomePE=mask, rc=status)
      _VERIFY(STATUS)
        
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
    character(len=ESMF_MAXSTR), pointer :: VarNamesFile(:) => null()
    type(ESMF_VM)                      :: VM
    logical                            :: foundInFile
    integer                            :: dna
    logical                            :: bootstrapable_
    logical                            :: isPresent
    character(len=:), allocatable      :: fname_by_face 
    ! get a list of variables in the file so we can skip if the 
    ! variable in the state is not in the file and it is bootstrapable
    ! will just let root do this since everybody will need it
    ! and avoid complications with doing later on when only readers_comm has opened file

    call ESMF_VMGetCurrent(VM,rc=status)
    _VERIFY(STATUS)

    if (MAPL_AM_I_Root()) then
       if(arrdes%read_restart_by_face) then
          fname_by_face = get_fname_by_face(filename, 1)
          status = nf_open(trim(fname_by_face),NF_NOWRITE, ncid) ! just pick one
          _VERIFY(STATUS)
       else
          status = nf_open(trim(filename),NF_NOWRITE, ncid)
          _VERIFY(STATUS)
       endif
       status = nf_inq_nvars(ncid, nVarFile)
       _VERIFY(STATUS)
    end if

    call MAPL_CommsBcast(vm, nVarFile, n=1, ROOT=MAPL_Root, rc=status)
    _VERIFY(STATUS)
    allocate(VarNamesFile(nVarFile),stat=status)
    _VERIFY(STATUS)

    if (MAPL_AM_I_Root()) then
       do i=1,nVarFile
          status = nf_inq_varname(ncid, i, VarNamesFile(i))
          _VERIFY(STATUS)
       end do
       status = nf_close(ncid)
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
                     _ASSERT(.false., "  Could not find field "//trim(FieldName)//" in "//trim(filename))
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
             if (skipReading) cycle
             call ESMF_AttributeGet(field, name='doNotAllocate', isPresent=isPresent, rc=status)
             _VERIFY(STATUS)
             if (isPresent) then
                call ESMF_AttributeGet(field, name='doNotAllocate', value=DNA, rc=status)
                _VERIFY(STATUS)
                skipReading = (DNA /= 0)
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
                    _ASSERT(.false., "  Could not find field "//trim(Fieldname)//" in "//trim(filename))
                end if
             end if

          end if

       end if

    end do

    tile = arrdes%tile

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

  subroutine MAPL_BundleWriteNCPar(Bundle, arrdes, CLOCK, filename, oClients, rc)
    type(ESMF_FieldBundle), intent(inout)   :: Bundle
    type(ArrDescr), intent(inout)           :: arrdes
    type(ESMF_Clock), intent(in)            :: CLOCK
    character(len=*), intent(in  )         :: filename
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
    integer                               :: myungriddim1, myungriddim2
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
    integer :: pfDataType
    character(len=:), allocatable         :: fname_by_face

    integer                            :: STATUS
    type (StringIntegerMap), save      :: RstCollections
    type (StringIntegerMapIterator)    :: iter
    type (StringVariableMap) :: var_map

    call ESMF_FieldBundleGet(Bundle,FieldCount=nVars, name=BundleName, rc=STATUS)
    _VERIFY(STATUS)


    ! verify that file is compatible with fields in bundle we are reading

    if (nVars == 0) then
       _ASSERT(.false., "The bundle you are trying to write is empty")
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
                _ASSERT(.false., "Unsupported DIMS type")
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
                _ASSERT(.false., "Unsupported DIMS type")
             end if
          else
             _ASSERT(.false., "Unsupported type/rank")
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
    if (arrdes%writers_comm/=MPI_COMM_NULL .or. arrdes%write_restart_by_oserver) then

       ! Create dimensions as needed
       if (Have_HorzVert .or. Have_HorzOnly) then

          if (arrdes%IM_WORLD*6 == arrdes%JM_WORLD) then
             isCubed = .true.
             x0=1.0d0
             x1=dble(arrdes%IM_WORLD)
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
             x1=dble(arrdes%JM_WORLD)
          else
             x0=-90.0d0
             x1=90.0d0
          endif
          lat = MAPL_Range(x0,x1,arrdes%JM_WORLD)
          
          if (arrdes%write_restart_by_face) then
             call cf%add_dimension('lat',arrdes%im_world,rc=status)
             _VERIFY(status)
             block
                integer :: j0, j1
                j0 = (arrdes%face_index -1)*arrdes%im_world+1
                j1 = arrdes%face_index * arrdes%im_world
                allocate(coordinate_data,source=lat(j0:j1))
             end block
             allocate(var,source=CoordinateVariable(Variable(type=pFIO_REAL64,dimensions='lat'),coordinate_data))
          else
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
             call var%add_attribute('positive','down')
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
             call var%add_attribute('positive','down')
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
   !ALT                if (tk .eq. ESMF_TYPEKIND_I1) DataType = NF_BYTE
   !ALT                if (tk .eq. ESMF_TYPEKIND_I2) DataType = NF_SHORT
          if (tk .eq. ESMF_TYPEKIND_I4) DataType = NF_INT
          if (tk .eq. ESMF_TYPEKIND_R4) DataType = NF_FLOAT
          if (tk .eq. ESMF_TYPEKIND_R8) DataType = NF_DOUBLE
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
                   _ASSERT(.false., 'ERROR: LOCATION not recognized for rank 1')
                endif
             elseif(DIMS(1)==MAPL_DimsTileOnly) then
                call add_fvar(cf,trim(fieldname),pfDataType,'tile',units,long_name,rc=status)
                _VERIFY(status)
             elseif(DIMS(1)==MAPL_DimsNone) then
                found = .false.
                do j=1,n_unique_ungrid_dims
                   if (ungrid_dims(i,1) == unique_ungrid_dims(j) ) then
                      myungriddim1 = j
                      myUngridDimName1 = trim(unique_ungrid_dim_name(j))
                      found = .true.
                      exit
                   end if
                end do
                _ASSERT(found, 'search failed')
                call add_fvar(cf,trim(fieldname),pfDataType,myUngridDimName1,units,long_name,rc=status)
                _VERIFY(status)
             else
                _ASSERT(.false., 'unsupported Dims case')
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
                      myungriddim1 = j
                      myUngridDimName1 = trim(unique_ungrid_dim_name(j))
                      exit
                   end if
                end do
                call add_fvar(cf,trim(fieldname),pfDataType,'tile,'//myUngridDimName1,units,long_name,rc=status)
                _VERIFY(status)
             else
                write(buffer,*)'ERROR: DIMS not recognized for rank 2 variable ',trim(FieldName), DIMS(1)
                _ASSERT(.false., trim(buffer))
             endif

          else if(arrayRank == 3) then
             if (DIMS(1)==MAPL_DimsHorzVert) then
                if (LOCATION(1) == MAPL_VLocationCenter) then
                   call add_fvar(cf,trim(fieldname),pfDataType,'lon,lat,lev',units,long_name,rc=status)
                   _VERIFY(status)
                else if(LOCATION(1) == MAPL_VLocationEdge) then
                   call add_fvar(cf,trim(fieldname),pfDataType,'lon,lat,edge',units,long_name,rc=status)
                   _VERIFY(status)
                else
                   _ASSERT(.false., 'ERROR: LOCATION not recognized for rank 3')
                endif
             else if(DIMS(1)==MAPL_DimsHorzOnly) then
                do j=1,n_unique_ungrid_dims
                   if (ungrid_dims(i,1) == unique_ungrid_dims(j) ) then
                      myungriddim1 = j
                      myUngridDimName1 = trim(unique_ungrid_dim_name(j))
                      exit
                   end if
                end do
                call add_fvar(cf,trim(fieldname),pfDataType,'lon,lat,'//myUngridDimName1,units,long_name,rc=status)
                _VERIFY(status)
             else if (DIMS(1)==MAPL_DimsTileOnly) then
                do j=1,n_unique_ungrid_dims
                   if (ungrid_dims(i,1) == unique_ungrid_dims(j) ) then
                      myungriddim1 = j
                      myUngridDimName1 = trim(unique_ungrid_dim_name(j))
                      exit
                   end if
                end do
                do j=1,n_unique_ungrid_dims
                   if (ungrid_dims(i,2) == unique_ungrid_dims(j) ) then
                      myungriddim2 = j
                      myUngridDimName2 = trim(unique_ungrid_dim_name(j))
                      exit
                   end if
                end do
                call add_fvar(cf,trim(fieldname),pfDataType,'tile,'//myUngridDimName1//','//myUngridDimName2,units,long_name,rc=status)
                _VERIFY(status)
             else if(DIMS(1)/=MAPL_DimsHorzVert .and. DIMS(1)/=MAPL_DimsHorzOnly) then
                _ASSERT(.false., 'ERROR: What else could it be')
             endif
          else if(arrayRank == 4) then
             if (DIMS(1)==MAPL_DimsHorzVert) then
                do j=1,n_unique_ungrid_dims
                   if (ungrid_dims(i,1) == unique_ungrid_dims(j) ) then
                      myungriddim1 = j
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
                   _ASSERT(.false., 'ERROR: LOCATION not recognized for rank 4')
                endif
             else if(DIMS(1)==MAPL_DimsHorzOnly) then
                do j=1,n_unique_ungrid_dims
                   if (ungrid_dims(i,1) == unique_ungrid_dims(j) ) then
                      myungriddim1 = j
                      myUngridDimName1 = trim(unique_ungrid_dim_name(j))
                      exit
                   end if
                end do
                do j=1,n_unique_ungrid_dims
                   if (ungrid_dims(i,2) == unique_ungrid_dims(j) ) then
                      myungriddim2 = j
                      myUngridDimName2 = trim(unique_ungrid_dim_name(j))
                      exit
                   end if
                end do
                call add_fvar(cf,trim(fieldname),pfDataType,'lon,lat,'//myUngridDimName1//','//myUngridDimName2,units,long_name,rc=status)
                _VERIFY(status)
             else if (DIMS(1)==MAPL_DimsTileOnly .or. &
                  DIMS(1)==MAPL_DimsTileTile) then
                _ASSERT(.false., 'ERROR: tiles with 2 or more UNGRIDDED dims not supported')
             else
                _ASSERT(.false., 'ERROR: What else could it be')
             endif
          else
             write(buffer,*) 'ERROR: arrayRank ',arrayRank, ' not supported'
             _ASSERT(.false., trim(buffer))
          endif

       enddo

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


       if (arrdes%write_restart_by_oserver) then
          _ASSERT(present(oClients), 'output server is needed')
          call oClients%set_optimal_server(1)
          iter = RstCollections%find(trim(BundleName))
          if (iter == RstCollections%end()) then
             arrdes%collection_id = oClients%add_hist_collection(cf)
             call RstCollections%insert(trim(BundleName), arrdes%collection_id)
          else
             arrdes%collection_id = iter%value()
             call oClients%modify_metadata(arrdes%collection_id, var_map = var_map, rc=status)
             _VERIFY(status)
          endif
          arrdes%filename = trim(filename)
       else ! not written by oserver

          if (arrdes%num_writers == 1) then
             call formatter%create(trim(filename), rc=status)
             _VERIFY(status)
             call formatter%write(cf,rc=status)
             _VERIFY(STATUS)
          else
             if (arrdes%write_restart_by_face) then
                fname_by_face = get_fname_by_face(trim(filename),arrdes%face_index)
                call formatter%create_par(trim(fname_by_face),comm=arrdes%face_writers_comm,info=info,rc=status)
                _VERIFY(status)
             else
                call formatter%create_par(trim(filename),comm=arrdes%writers_comm,info=info,rc=status)
                _VERIFY(status)
             endif
             call formatter%write(cf,rc=status)
             _VERIFY(STATUS)
          end if
       endif ! write_restart_by_oserver 

    endif !am writer or write_restart_by_oserver

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
       
    enddo

    
    if (arrdes%write_restart_by_oserver) then
       call oClients%done_collective_stage()
       call oClients%wait()
       call MPI_Info_free(info, status)
       _VERIFY(STATUS)
    elseif (arrdes%writers_comm/=MPI_COMM_NULL) then
       call formatter%close()
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

  subroutine MAPL_StateVarWriteNCPar(filename, STATE, ARRDES, CLOCK, NAME, forceWriteNoRestart, oClients, RC)
    character(len=*)            , intent(IN   ) :: filename
    type (ESMF_State)           , intent(IN   ) :: STATE
    type(ArrDescr)              , intent(INOUT) :: ARRDES
    type(ESMF_Clock)            , intent(IN   ) :: CLOCK
    character(len=*),   optional, intent(IN   ) :: NAME
    logical,            optional, intent(IN   ) :: forceWriteNoRestart
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

    type (ESMF_Field)                  :: new_field
    type (ESMF_FieldBundle)            :: bundle_write
    integer                            :: nBundle
    logical                            :: isPresent

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
    if(present(forceWriteNoRestart)) then
       forceWriteNoRestart_ = forceWriteNoRestart
    endif

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
             if (skipWriting) cycle

             call ESMF_AttributeGet(field, name='doNotAllocate', isPresent=isPresent, rc=status)
             _VERIFY(STATUS) 
             if (isPresent) then
                call ESMF_AttributeGet(field, name='doNotAllocate', value=dna, rc=status)
                _VERIFY(STATUS)
                skipWriting = (dna /= 0)
             endif
             if (skipWriting) cycle

             call MAPL_FieldBundleAdd(bundle_write,field,rc=status)
             _VERIFY(STATUS)

          end IF
       END IF

    END DO

    deallocate(ITEMNAMES)
    deallocate(ITEMTYPES)
    deallocate(DOIT     )

    call MAPL_BundleWriteNCPar(Bundle_Write, arrdes, CLOCK, filename, oClients=oClients, rc=status)
    _VERIFY(STATUS)

    _RETURN(ESMF_SUCCESS)

  end subroutine MAPL_StateVarWriteNCPar

  subroutine MAPL_NCIOGetFileType(filename,filetype,rc)
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
   logical                      :: typehdf5


   UNIT = 10
   INQUIRE(IOLENGTH=IREC) WORD
   open (UNIT=UNIT, FILE=FILENAME, FORM='unformatted', ACCESS='DIRECT', RECL=IREC, IOSTAT=status)
   _VERIFY(STATUS)

! Read first 8 characters and compare with HDF5 signature
   read (UNIT, REC=1, ERR=100) TwoWords(1:4)
   read (UNIT, REC=2, ERR=100) TwoWords(5:8)
   close(UNIT)

   typehdf5 = .true.
   filetype = -1 ! Unknown

   do i = 1, 8
      if (iachar(TwoWords(i)) /= hdf5(i)) then
         typehdf5 = .false.
         exit
      end if
   end do
   if (typehdf5) then
      filetype = 0 ! HDF5
      _RETURN(ESMF_SUCCESS)

   end if
   ! Attempt to identify as fortran binary
   cwrd = transfer(TwoWords(1:4), irec)
   ! check if divisible by 4 
   irec = cwrd/4
   filetype = irec
   if (cwrd /= 4*irec) then
      _RETURN(ESMF_FAILURE)
   end if

   filetype = -1
   _RETURN(ESMF_SUCCESS)

100   continue
   _RETURN(ESMF_FAILURE)

   end subroutine MAPL_NCIOGetFileType

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
     _ASSERT(.false., 'unsupported subclass for units')
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

   ! WJ notes: To avoid changing gcm_run.j script, insert "_face_x_", not append
   function get_fname_by_face(fname, face) result(name)
     character(len=:), allocatable :: name
     character(len=*), intent(in) :: fname
     integer, intent(in) :: face
     integer :: i

     i= index(fname,'_checkpoint')
     if (i /= 0) then
        name = fname(1:i-1)//'_face_'//i_to_string(face)//trim(fname(i:))
        return
     end if
     i= index(fname,'_rst')
     if (i /= 0) then
        name = fname(1:i-1)//'_face_'//i_to_string(face)//trim(fname(i:))
        return
     endif
     name = trim(fname)//'_face_'//i_to_string(face)

   end function get_fname_by_face

end module MAPL_IOMod
