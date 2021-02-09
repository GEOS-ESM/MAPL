
#include "MAPL_Generic.h"
#define MPI_NULL_TAG 99

#define DEALOC_(A) if(associated(A))then;A=0;if(MAPL_ShmInitialized)then; call MAPL_DeAllocNodeArray(A,rc=STATUS);else; deallocate(A,stat=STATUS);endif;_VERIFY(STATUS);NULLIFY(A);endif

#define DEALOC2_(A) if(associated(A)) then; deallocate(A, stat=STATUS); _VERIFY(STATUS); NULLIFY(A); endif

#include "unused_dummy.H"

module MAPL_CFIOMod
   use MAPL_ExceptionHandling
!BOP

! !MODULE: MAPL_CFIO --- CF Compliant I/O for ESMF

! !DESCRIPTION:  
!
! \input{MAPL_CFIODescr.tex}
!

! !USES:
!
  use ESMF
  use MAPL_BaseMod
  use MAPL_CommsMod
  use MAPL_ConstantsMod
  use ESMF_CFIOMod  
  use ESMF_CFIOUtilMod
  use ESMF_CFIOFileMod
  use MAPL_IOMod
  use ESMFL_Mod
  use MAPL_ShmemMod
  use MAPL_CFIOServerMod
  use MAPL_SortMod
  use MAPL_GridManagerMod
  use MAPL_RegridderManagerMod
  use MAPL_NewRegridderManager
  use MAPL_AbstractRegridderMod
  use MAPL_ConfigMod
  use mapl_RegridMethods
  use MAPL_MemUtilsMod
  use ESMF_CFIOCollectionVectorMod
  use ESMF_CFIOCollectionMod
  use PFIO
  use gFTL_IntegerVector
  use MAPL_StringTemplate

  use, intrinsic :: ISO_C_BINDING

  use, intrinsic :: iso_fortran_env, only: REAL64
  
  implicit none
  private

! !PUBLIC MEMBER FUNCTIONS:

  ! MAPL-style names
  ! ----------------
  public MAPL_CFIOCreate
  public MAPL_CFIOSet
  public MAPL_CFIOGet
  public MAPL_CFIOSetKrank
  public MAPL_CFIOOpenWrite
  public MAPL_CFIOCreateWrite
  public MAPL_CFIOClose
  public MAPL_CFIOWrite
  public MAPL_CFIOWriteBundlePost
  public MAPL_CFIOWriteBundleWait
  public MAPL_CFIOWriteBundleWrite
  public MAPL_CFIORead
  public MAPL_CFIODestroy
  public MAPL_GetCurrentFile
  public MAPL_CFIOIsCreated
  public MAPL_CFIOGetFilename
  public MAPL_CFIOGetTimeString
  public MAPL_CFIOStartAsyncColl
  public MAPL_CFIOBcastIONode

  public MAPL_CFIOPartition
  public MAPL_CFIOCreateFromFile
  public MAPL_CFIOReadBundleRead
  public MAPL_CFIOReadBundleWait
  public MAPL_CFIOReadParallel
  public MAPL_CFIOAddCollection
  !public MAPL_ExtDataAddCollection
  public MAPL_CFIOReadBundlePrefetch
  public MAPL_CFIOReadBundleReadPrefetch
  public MAPL_CFIOGetTimeFromIndex

  ! ESMF-style names
  ! ----------------
  public ESMF_ioRead     ! another name for MAPL_CFIORead
  public ESMF_ioCreate   ! another name for MAPL_CFIOCreate
  public ESMF_ioWrite    ! another name for MAPL_CFIOWrite
  public ESMF_ioDestroy  ! another name for MAPL_CFIODestroy 

! !PUBLIC TYPES:
!
  public MAPL_CFIO
  public collections

!EOP

! !METHOD OVERLOADING:

!                     MAPL Consistent Naming Convention
!                     ---------------------------------

  interface MAPL_CFIOStartAsyncColl
     module procedure MAPL_CFIOStartAsyncColl
  end interface

  interface MAPL_CFIOBcastIONode
     module procedure MAPL_CFIOBcastIONode
  end interface

  interface MAPL_CFIOCreate
     module procedure MAPL_CFIOCreateFromBundle
     module procedure MAPL_CFIOCreateFromState
  end interface

  interface MAPL_CFIOWrite
     module procedure MAPL_CFIOWriteState
     module procedure MAPL_CFIOWriteBundle
  end interface

  interface MAPL_CFIORead
     module procedure MAPL_CFIOReadState
     module procedure MAPL_CFIOReadBundle
     module procedure MAPL_CFIOReadField
     module procedure MAPL_CFIOReadArray3D
     module procedure MAPL_CFIOReadArray2D
  end interface

  interface MAPL_CFIOReadParallel
     module procedure MAPL_CFIOReadParallel_
  end interface


!                     ESMF Consistent Naming Convention
!                     ---------------------------------

  interface ESMF_ioCreate
     module procedure MAPL_CFIOCreateFromBundle
     module procedure MAPL_CFIOCreateFromState
  end interface

  interface ESMF_ioRead
     module procedure MAPL_CFIOReadState
     module procedure MAPL_CFIOReadBundle
     module procedure MAPL_CFIOReadField
     module procedure MAPL_CFIOReadArray3D
     module procedure MAPL_CFIOReadArray2D
  end interface

  interface ESMF_ioWrite
     module procedure MAPL_CFIOWriteState
     module procedure MAPL_CFIOWriteBundle
  end interface

  interface ESMF_ioDestroy
     module procedure MAPL_CFIODestroy
  end interface

  type Ptr3Arr
     real, pointer              :: Ptr(:,:,:)
  end type Ptr3Arr

  type Ptr2Arr
     real, pointer              :: Ptr(:,:)
  end type Ptr2Arr

  integer, parameter :: maxStoredCoords = 10
  type StoredGlobalCoords
     real(KIND=REAL64), pointer :: LONS2D(:) => NULL()
     real(KIND=REAL64), pointer :: LATS2D(:) => NULL()
     logical               :: created = .false.
     integer               :: IM = 0 
     integer               :: JM = 0 
  end type StoredGlobalCoords
     
  !BOP
  !BOC

  type :: MCFIO_Variable
     integer :: request_id
     integer :: num_dimensions
     real, allocatable :: data(:,:,:)
  end type MCFIO_VARIABLE

  type :: MAPL_CFIO
     private 
     logical                    :: CREATED=.false.
     character(len=ESMF_MAXSTR) :: NAME
     character(len=ESMF_MAXPATHLEN) :: fNAME
     character(len=ESMF_MAXSTR) :: format
     character(len=ESMF_MAXSTR) :: expid
     type(ESMF_CFIO)            :: CFIO
     integer                    :: XYOFFSET
     real                       :: VSCALE
     type(ESMF_TIMEINTERVAL)    :: OFFSET
     type(ESMF_CLOCK)           :: CLOCK
     type(ESMF_FIELDBUNDLE)     :: BUNDLE
     type(ESMF_GridComp)        :: GC
     type(ESMF_Grid)            :: Grid
     integer                    :: Root=1
     integer                    :: PartSize=1
     integer                    :: myPE
     integer                    :: numcores
     integer                    :: comm
     integer                    :: Order=-1
     integer                    :: Nbits=1000
     integer                    :: IM, JM, LM
     integer, pointer           :: VarDims(:)=>null()
     integer, pointer           :: VarType(:)=>null()
     integer, pointer           :: needVar(:)=>null()
     integer, pointer           :: pairList(:)=>null()
     logical                    :: doRotate
     character(len=ESMF_MAXSTR), &
                        pointer :: vectorList(:,:)=>null()
     logical                    :: Vinterp=.false.
     real                       :: pow=0.0
     character(len=ESMF_MAXSTR) :: Vvar
     character(len=3          ) :: Func
     character(len=ESMF_MAXSTR), &
                        pointer :: VarName(:)=>null()
     integer, pointer           :: Krank(:)=>null()
     integer                    :: rootRank = 0
     real,    pointer           :: levs(:)=>null()
     real,    pointer           :: unmodifiedLevs(:)=>null()
     type(MAPL_CommRequest), &
                        pointer :: reqs(:)=>null()
     class (AbstractRegridder), pointer :: regridder => null()
     class (AbstractRegridder), pointer :: new_regridder => null()
     integer :: regrid_method
     type (ESMF_Grid)           :: output_grid  
     logical                    :: async
     integer                    :: AsyncWorkRank
     integer                    :: globalComm
     logical                    :: regridConservative
     logical                    :: newFormat = .false.
     logical                    :: useFaceDim = .false.
     type(Ptr2Arr), pointer     :: Buffer(:) => null()
     logical                    :: gsiMode = .false.
     integer, pointer           :: varID(:) => null()
     logical                    :: kreverse,xshift
     integer                    :: collection_id
     ! new stuff for server
     integer                    :: scollection_id
     integer, pointer           :: request_ids(:) => null()
     type(Ptr2Arr), pointer     :: rBuffer(:)
     character(len=ESMF_MAXSTR), &
                        pointer :: levVarName(:)=>null()
     integer                    :: fraction
     integer                    :: regrid_type
     real, pointer              :: surfaceLayer(:,:) => null()
     logical                    :: ascending

     ! TLC components used in the new ESMF regrid variant
     integer :: n_vars
     type (MCFIO_Variable), allocatable :: variables(:)
     
     
  end type MAPL_CFIO
  !EOC
  !EOP
  integer, parameter :: trans_tag=9999

  integer, parameter :: CFIOMaxAge=60
  integer,dimension(CFIOMaxAge) :: CFIOAge
  type(ESMF_CFIO),dimension(CFIOMaxAge),target :: CFIORegister

  type(StoredGlobalCoords)    :: storedCoords(maxStoredCoords)

  type(CFIOCollectionVector) :: collections

  include "mpif.h"

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!BOP
 
! !IROUTINE: MAPL_CFIOCreate --- Creates a MAPL CFIO Object
!
! !IIROUTINE: MAPL_CFIOCreateFromBundle --- Creates MAPL CFIO Object from a Bundle

!
! !INTERFACE:
!
  subroutine MAPL_CFIOCreateFromBundle ( MCFIO, NAME, CLOCK, BUNDLE, OFFSET,      &
                                         OUTPUT_GRID, CHUNKSIZE, FREQUENCY, LEVELS, DESCR,    &
                                         XYOFFSET, VCOORD, VUNIT, VSCALE,         &
                                         SOURCE, INSTITUTION, COMMENT, CONTACT,   &
                                         FORMAT, EXPID, DEFLATE, GC,  ORDER, &
                                         NumCores, nbits, TM, Conservative,  &
                                         Async, VectorList, itemOrder, RC )
! !ARGUMENTS:
!
    type(MAPL_CFIO),             intent(OUT) :: MCFIO
    character(LEN=*),            intent(IN)  :: NAME
    type(ESMF_FIELDBUNDLE),      intent(INout)  :: BUNDLE
    type(ESMF_CLOCK),            intent(INout):: CLOCK
    type(ESMF_TIMEINTERVAL), &
         optional,   intent(INout):: OFFSET
    type (ESMF_Grid), optional, pointer :: OUTPUT_GRID
    integer,         optional,   pointer     :: CHUNKSIZE(:)
    integer,         optional,   intent(IN)  :: FREQUENCY
    real,            optional,   pointer     :: LEVELS(:)
    character(LEN=*),optional,   intent(IN)  :: DESCR
    integer,         optional,   intent(IN)  :: XYOFFSET
    real,            optional,   intent(IN)  :: VSCALE
    integer,         optional,   intent(IN)  :: DEFLATE
    character(len=*),optional,   intent(IN)  :: VUNIT     
    character(len=*),optional,   intent(IN)  :: VCOORD     
    character(len=*),optional,   intent(IN)  :: source
    character(len=*),optional,   intent(IN)  :: institution     
    character(len=*),optional,   intent(IN)  :: comment
    character(len=*),optional,   intent(IN)  :: contact     
    character(len=*),optional,   intent(IN)  :: format
    character(len=*),optional,   intent(IN)  :: EXPID
    integer,         optional,   intent(IN)  :: Conservative
    type(ESMF_GridComp),optional,intent(IN)  :: GC
    integer,         optional,   intent(IN)  :: Order
    integer,         optional,   intent(IN)  :: Nbits
    integer,         optional,   intent(IN)  :: NumCores
    integer,         optional,   intent(IN)  :: TM
    logical,         optional,   intent(IN)  :: Async
    character(len=*),optional,   pointer     :: vectorList(:,:)
    type(ESMF_ItemOrder_Flag),   optional,   intent(IN)  :: itemOrder
    integer,         optional,   intent(OUT) :: RC

#ifdef ___PROTEX___
!
   !DESCRIPTION:

   Creates a MAPL\_CFIO object from a Bundle. The MAPL\_CFIO objects
   is opaque and its properties can only be set by this method at
   creation. Currently, its properties cannot be queried. The object
   is used only as a handle in write operations. It is not needed for
   reading. 
  
   Its non-optional arguments associate a {\tt NAME}, an ESMF {\tt
   BUNDLE}, and a {\tt CLOCK} with the object. An ESMF TimeInterval
   {\tt OFFSET} is an optional argument that sets an offset between the
   time on the clock when eriting and the time stamp used for the data
   (defaults to no offset).

   The {\tt format} optional argument determines whether the write
   will use the linked self-describing format (SDF) library (HDF or
   netcdf) or write GrADS readable flat files. Currently only the SDF
   library option is supported.

   The remaining (optional) arguments are especialized and used
   primarily to support MAPL\_History, or to provide documentation in
   the form of character strings that will be placed in corresponding
   attributes in the SDF file.

  !REVISION HISTORY:
 
    19Apr2007 Todling  - Added ability to write out ak/bk
                       - Added experiment ID as optional argument

#endif
!
!EOP

! Locals
!-------

    integer                    :: STATUS


    type(ESMF_FIELD)    :: FIELD
    type(ESMF_GRID)     :: ESMFGRID
    type(ESMF_DELayout) :: LAYOUT
    type(ESMF_DistGrid) :: DISTGRID
    type(ESMF_TIME)     :: TIME
    type(ESMF_ALARM)    :: PERPETUAL
    type(ESMF_VM)       :: VM

    type(ESMF_CFIOVarInfo), pointer :: vars(:)
    type(ESMF_CFIOGrid),    pointer :: cfiogrid

    real, pointer  :: lats(:,:)
    real, pointer  :: lons(:,:)
    real, pointer  :: lats1d(:)
    real, pointer  :: lons1d(:)
    real, pointer  :: Local(:,:)
    real, pointer  :: lev (:  )
    real, pointer  :: ak  (:  )
    real, pointer  :: bk  (:  )
    real, pointer  :: ulevels(:  )
    real           :: range(2)

    real(KIND=REAL64), pointer  :: R8D2(:,:)

    integer        :: L, WriteInterval, counts(5), dims(3)
    integer        :: NumVars
    integer        :: IM,JM,LM
    integer        :: gridRank
    integer        :: fieldRank
    integer        :: Comm, nPEs
    integer        :: hours, mins, secs, timeInc
    integer        :: I, J, LT, K, IMO, JMO, LMG, IML, JML
    integer        :: IMBEG, IMEND, JMBEG, JMEND
    integer        :: Field_Type
    integer        :: Df
    integer        :: Num2DVars, Num3dVars
    integer        :: YY,MM,DD,H,M,S
    integer        :: noffset

    integer, allocatable :: Location(:)
    integer        :: CNT

    logical        :: Have2D, Have3D, HAVE_center, HAVE_edge, HAVE_ungrd
    logical        :: LPERP

    character(len=ESMF_MAXSTR)  :: Vunits
    character(len=ESMF_MAXSTR)  :: LongName
    character(len=ESMF_MAXSTR)  :: Units
    character(len=ESMF_MAXSTR)  :: StartTime
    character(len=esmf_maxstr)  :: Usource
    character(len=esmf_maxstr)  :: Uinstitution     
    character(len=esmf_maxstr)  :: Ucomment
    character(len=esmf_maxstr)  :: Ucontact     
    character(len=esmf_maxstr)  :: Utitle
    character(len=esmf_maxstr)  :: GridTypeAttribute

    character(len=ESMF_MAXSTR)  :: ClockName
    character(len=ESMF_MAXSTR)  :: Gridname
    character(len=ESMF_MAXSTR)  :: GridnameIn
    character(len=ESMF_MAXSTR)  :: GridnameOut
    character(len=2)            :: date
    character(len=2)            :: pole
    integer                     :: nn
    logical                     :: isGridRectilinear
    real, pointer               :: ptr3d(:,:,:)
    integer, allocatable        :: vsize(:)
    logical, allocatable        :: HasUngrid(:)
    character(len=ESMF_MAXSTR), pointer :: ungridded_units(:) => null()
    character(len=ESMF_MAXSTR), pointer :: ungridded_names(:) => null()
    character(len=ESMF_MAXSTR)  :: ungridded_unit, ungridded_name
    integer                     :: ungrdsize
    real, allocatable           :: ungridded_coord(:)
    real, allocatable           :: ungridded_coords(:,:)
    logical                     :: unGrdNameCheck, unGrdUnitCheck, unGrdCoordCheck
    logical                     :: found
    integer                     :: vectorListSize

    logical                     :: prevStored
    logical                     :: foundEmpty
    real(KIND=REAL64), pointer       :: LONS2D(:) => NULL()
    real(KIND=REAL64), pointer       :: LATS2D(:) => NULL()
    character(len=ESMF_MAXSTR), allocatable :: names(:)
    type(ESMF_ItemOrder_Flag) :: itemOrder_

    logical                     :: isPresent

! Begin
!------

    MCFIO%NAME   = NAME 
    MCFIO%CLOCK  = CLOCK
    MCFIO%BUNDLE = BUNDLE

! Number of variables in the bundle
!----------------------------------

    call ESMF_FieldBundleGet (BUNDLE, FieldCount=NumVars,    RC=STATUS)
    _VERIFY(STATUS)

    _ASSERT(NumVars>0, 'numVars must be > 0')

! Process optionals
!------------------

    if(present(NBITS)) then
       MCFIO%Nbits = NBITS
    else
       MCFIO%nBits = 1000
    end if

    if(present(deflate)) then
       df = deflate
    else
       df = 0
    endif

    if(present(Order)) then
       print*,'WARNING:   CFIO parameter "order" is no longer used.'
       print*,'           The new regrid facility uses ESMF parameters to'
       print*,'           specify the type of regridding to perform.'
       _ASSERT(.false., 'Order must be present')
       MCFIO%Order = Order
    else
       MCFIO%Order = -1
    endif

    if(present(source)) then
       Usource = source
    else
       Usource = "unknown"
    endif

    if(present(institution)) then
       Uinstitution = institution
    else
       Uinstitution = "unknown"
    endif

    if(present(comment)) then
       Ucomment = comment
    else
       Ucomment = "unknown"
    endif

    if(present(contact)) then
       Ucontact = contact
    else
       Ucontact = "unknown"
    endif

    if(present(format)) then
       MCFIO%format = format
    else
       MCFIO%format = "SDF"
    endif

    if(present(expid)) then
       MCFIO%expid = expid
    else
       MCFIO%expid = "No_ExpId"
    endif

    if(present(descr )) then
       Utitle  = descr 
    else
       Utitle  = "unknown"
    endif

    if(present(LEVELS)) then
       ulevels => LEVELS
    else
       nullify(ulevels)
    endif

    if(present(GC)) then
       MCFIO%GC   = GC
    else
!ALT       MCFIO%GC   = ESMF_GridCompCreate("NULL")
    endif

    if(present(VUNIT)) then
       vunits = trim(vunit)
    else
       vunits = ""
    endif

    if (present(Async)) then
       mcfio%async = async
    else
       mcfio%async = .false.
    end if

    if (present(vectorList)) then
       if (associated(vectorList)) then
          mcfio%vectorList => vectorList
       else
          mcfio%vectorList => null()
       end if
    else
       mcfio%vectorList => null()
    end if

! Get CommBndl, the communicator that is spanned by fields in the bundle
!-----------------------------------------------------------------------

    call ESMF_FieldBundleGet(BUNDLE, 1, FIELD,               RC=STATUS)
    _VERIFY(STATUS)
    call ESMF_FieldGet       (FIELD, grid=ESMFGRID,          RC=STATUS)
    _VERIFY(STATUS)
    call ESMF_GridGet        (ESMFGRID, dimCount=gridRank,   rc=STATUS)
    _VERIFY(STATUS)
    call ESMF_GridGet        (ESMFGRID, NAME=Gridname, rc=status )
    _VERIFY(STATUS)
    call ESMF_GridGet        (ESMFGRID, distgrid=DISTGRID,   RC=STATUS)
    _VERIFY(STATUS)
    call ESMF_DistGridGet    (DISTGRID, delayout=LAYOUT,     RC=STATUS)
    _VERIFY(STATUS)
    call ESMF_DELayoutGet    (LAYOUT,   vm=VM,               RC=STATUS)
    _VERIFY(STATUS)
    call ESMF_VMGet          (VM, mpiCommunicator=Comm,      RC=STATUS)
    _VERIFY(STATUS)
    call ESMF_VMGet          (VM, localpet=mCFIO%MYPE,       RC=STATUS)
    _VERIFY(STATUS)
    call ESMF_VMGet          (VM, petcount=NPES,             RC=STATUS)
    _VERIFY(STATUS)

! Save the ESMFGrid in the object
!--------------------------------

    mCFIO%comm = comm
    mCFIO%Grid = ESMFGRID

! Set the partition size to size of VM. Can be overridden later to call to MAPL_CFIOSet
! -------------------------------------------------------------------------------------

    if(present(NumCores)) then
       mcfio%Numcores = NumCores
    else
       mcfio%Numcores = MAPL_CoresPerNodeGet(comm,rc=status)
       _VERIFY(STATUS)
    end if

    mCFIO%partsize = size(MAPL_NodeRankList)

    MCFIO%regridConservative = .false.
    if (present(Conservative)) then
       if (Conservative /= 0) then
          MCFIO%regridConservative = .true.
       end if
    end if

! Vertical interpolation info
!----------------------------

    if(present(Vcoord)) then
       MCFIO%VVAR = adjustl(vcoord)
       MCFIO%Func = MCFIO%Vvar(1:3)
       if    (MCFIO%Func=='log') then
          MCFIO%Vvar = adjustl(MCFIO%Vvar(index(MCFIO%Vvar,'(')+1:index(MCFIO%Vvar,')')-1))
       elseif(MCFIO%Func=='pow') then
          read( MCFIO%Vvar(index(MCFIO%Vvar,',')+1:index(MCFIO%Vvar,')')-1) , *) mCFIO%pow 
          MCFIO%Vvar = adjustl(MCFIO%Vvar(index(MCFIO%Vvar,'(')+1:index(MCFIO%Vvar,',')-1))
       endif
    else
       MCFIO%VVAR = ""
       MCFIO%Func = ""
    end if

    if(present(vscale)) then
       MCFIO%Vscale = Vscale
    else
       MCFIO%Vscale = 1.0
    endif

! Determine the rank and vertical Location (Mid or Edge) of Fields within Bundle.
!   Note: If User-Defined ULEVELS is not present, ALL levels are written.
!   In this case, vertical Location must be consistent for ALL variables.
! ---------------------------------------------------------------------------------------

    allocate(MCFIO%VarDims(NumVars), stat=STATUS)
    _VERIFY(STATUS)
    allocate(MCFIO%VarName(NumVars), stat=STATUS)
    _VERIFY(STATUS)
    allocate(location     (NumVars), stat=STATUS)
    _VERIFY(STATUS)
    allocate(vsize        (NumVars), stat=STATUS)
    _VERIFY(STATUS)
    allocate(hasUngrid    (NumVars), stat=STATUS)
    _VERIFY(STATUS)
    allocate(MCFIO%VarType(NumVars), stat=STATUS)
    _VERIFY(STATUS)
    allocate(MCFIO%needVar(NumVars), stat=STATUS)
    _VERIFY(STATUS)
    allocate(ungridded_names(NumVars), stat=STATUS)
    _VERIFY(STATUS)
    ungridded_names=""
    allocate(ungridded_units(NumVars), stat=STATUS)
    _VERIFY(STATUS)
    ungridded_units=""

    allocate(names(NumVars),stat=status)
    _VERIFY(status)
    if (present(itemOrder)) then
       itemOrder_ = itemOrder
    else
       itemOrder_ = ESMF_ITEMORDER_ABC
    end if
    call ESMF_FieldBundleGet(bundle,itemorderflag=itemOrder_,fieldNameList=Names,rc=status)
    _VERIFY(status)

    VARIABLES_1: DO I = 1, NumVars

       call ESMF_FieldBundleGet(BUNDLE, names(i), field=FIELD,              RC=STATUS)
       _VERIFY(STATUS)
       call ESMF_FieldGet      (FIELD, NAME= mCFIO%VarName(I), RC=STATUS)
       _VERIFY(STATUS)

       vsize(i) = 1
       hasUngrid(I) = .false.
       if(mCFIO%VarName(I)==MCFIO%Vvar) then
          MCFIO%VarDims(I) = -1
          LOCATION(i)      = MAPL_VLocationNone
       else
          call ESMF_FieldGet(FIELD, dimCount=fieldRank, RC=STATUS)
          _VERIFY(STATUS)
          _ASSERT(fieldRank <= 3, 'fieldRank > 3')

          MCFIO%VarDims(I) = fieldRank

          call ESMF_AttributeGet(FIELD, NAME="VLOCATION", isPresent=isPresent, RC=STATUS)
          _VERIFY(STATUS)
          if ( isPresent ) then
             call ESMF_AttributeGet(FIELD, NAME="VLOCATION", VALUE=LOCATION(I), RC=STATUS)
             _VERIFY(STATUS)
          else
             LOCATION(I) = MAPL_VLocationNone
          end if

          if (fieldRank == 3) then
             call ESMF_FieldGet(field, farrayPtr=ptr3d, rc=status)
             _VERIFY(STATUS)
             vsize(i) = size(ptr3d,3)
          end if

          if (fieldRank >= 3 .and. location(I) == MAPL_VLocationNone) then
             hasUngrid(I) = .true.
             call ESMF_AttributeGet(field,NAME="UNGRIDDED_UNIT",value=ungridded_unit,rc=status)
             _VERIFY(STATUS)
             call ESMF_AttributeGet(field,NAME="UNGRIDDED_NAME",value=ungridded_name,rc=status)
             _VERIFY(STATUS)
             ungridded_names(i) = ungridded_name
             ungridded_units(i) = ungridded_unit
             call ESMF_AttributeGet(field,NAME="UNGRIDDED_COORDS",isPresent=isPresent,rc=status)
             _VERIFY(STATUS)
             if (isPresent) then
                call ESMF_AttributeGet(field,NAME="UNGRIDDED_COORDS",itemcount=ungrdsize,rc=status)
                _VERIFY(STATUS)
             else
                ungrdsize=0
             end if
             if (ungrdsize/=0) then
                _ASSERT(vsize(i)==ungrdsize,'inconsistent ungrdsize')
                if (.not.allocated(ungridded_coord)) allocate(ungridded_coord(ungrdsize),stat=status)
                if (.not.allocated(ungridded_coords)) allocate(ungridded_coords(NumVars,ungrdsize),stat=status)
                _VERIFY(STATUS)
                call ESMF_AttributeGet(field,NAME="UNGRIDDED_COORDS",valuelist=ungridded_coord,rc=status)
                _VERIFY(STATUS)
                ungridded_coords(i,:) = ungridded_coord
             end if
          end if

       endif

    end do VARIABLES_1
    ! now put a check in that we aren't trying to do something like have two different
    ! ungridded units if we have any ungridded dimensions, compare ungridded info for
    ! each variable to the last values retrieved, if any differ then obviously user
    ! is trying to put 2 different ungridded variables in a collection that have
    ! different attributes
    if (any(hasUngrid)) then
       do i=1,NumVars
          if (hasUngrid(i)) then
             unGrdUnitCheck = ungridded_units(i) /= ungridded_unit
             unGrdNameCheck = ungridded_names(i) /= ungridded_name
             if ( allocated(ungridded_coords) .and. allocated(ungridded_coords) ) then
                unGrdCoordCheck = any(ungridded_coords(i,:) /= ungridded_coord)
             else
                unGrdCoordCheck = .false.
             end if
             if ( unGrdUnitCheck .or. unGrdNameCheck .or. unGrdCoordCheck) then
                _ASSERT(.false., 'Ungridded attributes for variables in collection do not match') 
             end if    
          end if
       end do
    end if


!ALT: next segment is here only for initial testing
! we need a better logic how to prepare needVar list
! possibilities are: add staggering, vector pair to spec
! special key words in collection, etc.
    mCFIO%needVar = 0
    vectorListSize = 0
    if (associated(mCFIO%vectorList)) then
       vectorListSize = size(mCFIO%vectorList,2)
    end if
    VARLOOP: DO I = 1, NumVars
       DO k = 1, vectorListSize
          if (mCFIO%varName(I) == mCFIO%vectorList(1,k)) then
             ! this is first component of a vector (i.e. U)
             ! find the index of the V component
             found = .false.
             DO J = 1, NumVars
                if (trim(mCFIO%varName(J)) == mCFIO%vectorList(2,k)) then
                   found = .true.
                   exit
                end if
             end DO
             _ASSERT(found, 'search failed')
             mCFIO%needVar(I) = J ! I am first component of the vector
          else if(mCFIO%varName(I) == mCFIO%vectorList(2,k)) then
             ! find the index of the U component
             ! store it as negative number by convension suggested by Max
             found = .false.
             DO J = 1, NumVars
                if (trim(mCFIO%varName(J)) == mCFIO%vectorList(1,k)) then
                   found = .true.
                   exit
                end if
             end DO
             _ASSERT(found, 'search failed')
             mCFIO%needVar(I) = -J ! I am second component of the vector
          end if
       end DO
    end DO VARLOOP

! Sizes of global grid in the bundle. Sizes in the SDF may be different.
!----------------------------------------------------------------------

    call MAPL_GridGet( MCFIO%GRID, globalCellCountPerDim=COUNTS,  &
                       localCellCountPerDim=DIMS,     RC=STATUS)
    _VERIFY(STATUS)

    IML = DIMS(1)
    JML = DIMS(2)

    IM = COUNTS(1)
    JM = COUNTS(2)
    LMG = COUNTS(3)

    HAVE_center = any(LOCATION==MAPL_VLocationCenter)
    HAVE_edge   = any(LOCATION==MAPL_VLocationEdge  )
    HAVE_ungrd  = any(hasUngrid)

    if     ( associated(ULEVELS)  ) then
       LM = size(ULEVELS)
       HAVE_edge = .false.
       if (HAVE_ungrd) then
          _ASSERT(.false., 'ERROR: Specifying LEVELS is not allowed for UNGRIDDED vars')
       end if
    else 

!      Check on proper levels
!      -----------------------
       if(HAVE_center .and.  HAVE_edge) then
          DO I = 1, NumVars
             IF (LOCATION(I)==MAPL_VLocationEdge) print*, mCFIO%VarName(I)
          ENDDO
          _ASSERT(.false., 'ERROR: Mixed Vlocation in CFIO not allowed unless LEVELS is specified')
       endif

       if( all(MCFIO%VarDims==2)) then
          LM = 1
       else if (HAVE_ungrd) then
          if (HAVE_center .or. HAVE_edge) then
             _ASSERT(.false., 'ERROR: Mixed 3d and UNGRIDDED in CFIO not allowed')
          end if
          if (minval(vsize) /= maxval(vsize)) then
             _ASSERT(.false., 'ERROR: Outputting variables with different ungridded sizes in one collection')
          end if 
          LM = maxval(vsize)
       else
          LM = COUNTS(3)
          if (HAVE_edge) LM = LM+1
       endif
    end if

    mCFIO%LM   = LM

! Allocate request arrays for non-blocking gathers of bundle variables
!  Each 2D variable and each level of each 3D variable needs a request
!---------------------------------------------------------------------

    Num2DVars = count(MCFIO%VarDims==2)
    Num3DVars = count(MCFIO%VarDims==3)

    Have2D    = Num2DVars > 0
    Have3D    = Num3DVars > 0

    _ASSERT(HAVE2D .or. HAVE3D, 'must be 2D or 3D')

    LT        = Num2DVars + Num3DVars*LM

    allocate( MCFIO%reqs (LT),stat=STATUS)
    _VERIFY(STATUS)
    allocate( MCFIO%Krank(LT),stat=STATUS)
    _VERIFY(STATUS)
    mcfio%krank=0
    allocate(MCFIO%pairList(LT), stat=STATUS)
    _VERIFY(STATUS)
    if (allocated(MAPL_NodeRankList)) then
       call MAPL_RoundRobinPEList(mcfio%krank,size(MAPL_NodeRankList),rc=status)
       _VERIFY(status)
    end if

    MCFIO%pairList = 0

! Horizontal dimensions of output fields
!---------------------------------------

    if (present(OUTPUT_GRID)) then
       if (associated(OUTPUT_GRID)) then
          mcfio%output_grid = OUTPUT_GRID
       else
          mcfio%output_grid = mCFIO%grid
       end if
       
       call MAPL_GridGet(mcfio%output_grid, globalCellCountPerDim=dims, rc=status)
       _VERIFY(status)
       IMO = dims(1)
       JMO = dims(2)

    else
       mcfio%output_grid = mCFIO%grid
       IMO = IM
       JMO = JM
    end if

    MCFIO%IM = IMO
    MCFIO%JM = JMO

!ALT: this is first attempt to guess if the grid is rectilinear
!     i.e. if we need could use 1d LAT/LONs
!     we take clues from the gridname
    Gridname = adjustl(Gridname)
    nn   = len_trim(Gridname)
    pole = Gridname(1:2)
    date = Gridname(nn-1:nn)

!ALT: if change of horizontal resolution is requested, we
!     assume uniform output grid
    if (IM /= IMO .or. JM /= JMO) isGridRectilinear=.true.

    isGridRectilinear = .true. ! default: 1d LAT/LONs
        ! special cases requiring 2d LAT/LONs
    if (JMO == 6*IMO) isGridRectilinear = .false.
    if (pole == 'CM') isGridRectilinear = .false.
    if (date == 'TM') isGridRectilinear = .false.

!ALT: if change of horizontal resolution is requested, we
!     assume uniform output grid
    if (IM /= IMO .or. JM /= JMO) then
       isGridRectilinear=.true.
       mCFIO%newFormat = .false.
    end if

    if (isGridRectilinear) then
       allocate(LONS1D(IMO), STAT=status)
       _VERIFY(status)
       allocate(LATS1D(JMO), STAT=status)
       _VERIFY(status)
    else
       allocate(LONS1D(0), LATS1D(0), STAT=status)
       _VERIFY(status)
       ! the above 2 lines need to be replaced with:
       ! table search + set logical prevStored
       prevStored = .false.
       do i = 1, maxStoredCoords
          if (.not.storedCoords(i)%created) cycle
          if ( storedCoords(i)%IM == IMO .and. &
               storedCoords(i)%JM == JMO ) then
             prevStored = .true.
             LONS2D => storedCoords(i)%LONS2D
             LATS2D => storedCoords(i)%LATS2D
!             call Write_Parallel("DEBUG:Using stored LAT/LONs")
             exit
          end if
       end do
       if (.not.prevStored) then
          call MAPL_AllocateShared(LONS2D, (/IMO*JMO/), &
               TransRoot=.true., RC=STATUS)
          _VERIFY(STATUS)
          call MAPL_AllocateShared(LATS2D, (/IMO*JMO/), &
               TransRoot=.true., RC=STATUS)
          _VERIFY(STATUS)

          foundEmpty = .false.
!          call Write_Parallel("DEBUG:Storing LAT/LONs")
          do i = 1, maxStoredCoords
             if (storedCoords(i)%created) cycle
             storedCoords(i)%created = .true.
             storedCoords(i)%LONS2D => LONS2D
             storedCoords(i)%LATS2D => LATS2D
             storedCoords(i)%IM = IMO
             storedCoords(i)%JM = JMO
             foundEmpty = .true.
             exit
          end do
          if (.not.foundEmpty) then
             _ASSERT(.false., 'ERROR: Need bigger table with storedCoords')
          end if
       end if
    endif

! Process horizontal resolution change
!-------------------------------------

    IMBEG = 1
    IMEND = IMO
    JMBEG = 1
    JMEND = JMO

    TRANSFORM: if (IM /= IMO .or. JM /= JMO) then

       if (present(output_grid)) then

            call get_latlon_from_factory(output_grid, lons1d, lats1d, rc=status)
            _VERIFY(status)

       else

          if(present(xyoffset)) then
             select case(xyoffset)
             case(0) ! PC, DC
                lons1d = MAPL_Range(-180.,180.-(360./IMO), IMO)
                lats1d = MAPL_Range(-90., +90., JMO)
             case(1) ! PC, DE
                lons1d = MAPL_Range(-180.+(180./IMO), 180.-(180./IMO), IMO)
                lats1d = MAPL_Range(-90., +90., JMO)
             case(2) ! PE, DC
                lons1d = MAPL_Range(-180.,180.-(360./IMO), IMO)
                lats1d = MAPL_Range(-90.+(90./JMO), +90.-(90./JMO), JMO)
             case(3) ! PE, DE
                lons1d = MAPL_Range(-180.+(180./IMO), 180.-(180./IMO), IMO)
                lats1d = MAPL_Range(-90.+(90./JMO), +90.-(90./JMO), JMO)
             case default
                _ASSERT(.false.,'needs informative message')
             end select
             mcfio%xyoffset = xyoffset
          else
             mcfio%xyoffset = 0
             lons1d = MAPL_Range(-180.,180.-(360./IMO), IMO, conversion_factor=MAPL_DEGREES_TO_RADIANS)
             lats1d = MAPL_Range(-90., +90., JMO, conversion_factor=MAPL_DEGREES_TO_RADIANS)
          endif

       endif

       call ESMF_AttributeGet(ESMFGRID, name="GridType", isPresent=isPresent, rc=STATUS)
       _VERIFY(STATUS)
       if (isPresent) then
          call ESMF_AttributeGet(ESMFGRID, name="GridType", value=GridTypeAttribute, rc=STATUS)
          _VERIFY(STATUS)
       else
          GridTypeAttribute = 'UNKNOWN'
       endif

! If order of transform was not specified, do binning for coarser,
!   and bilinear for finer.
!------------------------------------------------------------------

       if     (mCFIO%order<0) then
          if (IMO <  IM .or. JMO < JM) then
             mCFIO%order=0
          else
             mCFIO%order=1
          end if
       end if

    else ! TRANSFORM
       

! Arrays of lats and lons from esmfgrid
!--------------------------------------

       !ALT we need to execute all of this only if
       ! the grid is rectilinear or not in the table
       if (isGridRectilinear .or. .not. prevStored) then
    !if (JMO /= 6*IMO) then

       allocate(LONS (IM ,JM ),STAT=STATUS)
       _VERIFY(STATUS)
       allocate(LATS (IM ,JM ),STAT=STATUS)
       _VERIFY(STATUS)
       allocate(LOCAL(IML,JML),STAT=STATUS)
       _VERIFY(STATUS) 

       call ESMF_GridGetCoord(esmfgrid, localDE=0, coordDim=1, &
            staggerloc=ESMF_STAGGERLOC_CENTER, &
            farrayPtr=R8D2, rc=status)
       _VERIFY(STATUS)

       LOCAL = R8D2*(180._REAL64/MAPL_PI_R8)
       call ArrayGather(LOCAL, LONS, ESMFGRID, RC=STATUS)
       _VERIFY(STATUS) 

       call ESMF_GridGetCoord(esmfgrid, localDE=0, coordDim=2, &
            staggerloc=ESMF_STAGGERLOC_CENTER, &
            farrayPtr=R8D2, rc=status)
       _VERIFY(STATUS) 

       LOCAL = R8D2*(180._REAL64/MAPL_PI_R8)
       call ArrayGather(LOCAL, LATS, ESMFGRID, RC=STATUS)
       _VERIFY(STATUS)

       call MAPL_CommsBcast (layout, lons, size(lons), 0, rc=status)
       _VERIFY(STATUS)
       call MAPL_CommsBcast (layout, lats, size(lats), 0, rc=status)
       _VERIFY(STATUS)

       if (isGridRectilinear) then
          LONS1D = LONS(:,1)
          LATS1D = LATS(1,:)
       else
          if (MAPL_AmNodeRoot .or. (.not. MAPL_ShmInitialized)) then
             k = 0
             do j = 1, jm
                do i = 1, im
                   k = k+1
                   LONS2D(k) = LONS(i,j)
                   LATS2D(k) = LATS(i,j)
                end do
             end do
          end if
          call MAPL_SyncSharedMemory(rc=STATUS)
          _VERIFY(STATUS)
       endif

       DEALLOCATE(LOCAL)
       DEALLOCATE(LONS)
       DEALLOCATE(LATS)

     end if
     !end if ! Cubed-Sphere ouput

    endif TRANSFORM

    if (present(output_grid)) then

       if (MCFIO%regridConservative) then
          call MAPL_GenGridName(imo, jmo, xyoffset=mcfio%xyoffset, gridname=gridnameOut, geos_style=.false.)
          gridnameIn = gridname
          call MAPL_GeosNameNew(gridnameIn)

          mCFIO%regridder => regridder_manager%make_regridder(mCFIO%grid, mCFIO%output_grid, &
               & REGRID_METHOD_CONSERVE, rc=status)
          _VERIFY(status)

       else

          mCFIO%regridder => regridder_manager%make_regridder(mCFIO%grid, mCFIO%output_grid, &
               & REGRID_METHOD_BILINEAR, rc=status)
          _VERIFY(status)

       end if

    end if
! Create the CFIO grid and populate it
!-------------------------------------

    allocate(CFIOGRID)

    CFIOGRID = ESMF_CFIOGridCreate(gName=trim(NAME)//"Grid", RC=STATUS)
    _VERIFY(STATUS)

! Horizontal grid info
!---------------------
    if (isGridRectilinear) then
       call ESMF_CFIOGridSet(CFIOGRID, LON=LONS1D(IMBEG:IMEND), LAT=LATS1D(JMBEG:JMEND),  TM=TM,  RC=STATUS)
       _VERIFY(STATUS)
    else
       !ALT: new approach
       ! create 2DR8 shared memory coordinate arrays
       ! actually, check if the object is in a table, if YES, use it
       ! else  create one and put it in the table
       ! fill them on node root
       ! pass them down
       call ESMF_CFIOGridSet(CFIOGRID, twoDimLat=.true., IM=IMO, JM=JMO, TM=TM,  &
            LON2=LONS2D, LAT2=LATS2D,  RC=STATUS)
       _VERIFY(STATUS)
    end if

    deallocate(LONS1D)
    deallocate(LATS1D)

! Vertical grid info
!--------------------

    mCFIO%Vinterp = .false.
    mCFIO%Vinterp = MCFIO%Vvar/=""
    VERTGRID: if(HAVE3D) THEN
       allocate(LEV(LM), stat=status)
       _VERIFY(STATUS)
       
       if (associated(ULEVELS)) then
          if (mCFIO%Vinterp .or. (size(ulevels) < LMG)) then
             LEV = ULEVELS
          else
             LEV = (/(L, L=1,size(ulevels))/)
          end if
       else if (HAVE_edge) then
          LEV = (/(L, L=0,LM-1)/)
       else if (HAVE_ungrd) then
          if (allocated(ungridded_coord)) then
             lev = ungridded_coord
          else
             lev = (/(L, L=1,LM)/)
          end if
       else
          LEV = (/(L, L=1,LM)/)
       end if

       allocate(mCFIO%levs(size(lev)),mcfio%unmodifiedLevs(size(lev)), stat=status)
       _VERIFY(STATUS)
       mCFIO%levs = lev
       mCFIO%unmodifiedLevs = lev
       if (HAVE_ungrd) then
          call ESMF_CFIOGridSet(cfiogrid, levUnit=ungridded_unit, RC=STATUS)
          _VERIFY(STATUS)
          call ESMF_CFIOGridSet(cfiogrid, standardName =ungridded_name, RC=STATUS)
          _VERIFY(STATUS)
          call ESMF_CFIOGridSet(cfiogrid, coordinate ='N/A', RC=STATUS)
          _VERIFY(STATUS)
          mCFIO%levs = -mCFIO%levs
       else if(mCFIO%Vinterp) then
          if    (mCFIO%Func=='log') then
             mCFIO%levs = log(lev* MCFIO%vscale)
          elseif(mCFIO%Func=='pow') then
             mCFIO%levs = (lev* MCFIO%vscale)**mCFIO%pow
          else
             mCFIO%levs = lev* MCFIO%vscale
          end if
          mCFIO%unmodifiedLevs=mCFIO%unmodifiedLevs*MCFIO%vscale

          if( trim(vunits).eq."" ) then
             call ESMF_AttributeGet(FIELD, NAME="UNITS", VALUE=units,         RC=STATUS)
             _VERIFY(STATUS)
             call ESMF_CFIOGridSet(cfiogrid, levUnit=trim(units),             RC=STATUS)
             _VERIFY(STATUS)
          else
             call ESMF_CFIOGridSet(cfiogrid, levUnit=trim(vunits),            RC=STATUS)
             _VERIFY(STATUS)
          endif
          call ESMF_CFIOGridSet(cfiogrid, standardName =trim(MCFIO%Vvar)//'_level', RC=STATUS)
          _VERIFY(STATUS)
          call ESMF_CFIOGridSet(cfiogrid, coordinate   =trim(MCFIO%Vvar),           RC=STATUS)
          _VERIFY(STATUS)
       else
          if (present(vunit) .and. trim(vunits) .ne. "") then
             call ESMF_CFIOGridSet(cfiogrid, levUnit      =trim(vunits),         RC=STATUS)
             _VERIFY(STATUS)
          else 
             call ESMF_CFIOGridSet(cfiogrid, levUnit      ='layer',              RC=STATUS)
             _VERIFY(STATUS)
          end if
          call ESMF_CFIOGridSet(cfiogrid, standardName ='model_layers',       RC=STATUS)
          _VERIFY(STATUS)
          call ESMF_CFIOGridSet(cfiogrid, coordinate   ='eta',                RC=STATUS)
          _VERIFY(STATUS)
       end if

       if (associated(ulevels)) then
          call ESMF_CFIOGridSet(cfiogrid, lev=ulevels,  RC=STATUS)
          _VERIFY(STATUS)
       else
          call ESMF_CFIOGridSet(cfiogrid, lev=abs(lev),  RC=STATUS)
          _VERIFY(STATUS)
       end if

       deallocate(LEV)

    else

       call ESMF_CFIOGridSet(cfiogrid, levUnit      ='none',         RC=STATUS)
       _VERIFY(STATUS)
       call ESMF_CFIOGridSet(cfiogrid, standardName ='2d_fields',    RC=STATUS)
       _VERIFY(STATUS)
       call ESMF_CFIOGridSet(cfiogrid, coordinate   ='N/A',          RC=STATUS)
       _VERIFY(STATUS)

    end if VERTGRID

! Create variable objects
!------------------------

    allocate(vars(Num2DVars+Num3dVars), stat=status)
    _VERIFY(STATUS)

! Disable range checking
!-----------------------

    RANGE(2) =  MAPL_UNDEF
    RANGE(1) = -MAPL_UNDEF

    K = 0

    mCFIO%vartype = MAPL_ScalarField
    VARIABLES_2: do L=1,NumVars

       if(mCFIO%VarDims(L)<1) cycle

       K = K + 1

       call ESMF_FieldBundleGet(BUNDLE, mCFIO%varName(L), field=FIELD,                       RC=STATUS)
       _VERIFY(STATUS)

       call ESMF_AttributeGet  (FIELD, NAME="LONG_NAME",isPresent=isPresent, RC=STATUS)
       _VERIFY(STATUS)
       if ( isPresent ) then
          call ESMF_AttributeGet  (FIELD, NAME="LONG_NAME",VALUE=LongName, RC=STATUS)
          _VERIFY(STATUS)
       else
          LongName = mCFIO%VarName(L)
       endif

       call ESMF_AttributeGet  (FIELD, NAME="UNITS"    ,isPresent=isPresent, RC=STATUS)
       _VERIFY(STATUS)
       if ( isPresent ) then
          call ESMF_AttributeGet  (FIELD, NAME="UNITS"    ,VALUE=Units, RC=STATUS)
          _VERIFY(STATUS)
       else
          Units = 'unknown'
       end if

       call ESMF_AttributeGet  (FIELD, NAME="FIELD_TYPE",isPresent=isPresent, RC=STATUS)
       _VERIFY(STATUS)
       if ( isPresent ) then
          call ESMF_AttributeGet  (FIELD, NAME="FIELD_TYPE",VALUE=Field_Type, RC=STATUS)
          _VERIFY(STATUS)
       else
          Field_Type = MAPL_ScalarField
       end if

       mCFIO%vartype(L) = Field_Type

       VARS(K) = ESMF_CFIOVarInfoCreate(vName=trim(mCFIO%VarName(L)),   RC=STATUS)
       _VERIFY(STATUS)

       call ESMF_CFIOVarInfoSet(VARS(K),           &
            vName        = mCFIO%VarName(L),       &
            vTitle       = LongName,               &
            grid         = cfioGRID,               &
            amiss        = MAPL_Undef,             &
            scaleFactor  = 1.,                     &
            addOffSet    = 0.,                     &
            standardName = LongName,               &
            twoDimVar    = MCFIO%VarDims(L)==2,    &
            validRange   = RANGE,                  &
            vUnits       = UNITS,                  &
                                         RC=STATUS )
       _VERIFY(STATUS)

       if (present(CHUNKSIZE)) then
          if (associated(CHUNKSIZE)) then
             call ESMF_CFIOVarInfoSet(VARS(K), &
                  ChunkSize = ChunkSize,       &
                         rc = status           )
          end if
       end if

    end do VARIABLES_2

! Get time info from the clock. Note the optional offset
!-------------------------------------------------------

    call ESMF_ClockGet(CLOCK, name=clockname, CurrTime =TIME, RC=STATUS)
    _VERIFY(STATUS)

    if(present(OFFSET)) then

        call ESMF_TimeIntervalGet( OFFSET, S=noffset, rc=status )
        _VERIFY(STATUS)
        if( noffset /= 0 ) then
            LPERP = ( index( trim(clockname),'_PERPETUAL' ).ne.0 )
        if( LPERP ) then
            call ESMF_ClockGetAlarm ( clock, alarmname='PERPETUAL', alarm=PERPETUAL, rc=status )
            _VERIFY(STATUS)
            if( ESMF_AlarmIsRinging(PERPETUAL) ) then
                call ESMF_TimeGet ( Time, YY = YY, &
                                          MM = MM, &
                                          DD = DD, &
                                          H  = H , &
                                          M  = M , &
                                          S  = S, rc=status )
                                          MM = MM + 1
                call ESMF_TimeSet ( Time, YY = YY, &
                                          MM = MM, &
                                          DD = DD, &
                                          H  = H , &
                                          M  = M , &
                                          S  = S, rc=status )
                if( MAPL_AM_I_ROOT() ) write(6,"(a,2x,i4.4,'/',i2.2,'/',i2.2,2x,'Time: ',i2.2,':',i2.2,':',i2.2)") &
                     & "Inside Create: ",YY,MM,DD,H,M,S
            endif
        endif
        endif

       TIME = TIME  - OFFSET
       MCFIO%OFFSET = OFFSET
    else
      call ESMF_TimeIntervalSet( MCFIO%OFFSET, S=0, rc=status )
      _VERIFY(STATUS)
    endif

    call ESMF_TimeGet (TIME,  timeString=StartTime, RC=STATUS)
    _VERIFY(STATUS)

! Create CFIO object
!-------------------

    MCFIO%cfio =  ESMF_CFIOCreate(cfioObjName=trim(Name))

! Set Internal MetaCode Writing interval. Default of 6 hours. If set to 0
!  it is reset to 6 hours.Currently CFIO and GFIO expect timeIncrement to be 
!  in HHMMSS format, this imposes severe limitations to the frequency of the output:
!  no writes should be done less frequently than once every 4 days (99 hours)
! ------------------------------------------------------------------------------

    if (present(FREQUENCY)) then
       !_ASSERT(FREQUENCY <= 4*86400,'bad frequency')
       if (frequency == 0 ) then
          writeInterval = 21600
       else
          writeInterval = FREQUENCY
       endif
    else
       writeInterval = 21600
    end if

    hours         = writeInterval/3600
    writeInterval = writeInterval-3600*hours
             mins = writeInterval/60
             secs = writeInterval-60*mins

    timeinc = 10000*hours + 100*mins + secs

! Set global attributes
!----------------------

    call ESMF_CFIOSet(MCFIO%CFIO,                                 &
         varObjs     = VARS,                                      &
         grid        = cfioGRID,                                  &
         format      = MCFIO%Format,                              &
         expid       = MCFIO%ExpId,                               &
         TimeString  = trim(StartTime),                           &
         timeInc     = timeInc,                                   &
         title       = trim(Utitle),                              &
         source      = Usource,                                   &
         history     = 'File written by MAPL_CFIO',               &
         institution = Uinstitution,                              &
         convention  = "COARDS",                                  &
         contact     = Ucontact,                                  &
         references  = "http://gmao.gsfc.nasa.gov",               &
         comment     = Ucomment,                                  & 
         prec        = 0,                                         &
         deflate     = df,                                        &
         RC=STATUS )
    _VERIFY(STATUS)

! Create AK/BKs
! -------------

    if(HAVE3D) then
       call ESMF_AttributeGet(ESMFGRID, NAME='ak', isPresent=isPresent, RC=STATUS)
       _VERIFY(STATUS)
       if (isPresent) then
          call ESMF_AttributeGet(ESMFGRID, NAME='ak', itemcount=CNT, RC=STATUS)
          _VERIFY(STATUS)
       else
          CNT=0
       endif
       if (CNT>0) then
          allocate ( ak(CNT), bk(CNT), stat=status )
          _VERIFY(STATUS)

          call ESMF_AttributeGet(ESMFGRID, name='ak', valueList=ak, rc=STATUS)
          _VERIFY(STATUS)
          call ESMF_CFIOSet(MCFIO%cfio, attRealName='ak', attReal=ak )

          call ESMF_AttributeGet(ESMFGRID, name='bk', valuelist=bk, rc=STATUS)
          _VERIFY(STATUS)
          call ESMF_CFIOSet(MCFIO%cfio, attRealName='bk', attReal=bk )

          deallocate ( ak, bk )
       end if
    endif

! All Done
!---------

    MCFIO%CREATED = .true.

    deallocate(hasUngrid)
    deallocate(vsize)
    deallocate(location)
!@    call ESMF_CFIOVarInfoDestroy(vars, __RC__)
    deallocate(vars)
!ALT we should do this:    call ESMF_CFIOGridDestroy(cfiogrid)
    deallocate(cfiogrid)
    deallocate(ungridded_names)
    deallocate(ungridded_units)
    if (allocated(ungridded_coord)) then
       deallocate(ungridded_coord)
    end if
    if (allocated(ungridded_coords)) then
       deallocate(ungridded_coords)
    end if

    _RETURN(ESMF_SUCCESS)
  end subroutine MAPL_CFIOCreateFromBundle


  subroutine MAPL_CFIOCreatewrite ( MCFIO, nsteps, RC )

    type(MAPL_CFIO),           intent(INOUT) :: MCFIO
    integer,         optional, intent(   IN) :: nsteps
    integer,         optional, intent(  OUT) :: RC

    type(ESMF_Time)                :: CurrTime
    type(ESMF_Alarm)               :: PERPETUAL
    character(len=ESMF_MAXSTR)     :: StartTime
    character(len=ESMF_MAXSTR)     :: ClockName

    logical                        :: LPERP
    integer                        :: YY,MM,DD,H,M,S
    integer                        :: noffset
    integer                        :: STATUS


    if(present(nsteps)) then
       call ESMF_CFIOSet(MCFIO%CFIO, nsteps=nsteps, RC=STATUS)
       _VERIFY(STATUS)
    else
       call ESMF_CFIOSet(MCFIO%CFIO, nsteps=1, RC=STATUS)
       _VERIFY(STATUS)
    endif 

! Get time info from the clock. Note the optional offset
!-------------------------------------------------------
    
    call ESMF_ClockGet(mCFIO%CLOCK, name=clockname, CurrTime=CurrTime, RC=STATUS)
    _VERIFY(STATUS)

        call ESMF_TimeIntervalGet( mCFIO%OFFSET, S=noffset, rc=status )
        _VERIFY(STATUS)
        if( noffset /= 0 ) then
            LPERP = ( index( trim(clockname),'_PERPETUAL' ).ne.0 )
        if( LPERP ) then
            call ESMF_ClockGetAlarm ( mCFIO%CLOCK, alarmName='PERPETUAL', alarm=PERPETUAL, rc=status )
            _VERIFY(STATUS)
            if( ESMF_AlarmIsRinging(PERPETUAL) ) then
                call ESMF_TimeGet ( CurrTime, YY = YY, &
                                              MM = MM, &
                                              DD = DD, &
                                              H  = H , &
                                              M  = M , &
                                              S  = S, rc=status )
                                              MM = MM + 1
                call ESMF_TimeSet ( CurrTime, YY = YY, &
                                              MM = MM, &
                                              DD = DD, &
                                              H  = H , &
                                              M  = M , &
                                              S  = S, rc=status )
            endif
        endif
        endif

    CurrTime = CurrTime - mCFIO%OFFSET

    call ESMF_TimeGet (CurrTime,  timeString=StartTime, RC=STATUS)
    _VERIFY(STATUS)

    call ESMF_CFIOSet(MCFIO%CFIO, TimeString=trim(StartTime), RC=STATUS)
    _VERIFY(STATUS)

    call ESMF_CFIOSet(MCFIO%CFIO, fName=trim(mCFIO%fName),     RC=STATUS)
    _VERIFY(STATUS)

    if (MCFIO%JM == 6 * MCFIO%IM) then
       if (MCFIO%newFormat) then
          call ESMF_CFIOSet(MCFIO%CFIO, formatVersion=2.90, RC=STATUS)
          _VERIFY(STATUS)
          MCFIO%useFaceDim = .true.
       end if
    end if

! Create FILE from the root of the partition working on this bundle.
!------------------------------------------------------------------

    AMROOT: if (mCFIO%MYPE==mCFIO%RootRank) then

       call ESMF_CFIOFileCreate(MCFIO%CFIO, format=MCFIO%format, &
                                expid=MCFIO%EXPID,      RC=STATUS)
       _VERIFY(STATUS)
!      print *, ' Created CFIO File: ', trim(mCFIO%fName)

    end if AMROOT

    _RETURN(ESMF_SUCCESS)
  end subroutine MAPL_CFIOCreateWrite



  subroutine MAPL_CFIOOpenWrite ( MCFIO, RC )

    type(MAPL_CFIO),           intent(INOUT) :: MCFIO
    integer,         optional, intent(  OUT) :: RC

    integer                        :: STATUS


! Open the file for writing only at the root process
!---------------------------------------------------

    AMROOT: if (mCFIO%MYPE==mCFIO%RootRank) then
       call ESMF_CFIOFileOpen(MCFIO%CFIO, fmode=0, RC=STATUS)
       _VERIFY(STATUS)
!      print *, ' Opened CFIO File: ', trim(mCFIO%fName)
    end if AMROOT

    _RETURN(ESMF_SUCCESS)
  end subroutine MAPL_CFIOOpenWrite

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!BOP

! !IIROUTINE: MAPL_CFIOCreateFromState --- Creates MAPL CFIO Object from a State

! !INTERFACE:
!
  subroutine MAPL_CFIOCreateFromState ( MCFIO, NAME, CLOCK, STATE, OFFSET,  &
                                        OUTPUT_GRID, CHUNKSIZE, FREQUENCY, &
                                        LEVELS, DESCR, BUNDLE, &
                                        XYOFFSET, VCOORD, VUNIT, VSCALE,   &
                                        SOURCE, INSTITUTION, COMMENT, CONTACT, &
                                        FORMAT, EXPID, DEFLATE, GC,  ORDER, &
                                        NumCores, nbits, TM, Conservative,  RC )

!
! !ARGUMENTS:
!
    type(MAPL_CFIO),             intent(OUT) :: MCFIO
    character(LEN=*),            intent(IN)  :: NAME
    type(ESMF_State),            intent(INout)  :: STATE
    type(ESMF_Clock),            intent(INOUT)  :: CLOCK
    type(ESMF_FieldBundle), optional,  pointer    :: BUNDLE
    type(ESMF_TimeInterval), &
                     optional,   intent(INOUT):: OFFSET
    type (ESMF_Grid),optional,   pointer     :: OUTPUT_GRID
    integer,         optional,   pointer     :: CHUNKSIZE(:)
    integer,         optional,   intent(IN)  :: FREQUENCY
    real,            optional,   pointer     :: LEVELS(:)
    character(LEN=*),optional,   intent(IN)  :: DESCR
    real,            optional,   intent(IN)  :: VSCALE
    character(len=*),optional,   intent(IN)  :: VUNIT     
    character(len=*),optional,   intent(IN)  :: VCOORD     
    integer,         optional,   intent(IN)  :: XYOFFSET
    character(len=*),optional,   intent(IN)  :: source
    character(len=*),optional,   intent(IN)  :: institution     
    character(len=*),optional,   intent(IN)  :: comment
    character(len=*),optional,   intent(IN)  :: contact     
    character(len=*),optional,   intent(IN)  :: format
    character(len=*),optional,   intent(IN)  :: EXPID
    integer,         optional,   intent(IN)  :: DEFLATE
    type(ESMF_GridComp),optional,intent(IN)  :: GC
    integer,         optional,   intent(IN)  :: Order
    integer,         optional,   intent(IN)  :: Nbits
    integer,         optional,   intent(IN)  :: NumCores
    integer,         optional,   intent(IN)  :: TM
    integer,         optional,   intent(IN)  :: CONSERVATIVE
    integer, optional,           intent(OUT) :: RC
!
#ifdef ___PROTEX___

    !DESCRIPTION: 

     Creates a MAPL\_CFIO object from a State. States are written by
     ``serializing'' all Fields in them, whether they are directly in
     the State or are contained within a hierarchy of embedded Bundles
     and States, into a single Bundle.

     The Method optionally returns a pointer to the serialized ESMF
     Bundle, but this is not needed for MAPL\_Write
     operations. Otherwise arguments are the same as for
     CreateFromBundle.

   Its non-optional arguments associate a {\tt NAME}, an ESMF {\tt
   BUNDLE}, and a {\tt CLOCK} with the object. An ESMF TimeInterval
   {\tt OFFSET} is an optional argument that sets an offset between the
   time on the clock when eriting and the time stamp used for the data
   (defaults to no offset).

   The {\tt format} optional argument determines whether the write
   will use the linked self-describing format (SDF) library (HDF or
   netcdf) or write GrADS readable flat files. Currently only the SDF
   library option is supported.

   The remaining (optional) arguments are especialized and used
   primarily to support MAPL\_History, or to provide documentation in
   the form of character strings that will be placed in corresponding
   attributes in the SDF file.

  !REVISION HISTORY:
 
   12Jun2007 Todling  Added EXPID as opt argument

#endif

!EOP

    character(len=ESMF_MAXSTR)     :: Iam="MAPL_CFIOCreateFromState"
    integer                        :: STATUS

! Locals

    type(ESMF_FieldBundle), target :: tBUNDLE

!   Create an empty bundle
!   ----------------------

    tBUNDLE = ESMF_FieldBundleCreate ( name=Iam, rc=STATUS )
    _VERIFY(STATUS)
    
!   Serialize the state
!   -------------------

    call ESMFL_BundleAddState ( tBUNDLE, STATE, rc=STATUS, VALIDATE=.true. )
    _VERIFY(STATUS)

!   Create the mapl_CFIO object
!   ----------------------

    call MAPL_CFIOCreateFromBundle ( MCFIO, NAME, CLOCK, tBUNDLE,        &
                                     OFFSET = OFFSET,                    & 
                                     OUTPUT_GRID=OUTPUT_GRID,            &
                                     CHUNKSIZE=CHUNKSIZE,                &
                                     FREQUENCY=FREQUENCY,                &
                                     LEVELS=LEVELS,                      &
                                     DESCR=DESCR,                        &
                                     XYOFFSET= XYOFFSET,                 &
                                     VCOORD  = VCOORD,                   &
                                     VUNIT   = VUNIT,                    &
                                     VSCALE  = VSCALE,                   &
                                     SOURCE  = SOURCE,                   &
                                     INSTITUTION = INSTITUTION,          &
                                     COMMENT = COMMENT,                  &
                                     CONTACT = CONTACT,                  &
                                     FORMAT  = FORMAT,                   &
                                     EXPID   = EXPID,                    &
                                     DEFLATE = DEFLATE,                  &
                                     GC      = GC,                       &
                                     ORDER   = ORDER,                    &
                                     NumCores= NUMCORES,                 &
                                     nbits   = NBITS,                    &
                                     TM      = TM,                       &
                                     CONSERVATIVE=CONSERVATIVE,          &
                                                               RC=STATUS )
    _VERIFY(STATUS)

    if ( present(BUNDLE) ) then
         BUNDLE => tBUNDLE
    end if

    _RETURN(ESMF_SUCCESS)

  end subroutine MAPL_CFIOCreateFromState
    

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!BOP

! !IROUTINE: MAPL_CFIOWrite --- Writing Methods
! !IIROUTINE: MAPL_CFIOWriteBundle --- Writes an ESMF Bundle 

! !INTERFACE:
!
  subroutine MAPL_CFIOWriteBundlePost( MCFIO, PrePost, RC )
!
! !ARGUMENTS:
!
    type(MAPL_CFIO  ),               intent(INOUT) :: MCFIO
    logical,               optional, intent(IN   ) :: PrePost
    integer,               optional, intent(  OUT) :: RC
!
#ifdef ___PROTEX___

 !DESCRIPTION:  

       Writes an ESMF Bundle to a File. Only the MAPL\_CFIO object is
       a required argument as pointers to the actual data to be
       written is recorded in it during creation.

       {\tt CLOCK, BUNDLE} can be used to override the choice
       made at creation, but this is of dubious value, particularly
       for {\tt BUNDLE} since it must be excatly conformant with the
       creation {\tt BUNDLE}. {\tt NBITS} if the number of bits of 
       the mantissa to retain. This is used to write files with degraded
       precision, which can then be compressed with standard utilities.
       The default is no degradation of precision.

       {\bf A note about compression.} NetCDF-4, HDF-4 and HDF-5 all
       support transparent internal GZIP compression of the data being
       written. However, very little is gained by compressing float
       point fields from earth system models. Compression yields can
       be greatly increased by setting to zero bits in the mantissa of float
       numbers. On average 50\% compression can be achieved, while
       preserving a meaningful accuracy in the fields. Unlike
       classical CF compression by means of {\tt scale\_factor} and
       {\tt add\_offset} attributes, internal GZIP compression
       requires no special handling by the users of the data. In fact,
       they do not even need to know that the data is compressed! At this
       point, MAPL\_CFIO does not activate this GZIP compression
       feature in the files being written, but the resulting precision 
       degredaded files can be compressed offline with the HDF-4 
       {\tt hrepack} utility.

#endif

!EOP

    integer                    :: status

    type(ESMF_FIELD)           :: FIELD
    integer                    :: L, K, k0, LM
    integer                    :: NN
    real,             pointer  :: Ptr2(:,:), Ptr3(:,:,:)
    real, target, allocatable  :: Ple3d(:,:,:)
    real,         allocatable  :: Pl3d(:,:,:)
    real,         allocatable  :: Ptrx(:,:,:)
    real,             pointer  :: layer(:,:),ps0(:,:)
    logical                    :: PrePost_
    integer                    :: globalcount(3)
    type(ESMF_VM)              :: vm 

!                              ---

    _ASSERT(MCFIO%CREATED, 'MCFIO%CREATED is false')

    if (present(PrePost)) then
       PrePost_ = PrePost
    else
       PrePost_ = .true.
    end if

!  Set centers and edges of interpolating field
!----------------------------------------------

    if(mCFIO%Vinterp) then
       call ESMF_FieldBundleGet(mCFIO%bundle, fieldName=mCFIO%Vvar, Field=Field,  RC=STATUS)
       _VERIFY(STATUS)

       nullify (ptr3)
       call ESMF_FieldGet(Field, localDE=0, farrayPtr=Ptr3, rc=status)
       _VERIFY(STATUS)

       allocate( LAYER(size(Ptr3,1),size(Ptr3,2) ), stat=status)
       _VERIFY(STATUS)

       if (associated(mcfio%regridder)) then
          call ESMF_VMGetCurrent(vm,rc=status)
          _VERIFY(status)
          call MAPL_GridGet(mcfio%grid,globalCellCountPerDim=globalCount,rc=status)
          _VERIFY(status)
          call MAPL_AllocNodeArray(ps0,[globalCount(1),globalCount(2)],rc=status)
          if(STATUS==MAPL_NoShm) allocate(ps0(globalCount(1),globalCount(2)),stat=status)
          _VERIFY(status)
          call MAPL_AllocNodeArray(mcfio%surfaceLayer,[mcfio%im,mcfio%jm],rc=status)
          if(STATUS==MAPL_NoShm) allocate(mcfio%surfaceLayer(mcfio%im,mcfio%jm),stat=status)
          _VERIFY(STATUS)
       end if

! The Ptr3 interpolating variable is a zero-based (0-LM) edge variable
!---------------------------------------------------------------------
       if(lbound(PTR3,3)==0) then
          allocate( ple3D(size(Ptr3,1),size(Ptr3,2),size(Ptr3,3)  ), stat=status)
          _VERIFY(STATUS)
          allocate(  pl3D(size(Ptr3,1),size(Ptr3,2),size(Ptr3,3)-1), stat=status)
          _VERIFY(STATUS)

          if    (mCFIO%Func=='log') then
             ple3D = log(Ptr3)
             pl3D  = log( 0.5*(Ptr3(:,:,1:)+Ptr3(:,:,0:ubound(Ptr3,3)-1)) )
          elseif(mCFIO%Func=='pow') then
             ple3D = Ptr3**mCFIO%pow
             pl3D  =    ( 0.5*(Ptr3(:,:,1:)+Ptr3(:,:,0:ubound(Ptr3,3)-1)) )**mCFIO%pow
          else
             ple3D = Ptr3
             pl3D  =    ( 0.5*(Ptr3(:,:,1:)+Ptr3(:,:,0:ubound(Ptr3,3)-1)) )
          end if
          if (associated(mCFIO%regridder)) then
             mcfio%ascending = (ptr3(1,1,0)<ptr3(1,1,1))
             call ArrayGather(ptr3(:,:,ubound(ptr3,3)),ps0,mcfio%grid,rc=status)
             _VERIFY(status) 
          end if

       else

! The Ptr3 interpolating variable is a (1-LM) mid-layer variable
!---------------------------------------------------------------
          allocate(  Ptrx(size(Ptr3,1),size(Ptr3,2),0:size(Ptr3,3)  ), stat=status)
          _VERIFY(STATUS)
          allocate( ple3D(size(Ptr3,1),size(Ptr3,2),0:size(Ptr3,3)  ), stat=status)
          _VERIFY(STATUS)
          allocate(  pl3D(size(Ptr3,1),size(Ptr3,2),  size(Ptr3,3)  ), stat=status)
          _VERIFY(STATUS)

          Ptrx(:,:,0               ) = 0.5*( 3* Ptr3(:,:,1)             -Ptr3(:,:,2)                )
          Ptrx(:,:,1:size(Ptr3,3)-1) = 0.5*(    Ptr3(:,:,2:size(Ptr3,3))+Ptr3(:,:,1:size(Ptr3,3)-1) )
          Ptrx(:,:,  size(Ptr3,3)  ) = 0.5*( 3* Ptr3(:,:,  size(Ptr3,3))-Ptr3(:,:,  size(Ptr3,3)-1) )

          if    (mCFIO%Func=='log') then
             ple3D = log(Ptrx)
             pl3D  = log( 0.5*(Ptrx(:,:,1:)+Ptrx(:,:,0:ubound(Ptrx,3)-1)) )
          elseif(mCFIO%Func=='pow') then
             ple3D = Ptrx**mCFIO%pow
             pl3D  =    ( 0.5*(Ptrx(:,:,1:)+Ptrx(:,:,0:ubound(Ptrx,3)-1)) )**mCFIO%pow
          else
             ple3D = Ptrx
             pl3D  =    ( 0.5*(Ptrx(:,:,1:)+Ptrx(:,:,0:ubound(Ptrx,3)-1)) )
          end if

          if (associated(mCFIO%regridder)) then
             mcfio%ascending = (ptrx(1,1,0)<ptrx(1,1,1))
             call ArrayGather(ptrx(:,:,ubound(ptrx,3)),ps0,mcfio%grid,rc=status)
             _VERIFY(status) 
             
          end if
          deallocate(Ptrx)
       end if

       if (associated(mCFIO%regridder)) then
          call MAPL_BcastShared(vm,data=ps0,N=globalCount(1)*globalCount(2),root=0,RootOnly=.false.,rc=status)
          _VERIFY(status)
          if (MAPL_AmNodeRoot .or. (.not. MAPL_ShmInitialized)) then
             call mCFIO%regridder%regrid(ps0,mcfio%surfaceLayer,rc=status)
             _VERIFY(status)
          end if
         
          if (MAPL_ShmInitialized) then 
             call MAPL_DeAllocNodeArray(ps0,rc=status)
             _VERIFY(status)
          else
             deallocate(ps0)
          end if
       end if

    end if

    call MAPL_CFIOSetVectorPairs(mCFIO,rc=status) 
    _VERIFY(status)

! Cycle through all variables posting receives.
!----------------------------------------------

    nn = 0

    POSTRECV: do L=1, size(MCFIO%VarDims)
       if    (mCFIO%VarDims(L)==2) then ! Rank == 2
          LM = 1
       elseif(MCFIO%VarDims(L)==3) then ! Rank == 3
          LM = MCFIO%LM
       else
          LM = 0
       endif

       do K=1,LM
          nn    = nn + 1
          call MAPL_CreateRequest(MCFIO%GRID, MCFIO%Krank(nn), MCFIO%reqs(nn), &
                                  tag=nn, RequestType=MAPL_IsGather, PrePost = PrePost_, RC=STATUS)
          _VERIFY(STATUS)
       enddo
    end do POSTRECV

! Cycle through all variables posting sends.
!-------------------------------------------

    nn = 0

    VARIABLES: do L=1, size(MCFIO%VarDims)

       call ESMF_FieldBundleGet(MCFIO%BUNDLE, MCFIO%VARNAME(L), FIELD=FIELD,                  RC=STATUS)
       _VERIFY(STATUS)

! We treat only fields with rank 2 (horizontal 2D) and 
!  rank 3 (first 2 dimension are horz, third is vert).
!--------------------------------------------------------

       RANK: if (MCFIO%VarDims(L)==2) then ! Rank == 2

          nn    = nn + 1
          ptr2  =>null()

          call ESMF_FieldGet      (FIELD, localDE=0, farrayPtr=PTR2, RC=STATUS)
          _VERIFY(STATUS)
          call MAPL_ArrayIGather  (Ptr2, MCFIO%reqs(NN), RC=STATUS)
          _VERIFY(STATUS)

       elseif(MCFIO%VarDims(L)==3) then ! Rank == 3

          ptr3 =>null()

          call ESMF_FieldGet      (FIELD, localDE=0, farrayPtr=PTR3, RC=STATUS)
          _VERIFY(STATUS)

          K0  = lbound(PTR3,3) - 1

! For each level, interpolate vertically and post gather
!-------------------------------------------------------

          LAYERS: do K=1,MCFIO%LM
             VINTERP: if(mCFIO%Vinterp) then
                call VertInterp(LAYER, PTR3, MCFIO%LEVS(K), ple3d, pl3d, rc=status)
                _VERIFY(STATUS)
             else if (MCFIO%LEVS(K)<0) then
                LAYER => PTR3(:,:,K+K0)
             else
                LAYER => PTR3(:,:,nint(MCFIO%LEVS(K)))
             end if VINTERP

             nn    = nn + 1

             call MAPL_ArrayIGather(LAYER, MCFIO%reqs(nn), rc=status)
             _VERIFY(STATUS)
          enddo LAYERS

       end if RANK

    end do VARIABLES



!   if(mCFIO%myPE==mCFIO%Root) then
!      print *, ' Posted to File: ', trim(mCFIO%fName)
!   endif


    if(mCFIO%Vinterp) then
       deallocate( ple3D, stat=status)
       _VERIFY(STATUS)
       deallocate( pl3D , stat=status)
       _VERIFY(STATUS)
       deallocate( Layer, stat=status)
       _VERIFY(STATUS)
    end if

    _RETURN(ESMF_SUCCESS)
  end subroutine MAPL_CFIOWriteBundlePost

  subroutine MAPL_CFIOWriteBundleWait( MCFIO, CLOCK, RC )

    type(MAPL_CFIO  ),                 intent(INOUT) :: MCFIO
    type(ESMF_CLOCK),                  intent(INOUT) :: CLOCK
    integer,                optional,  intent(  OUT) :: RC

! Locals
!-------

    integer                    :: status

    integer                    :: L, K, NN
    logical                    :: AmRoot, MyGlobal
    real,          pointer     :: Gptr2Out(:,:  )
    real,          pointer     :: PtrGlob (:,:  )
    integer                    :: counts(5)
    integer                    :: IM0,JM0,I,IP
    logical                    :: FixPole
    integer                    :: levsize
    integer                    :: lm, nv
    logical                    :: transAlreadyDone
    type(Ptr2Arr), allocatable :: globPtrArr(:)
    type(Ptr2Arr)              :: PtrTypeIn(2)
    type(Ptr2Arr)              :: PtrTypeOut(2)

    _UNUSED_DUMMY(CLOCK)

! Space for global arrays is allocated everywhere, even if not used.
!------------------------------------------------------------------

    _ASSERT(MCFIO%CREATED, 'MCFIO%CREATED is false')

! Allocate global 2d and 3d arrays at the writing resolution
!  Note that everybody allocated these.
!-----------------------------------------------------------

    call MAPL_GridGet( MCFIO%GRID, globalCellCountPerDim=COUNTS, RC=STATUS)
    _VERIFY(STATUS)

    IM0 = COUNTS(1)
    JM0 = COUNTS(2)

    !if(any(mCFIO%myPE==mCFIO%Krank)) then
       !allocate(Gptr3Out(Mcfio%IM, Mcfio%JM,1), stat=STATUS)
       !_VERIFY(STATUS)

       !Gptr2Out => Gptr3Out(:,:,1)
       !Gptr2Out(:,:) = 0.0
    !end if

    nn   = 0

    AmRoot     = mCFIO%myPE==MCFIO%RootRank

    allocate(globPtrArr(size(mCFIO%reqs)), stat=status)
    _VERIFY(STATUS)
    COLCTVWAIT: do nn=1,size(mCFIO%reqs)
       ! Wait on request for slice nn
       !-----------------------------
       call MAPL_CollectiveWait(MCFIO%reqs(nn), DstArray=PtrGlob,  rc=status)
       _VERIFY(STATUS)
       globPtrArr(nn)%ptr => PtrGlob ! this is valid only if myGlobal is .true.
    end do COLCTVWAIT

    nn   = 0
    VARIABLES: do L=1,size(MCFIO%VarDims)
          
       FixPole = (MCFIO%VarType(L) == MAPL_VectorField) .and. &
                 (JM0              == 6*IM0)            .and. &
                 (Mcfio%JM         /= 6*mcfio%IM)  
 
       RANK: if (MCFIO%VarDims(L)==2) then
          LM = 1
       else  if (MCFIO%VarDims(L)==3) then
          LM = MCFIO%lm
       else
          LM = 0
       end if RANK

       LEVELS: do k=1,LM
          nn       = nn + 1
          MyGlobal = mCFIO%Krank(nn) == MCFIO%MYPE
          PtrGlob => globPtrArr(nn)%ptr

! Horizontal Interpolation and Shaving on PEs with global data
! ------------------------------------------------------------

          if( MyGlobal ) then
             nv = mCFIO%pairList(nn)
             VECTORTEST: if (nv == 0) then
                ! scalar
                allocate( MCFIO%reqs(nn)%Trans_Array(Mcfio%IM, Mcfio%JM, 1), stat=STATUS )
                _VERIFY(STATUS)
                Gptr2Out => MCFIO%reqs(nn)%Trans_Array(:,:,1)
                PtrTypeIn (1)%ptr => globPtrArr(nn)%ptr
                PtrTypeOut(1)%ptr => Gptr2Out
                call TransShaveAndSend(PtrTypeIn(1:1),PtrTypeOut(1:1),MCFIO%reqs(nn)%s_rqst,doTrans=.true.,IdxOut=1)
                _VERIFY(status)
             else if (nv > 0) then 
                ! I am U part of vector
                if (associated(MCFIO%reqs(nn)%Trans_Array)) then
                   _ASSERT(associated(MCFIO%reqs(nv)%Trans_Array), 'Trans_Array not associated')
                   TransAlreadyDone = .true.
                else
                   TransAlreadyDone = .false.
                   allocate( MCFIO%reqs(nn)%Trans_Array(Mcfio%IM, Mcfio%JM, 1), stat=STATUS )
                   _VERIFY(STATUS)
                   allocate( MCFIO%reqs(nv)%Trans_Array(Mcfio%IM, Mcfio%JM, 1), stat=STATUS )
                   _VERIFY(STATUS)
                endif
                PtrTypeIn (1)%ptr => globPtrArr(nn)%ptr
                PtrTypeIn (2)%ptr => globPtrArr(nv)%ptr
                PtrTypeOut(1)%ptr => MCFIO%reqs(nn)%Trans_Array(:,:,1)
                PtrTypeOut(2)%ptr => MCFIO%reqs(nv)%Trans_Array(:,:,1)
                call TransShaveAndSend(PtrTypeIn(1:2),PtrTypeOut(1:2),MCFIO%reqs(nn)%s_rqst,doTrans=.not.TransAlreadyDone,IdxOut=1)
                _VERIFY(status)
             else 
                ! I am V part of vector
                nv = abs(nv)
                if (associated(MCFIO%reqs(nn)%Trans_Array)) then
                   _ASSERT(associated(MCFIO%reqs(nv)%Trans_Array), 'Trans_Array not associated')
                   TransAlreadyDone = .true.
                else
                   TransAlreadyDone = .false.
                   allocate( MCFIO%reqs(nn)%Trans_Array(Mcfio%IM, Mcfio%JM, 1), stat=STATUS )
                   _VERIFY(STATUS)
                   allocate( MCFIO%reqs(nv)%Trans_Array(Mcfio%IM, Mcfio%JM, 1), stat=STATUS )
                   _VERIFY(STATUS)
                endif
                PtrTypeIn (1)%ptr => globPtrArr(nv)%ptr
                PtrTypeIn (2)%ptr => globPtrArr(nn)%ptr
                PtrTypeOut(1)%ptr => MCFIO%reqs(nv)%Trans_Array(:,:,1)
                PtrTypeOut(2)%ptr => MCFIO%reqs(nn)%Trans_Array(:,:,1)
                call TransShaveAndSend(PtrTypeIn(1:2),PtrTypeOut(1:2),MCFIO%reqs(nn)%s_rqst,doTrans=.not.TransAlreadyDone,IdxOut=2)
                _VERIFY(status)
             end if VECTORTEST
          endif
       end do LEVELS

    end do VARIABLES

!    do nn=1,size(mCFIO%reqs)
!       MyGlobal = MCFIO%Krank(nn) == MCFIO%MYPE
!       if (myGlobal) then
!          deallocate(globPtrArr(nn)%ptr)
!          NULLIFY(globPtrArr(nn)%ptr)
!       end if
!    end do
    deallocate(globPtrArr)

   !if(AmRoot) then
   !   write(6,'(1X,"TransShaveAndSend: ",i6," Slices (",i3," Nodes, ",i2," CoresPerNode) to File:  ",a)') &
   !         size(MCFIO%reqs),mCFIO%partsize/mCFIO%numcores,mCFIO%numcores,trim(mCFIO%fName)
   !endif

    !if (any(mCFIO%myPE==mCFIO%Krank)) then
       !deallocate(Gptr3Out, stat=STATUS)
       !_VERIFY(STATUS)
    !end if

    _RETURN(ESMF_SUCCESS)

  contains
    
    subroutine TransShaveAndSend(PtrIn,PtrOut,request,doTrans,idxOut)
      type(Ptr2Arr) :: PtrIn(:)
      type(Ptr2Arr) :: PtrOut(:)
      integer       :: request
      logical       :: doTrans
      integer       :: idxOut

      real, pointer :: Gin (:,:)
      real, pointer :: Gout(:,:)
      real, dimension(:,:,:), pointer :: uin, uout, vin, vout
      integer :: im, jm

      type(c_ptr)   :: cptr

      if (size(PtrIn) == 1) then
         _ASSERT(idxOut ==1, 'idxOut /= 1')
         Gin => PtrIn(1)%ptr
         Gout => PtrOut(1)%ptr

         if (associated(mCFIO%regridder)) then
            if (mCFIO%regridConservative) then
               call mCFIO%regridder%regrid(Gin, Gout, rc=status)
               _VERIFY(STATUS)
            else
               call mCFIO%regridder%set_undef_value(MAPL_undef)
               call mCFIO%regridder%regrid(Gin, Gout, rc=status)
               _VERIFY(status)
            end if
            if (mcfio%vinterp .and. (lm .ne. 1) ) then
               if (mcfio%ascending) then
                  where(mcfio%surfaceLayer<mcfio%unmodifiedLevs(k)) gout=MAPL_UNDEF
               else
                  where(mcfio%surfaceLayer>mcfio%unmodifiedLevs(k)) gout=MAPL_UNDEF
               endif
            end if
         else
            _ASSERT( all(shape(gout)==shape(gin)), 'in-out shape mismatch')
            gout=gin
         end if

         ! if going from CS to LAT-LON pole winds are wrong, approximate fix below
         if (FixPole) then
            do i=1,mcfio%im
               ip = i+(mcfio%im/2)
               if (ip > mcfio%im) ip = ip - mcfio%im
               if ( (gout(i,mcfio%jm-1) == MAPL_UNDEF) .or. (gout(ip,mcfio%jm-1) == MAPL_UNDEF)) then
                  gout(i,mcfio%jm) = MAPL_UNDEF
               else
                  gout(i,mcfio%jm)=(gout(i,mcfio%jm-1)-gout(ip,mcfio%jm-1))/2.0
               end if
               if ( (gout(i,2) == MAPL_UNDEF) .or. (gout(ip,2) == MAPL_UNDEF)) then
                  gout(i,1) = MAPL_UNDEF
               else
                  gout(i,1)=(gout(i,2)-gout(ip,2))/2.0
               endif
            enddo
         endif

         deallocate(Gin)
         nullify   (Gin)
      else
         _ASSERT(size(PtrIn) == 2, 'if not scalar, ptrIn must be 2-vector') 
         _ASSERT(size(PtrOut) == 2, 'if not scalar, ptrOut must be 2-vector') 
         Gout => PtrOut(idxOut)%ptr
         ! TLC: Probably do not need this conditional now that there are identity regridders
         if (doTrans) then 
            _ASSERT(associated(mcfio%regridder), 'mcfio%regridder not associated')
            im = size(PtrIn(1)%ptr,1)
            jm = size(PtrIn(1)%ptr,2)

            ! MAT PGI cannot handle C_LOC call inside C_F_POINTER
            cptr = C_loc(PtrIn(1)%ptr(1,1))
            call C_F_pointer (cptr, uin,[im,jm,1])
            
            cptr = C_loc(PtrIn(2)%ptr(1,1))
            call C_F_pointer (cptr, vin,[im,jm,1])

!@#               allocate(uin(im,jm,1), vin(im,jm,1))
!@#               uin(:,:,1) = PtrIn(1)%ptr
!@#               vin(:,:,1) = PtrIn(2)%ptr

            im = size(PtrOut(1)%ptr,1)
            jm = size(PtrOut(1)%ptr,2)

            cptr = C_loc(PtrOut(1)%ptr(1,1))
            call C_F_pointer (cptr, uout,[im,jm,1])
            
            cptr = C_loc(PtrOut(2)%ptr(1,1))
            call C_F_pointer (cptr, vout,[im,jm,1])
            
!@#               allocate(uout(im,jm,1), vout(im,jm,1))
            
            call mCFIO%regridder%set_undef_value(MAPL_undef)
            call mCFIO%regridder%regrid(uin, vin, uout, vout, rc=status)
            _VERIFY(status)
            if (mcfio%vinterp .and. (lm .ne. 1) ) then
               if (mcfio%ascending) then
                  where(mcfio%surfaceLayer<mcfio%unmodifiedLevs(k)) uout(:,:,1)=MAPL_UNDEF
                  where(mcfio%surfaceLayer<mcfio%unmodifiedLevs(k)) vout(:,:,1)=MAPL_UNDEF
               else
                  where(mcfio%surfaceLayer>mcfio%unmodifiedLevs(k)) uout(:,:,1)=MAPL_UNDEF
                  where(mcfio%surfaceLayer>mcfio%unmodifiedLevs(k)) vout(:,:,1)=MAPL_UNDEF
               endif
            end if

            deallocate(PtrIn(1)%ptr)
            nullify(PtrIn(1)%ptr)
            deallocate(PtrIn(2)%ptr)
            nullify(PtrIn(2)%ptr)
         end if
      end if

      if(mCFIO%NBITS < 24) then
         call ESMF_CFIODownBit ( Gout, Gout, mCFIO%NBITS, undef=MAPL_undef, rc=STATUS )
         _VERIFY(STATUS)
      end if

      if (mcfio%async) then
         levsize = mcfio%im*mcfio%jm
         call MPI_ISend(Gout,levsize,MPI_REAL,mcfio%asyncWorkRank, &
              MAPL_TAG_SHIPDATA,mCFIO%globalComm,request,status)
         _VERIFY(STATUS)
      else
         call MPI_ISend(Gout, size(Gout), MPI_REAL, MCFIO%RootRank, &
                 trans_tag, mCFIO%comm, request,         STATUS)
            _VERIFY(STATUS)

      end if


      return
    end subroutine TransShaveAndSend

  end subroutine MAPL_CFIOWriteBundleWait

  subroutine MAPL_CFIOWriteBundleWrite( MCFIO, CLOCK, RC )

    type(MAPL_CFIO  ),                 intent(INOUT) :: MCFIO
    type(ESMF_CLOCK),                  intent(INOUT) :: CLOCK
    integer,                optional,  intent(  OUT) :: RC

! Locals
!-------

    integer                    :: status

    integer                    :: L, K, NN
    integer                    :: YY,MM,DD,H,M,S
    integer                    :: noffset
    logical                    :: AmRoot, MyGlobal, LPERP
    type(ESMF_TIME )           :: TIME
    type(ESMF_Alarm)           :: PERPETUAL
    character(len=ESMF_MAXSTR) :: DATE
    character(len=ESMF_MAXSTR) :: ClockName
    real,          pointer     :: Gptr3Out(:,:,:)
    real,          pointer     :: Gptr2Out(:,:  )
    integer                    :: counts(5)
    integer                    :: IM0,JM0
    integer                    :: nymd,nhms

! Space for global arrays is allocated everywhere, even if not used.
!------------------------------------------------------------------

    _ASSERT(MCFIO%CREATED, 'MCFIO%CREATED is false')

! Set the time at which we will be writing from the clock
!--------------------------------------------------------

    ASYNCIF: if (.not.mcfio%async) then

       call ESMF_ClockGet       (CLOCK, name=ClockName, CurrTime =TIME, RC=STATUS)
       _VERIFY(STATUS)

           call ESMF_TimeIntervalGet( MCFIO%OFFSET, S=noffset, rc=status )
           _VERIFY(STATUS)
           if( noffset /= 0 ) then
               LPERP = ( index( trim(clockname),'_PERPETUAL' ).ne.0 )
           if( LPERP ) then
               call ESMF_ClockGetAlarm ( clock, alarmName='PERPETUAL', alarm=PERPETUAL, rc=status )
               _VERIFY(STATUS)
               if( ESMF_AlarmIsRinging(PERPETUAL) ) then
                   call ESMF_TimeGet ( Time, YY = YY, &
                                             MM = MM, &
                                             DD = DD, &
                                             H  = H , &
                                             M  = M , &
                                             S  = S, rc=status )
                                             MM = MM + 1
                   call ESMF_TimeSet ( Time, YY = YY, &
                                             MM = MM, &
                                             DD = DD, &
                                             H  = H , &
                                             M  = M , &
                                             S  = S, rc=status )
               endif
           endif
           endif

       TIME = TIME - MCFIO%OFFSET

       call ESMF_TimeGet        (TIME,     timeString=DATE,     RC=STATUS)
       _VERIFY(STATUS)

   ! Allocate global 2d and 3d arrays at the writing resolution
   !  Note that everybody allocated these.
   !-----------------------------------------------------------

       call MAPL_GridGet( MCFIO%GRID, globalCellCountPerDim=COUNTS, RC=STATUS)
       _VERIFY(STATUS)

       IM0 = COUNTS(1)
       JM0 = COUNTS(2)

       if(any(mCFIO%myPE==mCFIO%Krank)) then
          allocate(Gptr3Out(Mcfio%IM, Mcfio%JM,1), stat=STATUS)
          _VERIFY(STATUS)

          Gptr2Out => Gptr3Out(:,:,1)
          Gptr2Out(:,:) = 0.0
       end if

       AmRoot     = mCFIO%myPE==mCFIO%rootRank

   ! Finally Do The Writes
   !______________________

       nn   = 0
       VARIABLESW: do L=1,size(MCFIO%VarDims)

          RANKW: if (MCFIO%VarDims(L)==2) then
             nn       = nn + 1
             MyGlobal = mCFIO%Krank(nn) == MCFIO%MYPE

   ! Horizontal Interpolation and Shaving on PEs with global data
   ! ------------------------------------------------------------

             IAMVARROOT: if(AmRoot) then
                Gptr2Out => Gptr3Out(:,:,1)
                call MPI_Recv(Gptr2Out,size(Gptr2Out),MPI_REAL, mCFIO%Krank(nn), &
                              trans_tag,  mCFIO%comm, MPI_STATUS_IGNORE, STATUS)
                _VERIFY(STATUS)

                call StrToInt(date,nymd,nhms)
                call ESMF_CFIOVarWrite(MCFIO%CFIO, trim(MCFIO%VARNAME(L)), &
                                       Gptr2Out, timeString=DATE,  RC=STATUS)
                _VERIFY(STATUS)

             end if IAMVARROOT

          elseif (MCFIO%VarDims(L)==3) then

   ! Everyone waits, processes their layer, and sends it to root.
   !   Root write it out.
   !-------------------------------------------------------------

             LEVELSW: do k=1,MCFIO%lm
                nn       = nn + 1
                MyGlobal = MCFIO%Krank(nn) == MCFIO%MYPE

                IAMLEVROOT: if(AmRoot) then
                   Gptr2Out => Gptr3Out(:,:,1)
                   call MPI_Recv(Gptr2Out, size(Gptr2Out), MPI_REAL, mCFIO%Krank(nn), &
                                 trans_tag,  mCFIO%comm, MPI_STATUS_IGNORE,   STATUS)
                   _VERIFY(STATUS)

                   call StrToInt(date,nymd,nhms)
                   call ESMF_CFIOVarWrite(MCFIO%CFIO, trim(MCFIO%VARNAME(L)), &
                                          Gptr3Out, kbeg=K, kount=1,          &
                                          timeString=DATE,           RC=STATUS)
                   _VERIFY(STATUS)

                end if IAMLEVROOT
             end do LEVELSW
          endif RANKW

       end do VARIABLESW

    end if ASYNCIF

   !if(AmRoot) then
   !   write(6,'(1X,"Wrote: ",i6," Slices (",i3," Nodes, ",i2," CoresPerNode) to File:  ",a)') &
   !         size(MCFIO%reqs),mCFIO%partsize/mCFIO%numcores,mCFIO%numcores,trim(mCFIO%fName)
   !endif

! Clean-up
!---------
    nn   = 0
    VARIABLESC: do L=1,size(MCFIO%VarDims)

       RANKC: if (MCFIO%VarDims(L)==2) then
          nn       = nn + 1
          MyGlobal = mCFIO%Krank(nn) == MCFIO%MYPE

             if( MyGlobal ) then
                call MPI_Wait(MCFIO%reqs(nn)%s_rqst, MPI_STATUS_IGNORE, STATUS)
                _VERIFY(STATUS)
                deallocate( MCFIO%reqs(nn)%Trans_Array, stat=STATUS)
                _VERIFY(STATUS)
                nullify( MCFIO%reqs(nn)%Trans_Array )
             endif

       elseif (MCFIO%VarDims(L)==3) then

          LEVELSC: do k=1,MCFIO%lm
             nn       = nn + 1
             MyGlobal = MCFIO%Krank(nn) == MCFIO%MYPE

             if( MyGlobal ) then
                call MPI_Wait(MCFIO%reqs(nn)%s_rqst, MPI_STATUS_IGNORE, STATUS)
                _VERIFY(STATUS)
                deallocate( MCFIO%reqs(nn)%Trans_Array, stat=STATUS)
                _VERIFY(STATUS)
                nullify( MCFIO%reqs(nn)%Trans_Array )
             endif

          end do LEVELSC
       endif RANKC

    end do VARIABLESC

  ! if(AmRoot) then
  !    write(6,'(1X,"Cleaned: ",i6," Slices (",i3," Nodes, ",i2," CoresPerNode) to File:  ",a)') &
  !          size(MCFIO%reqs),mCFIO%partsize/mCFIO%numcores,mCFIO%numcores,trim(mCFIO%fName)
  ! endif
    if (.not.mCFIO%async) then

       if (any(mCFIO%myPE==mCFIO%Krank)) then
          deallocate(Gptr3Out, stat=STATUS)
          _VERIFY(STATUS)
       end if

    end if

    _RETURN(ESMF_SUCCESS)

  end subroutine MAPL_CFIOWriteBundleWrite

!===========================================================================

!BOP

! !IROUTINE: MAPL_CFIOWrite --- Writing Methods
! !IIROUTINE: MAPL_CFIOWriteBundle --- Writes an ESMF Bundle 

! !INTERFACE:
!
  subroutine MAPL_CFIOWriteBundle( MCFIO, CLOCK, Bundle, &
                                   VERBOSE, NBITS, created, RC    )
!
! !ARGUMENTS:
!
    type(MAPL_CFIO  ),                 intent(INOUT) :: MCFIO
    type(ESMF_CLOCK),       optional,  intent(INOUT) :: CLOCK
    type(ESMF_FIELDBUNDLE), optional,  intent(INout) :: BUNDLE
    logical,                optional,  intent(IN   ) :: VERBOSE
    integer,                optional,  intent(IN   ) :: NBITS
    logical,                optional,  intent(IN   ) :: created
    integer,                optional,  intent(  OUT) :: RC
!
#ifdef ___PROTEX___

 !DESCRIPTION:  

       Writes an ESMF Bundle to a File. Only the MAPL\_CFIO object is
       a required argument as pointers to the actual data to be
       written is recorded in it during creation.

       {\tt CLOCK, BUNDLE} can be used to override the choice
       made at creation, but this is of dubious value, particularly
       for {\tt BUNDLE} since it must be excatly conformant with the
       creation {\tt BUNDLE}. {\tt NBITS} if the number of bits of 
       the mantissa to retain. This is used to write files with degraded
       precision, which can then be compressed with standard utilities.
       The default is no degradation of precision.

       {\bf A note about compression.} NetCDF-4, HDF-4 and HDF-5 all
       support transparent internal GZIP compression of the data being
       written. However, very little is gained by compressing float
       point fields from earth system models. Compression yields can
       be greatly increased by setting to zero bits in the mantissa of float
       numbers. On average 50\% compression can be achieved, while
       preserving a meaningful accuracy in the fields. Unlike
       classical CF compression by means of {\tt scale\_factor} and
       {\tt add\_offset} attributes, internal GZIP compression
       requires no special handling by the users of the data. In fact,
       they do not even need to know that the data is compressed! At this
       point, MAPL\_CFIO does not activate this GZIP compression
       feature in the files being written, but the resulting precision 
       degredaded files can be compressed offline with the HDF-4 
       {\tt hrepack} utility.

#endif

!EOP

    integer                    :: status
    logical                    :: isCreated

    _UNUSED_DUMMY(VERBOSE)

    _ASSERT(present(CLOCK), 'CLOCK argument must be present')
    _ASSERT(present(BUNDLE), 'BUNDLE argument must be present')

! for backward compatibility
!---------------------------

    if(present(NBITS)) then
       mCFIO%Nbits = Nbits
    end if

! DSK fName is set in History during runtime, but the G5 tutorial does not, so we must set it here
    if (IACHAR(MCFIO%fName(1:1)) == 0) then
       call MAPL_CFIOSet( MCFIO, fName=MCFIO%Name, RC=status )
       _VERIFY(STATUS)
    endif

    if (present(created)) then
       isCreated=created
    else
       isCreated=.false.
    end if
    if (.not.isCreated) then
       call MAPL_CFIOCreateWrite    ( MCFIO,         RC=status)
       _VERIFY(STATUS)
    end if

    call MAPL_CFIOWriteBundlePost( MCFIO,         RC=status)
    _VERIFY(STATUS)

    call MAPL_CFIOWriteBundleWait( MCFIO, CLOCK,  RC=status)
    _VERIFY(STATUS)

    call MAPL_CFIOWriteBundleWrite( MCFIO, CLOCK, RC=status)
    _VERIFY(STATUS)

    _RETURN(ESMF_SUCCESS)
  end subroutine MAPL_CFIOWriteBundle


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!BOP

! !IIROUTINE: MAPL_CFIOWriteState --- Writes an ESMF State

! !INTERFACE:
!
  subroutine MAPL_CFIOWriteState ( MCFIO, CLOCK, State, &
                                   VERBOSE, NBITS, RC   )
!
! !ARGUMENTS:
!
    type(MAPL_CFIO),             intent(INOUT) :: MCFIO
    type(ESMF_State),            intent(INout) :: STATE
    type(ESMF_CLOCK),            intent(INOUT) :: CLOCK
    integer, optional,           intent(  OUT) :: RC
    logical, optional,           intent(  IN)  :: VERBOSE
    integer, optional,           intent(  IN)  :: NBITS
!
#ifdef ___PROTEX___

    !DESCRIPTION:

     Serializes an ESMF state into a Bundle and writes it to a file.
     Only the MAPL\_CFIO object is a required argument as pointers to
     the actual data to be written is recorded in it during creation.

     {\tt CLOCK, BUNDLE} can be used to override the choice
     made at creation, but this is of dubious value, particularly
     for {\tt BUNDLE} since it must be excatly conformant with the
     creation {\tt BUNDLE}. {\tt NBITS} if the number of bits of 
     the mantissa to retain. This is used to write files with degraded
     precision, which can then be compressed with standard utilities.
     The default is no degradation of precision.

     {\bf A note about compression.} NetCDF-4, HDF-4 and HDF-5 all
     support transparent internal GZIP compression of the data being
     written. However, very little is gained by compressing float
     point fields from earth system models. Compression yields can
     be greatly increased by setting to zero bits in the mantissa of float
     numbers. On average 50\% compression can be achieved, while
     preserving a meaningful accuracy in the fields. Unlike
     classical CF compression by means of {\tt scale\_factor} and
     {\tt add\_offset} attributes, internal GZIP compression
     requires no special handling by the users of the data. In fact,
     they do not even need to know that the data is compressed! At this
     point, MAPL\_CFIO does not activate this GZIP compression
     feature in the files being written, but the resulting precision 
     degredaded files can be compressed offline with the HDF-4 
     {\tt hrepack} utility.

#endif

!EOP

    character(len=*), parameter  :: Iam="MAPL_CFIOWriteState"
    integer                      :: STATUS

! Locals

    type(ESMF_FieldBundle) :: tBUNDLE

! Get the appropriate bundle
!---------------------------

!!ALT    if(present(STATE)) then
       tBUNDLE = ESMF_FieldBundleCreate ( name=Iam, rc=STATUS )
       _VERIFY(STATUS)
       call ESMFL_BundleAddState ( tBUNDLE, STATE, rc=STATUS, VALIDATE=.true. )
       _VERIFY(STATUS)
!!ALT    else
!!ALT       tBUNDLE = MCFIO%BUNDLE
!!ALT    end if

!   Write the Bundle
!   ----------------

    call MAPL_CFIOWriteBundle ( MCFIO, CLOCK=CLOCK, BUNDLE=tBUNDLE, &
                                VERBOSE=VERBOSE, NBITS=NBITS, RC=STATUS   )
    _VERIFY(STATUS)

!!ALT    if(present(STATE)) then
       call ESMF_FieldBundleDestroy ( tBUNDLE, rc=STATUS )
       _VERIFY(STATUS)
!!ALT    endif

!   All done
!   --------

    _RETURN(ESMF_SUCCESS)

 end subroutine MAPL_CFIOWriteState

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!BOP

! !IROUTINE: MAPL_CFIORead --- Reading Methods
! !IIROUTINE: MAPL_CFIOReadBundle --- Reads an ESMF Bundle

! !INTERFACE:
!
  subroutine MAPL_CFIOReadBundle ( FILETMPL, TIME, BUNDLE, NOREAD, RC, &
                                   VERBOSE, FORCE_REGRID, ONLY_VARS, ONLY_LEVS, &
                                   TIME_IS_CYCLIC, TIME_INTERP, conservative, &
                                   voting, ignoreCase, doParallel, GSImode, getFrac, EXPID, collection_id )
!
! !ARGUMENTS:
!
    character(len=*),            intent(IN   ) :: FILETMPL
    type(ESMF_TIME),             intent(INout) :: TIME
    type(ESMF_FIELDBUNDLE),           intent(INOUT) :: BUNDLE
    logical, optional,           intent(IN   ) :: NOREAD
    integer, optional,           intent(  OUT) :: RC
    logical, optional,           intent(IN)    :: VERBOSE
    logical, optional,           intent(IN)    :: FORCE_REGRID 
    logical, optional,           intent(IN)    :: TIME_IS_CYCLIC
    logical, optional,           intent(IN)    :: TIME_INTERP
    logical, optional,           intent(IN)    :: conservative
    logical, optional,           intent(IN)    :: voting
    logical, optional,           intent(IN)    :: doParallel
    character(len=*), optional,  intent(IN)    :: ONLY_VARS 
    real,    optional,           intent(IN)    :: ONLY_LEVS(:)
    character(len=*), optional,  intent(IN)    :: EXPID
    logical, optional,           intent(IN)    :: ignoreCase
    logical, optional,           intent(IN)    :: GSImode
    integer, optional,           intent(IN)    :: getFrac
    integer, optional,           intent(IN)    :: collection_id
!
#ifdef ___PROTEX___
    !DESCRIPTION: 

     Reads an ESMF Bundle from a file on a given time. The file is
     open, read from, and closed on exit. The arguments are:
\bd
     \item[FILETMPL] A GrADS-style file name template. In its simplest
     form is the full path name for the file to be read. However, it
     can contain the following tokens which will be expanded from
     the current time in {\em TIME}:
   \bd
        \item[\%y4] 4 digits for year
        \item[\%m2] 2 digits for month, to expand to 01, 02, .., 12
        \item[\%m3] 3 digits for month, to expand to jan, feb, mar, ..., dec
        \item[\%d2] 2 digits for day
        \item[\%h2] 2 digits for hour
        \item[\%n2] 2 digits for minutes
   \ed
    Example: if FILETMPL = ``forecast.\%y4-\%m2-\%d2\_%h2z.nc4'', and the clock
    says it is 18Z on 05 February 2007, the template will expand in the
    following file name: ``forecast.2007-02-05\_18Z.nc4''
%
    \item[TIME] The ESMF time to read from the file
%
    \item[BUNDLE] An ESMF Bundle to read the data in. When the Bundle is empty
    one field is added for each variable present in the input file, and the
    necessary memory allocated according to the ESMF grid present in the Bundle.
%
    \item[{[NOREAD]}] If .TRUE., no data is actually read into the Bundle. This is
    useful to define a Bundle with the same variables as presented in the
    file, which in turn can be used to created a MAPL\_CFIO object for
    writing.
%
    \item[{[RC]}] Error return code; set to ESMF\_SUCCESS if all is well.    
%
    \item[{[VERBOSE]}] If .TRUE., prints progress messages to STDOUT; useful
     for debugging.
%
    \item[{[FORCE\_REGRID]}] Obsolete; kept for backward compatibility but
    has no effect.
%
    \item[{[TIME\_IS\_CYCLIC]}]  If .TRUE. it says that the input file is periodic
    in time. Useful for reading climatological files. For example, if the
    input file has 12 monthly means from January to December of 2001, setting
    this option to .TRUE. allows one to read this data for any other year. See 
    note below regarding issues with reading monthly mean data.
%
    \item[{[TIME\_INTERP]}] If .TRUE., the input file does not have to coincide with the
    actual times on file. In such cases, the data for the bracketing times are
    read and the data is properly interpolated in time. The input time, though,
    need to be within the range of times present on file 
   (unless {\tt TIME\_IS\_CYCLIC} is specified).
%
    \item[{[ONLY\_VARS]}] A list of comma separated vafriables to be read from the
    file. By default, all variables are read from the file. This option allows
    one to read a subset of vafriables. Example: ONLY\_VARS=``u,v,ps''.
%
\ed

    {\bf A note about storing monthly climatological data.} As per the CF
    conventions, month is not a well defined unit of time, as the time
    step is not constant throughout the year. When storing 12 months
    of climatological data one way around it is to use an average
    number of hours: use 732 or 730 hours depending on whether the year
    recorded in the file is a leap-year or not.

     !DESIGN ISSUES:

     The input argument {\tt TIME} should be replaced with {\tt CLOCK}
     for consistency with the rest of the API. One should also provide
     an interface involving the MAPL CFIO object.

#endif

!EOP
!--------------------------------------------------------------------------------
    character(len=*), parameter  :: Iam="MAPL_CFIOReadBundle"
    integer                      :: STATUS

! Locals


    type(ESMF_CFIO), pointer     :: CFIO
    type(ESMF_CFIOGrid), pointer :: CFIOGRID
    type(ESMF_GRID)              :: ESMFGRID
    type(ESMF_FIELD)             :: FIELD
    type(ESMF_ARRAY)             :: ARRAY
    type(ESMF_VM)                :: VM

    type(ESMF_CFIOVarInfo), pointer :: VARS(:)

    integer                      :: IM,  JM,  LM, ISTAR, JSTAR
    integer                      :: IM0, JM0
    integer                      :: L1, L, K
    integer                      :: NumVars, nVars
    integer                      :: counts(5)
    integer                      :: dims(3)
    integer                      :: arrayRank

    logical                      :: IamRoot, twoD
    logical                      :: amOnFirstNode

    real, pointer                ::  PTR2      (:,:),  PTR3      (:,:,:)
    real, pointer                :: GPTR2bundle(:,:), GPTR3bundle(:,:,:)
    real, pointer                :: GPTR2file  (:,:), GPTR3file  (:,:,:)

    character(len=ESMF_MAXSTR)   :: DATE
    character(len=ESMF_MAXSTR)   :: BundleVARNAME
    character(len=ESMF_MAXSTR)   :: CFIOVARNAME
    character(len=ESMF_MAXSTR)   :: LONG_NAME
    character(len=ESMF_MAXSTR)   :: UNITS

    real, pointer :: LONSfile(:),   LATSfile(:)
    real, pointer :: LONSbundle(:) => NULL()
    real, pointer :: LATSbundle(:) => NULL()

    !(stassi,14feb2012)--character(len=ESMF_MAXSTR) :: FILENAME
    !character(len=256) :: FILENAME
    character(len=1024) :: FILENAME
    integer :: nymd, nhms
    logical :: timeInterp=.false., VERB = .false., change_resolution, do_xshift, single_point, fcubed
    integer, allocatable    :: gridToFieldMap(:)
    integer                 :: gridRank
    integer                 :: comm
    logical                 :: found
    character(len=ESMF_MAXSTR) :: gridname
    logical :: RegridCnv
    logical :: Voting_, ldoFrac
    logical :: runParallel
    logical :: ignoreCase_
    integer, pointer        :: Krank(:) => null()
    logical                 :: myGlobal
    integer                 :: nn, CoresPerNode, myPet, nPet, numNodes
    logical :: selectedLevels
    real, pointer :: levsfile(:) => null()
    integer :: LM_FILE
    integer :: LL,klev
    integer, allocatable :: LEVIDX(:)
    type(ESMF_CFIOGrid)  :: varsGrid
    real, parameter      :: eps = 1.0e-4 ! tolerance to find "selected" levels
    logical :: kreverse
    integer :: i1w,inw,j1w,jnw
    logical :: gsiMode_
    integer :: hw
    integer :: halowidth(3)
    class (AbstractRegridder), pointer :: regridder
    integer :: method
    type(CFIOCollection), pointer :: collection => null()
    logical :: time_is_cyclic_
    logical :: cfioIsCreated

!                              ---
    _UNUSED_DUMMY(FORCE_REGRID)

    if ( present(VERBOSE) )     VERB = VERBOSE
    if ( present(TIME_INTERP) ) timeInterp = TIME_INTERP
    if (present(time_is_cyclic)) then
       time_is_cyclic_ = time_is_cyclic
    else
       time_is_cyclic_ = .false.
    end if
    if (present(conservative) ) then
       RegridCnv = conservative
    else
       RegridCnv = .false.
    end if
    if ( present(Voting) ) then
         Voting_ = Voting
    else
         Voting_ = .false.
    endif
    if ( present(getFrac) ) then
         ldoFrac = .true.
    else
         ldoFrac = .false.
    endif
    if ( present(ignoreCase) ) then
         ignoreCase_ = ignoreCase
    else
         ignoreCase_ = .false.
    end if

    if ( present(doParallel) ) then
       runParallel = doParallel
    else
       runParallel = .true.
    end if
    if (present(ONLY_LEVS)) then
       selectedLevels = .true.
    else
       selectedLevels = .false.
    end if
    if (present(GSImode)) then
       GSImode_=GSImode
    else
       GSImode_=.false.
    end if
    if (gsiMode_) then
       hw = 1
    else
       hw = 0
    end if
    haloWidth = (/HW,HW,0/)

    ! by default kreverse is false
    kreverse = .false.

! Transform ESMF time to string for use in CFIO
!----------------------------------------------
    call ESMF_TimeGet(TIME, timeString=DATE, RC=STATUS)
    _VERIFY(STATUS)

    call strToInt(DATE, nymd, nhms)
    call fill_grads_template ( filename, filetmpl, &
                           experiment_id=EXPID, nymd=nymd, nhms=nhms, rc=status )
    _VERIFY(STATUS)
    !call WRITE_PARALLEL("CFIO: Reading " // trim(filename))
    if (mapl_am_i_root()) write(*,*)"CFIO: Reading ",trim(filename)," at ",nymd," ",nhms

  
    cfioIsCreated = .false. 
    if (present(collection_id)) then
       collection => collections%at(collection_id)
       cfio => collection%find(filename) 
    else
       allocate(CFIO)
       cfio=ESMF_CFIOCreate(RC=status)
       _VERIFY(status)
       cfioIsCreated = .true.
       call ESMF_CFIOSet(CFIO, fName=trim(fileName), RC=STATUS)
       _VERIFY(STATUS)
       call ESMF_CFIOFileOpen  (CFIO, FMODE=1, cyclic=TIME_IS_CYCLIC_, RC=STATUS)
       _VERIFY(STATUS)
    end if
       
    

! Get info from the bundle
!-------------------------
    call ESMF_VMGetCurrent(VM, RC=STATUS)
    _VERIFY(STATUS)

    call ESMF_FieldBundleGet     (Bundle, FieldCount=NUMVARS, RC=STATUS)
    _VERIFY(STATUS)

    IamRoot = MAPL_AM_I_ROOT(VM)
    call ESMF_VMGet(VM, mpiCommunicator=comm, localPet=myPET, PETcount=nPet, rc=status)
    _VERIFY(STATUS)
    amOnFirstNode = MAPL_ShmemAmOnFirstNode(comm=comm, RC=status)
    _VERIFY(STATUS)
    CoresPerNode = MAPL_CoresPerNodeGet(comm,rc=status)
    _VERIFY(STATUS)

    ! Get lat/lons of input bundle
    ! ----------------------------
    call ESMF_FieldBundleGet     (Bundle,   Grid=ESMFGRID, RC=STATUS)
    _VERIFY(STATUS)
    call GridGetLatLons_ ( ESMFGRID, LONSbundle, LATSbundle, rc=status )
    _VERIFY(STATUS)
! Get info from the CFIO object
!------------------------------
    call ESMF_CFIOGet       (CFIO,     grid=CFIOGRID,                     RC=STATUS)
    _VERIFY(STATUS)
    call ESMF_CFIOGridGet   (CFIOGRID, IM=IM, JM=JM, KM=LM,               RC=STATUS)
    _VERIFY(STATUS)
    if(selectedLevels) then
       LM_FILE = LM
       LM = size(ONLY_LEVS)
       _ASSERT(LM <= LM_FILE, 'LM > LM_FILE')
    end if

    call ESMF_CFIOGridGet    (CFIOGRID, LON=LONSFILE, LAT=LATSFILE, RC=STATUS)
    _VERIFY(STATUS)
    deallocate(CFIOGRID)

    call ESMF_CFIOGet (CFIO,varObjs=VARS, nVars=nVars, RC=STATUS)
    _VERIFY(STATUS)


! If the bundle is empty, read entire varlist from file
!------------------------------------------------------

    if(NUMVARS==0) then

       call MAPL_GridGet(ESMFGRID, globalCellCountPerDim=COUNTS, &
            localCellCountPerDim=DIMS, RC=STATUS)
       _VERIFY(STATUS)
       call ESMF_GridGet(ESMFGRID, name=gridname, rc=rc)
       _VERIFY(STATUS)

       ! Assert compatibility of file and bundle
       !----------------------------------------
       _ASSERT( LM==0 .or. counts(3) == 0 .or. LM==counts(3) .or. LM==(counts(3)+1), 'file and bundle are incompatible')

       ! Get lat/lons of input bundle
       ! ----------------------------

       NUMVARS = nVars

       L1 = 0
       do L=1,NUMVARS

          call ESMF_CFIOVarInfoGet(VARS(L),vname=CFIOVARNAME, vtitle=LONG_NAME, vunits=UNITS, twoDimVar=twoD, &
               & grid=varsGrid, RC=STATUS)   
          _VERIFY(STATUS)

          if ( present(ONLY_VARS) ) then
               if ( index(','//trim(ONLY_VARS)  //',', &
                          ','//trim(CFIOVARNAME)//',') < 1 ) cycle 
          endif
          if (trim(CFIOVARNAME)=="lons" .or. trim(CFIOVARNAME)=="lats") cycle

          L1 = L1 + 1

          BundleVarName = CFIOVARNAME
          if(twoD) then
            allocate(PTR2(1-HW:DIMS(1)+HW,1-HW:DIMS(2)+HW),stat=STATUS)
            _VERIFY(STATUS)
            PTR2  = 0.0

            call ESMF_GridGet(ESMFGRID, dimCount=gridRank, rc=status)
            _VERIFY(STATUS)
            allocate(gridToFieldMap(gridRank), stat=status)
            _VERIFY(STATUS)
            if(gridRank == 2) then
               gridToFieldMap(1) = 1
               gridToFieldMap(2) = 2
            else if (gridRank == 3) then
               gridToFieldMap(1) = 1
               gridToFieldMap(2) = 2
               gridToFieldMap(3) = 0
            else
               _RETURN(ESMF_FAILURE)
            end if

            FIELD = ESMF_FieldCreate(grid=ESMFGRID, &
                            datacopyFlag = ESMF_DATACOPY_REFERENCE,   &
                            farrayPtr=PTR2, gridToFieldMap=gridToFieldMap, &
                            name=BundleVARNAME, &
                            totalLWidth=haloWidth(1:2),     &
                            totalUWidth=haloWidth(1:2),     &
                            rc = status)

            _VERIFY(STATUS)

            deallocate(gridToFieldMap)

!ALT: for now we add only HorzOnly (no tiles)
            call ESMF_AttributeSet(FIELD, NAME='LONG_NAME', VALUE=LONG_NAME, RC=STATUS)
            _VERIFY(STATUS)
            call ESMF_AttributeSet(FIELD, NAME='UNITS', VALUE=UNITS, RC=STATUS)
            _VERIFY(STATUS)
            call ESMF_AttributeSet(FIELD, NAME='DIMS', VALUE=MAPL_DimsHorzOnly, RC=STATUS)
            _VERIFY(STATUS)
            call ESMF_AttributeSet(FIELD, NAME='VLOCATION', &
                                        VALUE=MAPL_VLocationNone, RC=STATUS)
            _VERIFY(STATUS) 

          else
            ! 3-d case
             call ESMF_CFIOGridGet (varsGrid, lev=levsfile, rc=status)
             _VERIFY(STATUS) 
             if (levsfile(1) > levsfile(lm)) kreverse = .true.

             if (selectedLevels) then
                if (.not. allocated(levidx)) then
                   allocate(levidx(LM), stat=status)
                   _VERIFY(STATUS) 
                   ! build level index
                   DO K = 1, LM
                      found = .false.
                      DO LL = 1, LM_FILE
                         if (abs(LEVSFILE(LL) - ONLY_LEVS(K)) < eps) then
                            LEVIDX(K) = LL
                            found = .true.
                            exit
                         end if
                      END DO
                      _ASSERT(found, 'search failed')
                   END DO

                end if
             end if
            deallocate(levsfile)
            nullify(levsfile)

            if (lm == counts(3)) then 
               allocate(PTR3(1-HW:DIMS(1)+HW,1-HW:DIMS(2)+HW,LM),stat=STATUS)
               _VERIFY(STATUS)
            else if (lm == (counts(3)+1)) then
               allocate(PTR3(1-HW:DIMS(1)+HW,1-HW:DIMS(2)+HW,0:LM-1),stat=STATUS)
               _VERIFY(STATUS)
            endif
            PTR3  = 0.0
            FIELD = ESMF_FieldCreate(grid=ESMFGRID, &
                            datacopyFlag = ESMF_DATACOPY_REFERENCE,   &
                            farrayPtr=PTR3, name=BundleVARNAME,       & 
                            totalLWidth=haloWidth(1:2),     &
                            totalUWidth=haloWidth(1:2),     &
                            rc = status)
            _VERIFY(STATUS)
!ALT: for now we add only HorzVert (no tiles)
            call ESMF_AttributeSet(FIELD, NAME='LONG_NAME', VALUE=LONG_NAME, RC=STATUS)
            _VERIFY(STATUS)
            call ESMF_AttributeSet(FIELD, NAME='UNITS', VALUE=UNITS, RC=STATUS)
            _VERIFY(STATUS)
            call ESMF_AttributeSet(FIELD, NAME='DIMS', VALUE=MAPL_DimsHorzVert, RC=STATUS)
            _VERIFY(STATUS)
            if (lm == counts(3)) then
               call ESMF_AttributeSet(FIELD, NAME='VLOCATION', &
                                           VALUE=MAPL_VLocationCenter, RC=STATUS)
            else if (lm == (counts(3)+1)) then
               call ESMF_AttributeSet(FIELD, NAME='VLOCATION', &
                                           VALUE=MAPL_VLocationEdge, RC=STATUS)
            end if

            _VERIFY(STATUS)
          end if
          call MAPL_FieldBundleAdd(BUNDLE, FIELD, RC=STATUS)
          _VERIFY(STATUS)

       end do
       NUMVARS = L1  ! could be less than on file if user chooses to

    else
       
       do L=1,NumVars
          call ESMF_FieldBundleGet (BUNDLE, L, FIELD,                     RC=STATUS)
          _VERIFY(STATUS)
          call ESMF_FieldGet(FIELD,NAME=BundleVarName, RC=STATUS)
          _VERIFY(STATUS)
          call MAPL_GridGet(ESMFGRID, globalCellCountPerDim=COUNTS, RC=STATUS)
          _VERIFY(STATUS)
          call ESMF_GridGet(ESMFGRID, name=gridname, rc=rc)
          _VERIFY(STATUS)
          ! Assert compatibility of file and bundle
          !----------------------------------------
          _ASSERT( LM==0 .or. counts(3) == 0 .or. LM==counts(3) .or. lm == (counts(3)+1), 'incompatible file and bundle')

          found = .false.
          do K=1,size(VARS)
             call ESMF_CFIOVarInfoGet(VARS(K),vname=CFIOVARNAME,          RC=STATUS)
             _VERIFY(STATUS)
             if (ignoreCase_) then
                BUNDLEVARNAME = ESMF_UtilStringUpperCase(BUNDLEVARNAME,RC=status)
                _VERIFY(STATUS)
                CFIOVARNAME   = ESMF_UtilStringUpperCase(CFIOVARNAME,RC=status)
                _VERIFY(STATUS)
             end if
             if(trim(BUNDLEVARNAME)==trim(CFIOVARNAME)) then
               found = .true.
               exit
             end if
          end do
          _ASSERT(found, 'search failed')
          call ESMF_CFIOVarInfoGet(VARS(K), twoDimVar=twoD, grid=varsGrid, RC=STATUS)   
          _VERIFY(STATUS)
          if (.not. twoD) then
             call ESMF_CFIOGridGet (varsGrid, lev=levsfile, rc=status)
             _VERIFY(STATUS) 
             if (levsfile(1) > levsfile(lm)) kreverse = .true.
          end if
          if (selectedLevels) then
             if (.not. twoD) then
                ! 3-d case
                if (.not. allocated(levidx)) then
                   allocate(levidx(LM), stat=status)
                   _VERIFY(STATUS) 
                   ! build level index
                   DO K = 1, LM
                      found = .false.
                      DO LL = 1, LM_FILE
                         if (abs(LEVSFILE(LL) - ONLY_LEVS(K)) < eps) then
                            LEVIDX(K) = LL
                            found = .true.
                            exit
                         end if
                      END DO
                      _ASSERT(found, 'search failed')
                   END DO

                end if
             end if
          end if
          if (associated(levsfile)) then
             deallocate(levsfile)
             nullify(levsfile)
          end if
       end do
    end if

    if(present(NOREAD)) then
       if(NOREAD) goto 10
    end if

!   Do we have to run a transform?
!   ------------------------------
    IM0 = counts(1)
    JM0 = counts(2)

    if (IM /= IM0 .or. JM /= JM0)  then
        change_resolution = .true.
    else                              
        change_resolution = .false.
    end if

! 180 Degree Shifting and Cubed Sphere
! ------------------------------------
!   
!   In the earlier revisions of this subroutine there was an implicit assumption
!   of the input data being on the lat-lon grid. Since there were two
!   possibilities: Longitudinal origin at dateline, or at the Greewitch meridian,
!   the code used to perform Longitudinal shifting, if needed, so that the 
!   output is "properly" oriented at dateline center. 
!
!   Out current strategy is to correct the input (from the file), if needed.
!   We first check if the input is on the Cubed-Sphere grid. 
!   In this case no shifting is done. Otherwise we still assume that the
!   input is on a lat-lon grid and if shifting is needed,
!   it will be done prior to the optional MAPL_HorzTransformRun regridding.


    if ( JM == 6*IM )  then
        fcubed = .true.
    else                              
        fcubed = .false.
    end if

    do_xshift = .FALSE. ! Initialize: do not shift

    if ( IM0==1 .AND. JM0==1 ) then
       single_point = .TRUE.       ! running SCM with single point grid
       change_resolution = .FALSE. ! does not make sense in SCM mode
    else
       single_point = .FALSE. ! Normal case, not SCM
       ! never shift if cubed
       if (.not.fcubed) do_xshift = abs(LONSfile(1)+180._REAL64) .GT. abs(LONSfile(2)-LONSfile(1))
    end if

    if (change_resolution .and. RegridCnv) then

       runParallel = .false. ! override input, conservative regridding now done distributed

       if (Voting_) then
         method = REGRID_METHOD_VOTE
       elseif (ldofrac) then
         method = REGRID_METHOD_FRACTION
       else
         method = REGRID_METHOD_CONSERVE
       end if
       regridder => make_regridder(ESMFGRID, method, LONSfile, LATSfile, IM, JM, counts(3), runparallel, localTiles=.true., rc=status)
       _VERIFY(status)


    else if ( change_resolution .and. (.not.RegridCnv)) then
       method = REGRID_METHOD_BILINEAR
       regridder => make_regridder(ESMFGRID, method, LONSfile, LATSfile, IM, JM, counts(3), runparallel, rc=status)
       _VERIFY(status)
    end if
    call MAPL_SyncSharedMemory(rc=status)
    _VERIFY(STATUS)

! Allocate space for global arrays. If non-conservative perform
! parallel transform distributed across levels
! If conservative do not parallelize over levels this is done
! distributed already so this parallel strategy will not work
!------------------------------------------------------------

    if (RegridCnv .and. change_resolution) then

       call MAPL_GRID_INTERIOR(ESMFGRID,I1w,INw,J1w,JNw)
       if(MAPL_ShmInitialized) then
          call MAPL_AllocNodeArray(Gptr2file,(/im,jm/),rc=STATUS)
          _VERIFY(STATUS)
          call MAPL_AllocNodeArray(Gptr3file,(/im,jm,1/),rc=STATUS)
          _VERIFY(STATUS)
       else
          allocate(Gptr2file(im,jm),stat=status)
          _VERIFY(STATUS)
          allocate(Gptr3file(im,jm,1),stat=status)
          _VERIFY(STATUS)
       endif
       Allocate(Gptr2bundle(inw-i1w+1,jnw-j1w+1),stat=STATUS)
       _VERIFY(STATUS)
       allocate(Gptr3bundle(0,0,0), stat=STATUS)
       _VERIFY(STATUS)
       if (LM > 0) then
          allocate(krank(LM),stat=status)
       else
          allocate(krank(1) ,stat=status)
       end if
       krank = 0
   
    else

       IM0 = counts(1)
       JM0 = counts(2)

       CoresPerNode = MAPL_CoresPerNodeGet(comm,rc=status)
       _VERIFY(STATUS)
       if (LM > 0) then
          allocate(krank(LM),stat=status)
       else
          allocate(krank(1) ,stat=status)
       end if

       _VERIFY(STATUS)
       if (runParallel .and. (LM > 0) ) then
          numNodes = size(MAPL_NodeRankList)
          call MAPL_RoundRobinPEList(krank,numNodes,rc=status)
          _VERIFY(STATUS)
       else
          krank = 0
       end if
       nn=count(krank==myPet)

       if (nn > 0) then

          allocate(Gptr2bundle(IM0,JM0   ), stat=STATUS)
          _VERIFY(STATUS)
          allocate(Gptr3bundle(IM0,JM0,nn), stat=STATUS)
          _VERIFY(STATUS)
          allocate(Gptr2file  (IM ,JM    ), stat=STATUS)
          _VERIFY(STATUS)
          allocate(Gptr3file  (IM ,JM , 1), stat=STATUS)
          _VERIFY(STATUS)

       else

          allocate(Gptr2bundle(0,0   ), stat=STATUS)
          _VERIFY(STATUS)
          allocate(Gptr3bundle(0,0,0), stat=STATUS)
          _VERIFY(STATUS)
          allocate(Gptr2file  (0,0    ), stat=STATUS)
          _VERIFY(STATUS)
          allocate(Gptr3file  (0,0,0), stat=STATUS)
          _VERIFY(STATUS)

       end if

    end if

!   Special handling for single column case
!   Pick out index into file grid for lats and lons of scm grid - 
!   Assume that scm grid counts lon from -180 to 180 and lat from -90 to 90
    if(single_point) then
      if(LONSfile(1).lt.0.) then        !  assume lons on file go from -180 to 180
       ISTAR = 1 + (LONSbundle(1)+180.)/( 360./ IM )
      else                              !  lons on file go from 0 to 360
       if(LONSbundle(1).lt.0.) then
       ISTAR = 1 + (LONSbundle(1)+360.)/( 360./IM )
       else
       ISTAR = 1 + LONSbundle(1)/( 360./IM )
       endif
      endif
!  assume lats on file go from -90 to 90
       JSTAR = 1 + (LATSbundle(1)+90.)/( 180. / (JM-1) )
    endif

! Read each variable
!-------------------
    do L=1,NumVars

       call ESMF_FieldBundleGet (BUNDLE, L, FIELD,                       RC=STATUS)
       _VERIFY(STATUS)
       call ESMF_FieldGet       (FIELD, NAME=BundleVarName, array=ARRAY, RC=STATUS)
       _VERIFY(STATUS)
       
       if (ignoreCase_) call getVarNameIgnoreCase(BundleVarName,vars,RC=status)

       call ESMF_FieldGet(FIELD,   Grid=ESMFGRID, RC=STATUS)
       _VERIFY(STATUS)
       call ESMF_ArrayGet       (array, rank=arrayRank,                  RC=STATUS)
 
       _VERIFY(STATUS)

       if ( VERB .and. IamRoot ) &
            print *, Iam // ': Reading '//trim(BundleVARNAME)// &
                            ' at ' // trim(date)
       select case (arrayRank)

       case (2)

          call ESMF_ArrayGet(Array, localDE=0, farrayPtr=PTR2, RC=STATUS)
          _VERIFY(STATUS)
          
          ! read the data on root
          if (IamRoot) then
             if ( timeInterp ) then
                call ESMF_CFIOVarReadT(CFIO, trim(BundleVARNAME), GPTR2file, &
                                       timeString=DATE, RC=STATUS)
                if (ldofrac) GPTR2file=GPTR2file-getfrac
             else
                call ESMF_CFIOVarRead (CFIO, trim(BundleVARNAME), GPTR2file, &
                                       timeString=DATE, RC=STATUS)
                if (ldofrac) GPTR2file=GPTR2file-getfrac
             endif
             _VERIFY(STATUS)
             if ( do_xshift ) then
                 if ( VERB ) print *, Iam // &
                      ': shifting input longitudes by 180 degrees'
                 call shift180Lon2D_ ( Gptr2file, im, jm )
             end if
          end if
 
          ! transform and scatter
          if (change_resolution) then  
              if (RegridCnv) then 
                 call MAPL_SyncSharedMemory(rc=status)
                 _VERIFY(STATUS)
                 call MAPL_BcastShared(VM, Data=Gptr2file, N=im*jm, Root=0, RootOnly=.false., rc=status)
                 _VERIFY(STATUS)
                 call MAPL_SyncSharedMemory(rc=STATUS)
                 _VERIFY(STATUS)
                 call regridder%regrid(Gptr2file, Gptr2bundle, rc=status)
                 _VERIFY(status)
                 call MAPL_SyncSharedMemory(rc=status)
                 _VERIFY(STATUS)
                 ptr2 = Gptr2bundle
                 call MAPL_SyncSharedMemory(rc=STATUS)
                 _VERIFY(STATUS)
              else

                 if (IamRoot) then
                    call regridder%set_undef_value(MAPL_undef)
                    call regridder%regrid(Gptr2file, Gptr2bundle, rc=status)
                    _VERIFY(status)
                    if (GSImode_) call shift180Lon2D_ ( Gptr2bundle, im0, jm0 )
                 end if
                 call ArrayScatter(PTR2, GPTR2bundle, ESMFGRID, hw=hw, RC=STATUS)
                 _VERIFY(STATUS)
              end if
          else if ( single_point ) then
             Gptr2bundle(1,1) = Gptr2file(ISTAR,JSTAR)
             ptr2(1,1) = GPTR2bundle(1,1) ! single point SCM case
          else
             if (IamRoot) Gptr2bundle = Gptr2file
             call ArrayScatter(PTR2, GPTR2bundle, ESMFGRID, hw=hw, RC=STATUS)
             _VERIFY(STATUS)
          end if ! change resolution

       case(3)

          nn = 0


          call ESMF_FieldGet(Field, localDE=0, farrayPtr=PTR3, RC=STATUS)
          _VERIFY(STATUS)

          nn=0

          do k = 1, LM

             MyGlobal = Krank(k) == myPet

             call MAPL_SyncSharedMemory(rc=status)
             _VERIFY(STATUS)
             if (MyGlobal) then
                nn=nn+1
                if (selectedLevels) then
                   _ASSERT(allocated(levidx), 'levidx not allocated')
                   klev = levidx(k)
                else
                   klev = k
                end if
                if (kreverse) klev = lm - k + 1
                if ( timeInterp ) then
                   call ESMF_CFIOVarReadT(CFIO, trim(BundleVARNAME), GPTR3file, &
                        kbeg=klev, kount=1, timeString=DATE, RC=STATUS)
                        if (ldofrac) GPTR3file=GPTR3file-getfrac
                else
                   call ESMF_CFIOVarRead (CFIO, trim(BundleVARNAME), GPTR3file, &
                        kbeg=klev, kount=1, timeString=DATE, RC=STATUS)
                        if (ldofrac) GPTR3file=GPTR3file-getfrac
                end if
                _VERIFY(STATUS)
                GPTR2file = GPTR3file(:,:,1)
                if ( do_xshift ) then
                   call shift180Lon2D_ ( Gptr2file, im, jm )
                end if
             end if

             if (change_resolution) then 
                if (RegridCnv) then
                   call MAPL_SyncSharedMemory(rc=status)
                   _VERIFY(STATUS)
                   call MAPL_BcastShared(VM, Data=Gptr2file, N=im*jm, Root=0, RootOnly=.false., rc=status)
                   _VERIFY(STATUS)
                   call MAPL_SyncSharedMemory(rc=STATUS)
                   _VERIFY(STATUS)

                   call regridder%regrid(Gptr2file, Gptr2bundle, rc=status)
                   _VERIFY(status)

                   call MAPL_SyncSharedMemory(rc=status)
                   _VERIFY(STATUS)
                   L1 = LBOUND(PTR3,3)-1
                   ptr3(:,:,K+L1) = Gptr2bundle
                   call MAPL_SyncSharedMemory(rc=STATUS)
                   _VERIFY(STATUS) 

                 else
                    if (MyGlobal) then
                       call regridder%set_undef_value(MAPL_undef)
                       call regridder%regrid(Gptr2file, Gptr2bundle, rc=status)
                       _VERIFY(status)
                       if (GSImode_) call shift180Lon2D_ ( Gptr2bundle, im0, jm0 )
                       Gptr3bundle(:,:,nn)=Gptr2bundle
                    end if
                end if
             else if ( single_point ) then
                  Gptr3bundle(:,:,nn) = Gptr2file(ISTAR,JSTAR)
             else
                if (MyGlobal) Gptr3bundle(:,:,nn)=Gptr2file
             end if

          end do

          if (single_point) then
             ptr3(1,1,:) = Gptr3bundle(1,1,:)
          else
             if ( (.not.RegridCnv) .and. runParallel) then
                call MAPL_CollectiveScatter3D(esmfgrid,Gptr3bundle(:,:,:nn),ptr3,hw=hw,rc=status)
                _VERIFY(STATUS)
             else if ( (.not.RegridCnv) .and. (.not.RunParallel) ) then
                do K=1,LM
                   L1 = LBOUND(PTR3,3)-1
                   call ArrayScatter(PTR3(:,:,K+L1), Gptr3bundle(:,:,K), ESMFGRID, hw=hw, RC=STATUS)
                   _VERIFY(STATUS)
                end do
             end if
             if (GSImode_) call SwapV_(PTR3)
          end if

       end select

    end do
    deallocate(krank)

    deallocate(GPtr2bundle)
    deallocate(GPtr3bundle)

    if (RegridCnv .and. change_resolution) then
       ! make sure everyone is done before potentially releasing shared memory
       call MAPL_SyncSharedMemory(rc=status)
       _VERIFY(STATUS)
       DEALOC_(GPtr2file  )
       DEALOC_(GPtr3file  )
    else
       deallocate(Gptr2file)
       deallocate(Gptr3file)
    end if

10  continue 
! always do this cleanup

    deallocate(LONSfile,LATSfile)
    deallocate(LONSbundle,LATSbundle)
!@    call ESMF_CFIOVarInfoDestroy(vars, __RC__)
    deallocate(VARS)

    if (selectedLevels) then
       if (allocated(levidx)) then
          deallocate(levidx)
       end if
    end if

    if (cfioIsCreated) then
       call ESMF_CFIODestroy(CFIO, rc=status)
       _VERIFY(STATUS)
    end if

    _RETURN(ESMF_SUCCESS)

CONTAINS

    subroutine shift180Lon2D_ ( c, im, jm )
    integer, intent(in) :: im, jm
    real, intent(inout) :: c(im,jm)
    real :: cj(im)
    integer :: m(4), n(4), imh, j
    imh = nint(im/2.)
    m = [ 1,      imh, 1+imh,    im  ]
    n = [ 1,   im-imh, 1+im-imh, im  ]
    do j = 1, jm
       cj(n(1):n(2)) = c(m(3):m(4),j)
       cj(n(3):n(4)) = c(m(1):m(2),j)
       c(:,j) = cj
    end do
    return
    end subroutine shift180Lon2D_

    subroutine SwapV_(fld)
    implicit none
    real,intent(inout) ::  fld(:,:,:)
    real,allocatable   :: work(:,:,:)
    integer im, jm, km
    im   = size(fld,1)
    jm   = size(fld,2)
    km   = size(fld,3)
    allocate (work(im,jm,km))
    work = fld
    fld(:,:,km:1:-1) = work(:,:,1:km:+1)
    deallocate (work)
    end subroutine SwapV_

    subroutine getVarNameIgnoreCase(vname,vars,rc)
    character(len=*), intent(inout)  :: vname
    type(ESMF_CFIOVarInfo), pointer, intent(in) :: vars(:)
    integer, optional, intent(out)  :: rc

    integer :: status
    integer j
    character(len=ESMF_MAXSTR) :: cfiovarname,tname,tcfioname

    tname = vname
    tname = ESMF_UtilStringUpperCase(tname,rc=STATUS)
    _VERIFY(STATUS)
    do j=1,size(vars)
        call ESMF_CFIOVarInfoGet(vars(j),vname=cfiovarname,RC=STATUS)
        _VERIFY(STATUS)
        tcfioname = cfiovarname
        tcfioname = ESMF_UtilStringUpperCase(tcfioname,rc=STATUS)
        _VERIFY(STATUS)
        if (trim(tname) == trim(tcfioname)) then
           vname = cfiovarname
           exit
        end if
    enddo
    _RETURN(ESMF_SUCCESS)
    end subroutine getVarNameIgnoreCase

  end subroutine MAPL_CFIOReadBundle

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!BOP

! !IIROUTINE: MAPL_CFIOReadState --- Reads an ESMF State

! !INTERFACE:
!
  subroutine MAPL_CFIOReadState ( FILETMPL, TIME, STATE, NOREAD, RC, &
                                  VERBOSE, FORCE_REGRID, ONLY_VARS,  &
                                  TIME_IS_CYCLIC, TIME_INTERP,       &
                                  conservative, voting, ignoreCase, doParallel, getFrac )
!
! !ARGUMENTS:
!
    character(len=*),            intent(IN   ) :: FILETMPL
    type(ESMF_TIME),             intent(INout) :: TIME
    type(ESMF_STATE),            intent(INOUT) :: STATE
    logical, optional,           intent(IN   ) :: NOREAD
    integer, optional,           intent(  OUT) :: RC
    logical, optional,           intent(  IN)  :: VERBOSE
    logical, optional,           intent(IN)    :: FORCE_REGRID ! obsolete
    logical, optional,           intent(IN)    :: TIME_IS_CYCLIC
    logical, optional,           intent(IN)    :: TIME_INTERP
    logical, optional,           intent(IN)    :: conservative
    logical, optional,           intent(IN)    :: voting
    logical, optional,           intent(IN)    :: ignoreCase
    logical, optional,           intent(IN)    :: doParallel
    integer, optional,           intent(IN)    :: getFrac
    character(len=*), optional,  intent(IN   ) :: ONLY_VARS ! comma separated,
                                                            ! no spaces
!
#ifdef ___PROTEX___
!
    !DESCRIPTION: 

     Serializes an ESMF state into a Bundle and reads its content from
     a file. The file is open, read from, and closed on exit. The
     arguments are:
\bd
     \item[FILETMPL] A GrADS-style file name template. In its simplest
     form is the full path name for the file to be read. However, it
     can contain the following tokens which will be expanded from
     the current time in {\em TIME}:
   \bd
        \item[\%y4] 4 digits for year
        \item[\%m2] 2 digits for month, to expand to 01, 02, .., 12
        \item[\%m3] 3 digits for month, to expand to jan, feb, mar, ..., dec
        \item[\%d2] 2 digits for day
        \item[\%h2] 2 digits for hour
        \item[\%n2] 2 digits for minutes
   \ed
    Example: if FILETMPL = ``forecast.\%y4-\%m2-\%d2\_%h2z.nc4'', and the clock
    says it is 18Z on 05 February 2007, the template will expand in the
    following file name: ``forecast.2007-02-05\_18Z.nc4''
%
    \item[TIME] The ESMF time to read from the file
%
    \item[STATE] An ESMF State to read the data in. Usually used in conjubction
    with ONLY\_VARS.
%
    \item[{[NOREAD]}] If .TRUE., no data is actually read into the Bundle. This is
    useful to define a Bundle with the same variables as presented in the
    file, which in turn can be used to created a MAPL\_CFIO object for
    writing.
%
    \item[{[RC]}] Error return code; set to ESMF\_SUCCESS if all is well.    
%
    \item[{[VERBOSE]}] If .TRUE., prints progress messages to STDOUT; useful
     for debugging.
%
    \item[{[FORCE\_REGRID]}] Obsolete; kept for backward compatibility but
    has no effect.
%
    \item[{[TIME\_IS\_CYCLIC]}]  If .TRUE. it says that the input file is periodic
    in time. Useful for reading climatological files. For example, if the
    input file has 12 monthly means from January to December of 2001, setting
    this option to .TRUE. allows one to read this data for any other year. See 
    note below regarding issues with reading monthly mean data.
%
    \item[{[TIME\_INTERP]}] If .TRUE., the input file does not have to coincide with the
    actual times on file. In such cases, the data for the bracketing times are
    read and the data is properly interpolated in time. The input time, though,
    need to be within the range of times present on file 
   (unless {\tt TIME\_IS\_CYCLIC} is specified).
%
    \item[{[ONLY\_VARS]}] A list of comma separated vafriables to be read from the
    file. By default, all variables are read from the file. This option allows
    one to read a subset of vafriables. Example: ONLY\_VARS=``u,v,ps''.
%
\ed

     !DESIGN ISSUES:

     The input argument {\tt TIME} should be replaced with {\tt CLOCK}
     for consistency with the rest of the API. One should also provide
     an interface involving the MAPL CFIO object.

#endif

!EOP

    character(len=*), parameter  :: Iam="MAPL_CFIOReadState"
    integer                      :: STATUS

! Locals

    type(ESMF_FieldBundle) :: tBUNDLE

!                          ----

!   Create an empty bundle
!   ----------------------
    tBUNDLE = ESMF_FieldBundleCreate ( name=Iam, rc=STATUS )
    _VERIFY(STATUS)
    
!   Serialize the state
!   -------------------
    call ESMFL_BundleAddState ( tBUNDLE, STATE, rc=STATUS, VALIDATE=.true. )
    _VERIFY(STATUS)

!   Read the Bundle
!   ---------------
    call MAPL_CFIOReadBundle( FILETMPL, TIME, tBUNDLE,         &
                              NOREAD = NOREAD,                 &
                              VERBOSE = VERBOSE,               &
                              FORCE_REGRID=FORCE_REGRID,       &
                              ONLY_VARS = ONLY_VARS,           &
                              TIME_IS_CYCLIC = TIME_IS_CYCLIC, &
                              TIME_INTERP = TIME_INTERP,       &
                              conservative = conservative,     &
                              voting = voting,                 &
                              ignoreCase = ignoreCase,         &
                              doParallel = doParallel,         &
                              getFrac = getFrac,               &
                              RC = STATUS )

    _VERIFY(STATUS)

!   All done
!   --------
    call ESMF_FieldBundleDestroy ( tBUNDLE, rc=STATUS )
    _VERIFY(STATUS)

    _RETURN(ESMF_SUCCESS)

 end subroutine MAPL_CFIOReadState

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!BOP

! !IIROUTINE: MAPL_CFIOReadField --- Reads an ESMF Field

! !INTERFACE:
!
  subroutine MAPL_CFIOReadField     ( VARN, FILETMPL, TIME,       FIELD, RC, &
                                      VERBOSE, FORCE_REGRID, TIME_IS_CYCLIC, &
                                      TIME_INTERP,                           &
                                      conservative , voting, ignoreCase, doParallel, getFrac)
!
! !ARGUMENTS:
!
    character(len=*),            intent(IN   ) :: VARN       ! Variable name
    character(len=*),            intent(IN   ) :: FILETMPL   ! File name
    type(ESMF_TIME),             intent(INout) :: TIME
    type(ESMF_FIELD),            intent(INout) :: FIELD
    integer, optional,           intent(  OUT) :: RC
    logical, optional,           intent(  IN)  :: VERBOSE
    logical, optional,           intent(IN)    :: FORCE_REGRID
    logical, optional,           intent(IN)    :: TIME_IS_CYCLIC
    logical, optional,           intent(IN)    :: TIME_INTERP
    logical, optional,           intent(IN)    :: conservative
    logical, optional,           intent(IN)    :: voting
    logical, optional,           intent(IN)    :: ignoreCase
    logical, optional,           intent(IN)    :: doParallel
    integer, optional,           intent(IN)    :: getFrac
!
#ifdef ___PROTEX___

    !DESCRIPTION: 

     Reads a variable from a file and stores it on an ESMF Field.
     The file is open, read from, and closed on exit. The
     arguments are:
\bd
     \item[VARN] The variable name.
%
     \item[FILETMPL] A GrADS-style file name template. In its simplest
     form is the full path name for the file to be read. However, it
     can contain the following tokens which will be expanded from
     the current time in {\em TIME}:
   \bd
        \item[\%y4] 4 digits for year
        \item[\%m2] 2 digits for month, to expand to 01, 02, .., 12
        \item[\%m3] 3 digits for month, to expand to jan, feb, mar, ..., dec
        \item[\%d2] 2 digits for day
        \item[\%h2] 2 digits for hour
        \item[\%n2] 2 digits for minutes
   \ed
    Example: if FILETMPL = ``forecast.\%y4-\%m2-\%d2\_%h2z.nc4'', and the clock
    says it is 18Z on 05 February 2007, the template will expand in the
    following file name: ``forecast.2007-02-05\_18Z.nc4''
%
    \item[TIME] The ESMF time to read from the file
%
    \item[{[RC]}] Error return code; set to ESMF\_SUCCESS if all is well.    
%
    \item[{[VERBOSE]}] If .TRUE., prints progress messages to STDOUT; useful
     for debugging.
%
    \item[{[FORCE\_REGRID]}] Obsolete; kept for backward compatibility but
    has no effect.
%
    \item[{[TIME\_IS\_CYCLIC]}]  If .TRUE. it says that the input file is periodic
    in time. Useful for reading climatological files. For example, if the
    input file has 12 monthly means from January to December of 2001, setting
    this option to .TRUE. allows one to read this data for any other year. See 
    note below regarding issues with reading monthly mean data.
%
    \item[{[TIME\_INTERP]}] If .TRUE., the input file does not have to coincide with the
    actual times on file. In such cases, the data for the bracketing times are
    read and the data is properly interpolated in time. The input time, though,
    need to be within the range of times present on file 
   (unless {\tt TIME\_IS\_CYCLIC} is specified).
%
    \item[{[ONLY\_VARS]}] A list of comma separated vafriables to be read from the
    file. By default, all variables are read from the file. This option allows
    one to read a subset of vafriables. Example: ONLY\_VARS=``u,v,ps''.
%
\ed

     !DESIGN ISSUES:

     The input argument {\tt TIME} should be replaced with {\tt CLOCK}
     for consistency with the rest of the API. The input {\tt GRID} is not necessary
     as it can be found inside the field. One should also provide
     an interface involving the MAPL CFIO object.

#endif

!EOP

    character(len=*), parameter  :: Iam="MAPL_CFIOReadField"
    integer                      :: STATUS
    type(ESMF_GRID)              :: GRID

! Locals

    type(ESMF_FIELDBUNDLE)  :: BUNDLE
 
!   Create a temporary empty bundle
!   -------------------------------
    call ESMF_FieldGet(Field, grid=Grid, rc=status)
    _VERIFY(STATUS)
    BUNDLE =  ESMF_FieldBundleCreate ( name=Iam, rc=STATUS )
    _VERIFY(STATUS)
    call ESMF_FieldBundleSet ( bundle, grid=GRID, rc=STATUS )
    _VERIFY(STATUS)

!   Add the input field to the bundle
!   ---------------------------------
    call MAPL_FieldBundleAdd ( BUNDLE, FIELD, rc=STATUS )
    _VERIFY(STATUS)

!   Now, we read the variable into the bundle, which in turn will put
!    the data inside the input array
!   -----------------------------------------------------------------
    call MAPL_CFIOReadBundle( FILETMPL, TIME, BUNDLE,                    &
                              VERBOSE=VERBOSE,                           &
                              FORCE_REGRID=FORCE_REGRID,                 &
                              ONLY_VARS = trim(varn),                    &
                              TIME_IS_CYCLIC=TIME_IS_CYCLIC,             &
                              TIME_INTERP=TIME_INTERP,                   &
                              conservative=conservative,                 &
                              voting = voting, ignoreCase = ignoreCase,  &
                              doParallel = doParallel, getFrac=getFrac,  &
                              RC=STATUS)
    _VERIFY(STATUS)    


!   Destroy temporary bundle; field data will be preserved
!   ------------------------------------------------------
    call ESMF_FieldBundleDestroy ( BUNDLE, rc=STATUS )
    _VERIFY(STATUS)    

    _RETURN(ESMF_SUCCESS)

  end subroutine MAPL_CFIOReadField

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!BOP

! !IIROUTINE: MAPL_CFIOReadArray3D --- Reads a 3D Fortran Array

! !INTERFACE:
!
  subroutine MAPL_CFIOReadArray3D ( VARN, FILETMPL, TIME, GRID, farrayPtr, RC, &
                                    VERBOSE, FORCE_REGRID, TIME_IS_CYCLIC,     &
                                    TIME_INTERP, conservative, voting, ignoreCase, doParallel, getFrac )
!
! !ARGUMENTS:
!
    character(len=*),            intent(IN   ) :: VARN       ! Variable name
    character(len=*),            intent(IN   ) :: FILETMPL   ! File name
    type(ESMF_TIME),             intent(INout) :: TIME
    type(ESMF_GRID),             intent(IN   ) :: GRID
    real, pointer                              :: farrayPtr(:,:,:)
    integer, optional,           intent(  OUT) :: RC
    logical, optional,           intent(  IN)  :: VERBOSE
    logical, optional,           intent(IN)    :: FORCE_REGRID
    logical, optional,           intent(IN)    :: TIME_IS_CYCLIC
    logical, optional,           intent(IN)    :: TIME_INTERP
    logical, optional,           intent(IN)    :: conservative
    logical, optional,           intent(IN)    :: voting
    logical, optional,           intent(IN)    :: ignoreCase
    logical, optional,           intent(IN)    :: doParallel
    integer, optional,           intent(IN)    :: getFrac
!
#ifdef ___PROTEX___

    !DESCRIPTION: 

     Reads a variable from a file and stores it on an 3D Fortrran array.
     The file is open, read from, and closed on exit. The
     arguments are:
\bd
     \item[VARN] The variable name.
%
     \item[FILETMPL] A GrADS-style file name template. In its simplest
     form is the full path name for the file to be read. However, it
     can contain the following tokens which will be expanded from
     the current time in {\em TIME}:
   \bd
        \item[\%y4] 4 digits for year
        \item[\%m2] 2 digits for month, to expand to 01, 02, .., 12
        \item[\%m3] 3 digits for month, to expand to jan, feb, mar, ..., dec
        \item[\%d2] 2 digits for day
        \item[\%h2] 2 digits for hour
        \item[\%n2] 2 digits for minutes
   \ed
    Example: if FILETMPL = ``forecast.\%y4-\%m2-\%d2\_%h2z.nc4'', and the clock
    says it is 18Z on 05 February 2007, the template will expand in the
    following file name: ``forecast.2007-02-05\_18Z.nc4''
%
    \item[TIME] The ESMF time to read from the file
%
    \item[GRID] The ESMF grid associated with the Field. The data will be 
    (horizontally) interpolated to this grid if necessary.
%
    \item[{[RC]}] Error return code; set to ESMF\_SUCCESS if all is well.    
%
    \item[{[VERBOSE]}] If .TRUE., prints progress messages to STDOUT; useful
     for debugging.
%
    \item[{[FORCE\_REGRID]}] Obsolete; kept for backward compatibility but
    has no effect.
%
    \item[{[TIME\_IS\_CYCLIC]}]  If .TRUE. it says that the input file is periodic
    in time. Useful for reading climatological files. For example, if the
    input file has 12 monthly means from January to December of 2001, setting
    this option to .TRUE. allows one to read this data for any other year. See 
    note below regarding issues with reading monthly mean data.
%
    \item[{[TIME\_INTERP]}] If .TRUE., the input file does not have to coincide with the
    actual times on file. In such cases, the data for the bracketing times are
    read and the data is properly interpolated in time. The input time, though,
    need to be within the range of times present on file 
   (unless {\tt TIME\_IS\_CYCLIC} is specified).
%
    \item[{[ONLY\_VARS]}] A list of comma separated vafriables to be read from the
    file. By default, all variables are read from the file. This option allows
    one to read a subset of vafriables. Example: ONLY\_VARS=``u,v,ps''.
%
\ed

     !DESIGN ISSUES:

     The input argument {\tt TIME} should be replaced with {\tt CLOCK}
     for consistency with the rest of the API.  One should also
     provide an interface involving the MAPL CFIO object.

#endif

!EOP

    character(len=*), parameter  :: Iam="MAPL_CFIOReadArray3D"
    integer                      :: STATUS

    type(ESMF_Field)             :: FIELD

    real    :: const = 0.0
    integer :: ios, k

!                            ----

!   Special case: when filename is "/dev/null" it is assumed the user 
!   wants to set the variable to a constant
!   -----------------------------------------------------------------
    if ( FILETMPL(1:9) == '/dev/null' ) then    
         ios = -1
         k = index(FILETMPL,':')
         if ( k > 9 ) read(FILETMPL(k+1:),*,iostat=ios) const
         if ( ios /= 0 ) const = 0.0
         if ( MAPL_am_I_root() ) &
            print *, Iam // ': setting variable ' // trim(varn) // &
                            ' to constant = ', const
         _RETURN(ESMF_SUCCESS)
    end if

!   Create Field with input array
!   -----------------------------
    FIELD = ESMF_FieldCreate(grid=GRID,   &
            datacopyFlag = ESMF_DATACOPY_REFERENCE,   &
            farrayPtr=farrayPtr, name=trim(varn), RC=STATUS)
    _VERIFY(STATUS)

   
!   Read array data from file
!   -------------------------
    call MAPL_CFIOReadField ( VARN, FILETMPL, TIME,       FIELD,          &
                              VERBOSE=VERBOSE, FORCE_REGRID=FORCE_REGRID, &
                              TIME_IS_CYCLIC=TIME_IS_CYCLIC,              &
                              TIME_INTERP=TIME_INTERP,                    &
                              conservative=conservative,                  &
                              voting=voting, ignoreCase = ignoreCase,     &
                              doParallel = doParallel, getFrac=getFrac,   &
                              RC=STATUS)
    _VERIFY(STATUS)

!   Destroy the ESMF array (data will be preserved since we own it)
!   --------------------------------------------------------------
    call ESMF_FieldDestroy ( FIELD, RC=STATUS )
    _VERIFY(STATUS)

    _RETURN(ESMF_SUCCESS)

  end subroutine MAPL_CFIOReadArray3D

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!BOP

! !IIROUTINE: MAPL_CFIOReadArray2D --- Reads a 2D Fortran Array

! !INTERFACE:
!
  subroutine MAPL_CFIOReadArray2D ( VARN, FILETMPL, TIME, GRID, farrayPtr, RC, &
                                    VERBOSE, FORCE_REGRID, TIME_IS_CYCLIC,     &
                                    TIME_INTERP , conservative, voting, ignoreCase, doParallel, getFrac)
!
! !ARGUMENTS:
!
    character(len=*),            intent(IN)  :: VARN       ! Variable name
    character(len=*),            intent(IN)  :: FILETMPL   ! File name
    type(ESMF_TIME),             intent(INout)  :: TIME
    type(ESMF_GRID),             intent(IN)  :: GRID
    real, pointer                            :: farrayPtr(:,:)
    integer, optional,           intent(OUT) :: RC
    logical, optional,           intent(IN)  :: VERBOSE
    logical, optional,           intent(IN)  :: FORCE_REGRID
    logical, optional,           intent(IN)  :: TIME_IS_CYCLIC
    logical, optional,           intent(IN)  :: TIME_INTERP
    logical, optional,           intent(IN)    :: conservative
    logical, optional,           intent(IN)    :: voting
    logical, optional,           intent(IN)    :: ignoreCase
    logical, optional,           intent(IN)    :: doParallel
    integer, optional,           intent(IN)    :: getFrac
!
#ifdef ___PROTEX___

    !DESCRIPTION: 

     Reads a variable from a file and stores it on an 3D Fortrran array.
     The file is open, read from, and closed on exit. The
     arguments are:
\bd
     \item[VARN] The variable name.
%
     \item[FILETMPL] A GrADS-style file name template. In its simplest
     form is the full path name for the file to be read. However, it
     can contain the following tokens which will be expanded from
     the current time in {\em TIME}:
   \bd
        \item[\%y4] 4 digits for year
        \item[\%m2] 2 digits for month, to expand to 01, 02, .., 12
        \item[\%m3] 3 digits for month, to expand to jan, feb, mar, ..., dec
        \item[\%d2] 2 digits for day
        \item[\%h2] 2 digits for hour
        \item[\%n2] 2 digits for minutes
   \ed
    Example: if FILETMPL = ``forecast.\%y4-\%m2-\%d2\_%h2z.nc4'', and the clock
    says it is 18Z on 05 February 2007, the template will expand in the
    following file name: ``forecast.2007-02-05\_18Z.nc4''
%
    \item[TIME] The ESMF time to read from the file
%
    \item[GRID] The ESMF grid associated with the Field. The data will be 
    (horizontally) interpolated to this grid if necessary.
%
    \item[{[RC]}] Error return code; set to ESMF\_SUCCESS if all is well.    
%
    \item[{[VERBOSE]}] If .TRUE., prints progress messages to STDOUT; useful
     for debugging.
%
    \item[{[FORCE\_REGRID]}] Obsolete; kept for backward compatibility but
    has no effect.
%
    \item[{[TIME\_IS\_CYCLIC]}]  If .TRUE. it says that the input file is periodic
    in time. Useful for reading climatological files. For example, if the
    input file has 12 monthly means from January to December of 2001, setting
    this option to .TRUE. allows one to read this data for any other year. See 
    note below regarding issues with reading monthly mean data.
%
    \item[{[TIME\_INTERP]}] If .TRUE., the input file does not have to coincide with the
    actual times on file. In such cases, the data for the bracketing times are
    read and the data is properly interpolated in time. The input time, though,
    need to be within the range of times present on file 
   (unless {\tt TIME\_IS\_CYCLIC} is specified).
%
    \item[{[ONLY\_VARS]}] A list of comma separated vafriables to be read from the
    file. By default, all variables are read from the file. This option allows
    one to read a subset of vafriables. Example: ONLY\_VARS=``u,v,ps''.
%
\ed

     !DESIGN ISSUES:

     The input argument {\tt TIME} should be replaced with {\tt CLOCK}
     for consistency with the rest of the API.  One should also
     provide an interface involving the MAPL CFIO object.

#endif

!EOP

    character(len=*), parameter  :: Iam="MAPL_CFIOReadArray2D"
    integer                      :: STATUS

    type(ESMF_Field)             :: FIELD

    real    :: const = 0.0
    integer :: ios, k
    integer, allocatable    :: gridToFieldMap(:)
    integer                 :: gridRank

!                            ----


!   Special case: when filename is "/dev/null" it is assumed the user 
!   wants to set the variable to a constant
!   -----------------------------------------------------------------
    if ( FILETMPL(1:9) == '/dev/null' ) then    
         ios = -1
         k = index(FILETMPL,':')
         if ( k > 9 ) read(FILETMPL(k+1:),*,iostat=ios) const
         if ( ios /= 0 ) const = 0.0
         if ( MAPL_am_I_root() ) &
            print *, Iam // ': setting variable ' // trim(varn) // &
                            ' to constant = ', const
         _RETURN(ESMF_SUCCESS)
    end if

!   Create Field with input array
!   -----------------------------

    call ESMF_GridGet(GRID, dimCount=gridRank, rc=status)
    _VERIFY(STATUS)
    allocate(gridToFieldMap(gridRank), stat=status)
    _VERIFY(STATUS)
    if(gridRank == 2) then
       gridToFieldMap(1) = 1
       gridToFieldMap(2) = 2
    else if (gridRank == 3) then
       gridToFieldMap(1) = 1
       gridToFieldMap(2) = 2
       gridToFieldMap(3) = 0
    else
       _RETURN(ESMF_FAILURE)
    end if

    FIELD = ESMF_FieldCreate(grid=GRID, &
            datacopyFlag = ESMF_DATACOPY_REFERENCE,   &
            farrayPtr=farrayPtr, name=trim(varn), gridToFieldMap=gridToFieldMap, RC=STATUS)
    _VERIFY(STATUS)
   
    deallocate(gridToFieldMap)

!   Read array data from file
!   -------------------------
    call MAPL_CFIOReadField ( VARN, FILETMPL, TIME,       FIELD,          &
                              VERBOSE=VERBOSE, FORCE_REGRID=FORCE_REGRID, &
                              TIME_INTERP=TIME_INTERP,                    &
                              TIME_IS_CYCLIC=TIME_IS_CYCLIC,              &
                              conservative=conservative,                  &
                              voting = voting, ignoreCase = ignoreCase,   &
                              doParallel = doParallel, getFrac=getFrac,   &
                              RC=STATUS)
    _VERIFY(STATUS)

!   Destroy the ESMF array (data will be preserved since we own it)
!   --------------------------------------------------------------
    call ESMF_FieldDestroy ( FIELD, RC=STATUS )
    _VERIFY(STATUS)

    _RETURN(ESMF_SUCCESS)

   end subroutine MAPL_CFIOReadArray2D

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!BOP
! !IROUTINE: MAPL_CFIODestroy --- Destroys MAPL CFIO Object

! !INTERFACE:
!
  subroutine MAPL_CFIODestroy( MCFIO, RC )
!
! !ARGUMENTS:
!
  type(MAPL_CFIO),             intent(INOUT) :: MCFIO
  integer, optional,           intent(  OUT) :: RC

! !DESCRIPTION: 
!
!    Destroys a MAPL CFIO object. It closes any file associated with
!    it and deallocates memory.

!EOP

  integer :: status

  if(associated(MCFIO%Krank     )) deallocate(MCFIO%Krank     )   
  if(associated(MCFIO%reqs      )) deallocate(MCFIO%reqs      )
  if(associated(MCFIO%varname   )) deallocate(MCFIO%varname   )
  if(associated(MCFIO%vardims   )) deallocate(MCFIO%vardims   )
  if(associated(MCFIO%Levs      )) deallocate(MCFIO%Levs      )
  if(associated(MCFIO%vartype   )) deallocate(MCFIO%vartype   )
  if(associated(MCFIO%needvar   )) deallocate(MCFIO%needvar   )
  if(associated(MCFIO%pairList  )) deallocate(MCFIO%pairList  )
  if(associated(MCFIO%buffer    )) deallocate(MCFIO%buffer  )
  if(associated(MCFIO%varid     )) deallocate(MCFIO%varid  )

  nullify(MCFIO%Krank     )   
  nullify(MCFIO%reqs      )
  nullify(MCFIO%varname   )
  nullify(MCFIO%vardims   )
  nullify(MCFIO%Levs      )
  nullify(MCFIO%varid     )
  nullify(MCFIO%buffer    )

  if (MCFIO%Root > 0) then
     if (MCFIO%myPE == mCFIO%RootRank) then
        call ESMF_CFIOFileClose(MCFIO%CFIO,rc=status)
        _VERIFY(STATUS)
     end if
  end if

  call ESMF_CFIODestroy(MCFIO%CFIO,rc=status)
  _VERIFY(STATUS)

  MCFIO%created = .false.

  _RETURN(ESMF_SUCCESS)
  end subroutine MAPL_CFIODestroy


!BOP
! !IROUTINE: MAPL_CFIOClose --- Close file in MAPL CFIO Object

! !INTERFACE:
!
  subroutine MAPL_CFIOClose( MCFIO, filename, RC )
!
! !ARGUMENTS:
!
  type(MAPL_CFIO),             intent(INOUT) :: MCFIO
  character(len=*), optional,  intent(IN   ) :: filename
  integer, optional,           intent(  OUT) :: RC

! !DESCRIPTION: 
!
!    Not a full destroy; only closes the file.

!EOP

  integer :: status

  if (MCFIO%myPE == mCFIO%RootRank) then
     call ESMF_CFIOFileClose(MCFIO%CFIO,rc=status)
     _VERIFY(STATUS)
     if (present(filename)) then
        close(99)
        open (99,file=trim(filename)//".done",form='formatted')
        close(99)
     end if
  end if

  _RETURN(ESMF_SUCCESS)
  end subroutine MAPL_CFIOClose



  subroutine MAPL_CFIOSet( MCFIO, Root, Psize, fName, Krank, IOWorker, globalComm, newFormat, collectionID, fraction, RC )
!
! !ARGUMENTS:
!
  type(MAPL_CFIO),             intent(INOUT) :: MCFIO
  integer, optional,           intent(IN   ) :: Root, Psize
  character*(*), optional,     intent(IN   ) :: fName
  integer, optional,           intent(IN   ) :: Krank(:)
  integer, optional,           intent(IN   ) :: IOWorker
  integer, optional,           intent(IN   ) :: globalComm
  logical, optional,           intent(IN   ) :: newFormat
  integer, optional,           intent(IN   ) :: collectionID
  integer, optional,           intent(IN   ) :: fraction
  integer, optional,           intent(  OUT) :: RC

! !DESCRIPTION: 
!
!    Not a full destroy; only closes the file.

!EOP


  if(present(Root)) then
     mCFIO%Root = Root
  endif

  if(present(Psize)) then
     mCFIO%Partsize = Psize
  endif

  if(present(fName)) then
     mCFIO%fName = fName
  endif
 
  if(present(Krank)) then
    mCFIO%Krank = Krank
  endif

  if(present(IOWorker)) then
    mCFIO%AsyncWorkRank = IOWorker
  end if

  if(present(globalComm)) then
    mCFIO%globalComm = globalComm
  end if

  if(present(newFormat)) then
     mCFIO%newFormat = newFormat
  end if

  if(present(collectionID)) then
     mCFIO%collection_ID = collectionID
  end if

  if(present(fraction)) then
     mCFIO%fraction = fraction
  end if

  _RETURN(ESMF_SUCCESS)
  end subroutine MAPL_CFIOSet

  subroutine MAPL_CFIOSetKrank( MCFIO, RC )
  type(MAPL_CFIO),             intent(INOUT) :: MCFIO
  integer, optional,           intent(  OUT) :: RC

  integer :: status

  call MAPL_RoundRobinPEList(MCFIO%Krank,MCFIO%PartSize,root=MCFIO%ROOT,firstRank=MCFIO%RootRank,rc=status)
  _VERIFY(STATUS)

  _RETURN(ESMF_SUCCESS)
  end subroutine MAPL_CFIOSetKrank

  subroutine MAPL_CFIOGet(mcfio,RootRank,krank,collection_id,rc)
  type(MAPL_CFIO),             intent(INOUT) :: MCFIO
  integer, optional,           intent(  OUT) :: RootRank
  integer, optional,           intent(  OUT) :: krank
  integer, optional,           intent(  OUT) :: collection_id
  integer, optional,           intent(  OUT) :: RC


  if (present(RootRank)) RootRank = MCFIO%RootRank

  if (present(Krank)) Krank=size(MCFIO%krank)

  if (present(collection_id)) collection_id=MCFIO%collection_id

  _RETURN(ESMF_SUCCESS)
  end subroutine MAPL_CFIOGet


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! This is a candidate for ESMFL, here for dependency reasons
!  

  subroutine GridGetLatLons_ ( grid, lons, lats, rc )

    use MAPL_GetLatLonCoordMod
    implicit NONE
    type(ESMF_Grid) :: grid
    real, pointer   :: lons(:), lats(:)
    integer, optional :: rc

!                     ---


    integer                :: IM_WORLD, JM_WORLD, dims(3), STATUS

!                          ----

!      Get world dimensions
!      --------------------
       call MAPL_GridGet ( grid, globalCellCountPerDim=DIMS, RC=STATUS)
       _VERIFY(STATUS)

       IM_WORLD = dims(1)
       JM_WORLD = dims(2)

!      Allocate memory for output if necessary
!      ---------------------------------------
       if ( .not. associated(lons) ) then
            allocate(lons(IM_WORLD), stat=STATUS)
       else
            if(size(LONS,1) /= IM_WORLD) STATUS = 1
       end if
       _VERIFY(status)
       if ( .not. associated(lats) ) then
            allocate(lats(JM_WORLD), stat=STATUS)
       else
            if(size(LATS,1) /= JM_WORLD) STATUS = 1
       end if
       _VERIFY(status)

       call MAPL_GetLatLonCoord(grid,1,lons,rc=status)
       _VERIFY(status)
       call MAPL_GetLatLonCoord(grid,2,lats,rc=status)
       _VERIFY(status)
       lons = lons*(180._REAL64/MAPL_PI_R8)
       lats = lats*(180._REAL64/MAPL_PI_R8)
       
       _RETURN(ESMF_SUCCESS)
     end subroutine GridGetLatLons_

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


  subroutine VertInterp(v2,v3,pp,ple_,pl_,rc)

    real,              intent(OUT) :: v2(:,:)
    real,              intent(IN ) :: v3(:,:,:)
    real,              intent(IN ) :: pp
    real,     target,  intent(IN ) :: ple_(:,:,:)
    real,     target,  intent(IN ) :: pl_(:,:,:)
    integer, optional, intent(OUT) :: rc

    real, dimension(size(v2,1),size(v2,2)) :: al,PT,PB
    integer km, K, msn
    logical flip
    real    ppx
    real, pointer   :: plx(:,:,:),pl(:,:,:),ps(:,:)

    integer        :: status

    if(size(v3,3)==size(ple_,3)) then
       pl => ple_
       ps => ple_(:,:,ubound(ple_,3))
    else
       pl => pl_
       ps => null()
    endif

    km   = size(pl,3)

    flip = pl(1,1,2) < pl(1,1,1)

    if(flip) then
       allocate(plx(size(pl,1),size(pl,2),size(pl,3)),stat=status)
       _VERIFY(STATUS)
       plx = -pl
       ppx = -pp
       msn = -1
    else
       plx => pl
       ppx = pp
       msn = 1
    end if

    v2   = MAPL_UNDEF

       pb   = plx(:,:,km)
       do k=km-1,1,-1
          pt = plx(:,:,k)
          if(all(pb<ppx)) exit
          where(ppx>pt .and. ppx<=pb)
             al = (pb-ppx)/(pb-pt)
             where (v3(:,:,k)   .eq. MAPL_UNDEF ) v2 = v3(:,:,k+1) 
             where (v3(:,:,k+1) .eq. MAPL_UNDEF ) v2 = v3(:,:,k)
             where (v3(:,:,k)   .ne. MAPL_UNDEF .and.  v3(:,:,k+1) .ne. MAPL_UNDEF  ) 
                    v2 = v3(:,:,k)*al + v3(:,:,k+1)*(1.0-al)
             end where
          end where
          pb = pt
       end do

! Extend Lowest Level Value to the Surface
! ----------------------------------------
    if( associated(ps) ) then
        where( (ppx>plx(:,:,km).and.ppx<=ps*msn) )
                v2 = v3(:,:,km)
        end where
    end if

    if(flip) then
       deallocate(plx,stat=status)
       _VERIFY(STATUS)
    end if

    _RETURN(ESMF_SUCCESS)
  end subroutine VertInterp

  subroutine MAPL_GetCurrentFile(FileTmpl, Time, Filename, RC, EXPID)
    character(len=*),    intent(IN   ) :: filetmpl
    type(ESMF_Time),     intent(INout) :: time
    character(len=*),    intent(  out) :: filename
    integer, optional,   intent(  out) :: rc
    character(len=*), optional, intent(  in) :: EXPID

    integer :: status

    character(len=ESMF_MAXSTR)          :: DATE
    integer                             :: nymd
    integer                             :: nhms

    call ESMF_TimeGet(Time, timeString=DATE, RC=STATUS)
    _VERIFY(STATUS)
    
    call strToInt(DATE, nymd, nhms)
    call fill_grads_template ( Filename, FileTmpl,&
                           experiment_id=EXPID, nymd=nymd, nhms=nhms, rc=status )
    _VERIFY(STATUS)
    _RETURN(ESMF_SUCCESS)
  end subroutine MAPL_GetCurrentFile

  logical function MAPL_CFIOIsCreated(MCFIO)
    type(MAPL_CFIO),             intent(IN) :: MCFIO
    MAPL_CFIOIsCreated = MCFIO%Created
  end function MAPL_CFIOIsCreated

  character(len=ESMF_MAXSTR) function MAPL_CFIOGetFilename(MCFIO)
    type(MAPL_CFIO),             intent(IN) :: MCFIO
    MAPL_CFIOGetFilename = MCFIO%fNAME
  end function MAPL_CFIOGetFilename

  subroutine MAPL_CFIOGetTimeString(mcfio,Clock,Date,rc)
  
    type(MAPL_CFIO  ),          intent(inout) :: MCFIO
    type(ESMF_Clock),           intent(in   ) :: Clock 
    character(len=ESMF_MAXSTR), intent(inout) :: Date
    integer, optional,          intent(out  ) :: rc

    integer :: status

    type(ESMF_Time) :: time
    integer :: YY,MM,DD,H,M,S
    integer :: noffset
    logical :: LPERP
    type(ESMF_Alarm)           :: PERPETUAL
    character(len=ESMF_MAXSTR) :: ClockName


    call ESMF_ClockGet       (CLOCK, name=ClockName, CurrTime =TIME, RC=STATUS)
    _VERIFY(STATUS)

     call ESMF_TimeIntervalGet( MCFIO%OFFSET, S=noffset, rc=status )
     _VERIFY(STATUS)
     if( noffset /= 0 ) then
         LPERP = ( index( trim(clockname),'_PERPETUAL' ).ne.0 )
        if( LPERP ) then
            call ESMF_ClockGetAlarm ( clock, alarmName='PERPETUAL', alarm=PERPETUAL, rc=status )
            _VERIFY(STATUS)
            if( ESMF_AlarmIsRinging(PERPETUAL) ) then
                call ESMF_TimeGet ( Time, YY = YY, &
                                          MM = MM, &
                                          DD = DD, &
                                          H  = H , &
                                          M  = M , &
                                          S  = S, rc=status )
                                          MM = MM + 1
                call ESMF_TimeSet ( Time, YY = YY, &
                                          MM = MM, &
                                          DD = DD, &
                                          H  = H , &
                                          M  = M , &
                                          S  = S, rc=status )
            endif
        endif
     endif

    TIME = TIME - MCFIO%OFFSET

    call ESMF_TimeGet        (TIME,     timeString=DATE,     RC=STATUS)
    _VERIFY(STATUS)

    _RETURN(ESMF_SUCCESS)

    end subroutine MAPL_CFIOGetTimeString
    
    subroutine MAPL_CFIOstartAsyncColl(mcfio,clock,mapl_comm,markdone,rc)

    type(MAPL_CFIO  ),          intent(inout) :: MCFIO
    type(ESMF_Clock) ,          intent(inout) :: clock
    type(MAPL_Communicators),   intent(inout) :: mapl_comm
    integer,                    intent(in   ) :: markdone
    integer, optional       ,   intent(out  ) :: rc

    integer :: status

    integer :: slices
    character(len=ESMF_MAXSTR) :: TimeString,filename
    integer :: im,jm,ionode,l,nn,k,csize
    integer, allocatable :: levindex(:)
    character(len=esmf_maxstr), allocatable ::  levname(:)
    logical :: useFaceDim

    im = mcfio%im
    jm = mcfio%jm
    useFaceDim = mcfio%useFaceDim

    ! count the number of slices
    slices = 0
    do L=1,size(MCFIO%VarDims)
       if (MCFIO%VarDims(L)==2) then
          slices=slices+1
       else if (MCFIO%VarDims(L) ==3) then
          do k=1,MCFIO%lm
             slices=slices+1
          enddo
       endif
    enddo
 
    call MAPL_CFIOServerGetFreeNode(mapl_comm,IOnode,im,jm,slices,rc=status)
    _VERIFY(STATUS)
    call MAPL_CFIOSet(mcfio,IOWorker=IOnode,rc=status)
    _VERIFY(STATUS)
    call MAPL_CFIOGetTimeString(mcfio,clock,timeString,rc=status)
    _VERIFY(STATUS)
    filename = MAPL_CFIOGetFilename(mcfio)
    call MAPL_CFIOAsyncSendCollInfo(filename,IM,JM,timeString,slices &
           ,IOnode,markdone,mapl_comm,useFaceDim,rc=status)
    _VERIFY(STATUS)

    allocate(levindex(slices),stat=status)
    _VERIFY(STATUS)
    allocate(levname(slices),stat=status)
    _VERIFY(STATUS)
    nn = 0

    VARIABLES: do L=1,size(MCFIO%VarDims)

       RANK: if (MCFIO%VarDims(L)==2) then

          nn=nn+1
          levindex(nn)=0
          levname(nn)=mcfio%VarName(L)

       elseif (MCFIO%VarDims(L)==3) then


          LEVELS: do k=1,MCFIO%lm

             nn=nn+1
             levindex(nn)=k
             levname(nn)=mcfio%VarName(L)
          enddo LEVELS
       end if Rank
    enddo VARIABLES

    call MPI_Send(mcfio%krank, slices, MPI_INTEGER, IONODE, MAPL_TAG_SHIPINFO, &
     mapl_comm%mapl%comm,status)
    _VERIFY(STATUS)
    call MPI_Send(levindex, slices, MPI_INTEGER , IONODE, MAPL_TAG_SHIPINFO, &
     mapl_comm%mapl%comm,status)
    _VERIFY(STATUS)
    csize=slices*ESMF_MAXSTR
    call MPI_Send(levname,csize,MPI_CHARACTER, IONODE, MAPL_TAG_SHIPINFO, &
     mapl_comm%mapl%comm,status)
    _VERIFY(STATUS)

    deallocate(levindex)
    deallocate(levname)

    _RETURN(ESMF_SUCCESS)

    end subroutine


    subroutine MAPL_CFIOBcastIONode(mcfio,root,comm,rc)
    type(MAPL_CFIO  ),          intent(inout) :: MCFIO
    integer,                    intent(in   ) :: root
    integer,                    intent(in   ) :: comm
    integer, optional,          intent(out  ) :: rc

    integer :: status

    call mpi_bcast(mcfio%AsyncWorkRank,1,MPI_INTEGER,root,comm,status)
    _VERIFY(STATUS)

    _RETURN(ESMF_SUCCESS)
    end subroutine

  subroutine MAPL_CFIOAsyncSendCollInfo(filename,IM,JM,timeString,nlevs,workRank,markdone,mapl_comm,useFaceDim,rc)
  character(len=ESMF_MAXSTR), intent(in) :: filename
  integer,                    intent(in) :: IM
  integer,                    intent(in) :: JM
  character(len=ESMF_MAXSTR), intent(in) :: timeString
  integer,                    intent(in) :: nlevs
  integer,                    intent(in) :: workRank
  integer,                    intent(in) :: markdone
  type(MAPL_Communicators),   intent(in) :: mapl_comm
  logical,                    intent(in) :: useFaceDim
  integer, optional,          intent(out) :: rc

  integer :: status
  type(MAPL_CFIOServerIOinfo) :: ServerInfo
  integer :: nymd,nhms


  ServerInfo%filename = filename
  ServerInfo%lons = IM
  ServerInfo%lats = JM
  ServerInfo%nlevs = nlevs
  call StrToInt(timeString,nymd,nhms)
  ServerInfo%date = nymd
  ServerInfo%time = nhms
  ServerInfo%markdone = markdone
  ServerInfo%useFaceDim = useFaceDim

  call MPI_Send(ServerInfo, 1, mpi_io_server_info_type, workRank, MAPL_TAG_SHIPINFO, &
     mapl_comm%mapl%comm,status)
  _VERIFY(STATUS)

  _RETURN(ESMF_SUCCESS)

  end subroutine
  subroutine MAPL_CFIOPartition(Slices, NumColls, NumNodes, Writing, Psize,Root)
    integer, intent(IN ) :: NumColls
    integer, intent(IN ) :: Slices(NumColls), NumNodes
    logical, intent(IN ) :: Writing(NumColls)
    integer, intent(OUT) :: Psize(NumColls), Root(NumColls)

    integer :: MaxSlicesPerNode, n
    integer :: CurrNode, Len, SlicesInNode

    integer, dimension(NumColls) :: SortedSlices, CollNo

!!!  Returns Psize and Root, the size (in nodes) and root node 
!!!  of each node partition assigned to active collections.

!!!  NumNodes is the number of nodes being dealt out to the partitions
!!!  NumColls is the total number of collection, some of which may be inactive
!!!  Writing  identifies active collection
!!!  Slices   is the number of 2-D sections or slices in each collection
!!!           The number in inactive collections is ignored.

!!! Begin
!!!------

!!! Make sure all outputs are initialized.
!!! Needed only for inactive collections.
!!!---------------------------------------

    Psize = 0
    Root  = 1

!!! Sort the collection sizes (# of slices) in ascending order.
!!!  Also sort the collection index the same way, to fill the 
!!!  correct ones later.
!!!------------------------------------------------------------
    where(writing)
       SortedSlices = Slices
    elsewhere
       SortedSlices = 0
    endwhere

    do n=1,NumColls
       CollNo(n) = n
    end do

    call MAPL_Sort(SortedSlices, CollNo)

!!! This is the maximum number of slices in a node if all slices
!!!  were uniformly distributed without honoring collection and
!!!  node boundaries. Since every collection boundary must also be
!!!  a node boundary, this is a lower bound on MaxSlicesPerNode
!!! and is used as our initial guess.

    MaxSlicesPerNode = (sum(Slices,mask=Writing)-1)/NumNodes + 1
    ! ALT: The above expression could be zero if 
    !      NumNodes==1 and the sum over "writing" slices is 0 (i.e. no writing)
    MaxSlicesPerNode = max(MaxSlicesPerNode,1) ! make sure it is not 0

!!! We try to distribute the slices in active collections as uniformly
!!!  as possible. "Small" collections (<= MaxSlicesPerNode) are
!!!  assigned to a single node, others span multiple nodes. 
!!!  Small collections are grouped in a node without 
!!!  exceeding MaxSlicesPerNode. Multi-node collections are
!!!  not grouped in nodes. Since MaxSlicesPerNode  is generally
!!!  too small to fit all the collections, it is then increased,until
!!!  all the active collections fit in the given nodes.
!!!--------------------------------------------------------------------
    do
       CurrNode     = 1
       SlicesInNode = 0

       COLLECTIONS: do n=1,NumColls
          ACTIVE: if(Writing(CollNo(n))) then

             if(SortedSlices(n)<MaxSlicesPerNode) then ! A single-node collection
                SlicesInNode = SlicesInNode + SortedSlices(n)

                if(SlicesInNode > MaxSlicesPerNode) then ! Current Coll oveerfills node
                   CurrNode     = CurrNode + 1
                   SlicesInNode = SortedSlices(n)
                end if

                Psize(CollNo(n)) = 1
                Root (CollNo(n)) = (CurrNode-1) + 1

             else                                      ! A multi-node collection
                Len = (SortedSlices(n)-1)/MaxSlicesPerNode + 1

                Psize(CollNo(n)) = Len
                Root (CollNo(n)) = (CurrNode-1) + 1

                CurrNode = CurrNode + len
             endif

          endif ACTIVE
       end do COLLECTIONS

       if(CurrNode<=NumNodes) exit

       MaxSlicesPerNode = MaxSlicesPerNode + 1
    enddo

    return

  end subroutine MAPL_CFIOPartition

  subroutine MAPL_CFIOReadParallel_(bundlelist,filelist,time,blocksize,RegridMethod,gsiMode,timelist,rc)
     type(ESMF_FieldBundle), pointer, intent(inout) :: bundlelist(:)
     character(len=*), intent(in) :: filelist(:)
     type(ESMF_Time), intent(inout) ::time
     type(ESMF_Time), optional, intent(in) ::timelist(:)
     integer, optional, intent(in) :: blocksize
     integer, optional, intent(in) :: regridMethod
     logical, optional, intent(in) :: gsiMode
     integer, optional, intent(out) :: rc


     integer                    :: status

     type(MAPL_CFIO), pointer :: cfio(:)
     type(IntegerVector) :: tindex
     integer :: blocksize_
     type(IntegerVector) :: regridMethod_
     integer, allocatable :: slices(:), psize(:), root(:),gslices(:)
     logical, allocatable :: reading(:)
     integer :: n,nn,n1,n2, nnodes, nfiles, hw,tdx,myregridmethod,nPet,maxSlices,scount
     character(len=ESMF_MAXPATHLEN) :: fname
     type(IntegerVectorIterator) :: time_iter
     type(IntegerVectorIterator) :: regrid_iter
     integer :: collection_id
     type(ESMF_CFIO), pointer :: pcfio
     type(CFIOCollection), pointer :: collection => null()
     type(ESMF_VM) :: vm


     allocate(cfio(size(filelist)),stat=status)
     _VERIFY(status)

     do n=1,size(filelist)
        cfio(n)%collection_id = MAPL_CFIOAddCollection(filelist(n))
        cfio(n)%fname = filelist(n)
        collection => collections%at(cfio(n)%collection_id)
        pcfio => collection%find(cfio(n)%fname)
        if (present(timelist)) then
          call getTIndex(pcfio,timelist(n),nn,rc=status)
        else
          call getTIndex(pcfio,time,nn,rc=status)
        endif
        _VERIFY(status)
        call tindex%push_back(nn)
     enddo

     if (present(blocksize)) then 
        blocksize_=blocksize
     else
        blocksize_=1
     end if

     if (present(regridMethod)) then 
        call regridMethod_%resize(size(filelist))
        do n=1,size(filelist)
           call regridMethod_%set(n,RegridMethod)
        enddo
     else
        call regridMethod_%resize(size(filelist))
        do n=1,size(filelist)
           call regridMethod_%set(n,REGRID_METHOD_BILINEAR)
        enddo
     end if

     call ESMF_VMGetCurrent(vm,rc=status)
     _VERIFY(status)
     call ESMF_VMGet(vm,petCount=nPet,rc=status)
     _VERIFY(status)

     nfiles = size(bundlelist)
     blocksize_ = min(nfiles,blocksize_) 
     allocate(slices(blocksize_),psize(blocksize_),root(blocksize_),reading(blocksize_),gslices(nfiles),stat=status)
     _VERIFY(STATUS)
     reading=.false.
     hw=0
     if (present(gsiMode)) then 
        cfio(:)%gsiMode=gsiMode
        if (gsiMode) hw=1
     end if
     nnodes = size(MAPL_NodeRankList)
     n1=1
     time_iter = tindex%begin() 
     regrid_iter = RegridMethod_%begin()

     do n=1,nfiles
 
        fname = cfio(n)%fname
        myregridmethod = regrid_iter%get()
        collection_id = cfio(n)%collection_id
        call MAPL_CFIOCreateFromFile(cfio(n),bundlelist(n),regridmethod=myRegridMethod,hw=hw,rc=status)
        _VERIFY(status)
        if (mapl_am_i_root()) write(*,'(A,A)')'Parallel read file: ',trim(fname)
        call regrid_iter%next()
        gslices(n)=size(cfio(n)%krank)

     enddo 
     maxSlices = maxval(gslices)
     maxSlices = max(maxSlices,nPet)

     do while(n1 <=nfiles)
        nn=0
        scount=0
        do n=n1,nfiles
           nn=nn+1
           slices(nn) = size(cfio(n)%krank)
           scount=scount+slices(nn)
           reading(nn)=.true.
           if (nn == blocksize_) exit
           if (scount > maxSlices) then
              reading(nn)=.false.
              nn=nn-1
              exit
           end if
        enddo

        n2=n1+nn-1
        call MAPL_CFIOPartition(slices,blocksize_,nNodes,reading,psize,root) 

        nn=0
        do n=n1,n2
           nn=nn+1
           tdx = time_iter%get()
           call MAPL_CFIOSet(cfio(n),root=root(nn),psize=psize(nn),rc=status)
           _VERIFY(status)
           call MAPL_CFIOReadBundleRead(cfio(n),tdx,hw=hw,rc=status)
           _VERIFY(status)
           call time_iter%next()
        enddo

        do n=n1,n2
           call MAPL_CFIOReadBundleWait(cfio(n),rc=status)
           _VERIFY(status)
        enddo

        n1=n2+1

     enddo

     deallocate(slices,psize,root,reading,gslices,stat=status)
     _VERIFY(STATUS)
     deallocate(cfio,stat=status)
     _VERIFY(STATUS)

     _RETURN(ESMF_SUCCESS)

  end subroutine MAPL_CFIOReadParallel_

  subroutine MAPL_CFIOCreateFromFile(MCFIO,bundlein,RegridMethod,hw,only_vars,rc)

    type(MAPL_CFIO  ),               intent(INOUT) :: MCFIO
    type(ESMF_FieldBundle), optional, intent(INOUT) :: BundleIn
    integer,               optional, intent(IN   ) :: RegridMethod
    integer,               optional, intent(IN   ) :: hw
    character(len=*),      optional, intent(IN   ) :: only_vars
    integer,               optional, intent(  OUT) :: RC

    type(ESMF_CFIOGrid), pointer :: CFIOGRID => null()
    type(ESMF_GRID)              :: ESMFGRID
    type(ESMF_CFIOGrid)          :: varsGrid
    type(ESMF_CFIOVarInfo), pointer :: VARS(:) => null()
    type(ESMF_VM)    :: vm
    integer          :: i,k,kv
    integer          :: im,jm,lm,nvars,fvars,bvars,img,jmg,lt,num2dvars,num3dvars
    logical          :: twoD,fillbundle
    character(len=ESMF_MAXSTR) :: units,cfiovarname,long_name,bundlevarname,ctemp1,ctemp2
    integer          :: regridMethod_
    integer, allocatable    :: gridToFieldMap(:)
    integer          :: hw_, gridrank, dims(3), counts(3), halowidth(3)
    real, pointer    :: ptr2(:,:)
    real, pointer    :: ptr3(:,:,:)
    type(ESMF_Field) :: field
    integer          :: status
    logical          :: found
    logical          :: isPresent
    real, pointer    :: lonsfile(:) => null()
    real, pointer    :: latsfile(:) => null()
    real, pointer    :: levsfile(:) => null()
    type(ESMF_CFIO), pointer :: cfiop
    type(CFIOCollection), pointer :: collection

    call ESMF_VMGetCurrent(vm,rc=status)
    _VERIFY(STATUS)
    call ESMF_VMGet(vm,localPet=mcfio%myPE,rc=status)
    _VERIFY(STATUS)

    collection => collections%at(mcfio%collection_ID)
    cfiop => collection%find(mcfio%fname)

    call ESMF_CFIOGet       (cfiop,     grid=CFIOGRID,                     RC=STATUS)
    _VERIFY(STATUS)
    call ESMF_CFIOGridGet   (CFIOGRID, IM=IM, JM=JM, KM=LM,               RC=STATUS)
    _VERIFY(STATUS)
    call ESMF_CFIOGridGet    (CFIOGRID, LON=LONSFILE, LAT=LATSFILE, RC=STATUS)
    _VERIFY(STATUS)
    deallocate(CFIOGRID)

    call ESMF_CFIOGet (cfiop,varObjs=VARS, nVars=fVars, RC=STATUS)
    _VERIFY(STATUS)

    if (present(hw)) then
       hw_=hw
    else
       hw_=0
    end if
    haloWidth = (/hw_,hw_,0/)
    mcfio%kreverse = .false.
    mcfio%xshift   = .false.
    if (JM /= 6*IM) then
       mcfio%xshift = abs(LONSfile(1)+180._REAL64) .GT. abs(LONSfile(2)-LONSfile(1))
    end if

    call ESMF_FieldbundleGet(bundlein,grid=esmfgrid,rc=status)
    _VERIFY(status)
    call MAPL_GridGet(esmfgrid, globalCellCountPerDim=COUNTS, &
         localCellCountPerDim=DIMS, RC=STATUS)
    img=counts(1)
    jmg=counts(2)
   
    ! Get the number of variables we will be reading
    call ESMF_FieldBundleGet(bundlein,fieldCount=bvars,rc=status)
    _VERIFY(status)
    if (bvars>0) then
       fillbundle=.false.
       nvars=bvars
    else
       fillbundle=.true.
       nvars=0
       do i=1,fVars
          if ( present(ONLY_VARS) ) then
             if ( index(','//trim(ONLY_VARS)  //',', &
                        ','//trim(CFIOVARNAME)//',') < 1 ) cycle
          endif
          nvars=nvars+1
       end do
    end if

    allocate(MCFIO%VarDims(NVars), stat=STATUS)
    _VERIFY(STATUS)
    allocate(MCFIO%VarName(NVars), stat=STATUS)
    _VERIFY(STATUS)
    allocate(MCFIO%VarID(NVars), stat=STATUS)
    _VERIFY(STATUS)
    if (.not.fillbundle) then
       do i=1,nvars
          call ESMF_FieldBundleGet(bundlein,i,field,rc=status)
          _VERIFY(status)
          call ESMF_FieldGet(field,name=bundlevarname,rc=status)
          _VERIFY(status)
          found=.false.
          do k=1,fvars
             call ESMF_CFIOVarInfoGet(VARS(K),vname=CFIOVARNAME,twoDimVar=twoD,RC=STATUS)
             _VERIFY(STATUS)
             ctemp1 = ESMF_UtilStringLowerCase(cfiovarname,rc=status)
             _VERIFY(STATUS)
             ctemp2 = ESMF_UtilStringLowerCase(bundlevarname,rc=status)
             _VERIFY(STATUS)
             if (trim(ctemp1)==trim(ctemp2)) then
                found=.true.
                kv=k
                exit
             endif
          enddo
          _ASSERT(found, 'search failed')
          mcfio%varname(i)=bundleVarName
          if (twoD) then
             mcfio%vardims(i)=2
          else
             mcfio%vardims(i)=3
             call ESMF_CFIOVarinfoGet(Vars(kv),grid=varsgrid,rc=status)
             _VERIFY(STATUS)
             call ESMF_CFIOGridGet (varsGrid, lev=levsfile, rc=status)
             _VERIFY(STATUS)
             if (levsfile(1) > levsfile(lm)) mcfio%kreverse = .true.
          end if
          status = nf90_inq_varid(cfiop%fid,cfiovarname,mcfio%varid(i))
          _VERIFY(STATUS)
          call ESMF_FieldGet(FIELD,   Grid=ESMFGRID, RC=STATUS)
          _VERIFY(STATUS)
          call MAPL_GridGet(ESMFGRID, globalCellCountPerDim=COUNTS, RC=STATUS)
          _VERIFY(STATUS)
          _ASSERT( LM==0 .or. counts(3) == 0 .or. LM==counts(3) .or. lm == (counts(3)+1), 'incompatible file and bundle' )
       enddo

    else
       nVars=0
       do i=1,fvars

          call ESMF_CFIOVarInfoGet(VARS(i),vname=CFIOVARNAME, vtitle=LONG_NAME, vunits=UNITS, twoDimVar=twoD, RC=STATUS)
          _VERIFY(STATUS)
          if ( present(ONLY_VARS) ) then
             if ( index(','//trim(ONLY_VARS)  //',', &
                        ','//trim(CFIOVARNAME)//',') < 1 ) cycle
          endif
          nvars=nvars+1
          MCFIO%VarName(nvars) = cfiovarname

          if (twoD) then
             MCFIO%VarDims(nvars) = 2

             allocate(PTR2(1-hw_:DIMS(1)+hw_,1-hw_:DIMS(2)+hw_),stat=STATUS)
             _VERIFY(STATUS)
             PTR2  = 0.0

             call ESMF_GridGet(esmfgrid, dimCount=gridRank, rc=status)
             _VERIFY(STATUS)
             allocate(gridToFieldMap(gridRank), stat=status)
             _VERIFY(STATUS)
             if(gridRank == 2) then
                gridToFieldMap(1) = 1
                gridToFieldMap(2) = 2
             else if (gridRank == 3) then
                gridToFieldMap(1) = 1
                gridToFieldMap(2) = 2
                gridToFieldMap(3) = 0
             else
                _RETURN(ESMF_FAILURE)
             end if
             FIELD = ESMF_FieldCreate(grid=esmfgrid, &
                             datacopyFlag = ESMF_DATACOPY_REFERENCE,   &
                             farrayPtr=PTR2, gridToFieldMap=gridToFieldMap, &
                             name=CFIOVARNAME, &
                             totalLWidth=haloWidth(1:2),     &
                             totalUWidth=haloWidth(1:2),     &
                             rc = status)
             _VERIFY(STATUS)

             deallocate(gridToFieldMap)

             call ESMF_AttributeSet(FIELD, NAME='LONG_NAME', VALUE=LONG_NAME, RC=STATUS)
             _VERIFY(STATUS)
             call ESMF_AttributeSet(FIELD, NAME='UNITS', VALUE=UNITS, RC=STATUS)
             _VERIFY(STATUS)
             call ESMF_AttributeSet(FIELD, NAME='DIMS', VALUE=MAPL_DimsHorzOnly, RC=STATUS)
             _VERIFY(STATUS)
             call ESMF_AttributeSet(FIELD, NAME='VLOCATION', &
                                        VALUE=MAPL_VLocationNone, RC=STATUS)
             _VERIFY(STATUS)

          else
             call ESMF_CFIOVarinfoGet(Vars(i),grid=varsgrid,rc=status)
             _VERIFY(STATUS)
             call ESMF_CFIOGridGet (varsGrid, lev=levsfile, rc=status)
             _VERIFY(STATUS)
             if (levsfile(1) > levsfile(lm)) mcfio%kreverse = .true.
               MCFIO%VarDims(nvars) = 3
               if (lm == counts(3)) then
                  allocate(PTR3(1-hw_:DIMS(1)+hw_,1-hw_:DIMS(2)+hw_,LM),stat=STATUS)
                  _VERIFY(STATUS)
               else if (lm == (counts(3)+1)) then
                  allocate(PTR3(1-hw_:DIMS(1)+hw_,1-hw_:DIMS(2)+hw_,0:LM-1),stat=STATUS)
                  _VERIFY(STATUS)
               endif
               PTR3  = 0.0
               FIELD = ESMF_FieldCreate(grid=esmfgrid, &
                               datacopyFlag = ESMF_DATACOPY_REFERENCE,   &
                               farrayPtr=PTR3, name=CFIOVARNAME,       &
                               totalLWidth=haloWidth(1:2),     &
                               totalUWidth=haloWidth(1:2),     &
                               rc = status)
               _VERIFY(STATUS)
   !ALT: for now we add only HorzVert (no tiles)
               call ESMF_AttributeSet(FIELD, NAME='LONG_NAME', VALUE=LONG_NAME, RC=STATUS)
               _VERIFY(STATUS)
               call ESMF_AttributeSet(FIELD, NAME='UNITS', VALUE=UNITS, RC=STATUS)
               _VERIFY(STATUS)
               call ESMF_AttributeSet(FIELD, NAME='DIMS', VALUE=MAPL_DimsHorzVert, RC=STATUS)
               _VERIFY(STATUS)
               if (lm == counts(3)) then
                  call ESMF_AttributeSet(FIELD, NAME='VLOCATION', &
                                              VALUE=MAPL_VLocationCenter, RC=STATUS)
               else if (lm == (counts(3)+1)) then
                  call ESMF_AttributeSet(FIELD, NAME='VLOCATION', &
                                              VALUE=MAPL_VLocationEdge, RC=STATUS)
               end if

               _VERIFY(STATUS)

          end if

          call MAPL_FieldBundleAdd(bundlein,field,rc=status)
          _VERIFY(STATUS)
          status = nf90_inq_varid(cfiop%fid,cfiovarname,mcfio%varid(i))
          _VERIFY(STATUS)

       enddo
    end if

    ! Fill in MCFIO object
    mcfio%grid = esmfgrid
    mcfio%bundle = bundlein
    mcfio%im = im
    mcfio%jm = jm
    mcfio%lm = lm
    Num2DVars = count(MCFIO%VarDims==2)
    Num3DVars = count(MCFIO%VarDims==3)
    LT  = Num2DVars + Num3DVars*LM
    allocate( MCFIO%reqs (LT),stat=STATUS)
    _VERIFY(STATUS)
    allocate( MCFIO%Krank(LT),stat=STATUS)
    _VERIFY(STATUS)
    mcfio%krank = -1
    allocate( MCFIO%pairList(LT),stat=STATUS)
    _VERIFY(STATUS)

    if (present(regridmethod)) then
       regridmethod_=regridmethod
    else
       regridmethod_=REGRID_METHOD_BILINEAR
    endif
    mcfio%regrid_method = regridmethod_

    mcfio%regrid_type=-1
    if ( (img /= im .or. jmg /= jm) .and. (regridMethod_ /= -1) ) then
       if (regridmethod_==REGRID_METHOD_VOTE .or. regridmethod_==REGRID_METHOD_CONSERVE .or. regridmethod_==REGRID_METHOD_FRACTION) then
          mcfio%regridConservative = .true.
       end if
       mcfio%regrid_type=regridmethod_
    end if

    !check for vector pairs, right now limit to 1
    block
       character(len=ESMF_MAXSTR) :: vectorlist(2)
       logical :: found
       integer :: j
       integer :: rotation,gridstagger,rotation1,rotation2,gridStagger1,gridStagger2
       type(ESMF_Field) :: field1,field2
       allocate(mCFIO%needVar(size(mCFIO%varname)),stat=status)
       _VERIFY(status)
       mCFIO%needVar=0
       call ESMF_AttributeGet(bundlein,name="VectorList:",isPresent=isPresent,rc=status)
       _VERIFY(STATUS)
       if (isPresent) then
          call ESMF_AttributeGet(bundlein,name="VectorList:",valuelist=vectorlist,rc=status)
          _VERIFY(STATUS)

          do i=1,size(mCFIO%varname)
             if (mCFIO%varname(i) == vectorList(1)) then
                found=.false.
                do j=1,size(mCFIO%varname)
                   if (trim(mCFIO%varName(J)) == vectorlist(2)) then
                      found = .true.
                      exit
                   end if
                end do
                _ASSERT(found, 'search failed')
                mCFIO%needvar(i)=j
             else if (mCFIO%varname(i) == vectorList(2)) then
                found=.false.
                do j=1,size(mCFIO%varname)
                   if (trim(mCFIO%varName(J)) == vectorlist(1)) then
                      found = .true.
                      exit
                   end if
                end do
                _ASSERT(found, 'search failed')
                mCFIO%needvar(i)=-j
             end if
          end do

          call ESMF_FieldBundleGet(MCFIO%BUNDLE, trim(vectorList(1)), field=FIELD1,RC=STATUS)
          _VERIFY(STATUS)
          call ESMF_FieldBundleGet(MCFIO%BUNDLE, trim(vectorList(2)), field=FIELD2,RC=STATUS)
          _VERIFY(STATUS)
          mCFIO%doRotate=.false.
          call ESMF_AttributeGet(field1,name='ROTATION',value=rotation1,rc=status)
          call ESMF_AttributeGet(field1,name='STAGGERING',value=gridStagger1,rc=status)
          call ESMF_AttributeGet(field2,name='ROTATION',value=rotation2,rc=status)
          call ESMF_AttributeGet(field2,name='STAGGERING',value=gridStagger2,rc=status)
          _ASSERT(rotation1==rotation2,'rotation does not match')
          _ASSERT(gridStagger1==gridStagger2,'stagger does not match')
          rotation=rotation1
          gridStagger=gridStagger1
          if (gridStagger == MAPL_AGrid) then
             if (rotation == MAPL_RotateLL) then
                mCFIO%doRotate = .false.
             else if (rotation == MAPL_RotateCube) then
                mCFIO%doRotate = .true.
             end if
          else if (gridStagger == MAPL_DGrid) then
             if (rotation /= MAPL_RotateCube) then
                _ASSERT(.false.,'must rotate LL')
             else
                mCFIO%doRotate = .false.
             end if
          else if (gridStagger == MAPL_CGrid) then
             if (rotation /= MAPL_RotateCube) then
               _ASSERT(.false.,'must rotate LL')
             else
                mCFIO%doRotate = .false.
             end if
          end if
       end if
    end block
!@    call ESMF_CFIOVarInfoDestroy(vars, __RC__)
    deallocate(vars)
    deallocate(LONSfile,LATSfile)
    if (associated(levsfile)) then
       deallocate(levsfile)
       nullify(levsfile)
    end if

    mCFIO%PartSize=-1
    mCFIO%root=-1

    _RETURN(ESMF_SUCCESS)

  end subroutine MAPL_CFIOCreateFromFile

  subroutine MAPL_CFIOReadBundlePrefetch(MCFIO,time_index,filetemplate,state,init, rc)
     use MAPL_GenericMod, only: MAPL_MetaComp, MAPL_TimerOn, MAPL_TimerOff
    type(MAPL_CFIO  ),   target,     intent(INOUT) :: MCFIO
    integer,                         intent(INOUT) :: time_index
    character(len=*),                intent(IN   ) :: filetemplate
    type (MAPL_MetaComp), optional, intent(inout) :: state
    logical, optional, intent(in) :: init
    integer,               optional, intent(  OUT) :: RC

    integer          :: status

    type(ESMF_CFIO), pointer :: cfiop
    type(CFIOCollection), pointer :: collection
    type (ESMF_VM) :: vm
    integer :: pet

    type (CFIOCollectionVectorIterator) :: iter

    _UNUSED_DUMMY(filetemplate)
    
    call ESMF_VMGetCurrent(vm,rc=status)
    _VERIFY(STATUS)
    call ESMF_VMGet(vm,localPet=pet,rc=status)
    _VERIFY(STATUS)

    if (.not. init) then
       call MAPL_TimerOn(state,"----add-collection")
       iter = collections%begin()
       do while (iter /= collections%end())
          collection => iter%get()
          collection%scollection_id = i_Clients%add_ext_collection(trim(collection%template))
          call iter%next()
       end do
       call MAPL_TimerOff(state,"----add-collection")
    end if

    collection => collections%at(mcfio%collection_id)
    mcfio%scollection_id = collection%scollection_id
    cfiop => collection%find(trim(mcfio%fname))

    ! TODO: n_vars should be a more obvious thing here
    mcfio%n_vars = size(mcfio%varDims)
    allocate(mcfio%variables(mcfio%n_vars), stat=status)
    _VERIFY(status)

    mcfio%variables(:)%num_dimensions = mcfio%VarDims(:)
    
    call request_file_data(mcfio, rc=status)
    _VERIFY(status)

    _RETURN(ESMF_SUCCESS)

  contains


    ! Use IO client to request distributed data for each field.
    subroutine request_file_data(mcfio, rc)
      type (MAPL_CFIO), target, intent(inout) :: mcfio
      integer, optional, intent(out) :: rc

      integer :: i_var
      integer :: n_dims_var
      type (MCFIO_Variable), pointer :: variable
      integer :: LM_src
      integer :: i1, in, j1, jn ! local bounds on src grid
      integer, allocatable :: start(:)
      integer, allocatable :: global_start(:)
      integer, allocatable :: global_count(:)
      type (ArrayReference) :: ref
      logical :: HasDE

      call MAPL_TimerOn(state,"----request")
      do i_var = 1, mcfio%n_vars

         variable => mcfio%variables(i_var)
         n_dims_var = variable%num_dimensions
         select case (n_dims_var)
         case (2)
            LM_src = 1
         case (3)
            LM_src = MCFIO%lm
         case default
            LM_src = 0
         end select

         ! Note: src_grid is mcfio%grid   (This is apparently not true.)
         ! Note: Too bad that ESMF_GridGetFieldBounds does not accept an
         !       option for returning global indices unless it was constructed
         !       that way.

         call MAPL_grid_interior(collection%src_grid, i1, in, j1, jn)
         
         hasDE = MAPL_GridHasDE(collection%src_grid,rc=status)
         _VERIFY(status)
         if (hasDE) then
            allocate(variable%data(i1:in, j1:jn, LM_src))
            _VERIFY(status)
         else
            allocate(variable%data(0,0,0))
         end if

         select case (n_dims_var)
         case (2)
            start = [i1, j1, time_index] ! (i,j,t)
            global_start = [1, 1, time_index] ! (i,j,t)
            global_count = [mcfio%IM, mcfio%JM, 1]
         case (3)
            start = [i1, j1, 1, time_index] ! (i,j,k,t)
            global_start = [1, 1, 1, time_index] ! (i,j,t)
            global_count = [mcfio%IM, mcfio%JM, MCFIO%LM, 1]
         end select

         ! Request data from the server via the I-Client.
         call MAPL_TimerOn(state,"----make-reference")
         ref = ArrayReference(variable%data)
         call MAPL_TimerOff(state,"----make-reference")
!!$         variable%request_id = i_Clients%request_subset_data_reference( &
!!$              mcfio%scollection_id, mcfio%fname, trim(mcfio%varName(i_var)), &
!!$              & ref, start=start)
         !variable%request_id = i_Clients%collective_prefetch_data( &
         call i_Clients%collective_prefetch_data( &
              mcfio%scollection_id, mcfio%fname, trim(mcfio%varName(i_var)), &
              & ref, start=start, global_start=global_start, global_count=global_count)
      end do
      call MAPL_TimerOff(state,"----request")
       

      _RETURN(ESMF_SUCCESS)

    end subroutine request_file_data

  end subroutine MAPL_CFIOReadBundlePrefetch


  subroutine MAPL_CFIOReadBundleReadPrefetch(MCFIO,hw,state,rc)
     use MAPL_GenericMod, only: MAPL_MetaComp, MAPL_TimerOn, MAPL_TimerOff
    type(MAPL_CFIO  ),  target,   intent(INOUT) :: MCFIO
    integer,               optional, intent(IN   ) :: hw
    type (MAPL_MetaComp), optional, intent(inout) :: state
    integer,               optional, intent(  OUT) :: RC

    integer          :: status

    integer   :: hw_
    type(ESMF_CFIO), pointer :: cfiop
    type(CFIOCollection), pointer :: collection

    type (ESMF_Grid) :: src_grid ! from file
    type (ESMF_Grid) :: dst_grid ! from bundle

    if (present(hw)) then
       hw_=hw
       _ASSERT(.false., 'halo not supported - sorry')
    else
       hw_=0
    end if

    ! TODO: "collections" is a global variable.  Fix.
    collection => collections%at(mcfio%collection_id)
    cfiop => collection%find(trim(mcfio%fname))

    if (present(state)) call MAPL_TimerOn(state,"----RegridStore")
    mcfio%new_regridder => make_esmf_regridder(mcfio, rc=status)
    _VERIFY(status)
    if (present(state)) call MAPL_TimerOff(state,"----RegridStore")

    if (present(state)) call MAPL_TimerOn(state,"----RegridApply")
    call regrid_data(mcfio, rc=status)
    _VERIFY(status)
    if (present(state)) call MAPL_TimerOff(state,"----RegridApply")

    deallocate(mcfio%variables, stat=status)
    _VERIFY(status)

    _RETURN(ESMF_SUCCESS)

  contains

     function make_esmf_regridder(mcfio, rc) result(regridder)
        class (AbstractRegridder), pointer :: regridder
        type (MAPL_CFIO), intent(inout) :: mcfio
        integer, optional, intent(out) :: rc

        ! Local variables
        type(ESMF_Field) :: tmp_field
        
        ! TODO: Add check that the grid is a "2 D" grid.  Someday,
        !       someone clever may pass a grid from a component that
        !       treats the levels as actually gridded.  We should fail
        !       with a message rather than bungling the logic below.
        
        ! Determine the src and dst grids
        src_grid = collection%src_grid
           
        call ESMF_FieldBundleGet(mcfio%bundle, 1, tmp_field, rc=status)
        _VERIFY(status)
        call ESMF_FieldGet(tmp_field, grid=dst_grid,  rc=status)
        _VERIFY(status)
        
        regridder => new_regridder_manager%make_regridder(src_grid, dst_grid, regrid_method=mcfio%regrid_method, rc=status)
        _VERIFY(status)

        _RETURN(ESMF_SUCCESS)
        
     end function make_esmf_regridder
     
     
     ! Wait for data to arrive for each field.  Then use established regridder to
     ! to regrid onto model grid.
     subroutine regrid_data(mcfio, rc)
        type (MAPL_CFIO), target, intent(inout) :: mcfio
        integer, optional, intent(out) :: rc

        type (MCFIO_Variable), pointer :: variable
        type (ESMF_Field) :: bundle_field
        type (ESMF_Field) :: bundle_vector_fields(2)
        integer :: i_var, j_var
        
        ! Loop over variables and regrid each one.
        do i_var = 1, mcfio%n_vars
           variable => mcfio%variables(i_var)
           
           j_var = mcfio%needVar(i_var)
           select case (j_var)
           case (:-1)
              ! Field is northward component of tangent vector
              ! Skip - is processed with Eastward component
              cycle
           case (1:)
              ! Field is eastward component of tangent vector
              ! regrid with northward component
              ! Extract fields from bundle
              call ESMF_FieldBundleGet(mcfio%bundle, i_var, bundle_vector_fields(1), rc=status)
              _VERIFY(status)
              call ESMF_FieldBundleGet(mcfio%bundle, j_var, bundle_vector_fields(2), rc=status)
              _VERIFY(status)
              call regrid_vector(mcfio, mcfio%variables([i_var,j_var]), bundle_vector_fields, rc=status)
              _VERIFY(status)
           case (0)
            ! Field is a scalar quantity
              ! Get a Fortran pointer to the field in the bundle:
              call ESMF_FieldBundleGet(mcfio%bundle, i_var, bundle_field, rc=status)
              _VERIFY(status)
              
              call regrid_scalar(mcfio, variable, bundle_field, rc=status)
              _VERIFY(status)
           end select
           
        end do
        
        _RETURN(ESMF_SUCCESS)
        
     end subroutine regrid_data
     
     
     subroutine regrid_scalar(mcfio, variable, bundle_field, rc)
        type (MAPL_CFIO), intent(inout) :: mcfio
        type (MCFIO_Variable), target :: variable
        type (ESMF_Field), intent(inout) :: bundle_field
        integer, optional, intent(out) :: rc
        
        type (ESMF_DynamicMask) :: dynamic_mask
        integer :: n_dims_var
        
        real(kind=REAL32), pointer :: src_data_2d(:,:)
        real(kind=REAL32), pointer :: src_data_3d(:,:,:)
        
        real(kind=REAL32), pointer :: bundle_data_2d(:,:)
        real(kind=REAL32), pointer :: bundle_data_3d(:,:,:)
        
        ! Has the data arrived from the server.
        if (present(state)) call MAPL_TimerOn(state,"--IclientWait")
        !call i_Clients%wait(variable%request_id)
        call i_Clients%wait()
        if (present(state)) call MAPL_TimerOff(state,"--IclientWait")
       
        n_dims_var = variable%num_dimensions
        select case (n_dims_var)
        case (2)
           src_data_2d => variable%data(:,:,1)
           if (mcfio%regrid_method == REGRID_METHOD_FRACTION) src_data_2d=src_data_2d-mcfio%fraction
           call ESMF_FieldGet(bundle_field, farrayPtr=bundle_data_2d, rc=status)
           _VERIFY(status)
           call mcfio%new_regridder%regrid(src_data_2d, bundle_data_2d, rc=status)
           _VERIFY(status)
        case (3)
           src_data_3d => variable%data(:,:,:)
           if (mcfio%regrid_method == REGRID_METHOD_FRACTION) src_data_3d=src_data_3d-mcfio%fraction
           call ESMF_FieldGet(bundle_field, farrayPtr=bundle_data_3d, rc=status)
           _VERIFY(status)
           call mcfio%new_regridder%regrid(src_data_3d, bundle_data_3d, rc=status)
           _VERIFY(status)
        end select

        _RETURN(ESMF_SUCCESS)

     end subroutine regrid_scalar


     subroutine regrid_vector(mcfio, variables, bundle_fields, rc)
        use MAPL_AbstractGridFactoryMod
        type (MAPL_CFIO), intent(inout) :: mcfio
        type (MCFIO_Variable) :: variables(2)
        type (ESMF_Field), intent(inout) :: bundle_fields(2)
        integer, optional, intent(out) :: rc

        type (ESMF_DynamicMask) :: dynamic_mask
        integer :: n_dims_var

        real(kind=REAL32), pointer :: ew_bundle_data_2d(:,:)
        real(kind=REAL32), pointer :: ns_bundle_data_2d(:,:)
        real(kind=REAL32), pointer :: ew_bundle_data_3d(:,:,:)
        real(kind=REAL32), pointer :: ns_bundle_data_3d(:,:,:)
        type (ESMF_Field) :: src_field
        type (ESMF_Field) :: dst_field

        class (AbstractGridFactory), pointer :: factory

        ! Has the data arrived from the server.
        !call i_Clients%wait(variables(1)%request_id)
        !call i_Clients%wait(variables(2)%request_id)
        call i_Clients%wait()

        n_dims_var = variables(1)%num_dimensions
        _ASSERT(n_dims_var == variables(2)%num_dimensions, 'inconsistent number of dimensions')

        select case (n_dims_var)
        case (2)
           call ESMF_FieldGet(bundle_fields(1), farrayPtr=ew_bundle_data_2d, rc=status)
           _VERIFY(status)
           call ESMF_FieldGet(bundle_fields(2), farrayPtr=ns_bundle_data_2d, rc=status)
           _VERIFY(status)

           call mcfio%new_regridder%regrid( &
                & variables(1)%data(:,:,1), variables(2)%data(:,:,1), &
                & ew_bundle_data_2d, ns_bundle_data_2d, rc=status)
           _VERIFY(status)
        case (3)
           call ESMF_FieldGet(bundle_fields(1), farrayPtr=ew_bundle_data_3d, rc=status)
           _VERIFY(status)
           call ESMF_FieldGet(bundle_fields(2), farrayPtr=ns_bundle_data_3d, rc=status)
           _VERIFY(status)

           call mcfio%new_regridder%regrid( &
                & variables(1)%data, variables(2)%data, &
                & ew_bundle_data_3d, ns_bundle_data_3d, rc=status)
           _VERIFY(status)
        case default
           _RETURN(ESMF_FAILURE)
        end select

        _RETURN(ESMF_SUCCESS)

     end subroutine regrid_vector


     subroutine simpleDynMaskProcV(dynamicMaskList, dynamicSrcMaskValue, &
          dynamicDstMaskValue, rc)
        type(ESMF_DynamicMaskElementR4R8R4V), pointer              :: dynamicMaskList(:)
        real(ESMF_KIND_R4),            intent(in), optional :: dynamicSrcMaskValue
        real(ESMF_KIND_R4),            intent(in), optional :: dynamicDstMaskValue
        integer,                       intent(out)          :: rc
        integer :: i, j, k, n
        real(ESMF_KIND_R4), allocatable  :: renorm(:)

        if (associated(dynamicMaskList)) then
           n = size(dynamicMaskList(1)%srcElement(1)%ptr)
           allocate(renorm(n))

           do i=1, size(dynamicMaskList)
              dynamicMaskList(i)%dstElement = 0.0 ! set to zero

              renorm = 0.d0 ! reset
              do j=1, size(dynamicMaskList(i)%factor)
                 do k = 1, size(dynamicMaskList(i)%srcElement(j)%ptr)
                    if (.not. &
                         match(dynamicSrcMaskValue,dynamicMaskList(i)%srcElement(j)%ptr(k))) then
                       dynamicMaskList(i)%dstElement(k) = dynamicMaskList(i)%dstElement(k) & 
                            + dynamicMaskList(i)%factor(j) &
                            * dynamicMaskList(i)%srcElement(j)%ptr(k)
                       renorm(k) = renorm(k) + dynamicMaskList(i)%factor(j)
                    endif
                 end do
              end do
              where (renorm > 0.d0)
                 dynamicMaskList(i)%dstElement = dynamicMaskList(i)%dstElement / renorm
              elsewhere
                 dynamicMaskList(i)%dstElement = dynamicSrcMaskValue
              end where
           enddo
        endif
        ! return successfully
        rc = ESMF_SUCCESS
     end subroutine simpleDynMaskProcV

     logical function match(missing,b)
        real(kind=REAL32), intent(in) :: missing, b
        match = (missing==b) 
     end function match
  
end subroutine MAPL_CFIOReadBundleReadPrefetch


  subroutine MAPL_CFIOReadBundleRead(MCFIO,tindex,hw,rc)

    type(MAPL_CFIO  ),               intent(INOUT) :: MCFIO
    integer,                         intent(INOUT) :: tindex
    integer,               optional, intent(IN   ) :: hw
    integer,               optional, intent(  OUT) :: RC

    integer          :: status

    integer   :: nn,l,k,klev,lm,nv,lb
    integer   :: img,jmg,lt,hw_
    integer   :: counts(3)
    logical   :: myGlobal, transAlreadyDone
    real, pointer :: ptr2(:,:)
    real, pointer :: ptr3(:,:,:)
    type(ESMF_Field) :: field
    type(ESMF_CFIO), pointer :: cfiop
    type(CFIOCollection), pointer :: collection
    type(Ptr2Arr), allocatable :: globPtrArr(:)
    type(Ptr2Arr)        :: PtrTypeIn(2)
    type(Ptr2Arr)        :: PtrTypeOut(2)
    integer, allocatable :: varids(:)
    logical, allocatable :: transDone(:)
    integer :: status1,status2
    integer :: alloc_ra

    if (present(hw)) then
       hw_=hw
    else
       hw_=0
    end if

    collection => collections%at(mcfio%collection_id)
    cfiop => collection%find(trim(mcfio%fname))

    call MAPL_GridGet( MCFIO%GRID, globalCellCountPerDim=COUNTS, RC=STATUS)
    _VERIFY(STATUS)

    call MAPL_RoundRobinPEList(MCFIO%Krank,MCFIO%PartSize,root=MCFIO%ROOT,rc=status)
    _VERIFY(STATUS)

    if (mapL_am_i_root()) write(*,*)"CFIO Read Parallel Processing: ",trim(mcfio%fname)

    call MAPL_RoundRobinPEList(MCFIO%Krank,MCFIO%PartSize,root=MCFIO%ROOT,useFirstRank=.false.,rc=status)
    _VERIFY(STATUS)

    call MAPL_CFIOSetVectorPairs(MCFIO,rc=status)
    _VERIFY(Status)

    img = COUNTS(1)
    jmg = COUNTS(2)

    lt = size(mcfio%reqs)
    allocate(mcfio%buffer(lt),stat=status)
    _VERIFY(status)
    allocate(globPtrArr(lt),stat=status)
    _VERIFY(status)
    allocate(varids(lt),stat=status)
    _VERIFY(status)
    allocate(transDone(lt),source=.false.,stat=status)
    _VERIFY(status)
    
    nn = 0
    VARS1: do L=1,size(MCFIO%VarDims)

       call ESMF_FieldBundleGet(MCFIO%BUNDLE, trim(MCFIO%VARNAME(L)), field=FIELD,                  RC=STATUS)
       _VERIFY(STATUS)

       CREATE_REQ: if (MCFIO%VarDims(L)==2) then

          nn=nn+1
          varids(nn)=mcfio%varid(L)
          call ESMF_FieldGet(Field,localDE=0, farrayPtr=PTR2, RC=STATUS)
          _VERIFY(STATUS)
          call MAPL_CreateRequest(MCFIO%GRID, MCFIO%Krank(nn), MCFIO%reqs(nn), &
               tag=nn, RequestType=MAPL_IsScatter, DstArray = ptr2 , prepost=.true., hw=hw_, RC=STATUS)

       else if (MCFIO%VarDims(L)==3) then

          call ESMF_FieldGet(Field,localDE=0, farrayPtr=PTR3, RC=STATUS)
          _VERIFY(STATUS)

          do k = 1,MCFIO%lm
             nn=nn+1
             varids(nn)=mcfio%varid(L)
             lb=lbound(ptr3,3)
             call MAPL_CreateRequest(MCFIO%GRID, MCFIO%Krank(nn), MCFIO%reqs(nn), &
                  tag=nn, RequestType=MAPL_IsScatter, DstArray = ptr3(:,:,lb+k-1), prepost=.true.,hw=hw_,RC=STATUS)
          end do

       end if CREATE_REQ

    end do VARS1

    nn = 0
    VARS2: do L=1,size(MCFIO%VarDims)

       if (MCFIO%VarDims(L)==2) then
          LM = 1
       else  if (MCFIO%VarDims(L)==3) then
          LM = MCFIO%lm
       else
          LM = 0
       end if 

       do k = 1,lm
          nn=nn+1
          myGlobal = (mcfio%mype == mcfio%krank(nn))
          if (mcfio%kreverse) then
             klev= mcfio%lm-k+1
          else
             klev=k
          end if

          if (myGlobal) then
             nv = mCFIO%pairList(nn)
             alloc_ra = 0
             VECTORTEST: if (nv ==0) then
                alloc_ra = 1
                allocate(mcfio%reqs(nn)%read_array(mcfio%im,mcfio%jm),stat=status)
                _VERIFY(status)
                globPtrArr(nn)%ptr => mcfio%reqs(nn)%read_array
                ptrTypeIn(1)%ptr => globPtrArr(nn)%ptr
                allocate(mcfio%buffer(nn)%ptr(img,jmg),stat=status)
                _VERIFY(STATUS)
                ptrTypeOut(1)%ptr => mcfio%buffer(nn)%ptr
                if (lm == 1) then
                   call readlevel(ptrtypein(1)%ptr,cfiop%fid,varids(nn),cfiop%formatVersion,mcfio%im,mcfio%jm,tindex,rc=status)
                else
                   call readlevel(ptrtypein(1)%ptr,cfiop%fid,varids(nn),cfiop%formatVersion,mcfio%im,mcfio%jm,tindex,lev=klev,rc=status)
                end if
                _VERIFY(STATUS)
                if (mcfio%xshift) call shift180Lon2D_(ptrtypein(1)%ptr)
                call TransAndSave(mcfio,ptrTypein(1:1),ptrtypeout(1:1),mcfio%reqs(nn),.true.,1,hw,rc=status)
                _VERIFY(status)
             else if (nv > 0) then
                ! I am U part of vector
                if (transDone(nn)) then
                   TransAlreadyDone = .true.
                   transDone(nn) = .true.
                   transDone(nv) = .true.
                else
                   TransAlreadyDone = .false.
                   alloc_ra = 2
                   allocate(MCFIO%reqs(nn)%read_array(mcfio%im,mcfio%jm),stat=status)
                   _VERIFY(status)
                   allocate(MCFIO%reqs(nv)%read_array(mcfio%im,mcfio%jm),stat=status)
                   _VERIFY(status)
                   allocate(mcfio%buffer(nn)%ptr(img,jmg),stat=status)
                   _VERIFY(status)
                   allocate(mcfio%buffer(nv)%ptr(img,jmg),stat=status)
                   _VERIFY(status)
                end if
                globPtrArr(nn)%ptr => mcfio%reqs(nn)%read_array
                globPtrArr(nv)%ptr => mcfio%reqs(nv)%read_array
                ptrTypeIn(1)%ptr => globPtrArr(nn)%ptr
                ptrTypeIn(2)%ptr => globPtrArr(nv)%ptr
                ptrTypeOut(1)%ptr => mcfio%buffer(nn)%ptr
                ptrTypeOut(2)%ptr => mcfio%buffer(nv)%ptr
                if (.not.TransAlreadyDone) then
                   if (lm == 1) then
                      call readlevel(ptrtypein(1)%ptr,cfiop%fid,varids(nn),cfiop%formatVersion,mcfio%im,mcfio%jm,tindex,rc=status1)
                      call readlevel(ptrtypein(2)%ptr,cfiop%fid,varids(nv),cfiop%formatVersion,mcfio%im,mcfio%jm,tindex,rc=status2)
                   else
                      call readlevel(ptrtypein(1)%ptr,cfiop%fid,varids(nn),cfiop%formatVersion,mcfio%im,mcfio%jm,tindex,lev=klev,rc=status1)
                      call readlevel(ptrtypein(2)%ptr,cfiop%fid,varids(nv),cfiop%formatVersion,mcfio%im,mcfio%jm,tindex,lev=klev,rc=status2)
                   end if
                   _VERIFY(status1)
                   _VERIFY(status2)
                   if (mcfio%xshift) call shift180Lon2D_(ptrtypein(1)%ptr)
                   if (mcfio%xshift) call shift180Lon2D_(ptrtypein(2)%ptr)
                end if
                call TransAndSave(mcfio,ptrTypein(1:2),ptrtypeout(1:2),mcfio%reqs(nn),.not.TransAlreadyDone,1,hw_,rc=status)
             else
                ! I am V part of vector
                nv = abs(nv)
                if (.not.transDone(nn)) then
                   TransAlreadyDone = .true.
                   transDone(nn)=.true.
                   transDone(nv)=.true.
                else
                   TransAlreadyDone = .false.
                   alloc_ra = 2
                   allocate(MCFIO%reqs(nn)%read_array(mcfio%im,mcfio%jm),stat=status)
                   _VERIFY(status)
                   allocate(MCFIO%reqs(nv)%read_array(mcfio%im,mcfio%jm),stat=status)
                   _VERIFY(status)
                   allocate(mcfio%buffer(nn)%ptr(img,jmg),stat=status)
                   _VERIFY(status)
                   allocate(mcfio%buffer(nv)%ptr(img,jmg),stat=status)
                   _VERIFY(status)
                end if
                globPtrArr(nn)%ptr => mcfio%reqs(nn)%read_array
                globPtrArr(nv)%ptr => mcfio%reqs(nv)%read_array
                ptrTypeIn(1)%ptr => globPtrArr(nv)%ptr
                ptrTypeIn(2)%ptr => globPtrArr(nn)%ptr
                ptrTypeOut(1)%ptr => mcfio%buffer(nv)%ptr
                ptrTypeOut(2)%ptr => mcfio%buffer(nn)%ptr
                if (.not.transAlreadyDone) then
                   if (lm == 1) then
                      call readlevel(ptrtypein(1)%ptr,cfiop%fid,varids(nv),cfiop%formatVersion,mcfio%im,mcfio%jm,tindex,rc=status1)
                      call readlevel(ptrtypein(2)%ptr,cfiop%fid,varids(nn),cfiop%formatVersion,mcfio%im,mcfio%jm,tindex,rc=status2)
                   else
                      call readlevel(ptrtypein(1)%ptr,cfiop%fid,varids(nv),cfiop%formatVersion,mcfio%im,mcfio%jm,tindex,lev=klev,rc=status1)
                      call readlevel(ptrtypein(2)%ptr,cfiop%fid,varids(nn),cfiop%formatVersion,mcfio%im,mcfio%jm,tindex,lev=klev,rc=status2)
                   end if
                   _VERIFY(status1)
                   _VERIFY(status2)
                   if (mcfio%xshift) call shift180Lon2D_(ptrtypein(1)%ptr)
                   if (mcfio%xshift) call shift180Lon2D_(ptrtypein(2)%ptr)
                end if
                call TransAndSave(mcfio,ptrTypein(1:2),ptrtypeout(1:2),mcfio%reqs(nn),.not.TransAlreadyDone,2,hw_,rc=status)
             end if VECTORTEST
             if (alloc_ra > 0) then
                deallocate(mcfio%reqs(nn)%read_array)
                nullify(mcfio%reqs(nn)%read_array)
                if (alloc_ra > 1) then
                   deallocate(mcfio%reqs(nv)%read_array)
                   nullify(mcfio%reqs(nv)%read_array)
                end if
             end if

          end if
       end do

    end do VARS2

    deallocate(varids,globptrarr,transDone)

    _RETURN(ESMF_SUCCESS)

    contains
 
  subroutine TransAndSave(mcfio,ptrin,ptrout,req,doTrans,idxOut,hw,rc)
     type(MAPL_CFIO), intent(inout) :: mcfio
     type(Ptr2Arr), intent(inout) :: PtrIn(:)
     type(Ptr2Arr), intent(inout)  :: PtrOut(:)
     type(MAPL_CommRequest), intent(inout)  :: req
     logical, intent(in)  :: doTrans
     integer, intent(in)  :: idxOut
     integer, intent(in)     :: hw
     integer, optional, intent(out) :: rc
 
     __Iam__('TransAndSave')
     real, pointer  :: gin(:,:)
     real , pointer :: gout(:,:)
     real, dimension(:,:,:), pointer :: uin, uout, vin, vout
     integer :: im, jm
     type(c_ptr) :: cptr

     if (size(ptrin)==1) then

        _ASSERT(idxOut ==1, 'input is scalar, output is not')
        Gin => PtrIn(1)%ptr
        Gout => PtrOut(1)%ptr
        if ( all(shape(gin) == shape(gout)) ) then
           gout = gin
        else
           _ASSERT(associated(mCFIO%regridder), 'mCFIO%regridder is not associated')
           if (mCFIO%regridConservative) then
              call mCFIO%regridder%regrid(gin, gout, rc=status)
              _VERIFY(STATUS)
           else
              call mCFIO%regridder%set_undef_value(MAPL_undef)
              call mCFIO%regridder%regrid(gin,gout,rc=status)
              _VERIFY(STATUS)
           end if
        end if
        deallocate(gin)
        nullify(gin)
        if (mcfio%gsiMode) call shift180Lon2D_(gout)
     else
 
        _ASSERT(size(PtrIn) == 2, 'input is neither a scalar nor a tangent (2d) vector')
        _ASSERT(size(PtrOut) == 2, 'input is a vector, but output is not')
        gout => PtrOut(idxOut)%ptr
        if (doTrans) then
 
           im = size(PtrIn(1)%ptr,1)
           jm = size(PtrIn(1)%ptr,2)
 
           cptr = C_loc(PtrIn(1)%ptr(1,1))
           call C_F_pointer (cptr, uin,[im,jm,1])
 
           cptr = C_loc(PtrIn(2)%ptr(1,1))
           call C_F_pointer (cptr, vin,[im,jm,1])
 
           im = size(PtrOut(1)%ptr,1)
           jm = size(PtrOut(1)%ptr,2)
 
           cptr = C_loc(PtrOut(1)%ptr(1,1))
           call C_F_pointer (cptr, uout,[im,jm,1])
 
           cptr = C_loc(PtrOut(2)%ptr(1,1))
           call C_F_pointer (cptr, vout,[im,jm,1])

           call mCFIO%regridder%set_undef_value(MAPL_undef)
           call mCFIO%regridder%regrid(uin, vin, uout, vout, rotate=mCFIO%doRotate, rc=status)
           _VERIFY(status)

           deallocate(PtrIn(1)%ptr)
           nullify(PtrIn(1)%ptr)
           deallocate(PtrIn(2)%ptr)
           nullify(PtrIn(2)%ptr)

        end if

     end if
     call MAPL_ArrayIScatter(gout,req,hw=hw,rc=status)
     _VERIFY(STATUS)

  end subroutine TransAndSave

    subroutine shift180Lon2D_ ( c  )
    real, intent(inout) :: c(:,:)
    real, allocatable :: cj(:)
    integer :: m(4), n(4), imh, j, im ,jm
    im = size(c,1)
    jm = size(c,2)
    allocate(cj(im))
    imh = nint(im/2.)
    m = [ 1,      imh, 1+imh,    im  ]
    n = [ 1,   im-imh, 1+im-imh, im  ]
    do j = 1, jm
       cj(n(1):n(2)) = c(m(3):m(4),j)
       cj(n(3):n(4)) = c(m(1):m(2),j)
       c(:,j) = cj
    end do
    deallocate(cj)
    return
    end subroutine shift180Lon2D_

    subroutine readlevel(var_array,fid,varid,fformat,im,jm,tindex,lev,rc)
       real, pointer, intent(inout) :: var_array(:,:)
       integer,       intent(in) :: fid
       integer,       intent(in) :: varid
       real,          intent(in) :: fformat
       integer,       intent(in) :: im
       integer,       intent(in) :: jm
       integer,       intent(in) :: tindex
       integer, optional, intent(in) :: lev
       integer, optional, intent(out) :: rc

       integer :: status

       if (present(lev)) then
          if (cfiop%formatVersion > 2.0) then
             status = nf90_get_var(fid,varid,var_array,start=(/1,1,1,lev,tindex/),count=(/im,jm/6,6,1,1/))
          else
             status = nf90_get_var(fid,varid,var_array,start=(/1,1,lev,tindex/),count=(/im,jm,1,1/))
          end if
          _VERIFY(STATUS)
       else
          if (cfiop%formatVersion > 2.0) then
             status = nf90_get_var(fid,varid,var_array,start=(/1,1,1,tindex/),count=(/im,jm/6,6,1/))
          else
             status = nf90_get_var(fid,varid,var_array,start=(/1,1,tindex/),count=(/im,jm,1/))
          end if
          _VERIFY(STATUS)
       end if
       _RETURN(ESMF_SUCCESS)
    end subroutine readlevel

  end subroutine MAPL_CFIOReadBundleRead

  subroutine MAPL_CFIOReadBundleWait(MCFIO,rc)

    type(MAPL_CFIO  ),               intent(INOUT) :: MCFIO
    integer,               optional, intent(  OUT) :: RC

    integer :: nn,k,l
    logical :: myGlobal

    type(ESMF_Field) :: field
    real, pointer    :: ptr3d(:,:,:) => null()
    integer          :: status

    nn = 0
    VARS: do L=1,size(MCFIO%VarDims)

       RANK: if (MCFIO%VarDims(L)==2) then

          nn=nn+1
          myGlobal = (mcfio%mype == mcfio%krank(nn))

          call MAPL_CollectiveWait(mcfio%reqs(nn),rc=status)
          _VERIFY(STATUS)
          if (myGlobal) then
             deallocate(mcfio%buffer(nn)%ptr)
          end if

       else if (MCFIO%VarDims(L)==3) then

          do k = 1,MCFIO%lm
             nn=nn+1
             myGlobal = (mcfio%mype == mcfio%krank(nn))
             call MAPL_CollectiveWait(mcfio%reqs(nn),rc=status)
             _VERIFY(STATUS)

             if (myGlobal) then
                deallocate(mcfio%buffer(nn)%ptr)
                !ALT these two have been cleaned earlier
                ! deallocate(mcfio%reqs(nn)%read_array)
                ! nullify(mcfio%reqs(nn)%read_array)
             end if
          end do

          if (mcfio%gsiMode) then
             call ESMF_FieldBundleGet(MCFIO%BUNDLE, trim(mcfio%varname(L)), field=FIELD, RC=STATUS)
             _VERIFY(STATUS)
             call ESMF_FieldGet(field,0,farrayptr=ptr3d,rc=status)
             call SwapV_(ptr3d)
          end if

       end if RANK

    end do VARS

    _RETURN(ESMF_SUCCESS)

    contains

   subroutine SwapV_(fld)
    implicit none
    real,intent(inout) ::  fld(:,:,:)
    real,allocatable   :: work(:,:,:)
    integer im, jm, km
    im   = size(fld,1)
    jm   = size(fld,2)
    km   = size(fld,3)
    allocate (work(im,jm,km))
    work = fld
    fld(:,:,km:1:-1) = work(:,:,1:km:+1)
    deallocate (work)
    end subroutine SwapV_

  end subroutine MAPL_CFIOReadBundleWait

  subroutine MAPL_CFIOSetVectorPairs(MCFIO,rc)

    type(MAPL_CFIO), intent(inout) :: MCFIO
    integer,         intent(out  ) :: rc

    integer, allocatable       :: varStart(:)
    integer                    :: l,lm,nn,nv,sgn,k,np
    integer                    :: status


    ! adjust if we have vector pairs
    allocate(varStart(size(MCFIO%VarDims)),stat=status)
    _VERIFY(status)

    nn=0

    STARTVAR: do L=1, size(MCFIO%VarDims)
       if(mCFIO%VarDims(L)==2) then ! Rank == 2
          LM = 1
       elseif(MCFIO%VarDims(L)==3) then ! Rank == 3
          LM = MCFIO%LM
       else
          LM = 0
       endif
       if (L /= 0) then
          varStart(L) = nn + 1
          nn = nn + LM
       end if
    end do STARTVAR

    nn=0

    VECTPAIR: do L=1, size(MCFIO%VarDims)
       if(mCFIO%VarDims(L)==2) then ! Rank == 2
          LM = 1
       elseif(MCFIO%VarDims(L)==3) then ! Rank == 3
          LM = MCFIO%LM
       else
          LM = 0
       endif

       nv = mCFIO%needVar(L)
       if (nv > 0) then
          sgn=1
       else
          sgn = -1
       end if

       do K=1,LM
          nn = nn + 1
          if (nv /= 0) then
             MCFIO%pairList(nn) = sgn*(varStart(abs(nv)) - 1 + k)
          else
             MCFIO%pairList(nn) = 0
          end if
       enddo
    end do VECTPAIR

    deallocate(varStart)

    !Modify KRANK to make sure that for any pair,
    !all of its components are handled by the SAME processor
    do L=1,size(MCFIO%Krank)
       np = MCFIO%pairList(L)
       if (np > 0) then
          ! I am "U"; overwrite location of "V"
          mCFIO%Krank(abs(np)) = mCFIO%Krank(L)
       end if
    enddo

    _RETURN(ESMF_SUCCESS)

  end subroutine MAPL_CFIOSetVectorPairs

  subroutine get_latlon_from_factory(grid, lons, lats, rc)
    use MAPL_AbstractGridFactoryMod
    use MAPL_LatLonGridFactoryMod
    use MAPL_GridManagerMod
    use MAPL_ConstantsMod, only: MAPL_RADIANS_TO_DEGREES
    type (ESMF_GRid), intent(in) :: grid

    real, intent(out) :: lons(:), lats(:)
    integer :: i
    integer, optional, intent(out) :: rc
  
    integer :: status

    class (AbstractGridFactory), pointer :: factory
    
    factory => get_factory(grid, rc=status)
    _VERIFY(status)
    select type (factory)
    type is (LatLonGridFactory)
      lons = MAPL_RADIANS_TO_DEGREES * factory%get_longitudes()
      lats = MAPL_RADIANS_TO_DEGREES * factory%get_latitudes()
    class default
      do i=1,size(lons)
         lons(i)=i
      enddo
      do i=1,size(lats)
         lats(i)=i
      enddo
    end select
    
  end subroutine get_latlon_from_factory

  function make_regridder(esmfgrid, method, lons, lats, im,jm,lm, runparallel, LocalTiles, rc) result(regridder)
    use MAPL_AbstractGridFactoryMod
    use MAPL_AbstractRegridderMod
    use MAPL_GridManagerMod
    use MAPL_RegridderManagerMod

    class (AbstractRegridder), pointer :: regridder
    type (ESMF_Grid), intent(in) :: esmfgrid
    integer, intent(in) :: method
    real, intent(in) :: lons(:) ! degrees
    real, intent(in) :: lats(:) ! degrees
    integer, intent(in) :: im, jm, lm
    logical, intent(in) :: runparallel
    logical, optional, intent(in) :: LocalTiles
    integer, optional, intent(out) :: rc

    integer :: status

    type (ESMF_Grid) :: grid
    
    type (ESMF_DistGrid) :: dist_grid
    type (ESMF_LocalArray) :: lon_array, lat_array
    
    integer, allocatable :: krank(:)
    type (ESMF_DELayout) :: layout
    
    real, pointer :: lons_radians(:)
    real, pointer :: lats_radians(:)
    integer :: numNodes, k
    integer :: regrid_hints

    if (present(LocalTiles)) then
       if (localtiles) then
           regrid_hints=REGRID_HINT_LOCAL
       else
           regrid_hints=0
       end if
    else
       regrid_hints=REGRID_HINT_LOCAL
    end if

    if (LM > 0) then
      allocate(krank(LM),stat=status)
    else
      allocate(krank(1) ,stat=status)
    end if
    _VERIFY(status)
    if (runParallel .and. (LM > 0) ) then
      numNodes = size(MAPL_NodeRankList)
      call MAPL_RoundRobinPEList(krank,numNodes,rc=status)
      _VERIFY(STATUS)
    else
      krank = 0
    end if
    
    allocate(lons_radians(size(lons)))
    allocate(lats_radians(size(lats)))
    
    lons_radians = MAPL_DEGREES_TO_RADIANS * lons
    lats_radians = MAPL_DEGREES_TO_RADIANS * lats
    
    lon_array = ESMF_LocalArrayCreate(lons_radians, rc=status)
    _VERIFY(status)
    lat_array = ESMF_LocalArrayCreate(lats_radians, rc=status)
    _VERIFY(status)
    
    
    layout = ESMF_DELayoutCreate(petMap=krank, rc=status)
    _VERIFY(status)
    dist_grid = ESMF_DistGridCreate([1,1,1],[IM,JM,LM], &
         & deBlockList=reshape([([[1,1,k],[IM,JM,k]],k=1,LM)],[3,2,LM]), &
         & deLayout = layout, rc=status)
    _VERIFY(status)
    grid = grid_manager%make_grid('LatLon', dist_grid, lon_array, lat_array, rc=status)
    _VERIFY(status)
    
    call ESMF_DistGridDestroy(dist_grid, rc=status)
    _VERIFY(status)
    deallocate(lons_radians, lats_radians)
    call ESMF_LocalArrayDestroy(lon_array)
    _VERIFY(status)
    call ESMF_LocalArrayDestroy(lat_array)
    _VERIFY(status)

    if (method == REGRID_METHOD_CONSERVE .or. method == REGRID_METHOD_VOTE .or. & 
        method == REGRID_METHOD_FRACTION) then    
       regridder => regridder_manager%make_regridder(grid, ESMFGRID, &
            & method, hints=regrid_hints, rc=status)
       _VERIFY(status)
    else
       regridder => regridder_manager%make_regridder(grid, ESMFGRID, &
            & method, hints=regrid_hints, rc=status)
       _VERIFY(status)
    end if

    _RETURN(ESMF_SUCCESS)
    
  end function make_regridder

  function MAPL_CFIOAddCollection(template) result(id)
     character(len=*), intent(in) :: template
      integer :: n
      logical :: found
      type (CFIOCollectionVectorIterator) :: iter
      type (CFIOCollection), pointer :: collection
      type (CFIOCollection) :: c
      integer :: id

      iter = collections%begin()
      n = 1

      ! Is it a new collection?
      found = .false.
      do while (iter /= collections%end())
         collection => iter%get()
         if (template == collection%template) then
            found = .true.
            exit
         end if
         n = n + 1
         call iter%next()
      end do

      if (.not. found) then
         c = new_CFIOCollection(template)
         call collections%push_back(c)
      end if

      id = n

   end function MAPL_CFIOAddCollection

  subroutine GetTIndex(cfio,time,tindex,rc)
     type(ESMF_CFIO)                           :: cfio
     type(ESMF_Time)                           :: time
     integer                                   :: tindex
     integer, optional,          intent(out  ) :: rc

     integer(ESMF_KIND_I4)              :: iyr,imm,idd,ihr,imn,isc
     integer                            :: i,status
     integer(ESMF_KIND_I8)              :: iCurrInterval
     integer                            :: nhmsB, nymdB
     integer                            :: begDate, begTime
     integer(ESMF_KIND_I8),allocatable  :: tSeriesInt(:)
     type(ESMF_TIME)                    :: ctime

     tindex=-1
     allocate(tSeriesInt(cfio%tSteps))
     call getDateTimeVec(cfio%fid,begDate,begTime,tSeriesInt,__RC__)

     do i=1,cfio%tSteps
        iCurrInterval = tSeriesInt(i)
        call GetDate ( begDate, begTime, iCurrInterval, nymdB, nhmsB, status )
        call MAPL_UnpackTime(nymdB,iyr,imm,idd)
        call MAPL_UnpackTime(nhmsB,ihr,imn,isc)
        call ESMF_TimeSet(ctime, yy=iyr, mm=imm, dd=idd,  h=ihr,  m=imn, s=isc,__RC__)
        if (ctime == time) tindex =i
     enddo

     deallocate(tSeriesInt)
     _ASSERT(tindex/=-1, 'tindex should not be -1')
     _RETURN(ESMF_SUCCESS)

  end subroutine GetTIndex

  subroutine MAPL_CFIOGetTimeFromIndex(mcfio,tindex,time,rc)
     type(MAPL_CFIO),intent(inout)             :: mcfio
     type(ESMF_Time),intent(inout)             :: time
     integer, intent(in)                       :: tindex
     integer, optional,          intent(out  ) :: rc

     integer(ESMF_KIND_I4)              :: iyr,imm,idd,ihr,imn,isc
     integer                            :: i,status
     integer(ESMF_KIND_I8)              :: iCurrInterval
     integer                            :: nhmsB, nymdB
     integer                            :: begDate, begTime
     type(ESMF_Time)                    :: ltime
     integer(ESMF_KIND_I8),allocatable  :: tSeriesInt(:)
     logical                            :: found
     type(CFIOCollection), pointer      :: collection
     type(ESMF_CFIO), pointer           :: cfio

     found=.false.

     collection => collections%at(mcfio%collection_ID)
     cfio => collection%find(mcfio%fname)

     allocate(tSeriesInt(cfio%tSteps))
     call getDateTimeVec(cfio%fid,begDate,begTime,tSeriesInt,__RC__)
    
     do i=1,cfio%tSteps
        iCurrInterval = tSeriesInt(i)
        call GetDate ( begDate, begTime, iCurrInterval, nymdB, nhmsB, status )
        call MAPL_UnpackTime(nymdB,iyr,imm,idd)
        call MAPL_UnpackTime(nhmsB,ihr,imn,isc)
        call ESMF_TimeSet(ltime, yy=iyr, mm=imm, dd=idd,  h=ihr,  m=imn, s=isc,__RC__)
        if (i==tindex) then
           time=ltime
           found=.true.
           exit
        end if
     enddo

     deallocate(tSeriesInt)
     _ASSERT(found, 'search failed')
     _RETURN(ESMF_SUCCESS)

  end subroutine MAPL_CFIOGetTimeFromIndex
  
end module MAPL_CFIOMod
