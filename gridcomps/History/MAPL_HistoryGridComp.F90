
#include "MAPL_Generic.h"
#include "unused_dummy.H"
module MAPL_HistoryGridCompMod

!BOP

! !MODULE: MAPL_HistoryGridCompMod

! !USES:

  use ESMF
  use ESMFL_Mod
  use MAPL_BaseMod
  use MAPL_VarSpecMod
  use MAPL_ConstantsMod
  use MAPL_IOMod
  use MAPL_CommsMod
  use MAPL_GenericMod
  use MAPL_LocStreamMod
  use MAPL_CFIOMod
  use MAPL_GenericCplCompMod
  use MAPL_NewArthParserMod
  use MAPL_SortMod
  use MAPL_CFIOServerMod
  use MAPL_ShmemMod
  use MAPL_StringGridMapMod
  use MAPL_GridManagerMod
  use MAPL_ConfigMod
  use, intrinsic :: iso_fortran_env, only: REAL32, REAL64
  use MAPL_HistoryCollectionMod, only: HistoryCollection, FieldSet
  use MAPL_HistoryCollectionVectorMod, only: HistoryCollectionVector
  use MAPL_StringFieldSetMapMod, only: StringFieldSetMap
  use MAPL_StringFieldSetMapMod, only: StringFieldSetMapIterator
  use MAPL_ExceptionHandling
  use MAPL_VerticalDataMod
  use MAPL_TimeDataMod
  use mapl_RegridMethods
  use MAPL_newCFIOitemVectorMod
  use MAPL_newCFIOitemMod
  use pFIO_ClientManagerMod, only: o_Clients
  use HistoryTrajectoryMod
  use MAPL_StringTemplate
  use regex_module
  !use ESMF_CFIOMOD

  implicit none
  private

! !PUBLIC MEMBER FUNCTIONS:

  public SetServices

! !DESCRIPTION: 
!                \input{MAPL_HistoryDescr.tex}
!
!EOP

  type :: SpecWrapper
     type (MAPL_VarSpec),              pointer :: SPEC(:)
  end type SpecWrapper

  type :: ExchangeRegridType
     type(MAPL_LocStreamXform) :: XFORM
     type(MAPL_LocStreamXform) :: XFORMntv
     type(MAPL_LocStream)      :: LocIn
     type(MAPL_LocStream)      :: LocOut
     type(MAPL_LocStream)      :: LocNative
     type(ESMF_State)          :: state_out
     integer                   :: ntiles_in
     integer                   :: ntiles_out
!ALT: this will not be needed when we modify LocStream to take vm instead of layout
     character(len=ESMF_MAXSTR)     :: tilefile
     character(len=ESMF_MAXSTR)     :: gridname
     logical                        :: noxform
     logical                        :: ontiles
     integer                        :: regridType
  end type ExchangeRegridType

  type :: ExchangeRegrid
     type(ExchangeRegridType), pointer :: PTR
  end type ExchangeRegrid

  type :: HISTORY_STATE
     type (HistoryCollection),        pointer :: list(:)       => null()
     type(HistoryCollectionVector) :: collections 
     type (ExchangeRegrid),      pointer :: Regrid(:)     => null()
!     character(len=ESMF_MAXSTR), pointer :: GCNameList(:) => null()
!     type (ESMF_GridComp),       pointer :: gcs(:)        => null()
     type (ESMF_State),          pointer :: GIM(:)        => null()
     type (ESMF_State),          pointer :: GEX(:)        => null()
     type (ESMF_CplComp),        pointer :: CCS(:)        => null()
     type (ESMF_State),          pointer :: CIM(:)        => null()
     type (ESMF_State),          pointer :: CEX(:)        => null()
     type (ESMF_TimeInterval),   pointer :: STAMPOFFSET(:) => null()
     logical,                    pointer :: LCTL(:)       => null()
     logical,                    pointer :: average(:)    => null()
     type (SpecWrapper),         pointer :: SRCS(:)       => null()
     type (SpecWrapper),         pointer :: DSTS(:)       => null()
     type (StringGridMap)                :: output_grids
     type (StringFieldSetMap)           :: field_sets
     character(len=ESMF_MAXSTR)          :: expsrc
     character(len=ESMF_MAXSTR)          :: expid
     character(len=ESMF_MAXSTR)          :: expdsc
     integer                             :: CoresPerNode, mype, npes
     integer                             :: AvoidRootNodeThreshold
     integer                             :: blocksize
     integer                             :: MarkDone
     integer                             :: PrePost
     integer                             :: version
     logical                             :: fileOrderAlphabetical
     integer                             :: collectionWriteSplit
     integer                             :: serverSizeSplit 
  end type HISTORY_STATE
  
  type HISTORY_wrap
     type (HISTORY_STATE), pointer :: PTR
  end type HISTORY_wrap

  type HISTORY_ExchangeListType
     integer*8, pointer                  :: lsaddr_ptr(:) => null()
  end type HISTORY_ExchangeListType

  type HISTORY_ExchangeListWrap
     type(HISTORY_ExchangeListType), pointer :: PTR
  end type HISTORY_ExchangeListWrap

  integer, parameter :: MAPL_G2G = 1
  integer, parameter :: MAPL_T2G = 2
  integer, parameter :: MAPL_T2G2G = 3

  public HISTORY_ExchangeListWrap

  include "mpif.h"

contains

!=====================================================================
  subroutine SetServices ( gc, rc )
    type(ESMF_GridComp), intent(inout) :: gc     ! composite gridded component
    integer, optional               :: rc     ! return code
    
    integer                         :: status
    type (HISTORY_wrap)             :: wrap
    type (HISTORY_STATE), pointer   :: internal_state

! Register services for this component
! ------------------------------------

    call MAPL_GridCompSetEntryPoint ( gc, ESMF_METHOD_INITIALIZE, Initialize, rc=status)
    _VERIFY(status)

    call MAPL_GridCompSetEntryPoint ( gc, ESMF_METHOD_RUN,   Run,       rc=status)
    _VERIFY(status)

    call MAPL_GridCompSetEntryPoint ( gc, ESMF_METHOD_FINALIZE, Finalize,  rc=status)
    _VERIFY(status)

    call MAPL_GridCompSetEntryPoint ( gc, ESMF_METHOD_WRITERESTART, RecordRestart, rc=status)
    _VERIFY(status)

! Allocate an instance of the private internal state...
!------------------------------------------------------

    allocate(internal_state, stat=status)
    _VERIFY(status)

! and save its pointer in the GC
!-------------------------------

    wrap%ptr => internal_state
    call ESMF_GridCompSetInternalState(gc, wrap, status)
    _VERIFY(status)

! Set the Profiling timers
! ------------------------

    call MAPL_TimerAdd (gc,name="Initialize"     ,rc=status)
    call MAPL_TimerAdd (gc,name="Finalize"       ,rc=status)
    call MAPL_TimerAdd (gc,name="Run"            ,rc=status)
    call MAPL_TimerAdd (gc,name="--Couplers"     ,rc=status)
    call MAPL_TimerAdd (gc,name="--I/O"          ,rc=status)
    call MAPL_TimerAdd (gc,name="----IO Create"  ,rc=status)
    call MAPL_TimerAdd (gc,name="----IO Write"   ,rc=status)
    call MAPL_TimerAdd (gc,name="-----IO Post"   ,rc=status)
    call MAPL_TimerAdd (gc,name="-----IO Wait"   ,rc=status)
    call MAPL_TimerAdd (gc,name="-----IO Write"  ,rc=status)
    call MAPL_TimerAdd (gc,name="-ParserRun"     ,rc=status)

! Generic Set Services
! --------------------
    call MAPL_GenericSetServices ( gc,RC=STATUS )
    _VERIFY(STATUS)

    _RETURN(ESMF_SUCCESS)

  end subroutine SetServices

!======================================================
! BOP
! !IROUTINE: Initialize -- Initializes MAPL History Lists for Diagnostic Output

! !INTERFACE:

  subroutine Initialize ( gc, import, dumexport, clock, rc )

! !ARGUMENTS:

    type(ESMF_GridComp), intent(inout)    :: gc     ! composite gridded component 
    type(ESMF_State),       intent(inout) :: import ! import state
    type(ESMF_State),       intent(inout) :: dumexport ! export state
    type(ESMF_Clock),       intent(inout) :: clock  ! the clock
      integer, intent(out), OPTIONAL        :: rc     ! Error code:

! !DESCRIPTION:
! Initialize initializes MAPL History Lists for Diagnostic Output.
! Diagnostics have the following attributes:
!
! \begin{description}
! \item[1)] Diagnostics may be "instantaneous" or "time-averaged"
! \item[2)] Diagnostics have a "frequency" and an associated "ref_date" and "ref_time"
!           from which the frequency is based.  An "end_date" and "end_time" may also be used
!           to turn off diagnostics after a given date and time.
! \item[3)] Time-Averaged Diagnostics have an associated accumulation interval, "acc_interval",
!           which may be <= to the diagnostic "frequency"
! \item[4)] Diagnostics are "time-stamped" with the center of the time-averaged period.
! \item[5)] The default "acc_interval" is the diagnostic "frequency"
! \item[6)] The default "ref_date" is the beginning date of the experiment
! \item[7)] The default "ref_time" is 0z
! \item[8)] The default "end_date" and "end_time" is disabled
! \end{description}
!
! Through the use of History Lists, the user may define the type of diagnostic output desired.
! History Lists contain the following attributes:
!
! \begin{description}
! \item[filename]     Character string defining the filename of a particular diagnostic output stream.
! \item[template]     Character string defining the time stamping template following GrADS convensions. The default value depends on the duration of the file.
! \item[format]       Character string defining file format ("flat" or "CFIO" or "CFIOasync"). Default = "flat".
! \item[mode]         Character string equal to "instantaneous" or "time-averaged". Default = "instantaneous".
! \item[descr]        Character string equal to the list description. Defaults to "expdsc".
! \item[frequency]    Integer (HHMMSS) for the frequency of output.  Default = 060000.
! \item[acc_interval] Integer (HHMMSS) for the acculation interval (<= frequency) for time-averaged diagnostics.
!                     Default = Diagnostic Frequency.
! \item[ref_date]     Integer (YYYYMMDD) reference date from which the frequency is based.
!                     Default is the Experiment beginning date.
! \item[ref_time]     Integer (HHMMSS) reference time from which the frequency is based.
!                     Default is 000000.
! \item[end_date]     Integer (YYYYMMDD) ending date to stop diagnostic output.  Default is disabled.
! \item[end_time]     Integer (HHMMSS) ending time to stop diagnostic output. Default is disabled.
! \item[duration]     Integer (HHMMSS) for the duration of each file.  Default = frequency (1 time-record per file).
! \item[fields]       Paired character strings for the diagnostic Name and its associated Gridded Component.
! \item[subset]       Optional subset (lonMin lonMax latMin latMax) for the output
! \item[xyoffset]     Optional Flag for Grid Staggering (0:DcPc, 1:DePc, 2:DcPe, 3:DePe)
! \item[levels]       Optional list of output levels (Default is all levels on Native Grid).
! \item[vvars]        Optional Field (and Transform) to use for Vertical Interpolation (eg., 'log(PLE)' , 'DYN' ).
! \item[vunit]        Optional Units to use for Vertical Index of Output File.
! \item[vscale]       Optional Scaling to use between Output Unit and VVARS unit.
! \end{description}
!
! !REVISION HISTORY:
!   14Jan2005 Todling  Implemented GRADS template-ready CFIO filename.
! EOP

    integer                         :: status

    logical                         :: errorFound
    logical                         :: found
    type(HistoryCollection), pointer     :: list(:)
    type(HISTORY_wrap)              :: wrap
    type (HISTORY_STATE), pointer   :: IntState
    type(HISTORY_ExchangeListWrap)  :: lswrap

    type(ESMF_State), pointer      :: export (:) => null()
    type(ESMF_State), pointer      :: exptmp (:)
    type(ESMF_Time)                :: StartTime
    type(ESMF_Time)                :: CurrTime
    type(ESMF_Time)                ::  RingTime
    type(ESMF_Time)                ::   RefTime
    type(ESMF_TimeInterval)        :: Frequency
    type(ESMF_Array)               :: array
    type(ESMF_Field)               :: field
    type(ESMF_Field)               :: f
    type(ESMF_Calendar)            ::  cal
    type(ESMF_Config)              :: config
    type(ESMF_DELayout)            :: layout
    type(MAPL_MetaComp), pointer   :: GENSTATE

    character(len=ESMF_MAXSTR)     :: string
    character(len=ESMF_MAXSTR)     :: tmpstring
    character(len=ESMF_MAXSTR)     :: tilefile
    character(len=ESMF_MAXSTR)     :: gridname
    character(len=MAPL_TileNameLength), pointer :: gnames(:)
    integer                        :: L, LM
    integer                        :: NG
    integer                        :: NGRIDS
    integer                        :: COUNTS(ESMF_MAXDIM)
    integer                        :: I1,J1
    integer                        :: dimCount
    real, pointer                  :: levels(:)
    integer                        :: DIMS
    integer                        :: VLOCATION
    integer                        :: FIELD_TYPE
    integer                        :: avgint
    integer                        :: REFRESH
    character(ESMF_MAXSTR)         :: SHORT_NAME
    character(ESMF_MAXSTR)         :: LONG_NAME
    character(ESMF_MAXSTR)         :: UNITS
    character(ESMF_MAXSTR), pointer:: VVARn(:)
    character(ESMF_MAXSTR)         :: VVAR
    character(ESMF_MAXSTR), pointer:: fields (:,:)
    character(ESMF_MAXSTR)         :: export_name
    character(ESMF_MAXSTR)         :: component_name
    character(ESMF_MAXSTR)         :: export_alias
    character(ESMF_MAXSTR)         :: coupler_function_name
    logical                        :: tend
    character(len=ESMF_MAXSTR),allocatable :: statelist(:)
    logical,                   allocatable :: statelistavail(:)
    character(len=ESMF_MAXSTR),allocatable ::   tmplist(:)
    
    integer :: nlist,unit,nstatelist
    integer :: k,m,n,sec,rank,size0
    integer :: year,month,day,hour,minute,second,nymd0,nhms0,nymdc,nhmsc
    integer :: ref_time(6)
    integer :: len, i, j, mype, npes, nx, ny

    type (ESMF_Grid)                          :: grid
    type (ESMF_Grid), pointer                 :: pgrid
    type (ESMF_Grid)                          :: grid_attached
    type (ESMF_DistGrid)                      :: distgrid
    type (ESMF_Grid)                          :: grid_in, grid_out
    type (MAPL_LocStream)                     :: exch
    type (MAPL_LocStream)                     :: locstream
    type (ESMF_VM)                            :: vm
    type(ESMF_TypeKind_Flag)                  :: tk
    logical                                   :: use_this_gridname
    logical                                   :: ontiles
    logical                                   :: disableSubVmChecks
    character(len=ESMF_MAXSTR)                :: tmpstr, attachedName
    integer                                   :: localStatus, globalStatus
    integer, pointer :: allPes(:)
    integer          :: localPe(1), nactual, minactual
    integer*8                                 :: ADDR
    integer*8, pointer                        :: LSADDR_PTR(:) => null()
    type(ESMF_State)                          :: state_out
    integer                                   :: fieldRank, gridRank
    integer                                   :: undist
    integer, allocatable                      :: ungrd(:)
    integer                                   :: ungridDims
    integer                                   :: notGridded
    logical                                   :: hasUngridDims
    character(len=ESMF_MAXSTR)                :: ungridded_name, ungridded_unit
    integer                                   :: ungrdSize
    real,    allocatable                      :: ungridded_coord(:)
    integer, allocatable                      :: gridToFieldMap(:)
    integer, allocatable                      :: ungriddedLBound(:)
    integer, allocatable                      :: ungriddedUBound(:)
    type (ESMF_LocalArray), target            :: larrayList(1)
    type (ESMF_LocalArray), pointer           :: larray
    integer                                   :: c
    logical                                   :: isFileName
    logical                                   :: fileExists
    logical                                   :: isPresent
    real                                      :: lvl

    integer                                   :: mntly
    integer                                   :: spltFld
    integer                                   :: useRegex
    integer                                   :: unitr, unitw
    integer                                   :: tm,resolution(2)
    logical                                   :: match, contLine
    character(len=2048)                       :: line
    type(ESMF_Config)                         :: cfg
    character(len=ESMF_MAXSTR)                :: HIST_CF
    character(len=ESMF_MAXSTR)                :: BLANK=""

!   Parser Variables
    logical          :: DoCopy
    type(ESMF_State) :: parser_state
    type(ESMF_Field) :: parser_field

!   Async cfio option
    type(MAPL_Communicators)       :: mapl_comm
    logical                        :: doAsync

!   Single colum flag used to set different defalut for TM
    integer                        :: snglcol
    integer                        :: tm_default

!   variable for vector handling
    integer                        :: idx
    character(len=ESMF_MAXSTR)     :: f1copy, f3copy

!   variables for "backwards" mode
    integer                        :: reverse

!   variables for "newFormat" mode
    integer                        :: newFormat
    integer                        :: cubeFormat

!   variables for proper counting the number of slices to include tile-grids
    type (ESMF_Grid)     :: bgrid
    type (ESMF_DistGrid) :: bdistgrid
    integer              :: nslices
    integer              :: distRank

    type(ESMF_Field)     :: r4field

    integer              :: chnksz
    logical :: table_end
    logical :: old_fields_style

    type(HistoryCollection) :: collection
    character(len=ESMF_MAXSTR) :: cFileOrder
    type(FieldSet), pointer :: field_set
    type(FieldSet), pointer :: fld_set
    type(FieldSet), pointer :: newFieldSet => null()
    character(len=:), pointer :: key
    type(StringFieldSetMapIterator) :: field_set_iter
    character(ESMF_MAXSTR) :: field_set_name
    integer :: collection_id
    logical, allocatable :: needSplit(:)
    type(ESMF_Field), allocatable :: fldList(:)
    character(len=ESMF_MAXSTR), allocatable :: regexList(:)
    
! Begin
!------

    _UNUSED_DUMMY(dumexport)

    call MAPL_GetObjectFromGC ( gc, GENSTATE, RC=STATUS)
    _VERIFY(STATUS)

    call MAPL_TimerOn(GENSTATE,"TOTAL")
    call MAPL_TimerOn(GENSTATE,"Initialize")

! Retrieve the pointer to the state
    call ESMF_GridCompGetInternalState(gc, wrap, status)
    _VERIFY(status)
    IntState => wrap%ptr

    call ESMF_UserCompGetInternalState(GC, 'MAPL_LocStreamList', &
        lswrap, STATUS)
    if (status == ESMF_SUCCESS) then
       lsaddr_ptr => lswrap%ptr%lsaddr_ptr
    end if

    call ESMF_GridCompGet(gc, vm=vm, rc=status)
    _VERIFY(status)

    call ESMF_VMGetCurrent(vm, rc=status)
    _VERIFY(status)
    call ESMF_VMGet       (VM, localpet=MYPE, petcount=NPES,  RC=STATUS)
    _VERIFY(STATUS)

    IntState%mype = mype
    IntState%npes = npes


! Get Clock StartTime for Default ref_date, ref_time
! --------------------------------------------------
    call ESMF_ClockGet ( clock,     calendar=cal,       rc=STATUS ) ; _VERIFY(STATUS)
    call ESMF_ClockGet ( clock,     currTime=CurrTime,  rc=STATUS ) ; _VERIFY(STATUS)
    call ESMF_ClockGet ( clock,     StartTime=StartTime,rc=STATUS ) ; _VERIFY(STATUS)
    call ESMF_TimeGet  ( StartTime, TimeString=string  ,rc=STATUS ) ; _VERIFY(STATUS)
    
    read(string( 1: 4),'(i4.4)') year
    read(string( 6: 7),'(i2.2)') month
    read(string( 9:10),'(i2.2)') day
    read(string(12:13),'(i2.2)') hour
    read(string(15:16),'(i2.2)') minute
    read(string(18:18),'(i2.2)') second
    
    nymd0 =  year*10000 +  month*100 + day
    nhms0 =  hour*10000 + minute*100 + second

    call ESMF_TimeGet  ( CurrTime, TimeString=string  ,rc=STATUS ) ; _VERIFY(STATUS)
    
    read(string( 1: 4),'(i4.4)') year
    read(string( 6: 7),'(i2.2)') month
    read(string( 9:10),'(i2.2)') day
    read(string(12:13),'(i2.2)') hour
    read(string(15:16),'(i2.2)') minute
    read(string(18:18),'(i2.2)') second
    
    nymdc =  year*10000 +  month*100 + day
    nhmsc =  hour*10000 + minute*100 + second

! Read User-Supplied History Lists from Config File
! -------------------------------------------------
    call ESMF_GridCompGet( gc, config=config, rc=STATUS ) ; _VERIFY(STATUS)

    call ESMF_ConfigGetAttribute ( config, value=INTSTATE%expsrc, &
                                   label ='EXPSRC:', default='', rc=status )
    _VERIFY(STATUS)
    call ESMF_ConfigGetAttribute ( config, value=INTSTATE%expid, &
                                   label ='EXPID:', default='', rc=status )
    _VERIFY(STATUS)
    call ESMF_ConfigGetAttribute ( config, value=INTSTATE%expdsc, &
                                   label ='EXPDSC:', default='', rc=status )
    _VERIFY(STATUS)
    call ESMF_ConfigGetAttribute ( config, value=INTSTATE%CoresPerNode, &
                                   label ='CoresPerNode:', default=min(npes,8), rc=status )
    _VERIFY(STATUS)
    call ESMF_ConfigGetAttribute ( config, value=disableSubVmChecks, &
                                   label ='DisableSubVmChecks:', default=.false., rc=status )
    _VERIFY(STATUS)
    call ESMF_ConfigGetAttribute ( config, value=INTSTATE%AvoidRootNodeThreshold, &
                                   label ='AvoidRootNodeThreshold:', default=1024, rc=status )
    _VERIFY(STATUS)

    call ESMF_ConfigGetAttribute(config, value=INTSTATE%blocksize,         &
                                         label='BlockSize:', default=10, rc=status)
    _VERIFY(STATUS)

    call ESMF_ConfigGetAttribute(config, value=cFileOrder,         &
                                         label='FileOrder:', default='ABC', rc=status)
    _VERIFY(STATUS)
    if (trim(cFileOrder) == 'ABC') then
       intstate%fileOrderAlphabetical = .true.
    else if (trim(cFileOrder) == 'AddOrder') then
       intstate%fileOrderAlphabetical = .false.
    else
       _ASSERT(.false.,'needs informative message')
    end if

    call ESMF_ConfigGetAttribute(config, value=IntState%collectionWriteSplit, &
         label = 'CollectionWriteSplit:', default=0, rc=status)
    _VERIFY(status)
    call ESMF_ConfigGetAttribute(config, value=IntState%serverSizeSplit, &
         label = 'ServerSizeSplit:', default=0, rc=status)
    _VERIFY(status)
    call o_Clients%split_server_pools(n_server_split = IntState%serverSizeSplit, &
                                      n_hist_split   = IntState%collectionWriteSplit,rc=status)
    _VERIFY(status)

    call ESMF_ConfigGetAttribute(config, value=INTSTATE%MarkDone,          &
                                         label='MarkDone:', default=0, rc=status)
    _VERIFY(STATUS)
    call ESMF_ConfigGetAttribute(config, value=INTSTATE%PrePost,          &
                                         label='PrePost:', default=1, rc=status)
    _VERIFY(STATUS)


    call ESMF_ConfigGetAttribute(config, value=snglcol,          &
                                         label='SINGLE_COLUMN:', default=0, rc=status)
    _VERIFY(STATUS)
    call ESMF_ConfigGetAttribute(config, value=intstate%version,          &
                                         label='VERSION:', default=0, rc=status)
    _VERIFY(STATUS)
    if( MAPL_AM_I_ROOT() ) then
       print *
       print *, 'EXPSRC:',trim(INTSTATE%expsrc)
       print *, 'EXPID: ',trim(INTSTATE%expid)
       print *, 'Descr: ',trim(INTSTATE%expdsc)
       print *, 'DisableSubVmChecks:', disableSubVmChecks
       print *, 'BlockSize: '        , INTSTATE%blocksize
       print *, 'MarkDone:  '        , INTSTATE%MarkDone
       print *, 'PrePost:   '        , INTSTATE%PrePost
       print *
    endif

! Determine Number of Output Streams
! ----------------------------------
    if( MAPL_AM_I_ROOT() ) then
       print *, 'Reading HISTORY RC Files:'
       print *, '-------------------------'
    endif

    call ESMF_ConfigFindLabel ( config,'COLLECTIONS:',rc=STATUS )
    _VERIFY(STATUS)
    tend  = .false.
    nlist = 0
    allocate(IntState%list(nlist), stat=status)
    _VERIFY(STATUS)
    do while (.not.tend)
          call ESMF_ConfigGetAttribute ( config,value=tmpstring,default='',rc=STATUS) !ALT: we don't check return status!!!
          if (tmpstring /= '')  then
             
             collection%collection = tmpstring
             collection%filename = tmpstring
             call IntState%collections%push_back(collection)             
             
             nlist = nlist + 1
             allocate( list(nlist), stat=status )
             _VERIFY(STATUS)
             list(1:nlist-1)=IntState%list
             list(nlist)%collection = tmpstring
             list(nlist)%filename = list(nlist)%collection
             deallocate(IntState%list)
             IntState%list => list
          end if
          call ESMF_ConfigNextLine     ( config,tableEnd=tend,rc=STATUS )
          _VERIFY(STATUS)
    enddo
    
    if (nlist == 0) then
       _RETURN(ESMF_SUCCESS)
    end if

    if (intstate%version >= 1) then
       OUTPUT_GRIDS: block
         type (ESMF_Grid) :: output_grid
         type (StringGridMapIterator) :: iter
         integer :: nl
         character(len=60) :: grid_type
         
         call ESMF_ConfigFindLabel ( config,'GRID_LABELS:',rc=STATUS )
         _VERIFY(status)
         tend  = .false.
         do while (.not.tend)
             call ESMF_ConfigGetAttribute ( config,value=tmpstring,default='',rc=STATUS) !ALT: we don't check return status!!!
             if (tmpstring /= '')  then
                call IntState%output_grids%insert(trim(tmpString), output_grid)
             end if
             call ESMF_ConfigNextLine     ( config,tableEnd=tend,rc=STATUS )
             _VERIFY(STATUS)
          enddo
          
          iter = IntState%output_grids%begin()
          do while (iter /= IntState%output_grids%end())
             key => iter%key()
             call ESMF_ConfigGetAttribute(config, value=grid_type, label=trim(key)//".GRID_TYPE:",rc=status)
             _VERIFY(status)
             if (trim(grid_type)=='Cubed-Sphere') then
                call MAPL_MakeDecomposition(nx,ny,reduceFactor=6,rc=status)
                _VERIFY(status)
             else
                call MAPL_MakeDecomposition(nx,ny,rc=status)
                _VERIFY(status)
             end if
             call MAPL_ConfigSetAttribute(config, value=nx,label=trim(key)//".NX:",rc=status)
             call MAPL_ConfigSetAttribute(config, value=ny,label=trim(key)//".NY:",rc=status)
             output_grid = grid_manager%make_grid(config, prefix=key//'.', rc=status)
             _VERIFY(status)
             call IntState%output_grids%set(key, output_grid)
             call iter%next()
          end do
       end block OUTPUT_GRIDS
    end if

    if (intstate%version >= 2) then
       call ESMF_ConfigFindLabel(config, 'FIELD_SETS:', rc=status)
       _VERIFY(status)
       table_end = .false.
       do while (.not. table_end)
          call ESMF_ConfigGetAttribute ( config, value=tmpstring,default='',rc=STATUS) !ALT: we don't check return status!!!
          if (tmpstring /= '')  then
             ! Add empty FieldSet to dictionary of field collections
             allocate(field_set)
             call intstate%field_sets%insert(trim(tmpString), field_set)
             deallocate(field_set)
          end if
          call ESMF_ConfigNextLine     ( config,tableEnd=table_end,rc=STATUS )
          _VERIFY(STATUS)
       enddo

       field_set_iter = intState%field_sets%begin()
       do while (field_set_iter /= intState%field_sets%end())
          key => field_set_iter%key()
          field_set => field_set_iter%value()
          call parse_fields(config, key, field_set, rc=status)
          _VERIFY(status)
          call field_set_iter%next()
       end do

    end if
       

    allocate(IntState%Regrid(nlist), stat=STATUS)
    _VERIFY(STATUS)
    allocate(          Vvarn(nlist), stat=STATUS)
    _VERIFY(STATUS)
    allocate(INTSTATE%STAMPOFFSET(nlist), stat=status)
    _VERIFY(STATUS)

! We are parsing HISTORY config file to split each collection into separate RC
! ----------------------------------------------------------------------------

    if( MAPL_AM_I_ROOT(vm) ) then

       call ESMF_ConfigGetAttribute(config, value=HIST_CF, &
            label="HIST_CF:", default="HIST.rc", RC=STATUS ) 
       _VERIFY(STATUS)
       unitr = GETFILE(HIST_CF, FORM='formatted', RC=status)
       _VERIFY(STATUS)

!       for each collection
       do n = 1, nlist
         rewind(unitr)
         string = trim( list(n)%collection ) // '.'
         unitw = GETFILE(trim(string)//'rcx', FORM='formatted', RC=status)

         match = .false.
         contLine = .false.

         do while (.true.)
            read(unitr, '(A)', end=1234) line 
            j = index( adjustl(line), trim(adjustl(string)) )
            match = (j == 1)
            if (match) then
               j = index(line, trim(string)//'fields:')
               contLine = (j > 0)
            end if
            if (match .or. contLine) then
               write(unitw,'(A)') trim(line)
            end if
            if (contLine) then
               if (adjustl(line) == '::') contLine = .false.
            end if

         end do

1234     continue
         call free_file(unitw, rc=status)
         _VERIFY(STATUS)
      end do

      call free_file(unitr, rc=status)
      _VERIFY(STATUS)

    end if


    call ESMF_VMbarrier(vm, RC=status)
    _VERIFY(STATUS)

! Initialize History Lists
! ------------------------

    allocate(INTSTATE%AVERAGE    (nlist), stat=status)
    _VERIFY(STATUS)
    allocate(INTSTATE%STAMPOFFSET(nlist), stat=status)
    _VERIFY(STATUS)

    IntState%average = .false.
    LISTLOOP: do n=1,nlist

       list(n)%unit = 0

       string = trim( list(n)%collection ) // '.'

       if (trim(list(n)%filename) == "/dev/null") then
          list(n)%disabled = .true.
       else
          list(n)%disabled = .false.
       end if
       
       list(n)%monthly = .false.
       list(n)%splitField = .false.
       list(n)%regex = .false.

       cfg = ESMF_ConfigCreate(rc=STATUS)
       _VERIFY(STATUS)

       call ESMF_ConfigLoadFile(cfg, filename = trim(string)//'rcx', rc=status)
       _VERIFY(STATUS)
       call ESMF_ConfigGetAttribute ( cfg, value=list(n)%template, default="", &
                                      label=trim(string) // 'template:' ,rc=status )
       _VERIFY(STATUS)
       call ESMF_ConfigGetAttribute ( cfg, value=list(n)%format,default='flat', &
                                      label=trim(string) // 'format:' ,rc=status )
       _VERIFY(STATUS)
       call ESMF_ConfigGetAttribute ( cfg, value=list(n)%mode,default='instantaneous', &
                                      label=trim(string) // 'mode:' ,rc=status )
       _VERIFY(STATUS)
       call ESMF_ConfigGetAttribute ( cfg, value=list(n)%descr, &
                                      default=INTSTATE%expdsc, &
                                      label=trim(string) // 'descr:' ,rc=status )
       _VERIFY(STATUS)

       call ESMF_ConfigGetAttribute ( cfg, mntly, default=0, &
	                              label=trim(string) // 'monthly:',rc=status )
       _VERIFY(STATUS)
       list(n)%monthly = (mntly /= 0)
       call ESMF_ConfigGetAttribute ( cfg, spltFld, default=0, &
	                              label=trim(string) // 'splitField:',rc=status )
       _VERIFY(STATUS)
       list(n)%splitField = (spltFld /= 0)
       call ESMF_ConfigGetAttribute ( cfg, useRegex, default=0, &
	                              label=trim(string) // 'UseRegex:',rc=status )
       _VERIFY(STATUS)
       list(n)%regex = (useRegex /= 0)
       call ESMF_ConfigGetAttribute ( cfg, list(n)%frequency, default=060000, &
	                              label=trim(string) // 'frequency:',rc=status )
       _VERIFY(STATUS)
       call ESMF_ConfigGetAttribute ( cfg, list(n)%acc_interval, default=list(n)%frequency, &
	                              label=trim(string) // 'acc_interval:',rc=status )
       _VERIFY(STATUS)

       call ESMF_ConfigGetAttribute ( cfg, list(n)%ref_date, default=nymdc, &
	                              label=trim(string) // 'ref_date:',rc=status )
       _VERIFY(STATUS)
       call ESMF_ConfigGetAttribute ( cfg, list(n)%ref_time, default=000000, &
                                      label=trim(string) // 'ref_time:',rc=status )
       _VERIFY(STATUS)

       call ESMF_ConfigGetAttribute ( cfg, list(n)%end_date, default=-999, &
	                              label=trim(string) // 'end_date:',rc=status )
       _VERIFY(STATUS)
       call ESMF_ConfigGetAttribute ( cfg, list(n)%end_time, default=-999, &
                                      label=trim(string) // 'end_time:',rc=status )
       _VERIFY(STATUS)

       call ESMF_ConfigGetAttribute ( cfg, list(n)%duration, default=list(n)%frequency, &
	                              label=trim(string) // 'duration:'  ,rc=status )
       _VERIFY(STATUS)
       call ESMF_ConfigGetAttribute ( cfg, list(n)%verbose, default=0, &
	                              label=trim(string) // 'verbose:'  ,rc=status )
       _VERIFY(STATUS)

       call ESMF_ConfigGetAttribute ( cfg, list(n)%vscale, default=1.0, &
	                              label=trim(string) // 'vscale:'  ,rc=status )
       _VERIFY(STATUS)
       call ESMF_ConfigGetAttribute ( cfg, list(n)%vunit, default="", &
	                              label=trim(string) // 'vunit:'  ,rc=status )
       _VERIFY(STATUS)
       call ESMF_ConfigGetAttribute ( cfg, list(n)%nbits, default=100, &
                                      label=trim(string) // 'nbits:' ,rc=status )
       _VERIFY(STATUS)
       call ESMF_ConfigGetAttribute ( cfg, list(n)%deflate, default=0, &
                                      label=trim(string) // 'deflate:' ,rc=status )
       _VERIFY(STATUS)

       tm_default = -1
       call ESMF_ConfigGetAttribute ( cfg, list(n)%tm, default=tm_default, &
                                      label=trim(string) // 'tm:', rc=status )
       _VERIFY(STATUS)
       call ESMF_ConfigGetAttribute ( cfg, list(n)%conservative, default=0, &
	                              label=trim(string) // 'conservative:'  ,rc=status )
       _VERIFY(STATUS)
       if (list(n)%conservative==0) then
          list(n)%conservative=REGRID_METHOD_BILINEAR
       else if (list(n)%conservative==1) then
          list(n)%conservative=REGRID_METHOD_CONSERVE
       end if

! Get an optional file containing a 1-D track for the output
       call ESMF_ConfigGetAttribute(cfg, value=list(n)%trackFile, default="", &
                                    label=trim(string) // 'track_file:', rc=status)
       if (trim(list(n)%trackfile) /= '') list(n)%timeseries_output = .true.
       call ESMF_ConfigGetAttribute(cfg, value=list(n)%recycle_track, default=.false., &
                                    label=trim(string) // 'recycle_track:', rc=status)

! Handle "backwards" mode: this is hidden (i.e. not documented) feature
! Defaults to .false.
       call ESMF_ConfigGetAttribute ( cfg, reverse, default=0, &
	                              label=trim(string) // 'backwards:'  ,rc=status )
       _VERIFY(STATUS)
       list(n)%backwards = (reverse /= 0)

!      Disable streams when frequencies, times are negative
!      ----------------------------------------------------
       if ( list(n)%frequency < 0 .OR. &
            list(n)%ref_date  < 0 .OR. &
            list(n)%ref_time  < 0 .OR. &
            list(n)%duration  < 0      )   list(n)%disabled = .true.


       old_fields_style = .true. ! unless
       if (intstate%version >= 2) then
          call ESMF_ConfigGetAttribute ( cfg, value=field_set_name, label=trim(string)//'field_set:', &
               & default='', rc=status)
          _VERIFY(status)
          if (field_set_name /= '') then  ! field names already parsed
             old_fields_style = .false.
             field_set => intstate%field_sets%at(trim(field_set_name))
             _ASSERT(associated(field_set),'needs informative message')
          end if
       end if

       if (old_fields_style) then
          field_set_name = trim(string) // 'fields'
          allocate(field_set)
          call parse_fields(cfg, trim(field_set_name), field_set, list(n)%items, rc=status)
       end if

       list(n)%field_set => field_set

! Get an optional list of output levels
! -------------------------------------

       list(n)%vvars = ""
 
       len = ESMF_ConfigGetLen( cfg, label=trim(trim(string) // 'levels:'), rc = status )

       LEVS: if( status == ESMF_SUCCESS ) then
          call ESMF_ConfigFindLabel( cfg, label=trim(trim(string) // 'levels:'), rc = status )
          _VERIFY(STATUS)
             j = 0
          do i = 1, len
             call ESMF_ConfigGetAttribute ( cfg,value=tmpstring ,rc=STATUS)
             _VERIFY(STATUS)
             if( trim(tmpstring) == ',' )  cycle
             j = j + 1

             ! Allow for possibility that levels could point to a file
             isFileName = .false.
             if (j == 1) then 
                !ALT: only the first non-comma entry could be filename
                tmpstring = trim(adjustl(tmpstring))
                l = len_trim(tmpstring)
                do k = 1,l
                   c = ichar(tmpstring(k:k))
                   if((c > 64 .and. c < 91) .or. (c>96 .and. c < 123)) then
                      isFileName = .true.
                      exit
                   end if
                end do

                if (isFileName) then
                   INQUIRE ( FILE=trim(tmpstring), EXIST=fileExists )
                   _ASSERT(fileExists,'needs informative message')

                   unit = GETFILE(trim(tmpstring), form='formatted', rc=status)
                   _VERIFY(STATUS)

                   if (MAPL_Am_I_Root(vm)) then
                      k=0
                      do while (.true.)
                         read(unit, *, end=987) lvl
                         k = k+1
                      end do
987                   continue

                   end if
             
                   call MAPL_CommsBcast(vm, DATA=k, N=1, ROOT=MAPL_Root, RC=status)
                   _VERIFY(STATUS)

                   allocate( list(n)%levels(k), stat = status )  
                   _VERIFY(STATUS)

                   if (MAPL_Am_I_Root(vm)) then
                      rewind(unit)
                      do l=1,k
                         read(unit, *) list(n)%levels(l)
                      end do
                   end if
             
                   call MAPL_CommsBcast(vm, DATA=list(n)%levels, N=k, &
                        ROOT=MAPL_Root, RC=status)
                   _VERIFY(STATUS)

                   call FREE_FILE(UNIT)
                end if
             end if

             if(isFileName) cycle

             allocate( levels(j), stat = status )
             _VERIFY(STATUS)
                     i1 = index(tmpstring(:),",")
                 if( i1.eq.1 )  tmpstring = adjustl( tmpstring(2:)   )
                     j1 = index(tmpstring(:),",")-1
                 if( j1.gt.0 )  tmpstring = adjustl( tmpstring(1:j1) )
             read(tmpstring,*)  levels(j)
             if( j.eq.1 ) then
                 allocate( list(n)%levels(j), stat = status )  
                 _VERIFY(STATUS)
                 list(n)%levels(j) = levels(j)
             else
                 levels(1:j-1) = list(n)%levels(:)
                 deallocate( list(n)%levels )
                   allocate( list(n)%levels(j), stat = status )  
                   _VERIFY(STATUS)
                   list(n)%levels(:) = levels(:)
             endif
             deallocate( levels )
          enddo

! Get an interpolating variable
! -----------------------------

          call ESMF_ConfigFindLabel ( cfg,trim(string) // 'vvars:',isPresent=isPresent,rc=STATUS )
          VINTRP: if(isPresent) then

             call ESMF_ConfigGetAttribute ( cfg,value=list(n)%vvars(1), rc=STATUS)
             _VERIFY(STATUS) 
             i = index(list(n)%vvars(1)(  1:),"'")
             j = index(list(n)%vvars(1)(i+1:),"'")+i
             if( i.ne.0 ) then
                 list(n)%vvars(1) = adjustl( list(n)%vvars(1)(i+1:j-1) )
             else
                 list(n)%vvars(1) = adjustl( list(n)%vvars(1) )
             endif

             call ESMF_ConfigGetAttribute ( cfg,value=tmpstring ,rc=STATUS)
             _VERIFY(STATUS)
             if( trim(tmpstring) == ',' )  then
                 call ESMF_ConfigGetAttribute ( cfg,value=list(n)%vvars(2),rc=STATUS)
                 _VERIFY(STATUS)
             else
                 list(n)%vvars(2) = tmpstring
             endif
             i = index(list(n)%vvars(2)(  1:),"'")
             j = index(list(n)%vvars(2)(i+1:),"'")+i
             if( i.ne.0 ) then
                 list(n)%vvars(2) = adjustl( list(n)%vvars(2)(i+1:j-1) )
             else
                 list(n)%vvars(2) = adjustl( list(n)%vvars(2) )
             endif

! Add Vertical Coordinate Variables to Field List (if not already present)
! ------------------------------------------------------------------------

             list(n)%vvars(1) = trim(adjustl(list(n)%vvars(1)))
             vvar = adjustl(list(n)%vvars(1))
             if(vvar/="") then
                if    (Vvar(1:3)=='log') then
                   Vvar  = adjustl(Vvar(index(vvar,'(')+1:index(vvar,')')-1))
                elseif(Vvar(1:3)=='pow') then 
                   Vvar  = adjustl(Vvar(index(vvar,'(')+1:index(vvar,',')-1))
                endif

                do i=1,list(n)%field_set%nfields
                   found = list(n)%field_set%fields(1,i).eq.vvar   .and. &
                        list(n)%field_set%fields(2,i).eq.list(n)%vvars(2)
                   if(found)exit
                enddo

                if( .not.found ) then
                   list(n)%field_set%nfields = list(n)%field_set%nfields + 1
                   allocate( fields(4,  list(n)%field_set%nfields), stat=status )
                   fields(1,1:list(n)%field_set%nfields-1) = list(n)%field_set%fields(1,:)
                   fields(2,1:list(n)%field_set%nfields-1) = list(n)%field_set%fields(2,:)
                   fields(3,1:list(n)%field_set%nfields-1) = list(n)%field_set%fields(3,:)
                   fields(4,1:list(n)%field_set%nfields-1) = list(n)%field_set%fields(4,:)
                   fields(1,  list(n)%field_set%nfields  ) = Vvar
                   fields(2,  list(n)%field_set%nfields  ) = list(n)%vvars (2)
                   fields(3,  list(n)%field_set%nfields  ) = Vvar
                   fields(4,  list(n)%field_set%nfields  ) = BLANK
                   deallocate( list(n)%field_set%fields, stat=status )
                   _VERIFY(STATUS)
                   list(n)%field_set%fields => fields
                endif
             end if
          endif VINTRP ! Vertical interp var

       endif LEVS ! selected levels

       vvarn(n) = vvar

       cubeFormat = 1
       list(n)%xyoffset = 0
       ! Determine the file-side grid to use for the collection.
       select case (intstate%version)
       case(1:)
          call ESMF_ConfigGetAttribute ( cfg, tmpString, default='' , &
                                         label=trim(string) // 'grid_label:' ,rc=status )
          _VERIFY(status)
          if (len_trim(tmpString) == 0) then
             list(n)%output_grid_label=''
          else
             cubeFormat = 0
             i1 = index(tmpstring(:),",")
             if( i1.eq.1 )  tmpstring = adjustl( tmpstring(2:)   )
             j1 = index(tmpstring(:),",")-1
             if( j1.gt.0 )  tmpstring = adjustl( tmpstring(1:j1) )
             pgrid => IntState%output_grids%at(trim(tmpString))
             ! If user specifies a grid label, then it is required.
             ! Do not default to native in this case
             _ASSERT(associated(pgrid),'needs informative message')
             list(n)%output_grid_label = trim(tmpString)
          end if
       case(0)
          call ESMF_ConfigFindLabel( cfg, label=trim(trim(string) // 'resolution:'), rc = status )
          if (status==ESMF_SUCCESS) then
             cubeFormat = 0
             j = 0
             do i = 1,2
                call ESMF_ConfigGetAttribute ( cfg,value=tmpstring ,rc=STATUS)
                _VERIFY(STATUS)
                if( trim(tmpstring) == ',' )  cycle
                j = j + 1
                _ASSERT(j<=2,'needs informative message')
                        i1 = index(tmpstring(:),",")
                    if( i1.eq.1 )  tmpstring = adjustl( tmpstring(2:)   )
                        j1 = index(tmpstring(:),",")-1
                    if( j1.gt.0 )  tmpstring = adjustl( tmpstring(1:j1) )
                read(tmpstring,*)  resolution(j)
             enddo
             call list(n)%AddGrid(IntState%output_grids,resolution,rc=status)
             _VERIFY(status)
          else
             list(n)%output_grid_label=''
          end if
       end select

! Handle "useNewFormat" mode: this is hidden (i.e. not documented) feature
! Affects only "new" cubed-sphere native output
! Defaults to .true.
       newFormat = cubeFormat
       if (cubeFormat /= 0) then
          call ESMF_ConfigGetAttribute ( cfg, newFormat, default=cubeFormat, &
	                                 label=trim(string) // 'cubeFormat:'  ,rc=status )
          _VERIFY(STATUS)
       end if
       list(n)%useNewFormat = (newFormat /= 0)

! Force history so that time averaged collections are timestamped with write time
       call ESMF_ConfigGetAttribute(cfg, list(n)%ForceOffsetZero, default=.false., & 
                                    label=trim(string)//'timestampEnd:', rc=status)
       _VERIFY(status) 

! Get an optional chunk size
! --------------------------
       len = ESMF_ConfigGetLen(cfg, label=trim(trim(string) // 'chunksize:'), rc = status)
       if ( status == ESMF_SUCCESS ) then
          call ESMF_ConfigFindLabel( cfg, label=trim(trim(string) // 'chunksize:'), rc =status)
          _VERIFY(STATUS)
          chnksz = 4
          if (list(n)%useNewFormat) then
             chnksz = 5
          end if
          allocate( list(n)%chunksize(chnksz), stat = status)
          _VERIFY(STATUS)
          j=0
          do i=1,len
             call ESMF_ConfigGetAttribute( cfg,value=tmpstring, rc=status)
             _VERIFY(STATUS)
             if (trim(tmpstring) == ',' ) cycle
             j = j + 1
             _ASSERT(j<=6,'needs informative message')
             i1 = index(tmpstring(:),",")
             if (i1.eq.1) tmpstring = adjustl( tmpstring(2:)  )
             j1 = index(tmpstring(:),",")-1
             if (j1.gt.0) tmpstring = adjustl( tmpstring(1:j1) )
             if (j<=chnksz) read(tmpstring,*) list(n)%chunksize(j)
          enddo
       end if

! Get an optional tile file for regridding the output
! ---------------------------------------------------
       call ESMF_ConfigGetAttribute ( cfg, value=tilefile, default="", &
                                      label=trim(string) // 'regrid_exch:' ,rc=status )
       _VERIFY(STATUS)

       call ESMF_ConfigGetAttribute ( cfg, value=gridname, default="", &
                                      label=trim(string) // 'regrid_name:' ,rc=status )
       _VERIFY(STATUS)

       NULLIFY(IntState%Regrid(n)%PTR)
       if (tilefile /= '' .OR. gridname /= '') then
          allocate(IntState%Regrid(n)%PTR, stat=status)
          _VERIFY(STATUS)
          IntState%Regrid(n)%PTR%tilefile = tilefile
          IntState%Regrid(n)%PTR%gridname = gridname
       end if

! Set Alarms
! ----------

       if (list(n)%disabled) cycle

       if(list(n)%mode == "instantaneous" .or. list(n)%ForceOffsetZero) then
          sec = 0
       else
          IntState%average(n) = .true.
          sec = MAPL_nsecf(list(n)%acc_interval) / 2
       endif
       call ESMF_TimeIntervalSet( INTSTATE%STAMPOFFSET(n), S=sec, rc=status )
       _VERIFY(STATUS)

! His and Seg Alarms based on Reference Date and Time
! ---------------------------------------------------
       REF_TIME(1) =     list(n)%ref_date/10000
       REF_TIME(2) = mod(list(n)%ref_date,10000)/100
       REF_TIME(3) = mod(list(n)%ref_date,100)
       REF_TIME(4) =     list(n)%ref_time/10000
       REF_TIME(5) = mod(list(n)%ref_time,10000)/100
       REF_TIME(6) = mod(list(n)%ref_time,100)

       !ALT if monthly, modify ref_time(4:6)=0
       if (list(n)%monthly) REF_TIME(4:6) = 0
       
       call ESMF_TimeSet( RefTime, YY = REF_TIME(1), &
                                   MM = REF_TIME(2), &
                                   DD = REF_TIME(3), &
                                   H  = REF_TIME(4), &
                                   M  = REF_TIME(5), &
                                   S  = REF_TIME(6), calendar=cal, rc=rc )

       ! ALT if monthly, set interval "Frequncy" to 1 month
       ! also in this case sec should be set to non-zero
       sec = MAPL_nsecf( list(n)%frequency )
       call ESMF_TimeIntervalSet( Frequency, S=sec, calendar=cal, rc=status ) ; _VERIFY(STATUS)
       RingTime = RefTime

! Added Logic to eliminate BEG_DATE = cap_restart date problem
! ------------------------------------------------------------
       if (RefTime == startTime) then
           RingTime = RefTime + Frequency
       end if
!
       if (RingTime < currTime .and. sec /= 0 ) then
           RingTime = RingTime + (INT((currTime - RingTime)/frequency)+1)*frequency
       endif
       if ( list(n)%backwards ) then
          list(n)%his_alarm = ESMF_AlarmCreate( clock=clock, RingInterval=Frequency, RingTime=RingTime, rc=status )
       else
          list(n)%his_alarm = ESMF_AlarmCreate( clock=clock, RingInterval=Frequency, RingTime=RingTime, sticky=.false., rc=status )
       endif
       _VERIFY(STATUS)

       !ALT if monthly overwrite duration and frequency
       if (list(n)%monthly) then
          list(n)%duration = 1 !ALT simply non-zero
       end if
       if( list(n)%duration.ne.0 ) then
          if (.not.list(n)%monthly) then
             sec = MAPL_nsecf( list(n)%duration )
             call ESMF_TimeIntervalSet( Frequency, S=sec, calendar=cal, rc=status ) ; _VERIFY(STATUS)
             RingTime = RefTime
          else
             call ESMF_TimeIntervalSet( Frequency, MM=1, calendar=cal, rc=status ) ; _VERIFY(STATUS)
             !ALT keep the values from above
             ! and for debugging print
             call WRITE_PARALLEL("DEBUG: monthly averaging is active for collection "//trim(list(n)%collection))
          end if
          if (RingTime < currTime) then
              RingTime = RingTime + (INT((currTime - RingTime)/frequency)+1)*frequency
          endif
          if ( list(n)%backwards ) then
             list(n)%seg_alarm = ESMF_AlarmCreate( clock=clock, RingInterval=Frequency, RingTime=RingTime, rc=status )
          else
             list(n)%seg_alarm = ESMF_AlarmCreate( clock=clock, RingInterval=Frequency, RingTime=RingTime, sticky=.false., rc=status )
          endif
          _VERIFY(STATUS)
          if (list(n)%monthly .and. (currTime == RingTime)) then
             call ESMF_AlarmRingerOn( list(n)%his_alarm,rc=status )
             _VERIFY(STATUS)
          end if

       else
          ! this alarm should never ring, but it is checked if ringing
          list(n)%seg_alarm = ESMF_AlarmCreate( clock=clock, enabled=.false., &
               ringTime=currTime, name='historyNewSegment', rc=status )
          _VERIFY(STATUS)
       endif

! Mon Alarm based on 1st of Month 00Z
! -----------------------------------
       REF_TIME(1) =     list(n)%ref_date/10000
       REF_TIME(2) = mod(list(n)%ref_date,10000)/100
       REF_TIME(3) = 1
       REF_TIME(4) = 0
       REF_TIME(5) = 0
       REF_TIME(6) = 0

       call ESMF_TimeSet( RefTime, YY = REF_TIME(1), &
                                   MM = REF_TIME(2), &
                                   DD = REF_TIME(3), &
                                   H  = REF_TIME(4), &
                                   M  = REF_TIME(5), &
                                   S  = REF_TIME(6), calendar=cal, rc=rc )

       call ESMF_TimeIntervalSet( Frequency, MM=1, calendar=cal, rc=status ) ; _VERIFY(STATUS)
       RingTime = RefTime
       do while ( RingTime < currTime )
          RingTime = RingTime + Frequency
       enddo
       if ( list(n)%backwards ) then
          list(n)%mon_alarm = ESMF_AlarmCreate( clock=clock, RingInterval=Frequency, RingTime=RingTime, rc=status )
       else
          list(n)%mon_alarm = ESMF_AlarmCreate( clock=clock, RingInterval=Frequency, RingTime=RingTime, sticky=.false., rc=status )
       endif
       _VERIFY(STATUS)
       if(list(n)%monthly) then
          !ALT this is temporary workaround. It has a memory leak
          ! we need to at least destroy his_alarm before assignment
          ! better yet, create it like this one in the first place
          call ESMF_AlarmDestroy(list(n)%his_alarm)
          list(n)%his_alarm = list(n)%mon_alarm
          intState%stampOffset(n) = Frequency ! we go to the beginning of the month
       end if

! End Alarm based on end_date and end_time
! ----------------------------------------
       if( list(n)%end_date.ne.-999 .and. list(n)%end_time.ne.-999 ) then
           REF_TIME(1) =     list(n)%end_date/10000
           REF_TIME(2) = mod(list(n)%end_date,10000)/100
           REF_TIME(3) = mod(list(n)%end_date,100)
           REF_TIME(4) =     list(n)%end_time/10000
           REF_TIME(5) = mod(list(n)%end_time,10000)/100
           REF_TIME(6) = mod(list(n)%end_time,100) + 1 ! Add 1 second to make end_time inclusive
       
           call ESMF_TimeSet( RingTime, YY = REF_TIME(1), &
                                        MM = REF_TIME(2), &
                                        DD = REF_TIME(3), &
                                        H  = REF_TIME(4), &
                                        M  = REF_TIME(5), &
                                        S  = REF_TIME(6), calendar=cal, rc=rc )

           if ( list(n)%backwards ) then
              list(n)%end_alarm = ESMF_AlarmCreate( clock=clock, RingTime=RingTime, rc=status )
           else
              list(n)%end_alarm = ESMF_AlarmCreate( clock=clock, RingTime=RingTime, sticky=.false., rc=status )
           endif
           _VERIFY(STATUS)
        else
           if ( list(n)%backwards ) then
              list(n)%end_alarm = ESMF_AlarmCreate( clock=clock, RingTime=CurrTime, rc=status )
           else
              list(n)%end_alarm = ESMF_AlarmCreate( clock=clock, RingTime=CurrTime, sticky=.false., rc=status )
           endif
           _VERIFY(STATUS)
           call  ESMF_AlarmRingerOff(list(n)%end_alarm, rc=status )
           _VERIFY(STATUS)
       endif

       call ESMF_ConfigDestroy(cfg, rc=status)
       _VERIFY(STATUS)
    enddo LISTLOOP

    if( MAPL_AM_I_ROOT() ) print *

! START OF PARSER STUFF
    size0 = 1 !size( export )
    nstatelist = 0
    allocate( statelist(size0), stat=status )
    _VERIFY(STATUS)
    statelist(1) = ''


    do n=1,nlist
       do m=1,list(n)%field_set%nfields
          k=1
          if (list(n)%regex .or. &
              scan(trim(list(n)%field_set%fields(1,m)),'()^/*+-')==0)then
             do while ( k.le.nstatelist )
                if (statelist(k) == '') statelist(k) = list(n)%field_set%fields(2,m)
                if( statelist(k).ne.list(n)%field_set%fields(2,m)) then
                   k=k+1
                else
                   exit
                end if
             enddo
             if(k.eq.nstatelist+1) then
                allocate( tmplist (nstatelist), stat=status )
                _VERIFY(STATUS)
                tmplist = statelist
                nstatelist = k
                deallocate( statelist )
                allocate( statelist(nstatelist), stat=status )
                _VERIFY(STATUS)
                if (k > 1) statelist(1:k-1) = tmplist
                statelist(k)     = list(n)%field_set%fields(2,m)
                deallocate(   tmplist )
             endif
          else
             if (index(list(n)%field_set%fields(1,m),'%') /= 0) then
                call WRITE_PARALLEL('Can not do arithmetic expression with bundle item')
                _ASSERT(.false.,'needs informative message')
             end if
          end if
       enddo
    enddo

! Get Output Export States
! ------------------------

    allocate ( exptmp (size0), stat=status )
    _VERIFY(STATUS)
    exptmp(1) = import
    allocate ( export(nstatelist), stat=status )
    _VERIFY(STATUS)
    errorFound = .false.
    allocate ( stateListAvail(nstatelist), stat=status )
    _VERIFY(STATUS)
    stateListAvail = .true.
    if (disableSubVmChecks) then
!ALT: setting disableSubVmChecks to .true. automatically assumes that subVm = .false.
       do n=1,nstatelist
          call MAPL_ExportStateGet ( exptmp,statelist(n),export(n),rc=status )
          if( STATUS/= ESMF_SUCCESS ) then
             call WRITE_PARALLEL('Cannot Find ' // trim(statelist(n)))
             errorFound = .true.
          endif
       enddo
    else
       do n=1,nstatelist
          call MAPL_ExportStateGet ( exptmp,statelist(n),export(n),rc=status )
          call ESMF_VMAllReduce(vm, sendData=status, recvData=globalStatus, &
               reduceflag=ESMF_REDUCE_MAX, rc=localStatus)
          _VERIFY(localStatus)

          if( STATUS/= ESMF_SUCCESS ) then
             stateListAvail(n) = .false.
          end if

          if( globalSTATUS/= ESMF_SUCCESS ) then
             call WRITE_PARALLEL('Cannot Find ' // trim(statelist(n)))
             errorFound = .true.
          endif

       enddo
    end if
    _ASSERT(.not. errorFound,'needs informative message')
    deallocate ( exptmp )

! Associate Output Names with EXPORT State Index
! ----------------------------------------------
    list(:)%subVm = .false.
    do n=1,nlist
       allocate( list(n)%expSTATE(list(n)%field_set%nfields), stat=status )
       _VERIFY(STATUS)
       do m=1,list(n)%field_set%nfields
! when we allow regex; some syntax resembles math expressions
        if (list(n)%regex .or. &
            scan(trim(list(n)%field_set%fields(1,m)),'()^/*+-')==0)then
          do k=1,nstatelist
             if( trim(list(n)%field_set%fields(2,m)) .eq. trim(statelist(k)) ) then
                if (.not. stateListAvail(k)) then
                   list(n)%subVm = .true.
                   cycle
                end if
                list(n)%expSTATE(m) = k
             end if
          enddo
        endif 
       enddo
    enddo

    ! Important: the next 2 calls modify the field's list
    ! first we check if any regex expressions need to expanded
    !---------------------------------------------------------
    call wildCardExpand(rc=status)
    _VERIFY(status)

    ! Deal with split 4d field
    !--------------------------
    call split4dFields(rc=status)
    _VERIFY(status)

    do n=1,nlist
       m=list(n)%field_set%nfields
       allocate(list(n)%r4(m), list(n)%r8(m), list(n)%r8_to_r4(m), stat=status)  
       _VERIFY(STATUS)
    end do
    
PARSER: do n=1,nlist

       do m=1,list(n)%field_set%nfields
          if (scan(trim(list(n)%field_set%fields(1,m)),'()^/*+-')==0)then
             call MAPL_StateGet( export(list(n)%expSTATE(m)),trim(list(n)%field_set%fields(1,m)),field,rc=status )
             IF (STATUS /= ESMF_SUCCESS) then
                call WRITE_PARALLEL( "ERROR: cannot find output " // &
                     trim(list(n)%field_set%fields(1,m)) // " in " // &
                     trim(list(n)%field_set%fields(2,m)))
                errorFound = .true.
                status=ESMF_SUCCESS
             endif
         endif
      enddo

      allocate(list(n)%tmpfields(list(n)%field_set%nfields), stat=status)
      _VERIFY(STATUS)
      allocate(list(n)%ReWrite(list(n)%field_set%nfields), stat=status)
      _VERIFY(STATUS)

      list(n)%tmpfields=''
      list(n)%ReWrite= .FALSE.

      call MAPL_SetExpression(list(n)%field_set%nfields,list(n)%field_set%fields,list(n)%tmpfields,list(n)%rewrite,  &
                              list(n)%nPExtraFields, &
                              list(n)%PExtraFields, list(n)%PExtraGridComp, import,rc=STATUS)
      _VERIFY(STATUS)

ENDDO PARSER

    _ASSERT(.not. errorFound,'needs informative message')
    deallocate(stateListAvail)
    deallocate(export)
    deallocate(statelist)
    do n=1,nlist
     deallocate(list(n)%expSTATE)
    enddo

! END OF PARSER STUFF

! Extract List of Unique Export State Names
! -----------------------------------------

    size0 = 1 !size( export )
    nstatelist = 0
    allocate( statelist(size0), stat=status )
    _VERIFY(STATUS)
    statelist(1) = ''

    
    do n=1,nlist
       do m=1,list(n)%field_set%nfields
          k=1
          do while ( k.le.nstatelist )
             if (statelist(k) == '') statelist(k) = list(n)%field_set%fields(2,m)
             if( statelist(k).ne.list(n)%field_set%fields(2,m)) then
                k=k+1
             else
                exit
             end if
          enddo
          if(k.eq.nstatelist+1) then
             allocate( tmplist (nstatelist), stat=status )
             _VERIFY(STATUS)
             tmplist = statelist
             nstatelist = k
             deallocate( statelist )
             allocate( statelist(nstatelist), stat=status )
             _VERIFY(STATUS)
             if (k > 1) statelist(1:k-1) = tmplist
             statelist(k)     = list(n)%field_set%fields(2,m)
             deallocate(   tmplist )
          endif
       enddo
    enddo
 
! Get Output Export States
! ------------------------

    allocate ( exptmp (size0), stat=status )
    _VERIFY(STATUS)
    exptmp(1) = import
!    deallocate ( export )
    allocate ( export(nstatelist), stat=status )
    _VERIFY(STATUS)
    errorFound = .false.
    allocate ( stateListAvail(nstatelist), stat=status )
    _VERIFY(STATUS)
    stateListAvail = .true.
    if (disableSubVmChecks) then
!ALT: setting disableSubVmChecks to .true. automatically assumes that subVm = .false.
       do n=1,nstatelist
          call MAPL_ExportStateGet ( exptmp,statelist(n),export(n),rc=status )
          if( STATUS/= ESMF_SUCCESS ) then
             call WRITE_PARALLEL('Cannot Find ' // trim(statelist(n)))
             errorFound = .true.
          endif
       enddo
    else
       do n=1,nstatelist
          call MAPL_ExportStateGet ( exptmp,statelist(n),export(n),rc=status )
          call ESMF_VMAllReduce(vm, sendData=status, recvData=globalStatus, &
               reduceflag=ESMF_REDUCE_MAX, rc=localStatus)
          _VERIFY(localStatus)

          if( STATUS/= ESMF_SUCCESS ) then
             stateListAvail(n) = .false.
          end if

          if( globalSTATUS/= ESMF_SUCCESS ) then
             call WRITE_PARALLEL('Cannot Find ' // trim(statelist(n)))
             errorFound = .true.
          endif

       enddo
    end if
    _ASSERT(.not. errorFound,'needs informative message')
    deallocate ( exptmp )

! Associate Output Names with EXPORT State Index
! ----------------------------------------------
    list(:)%subVm = .false.
    do n=1,nlist
       allocate( list(n)%expSTATE(list(n)%field_set%nfields), stat=status )
       _VERIFY(STATUS)
       do m=1,list(n)%field_set%nfields
          do k=1,nstatelist
             if( trim(list(n)%field_set%fields(2,m)) .eq. trim(statelist(k)) ) then
                if (.not. stateListAvail(k)) then
                   list(n)%subVm = .true.
                   cycle
                end if
                list(n)%expSTATE(m) = k
             end if
          enddo
       enddo
    enddo

! Ensure Diagnostic Output has been Allocated
! -------------------------------------------
    errorFound = .false.
    do n=1,nlist
       if (list(n)%disabled) cycle
       if (list(n)%subVm) cycle
       do m=1,list(n)%field_set%nfields
          call MAPL_StateGet( export(list(n)%expSTATE(m)), &
               trim(list(n)%field_set%fields(1,m)), Field, rc=status )
          IF (STATUS /= ESMF_SUCCESS) then
             call WRITE_PARALLEL( "ERROR: cannot find output " // &
                  trim(list(n)%field_set%fields(1,m)) // " in " // &
                  trim(list(n)%field_set%fields(2,m)))
             errorFound = .true.
          else
             if (index(list(n)%field_set%fields(1,m),'%') ==0) then
                call MAPL_AllocateCoupling(Field, rc=status)
                _VERIFY(STATUS)
             end if

          end IF
       enddo
    enddo

    _ASSERT(.not. errorFound,'needs informative message')


    allocate(INTSTATE%AVERAGE    (nlist), stat=status)
    _VERIFY(STATUS)

    IntState%average = .false.
    do n=1, nlist
       if (list(n)%disabled) cycle
       if(list(n)%mode == "instantaneous" .or. list(n)%ForceOffsetZero) then
          sec = 0
       else
          IntState%average(n) = .true.
          sec = MAPL_nsecf(list(n)%acc_interval) / 2
          if(list(n)%monthly) cycle
       endif
       call ESMF_TimeIntervalSet( INTSTATE%STAMPOFFSET(n), S=sec, rc=status )
       _VERIFY(STATUS)
    end do

   nactual = npes
   if (.not. disableSubVmChecks) then
      allocate(allPes(npes), stat=status)
      _VERIFY(STATUS)
      minactual = npes
      do n=1, nlist
         NULLIFY(list(n)%peAve)
         if (list(n)%disabled) cycle
         localPe(1) = mype
         if (list(n)%subVm) localPe(1) = -1
         call ESMF_VMAllGather(vm, sendData=localPe, recvData=allPEs, &
              count=1, rc=status)
         _VERIFY(STATUS)
         nactual = count(allPEs >= 0)
         minactual = min(minactual, nactual)
         allocate(list(n)%peAve(nactual), stat=status)
         _VERIFY(STATUS)
         list(n)%peAve = pack(allPEs, allPEs>=0)
      end do
   
      IntState%npes = minactual
      deallocate(allPEs)
   end if

   allocate(INTSTATE%CCS(nlist), stat=status)
   _VERIFY(STATUS)
   allocate(INTSTATE%GIM(nlist), stat=status)
   _VERIFY(STATUS)
   allocate(INTSTATE%CIM(nlist), stat=status)
   _VERIFY(STATUS)
   allocate(INTSTATE%SRCS(nlist), stat=status)
   _VERIFY(STATUS)
   allocate(INTSTATE%DSTS(nlist), stat=status)
   _VERIFY(STATUS)
!   allocate(INTSTATE%GEX(nlist), stat=status)
!   _VERIFY(STATUS)
!   allocate(INTSTATE%GCNameList(nlist), stat=status)
!   _VERIFY(STATUS)

! Initialize Logical for Grads Control File
! -----------------------------------------

   allocate( INTSTATE%LCTL(nlist), stat=status )
   _VERIFY(STATUS)
   do n=1,nlist
      if (list(n)%disabled) cycle
      if( list(n)%format == 'flat' ) then
         INTSTATE%LCTL(n) = .true.
      else
         INTSTATE%LCTL(n) = .false.
      endif
   enddo

   do n=1, nlist
      if (list(n)%disabled) cycle
      if (list(n)%subVm) cycle
      
      IntState%GIM(n) = ESMF_StateCreate ( name=trim(list(n)%filename), &
           stateIntent = ESMF_STATEINTENT_IMPORT, &
           rc=status )
      _VERIFY(STATUS)
      if(list(n)%mode == "instantaneous") then
         IntState%average(n) = .false.
      else
         IntState%average(n) = .true.
         IntState%CIM(n) = ESMF_StateCreate ( name=trim(list(n)%filename), &
              stateIntent = ESMF_STATEINTENT_IMPORT, &
              rc=status )
         _VERIFY(STATUS)
         NULLIFY(INTSTATE%SRCS(n)%SPEC)
         NULLIFY(INTSTATE%DSTS(n)%SPEC)
      endif

      if (associated(IntState%Regrid(n)%PTR)) then
         _ASSERT(.not. list(n)%subVm,'needs informative message') ! ALT: currently we are not supporting regridding on subVM
! query a field from export (arbitrary first field in the stream) for grid_in
         _ASSERT(size(export(list(n)%expSTATE)) > 0,'needs informative message')
         call MAPL_StateGet( export(list(n)%expSTATE(1)), &
                             trim(list(n)%field_set%fields(1,1)), field, rc=status )
         _VERIFY(STATUS)
         IntState%Regrid(n)%PTR%state_out = ESMF_StateCreate ( name=trim(list(n)%filename)//'regrid_in', &
              stateIntent = ESMF_STATEINTENT_IMPORT, &
              rc=status )
         _VERIFY(STATUS)

! get grid name, layout, dims
         call ESMF_FieldGet(field, grid=grid_in, rc=status)
         _VERIFY(STATUS)
         call ESMF_GridGet(grid_in, name=gridname, distgrid=distgrid, rc=status)
         _VERIFY(STATUS)
         call ESMF_DistGridGet(distgrid, delayout=layout, rc=status)
         _VERIFY(STATUS)

         IntState%Regrid(n)%PTR%noxform = .false.

!        Check if is is tile variable: we could go the same grid attached to LS 
!        and use T2G or go to the "other" grid in the LS. In the later case, 
!        we need to find then "other LS" from the list of available LS in 
!        History, and calculate Xform, then do T2T, followed by T2G


         if (gridname(1:10) == 'tile_grid_') then

            ontiles = .true.

            _ASSERT(IntState%Regrid(n)%PTR%gridname /= '','needs informative message')

!ALT:       here we are getting the address of LocStream from the TILEGRID 
!           as INTEGER*8 attribute and we are using a C routine to 
!           set the pointer to LocStream

            call ESMF_AttributeGet(grid_in, name='TILEGRID_LOCSTREAM_ADDR', &
                 value=ADDR, rc=status)
            _VERIFY(STATUS)
            call c_MAPL_LocStreamRestorePtr(exch, ADDR)

!           Get the attached grid
            call MAPL_LocStreamGet(EXCH, ATTACHEDGRID=GRID_ATTACHED, RC=STATUS)
            _VERIFY(STATUS)

            call ESMF_GridGet(grid_attached, name=attachedName, rc=status)
            _VERIFY(STATUS)
            
            if (attachedName == IntState%Regrid(n)%PTR%gridname) then
!              T2G
               IntState%Regrid(n)%PTR%regridType = MAPL_T2G

               IntState%Regrid(n)%PTR%locOut = exch 

               IntState%Regrid(n)%PTR%noxform = .true.
               grid_out = grid_attached
               use_this_gridname = .true.
            else
!              this is also T2G but the grid is not the attached grid
!              done as T2T followed by T2G
               IntState%Regrid(n)%PTR%locIn = exch 
               IntState%Regrid(n)%PTR%regridType = MAPL_T2G
               IntState%Regrid(n)%PTR%noxform = .false.

! find the "other" locstream
               found = .false.
               _ASSERT(associated(LSADDR_PTR),'needs informative message')
               do i = 1, size(LSADDR_PTR)
                  call c_MAPL_LocStreamRestorePtr(locStream, LSADDR_PTR(i))
                  call MAPL_LocStreamGet(locStream, ATTACHEDGRID=GRID, RC=STATUS)
                  _VERIFY(STATUS)
                  call ESMF_GridGet(grid, name=tmpstr, rc=status)
                  _VERIFY(STATUS)
                  if (tmpstr == IntState%Regrid(n)%PTR%gridname) then
                     found = .true.
                     exit
                  end if
               end do

               if (found) then
                  IntState%Regrid(n)%PTR%locOut = locStream
                  grid_out = grid
               else
!ALT: added new logic by Max request: if not found 
! open tile file get gridnames, make sure that "output" grid and "attached" grid are 2
! grids assoc with tile file, else ERROR
! do T2G on "internal" locstream, followed by G2G (G2T on "output" LS(attached grid),
! followed by T2T (Xform), and finally G2T on "output" LS("output" grid)

                  IntState%Regrid(n)%PTR%regridType = MAPL_T2G2G
                  _ASSERT(IntState%Regrid(n)%PTR%tilefile /= '','needs informative message')

                  ontiles = .false. !ALT: this is needed to force execution of G2G part

!>>>
!           get gridnames from exch
                  call MAPL_LocStreamGet(exch, GRIDNAMES = GNAMES, RC=STATUS)
                  _VERIFY(STATUS)

                  ngrids = size(gnames)
                  _ASSERT(ngrids==2,'needs informative message')

                  ! find "complement" of attached grid
                  found = .false.
                  DO I = 1, NGRIDS
                     IF (GNAMES(I) == attachedNAME) THEN
                        FOUND = .TRUE.
                        exit
                     ENDIF
                  ENDDO
                  _ASSERT(FOUND,'needs informative message')
                  NG = 3-I

                  ! find "complement" of exch
                  found = .false.
                  do i = 1, size(LSADDR_PTR)
                     call c_MAPL_LocStreamRestorePtr(locStream, LSADDR_PTR(i))
                     call MAPL_LocStreamGet(locStream, ATTACHEDGRID=GRID, RC=STATUS)
                     _VERIFY(STATUS)
                     call ESMF_GridGet(grid, name=tmpstr, rc=status)
                     _VERIFY(STATUS)
                     if (tmpstr == gnames(NG)) then
                        found = .true.
                        exit
                     end if
                  end do
                  _ASSERT(FOUND,'needs informative message')
!<<<
                  grid_in = grid                               ! grid_attached
                  IntState%Regrid(n)%PTR%locNative = locStream ! exch
!XFORM create exch+locStream; and store it!
                  call MAPL_LocStreamCreateXform(XFORM=INTSTATE%Regrid(n)%PTR%XFORMntv, &
                       LocStreamOut=locStream, &
                       LocStreamIn=exch, &
                       NAME='historyXFORMnative', &
                       UseFCollect=.true., &
                       RC=STATUS )
                  _VERIFY(STATUS)

                  ! get the name and layout of attached grid
                  call ESMF_GridGet(grid_in, name=gridname, distgrid=distgrid, rc=status)
                  _VERIFY(STATUS)
                  call ESMF_DistGridGet(distgrid, delayout=layout, rc=status)
                  _VERIFY(STATUS)

                  call MAPL_LocStreamCreate(IntState%Regrid(n)%PTR%locIn, &
                       layout, FILENAME=IntState%Regrid(n)%PTR%TILEFILE, &
                       NAME='history_in', MASK=(/MAPL_Ocean/), grid=grid_in, RC=STATUS)
                  _VERIFY(STATUS)
               end if

            end if

         else
!           this is G2G done as G2T followed by T2T and then T2G
            IntState%Regrid(n)%PTR%regridType = MAPL_G2G
            _ASSERT(IntState%Regrid(n)%PTR%tilefile /= '','needs informative message')

            ontiles = .false.

            call MAPL_LocStreamCreate(IntState%Regrid(n)%PTR%locIn, &
                 layout, FILENAME=IntState%Regrid(n)%PTR%TILEFILE, &
                 NAME='history_in', MASK=(/MAPL_Ocean/), grid=grid_in, RC=STATUS)
            _VERIFY(STATUS)

         end if

         IntState%Regrid(n)%PTR%ontiles = ontiles

         if (.not. ontiles) then
!           get gridnames from loc_in
            call MAPL_LocStreamGet(IntState%Regrid(n)%PTR%locIn, &
                 GRIDNAMES = GNAMES, RC=STATUS)
            _VERIFY(STATUS)
! query loc_in for ngrids
            ngrids = size(gnames)
            _ASSERT(ngrids==2,'needs informative message')

            use_this_gridname = .false.
            IntState%Regrid(n)%PTR%noxform = .false.
! validate that gridname_in is there
            found = .false.
            DO I = 1, NGRIDS
               IF (GNAMES(I) == GRIDNAME) THEN
                  FOUND = .TRUE.
                  exit
               ENDIF
            ENDDO
            _ASSERT(FOUND,'needs informative message')

! pick gridname_out
! we pick the "other" gridname. this works only when ngrids==2; 3-1=2;3-2=1
            NG = 3 - I 

!@@            if (use_this_gridname) then
!@@               NG = I
!@@            else
!@@               NG = 3 - I 
!@@            end if
! create grid_out

            pgrid => IntState%output_grids%at(trim(gnames(ng)))
! create and attach loc_out to grid_out
            grid_out=pgrid
            call MAPL_LocStreamCreate(IntState%Regrid(n)%PTR%locOut, &
                 layout, FILENAME=IntState%Regrid(n)%PTR%TILEFILE, &
                 NAME='history_out', MASK=(/MAPL_Ocean/), Grid=grid_out, RC=STATUS)
            _VERIFY(STATUS)

         endif

! query ntiles
         call MAPL_LocStreamGet(IntState%Regrid(n)%PTR%locOut, &
              NT_LOCAL = IntState%Regrid(n)%PTR%ntiles_out, rc=status)
         _VERIFY(STATUS)

         if (.not.INTSTATE%Regrid(n)%PTR%noxform) then
! query ntiles
            call MAPL_LocStreamGet(IntState%Regrid(n)%PTR%locIn, &
                 NT_LOCAL = IntState%Regrid(n)%PTR%ntiles_in, rc=status)
            _VERIFY(STATUS)

! create XFORM
            call MAPL_LocStreamCreateXform ( XFORM=INTSTATE%Regrid(n)%PTR%XFORM, &
                 LocStreamOut=INTSTATE%Regrid(n)%PTR%LocOut, &
                 LocStreamIn=INTSTATE%Regrid(n)%PTR%LocIn, &
                 NAME='historyXFORM', &
                 UseFCollect=.true., &
                 RC=STATUS )
            _VERIFY(STATUS)
         end if

      endif

! Handle possible extra fields needed for the parser
      if (list(n)%nPExtraFields > 0) then

         allocate ( exptmp (1), stat=status )
         _VERIFY(STATUS)
         exptmp(1) = import

         do m=1,list(n)%nPExtraFields
            call MAPL_ExportStateGet(exptmp,list(n)%PExtraGridComp(m),parser_state,rc=status)
            _VERIFY(STATUS)
            call MAPL_StateGet(parser_state,list(n)%PExtraFields(m),parser_field,rc=status)
            _VERIFY(STATUS)
            call MAPL_AllocateCoupling(parser_field, rc=status)
            _VERIFY(STATUS)
            f = MAPL_FieldCreate(parser_field, name=list(n)%PExtraFields(m), rc=status)
            _VERIFY(STATUS)
            if (IntState%average(n)) then
               call MAPL_StateAdd(IntState%CIM(N), f, rc=status)
               _VERIFY(STATUS)
            else
               call MAPL_StateAdd(IntState%GIM(N), f, rc=status)
               _VERIFY(STATUS)
            end if                  
         end do

         deallocate(exptmp)

      end if

      do m=1,list(n)%field_set%nfields
         call MAPL_StateGet( export(list(n)%expSTATE(m)), &
                             trim(list(n)%field_set%fields(1,m)), field, rc=status )
         _VERIFY(STATUS)

         call ESMF_FieldGet(FIELD, typekind=tk, RC=STATUS)
         _VERIFY(STATUS)
         if (tk == ESMF_TypeKind_R8) then
            list(n)%r8_to_r4(m) = .true.
            list(n)%r8(m) = field
            ! Create a new field with R4 precision
            r4field = MAPL_FieldCreate(field,RC=status)
            _VERIFY(STATUS)
            field=r4field
            list(n)%r4(m) = field
         else
            list(n)%r8_to_r4(m) = .false.
         end if

         if (.not.list(n)%rewrite(m) .or.list(n)%field_set%fields(4,m) /= BLANK ) then
          f = MAPL_FieldCreate(field, name=list(n)%field_set%fields(3,m), rc=status) 
         else
          DoCopy=.True.
          f = MAPL_FieldCreate(field, name=list(n)%field_set%fields(3,m), DoCopy=DoCopy, rc=status)
         endif
         _VERIFY(STATUS)
         if (list(n)%field_set%fields(4,m) /= BLANK) then
            if (list(n)%field_set%fields(4,m) == 'MIN') then
               call ESMF_AttributeSet(f, NAME='CPLFUNC', VALUE=MAPL_CplMin, RC=STATUS)
               _VERIFY(STATUS)
            else if (list(n)%field_set%fields(4,m) == 'MAX') then
               call ESMF_AttributeSet(f, NAME='CPLFUNC', VALUE=MAPL_CplMax, RC=STATUS)
               _VERIFY(STATUS)
            else if (list(n)%field_set%fields(4,m) == 'ACCUMULATE') then
               call ESMF_AttributeSet(f, NAME='CPLFUNC', VALUE=MAPL_CplAccumulate, RC=STATUS)
               _VERIFY(STATUS)
            else
               call WRITE_PARALLEL("Functionality not supported yet")
            end if
         end if

         if (IntState%average(n)) then
            call MAPL_StateAdd(IntState%CIM(N), f, rc=status)
            _VERIFY(STATUS)

            ! borrow SPEC from FIELD
            ! modify SPEC to reflect accum/avg
            call ESMF_FieldGet(f, name=short_name, grid=grid, rc=status)
            _VERIFY(STATUS)

            call ESMF_AttributeGet(FIELD, NAME='DIMS', VALUE=DIMS, RC=STATUS)
            _VERIFY(STATUS)
            call ESMF_AttributeGet(FIELD, NAME='VLOCATION', VALUE=VLOCATION, RC=STATUS)
            _VERIFY(STATUS)
            call ESMF_AttributeGet(FIELD, NAME='LONG_NAME', VALUE=LONG_NAME, RC=STATUS)
            _VERIFY(STATUS)
            call ESMF_AttributeGet(FIELD, NAME='UNITS', VALUE=UNITS, RC=STATUS)
            _VERIFY(STATUS)
            call ESMF_AttributeGet(FIELD, NAME='FIELD_TYPE', VALUE=FIELD_TYPE, RC=STATUS)
            _VERIFY(STATUS)

            call ESMF_AttributeGet(FIELD, NAME='REFRESH_INTERVAL', VALUE=REFRESH, RC=STATUS)
            _VERIFY(STATUS)
            call ESMF_AttributeGet(FIELD, NAME='AVERAGING_INTERVAL', VALUE=avgint, RC=STATUS)
            _VERIFY(STATUS)

            call ESMF_FieldGet(FIELD, dimCount=fieldRank, RC=STATUS)
            _VERIFY(STATUS)
            call ESMF_GridGet(GRID, dimCount=gridRank, rc=status)
            _VERIFY(STATUS)
            allocate(gridToFieldMap(gridRank), stat=status)
            _VERIFY(STATUS)
            call ESMF_FieldGet(FIELD, gridToFieldMap=gridToFieldMap, RC=STATUS)
            _VERIFY(STATUS)

            notGridded = count(gridToFieldMap==0)
            unGridDims = fieldRank - gridRank + notGridded

            hasUngridDims = .false.
            if (unGridDims > 0) then
               hasUngridDims = .true.
!ALT: special handling for 2d-MAPL grid (the vertical is treated as ungridded)
               if ((gridRank == 2) .and. (DIMS == MAPL_DimsHorzVert) .and. &
                    (unGridDims == 1)) then
                  hasUngridDims = .false.
               end if
            endif

            if (hasUngridDims) then
               allocate(ungriddedLBound(unGridDims), &
                        ungriddedUBound(unGridDims), &
                        ungrd(unGridDims),           &
                        stat=status)
               _VERIFY(STATUS)

!@               call ESMF_FieldGet(FIELD, &
!@                    ungriddedLBound=ungriddedLBound, &
!@                    ungriddedUBound=ungriddedUBound, &
!@                    RC=STATUS)
!@               _VERIFY(STATUS)
               

               call ESMF_FieldGet(field, Array=array, rc=status)
               _VERIFY(STATUS)

               call ESMF_ArrayGet(array, rank=rank, dimCount=dimCount, rc=status)
               _VERIFY(STATUS)
               undist = rank-dimCount
               _ASSERT(undist == ungridDims,'needs informative message')

               call ESMF_ArrayGet(array, undistLBound=ungriddedLBound, &
                    undistUBound=ungriddedUBound, rc=status)
               _VERIFY(STATUS)

               ungrd = ungriddedUBound - ungriddedLBound + 1
               call ESMF_AttributeGet(field,name="UNGRIDDED_UNIT",value=ungridded_unit,rc=status)
               _VERIFY(STATUS)
               call ESMF_AttributeGet(field,name="UNGRIDDED_NAME",value=ungridded_name,rc=status)
               _VERIFY(STATUS)
               call ESMF_AttributeGet(field,name="UNGRIDDED_COORDS",isPresent=isPresent,rc=status)
               _VERIFY(STATUS)
               if (isPresent) then
                  call ESMF_AttributeGet(field,name="UNGRIDDED_COORDS",itemcount=ungrdsize,rc=status)
                  _VERIFY(STATUS)
                  if ( ungrdsize /= 0 ) then
                     allocate(ungridded_coord(ungrdsize),stat=status)
                     _VERIFY(STATUS)
                     call ESMF_AttributeGet(field,NAME="UNGRIDDED_COORDS",valuelist=ungridded_coord,rc=status)
                     _VERIFY(STATUS)
                  end if
               else
                  ungrdsize = 0
               end if

               deallocate(ungriddedLBound,ungriddedUBound)

               if (ungrdsize > 0) then
                  call MAPL_VarSpecCreateInList(INTSTATE%SRCS(n)%SPEC,          &
                       SHORT_NAME = SHORT_NAME,                                 &
                       LONG_NAME  = LONG_NAME,                                  &
                       UNITS      = UNITS,                                      &
                       DIMS       = DIMS,                                       &
                       UNGRIDDED_DIMS = UNGRD,                                  &
                       UNGRIDDED_NAME = ungridded_name,                       &
                       UNGRIDDED_UNIT = ungridded_unit,                       &
                       UNGRIDDED_COORDS = ungridded_coord,                      &
                       ACCMLT_INTERVAL= avgint,                                 &
                       COUPLE_INTERVAL= REFRESH,                                &
                       VLOCATION  = VLOCATION,                                  &
                       FIELD_TYPE = FIELD_TYPE,                                 &
                       RC=STATUS  )
                  _VERIFY(STATUS)

                  call MAPL_VarSpecCreateInList(INTSTATE%DSTS(n)%SPEC,          &
                       SHORT_NAME = list(n)%field_set%fields(3,m),                        &
                       LONG_NAME  = LONG_NAME,                                  &
                       UNITS      = UNITS,                                      &
                       DIMS       = DIMS,                                       &
                       UNGRIDDED_DIMS = UNGRD,                                  &
                       UNGRIDDED_NAME = ungridded_name,                       &
                       UNGRIDDED_UNIT = ungridded_unit,                       &
                       UNGRIDDED_COORDS = ungridded_coord,                      &
                       ACCMLT_INTERVAL= MAPL_nsecf(list(n)%acc_interval),       &
                       COUPLE_INTERVAL= MAPL_nsecf(list(n)%frequency   ),       &
                       VLOCATION  = VLOCATION,                                  &
                       GRID       = GRID,                                       &
                       FIELD_TYPE = FIELD_TYPE,                                 &
                       RC=STATUS  )
                  _VERIFY(STATUS)
               else

                  call MAPL_VarSpecCreateInList(INTSTATE%SRCS(n)%SPEC,          &
                       SHORT_NAME = SHORT_NAME,                                 &
                       LONG_NAME  = LONG_NAME,                                  &
                       UNITS      = UNITS,                                      &
                       DIMS       = DIMS,                                       &
                       UNGRIDDED_DIMS = UNGRD,                                  &
                       UNGRIDDED_NAME = ungridded_name,                       &
                       UNGRIDDED_UNIT = ungridded_unit,                       &
                       ACCMLT_INTERVAL= avgint,                                 &
                       COUPLE_INTERVAL= REFRESH,                                &
                       VLOCATION  = VLOCATION,                                  &
                       FIELD_TYPE = FIELD_TYPE,                                 &
                       RC=STATUS  )
                  _VERIFY(STATUS)

                  call MAPL_VarSpecCreateInList(INTSTATE%DSTS(n)%SPEC,          &
                       SHORT_NAME = list(n)%field_set%fields(3,m),                        &
                       LONG_NAME  = LONG_NAME,                                  &
                       UNITS      = UNITS,                                      &
                       DIMS       = DIMS,                                       &
                       UNGRIDDED_DIMS = UNGRD,                                  &
                       UNGRIDDED_NAME = ungridded_name,                       &
                       UNGRIDDED_UNIT = ungridded_unit,                       &
                       ACCMLT_INTERVAL= MAPL_nsecf(list(n)%acc_interval),       &
                       COUPLE_INTERVAL= MAPL_nsecf(list(n)%frequency   ),       &
                       VLOCATION  = VLOCATION,                                  &
                       GRID       = GRID,                                       &
                       FIELD_TYPE = FIELD_TYPE,                                 &
                       RC=STATUS  )
                  _VERIFY(STATUS)

               end if
               deallocate(ungrd)
               if (allocated(ungridded_coord)) deallocate(ungridded_coord)

            else

               call MAPL_VarSpecCreateInList(INTSTATE%SRCS(n)%SPEC,    &
                    SHORT_NAME = SHORT_NAME,                                 &
                    LONG_NAME  = LONG_NAME,                                  &
                    UNITS      = UNITS,                                      &
                    DIMS       = DIMS,                                       &
                    ACCMLT_INTERVAL= avgint,                                 &
                    COUPLE_INTERVAL= REFRESH,                                &
                    VLOCATION  = VLOCATION,                                  &
                    FIELD_TYPE = FIELD_TYPE,                                 &
                    RC=STATUS  )
               _VERIFY(STATUS)

               call MAPL_VarSpecCreateInList(INTSTATE%DSTS(n)%SPEC,    &
                    SHORT_NAME = list(n)%field_set%fields(3,m),                        &
                    LONG_NAME  = LONG_NAME,                                  &
                    UNITS      = UNITS,                                      &
                    DIMS       = DIMS,                                       &
                    ACCMLT_INTERVAL= MAPL_nsecf(list(n)%acc_interval),       &
                    COUPLE_INTERVAL= MAPL_nsecf(list(n)%frequency   ),       &
                    VLOCATION  = VLOCATION,                                  &
                    GRID       = GRID,                                       &
                    FIELD_TYPE = FIELD_TYPE,                                 &
                    RC=STATUS  )
               _VERIFY(STATUS)

            endif ! has_ungrid
            deallocate(gridToFieldMap)

         else ! else for if averaged

            REFRESH = MAPL_nsecf(list(n)%acc_interval)
            AVGINT  = MAPL_nsecf( list(n)%frequency )
            call ESMF_AttributeSet(F, NAME='REFRESH_INTERVAL', VALUE=REFRESH, RC=STATUS)
            _VERIFY(STATUS)
            call ESMF_AttributeSet(F, NAME='AVERAGING_INTERVAL', VALUE=AVGINT, RC=STATUS)
            _VERIFY(STATUS)
            call MAPL_StateAdd(IntState%GIM(N), f, rc=status)
            _VERIFY(STATUS)

         endif

! Handle possible regridding through user supplied exchange grid
!---------------------------------------------------------------
         if (associated(IntState%Regrid(n)%PTR)) then
! replace field with newly created fld on grid_out
            field = MAPL_FieldCreate(f, grid_out, rc=status)
            _VERIFY(STATUS)
! add field to state_out
            call MAPL_StateAdd(IntState%Regrid(N)%PTR%state_out, &
                 field, rc=status)
            _VERIFY(STATUS)
         endif

      end do

   end do

   do n=1, nlist
      if (list(n)%disabled) cycle
      if (IntState%average(n)) then
         
         call MAPL_StateCreateFromSpec(IntState%GIM(n), &
              IntState%DSTS(n)%SPEC,   &
              RC=STATUS  )
         _VERIFY(STATUS)

!         create CC
         if (nactual == npes) then
            IntState%CCS(n) = ESMF_CplCompCreate (                  &
                 NAME       = list(n)%collection, & 
                 contextFlag = ESMF_CONTEXT_PARENT_VM,              &
                 RC=STATUS )
            _VERIFY(STATUS)
         else
            IntState%CCS(n) = ESMF_CplCompCreate (                  &
                 NAME       = list(n)%collection, & 
                 petList    = list(n)%peAve, &
                 contextFlag = ESMF_CONTEXT_OWN_VM,              &
                 RC=STATUS )
            _VERIFY(STATUS)
         end if

!         CCSetServ
         call ESMF_CplCompSetServices (IntState%CCS(n), &
                                       GenericCplSetServices, RC=STATUS )
         _VERIFY(STATUS)

         call MAPL_CplCompSetVarSpecs(IntState%CCS(n), &
                                      INTSTATE%SRCS(n)%SPEC,&
                                      INTSTATE%DSTS(n)%SPEC,RC=STATUS)
         _VERIFY(STATUS)

         if (list(n)%monthly) then
            call MAPL_CplCompSetAlarm(IntState%CCS(n), &
                 list(n)%his_alarm, RC=STATUS)
            _VERIFY(STATUS)
         end if

!         CCInitialize
         call ESMF_CplCompInitialize (INTSTATE%CCS(n), &
                                      importState=INTSTATE%CIM(n), &
                                      exportState=INTSTATE%GIM(n), &
                                      clock=CLOCK,           &
                                      userRC=STATUS)
         _VERIFY(STATUS)

         if(list(n)%monthly) then
            ! check if alarm is ringing
            if (.not. ESMF_AlarmIsRinging ( list(n)%his_alarm )) then
               call ESMF_CplCompReadRestart (INTSTATE%CCS(n), &
                                             importState=INTSTATE%CIM(n), &
                                             exportState=INTSTATE%GIM(n), &
                                             clock=CLOCK,           &
                                             userRC=STATUS)
               if (status == ESMF_RC_FILE_READ) then
                  list(n)%partial = .true.
                  STATUS = ESMF_SUCCESS
                  call WRITE_PARALLEL("DEBUG: no cpl restart found, producing partial month")
               end if
               _VERIFY(STATUS)
            end if
         end if
      end if

   end do

    do n=1,nlist
       if (list(n)%disabled) cycle
       if (list(n)%subVm) list(n)%disabled = .true.
    end do


! CFIO
    do n=1,nlist
       if (list(n)%disabled) cycle

!ALT do this all the time       if (list(n)%format == 'CFIO') then
          write(string,'(a,i3.0)') 'STREAM',n

          list(n)%bundle = ESMF_FieldBundleCreate(NAME=string, RC=STATUS)
          _VERIFY(STATUS)

          if(associated(list(n)%levels)) then
             LM = size(list(n)%levels)
          else
             call ESMF_StateGet(INTSTATE%GIM(n), &
                  trim(list(n)%field_set%fields(3,1)), field, rc=status )
             _VERIFY(STATUS)
             call ESMF_FieldGet(field, grid=grid,   rc=status )
             _VERIFY(STATUS)
             call MAPL_GridGet(GRID, globalCellCountPerDim=COUNTS, RC=STATUS)
             _VERIFY(STATUS)
             LM = counts(3)
          endif

          list(n)%slices = 0

          if (associated(IntState%Regrid(n)%PTR)) then
             state_out = INTSTATE%REGRID(n)%PTR%state_out
          else
             state_out = INTSTATE%GIM(n)
          end if

          do m=1,list(n)%field_set%nfields
             call ESMF_StateGet( state_out, &
                  trim(list(n)%field_set%fields(3,m)), field, rc=status )
             _VERIFY(STATUS)

             call MAPL_FieldBundleAdd( list(n)%bundle, field, rc=status )
             _VERIFY(STATUS)

             call ESMF_FieldGet(field, Array=array, grid=bgrid, rc=status)
             _VERIFY(STATUS)
             call ESMF_ArrayGet(array, rank=rank, rc=status)
             _VERIFY(STATUS)
             call ESMF_ArrayGet(array, localarrayList=larrayList, rc=status)
             _VERIFY(STATUS)
             larray => lArrayList(1) ! alias
             call ESMF_GridGet(bgrid, distgrid=bdistgrid, rc=status)
             _VERIFY(STATUS)
             !ALT: we need the rank of the distributed grid
             ! MAPL (and GEOS-5) grid are distributed along X-Y
             ! tilegrids are distributed only along "tile" dimension
             call ESMF_DistGridGet(bdistgrid, dimCount=distRank, rc=status)
             _VERIFY(STATUS)
             call ESMF_LocalArrayGet(larray, totalCount=counts, rc=status)
             _VERIFY(STATUS)

             if(list(n)%field_set%fields(3,m)/=vvarn(n)) then
                nslices = 1
                do k=distRank+1, rank
                   nslices = nslices*counts(k)
                end do
                if(associated(list(n)%levels) .and. rank==3 .and. distRank==2) then
                   list(n)%slices = list(n)%slices + LM
                else
                   list(n)%slices = list(n)%slices + nslices
                end if
             endif
          end do

!       endif
    enddo

    do n=1,nlist
       if (associated(list(n)%peAve)) then
          deallocate(list(n)%peAve)
          NULLIFY(list(n)%peAve)
       end if
    end do
    deallocate(Vvarn)
    deallocate (export)

    do n=1,nlist
       if (list(n)%disabled) cycle
       if (list(n)%format == 'CFIO' .or. list(n)%format == 'CFIOasync') then
          call Get_Tdim (list(n), clock, tm)
          if (associated(list(n)%levels) .and. list(n)%vvars(1) /= "") then
             list(n)%vdata = VerticalData(levels=list(n)%levels,vcoord=list(n)%vvars(1),vscale=list(n)%vscale,vunit=list(n)%vunit,rc=status)
             _VERIFY(status)
          else if (associated(list(n)%levels) .and. list(n)%vvars(1) == "") then
             list(n)%vdata = VerticalData(levels=list(n)%levels,rc=status)
             _VERIFY(status)
          else
             list(n)%vdata = VerticalData(rc=status)
             _VERIFY(status)
          end if
          call list(n)%mNewCFIO%set_param(deflation=list(n)%deflate,rc=status)
          _VERIFY(status)
          call list(n)%mNewCFIO%set_param(chunking=list(n)%chunkSize,rc=status)
          _VERIFY(status)
          call list(n)%mNewCFIO%set_param(nbits=list(n)%nbits,rc=status)
          _VERIFY(status)
          call list(n)%mNewCFIO%set_param(regrid_method=list(n)%conservative,rc=status)
          _VERIFY(status)
          call list(n)%mNewCFIO%set_param(itemOrder=intState%fileOrderAlphabetical,rc=status)
          _VERIFY(status)
          list(n)%timeInfo = TimeData(clock,tm,MAPL_nsecf(list(n)%frequency),IntState%stampoffset(n))
          if (list(n)%timeseries_output) then
             list(n)%trajectory = HistoryTrajectory(trim(list(n)%trackfile),rc=status)
             _VERIFY(status)
             call list(n)%trajectory%initialize(list(n)%items,list(n)%bundle,list(n)%timeInfo,vdata=list(n)%vdata,recycle_track=list(n)%recycle_track,rc=status)
             _VERIFY(status)
          else
             if (trim(list(n)%output_grid_label)/='') then
                pgrid => IntState%output_grids%at(trim(list(n)%output_grid_label)) 
                call list(n)%mNewCFIO%CreateFileMetaData(list(n)%items,list(n)%bundle,list(n)%timeInfo,ogrid=pgrid,vdata=list(n)%vdata,rc=status)
                _VERIFY(status)
             else
                call list(n)%mNewCFIO%CreateFileMetaData(list(n)%items,list(n)%bundle,list(n)%timeInfo,vdata=list(n)%vdata,rc=status)
                _VERIFY(status)
             end if
             collection_id = o_Clients%add_hist_collection(list(n)%mNewCFIO%metadata)
             call list(n)%mNewCFIO%set_param(write_collection_id=collection_id)
          end if
       end if
   end do

! Echo History List Data Structure
! --------------------------------

   if( MAPL_AM_I_ROOT() ) then

      print *
      print *, 'Independent Output Export States:'
      print *, '---------------------------------'
      do n=1,nstatelist
         print *, n,trim(statelist(n))
      enddo
      print *

      do n=1,nlist
         if (list(n)%disabled) cycle
         print *, 'Initializing Output Stream: ',  trim(list(n)%filename)
         print *, '--------------------------- '
         print *, '      Format: ',  trim(list(n)%format)
         print *, '        Mode: ',  trim(list(n)%mode)
         print *, '       Nbits: ',       list(n)%nbits
         print *, '      Slices: ',       list(n)%Slices
         print *, '     Deflate: ',       list(n)%deflate
         print *, '   Frequency: ',       list(n)%frequency
         if(IntState%average(n) ) &
              print *, 'Acc_Interval: ',  list(n)%acc_interval
         print *, '    Ref_Date: ',       list(n)%ref_date
         print *, '    Ref_Time: ',       list(n)%ref_time
         print *, '    Duration: ',       list(n)%duration
         if( list(n)%end_date.ne.-999 ) then
         print *, '    End_Date: ',       list(n)%end_date
         print *, '    End_Time: ',       list(n)%end_time
         endif
         
         block 
            integer :: im_world, jm_world,dims(3)
            pgrid => IntState%output_grids%at(trim(list(n)%output_grid_label))
            if (associated(pgrid)) then
               call MAPL_GridGet(pgrid,globalCellCountPerDim=dims,RC=status)
               _VERIFY(status)
               print *, ' Output RSLV: ',dims(1),dims(2)
            end if
         end block
         
         select case ( list(n)%xyoffset   )
                case (0)
                           print *, '   XY-offset: ',list(n)%xyoffset,'  (DcPc: Dateline Center, Pole Center)'
                case (1)
                           print *, '   XY-offset: ',list(n)%xyoffset,'  (DePc: Dateline Edge, Pole Center)'
                case (2)
                           print *, '   XY-offset: ',list(n)%xyoffset,'  (DcPe: Dateline Center, Pole Edge)'
                case (3)
                           print *, '   XY-offset: ',list(n)%xyoffset,'  (DePe: Dateline Edge, Pole Edge)'
                case default
                _ASSERT(.false.,'needs informative message')
         end select

         !print *, '      Fields: ',((trim(list(n)%field_set%fields(3,m)),' '),m=1,list(n)%field_set%nfields)
         write (*,'(A)',ADVANCE='NO') '      Fields: '
         do m=1,list(n)%field_set%nfields
            if( trim(list(n)%field_set%fields(3,m)).ne.BLANK ) then
               write (*,'(A,X)',ADVANCE='NO') trim(list(n)%field_set%fields(3,m))
            endif
         enddo
         ! Now advance the write
         write (*,*)
         do m=1,list(n)%field_set%nfields
            if( trim(list(n)%field_set%fields(4,m)).ne.BLANK ) then
                print *, '   CPLFUNC Variable: ',trim(list(n)%field_set%fields(3,m)),'  Function: ',trim(list(n)%field_set%fields(4,m))
            endif
         enddo

         if( list(n)%vvars(1)/="" ) then
                                           print *, '   Vert Interp  Var: ',  trim(list(n)%vvars(1))
            if( trim(list(n)%vunit)/=""  ) print *, '   Vertical    Unit: ',  trim(list(n)%vunit)
            if(      list(n)%vscale/=1.0 ) print *, '   Vertical Scaling: ',       list(n)%vscale
                                           print *, '   Vertical  Levels: ',       list(n)%levels
         elseif(associated(list(n)%levels)) then
                                           print *, '   Vertical  Levels: ',  nint(list(n)%levels)
         endif

         print *
         print *
      enddo
   endif

    doAsync = .false.
    do n=1,nlist
       if (list(n)%format == 'CFIOasync') then
          doAsync = .true.
          exit
       end if
    enddo

    if (doAsync) then
       call MAPL_Get(GENSTATE,mapl_comm=mapl_comm,rc=status)
       _VERIFY(STATUS)
       if (mapl_comm%io%size == 0) then
          call WRITE_PARALLEL('You requested the asynchronous option but did not allocate any resources')
          _ASSERT(.false.,'needs informative message')
       end if
    end if

    deallocate(stateListAvail)
    deallocate( statelist )

    call MAPL_GenericInitialize( gc, import, dumexport, clock, rc=status )
    _VERIFY(status)

    call MAPL_TimerOff(GENSTATE,"Initialize")
    call MAPL_TimerOff(GENSTATE,"TOTAL")

    _RETURN(ESMF_SUCCESS)

  contains

    subroutine wildCardExpand(rc)
      integer, optional, intent(out) :: rc

      ! local vars
      integer :: status
      
      integer, pointer :: newExpState(:) => null()
      type(newCFIOitemVectorIterator) :: iter
      type(newCFIOitem), pointer :: item
      integer :: nfields
      integer :: nregex
      character(len=ESMF_MAXSTR), allocatable :: fieldNames(:)
      type(ESMF_State) :: expState
      type(newCFIOItemVector), pointer  :: newItems
      character(ESMF_MAXSTR) :: fldName, stateName
      logical :: expand
      integer :: k, i
      
      do n = 1, nlist
         if (.not.list(n)%regex) cycle
         fld_set => list(n)%field_set
         nfields = fld_set%nfields

         allocate(needSplit(nfields), regexList(nfields), stat=status)
         _VERIFY(status)
         regexList = ""
         
         allocate(newItems, stat=status); _VERIFY(status)

         needSplit = .false.
       
         iter = list(n)%items%begin()
         m = 0 ! m is the "old" field-index
         do while(iter /= list(n)%items%end())
            item => iter%get()
            if (item%itemType == ItemTypeScalar) then
               expand = hasRegex(fldName=item%xname, rc=status)
               _VERIFY(status)
               if (.not.expand) call newItems%push_back(item)
            else if (item%itemType == ItemTypeVector) then
               ! Lets' not allow regex expand for vectors
               expand = hasRegex(fldName=item%xname, rc=status)
               _VERIFY(status)
               expand = expand.or.hasRegex(fldName=item%yname, rc=status)
               _VERIFY(status)
               if (.not.expand) call newItems%push_back(item)
            end if
   
            call iter%next()
         end do

         ! re-pack field_set
         nregex = count(needSplit)

         if (nregex /= 0) then
            nfields = nfields - nregex
            allocate(newExpState(nfields), stat=status)
            _VERIFY(status)
            allocate(newFieldSet, stat=status); _VERIFY(status)
            allocate(fields(4,nfields), stat=status); _VERIFY(status)
            do k = 1, size(fld_set%fields,1)
               fields(k,:) = pack(fld_set%fields(k,:), mask=.not.needSplit)
            end do
            newFieldSet%fields => fields
            newFieldSet%nfields = nfields

            newExpState = pack(list(n)%expState, mask=.not.needSplit)

            ! regex and add the expanded fields to the list

            do k = 1, size(needSplit) ! loop over "old" fld_set
               if (.not. needSplit(k)) cycle

               stateName = fld_set%fields(2,k)
               expState = export(list(n)%expSTATE(k))
               
               call MAPL_WildCardExpand(state=expState, regexStr=regexList(k), &
                    fieldNames=fieldNames, RC=status)
               _VERIFY(STATUS)

               do i=1,size(fieldNames)
                  fldName = fieldNames(i)
                  call appendFieldSet(newFieldSet, fldName, &
                       stateName=stateName, &
                       aliasName=fldName, &
                       specialName='', rc=status)

                  _VERIFY(status)
                  ! append expState
                  call appendArray(newExpState,idx=list(n)%expState(k),rc=status)
                  _VERIFY(status)

                  item%itemType = ItemTypeScalar
                  item%xname = trim(fldName)
                  item%yname = ''

                  call newItems%push_back(item)

               end do

               deallocate(fieldNames)
            end do

            ! set nfields to ...

            list(n)%field_set => newFieldSet
            deallocate(list(n)%expState)
            list(n)%expState => newExpState
            list(n)%items = newItems
         end if
         ! clean-up
         deallocate(needSplit, regexList)
      enddo

      _RETURN(ESMF_SUCCESS)
    end subroutine wildCardExpand

    function hasRegex(fldName, rc) result(haveIt)
      logical :: haveIt
      character(len=*),  intent(in)   :: fldName
      integer, optional, intent(out) :: rc

      ! local vars
      integer :: k
      integer :: status
      character(len=ESMF_MAXSTR) :: tmpString
      character(len=1), parameter :: BOR = "`"
      character(len=1), parameter :: EOR = "`"
      
      ! and these vars are declared in the caller
      ! fld_set
      ! m

      haveIt = .false.

      m = m + 1
      _ASSERT(fldName == fld_set%fields(3,m), 'Incorrect order') ! we got "m" right

      tmpString = adjustl(fldName)
      _ASSERT(len_trim(tmpString) > 0, "Empty name not allowed")
      
      ! begin-of-regex
      haveIt = tmpString(1:1) == BOR

      needSplit(m) = haveIt

      if (haveIt) then
         ! search for end-of-regex
         k = index(tmpString(2:), EOR)
         _ASSERT(k>1, "No EOR (end-of-regex)")
         ! strip BOR and EOR
         fld_set%fields(1,m) = tmpString(2:k)
         fld_set%fields(3,m) = tmpString(2:k)
         regexList(m) = tmpString(2:k)
      end if


      _RETURN(ESMF_SUCCESS)
     
    end function hasRegex

    subroutine MAPL_WildCardExpand(state, regexStr, fieldNames, rc)
      type(ESMF_State), intent(in) :: state
      character(len=*), intent(in) :: regexStr
      character(len=*), allocatable, intent(inout) :: fieldNames(:)
      integer, optional, intent(out) :: rc

      ! local vars
      integer :: nitems, i, count
      integer :: status
      character (len=ESMF_MAXSTR), allocatable  :: itemNameList(:)
      type(ESMF_StateItem_Flag),   allocatable  :: itemtypeList(:)
      type(regex_type) :: regex
      logical :: match
      integer :: nmatches(2, ESMF_MAXSTR)
      character(len=ESMF_MAXSTR), allocatable :: tmpFldNames(:)

      call ESMF_StateGet(state, itemcount=nitems,  rc=status)
      _VERIFY(status)

      allocate(itemNameList(nitems), itemtypeList(nitems), stat=status)
      _VERIFY(status)

      call ESMF_StateGet(state,itemNameList=itemNameList,&
                       itemTypeList=itemTypeList,RC=STATUS)
    _VERIFY(STATUS)
      call regcomp(regex,trim(regexStr),'xmi',status=status)
    _VERIFY(STATUS)

      if (.not.allocated(fieldNames)) then
         allocate(fieldNames(0), stat=status)
         _VERIFY(status)
      end if
      count = size(fieldNames)
      
      do i=1,nitems
         if (itemTypeList(i) /= ESMF_STATEITEM_FIELD) cycle
         
         match = regexec(regex,trim(itemNameList(i)),nmatches,status=status)
!non-zero indicate no match         _VERIFY(status)
         if (match) then
            ! debugging print
            if (MAPL_AM_I_ROOT()) then
               print *,'DEBUG:adding field to the list '//trim(itemNameList(i))
            end if

            count = count + 1
            ! logic to grow the list
            allocate(tmpFldNames(count), stat=status)
            _VERIFY(status)
            tmpFldNames(1:count-1) = fieldNames
            call move_alloc(tmpFldNames, fieldNames)

            fieldNames(count) = itemNameList(i)
         end if
         
      end do

      call regfree(regex)
      deallocate(itemNameList, itemtypeList)

      _RETURN(ESMF_SUCCESS)
    end subroutine MAPL_WildCardExpand
    
    subroutine split4dFields(rc)
      integer, optional, intent(out) :: rc

      ! local vars
      integer :: status
      
      integer, pointer :: newExpState(:) => null()
      type(newCFIOitemVectorIterator) :: iter
      type(newCFIOitem), pointer :: item
      integer :: nfields
      integer :: nfield4d
      type(ESMF_Field), pointer :: splitFields(:) => null()
      type(ESMF_State) :: expState
      type(newCFIOItemVector), pointer  :: newItems
      character(ESMF_MAXSTR) :: fldName, stateName
      logical :: split
      integer :: k, i
      
      ! Restrictions:
      ! 1) we do not split 4d vectors
      ! 2) use alias for split-field name base
      do n = 1, nlist
         if (.not.list(n)%splitField) cycle
         fld_set => list(n)%field_set
         nfields = fld_set%nfields
         allocate(needSplit(nfields), fldList(nfields), stat=status)
         _VERIFY(status)

         allocate(newItems, stat=status); _VERIFY(status)

         needSplit = .false.
       
         iter = list(n)%items%begin()
         m = 0 ! m is the "old" field-index
         split = .false.
         do while(iter /= list(n)%items%end())
            item => iter%get()
            if (item%itemType == ItemTypeScalar) then
               split = has4dField(fldName=item%xname, rc=status)
               _VERIFY(status)
               if (.not.split) call newItems%push_back(item)
            else if (item%itemType == ItemTypeVector) then
               ! Lets' not allow 4d split for vectors (at least for now);
               ! it is easy to implement; just tedious

               split = has4dField(fldName=item%xname, rc=status)
               _VERIFY(status)
               split = split.or.has4dField(fldName=item%yname, rc=status)
               _VERIFY(status)
               if (.not.split) call newItems%push_back(item)
             
               _ASSERT(.not. split, 'vectors of 4d fields not allowed yet')
             
            end if
   
            call iter%next()
         end do

         ! re-pack field_set
         nfield4d = count(needSplit)

         if (nfield4d /= 0) then
            nfields = nfields - nfield4d
            allocate(newExpState(nfields), stat=status)
            _VERIFY(status)
            ! do the same for statename
            !create/if_needed newFieldSet (nfields=0;allocate%fields)
            !          if (associated(newFieldSet%fields)) deallocate(newFieldSet%fields) 
            !          items = list(n)%items
            allocate(newFieldSet, stat=status); _VERIFY(status)
            allocate(fields(4,nfields), stat=status); _VERIFY(status)
            do k = 1, size(fld_set%fields,1) ! 4
               fields(k,:) = pack(fld_set%fields(k,:), mask=.not.needSplit)
            end do
            newFieldSet%fields => fields
            newFieldSet%nfields = nfields

            newExpState = pack(list(n)%expState, mask=.not.needSplit)

            ! split and add the splitted fields to the list

            do k = 1, size(needSplit) ! loop over "old" fld_set
               if (.not. needSplit(k)) cycle

               call MAPL_FieldSplit(fldList(k), splitFields, RC=status)
               _VERIFY(STATUS)
               stateName = fld_set%fields(2,k)

               expState = export(list(n)%expSTATE(k))

               do i=1,size(splitFields)
                  call ESMF_FieldGet(splitFields(i), name=fldName, &
                       rc=status)
                  _VERIFY(status)

                  call appendFieldSet(newFieldSet, fldName, &
                       stateName=stateName, &
                       aliasName=fldName, &
                       specialName='', rc=status)

                  _VERIFY(status)
                  ! append expState
                  call appendArray(newExpState,idx=list(n)%expState(k),rc=status)
                  _VERIFY(status)

                  call MAPL_StateAdd(expState, field=splitFields(i), rc=status)
                  _VERIFY(status)

                  item%itemType = ItemTypeScalar
                  item%xname = trim(fldName)
                  item%yname = ''

                  call newItems%push_back(item)

               end do

               deallocate(splitFields)
               NULLIFY(splitFields)
            end do

            ! set nfields to ...

            list(n)%field_set => newFieldSet
            deallocate(list(n)%expState)
            list(n)%expState => newExpState
            list(n)%items = newItems
         end if
         ! clean-up
         deallocate(needSplit, fldList)
      enddo

      _RETURN(ESMF_SUCCESS)
    end subroutine split4dFields

    function has4dField(fldName, rc) result(have4d)
      logical :: have4d
      character(len=*),  intent(in)   :: fldName
      integer, optional, intent(out) :: rc

      ! local vars
      integer :: k
      integer :: fldRank
      integer :: status
      type(ESMF_State) :: exp_state
      type(ESMF_Field) :: fld
      type(ESMF_FieldStatus_Flag) :: fieldStatus
      
      ! and these vars are declared in the caller
      ! fld_set
      ! m

      have4d = .false.
      fldRank = 0

      m = m + 1
      _ASSERT(fldName == fld_set%fields(3,m), 'Incorrect order') ! we got "m" right
      
      k = list(n)%expSTATE(m)
      exp_state = export(k)
   
      call MAPL_StateGet(exp_state,fldName,fld,rc=status )
      _VERIFY(status)

      call ESMF_FieldGet(fld, status=fieldStatus, rc=status)
      _VERIFY(STATUS)

      if (fieldStatus /= ESMF_FIELDSTATUS_COMPLETE) then
         call MAPL_AllocateCoupling(fld, rc=status)
         _VERIFY(STATUS)
      end if

      call ESMF_FieldGet(fld,dimCount=fldRank,rc=status)
      _VERIFY(status)

      _ASSERT(fldRank < 5, "unsupported rank")
      
      have4d = (fldRank == 4)
      if (have4d) then
         fldList(m) = fld
      end if
      needSplit(m) = have4d

      _RETURN(ESMF_SUCCESS)
     
    end function has4dField

    subroutine appendArray(array, idx, rc)
      integer, pointer,  intent(inout)   :: array(:)
      integer, intent(in) :: idx
      integer, optional, intent(out) :: rc

     ! local vars
     integer :: n
     integer :: k
     integer :: status
     integer, pointer :: tmp(:)
     
     if (.not.associated(array)) then
        _RETURN(ESMF_FAILURE)
     end if

     k = size(array)
     n = k + 1
     allocate(tmp(n), stat=status) ; _VERIFY(status)
     tmp(1:k) = array
     tmp(n) = idx
     
     deallocate(array)
     array => tmp
     
     _RETURN(ESMF_SUCCESS)
     
   end subroutine appendArray
   
   subroutine appendFieldSet(fldset, fldName, stateName, aliasName, specialName, rc)
     type(FieldSet),  intent(inout)   :: fldset
     character(len=*), intent(in) :: fldName, stateName
     character(len=*), intent(in) :: aliasName, specialName
     integer, optional, intent(out) :: rc
     
     ! local vars
     integer :: nn, mm
     integer :: k
     integer :: status
     character(len=ESMF_MAXSTR), pointer :: flds(:,:) => null()
     
    ! if (.not.associated(fldset%fields)) then
    !    _RETURN(ESMF_FAILURE)
    ! end if
     
     mm = size(fldset%fields, 1)
     _ASSERT(mm == 4, 'wrong size for fields')
     k = size(fldset%fields, 2)
     nn = k + 1
     allocate(flds(mm,nn), stat=status) ; _VERIFY(status)
     flds(:,1:k) = fldset%fields
     flds(1,nn) = fldName
     flds(2,nn) = stateName
     flds(3,nn) = aliasName
     flds(4,nn) = specialName
     
     deallocate( fldSet%fields, stat=status )
     _VERIFY(STATUS)
     fldset%fields => flds

     fldSet%nfields = nn
     
     _RETURN(ESMF_SUCCESS)
     
   end subroutine appendFieldSet
   
    function extract_unquoted_item(string_list) result(item)
       character(:), allocatable :: item
       character(*), intent(in) :: string_list
       
       integer :: i
       integer :: j
       
       character(1) :: QUOTE = "'"
       
       i = index(string_list(  1:), QUOTE)
       j = index(string_list(i+1:), QUOTE)+i
       if( i.ne.0 ) then
          item = adjustl( string_list(i+1:j-1) )
       else
          item = adjustl( string_list)
       endif
    end function extract_unquoted_item
    

    subroutine parse_fields(cfg, label, field_set, items, rc)
       type(ESMF_Config), intent(inout) :: cfg
       character(*), intent(in) :: label
       type (FieldSet), intent(inout) :: field_set
       type(newCFIOitemVector), intent(inout), optional :: items
       integer, optional, intent(out) :: rc
       logical :: table_end
       logical :: vectorDone
       integer :: m
       character(ESMF_MAXSTR), pointer:: fields (:,:)

       type(newCFIOitem) :: item
       integer :: status
       
       call ESMF_ConfigFindLabel ( cfg, label=label//':', rc=status)
       _VERIFY(status)

       table_end = .false.
       m = 0
       do while (.not.table_end)
          m = m+1

! Get EXPORT Name
! ---------------
          call ESMF_ConfigGetAttribute ( cfg,value=export_name,rc=STATUS)
          if (status /= ESMF_SUCCESS)  then
              if( MAPL_AM_I_ROOT(vm) ) then
                  print *
                  print *, '**************************************************************'
                  print *, 'Attributes NOT set for Collection: ',trim( list(n)%collection )
                  print *, '**************************************************************'
                  print *
              endif
          endif
          _VERIFY(STATUS)
          export_name = extract_unquoted_item(export_name)

! Get GC Name
! ------------
          call ESMF_ConfigGetAttribute ( cfg,value=tmpstring ,rc=STATUS)
          _VERIFY(STATUS)
          if( trim(tmpstring) == ',' )  then
              call ESMF_ConfigGetAttribute ( cfg,value=component_name,rc=STATUS)
              _VERIFY(STATUS)
          else
              component_name = tmpstring
          endif

          component_name = extract_unquoted_item(component_name)

! Get Possible ALIAS Name
! -----------------------
          call ESMF_ConfigGetAttribute ( cfg,value=tmpstring ,rc=STATUS)
          if( trim(tmpstring) == ',' )  then
              call ESMF_ConfigGetAttribute ( cfg,value=export_alias,default=export_name,rc=STATUS)
          else
              if( trim(tmpstring) /= ' ' )  then
                  export_alias = tmpstring
              else
                  export_alias = export_name
              endif
           endif

           export_alias = extract_unquoted_item(export_alias)
!         if this is a bundle and we did not provide alias, strip off bundle name
          i = index(export_alias(1:),"%")
          if (i.ne.0 .and. scan(trim(export_alias),'()^/*+-')==0 ) export_alias = adjustl( export_alias(i+1:) )

! Get Possible COUPLER Function
! -----------------------------
          call ESMF_ConfigGetAttribute ( cfg,value=tmpstring ,rc=STATUS)
          if( trim(tmpstring) == ',' )  then
              call ESMF_ConfigGetAttribute ( cfg,value=coupler_function_name,default=BLANK,rc=STATUS)
          else
              if( trim(tmpstring) /= ' ' )  then
                  coupler_function_name = tmpstring
              else
                  coupler_function_name = BLANK
              endif
          endif
          coupler_function_name = extract_unquoted_item(coupler_function_name)
! convert to uppercase
          tmpstring = ESMF_UtilStringUpperCase(coupler_function_name,rc=status)
          _VERIFY(status)
! -------------

          call ESMF_ConfigNextLine  ( cfg,tableEnd=table_end,rc=STATUS )
          _VERIFY(STATUS)
          vectorDone=.false.

          idx = index(export_name,";")
          if (idx ==0) then
             item%itemType = ItemTypeScalar
             item%xname = trim(export_alias)
          else
             item%itemType = ItemTypeVector
          end if
          VECTORPAIR: do while(.not.vectorDone)
             allocate( fields(4,m), stat=status )
             _VERIFY(STATUS)

             idx = index(export_name,";")
             if (idx == 0) then
                vectorDone=.true.
             else
                f1copy = export_name(idx+1:)
                export_name = export_name(1:idx-1)
                idx = index(export_alias,";")
                _ASSERT(idx > 0,'needs informative message')
                f3copy = export_alias(idx+1:)
                export_alias = export_alias(1:idx-1)
             end if

             if( m==1 ) then
                fields(1,m)     = export_name
                fields(2,m)     = component_name
                fields(3,m)     = export_alias
                fields(4,m)     = coupler_function_name
             else
                fields(1,1:m-1) = field_set%fields(1,:)
                fields(2,1:m-1) = field_set%fields(2,:)
                fields(3,1:m-1) = field_set%fields(3,:)
                fields(4,1:m-1) = field_set%fields(4,:)
                fields(1,m)     = export_name
                fields(2,m)     = component_name
                fields(3,m)     = export_alias
                fields(4,m)     = coupler_function_name
                deallocate (field_set%fields)
             endif
             allocate( field_set%fields(4,m), stat=status)
             _VERIFY(STATUS)
             field_set%fields = fields
             deallocate (fields)
             if (.not.vectorDone) then
!ALT: next if-block builds a vectorList for proper processing of vectors
!     by MAPL_HorzTransformRun done in MAPL_CFIO. 
!     The logic of construction the vectorList is somewhat flawed
!     it works for vectors with two components (i.e. U;V), 
!     but ideally should be more general

                item%xname = trim(export_alias)
                item%yname = trim(f3copy)

                export_name = f1copy
                export_alias = f3copy
                m = m + 1

             end if
          end do VECTORPAIR
          if(present(items)) call items%push_back(item)
       enddo
       field_set%nfields = m

       end subroutine parse_fields

    
 end subroutine Initialize

!======================================================


 subroutine Run ( gc, import, export, clock, rc )

! !ARGUMENTS:

    type(ESMF_GridComp),    intent(inout) :: gc     
    type(ESMF_State),       intent(inout) :: import 
    type(ESMF_State),       intent(inout) :: export 
    type(ESMF_Clock),       intent(inout) :: clock  
    integer, optional,      intent(  out) :: rc     
                                                    
                                                    
! Locals

    type(MAPL_MetaComp),  pointer  :: GENSTATE
    type(HistoryCollection),   pointer  :: list(:)
    type(HISTORY_STATE),  pointer  :: IntState
    type(HISTORY_wrap)             :: wrap
    integer                        :: nlist
    character(len=ESMF_MAXSTR)     :: fntmpl
    character(len=ESMF_MAXSTR),pointer     :: filename(:)
    integer                        :: n,m
    logical                        :: NewSeg
    logical, allocatable           :: Writing(:)
    type(ESMF_State)               :: state_out
    integer                        :: nymd, nhms
    character(len=ESMF_MAXSTR)     :: DateStamp
    integer                        :: CollBlock
    type(MAPL_Communicators)       :: mapl_Comm
    type(ESMF_Time)                :: current_time

!   variables for "backwards" mode
    logical                        :: fwd
    logical, allocatable           :: Ignore(:)

!   ErrLog vars
    integer                        :: status

!=============================================================================

! Begin...

    _UNUSED_DUMMY(import)
    _UNUSED_DUMMY(export)

! Retrieve the pointer to the state
!----------------------------------

    call ESMF_GridCompGetInternalState(gc, wrap, status)
    _VERIFY(status)
    IntState => wrap%ptr

! the collections
!----------------

    list => IntState%list
    nlist = size(list)

    CollBlock = IntState%blocksize
! Retrieve the pointer to the generic state
!------------------------------------------

    call MAPL_GetObjectFromGC ( gc, GENSTATE, RC=STATUS)
    _VERIFY(STATUS)

    call MAPL_Get(GENSTATE,mapl_comm=mapl_comm,rc=status)
    _VERIFY(STATUS)

    call MAPL_TimerOn(GENSTATE,"TOTAL")
    call MAPL_TimerOn(GENSTATE,"Run"  )

!   Get clocks' direction
    FWD = .not. ESMF_ClockIsReverse(clock)
 
   allocate(Ignore (nlist), stat=status)
   _VERIFY(STATUS)
   Ignore = .false.

  ! decide if clock direction and collections' backwards mode agree

   do n=1,nlist
      if (list(n)%backwards .eqv. FWD) Ignore(n) = .true.
   end do

!  Perform arithemetic parser operations
   do n=1,nlist
    if(Ignore(n)) cycle
    if ( Any(list(n)%ReWrite) ) then 
     call MAPL_TimerOn(GENSTATE,"-ParserRun")
     if( (.not.list(n)%disabled .and. IntState%average(n)) ) then
      call MAPL_RunExpression(IntState%CIM(n),list(n)%field_set%fields,list(n)%tmpfields, &
         list(n)%ReWrite,list(n)%field_set%nfields,RC=STATUS)
      _VERIFY(STATUS)
     end if
     if( (.not.list(n)%disabled) .and. (.not.IntState%average(n)) ) then
      call MAPL_RunExpression(IntState%GIM(n),list(n)%field_set%fields,list(n)%tmpfields, &
         list(n)%ReWrite,list(n)%field_set%nfields,RC=STATUS)
      _VERIFY(STATUS)
     end if
     call MAPL_TimerOff(GENSTATE,"-ParserRun")
    endif
   end do

! We could make a copy for precision conversion here, if needed
! However, this is not very efficient. Copy is needed if it is
! time-averaged (i.e. couplers will be run), or if it is time to
! write instantaneous collection
!@   do n=1,nlist
!@      do m=1,list(n)%field_set%nfields
!@         if (list(n)%r8_to_r4(m)) then
!@            call MAPL_FieldCopy(from=list(n)%r8(m), to=list(n)%r4(m), rc=status)
!@            _VERIFY(status)
!@         end if
!@      end do
!@   end do

! Couplers are done here for now
!-------------------------------

    call MAPL_TimerOn(GENSTATE,"--Couplers")
    do n = 1, nlist
       if(Ignore(n)) cycle
       if (.not.list(n)%disabled .and. IntState%average(n)) then
          ! R8 to R4 copy (if needed!)
          do m=1,list(n)%field_set%nfields
             if (list(n)%r8_to_r4(m)) then
                call MAPL_FieldCopy(from=list(n)%r8(m), &
                                    to=list(n)%r4(m), rc=status)
                _VERIFY(status)
             end if
          end do
          
          call ESMF_CplCompRun (INTSTATE%CCS(n), &
                                importState=INTSTATE%CIM(n), &
                                exportState=INTSTATE%GIM(n), &
                                clock=CLOCK,           &
                                userRC=STATUS)
          _VERIFY(STATUS)
       end if
    end do
    call MAPL_TimerOff(GENSTATE,"--Couplers")
            
! Check for History Output
! ------------------------

   allocate(Writing (nlist), stat=status)
   _VERIFY(STATUS)
   allocate(filename(nlist), stat=status)
   _VERIFY(STATUS)

  ! decide if we are writing based on alarms

   do n=1,nlist
      if (list(n)%disabled .or. ESMF_AlarmIsRinging(list(n)%end_alarm) ) then
         list(n)%disabled = .true.
         Writing(n) = .false.
      else if (list(n)%timeseries_output) then
         Writing(n) = .true.
      else
         Writing(n) = ESMF_AlarmIsRinging ( list(n)%his_alarm )
      endif

!      if(Writing(n)) then
!         call ESMF_AlarmRingerOff( list(n)%his_alarm,rc=status )
!         _VERIFY(STATUS)
!      end if

      if (Ignore(n)) then
         ! "Exersise" the alarms and then do nothing
         Writing(n) = .false.
!         if (ESMF_AlarmIsRinging ( list(n)%his_alarm )) then
!            call ESMF_AlarmRingerOff( list(n)%his_alarm,rc=status )
!            _VERIFY(STATUS)
!         end if
         if (ESMF_AlarmIsRinging ( list(n)%seg_alarm )) then
            call ESMF_AlarmRingerOff( list(n)%seg_alarm,rc=status )
            _VERIFY(STATUS)
         end if
      end if

       if (writing(n) .and. .not.IntState%average(n)) then
          ! R8 to R4 copy (if needed!)
          do m=1,list(n)%field_set%nfields
             if (list(n)%r8_to_r4(m)) then
                call MAPL_FieldCopy(from=list(n)%r8(m), &
                                    to=list(n)%r4(m), rc=status)
                _VERIFY(status)
             end if
          end do
       end if

       ! Check for new segment
       !----------------------

       NewSeg = ESMF_AlarmIsRinging ( list(n)%seg_alarm )

       if( NewSeg) then 
          call ESMF_AlarmRingerOff( list(n)%seg_alarm,rc=status )
          _VERIFY(STATUS)
       endif

       if( NewSeg .and. list(n)%unit /= 0 .and. list(n)%duration /= 0 ) then
          if (list(n)%unit > 0 ) then
             call FREE_FILE( list(n)%unit )
          end if
          list(n)%unit = 0
       endif

   end do

   if(any(Writing)) call WRITE_PARALLEL("")

! Write Id and time
! -----------------

   call MAPL_TimerOn(GENSTATE,"--I/O")

   call MAPL_TimerOn(GENSTATE,"----IO Create")

   if (any(writing)) call o_Clients%set_optimal_server(count(writing))

   OPENLOOP: do n=1,nlist
      if( Writing(n) ) then

         call get_DateStamp ( clock, DateStamp=DateStamp,  &
              OFFSET = INTSTATE%STAMPOFFSET(n),            &
                                                 rc=status )
         _VERIFY(STATUS)

         if (trim(INTSTATE%expid) == "") then
            fntmpl =          trim(list(n)%filename)
         else
            fntmpl = "%s." // trim(list(n)%filename)
         endif

         if (trim(list(n)%template) /= "") then
            fntmpl = trim(fntmpl) // "." //trim(list(n)%template)
         endif

         read(DateStamp( 1: 8),'(i8.8)') nymd
         read(DateStamp(10:15),'(i6.6)') nhms

         call fill_grads_template ( filename(n), fntmpl, &
              experiment_id=trim(INTSTATE%expid), &
              nymd=nymd, nhms=nhms, rc=status ) ! here is where we get the actual filename of file we will write
         _VERIFY(STATUS)

         if(list(n)%monthly .and. list(n)%partial) then
            filename(n)=trim(filename(n)) // '-partial'
            list(n)%currentFile = filename(n)
         end if

         if( NewSeg) then 
            list(n)%partial = .false.
         endif

         if (list(n)%timeseries_output) then
            if (list(n)%unit.eq.0) then
               if (mapl_am_i_root()) write(6,*)"Sampling to new file: ",trim(filename(n))
               call list(n)%trajectory%close_file_handle(rc=status)
               _VERIFY(status)
               call list(n)%trajectory%create_file_handle(filename(n),rc=status) 
               _VERIFY(status)
               list(n)%currentFile = filename(n)
               list(n)%unit = -1
            end if
            list(n)%currentFile = filename(n)
         else
            if( list(n)%unit.eq.0 ) then
               if (list(n)%format == 'CFIO') then
                  call list(n)%mNewCFIO%modifyTime(oClients=o_Clients,rc=status)
                  _VERIFY(status)
                  list(n)%currentFile = filename(n)
                  list(n)%unit = -1
               else
                  list(n)%unit = GETFILE( trim(filename(n)),all_pes=.true.)
               end if
            end if
         end if

         if(  MAPL_AM_I_ROOT() ) then
              if (index(list(n)%format,'flat') == 0 .and. (.not.list(n)%timeseries_output)) &
              write(6,'(1X,"Writing: ",i6," Slices to File:  ",a)') &
                    list(n)%slices,trim(list(n)%currentFile)
         endif

      end if
!
   enddo OPENLOOP
   call MAPL_TimerOff(GENSTATE,"----IO Create")

   call MAPL_TimerOn(GENSTATE,"----IO Write")
   call MAPL_TimerOn(GENSTATE,"-----IO Post")
   POSTLOOP: do n=1,nlist

      OUTTIME: if( Writing(n) ) then

         if (associated(IntState%Regrid(n)%PTR)) then
            state_out = INTSTATE%REGRID(n)%PTR%state_out

            if (.not. IntState%Regrid(n)%PTR%ontiles) then
               if (IntState%Regrid(n)%PTR%regridType == MAPL_T2G2G) then
                  call RegridTransformT2G2G(IntState%GIM(n), &
                       IntState%Regrid(n)%PTR%xform, &
                       IntState%Regrid(n)%PTR%xformNtv, &
                       state_out, &
                       IntState%Regrid(n)%PTR%LocIn, &
                       IntState%Regrid(n)%PTR%LocOut, &
                       IntState%Regrid(n)%PTR%LocNative, &
                       IntState%Regrid(n)%PTR%ntiles_in, &
                       IntState%Regrid(n)%PTR%ntiles_out,&
                       rc=status)
               else
                  call RegridTransform(IntState%GIM(n), &
                       IntState%Regrid(n)%PTR%xform, &
                       state_out, &
                       IntState%Regrid(n)%PTR%LocIn, &
                       IntState%Regrid(n)%PTR%LocOut, &
                       IntState%Regrid(n)%PTR%ntiles_in, &
                       IntState%Regrid(n)%PTR%ntiles_out,&
                       rc=status)
               end if
            else
               if (IntState%Regrid(n)%PTR%noxform) then
                  call RegridTransformT2G(STATE_IN=IntState%GIM(n), &
                       STATE_OUT=state_out, &
                       LS_OUT=IntState%Regrid(n)%PTR%LocOut, &
                       NTILES_OUT=IntState%Regrid(n)%PTR%ntiles_out, &
                       rc=status)
               else
                  call RegridTransformT2G(STATE_IN=IntState%GIM(n), &
                       XFORM=IntState%Regrid(n)%PTR%xform, &
                       STATE_OUT=state_out, &
                       LS_OUT=IntState%Regrid(n)%PTR%LocOut, &
                       NTILES_OUT=IntState%Regrid(n)%PTR%ntiles_out, &
                       rc=status)
               end if
            end if
            _VERIFY(STATUS)
         else
            state_out = INTSTATE%GIM(n)
         end if

         if (.not.list(n)%timeseries_output) then
            IOTYPE: if (list(n)%unit < 0) then    ! CFIO

               call list(n)%mNewCFIO%bundlepost(list(n)%currentFile,oClients=o_Clients,rc=status)
               _VERIFY(status)

            else

               if( INTSTATE%LCTL(n) ) then
                  call MAPL_GradsCtlWrite ( clock, state_out, list(n), &
                       filename(n), INTSTATE%expid, &
                       list(n)%descr, intstate%output_grids,rc )
                  INTSTATE%LCTL(n) = .false.
               endif

               do m=1,list(n)%field_set%nfields
                  call MAPL_VarWrite ( list(n)%unit, STATE=state_out, &
                       NAME=trim(list(n)%field_set%fields(3,m)), &
                       forceWriteNoRestart=.true., rc=status )
                  _VERIFY(STATUS)
               enddo
               call WRITE_PARALLEL("Wrote GrADS Output for File: "//trim(filename(n)))

            end if IOTYPE
         end if

      endif OUTTIME

   enddo POSTLOOP

   call o_Clients%done_collective_stage()
   call o_Clients%wait() 

   call MAPL_TimerOff(GENSTATE,"-----IO Post")
   call MAPL_TimerOff(GENSTATE,"----IO Write")

   call MAPL_TimerOn(GENSTATE,"----IO Write")
   call MAPL_TimerOn(GENSTATE,"-----IO Wait")

   WAITLOOP: do n=1,nlist

      if( Writing(n) .and. list(n)%unit < 0) then
         ! cleanup times
         if (allocated(list(n)%mNewCFIO%times)) deallocate(list(n)%mNewCFIO%times)
      end if

   enddo WAITLOOP

   call MAPL_TimerOff(GENSTATE,"-----IO Wait")
   call MAPL_TimerOff(GENSTATE,"----IO Write")

   call MAPL_TimerOn(GENSTATE,"----IO Write")
   call MAPL_TimerOn(GENSTATE,"-----IO Write")

   WRITELOOP: do n=1,nlist

      if (list(n)%timeseries_output) then
         call ESMF_ClockGet(clock,currTime=current_time,rc=status)
         _VERIFY(status)
         call list(n)%trajectory%append_file(current_time,rc=status)
         _VERIFY(status)
      end if
         
      if( Writing(n) .and. list(n)%unit < 0) then

         list(n)%unit = -2

      end if

   enddo WRITELOOP

   call MAPL_TimerOff(GENSTATE,"-----IO Write")
   call MAPL_TimerOff(GENSTATE,"----IO Write")

   call MAPL_TimerOff(GENSTATE,"--I/O"       )

   if(any(Writing)) call WRITE_PARALLEL("")

   deallocate(filename)
   deallocate(Writing)
   deallocate(Ignore)

   call MAPL_TimerOff(GENSTATE,"Run"         )
   call MAPL_TimerOff(GENSTATE,"TOTAL"       )

   _RETURN(ESMF_SUCCESS)
 end subroutine Run

!======================================================

  subroutine Finalize ( gc, import, export, clock, rc )

! !ARGUMENTS:

    type(ESMF_GridComp), intent(inout)    :: gc     ! composite gridded component 
    type(ESMF_State),       intent(inout) :: import ! import state
    type(ESMF_State),       intent(  out) :: export ! export state
    type(ESMF_Clock),       intent(inout) :: clock  ! the clock
  
    integer, intent(out), OPTIONAL        :: rc     ! Error code:
                                                     ! = 0 all is well
                                                     ! otherwise, error

    integer                         :: status
    type(HistoryCollection), pointer     :: list(:)
    type(HISTORY_wrap)              :: wrap
    type (HISTORY_STATE), pointer   :: IntState
    integer                         :: nlist, n
    type (MAPL_MetaComp), pointer :: GENSTATE
 

! Begin...

    call MAPL_GetObjectFromGC ( gc, GENSTATE, RC=STATUS)
    _VERIFY(STATUS)

    call MAPL_TimerOn(GENSTATE,"TOTAL")
    call MAPL_TimerOn(GENSTATE,"Finalize")

! Retrieve the pointer to the state

    call ESMF_GridCompGetInternalState(gc, wrap, status)
    _VERIFY(status)
    IntState => wrap%ptr
    list => IntState%list
    nlist = size(list)

! Close UNITs of GEOSgcm History Data
! -----------------------------------

   do n=1,nlist
      deallocate(list(n)%r4, list(n)%r8, list(n)%r8_to_r4)
      if (list(n)%disabled) cycle
      IF (list(n)%format == 'CFIO' .or. list(n)%format == 'CFIOasync') then
         if( MAPL_CFIOIsCreated(list(n)%mcfio) ) then
            CALL MAPL_CFIOdestroy (list(n)%mcfio, rc=STATUS)
            _VERIFY(STATUS)
         end if
      ELSE
         if( list(n)%unit.ne.0 ) call FREE_FILE( list(n)%unit )
      END if
      if(list(n)%monthly) then
         !ALT need some logic if alarm if not ringing
         if (.not. ESMF_AlarmIsRinging ( list(n)%his_alarm )) then
            if (.not. list(n)%partial) then
               call ESMF_CplCompWriteRestart (INTSTATE%CCS(n), &
                    importState=INTSTATE%CIM(n), &
                    exportState=INTSTATE%GIM(n), &
                    clock=CLOCK,           &
                    userRC=STATUS)
               _VERIFY(STATUS)
            end if
         end if
      end if
   enddo

#if 0
   do n=1,nlist
      IF (IntState%average(n)) then
         call MAPL_StateDestroy(IntState%gim(n), rc=status)
         _VERIFY(STATUS)
         call MAPL_StateDestroy(IntState%cim(n), rc=status)
         _VERIFY(STATUS)
      end IF
   enddo
#endif

   !call MAPL_Get(GENSTATE,maplcomm=mapl_comm,rc=status)
   !_VERIFY(STATUS)
   !if (mapl_comm%io%size > 0) then
      !if (mapl_am_i_root()) then
         !call MPI_Send(mapl_comm%global%rank,1,MPI_INTEGER,mapl_comm%io%root,MAPL_TAG_NORMALEXIT, &
                       !mapL_comm%mapl%comm,status)
         !_VERIFY(STATUS)
         !call MPI_Recv(n,1,MPI_INTEGER,mapl_comm%io%root,MAPL_TAG_WORKEREXIT, &
                       !mapl_comm%mapl%comm,MPI_STATUS_IGNORE,status)
      !end if
   !end if

    call MAPL_TimerOff(GENSTATE,"Finalize")
    call MAPL_TimerOff(GENSTATE,"TOTAL")

    call  MAPL_GenericFinalize ( GC, IMPORT, EXPORT, CLOCK, RC=status )
    _VERIFY(STATUS)


    _RETURN(ESMF_SUCCESS)
  end subroutine Finalize

!======================================================
 subroutine MAPL_GradsCtlWrite ( clock, state,list,fname,expid,expdsc,output_grids,rc )
   
   type(ESMF_Clock),  intent(inout) :: clock
   type(ESMF_State)                 :: state
   type(HistoryCollection)               :: list
   character(len=*)                 :: expid
   character(len=*)                 :: expdsc
   character(len=*)                 :: fname
   type(StringGridMap), intent(in)  :: output_grids
   integer, optional, intent(out)   :: rc
   
   type(ESMF_Array)               :: array
   type(ESMF_LocalArray)          :: larraylist(1)
   type(ESMF_Field)               :: field
   type(ESMF_Grid)                :: grid
   type(ESMF_Time)                :: CurrTime
   type(ESMF_Time)                :: StopTime
   type(ESMF_Calendar)            :: cal
   type(ESMF_TimeInterval)        :: ti, Frequency
   integer                        :: nsteps
   integer, dimension(ESMF_MAXDIM):: lbounds, ubounds
   integer, allocatable           :: vdim(:)
   character(len=ESMF_MAXSTR)     :: TimeString
   character(len=ESMF_MAXSTR)     :: filename
   character(len=ESMF_MAXSTR)     :: options
   integer                        :: DIMS(3)
   integer                        :: IM,JM,LM
   
   character*3                    :: months(12)
   data months /'JAN','FEB','MAR','APR','MAY','JUN', &
                'JUL','AUG','SEP','OCT','NOV','DEC'/
   
   integer      :: unit,nfield
   integer      :: k,m,rank,status
   integer      :: year,month,day,hour,minute
   real(kind=REAL64)   LONBEG,DLON
   real(kind=REAL64)   LATBEG,DLAT
   integer  mass, freq,zero
   real(kind=REAL32),      pointer :: LATS(:,:), LONS(:,:)
   character(len=ESMF_MAXSTR):: gridname
   type(ESMF_Grid), pointer :: pgrid
   
! Mass-Weighted Diagnostics
! -------------------------
   integer     km
   parameter ( km = 4 )
   character(len=ESMF_MAXSTR) :: name(2,km)
   data name / 'THIM'     , 'PHYSICS'    , & 
               'SIT'      , 'PHYSICS'    , & 
               'DTDT'     , 'PHYSICS'    , &
               'DTDT'     , 'GWD'        /

   call ESMF_ClockGet ( clock,  currTime=CurrTime ,rc=STATUS ) ; _VERIFY(STATUS)
   call ESMF_ClockGet ( clock,  StopTime=StopTime ,rc=STATUS ) ; _VERIFY(STATUS)
   call ESMF_ClockGet ( clock,  Calendar=cal      ,rc=STATUS ) ; _VERIFY(STATUS)
   
   call ESMF_TimeGet  ( CurrTime, timeString=TimeString, rc=status ) ; _VERIFY(STATUS)
   
   read(timestring( 1: 4),'(i4.4)') year
   read(timestring( 6: 7),'(i2.2)') month
   read(timestring( 9:10),'(i2.2)') day
   read(timestring(12:13),'(i2.2)') hour
   read(timestring(15:16),'(i2.2)') minute
   
   ti = StopTime-CurrTime
   freq = MAPL_nsecf( list%frequency )
   call ESMF_TimeIntervalSet( Frequency, S=freq, calendar=cal, rc=status ) ; _VERIFY(STATUS)
   
   nsteps =  ti/Frequency + 1
   
   if( trim(expid) == "" ) then
       filename =                       trim(list%collection)
   else
       filename = trim(expid) // '.' // trim(list%collection)
   endif
           unit = GETFILE( trim(filename) // '.ctl', form="formatted" )

   if( list%template == "" .or. list%duration == 0 ) then
       options  = 'options sequential'
       filename = trim(fname)
   else
       options  = 'options sequential template'
       filename = trim(filename) // '.' // trim(list%template)
   endif

! Get Global Horizontal Dimensions
! --------------------------------
   call ESMF_StateGet ( state,trim(list%field_set%fields(3,1)),field,rc=status )
   _VERIFY(STATUS)
   call ESMF_FieldGet ( field, grid=grid, rc=status )
   _VERIFY(STATUS)
   
   call MAPL_GridGet(GRID, globalCellCountPerDim=DIMS, RC=STATUS)
   _VERIFY(STATUS)

   ZERO   =  0
   IM     =  DIMS(1)
   JM     =  DIMS(2)
   LM     =  DIMS(3)
   if (LM == 0) LM = 1 ! needed for tilegrids

   call ESMF_GridGet(grid, name=gridname, rc=status)
   _VERIFY(STATUS)

   if (gridname(1:10) == 'tile_grid_') then
      DLON = 1.0
      DLAT = 1.0
      LATBEG = 0.0
      LONBEG = 0.0
   else
      if (IM /= 1) then
         DLON   =  360._REAL64/ IM
      else
         DLON = 1.0
      end if

      if (JM /= 1) then
         DLAT   =  180._REAL64/(JM-1)
      else
         DLAT   =  1.0
      end if

      call ESMFL_GridCoordGet(   GRID, LATS       , &
                                 Name     = "Latitude"              , &
                                 Location = ESMF_STAGGERLOC_CENTER  , &
                                 Units    = ESMFL_UnitsRadians      , &
                                 RC       = STATUS                    )
      _VERIFY(STATUS)

      call ESMFL_GridCoordGet(   GRID, LONS       , &
                                 Name     = "Longitude"             , &
                                 Location = ESMF_STAGGERLOC_CENTER  , &
                                 Units    = ESMFL_UnitsRadians      , &
                                 RC       = STATUS                    )
      _VERIFY(STATUS)

!ALT: Note: the LATS(1,1) and LONS(1,1) are correct ONLY on root
      if( MAPL_AM_I_ROOT() ) then
         LONBEG = LONS(1,1)*(180._REAL64/MAPL_PI_R8)
         if (size(LONS,1) > 1) then
            DLON = (LONS(2,1)-LONS(1,1))*(180._REAL64/MAPL_PI_R8)
         end if

         LATBEG = LATS(1,1)*(180._REAL64/MAPL_PI_R8)
         if (size(LATS,2) > 1) then
            DLAT = (LATS(1,2)-LATS(1,1))*(180._REAL64/MAPL_PI_R8)
         end if
      endif

!
! Check if changing resolution
! -------------------------------------------------------------------------
      block
         integer :: dims(3)
         pgrid => output_grids%at(trim(list%output_grid_label))
         if (associated(pgrid)) then
            call MAPL_GridGet(pgrid,globalCellCountPerDim=dims,RC=status)
            _VERIFY(status)
            IM = dims(1)
            JM = dims(2)
            DLON   =  360._REAL64/float(IM)
            if (JM /= 1) then
               DLAT   =  180._REAL64/float(JM-1)
            else
               DLAT   =  1.0
            end if
            LONBEG = -180._REAL64
            LATBEG =  -90._REAL64
         endif
      end block
   end if

! Compute Vertical Dimension for each Field (Augment nfield for VDIMS > LM)
! -------------------------------------------------------------------------
   allocate( vdim(list%field_set%nfields), stat=status )
   _VERIFY(STATUS)
   vdim = 0
   nfield =   list%field_set%nfields
   do m = 1,list%field_set%nfields
      call ESMFL_StateGetFieldArray( state,trim(list%field_set%fields(3,m)),array,status )
      _VERIFY(STATUS)
      call ESMF_ArrayGet( array, localarrayList=larrayList, rc=status )
      _VERIFY(STATUS)
      call ESMF_LocalArrayGet( larrayList(1), RANK=rank, totalLBound=lbounds, &
           totalUBound=ubounds, rc=status )
      _VERIFY(STATUS)
      if( rank==3 ) then
         vdim(m) = ubounds(3)-lbounds(3)+1
         if( vdim(m).gt.LM ) nfield = nfield+1
      else if( rank==4 ) then
         vdim(m) = -(ubounds(3)-lbounds(3)+1)*(ubounds(4)-lbounds(4)+1)
      endif
   enddo

! Create Grads Control File
! -------------------------
   if( MAPL_AM_I_ROOT() ) then
      print *
      if ( freq < 3600 ) then
         write(unit,201) trim(filename),trim(expdsc),trim(options), &
              MAPL_UNDEF,IM,LONBEG,DLON, JM,LATBEG,DLAT, LM,  &
              nsteps, &
              hour,minute,day,months(month),year,&
              freq/60, nfield
      else if ( freq < 86400 ) then
         write(unit,202) trim(filename),trim(expdsc),trim(options), &
              MAPL_UNDEF,IM,LONBEG,DLON, JM,LATBEG,DLAT, LM,  &
              nsteps, &
              hour,minute,day,months(month),year,&
              freq/3600, nfield
      else if ( freq < 30*86400 ) then
         write(unit,203) trim(filename),trim(expdsc),trim(options), &
              MAPL_UNDEF,IM,LONBEG,DLON, JM,LATBEG,DLAT, LM,  &
              nsteps, &
              hour,minute,day,months(month),year,&
              freq/86400, nfield
      else 
         write(unit,204) trim(filename),trim(expdsc),trim(options), &
              MAPL_UNDEF,IM,LONBEG,DLON, JM,LATBEG,DLAT, LM,  &
              nsteps, &
              hour,minute,day,months(month),year,&
              freq/(30*86400), nfield
      endif
      do m=1,list%field_set%nfields
         mass = 0
         do k=1,km
            if( trim(list%field_set%fields(1,m)).eq.trim(name(1,k))  .and. &
                 trim(list%field_set%fields(2,m)).eq.trim(name(2,k)) ) mass = 1  ! Check for Mass-Weighted Diagnostics
         enddo
         if( vdim(m).le.LM ) then
            write(unit,102) trim(list%field_set%fields(3,m)),abs(vdim(m)),mass,trim(list%field_set%fields(3,m))
         else
            write(unit,102) trim(list%field_set%fields(3,m)),LM     ,mass,trim(list%field_set%fields(3,m))
            if( trim(list%field_set%fields(1,m)).eq.'PLE' ) then
               write(unit,102) 'PS',zero,mass,'PS'
            else
               write(unit,102) trim(list%field_set%fields(3,m)) // 's',zero,mass,trim(list%field_set%fields(3,m)) // 's'
            endif
         endif
      enddo
      write(unit,103)
   endif
   call FREE_FILE( unit )
   deallocate( vdim )
   
201     format('dset ^',a,/, 'title ',a,/,a,/,             &
               'undef ',e15.6,/,                           &
	       'xdef ',i5,' linear ',f8.3,2x,f14.9,/,      &
	       'ydef ',i4,' linear ',f8.3,2x,f14.9,/,      &
	       'zdef ',i3,' linear  1  1',/,               &
	       'tdef ',i5,' linear  ',i2.2,':',i2.2,'z',i2.2,a3,i4.4,3x,i2.2,'mn',/, &
	       'vars  ',i3)
202     format('dset ^',a,/, 'title ',a,/,a,/,             &
               'undef ',e15.6,/,                           &
	       'xdef ',i5,' linear ',f8.3,2x,f14.9,/,      &
	       'ydef ',i4,' linear ',f8.3,2x,f14.9,/,      &
	       'zdef ',i3,' linear  1  1',/,               &
	       'tdef ',i5,' linear  ',i2.2,':',i2.2,'z',i2.2,a3,i4.4,3x,i2.2,'hr',/, &
	       'vars  ',i3)
203     format('dset ^',a,/, 'title ',a,/,a,/,             &
               'undef ',e15.6,/,                           &
	       'xdef ',i5,' linear ',f8.3,2x,f14.9,/,      &
	       'ydef ',i4,' linear ',f8.3,2x,f14.9,/,      &
	       'zdef ',i3,' linear  1  1',/,               &
	       'tdef ',i5,' linear  ',i2.2,':',i2.2,'z',i2.2,a3,i4.4,3x,i2.2,'dy',/, &
	       'vars  ',i3)
204     format('dset ^',a,/, 'title ',a,/,a,/,             &
               'undef ',e15.6,/,                           &
	       'xdef ',i5,' linear ',f8.3,2x,f14.9,/,      &
	       'ydef ',i4,' linear ',f8.3,2x,f14.9,/,      &
	       'zdef ',i3,' linear  1  1',/,               &
	       'tdef ',i5,' linear  ',i2.2,':',i2.2,'z',i2.2,a3,i4.4,3x,i2.2,'mo',/, &
	       'vars  ',i3)
102     format(a,i3,2x,i3,2x,"'",a,"'")
103     format('endvars')

   _RETURN(ESMF_SUCCESS)
 end subroutine MAPL_GradsCtlWrite


  subroutine get_DateStamp (clock, DateStamp, offset, rc)
    type (ESMF_Clock)                   :: clock
    character(len=ESMF_MAXSTR),optional :: DateStamp
    type(ESMF_TimeInterval),   optional :: offset
    integer, optional                   :: rc

    type(ESMF_Time)                   :: currentTime
    type(ESMF_Alarm)                  :: PERPETUAL
    character(len=ESMF_MAXSTR)        :: TimeString
    character(len=ESMF_MAXSTR)        :: clockname
    character                         :: String(ESMF_MAXSTR)
    logical                           :: LPERP
    integer                           :: YY,MM,DD,H,M,S
    integer                           :: noffset

    character*4 year
    character*2 month
    character*2 day
    character*2 hour
    character*2 minute
    character*2 second

    integer                    :: STATUS

    equivalence ( string(01),TimeString )
    equivalence ( string(01),year       )
    equivalence ( string(06),month      )
    equivalence ( string(09),day        )
    equivalence ( string(12),hour       )
    equivalence ( string(15),minute     )
    equivalence ( string(18),second     )

    call ESMF_ClockGet ( clock, name=clockname, currTime=currentTime, rc=status)
    _VERIFY(STATUS)

    if (present(offset)) then
        call ESMF_TimeIntervalGet( OFFSET, S=noffset, rc=status )
        _VERIFY(STATUS)
        if( noffset /= 0 ) then
            LPERP = ( index( trim(clockname),'_PERPETUAL' ).ne.0 )
        if( LPERP ) then
            call ESMF_ClockGetAlarm ( clock, AlarmName='PERPETUAL', alarm=PERPETUAL, rc=status )
            _VERIFY(STATUS)
            if( ESMF_AlarmIsRinging(PERPETUAL) ) then
!
! Month has already been set back to PERPETUAL Month, therefore
! Time-Averaged Files (i.e., non-zero offset) need Month to be advanced for proper offset calculation
! ---------------------------------------------------------------------------------------------------
                call ESMF_TimeGet ( CurrentTime, YY = YY, &
                                                 MM = MM, &
                                                 DD = DD, &
                                                 H  = H , &
                                                 M  = M , &
                                                 S  = S, rc=status )
                                                 MM = MM + 1
                call ESMF_TimeSet ( CurrentTime, YY = YY, &
                                                 MM = MM, &
                                                 DD = DD, &
                                                 H  = H , &
                                                 M  = M , &
                                                 S  = S, rc=status )
#ifdef DEBUG
      if( MAPL_AM_I_ROOT() ) write(6,"(a,2x,i4.4,'/',i2.2,'/',i2.2,2x,'Time: ',i2.2,':',i2.2,':',i2.2)") "Inside HIST GetDate: ",YY,MM,DD,H,M,S
#endif
            endif
        endif
        endif
        currentTime = currentTime - offset
    end if

    call ESMF_TimeGet (currentTime, timeString=TimeString, rc=status)
    _VERIFY(STATUS)

    if(present(DateStamp)) then
       DateStamp = year//month//day//'_'//hour//minute//second //'z'
    end if
    
    _RETURN(ESMF_SUCCESS)
  end subroutine get_DateStamp

  subroutine RegridTransform(STATE_IN, XFORM, STATE_OUT, LS_IN, LS_OUT, NTILES_IN, NTILES_OUT, RC)
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

    allocate(tile_in (ntiles_in ), stat=status)
    _VERIFY(STATUS)
    allocate(tile_out(ntiles_out), stat=status)
    _VERIFY(STATUS)


    call ESMF_StateGet(STATE_IN,  ITEMCOUNT=ITEMCOUNT_IN,  RC=STATUS)
    _VERIFY(STATUS)
    call ESMF_StateGet(STATE_OUT, ITEMCOUNT=ITEMCOUNT_OUT, RC=STATUS)
    _VERIFY(STATUS)

    _ASSERT(ITEMCOUNT_IN == ITEMCOUNT_OUT,'needs informative message')

    ITEMCOUNT = ITEMCOUNT_IN
    _ASSERT(ITEMCOUNT>0,'needs informative message')

    allocate(ITEMNAMES_IN(ITEMCOUNT),STAT=STATUS)
    _VERIFY(STATUS)
    allocate(ITEMTYPES_IN(ITEMCOUNT),STAT=STATUS)
    _VERIFY(STATUS)

    call ESMF_StateGet(STATE_IN, ITEMNAMELIST=ITEMNAMES_IN, &
                       ITEMTYPELIST=ITEMTYPES_IN, RC=STATUS)
    _VERIFY(STATUS)

    allocate(ITEMNAMES_OUT(ITEMCOUNT),STAT=STATUS)
    _VERIFY(STATUS)
    allocate(ITEMTYPES_OUT(ITEMCOUNT),STAT=STATUS)
    _VERIFY(STATUS)

    call ESMF_StateGet(STATE_OUT, ITEMNAMELIST=ITEMNAMES_OUT, &
                       ITEMTYPELIST=ITEMTYPES_OUT, RC=STATUS)
    _VERIFY(STATUS)

    DO I=1, ITEMCOUNT
       _ASSERT(ITEMTYPES_IN (I) == ESMF_StateItem_Field,'needs informative message')
       _ASSERT(ITEMTYPES_OUT(I) == ESMF_StateItem_Field,'needs informative message')

       call ESMF_StateGet(STATE_IN , ITEMNAMES_IN (i), field, rc=status)
       _VERIFY(STATUS)
       call ESMF_FieldGet(field, Array=array_in , rc=status)
       _VERIFY(STATUS)
       call ESMF_StateGet(STATE_OUT, ITEMNAMES_OUT(i), field, rc=status)
       _VERIFY(STATUS)
       call ESMF_FieldGet(field, Array=array_out, rc=status)
       _VERIFY(STATUS)

       call ESMF_ArrayGet(array_in , rank=rank_in , rc=status)
       _VERIFY(STATUS)
       call ESMF_ArrayGet(array_out, rank=rank_out, rc=status)
       _VERIFY(STATUS)
       _ASSERT(rank_in == rank_out,'needs informative message')
       _ASSERT(rank_in >=2 .and. rank_in <= 3,'needs informative message')

       if (rank_in == 2) then
          LM = 1
          LL = 1
          LU = 1
          call ESMF_ArrayGet(array_in , localDE=0, farrayptr=ptr2d_in , rc=status)
          _VERIFY(STATUS)
          call ESMF_ArrayGet(array_out, localDE=0, farrayptr=ptr2d_out, rc=status)
          _VERIFY(STATUS)
       else
          call ESMF_ArrayGet(array_in , localDE=0, farrayptr=ptr3d_in , rc=status)
          _VERIFY(STATUS)
          call ESMF_ArrayGet(array_out, localDE=0, farrayptr=ptr3d_out, rc=status)
          _VERIFY(STATUS)
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

          call MAPL_LocStreamTransform(LS_IN, TILE_IN, PTR2d_IN, RC=STATUS)
          _VERIFY(STATUS)

          call MAPL_LocStreamTransform( tile_out, XFORM, tile_in, RC=STATUS ) 
          _VERIFY(STATUS)

          call MAPL_LocStreamTransform(LS_OUT, PTR2d_OUT, TILE_OUT, RC=STATUS)
          _VERIFY(STATUS)

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

  subroutine RegridTransformT2G2G(STATE_IN, XFORM, XFORMntv, STATE_OUT, LS_IN, LS_OUT, LS_NTV, NTILES_IN, NTILES_OUT, RC)
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

    allocate(tt_in (ntiles_in ), stat=status)
    _VERIFY(STATUS)
    allocate(tile_out(ntiles_out), stat=status)
    _VERIFY(STATUS)


    call ESMF_StateGet(STATE_IN,  ITEMCOUNT=ITEMCOUNT_IN,  RC=STATUS)
    _VERIFY(STATUS)
    call ESMF_StateGet(STATE_OUT, ITEMCOUNT=ITEMCOUNT_OUT, RC=STATUS)
    _VERIFY(STATUS)

    _ASSERT(ITEMCOUNT_IN == ITEMCOUNT_OUT,'needs informative message')

    ITEMCOUNT = ITEMCOUNT_IN
    _ASSERT(ITEMCOUNT>0,'needs informative message')

    allocate(ITEMNAMES_IN(ITEMCOUNT),STAT=STATUS)
    _VERIFY(STATUS)
    allocate(ITEMTYPES_IN(ITEMCOUNT),STAT=STATUS)
    _VERIFY(STATUS)

    call ESMF_StateGet(STATE_IN, ITEMNAMELIST=ITEMNAMES_IN, &
                       ITEMTYPELIST=ITEMTYPES_IN, RC=STATUS)
    _VERIFY(STATUS)

    allocate(ITEMNAMES_OUT(ITEMCOUNT),STAT=STATUS)
    _VERIFY(STATUS)
    allocate(ITEMTYPES_OUT(ITEMCOUNT),STAT=STATUS)
    _VERIFY(STATUS)

    call ESMF_StateGet(STATE_OUT, ITEMNAMELIST=ITEMNAMES_OUT, &
                       ITEMTYPELIST=ITEMTYPES_OUT, RC=STATUS)
    _VERIFY(STATUS)

    call MAPL_LocStreamGet(LS_NTV, ATTACHEDGRID=GRID, RC=STATUS)
    _VERIFY(STATUS)
    call MAPL_GridGet(grid, localCellCountPerDim=COUNTS, RC=STATUS)
    _VERIFY(STATUS)
    allocate(G2d_in(COUNTS(1),COUNTS(2)), stat=status)
    _VERIFY(STATUS)

    call MAPL_LocStreamGet(LS_ntv, NT_LOCAL = sizett, rc=status)
    _VERIFY(STATUS)
    allocate(tt(sizett), stat=status)
    _VERIFY(STATUS)

    DO I=1, ITEMCOUNT
       _ASSERT(ITEMTYPES_IN (I) == ESMF_StateItem_Field,'needs informative message')
       _ASSERT(ITEMTYPES_OUT(I) == ESMF_StateItem_Field,'needs informative message')

       call ESMF_StateGet(STATE_IN , ITEMNAMES_IN (i), field, rc=status)
       _VERIFY(STATUS)
       call ESMF_FieldGet(field, Array=array_in , rc=status)
       _VERIFY(STATUS)
       call ESMF_StateGet(STATE_OUT, ITEMNAMES_OUT(i), field, rc=status)
       _VERIFY(STATUS)
       call ESMF_FieldGet(field, Array=array_out, rc=status)
       _VERIFY(STATUS)

       call ESMF_ArrayGet(array_in , rank=rank_in , typekind=tk, rc=status)
       _VERIFY(STATUS)
       call ESMF_ArrayGet(array_out, rank=rank_out, rc=status)
       _VERIFY(STATUS)

       _ASSERT(rank_in+1 == rank_out,'needs informative message')
       _ASSERT(rank_in >=1 .and. rank_in <= 3,'needs informative message')

       KM = 1
       if (rank_in == 1) then
          if (tk == ESMF_TypeKind_R4) then
             call ESMF_ArrayGet(array_in , localDE=0, farrayptr=ptr1d_in , rc=status)
             _VERIFY(STATUS)
             tile_in => ptr1d_in
          else if (tk == ESMF_TypeKind_R8) then
             call ESMF_ArrayGet(array_in , localDE=0, farrayptr=p1dr8_in , rc=status)
             _VERIFY(STATUS)
             if (.not. associated(tile1d)) then
                allocate(tile1d(size(p1dr8_in)), stat=status)
                _VERIFY(STATUS)
             end if
             tile1d = p1dr8_in
             tile_in => tile1d
          end if

          call ESMF_ArrayGet(array_out, localDE=0, farrayptr=ptr2d_out, rc=status)
          _VERIFY(STATUS)
          out2d   => ptr2d_out
          LM = 1
       else if (rank_in == 2) then
          if (tk == ESMF_TypeKind_R4) then
             call ESMF_ArrayGet(array_in , localDE=0, farrayptr=ptr2d_in , rc=status)
             _VERIFY(STATUS)
          else if (tk == ESMF_TypeKind_R8) then
             call ESMF_ArrayGet(array_in , localDE=0, farrayptr=p2dr8_in , rc=status)
             _VERIFY(STATUS)
             if (.not. associated(tile1d)) then
                allocate(tile1d(size(p2dr8_in,1)), stat=status)
                _VERIFY(STATUS)
             end if
          end if

          call ESMF_ArrayGet(array_out, localDE=0, farrayptr=ptr3d_out, rc=status)
          _VERIFY(STATUS)
          LM = size(ptr3d_out,3)
       else if (rank_in == 3) then
          if (tk == ESMF_TypeKind_R4) then
             call ESMF_ArrayGet(array_in , localDE=0, farrayptr=ptr3d_in , rc=status)
             _VERIFY(STATUS)
          else if (tk == ESMF_TypeKind_R8) then
             call ESMF_ArrayGet(array_in , localDE=0, farrayptr=p3dr8_in , rc=status)
             _VERIFY(STATUS)
             if (.not. associated(tile1d)) then
                allocate(tile1d(size(p3dr8_in,1)), stat=status)
                _VERIFY(STATUS)
             end if
          end if

          call ESMF_ArrayGet(array_out, localDE=0, farrayptr=ptr4d_out, rc=status)
          _VERIFY(STATUS)
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
             call MAPL_LocStreamTransform( tt, XFORMntv, tile_in, RC=STATUS ) 
             _VERIFY(STATUS)
             ! T2G
             call MAPL_LocStreamTransform(LS_NTV, G2d_IN, tt, RC=STATUS)
             _VERIFY(STATUS)

             ! G2T
             call MAPL_LocStreamTransform(LS_IN, TT_IN, G2d_IN, RC=STATUS)
             _VERIFY(STATUS)
             ! T2T
             call MAPL_LocStreamTransform( tile_out, XFORM, tt_in, RC=STATUS ) 
             _VERIFY(STATUS)
             ! T2G
             call MAPL_LocStreamTransform(LS_OUT, PTR2d_OUT, TILE_OUT, RC=STATUS)
             _VERIFY(STATUS)

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

  subroutine RegridTransformT2G(STATE_IN, XFORM, STATE_OUT, LS_OUT, NTILES_OUT, RC)
    type (ESMF_State)        , intent(IN   ) :: STATE_IN
    type (ESMF_State)        , intent(INOUT) :: STATE_OUT
    type(MAPL_LocStreamXform), optional, intent(IN   ) :: XFORM
    type(MAPL_LocStream)     , intent(IN   ) :: LS_OUT
    integer                  , intent(IN   ) :: NTILES_OUT
    integer, optional        , intent(  OUT) :: RC

    integer                    :: STATUS

    integer                         :: I, L, K, LM, KM
    integer                         :: rank_in
    integer                         :: rank_out
    integer                         :: itemcount, itemcount_in, itemcount_out
    real, pointer                   :: tile_in(:), tile_out(:)
    real, pointer                   :: ptr1d_in(:)
    real, pointer                   :: ptr2d_in(:,:)
    real, pointer                   :: ptr3d_in(:,:,:)
    real(kind=REAL64), pointer                 :: p1dr8_in(:)
    real(kind=REAL64), pointer                 :: p2dr8_in(:,:)
    real(kind=REAL64), pointer                 :: p3dr8_in(:,:,:)
    real, pointer                   :: ptr2d_out(:,:)
    real, pointer                   :: ptr3d_out(:,:,:)
    real, pointer                   :: ptr4d_out(:,:,:,:)
    real, pointer                   :: out2d(:,:)
    real, pointer                   :: tile1d(:) => null()
    type(ESMF_Array)                :: array_in
    type(ESMF_Array)                :: array_out
    type(ESMF_Field)                :: field
    type (ESMF_TypeKind_Flag)       :: tk
    type (ESMF_StateItem_Flag),  pointer :: ITEMTYPES_IN(:), ITEMTYPES_OUT(:)
    character(len=ESMF_MAXSTR ), pointer :: ITEMNAMES_IN(:), ITEMNAMES_OUT(:)

    if (present(XFORM)) then
       allocate(tile_out(ntiles_out), stat=status)
       _VERIFY(STATUS)
    end if

    call ESMF_StateGet(STATE_IN,  ITEMCOUNT=ITEMCOUNT_IN,  RC=STATUS)
    _VERIFY(STATUS)
    call ESMF_StateGet(STATE_OUT, ITEMCOUNT=ITEMCOUNT_OUT, RC=STATUS)
    _VERIFY(STATUS)

    _ASSERT(ITEMCOUNT_IN == ITEMCOUNT_OUT,'needs informative message')

    ITEMCOUNT = ITEMCOUNT_IN
    _ASSERT(ITEMCOUNT>0,'needs informative message')

    allocate(ITEMNAMES_IN(ITEMCOUNT),STAT=STATUS)
    _VERIFY(STATUS)
    allocate(ITEMTYPES_IN(ITEMCOUNT),STAT=STATUS)
    _VERIFY(STATUS)

    call ESMF_StateGet(STATE_IN, ITEMNAMELIST=ITEMNAMES_IN, &
                       ITEMTYPELIST=ITEMTYPES_IN, RC=STATUS)
    _VERIFY(STATUS)

    allocate(ITEMNAMES_OUT(ITEMCOUNT),STAT=STATUS)
    _VERIFY(STATUS)
    allocate(ITEMTYPES_OUT(ITEMCOUNT),STAT=STATUS)
    _VERIFY(STATUS)

    call ESMF_StateGet(STATE_OUT, ITEMNAMELIST=ITEMNAMES_OUT, &
                       ITEMTYPELIST=ITEMTYPES_OUT, RC=STATUS)
    _VERIFY(STATUS)

    DO I=1, ITEMCOUNT
       _ASSERT(ITEMTYPES_IN (I) == ESMF_StateItem_Field,'needs informative message')
       _ASSERT(ITEMTYPES_OUT(I) == ESMF_StateItem_Field,'needs informative message')

       call ESMF_StateGet(STATE_IN , ITEMNAMES_IN (i), field, rc=status)
       _VERIFY(STATUS)
       call ESMF_FieldGet(field, Array=array_in , rc=status)
       _VERIFY(STATUS)
       call ESMF_StateGet(STATE_OUT, ITEMNAMES_OUT(i), field, rc=status)
       _VERIFY(STATUS)
       call ESMF_FieldGet(field, Array=array_out, rc=status)
       _VERIFY(STATUS)

       call ESMF_ArrayGet(array_in , rank=rank_in , typekind=tk, rc=status)
       _VERIFY(STATUS)
       call ESMF_ArrayGet(array_out, rank=rank_out, rc=status)
       _VERIFY(STATUS)
       _ASSERT(rank_out == rank_in + 1,'needs informative message')

       KM = 1
       if (rank_in == 1) then
          if (tk == ESMF_TypeKind_R4) then
             call ESMF_ArrayGet(array_in , localDE=0, farrayptr=ptr1d_in , rc=status)
             _VERIFY(STATUS)
             tile_in => ptr1d_in
          else if (tk == ESMF_TypeKind_R8) then
             call ESMF_ArrayGet(array_in , localDE=0, farrayptr=p1dr8_in , rc=status)
             _VERIFY(STATUS)
             if (.not. associated(tile1d)) then
                allocate(tile1d(size(p1dr8_in)), stat=status)
                _VERIFY(STATUS)
             end if
             tile1d = p1dr8_in
             tile_in => tile1d
          end if

          call ESMF_ArrayGet(array_out, localDE=0, farrayptr=ptr2d_out, rc=status)
          _VERIFY(STATUS)
          out2d   => ptr2d_out
          LM = 1
       else if (rank_in == 2) then
          if (tk == ESMF_TypeKind_R4) then
             call ESMF_ArrayGet(array_in , localDE=0, farrayptr=ptr2d_in , rc=status)
             _VERIFY(STATUS)
          else if (tk == ESMF_TypeKind_R8) then
             call ESMF_ArrayGet(array_in , localDE=0, farrayptr=p2dr8_in , rc=status)
             _VERIFY(STATUS)
             if (.not. associated(tile1d)) then
                allocate(tile1d(size(p2dr8_in,1)), stat=status)
                _VERIFY(STATUS)
             end if
          end if

          call ESMF_ArrayGet(array_out, localDE=0, farrayptr=ptr3d_out, rc=status)
          _VERIFY(STATUS)
          LM = size(ptr3d_out,3)
       else if (rank_in == 3) then
          if (tk == ESMF_TypeKind_R4) then
             call ESMF_ArrayGet(array_in , localDE=0, farrayptr=ptr3d_in , rc=status)
             _VERIFY(STATUS)
          else if (tk == ESMF_TypeKind_R8) then
             call ESMF_ArrayGet(array_in , localDE=0, farrayptr=p3dr8_in , rc=status)
             _VERIFY(STATUS)
             if (.not. associated(tile1d)) then
                allocate(tile1d(size(p3dr8_in,1)), stat=status)
                _VERIFY(STATUS)
             end if
          end if

          call ESMF_ArrayGet(array_out, localDE=0, farrayptr=ptr4d_out, rc=status)
          _VERIFY(STATUS)
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

             if (present(XFORM)) then
                call MAPL_LocStreamTransform( tile_out, XFORM, tile_in, RC=STATUS ) 
                _VERIFY(STATUS)
             else
                tile_out => tile_in
             endif

             call MAPL_LocStreamTransform(LS_OUT, OUT2d, TILE_OUT, RC=STATUS)
             _VERIFY(STATUS)

          END DO
       END DO

    ENDDO

    deallocate(itemtypes_out)
    deallocate(itemnames_out)
    deallocate(itemtypes_in)
    deallocate(itemnames_in)
    if (present(XFORM)) then
       deallocate(tile_out)
    end if
    if (associated(tile1d)) deallocate(tile1d)

    _RETURN(ESMF_SUCCESS)
  end subroutine RegridTransformT2G

  subroutine Get_Tdim (list, clock, tdim)

! !IROUTINE: Get_Tdim -- Returns Time Dimension (Number of Records) in a HISTORY.rc collection file

! !USES:
    use ESMF
    use MAPL_CommsMod, only: MAPL_AM_I_ROOT

    implicit none

! !ARGUMENTS:

    type (HistoryCollection),  intent(IN ) :: list
    type (ESMF_Clock),    intent(IN ) :: clock
    integer,              intent(OUT) :: tdim

! ESMF stuff
!-----------
    type (ESMF_Time)            :: currTime
    type (ESMF_Time)            :: stopTime
    type (ESMF_TimeInterval)    :: tint

! Misc locals
!------------
    real                         :: rfreq
    real                         :: rdelt
    real                         :: rfrac
    integer                      :: nfreq
    integer                      :: ndelt
    integer                      :: STATUS

!  Initialize TDIM=-1 (UNLIMITED)
!--------------------------------
    tdim = -1

    if( list%tm == 0) then  ! Dynamic calculation of time dimension

       if( list%duration == 0 ) then
          ! compute duration from the ESMF clock
          call ESMF_ClockGet(clock, currTime=currTime, stopTime=stopTime, &
               RC=status)
          if (status /= ESMF_SUCCESS) goto 200
          tint = stopTime - currTime
          call ESMF_TimeIntervalGet(tint, s=ndelt, RC=status)
          if (status /= ESMF_SUCCESS) goto 200

          nfreq = MAPL_nsecf( list%frequency )
          rfreq = real(nfreq)
          rdelt = real(ndelt)
          rfrac = rdelt/rfreq - ndelt/nfreq
          if( rfrac.ne.0 ) rfrac = 1.0 - rfrac
          ndelt = ndelt  + rfrac*nfreq

       else
          ndelt = MAPL_nsecf( list%duration )
       endif

       nfreq = MAPL_nsecf( list%frequency )
       if (nfreq /=0) then
          tdim  = ndelt/nfreq
       end if

    else
       tdim = list%tm
    endif  ! End TM=0 Test

! Debug Prints
! ------------
200 continue
    if( MAPL_AM_I_ROOT() ) then
       write(6,100) list%frequency, list%duration, tdim, trim(list%collection)
100    format(1x,'Freq: ',i8.8,'  Dur: ',i8.8,'  TM: ',i4,'  Collection: ',a)
    endif

    return
  end subroutine Get_Tdim

  subroutine MAPL_SetExpression(nfield,fields,tmpfields,rewrite,nPExtraFields, &
           ExtraFields,ExtraGridComp,ExpState,rc)

  integer,intent(in)::nfield
  character*(*),     intent(inout) :: fields(:,:)
  character*(*),     intent(inout) :: tmpfields(:)
  logical,           intent(inout) :: rewrite(:)
  integer,           intent(inout) :: nPExtraFields
  character*(*), pointer, intent(inout) :: ExtraFields(:)
  character*(*), pointer, intent(inout) :: ExtraGridComp(:)
  type(ESMF_State),  intent(inout) :: ExpState
  integer, optional, intent(out  ) :: rc

! Local variables:

  integer:: i,j,m,k,status,largest_rank,iRepField,ivLoc
  logical :: ifound_vloc
  character(len=ESMF_MAXSTR) :: tmpList
  character(len=ESMF_MAXSTR) :: VarName
  integer                    :: idx
  character(len=ESMF_MAXSTR), allocatable :: VarNames(:)
  logical,                    allocatable :: VarNeeded(:)
  integer                                 :: iRealFields
  character(len=256)                      :: ExtVars
  integer                                 :: nExtraFields,nUniqueExtraFields
  character(len=ESMF_MAXSTR), allocatable :: NonUniqueVarNames(:,:)

  character(len=ESMF_MAXSTR), allocatable :: TotVarNames(:)
  character(len=ESMF_MAXSTR), allocatable :: TotCmpNames(:)
  character(len=ESMF_MAXSTR), allocatable :: TotAliasNames(:)
  integer,                    allocatable :: totRank(:)
  integer,                    allocatable :: totLoc(:)
  integer                                 :: totFields
  type(ESMF_State), pointer               :: exptmp (:)
  type(ESMF_State)                        :: state
  type(ESMF_Field)                        :: field
  integer                                 :: dims
  logical, allocatable                    :: isBundle(:)

! Set rewrite flag and tmpfields.
! To keep consistency, all the arithmetic parsing output fields must
! only be combinations of the alias output field variables (i.e., fields(3,:))
! rather than the actual output field variables (i.e., fields(1,:)).
! Also do check that there are no illegal operations
!-------------------------------------------------------------------
  ! check which fields are actual exports or expressions
  nPExtraFields = 0
  iRealFields = 0
  allocate(isBundle(nfield))
  do m=1,nfield
    if (scan(trim(fields(1,m)),'()^*/+-.')/=0) then
       rewrite(m)= .TRUE.
       tmpfields(m)= trim(fields(1,m))
    else
       if (index(fields(1,m),'%') == 0) then
          iRealFields = iRealFields + 1
          rewrite(m)= .FALSE.
          isBundle(m) = .FALSE.
          tmpfields(m)= trim(fields(1,m))
       else
          isBundle(m)=.true.
          rewrite(m)= .FALSE.
          tmpfields(m)= trim(fields(1,m))
       endif
    endif
  enddo

  ! now that we know this allocated a place to store the names of the real fields
  allocate(VarNames(iRealFields),stat=status)
  _VERIFY(STATUS)
  allocate(VarNeeded(iRealFields),stat=status)
  _VERIFY(STATUS)
  k=0
  do m=1,nfield
     if ( (rewrite(m) .eqv. .False.) .and. (isBundle(m) .eqv. .False.) ) then
        k=k+1
        VarNames(k)=fields(3,m)
     endif
  enddo

  ! now we can have extra fields that are not in collection if they are in the component
  ! we specify with the expression we get the number of these

  nExtraFields=0
  do m=1,nfield
     if (rewrite(m)) then

         ExtVars = ""
         call CheckSyntax(tmpfields(m),VarNames,VarNeeded,ExtVar=ExtVars,rc=status)
         _VERIFY(STATUS)

         tmpList=ExtVars
         do i=1,len_trim(tmpList)
            idx=index(tmpList,',')
            if (idx /= 0) then
               varName = tmpList(1:idx-1)
               nExtraFields=nExtraFields+1
               tmpList = tmpList(idx+1:)
            else
               exit
            end if
         end do

      end if
   end do

  allocate(NonUniqueVarNames(nExtraFields,2)) 

  ! get the number of extra fields, after this we will have to check for duplicates
  nExtraFields=0
  do m=1,nfield
     if (rewrite(m)) then

         ExtVars = ""
         call CheckSyntax(tmpfields(m),VarNames,VarNeeded,ExtVar=ExtVars,rc=status)
         _VERIFY(STATUS)

         tmpList=ExtVars
         do i=1,len_trim(tmpList)
            idx=index(tmpList,',')
            if (idx /= 0) then
               varName = tmpList(1:idx-1)
               nExtraFields=nExtraFields+1
               NonUniqueVarNames(nExtraFields,1) = trim(VarName)
               NonUniqueVarNames(nExtraFields,2) = fields(2,m)
               tmpList = tmpList(idx+1:)
            else
               exit
            end if
         end do

      end if
   end do


   deallocate(VarNames)
   deallocate(VarNeeded)

   ! blank out any duplicates
   do i=1,nExtraFields
      VarName = NonUniqueVarNames(i,1)
      do j=i+1,nExtraFields
         if (trim(VarName) == trim(NonUniqueVarNames(j,1))) then
            NonUniqueVarNames(j,1)="DUPLICATE"
         end if
      end do
   end do

   nUniqueExtraFields = 0
   do i=1,nExtraFields
      if (trim(NonUniqueVarNames(i,1)) /= "DUPLICATE") nUniqueExtraFields = nUniqueExtraFields + 1
   end do

  totFields = iRealFields + nUniqueExtraFields
  allocate(TotVarNames(totFields),stat=status)
  _VERIFY(STATUS)
  allocate(TotCmpNames(totFields),stat=status)
  _VERIFY(STATUS)
  allocate(TotAliasNames(totFields),stat=status)
  _VERIFY(STATUS)
  allocate(TotRank(totFields),stat=status)
  _VERIFY(STATUS)
  allocate(TotLoc(totFields),stat=status)
  _VERIFY(STATUS)

  allocate ( exptmp (1), stat=status )
  _VERIFY(STATUS)
  exptmp(1) = ExpState
  iRealFields = 0
  do i=1,nfield
    if ( (.not.rewrite(i)) .and. (.not.isBundle(i)) ) then
       iRealFields = iRealFields + 1
       TotVarNames(iRealFields) = trim(fields(1,i))
       TotCmpNames(iRealFields) = trim(fields(2,i))
       TotAliasNames(iRealFields) = trim(fields(3,i))

       call MAPL_ExportStateGet(exptmp,fields(2,i),state,rc=status)
       _VERIFY(STATUS)
       call MAPL_StateGet(state,fields(1,i),field,rc=status)
       _VERIFY(STATUS)
       call ESMF_AttributeGet(field,name='DIMS',value=dims,rc=status)
       _VERIFY(STATUS)
       TotRank(iRealFields) = dims
       call ESMF_AttributeGet(field,name='VLOCATION',value=dims,rc=status)
       _VERIFY(STATUS)
       TotLoc(iRealFields) = dims
           
    endif
  enddo
  nUniqueExtraFields = 0
  do i=1, nExtraFields
     if (trim(NonUniqueVarNames(i,1)) /= "DUPLICATE") then
        nUniqueExtraFields = nUniqueExtraFields + 1
        TotVarNames(iRealFields+nUniqueExtraFields) = NonUniqueVarNames(i,1)
        TotCmpNames(iRealFields+nUniqueExtraFields) = NonUniqueVarNames(i,2)
        TotAliasNames(iRealFields+nUniqueExtraFields) = NonUniqueVarNames(i,1)
        call MAPL_ExportStateGet ( exptmp,NonUniqueVarNames(i,2),state,rc=status )
        _VERIFY(STATUS)
        call MAPL_StateGet(state, NonUniqueVarNames(i,1),field,rc=status)
        _VERIFY(STATUS)

        call ESMF_AttributeGet(field,name='DIMS',value=dims,rc=status)
        _VERIFY(STATUS)
        TotRank(iRealFields+nUniqueExtraFields) = dims
        call ESMF_AttributeGet(field,name='VLOCATION',value=dims,rc=status)
        _VERIFY(STATUS)
        TotLoc(iRealFields+nUniqueExtraFields) = dims
     end if
  end do 

  allocate(extraFields(nUniqueExtraFields),stat=status)
  _VERIFY(STATUS)
  allocate(extraGridComp(nUniqueExtraFields),stat=status)
  _VERIFY(STATUS)
  nPExtraFields = nUniqueExtraFields
  nUniqueExtraFields = 0
  do i=1,nExtraFields
     if (trim(NonUniqueVarNames(i,1)) /= "DUPLICATE") then
        nUniqueExtraFields = nUniqueExtraFields + 1
        extraFields(nUniqueExtraFields) = NonUniqueVarNames(i,1)
        extraGridComp(nUniqueExtraFields) = NonUniqueVarNames(i,2)
     end if
  end do

  deallocate(NonUniqueVarNames)
  deallocate(exptmp) 
! Change the arithmetic parsing field containing mutiple variables
! to the dummy default field containing a single field variable.
! Since MAPL_HistoryGridCompMod does not understand arithmetic parsing field variable,
! we need to change the arithmetic parsing field variable to the dummy field to allocate memory.
! But the actual arithmetic parsing field already has been copied to the temporialy field.
! Also we will do some syntax checking here since this is a good place
!----------------------------------------------------------------------
 allocate(VarNeeded(TotFields),stat=status)
 _VERIFY(STATUS)

 do m=1,nfield
     if (Rewrite(m) .eqv. .TRUE.) then
         largest_rank =0
         ifound_vloc=.false.
         call CheckSyntax(tmpfields(m),TotAliasNames,VarNeeded,rc=status)
         _VERIFY(STATUS)
         do i=1,TotFields
            if (VarNeeded(i)) then
               if (TotRank(i)> largest_rank) then
                  largest_rank=TotRank(i)
                  iRepField=i
               end if

               if (ifound_vloc) then
                  if (ivLoc /= Totloc(i) .and. totloc(i) /= MAPL_VLocationNone) then
                     if (mapl_am_I_root()) write(*,*)'arithmetic expression has two different vlocations'
                     _ASSERT(.false.,'needs informative message')
                  end if
               else
                  if (totloc(i) /= MAPL_VLocationNone) then
                     ivloc = totloc(i)
                     ifound_vloc = .true.
                  endif
               end if
            end if
         end do
         fields(1,m)= TotVarNames(iRepField)
         fields(2,m)= TotCmpNames(iRepField)

     endif
 enddo

 deallocate(VarNeeded)
 deallocate(TotVarNames)
 deallocate(TotCmpNames)
 deallocate(TotAliasNames)
 deallocate(TotRank)
 deallocate(TotLoc)
 deallocate(isBundle)

 _RETURN(ESMF_SUCCESS)

 end subroutine MAPL_SetExpression

  subroutine MAPL_RunExpression(state,fields,tmpfields,rewrite,nfield,rc)

  type (ESMF_State),  intent(in)    :: state
  character*(*), intent(in):: fields(:,:),tmpfields(:)
  logical, intent(inout) :: rewrite(:)
  integer, intent(in):: nfield
  integer, optional, intent(out) :: rc

! Local variables:
  character(len=ESMF_MAXSTR)     :: fname,fexpr
  integer:: m,STATUS
  type(ESMF_Field) :: field

  do m=1,nfield
     if (rewrite(m)) then
        fname = trim(fields(3,m))
        call MAPL_StateGet(state,fname,field,rc=status)
        _VERIFY(STATUS)
        fexpr = tmpfields(m)
        call MAPL_StateEval(state,fexpr,field,rc=status)
        _VERIFY(STATUS)
     end if
  enddo

  _RETURN(ESMF_SUCCESS)

  end subroutine MAPL_RunExpression

#if 0
  subroutine MAPL_StateDestroy(State, RC)
    type(ESMF_State), intent(inout) :: state
    integer, optional,intent(  out) :: rc

! Local variables:
    integer                    :: STATUS

    type(ESMF_Field)                      :: field
    type(ESMF_FieldBundle)                :: bundle
    type (ESMF_StateItem_Flag),  pointer  :: itemTypeList(:)
    character(len=ESMF_MAXSTR ), pointer  :: itemNameList(:)

    integer                               :: I, J, N, NF

    call ESMF_StateGet(state, ITEMCOUNT=N,  RC=STATUS)
    _VERIFY(STATUS)

    allocate(itemNameList(N), STAT=STATUS)
    _VERIFY(STATUS)
    allocate(itemtypeList(N), STAT=STATUS)
    _VERIFY(STATUS)

    call ESMF_StateGet(state,ITEMNAMELIST=itemNamelist,ITEMTYPELIST=itemtypeList,RC=STATUS)
    _VERIFY(STATUS)

    do I=1,N
       if(itemtypeList(I)==ESMF_STATEITEM_FIELD) then
          call ESMF_StateGet(state,itemNameList(I),FIELD,RC=STATUS)
          _VERIFY(STATUS)
          call ESMF_FieldDestroy(FIELD, rc=status)
          _VERIFY(STATUS)
       else if(itemtypeList(I)==ESMF_STATEITEM_FieldBundle) then
          call ESMF_StateGet(state,itemNameList(I), BUNDLE, RC=STATUS)
          _VERIFY(STATUS)
          call ESMF_FieldBundleGet(BUNDLE,FieldCount=NF, RC=STATUS)
          _VERIFY(STATUS)
          DO J=1,NF
             call ESMF_FieldBundleGet(BUNDLE, J, FIELD, RC=STATUS)
             _VERIFY(STATUS)
             call ESMF_FieldDestroy(field, rc=status)
             _VERIFY(STATUS)
          END DO
          call ESMF_FieldBundleDestroy(BUNDLE, RC=STATUS)
          _VERIFY(STATUS)
       else if(itemtypeList(I)==ESMF_STATEITEM_State) then
!ALT we ingore nested states for now, they will get destroyed by their GC
       end if
    end do
    call ESMF_StateDestroy(STATE, RC=STATUS)
    _VERIFY(STATUS)

    deallocate(itemNameList, STAT=STATUS)
    _VERIFY(STATUS)
    deallocate(itemtypeList, STAT=STATUS)
    _VERIFY(STATUS)

    _RETURN(ESMF_SUCCESS)
  end subroutine MAPL_StateDestroy
#endif

  subroutine MAPL_StateGet(state,name,field,rc)
    type(ESMF_State), intent(in) :: state
    character(len=*), intent(in) :: name
    type(ESMF_Field), intent(inout) :: field
    integer, optional, intent(out  ) :: rc

    integer :: status
    character(len=ESMF_MAXSTR) :: bundlename, fieldname
    type(ESMF_FieldBundle) :: bundle

    integer :: i

    i = index(name,"%")
    if (i.ne.0) then
        bundlename = name(:i-1)
        fieldname = name(i+1:)
        call ESMF_StateGet(state,trim(bundlename),bundle,rc=status)
        _ASSERT(status==ESMF_SUCCESS,'Bundle '//trim(bundlename)//' not found')
        call ESMF_FieldBundleGet(bundle,trim(fieldname),field=field,rc=status)
        _ASSERT(status==ESMF_SUCCESS,'Field '//trim(fieldname)//' not found')
    else
       call ESMF_StateGet(state,trim(name),field,rc=status)
        _ASSERT(status==ESMF_SUCCESS,'Field '//trim(name)//' not found')
       _VERIFY(STATUS)
    end if

    _RETURN(ESMF_SUCCESS)

  end subroutine MAPL_StateGet

  subroutine RecordRestart( gc, import, export, clock, rc )

! !ARGUMENTS:

    type(ESMF_GridComp), intent(inout)    :: gc     ! composite gridded component 
    type(ESMF_State),       intent(inout) :: import ! import state
    type(ESMF_State),       intent(  out) :: export ! export state
    type(ESMF_Clock),       intent(inout) :: clock  ! the clock
  
    integer, intent(out), OPTIONAL        :: rc     ! Error code:
                                                     ! = 0 all is well
                                                     ! otherwise, error

    integer                         :: status


    character(len=14)                :: datestamp ! YYYYMMDD_HHMMz
    type(HistoryCollection), pointer :: list(:)
    type(HISTORY_wrap)               :: wrap
    type (HISTORY_STATE), pointer    :: IntState
    integer                          :: n, nlist
    logical                          :: doRecord
    character(len=ESMF_MAXSTR)       :: fname_saved, filename
    type (MAPL_MetaComp), pointer    :: meta

! Check if it is time to do anything
    doRecord = .false.

    call MAPL_InternalStateRetrieve(GC, meta, rc=status)
    _VERIFY(status)

    doRecord = MAPL_RecordAlarmIsRinging(meta, rc=status)
    if (.not. doRecord) then
       _RETURN(ESMF_SUCCESS)
    end if

    call MAPL_DateStampGet(clock, datestamp, rc=status)
    _VERIFY(STATUS)

! Retrieve the pointer to the state
    call ESMF_GridCompGetInternalState(gc, wrap, status)
    _VERIFY(status)
    IntState => wrap%ptr
    list => IntState%list
    nlist = size(list)

    do n=1,nlist
       if(list(n)%monthly) then
          !ALT: To avoid waste, we should not write checkpoint files
          ! when History just wrote the collection, 
          ! since the accumulators and the counters have been reset
          if (.not. ESMF_AlarmIsRinging ( list(n)%his_alarm )) then
             if (.not. list(n)%partial) then

                ! save the compname
                call ESMF_CplCompGet (INTSTATE%CCS(n), name=fname_saved, rc=status)
                _VERIFY(status)
                ! add timestamp to filename
                filename = trim(fname_saved) // datestamp
                call ESMF_CplCompSet (INTSTATE%CCS(n), name=filename, rc=status)
                _VERIFY(status)

                call ESMF_CplCompWriteRestart (INTSTATE%CCS(n), &
                     importState=INTSTATE%CIM(n), &
                     exportState=INTSTATE%GIM(n), &
                     clock=CLOCK,           &
                     userRC=STATUS)
                _VERIFY(STATUS)
                ! restore the compname
                call ESMF_CplCompSet (INTSTATE%CCS(n), name=fname_saved, rc=status)
                _VERIFY(status)
             end if
          end if
       end if
    enddo
    _RETURN(ESMF_SUCCESS)
  end subroutine RecordRestart

end module MAPL_HistoryGridCompMod

