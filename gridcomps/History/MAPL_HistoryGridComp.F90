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
  module MAPL_HistoryGridCompMod
!
! !USES:
!
  use ESMF
  use ESMFL_Mod
  use MAPL_BaseMod
  use MAPL_VarSpecMiscMod
  use MAPL_Constants
  use MAPL_IOMod
  use MAPL_CommsMod
  use MAPL_GenericMod
  use MAPL_LocStreamMod
  use MAPL_CFIOMod
  use MAPL_GenericCplCompMod
  use MAPL_StateUtils
  use MAPL_SortMod
  use MAPL_ShmemMod
  use MAPL_StringGridMapMod
  use MAPL_GridManagerMod
  use MAPL_ConfigMod
  use, intrinsic :: iso_fortran_env, only: INT64
  use, intrinsic :: iso_fortran_env, only: REAL32, REAL64
  use MAPL_HistoryCollectionMod, only: HistoryCollection, FieldSet, HistoryCollectionGlobalAttributes
  use MAPL_HistoryCollectionVectorMod, only: HistoryCollectionVector
  use MAPL_StringFieldSetMapMod, only: StringFieldSetMap
  use MAPL_StringFieldSetMapMod, only: StringFieldSetMapIterator
  use MAPL_ExceptionHandling
  use MAPL_VerticalDataMod
  use MAPL_TimeDataMod
  use mapl_RegridMethods
  use MAPL_GriddedIOitemVectorMod
  use MAPL_GriddedIOitemMod
  use pFIO_ClientManagerMod, only: o_Clients
  use MAPL_DownbitMod
  use pFIO_ConstantsMod
  use HistoryTrajectoryMod
  use StationSamplerMod
  use MaskSamplerMod
  use MAPL_StringTemplate
  use regex_module
  use MAPL_TimeUtilsMod, only: is_valid_time, is_valid_date, MAPL_UndefInt
  use gFTL_StringStringMap
  !use ESMF_CFIOMOD
  use MAPL_EpochSwathMod

  use pflogger, only: Logger, logging
  use mpi

  implicit none
  private

! !PUBLIC MEMBER FUNCTIONS:

  public SetServices

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
     type (StringFieldSetMap)            :: field_sets
     character(len=ESMF_MAXSTR)          :: expsrc
     character(len=ESMF_MAXSTR)          :: expid
     character(len=ESMF_MAXSTR)          :: expdsc
     type(HistoryCollectionGlobalAttributes) :: global_atts
     integer                             :: CoresPerNode, mype, npes
     integer                             :: AvoidRootNodeThreshold
     integer                             :: version
     logical                             :: fileOrderAlphabetical
     logical                             :: integer_time
     integer                             :: collectionWriteSplit
     integer                             :: serverSizeSplit
     logical                             :: allow_overwrite
     logical                             :: file_weights
  end type HISTORY_STATE

  type HISTORY_wrap
     type (HISTORY_STATE), pointer :: PTR
  end type HISTORY_wrap

  type HISTORY_ExchangeListType
     integer(kind=INT64), pointer                  :: lsaddr_ptr(:) => null()
  end type HISTORY_ExchangeListType

  type HISTORY_ExchangeListWrap
     type(HISTORY_ExchangeListType), pointer :: PTR
  end type HISTORY_ExchangeListWrap

  integer, parameter :: MAPL_G2G = 1
  integer, parameter :: MAPL_T2G = 2
  integer, parameter :: MAPL_T2G2G = 3

  public HISTORY_ExchangeListWrap

  type(samplerHQ) :: Hsampler

contains

!=====================================================================
!>
! Sets Initialize, Run and Finalize services for the `MAPL_HistoryGridComp` component.
!
  subroutine SetServices ( gc, rc )
    type(ESMF_GridComp), intent(inout) :: gc     !! composite gridded component
    integer, intent(out), optional     :: rc     !! return code

    integer                         :: status
    type (HISTORY_wrap)             :: wrap
    type (HISTORY_STATE), pointer   :: internal_state

! Register services for this component
! ------------------------------------

    call MAPL_GridCompSetEntryPoint ( gc, ESMF_METHOD_INITIALIZE, Initialize, _RC)

    call MAPL_GridCompSetEntryPoint ( gc, ESMF_METHOD_RUN,   Run,       _RC)

    call MAPL_GridCompSetEntryPoint ( gc, ESMF_METHOD_FINALIZE, Finalize,  _RC)

    call MAPL_GridCompSetEntryPoint ( gc, ESMF_METHOD_WRITERESTART, RecordRestart, _RC)

! Allocate an instance of the private internal state...
!------------------------------------------------------

    allocate(internal_state, _STAT)

! and save its pointer in the GC
!-------------------------------

    wrap%ptr => internal_state
    call ESMF_GridCompSetInternalState(gc, wrap, status)

! Generic Set Services
! --------------------
    call MAPL_GenericSetServices ( gc,_RC )

    _RETURN(ESMF_SUCCESS)

  end subroutine SetServices

!======================================================
!>
! Initialize initializes MAPL History Lists for Diagnostic Output.
! Diagnostics have the following attributes:
!
!1. Diagnostics may be `instantaneous` or `time-averaged`
!2. Diagnostics have a `frequency` and an associated `ref_date` and `ref_time`
!    from which the frequency is based. An `end_date` and `end_time` may also be
!    used to turn off diagnostics after a given date and time.
!3. Time-Averaged Diagnostics have an associated accumulation interval,
!    `acc_interval`, which may be <= to the diagnostic `frequency`
!4. Diagnostics are `time-stamped` with the center of the time-averaged period.
!5. The default `acc_interval` is the diagnostic `frequency`
!6. The default `ref_date` is the beginning date of the experiment
!7. The default `ref_time` is 0z
!8.  The default `end_date` and `end_time` is disabled
!
! Through the use of History Lists, the user may define the type of diagnostic output desired.
! History Lists contain the following attributes:
!
!- **filename**:     Character string defining the filename of a particular diagnostic output stream.
!- **template**:     Character string defining the time stamping template following GrADS convensions.
!    The default value depends on the duration of the file.
!- **format**:       Character string defining file format ("flat" or "CFIO"). Default = "flat".
!- **mode**:         Character string equal to "instantaneous" or "time-averaged". Default = "instantaneous".
!- **descr**:        Character string equal to the list description. Defaults to "expdsc".
!- **commment**:     Character string defining a comment.
!                     Defaults to "NetCDF-4". Can be globally set for all collections with "COMMENT:"
!- **contact**:      Character string defining a contact.
!    Defaults to "http://gmao.gsfc.nasa.gov". Can be globally set for all collections with "CONTACT:"
!- **conventions**:  Character string defining the conventions.
!    Defaults to "CF". Can be globally set for all collections with "CONVENTIONS:"
!- **institution**:  Character string defining an institution.
!    Defaults to "NASA Global Modeling and Assimilation Office". Can be globally set for all collections with "INSTITUTION:"
!- **references**:   Character string defining references.
!    Defaults to "see MAPL documentation". Can be globally set for all collections with "REFERENCES:"
!- **source**:       Character string defining source.
!    Defaults to "unknown". Can be globally set for all collections with "SOURCE:"
!- **frequency**:    Integer (HHMMSS) for the frequency of output.  Default = 060000.
!- **acc_interval**: Integer (HHMMSS) for the acculation interval (<= frequency) for time-averaged diagnostics.
!    Default = Diagnostic Frequency.
!- **ref_date**:     Integer (YYYYMMDD) reference date from which the frequency is based.
!    Default is the Experiment beginning date.
!- **ref_time**:     Integer (HHMMSS) reference time from which the frequency is based.
!    Default is 000000.
!- **end_date**:     Integer (YYYYMMDD) ending date to stop diagnostic output.  Default is disabled.
!- **end_time**:     Integer (HHMMSS) ending time to stop diagnostic output. Default is disabled.
!- **duration**:     Integer (HHMMSS) for the duration of each file.  Default = frequency (1 time-record per file).
!- **fields**:       Paired character strings for the diagnostic Name and its associated Gridded Component.
!- **subset**:       Optional subset (lonMin lonMax latMin latMax) for the output
!- **xyoffset**:     Optional Flag for Grid Staggering (0:DcPc, 1:DePc, 2:DcPe, 3:DePe)
!- **levels**:       Optional list of output levels (Default is all levels on Native Grid).
!- **vvars**:        Optional Field (and Transform) to use for Vertical Interpolation (eg., 'log(PLE)' , 'DYN' ).
!- **vunit**:        Optional Units to use for Vertical Index of Output File.
!- **vscale**:       Optional Scaling to use between Output Unit and VVARS unit.
!
  subroutine Initialize ( gc, import, dumexport, clock, rc )

    type(ESMF_GridComp), intent(inout)    :: gc        !! composite gridded component
    type(ESMF_State),       intent(inout) :: import    !! import state
    type(ESMF_State),       intent(inout) :: dumexport !! export state
    type(ESMF_Clock),       intent(inout) :: clock     !! the clock
    integer, intent(out), OPTIONAL        :: rc        !! Error code:

    integer                         :: status

    logical                         :: errorFound
    logical                         :: found
    type(HistoryCollection), pointer     :: list(:)
    type(HISTORY_wrap)              :: wrap
    type (HISTORY_STATE), pointer   :: IntState
    type(HISTORY_ExchangeListWrap)  :: lswrap

    type(ESMF_State), pointer      :: export (:) => null()
    type(ESMF_State), pointer      :: exptmp (:)
    type(ESMF_State)               :: expsrc, expdst
    type(ESMF_Time)                :: StartTime
    type(ESMF_Time)                :: CurrTime
    type(ESMF_Time)                ::  RingTime
    type(ESMF_Time)                ::   RefTime
    type(ESMF_Time)                :: StartOfThisMonth
    type(ESMF_Time)                :: nextMonth
    type(ESMF_TimeInterval)        :: oneMonth, dur
    type(ESMF_TimeInterval)        :: Frequency
    type(ESMF_Array)               :: array
    type(ESMF_Field)               :: field,f_extra
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
    integer(kind=INT64)                                 :: ADDR
    integer(kind=INT64), pointer                        :: LSADDR_PTR(:) => null()
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
    logical                                   :: isPresent,hasNX,hasNY
    real                                      :: lvl

    integer                                   :: mntly
    integer                                   :: spltFld
    integer                                   :: useRegex
    integer                                   :: unitr, unitw
    integer                                   :: tm,resolution(2)
    logical                                   :: match, contLine, con3
    character(len=2048)                       :: line
    type(ESMF_Config)                         :: cfg
    character(len=ESMF_MAXSTR)                :: HIST_CF
    character(len=ESMF_MAXSTR)                :: BLANK=""

!   Parser Variables
    logical          :: DoCopy
    type(ESMF_State) :: parser_state
    type(ESMF_Field) :: parser_field

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

!   variables for counting table
    integer :: nline, ncol
    integer :: swath_count

    type(HistoryCollection) :: collection
    character(len=ESMF_MAXSTR) :: cFileOrder
    type(FieldSet), pointer :: field_set
    type(FieldSet), pointer :: fld_set
    type(FieldSet), pointer :: newFieldSet => null()
    character(len=:), pointer :: key
    type(StringFieldSetMapIterator) :: field_set_iter
    character(ESMF_MAXSTR) :: field_set_name
    integer :: collection_id, regrid_hints
    logical, allocatable :: needSplit(:)
    type(ESMF_Field), allocatable :: fldList(:)
    character(len=ESMF_MAXSTR), allocatable :: regexList(:)
    type(StringStringMap) :: global_attributes
    character(len=ESMF_MAXSTR) :: name,regrid_method
    logical :: has_conservative_keyword, has_regrid_keyword, has_extrap_keyword
    integer :: create_mode
    character(len=:), allocatable :: uppercase_algorithm
    character(len=2) :: tmpchar

! Begin
!------

    _UNUSED_DUMMY(dumexport)

    call MAPL_GetObjectFromGC ( gc, GENSTATE, _RC)

! Retrieve the pointer to the state
    call ESMF_GridCompGetInternalState(gc, wrap, status)
    IntState => wrap%ptr

    call ESMF_UserCompGetInternalState(GC, 'MAPL_LocStreamList', &
        lswrap, STATUS)
    if (status == ESMF_SUCCESS) then
       lsaddr_ptr => lswrap%ptr%lsaddr_ptr
    end if

    call ESMF_GridCompGet(gc, vm=vm, _RC)

    call ESMF_VMGetCurrent(vm, _RC)
    call ESMF_VMGet       (VM, localpet=MYPE, petcount=NPES,  _RC)

    IntState%mype = mype
    IntState%npes = npes


! Get Clock StartTime for Default ref_date, ref_time
! --------------------------------------------------
    call ESMF_ClockGet ( clock,     calendar=cal,       _RC )
    call ESMF_ClockGet ( clock,     currTime=CurrTime,  _RC )
    call ESMF_ClockGet ( clock,     StartTime=StartTime,_RC )
    call ESMF_TimeGet  ( StartTime, TimeString=string  ,_RC )

    read(string( 1: 4),'(i4.4)') year
    read(string( 6: 7),'(i2.2)') month
    read(string( 9:10),'(i2.2)') day
    read(string(12:13),'(i2.2)') hour
    read(string(15:16),'(i2.2)') minute
    read(string(18:18),'(i2.2)') second

    nymd0 =  year*10000 +  month*100 + day
    nhms0 =  hour*10000 + minute*100 + second

    call ESMF_TimeGet  ( CurrTime, TimeString=string  ,_RC )

    read(string( 1: 4),'(i4.4)') year
    read(string( 6: 7),'(i2.2)') month
    read(string( 9:10),'(i2.2)') day
    read(string(12:13),'(i2.2)') hour
    read(string(15:16),'(i2.2)') minute
    read(string(18:18),'(i2.2)') second

    nymdc =  year*10000 +  month*100 + day
    nhmsc =  hour*10000 + minute*100 + second

    ! set up few variables to deal with monthly
    startOfThisMonth = currTime
    call ESMF_TimeSet(startOfThisMonth,dd=1,h=0,m=0,s=0,_RC)
    call ESMF_TimeIntervalSet( oneMonth, MM=1, StartTime=StartTime, _RC)


! Read User-Supplied History Lists from Config File
! -------------------------------------------------
    call ESMF_GridCompGet( gc, config=config, _RC )
    call ESMF_ConfigGetAttribute ( config, value=INTSTATE%expsrc, &
                                   label ='EXPSRC:', default='', _RC )
    call ESMF_ConfigGetAttribute ( config, value=INTSTATE%expid, &
                                   label ='EXPID:', default='', _RC )
    call ESMF_ConfigGetAttribute ( config, value=INTSTATE%expdsc, &
                                   label ='EXPDSC:', default='', _RC )
    call ESMF_ConfigGetAttribute ( config, value=INTSTATE%global_atts%institution, &
                                   label ='INSTITUTION:', default='NASA Global Modeling and Assimilation Office', _RC)
    call ESMF_ConfigGetAttribute ( config, value=INTSTATE%global_atts%references, &
                                   label ='REFERENCES:', default='see MAPL documentation', _RC)
    call ESMF_ConfigGetAttribute ( config, value=INTSTATE%global_atts%contact, &
                                   label ='CONTACT:', default='', _RC)
    call ESMF_ConfigGetAttribute ( config, value=INTSTATE%global_atts%comment, &
                                   label ='COMMENT:', default='NetCDF-4', _RC)
    call ESMF_ConfigGetAttribute ( config, value=INTSTATE%global_atts%conventions, &
                                   label ='CONVENTIONS:', default='CF', _RC)
    call ESMF_ConfigGetAttribute ( config, value=INTSTATE%global_atts%source, &
                                   label ='SOURCE:', &
                                   default=trim(INTSTATE%expsrc) // ' experiment_id: ' // trim(INTSTATE%expid), _RC)
    call ESMF_ConfigGetAttribute ( config, value=INTSTATE%CoresPerNode, &
                                   label ='CoresPerNode:', default=min(npes,8), _RC )
    call ESMF_ConfigGetAttribute ( config, value=disableSubVmChecks, &
                                   label ='DisableSubVmChecks:', default=.false., _RC )
    call ESMF_ConfigGetAttribute ( config, value=INTSTATE%AvoidRootNodeThreshold, &
                                   label ='AvoidRootNodeThreshold:', default=1024, _RC )

    call ESMF_ConfigGetAttribute(config, value=cFileOrder,         &
                                         label='FileOrder:', default='ABC', _RC)
    call ESMF_ConfigGetAttribute(config, value=intState%allow_overwrite,  &
                                         label='Allow_Overwrite:', default=.false., _RC)
    call ESMF_ConfigGetAttribute(config, value=intState%file_weights,  &
                                         label='file_weights:', default=.false., _RC)
    create_mode = PFIO_NOCLOBBER ! defaut no overwrite
    if (intState%allow_overwrite) create_mode = PFIO_CLOBBER

    if (trim(cFileOrder) == 'ABC') then
       intstate%fileOrderAlphabetical = .true.
    else if (trim(cFileOrder) == 'AddOrder') then
       intstate%fileOrderAlphabetical = .false.
    else
       _FAIL('needs informative message')
    end if

    call ESMF_ConfigGetAttribute(config, value=intstate%integer_time,label="IntegerTime:", default=.false.,_RC)

    call ESMF_ConfigGetAttribute(config, value=IntState%collectionWriteSplit, &
         label = 'CollectionWriteSplit:', default=0, _RC)
    call ESMF_ConfigGetAttribute(config, value=IntState%serverSizeSplit, &
         label = 'ServerSizeSplit:', default=0, _RC)
    call o_Clients%split_server_pools(n_server_split = IntState%serverSizeSplit, &
                                      n_hist_split   = IntState%collectionWriteSplit,_RC)

    call ESMF_ConfigGetAttribute(config, value=snglcol,          &
                                         label='SINGLE_COLUMN:', default=0, _RC)
    call ESMF_ConfigGetAttribute(config, value=intstate%version,          &
                                         label='VERSION:', default=0, _RC)
    if( MAPL_AM_I_ROOT() ) then
       print *
       print *, 'EXPSRC:',trim(INTSTATE%expsrc)
       print *, 'EXPID: ',trim(INTSTATE%expid)
       print *, 'Descr: ',trim(INTSTATE%expdsc)
       print *, 'DisableSubVmChecks:', disableSubVmChecks
       print *
    endif

! Determine Number of Output Streams
! ----------------------------------
    if( MAPL_AM_I_ROOT() ) then
       print *, 'Reading HISTORY RC Files:'
       print *, '-------------------------'
    endif

    call ESMF_ConfigFindLabel ( config,'COLLECTIONS:',_RC )
    tend  = .false.
    nlist = 0
    allocate(IntState%list(nlist), _STAT)
    do while (.not.tend)
          call ESMF_ConfigGetAttribute ( config,value=tmpstring,default='',rc=STATUS) !ALT: we don't check return status!!!
          if (tmpstring /= '')  then

             collection%collection = tmpstring
             collection%filename = tmpstring
             call IntState%collections%push_back(collection)

             nlist = nlist + 1
             allocate( list(nlist), _STAT )
             if (nlist > 1) list(1:nlist-1)=IntState%list
             list(nlist)%collection = tmpstring
             list(nlist)%filename = list(nlist)%collection
             deallocate(IntState%list)
             IntState%list => list
          end if
          call ESMF_ConfigNextLine     ( config,tableEnd=tend,_RC )
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

         call ESMF_ConfigFindLabel ( config,'GRID_LABELS:',_RC )
         tend  = .false.
         do while (.not.tend)
             call ESMF_ConfigGetAttribute ( config,value=tmpstring,default='',rc=STATUS) !ALT: we don't check return status!!!
             if (tmpstring /= '')  then
                call IntState%output_grids%insert(trim(tmpString), output_grid)
             end if
             call ESMF_ConfigNextLine     ( config,tableEnd=tend,_RC )
          enddo

          swath_count = 0
          iter = IntState%output_grids%begin()
          do while (iter /= IntState%output_grids%end())
             key => iter%key()
             call ESMF_ConfigGetAttribute(config, value=grid_type, label=trim(key)//".GRID_TYPE:",_RC)
             call  ESMF_ConfigFindLabel(config,trim(key)//".NX:",isPresent=hasNX,_RC)
             call  ESMF_ConfigFindLabel(config,trim(key)//".NY:",isPresent=hasNY,_RC)
             if ((.not.hasNX) .and. (.not.hasNY)) then
                if (trim(grid_type)=='Cubed-Sphere') then
                   call MAPL_MakeDecomposition(nx,ny,reduceFactor=6,_RC)
                else
                   call MAPL_MakeDecomposition(nx,ny,_RC)
                end if
                call MAPL_ConfigSetAttribute(config, value=nx,label=trim(key)//".NX:",_RC)
                call MAPL_ConfigSetAttribute(config, value=ny,label=trim(key)//".NY:",_RC)
             end if

             if (trim(grid_type)/='Swath') then
                output_grid = grid_manager%make_grid(config, prefix=key//'.', _RC)
             else
                swath_count = swath_count + 1
                !
                ! Hsampler use the first config to setup epoch
                !
                if (swath_count == 1) then
                   Hsampler = samplerHQ(clock, key, config, _RC)
                end if
                call Hsampler%config_accumulate(key, config, _RC)
                output_grid = Hsampler%create_grid(key, currTime, grid_type=grid_type, _RC)
             end if
             call IntState%output_grids%set(key, output_grid)
             call iter%next()
          end do
       end block OUTPUT_GRIDS
    end if

    if (intstate%version >= 2) then
       call ESMF_ConfigFindLabel(config, 'FIELD_SETS:', _RC)
       table_end = .false.
       do while (.not. table_end)
          call ESMF_ConfigGetAttribute ( config, value=tmpstring,default='',rc=STATUS) !ALT: we don't check return status!!!
          if (tmpstring /= '')  then
             ! Add empty FieldSet to dictionary of field collections
             allocate(field_set)
             call intstate%field_sets%insert(trim(tmpString), field_set)
             deallocate(field_set)
          end if
          call ESMF_ConfigNextLine     ( config,tableEnd=table_end,_RC )
       enddo

       field_set_iter = intState%field_sets%begin()
       do while (field_set_iter /= intState%field_sets%end())
          key => field_set_iter%key()
          field_set => field_set_iter%value()
          call parse_fields(config, key, field_set, _RC)
          call field_set_iter%next()
       end do

    end if

    allocate(IntState%Regrid(nlist), _STAT)
    allocate(          Vvarn(nlist), _STAT)
    allocate(INTSTATE%STAMPOFFSET(nlist), _STAT)

! We are parsing HISTORY config file to split each collection into separate RC
! ----------------------------------------------------------------------------

    if( MAPL_AM_I_ROOT(vm) ) then
       call ESMF_ConfigGetAttribute(config, value=HIST_CF, &
            label="HIST_CF:", default="HIST.rc", _RC )
       unitr = GETFILE(HIST_CF, FORM='formatted', _RC)
!       for each collection
       do n = 1, nlist
         rewind(unitr)
         string = trim( list(n)%collection ) // '.'
         unitw = GETFILE(trim(string)//'rcx', FORM='formatted', _RC)
         match = .false.
         contLine = .false.
         con3 = .false.

         do while (.true.)
            read(unitr, '(A)', end=1234) line
            j = index( adjustl(line), trim(adjustl(string)) )
            match = (j == 1)
            if (match) then
               j = index(line, trim(string)//'fields:')
               contLine = (j > 0)
               k = index(line, trim(string)//'obs_files:')
               con3 = (k > 0)
            end if
            if (match .or. contLine .or. con3) then
               write(unitw,'(A)') trim(line)
            end if
            if (contLine) then
               if (adjustl(line) == '::') contLine = .false.
            end if
            if (con3) then
               if (adjustl(line) == '::') con3 = .false.
            endif
         end do

1234     continue
         call free_file(unitw, _RC)
      end do

      call free_file(unitr, _RC)

    end if

! Overwrite the above process if HISTORY.rc encounters DEFINE_OBS_PLATFORM for OSSE
! ----------------------------------------------------------------------------
    if( MAPL_AM_I_ROOT(vm) ) then
       call regen_rcx_for_obs_platform (config, nlist, list, _RC)
    end if

    call ESMF_VMbarrier(vm, _RC)

! Initialize History Lists
! ------------------------

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

       cfg = ESMF_ConfigCreate(_RC)

       call ESMF_ConfigLoadFile(cfg, filename = trim(string)//'rcx', _RC)
       call ESMF_ConfigGetAttribute ( cfg, value=list(n)%template, default="", &
                                      label=trim(string) // 'template:' ,_RC )
       call ESMF_ConfigGetAttribute ( cfg, value=list(n)%format,default='flat', &
                                      label=trim(string) // 'format:' ,_RC )
       call ESMF_ConfigGetAttribute ( cfg, value=list(n)%mode,default='instantaneous', &
                                      label=trim(string) // 'mode:' ,_RC )

       ! Fill the global attributes

       ! filename is special as it does double duty, so we fill directly
       ! from HistoryCollection object
       list(n)%global_atts%filename = list(n)%filename
       call ESMF_ConfigGetAttribute ( cfg, value=list(n)%global_atts%descr, &
                                      default=INTSTATE%expdsc, &
                                      label=trim(string) // 'descr:' ,_RC )
       call ESMF_ConfigGetAttribute ( cfg, value=list(n)%global_atts%comment, &
                                      default=INTSTATE%global_atts%comment, &
                                      label=trim(string) // 'comment:' ,_RC)
       call ESMF_ConfigGetAttribute ( cfg, value=list(n)%global_atts%contact, &
                                      default=INTSTATE%global_atts%contact, &
                                      label=trim(string) // 'contact:' ,_RC)
       call ESMF_ConfigGetAttribute ( cfg, value=list(n)%global_atts%conventions, &
                                      default=INTSTATE%global_atts%conventions, &
                                      label=trim(string) // 'conventions:' ,_RC)
       call ESMF_ConfigGetAttribute ( cfg, value=list(n)%global_atts%institution, &
                                      default=INTSTATE%global_atts%institution, &
                                      label=trim(string) // 'institution:' ,_RC)
       call ESMF_ConfigGetAttribute ( cfg, value=list(n)%global_atts%references, &
                                      default=INTSTATE%global_atts%references, &
                                      label=trim(string) // 'references:' ,_RC)
       call ESMF_ConfigGetAttribute ( cfg, value=list(n)%global_atts%source, &
                                      default=INTSTATE%global_atts%source, &
                                      label=trim(string) // 'source:' ,_RC)

       call ESMF_ConfigGetAttribute ( cfg, mntly, default=0, &
                                      label=trim(string) // 'monthly:',_RC )
       list(n)%monthly = (mntly /= 0)
       call ESMF_ConfigGetAttribute ( cfg, spltFld, default=0, &
                                      label=trim(string) // 'splitField:',_RC )
       list(n)%splitField = (spltFld /= 0)
       call ESMF_ConfigGetAttribute ( cfg, useRegex, default=0, &
                                      label=trim(string) // 'UseRegex:',_RC )
       list(n)%regex = (useRegex /= 0)
       call ESMF_ConfigGetAttribute ( cfg, list(n)%frequency, default=060000, &
                                      label=trim(string) // 'frequency:',_RC )

       call ESMF_ConfigGetAttribute ( cfg, list(n)%acc_interval, default=list(n)%frequency, &
                                      label=trim(string) // 'acc_interval:',_RC )

       call ESMF_ConfigFindLabel(cfg,label= trim(string) // 'acc_ref_time',isPresent = isPresent, _RC)
       if (isPresent) then
          call ESMF_ConfigGetAttribute ( cfg, list(n)%acc_ref_time, default=000000, &
                                         label=trim(string) // 'acc_ref_time:',_RC )
          _ASSERT(is_valid_time(list(n)%ref_time),'Invalid acc_ref_time')
          list(n)%acc_offset = get_acc_offset(currTime,list(n)%acc_ref_time,_RC)
       else
          list(n)%acc_offset = 0
       end if

       call ESMF_ConfigGetAttribute ( cfg, list(n)%ref_date, default=nymdc, &
                                      label=trim(string) // 'ref_date:',_RC )
       _ASSERT(is_valid_date(list(n)%ref_date),'Invalid ref_date')
       call ESMF_ConfigGetAttribute ( cfg, list(n)%ref_time, default=000000, &
                                      label=trim(string) // 'ref_time:',_RC )
       _ASSERT(is_valid_time(list(n)%ref_time),'Invalid ref_time')

       call ESMF_ConfigGetAttribute ( cfg, list(n)%start_date, default=MAPL_UndefInt, &
                                      label=trim(string) // 'start_date:',_RC )
       _ASSERT(is_valid_date(list(n)%start_date),'Invalid start_date')
       call ESMF_ConfigGetAttribute ( cfg, list(n)%start_time, default=MAPL_UndefInt, &
                                      label=trim(string) // 'start_time:',_RC )
       _ASSERT(is_valid_time(list(n)%start_time),'Invalid start_time')

       call ESMF_ConfigGetAttribute ( cfg, list(n)%end_date, default=MAPL_UndefInt, &
                                      label=trim(string) // 'end_date:',_RC )
       _ASSERT(is_valid_date(list(n)%end_date),'Invalid end_date')
       call ESMF_ConfigGetAttribute ( cfg, list(n)%end_time, default=MAPL_UndefInt, &
                                      label=trim(string) // 'end_time:',_RC )
       _ASSERT(is_valid_time(list(n)%end_time),'Invalid end_time')

       call ESMF_ConfigGetAttribute ( cfg, list(n)%duration, default=list(n)%frequency, &
                                      label=trim(string) // 'duration:'  ,_RC )
       call ESMF_ConfigGetAttribute ( cfg, list(n)%verbose, default=0, &
                                      label=trim(string) // 'verbose:'  ,_RC )

       call ESMF_ConfigGetAttribute ( cfg, list(n)%vscale, default=1.0, &
                                      label=trim(string) // 'vscale:'  ,_RC )
       call ESMF_ConfigGetAttribute ( cfg, list(n)%vunit, default="", &
                                      label=trim(string) // 'vunit:'  ,_RC )
       call ESMF_ConfigGetAttribute ( cfg, list(n)%nbits_to_keep, default=MAPL_NBITS_NOT_SET, &
                                      label=trim(string) // 'nbits:' ,_RC )
       call ESMF_ConfigGetAttribute ( cfg, list(n)%deflate, default=0, &
                                      label=trim(string) // 'deflate:' ,_RC )

       ! We only allow deflate level to be between 0 and 9
       _ASSERT( .not. (list(n)%deflate < 0 .or. list(n)%deflate > 9), 'deflate level must be between 0 and 9')

       call ESMF_ConfigGetAttribute ( cfg, list(n)%zstandard_level, default=0, &
                                      label=trim(string) // 'zstandard_level:' ,_RC )

       ! We only allow zstandard level to be between 0 and 22
       _ASSERT( .not. (list(n)%zstandard_level < 0 .or. list(n)%zstandard_level > 22), 'zstandard level must be between 0 and 22')

       ! We only allow either deflate or zstandard compression to be used, not both
       _ASSERT( .not. (list(n)%deflate > 0 .and. list(n)%zstandard_level > 0), 'deflate and zstandard_level cannot be used together')

       call ESMF_ConfigGetAttribute ( cfg, list(n)%quantize_algorithm_string, default='NONE', &
                                      label=trim(string) // 'quantize_algorithm:' ,_RC )

       call ESMF_ConfigGetAttribute ( cfg, list(n)%quantize_level, default=0, &
                                      label=trim(string) // 'quantize_level:' ,_RC )

       ! Uppercase the algorithm string just to allow for any case
       ! CF Conventions will prefer 'bitgroom', 'bitround', and 'granular_bitround'
       ! but we will allow 'GranularBR' in MAPL2, deprecate it, and remove it in MAPL3
       uppercase_algorithm = ESMF_UtilStringUpperCase(list(n)%quantize_algorithm_string,_RC)
       select case (trim(uppercase_algorithm))
       case ('NONE')
          list(n)%quantize_algorithm = MAPL_NOQUANTIZE
          ! If quantize_algorithm is 0, then quantize_level must be 0
          _ASSERT( list(n)%quantize_level == 0, 'quantize_algorithm is none, so quantize_level must be "none"')
       case ('BITGROOM')
          list(n)%quantize_algorithm = MAPL_QUANTIZE_BITGROOM
       case ('GRANULARBR', 'GRANULAR_BITROUND')
          list(n)%quantize_algorithm = MAPL_QUANTIZE_GRANULAR_BITROUND
       case ('BITROUND')
          list(n)%quantize_algorithm = MAPL_QUANTIZE_BITROUND
       case default
          _FAIL('Invalid quantize_algorithm. Allowed values are none, bitgroom, granular_bitround, granularbr (deprecated), and bitround')
       end select

       ! If nbits_to_keep < MAPL_NBITS_UPPER_LIMIT (24) and quantize_algorithm greater than 0, then a user might be doing different
       ! shaving algorithms. We do not allow this
       _ASSERT( .not. ( (list(n)%nbits_to_keep < MAPL_NBITS_UPPER_LIMIT) .and. (list(n)%quantize_algorithm > MAPL_NOQUANTIZE) ), 'nbits < 24 and quantize_algorithm not "none" is not allowed. Choose a supported quantization method.')

       ! Now we test in the case that a valid quantize algorithm is chosen
       if (list(n)%quantize_algorithm /= MAPL_NOQUANTIZE) then
         ! If quantize_algorithm is greater than 0, then quantize_level must be greater than or equal to 0
         _ASSERT( list(n)%quantize_level >= 0, 'netCDF quantize has been enabled, so quantize_level must be greater than or equal to 0')
       end if

       ! If a user has chosen MAPL_QUANTIZE_BITROUND, then we allow a maximum of 23 bits to be kept
       if (list(n)%quantize_algorithm == MAPL_QUANTIZE_BITROUND) then
          write(tmpchar, '(I2)') MAPL_QUANTIZE_MAX_NSB
         _ASSERT( list(n)%quantize_level <= MAPL_QUANTIZE_MAX_NSB, 'netCDF bitround has been enabled, so number of significant bits (quantize_level) must be less than or equal to ' // trim(tmpchar))
       end if

       ! For MAPL_QUANTIZE_GRANULAR_BITROUND and MAPL_QUANTIZE_BITGROOM, these use number of
       ! significant digits, so for single precision, we allow a maximum of 7 digits to be kept
       if (list(n)%quantize_algorithm == MAPL_QUANTIZE_GRANULAR_BITROUND .or. list(n)%quantize_algorithm == MAPL_QUANTIZE_BITGROOM) then
          write(tmpchar, '(I2)') MAPL_QUANTIZE_MAX_NSD
         _ASSERT( list(n)%quantize_level <= MAPL_QUANTIZE_MAX_NSD, 'netCDF granular bitround or bitgroom has been enabled, so number of significant digits (quantize_level) must be less than or equal to ' // trim(tmpchar))
       end if

       tm_default = -1
       call ESMF_ConfigGetAttribute ( cfg, list(n)%tm, default=tm_default, &
                                      label=trim(string) // 'tm:', _RC )

       call ESMF_ConfigFindLabel ( cfg, label=trim(string) // 'ecmwf_extrap:',isPresent=has_extrap_keyword,_RC)
       if (has_extrap_keyword) then
          call ESMF_ConfigGetAttribute ( cfg, list(n)%extrap_below_surf, &
                                         label=trim(string) // 'ecmwf_extrap:'  ,_RC )
       end if

       call ESMF_ConfigFindLabel ( cfg, label=trim(string) // 'conservative:',isPresent=has_conservative_keyword,_RC)
       call ESMF_ConfigFindLabel ( cfg, label=trim(string) // 'regrid_method:',isPresent=has_regrid_keyword,_RC)
       _ASSERT(.not.(has_conservative_keyword .and. has_regrid_keyword),trim(string)//" specified both conservative and regrid_method")

       list(n)%regrid_method = REGRID_METHOD_BILINEAR
       if (has_conservative_keyword) then
          call ESMF_ConfigGetAttribute ( cfg, list(n)%regrid_method, default=0, &
                                         label=trim(string) // 'conservative:'  ,_RC )
          if (list(n)%regrid_method==0) then
             list(n)%regrid_method=REGRID_METHOD_BILINEAR
          else if (list(n)%regrid_method==1) then
             list(n)%regrid_method=REGRID_METHOD_CONSERVE
          end if
       end if
       if (has_regrid_keyword) then
          call ESMF_ConfigGetAttribute ( cfg, regrid_method, label=trim(string) // 'regrid_method:'  ,_RC )
           list(n)%regrid_method = regrid_method_string_to_int(trim(regrid_method))
       end if

       call ESMF_ConfigGetAttribute(cfg, value=list(n)%sampler_spec, default="", &
            label=trim(string) // 'sampler_spec:', _RC)
       call ESMF_ConfigGetAttribute(cfg, value=list(n)%stationIdFile, default="", &
            label=trim(string) // 'station_id_file:', _RC)
       call ESMF_ConfigGetAttribute(cfg, value=list(n)%stationSkipLine, default=0, &
            label=trim(string) // 'station_skip_line:', _RC)

! Get an optional file containing a 1-D track for the output
       call ESMF_ConfigGetDim(cfg, nline, ncol,  label=trim(string)//'obs_files:', rc=rc)  ! here donot check rc on purpose
       if (list(n)%sampler_spec == 'trajectory') then
          list(n)%timeseries_output = .true.
       end if


! Handle "backwards" mode: this is hidden (i.e. not documented) feature
! Defaults to .false.
       call ESMF_ConfigGetAttribute ( cfg, reverse, default=0, &
                                      label=trim(string) // 'backwards:'  ,_RC )
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
               & default='', _RC)
          if (field_set_name /= '') then  ! field names already parsed
             old_fields_style = .false.
             field_set => intstate%field_sets%at(trim(field_set_name))
             _ASSERT(associated(field_set),'needs informative message')
          end if
       end if

       if (old_fields_style) then
          field_set_name = trim(string) // 'fields'
          allocate(field_set)
          call parse_fields(cfg, trim(field_set_name), field_set, collection_name = list(n)%collection, items = list(n)%items, _RC)
       end if

       list(n)%field_set => field_set

! Decide on orientation of output
! -------------------------------

          call ESMF_ConfigFindLabel(cfg,trim(string)//'positive:',isPresent=isPresent,_RC)
          if (isPresent) then
             call ESMF_ConfigGetAttribute(cfg,value=list(n)%positive,_RC)
             _ASSERT(list(n)%positive=='down'.or.list(n)%positive=='up',"positive value for collection must be down or up")
          else
             list(n)%positive = 'down'
          end if

! Get an optional list of output levels
! -------------------------------------

       list(n)%vvars = ""

       len = ESMF_ConfigGetLen( cfg, label=trim(trim(string) // 'levels:'), rc = status )

       LEVS: if( status == ESMF_SUCCESS ) then
          call ESMF_ConfigFindLabel( cfg, label=trim(trim(string) // 'levels:'),_RC)
             j = 0
          do i = 1, len
             call ESMF_ConfigGetAttribute ( cfg,value=tmpstring ,_RC)
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

                   unit = GETFILE(trim(tmpstring), form='formatted', _RC)

                   if (MAPL_Am_I_Root(vm)) then
                      k=0
                      do while (.true.)
                         read(unit, *, end=987) lvl
                         k = k+1
                      end do
987                   continue

                   end if

                   call MAPL_CommsBcast(vm, DATA=k, N=1, ROOT=MAPL_Root, _RC)

                   allocate( list(n)%levels(k), stat = status )

                   if (MAPL_Am_I_Root(vm)) then
                      rewind(unit)
                      do l=1,k
                         read(unit, *) list(n)%levels(l)
                      end do
                   end if

                   call MAPL_CommsBcast(vm, DATA=list(n)%levels, N=k, &
                        ROOT=MAPL_Root, _RC)

                   call FREE_FILE(UNIT)
                end if
             end if

             if(isFileName) cycle

             allocate( levels(j), stat = status )
                     i1 = index(tmpstring(:),",")
                 if( i1.eq.1 )  tmpstring = adjustl( tmpstring(2:)   )
                     j1 = index(tmpstring(:),",")-1
                 if( j1.gt.0 )  tmpstring = adjustl( tmpstring(1:j1) )
             read(tmpstring,*)  levels(j)
             if( j.eq.1 ) then
                 allocate( list(n)%levels(j), stat = status )
                 list(n)%levels(j) = levels(j)
             else
                 levels(1:j-1) = list(n)%levels(:)
                 deallocate( list(n)%levels )
                   allocate( list(n)%levels(j), stat = status )
                   list(n)%levels(:) = levels(:)
             endif
             deallocate( levels )
          enddo

! Get an interpolating variable
! -----------------------------

          call ESMF_ConfigFindLabel ( cfg,trim(string) // 'vvars:',isPresent=isPresent,_RC )
          VINTRP: if(isPresent) then

             call ESMF_ConfigGetAttribute ( cfg,value=list(n)%vvars(1), _RC)
             i = index(list(n)%vvars(1)(  1:),"'")
             j = index(list(n)%vvars(1)(i+1:),"'")+i
             if( i.ne.0 ) then
                 list(n)%vvars(1) = adjustl( list(n)%vvars(1)(i+1:j-1) )
             else
                 list(n)%vvars(1) = adjustl( list(n)%vvars(1) )
             endif

             call ESMF_ConfigGetAttribute ( cfg,value=tmpstring ,_RC)
             if( trim(tmpstring) == ',' )  then
                 call ESMF_ConfigGetAttribute ( cfg,value=list(n)%vvars(2),_RC)
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
                   allocate( fields(4,  list(n)%field_set%nfields), _STAT )
                   fields(1,1:list(n)%field_set%nfields-1) = list(n)%field_set%fields(1,:)
                   fields(2,1:list(n)%field_set%nfields-1) = list(n)%field_set%fields(2,:)
                   fields(3,1:list(n)%field_set%nfields-1) = list(n)%field_set%fields(3,:)
                   fields(4,1:list(n)%field_set%nfields-1) = list(n)%field_set%fields(4,:)
                   fields(1,  list(n)%field_set%nfields  ) = Vvar
                   fields(2,  list(n)%field_set%nfields  ) = list(n)%vvars (2)
                   fields(3,  list(n)%field_set%nfields  ) = Vvar
                   fields(4,  list(n)%field_set%nfields  ) = BLANK
                   deallocate( list(n)%field_set%fields, _STAT )
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
                                         label=trim(string) // 'grid_label:' ,_RC )
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
                call ESMF_ConfigGetAttribute ( cfg,value=tmpstring ,_RC)
                if( trim(tmpstring) == ',' )  cycle
                j = j + 1
                _ASSERT(j<=2,'needs informative message')
                        i1 = index(tmpstring(:),",")
                    if( i1.eq.1 )  tmpstring = adjustl( tmpstring(2:)   )
                        j1 = index(tmpstring(:),",")-1
                    if( j1.gt.0 )  tmpstring = adjustl( tmpstring(1:j1) )
                read(tmpstring,*)  resolution(j)
             enddo
             call list(n)%AddGrid(IntState%output_grids,resolution,_RC)
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
                                         label=trim(string) // 'cubeFormat:'  ,_RC )
       end if
       list(n)%useNewFormat = (newFormat /= 0)

! Force history so that time averaged collections are timestamped with write time
       call ESMF_ConfigGetAttribute(cfg, list(n)%ForceOffsetZero, default=.false., &
                                    label=trim(string)//'timestampEnd:', _RC)
! Force history so that time averaged collections are timestamped at the begining of the accumulation interval
       call ESMF_ConfigGetAttribute(cfg, list(n)%timeStampStart, default=.false., &
                                    label=trim(string)//'timestampStart:', _RC)

! Get an optional chunk size
! --------------------------
       len = ESMF_ConfigGetLen(cfg, label=trim(trim(string) // 'chunksize:'), rc = status)
       if ( status == ESMF_SUCCESS ) then
          call ESMF_ConfigFindLabel( cfg, label=trim(trim(string) // 'chunksize:'), _RC)
          chnksz = 4
          if (list(n)%useNewFormat) then
             chnksz = 5
          end if
          allocate( list(n)%chunksize(chnksz), stat = status)
          j=0
          do i=1,len
             call ESMF_ConfigGetAttribute( cfg,value=tmpstring, _RC)
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
                                      label=trim(string) // 'regrid_exch:' ,_RC )

       call ESMF_ConfigGetAttribute ( cfg, value=gridname, default="", &
                                      label=trim(string) // 'regrid_name:' ,_RC )

       NULLIFY(IntState%Regrid(n)%PTR)
       if (tilefile /= '' .OR. gridname /= '') then
          allocate(IntState%Regrid(n)%PTR, _STAT)
          IntState%Regrid(n)%PTR%tilefile = tilefile
          IntState%Regrid(n)%PTR%gridname = gridname
       end if

! Set Alarms
! ----------

       if (list(n)%disabled) cycle

! His and Seg Alarms based on Reference Date and Time
! ---------------------------------------------------
       REF_TIME(1) =     list(n)%ref_date/10000
       REF_TIME(2) = mod(list(n)%ref_date,10000)/100
       REF_TIME(3) = mod(list(n)%ref_date,100)
       REF_TIME(4) =     list(n)%ref_time/10000
       REF_TIME(5) = mod(list(n)%ref_time,10000)/100
       REF_TIME(6) = mod(list(n)%ref_time,100)

       !ALT if monthly, modify ref_time to midnight first of the month
       if (list(n)%monthly) then
          REF_TIME(3) = 1
          REF_TIME(4:6) = 0
          list(n)%ref_time = 0
          list(n)%ref_date = 10000*REF_TIME(1) + 100*REF_TIME(2) + REF_TIME(3)
       end if

       call ESMF_TimeSet( RefTime, YY = REF_TIME(1), &
                                   MM = REF_TIME(2), &
                                   DD = REF_TIME(3), &
                                   H  = REF_TIME(4), &
                                   M  = REF_TIME(5), &
                                   S  = REF_TIME(6), calendar=cal, rc=rc )

       ! ALT if monthly, set interval "Frequncy" to 1 month
       ! also in this case sec should be set to non-zero
       !ALT if monthly overwrite duration and frequency
       if (list(n)%monthly) then
          list(n)%duration = 1 !ALT simply non-zero
          sec = 1              !ALT simply non-zero
          Frequency = oneMonth
          RingTime = startOfThisMonth
       else
          sec = MAPL_nsecf( list(n)%frequency )
          call ESMF_TimeIntervalSet( Frequency, S=sec, StartTime=StartTime, _RC )
          RingTime = RefTime
       end if

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
          list(n)%his_alarm = ESMF_AlarmCreate( clock=clock, RingInterval=Frequency, RingTime=RingTime, _RC )
       else
          list(n)%his_alarm = ESMF_AlarmCreate( clock=clock, RingInterval=Frequency, RingTime=RingTime, sticky=.false., _RC )
       endif

       if( list(n)%duration.ne.0 ) then
          if (.not.list(n)%monthly) then
             sec = MAPL_nsecf( list(n)%duration )
             call ESMF_TimeIntervalSet( Frequency, S=sec, StartTime=StartTime, _RC )
          else
             Frequency = oneMonth
             !ALT keep the values from above
             ! and for debugging print
             call WRITE_PARALLEL("DEBUG: monthly averaging is active for collection "//trim(list(n)%collection))
          end if
          RingTime = RefTime
          if (RingTime < currTime) then
              RingTime = RingTime + (INT((currTime - RingTime)/frequency)+1)*frequency
          endif
          if ( list(n)%backwards ) then
             list(n)%seg_alarm = ESMF_AlarmCreate( clock=clock, RingInterval=Frequency, RingTime=RingTime, _RC )
          else
             list(n)%seg_alarm = ESMF_AlarmCreate( clock=clock, RingInterval=Frequency, RingTime=RingTime, sticky=.false., _RC )
          endif
          if (list(n)%monthly .and. (currTime == RingTime)) then
             call ESMF_AlarmRingerOn( list(n)%his_alarm,_RC )
          end if

       else
          ! this alarm should never ring, but it is checked if ringing
          list(n)%seg_alarm = ESMF_AlarmCreate( clock=clock, enabled=.false., &
               ringTime=currTime, name='historyNewSegment', _RC )
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

       call ESMF_TimeIntervalSet( Frequency, MM=1, calendar=cal, _RC )
       RingTime = RefTime
       do while ( RingTime < currTime )
          RingTime = RingTime + Frequency
       enddo
       if ( list(n)%backwards ) then
          list(n)%mon_alarm = ESMF_AlarmCreate( clock=clock, RingInterval=Frequency, RingTime=RingTime, _RC )
       else
          list(n)%mon_alarm = ESMF_AlarmCreate( clock=clock, RingInterval=Frequency, RingTime=RingTime, sticky=.false., _RC )
       endif
       if(list(n)%monthly) then
          !ALT this is temporary workaround. It has a memory leak
          ! we need to at least destroy his_alarm before assignment
          ! better yet, create it like this one in the first place
          call ESMF_AlarmDestroy(list(n)%his_alarm)
          list(n)%his_alarm = list(n)%mon_alarm
          intState%stampOffset(n) = Frequency ! we go to the beginning of the month
       end if

! End Alarm based on start_date and start_time
! ----------------------------------------
       if( list(n)%start_date.ne.MAPL_UndefInt .and. list(n)%start_time.ne.MAPL_UndefInt ) then
          REF_TIME(1) =     list(n)%start_date/10000
          REF_TIME(2) = mod(list(n)%start_date,10000)/100
          REF_TIME(3) = mod(list(n)%start_date,100)
          REF_TIME(4) =     list(n)%start_time/10000
          REF_TIME(5) = mod(list(n)%start_time,10000)/100
          REF_TIME(6) = mod(list(n)%start_time,100)

          call ESMF_TimeSet( RingTime, YY = REF_TIME(1), &
                                       MM = REF_TIME(2), &
                                       DD = REF_TIME(3), &
                                       H  = REF_TIME(4), &
                                       M  = REF_TIME(5), &
                                       S  = REF_TIME(6), calendar=cal, rc=rc )
       else
          RingTime = CurrTime
       end if
       list(n)%start_alarm = ESMF_AlarmCreate( clock=clock, RingTime=RingTime, sticky=.false., _RC )

       list(n)%skipWriting = .true.
       if (RingTime == CurrTime) then
          call  ESMF_AlarmRingerOn(list(n)%start_alarm, _RC )
          list(n)%skipWriting = .false.
       else
          if (RingTime < CurrTime .NEQV. list(n)%backwards) then
             list(n)%skipWriting = .false.
          endif
       end if


! End Alarm based on end_date and end_time
! ----------------------------------------
       if( list(n)%end_date.ne.MAPL_UndefInt .and. list(n)%end_time.ne.MAPL_UndefInt ) then
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
              list(n)%end_alarm = ESMF_AlarmCreate( clock=clock, RingTime=RingTime, _RC )
           else
              list(n)%end_alarm = ESMF_AlarmCreate( clock=clock, RingTime=RingTime, sticky=.false., _RC )
           endif
        else
           if ( list(n)%backwards ) then
              list(n)%end_alarm = ESMF_AlarmCreate( clock=clock, RingTime=CurrTime, _RC )
           else
              list(n)%end_alarm = ESMF_AlarmCreate( clock=clock, RingTime=CurrTime, sticky=.false., _RC )
           endif
           call  ESMF_AlarmRingerOff(list(n)%end_alarm, _RC )
       endif

       call ESMF_ConfigDestroy(cfg, _RC)
    enddo LISTLOOP

    if( MAPL_AM_I_ROOT() ) print *

! START OF PARSER STUFF
    size0 = 1 !size( export )
    nstatelist = 0
    allocate( statelist(size0), _STAT )
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
                allocate( tmplist (nstatelist), _STAT )
                tmplist = statelist
                nstatelist = k
                deallocate( statelist )
                allocate( statelist(nstatelist), _STAT )
                if (k > 1) statelist(1:k-1) = tmplist
                statelist(k)     = list(n)%field_set%fields(2,m)
                deallocate(   tmplist )
             endif
          !else
             !if (index(list(n)%field_set%fields(1,m),'%') /= 0) then
                !call WRITE_PARALLEL('Can not do arithmetic expression with bundle item')
                !_FAIL('needs informative message')
             !end if
          end if
       enddo
    enddo
! Get Output Export States
! ------------------------

    allocate ( exptmp(size0), _STAT )
    exptmp(1) = import
    allocate ( export(nstatelist), _STAT )
    errorFound = .false.
    allocate ( stateListAvail(nstatelist), _STAT )
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
          call MAPL_ExportStateGet ( exptmp,statelist(n),export(n),_RC )
          call ESMF_VMAllReduce(vm, sendData=status, recvData=globalStatus, &
               reduceflag=ESMF_REDUCE_MAX, rc=localStatus)

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
       allocate( list(n)%expSTATE(list(n)%field_set%nfields), _STAT )
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

    ! Important: the next modifies the field's list
    ! first we check if any regex expressions need to expanded
    !---------------------------------------------------------
    call wildCardExpand(_RC)

    do n=1,nlist
       m=list(n)%field_set%nfields
       allocate(list(n)%r4(m), list(n)%r8(m), list(n)%r8_to_r4(m), _STAT)
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

      allocate(list(n)%tmpfields(list(n)%field_set%nfields), _STAT)
      allocate(list(n)%ReWrite(list(n)%field_set%nfields), _STAT)

      list(n)%tmpfields=''
      list(n)%ReWrite= .FALSE.

      call MAPL_SetExpression(list(n)%field_set%nfields,list(n)%field_set%fields,list(n)%tmpfields,list(n)%rewrite,  &
                              list(n)%nPExtraFields, &
                              list(n)%PExtraFields, list(n)%PExtraGridComp, import,_RC)

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
    allocate( statelist(size0), _STAT )
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
             allocate( tmplist (nstatelist), _STAT )
             tmplist = statelist
             nstatelist = k
             deallocate( statelist )
             allocate( statelist(nstatelist), _STAT )
             if (k > 1) statelist(1:k-1) = tmplist
             statelist(k)     = list(n)%field_set%fields(2,m)
             deallocate(   tmplist )
          endif
       enddo
    enddo

! Get Output Export States
! ------------------------

    allocate ( exptmp (size0), _STAT )
    exptmp(1) = import
!    deallocate ( export )
    allocate ( export(nstatelist), _STAT )
    errorFound = .false.
    allocate ( stateListAvail(nstatelist), _STAT )
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

! Create a copy of the original (i.e. gridded component's export) to
! be able to modify if safely (for example by splitField)
! ------------------------------------------------------------------
    do n=1,nstatelist
       expsrc = export(n)
       call ESMF_StateGet(expsrc, name=name, _RC)
       expdst = ESMF_StateCreate(name=name, _RC)
       call CopyStateItems(src=expsrc, dst=expdst, _RC)
       export(n) = expdst
    end do

! Associate Output Names with EXPORT State Index
! ----------------------------------------------
    list(:)%subVm = .false.
    do n=1,nlist
       allocate( list(n)%expSTATE(list(n)%field_set%nfields), _STAT )
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
                call MAPL_AllocateCoupling(Field, _RC)
             end if

          end IF
       enddo
    enddo

    _ASSERT(.not. errorFound,'needs informative message')


    allocate(INTSTATE%AVERAGE    (nlist), _STAT)

    IntState%average = .false.
    do n=1, nlist
       if (list(n)%disabled) cycle
       if(list(n)%monthly) cycle
       if(list(n)%mode == "instantaneous" .or. list(n)%ForceOffsetZero) then
          sec = 0
       else if (list(n)%timeStampStart) then
          sec = MAPL_nsecf(list(n)%frequency)
       else
          sec = MAPL_nsecf(list(n)%frequency) / 2
       endif
       if (index(trim(list(n)%output_grid_label), 'SwathGrid') > 0) then
          call ESMF_TimeIntervalGet(Hsampler%Frequency_epoch, s=sec, _RC)
       end if
       if (list(n)%sampler_spec == 'station' .OR. list(n)%sampler_spec == 'mask') then
          sec = MAPL_nsecf(list(n)%frequency)
       end if
       call ESMF_TimeIntervalSet( INTSTATE%STAMPOFFSET(n), S=sec, _RC )
    end do

   nactual = npes
   if (.not. disableSubVmChecks) then
      allocate(allPes(npes), _STAT)
      minactual = npes
      do n=1, nlist
         NULLIFY(list(n)%peAve)
         if (list(n)%disabled) cycle
         localPe(1) = mype
         if (list(n)%subVm) localPe(1) = -1
         call ESMF_VMAllGather(vm, sendData=localPe, recvData=allPEs, &
              count=1, _RC)
         nactual = count(allPEs >= 0)
         minactual = min(minactual, nactual)
         allocate(list(n)%peAve(nactual), _STAT)
         list(n)%peAve = pack(allPEs, allPEs>=0)
      end do

      IntState%npes = minactual
      deallocate(allPEs)
   end if

   allocate(INTSTATE%CCS(nlist), _STAT)
   allocate(INTSTATE%GIM(nlist), _STAT)
   allocate(INTSTATE%CIM(nlist), _STAT)
   allocate(INTSTATE%SRCS(nlist), _STAT)
   allocate(INTSTATE%DSTS(nlist), _STAT)
!   allocate(INTSTATE%GEX(nlist), _STAT)
!   allocate(INTSTATE%GCNameList(nlist), _STAT)

! Initialize Logical for Grads Control File
! -----------------------------------------

   allocate( INTSTATE%LCTL(nlist), _STAT )
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
           _RC )

      select case (list(n)%mode)
      case ("instantaneous")
         IntState%average(n) = .false.
      case ("time-averaged")
         IntState%average(n) = .true.
         IntState%CIM(n) = ESMF_StateCreate ( name=trim(list(n)%filename), &
              stateIntent = ESMF_STATEINTENT_IMPORT, _RC)
         NULLIFY(INTSTATE%SRCS(n)%SPEC)
         NULLIFY(INTSTATE%DSTS(n)%SPEC)
      case default
         _FAIL("Invalid mode ["//trim(list(n)%mode)//"] for collection ["//trim(list(n)%collection)//"]. Only 'instantaneous' and 'time-averaged' are supported")
      end select

      if (associated(IntState%Regrid(n)%PTR)) then
         _ASSERT(.not. list(n)%subVm,'needs informative message') ! ALT: currently we are not supporting regridding on subVM
! query a field from export (arbitrary first field in the stream) for grid_in
         _ASSERT(size(export(list(n)%expSTATE)) > 0,'needs informative message')
         call MAPL_StateGet( export(list(n)%expSTATE(1)), &
                             trim(list(n)%field_set%fields(1,1)), field, _RC )
         IntState%Regrid(n)%PTR%state_out = ESMF_StateCreate ( name=trim(list(n)%filename)//'regrid_in', &
              stateIntent = ESMF_STATEINTENT_IMPORT, &
              _RC )

! get grid name, layout, dims
         call ESMF_FieldGet(field, grid=grid_in, _RC)
         call ESMF_GridGet(grid_in, name=gridname, distgrid=distgrid, _RC)
         call ESMF_DistGridGet(distgrid, delayout=layout, _RC)

         IntState%Regrid(n)%PTR%noxform = .false.

!        Check if is is tile variable: we could go the same grid attached to LS
!        and use T2G or go to the "other" grid in the LS. In the later case,
!        we need to find then "other LS" from the list of available LS in
!        History, and calculate Xform, then do T2T, followed by T2G


         if (gridname(1:10) == 'tile_grid_') then

            ontiles = .true.

            _ASSERT(IntState%Regrid(n)%PTR%gridname /= '','needs informative message')

!ALT:       here we are getting the address of LocStream from the TILEGRID
!           as INTEGER(KIND=INT64) attribute and we are using a C routine to
!           set the pointer to LocStream

            call ESMF_AttributeGet(grid_in, name='TILEGRID_LOCSTREAM_ADDR', &
                 value=ADDR, _RC)
            call c_MAPL_LocStreamRestorePtr(exch, ADDR)

!           Get the attached grid
            call MAPL_LocStreamGet(EXCH, ATTACHEDGRID=GRID_ATTACHED, _RC)

            call ESMF_GridGet(grid_attached, name=attachedName, _RC)

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
                  call MAPL_LocStreamGet(locStream, ATTACHEDGRID=GRID, _RC)
                  call ESMF_GridGet(grid, name=tmpstr, _RC)
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
                  call MAPL_LocStreamGet(exch, GRIDNAMES = GNAMES, _RC)

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
                     call MAPL_LocStreamGet(locStream, ATTACHEDGRID=GRID, _RC)
                     call ESMF_GridGet(grid, name=tmpstr, _RC)
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
                       _RC )

                  ! get the name and layout of attached grid
                  call ESMF_GridGet(grid_in, name=gridname, distgrid=distgrid, _RC)
                  call ESMF_DistGridGet(distgrid, delayout=layout, _RC)

                  call MAPL_LocStreamCreate(IntState%Regrid(n)%PTR%locIn, &
                       layout, FILENAME=IntState%Regrid(n)%PTR%TILEFILE, &
                       NAME='history_in', MASK=(/MAPL_Ocean/), grid=grid_in, _RC)
               end if

            end if

         else
!           this is G2G done as G2T followed by T2T and then T2G
            IntState%Regrid(n)%PTR%regridType = MAPL_G2G
            _ASSERT(IntState%Regrid(n)%PTR%tilefile /= '','needs informative message')

            ontiles = .false.

            call MAPL_LocStreamCreate(IntState%Regrid(n)%PTR%locIn, &
                 layout, FILENAME=IntState%Regrid(n)%PTR%TILEFILE, &
                 NAME='history_in', MASK=(/MAPL_Ocean/), grid=grid_in, _RC)

         end if

         IntState%Regrid(n)%PTR%ontiles = ontiles

         if (.not. ontiles) then
!           get gridnames from loc_in
            call MAPL_LocStreamGet(IntState%Regrid(n)%PTR%locIn, &
                 GRIDNAMES = GNAMES, _RC)
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
                 NAME='history_out', MASK=(/MAPL_Ocean/), Grid=grid_out, _RC)

         endif

! query ntiles
         call MAPL_LocStreamGet(IntState%Regrid(n)%PTR%locOut, &
              NT_LOCAL = IntState%Regrid(n)%PTR%ntiles_out, _RC)

         if (.not.INTSTATE%Regrid(n)%PTR%noxform) then
! query ntiles
            call MAPL_LocStreamGet(IntState%Regrid(n)%PTR%locIn, &
                 NT_LOCAL = IntState%Regrid(n)%PTR%ntiles_in, _RC)

! create XFORM
            call MAPL_LocStreamCreateXform ( XFORM=INTSTATE%Regrid(n)%PTR%XFORM, &
                 LocStreamOut=INTSTATE%Regrid(n)%PTR%LocOut, &
                 LocStreamIn=INTSTATE%Regrid(n)%PTR%LocIn, &
                 NAME='historyXFORM', &
                 UseFCollect=.true., &
                 _RC )
         end if

      endif

! Handle possible extra fields needed for the parser
      if (list(n)%nPExtraFields > 0) then

         allocate ( exptmp (1), _STAT )
         exptmp(1) = import

         do m=1,list(n)%nPExtraFields
            call MAPL_ExportStateGet(exptmp,list(n)%PExtraGridComp(m),parser_state,_RC)
            call MAPL_StateGet(parser_state,list(n)%PExtraFields(m),parser_field,_RC)
            call MAPL_AllocateCoupling(parser_field, _RC)
            f_extra = MAPL_FieldCreate(parser_field, name=list(n)%PExtraFields(m), _RC)
            if (IntState%average(n)) then
               call MAPL_StateAdd(IntState%CIM(N), f_extra, _RC)
            else
               call MAPL_StateAdd(IntState%GIM(N), f_extra, _RC)
            end if
         end do

         deallocate(exptmp)

      end if

      block
        type (ESMF_Field), pointer :: splitFields(:)
        logical :: split
        character(ESMF_MAXSTR) :: field_name, alias_name, special_name
        integer :: m1, big, szf, szr
        integer :: lungrd, trueUngridDims
        logical, allocatable               :: tmp_r8_to_r4(:)
        type(ESMF_FIELD), allocatable      :: tmp_r8(:)
        type(ESMF_FIELD), allocatable      :: tmp_r4(:)

      m1 = 0
      do m=1,list(n)%field_set%nfields
         field_name = list(n)%field_set%fields(1,m)
         alias_name = list(n)%field_set%fields(3,m)
         special_name = list(n)%field_set%fields(4,m)

         call MAPL_StateGet( export(list(n)%expSTATE(m)), &
                             trim(field_name), field, _RC )

         if (list(n)%splitField) then
            split = hasSplitField(field, _RC)
         else
            split = .false.
         end if
         ! check if split is needed
         if (.not. split) then
            allocate(splitFields(1), _STAT)
            splitFields(1) = field
         else
            call MAPL_FieldSplit(field, splitFields, aliasName=alias_name, _RC)
         endif

         szf = size(splitFields)
         big = m1 + szf
         szr = size(list(n)%r4)
         if (big > szr) then
            ! grow
            allocate(tmp_r4(big), tmp_r8(big), tmp_r8_to_r4(big), _STAT)
            tmp_r4(1:szr) = list(n)%r4
            tmp_r8(1:szr) = list(n)%r8
            tmp_r8_to_r4(1:szr) = list(n)%r8_to_r4
            call move_alloc(tmp_r4, list(n)%r4)
            call move_alloc(tmp_r8, list(n)%r8)
            call move_alloc(tmp_r8_to_r4, list(n)%r8_to_r4)
         end if
         do j=1,szf
            m1 = m1 + 1
            field = splitFields(j)
            ! reset alias name when split
            if (split) then
               call ESMF_FieldGet(field, name=alias_name, _RC)
            end if
            call ESMF_FieldGet(FIELD, typekind=tk, _RC)
            if (tk == ESMF_TypeKind_R8) then
               list(n)%r8_to_r4(m1) = .true.
               list(n)%r8(m1) = field
               ! Create a new field with R4 precision
               r4field = MAPL_FieldCreate(field,_RC)
               field=r4field
               list(n)%r4(m1) = field
            else
               list(n)%r8_to_r4(m1) = .false.
            end if

            if (.not.list(n)%rewrite(m) .or.special_name /= BLANK ) then
               f_extra = MAPL_FieldCreate(field, name=alias_name, _RC)
            else
               DoCopy=.True.
               f_extra = MAPL_FieldCreate(field, name=alias_name, DoCopy=DoCopy, _RC)
            endif
            if (special_name /= BLANK) then
               if (special_name == 'MIN') then
                  call ESMF_AttributeSet(f_extra, NAME='CPLFUNC', VALUE=MAPL_CplMin, _RC)
               else if (special_name == 'MAX') then
                  call ESMF_AttributeSet(f_extra, NAME='CPLFUNC', VALUE=MAPL_CplMax, _RC)
               else if (special_name == 'ACCUMULATE') then
                  call ESMF_AttributeSet(f_extra, NAME='CPLFUNC', VALUE=MAPL_CplAccumulate, _RC)
               else
                  call WRITE_PARALLEL("Functionality not supported yet")
               end if
            end if

            if (IntState%average(n)) then
               call MAPL_StateAdd(IntState%CIM(N), f_extra, _RC)

               ! borrow SPEC from FIELD
               ! modify SPEC to reflect accum/avg
               call ESMF_FieldGet(f_extra, name=short_name, grid=grid, _RC)

               call ESMF_AttributeGet(FIELD, NAME='DIMS', VALUE=DIMS, _RC)
               call ESMF_AttributeGet(FIELD, NAME='VLOCATION', VALUE=VLOCATION, _RC)
               call ESMF_AttributeGet(FIELD, NAME='LONG_NAME', VALUE=LONG_NAME, _RC)
               call ESMF_AttributeGet(FIELD, NAME='UNITS', VALUE=UNITS, _RC)
               call ESMF_AttributeGet(FIELD, NAME='FIELD_TYPE', VALUE=FIELD_TYPE, _RC)

               call ESMF_AttributeGet(FIELD, NAME='REFRESH_INTERVAL', VALUE=REFRESH, _RC)
               call ESMF_AttributeGet(FIELD, NAME='AVERAGING_INTERVAL', VALUE=avgint, _RC)

               call ESMF_FieldGet(FIELD, dimCount=fieldRank, _RC)
               call ESMF_GridGet(GRID, dimCount=gridRank, _RC)
               allocate(gridToFieldMap(gridRank), _STAT)
               call ESMF_FieldGet(FIELD, gridToFieldMap=gridToFieldMap, _RC)

               notGridded = count(gridToFieldMap==0)
               unGridDims = fieldRank - gridRank + notGridded
               trueUnGridDims = unGridDims

               if (unGridDims > 0) then
                  !ALT: special handling for 2d-MAPL grid (the vertical is treated as ungridded)
                  lungrd = 1
                  if ((gridRank == 2) .and. (DIMS == MAPL_DimsHorzVert)) then
                     trueUnGridDims = trueUnGridDims - 1
                     lungrd = 2
                  end if
               endif
               hasUngridDims = .false.
               if (trueUnGridDims > 0) hasUngridDims = .true.

               if (hasUngridDims) then
                  allocate(ungriddedLBound(unGridDims), &
                       ungriddedUBound(unGridDims), &
                       ungrd(trueUnGridDims),           &
                       _STAT)

                  call ESMF_FieldGet(field, Array=array, _RC)

                  call ESMF_ArrayGet(array, rank=rank, dimCount=dimCount, _RC)
                  undist = rank-dimCount
                  _ASSERT(undist == ungridDims,'needs informative message')

                  call ESMF_ArrayGet(array, undistLBound=ungriddedLBound, &
                       undistUBound=ungriddedUBound, _RC)

                  ungrd = ungriddedUBound(lungrd:) - ungriddedLBound(lungrd:) + 1
                  call ESMF_AttributeGet(field,name="UNGRIDDED_UNIT",value=ungridded_unit,_RC)
                  call ESMF_AttributeGet(field,name="UNGRIDDED_NAME",value=ungridded_name,_RC)
                  call ESMF_AttributeGet(field,name="UNGRIDDED_COORDS",isPresent=isPresent,_RC)
                  if (isPresent) then
                     call ESMF_AttributeGet(field,name="UNGRIDDED_COORDS",itemcount=ungrdsize,_RC)
                     if ( ungrdsize /= 0 ) then
                        allocate(ungridded_coord(ungrdsize),_STAT)
                        call ESMF_AttributeGet(field,NAME="UNGRIDDED_COORDS",valuelist=ungridded_coord,_RC)
                     end if
                  else
                     ungrdsize = 0
                  end if

                  deallocate(ungriddedLBound,ungriddedUBound)

                  if (ungrdsize > 0) then
                     call MAPL_VarSpecCreateInList(INTSTATE%SRCS(n)%SPEC,   &
                          SHORT_NAME = SHORT_NAME,                          &
                          LONG_NAME  = LONG_NAME,                           &
                          UNITS      = UNITS,                               &
                          DIMS       = DIMS,                                &
                          UNGRIDDED_DIMS = UNGRD,                           &
                          UNGRIDDED_NAME = ungridded_name,                  &
                          UNGRIDDED_UNIT = ungridded_unit,                  &
                          UNGRIDDED_COORDS = ungridded_coord,               &
                          ACCMLT_INTERVAL= avgint,                          &
                          COUPLE_INTERVAL= REFRESH,                         &
                          VLOCATION  = VLOCATION,                           &
                          FIELD_TYPE = FIELD_TYPE,                          &
                          _RC)

                     call MAPL_VarSpecCreateInList(INTSTATE%DSTS(n)%SPEC,   &
                          SHORT_NAME = alias_name,                          &
                          LONG_NAME  = LONG_NAME,                           &
                          UNITS      = UNITS,                               &
                          DIMS       = DIMS,                                &
                          UNGRIDDED_DIMS = UNGRD,                           &
                          UNGRIDDED_NAME = ungridded_name,                  &
                          UNGRIDDED_UNIT = ungridded_unit,                  &
                          UNGRIDDED_COORDS = ungridded_coord,               &
                          ACCMLT_INTERVAL= MAPL_nsecf(list(n)%acc_interval),&
                          COUPLE_INTERVAL= MAPL_nsecf(list(n)%frequency   ),&
                          offset = list(n)%acc_offset, &
                          VLOCATION  = VLOCATION,                           &
                          GRID       = GRID,                                &
                          FIELD_TYPE = FIELD_TYPE,                          &
                          _RC)
                  else

                     call MAPL_VarSpecCreateInList(INTSTATE%SRCS(n)%SPEC,   &
                          SHORT_NAME = SHORT_NAME,                          &
                          LONG_NAME  = LONG_NAME,                           &
                          UNITS      = UNITS,                               &
                          DIMS       = DIMS,                                &
                          UNGRIDDED_DIMS = UNGRD,                           &
                          UNGRIDDED_NAME = ungridded_name,                  &
                          UNGRIDDED_UNIT = ungridded_unit,                  &
                          ACCMLT_INTERVAL= avgint,                          &
                          COUPLE_INTERVAL= REFRESH,                         &
                          VLOCATION  = VLOCATION,                           &
                          FIELD_TYPE = FIELD_TYPE,                          &
                          _RC)

                     call MAPL_VarSpecCreateInList(INTSTATE%DSTS(n)%SPEC,   &
                          SHORT_NAME = alias_name,                          &
                          LONG_NAME  = LONG_NAME,                           &
                          UNITS      = UNITS,                               &
                          DIMS       = DIMS,                                &
                          UNGRIDDED_DIMS = UNGRD,                           &
                          UNGRIDDED_NAME = ungridded_name,                  &
                          UNGRIDDED_UNIT = ungridded_unit,                  &
                          ACCMLT_INTERVAL= MAPL_nsecf(list(n)%acc_interval),&
                          COUPLE_INTERVAL= MAPL_nsecf(list(n)%frequency   ),&
                          offset = list(n)%acc_offset, &
                          VLOCATION  = VLOCATION,                           &
                          GRID       = GRID,                                &
                          FIELD_TYPE = FIELD_TYPE,                          &
                          _RC)
                  end if
                  deallocate(ungrd)
                  if (allocated(ungridded_coord)) deallocate(ungridded_coord)

               else
                  call MAPL_VarSpecCreateInList(INTSTATE%SRCS(n)%SPEC,     &
                       SHORT_NAME = SHORT_NAME,                            &
                       LONG_NAME  = LONG_NAME,                             &
                       UNITS      = UNITS,                                 &
                       DIMS       = DIMS,                                  &
                       ACCMLT_INTERVAL= avgint,                            &
                       COUPLE_INTERVAL= REFRESH,                           &
                       VLOCATION  = VLOCATION,                             &
                       FIELD_TYPE = FIELD_TYPE,                            &
                       _RC)

                  call MAPL_VarSpecCreateInList(INTSTATE%DSTS(n)%SPEC,     &
                       SHORT_NAME = alias_name,                            &
                       LONG_NAME  = LONG_NAME,                             &
                       UNITS      = UNITS,                                 &
                       DIMS       = DIMS,                                  &
                       ACCMLT_INTERVAL= MAPL_nsecf(list(n)%acc_interval),  &
                       COUPLE_INTERVAL= MAPL_nsecf(list(n)%frequency   ),  &
                       offset = list(n)%acc_offset, &
                       VLOCATION  = VLOCATION,                             &
                       GRID       = GRID,                                  &
                       FIELD_TYPE = FIELD_TYPE,                            &
                       _RC)

               endif ! has_ungrid
               deallocate(gridToFieldMap)

            else ! else for if averaged

               REFRESH = MAPL_nsecf(list(n)%acc_interval)
               AVGINT  = MAPL_nsecf( list(n)%frequency )
               call ESMF_AttributeSet(F_extra, NAME='REFRESH_INTERVAL', VALUE=REFRESH, _RC)
               call ESMF_AttributeSet(F_extra, NAME='AVERAGING_INTERVAL', VALUE=AVGINT, _RC)
               call MAPL_StateAdd(IntState%GIM(N), f_extra, _RC)

            endif

            ! Handle possible regridding through user supplied exchange grid
            !---------------------------------------------------------------
            if (associated(IntState%Regrid(n)%PTR)) then
               ! replace field with newly created fld on grid_out
               field = MAPL_FieldCreate(f_extra, grid_out, _RC)
               ! add field to state_out
               call MAPL_StateAdd(IntState%Regrid(N)%PTR%state_out, &
                    field, _RC)
            endif
         end do ! j-loop
         if (split) then
            do j=1,szf
               call ESMF_FieldDestroy(splitFields(j), _RC)
            end do
         end if
         deallocate(splitFields)
      end do ! m-loop
      end block

      ! reset list(n)%field_set and list(n)%items, if split
      !----------------------------------------------------
      call splitUngriddedFields(_RC)

   end do

   do n=1, nlist
      if (list(n)%disabled) cycle
      if (IntState%average(n)) then

         call MAPL_StateCreateFromSpec(IntState%GIM(n), &
              IntState%DSTS(n)%SPEC,   &
              _RC  )

!         create CC
         if (nactual == npes) then
            IntState%CCS(n) = ESMF_CplCompCreate (                  &
                 NAME       = list(n)%collection, &
                 contextFlag = ESMF_CONTEXT_PARENT_VM,              &
                 _RC )
         else
            IntState%CCS(n) = ESMF_CplCompCreate (                  &
                 NAME       = list(n)%collection, &
                 petList    = list(n)%peAve, &
                 contextFlag = ESMF_CONTEXT_OWN_VM,              &
                 _RC )
         end if

!         CCSetServ
         call ESMF_CplCompSetServices (IntState%CCS(n), &
                                       GenericCplSetServices, _RC )

         call MAPL_CplCompSetVarSpecs(IntState%CCS(n), &
                                      INTSTATE%SRCS(n)%SPEC,&
                                      INTSTATE%DSTS(n)%SPEC,_RC)

         if (list(n)%monthly) then
            call MAPL_CplCompSetAlarm(IntState%CCS(n), &
                 list(n)%his_alarm, _RC)
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

          list(n)%bundle = ESMF_FieldBundleCreate(NAME=string, _RC)

          if(associated(list(n)%levels)) then
             LM = size(list(n)%levels)
          else
             call ESMF_StateGet(INTSTATE%GIM(n), &
                  trim(list(n)%field_set%fields(3,1)), field, _RC )
             call ESMF_FieldGet(field, grid=grid,   _RC )
             call MAPL_GridGet(GRID, globalCellCountPerDim=COUNTS, _RC)
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
                  trim(list(n)%field_set%fields(3,m)), field, _RC )

             call MAPL_FieldBundleAdd( list(n)%bundle, field, _RC )

             call ESMF_FieldGet(field, Array=array, grid=bgrid, _RC)
             call ESMF_ArrayGet(array, rank=rank, _RC)
             call ESMF_ArrayGet(array, localarrayList=larrayList, _RC)
             larray => lArrayList(1) ! alias
             call ESMF_GridGet(bgrid, distgrid=bdistgrid, _RC)
             !ALT: we need the rank of the distributed grid
             ! MAPL (and GEOS-5) grid are distributed along X-Y
             ! tilegrids are distributed only along "tile" dimension
             call ESMF_DistGridGet(bdistgrid, dimCount=distRank, _RC)
             call ESMF_LocalArrayGet(larray, totalCount=counts, _RC)

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
       string = trim( list(n)%collection ) // '.'
       cfg = ESMF_ConfigCreate(_RC)
       call ESMF_ConfigLoadFile(cfg, filename = trim(string)//'rcx', _RC)
       if (list(n)%format == 'CFIOasync') then
          list(n)%format = 'CFIO'
          if (mapl_am_i_root()) write(*,*)'Chose CFIOasync setting to CFIO, update your History.rc file'
       end if
       if (list(n)%format == 'CFIO') then
          call Get_Tdim (list(n), clock, tm)
          if (associated(list(n)%levels) .and. list(n)%vvars(1) /= "") then
             list(n)%vdata = VerticalData(levels=list(n)%levels,vcoord=list(n)%vvars(1),vscale=list(n)%vscale,vunit=list(n)%vunit,_RC)
          else if (associated(list(n)%levels) .and. list(n)%vvars(1) == "") then
             list(n)%vdata = VerticalData(levels=list(n)%levels,_RC)
          else
             list(n)%vdata = VerticalData(positive=list(n)%positive,_RC)
          end if
          if (index(trim(list(n)%output_grid_label), 'SwathGrid') > 0) then
             call list(n)%xsampler%set_param(deflation=list(n)%deflate,_RC)
             call list(n)%xsampler%set_param(quantize_algorithm=list(n)%quantize_algorithm,_RC)
             call list(n)%xsampler%set_param(quantize_level=list(n)%quantize_level,_RC)
             call list(n)%xsampler%set_param(zstandard_level=list(n)%zstandard_level,_RC)
             call list(n)%xsampler%set_param(chunking=list(n)%chunkSize,_RC)
             call list(n)%xsampler%set_param(nbits_to_keep=list(n)%nbits_to_keep,_RC)
             call list(n)%xsampler%set_param(regrid_method=list(n)%regrid_method,_RC)
             call list(n)%xsampler%set_param(itemOrder=intState%fileOrderAlphabetical,_RC)
             call Hsampler%verify_epoch_equals_freq (list(n)%frequency, list(n)%output_grid_label, _RC)
          endif

          call list(n)%mGriddedIO%set_param(deflation=list(n)%deflate,_RC)
          call list(n)%mGriddedIO%set_param(quantize_algorithm=list(n)%quantize_algorithm,_RC)
          call list(n)%mGriddedIO%set_param(quantize_level=list(n)%quantize_level,_RC)
          call list(n)%mGriddedIO%set_param(zstandard_level=list(n)%zstandard_level,_RC)
          call list(n)%mGriddedIO%set_param(chunking=list(n)%chunkSize,_RC)
          call list(n)%mGriddedIO%set_param(nbits_to_keep=list(n)%nbits_to_keep,_RC)
          call list(n)%mGriddedIO%set_param(regrid_method=list(n)%regrid_method,_RC)
          call list(n)%mGriddedIO%set_param(itemOrder=intState%fileOrderAlphabetical,_RC)
          if (intState%file_weights) then
             regrid_hints = 0
             regrid_hints = IOR(regrid_hints,REGRID_HINT_FILE_WEIGHTS)
             call list(n)%mGriddedIO%set_param(regrid_hints=regrid_hints,_RC)
          end if

          if (list(n)%monthly) then
             nextMonth = currTime - oneMonth
             dur = nextMonth - currTime
             call ESMF_TimeIntervalGet(dur, s=sec, _RC)
             list(n)%timeInfo = TimeData(clock,tm,sec,IntState%stampoffset(n),funits='days')
          else
             list(n)%timeInfo = TimeData(clock,tm,MAPL_nsecf(list(n)%frequency),IntState%stampoffset(n),integer_time=intstate%integer_time)
          end if
          if (list(n)%timeseries_output) then
             list(n)%trajectory = HistoryTrajectory(cfg,string,clock,genstate=GENSTATE,_RC)
             call list(n)%trajectory%initialize(items=list(n)%items,bundle=list(n)%bundle,timeinfo=list(n)%timeInfo,vdata=list(n)%vdata,_RC)
             IntState%stampoffset(n) = list(n)%trajectory%epoch_frequency
          elseif (list(n)%sampler_spec == 'mask') then
             call MAPL_TimerOn(GENSTATE,"mask_init")
             global_attributes = list(n)%global_atts%define_collection_attributes(_RC)
             list(n)%mask_sampler = MaskSampler(cfg,string,clock,genstate=GENSTATE,_RC)
             ! initialize : create grid / metadata
             call list(n)%mask_sampler%set_param(oClients=o_Clients)
             call list(n)%mask_sampler%set_param(itemOrder=intState%fileOrderAlphabetical,_RC)
             call list(n)%mask_sampler%initialize(list(n)%duration,list(n)%frequency,items=list(n)%items,&
                  bundle=list(n)%bundle,timeinfo=list(n)%timeInfo,vdata=list(n)%vdata,global_attributes=global_attributes,_RC)

             collection_id = o_Clients%add_hist_collection(list(n)%mask_sampler%metadata, mode = create_mode)
             call list(n)%mask_sampler%set_param(write_collection_id=collection_id)
             call MAPL_TimerOff(GENSTATE,"mask_init")
          elseif (list(n)%sampler_spec == 'station') then
             list(n)%station_sampler = StationSampler (list(n)%bundle, trim(list(n)%stationIdFile), nskip_line=list(n)%stationSkipLine, genstate=GENSTATE, _RC)
             call list(n)%station_sampler%add_metadata_route_handle(items=list(n)%items,bundle=list(n)%bundle,timeinfo=list(n)%timeInfo,vdata=list(n)%vdata,_RC)
          else
             global_attributes = list(n)%global_atts%define_collection_attributes(_RC)
             if (index(trim(list(n)%output_grid_label), 'SwathGrid') > 0) then
                pgrid => IntState%output_grids%at(trim(list(n)%output_grid_label))
                call list(n)%xsampler%Create_bundle_RH(list(n)%items,list(n)%bundle,Hsampler%tunit,ogrid=pgrid,vdata=list(n)%vdata,_RC)
             else
                if (trim(list(n)%output_grid_label)/='') then
                   pgrid => IntState%output_grids%at(trim(list(n)%output_grid_label))
                   call list(n)%mGriddedIO%CreateFileMetaData(list(n)%items,list(n)%bundle,list(n)%timeInfo,ogrid=pgrid,vdata=list(n)%vdata,global_attributes=global_attributes,_RC)
                else
                   call list(n)%mGriddedIO%CreateFileMetaData(list(n)%items,list(n)%bundle,list(n)%timeInfo,vdata=list(n)%vdata,global_attributes=global_attributes,_RC)
                end if
                collection_id = o_Clients%add_hist_collection(list(n)%mGriddedIO%metadata, mode = create_mode)
                call list(n)%mGriddedIO%set_param(write_collection_id=collection_id)
             endif
          end if
       end if
       call ESMF_ConfigDestroy(cfg, _RC)
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
         if (list(n)%nbits_to_keep < MAPL_NBITS_UPPER_LIMIT) then
            print *, '       Nbits: ',       list(n)%nbits_to_keep
         end if
         print *, '      Slices: ',       list(n)%Slices
         print *, '     Deflate: ',       list(n)%deflate
         if (list(n)%quantize_algorithm > 0) then
            print *, 'Quantize Alg: ',       trim(list(n)%quantize_algorithm_string)
            print *, 'Quantize Lvl: ',       list(n)%quantize_level
         end if
         if (list(n)%zstandard_level > 0) then
            print *, 'Zstandard Lvl: ',       list(n)%zstandard_level
         end if
         if (associated(list(n)%chunksize)) then
            print *, '   ChunkSize: ',       list(n)%chunksize
         end if
         if (list(n)%monthly) then
            print *, '   Frequency: ',       'monthly'
         else
            print *, '   Frequency: ',       list(n)%frequency
         end if
         if(IntState%average(n) .and. .not. list(n)%monthly) &
              print *, 'Acc_Interval: ',  list(n)%acc_interval
         print *, '    Ref_Date: ',       list(n)%ref_date
         print *, '    Ref_Time: ',       list(n)%ref_time
         if (list(n)%monthly) then
            print *, '    Duration: ',       'one month'
         else
            print *, '    Duration: ',       list(n)%duration
         end if
         if( list(n)%start_date.ne.MAPL_UndefInt ) then
            print *, '    Start_Date: ',       list(n)%start_date
            print *, '    Start_Time: ',       list(n)%start_time
         endif
         if( list(n)%end_date.ne.MAPL_UndefInt ) then
            print *, '    End_Date: ',       list(n)%end_date
            print *, '    End_Time: ',       list(n)%end_time
         endif
         if (trim(list(n)%output_grid_label)/='') then
            print *, ' Regrid Mthd: ',       regrid_method_int_to_string(list(n)%regrid_method)
         else
            print *, ' Regrid Mthd: ',       'identity'
         end if


         block
            integer :: im_world, jm_world,dims(3)
            pgrid => IntState%output_grids%at(trim(list(n)%output_grid_label))
            if (associated(pgrid)) then
               call MAPL_GridGet(pgrid,globalCellCountPerDim=dims,_RC)
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
                _FAIL('needs informative message')
         end select

         !print *, '      Fields: ',((trim(list(n)%field_set%fields(3,m)),' '),m=1,list(n)%field_set%nfields)
         write (*,'(A)',ADVANCE='NO') '      Fields: '
         do m=1,list(n)%field_set%nfields
            if( trim(list(n)%field_set%fields(3,m)).ne.BLANK ) then
               write (*,'(A,1X)',ADVANCE='NO') trim(list(n)%field_set%fields(3,m))
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

    deallocate(stateListAvail)
    deallocate( statelist )

    call MAPL_GenericInitialize( gc, import, dumexport, clock, _RC )

    _RETURN(ESMF_SUCCESS)

  contains

    subroutine wildCardExpand(rc)
      integer, optional, intent(out) :: rc

      ! local vars
      integer :: status

      integer, pointer :: newExpState(:) => null()
      type(GriddedIOitemVectorIterator) :: iter
      type(GriddedIOitem), pointer :: item
      integer :: nfields
      integer :: nregex
      character(len=ESMF_MAXSTR), allocatable :: fieldNames(:)
      type(ESMF_State) :: expState
      type(GriddedIOItemVector), pointer  :: newItems
      character(ESMF_MAXSTR) :: fldName, stateName
      logical :: expand
      integer :: k, i
      integer :: n

      ! Restrictions:
      ! 1) we do not do wildcard expansion for vectors
      ! 2) no use of aliases for wildcard-expanded-field name base
      do n = 1, nlist
         if (.not.list(n)%regex) cycle
         fld_set => list(n)%field_set
         nfields = fld_set%nfields

         allocate(needSplit(nfields), regexList(nfields), _STAT)
         regexList = ""

         allocate(newItems, _STAT)

         needSplit = .false.

         iter = list(n)%items%begin()
         m = 0 ! m is the "old" field-index
         do while(iter /= list(n)%items%end())
            item => iter%get()
            if (item%itemType == ItemTypeScalar) then
               expand = hasRegex(fldName=item%xname, _RC)
               if (.not.expand) call newItems%push_back(item)
            else if (item%itemType == ItemTypeVector) then
               ! Lets' not allow regex expand for vectors
               expand = hasRegex(fldName=item%xname, _RC)
               expand = expand.or.hasRegex(fldName=item%yname, _RC)
               if (.not.expand) call newItems%push_back(item)
            end if

            call iter%next()
         end do

         ! re-pack field_set
         nregex = count(needSplit)

         if (nregex /= 0) then
            nfields = nfields - nregex
            allocate(newExpState(nfields), _STAT)
            allocate(newFieldSet, _STAT)
            allocate(fields(4,nfields), _STAT)
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
                    fieldNames=fieldNames, _RC)

               do i=1,size(fieldNames)
                  fldName = fieldNames(i)
                  call appendFieldSet(newFieldSet, fldName, &
                       stateName=stateName, &
                       aliasName=fldName, &
                       specialName='', _RC)

                  ! append expState
                  call appendArray(newExpState,idx=list(n)%expState(k),_RC)

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

      call ESMF_StateGet(state, itemcount=nitems,  _RC)

      allocate(itemNameList(nitems), itemtypeList(nitems), _STAT)

      call ESMF_StateGet(state,itemNameList=itemNameList,&
                       itemTypeList=itemTypeList,_RC)
      call regcomp(regex,trim(regexStr),'xmi',status=status)

      if (.not.allocated(fieldNames)) then
         allocate(fieldNames(0), _STAT)
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
            allocate(tmpFldNames(count), _STAT)
            tmpFldNames(1:count-1) = fieldNames
            call move_alloc(tmpFldNames, fieldNames)

            fieldNames(count) = itemNameList(i)
         end if

      end do

      call regfree(regex)
      deallocate(itemNameList, itemtypeList)

      _RETURN(ESMF_SUCCESS)
    end subroutine MAPL_WildCardExpand

    subroutine splitUngriddedFields(rc)
      integer, optional, intent(out) :: rc

      ! local vars
      integer :: status

      integer, pointer :: newExpState(:) => null()
      type(GriddedIOitemVectorIterator) :: iter
      type(GriddedIOitem), pointer :: item
      integer :: nfields
      integer :: nsplit
      type(ESMF_Field), pointer :: splitFields(:) => null()
      type(ESMF_State) :: expState
      type(GriddedIOItemVector), pointer  :: newItems
      character(ESMF_MAXSTR) :: fldName, stateName
      character(ESMF_MAXSTR) :: aliasName, alias
      logical :: split
      integer :: k, i, idx
      logical :: hasField

      ! Restrictions:
      ! 1) we do not split vectors
!@@      do n = 1, nlist
      if (.not.list(n)%splitField) then
         _RETURN(ESMF_SUCCESS)
      end if
      fld_set => list(n)%field_set
      nfields = fld_set%nfields
      allocate(needSplit(nfields), fldList(nfields), _STAT)

      allocate(newItems, _STAT)

      needSplit = .false.

      iter = list(n)%items%begin()
      m = 0 ! m is the "old" field-index
      do while(iter /= list(n)%items%end())
         split = .false.
         item => iter%get()
         if (item%itemType == ItemTypeScalar) then
            split = hasSplitableField(fldName=item%xname, _RC)
            if (.not.split) call newItems%push_back(item)
         else if (item%itemType == ItemTypeVector) then
            ! Lets' not allow field split for vectors (at least for now);
            ! it is easy to implement; just tedious

            split = hasSplitableField(fldName=item%xname, _RC)
            split = split.or.hasSplitableField(fldName=item%yname, _RC)
            if (.not.split) call newItems%push_back(item)

            _ASSERT(.not. split, 'split field vectors of not allowed yet')

         end if

         needSplit(m) = split
         call iter%next()
      end do

      ! re-pack field_set
      nsplit = count(needSplit)

      if (nsplit /= 0) then
         nfields = nfields - nsplit
         allocate(newExpState(nfields), _STAT)

         allocate(newFieldSet, _STAT)
         allocate(fields(4,nfields), _STAT)
         do k = 1, size(fld_set%fields,1) ! 4
            fields(k,:) = pack(fld_set%fields(k,:), mask=.not.needSplit)
         end do
         newFieldSet%fields => fields
         newFieldSet%nfields = nfields

         newExpState = pack(list(n)%expState, mask=.not.needSplit)

         ! split and add the splitted fields to the list

         do k = 1, size(needSplit) ! loop over "old" fld_set
            if (.not. needSplit(k)) cycle

            stateName = fld_set%fields(2,k)
            aliasName = fld_set%fields(3,k)

            call MAPL_FieldSplit(fldList(k), splitFields, aliasName=aliasName, _RC)

            expState = export(list(n)%expSTATE(k))

            do i=1,size(splitFields)
               call ESMF_FieldGet(splitFields(i), name=fldName, &
                    _RC)

               alias = fldName

               call appendFieldSet(newFieldSet, fldName, &
                    stateName=stateName, &
                    aliasName=alias, &
                    specialName='', _RC)

               ! append expState
               call appendArray(newExpState,idx=list(n)%expState(k),_RC)

               item%itemType = ItemTypeScalar
               item%xname = trim(alias)
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

      _RETURN(ESMF_SUCCESS)
    end subroutine splitUngriddedFields

    function hasSplitableField(fldName, rc) result(okToSplit)
      logical :: okToSplit
      character(len=*),  intent(in)   :: fldName
      integer, optional, intent(out) :: rc

      ! local vars
      integer :: k
      integer :: status
      type(ESMF_State) :: exp_state
      type(ESMF_Field) :: fld
      character(ESMF_MAXSTR) :: baseName

      ! and these vars are declared in the caller
      ! fld_set
      ! m

      okToSplit = .false.

      m = m + 1
      _ASSERT(fldName == fld_set%fields(3,m), 'Incorrect order') ! we got "m" right

      baseName = fld_set%fields(1,m)
      k = list(n)%expSTATE(m)
      exp_state = export(k)

      call MAPL_StateGet(exp_state,baseName,fld,_RC)

      okToSplit = hasSplitField(fld, _RC)

      if (okToSplit) then
         fldList(m) = fld
      end if
      needSplit(m) = okToSplit

      _RETURN(ESMF_SUCCESS)
    end function hasSplitableField

    function hasSplitField(fld, rc) result(okToSplit)
      logical :: okToSplit
      type(ESMF_Field),  intent(inout)   :: fld
      integer, optional, intent(out) :: rc

      ! local vars
      integer :: fldRank
      integer :: dims
      integer :: status
      logical :: has_ungrd
      type(ESMF_FieldStatus_Flag) :: fieldStatus

      ! and these vars are declared in the caller
      ! fld_set
      ! m

      okToSplit = .false.
      fldRank = 0

      call ESMF_FieldGet(fld, status=fieldStatus, _RC)

      if (fieldStatus /= ESMF_FIELDSTATUS_COMPLETE) then
         call MAPL_AllocateCoupling(fld, _RC)
      end if

      call ESMF_FieldGet(fld,dimCount=fldRank,_RC)

      _ASSERT(fldRank < 5, "unsupported rank")

      if (fldRank == 4) then
         okToSplit = .true.
      else if (fldRank == 3) then
         ! split ONLY if X and Y are "gridded" and Z is "ungridded"
         call ESMF_AttributeGet(fld, name='DIMS', value=dims, _RC)
        if (dims == MAPL_DimsHorzOnly) then
           call ESMF_AttributeGet(fld, name='UNGRIDDED_DIMS', &
                isPresent=has_ungrd, _RC)
            if (has_ungrd) then
               okToSplit = .true.
            end if
         end if
      end if

      _RETURN(ESMF_SUCCESS)

    end function hasSplitField

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
     allocate(tmp(n), _STAT)
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
     allocate(flds(mm,nn), _STAT)
     flds(:,1:k) = fldset%fields
     flds(1,nn) = fldName
     flds(2,nn) = stateName
     flds(3,nn) = aliasName
     flds(4,nn) = specialName

     deallocate( fldSet%fields, _STAT )
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


    subroutine parse_fields(cfg, label, field_set, collection_name, items, rc)
       type(ESMF_Config), intent(inout) :: cfg
       character(*), intent(in) :: label
       type (FieldSet), intent(inout) :: field_set
       character(*), intent(in), optional :: collection_name
       type(GriddedIOitemVector), intent(inout), optional :: items
       integer, optional, intent(out) :: rc
       logical :: table_end
       logical :: vectorDone,match_alias
       integer :: m,i,j
       character(ESMF_MAXSTR), pointer:: fields (:,:)

       type(GriddedIOitem) :: item
       integer :: status
       character(len=:), allocatable :: usable_collection_name

       if (present(collection_name)) then
          usable_collection_name = trim(collection_name)
       else
          usable_collection_name = "unknown"
       end if
       call ESMF_ConfigFindLabel ( cfg, label=label//':', _RC)
       m = ESMF_ConfigGetLen(cfg, _RC)
       call ESMF_ConfigFindLabel ( cfg, label=label//':', _RC)
       if (m == 0) then
          ! allow for no entries on the fields: line
          call ESMF_ConfigNextLine  ( cfg,tableEnd=table_end,_RC )
          _ASSERT(.not.table_end, 'Premature end of fields list')
       end if
 
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
          export_name = extract_unquoted_item(export_name)

! Get GC Name
! ------------
          call ESMF_ConfigGetAttribute ( cfg,value=tmpstring ,_RC)
          if( trim(tmpstring) == ',' )  then
              call ESMF_ConfigGetAttribute ( cfg,value=component_name,_RC)
          else
              component_name = tmpstring
          endif

          component_name = extract_unquoted_item(component_name)

! Get Possible ALIAS Name
! -----------------------
          call ESMF_ConfigGetAttribute ( cfg,value=tmpstring ,rc=STATUS) ! MAT We don't check this status
          if( trim(tmpstring) == ',' )  then
              call ESMF_ConfigGetAttribute ( cfg,value=export_alias,default=export_name,rc=STATUS) ! MAT We don't check this status
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
          call ESMF_ConfigGetAttribute ( cfg,value=tmpstring ,rc=STATUS) ! MAT We don't check this status
          if( trim(tmpstring) == ',' )  then
              call ESMF_ConfigGetAttribute ( cfg,value=coupler_function_name,default=BLANK,rc=STATUS) ! MAT We don't check this status
          else
              if( trim(tmpstring) /= ' ' )  then
                  coupler_function_name = tmpstring
              else
                  coupler_function_name = BLANK
              endif
          endif
          coupler_function_name = extract_unquoted_item(coupler_function_name)
! convert to uppercase
          tmpstring = ESMF_UtilStringUpperCase(coupler_function_name,_RC)
! -------------

          call ESMF_ConfigNextLine  ( cfg,tableEnd=table_end,_RC )
          vectorDone=.false.

          idx = index(export_name,";")
          if (idx ==0) then
             item%itemType = ItemTypeScalar
             item%xname = trim(export_alias)
          else
             item%itemType = ItemTypeVector
          end if
          VECTORPAIR: do while(.not.vectorDone)
             allocate( fields(4,m), _STAT )

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
             allocate( field_set%fields(4,m), _STAT)
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
!      check for duplicates
       do i=1,field_set%nfields-1
          do j=i+1,field_set%nfields

             match_alias = field_set%fields(3,i) == field_set%fields(3,j)
             if (match_alias) then
                _FAIL("Caught collection "//usable_collection_name//" with this duplicate alias or shortname if no alias provided: "//trim(field_set%fields(3,i)))
             end if

          enddo
       enddo

       end subroutine parse_fields


 end subroutine Initialize

!======================================================
!>
! Run the `MAPL_HistoryGridComp` component.
!
 subroutine Run ( gc, import, export, clock, rc )

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
    logical, allocatable           :: NewSeg(:)
    logical, allocatable           :: Writing(:)
    type(ESMF_State)               :: state_out, final_state
    type(ESMF_Field)               :: temp_field, state_field
    integer                        :: nymd, nhms
    character(len=ESMF_MAXSTR)     :: DateStamp
    type(ESMF_Time)                :: current_time
    type(ESMF_Time)                :: lastMonth
    type(ESMF_TimeInterval)        :: dur, oneMonth
    integer                        :: sec
    type (StringGridMap)           :: pt_output_grids
    character(len=ESMF_MAXSTR)     :: key_grid_label
    type (ESMF_Grid), pointer      :: pgrid

    integer :: collection_id
    integer :: create_mode
    type(StringStringMap) :: global_attributes
    type(timeData) :: timeinfo_uninit
    type(ESMF_Grid) :: new_grid
!   variables for "backwards" mode
    logical                        :: fwd
    logical, allocatable           :: Ignore(:)

!   ErrLog vars
    integer                        :: status
    logical                        :: file_exists
    type(GriddedIOitem) :: item

    type(Logger), pointer          :: lgr

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

! Retrieve the pointer to the generic state
!------------------------------------------

    call MAPL_GetObjectFromGC ( gc, GENSTATE, _RC)

!   Get clocks' direction
    FWD = .not. ESMF_ClockIsReverse(clock)

   allocate(Ignore (nlist), _STAT)
   Ignore = .false.

  ! decide if clock direction and collections' backwards mode agree

   do n=1,nlist
      if (list(n)%backwards .eqv. FWD) Ignore(n) = .true.
   end do

!  Perform arithemetic parser operations
   do n=1,nlist
    if(Ignore(n)) cycle
    if ( Any(list(n)%ReWrite) ) then
     call MAPL_TimerOn(GENSTATE,"ParserRun")
     if( (.not.list(n)%disabled .and. IntState%average(n)) ) then
      call MAPL_RunExpression(IntState%CIM(n),list(n)%field_set%fields,list(n)%tmpfields, &
         list(n)%ReWrite,list(n)%field_set%nfields,_RC)
     end if
     if( (.not.list(n)%disabled) .and. (.not.IntState%average(n)) ) then
      call MAPL_RunExpression(IntState%GIM(n),list(n)%field_set%fields,list(n)%tmpfields, &
         list(n)%ReWrite,list(n)%field_set%nfields,_RC)
     end if
     call MAPL_TimerOff(GENSTATE,"ParserRun")
    endif
   end do

! We could make a copy for precision conversion here, if needed
! However, this is not very efficient. Copy is needed if it is
! time-averaged (i.e. couplers will be run), or if it is time to
! write instantaneous collection
!@   do n=1,nlist
!@      do m=1,list(n)%field_set%nfields
!@         if (list(n)%r8_to_r4(m)) then
!@            call MAPL_FieldCopy(from=list(n)%r8(m), to=list(n)%r4(m), _RC)
!@         end if
!@      end do
!@   end do

! Couplers are done here for now
!-------------------------------

    do n = 1, nlist
       if(Ignore(n)) cycle
       call MAPL_TimerOn(GENSTATE,trim(list(n)%collection))
       call MAPL_TimerOn(GENSTATE,"Couplers")
       if (.not.list(n)%disabled .and. IntState%average(n)) then
          ! R8 to R4 copy (if needed!)
          do m=1,list(n)%field_set%nfields
             if (list(n)%r8_to_r4(m)) then
                call MAPL_FieldCopy(from=list(n)%r8(m), &
                                    to=list(n)%r4(m), _RC)
             end if
          end do

          call ESMF_CplCompRun (INTSTATE%CCS(n), &
                                importState=INTSTATE%CIM(n), &
                                exportState=INTSTATE%GIM(n), &
                                clock=CLOCK,           &
                                userRC=STATUS)
          _VERIFY(STATUS)
       end if
       call MAPL_TimerOff(GENSTATE,"Couplers")
       call MAPL_TimerOff(GENSTATE,trim(list(n)%collection))
    end do

! Check for History Output
! ------------------------

   allocate(Writing (nlist), _STAT)
   allocate(filename(nlist), _STAT)
   allocate(NewSeg (nlist), _STAT)
   newSeg = .false.

  ! decide if we are writing based on alarms

   do n=1,nlist
      if (list(n)%skipWriting) then
         if (ESMF_AlarmIsRinging(list(n)%start_alarm)) then
            list(n)%skipWriting = .false.
         endif
      endif
   end do

   do n=1,nlist
      if (list(n)%disabled .or. ESMF_AlarmIsRinging(list(n)%end_alarm) ) then
         list(n)%disabled = .true.
         Writing(n) = .false.
      else if (list(n)%timeseries_output) then
         Writing(n) = ESMF_AlarmIsRinging ( list(n)%trajectory%alarm )
      else if (index(trim(list(n)%output_grid_label), 'SwathGrid') > 0) then
         Writing(n) = ESMF_AlarmIsRinging ( Hsampler%alarm )
      else
         Writing(n) = ESMF_AlarmIsRinging ( list(n)%his_alarm )
      endif

!      if(Writing(n)) then
!         call ESMF_AlarmRingerOff( list(n)%his_alarm,_RC )
!      end if

      if (Ignore(n)) then
         ! "Exersise" the alarms and then do nothing
         Writing(n) = .false.
!         if (ESMF_AlarmIsRinging ( list(n)%his_alarm )) then
!            call ESMF_AlarmRingerOff( list(n)%his_alarm,_RC )
!         end if
         if (ESMF_AlarmIsRinging ( list(n)%seg_alarm )) then
            call ESMF_AlarmRingerOff( list(n)%seg_alarm,_RC )
         end if
      end if

      if (list(n)%skipWriting) writing(n) = .false.

       if (writing(n) .and. .not.IntState%average(n)) then
          ! R8 to R4 copy (if needed!)
          do m=1,list(n)%field_set%nfields
             if (list(n)%r8_to_r4(m)) then
                call MAPL_FieldCopy(from=list(n)%r8(m), &
                                    to=list(n)%r4(m), _RC)
             end if
          end do
       end if

       ! Check for new segment
       !----------------------

       NewSeg(n) = ESMF_AlarmIsRinging ( list(n)%seg_alarm )

       if( NewSeg(n)) then
          call ESMF_AlarmRingerOff( list(n)%seg_alarm,_RC )
       endif

   end do


   if(any(Writing)) call WRITE_PARALLEL("")


  ! swath only
   epoch_swath_grid_case: do n=1,nlist
      call MAPL_TimerOn(GENSTATE,trim(list(n)%collection))
      if (index(trim(list(n)%output_grid_label), 'SwathGrid') > 0) then
         call MAPL_TimerOn(GENSTATE,"Swath")
         call MAPL_TimerOn(GENSTATE,"RegridAccum")
         call Hsampler%regrid_accumulate(list(n)%xsampler,_RC)
         call MAPL_TimerOff(GENSTATE,"RegridAccum")

         if( ESMF_AlarmIsRinging ( Hsampler%alarm ) ) then
            call MAPL_TimerOn(GENSTATE,"RegenGriddedio")
            create_mode = PFIO_NOCLOBBER ! defaut no overwrite
            if (intState%allow_overwrite) create_mode = PFIO_CLOBBER
            ! add time to items
            ! true metadata comes here from mGriddedIO%metadata
            ! the list(n)%mGriddedIO below only touches metadata, collection_id etc.
            !
            if (.NOT. list(n)%xsampler%have_initalized) then
               list(n)%xsampler%have_initalized = .true.
               global_attributes = list(n)%global_atts%define_collection_attributes(_RC)
            endif
            item%itemType = ItemTypeScalar
            item%xname = 'time'
            call list(n)%items%push_back(item)
            call Hsampler%fill_time_in_bundle ('time', list(n)%xsampler%acc_bundle, list(n)%xsampler%output_grid, _RC)
            call list(n)%mGriddedIO%destroy(_RC)
            call list(n)%mGriddedIO%CreateFileMetaData(list(n)%items,list(n)%xsampler%acc_bundle,timeinfo_uninit,vdata=list(n)%vdata,global_attributes=global_attributes,_RC)
            call list(n)%items%pop_back()
            collection_id = o_Clients%add_hist_collection(list(n)%mGriddedIO%metadata, mode = create_mode)
            call list(n)%mGriddedIO%set_param(write_collection_id=collection_id)
            call MAPL_TimerOff(GENSTATE,"RegenGriddedio")
         endif
         call MAPL_TimerOff(GENSTATE,"Swath")
      end if

      call MAPL_TimerOff(GENSTATE,trim(list(n)%collection))
   end do epoch_swath_grid_case

! Write Id and time
! -----------------

   if (any(writing)) call o_Clients%set_optimal_server(count(writing))

   OPENLOOP: do n=1,nlist
      call MAPL_TimerOn(GENSTATE,trim(list(n)%collection))
      call MAPL_TimerOn(GENSTATE,"IO Create")
      if( Writing(n) ) then

         call get_DateStamp ( clock, DateStamp=DateStamp,  &
              OFFSET = INTSTATE%STAMPOFFSET(n),            &
                                                 _RC )

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
              nymd=nymd, nhms=nhms, _RC ) ! here is where we get the actual filename of file we will write

         if(list(n)%monthly .and. list(n)%partial) then
            filename(n)=trim(filename(n)) // '-partial'
            list(n)%currentFile = filename(n)
         end if

         if( NewSeg(n)) then
            list(n)%partial = .false.
            if (list(n)%monthly) then
               ! get the number of seconds in this month
               ! it's tempting to use the variable "oneMonth" but it does not work
               ! instead we compute the differece between
               ! thisMonth and lastMonth and as a new timeInterval
               !
               call ESMF_ClockGet(clock,currTime=current_time,_RC)
               call ESMF_TimeIntervalSet( oneMonth, MM=1, _RC)
               lastMonth = current_time - oneMonth
               dur = current_time - lastMonth
               call ESMF_TimeIntervalGet(dur, s=sec, _RC)
               call list(n)%mGriddedIO%modifyTimeIncrement(sec, _RC)
            end if
         endif

         lgr => logging%get_logger('HISTORY.sampler')
         if (list(n)%timeseries_output) then
            if( ESMF_AlarmIsRinging ( list(n)%trajectory%alarm ) ) then
               call list(n)%trajectory%create_file_handle(filename(n),_RC)
               list(n)%currentFile = filename(n)
               list(n)%unit = -1
            end if
         elseif (list(n)%sampler_spec == 'station') then
            if (list(n)%unit.eq.0) then
               call lgr%debug('%a %a',&
                    "Station_data output to new file:",trim(filename(n)))
               call list(n)%station_sampler%close_file_handle(_RC)
               call list(n)%station_sampler%create_file_handle(filename(n),_RC)
               list(n)%currentFile = filename(n)
               list(n)%unit = -1
            end if
         elseif (list(n)%sampler_spec == 'mask') then
            if( list(n)%unit.eq.0 ) then
               if (list(n)%format == 'CFIO') then
                  if (.not.intState%allow_overwrite) then
                     inquire (file=trim(filename(n)),exist=file_exists)
                     _ASSERT(.not.file_exists,trim(filename(n))//" being created for History output already exists")
                  end if
!!                  call list(n)%mask_sampler%modifyTime(oClients=o_Clients,_RC)
                  list(n)%currentFile = filename(n)
                  list(n)%unit = -1
               else
                  list(n)%unit = GETFILE( trim(filename(n)),all_pes=.true.)
               end if
            end if
         else
            if( list(n)%unit.eq.0 ) then
               if (list(n)%format == 'CFIO') then
                  if (.not.intState%allow_overwrite) then
                     inquire (file=trim(filename(n)),exist=file_exists)
                     _ASSERT(.not.file_exists,trim(filename(n))//" being created for History output already exists")
                  end if
                  if (index(trim(list(n)%output_grid_label), 'SwathGrid') == 0) then
                     call list(n)%mGriddedIO%modifyTime(oClients=o_Clients,_RC)
                  endif
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
      call MAPL_TimerOff(GENSTATE,"IO Create")
      call MAPL_TimerOff(GENSTATE,trim(list(n)%collection))
   enddo OPENLOOP


   POSTLOOP: do n=1,nlist
      call MAPL_TimerOn(GENSTATE,trim(list(n)%collection))
      call MAPL_TimerOn(GENSTATE,"IO Post")

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
                       _RC)
               else
                  call RegridTransform(IntState%GIM(n), &
                       IntState%Regrid(n)%PTR%xform, &
                       state_out, &
                       IntState%Regrid(n)%PTR%LocIn, &
                       IntState%Regrid(n)%PTR%LocOut, &
                       IntState%Regrid(n)%PTR%ntiles_in, &
                       IntState%Regrid(n)%PTR%ntiles_out,&
                       _RC)
               end if
            else
               if (IntState%Regrid(n)%PTR%noxform) then
                  call RegridTransformT2G(STATE_IN=IntState%GIM(n), &
                       STATE_OUT=state_out, &
                       LS_OUT=IntState%Regrid(n)%PTR%LocOut, &
                       NTILES_OUT=IntState%Regrid(n)%PTR%ntiles_out, &
                       _RC)
               else
                  call RegridTransformT2G(STATE_IN=IntState%GIM(n), &
                       XFORM=IntState%Regrid(n)%PTR%xform, &
                       STATE_OUT=state_out, &
                       LS_OUT=IntState%Regrid(n)%PTR%LocOut, &
                       NTILES_OUT=IntState%Regrid(n)%PTR%ntiles_out, &
                       _RC)
               end if
            end if
         else
            state_out = INTSTATE%GIM(n)
         end if

         if (.not.list(n)%timeseries_output .AND. &
              list(n)%sampler_spec /= 'station' .AND. &
              list(n)%sampler_spec /= 'mask') then

            IOTYPE: if (list(n)%unit < 0) then    ! CFIO
               call list(n)%mGriddedIO%bundlepost(list(n)%currentFile,oClients=o_Clients,_RC)
            else

               if( INTSTATE%LCTL(n) ) then
                  call MAPL_GradsCtlWrite ( clock, state_out, list(n), &
                       filename(n), INTSTATE%expid, &
                       list(n)%global_atts%descr, intstate%output_grids,rc )
                  INTSTATE%LCTL(n) = .false.
               endif

               if (list(n)%nbits_to_keep < MAPL_NBITS_UPPER_LIMIT) then
                  final_state = ESMF_StateCreate(_RC)
                  do m=1,list(n)%field_set%nfields
                     call ESMF_StateGet(state_out,trim(list(n)%field_set%fields(3,m)),state_field,_RC)
                     temp_field = MAPL_FieldCreate(state_field,list(n)%field_set%fields(3,m),DoCopy=.true.,_RC)
                     call ESMF_StateAdd(final_state,[temp_field],_RC)
                  enddo
                  call ESMF_AttributeCopy(state_out,final_state,_RC)
                  call shavebits(final_state,list(n),_RC)
               end if

               do m=1,list(n)%field_set%nfields
                  if (list(n)%nbits_to_keep >=MAPL_NBITS_UPPER_LIMIT) then
                     call MAPL_VarWrite ( list(n)%unit, STATE=state_out, &
                        NAME=trim(list(n)%field_set%fields(3,m)), &
                        forceWriteNoRestart=.true., _RC )
                  else
                     call MAPL_VarWrite ( list(n)%unit, STATE=final_state, &
                        NAME=trim(list(n)%field_set%fields(3,m)), &
                        forceWriteNoRestart=.true., _RC )
                  endif
               enddo

               if (list(n)%nbits_to_keep < MAPL_NBITS_UPPER_LIMIT) then
                  do m=1,list(n)%field_set%nfields
                     call ESMF_StateGet(final_state,trim(list(n)%field_set%fields(3,m)),temp_field,_RC)
                     call ESMF_FieldDestroy(temp_field,noGarbage=.true.,_RC)
                  enddo
                  call ESMF_StateDestroy(final_state,noGarbage=.true.,_RC)
               end if
               call WRITE_PARALLEL("Wrote GrADS Output for File: "//trim(filename(n)))

            end if IOTYPE
         end if


         if (list(n)%sampler_spec == 'station') then
            call ESMF_ClockGet(clock,currTime=current_time,_RC)
            call MAPL_TimerOn(GENSTATE,"Station")
            call MAPL_TimerOn(GENSTATE,"AppendFile")
            call list(n)%station_sampler%append_file(current_time,_RC)
            call MAPL_TimerOff(GENSTATE,"AppendFile")
            call MAPL_TimerOff(GENSTATE,"Station")
         elseif (list(n)%sampler_spec == 'mask') then
            call ESMF_ClockGet(clock,currTime=current_time,_RC)
            call MAPL_TimerOn(GENSTATE,"Mask_append")
            if (list(n)%unit < 0) then    ! CFIO
               call list(n)%mask_sampler%regrid_append_file(current_time,&
                    list(n)%currentFile,oClients=o_Clients,_RC)
               call lgr%debug('%a %a', 'mask sampler list(n)%currentFile: ', trim(list(n)%currentFile))
            end if
            call MAPL_TimerOff(GENSTATE,"Mask_append")
         endif

      endif OUTTIME

      if( NewSeg(n) .and. list(n)%unit /= 0 .and. list(n)%duration /= 0 ) then
         if (list(n)%unit > 0 ) then
            call FREE_FILE( list(n)%unit )
         end if
         list(n)%unit = 0
       endif

      call MAPL_TimerOff(GENSTATE,"IO Post")
      call MAPL_TimerOff(GENSTATE,trim(list(n)%collection))
   enddo POSTLOOP


   call MAPL_TimerOn(GENSTATE,"Done Wait")
   if (any(writing)) then
      call o_Clients%done_collective_stage(_RC)
      call o_Clients%post_wait()
   endif
   call MAPL_TimerOff(GENSTATE,"Done Wait")


  ! destroy ogrid/RH/acc_bundle, regenerate them
  ! swath only
   epoch_swath_regen_grid: do n=1,nlist
      call MAPL_TimerOn(GENSTATE,trim(list(n)%collection))
      if (index(trim(list(n)%output_grid_label), 'SwathGrid') > 0) then
         call MAPL_TimerOn(GENSTATE,"Swath")
         if( ESMF_AlarmIsRinging ( Hsampler%alarm ) .and. .not. ESMF_AlarmIsRinging(list(n)%end_alarm) ) then
            call MAPL_TimerOn(GENSTATE,"RegenGrid")
            key_grid_label = list(n)%output_grid_label
            call Hsampler%destroy_rh_regen_ogrid ( key_grid_label, IntState%output_grids, list(n)%xsampler, _RC )
            pgrid => IntState%output_grids%at(trim(list(n)%output_grid_label))
            call list(n)%xsampler%Create_bundle_RH(list(n)%items,list(n)%bundle,Hsampler%tunit, &
                 ogrid=pgrid,vdata=list(n)%vdata,_RC)
            if( MAPL_AM_I_ROOT() )  write(6,'(//)')
            call MAPL_TimerOff(GENSTATE,"RegenGrid")
         endif
         call MAPL_TimerOff(GENSTATE,"Swath")
      end if
      call MAPL_TimerOff(GENSTATE,trim(list(n)%collection))
   end do epoch_swath_regen_grid


   WAITLOOP: do n=1,nlist

      if( Writing(n) .and. list(n)%unit < 0) then
         ! cleanup times
         if (allocated(list(n)%mGriddedIO%times)) deallocate(list(n)%mGriddedIO%times)
      end if

   enddo WAITLOOP

   WRITELOOP: do n=1,nlist

      call MAPL_TimerOn(GENSTATE,trim(list(n)%collection))

      if (list(n)%timeseries_output) then
         call MAPL_TimerOn(GENSTATE,"Trajectory")
         call MAPL_TimerOn(GENSTATE,"RegridAccum")
         call list(n)%trajectory%regrid_accumulate(_RC)
         call MAPL_TimerOff(GENSTATE,"RegridAccum")
         if( ESMF_AlarmIsRinging ( list(n)%trajectory%alarm ) ) then
            call MAPL_TimerOn(GENSTATE,"AppendFile")
            call list(n)%trajectory%append_file(current_time,_RC)
            call list(n)%trajectory%close_file_handle(_RC)
            call MAPL_TimerOff(GENSTATE,"AppendFile")
            if ( .not. ESMF_AlarmIsRinging(list(n)%end_alarm) ) then
               call MAPL_TimerOn(GENSTATE,"RegenLS")
               call list(n)%trajectory%destroy_rh_regen_LS (_RC)
               call MAPL_TimerOff(GENSTATE,"RegenLS")
            end if
         end if
         call MAPL_TimerOff(GENSTATE,"Trajectory")
      end if

      if( Writing(n) .and. list(n)%unit < 0) then

         list(n)%unit = -2

      end if

      call MAPL_TimerOff(GENSTATE,trim(list(n)%collection))
   enddo WRITELOOP

   if(any(Writing)) call WRITE_PARALLEL("")

   deallocate(NewSeg)
   deallocate(filename)
   deallocate(Writing)
   deallocate(Ignore)

   _RETURN(ESMF_SUCCESS)
 end subroutine Run

!======================================================
!>
! Finanlize the `MAPL_HistoryGridComp` component.
!
  subroutine Finalize ( gc, import, export, clock, rc )

    type(ESMF_GridComp), intent(inout)    :: gc     !! composite gridded component
    type(ESMF_State),       intent(inout) :: import !! import state
    type(ESMF_State),       intent(  out) :: export !! export state
    type(ESMF_Clock),       intent(inout) :: clock  !! the clock

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

    call MAPL_GetObjectFromGC ( gc, GENSTATE, _RC)

! Retrieve the pointer to the state

    call ESMF_GridCompGetInternalState(gc, wrap, status)
    IntState => wrap%ptr
    list => IntState%list
    nlist = size(list)

    do n=1,nlist
       if (list(n)%sampler_spec == 'mask') then
          call list(n)%mask_sampler%finalize(_RC)
       end if
    end do

! Close UNITs of GEOSgcm History Data
! -----------------------------------

   do n=1,nlist
      deallocate(list(n)%r4, list(n)%r8, list(n)%r8_to_r4)
      if (list(n)%disabled) cycle
      IF (list(n)%format == 'CFIO') then
         if( MAPL_CFIOIsCreated(list(n)%mcfio) ) then
            CALL MAPL_CFIOdestroy (list(n)%mcfio, _RC)
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
         call MAPL_StateDestroy(IntState%gim(n), _RC)
         call MAPL_StateDestroy(IntState%cim(n), _RC)
      end IF
   enddo
#endif


    call  MAPL_GenericFinalize ( GC, IMPORT, EXPORT, CLOCK, _RC )


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
   type(ESMF_Time)                :: StartTime
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

   character(len=3)               :: months(12)
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

   call ESMF_ClockGet ( clock, currTime=CurrTime,   _RC )
   call ESMF_ClockGet ( clock, StopTime=StopTime,   _RC )
   call ESMF_ClockGet ( clock, StartTime=StartTime, _RC )
   call ESMF_ClockGet ( clock, Calendar=cal,        _RC )

   call ESMF_TimeGet  ( CurrTime, timeString=TimeString, _RC )

   read(timestring( 1: 4),'(i4.4)') year
   read(timestring( 6: 7),'(i2.2)') month
   read(timestring( 9:10),'(i2.2)') day
   read(timestring(12:13),'(i2.2)') hour
   read(timestring(15:16),'(i2.2)') minute

   ti = StopTime-CurrTime
   freq = MAPL_nsecf( list%frequency )
   call ESMF_TimeIntervalSet( Frequency, S=freq, StartTime=StartTime, _RC )

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
   call ESMF_StateGet ( state,trim(list%field_set%fields(3,1)),field,_RC )
   call ESMF_FieldGet ( field, grid=grid, _RC )

   call MAPL_GridGet(GRID, globalCellCountPerDim=DIMS, _RC)

   ZERO   =  0
   IM     =  DIMS(1)
   JM     =  DIMS(2)
   LM     =  DIMS(3)
   if (LM == 0) LM = 1 ! needed for tilegrids

   call ESMF_GridGet(grid, name=gridname, _RC)

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
                                 Units    = MAPL_UnitsRadians      , &
                                 _RC)

      call ESMFL_GridCoordGet(   GRID, LONS       , &
                                 Name     = "Longitude"             , &
                                 Location = ESMF_STAGGERLOC_CENTER  , &
                                 Units    = MAPL_UnitsRadians      , &
                                 _RC)

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
            call MAPL_GridGet(pgrid,globalCellCountPerDim=dims,_RC)
            IM = dims(1)
            JM = dims(2)
            DLON   =  360._REAL64/IM
            if (JM /= 1) then
               DLAT   =  180._REAL64/(JM-1)
            else
               DLAT   =  1._REAL64
            end if
            LONBEG = -180._REAL64
            LATBEG =  -90._REAL64
         endif
      end block
   end if

! Compute Vertical Dimension for each Field (Augment nfield for VDIMS > LM)
! -------------------------------------------------------------------------
   allocate( vdim(list%field_set%nfields), _STAT )
   vdim = 0
   nfield =   list%field_set%nfields
   do m = 1,list%field_set%nfields
      call ESMFL_StateGetFieldArray( state,trim(list%field_set%fields(3,m)),array,status )
      call ESMF_ArrayGet( array, localarrayList=larrayList, _RC )
      call ESMF_LocalArrayGet( larrayList(1), RANK=rank, totalLBound=lbounds, &
           totalUBound=ubounds, _RC )
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
               'xdef ',i8,' linear ',f8.3,2x,f14.9,/,      &
               'ydef ',i8,' linear ',f8.3,2x,f14.9,/,      &
               'zdef ',i3,' linear  1  1',/,               &
               'tdef ',i5,' linear  ',i2.2,':',i2.2,'z',i2.2,a3,i4.4,3x,i2.2,'mn',/, &
               'vars  ',i3)
202     format('dset ^',a,/, 'title ',a,/,a,/,             &
               'undef ',e15.6,/,                           &
               'xdef ',i8,' linear ',f8.3,2x,f14.9,/,      &
               'ydef ',i8,' linear ',f8.3,2x,f14.9,/,      &
               'zdef ',i3,' linear  1  1',/,               &
               'tdef ',i5,' linear  ',i2.2,':',i2.2,'z',i2.2,a3,i4.4,3x,i2.2,'hr',/, &
               'vars  ',i3)
203     format('dset ^',a,/, 'title ',a,/,a,/,             &
               'undef ',e15.6,/,                           &
               'xdef ',i8,' linear ',f8.3,2x,f14.9,/,      &
               'ydef ',i8,' linear ',f8.3,2x,f14.9,/,      &
               'zdef ',i3,' linear  1  1',/,               &
               'tdef ',i5,' linear  ',i2.2,':',i2.2,'z',i2.2,a3,i4.4,3x,i2.2,'dy',/, &
               'vars  ',i3)
204     format('dset ^',a,/, 'title ',a,/,a,/,             &
               'undef ',e15.6,/,                           &
               'xdef ',i8,' linear ',f8.3,2x,f14.9,/,      &
               'ydef ',i8,' linear ',f8.3,2x,f14.9,/,      &
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
    logical                           :: LPERP
    integer                           :: YY,MM,DD,H,M,S
    integer                           :: noffset

    integer                    :: STATUS

    call ESMF_ClockGet ( clock, name=clockname, currTime=currentTime, _RC)

    if (present(offset)) then
        call ESMF_TimeIntervalGet( OFFSET, S=noffset, _RC )
        if( noffset /= 0 ) then
            LPERP = ( index( trim(clockname),'_PERPETUAL' ).ne.0 )
        if( LPERP ) then
            call ESMF_ClockGetAlarm ( clock, AlarmName='PERPETUAL', alarm=PERPETUAL, _RC )
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
                                                 S  = S, _RC )
                                                 MM = MM + 1
                call ESMF_TimeSet ( CurrentTime, YY = YY, &
                                                 MM = MM, &
                                                 DD = DD, &
                                                 H  = H , &
                                                 M  = M , &
                                                 S  = S, _RC )
#ifdef DEBUG
      if( MAPL_AM_I_ROOT() ) write(6,"(a,2x,i4.4,'/',i2.2,'/',i2.2,2x,'Time: ',i2.2,':',i2.2,':',i2.2)") "Inside HIST GetDate: ",YY,MM,DD,H,M,S
#endif
            endif
        endif
        endif
        currentTime = currentTime - offset
    end if

    call ESMF_TimeGet (currentTime, timeString=TimeString, _RC)

    if(present(DateStamp)) then
       associate ( &
         year   => TimeString( 1: 4), &
         month  => TimeString( 6: 7), &
         day    => TimeString( 9:10), &
         hour   => TimeString(12:13), &
         minute => TimeString(15:16), &
         second => TimeString(18:19)  &
         )
         DateStamp = year//month//day//'_'//hour//minute//second //'z'
      end associate

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
       allocate(tile_out(ntiles_out), _STAT)
    end if

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

       call ESMF_ArrayGet(array_in , rank=rank_in , typekind=tk, _RC)
       call ESMF_ArrayGet(array_out, rank=rank_out, _RC)
       _ASSERT(rank_out == rank_in + 1,'needs informative message')

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

             if (present(XFORM)) then
                call MAPL_LocStreamTransform( tile_out, XFORM, tile_in, _RC )
             else
                tile_out => tile_in
             endif

             call MAPL_LocStreamTransform(LS_OUT, OUT2d, TILE_OUT, _RC)

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
  character(len=*),  intent(inout) :: fields(:,:)
  character(len=*),  intent(inout) :: tmpfields(:)
  logical,           intent(inout) :: rewrite(:)
  integer,           intent(inout) :: nPExtraFields
  character(len=*), pointer, intent(inout) :: ExtraFields(:)
  character(len=*), pointer, intent(inout) :: ExtraGridComp(:)
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
  logical                                 :: hasField

! Set rewrite flag and tmpfields.
! To keep consistency, all the arithmetic parsing output fields must
! only be combinations of the alias output field variables (i.e., fields(3,:))
! rather than the actual output field variables (i.e., fields(1,:)).
! Also do check that there are no illegal operations
!-------------------------------------------------------------------
  allocate ( exptmp (1), _STAT )
  exptmp(1) = ExpState
  ! check which fields are actual exports or expressions
  nPExtraFields = 0
  iRealFields = 0
  do m=1,nfield

    call MAPL_ExportStateGet(exptmp,fields(2,m),state,_RC)
    call checkIfStateHasField(state, fields(1,m), hasField, _RC)
    if (hasField) then
       iRealFields = iRealFields + 1
       rewrite(m)= .FALSE.
       tmpfields(m)= trim(fields(1,m))
    else
       rewrite(m)= .TRUE.
       tmpfields(m)= trim(fields(1,m))
      end if
  enddo

  ! now that we know this allocated a place to store the names of the real fields
  allocate(VarNames(iRealFields),_STAT)
  allocate(VarNeeded(iRealFields),_STAT)
  k=0
  do m=1,nfield
     if ( (rewrite(m) .eqv. .False.)) then
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
         call CheckSyntax(tmpfields(m),VarNames,VarNeeded,ExtVar=ExtVars,_RC)

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
         call CheckSyntax(tmpfields(m),VarNames,VarNeeded,ExtVar=ExtVars,_RC)

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
  allocate(TotVarNames(totFields),_STAT)
  allocate(TotCmpNames(totFields),_STAT)
  allocate(TotAliasNames(totFields),_STAT)
  allocate(TotRank(totFields),_STAT)
  allocate(TotLoc(totFields),_STAT)

  iRealFields = 0
  do i=1,nfield
    if ( (.not.rewrite(i)) ) then
       iRealFields = iRealFields + 1
       TotVarNames(iRealFields) = trim(fields(1,i))
       TotCmpNames(iRealFields) = trim(fields(2,i))
       TotAliasNames(iRealFields) = trim(fields(3,i))

       call MAPL_ExportStateGet(exptmp,fields(2,i),state,_RC)
       call MAPL_StateGet(state,fields(1,i),field,_RC)
       call ESMF_AttributeGet(field,name='DIMS',value=dims,_RC)
       TotRank(iRealFields) = dims
       call ESMF_AttributeGet(field,name='VLOCATION',value=dims,_RC)
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
        call MAPL_ExportStateGet ( exptmp,NonUniqueVarNames(i,2),state,_RC )
        call MAPL_StateGet(state, NonUniqueVarNames(i,1),field,_RC)

        call ESMF_AttributeGet(field,name='DIMS',value=dims,_RC)
        TotRank(iRealFields+nUniqueExtraFields) = dims
        call ESMF_AttributeGet(field,name='VLOCATION',value=dims,_RC)
        TotLoc(iRealFields+nUniqueExtraFields) = dims
     end if
  end do

  allocate(extraFields(nUniqueExtraFields),_STAT)
  allocate(extraGridComp(nUniqueExtraFields),_STAT)
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
 allocate(VarNeeded(TotFields),_STAT)

 do m=1,nfield
     if (Rewrite(m) .eqv. .TRUE.) then
         largest_rank =0
         ifound_vloc=.false.
         call CheckSyntax(tmpfields(m),TotAliasNames,VarNeeded,_RC)
         do i=1,TotFields
            if (VarNeeded(i)) then
               if (TotRank(i)> largest_rank) then
                  largest_rank=TotRank(i)
                  iRepField=i
               end if

               if (ifound_vloc) then
                  if (ivLoc /= Totloc(i) .and. totloc(i) /= MAPL_VLocationNone) then
                     _FAIL('arithmetic expression has two different vlocations')
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

 _RETURN(ESMF_SUCCESS)

 end subroutine MAPL_SetExpression

  subroutine MAPL_RunExpression(state,fields,tmpfields,rewrite,nfield,rc)

  type (ESMF_State),  intent(in)    :: state
  character(len=*), intent(in):: fields(:,:),tmpfields(:)
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
        call MAPL_StateGet(state,fname,field,force_field=.true.,_RC)
        fexpr = tmpfields(m)
        call MAPL_StateEval(state,fexpr,field,_RC)
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

    call ESMF_StateGet(state, ITEMCOUNT=N,  _RC)

    allocate(itemNameList(N), _STAT)
    allocate(itemtypeList(N), _STAT)

    call ESMF_StateGet(state,ITEMNAMELIST=itemNamelist,ITEMTYPELIST=itemtypeList,_RC)

    do I=1,N
       if(itemtypeList(I)==ESMF_STATEITEM_FIELD) then
          call ESMF_StateGet(state,itemNameList(I),FIELD,_RC)
          call ESMF_FieldDestroy(FIELD, _RC)
       else if(itemtypeList(I)==ESMF_STATEITEM_FieldBundle) then
          call ESMF_StateGet(state,itemNameList(I), BUNDLE, _RC)
          call ESMF_FieldBundleGet(BUNDLE,FieldCount=NF, _RC)
          DO J=1,NF
             call ESMF_FieldBundleGet(BUNDLE, J, FIELD, _RC)
             call ESMF_FieldDestroy(field, _RC)
          END DO
          call ESMF_FieldBundleDestroy(BUNDLE, _RC)
       else if(itemtypeList(I)==ESMF_STATEITEM_State) then
!ALT we ingore nested states for now, they will get destroyed by their GC
       end if
    end do
    call ESMF_StateDestroy(STATE, _RC)

    deallocate(itemNameList, _STAT)
    deallocate(itemtypeList, _STAT)

    _RETURN(ESMF_SUCCESS)
  end subroutine MAPL_StateDestroy
#endif

  subroutine MAPL_StateGet(state,name,field,force_field,rc)
    type(ESMF_State), intent(in) :: state
    character(len=*), intent(in) :: name
    type(ESMF_Field), intent(inout) :: field
    logical, optional, intent(in) :: force_field
    integer, optional, intent(out  ) :: rc

    integer :: status
    character(len=ESMF_MAXSTR) :: bundlename, fieldname
    type(ESMF_FieldBundle) :: bundle
    logical :: local_force_field
    integer :: i

    if (present(force_field)) then
       local_force_field = force_field
    else
       local_force_field = .false.
    end if
    i = 0
    if (.not.local_force_field) i = index(name,"%")
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

    _UNUSED_DUMMY(import)
    _UNUSED_DUMMY(export)
! Check if it is time to do anything
    doRecord = .false.

    call MAPL_InternalStateRetrieve(GC, meta, _RC)

    doRecord = MAPL_RecordAlarmIsRinging(meta, _RC)
    if (.not. doRecord) then
       _RETURN(ESMF_SUCCESS)
    end if

    call MAPL_DateStampGet(clock, datestamp, _RC)

! Retrieve the pointer to the state
    call ESMF_GridCompGetInternalState(gc, wrap, status)
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
                call ESMF_CplCompGet (INTSTATE%CCS(n), name=fname_saved, _RC)
                ! add timestamp to filename
                filename = trim(fname_saved) // datestamp
                call ESMF_CplCompSet (INTSTATE%CCS(n), name=filename, _RC)

                call ESMF_CplCompWriteRestart (INTSTATE%CCS(n), &
                     importState=INTSTATE%CIM(n), &
                     exportState=INTSTATE%GIM(n), &
                     clock=CLOCK,           &
                     userRC=STATUS)
                _VERIFY(STATUS)
                ! restore the compname
                call ESMF_CplCompSet (INTSTATE%CCS(n), name=fname_saved, _RC)
             end if
          end if
       end if
    enddo
    _RETURN(ESMF_SUCCESS)
  end subroutine RecordRestart

  subroutine  checkIfStateHasField(state, input_fieldName, hasField, rc)
    type(ESMF_State), intent(in) :: state ! export state
    character(len=*), intent(in) :: input_fieldName
    logical, intent(out)         :: hasField
    integer, intent(out), optional :: rc ! Error code:

    integer :: n, i, status, p_index
    character (len=ESMF_MAXSTR), allocatable  :: itemNameList(:)
    type(ESMF_StateItem_Flag),   allocatable  :: itemTypeList(:)
    character(len=:),allocatable :: field_name,bundle_name
    logical :: is_bundle,isPresent
    type(ESMF_FieldBundle) :: bundle

    call ESMF_StateGet(state, itemcount=n,  _RC)

    allocate(itemNameList(n), _STAT)
    allocate(itemTypeList(n), _STAT)
    call ESMF_StateGet(state,itemnamelist=itemNamelist,itemtypelist=itemTypeList,_RC)
    p_index = index(input_fieldName,"%")
    if (p_index/=0) then
       is_bundle = .true.
       bundle_name = input_fieldName(1:p_index-1)
       field_name = input_fieldName(p_index+1:)
    else
       is_bundle = .false.
       field_name = input_fieldName
    end if

    hasField = .false.
    if (is_bundle) then
      do I=1,N
         if(itemTypeList(I)/=ESMF_STATEITEM_FIELDBUNDLE) cycle
         if(itemNameList(I)==bundle_name) then
            call ESMF_StateGet(state,bundle_name,bundle,_RC)
            call ESMF_FieldBundleGet(bundle,field_name,isPresent=isPresent,_RC)
            if (isPresent) then
               hasField = .true.
               exit
            end if
         end if
      end do

    else
      do I=1,N
         if(itemTypeList(I)/=ESMF_STATEITEM_FIELD) cycle
         if(itemNameList(I)==field_name) then
            hasField = .true.
            exit
         end if
      end do
    end if
    deallocate(itemNameList, _STAT)
    deallocate(itemTypeList, _STAT)

    _RETURN(ESMF_SUCCESS)
  end subroutine checkIfStateHasField

    subroutine shavebits( state, list, rc)
    type(ESMF_state), intent(inout) :: state
    type (HistoryCollection), intent(in) :: list
    integer, optional, intent(out):: rc

    integer :: m, fieldRank, status
    type(ESMF_Field) :: field
    real, pointer :: ptr1d(:), ptr2d(:,:), ptr3d(:,:,:)
    type(ESMF_VM) :: vm
    integer :: comm

    call ESMF_VMGetCurrent(vm,_RC)
    call ESMF_VMGet(vm,mpiCommunicator=comm,_RC)

    do m=1,list%field_set%nfields
       call ESMF_StateGet(state, trim(list%field_set%fields(3,m)),field,_RC )
       call ESMF_FieldGet(field, rank=fieldRank,_RC)
       if (fieldRank ==1) then
          call ESMF_FieldGet(field, farrayptr=ptr1d, _RC)
          call DownBit(ptr1d,ptr1d,list%nbits_to_keep,undef=MAPL_undef,mpi_comm=comm,_RC)
       elseif (fieldRank ==2) then
          call ESMF_FieldGet(field, farrayptr=ptr2d, _RC)
          call DownBit(ptr2d,ptr2d,list%nbits_to_keep,undef=MAPL_undef,mpi_comm=comm,_RC)
       elseif (fieldRank ==3) then
          call ESMF_FieldGet(field, farrayptr=ptr3d, _RC)
          call DownBit(ptr3d,ptr3d,list%nbits_to_keep,undef=MAPL_undef,mpi_comm=comm,_RC)
       else
          _FAIL('The field rank is not implmented')
       endif
    enddo

    _RETURN(ESMF_SUCCESS)

  end subroutine

  subroutine CopyStateItems(src, dst, rc)
    type(ESMF_State), intent(in) :: src
    type(ESMF_State), intent(inout) :: dst
    integer, optional, intent(out) :: rc

! local vars
    type (ESMF_StateItem_Flag), pointer  :: itemTypes(:)
    character(len=ESMF_MAXSTR ), pointer :: itemNames(:)
    integer :: status
    integer :: n, itemCount
    type(ESMF_Field) :: field(1)
    type(ESMF_FieldBundle) :: bundle(1)

    call ESMF_StateGet(src,  itemCount=itemCount, _RC)

    allocate(itemnames(itemcount), _STAT)
    allocate(itemtypes(itemcount), _STAT)

    call ESMF_StateGet(src, itemNameList=itemNames, &
                       itemTypeList=itemTypes, _RC)

    do n=1,itemCount
       if(itemTypes(n)==ESMF_STATEITEM_FIELD) then
          call ESMF_StateGet(src, itemNames(n), field(1), _RC)
          call ESMF_StateAdd(dst, field, _RC)
       else if(itemTypes(n)==ESMF_STATEITEM_FieldBundle) then
          call ESMF_StateGet(src, itemNames(n), bundle(1), _RC)
          call ESMF_StateAdd(dst, bundle, _RC)
       end if
    end do

    deallocate(itemTypes)
    deallocate(itemNames)

    _RETURN(ESMF_SUCCESS)
  end subroutine CopyStateItems

  function get_acc_offset(current_time,ref_time,rc) result(acc_offset)
     integer :: acc_offset
     type(ESMF_Time), intent(in) :: current_time
     integer, intent(in) :: ref_time
     integer, optional, intent(out) :: rc

     integer :: status
     integer :: hour,minute,second,year,month,day,diff_sec
     type(ESMF_Time) :: new_time
     type(ESMF_TimeInterval) :: t_int

     call ESMF_TimeGet(current_time,yy=year,mm=month,dd=day,h=hour,m=minute,s=second,_RC)
     call MAPL_UnpackTime(ref_time,hour,minute,second)
     call ESMF_TimeSet(new_time,yy=year,mm=month,dd=day,h=hour,m=minute,s=second,_RC)
     t_int = new_time - current_time

     call ESMF_TimeIntervalGet(t_int,s=diff_sec,_RC)
     if (diff_sec == 0) then
        acc_offset = 0
     else if (diff_sec > 0) then
        acc_offset = diff_sec - 86400
     else if (diff_sec < 0) then
        acc_offset = diff_sec
     end if
     _RETURN(_SUCCESS)
  end function


  ! __ read data to object: obs_platform
  ! __ for each collection: find union fields, write to collection.rcx
  ! __ note: this subroutine is called by MPI root only
  !
  subroutine regen_rcx_for_obs_platform (config, nlist, list, rc)
    use  MAPL_scan_pattern_in_file
    use MAPL_ObsUtilMod, only : obs_platform, union_platform
    !
    !  Plan:
    !- read and write  schema
    !- extract union of field lines, print out to rc
    integer, parameter :: ESMF_MAXSTR2 = 2*ESMF_MAXSTR
    type(ESMF_Config), intent(inout)       :: config
    integer, intent(in)                    :: nlist
    type(HistoryCollection), pointer       :: list(:)
    integer, intent(inout), optional :: rc

    character(len=ESMF_MAXSTR) :: HIST_CF
    integer :: n, unitr, unitw
    logical :: match, contLine, con, con2
    integer :: status

    character (len=ESMF_MAXSTR) :: marker
    character (len=ESMF_MAXSTR) :: string
    character (len=ESMF_MAXSTR2) :: line, line2
    character (len=ESMF_MAXSTR2), allocatable :: str_piece(:)
    type(obs_platform), allocatable :: PLFS(:)
    type(obs_platform) :: p1
    integer :: k, i, j, m, i2
    integer :: ios, ngeoval, count, nplf
    integer :: length_mx
    integer :: mxseg
    integer :: nseg
    integer :: nseg_ub
    integer :: nfield, nplatform
    integer :: nfield_name_max
    logical :: obs_flag
    integer, allocatable :: map(:)
    type(Logger), pointer          :: lgr

    lgr => logging%get_logger('HISTORY.sampler')

    !
    !
    call ESMF_ConfigGetAttribute(config, value=HIST_CF, &
         label="HIST_CF:", default="HIST.rc", _RC )
    unitr = GETFILE(HIST_CF, FORM='formatted', _RC)

    call scan_count_match_bgn (unitr, 'PLATFORM.', nplf, .false.)
    rewind(unitr)

    if (nplf==0) then
       rc = 0
       return
    endif
    allocate (PLFS(nplf))
    allocate (map(nplf))

    ! __ global set for call split_string by space
    length_mx = ESMF_MAXSTR2
    mxseg = 100

    ! __ s1. scan get  platform name + index_name_x  var_name_lat/lon/time
    do k=1, nplf
       call scan_begin(unitr, 'PLATFORM.', .false.)
       backspace(unitr)
       read(unitr, '(a)', iostat=ios) line
       _ASSERT (ios==0, 'read line failed')
       i=index(line, '.')
       j=index(line, ':')
       _ASSERT(i>1 .AND. j>1, 'keyword PLATFORM.X is not found')
       PLFS(k)%name = line(i+1:j-1)
       marker=line(1:j)

       call scan_contain(unitr, marker, .true.)
       call scan_contain(unitr, 'index_name_x:', .false.)
       backspace(unitr)
       read(unitr, '(a)', iostat=ios) line
       _ASSERT (ios==0, 'read line failed')
       i=index(line, ':')
       PLFS(k)%index_name_x = trim(line(i+1:))

       call scan_contain(unitr, marker, .true.)
       call scan_contain(unitr, 'var_name_lon:', .false.)
       backspace(unitr)
       read(unitr, '(a)', iostat=ios) line
       _ASSERT (ios==0, 'read line failed')
       i=index(line, ':')
       PLFS(k)%var_name_lon = trim(line(i+1:))

       call scan_contain(unitr, marker, .true.)
       call scan_contain(unitr, 'var_name_lat:', .false.)
       backspace(unitr)
       read(unitr, '(a)', iostat=ios) line
       _ASSERT (ios==0, 'read line failed')
       i=index(line, ':')
       PLFS(k)%var_name_lat = trim(line(i+1:))

       call scan_contain(unitr, marker, .true.)
       call scan_contain(unitr, 'var_name_time:', .false.)
       backspace(unitr)
       read(unitr, '(a)', iostat=ios) line
       _ASSERT (ios==0, 'read line failed')
       i=index(line, ':')
       PLFS(k)%var_name_time = trim(line(i+1:))

       call scan_contain(unitr, marker, .true.)
       call scan_contain(unitr, 'file_name_template:', .false.)
       backspace(unitr)
       read(unitr, '(a)', iostat=ios) line
       _ASSERT (ios==0, 'read line failed')
       i=index(line, ':')
       PLFS(k)%file_name_template = trim(line(i+1:))

       call lgr%debug('%a %a %a %a %a', &
            trim( PLFS(k)%name ), &
            trim( PLFS(k)%var_name_lon ), &
            trim( PLFS(k)%var_name_lat ), &
            trim( PLFS(k)%var_name_time ), &
            trim( PLFS(k)%file_name_template ) )

    end do



    ! __ s2.1 scan fields: only determine ngeoval / nfield_name_max = nword
    allocate (str_piece(mxseg))
    rewind(unitr)
    do k=1, nplf
       call scan_begin(unitr, 'PLATFORM.', .false.)
       call scan_contain(unitr, 'geovals_fields:', .false.)
       ios=0
       ngeoval=0
       nseg_ub=0
       do while (ios == 0)
          read (unitr, '(A)', iostat=ios) line
          _ASSERT (ios==0, 'read line failed')
          con = (adjustl(trim(line))=='::')
          if (con) exit
          !! print *, 'line, con', trim(line), con
          con2= (index ( adjustl(line), '#' ) == 1)    ! skip comment line
          if ( .not. con2 ) then
             ngeoval = ngeoval + 1
             call  split_string_by_space (line, length_mx, mxseg, &
                  nseg, str_piece, status)
             nseg_ub = max(nseg_ub, nseg)
          end if
       enddo
       PLFS(k)%ngeoval = ngeoval
       nseg_ub = PLFS(k)%nfield_name_mx
       allocate ( PLFS(k)%field_name (nseg_ub, ngeoval) )
       PLFS(k)%field_name = ''
       !! print*, 'k, ngeoval, nfield_name_max', k, ngeoval, nseg_ub
    end do


    ! __ s2.2 scan fields: get splitted PLFS(k)%field_name
    rewind(unitr)
    do k=1, nplf
       call scan_begin(unitr, 'PLATFORM.', .false.)
       backspace(unitr)
       read(unitr, '(a)', iostat=ios) line
       _ASSERT (ios==0, 'read line failed')
       i=index(line, 'PLATFORM.')
       j=index(line, ':')
       marker=line(1:j)
       !
       call scan_begin(unitr, marker, .true.)
       call scan_contain(unitr, 'geovals_fields:', .false.)
       ios=0
       ngeoval=0
       do while (ios == 0)
          read (unitr, '(A)', iostat=ios) line
          _ASSERT (ios==0, 'read line failed')
          !! write(6,*) 'k in nplf, line', k, trim(line)
          con = (adjustl(trim(line))=='::')
          if (con) exit
          con2= (index ( adjustl(line), '#' ) == 1)    ! skip comment line
          if (.NOT.con2) then
             ngeoval = ngeoval + 1
             call  split_string_by_space (line, length_mx, mxseg, &
                  nseg, str_piece, status)
             do m=1, nseg
                PLFS(k)%field_name (m, ngeoval) = trim(str_piece(m))
             end do
          endif
       enddo
    end do
    deallocate(str_piece)
    rewind(unitr)


    call lgr%debug('%a %i8','count PLATFORM.', nplf)
    if (mapl_am_i_root()) then
       do k=1, nplf
          write(6, '(10x,a,i3,a,2x,a)') 'PLFS(', k, ') =',  trim(PLFS(k)%name)
          do i=1, size(PLFS(k)%field_name, 2)
             line=''
             do j=1, size(PLFS(k)%field_name, 1)
                write(line2, '(a)')  trim(PLFS(k)%field_name(j,i))
                line=trim(line)//trim(line2)
             end do
             write(6, '(24x,a)') trim(line)
          enddo
       enddo
    end if
!!    write(6,*) 'nlist=', nlist


    ! __ s3: Add more entry:  'obs_files:' and 'fields:' to rcx
    !  for each collection
    obs_flag=.false.
    do n = 1, nlist
       rewind(unitr)
       string = trim( list(n)%collection ) // '.'
       unitw = GETFILE(trim(string)//'rcx', FORM='formatted', _RC)
       match = .false.
       contLine = .false.
       obs_flag = .false.
       do while (.true.)
          read(unitr, '(A)', iostat=ios, end=1236) line
          _ASSERT (ios==0, 'read line failed')
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
          if ( index(adjustl(line), trim(string)//'ObsPlatforms:') == 1 ) then
             obs_flag =.true.
             line2 = line
             !! write(6,*) 'first line for ObsPlatforms:=', trim(line)
          endif
       end do
1236   continue


       if (obs_flag) then
          allocate (str_piece(mxseg))
          i = index(line2, ':')
          line = adjustl ( line2(i+1:) )
          call split_string_by_space (line, length_mx, mxseg, &
               nplatform, str_piece, status)

          call lgr%debug('%a %a', 'line for obsplatforms=', trim(line))
          call lgr%debug('%a %i6', 'split string,  nplatform=', nplatform)
          call lgr%debug('%a %i6', 'nplf=', nplf)
          !if (mapl_am_i_root()) then
          !   write(6,*)  '     str_piece=', str_piece(1:nplatform)
          !end if

          !
          !   a) union the platform
          !
          ! find the index for each str_piece
          map(:) = -1
          do i=1, nplatform  ! for loc collection
             do j=1, nplf    ! tot
                if ( trim(str_piece(i)) == trim( PLFS(j)%name ) ) then
                   map(i)=j
                   exit
                end if
             end do
          end do
          deallocate(str_piece)
          !if (mapl_am_i_root()) then
          !   write(6,*) 'collection n=',n, 'map(:)=', map(:)
          !end if


          ! __ write common nc_index,time,lon,lat
          k=map(1)   ! plat form # 1
          write(unitw, '(2(2x,a))') trim(string)//'index_name_x:    ', trim(adjustl(PLFS(k)%index_name_x))
          write(unitw, '(2(2x,a))') trim(string)//'var_name_time:   ', trim(adjustl(PLFS(k)%var_name_time))
          write(unitw, '(2(2x,a))') trim(string)//'var_name_lon:    ', trim(adjustl(PLFS(k)%var_name_lon))
          write(unitw, '(2(2x,a))') trim(string)//'var_name_lat:    ', trim(adjustl(PLFS(k)%var_name_lat))

          do i=1, nplatform
             k=map(i)
             if (i==1) then
                p1 = PLFS(k)
             else
                p1 = union_platform(p1, PLFS(k), _RC)
             end if
          end do

          nfield = p1%ngeoval
          nfield_name_max = p1%nfield_name_mx
          do j=1, nfield
             line=''
             do i=1, nfield_name_max
                line = trim(line)//' '//trim(p1%field_name(i,j))
             enddo
              if (j==1) then
                write(unitw, '(10(2x,a))') trim(string)//'fields:', trim(line)
             else
                write(unitw, '(12x,a)') trim(line)
             end if
          end do
          write(unitw,'(a,/)') '::'
          write(unitw,'(a)') trim(string)//'obs_files:     # table start from next line'

          !! TODO: add debug
          !! write(6,*) 'nplatform', nplatform
          do i2=1, nplatform
             k=map(i2)
             write(unitw, '(a)') trim(adjustl(PLFS(k)%file_name_template))
             do j=1, PLFS(k)%ngeoval
                line=''
                do i=1, nfield_name_max
                   line = trim(line)//' '//trim(adjustl(PLFS(k)%field_name(i,j)))
                enddo
                write(unitw, '(a)') trim(adjustl(line))
             enddo
             write(unitw, '(20a)') (('-'), j=1,20)
          enddo
          write(unitw,'(a)') '::'
       end if
       call free_file(unitw, _RC)
    end do
    call free_file(unitr, _RC)

    _RETURN(ESMF_SUCCESS)
  end subroutine regen_rcx_for_obs_platform


end module MAPL_HistoryGridCompMod
