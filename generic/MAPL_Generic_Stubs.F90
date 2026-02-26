#include "MAPL.h"

module MAPL_GenericMod

   use ESMF
   use MAPL_BaseMod
   use MaplGeneric
   use MAPL_Profiler, only: DistributedProfiler
   use MAPL_SunMod, only: MAPL_SunOrbit
   use MAPL_LocStreamMod, only: MAPL_LocStream
   use MAPL_ExceptionHandling
   use MAPL_KeywordEnforcerMod
   use pFlogger, only: logging, Logger
   use, intrinsic :: iso_fortran_env, only: REAL32, REAL64, int32, int64

   implicit none
   private

   public :: MAPL_GenericSetServices
   public :: MAPL_GenericInitialize
   public :: MAPL_GenericRunChildren
   public :: MAPL_GenericFinalize

   public :: MAPL_AddInternalSpec
   public :: MAPL_AddImportSpec
   public :: MAPL_AddExportSpec

   public :: MAPL_DoNotDeferExport

   public :: MAPL_GridCompSetEntryPoint
   public :: MAPL_GetObjectFromGC
   public :: MAPL_Get
   public :: MAPL_Set
   public :: MAPL_GenericRunCouplers

   public :: MAPL_StateAlarmAdd
   public :: MAPL_StateAlarmGet
   public :: MAPL_StateCreateFromSpec
   public :: MAPL_StateCreateFromSpecNew
   public :: MAPL_FriendlyGet
   public :: MAPL_GridCompGetFriendlies
   public :: MAPL_SetVarSpecForCC
   public :: MAPL_DateStampGet
   public :: MAPL_ExchangeGridGet
   public :: MAPL_ExchangeGridSet
   public :: MAPL_ImportStateGet
   public :: MAPL_InternalESMFStateGet
   public :: MAPL_ExportStateGet
   public :: MAPL_GetChildLocstream
   public :: MAPL_CopyFriendliness
   public :: MAPL_VerifyFriendly
   public :: MAPL_GenericMakeXchgNatural
   public :: MAPL_GridCreate
   public :: MAPL_GenericRefresh
   public :: MAPL_AddRecord
   public :: MAPL_DisableRecord
   public :: MAPL_RecordAlarmIsRinging
   public :: MAPL_GenericRecord
   public :: MAPL_GetAllExchangeGrids
   public :: MAPL_DoNotAllocateImport
   public :: MAPL_DoNotAllocateInternal
   public :: MAPL_GCGet
   public :: MAPL_CheckpointState
   public :: MAPL_ESMFStateReadFromFile
   public :: MAPL_InternalStateRetrieve
   public :: MAPL_GetLogger
   public :: MAPL_SetStateSave
   public :: MAPL_DestroyStateSave
   public :: MAPL_GenericStateSave
   public :: MAPL_GenericStateRestore
   public :: MAPL_RootGcRetrieve
   public :: MAPL_AddAttributeToFields
   public :: MAPL_MethodAdd

   public :: MAPL_AddChild
   public :: MAPL_AddConnectivity
   public :: MAPL_TerminateImport

   public :: MAPL_TimerOn
   public :: MAPL_TimerOff
   public :: MAPL_TimerAdd
   public :: MAPL_GetResource
   public :: MAPL_ReadForcing

   public :: MAPL_MetaComp

   interface MAPL_AddChild
      module procedure AddChildFromGC
      module procedure AddChildFromMeta
      module procedure AddChildFromDSO_old
      module procedure AddChildFromDSO
      module procedure AddChildFromDSOMeta
   end interface MAPL_AddChild

   interface MAPL_AddImportSpec
      module procedure MAPL_StateAddImportSpec_
      module procedure MAPL_StateAddImportSpecFrmChld
   end interface MAPL_AddImportSpec

   interface MAPL_AddInternalSpec
      module procedure MAPL_StateAddInternalSpec
   end interface MAPL_AddInternalSpec

   interface MAPL_AddExportSpec
      module procedure MAPL_StateAddExportSpec_
      module procedure MAPL_StateAddExportSpecFrmChld
      module procedure MAPL_StateAddExportSpecFrmChld_all
      module procedure MAPL_StateAddExportSpecFrmAll
   end interface MAPL_AddExportSpec

   interface MAPL_Get
      module procedure MAPL_GenericStateGet
   end interface MAPL_Get

   interface MAPL_Set
      module procedure MAPL_GenericStateSet
      module procedure MAPL_GenericStateSetFromGC
   end interface MAPL_Set

   interface MAPL_GetObjectFromGC
      module procedure MAPL_InternalStateGet
   end interface MAPL_GetObjectFromGC

   interface MAPL_TimerOn
      module procedure MAPL_GenericStateClockOn
   end interface MAPL_TimerOn

   interface MAPL_TimerOff
      module procedure MAPL_GenericStateClockOff
   end interface MAPL_TimerOff

   interface MAPL_TimerAdd
      module procedure MAPL_GenericStateClockAdd
   end interface MAPL_TimerAdd

   interface MAPL_TerminateImport
      module procedure MAPL_DoNotConnect
      module procedure MAPL_DoNotConnectMany
      module procedure MAPL_DoNotConnectAnyImport
      module procedure MAPL_TerminateImportAllBut
      module procedure MAPL_TerminateImportAll
   end interface MAPL_TerminateImport

   interface MAPL_AddConnectivity
      ! module procedure MAPL_AddConnectivityE2E
      module procedure MAPL_AddConnectivityRename
      module procedure MAPL_AddConnectivityRenameMany
      module procedure MAPL_AddConnectivityMany
   end interface MAPL_AddConnectivity

   interface MAPL_GridCompGetFriendlies
      module procedure MAPL_GridCompGetFriendlies0
      module procedure MAPL_GridCompGetFriendlies1
      module procedure MAPL_GridCompGetFriendlies2
      module procedure MAPL_GridCompGetFriendlies3
   end interface MAPL_GridCompGetFriendlies

   interface MAPL_GetResource
      module procedure MAPL_GetResourceFromConfig_scalar
      module procedure MAPL_GetResourceFromMAPL_scalar
      module procedure MAPL_GetResourceFromConfig_array
      module procedure MAPL_GetResourceFromMAPL_array
   end interface MAPL_GetResource

   interface MAPL_CopyFriendliness
      module procedure MAPL_CopyFriendlinessInField
      module procedure MAPL_CopyFriendlinessInState
   end interface MAPL_CopyFriendliness

   interface MAPL_VerifyFriendly
      module procedure MAPL_VerifyFriendlyInField
      module procedure MAPL_VerifyFriendlyInState
   end interface MAPL_VerifyFriendly

   interface MAPL_ReadForcing
      module procedure MAPL_ReadForcing1
      module procedure MAPL_ReadForcing2
   end interface MAPL_ReadForcing

   interface MAPL_CheckpointState
      module procedure MAPL_ESMFStateWriteToFile
   end interface MAPL_CheckpointState

   type MAPL_InitialState
      integer :: FILETYPE = MAPL_Write2Ram
      character(len=:), allocatable :: IMP_FNAME
      character(len=:), allocatable :: INT_FNAME
   end type MAPL_InitialState

<<<<<<< HEAD
=======
   type MAPL_Connectivity
      type(VarConn) :: CONNECT
      type(VarConn) :: DONOTCONN
   end type MAPL_Connectivity

>>>>>>> 5a9b0039 (Building fewer files in generic/)
   type MAPL_LinkType
      type(ESMF_GridComp) :: gc
      integer :: StateType
      integer :: SpecId
   end type MAPL_LinkType

   type MAPL_LinkForm
      type(MAPL_LinkType) :: FROM
      type(MAPL_LinkType) :: TO
   end type MAPL_LinkForm

   type MAPL_Link
      type(MAPL_LinkForm), pointer :: PTR
   end type MAPL_Link

   integer, parameter :: LAST_ALARM = 99

   type(ESMF_Method_Flag), public :: MAPL_Method_Refresh = ESMF_Method_None

   type MAPL_GenericRecordType
      type(ESMF_Alarm), pointer :: ALARM(:)
      integer, pointer :: FILETYPE(:)
      character(len=ESMF_MAXSTR) :: IMP_FNAME
      integer :: IMP_LEN
      character(len=ESMF_MAXSTR) :: INT_FNAME
      integer :: INT_LEN
   end type MAPL_GenericRecordType

<<<<<<< HEAD
   type MAPL_MetaComp
=======
   type, extends(MaplGenericComponent) :: MAPL_MetaComp
>>>>>>> 5a9b0039 (Building fewer files in generic/)
      character(len=ESMF_MAXSTR) :: COMPNAME
      type(ESMF_Config) :: CF
      character(:), allocatable :: full_name
      real :: HEARTBEAT
      type(DistributedProfiler), public :: t_profiler
      type(ESMF_CplComp), pointer :: CCS(:, :) => null()
      type(ESMF_State), pointer :: CIM(:, :) => null()
      type(ESMF_State), pointer :: CEX(:, :) => null()
      logical, pointer :: CCcreated(:, :) => null()
      type(MAPL_Link), pointer :: LINK(:) => null()
      character(len=ESMF_MAXSTR), allocatable :: GCNameList(:)
      integer, pointer :: phase_init(:) => null()
      integer, public, pointer :: phase_run(:) => null()
      integer, pointer :: phase_final(:) => null()
      integer, pointer :: phase_record(:) => null()
      integer, pointer :: phase_coldstart(:) => null()
      integer, pointer :: phase_refresh(:) => null()
      type(ESMF_GridComp) :: RootGC
      type(ESMF_GridComp), pointer :: parentGC => null()
      type(ESMF_Alarm) :: ALARM(0:LAST_ALARM)
      integer :: ALARMLAST = 0
      type(ESMF_Clock) :: clock
      type(MAPL_SunOrbit) :: ORBIT
      logical :: ChildInit = .true.
      type(MAPL_LocStream) :: ExchangeGrid
      type(MAPL_LocStream) :: LOCSTREAM
      type(MAPL_GenericRecordType), pointer :: RECORD => null()
      type(MAPL_InitialState) :: initial_state
      type(ESMF_State) :: FORCING
   end type MAPL_MetaComp

contains

   recursive subroutine MAPL_GenericSetServices(gc, rc)
      type(ESMF_GridComp), intent(inout) :: gc
      integer, optional, intent(out) :: rc

      integer :: status
      character(len=ESMF_MAXSTR) :: IAm = "MAPL_GenericSetServices"

      _FAIL("Time to port to MAPL3")
   end subroutine MAPL_GenericSetServices

   recursive subroutine MAPL_GenericInitialize(gc, IMPORT, EXPORT, clock, rc)
      type(ESMF_GridComp), intent(inout) :: gc
      type(ESMF_State), intent(inout) :: IMPORT
      type(ESMF_State), intent(inout) :: EXPORT
      type(ESMF_Clock), intent(inout) :: clock
      integer, optional, intent(out) :: rc

      integer :: status
      character(len=ESMF_MAXSTR) :: IAm = "MAPL_GenericInitialize"

      _FAIL("Time to port to MAPL3")
   end subroutine MAPL_GenericInitialize

   recursive subroutine MAPL_GenericRunChildren(gc, IMPORT, EXPORT, clock, rc)
      type(ESMF_GridComp), intent(inout) :: gc
      type(ESMF_State), intent(inout) :: IMPORT
      type(ESMF_State), intent(inout) :: EXPORT
      type(ESMF_Clock), intent(inout) :: clock
      integer, optional, intent(out) :: rc

      integer :: status
      character(len=ESMF_MAXSTR) :: IAm = "MAPL_GenericRunChildren"

      _FAIL("Time to port to MAPL3")
   end subroutine MAPL_GenericRunChildren

   recursive subroutine MAPL_GenericFinalize(gc, IMPORT, EXPORT, clock, rc)
      type(ESMF_GridComp), intent(inout) :: gc
      type(ESMF_State), intent(inout) :: IMPORT
      type(ESMF_State), intent(inout) :: EXPORT
      type(ESMF_Clock), intent(inout) :: clock
      integer, optional, intent(out) :: rc

      integer :: status
      character(len=ESMF_MAXSTR) :: IAm = "MAPL_GenericFinalize"

      _FAIL("Time to port to MAPL3")
   end subroutine MAPL_GenericFinalize

   subroutine MAPL_StateAlarmAdd(STATE, ALARM, rc)
      type(MAPL_MetaComp), intent(inout) :: STATE
      type(ESMF_Alarm), intent(in) :: ALARM
      integer, optional, intent(out) :: rc

      integer :: status
      character(len=ESMF_MAXSTR) :: IAm = "MAPL_StateAlarmAdd"

      _FAIL("Time to port to MAPL3")
   end subroutine MAPL_StateAlarmAdd

   subroutine MAPL_StateAlarmGet(STATE, ALARM, NAME, rc)
      type(MAPL_MetaComp), intent(inout) :: STATE
      type(ESMF_Alarm), intent(out) :: ALARM
      character(len=*), intent(in) :: NAME
      integer, optional, intent(out) :: rc

      integer :: status
      character(len=ESMF_MAXSTR) :: IAm = "MAPL_StateAlarmGet"

      _FAIL("Time to port to MAPL3")
   end subroutine MAPL_StateAlarmGet

   subroutine MAPL_StateCreateFromSpec(gc, rc)
      type(ESMF_GridComp), intent(inout) :: gc
      integer, optional, intent(out) :: rc

      integer :: status
      character(len=ESMF_MAXSTR) :: IAm = "MAPL_StateCreateFromSpec"

      _FAIL("Time to port to MAPL3")
   end subroutine MAPL_StateCreateFromSpec

   subroutine MAPL_StateCreateFromSpecNew(gc, rc)
      type(ESMF_GridComp), intent(inout) :: gc
      integer, optional, intent(out) :: rc

      integer :: status
      character(len=ESMF_MAXSTR) :: IAm = "MAPL_StateCreateFromSpecNew"

      _FAIL("Time to port to MAPL3")
   end subroutine MAPL_StateCreateFromSpecNew

   subroutine MAPL_FriendlyGet(gc, FRIENDLY, rc)
      type(ESMF_GridComp), intent(inout) :: gc
      character(len=*), intent(out) :: FRIENDLY
      integer, optional, intent(out) :: rc

      integer :: status
      character(len=ESMF_MAXSTR) :: IAm = "MAPL_FriendlyGet"

      FRIENDLY = ""
      _FAIL("Time to port to MAPL3")
   end subroutine MAPL_FriendlyGet

   recursive subroutine MAPL_GridCompGetFriendlies0(gc, TO, BUNDLE, AddGCPrefix, rc)
      type(ESMF_GridComp), intent(inout) :: gc
      character(len=*), intent(in) :: TO(:)
      type(ESMF_FieldBundle), intent(inout) :: BUNDLE
      logical, optional, intent(in) :: AddGCPrefix
      integer, optional, intent(out) :: rc

      integer :: status
      character(len=ESMF_MAXSTR) :: IAm = "MAPL_GridCompGetFriendlies0"

      _FAIL("Time to port to MAPL3")
   end subroutine MAPL_GridCompGetFriendlies0

   recursive subroutine MAPL_GridCompGetFriendlies1(gc, TO, BUNDLE, AddGCPrefix, rc)
      type(ESMF_GridComp), intent(inout) :: gc
      character(len=*), intent(in) :: TO
      type(ESMF_FieldBundle), intent(inout) :: BUNDLE
      logical, optional, intent(in) :: AddGCPrefix
      integer, optional, intent(out) :: rc

      integer :: status
      character(len=ESMF_MAXSTR) :: IAm = "MAPL_GridCompGetFriendlies1"

      _FAIL("Time to port to MAPL3")
   end subroutine MAPL_GridCompGetFriendlies1

   recursive subroutine MAPL_GridCompGetFriendlies2(gc, TO, BUNDLE, AddGCPrefix, rc)
      type(ESMF_GridComp), intent(inout) :: gc(:)
      character(len=*), intent(in) :: TO
      type(ESMF_FieldBundle), intent(inout) :: BUNDLE
      logical, optional, intent(in) :: AddGCPrefix
      integer, optional, intent(out) :: rc

      integer :: status
      character(len=ESMF_MAXSTR) :: IAm = "MAPL_GridCompGetFriendlies2"

      _FAIL("Time to port to MAPL3")
   end subroutine MAPL_GridCompGetFriendlies2

   recursive subroutine MAPL_GridCompGetFriendlies3(gc, TO, BUNDLE, AddGCPrefix, rc)
      type(ESMF_GridComp), intent(inout) :: gc(:)
      character(len=*), intent(in) :: TO(:)
      type(ESMF_FieldBundle), intent(inout) :: BUNDLE
      logical, optional, intent(in) :: AddGCPrefix
      integer, optional, intent(out) :: rc

      integer :: status
      character(len=ESMF_MAXSTR) :: IAm = "MAPL_GridCompGetFriendlies3"

      _FAIL("Time to port to MAPL3")
   end subroutine MAPL_GridCompGetFriendlies3

   subroutine MAPL_SetVarSpecForCC(gc, rc)
      type(ESMF_GridComp), intent(inout) :: gc
      integer, optional, intent(out) :: rc

      integer :: status
      character(len=ESMF_MAXSTR) :: IAm = "MAPL_SetVarSpecForCC"

      _FAIL("Time to port to MAPL3")
   end subroutine MAPL_SetVarSpecForCC

   subroutine MAPL_DateStampGet(clock, DATESTAMP, rc)
      type(ESMF_Clock), intent(inout) :: clock
      character(len=*), intent(out) :: DATESTAMP
      integer, optional, intent(out) :: rc

      integer :: status
      character(len=ESMF_MAXSTR) :: IAm = "MAPL_DateStampGet"

      DATESTAMP = ""
      _FAIL("Time to port to MAPL3")
   end subroutine MAPL_DateStampGet

   subroutine MAPL_ExchangeGridGet(gc, ExchangeGrid, rc)
      type(ESMF_GridComp), intent(inout) :: gc
      type(MAPL_LocStream), intent(inout) :: ExchangeGrid
      integer, optional, intent(out) :: rc

      integer :: status
      character(len=ESMF_MAXSTR) :: IAm = "MAPL_ExchangeGridGet"

      _FAIL("Time to port to MAPL3")
   end subroutine MAPL_ExchangeGridGet

   subroutine MAPL_ExchangeGridSet(gc, ExchangeGrid, rc)
      type(ESMF_GridComp), intent(inout) :: gc
      type(MAPL_LocStream), intent(in) :: ExchangeGrid
      integer, optional, intent(out) :: rc

      integer :: status
      character(len=ESMF_MAXSTR) :: IAm = "MAPL_ExchangeGridSet"

      _FAIL("Time to port to MAPL3")
   end subroutine MAPL_ExchangeGridSet

   recursive subroutine MAPL_ImportStateGet(gc, IMPORT, NAME, result, rc)
      type(ESMF_GridComp), intent(inout) :: gc
      type(ESMF_State), intent(in) :: IMPORT
      character(len=*), intent(in) :: NAME
      type(ESMF_State), intent(out) :: result
      integer, intent(out) :: rc

      integer :: status
      character(len=ESMF_MAXSTR) :: IAm = "MAPL_ImportStateGet"

      rc = ESMF_FAILURE
   end subroutine MAPL_ImportStateGet

   subroutine MAPL_InternalESMFStateGet(gc, INTERNAL, rc)
      type(ESMF_GridComp), intent(inout) :: gc
      type(ESMF_State), intent(out) :: INTERNAL
      integer, optional, intent(out) :: rc

      integer :: status
      character(len=ESMF_MAXSTR) :: IAm = "MAPL_InternalESMFStateGet"

      _FAIL("Time to port to MAPL3")
   end subroutine MAPL_InternalESMFStateGet

   recursive subroutine MAPL_ExportStateGet(EXPORT, NAME, result, rc)
      type(ESMF_State), intent(in) :: EXPORT(:)
      character(len=*), intent(in) :: NAME
      type(ESMF_State), intent(out) :: result
      integer, intent(out) :: rc

      integer :: status
      character(len=ESMF_MAXSTR) :: IAm = "MAPL_ExportStateGet"

      rc = ESMF_FAILURE
   end subroutine MAPL_ExportStateGet

   recursive subroutine MAPL_GetChildLocstream(gc, result, NAME, rc)
      type(ESMF_GridComp), intent(inout) :: gc
      type(MAPL_LocStream), intent(out) :: result
      character(len=*), intent(in) :: NAME
      integer, intent(out) :: rc

      integer :: status
      character(len=ESMF_MAXSTR) :: IAm = "MAPL_GetChildLocstream"

      rc = ESMF_FAILURE
   end subroutine MAPL_GetChildLocstream

   subroutine MAPL_CopyFriendlinessInState(STATEOUT, NAMEOUT, STATEIN, NAMEIN, rc)
      type(ESMF_State), intent(inout) :: STATEOUT
      character(len=*), intent(in) :: NAMEOUT
      type(ESMF_State), intent(in) :: STATEIN
      character(len=*), intent(in) :: NAMEIN
      integer, optional, intent(out) :: rc

      integer :: status
      character(len=ESMF_MAXSTR) :: IAm = "MAPL_CopyFriendlinessInState"

      _FAIL("Time to port to MAPL3")
   end subroutine MAPL_CopyFriendlinessInState

   subroutine MAPL_CopyFriendlinessInField(FIELDOUT, FIELDIN, rc)
      type(ESMF_Field), intent(inout) :: FIELDOUT
      type(ESMF_Field), intent(inout) :: FIELDIN
      integer, optional, intent(out) :: rc

      integer :: status
      character(len=ESMF_MAXSTR) :: IAm = "MAPL_CopyFriendlinessInField"

      _FAIL("Time to port to MAPL3")
   end subroutine MAPL_CopyFriendlinessInField

   function MAPL_VerifyFriendlyInField(FIELD, FRIEND2COMP, rc) result(FRIENDLY)
      type(ESMF_Field), intent(inout) :: FIELD
      character(len=*), intent(in) :: FRIEND2COMP
      integer, optional, intent(out) :: rc
      logical :: FRIENDLY

      integer :: status
      character(len=ESMF_MAXSTR) :: IAm = "MAPL_VerifyFriendlyInField"

      FRIENDLY = .false.
      _FAIL("Time to port to MAPL3")
   end function MAPL_VerifyFriendlyInField

   function MAPL_VerifyFriendlyInState(STATE, NAME, FRIEND2COMP, rc) result(FRIENDLY)
      type(ESMF_State), intent(in) :: STATE
      character(len=*), intent(in) :: NAME
      character(len=*), intent(in) :: FRIEND2COMP
      integer, optional, intent(out) :: rc
      logical :: FRIENDLY

      integer :: status
      character(len=ESMF_MAXSTR) :: IAm = "MAPL_VerifyFriendlyInState"

      FRIENDLY = .false.
      _FAIL("Time to port to MAPL3")
   end function MAPL_VerifyFriendlyInState

   subroutine MAPL_GenericMakeXchgNatural(STATE, rc)
      type(MAPL_MetaComp), intent(inout) :: STATE
      integer, optional, intent(out) :: rc

      integer :: status
      character(len=ESMF_MAXSTR) :: IAm = "MAPL_GenericMakeXchgNatural"

      _FAIL("Time to port to MAPL3")
   end subroutine MAPL_GenericMakeXchgNatural

   subroutine MAPL_GridCreate(gc, MAPLOBJ, ESMFGRID, srcGC, rc)
      type(ESMF_GridComp), optional, target, intent(inout) :: gc
      type(MAPL_MetaComp), optional, target, intent(inout) :: MAPLOBJ
      type(ESMF_Grid), optional, intent(out) :: ESMFGRID
      type(ESMF_GridComp), optional, intent(inout) :: srcGC
      integer, optional, intent(out) :: rc

      integer :: status
      character(len=ESMF_MAXSTR) :: IAm = "MAPL_GridCreate"

      _FAIL("Time to port to MAPL3")
   end subroutine MAPL_GridCreate

   subroutine MAPL_GenericRefresh(gc, IMPORT, EXPORT, clock, rc)
      type(ESMF_GridComp), intent(inout) :: gc
      type(ESMF_State), intent(inout) :: IMPORT
      type(ESMF_State), intent(inout) :: EXPORT
      type(ESMF_Clock), intent(inout) :: clock
      integer, optional, intent(out) :: rc

      integer :: status
      character(len=ESMF_MAXSTR) :: IAm = "MAPL_GenericRefresh"

      _FAIL("Time to port to MAPL3")
   end subroutine MAPL_GenericRefresh

   subroutine MAPL_AddRecord(MAPLOBJ, ALARM, FILETYPE, rc)
      type(MAPL_MetaComp), intent(inout) :: MAPLOBJ
      type(ESMF_Alarm), intent(inout) :: ALARM(:)
      integer, intent(in) :: FILETYPE(:)
      integer, optional, intent(out) :: rc

      integer :: status
      character(len=ESMF_MAXSTR) :: IAm = "MAPL_AddRecord"

      _FAIL("Time to port to MAPL3")
   end subroutine MAPL_AddRecord

   recursive subroutine MAPL_DisableRecord(MAPLOBJ, ALARM_NAME, rc)
      type(MAPL_MetaComp), intent(inout) :: MAPLOBJ
      character(len=*), intent(in) :: ALARM_NAME
      integer, optional, intent(out) :: rc

      integer :: status
      character(len=ESMF_MAXSTR) :: IAm = "MAPL_DisableRecord"

      _FAIL("Time to port to MAPL3")
   end subroutine MAPL_DisableRecord

   logical function MAPL_RecordAlarmIsRinging(META, unusable, MODE, rc)
      type(MAPL_MetaComp), intent(inout) :: META
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(in) :: MODE
      integer, optional, intent(out) :: rc

      integer :: status
      character(len=ESMF_MAXSTR) :: IAm = "MAPL_RecordAlarmIsRinging"

      MAPL_RecordAlarmIsRinging = .false.
      _FAIL("Time to port to MAPL3")
   end function MAPL_RecordAlarmIsRinging

   recursive subroutine MAPL_GenericRecord(gc, IMPORT, EXPORT, clock, rc)
      type(ESMF_GridComp), intent(inout) :: gc
      type(ESMF_State), intent(inout) :: IMPORT
      type(ESMF_State), intent(inout) :: EXPORT
      type(ESMF_Clock), intent(inout) :: clock
      integer, optional, intent(out) :: rc

      integer :: status
      character(len=ESMF_MAXSTR) :: IAm = "MAPL_GenericRecord"

      _FAIL("Time to port to MAPL3")
   end subroutine MAPL_GenericRecord

   subroutine MAPL_GetAllExchangeGrids(gc, rc)
      type(ESMF_GridComp), intent(inout) :: gc
      integer, optional, intent(out) :: rc

      integer :: status
      character(len=ESMF_MAXSTR) :: IAm = "MAPL_GetAllExchangeGrids"

      _FAIL("Time to port to MAPL3")
   end subroutine MAPL_GetAllExchangeGrids

   subroutine MAPL_DoNotAllocateImport(gc, rc)
      type(ESMF_GridComp), intent(inout) :: gc
      integer, optional, intent(out) :: rc

      integer :: status
      character(len=ESMF_MAXSTR) :: IAm = "MAPL_DoNotAllocateImport"

      _FAIL("Time to port to MAPL3")
   end subroutine MAPL_DoNotAllocateImport

   subroutine MAPL_DoNotAllocateInternal(gc, rc)
      type(ESMF_GridComp), intent(inout) :: gc
      integer, optional, intent(out) :: rc

      integer :: status
      character(len=ESMF_MAXSTR) :: IAm = "MAPL_DoNotAllocateInternal"

      _FAIL("Time to port to MAPL3")
   end subroutine MAPL_DoNotAllocateInternal

   subroutine MAPL_GCGet(gc, rc)
      type(ESMF_GridComp), intent(inout) :: gc
      integer, optional, intent(out) :: rc

      integer :: status
      character(len=ESMF_MAXSTR) :: IAm = "MAPL_GCGet"

      _FAIL("Time to port to MAPL3")
   end subroutine MAPL_GCGet

   subroutine MAPL_ESMFStateWriteToFile(STATE, clock, FILENAME, FILETYPE, &
        MPL, HDR, write_with_oserver, clobber, rc)
      type(ESMF_State), intent(inout) :: STATE
      type(ESMF_Clock), intent(in) :: clock
      character(len=*), intent(in) :: FILENAME
      character(len=*), intent(inout) :: FILETYPE
      type(MAPL_MetaComp), intent(inout) :: MPL
      logical, intent(in) :: HDR
      logical, optional, intent(in) :: write_with_oserver
      logical, optional, intent(in) :: clobber
      integer, optional, intent(out) :: rc

      integer :: status
      character(len=ESMF_MAXSTR) :: IAm = "MAPL_ESMFStateWriteToFile"

      _FAIL("Time to port to MAPL3")
   end subroutine MAPL_ESMFStateWriteToFile

   subroutine MAPL_ESMFStateReadFromFile(STATE, clock, FILENAME, MPL, HDR, rc)
      type(ESMF_State), intent(inout) :: STATE
      type(ESMF_Clock), intent(inout) :: clock
      character(len=*), intent(in) :: FILENAME
      type(MAPL_MetaComp), intent(inout) :: MPL
      logical, intent(in) :: HDR
      integer, optional, intent(out) :: rc

      integer :: status
      character(len=ESMF_MAXSTR) :: IAm = "MAPL_ESMFStateReadFromFile"

      _FAIL("Time to port to MAPL3")
   end subroutine MAPL_ESMFStateReadFromFile

   subroutine MAPL_InternalStateRetrieve(gc, STATE, rc)
      type(ESMF_GridComp), intent(inout) :: gc
      type(MAPL_MetaComp), pointer, intent(out) :: STATE
      integer, optional, intent(out) :: rc

      integer :: status
      character(len=ESMF_MAXSTR) :: IAm = "MAPL_InternalStateRetrieve"

      STATE => null()
      _FAIL("Time to port to MAPL3")
   end subroutine MAPL_InternalStateRetrieve

   function MAPL_GetLogger(gc, rc) result(lgr)
      type(ESMF_GridComp), intent(inout) :: gc
      integer, optional, intent(out) :: rc
      type(Logger), pointer :: lgr

      integer :: status
      character(len=ESMF_MAXSTR) :: IAm = "MAPL_GetLogger"

      lgr => null()
      _FAIL("Time to port to MAPL3")
   end function MAPL_GetLogger

   subroutine MAPL_SetStateSave(gc, rc)
      type(ESMF_GridComp), intent(inout) :: gc
      integer, optional, intent(out) :: rc

      integer :: status
      character(len=ESMF_MAXSTR) :: IAm = "MAPL_SetStateSave"

      _FAIL("Time to port to MAPL3")
   end subroutine MAPL_SetStateSave

   subroutine MAPL_DestroyStateSave(gc, rc)
      type(ESMF_GridComp), intent(inout) :: gc
      integer, optional, intent(out) :: rc

      integer :: status
      character(len=ESMF_MAXSTR) :: IAm = "MAPL_DestroyStateSave"

      _FAIL("Time to port to MAPL3")
   end subroutine MAPL_DestroyStateSave

   subroutine MAPL_GenericStateSave(gc, IMPORT, EXPORT, clock, rc)
      type(ESMF_GridComp), intent(inout) :: gc
      type(ESMF_State), intent(inout) :: IMPORT
      type(ESMF_State), intent(inout) :: EXPORT
      type(ESMF_Clock), intent(inout) :: clock
      integer, optional, intent(out) :: rc

      integer :: status
      character(len=ESMF_MAXSTR) :: IAm = "MAPL_GenericStateSave"

      _FAIL("Time to port to MAPL3")
   end subroutine MAPL_GenericStateSave

   subroutine MAPL_GenericStateRestore(gc, IMPORT, EXPORT, clock, rc)
      type(ESMF_GridComp), intent(inout) :: gc
      type(ESMF_State), intent(inout) :: IMPORT
      type(ESMF_State), intent(inout) :: EXPORT
      type(ESMF_Clock), intent(inout) :: clock
      integer, optional, intent(out) :: rc

      integer :: status
      character(len=ESMF_MAXSTR) :: IAm = "MAPL_GenericStateRestore"

      _FAIL("Time to port to MAPL3")
   end subroutine MAPL_GenericStateRestore

   function MAPL_RootGcRetrieve(META) result(gc)
      type(MAPL_MetaComp), intent(in) :: META
      type(ESMF_GridComp) :: gc

      gc = META%RootGC
   end function MAPL_RootGcRetrieve

   subroutine MAPL_AddAttributeToFields(STATE, NAME, VALUE, rc)
      type(ESMF_State), intent(inout) :: STATE
      character(len=*), intent(in) :: NAME
      character(len=*), intent(in) :: VALUE
      integer, optional, intent(out) :: rc

      integer :: status
      character(len=ESMF_MAXSTR) :: IAm = "MAPL_AddAttributeToFields"

      _FAIL("Time to port to MAPL3")
   end subroutine MAPL_AddAttributeToFields

   subroutine MAPL_MethodAdd(gc, METHOD, PHASE, rc)
      type(ESMF_GridComp), intent(inout) :: gc
      type(ESMF_Method_Flag), intent(in) :: METHOD
      integer, intent(in) :: PHASE
      integer, optional, intent(out) :: rc

      integer :: status
      character(len=ESMF_MAXSTR) :: IAm = "MAPL_MethodAdd"

      _FAIL("Time to port to MAPL3")
   end subroutine MAPL_MethodAdd

   subroutine MAPL_StateAddImportSpec_(gc, SHORT_NAME, LONG_NAME, &
        UNITS, DIMS, VLOCATION, &
        DATATYPE, NUM_SUBTILES, &
        REFRESH_INTERVAL, AVERAGING_INTERVAL, &
        HALOWIDTH, PRECISION, DEFAULT, UNGRIDDED_DIMS, &
        FIELD_TYPE, STAGGERING, ROTATION, RESTART, rc)
      type(ESMF_GridComp), intent(inout) :: gc
      character(len=*), intent(in) :: SHORT_NAME
      character(len=*), optional, intent(in) :: LONG_NAME
      character(len=*), optional, intent(in) :: UNITS
      integer, optional, intent(in) :: DIMS
      integer, optional, intent(in) :: DATATYPE
      integer, optional, intent(in) :: NUM_SUBTILES
      integer, optional, intent(in) :: VLOCATION
      integer, optional, intent(in) :: REFRESH_INTERVAL
      integer, optional, intent(in) :: AVERAGING_INTERVAL
      integer, optional, intent(in) :: HALOWIDTH
      integer, optional, intent(in) :: PRECISION
      real, optional, intent(in) :: DEFAULT
      integer, optional, intent(in) :: RESTART
      integer, optional, intent(in) :: UNGRIDDED_DIMS(:)
      integer, optional, intent(in) :: FIELD_TYPE
      integer, optional, intent(in) :: STAGGERING
      integer, optional, intent(in) :: ROTATION
      integer, optional, intent(out) :: rc

      integer :: status
      character(len=ESMF_MAXSTR) :: IAm = "MAPL_StateAddImportSpec_"

      _FAIL("Time to port to MAPL3")
   end subroutine MAPL_StateAddImportSpec_

   subroutine MAPL_StateAddImportSpecFrmChld(gc, CHILD_ID, &
        REFRESH_INTERVAL, AVERAGING_INTERVAL, OFFSET, rc)
      type(ESMF_GridComp), intent(inout) :: gc
      integer, intent(in) :: CHILD_ID
      integer, optional, intent(in) :: REFRESH_INTERVAL
      integer, optional, intent(in) :: AVERAGING_INTERVAL
      integer, optional, intent(in) :: OFFSET
      integer, optional, intent(out) :: rc

      integer :: status
      character(len=ESMF_MAXSTR) :: IAm = "MAPL_StateAddImportSpecFrmChld"

      _FAIL("Time to port to MAPL3")
   end subroutine MAPL_StateAddImportSpecFrmChld

   subroutine MAPL_StateAddInternalSpec(gc, &
        SHORT_NAME, &
        LONG_NAME, &
        UNITS, &
        DIMS, &
        VLOCATION, &
        DATATYPE, &
        NUM_SUBTILES, &
        REFRESH_INTERVAL, &
        AVERAGING_INTERVAL, &
        DEFAULT, &
        RESTART, &
        HALOWIDTH, &
        PRECISION, &
        FRIENDLYTO, &
        ADD2EXPORT, &
        ATTR_RNAMES, &
        ATTR_INAMES, &
        ATTR_RVALUES, &
        ATTR_IVALUES, &
        UNGRIDDED_DIMS, &
        FIELD_TYPE, &
        STAGGERING, &
        ROTATION, &
        rc)
      type(ESMF_GridComp), intent(inout) :: gc
      character(len=*), intent(in) :: SHORT_NAME
      character(len=*), optional, intent(in) :: LONG_NAME
      character(len=*), optional, intent(in) :: UNITS
      integer, optional, intent(in) :: DIMS
      integer, optional, intent(in) :: DATATYPE
      integer, optional, intent(in) :: VLOCATION
      integer, optional, intent(in) :: NUM_SUBTILES
      integer, optional, intent(in) :: REFRESH_INTERVAL
      integer, optional, intent(in) :: AVERAGING_INTERVAL
      integer, optional, intent(in) :: PRECISION
      real, optional, intent(in) :: DEFAULT
      integer, optional, intent(in) :: RESTART
      character(len=*), optional, intent(in) :: HALOWIDTH
      character(len=*), optional, intent(in) :: FRIENDLYTO
      logical, optional, intent(in) :: ADD2EXPORT
      character(len=*), optional, intent(in) :: ATTR_INAMES(:)
      character(len=*), optional, intent(in) :: ATTR_RNAMES(:)
      integer, optional, intent(in) :: ATTR_IVALUES(:)
      real, optional, intent(in) :: ATTR_RVALUES(:)
      integer, optional, intent(in) :: UNGRIDDED_DIMS(:)
      integer, optional, intent(in) :: FIELD_TYPE
      integer, optional, intent(in) :: STAGGERING
      integer, optional, intent(in) :: ROTATION
      integer, optional, intent(out) :: rc

      integer :: status
      character(len=ESMF_MAXSTR) :: IAm = "MAPL_StateAddInternalSpec"

      _FAIL("Time to port to MAPL3")
   end subroutine MAPL_StateAddInternalSpec

   subroutine MAPL_StateAddExportSpec_(gc, SHORT_NAME, LONG_NAME, &
        UNITS, DIMS, VLOCATION, &
        DATATYPE, NUM_SUBTILES, &
        REFRESH_INTERVAL, AVERAGING_INTERVAL, &
        HALOWIDTH, PRECISION, DEFAULT, UNGRIDDED_DIMS, &
        UNGRIDDED_UNIT, UNGRIDDED_NAME, UNGRIDDED_COORDS, &
        FIELD_TYPE, STAGGERING, ROTATION, &
        DEPENDS_ON, DEPENDS_ON_CHILDREN, POSITIVE, rc)
      type(ESMF_GridComp), intent(inout) :: gc
      character(len=*), intent(in) :: SHORT_NAME
      character(len=*), optional, intent(in) :: LONG_NAME
      character(len=*), optional, intent(in) :: UNITS
      integer, optional, intent(in) :: DIMS
      integer, optional, intent(in) :: DATATYPE
      integer, optional, intent(in) :: NUM_SUBTILES
      integer, optional, intent(in) :: VLOCATION
      integer, optional, intent(in) :: REFRESH_INTERVAL
      integer, optional, intent(in) :: AVERAGING_INTERVAL
      integer, optional, intent(in) :: HALOWIDTH
      integer, optional, intent(in) :: PRECISION
      real, optional, intent(in) :: DEFAULT
      integer, optional, intent(in) :: UNGRIDDED_DIMS(:)
      character(len=*), optional, intent(in) :: UNGRIDDED_UNIT
      character(len=*), optional, intent(in) :: UNGRIDDED_NAME
      real, optional, intent(in) :: UNGRIDDED_COORDS(:)
      integer, optional, intent(in) :: FIELD_TYPE
      integer, optional, intent(in) :: STAGGERING
      integer, optional, intent(in) :: ROTATION
      character(len=*), optional, intent(in) :: DEPENDS_ON
      logical, optional, intent(in) :: DEPENDS_ON_CHILDREN
      character(len=*), optional, intent(in) :: POSITIVE
      integer, optional, intent(out) :: rc

      integer :: status
      character(len=ESMF_MAXSTR) :: IAm = "MAPL_StateAddExportSpec_"

      _FAIL("Time to port to MAPL3")
   end subroutine MAPL_StateAddExportSpec_

   subroutine MAPL_StateAddExportSpecFrmChld(gc, SHORT_NAME, CHILD_ID, rc, TO_NAME)
      type(ESMF_GridComp), intent(inout) :: gc
      character(len=*), intent(in) :: SHORT_NAME
      integer, intent(in) :: CHILD_ID
      integer, optional, intent(out) :: rc
      character(len=*), optional, intent(out) :: TO_NAME

      integer :: status
      character(len=ESMF_MAXSTR) :: IAm = "MAPL_StateAddExportSpecFrmChld"

      _FAIL("Time to port to MAPL3")
   end subroutine MAPL_StateAddExportSpecFrmChld

   subroutine MAPL_StateAddExportSpecFrmChld_all(gc, CHILD_ID, rc)
      type(ESMF_GridComp), intent(inout) :: gc
      integer, intent(in) :: CHILD_ID
      integer, optional, intent(out) :: rc

      integer :: status
      character(len=ESMF_MAXSTR) :: IAm = "MAPL_StateAddExportSpecFrmChld_all"

      _FAIL("Time to port to MAPL3")
   end subroutine MAPL_StateAddExportSpecFrmChld_all

   subroutine MAPL_StateAddExportSpecFrmAll(STATE, rc)
      type(MAPL_MetaComp), intent(inout) :: STATE
      integer, optional, intent(out) :: rc

      integer :: status
      character(len=ESMF_MAXSTR) :: IAm = "MAPL_StateAddExportSpecFrmAll"

      _FAIL("Time to port to MAPL3")
   end subroutine MAPL_StateAddExportSpecFrmAll

   recursive integer function AddChildFromGC(gc, NAME, SS, petList, configFile, rc)
      type(ESMF_GridComp), intent(inout) :: gc
      character(len=*), intent(in) :: NAME
      external :: SS
      integer, optional, intent(in) :: petList(:)
      character(len=*), optional, intent(in) :: configFile
      integer, optional, intent(out) :: rc

      integer :: status
      character(len=ESMF_MAXSTR) :: IAm = "AddChildFromGC"

      AddChildFromGC = -1
      _FAIL("Time to port to MAPL3")
   end function AddChildFromGC

   recursive integer function AddChildFromMeta(META, NAME, SS, petList, configFile, parentGC, rc)
      type(MAPL_MetaComp), target, intent(inout) :: META
      character(len=*), intent(in) :: NAME
      external :: SS
      integer, optional, intent(in) :: petList(:)
      character(len=*), optional, intent(in) :: configFile
      type(ESMF_GridComp), optional, intent(in) :: parentGC
      integer, optional, intent(out) :: rc

      integer :: status
      character(len=ESMF_MAXSTR) :: IAm = "AddChildFromMeta"

      AddChildFromMeta = -1
      _FAIL("Time to port to MAPL3")
   end function AddChildFromMeta

   recursive integer function AddChildFromDSOMeta(META, NAME, userRoutine, sharedObj, &
        grid, petList, configFile, parentGC, rc)
      type(MAPL_MetaComp), target, intent(inout) :: META
      character(len=*), intent(in) :: NAME
      character(len=*), intent(in) :: userRoutine
      character(len=*), intent(in) :: sharedObj
      type(ESMF_Grid), optional, intent(inout) :: grid
      integer, optional, intent(in) :: petList(:)
      character(len=*), optional, intent(in) :: configFile
      type(ESMF_GridComp), optional, intent(in) :: parentGC
      integer, optional, intent(out) :: rc

      integer :: status
      character(len=ESMF_MAXSTR) :: IAm = "AddChildFromDSOMeta"

      AddChildFromDSOMeta = -1
      _FAIL("Time to port to MAPL3")
   end function AddChildFromDSOMeta

   recursive integer function AddChildFromDSO(gc, NAME, userRoutine, grid, sharedObj, &
        petList, configFile, rc)
      type(ESMF_GridComp), intent(inout) :: gc
      character(len=*), intent(in) :: NAME
      character(len=*), intent(in) :: userRoutine
      type(ESMF_Grid), optional, intent(inout) :: grid
      character(len=*), optional, intent(in) :: sharedObj
      integer, optional, intent(in) :: petList(:)
      character(len=*), optional, intent(in) :: configFile
      integer, optional, intent(out) :: rc

      integer :: status
      character(len=ESMF_MAXSTR) :: IAm = "AddChildFromDSO"

      AddChildFromDSO = -1
      _FAIL("Time to port to MAPL3")
   end function AddChildFromDSO

   recursive integer function AddChildFromDSO_old(NAME, userRoutine, grid, parentGC, &
        sharedObj, petList, configFile, rc)
      character(len=*), intent(in) :: NAME
      character(len=*), intent(in) :: userRoutine
      type(ESMF_Grid), optional, intent(inout) :: grid
      type(ESMF_GridComp), optional, intent(inout) :: parentGC
      character(len=*), optional, intent(in) :: sharedObj
      integer, optional, intent(in) :: petList(:)
      character(len=*), optional, intent(in) :: configFile
      integer, optional, intent(out) :: rc

      integer :: status
      character(len=ESMF_MAXSTR) :: IAm = "AddChildFromDSO_old"

      AddChildFromDSO_old = -1
      _FAIL("Time to port to MAPL3")
   end function AddChildFromDSO_old

   subroutine MAPL_GridCompSetEntryPoint(gc, registeredMethod, usersRoutine, rc)
      type(ESMF_GridComp), intent(inout) :: gc
      type(ESMF_Method_Flag), intent(in) :: registeredMethod
      external :: usersRoutine
      integer, optional, intent(out) :: rc

      integer :: status
      character(len=ESMF_MAXSTR) :: IAm = "MAPL_GridCompSetEntryPoint"

      _FAIL("Time to port to MAPL3")
   end subroutine MAPL_GridCompSetEntryPoint

   subroutine MAPL_GenericStateGet(STATE, IM, JM, LM, VERTDIM, &
        NX, NY, NX0, NY0, LAYOUT, &
        GCNames, &
        LONS, LATS, grid, ORBIT, RUNALARM, &
        IMPORTspec, EXPORTspec, INTERNALspec, &
        INTERNAL_ESMF_STATE, &
        TILETYPES, TILEKIND, &
        TILELATS, TILELONS, TILEAREA, LOCSTREAM, &
        ExchangeGrid, &
        clock, &
        NumInitPhases, &
        NumRunPhases, &
        GCS, CCS, GIM, GEX, CF, HEARTBEAT, &
        childrens_names, childrens_gridcomps, &
        childrens_import_states, childrens_export_states, &
        rc)

      type(MAPL_MetaComp), target, intent(inout) :: STATE
      type(ESMF_Alarm), optional, intent(out) :: RUNALARM
      type(MAPL_SunOrbit), optional, intent(out) :: ORBIT
      integer, optional, intent(out) :: IM, JM, LM
      integer, optional, intent(out) :: VERTDIM
      integer, optional, intent(out) :: NX, NY, NX0, NY0
      type(ESMF_DELayout), optional, intent(out) :: LAYOUT
      real, pointer, optional :: LONS(:, :)
      real, pointer, optional :: LATS(:, :)
      type(ESMF_Grid), optional :: grid
      integer, optional, intent(out) :: rc
      type(MAPL_VarSpec), optional, pointer :: IMPORTspec(:)
      type(MAPL_VarSpec), optional, pointer :: EXPORTspec(:)
      type(MAPL_VarSpec), optional, pointer :: INTERNALspec(:)
      type(ESMF_State), optional, intent(out) :: INTERNAL_ESMF_STATE
      integer, optional, pointer :: TILETYPES(:)
      integer, optional, pointer :: TILEKIND(:)
      real, pointer, optional :: TILELONS(:)
      real, pointer, optional :: TILELATS(:)
      real, pointer, optional :: TILEAREA(:)
      type(MAPL_LocStream), optional, intent(out) :: LOCSTREAM
      type(MAPL_LocStream), optional, intent(out) :: ExchangeGrid
      type(ESMF_Clock), optional, intent(out) :: clock
      type(ESMF_CplComp), optional, pointer :: CCS(:, :)

      ! Next four are deprecated (now have memory leak)
      character(len=ESMF_MAXSTR), optional, pointer :: GCNames(:)
      type(ESMF_GridComp), optional, pointer :: GCS(:)
      type(ESMF_State), optional, pointer :: GIM(:)
      type(ESMF_State), optional, pointer :: GEX(:)

      character(len=ESMF_MAXSTR), optional, allocatable :: childrens_names(:)
      type(ESMF_GridComp), optional, allocatable :: childrens_gridcomps(:)
      type(ESMF_State), optional, allocatable :: childrens_import_states(:)
      type(ESMF_State), optional, allocatable :: childrens_export_states(:)

      real, optional, intent(out) :: HEARTBEAT
      integer, optional, intent(out) :: NumInitPhases
      integer, optional, intent(out) :: NumRunPhases
      type(ESMF_Config), optional, intent(out) :: CF

      integer :: status
      character(len=ESMF_MAXSTR) :: IAm = "MAPL_GenericStateGet"

      _FAIL("Time to port to MAPL3")
   end subroutine MAPL_GenericStateGet

   subroutine MAPL_GenericStateClockOn(STATE, NAME, rc)
      type(MAPL_MetaComp), intent(inout) :: STATE
      character(len=*), intent(in) :: NAME
      integer, optional, intent(out) :: rc

      integer :: status
      character(len=ESMF_MAXSTR) :: IAm = "MAPL_GenericStateClockOn"

      _FAIL("Time to port to MAPL3")
   end subroutine MAPL_GenericStateClockOn

   subroutine MAPL_GenericStateClockOff(STATE, NAME, rc)
      type(MAPL_MetaComp), intent(inout) :: STATE
      character(len=*), intent(in) :: NAME
      integer, optional, intent(out) :: rc

      integer :: status
      character(len=ESMF_MAXSTR) :: IAm = "MAPL_GenericStateClockOff"

      _FAIL("Time to port to MAPL3")
   end subroutine MAPL_GenericStateClockOff

   subroutine MAPL_GenericStateClockAdd(gc, NAME, rc)
      type(ESMF_GridComp), intent(inout) :: gc
      character(len=*), intent(in) :: NAME
      integer, optional, intent(out) :: rc

      integer :: status
      character(len=ESMF_MAXSTR) :: IAm = "MAPL_GenericStateClockAdd"

      _FAIL("Time to port to MAPL3")
   end subroutine MAPL_GenericStateClockAdd

   subroutine MAPL_InternalStateGet(gc, MAPLOBJ, rc)
      type(ESMF_GridComp), intent(inout) :: gc
      type(MAPL_MetaComp), pointer :: MAPLOBJ
      integer, optional, intent(out) :: rc

      integer :: status
      character(len=ESMF_MAXSTR) :: IAm = "MAPL_InternalStateGet"

      MAPLOBJ => null()
      _FAIL("Time to port to MAPL3")
   end subroutine MAPL_InternalStateGet

   subroutine MAPL_GenericStateSet(STATE, ORBIT, LM, RUNALARM, ChildInit, &
        LOCSTREAM, ExchangeGrid, clock, NAME, &
        CF, configFile, component, rc)

      type(MAPL_MetaComp), intent(inout) :: STATE
      type(ESMF_Alarm), optional, intent(in) :: RUNALARM
      type(MAPL_SunOrbit), optional, intent(in) :: ORBIT
      integer, optional, intent(in) :: LM
      logical, optional, intent(in) :: ChildInit
      type(MAPL_LocStream), optional, intent(in) :: LOCSTREAM
      type(MAPL_LocStream), optional, intent(in) :: ExchangeGrid
      type(ESMF_Clock), optional, intent(in) :: clock
      type(ESMF_Config), optional, intent(in) :: CF
      character(len=*), optional, intent(in) :: NAME
      character(len=*), optional, intent(in) :: configFile
      class(AbstractComponent), optional, intent(in) :: component
      integer, optional, intent(out) :: rc

      integer :: status
      character(len=ESMF_MAXSTR) :: IAm = "MAPL_GenericStateSet"

      _FAIL("Time to port to MAPL3")
   end subroutine MAPL_GenericStateSet

   subroutine MAPL_GenericStateSetFromGC(gc, ORBIT, LM, RUNALARM, ChildInit, &
        LOCSTREAM, ExchangeGrid, clock, rc)
      type(ESMF_GridComp), intent(inout) :: gc
      type(ESMF_Alarm), optional, intent(in) :: RUNALARM
      type(MAPL_SunOrbit), optional, intent(in) :: ORBIT
      integer, optional, intent(in) :: LM
      logical, optional, intent(in) :: ChildInit
      type(MAPL_LocStream), optional, intent(in) :: LOCSTREAM
      type(MAPL_LocStream), optional, intent(in) :: ExchangeGrid
      type(ESMF_Clock), optional, intent(in) :: clock
      integer, optional, intent(out) :: rc

      integer :: status
      character(len=ESMF_MAXSTR) :: IAm = "MAPL_GenericStateSetFromGC"

      _FAIL("Time to port to MAPL3")
   end subroutine MAPL_GenericStateSetFromGC

   subroutine MAPL_GenericRunCouplers(STATE, child, clock, rc)
      type(MAPL_MetaComp), intent(inout) :: STATE
      integer, intent(in) :: child
      type(ESMF_Clock), intent(inout) :: clock
      integer, optional, intent(out) :: rc

      integer :: status
      character(len=ESMF_MAXSTR) :: IAm = "MAPL_GenericRunCouplers"

      _FAIL("Time to port to MAPL3")
   end subroutine MAPL_GenericRunCouplers

   subroutine MAPL_GetResourceFromMAPL_scalar(STATE, val, label, unusable, &
        DEFAULT, value_is_set, rc)
      type(MAPL_MetaComp), intent(inout) :: STATE
      character(len=*), intent(in) :: label
      class(*), intent(inout) :: val
      class(KeywordEnforcer), optional, intent(in) :: unusable
      class(*), optional, intent(in) :: DEFAULT
      logical, optional, intent(out) :: value_is_set
      integer, optional, intent(out) :: rc

      integer :: status
      character(len=ESMF_MAXSTR) :: IAm = "MAPL_GetResourceFromMAPL_scalar"

      _FAIL("Time to port to MAPL3")
   end subroutine MAPL_GetResourceFromMAPL_scalar

   subroutine MAPL_GetResourceFromConfig_scalar(config, val, label, unusable, &
        DEFAULT, value_is_set, rc)
      type(ESMF_Config), intent(inout) :: config
      character(len=*), intent(in) :: label
      class(*), intent(inout) :: val
      class(KeywordEnforcer), optional, intent(in) :: unusable
      class(*), optional, intent(in) :: DEFAULT
      logical, optional, intent(out) :: value_is_set
      integer, optional, intent(out) :: rc

      integer :: status
      character(len=ESMF_MAXSTR) :: IAm = "MAPL_GetResourceFromConfig_scalar"

      _FAIL("Time to port to MAPL3")
   end subroutine MAPL_GetResourceFromConfig_scalar

   subroutine MAPL_GetResourceFromMAPL_array(STATE, vals, label, unusable, &
        DEFAULT, value_is_set, rc)
      type(MAPL_MetaComp), intent(inout) :: STATE
      character(len=*), intent(in) :: label
      class(*), intent(inout) :: vals(:)
      class(KeywordEnforcer), optional, intent(in) :: unusable
      class(*), optional, intent(in) :: DEFAULT(:)
      logical, optional, intent(out) :: value_is_set
      integer, optional, intent(out) :: rc

      integer :: status
      character(len=ESMF_MAXSTR) :: IAm = "MAPL_GetResourceFromMAPL_array"

      _FAIL("Time to port to MAPL3")
   end subroutine MAPL_GetResourceFromMAPL_array

   subroutine MAPL_GetResourceFromConfig_array(config, vals, label, unusable, &
        DEFAULT, value_is_set, rc)
      type(ESMF_Config), intent(inout) :: config
      character(len=*), intent(in) :: label
      class(*), intent(inout) :: vals(:)
      class(KeywordEnforcer), optional, intent(in) :: unusable
      class(*), optional, intent(in) :: DEFAULT(:)
      logical, optional, intent(out) :: value_is_set
      integer, optional, intent(out) :: rc

      integer :: status
      character(len=ESMF_MAXSTR) :: IAm = "MAPL_GetResourceFromConfig_array"

      _FAIL("Time to port to MAPL3")
   end subroutine MAPL_GetResourceFromConfig_array

   subroutine MAPL_ReadForcing1(STATE, NAME, DATAFILE, CURRENTTIME, &
        FORCING, INIT_ONLY, ON_TILES, rc)
      type(MAPL_MetaComp), intent(inout) :: STATE
      character(len=*), intent(in) :: NAME
      character(len=*), intent(in) :: DATAFILE
      type(ESMF_Time), intent(inout) :: CURRENTTIME
      real, intent(out) :: FORCING(:)
      logical, optional, intent(in) :: INIT_ONLY
      logical, optional, intent(in) :: ON_TILES
      integer, optional, intent(out) :: rc

      integer :: status
      character(len=ESMF_MAXSTR) :: IAm = "MAPL_ReadForcing1"

      _FAIL("Time to port to MAPL3")
   end subroutine MAPL_ReadForcing1

   subroutine MAPL_ReadForcing2(STATE, NAME, DATAFILE, CURRENTTIME, &
        FORCING, INIT_ONLY, rc)
      type(MAPL_MetaComp), intent(inout) :: STATE
      character(len=*), intent(in) :: NAME
      character(len=*), intent(in) :: DATAFILE
      type(ESMF_Time), intent(inout) :: CURRENTTIME
      real, intent(out) :: FORCING(:, :)
      logical, optional, intent(in) :: INIT_ONLY
      integer, optional, intent(out) :: rc

      integer :: status
      character(len=ESMF_MAXSTR) :: IAm = "MAPL_ReadForcing2"

      _FAIL("Time to port to MAPL3")
   end subroutine MAPL_ReadForcing2

   subroutine MAPL_ReadForcingX(MPL, NAME, DATAFILE, CURRTIME, &
        FORCING1, FORCING2, INIT_ONLY, ON_TILES, rc)
      type(MAPL_MetaComp), intent(inout) :: MPL
      character(len=*), intent(in) :: NAME
      character(len=*), intent(in) :: DATAFILE
      type(ESMF_Time), intent(inout) :: CURRTIME
      real, optional, intent(out) :: FORCING1(:)
      real, optional, intent(out) :: FORCING2(:, :)
      logical, optional, intent(in) :: INIT_ONLY
      logical, optional, intent(in) :: ON_TILES
      integer, optional, intent(out) :: rc

      integer :: status
      character(len=ESMF_MAXSTR) :: IAm = "MAPL_ReadForcingX"

      _FAIL("Time to port to MAPL3")
   end subroutine MAPL_ReadForcingX

   subroutine MAPL_DoNotConnect(gc, SHORT_NAME, child, rc)
      type(ESMF_GridComp), intent(inout) :: gc
      character(len=*), intent(in) :: SHORT_NAME
      integer, intent(in) :: child
      integer, optional, intent(out) :: rc

      integer :: status
      character(len=ESMF_MAXSTR) :: IAm = "MAPL_DoNotConnect"

      _FAIL("Time to port to MAPL3")
   end subroutine MAPL_DoNotConnect

   subroutine MAPL_DoNotConnectMany(gc, SHORT_NAME, child, rc)
      type(ESMF_GridComp), intent(inout) :: gc
      character(len=*), intent(in) :: SHORT_NAME(:)
      integer, intent(in) :: child
      integer, optional, intent(out) :: rc

      integer :: status
      character(len=ESMF_MAXSTR) :: IAm = "MAPL_DoNotConnectMany"

      _FAIL("Time to port to MAPL3")
   end subroutine MAPL_DoNotConnectMany

   subroutine MAPL_DoNotConnectAnyImport(gc, child, rc)
      type(ESMF_GridComp), intent(inout) :: gc
      integer, intent(in) :: child
      integer, optional, intent(out) :: rc

      integer :: status
      character(len=ESMF_MAXSTR) :: IAm = "MAPL_DoNotConnectAnyImport"

      _FAIL("Time to port to MAPL3")
   end subroutine MAPL_DoNotConnectAnyImport

   subroutine MAPL_TerminateImportAllBut(gc, SHORT_NAMES, CHILD_IDS, rc)
      type(ESMF_GridComp), intent(inout) :: gc
      character(len=*), intent(in) :: SHORT_NAMES(:)
      integer, intent(in) :: CHILD_IDS(:)
      integer, optional, intent(out) :: rc

      integer :: status
      character(len=ESMF_MAXSTR) :: IAm = "MAPL_TerminateImportAllBut"

      _FAIL("Time to port to MAPL3")
   end subroutine MAPL_TerminateImportAllBut

   subroutine MAPL_TerminateImportAll(gc, ALL, rc)
      type(ESMF_GridComp), intent(inout) :: gc
      logical, intent(in) :: ALL
      integer, optional, intent(out) :: rc

      integer :: status
      character(len=ESMF_MAXSTR) :: IAm = "MAPL_TerminateImportAll"

      _FAIL("Time to port to MAPL3")
   end subroutine MAPL_TerminateImportAll

   subroutine MAPL_AddConnectivityE2E(gc, SHORT_NAME, SRC_ID, TO_EXPORT, TO_NAME, rc)
      type(ESMF_GridComp), intent(inout) :: gc
      character(len=*), intent(in) :: SHORT_NAME
      character(len=*), optional, intent(in) :: TO_NAME
      integer, intent(in) :: SRC_ID
      integer, intent(in) :: TO_EXPORT
      integer, optional, intent(out) :: rc

      integer :: status
      character(len=ESMF_MAXSTR) :: IAm = "MAPL_AddConnectivityE2E"

      _FAIL("Time to port to MAPL3")
   end subroutine MAPL_AddConnectivityE2E

   subroutine MAPL_AddConnectivityRename(gc, SRC_NAME, SRC_ID, &
        DST_NAME, DST_ID, rc)
      type(ESMF_GridComp), intent(inout) :: gc
      character(len=*), intent(in) :: SRC_NAME
      character(len=*), intent(in) :: DST_NAME
      integer, intent(in) :: SRC_ID
      integer, intent(in) :: DST_ID
      integer, optional, intent(out) :: rc

      integer :: status
      character(len=ESMF_MAXSTR) :: IAm = "MAPL_AddConnectivityRename"

      _FAIL("Time to port to MAPL3")
   end subroutine MAPL_AddConnectivityRename

   subroutine MAPL_AddConnectivityRenameMany(gc, SRC_NAME, SRC_ID, &
        DST_NAME, DST_ID, rc)
      type(ESMF_GridComp), intent(inout) :: gc
      character(len=*), intent(in) :: SRC_NAME(:)
      character(len=*), intent(in) :: DST_NAME(:)
      integer, intent(in) :: SRC_ID
      integer, intent(in) :: DST_ID
      integer, optional, intent(out) :: rc

      integer :: status
      character(len=ESMF_MAXSTR) :: IAm = "MAPL_AddConnectivityRenameMany"

      _FAIL("Time to port to MAPL3")
   end subroutine MAPL_AddConnectivityRenameMany

   subroutine MAPL_AddConnectivityMany(gc, SHORT_NAME, SRC_ID, DST_ID, rc)
      type(ESMF_GridComp), intent(inout) :: gc
      character(len=*), intent(in) :: SHORT_NAME(:)
      integer, intent(in) :: SRC_ID
      integer, intent(in) :: DST_ID
      integer, optional, intent(out) :: rc

      integer :: status
      character(len=ESMF_MAXSTR) :: IAm = "MAPL_AddConnectivityMany"

      _FAIL("Time to port to MAPL3")
   end subroutine MAPL_AddConnectivityMany

   subroutine MAPL_DoNotDeferExport(gc, NAMES, rc)
      type(ESMF_GridComp), intent(inout) :: gc
      character(len=*), intent(in) :: NAMES(:)
      integer, optional, intent(out) :: rc

      integer :: status
      character(len=ESMF_MAXSTR) :: IAm = "MAPL_DoNotDeferExport"

      _FAIL("Time to port to MAPL3")
   end subroutine MAPL_DoNotDeferExport

end module MAPL_GenericMod
