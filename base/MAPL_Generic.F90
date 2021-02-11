#include "MAPL_Exceptions.h"
#include "MAPL_ErrLog.h"
#define GET_POINTER ESMFL_StateGetPointerToData
#define MAPL_MAX_PHASES 10

! MAT: The ftell function is a non-portable Fortran
!      extension. While the 32-bit function has a
!      common name, the 64-bit does not. So we select
!      the correct Intel or PGI function, and default
!      to the 32-bit. Note, this will present problems
!      if a >2gb file is read with PARALLEL_READFORCING.

#if defined(__INTEL_COMPILER)
# define _FTELL ftelli8
#elif defined(__PGI)
# define _FTELL ftell64
#else
# define _FTELL ftell
#endif

#include "unused_dummy.H"

!=============================================================================


module MAPL_GenericMod

!BOP
! !MODULE: MAPL_GenericMod
!
! !DESCRIPTION:  MAPL\_Generic allows the user to easily build ESMF gridded
!  components.  It has its own SetServices, Initialize, Run, and Finalize
!  (IRF) methods, and thus is itself a valid gridded component, although somewhat
!  non-standard since it makes its IRF methods public. An instance of 
!  MAPL\_Generic does no useful work, but can be used as a null MAPL\_Generic component.
!
!  The standard way to use MAPL\_Generic is as an aid in building ESMF gridded
!  components. A MAPL/ESMF gridded component built in this way will always have
!  its own SetServices, which will call the subroutine MAPL\_GenericSetServices.
!  When MAPL\_GenericSetServices is called it sets the
!  component's IRF methods to the generic versions, MAPL\_GenericInitialize, MAPL\_GenericFinalize, and
!  MAPL\_GenericRun.  Any (or all) of
!  these may be used as default methods by a gridded component. (As we will see below, 
!  using all three default IRF methods in this way need not be equivalent to instanciating
!  a null component.) If for any of the
!  three IRF methods the default version is inadequate, it can simply be overrided
!  by having the component register its own method after the call to MAPL\_GenericSetServices.

!  The generic IRF methods perform a number of useful functions, including 
!  creating, allocating, and initializing the components Import, Export, 
!  and Internal states. It would be a shame to waste this capability when a component
!  needs to write its own version of an IRF method. A common situation is that the component wants support
!  in performing these functions, but needs to do some (usually small) additional specialized
!  work; for example, it may need to do some special initializations. In this case,
!  one would write a light version of the IRF method that does the specialized work
!  and {\it calls directly} the corresponding MAPL\_Generic method to do the boilerplate.
!  This is why MAPL\_Generic, unlike a standard ESMF gridded component, makes its 
!  IRF methods public and why we added the ``Generic'' modifier (i.e., MAPL\_GenericInitialize,
!  rather than MAPL\_Initialize), to emphasize that they are directly callable IRF methods. 
!  
!  MAPL\_Generic may also be viewed as a fairly standard Fortran 90 ``class,'' which
!  defines and makes public an opaque object that we refer to as a ``MAPL\_Generic State.'' 
!  This object can be created only in association with a standard ESMF Gridded Component (GC),
!  by making a MAPL\_GenericSetServices call.  This object can be obtained through an ESMF GC method
!  which is currently provided with MAPL. The MAPL\_Generic State is, therefore, just another thing that
!  lives in the ESMF GC, like the grid and the configuration. The MAPL\_Generic State
!  is private, but user components can access its contents through public
!  MAPL\_Generic methods (Get, Set, etc). The bulk of MAPL\_Generic consists of methods that act
!  on this object.
!
!  MAPL\_GenericSetServices and MAPL\_Generic IRF methods cannot create their own ESMF grid.
!  The grid must be inherited from the parent or created by the component
!  either in its own SetServices or in its Initialize, if it is writing one. 
!  In any case, an important assumption of MAPL is that the grid must  already be {\it present
!  in the component and initialized} when MAPL\_GenericSetServices is invoked.
!  The same is true of the configuration.
!
!  In MAPL\_Generic, we distinguish between {\em simple (leaf)}
!  gridded compnents and {\em composite} gridded components, which contain other
!  ({\em child}) gridded components.  We also define three types of services,
!  which can be registered by the component's SetServices routine.

!  \begin{itemize}
!    \item {\bf Functional services}: 
!                   These are the standard EMSF callable IRF methods for
!         the component.
!
!    \item {\bf Data services:}
!                   These are descriptions of the component's import, export,
!         and internal states, which can be manipulated by MAPL\_Generic.
!
!    \item {\bf Child services:} 
!                   These are the services of the component's children and 
!         their connectivity.
!
!    \item {\bf Profiling Services:}
!                   These are profiling counters (clocks) that can be used
!         by the component and are automatically reported by generic finalize.
!  \end{itemize}

!   MAPL\_GenericSetServices provides generic versions of all these, as described below.


! !USES:

  use ESMF
  use ESMFL_Mod

  use pFIO
  use gFTL_StringVector
  use pFIO_ClientManagerMod
  use MAPL_BaseMod
  use MAPL_IOMod
  use MAPL_ProfMod
  use MAPL_Profiler
  use MAPL_MemUtilsMod
  use MAPL_CommsMod
  use MAPL_ConstantsMod
  use MAPL_SunMod
  use MaplGeneric
  use MAPL_VarSpecMod
  use MAPL_GenericCplCompMod
  use MAPL_LocStreamMod
  use MAPL_ConfigMod
  use MAPL_ExceptionHandling
  use MAPL_KeywordEnforcerMod
  use MAPL_StringTemplate
  use mpi
  use netcdf
  use pFlogger, only: logging, Logger
  use, intrinsic :: ISO_C_BINDING
  use, intrinsic :: iso_fortran_env, only: REAL32, REAL64, int32, int64
  use, intrinsic :: iso_fortran_env, only: OUTPUT_UNIT

  ! !PUBLIC MEMBER FUNCTIONS:
  
  implicit none
  private
  
  public MAPL_GenericSetServices
  public MAPL_GenericInitialize
  public MAPL_GenericRunChildren
  public MAPL_GenericFinalize
  
  public MAPL_AddInternalSpec
  public MAPL_AddImportSpec
  public MAPL_AddExportSpec

  public MAPL_DoNotDeferExport
  
  public MAPL_GridCompSetEntryPoint
  public MAPL_GetObjectFromGC
  public MAPL_Get
  public MAPL_Set
  public MAPL_InternalStateCreate
  public MAPL_GenericRunCouplers
  
  public MAPL_ChildAddAttribToImportSpec
  !public MAPL_StateGetSpecAttrib
  !public MAPL_StateSetSpecAttrib
  !public MAPL_StateGetVarSpecs
  !public MAPL_StatePrintSpec
  public MAPL_StatePrintSpecCSV

  ! MAPL_Connect
  public MAPL_AddChild
  public MAPL_AddConnectivity
  public MAPL_TerminateImport
  
  ! MAPL_Util
  !public MAPL_GenericStateClockOn
  !public MAPL_GenericStateClockOff
  !public MAPL_GenericStateClockAdd
  public MAPL_TimerOn
  public MAPL_TimerOff
  public MAPL_TimerAdd
  public MAPL_GetResource
  public MAPL_ReadForcing

!EOP  
  ! Internal public
  
  public MAPL_StateAlarmAdd
  public MAPL_StateAlarmGet
  public MAPL_StateCreateFromSpec
  public MAPL_StateCreateFromSpecNew
  public MAPL_FriendlyGet
  public MAPL_GridCompGetFriendlies
  public MAPL_SetVarSpecForCC
  public MAPL_DateStampGet
  public MAPL_ExchangeGridGet
  public MAPL_ExchangeGridSet
  public MAPL_ImportStateGet
  public MAPL_InternalESMFStateGet
  public MAPL_ExportStateGet
  public MAPL_GetChildLocstream
  public MAPL_CopyFriendliness
  public MAPL_VerifyFriendly
  public MAPL_GenericMakeXchgNatural
  public MAPL_GridCreate
  public MAPL_GenericRefresh
  public MAPL_AddRecord
  public MAPL_DisableRecord
  public MAPL_RecordAlarmIsRinging
  public MAPL_GenericRecord
  public MAPL_GetAllExchangeGrids
  public MAPL_DoNotAllocateImport
  public MAPL_DoNotAllocateInternal
  public MAPL_GCGet
  public MAPL_CheckpointState
  public MAPL_ESMFStateReadFromFile
  public MAPL_InternalStateRetrieve
  public :: MAPL_GetLogger
  public MAPL_SetStateSave
  public MAPL_DestroyStateSave
  public MAPL_GenericStateSave
  public MAPL_GenericStateRestore
!BOP  
  ! !PUBLIC TYPES:

  public MAPL_MetaComp
!  \ev
!  This defines the MAPL\_Generic class. It is an opaque object that
!  can be queried using {\tt MAPL\_GenericStateGet}. An instance of
!  this type is placed in the default internal state location of the
!  ESMF gridded component by 
!  {\tt MAPL\_GenericSetServices}. This instance can be retreived by using  
!  {\tt MAPL\_InternalStateGet}.\bv

!EOP
  
  interface MAPL_AddChild
     module procedure MAPL_AddChildFromGC
     module procedure MAPL_AddChildFromMeta
     module procedure MAPL_AddChildFromDSO
  end interface
  
  interface MAPL_AddImportSpec
     module procedure MAPL_StateAddImportSpec_
     module procedure MAPL_StateAddImportSpecFrmChld
  end interface
  
  interface MAPL_AddInternalSpec
     module procedure MAPL_StateAddInternalSpec
  end interface
  
  interface MAPL_AddExportSpec
     module procedure MAPL_StateAddExportSpec_
     module procedure MAPL_StateAddExportSpecFrmChld
     module procedure MAPL_StateAddExportSpecFrmAll
  end interface
  
  interface MAPL_Get
     module procedure MAPL_GenericStateGet
  end interface
  
  interface MAPL_Set
     module procedure MAPL_GenericStateSet
     module procedure MAPL_GenericStateSetFromGC
  end interface
  
  interface MAPL_GetObjectFromGC
     module procedure MAPL_InternalStateGet
  end interface

  interface MAPL_GCGet
     module procedure MAPL_GCGet
  end interface
  
  interface MAPL_TimerOn
     module procedure MAPL_GenericStateClockOn
  end interface
  
  interface MAPL_TimerOff
     module procedure MAPL_GenericStateClockOff
  end interface
  
  interface MAPL_TimerAdd
     module procedure MAPL_GenericStateClockAdd
  end interface
  
  
  interface MAPL_TerminateImport
     module procedure MAPL_DoNotConnect
     module procedure MAPL_DoNotConnectMany
     module procedure MAPL_DoNotConnectAnyImport
     module procedure MAPL_TerminateImportAllBut
     module procedure MAPL_TerminateImportAll
  end interface
  
  interface MAPL_AddConnectivity
     ! module procedure MAPL_AddConnectivityE2E
     module procedure MAPL_AddConnectivityRename
     module procedure MAPL_AddConnectivityRenameMany
     module procedure MAPL_AddConnectivityMany
     ! module procedure MAPL_AddConnectivityOld
  end interface
  
  interface  MAPL_GridCompGetFriendlies
     module procedure MAPL_GridCompGetFriendlies0
     module procedure MAPL_GridCompGetFriendlies1
     module procedure MAPL_GridCompGetFriendlies2
     module procedure MAPL_GridCompGetFriendlies3
  end interface
  
  interface  MAPL_GetResource
     module procedure MAPL_GetResource_scalar
     module procedure MAPL_GetResource_array
  end interface
  
  interface MAPL_CopyFriendliness
     module procedure MAPL_CopyFriendlinessInField
     module procedure MAPL_CopyFriendlinessInState
  end interface
  
  interface MAPL_VerifyFriendly
     module procedure MAPL_VerifyFriendlyInField
     module procedure MAPL_VerifyFriendlyInState
  end interface
  
  interface MAPL_ReadForcing
     module procedure MAPL_ReadForcing1
     module procedure MAPL_ReadForcing2
  end interface
  
  interface MAPL_CheckpointState
     module procedure MAPL_ESMFStateWriteToFile
  end interface MAPL_CheckpointState

  interface MAPL_GetLogger
     module procedure MAPL_GetLogger_gc
     module procedure MAPL_GetLogger_meta
  end interface MAPL_GetLogger

  
! =======================================================================


integer, parameter :: LAST_ALARM = 99

character(len=*), parameter :: CF_COMPONENT_SEPARATOR = '.'

type MAPL_GenericWrap
   type(MAPL_MetaComp       ), pointer :: MAPLOBJ
end type MAPL_GenericWrap

type MAPL_GenericRecordType
   type(ESMF_Alarm), pointer                :: ALARM(:)
   integer, pointer                         :: FILETYPE(:)
   character (len=ESMF_MAXSTR)              :: IMP_FNAME
   integer                                  :: IMP_LEN
   character (len=ESMF_MAXSTR)              :: INT_FNAME
   integer                                  :: INT_LEN
end type  MAPL_GenericRecordType

type MAPL_InitialState
   integer                                  :: FILETYPE = MAPL_Write2Ram
   character(len=:), allocatable            :: IMP_FNAME
   character(len=:), allocatable            :: INT_FNAME
end type  MAPL_InitialState


type MAPL_Connectivity
   type (MAPL_VarConn), pointer :: CONNECT(:)       => null()
   type (MAPL_VarConn), pointer :: DONOTCONN(:)     => null()
end type MAPL_Connectivity

type MAPL_LinkType
   type (ESMF_GridComp) :: GC
   integer              :: StateType
   integer              :: SpecId
end type MAPL_LinkType

type MAPL_LinkForm
   type (MAPL_LinkType) :: FROM
   type (MAPL_LinkType) :: TO
end type MAPL_LinkForm

type MAPL_Link
   type (MAPL_LinkForm), pointer :: PTR
end type MAPL_Link

!BOP
!BOC
type, extends(MaplGenericComponent) ::  MAPL_MetaComp
   private
   ! Move to Base ?
   character(len=ESMF_MAXSTR)               :: COMPNAME
   type (ESMF_Config             )          :: CF
   character(:), allocatable :: full_name ! Period separated list of ancestor names
   real                                     :: HEARTBEAT

   ! Move to decorator?
   type (TimeProfiler), public :: t_profiler

   ! Couplers and connectivity
   type (ESMF_CplComp            ), pointer :: CCS(:,:)         => null()
   type (ESMF_State              ), pointer :: CIM(:,:)         => null()
   type (ESMF_State              ), pointer :: CEX(:,:)         => null()
   logical,                         pointer :: CCcreated(:,:)   => null()
   type (MAPL_Link)               , pointer :: LINK(:)          => null()
   type (MAPL_Connectivity)                 :: connectList

   ! Obsolescent
   character(len=ESMF_MAXSTR)     , allocatable :: GCNameList(:)
   type (MAPL_Prof               ), pointer :: TIMES(:)         => null()
   integer                        , pointer :: phase_init (:)    => null()
   integer                        , pointer :: phase_run  (:)    => null()
   integer                        , pointer :: phase_final(:)    => null()
   integer                        , pointer :: phase_record(:)   => null()
   integer                        , pointer :: phase_coldstart(:)=> null()

   ! Make accessors?
   type(ESMF_GridComp)                      :: RootGC
   type(ESMF_GridComp)            , pointer :: parentGC         => null()

   type (ESMF_Alarm              )          :: ALARM(0:LAST_ALARM)
   integer                                  :: ALARMLAST=0
   type (ESMF_Clock              )          :: CLOCK

   type (MAPL_SunOrbit           )          :: ORBIT

   ! Odd ordering suport.  Needs thought
   logical                                  :: ChildInit = .true.

   ! Migrate to MaplGrid?
   type (MAPL_LocStream)                    :: ExchangeGrid
   type (MAPL_LocStream)                    :: LOCSTREAM

   ! Intermediate checkpointing and replay
   type (MAPL_GenericRecordType)  , pointer :: RECORD           => null()

   ! We don't know what this is for.
   type (MAPL_InitialState)                 :: initial_state

   ! Buffering prev/next buffers.
   ! Could become ExtData if Tiles could be handled???
   type (ESMF_State)                        :: FORCING

contains

   procedure :: get_ith_child

   procedure :: get_child_gridcomp
   procedure :: get_child_import_state
   procedure :: get_child_export_state

end type MAPL_MetaComp
!EOC
!EOP

type MAPL_MetaPtr
   type(MAPL_MetaComp), pointer  :: PTR
end type MAPL_MetaPtr

character(*), parameter :: SEPARATOR = '.'

contains

#define LOWEST_(c) m=0; do while (m /= c) ;\
 m = c; c=label(c);\
enddo

!=============================================================================
!=============================================================================
!=============================================================================
!=============================================================================



!BOPI
! !IROUTINE: MAPL_GenericSetServices

! !INTERFACE:
recursive subroutine MAPL_GenericSetServices ( GC, RC )

  !ARGUMENTS:
  type(ESMF_GridComp),                  intent(INOUT) :: GC  ! Gridded component
  integer,                              intent(  OUT) :: RC  ! Return code

! !DESCRIPTION: {\tt MAPL\_GenericSetServices} performs the following tasks:

!\begin{itemize}
!\item
!  Allocate an instance of MAPL\_GenericState, wrap it, and set it as the
!  GC's internal state.
!\item
!  Exract the grid and configuration from the GC and save them in the 
!  generic state.
!\item
!  Set GC's IRF methods to the generic versions
!\item
!  If there are children
!\begin{itemize}
!\item
!   Allocate a gridded comoponent and an import and export state for each child
!\item
!   Create each child's GC using the natural grid and the inherited configuration.
!\item
!  Create each child's Import and Export states. These are named
!  {\tt GCNames(I)//"\_IMPORT"} and {\tt GCNames(I)//"\_EXPORT"}
!\item
!   Invoke each child's set services.
!\item
!   Add each item in each child's export state to GC's export state.
!\item
!   Add each item in each child's import state to GC's import, 
!   eliminating duplicates.  
!\end{itemize}
!\end{itemize}
! Since {\tt MAPL\_GenericSetServices} calls SetServices for the children,
! which may be generic themselves, the routine must be recursive.
!
! The optional arguments describe the component's children. There can be any 
!  number of children but they must be of one of the types specified by the
!  five SetServices entry points passed. If SSptr is not specified there can
!  only be five children, one for each {\tt SSn}, and the names must be in
!  {\tt SSn} order.
!EOPI


! ErrLog Variables
!-----------------

character(len=ESMF_MAXSTR)        :: IAm
character(len=ESMF_MAXSTR)        :: COMP_NAME
integer                           :: STATUS

! Local variables
! ---------------

type (MAPL_MetaComp), pointer     :: MAPLOBJ
integer                           :: I
integer                           :: NC
integer                           :: TS
integer                           :: LBL, K, M
integer                           :: fLBL, tLBL
integer                           :: good_label, bad_label
integer, pointer                  :: LABEL(:)
type(StateSpecification) :: specs
type(ESMF_Field), pointer         :: FIELD
type(ESMF_FieldBundle), pointer   :: BUNDLE
type(ESMF_State), pointer         :: STATE
type(ESMF_VM)                     :: VM

type (MAPL_VarConn),      pointer :: CONNECT(:)
type (MAPL_VarSpec),      pointer :: IM_SPECS(:)
type (MAPL_VarSpec),      pointer :: EX_SPECS(:)
type (MAPL_VarSpecPtr),   pointer :: ImSpecPtr(:)
type (MAPL_VarSpecPtr),   pointer :: ExSpecPtr(:)
type(ESMF_GridComp)               :: rootGC
type(ESMF_GridComp), pointer :: gridcomp

!=============================================================================

! Begin...

! Get the target components name and set-up traceback handle.
! -----------------------------------------------------------

   Iam = "MAPL_GenericSetServices"
   call ESMF_GridCompGet( GC, name=COMP_NAME, VM=VM, RC=STATUS )
   _VERIFY(STATUS)
   Iam = trim(COMP_NAME) // trim(Iam)

! Create the generic state, intializing its configuration and grid.
!  The parents grid is probably not defined at this point
!----------------------------------------------------------

   call MAPL_InternalStateRetrieve( GC, MAPLOBJ, RC=STATUS)
   _VERIFY(STATUS)

   call MAPLOBJ%t_profiler%start('GenSetService',__RC__)

! Set the Component's Total timer
! -------------------------------

    call MAPL_GenericStateClockAdd(GC,    name="TOTAL"  ,RC=STATUS)
    _VERIFY(STATUS)

! Fill in the rootGC
!-------------------
    call MAPL_GetRootGC(GC, rootGC, rc=status)
    _VERIFY(STATUS)
    MAPLOBJ%rootGC = rootGC

! If this is a composite component, Setup the children
! ----------------------------------------------------

   CHILDREN: if(allocated(MAPLOBJ%GCNameList)) then

      NC = MAPLOBJ%get_num_children()
    do I=1,NC
       call MAPL_GenericStateClockAdd(GC, name=trim(MAPLOBJ%GCNameList(I))  ,RC=STATUS)
       _VERIFY(STATUS)
    end do


! The child should've been already created by MAPL_AddChild
! and set his services should've been called.
! -------------------------------------

! Create internal couplers and composite
! component's Im/Ex specs.
!---------------------------------------

      call MAPL_WireComponent(GC, RC=STATUS)
      _VERIFY(STATUS)

! Relax connectivity for non-existing imports
      if (NC > 0) then
         
         CONNECT => MAPLOBJ%connectList%CONNECT

         allocate (ImSpecPtr(NC), ExSpecPtr(NC), stat=status)
         _VERIFY(STATUS)

         DO I = 1, NC
            gridcomp => Maplobj%get_child_gridcomp(i)
            call MAPL_GridCompGetVarSpecs(gridcomp, &
                 IMPORT=IM_SPECS, EXPORT=EX_SPECS, RC=STATUS)
            _VERIFY(STATUS)
            ImSpecPtr(I)%Spec => IM_SPECS
            ExSpecPtr(I)%Spec => EX_SPECS
         END DO

         call MAPL_ConnCheckReq(CONNECT, ImSpecPtr, ExSpecPtr, rc=status)
         _VERIFY(STATUS)

         deallocate (ImSpecPtr, ExSpecPtr)

      end if

! If I am root call Label from here; everybody else
!  will be called recursively from Label
!--------------------------------------------------
      ROOT: if (.not. associated(MAPLOBJ%parentGC)) then

         call MAPL_GenericConnCheck(GC, RC=status)
         _VERIFY(STATUS)

! Collect all IMPORT and EXPORT specs in the entire tree in one list
!-------------------------------------------------------------------

         call MAPL_GenericSpecEnum(GC, SPECS, RC=STATUS)
         _VERIFY(STATUS)

! Label each spec by its place on the list--sort of.
!--------------------------------------------------

         TS = SPECS%var_specs%size()
         allocate(LABEL(TS), STAT=STATUS)
         _VERIFY(STATUS)

         do I = 1, TS
            LABEL(I)=I
         end do

! For each spec...
!-----------------

         do I = 1, TS

!  Get the LABEL attribute on the spec
!-------------------------------------

            call MAPL_VarSpecGet(SPECS%old_var_specs(I), LABEL = LBL, RC = STATUS)
            _VERIFY(STATUS)
            if (LBL <= 0) then
               _RETURN(ESMF_FAILURE)
            endif

! Do something to sort labels???
!-------------------------------

            LOWEST_(LBL)

            if (LBL < I) then
               good_label = LBL
               bad_label  = I
            else
               good_label = I
               bad_label  = LBL
            end if
            label(bad_label) = good_label
         end do

         if (associated(MAPLOBJ%LINK)) then
            do I = 1, size(MAPLOBJ%LINK)
               fLBL = MAPL_LabelGet(MAPLOBJ%LINK(I)%ptr%FROM, RC=STATUS)
               _VERIFY(STATUS)
               tLBL = MAPL_LabelGet(MAPLOBJ%LINK(I)%ptr%TO,   RC=STATUS)
               _VERIFY(STATUS)
               LOWEST_(fLBL)
               LOWEST_(tLBL)

               if (fLBL < tLBL) then
                  good_label = fLBL
                  bad_label  = tLBL
               else
                  good_label = tLBL
                  bad_label  = fLBL
               end if
               label(bad_label) = good_label
            end do
         end if

         K=0
         do I = 1, TS
            LBL = LABEL(I)
            LOWEST_(LBL)
            
            if (LBL == I) then
               K = K+1
            else
               call MAPL_VarSpecGet(SPECS%old_var_specs(LBL), FIELDPTR = FIELD, RC=STATUS  )
               _VERIFY(STATUS)
               call MAPL_VarSpecSet(SPECS%old_var_specs(I), FIELDPTR = FIELD, RC=STATUS  )
               _VERIFY(STATUS)
               call MAPL_VarSpecGet(SPECS%old_var_specs(LBL), BUNDLEPTR = BUNDLE, RC=STATUS  )
               _VERIFY(STATUS)
               call MAPL_VarSpecSet(SPECS%old_var_specs(I), BUNDLEPTR = BUNDLE, RC=STATUS  )
               _VERIFY(STATUS)
               call MAPL_VarSpecGet(SPECS%old_var_specs(LBL), STATEPTR = STATE, RC=STATUS  )
               _VERIFY(STATUS)
               call MAPL_VarSpecSet(SPECS%old_var_specs(I), STATEPTR = STATE, RC=STATUS  )
               _VERIFY(STATUS)
            end if
            
            call MAPL_VarSpecSet(SPECS%old_var_specs(I), LABEL=LBL, RC=STATUS)
            _VERIFY(STATUS)
         end do

         DEALLOCATE(LABEL, STAT=status)
         _VERIFY(STATUS)

      end if ROOT

   end if CHILDREN  !  Setup children

! Register default services for this component if needed
! --------------------------------------------

   if (.not. associated(MAPLOBJ%phase_init)) then
      call MAPL_GridCompSetEntrypoint(GC, ESMF_METHOD_INITIALIZE, MAPL_GenericInitialize,  RC=STATUS)
      _VERIFY(STATUS)
   endif

   if (.not. associated(MAPLOBJ%phase_run)) then
      call MAPL_GridCompSetEntrypoint(GC, ESMF_METHOD_RUN, MAPL_GenericRunChildren,  RC=STATUS)
      _VERIFY(STATUS)
   endif


   if (.not. associated(MAPLOBJ%phase_final)) then
      call MAPL_GridCompSetEntrypoint(GC, ESMF_METHOD_FINALIZE, MAPL_GenericFinalize,  RC=STATUS)
      _VERIFY(STATUS)
   endif

!ALT check record!!!
   if (.not. associated(MAPLOBJ%phase_record)) then
      call MAPL_GridCompSetEntryPoint ( GC, ESMF_METHOD_WRITERESTART, MAPL_GenericRecord, RC=STATUS)
      _VERIFY(STATUS)
   end if
   _ASSERT(size(MAPLOBJ%phase_record)==1,'needs informative message')  !ALT: currently we support only 1 record
   

   if (.not.associated(MAPLOBJ%phase_coldstart)) then
!ALT: this part is not supported yet
!      call MAPL_GridCompSetEntryPoint(GC, ESMF_METHOD_READRESTART, &
!                                      MAPL_Coldstart, RC=status)
!      _VERIFY(STATUS)
   endif

!ALT ATTENTION HERE!!!!

! Timers for generic initialize and finalize
!-------------------------------------------

   call MAPL_GenericStateClockAdd(GC, name="GenInitTot"     ,RC=STATUS)
   _VERIFY(STATUS)
   call MAPL_GenericStateClockAdd(GC, name="--GenInitMine"  ,RC=STATUS)
   _VERIFY(STATUS)
   call MAPL_GenericStateClockAdd(GC, name="GenRunTot"      ,RC=STATUS)
   _VERIFY(STATUS)
   call MAPL_GenericStateClockAdd(GC, name="--GenRunMine"   ,RC=STATUS)
   _VERIFY(STATUS)
   call MAPL_GenericStateClockAdd(GC, name="GenFinalTot"    ,RC=STATUS)
   _VERIFY(STATUS)
   call MAPL_GenericStateClockAdd(GC, name="--GenFinalMine" ,RC=STATUS)
   _VERIFY(STATUS)
   call MAPL_GenericStateClockAdd(GC, name="GenRecordTot"   ,RC=STATUS)
   _VERIFY(STATUS)
   call MAPL_GenericStateClockAdd(GC, name="--GenRecordMine",RC=STATUS)
   _VERIFY(STATUS)
   call MAPL_GenericStateClockAdd(GC, name="GenRefreshTot"   ,RC=STATUS)
   _VERIFY(STATUS)
   call MAPL_GenericStateClockAdd(GC, name="--GenRefreshMine",RC=STATUS)
   _VERIFY(STATUS)

! All done
!---------
   call MAPLOBJ%t_profiler%stop('GenSetService',__RC__)

   _RETURN(ESMF_SUCCESS)

end subroutine MAPL_GenericSetServices

#undef LOWEST_
!=============================================================================
!=============================================================================
!=============================================================================
!=============================================================================
!=============================================================================


!BOPI
! !IROUTINE: MAPL_GenericInitialize -- Initializes the component and its children

! !INTERFACE:
recursive subroutine MAPL_GenericInitialize ( GC, IMPORT, EXPORT, CLOCK, RC )

  !ARGUMENTS:
  type(ESMF_GridComp), intent(INOUT) :: GC     ! Gridded component 
  type(ESMF_State),    intent(INOUT) :: IMPORT ! Import state
  type(ESMF_State),    intent(INOUT) :: EXPORT ! Export state
  type(ESMF_Clock),    intent(INOUT) :: CLOCK  ! The clock
  integer, optional,   intent(  OUT) :: RC     ! Error code:
!EOPI

! ErrLog Variables

  character(len=ESMF_MAXSTR)    :: IAm
  character(len=ESMF_MAXSTR)    :: COMP_NAME
  integer                       :: STATUS
  integer                       :: userRC

! Local derived type aliases

  type (MAPL_MetaComp),pointer     :: STATE 
  type (MaplGrid ),pointer :: MYGRID
  
! Local variables

  type (ESMF_Time  )            :: ringTime
  type (ESMF_TimeInterval)      :: TIMEINT
  type (ESMF_TimeInterval)      :: TSTEP
  type (ESMF_VM)                :: VM
  character(len=ESMF_MAXSTR)    :: FILENAME
  real                          :: DT
  real                          :: DEFDT
  integer                       :: COUNTS(3)
  integer                       :: COMM
  integer                       :: I, J
  integer                       :: NC
  integer                       :: NSUBTILES
  integer                       :: DIMCOUNT
  type (ESMF_Grid)              :: TILEGRID
  type (ESMF_Calendar)          :: cal
  type (ESMF_Alarm)             :: recordAlarm
  type (ESMF_Alarm), allocatable :: R_ALARM(:)
  integer, allocatable          :: R_FILETYPE(:)
  integer, dimension(:), allocatable :: ref_date, ref_time, ref_freq
  integer                       :: NRA, sec
  character(len=ESMF_MAXSTR)    :: AlarmName
  character(len=3)              :: alarmNum
  type(ESMF_Time)               :: CurrTime    ! Current time of the ESMF clock
  type(ESMF_Time)               :: RefTime
  type(ESMF_TimeInterval)       :: Frequency
  character(len=ESMF_MAXSTR)    :: CHILD_NAME
  type(ESMF_Grid)               :: CHLGRID
  type(ESMF_DistGrid)           :: distGRID

  integer                          :: nhms  ! Current Time date and hour/minute
  integer                          :: PHASE
  integer                          :: NUMPHASES
  integer                          :: MAXPHASES
  type (MAPL_MetaPtr), allocatable :: CHLDMAPL(:)
  type (MAPL_MetaComp), pointer    :: PMAPL
  integer                          :: hdr
  integer                          :: DELTSEC
  integer                          :: DTSECS
  type(ESMF_TimeInterval)          :: DELT
  integer                          :: color
  integer                          :: ndes
  integer                          :: num_readers, ny_by_readers
  integer                          :: num_writers, ny_by_writers
  integer, allocatable             :: minindex(:,:)
  integer, allocatable             :: maxindex(:,:)
  integer, pointer                 :: ims(:) => null()
  integer, pointer                 :: jms(:) => null()
  logical                          :: isGridValid
  logical                          :: ChldGridValid
  integer                          :: reference_date
  integer                          :: reference_time
  integer                          :: yyyymmdd, hhmmss
  integer                          :: year, month, day, hh, mm, ss
  character(len=ESMF_MAXSTR)       :: gridTypeAttribute
  character(len=ESMF_MAXSTR)       :: tmp_label, FILEtpl
  character(len=ESMF_MAXSTR)       :: id_string
  integer                          :: ens_id_width
  real(ESMF_KIND_R8)               :: fixedLons, fixedLats
  type(ESMF_GridComp)              :: GCCS ! this is needed as a workaround 
                                           ! for recursive ESMF method within method
                                           ! calls (see ESMF bug 3004440). 
                                           ! Only coldstart is affected
  logical                          :: isPresent
  logical                          :: isCreated
  logical                          :: gridIsPresent
  character(len=ESMF_MAXSTR)       :: write_restart_by_face
  character(len=ESMF_MAXSTR)       :: read_restart_by_face
  character(len=ESMF_MAXSTR)       :: write_restart_by_oserver
  type(ESMF_GridComp), pointer :: gridcomp
  type(ESMF_State), pointer :: child_import_state
  type(ESMF_State), pointer :: child_export_state
  type(ESMF_State), pointer :: internal_state
!=============================================================================

! Begin...

! Get the target components name and set-up traceback handle.
! -----------------------------------------------------------

  Iam = "MAPL_GenericInitialize"
  call ESMF_GridCompGet( GC, NAME=COMP_NAME, RC=STATUS )
  _VERIFY(STATUS)
  Iam = trim(COMP_NAME) // trim(Iam)

! Retrieve the pointer to the internal state.
! -------------------------------------------

  call MAPL_InternalStateGet ( GC, STATE, RC=STATUS)
  _VERIFY(STATUS)

! Start my timer
!---------------
  
  call state%t_profiler%start('GenInitialize',__RC__)

  call MAPL_GenericStateClockOn(STATE,"TOTAL")
  call MAPL_GenericStateClockOn(STATE,"GenInitTot")
  call MAPL_GenericStateClockOn(STATE,"--GenInitMine")
  call state%t_profiler%start('GenInitialize_self',__RC__)

! Put the inherited grid in the generic state
!--------------------------------------------

  MYGRID    =>  STATE%GRID

  call ESMF_VmGetCurrent(VM, rc=status)
  _VERIFY(STATUS)
  call ESMF_VmGet(VM, localPet=MYGRID%MYID, petCount=ndes, rc=status)
  _VERIFY(STATUS)
  call ESMF_VmGet(VM, mpicommunicator=comm, rc=status)
  _VERIFY(STATUS)

  isGridValid = .false.
  call ESMF_GridCompGet( GC, gridIsPresent=gridIsPresent, RC=STATUS )
  _VERIFY(STATUS) 
  
  if (gridIsPresent) then
     call ESMF_GridCompGet( GC, GRID = MYGRID%ESMFGRID, RC=STATUS )
     _VERIFY(STATUS)

     isCreated = ESMF_GridIsCreated(MYGRID%ESMFGRID, RC=STATUS)
     _VERIFY(STATUS)
     if (isCreated) then
        call ESMF_GridValidate(MYGRID%ESMFGRID, RC=STATUS)
        _VERIFY(STATUS)

        isGridValid = .true.
     end if
  end if

! At this point, this component must have a valid grid!
!------------------------------------------------------
  if (isGridValid) then
! Check children's grid. If they don't have a valid grid yet, put this one in their GC
! ------------------------------------------------------------------------------------
      do I=1, STATE%get_num_children()
         chldGridValid = .false.
         gridcomp => STATE%GET_CHILD_GRIDCOMP(I)
         call ESMF_GridCompGet(gridcomp, gridIsPresent=gridIsPresent, rc=status)
         _VERIFY(STATUS) 
         if (gridIsPresent) then
            call ESMF_GridCompGet(gridcomp, grid=ChlGrid, rc=status)
            _VERIFY(STATUS) 
            call ESMF_GridValidate(ChlGrid, RC=STATUS)
            if (STATUS == ESMF_SUCCESS) then
               chldGridValid = .true.
            end if
         end if
         if (.not. chldGridValid) then
            ! This child does not have a valid grid
            call ESMF_GridCompSet( gridcomp, GRID = MYGRID%ESMFGRID, RC=STATUS )
            _VERIFY(STATUS)
         end if
      end do

! We keep these in the component's grid  for convenience
!-------------------------------------------------------

  call ESMF_GridGet(MYGRID%ESMFGRID, DistGrid=distgrid, dimCount=dimCount, RC=STATUS)
  _VERIFY(STATUS)
  call ESMF_DistGridGet(distGRID, deLayout=MYGRID%LAYOUT, RC=STATUS)
  _VERIFY(STATUS)

! Vertical coordinate must exist and be THE THIRD DIMENSION
! ---------------------------------------------------------

  MYGRID%VERTDIM = 3

  call MAPL_GridGet(MYGRID%ESMFGRID, localCellCountPerDim=COUNTS, RC=STATUS)
  _VERIFY(STATUS)

#ifdef DEBUG
  print *,'dbg:myId=',MYGRID%MYID,trim(Iam)
  print *,'dbg:local gridcounts=',counts
#endif

! Local sizes of three dimensions
!--------------------------------

  MYGRID%IM = COUNTS(1)
  MYGRID%JM = COUNTS(2)
  MYGRID%LM = COUNTS(3)

  call MAPL_GridGet(MYGRID%ESMFGRID, globalCellCountPerDim=COUNTS, RC=STATUS)
  _VERIFY(STATUS)

  MYGRID%IM_WORLD = COUNTS(1)
  MYGRID%JM_WORLD = COUNTS(2)
  
  allocate(minindex(dimCount,ndes), maxindex(dimCount,ndes), stat=status)
  _VERIFY(STATUS)

! Processors in each direction
!-----------------------------

  call MAPl_DistGridGet(distgrid, &
       minIndex=minindex, &
       maxIndex=maxindex, rc=status)
  _VERIFY(STATUS)

  call MAPL_GetImsJms(Imins=minindex(1,:),Imaxs=maxindex(1,:),&
       Jmins=minindex(2,:),Jmaxs=maxindex(2,:),Ims=ims,Jms=jms,rc=status)
  _VERIFY(STATUS)

  MYGRID%NX = size(ims)
  MYGRID%NY = size(jms)

  allocate(mygrid%i1( MYGRID%nx), mygrid%in( MYGRID%nx)) 
  allocate(mygrid%j1( MYGRID%ny), mygrid%jn( MYGRID%ny)) 

  mygrid%i1 = minindex(1,:mygrid%nx)
  mygrid%in = maxindex(1,:mygrid%nx)
  mygrid%j1 = minindex(2,1:ndes: MYGRID%nx)
  mygrid%jn = maxindex(2,1:ndes: MYGRID%nx)

  deallocate(maxindex, minindex)

! My processor coordinates
!-------------------------

#if 0
  call ESMF_DELayoutGetDELocalInfo(delayout=MYGRID%LAYOUT, de=MYGRID%MYID, coord=DECOUNT, rc=status)
  _VERIFY(STATUS)

  MYGRID%NX0 = DECOUNT(1)
  MYGRID%NY0 = DECOUNT(2)
#else
  MYGRID%NX0 = mod(MYGRID%MYID,MYGRID%NX) + 1
  MYGRID%NY0 = MYGRID%MYID/MYGRID%NX + 1
#endif

  call MAPL_GetResource( STATE, num_readers, Label="NUM_READERS:", &
        default=1, RC=STATUS)
  _VERIFY(STATUS)
  call MAPL_GetResource( STATE, num_writers, Label="NUM_WRITERS:", &
        default=1, RC=STATUS)
  _VERIFY(STATUS)
  call MAPL_GetResource( STATE, write_restart_by_face, Label="WRITE_RESTART_BY_FACE:", &
        default='NO', RC=STATUS)
  _VERIFY(STATUS)
  write_restart_by_face = ESMF_UtilStringUpperCase(write_restart_by_face,rc=status)
  _VERIFY(STATUS)

  call MAPL_GetResource( STATE, write_restart_by_oserver, Label="WRITE_RESTART_BY_OSERVER:", &
        default='NO', RC=STATUS)
  _VERIFY(STATUS)
  write_restart_by_oserver = ESMF_UtilStringUpperCase(write_restart_by_oserver,rc=status)
  _VERIFY(STATUS)

  call MAPL_GetResource( STATE, read_restart_by_face, Label="READ_RESTART_BY_FACE:", &
        default='NO', RC=STATUS)
  _VERIFY(STATUS)
  read_restart_by_face = ESMF_UtilStringUpperCase(read_restart_by_face,rc=status)
  _VERIFY(STATUS)

  if (trim(write_restart_by_oserver) == 'YES') then
    ! reset other choices
    ! io_rank 0 becomes the root 
    num_writers = 1
    write_restart_by_face = 'NO'
    mygrid%write_restart_by_oserver = .true.
  endif

  mygrid%comm = comm
  mygrid%num_readers =  num_readers
  mygrid%num_writers =  num_writers
  if (trim(write_restart_by_face) == 'YES') then
    mygrid%write_restart_by_face = .true.
  endif
  if (trim(read_restart_by_face) == 'YES') then
    mygrid%read_restart_by_face = .true.
  endif

! Y-dir communicators
  color =  MYGRID%NX0
  call MPI_COMM_SPLIT(COMM, color, MYGRID%MYID, mygrid%Ycomm, STATUS)
  _VERIFY(STATUS)

! X-dir communicators
  color = MYGRID%NY0
  call MPI_COMM_SPLIT(COMM, color, MYGRID%MYID, mygrid%Xcomm, STATUS)
  _VERIFY(STATUS)

! READER-communicator
   if( num_readers>MYGRID%ny .or. mod(MYGRID%ny,num_readers)/=0 ) then
       if (MAPL_AM_I_Root(VM)) then
            print *
            print *,'***********************************************************'
            print *,'Error!  NUM_READERS must be <= MYGRID%ny: ',MYGRID%ny
            print *,'    and NUM_READERS must divide evenly into MYGRID%ny'
            print *,'***********************************************************'
            print *
       end if
   endif
   _ASSERT(num_readers<=MYGRID%ny,'needs informative message')
   _ASSERT(mod(MYGRID%ny,num_readers)==0,'needs informative message')
   ny_by_readers = MYGRID%ny/num_readers
   if (mod(MYGRID%MYID,MYGRID%nx*MYGRID%ny/num_readers) == 0) then
      color = 0
   else
      color = MPI_UNDEFINED
   endif
   call MPI_COMM_SPLIT(COMM, color, MYGRID%MYID, mygrid%readers_comm, STATUS)
   _VERIFY(STATUS)
   if (num_readers==MYGRID%ny) then
      mygrid%IOscattercomm = mygrid%Xcomm
   else
      j = MYGRID%NY0 - mod(MYGRID%NY0-1,ny_by_readers)
      call MPI_COMM_SPLIT(COMM, j, MYGRID%MYID, mygrid%IOscattercomm, STATUS)
      _VERIFY(STATUS)
   endif

! WRITER-communicator
   if( num_writers>MYGRID%ny .or. mod(MYGRID%ny,num_writers)/=0 ) then
       if (MAPL_AM_I_Root(VM)) then
            print *
            print *,'***********************************************************'
            print *,'Error!  NUM_WRITERS must be <= MYGRID%ny: ',MYGRID%ny
            print *,'    and NUM_WRITERS must divide evenly into MYGRID%ny'
            print *,'***********************************************************'
            print *
       end if
   endif
   _ASSERT(num_writers<=MYGRID%ny,'needs informative message')
   _ASSERT(mod(MYGRID%ny,num_writers)==0,'needs informative message')
   ny_by_writers = MYGRID%ny/num_writers
   if (mod(MYGRID%MYID,MYGRID%nx*MYGRID%ny/num_writers) == 0) then
      color = 0
   else
      color = MPI_UNDEFINED
   endif
   call MPI_COMM_SPLIT(COMM, color, MYGRID%MYID, mygrid%writers_comm, STATUS)
   _VERIFY(STATUS)
   if (num_writers==MYGRID%ny) then
      mygrid%IOgathercomm = mygrid%Xcomm
   else
      j = MYGRID%NY0 - mod(MYGRID%NY0-1,ny_by_writers)
      call MPI_COMM_SPLIT(COMM, j, MYGRID%MYID, mygrid%IOgathercomm, STATUS)
      _VERIFY(STATUS)
   endif

#ifdef DEBUG
  print *,"dbg: grid global max=",counts
  print *, "NX NY:", MYGRID%NX, MYGRID%NY
  print *,'dbg:NX0 NY0=', MYGRID%NX0, MYGRID%NY0
  print *, "dbg:ims=", ims
  print *, "dbg:jms=", jms
  print *,"========================="
#endif

! Clean up

  deallocate(jms, ims)

! Create and initialize factors saved as ESMF arrays in MYGRID
!-------------------------------------------------------------

  call ESMFL_GridCoordGet(   MYGRID%ESMFGRID, MYGRID%LATS       , &
                             Name     = "Latitude"              , &
                             Location = ESMF_STAGGERLOC_CENTER  , &
                             Units    = ESMFL_UnitsRadians      , &
                             RC       = STATUS                    )
  _VERIFY(STATUS)

  call ESMFL_GridCoordGet(   MYGRID%ESMFGRID, MYGRID%LONS       , &
                             Name     = "Longitude"             , &
                             Location = ESMF_STAGGERLOC_CENTER  , &
                             Units    = ESMFL_UnitsRadians      , &
                             RC       = STATUS                    )
  _VERIFY(STATUS)

  gridTypeAttribute = ''
  call ESMF_AttributeGet(MYGRID%ESMFGRID, name='GridType', isPresent=isPresent, RC=status)
  _VERIFY(STATUS)
  if (isPresent) then
     call ESMF_AttributeGet(MYGRID%ESMFGRID, name='GridType', value=gridTypeAttribute, RC=status)
     _VERIFY(STATUS)
     if (gridTypeAttribute == 'Doubly-Periodic') then

        ! this is special case: doubly periodic grid
        ! we ignore ESMF grid coordinates and set LONS/LATS from resource
        call MAPL_GetResource( STATE, fixedLons, Label="FIXED_LONS:", RC=STATUS)
        _VERIFY(STATUS)
        call MAPL_GetResource( STATE, fixedLats, Label="FIXED_LATS:", RC=STATUS)
        _VERIFY(STATUS)
        MYGRID%LONS = fixedLons * (MAPL_PI_R8/180._REAL64)
        MYGRID%LATS = fixedLats * (MAPL_PI_R8/180._REAL64)
     endif ! doubly-periodic
  end if ! isPresent
  end if ! isGridValid
! Put the clock passed down in the generic state
!-----------------------------------------------

   STATE%CLOCK = CLOCK
   call ESMF_ClockGet(CLOCK, TIMESTEP = DELT, RC=STATUS)
   _VERIFY(STATUS)
   call ESMF_TimeIntervalGet(DELT, S=DELTSEC, RC=STATUS)
   _VERIFY(STATUS)
   _ASSERT(DELTSEC /= 0,'needs informative message')
   STATE%HEARTBEAT = DELTSEC

! We get our calling interval from the configuration,
! set the alarm, and attach it to the callers clock.
! ---------------------------------------------------
 
   call ESMF_ClockGet(clock, calendar = cal, currTime=currTime, timestep=tstep, rc=status)
   _VERIFY(STATUS)
   call ESMF_ConfigGetAttribute( state%CF, DEFDT, Label="RUN_DT:", RC=STATUS)
   _VERIFY(STATUS)

   DTSECS = nint(DEFDT)
   ! Make sure this component clock's DT is multiple of RUN_DT (heartbeat)
   ! It should be the same unless we have create a special clock for this
   ! component
   _ASSERT(MOD(DELTSEC,DTSECS)==0,'needs informative message')

   call MAPL_GetResource( STATE   , DT, Label="DT:", default=DEFDT, RC=STATUS)
   _VERIFY(STATUS)

   _ASSERT(DT /= 0.0,'needs informative message')

   DTSECS = nint(DT)
   ! Make sure this component's DT is multiple of CLOCK's timestep
   _ASSERT(MOD(DTSECS,DELTSEC)==0,'needs informative message')

   call ESMF_TimeIntervalSet(TIMEINT,  S=DTSECS , calendar=cal, RC=STATUS)
   _VERIFY(STATUS)

   ! get current time from clock and create a reference time with optonal override
   call ESMF_TimeGet( currTime, YY = YEAR, MM = MONTH, DD = DAY, H=HH, M=MM, S=SS, rc = STATUS  )
   _VERIFY(STATUS)

   yyyymmdd = year*10000 + month*100 + day
   hhmmss   = HH*10000 + MM*100 + SS

!  Get Alarm reference date and time from resouce, it defaults to midnight of the current day
   call MAPL_GetResource (STATE, reference_date, label='REFERENCE_DATE:', &
        default=yyyymmdd, RC=STATUS )
   _VERIFY(STATUS)

   call MAPL_GetResource (STATE, reference_time, label='REFERENCE_TIME:', &
        default=0, RC=STATUS )
   _VERIFY(STATUS)

   YEAR = reference_date/10000
   MONTH = mod(reference_date,10000)/100
   DAY = mod(reference_date,100)

   HH = reference_time/10000
   MM = mod(reference_time,10000)/100
   SS = mod(reference_time,100)

   call ESMF_TimeSet( ringTime, YY = YEAR, MM = MONTH, DD = DAY, &
        H = HH, M = MM, S = SS, rc = STATUS  )
   _VERIFY(STATUS)

   if (ringTime > currTime) then
      ringTime = ringTime - (INT((ringTime - currTime)/TIMEINT)+1)*TIMEINT
   end if

   ringTime = ringTime-TSTEP ! we back off current time with clock's dt since
                             ! we advance the clock AFTER run method

! make sure that ringTime is not in the past
   do while (ringTime < currTime) 
      ringTime = ringTime + TIMEINT
   end do

   STATE%ALARM(0) = ESMF_AlarmCreate(CLOCK = CLOCK, &
        name = trim(COMP_NAME) // "_Alarm" , &
        RingInterval = TIMEINT  ,  &
        RingTime     = ringTime,  & 
!        Enabled      = .true.   ,  &
        sticky       = .false.  ,  &
        RC           = STATUS      )
   _VERIFY(STATUS)
   if(ringTime == currTime) then
      call ESMF_AlarmRingerOn(STATE%ALARM(0), rc=status); _VERIFY(STATUS)
   end if

! Create tiling for all gridded components with associated LocationStream
! -----------------------------------------------------------------------

   if (MAPL_LocStreamIsAssociated(STATE%LOCSTREAM, RC=STATUS)) then
      NSUBTILES = MAPL_GetNumSubtiles(STATE, RC=STATUS)
      _VERIFY(STATUS)

      call MAPL_LocStreamAdjustNsubtiles(STATE%LocStream, NSUBTILES, RC=STATUS)
      _VERIFY(STATUS)

      call MAPL_LocStreamGet(STATE%LocStream, TILEGRID=TILEGRID, RC=STATUS)
      _VERIFY(STATUS)

   endif

! Copy RECORD struct from parent

   if (associated(STATE%parentGC)) then
      call MAPL_GetObjectFromGC(STATE%parentGC, PMAPL, RC=STATUS)
      _VERIFY(STATUS)
      if (associated(PMAPL%RECORD)) then
         call MAPL_AddRecord(STATE, PMAPL%RECORD%ALARM, PMAPL%RECORD%FILETYPE, RC=STATUS)
         _VERIFY(STATUS)
      end if
   end if
! Add this component's own RECORD

   call ESMF_ConfigFindLabel( STATE%CF, LABEL="RECORD_FREQUENCY:", isPresent=isPresent, RC=STATUS)
   _VERIFY(STATUS)
   if (isPresent) then
      nra = ESMF_ConfigGetLen( STATE%CF, RC = STATUS)
      _ASSERT( NRA > 0,'Empty list is not allowed')

      allocate (ref_date(NRA), ref_time(NRA), ref_freq(NRA), stat=STATUS)
      _VERIFY(STATUS)

      call ESMF_ConfigFindLabel( STATE%CF, LABEL="RECORD_FREQUENCY:", RC=STATUS)
      call ESMF_ConfigGetAttribute( STATE%CF, valueList=ref_freq, count=NRA, RC=STATUS)
      _VERIFY(STATUS)

      call ESMF_ConfigFindLabel( STATE%CF, LABEL="RECORD_REF_DATE:", RC=STATUS)
      _VERIFY(STATUS)
!      _ASSERT(NRA == ESMF_ConfigGetLen(STATE%CF),'needs informative message')
      call ESMF_ConfigGetAttribute( STATE%CF, valueList=ref_date, count=NRA, RC=STATUS)
      _VERIFY(STATUS)

      call ESMF_ConfigFindLabel( STATE%CF, LABEL="RECORD_REF_TIME:", RC=STATUS)
      _VERIFY(STATUS)
!      _ASSERT(NRA == ESMF_ConfigGetLen(STATE%CF),'needs informative message')
      call ESMF_ConfigGetAttribute( STATE%CF, valueList=ref_time, count=NRA, RC=STATUS)
      _VERIFY(STATUS)

      allocate (R_ALARM(NRA), STAT=STATUS)
      _VERIFY(STATUS)

      allocate (R_FILETYPE(NRA), STAT=STATUS)
      _VERIFY(STATUS)

      DO  I = 1, NRA
         write(alarmNum,'(I3.3)') I
         AlarmName = "RecordAlarm" // alarmNum
         call ESMF_ClockGetAlarm(clock, trim(AlarmName), recordAlarm, rc=status)
         if (STATUS/=ESMF_SUCCESS) then
            ! create alarm
            call ESMF_TimeSet( RefTime, YY = ref_date(I)/10000, &
                               MM = mod(ref_date(I),10000)/100, &
                               DD = mod(ref_date(I),100), &
                               H = ref_time(I)/10000, &
                               M = mod(ref_time(I),10000)/100, &
                               S = mod(ref_time(I),100), calendar=cal, rc=status )
if (status /= 0) then
   print *,'Error: ref_date/time ',ref_date(i), ref_time(i)
endif
            _VERIFY(STATUS)

            nhms = ref_freq(I)
            sec = nhms/10000*3600 + mod(nhms,10000)/100*60 + mod(nhms,100)
            call ESMF_TimeIntervalSet( frequency, S=sec, rc=status )
            _VERIFY(STATUS)
            RingTime = RefTime
            if (RingTime < currTime .and. sec /= 0) then
               RingTime = RingTime + (INT((currTime - RingTime)/frequency)+1)*frequency
            endif

            RecordAlarm = ESMF_AlarmCreate( name=trim(AlarmName), clock=clock, RingInterval=Frequency, &
                                             RingTime=RingTime, sticky=.false.,rc=status )
            _VERIFY(STATUS)

            if(ringTime == currTime) then
               call ESMF_AlarmRingerOn(RecordAlarm, rc=status); _VERIFY(STATUS)
            end if

         end if
         R_ALARM(I) = recordAlarm
         R_FILETYPE(I) = MAPL_Write2DIsk ! default
      END DO
      call MAPL_AddRecord(STATE, R_ALARM, R_FILETYPE, RC=STATUS)
      _VERIFY(STATUS)
      deallocate (ref_freq, ref_time, ref_date)
      deallocate(R_FILETYPE, R_ALARM)
   endif

   call MAPL_GetResource( STATE, ens_id_width,         &
                             LABEL="ENS_ID_WIDTH:", default=0, &
                             RC=STATUS)

   if (associated(STATE%RECORD)) then
      call MAPL_GetResource( STATE, FILENAME,         &
                             LABEL="IMPORT_CHECKPOINT_FILE:", &
                             RC=STATUS)
      if(STATUS==ESMF_SUCCESS) then
         STATE%RECORD%IMP_FNAME = FILENAME
         STATE%RECORD%IMP_LEN = LEN_TRIM(FILENAME)
      else
         STATE%RECORD%IMP_LEN = 0
      end if

      id_string=""
      tmp_label = "INTERNAL_CHECKPOINT_FILE:"
      call MAPL_GetResource( STATE   , FILEtpl,         &
                                 LABEL=trim(tmp_label), &
                           RC=STATUS)
      if((STATUS /= ESMF_SUCCESS) .and. ens_id_width > 0) then
         i = len(trim(COMP_NAME))
         id_string = COMP_NAME(i-ens_id_width+1:i)
         tmp_label =COMP_NAME(1:i-ens_id_width)//"_"//trim(tmp_label)
         call MAPL_GetResource( STATE   , FILEtpl,      &
                                 LABEL=trim(tmp_label), &
                           RC=STATUS)
      endif

      if(STATUS==ESMF_SUCCESS) then
         ! if the filename is tempate
         call fill_grads_template(filename,trim(adjustl(FILEtpl)),experiment_id=trim(id_string), nymd=yyyymmdd,nhms=hhmmss,rc=status)
         STATE%RECORD%INT_FNAME = FILENAME
         STATE%RECORD%INT_LEN = LEN_TRIM(FILENAME)
      else
         STATE%RECORD%INT_LEN = 0
      end if
   end if
   call state%t_profiler%stop('GenInitialize_self',__RC__)
   call MAPL_GenericStateClockOff(STATE,"--GenInitMine")

! Initialize the children
! -----------------------

      NC = STATE%get_num_children()
      if (STATE%ChildInit) then
         allocate(CHLDMAPL(NC), stat=status)
         MAXPHASES = 0
         do I=1,NC
            gridcomp => STATE%GET_CHILD_GRIDCOMP(I)
            call MAPL_GetObjectFromGC(gridcomp, CHLDMAPL(I)%PTR, RC=STATUS)
            _VERIFY(STATUS)
            MAXPHASES = MAX(MAXPHASES, SIZE(CHLDMAPL(I)%PTR%PHASE_INIT))
         end do
         if (MAXPHASES > 1) then
            call WRITE_PARALLEL( &
                 "WARNING: multiple INITIALIZE methods detected " // &
                 "for the children of " // &
                 trim(COMP_NAME)// ". " // &
                 "Although this is allowed, MAPL is currently restricted " //&
                 "to the default PHASE=1 and no longer will " // &
                 "automatically execute all of them" )
         end if

         PHASE = 1
         do I=1,NC
            NUMPHASES = SIZE(CHLDMAPL(I)%PTR%PHASE_INIT)
            if (PHASE .le. NUMPHASES) then
               gridcomp => STATE%GET_CHILD_GRIDCOMP(I)
               call ESMF_GridCompGet( gridcomp, NAME=CHILD_NAME, RC=STATUS )
               _VERIFY(STATUS)
      
               call MAPL_GenericStateClockOn (STATE,trim(CHILD_NAME))
               child_import_state => STATE%get_child_import_state(i)
               child_export_state => STATE%get_child_export_state(i)
               call ESMF_GridCompInitialize (gridcomp, &
                    importState=child_import_state, &
                    exportState=child_export_state, &
                    clock=CLOCK, PHASE=CHLDMAPL(I)%PTR%PHASE_INIT(PHASE), &
                    userRC=userRC, RC=STATUS )
               _ASSERT(userRC==ESMF_SUCCESS .and. STATUS==ESMF_SUCCESS,'needs informative message')
               call MAPL_GenericStateClockOff(STATE,trim(CHILD_NAME))
            end if
         end do
         deallocate(CHLDMAPL)

!ALT addition for ExtData component. 
! We are doing this after all children have been initialized
!----------------------------------
      if (.not. isGridValid) then
         if (associated(STATE%COMPONENT_SPEC%import%OLD_VAR_SPECS)) then
            call MAPL_StateCreateFromSpecNew(IMPORT,STATE%COMPONENT_SPEC%IMPORT,RC=STATUS)
            _VERIFY(STATUS)
         end if
      end if

! Initialize all needed couplers
! ---------------------------------------------------

      do I=1,NC
         do J=1,NC
            if(STATE%CCcreated(J,I)) then
!               call WRITE_PARALLEL( "DEBUG: initilaizing CPL in " // &
!                    trim(comp_name) // " for " // &
!                    trim(STATE%GCNameList(J)) // " and " // &
!                    trim(STATE%GCNameList(I)))
               child_export_state => STATE%get_child_export_state(j)
               child_import_state => STATE%get_child_import_state(i)
               call ESMF_CplCompInitialize (STATE%CCS(J,I), &
                    importState=child_export_state, &
                    exportState=child_import_state, &
                    clock=CLOCK, userRC=userRC, RC=STATUS )
               _ASSERT(userRC==ESMF_SUCCESS .and. STATUS==ESMF_SUCCESS,'needs informative message')
            endif
         enddo
! ---------------------------------------------------
      enddo
   endif
   call MAPL_GenericStateClockOn(STATE,"--GenInitMine")
   call state%t_profiler%start('GenInitialize_self',__RC__)

! Create import and initialize state variables
! --------------------------------------------
   if (associated(STATE%COMPONENT_SPEC%IMPORT%OLD_VAR_SPECS) .and. isGridValid) then

      if (MAPL_LocStreamIsAssociated(STATE%LOCSTREAM, RC=STATUS)) then
         call MAPL_StateCreateFromVarSpecNew(IMPORT,STATE%COMPONENT_SPEC%IMPORT,     &
                                       MYGRID%ESMFGRID,              &
                                       TILEGRID=TILEGRID,            &
                                       RC=STATUS       )
      else
         call MAPL_StateCreateFromVarSpecNew(IMPORT,STATE%COMPONENT_SPEC%IMPORT,     &
                                       MYGRID%ESMFGRID,              &
                                       RC=STATUS       )
      endif
      _VERIFY(STATUS)

      call MAPL_GetResource( STATE   , FILENAME,         &
                                 LABEL="IMPORT_RESTART_FILE:", &
                                 RC=STATUS)
      if(STATUS==ESMF_SUCCESS) then

         call MAPL_ESMFStateReadFromFile(IMPORT, CLOCK, FILENAME, &
                                         STATE, .FALSE., RC=STATUS)
         if (STATUS /= ESMF_SUCCESS) then
            if (MAPL_AM_I_Root(VM)) then
               call ESMF_StatePrint(Import)
            end if
         end if
         _VERIFY(STATUS)
      endif
   end if

! Create internal and initialize state variables
! -----------------------------------------------

   internal_state => STATE%get_internal_state()
   internal_state = ESMF_StateCreate(name = trim(COMP_NAME) // "_INTERNAL", __RC__)

   if (associated(STATE%COMPONENT_SPEC%INTERNAL%OLD_VAR_SPECS)) then
      if (MAPL_LocStreamIsAssociated(STATE%LOCSTREAM, RC=STATUS)) then
         call MAPL_StateCreateFromVarSpecNew(internal_state,STATE%COMPONENT_SPEC%INTERNAL, &
                                        MYGRID%ESMFGRID,             &
                                        TILEGRID=TILEGRID,           &
                                        RC=STATUS       )
      else
         call MAPL_StateCreateFromVarSpecNew(internal_state,STATE%COMPONENT_SPEC%INTERNAL, &
                                        MYGRID%ESMFGRID,             &
                                        RC=STATUS       )
      end if
      _VERIFY(STATUS)

      id_string = ""
      tmp_label = "INTERNAL_RESTART_FILE:"
      call MAPL_GetResource( STATE   , FILEtpl,         &
                                 LABEL=trim(tmp_label), &
                                 RC=STATUS)
      if((STATUS /=ESMF_SUCCESS) .and. ens_id_width >0) then
         i = len(trim(COMP_NAME))
         id_string = COMP_NAME(i-ens_id_width+1:i)
         tmp_label =COMP_NAME(1:i-ens_id_width)//"_"//trim(tmp_label)
         call MAPL_GetResource( STATE   , FILEtpl,         &
                                 LABEL=trim(tmp_label), &
                                 RC=STATUS)
      endif

      if(STATUS==ESMF_SUCCESS) then
        ! if the filename is tempate
         call fill_grads_template(filename,trim(adjustl(FILEtpl)),experiment_id=trim(id_string), &
         nymd=yyyymmdd,nhms=hhmmss,rc=status)
         call MAPL_GetResource( STATE   , hdr,         &
                                 default=0, &
                                 LABEL="INTERNAL_HEADER:", &
                                 RC=STATUS)
         _VERIFY(STATUS)
         call MAPL_ESMFStateReadFromFile(internal_state, CLOCK, FILENAME, &
                                         STATE, hdr/=0, RC=STATUS)
         if (STATUS /= ESMF_SUCCESS) then
            if (MAPL_AM_I_Root(VM)) then
               call ESMF_StatePrint(internal_state)
            end if
            _RETURN(ESMF_FAILURE)
         end if
      else
! try to coldstart the internal state 
! -------------------------------
         if (associated(STATE%phase_coldstart)) then
            ! ALT: workaround bug 3004440 in ESMF (fixed in ESMF_5_1_0)
            ! please, do not remove, nor change order until we move to 510 or later
            allocate(GCCS%compp, stat=status)
            _VERIFY(STATUS)
            GCCS%compp = GC%compp
            call ESMF_GridCompReadRestart(GC, importState=import, &
                 exportState=export, clock=CLOCK, userRC=userRC, RC=STATUS)
            GC%compp = GCCS%compp
            deallocate(GCCS%compp)
            _ASSERT(userRC==ESMF_SUCCESS .and. STATUS==ESMF_SUCCESS,'needs informative message')
         endif

      endif
   end if

! Create export state variables
!------------------------------

   if (associated(STATE%COMPONENT_SPEC%EXPORT%OLD_VAR_SPECS)) then
      if (MAPL_LocStreamIsAssociated(STATE%LOCSTREAM, RC=STATUS)) then
         call MAPL_StateCreateFromVarSpecNew(EXPORT,STATE%COMPONENT_SPEC%EXPORT,     &
                                       MYGRID%ESMFGRID,              &
                                       TILEGRID=TILEGRID,            &
                                       DEFER=.true., RC=STATUS       )
      else
         call MAPL_StateCreateFromVarSpecNew(EXPORT,STATE%COMPONENT_SPEC%EXPORT,     &
                                       MYGRID%ESMFGRID,              &
                                       DEFER=.true., RC=STATUS       )
      end if
      _VERIFY(STATUS)
   end if

! Create forcing state
   STATE%FORCING = ESMF_StateCreate(name = trim(COMP_NAME) // "_FORCING", &
                              RC=STATUS)
   _VERIFY(STATUS)

! Put the Export state of each child into my export
! -------------------------------------------------

!ALT: export might have to be declared ESMF_STATELIST
      do i = 1, state%get_num_children()
         child_export_state => state%get_child_export_state(i)
         call ESMF_StateAdd(EXPORT, [child_export_state], RC=STATUS)
         _VERIFY(STATUS)
      end do

   call state%t_profiler%stop('GenInitialize_self',__RC__)
   call MAPL_GenericStateClockOff(STATE,"--GenInitMine")

   if (.not. associated(STATE%parentGC)) then
      call MAPL_AdjustIsNeeded(GC, EXPORT, RC=STATUS)
      _VERIFY(STATUS)
   end if

  call MAPL_GenericStateClockOff(STATE,"GenInitTot")
  call MAPL_GenericStateClockOff(STATE,"TOTAL")

  call state%t_profiler%stop('GenInitialize',__RC__)

! Write Memory Use Statistics.
! -------------------------------------------
  call MAPL_MemUtilsWrite(VM, Iam, RC=STATUS )
  _VERIFY(STATUS)

  _RETURN(ESMF_SUCCESS)

end subroutine MAPL_GenericInitialize

!=============================================================================
!=============================================================================
!=============================================================================
!=============================================================================

subroutine MAPL_GenericWrapper ( GC, IMPORT, EXPORT, CLOCK, RC)

  !ARGUMENTS:
  type(ESMF_GridComp)  :: GC     ! Gridded component 
  type(ESMF_State)     :: IMPORT ! Import state
  type(ESMF_State)     :: EXPORT ! Export state
  type(ESMF_Clock)     :: CLOCK  ! The clock
  integer, intent(OUT) :: RC     ! Error code
  
!EOPI

! ErrLog Variables


  character(len=ESMF_MAXSTR)    :: IAm
  character(len=ESMF_MAXSTR)    :: COMP_NAME
  integer                       :: STATUS
  integer                       :: userRC

! Local derived type aliases

  type (MAPL_MetaComp),pointer :: STATE 

  integer                          :: PHASE
  integer                          :: PHASE_
  integer                          :: I
  type(ESMF_Method_Flag)           :: method
  type(ESMF_VM) :: VM
  class(BaseProfiler), pointer :: t_p
  character(1) :: char_phase

  character(len=12), pointer :: timers(:) => NULL()
! the next declaration assumes all 5 methods have the same signature
! we just picked one of them
  procedure(ESMF_GridCompRun), pointer :: func_ptr => NULL()
  character(len=12), target :: timers_run(2) = &
       [character(len=12):: 'GenRunTot','--GenRunMine'] 
  character(len=12) :: sbrtn


!=============================================================================

! Begin...

  _UNUSED_DUMMY(IMPORT)
  _UNUSED_DUMMY(EXPORT)

! Get the target components name and set-up traceback handle.
! -----------------------------------------------------------

  Iam = "MAPL_GenericWrapper"
  call ESMF_GridCompGet( GC, NAME=COMP_NAME, currentPhase=PHASE, &
       currentMethod=method, RC=STATUS )
  _VERIFY(STATUS)
  Iam = trim(COMP_NAME) // trim(Iam)

  call ESMF_VmGetCurrent(VM)
! Retrieve the pointer to the internal state. It comes in a wrapper.
! ------------------------------------------------------------------

  call MAPL_InternalStateGet ( GC, STATE, RC=STATUS)
  _VERIFY(STATUS)

  phase_ = MAPL_MAX_PHASES+phase ! this is the "actual" phase, i.e. the one user registered

! TIMERS on

  t_p => get_global_time_profiler()

  MethodBlock: if (method == ESMF_METHOD_RUN) then
     func_ptr => ESMF_GridCompRun
     timers => timers_run
     sbrtn = 'Run'
     if (phase > 1) then
        write(char_phase,'(i1)')phase
        sbrtn = 'Run'//char_phase
     end if
  else if (method == ESMF_METHOD_INITIALIZE) then
     func_ptr => ESMF_GridCompInitialize
!ALT: enable this when fully implemented (for now NULLIFY)
!     timers => timers_initialize
     NULLIFY(timers)
     sbrtn = 'Initialize'
  else if (method == ESMF_METHOD_FINALIZE) then
     func_ptr => ESMF_GridCompFinalize
!ALT: enable this when fully implemented (for now NULLIFY)
!     timers => timers_finalize
     NULLIFY(timers)
     sbrtn = 'Finalize'
  else if (method == ESMF_METHOD_READRESTART) then
     func_ptr => ESMF_GridCompReadRestart
!ALT: enable this when fully implemented (for now NULLIFY)
!     timers => timers_readreastart
     NULLIFY(timers)
     sbrtn = 'ReadRestart'
  else if (method == ESMF_METHOD_WRITERESTART) then
     func_ptr => ESMF_GridCompWriteRestart
!ALT: enable this when fully implemented (for now NULLIFY)
!     timers => timers_writereastart
     NULLIFY(timers)
     sbrtn = 'WriteRestart'
  endif MethodBlock

! TIMERS on
  call t_p%start(trim(state%compname),__RC__)
  if (method /= ESMF_METHOD_READRESTART .and. method /= ESMF_METHOD_WRITERESTART) then
     call state%t_profiler%start(__RC__)
     call state%t_profiler%start(trim(sbrtn),__RC__)
  end if



  if (associated(timers)) then
     do i = 1, size(timers)
        call MAPL_TimerOn (STATE,timers(i))
     end do
  end if

  ! Method itself
  ! ----------
#ifdef DEBUG
  IF (mapl_am_i_root(vm)) then
     print *,'DBG: running ', sbrtn, ' phase ',phase,' of ',trim(COMP_NAME)
  end IF
#endif


  call func_ptr (GC, &
       importState=IMPORT, &
       exportState=EXPORT, &
       clock=CLOCK, PHASE=PHASE_, &
       userRC=userRC, RC=STATUS )
  _ASSERT(userRC==ESMF_SUCCESS .and. STATUS==ESMF_SUCCESS,'needs informative message')

  ! TIMERS off
  if (associated(timers)) then
     do i = size(timers),1,-1
        call MAPL_TimerOff (STATE,timers(i))
     end do
  end if

  if (method /= ESMF_METHOD_FINALIZE) then 
      if (method /= ESMF_METHOD_WRITERESTART .and. &
          method /= ESMF_METHOD_READRESTART) then 
        call state%t_profiler%stop(trim(sbrtn),__RC__)
        call state%t_profiler%stop(__RC__)
      end if
     call t_p%stop(trim(state%compname),__RC__)
  endif


  _RETURN(ESMF_SUCCESS)

end subroutine MAPL_GenericWrapper

!=============================================================================
!=============================================================================

!BOPI

! !IROUTINE: MAPL_GenericRunChildren

! !INTERFACE:
recursive subroutine MAPL_GenericRunChildren ( GC, IMPORT, EXPORT, CLOCK, RC)

  !ARGUMENTS:
  type(ESMF_GridComp), intent(INOUT) :: GC     ! Gridded component 
  type(ESMF_State),    intent(INOUT) :: IMPORT ! Import state
  type(ESMF_State),    intent(INOUT) :: EXPORT ! Export state
  type(ESMF_Clock),    intent(INOUT) :: CLOCK  ! The clock
  integer, optional,   intent(  OUT) :: RC     ! Error code:
  
!EOPI

! ErrLog Variables


character(len=ESMF_MAXSTR)    :: IAm
character(len=ESMF_MAXSTR)    :: COMP_NAME
integer                       :: STATUS
integer                       :: userRC

! Local derived type aliases

type (MAPL_MetaComp),pointer :: STATE 

character(len=ESMF_MAXSTR)       :: CHILD_NAME
integer                          :: I, J
integer                          :: NC
integer                          :: PHASE
integer                          :: NUMPHASES
integer                          :: MAXPHASES
type (MAPL_MetaPtr), allocatable :: CHLDMAPL(:)
type(ESMF_GridComp), pointer :: gridcomp
type(ESMF_State), pointer :: child_import_state
type(ESMF_State), pointer :: child_export_state
!=============================================================================

! Begin...

_UNUSED_DUMMY(IMPORT)
_UNUSED_DUMMY(EXPORT)

! Get the target components name and set-up traceback handle.
! -----------------------------------------------------------

Iam = "MAPL_GenericRunChildren"
call ESMF_GridCompGet( GC, NAME=COMP_NAME, RC=STATUS )
_VERIFY(STATUS)
Iam = trim(COMP_NAME) // trim(Iam)

! Retrieve the pointer to the internal state. It comes in a wrapper.
! ------------------------------------------------------------------

call MAPL_InternalStateGet ( GC, STATE, RC=STATUS)
_VERIFY(STATUS)

call MAPL_GenericStateClockOn (STATE,"TOTAL")
!@ call MAPL_GenericStateClockOn (STATE,"GenRunTot")

! Run the children
! ----------------

   NC = STATE%get_num_children()
   allocate(CHLDMAPL(NC), stat=status)
   MAXPHASES = 0
   do I=1,NC
      gridcomp => STATE%GET_CHILD_GRIDCOMP(I)
      call MAPL_GetObjectFromGC(gridcomp, CHLDMAPL(I)%PTR, RC=STATUS)
      _VERIFY(STATUS)
      MAXPHASES = MAX(MAXPHASES, SIZE(CHLDMAPL(I)%PTR%PHASE_RUN))
   end do

   do PHASE = 1, MAXPHASES
      do I=1,NC
         NUMPHASES = SIZE(CHLDMAPL(I)%PTR%PHASE_RUN)
         if (PHASE .le. NUMPHASES) then
            gridcomp => STATE%GET_CHILD_GRIDCOMP(I)
            call ESMF_GridCompGet( gridcomp, NAME=CHILD_NAME, RC=STATUS )
            _VERIFY(STATUS)
      
            call MAPL_GenericStateClockOn (STATE,trim(CHILD_NAME))
            child_import_state => STATE%get_child_import_state(i)
            child_export_state => STATE%get_child_export_state(i)
            call ESMF_GridCompRun (gridcomp, &
                 importState=child_import_state, &
                 exportState=child_export_state, &
                 clock=CLOCK, PHASE=CHLDMAPL(I)%PTR%PHASE_RUN(PHASE), &
                 userRC=userRC, RC=STATUS )
            _ASSERT(userRC==ESMF_SUCCESS .and. STATUS==ESMF_SUCCESS,'needs informative message')
            call MAPL_GenericStateClockOff(STATE,trim(CHILD_NAME))
         end if

!ALT question for Max - if user wants to run particular phase only, when should we run couplers
         if (PHASE == NUMPHASES) then
            do J=1,NC
               if(STATE%CCcreated(I,J)) then
                  child_export_state => STATE%get_child_export_state(i)
                  child_import_state => STATE%get_child_import_state(j)
                  call ESMF_CplCompRun (STATE%CCS(I,J), &
                       importState=child_export_state, &
                       exportState=child_import_state, &
                       clock=CLOCK, userRC=userRC, RC=STATUS)
                  _ASSERT(userRC==ESMF_SUCCESS .and. STATUS==ESMF_SUCCESS,'needs informative message')
               endif
            enddo
         end if
      enddo
   enddo
   deallocate(CHLDMAPL)

!@ call MAPL_GenericStateClockOff(STATE,"GenRunTot")
call MAPL_GenericStateClockOff(STATE,"TOTAL")

_RETURN(ESMF_SUCCESS)

end subroutine MAPL_GenericRunChildren


!BOPI
! !IROUTINE: MAPL_GenericFinalize -- Finalizes the component and its children

! !INTERFACE:
recursive subroutine MAPL_GenericFinalize ( GC, IMPORT, EXPORT, CLOCK, RC )

  !ARGUMENTS:
  type(ESMF_GridComp), intent(inout) :: GC     ! composite gridded component 
  type(ESMF_State),    intent(inout) :: IMPORT ! import state
  type(ESMF_State),    intent(inout) :: EXPORT ! export state
  type(ESMF_Clock),    intent(inout) :: CLOCK  ! the clock
  integer, optional,   intent(  out) :: RC     ! Error code:
                                               ! = 0 all is well
                                               ! otherwise, error
!EOPI

! LOCAL VARIABLES

  character(len=ESMF_MAXSTR)                  :: IAm
  character(len=ESMF_MAXSTR)                  :: COMP_NAME
  integer                                     :: STATUS
  integer                                     :: userRC

  character(len=ESMF_MAXSTR)                  :: FILENAME
  character(len=ESMF_MAXSTR)                  :: FILETYPE
  character(len=ESMF_MAXSTR)                  :: CHILD_NAME
  character(len=ESMF_MAXSTR)                  :: RECFIN
  type (MAPL_MetaComp), pointer               :: STATE
  integer                                     :: I
  logical                                     :: final_checkpoint
  logical                                     :: nwrgt1
  integer                                     :: NC
  integer                                     :: PHASE
  integer                                     :: NUMPHASES
  integer                                     :: MAXPHASES
  type (MAPL_MetaPtr), allocatable            :: CHLDMAPL(:)
  integer                                     :: hdr
  integer                                     :: yyyymmdd, hhmmss
  integer                                     :: year, month, day, hh, mm, ss
  character(len=ESMF_MAXSTR)                  :: tmp_label, FILEtpl
  character(len=ESMF_MAXSTR)                  :: id_string
  integer                                     :: ens_id_width
  type(ESMF_Time)                             :: CurrTime 
  class(BaseProfiler), pointer                :: t_p
  type(ESMF_GridComp), pointer :: gridcomp
  type(ESMF_State), pointer :: child_import_state
  type(ESMF_State), pointer :: child_export_state
  type(ESMF_State), pointer :: internal_state
!=============================================================================

!  Begin...

  _UNUSED_DUMMY(EXPORT)

  Iam = "MAPL_GenericFinalize"
  call ESMF_GridCompGet(GC, name=COMP_NAME, RC=STATUS )
  _VERIFY(STATUS)
  Iam = trim(COMP_NAME) // Iam


! Retrieve the pointer to the state
!----------------------------------

  call MAPL_InternalStateRetrieve(GC, STATE, RC=STATUS)
  _VERIFY(STATUS)

! Finalize the children
! ---------------------

  t_p => get_global_time_profiler()

  call MAPL_GenericStateClockOn(STATE,"TOTAL")
  call MAPL_GenericStateClockOn(STATE,"GenFinalTot")
      NC = STATE%get_num_children()
      allocate(CHLDMAPL(NC), stat=status)
      MAXPHASES = 0
      do I=1,NC
         gridcomp => STATE%GET_CHILD_GRIDCOMP(I)
         call MAPL_GetObjectFromGC(gridcomp, CHLDMAPL(I)%PTR, RC=STATUS)
         _VERIFY(STATUS)
         MAXPHASES = MAX(MAXPHASES, SIZE(CHLDMAPL(I)%PTR%PHASE_FINAL))
      end do

      do PHASE = 1, MAXPHASES
         do I=1,NC
            NUMPHASES = SIZE(CHLDMAPL(I)%PTR%PHASE_FINAL)
            if (PHASE .le. NUMPHASES) then
               gridcomp => STATE%GET_CHILD_GRIDCOMP(I)
               call ESMF_GridCompGet( gridcomp, NAME=CHILD_NAME, RC=STATUS )
               _VERIFY(STATUS)
      
               call MAPL_GenericStateClockOn (STATE,trim(CHILD_NAME))
               child_import_state => STATE%get_child_import_state(i)
               child_export_state => STATE%get_child_export_state(i)
               call ESMF_GridCompFinalize (gridcomp, &
                    importState=child_import_state, &
                    exportState=child_export_state, &
                    clock=CLOCK, PHASE=CHLDMAPL(I)%PTR%PHASE_FINAL(PHASE), &
                    userRC=userRC, RC=STATUS )
               _ASSERT(userRC==ESMF_SUCCESS .and. STATUS==ESMF_SUCCESS,'needs informative message')
               call MAPL_GenericStateClockOff(STATE,trim(CHILD_NAME))
            end if
         enddo
      end do
      deallocate(CHLDMAPL)

  call MAPL_GenericStateClockOn(STATE,"--GenFinalMine")
  call state%t_profiler%start('Final_self',__RC__)

  call MAPL_GetResource( STATE, RECFIN, LABEL="RECORD_FINAL:", &
       RC=STATUS )
  final_checkpoint = .true.
  IF (STATUS == ESMF_SUCCESS) then
     IF (RECFIN == "NO")  final_checkpoint = .false.
  END IF

  if (final_checkpoint) then
! Checkpoint the internal state if required.
!------------------------------------------

     call ESMF_ClockGet (clock, currTime=currTime, rc=status)
     _VERIFY(STATUS)
     call ESMF_TimeGet( currTime, YY = YEAR, MM = MONTH, DD = DAY, H=HH, M=MM, S=SS, rc = STATUS  )
     _VERIFY(STATUS)

     yyyymmdd = year*10000 + month*100 + day
     hhmmss   = HH*10000 + MM*100 + SS

     call MAPL_GetResource( STATE, ens_id_width,         &
                             LABEL="ENS_ID_WIDTH:", default=0, &
                             RC=STATUS)

     id_string=""
     tmp_label = "INTERNAL_CHECKPOINT_FILE:"
     call MAPL_GetResource( STATE   , FILEtpl,         &
                                 LABEL=trim(tmp_label), &
                                 RC=STATUS)
     if((STATUS /= ESMF_SUCCESS) .and. ens_id_width>0) then
        i = len(trim(COMP_NAME))
        id_string = COMP_NAME(i-ens_id_width+1:i)
        tmp_label =COMP_NAME(1:i-ens_id_width)//"_"//trim(tmp_label)
        call MAPL_GetResource( STATE   , FILEtpl,       &
                                 LABEL=trim(tmp_label), &
                                 RC=STATUS)
     endif

     if(STATUS==ESMF_SUCCESS) then
        ! if the filename is tempate
        call fill_grads_template(filename,trim(adjustl(filetpl)),experiment_id=trim(id_string), nymd=yyyymmdd,nhms=hhmmss,rc=status)
        call    MAPL_GetResource( STATE, FILETYPE, LABEL="INTERNAL_CHECKPOINT_TYPE:",                RC=STATUS )
        if ( STATUS/=ESMF_SUCCESS  .or.  FILETYPE == "default" ) then
           call MAPL_GetResource( STATE, FILETYPE, LABEL="DEFAULT_CHECKPOINT_TYPE:", default='pnc4', RC=STATUS )
           _VERIFY(STATUS)
        end if
        FILETYPE = ESMF_UtilStringLowerCase(FILETYPE,rc=STATUS)
        _VERIFY(STATUS)
#ifndef H5_HAVE_PARALLEL
         nwrgt1 = ((state%grid%num_readers > 1) .or. (state%grid%num_writers > 1))
         if(FILETYPE=='pnc4' .and. nwrgt1) then
            print*,trim(Iam),': num_readers and number_writers must be 1 with pnc4 unless HDF5 was built with -enable-parallel'
            _ASSERT(.false.,'needs informative message')
         endif
#endif
        call MAPL_GetResource( STATE   , hdr,         &
                               default=0, &
                               LABEL="INTERNAL_HEADER:", &
                               RC=STATUS)
        _VERIFY(STATUS)
        internal_state => state%get_internal_state()
        call MAPL_ESMFStateWriteToFile(internal_state,CLOCK,FILENAME, &
             FILETYPE, STATE, hdr/=0, oClients = o_Clients, RC=STATUS)
        _VERIFY(STATUS)
     endif

! Checkpoint the import state if required.
!----------------------------------------

     call       MAPL_GetResource( STATE, FILENAME, LABEL="IMPORT_CHECKPOINT_FILE:",                  RC=STATUS )
     if(STATUS==ESMF_SUCCESS) then
        call    MAPL_GetResource( STATE, FILETYPE, LABEL="IMPORT_CHECKPOINT_TYPE:",                  RC=STATUS )
        if ( STATUS/=ESMF_SUCCESS  .or.  FILETYPE == "default" ) then
           call MAPL_GetResource( STATE, FILETYPE, LABEL="DEFAULT_CHECKPOINT_TYPE:", default='pnc4', RC=STATUS )
           _VERIFY(STATUS)
        end if
        FILETYPE = ESMF_UtilStringLowerCase(FILETYPE,rc=STATUS)
        _VERIFY(STATUS)
#ifndef H5_HAVE_PARALLEL
         nwrgt1 = ((state%grid%num_readers > 1) .or. (state%grid%num_writers > 1))
         if(FILETYPE=='pnc4' .and. nwrgt1) then
            print*,trim(Iam),': num_readers and number_writers must be 1 with pnc4 unless HDF5 was built with -enable-parallel'
            _ASSERT(.false.,'needs informative message')
         endif
#endif
        call MAPL_ESMFStateWriteToFile(IMPORT,CLOCK,FILENAME, &
             FILETYPE, STATE, .FALSE., oClients = o_Clients, RC=STATUS)
        _VERIFY(STATUS)
     endif
  end if

  call state%t_profiler%stop('Final_self',__RC__)
  call MAPL_GenericStateClockOff(STATE,"--GenFinalMine")
  call MAPL_GenericStateClockOff(STATE,"GenFinalTot")
  call MAPL_GenericStateClockOff(STATE,"TOTAL")

! Write summary of profiled times
!--------------------------------
  
  call state%t_profiler%stop('Finalize',__RC__)
  call state%t_profiler%stop(__RC__)

  if (.not. MAPL_ProfIsDisabled()) then

     call report_generic_profile()

     ! WJ node: the old report will be removed
     call WRITE_PARALLEL(" ")
     call WRITE_PARALLEL(" Times for "//trim(COMP_NAME))

     call MAPL_ProfWrite(STATE%TIMES,RC=STATUS)
     _VERIFY(STATUS)

     call WRITE_PARALLEL(" ")
  end if

  call t_p%stop(trim(state%compname),__RC__)

! Clean-up
!---------
!ALT
  call MAPL_GenericStateDestroy (STATE,  RC=STATUS)
  _VERIFY(STATUS)

  _RETURN(ESMF_SUCCESS)

contains

   subroutine report_generic_profile( rc )
      integer, optional,   intent(  out) :: RC     ! Error code:
      character(:), allocatable :: report(:)
      type (ProfileReporter) :: reporter
      type (MultiColumn) :: inclusive, exclusive
      type (ESMF_VM) :: vm
      character(1) :: empty(0)

      call ESMF_VmGetCurrent(vm, rc=status)
      _VERIFY(STATUS)

      if  (MAPL_AM_I_Root(vm)) then

          reporter = ProfileReporter(empty)
          call reporter%add_column(NameColumn(50 , separator=" "))
          call reporter%add_column(FormattedTextColumn('#-cycles','(i8.0)', 8, NumCyclesColumn(),separator='-'))
          inclusive = MultiColumn(['Inclusive'], separator='=')
          call inclusive%add_column(FormattedTextColumn(' T (sec) ','(f9.3)', 9, InclusiveColumn(), separator='-'))
          call inclusive%add_column(FormattedTextColumn('   %  ','(f6.2)', 6, PercentageColumn(InclusiveColumn(),'MAX'),separator='-'))
          call reporter%add_column(inclusive)
          exclusive = MultiColumn(['Exclusive'], separator='=')
          call exclusive%add_column(FormattedTextColumn(' T (sec) ','(f9.3)', 9, ExclusiveColumn(), separator='-'))
          call exclusive%add_column(FormattedTextColumn('   %  ','(f6.2)', 6, PercentageColumn(ExclusiveColumn()), separator='-'))
          call reporter%add_column(exclusive)

          report = reporter%generate_report(state%t_profiler)
          write(OUTPUT_UNIT,*)''
          write(OUTPUT_UNIT,*)'Time for ' // trim(comp_name)
          do i = 1, size(report)
             write(OUTPUT_UNIT,'(a)')report(i)
          end do
          write(OUTPUT_UNIT,*)''
      end if
      
     _RETURN(ESMF_SUCCESS)
   end subroutine report_generic_profile
   
end subroutine MAPL_GenericFinalize


! !IROUTINE: MAPL_GenericRecord -- Record the component and its children

! !INTERFACE:

  recursive subroutine MAPL_GenericRecord ( GC, IMPORT, EXPORT, CLOCK, RC )

! !ARGUMENTS:

    type(ESMF_GridComp), intent(inout) :: GC     ! composite gridded component 
    type(ESMF_State),    intent(inout) :: IMPORT ! import state
    type(ESMF_State),    intent(inout) :: EXPORT ! export state
    type(ESMF_Clock),    intent(inout) :: CLOCK  ! the clock
    integer, optional,   intent(  out) :: RC     ! Error code:
                                                 ! = 0 all is well
                                                 ! otherwise, error
!EOPI

! LOCAL VARIABLES

  character(len=ESMF_MAXSTR)                  :: IAm
  character(len=ESMF_MAXSTR)                  :: COMP_NAME
  character(len=ESMF_MAXSTR)                  :: CHILD_NAME
  character(len=14)                           :: datestamp ! YYYYMMDD_HHMMz
  integer                                     :: STATUS
  integer                                     :: userRC
  integer                                     :: I
  type (MAPL_MetaComp), pointer               :: STATE

  integer                                     :: filetype
  character(len=1)                            :: separator

  character(len=ESMF_MAXSTR)                  :: filetypechar
  character(len=4)                            :: extension
  integer                                     :: hdr

  integer                                     :: K
  logical                                     :: ftype(0:1)
  class(BaseProfiler), pointer                :: t_p
  type(ESMF_GridComp), pointer :: gridcomp
  type(ESMF_State), pointer :: child_import_state
  type(ESMF_State), pointer :: child_export_state
!=============================================================================

!  Begin...

  Iam = "MAPL_GenericRecord"
  call ESMF_GridCompGet(GC, name=COMP_NAME, RC=STATUS )
  _VERIFY(STATUS)
  Iam = trim(COMP_NAME) // Iam

! Retrieve the pointer to the state
!----------------------------------

  call MAPL_InternalStateRetrieve(GC, STATE, RC=STATUS)
  _VERIFY(STATUS)

  t_p => get_global_time_profiler()
  call state%t_profiler%start(__RC__)
  call state%t_profiler%start('Record',__RC__)


  call MAPL_GenericStateClockOn(STATE,"TOTAL")
  call MAPL_GenericStateClockOn(STATE,"GenRecordTot")
! Record the children
! ---------------------
     do I = 1, STATE%get_num_children()
        call ESMF_GridCompGet( STATE%GET_CHILD_GRIDCOMP(I), NAME=CHILD_NAME, RC=STATUS )
        _VERIFY(STATUS)
        call MAPL_GenericStateClockOn (STATE,trim(CHILD_NAME))
        gridcomp => STATE%GET_CHILD_GRIDCOMP(I)
        child_import_state => STATE%get_child_import_state(i)
        child_export_state => STATE%get_child_export_state(i)
        call ESMF_GridCompWriteRestart (gridcomp, &
             importState=child_import_state, &
             exportState=child_export_state, &
             clock=CLOCK, userRC=userRC, RC=STATUS ) ! number of phases is currently limited to 1
        _ASSERT(userRC==ESMF_SUCCESS .and. STATUS==ESMF_SUCCESS,'needs informative message')
        call MAPL_GenericStateClockOff(STATE,trim(CHILD_NAME))
     enddo

! Do my "own" record
! ------------------
  call MAPL_GenericStateClockOn(STATE,"--GenRecordMine")
  call state%t_profiler%start('Record_self',__RC__)

  if (associated(STATE%RECORD)) then

     FILETYPE = MAPL_Write2Disk
     ftype = .false.
     DO I = 1, size(STATE%RECORD%ALARM)
        if ( ESMF_AlarmIsRinging(STATE%RECORD%ALARM(I), RC=STATUS) ) then
           _VERIFY(STATUS)
           filetype = STATE%RECORD%FILETYPE(I)

           if (.not. ftype(filetype)) then
              !ALT: we do this only ONCE per given filetype (RAM or Disk)
              ftype(filetype) = .true.
              ! add timestamp to filename
              call MAPL_DateStampGet(clock, datestamp, rc=status)
              _VERIFY(STATUS)

              if (FILETYPE /= MAPL_Write2Disk) then
                 separator = '*'
              else
                 separator = '.'
              end if

              K=STATE%RECORD%IMP_LEN
              if (K > 0) then
                 call    MAPL_GetResource( STATE, filetypechar, LABEL="IMPORT_CHECKPOINT_TYPE:",                  RC=STATUS )
                 if ( STATUS/=ESMF_SUCCESS  .or.  filetypechar == "default" ) then
                    call MAPL_GetResource( STATE, filetypechar, LABEL="DEFAULT_CHECKPOINT_TYPE:", default='pnc4', RC=STATUS )
                    _VERIFY(STATUS)
                 end if
                 filetypechar = ESMF_UtilStringLowerCase(filetypechar,rc=STATUS)
                 _VERIFY(STATUS)
                 if (filetypechar == 'pnc4') then
                    extension = '.nc4'
                 else
                    extension = '.bin'
                 end if
                 STATE%RECORD%IMP_FNAME(K+1:) = separator // DATESTAMP // extension
              end if

              K=STATE%RECORD%INT_LEN
              if (K > 0) then
                 call    MAPL_GetResource( STATE, hdr,      LABEL="INTERNAL_HEADER:",         default=0,      RC=STATUS )
                 _VERIFY(STATUS)
                 call    MAPL_GetResource( STATE, filetypechar, LABEL="INTERNAL_CHECKPOINT_TYPE:",                RC=STATUS )
                 if ( STATUS/=ESMF_SUCCESS  .or.  filetypechar == "default" ) then
                    call MAPL_GetResource( STATE, filetypechar, LABEL="DEFAULT_CHECKPOINT_TYPE:", default='pnc4', RC=STATUS )
                    _VERIFY(STATUS)
                 end if
                 filetypechar = ESMF_UtilStringLowerCase(filetypechar,rc=STATUS)
                 _VERIFY(STATUS)
                 if (filetypechar == 'pnc4') then
                    extension = '.nc4'
                 else
                    extension = '.bin'
                 end if
                 STATE%RECORD%INT_FNAME(K+1:) = separator // DATESTAMP // extension
              end if

              ! call the actual record method
              call MAPL_StateRecord (GC, IMPORT, EXPORT, CLOCK, RC=STATUS )
              _VERIFY(STATUS)
           endif
        end if
     END DO
  endif
  call state%t_profiler%stop('Record_self',__RC__)
  call MAPL_GenericStateClockOff(STATE,"--GenRecordMine")
  call MAPL_GenericStateClockOff(STATE,"GenRecordTot")
  call MAPL_GenericStateClockOff(STATE,"TOTAL")

  call state%t_profiler%stop('Record',__RC__)
  call state%t_profiler%stop(__RC__)

  _RETURN(ESMF_SUCCESS)
end subroutine MAPL_GenericRecord

subroutine MAPL_StateRecord( GC, IMPORT, EXPORT, CLOCK, RC )

! !ARGUMENTS:

    type(ESMF_GridComp), intent(inout) :: GC     ! composite gridded component 
    type(ESMF_State),    intent(inout) :: IMPORT ! import state
    type(ESMF_State),    intent(inout) :: EXPORT ! export state
    type(ESMF_Clock),    intent(inout) :: CLOCK  ! the clock
    integer, optional,   intent(  out) :: RC     ! Error code:
                                                 ! = 0 all is well
                                                 ! otherwise, error
!EOPI

! LOCAL VARIABLES

  character(len=ESMF_MAXSTR)                  :: IAm
  character(len=ESMF_MAXSTR)                  :: COMP_NAME
  integer                                     :: STATUS

  type (MAPL_MetaComp), pointer               :: STATE
  integer                                     :: hdr
  character(len=ESMF_MAXSTR)                  :: FILETYPE
  type(ESMF_State), pointer :: internal_state
!=============================================================================

!  Begin...

  _UNUSED_DUMMY(EXPORT)

  Iam = "MAPL_StateRecord"
  call ESMF_GridCompGet(GC, name=COMP_NAME, RC=STATUS )
  _VERIFY(STATUS)
  Iam = trim(COMP_NAME) // Iam


! Retrieve the pointer to the state
!----------------------------------

  call MAPL_InternalStateRetrieve(GC, STATE, RC=STATUS)
  _VERIFY(STATUS)

  if (.not.associated(STATE%RECORD)) then
     _RETURN(ESMF_SUCCESS)
  end if

  if (STATE%RECORD%IMP_LEN > 0) then
     call    MAPL_GetResource( STATE, FILETYPE, LABEL="IMPORT_CHECKPOINT_TYPE:",                  RC=STATUS )
     if ( STATUS/=ESMF_SUCCESS  .or.  FILETYPE == "default" ) then
        call MAPL_GetResource( STATE, FILETYPE, LABEL="DEFAULT_CHECKPOINT_TYPE:", default='pnc4', RC=STATUS )
        _VERIFY(STATUS)
     end if
     call MAPL_ESMFStateWriteToFile(IMPORT, CLOCK, &
                                    STATE%RECORD%IMP_FNAME, &
                                    FILETYPE, STATE, .FALSE., oClients = o_Clients, &
                                    RC=STATUS)
     _VERIFY(STATUS)
  end if
  
  if (STATE%RECORD%INT_LEN > 0) then
     call    MAPL_GetResource( STATE, hdr,      LABEL="INTERNAL_HEADER:",         default=0,      RC=STATUS )
     _VERIFY(STATUS)
     call    MAPL_GetResource( STATE, FILETYPE, LABEL="INTERNAL_CHECKPOINT_TYPE:",                RC=STATUS )
     if ( STATUS/=ESMF_SUCCESS  .or.  FILETYPE == "default" ) then
        call MAPL_GetResource( STATE, FILETYPE, LABEL="DEFAULT_CHECKPOINT_TYPE:", default='pnc4', RC=STATUS )
        _VERIFY(STATUS)
     end if

     internal_state => STATE%get_internal_state()
     call MAPL_ESMFStateWriteToFile(internal_state, CLOCK, &
                                    STATE%RECORD%INT_FNAME, &
                                    FILETYPE, STATE, hdr/=0, oClients = o_Clients, &
                                    RC=STATUS)
     _VERIFY(STATUS)
  end if
  

  _RETURN(ESMF_SUCCESS)
end subroutine MAPL_StateRecord

  recursive subroutine MAPL_GenericRefresh ( GC, IMPORT, EXPORT, CLOCK, RC )

! !ARGUMENTS:

    type(ESMF_GridComp), intent(inout) :: GC     ! composite gridded component 
    type(ESMF_State),    intent(inout) :: IMPORT ! import state
    type(ESMF_State),    intent(inout) :: EXPORT ! export state
    type(ESMF_Clock),    intent(inout) :: CLOCK  ! the clock
    integer, optional,   intent(  out) :: RC     ! Error code:
                                                 ! = 0 all is well
                                                 ! otherwise, error
!EOPI

! LOCAL VARIABLES

  character(len=ESMF_MAXSTR)                  :: IAm
  character(len=ESMF_MAXSTR)                  :: COMP_NAME
  character(len=ESMF_MAXSTR)                  :: CHILD_NAME
  character(len=14)                           :: datestamp ! YYYYMMDD_HHMMz
  integer                                     :: STATUS
  integer                                     :: I
  type (MAPL_MetaComp), pointer               :: STATE
  character(len=1)                            :: separator
  character(len=ESMF_MAXSTR)                  :: filetypechar
  character(len=4)                            :: extension
  integer                                     :: hdr
  class(BaseProfiler), pointer                :: t_p
  type(ESMF_GridComp), pointer :: gridcomp
  type(ESMF_State), pointer :: child_import_state
  type(ESMF_State), pointer :: child_export_state
!=============================================================================

!  Begin...

  Iam = "MAPL_GenericRefresh"
  call ESMF_GridCompGet(GC, name=COMP_NAME, RC=STATUS )
  _VERIFY(STATUS)
  Iam = trim(COMP_NAME) // Iam

! Retrieve the pointer to the state
!----------------------------------

  call MAPL_InternalStateRetrieve(GC, STATE, RC=STATUS)
  _VERIFY(STATUS)

  t_p => get_global_time_profiler()
  call state%t_profiler%start(__RC__)
  call state%t_profiler%start('Refresh',__RC__)

  call MAPL_GenericStateClockOn(STATE,"TOTAL")
  call MAPL_GenericStateClockOn(STATE,"GenRefreshTot")
! Refresh the children
! ---------------------
     do I=1,STATE%get_num_children()
        gridcomp => STATE%GET_CHILD_GRIDCOMP(I)
        call ESMF_GridCompGet( gridcomp, NAME=CHILD_NAME, RC=STATUS )
        _VERIFY(STATUS)
        call MAPL_GenericStateClockOn (STATE,trim(CHILD_NAME))
        child_import_state => STATE%get_child_import_state(i)
        child_export_state => STATE%get_child_export_state(i)
        call MAPL_GenericRefresh (gridcomp, child_import_state, child_export_state, CLOCK, &
             RC=STATUS )
        _VERIFY(STATUS)
        call MAPL_GenericStateClockOff(STATE,trim(CHILD_NAME))
     enddo

! Do my "own" refresh
! ------------------
  call MAPL_GenericStateClockOn(STATE,"--GenRefreshMine")
  call state%t_profiler%start('Refresh_self',__RC__)

  if (associated(STATE%RECORD)) then

! add timestamp to filename
     call MAPL_DateStampGet(clock, datestamp, rc=status)
     _VERIFY(STATUS)

!ALT: If any value of Record%filetype is MAPL_Write2RAM
!     the restart must have been written to RAM by some component (or MAPL)
!     and we read it from there (and ignore any disk files!)

     if (ANY(STATE%RECORD%FILETYPE == MAPL_Write2RAM)) then
        separator = '*'
     else
        separator = '.'
     end if

     I=STATE%RECORD%IMP_LEN
     if (I > 0) then
        call    MAPL_GetResource( STATE, filetypechar, LABEL="IMPORT_CHECKPOINT_TYPE:",                  RC=STATUS )
        if ( STATUS/=ESMF_SUCCESS  .or.  filetypechar == "default" ) then
           call MAPL_GetResource( STATE, filetypechar, LABEL="DEFAULT_CHECKPOINT_TYPE:", default='pnc4', RC=STATUS )
           _VERIFY(STATUS)
        end if
        filetypechar = ESMF_UtilStringLowerCase(filetypechar,rc=STATUS)
        _VERIFY(STATUS) 
        if (filetypechar == 'pnc4') then
           extension = '.nc4'
        else
           extension = '.bin'
        end if
        STATE%RECORD%IMP_FNAME(I+1:) = separator // DATESTAMP // extension
     end if
     
     I=STATE%RECORD%INT_LEN
     if (I > 0) then
        call    MAPL_GetResource( STATE, hdr,      LABEL="INTERNAL_HEADER:",         default=0,      RC=STATUS )
        _VERIFY(STATUS)
        call    MAPL_GetResource( STATE, filetypechar, LABEL="INTERNAL_CHECKPOINT_TYPE:",                RC=STATUS )
        if ( STATUS/=ESMF_SUCCESS  .or.  filetypechar == "default" ) then
           call MAPL_GetResource( STATE, filetypechar, LABEL="DEFAULT_CHECKPOINT_TYPE:", default='pnc4', RC=STATUS )
           _VERIFY(STATUS)
        end if
        filetypechar = ESMF_UtilStringLowerCase(filetypechar,rc=STATUS)
        _VERIFY(STATUS)
        if (filetypechar == 'pnc4') then
           extension = '.nc4'
        else
           extension = '.bin'
        end if
        STATE%RECORD%INT_FNAME(I+1:) = separator // DATESTAMP // extension
     end if

! call the actual record method
     call MAPL_StateRefresh (GC, IMPORT, EXPORT, CLOCK, RC=STATUS )
     _VERIFY(STATUS)
  endif
  call MAPL_GenericStateClockOff(STATE,"--GenRefreshMine")
  call MAPL_GenericStateClockOff(STATE,"GenRefreshTot")
  call MAPL_GenericStateClockOff(STATE,"TOTAL")

  call state%t_profiler%stop('Refresh_self',__RC__)
  call state%t_profiler%stop('Refresh',__RC__)
  call state%t_profiler%stop(__RC__)

  _RETURN(ESMF_SUCCESS)
end subroutine MAPL_GenericRefresh

subroutine MAPL_StateRefresh( GC, IMPORT, EXPORT, CLOCK, RC )

! !ARGUMENTS:

    type(ESMF_GridComp), intent(inout) :: GC     ! composite gridded component 
    type(ESMF_State),    intent(inout) :: IMPORT ! import state
    type(ESMF_State),    intent(inout) :: EXPORT ! export state
    type(ESMF_Clock),    intent(inout) :: CLOCK  ! the clock
    integer, optional,   intent(  out) :: RC     ! Error code:
                                                 ! = 0 all is well
                                                 ! otherwise, error
!EOPI

! LOCAL VARIABLES

  character(len=ESMF_MAXSTR)                  :: IAm
  character(len=ESMF_MAXSTR)                  :: COMP_NAME
  integer                                     :: STATUS

  type (MAPL_MetaComp), pointer               :: STATE
  integer                                     :: hdr
  integer                                     :: unit
  type(ESMF_State), pointer :: internal_state
!=============================================================================

  _UNUSED_DUMMY(EXPORT)

!  Begin...

  Iam = "MAPL_StateRefresh"
  call ESMF_GridCompGet(GC, name=COMP_NAME, RC=STATUS )
  _VERIFY(STATUS)
  Iam = trim(COMP_NAME) // Iam


! Retrieve the pointer to the state
!----------------------------------

  call MAPL_InternalStateRetrieve(GC, STATE, RC=STATUS)
  _VERIFY(STATUS)

  if (.not.associated(STATE%RECORD)) then
     _RETURN(ESMF_SUCCESS)
  end if

  if (STATE%RECORD%IMP_LEN > 0) then
     call MAPL_ESMFStateReadFromFile(IMPORT, CLOCK, &
                                     STATE%RECORD%IMP_FNAME, &
                                     STATE, .FALSE., RC=STATUS)
     _VERIFY(STATUS)
     UNIT = GETFILE(STATE%RECORD%IMP_FNAME, RC=STATUS)
     _VERIFY(STATUS)
     call MAPL_DestroyFile(unit = UNIT, rc=STATUS)
     _VERIFY(STATUS)
  end if
  
  if (STATE%RECORD%INT_LEN > 0) then
     call MAPL_GetResource( STATE   , hdr,         &
                            default=0, &
                            LABEL="INTERNAL_HEADER:", &
                            RC=STATUS)
     _VERIFY(STATUS)
     internal_state => state%get_internal_state()
     call MAPL_ESMFStateReadFromFile(internal_state, CLOCK, &
                                     STATE%RECORD%INT_FNAME, &
                                     STATE, hdr/=0, RC=STATUS)
     _VERIFY(STATUS)
     UNIT = GETFILE(STATE%RECORD%INT_FNAME, RC=STATUS)
     _VERIFY(STATUS)
     call MAPL_DestroyFile(unit = UNIT, rc=STATUS)
     _VERIFY(STATUS)
  end if

  _RETURN(ESMF_SUCCESS)
end subroutine MAPL_StateRefresh



subroutine MAPL_DateStampGet (clock, DateStamp, rc)
  type (ESMF_Clock)                 :: clock
  character(len=*)        :: DateStamp
  integer, optional                 :: rc
  
  type(ESMF_Time)                   :: currentTime
  character(len=ESMF_MAXSTR)        :: TimeString
  character                         :: String(ESMF_MAXSTR)
  
  character(len=ESMF_MAXSTR)                  :: IAm
  integer                                     :: STATUS

  character*4 year
  character*2 month
  character*2 day
  character*2 hour
  character*2 minute
  character*2 second

  equivalence ( string(01),TimeString )
  equivalence ( string(01),year       )
  equivalence ( string(06),month      )
  equivalence ( string(09),day        )
  equivalence ( string(12),hour       )
  equivalence ( string(15),minute     )
  equivalence ( string(18),second     )
  
  Iam = "MAPL_DateStampGet"
  
  call ESMF_ClockGet (clock, currTime=currentTime, rc=status)
  _VERIFY(STATUS)
  call ESMF_TimeGet  (currentTime, timeString=TimeString, rc=status)
  _VERIFY(STATUS)
  
  DateStamp = year // month // day // '_' // hour // minute // 'z'

  _RETURN(ESMF_SUCCESS)
end subroutine MAPL_DateStampGet



!=============================================================================
!=============================================================================
!=============================================================================
!=============================================================================


  subroutine MAPL_InternalStateCreate( GC, MAPLOBJ, RC)
    use mapl_ConcreteComposite
    type(ESMF_GridComp),                  intent(INOUT) :: GC ! Gridded component
    type (MAPL_MetaComp),                       pointer :: MAPLOBJ
    integer,                    optional, intent(  OUT) :: RC ! Return code

! ErrLog Variables

    character(len=ESMF_MAXSTR)        :: IAm
    character(len=ESMF_MAXSTR)        :: COMP_NAME
    integer                           :: STATUS
! Local variables
! ---------------

    type (MAPL_GenericWrap )          :: WRAP
#if defined(ABSOFT) || defined(sysIRIX64)
    type(MAPL_MetaComp ), target      :: DUMMY
#endif
    type(ConcreteComposite), pointer :: root_composite
    class(AbstractFrameworkComponent), pointer :: tmp_component
!=============================================================================

! Begin...

! Get the target components name and set-up traceback handle.
! -----------------------------------------------------------

    Iam = "MAPL_InternalStateCreate"
    call ESMF_GridCompGet( GC, name=COMP_NAME, RC=STATUS )
    _VERIFY(STATUS)
    Iam = trim(COMP_NAME) // trim(Iam)

#if defined(ABSOFT) || defined(sysIRIX64)
    WRAP%MAPLOBJ => DUMMY
#endif

! Allocate this instance of the internal state and put it in wrapper.
! -------------------------------------------------------------------

    if (.not. associated(MAPLOBJ)) then
       ! Root component (hopefully)
       allocate(MAPLOBJ, STAT=STATUS)
       _VERIFY(STATUS)
       !!!! Memory leak !!!!
       allocate(root_composite)
       ! TODO: test if workaround is needed for 10.2
       ! workaround for gfortran 10.1
!!$       root_composite = ConcreteComposite(MAPLOBJ)
       call root_composite%initialize(MAPLOBJ)
       tmp_component => root_composite%get_component()
       select type (tmp_component)
       class is (MAPL_MetaComp)
          MAPLOBJ => tmp_component
       end select
       call MAPLOBJ%set_composite(root_composite)
    end if

    WRAP%MAPLOBJ => MAPLOBJ

! Have ESMF save pointer to the wrapped internal state in the G.C.
! ----------------------------------------------------------------

    call ESMF_UserCompSetInternalState(GC, "MAPL_GenericInternalState", WRAP, STATUS)
    _VERIFY(STATUS)

! Initialize the config and grid in the generic state.
!-----------------------------------------------------

    call ESMF_GridCompGet( GC, CONFIG = MAPLOBJ%CF, RC=STATUS )
    _VERIFY(STATUS)

    _RETURN(ESMF_SUCCESS)
  end subroutine MAPL_InternalStateCreate


!=============================================================================
!=============================================================================
!=============================================================================
!=============================================================================


  subroutine MAPL_GenericStateDestroy (STATE,RC)
    type (MAPL_MetaComp), pointer              :: STATE
    integer, optional,           intent(  OUT) :: rc     ! Error code:

    character(len=ESMF_MAXSTR), parameter :: IAm = "MAPL_GenericStateDestroy"
    logical :: isCreated
    integer :: STATUS
    type(ESMF_State), pointer :: internal_state

    if(associated(STATE)) then
       call MAPL_SunOrbitDestroy    (STATE%ORBIT         ,RC=STATUS)
       _VERIFY(STATUS)

       internal_state => state%get_internal_state()
       isCreated = ESMF_StateIsCreated(internal_state, RC=STATUS)
       _VERIFY(STATUS)
       if (isCreated) then
          call ESMF_StateDestroy       (internal_state, __RC__)
       end if

       isCreated = ESMF_StateIsCreated(STATE%FORCING, RC=STATUS)
       _VERIFY(STATUS)
       if (isCreated) then
          call ESMF_StateDestroy       (STATE%FORCING      ,RC=STATUS)
          _VERIFY(STATUS)
       end if

!       call MAPL_VarSpecDestroy     (STATE%COMPONENT_SPEC%IMPORT%OLD_VAR_SPECS   ,RC=STATUS)
!       _VERIFY(STATUS)

!       call MAPL_VarSpecDestroy     (STATE%COMPONENT_SPEC%EXPORT%OLD_VAR_SPECS   ,RC=STATUS)
!       _VERIFY(STATUS)

!       call MAPL_VarSpecDestroy     (STATE%COMPONENT_SPEC%INTERNAL%OLD_VAR_SPECS ,RC=STATUS)
!       _VERIFY(STATUS)

       call MAPL_VarSpecDestroy     (STATE%COMPONENT_SPEC%FORCING%OLD_VAR_SPECS   ,RC=STATUS)
       _VERIFY(STATUS)
       if(associated(STATE%COMPONENT_SPEC%EXPORT%OLD_VAR_SPECS)  ) deallocate(STATE%COMPONENT_SPEC%EXPORT%OLD_VAR_SPECS)
       if(associated(STATE%COMPONENT_SPEC%INTERNAL%OLD_VAR_SPECS)) deallocate(STATE%COMPONENT_SPEC%INTERNAL%OLD_VAR_SPECS)
       if(allocated(STATE%GCNameList   )) deallocate(STATE%GCNameList   )
       if(associated(STATE%CCS          )) deallocate(STATE%CCS          )
       if(associated(STATE%CIM          )) deallocate(STATE%CIM          )
       if(associated(STATE%CEX          )) deallocate(STATE%CEX          )
       if(associated(STATE%CCCREATED    )) deallocate(STATE%CCCREATED    )
       if(associated(STATE%TIMES        )) deallocate(STATE%TIMES        )

!ALT: still to do: clean LINK, LOCSTREAM, EXCHANGEGRID, RECORD
    end if

    _RETURN(ESMF_SUCCESS)
  end subroutine MAPL_GenericStateDestroy

  subroutine MAPL_StateSetSpecAttrib(STATE, NAME,    &
       INTERNAL, IMPORT, EXPORT, FORCING,            &
       REFRESH_INTERVAL, AVERAGING_INTERVAL, RC      )

    type (MAPL_MetaComp)            , intent(INOUT)   :: STATE
    character (len=*)               , intent(IN)      :: NAME
    integer            , optional   , intent(IN)      :: REFRESH_INTERVAL
    integer            , optional   , intent(IN)      :: AVERAGING_INTERVAL
    logical            , optional   , intent(IN)      :: INTERNAL, EXPORT, IMPORT, FORCING
    integer            , optional   , intent(OUT)     :: RC

    character(len=ESMF_MAXSTR), parameter :: IAm="MAPL_StateSetSpecAttrib"
    integer                               :: STATUS

    type (MAPL_VarSpec ), pointer         :: SPEC => null()


    if(present(INTERNAL))then
       if(INTERNAL) then
          SPEC=>STATE%COMPONENT_SPEC%INTERNAL%OLD_VAR_SPECS(MAPL_VarSpecGetIndex(STATE%COMPONENT_SPEC%INTERNAL%OLD_VAR_SPECS,NAME))
       endif
    endif

    if(present(IMPORT))then
       if(import) then
          allocate(spec)
          SPEC => STATE%COMPONENT_SPEC%IMPORT%var_specs%of(MAPL_VarSpecGetIndex(STATE%COMPONENT_SPEC%IMPORT%OLD_VAR_SPECS,NAME))
       endif
    endif

    if(present(EXPORT))then
       if(EXPORT) then
          SPEC=>STATE%COMPONENT_SPEC%EXPORT%OLD_VAR_SPECS(MAPL_VarSpecGetIndex(STATE%COMPONENT_SPEC%EXPORT%OLD_VAR_SPECS,NAME))
       endif
    endif

    if(present(FORCING))then
       if(FORCING) then
          SPEC=>STATE%COMPONENT_SPEC%FORCING%OLD_VAR_SPECS(MAPL_VarSpecGetIndex(STATE%COMPONENT_SPEC%FORCING%OLD_VAR_SPECS,NAME))
       endif
    endif

    
    call MAPL_VarSpecSet(SPEC,            &
      ACCMLT_INTERVAL=AVERAGING_INTERVAL, &
      COUPLE_INTERVAL=REFRESH_INTERVAL,   &
                               RC=STATUS  )

    _VERIFY(STATUS)

    _RETURN(ESMF_SUCCESS)

  end subroutine MAPL_StateSetSpecAttrib


  subroutine MAPL_StateGetSpecAttrib(STATE, NAME,    &
       INTERNAL, IMPORT, EXPORT, FORCING,            &
       REFRESH_INTERVAL, AVERAGING_INTERVAL, RC      )

    type (MAPL_MetaComp)            , intent(IN)      :: STATE
    character (len=*)               , intent(IN )     :: NAME
    integer            , optional   , intent(OUT)     :: REFRESH_INTERVAL
    integer            , optional   , intent(OUT)     :: AVERAGING_INTERVAL
    logical            , optional   , intent(IN )     :: INTERNAL, EXPORT, IMPORT, FORCING
    integer            , optional   , intent(OUT)     :: RC

    character(len=ESMF_MAXSTR), parameter :: IAm="MAPL_StateGetSpecAttrib"
    integer                               :: STATUS
    type (MAPL_VarSpec ), pointer         :: SPEC=>null()


    if(present(INTERNAL))then
       if(INTERNAL) then
          SPEC=>STATE%COMPONENT_SPEC%INTERNAL%OLD_VAR_SPECS(MAPL_VarSpecGetIndex(STATE%COMPONENT_SPEC%INTERNAL%OLD_VAR_SPECS,NAME))
       endif
    endif

    if(present(IMPORT))then
       if(IMPORT) then
          SPEC=>STATE%COMPONENT_SPEC%IMPORT%OLD_VAR_SPECS(MAPL_VarSpecGetIndex(STATE%COMPONENT_SPEC%IMPORT%OLD_VAR_SPECS,NAME))
       endif
    endif

    if(present(EXPORT))then
       if(EXPORT) then
          SPEC=>STATE%COMPONENT_SPEC%EXPORT%OLD_VAR_SPECS(MAPL_VarSpecGetIndex(STATE%COMPONENT_SPEC%EXPORT%OLD_VAR_SPECS,NAME))
       endif
    endif

    if(present(FORCING))then
       if(FORCING) then
          SPEC=>STATE%COMPONENT_SPEC%FORCING%OLD_VAR_SPECS(MAPL_VarSpecGetIndex(STATE%COMPONENT_SPEC%FORCING%OLD_VAR_SPECS,NAME))
       endif
    endif

    _ASSERT(associated(SPEC),'needs informative message')

    
    call MAPL_VarSpecGet(SPEC,            &
      ACCMLT_INTERVAL=AVERAGING_INTERVAL, &
      COUPLE_INTERVAL=REFRESH_INTERVAL,   &
                               RC=STATUS  )

    _VERIFY(STATUS)

    _RETURN(ESMF_SUCCESS)

  end subroutine MAPL_StateGetSpecAttrib

  subroutine MAPL_InternalStateRetrieve(GC, MAPLOBJ, RC)

! !ARGUMENTS:
!  
    type(ESMF_GridComp),                  intent(INOUT) :: GC ! Gridded component
    type (MAPL_MetaComp),                       pointer :: MAPLOBJ
    integer,                    optional, intent(  OUT) :: RC ! Return code
!EOPI

! ErrLog Variables

    integer                           :: STATUS

! Local variables
! ---------------

    call MAPL_InternalStateGet( GC, MAPLOBJ, RC=STATUS)
    if (STATUS /= ESMF_SUCCESS) then
       call MAPL_InternalStateCreate( GC, MAPLOBJ, RC=STATUS)
       _VERIFY(STATUS)
    end if

    _RETURN(ESMF_SUCCESS)

  end subroutine MAPL_InternalStateRetrieve


  !BOPI
  ! !IROUTINE: MAPL_StateAddImportSpec --- Sets the specifications for an item in the {\tt IMPORT} state.
  ! !IIROUTINE: MAPL_StateAddImportSpec_ 

  !INTERFACE:
  subroutine MAPL_StateAddImportSpec_(GC, SHORT_NAME, LONG_NAME,               &
                                      UNITS,  Dims, VLocation,                 &
                                      DATATYPE,NUM_SUBTILES, REFRESH_INTERVAL, &
                                      AVERAGING_INTERVAL, HALOWIDTH, PRECISION, DEFAULT,  &
                                      RESTART, UNGRIDDED_DIMS, FIELD_TYPE,     &
                                      STAGGERING, ROTATION, RC)

    !ARGUMENTS:
    type (ESMF_GridComp)            , intent(INOUT)   :: GC
    character (len=*)               , intent(IN)      :: SHORT_NAME
    character (len=*)  , optional   , intent(IN)      :: LONG_NAME
    character (len=*)  , optional   , intent(IN)      :: UNITS
    integer            , optional   , intent(IN)      :: DIMS
    integer            , optional   , intent(IN)      :: DATATYPE
    integer            , optional   , intent(IN)      :: NUM_SUBTILES
    integer            , optional   , intent(IN)      :: VLOCATION
    integer            , optional   , intent(IN)      :: REFRESH_INTERVAL
    integer            , optional   , intent(IN)      :: AVERAGING_INTERVAL
    integer            , optional   , intent(IN)      :: HALOWIDTH
    integer            , optional   , intent(IN)      :: PRECISION
    real               , optional   , intent(IN)      :: DEFAULT
    integer            , optional   , intent(IN)      :: RESTART
    integer            , optional   , intent(IN)      :: UNGRIDDED_DIMS(:)
    integer            , optional   , intent(IN)      :: FIELD_TYPE
    integer            , optional   , intent(IN)      :: STAGGERING
    integer            , optional   , intent(IN)      :: ROTATION
    integer            , optional   , intent(OUT)     :: RC
    !EOPI

    character(len=ESMF_MAXSTR), parameter :: IAm="MAPL_StateAddImportSpec"
    integer                               :: STATUS
    integer                               :: usable_AI
    integer                               :: usable_RI
    integer                               :: usable_RS
    real                                  :: dt
    type (ESMF_Config)                    :: CF
    type (MAPL_MetaComp), pointer         :: STATE

    call MAPL_InternalStateRetrieve(GC, STATE, RC=status)
    _VERIFY(STATUS)

    CF = STATE%CF

!  Get the clock increment interval
    call ESMF_ConfigGetAttribute( CF, dt,  Label="RUN_DT:", RC=STATUS)
    _VERIFY(STATUS)

    if (present(REFRESH_INTERVAL)) then
       usable_RI = REFRESH_INTERVAL
    else
       usable_RI = nint(dt)
    endif

    if (present(AVERAGING_INTERVAL)) then
       usable_AI = AVERAGING_INTERVAL
    else
       usable_AI = nint(dt)
    endif

    if (present(Restart)) then
       usable_RS  = Restart
    else
       usable_RS = MAPL_RestartOptional
    endif

    if (present(DIMS)) then
       _ASSERT(DIMS /= MAPL_DimsNone,'needs informative message')
    end if

    call MAPL_VarSpecCreateInListNew(STATE%COMPONENT_SPEC%IMPORT,               &
       LONG_NAME  = LONG_NAME,                                               &
       UNITS      = UNITS,                                                   &
       SHORT_NAME = SHORT_NAME,                                              &
       DIMS       = DIMS,                                                    &
       STAT       = DATATYPE,                                                &
       NUM_SUBTILES=NUM_SUBTILES,                                            &
       ACCMLT_INTERVAL= usable_AI,                                           &
       COUPLE_INTERVAL= usable_RI,                                           &
       VLOCATION  = VLOCATION,                                               &
       HALOWIDTH  = HALOWIDTH,                                               &
       PRECISION  = PRECISION,                                               &
       RESTART    = usable_RS,                                               &
       DEFAULT    = DEFAULT,                                                 &
       UNGRIDDED_DIMS = UNGRIDDED_DIMS,                                      &
       FIELD_TYPE = FIELD_TYPE,                                              &
       STAGGERING = STAGGERING,                                              &
       ROTATION = ROTATION,                                                  &
       RC=STATUS  )
    _VERIFY(STATUS)


    _RETURN(ESMF_SUCCESS)
  end subroutine MAPL_StateAddImportSpec_


  !BOPI
  ! !IIROUTINE: MAPL_StateAddImportSpecFrmChld --- Add \texttt{IMPORT} spec from child

  !INTERFACE:
  subroutine MAPL_StateAddImportSpecFrmChld ( STATE, SHORT_NAME, CHILD_ID, RC )

    !ARGUMENTS:
    type (MAPL_MetaComp)            , intent(INOUT)   :: STATE
    character (len=*)               , intent(IN)      :: SHORT_NAME
    integer                         , intent(IN)      :: CHILD_ID
    integer            , optional   , intent(OUT)     :: RC
    !EOPI

    character(len=ESMF_MAXSTR), parameter :: IAm="MAPL_StateAddImportSpecFrmChld"
    integer                               :: STATUS
    type (MAPL_VarSpec),      pointer     :: SPECS(:)
    integer                               :: I
    type(ESMF_GridComp), pointer :: gridcomp


    if (.not. STATE%get_num_children() > 0) then
       _RETURN(ESMF_FAILURE)
    end if

    gridcomp => STATE%GET_CHILD_GRIDCOMP(CHILD_ID)
    call MAPL_GridCompGetVarSpecs(gridcomp, IMPORT=SPECS, RC=STATUS)
    _VERIFY(STATUS)

    I=MAPL_VarSpecGetIndex(SPECS, SHORT_NAME, RC=STATUS)

    if (I == -1) then
       _RETURN(ESMF_FAILURE)
    endif
    _VERIFY(STATUS)
    
    call MAPL_VarSpecAddRefToList(STATE%COMPONENT_SPEC%IMPORT%OLD_VAR_SPECS, SPECS(I), RC=STATUS)
    if (STATUS /= MAPL_DuplicateEntry) then
       _VERIFY(STATUS)
    else
       _RETURN(ESMF_FAILURE) ! ALT this needs to be revisited
    endif

!ALT: is reconnect needed
!    call MAPL_AddConnectivity ( STATE, SHORT_NAME=SHORT_NAME, &
!       FROM_IMPORT=CHILD_ID, TO_IMPORT=MAPL_Self, RC=STATUS  )
!    _VERIFY(STATUS)

    _RETURN(ESMF_SUCCESS)
  end subroutine MAPL_StateAddImportSpecFrmChld


  subroutine MAPL_ChildAddAttribToImportSpec ( STATE, CHILD_ID, &
       REFRESH_INTERVAL, AVERAGING_INTERVAL, OFFSET, RC )

    !ARGUMENTS:
    type (MAPL_MetaComp)            , intent(INOUT)   :: STATE
    integer                         , intent(IN)      :: CHILD_ID
    integer            , optional   , intent(IN)      :: REFRESH_INTERVAL
    integer            , optional   , intent(IN)      :: AVERAGING_INTERVAL
    integer            , optional   , intent(IN)      :: OFFSET
    integer            , optional   , intent(OUT)     :: RC
    !EOPI

    character(len=ESMF_MAXSTR), parameter :: IAm="MAPL_ChildAddAttribToImportSpec"
    integer                               :: STATUS
    type (MAPL_VarSpec),      pointer     :: SPECS(:)
    integer                               :: I
    type(ESMF_GridComp), pointer :: gridcomp


    if (.not. STATE%get_num_children() > 0) then
       _RETURN(ESMF_FAILURE)
    end if

    gridcomp => STATE%GET_CHILD_GRIDCOMP(CHILD_ID)
    call MAPL_GridCompGetVarSpecs(gridcomp, IMPORT=SPECS, RC=STATUS)
    _VERIFY(STATUS)

    do I = 1, size(SPECS)
    
       call MAPL_VarSpecSet(SPECS(I),                  &
            ACCMLT_INTERVAL= AVERAGING_INTERVAL,       &
            COUPLE_INTERVAL= REFRESH_INTERVAL,         &
            OFFSET         = OFFSET,                   &
            RC=STATUS  )
       _VERIFY(STATUS)
    end do

    _RETURN(ESMF_SUCCESS)
  end subroutine MAPL_ChildAddAttribToImportSpec


!BOPI
! !IROUTINE: MAPL_StateAddExportSpec --- sets the specifications for an item in the \texttt{EXPORT} state
! !IIROUTINE: MAPL_StateAddExportSpec_

! !INTERFACE:
  subroutine MAPL_StateAddExportSpec_(GC, SHORT_NAME, LONG_NAME,            &
                                      UNITS, Dims, VLocation,               &
                                      DATATYPE,NUM_SUBTILES,                &
                                      REFRESH_INTERVAL, AVERAGING_INTERVAL, &
                                      HALOWIDTH, PRECISION, DEFAULT, UNGRIDDED_DIMS,   &
                                      UNGRIDDED_UNIT, UNGRIDDED_NAME,     &
                                      UNGRIDDED_COORDS,                     &
                                      FIELD_TYPE, STAGGERING, ROTATION, RC )

    !ARGUMENTS:
    type (ESMF_GridComp)            , intent(INOUT)   :: GC
    character (len=*)               , intent(IN)      :: SHORT_NAME
    character (len=*)  , optional   , intent(IN)      :: LONG_NAME
    character (len=*)  , optional   , intent(IN)      :: UNITS
    integer            , optional   , intent(IN)      :: DIMS
    integer            , optional   , intent(IN)      :: DATATYPE
    integer            , optional   , intent(IN)      :: VLOCATION
    integer            , optional   , intent(IN)      :: NUM_SUBTILES
    integer            , optional   , intent(IN)      :: REFRESH_INTERVAL
    integer            , optional   , intent(IN)      :: AVERAGING_INTERVAL
    integer            , optional   , intent(IN)      :: HALOWIDTH
    integer            , optional   , intent(IN)      :: PRECISION
    real               , optional   , intent(IN)      :: DEFAULT
    integer            , optional   , intent(IN)      :: UNGRIDDED_DIMS(:)
    character (len=*)  , optional   , intent(IN)      :: UNGRIDDED_NAME
    character (len=*)  , optional   , intent(IN)      :: UNGRIDDED_UNIT
    real               , optional   , intent(IN)      :: UNGRIDDED_COORDS(:)
    integer            , optional   , intent(IN)      :: FIELD_TYPE
    integer            , optional   , intent(IN)      :: STAGGERING
    integer            , optional   , intent(IN)      :: ROTATION
    integer            , optional   , intent(OUT)     :: RC
!EOPI


    character(len=ESMF_MAXSTR), parameter :: IAm="MAPL_StateAddExportSpec"
    integer                               :: STATUS
    integer                               :: usable_AI
    integer                               :: usable_RI
    real                                  :: dt
    type (ESMF_Config)                    :: CF
    type (MAPL_MetaComp), pointer         :: STATE

    call MAPL_InternalStateRetrieve(GC, STATE, RC=status)
    _VERIFY(STATUS)

    CF = STATE%CF

!  Get the clock increment interval
    call ESMF_ConfigGetAttribute( CF, dt,  Label="RUN_DT:", RC=STATUS)
    _VERIFY(STATUS)

    if (present(REFRESH_INTERVAL)) then
       usable_RI = REFRESH_INTERVAL
    else
       usable_RI = nint(dt)
    endif

    if (present(AVERAGING_INTERVAL)) then
       usable_AI = AVERAGING_INTERVAL
    else
       usable_AI = nint(dt)
    endif

    if (present(UNGRIDDED_DIMS)) then
       if (present(UNGRIDDED_UNIT) .or. present(UNGRIDDED_NAME) .or. present(UNGRIDDED_COORDS)) then
          _ASSERT(size(UNGRIDDED_DIMS) == 1,'needs informative message')
          _ASSERT(UNGRIDDED_DIMS(1) == size(UNGRIDDED_COORDS),'needs informative message')
       end if
    end if

    if (present(DIMS)) then
       _ASSERT(DIMS /= MAPL_DimsNone,'needs informative message')
    end if

    call MAPL_VarSpecCreateInListNew(STATE%COMPONENT_SPEC%EXPORT,                         &
       LONG_NAME  = LONG_NAME,                                               &
       UNITS      = UNITS,                                                   &
       SHORT_NAME = SHORT_NAME,                                              &
       DIMS       = DIMS,                                                    &
       STAT       = DATATYPE,                                                &
       NUM_SUBTILES=NUM_SUBTILES,                                            &
       ACCMLT_INTERVAL= usable_AI,                                           &
       COUPLE_INTERVAL= usable_RI,                                           &
       VLOCATION  = VLOCATION,                                               &
       HALOWIDTH  = HALOWIDTH,                                               &
       PRECISION  = PRECISION,                                               &
       DEFAULT    = DEFAULT,                                                 &
       UNGRIDDED_DIMS = UNGRIDDED_DIMS,                                      &
       UNGRIDDED_NAME = UNGRIDDED_NAME,                                    &
       UNGRIDDED_UNIT = UNGRIDDED_UNIT,                                    &
       UNGRIDDED_COORDS = UNGRIDDED_COORDS,                                  &
       FIELD_TYPE = FIELD_TYPE,                                              &
       STAGGERING = STAGGERING,                                              &
       ROTATION = ROTATION,                                                  &
       RC=STATUS  )
    _VERIFY(STATUS)


    _RETURN(ESMF_SUCCESS)
  end subroutine MAPL_StateAddExportSpec_


  !BOPI
  ! !IIROUTINE: MAPL_StateAddExportSpecFrmChld --- Add \texttt{EXPORT} spec from child

  !INTERFACE:
  subroutine MAPL_StateAddExportSpecFrmChld ( GC, SHORT_NAME, CHILD_ID, RC )

    !ARGUMENTS:
    type(ESMF_GridComp),              intent(INOUT)   :: GC 
    character (len=*)               , intent(IN)      :: SHORT_NAME
    integer                         , intent(IN)      :: CHILD_ID
    integer            , optional   , intent(OUT)     :: RC
    !EOPI

    character(len=ESMF_MAXSTR), parameter :: IAm="MAPL_StateAddExportSpecFrmChld"
    integer                               :: STATUS


    call MAPL_AddConnectivityE2E ( GC, SHORT_NAME, &
                                SRC_ID = CHILD_ID, &
                                TO_EXPORT = MAPL_Self, RC=STATUS  )
    _VERIFY(STATUS)


    _RETURN(ESMF_SUCCESS)
  end subroutine MAPL_StateAddExportSpecFrmChld


  !BOPI
  ! !IIROUTINE: MAPL_StateAddExportSpecFrmAll --- Add \texttt{EXPORT} spec from all

  !INTERFACE:
  subroutine MAPL_StateAddExportSpecFrmAll ( STATE, RC )

    !ARGUMENTS:
    type (MAPL_MetaComp)            , intent(INOUT)   :: STATE
    integer            , optional   , intent(OUT)     :: RC
    !EOPI

    character(len=ESMF_MAXSTR), parameter :: IAm="MAPL_StateAddExportSpecFrmAll"
    integer                               :: STATUS
    type (MAPL_VarSpec),      pointer     :: SPECS(:)
    type (MAPL_VarSpec),      pointer     :: MYSPEC(:) => null()
    integer                               :: I
    integer                               :: N
    character(len=ESMF_MAXSTR)            :: NAME
    character(len=ESMF_MAXSTR)            :: GCNAME
    character (len=ESMF_MAXSTR)           :: LONG_NAME
    character (len=ESMF_MAXSTR)           :: UNITS
    integer                               :: FIELD_TYPE
    integer                               :: STAGGERING
    integer                               :: ROTATION
    integer                               :: DIMS
    integer                               :: VLOCATION
    integer                               :: NUM_SUBTILES
    integer                               :: ACCMLT_INTERVAL
    integer                               :: COUPLE_INTERVAL
    integer                               :: STAT
    type(ESMF_Field), pointer             :: FIELD
    type(ESMF_GridComp), pointer :: gridcomp


    if (.not. STATE%get_num_children() > 0) then
       _RETURN(ESMF_FAILURE)
    end if

    do N = 1, STATE%get_num_children()

       gridcomp => STATE%GET_CHILD_GRIDCOMP(N)
       call MAPL_GridCompGetVarSpecs(gridcomp, EXPORT=SPECS, RC=STATUS)
       _VERIFY(STATUS)

       call ESMF_GridCompGet( STATE%GET_CHILD_GRIDCOMP(N), name=GCNAME, RC=STATUS )
       _VERIFY(STATUS)

       
       do I = 1, size(SPECS)
    
          call MAPL_VarSpecGet(SPECS(I), SHORT_NAME=NAME,  &
                 LONG_NAME  = LONG_NAME,                                  &
                 UNITS      = UNITS,                                      &
                 FIELD_TYPE = FIELD_TYPE,                                 &
                 STAGGERING = STAGGERING,                                 &
                 ROTATION = ROTATION,                                     &
                 DIMS       = DIMS,                                       &
                 VLOCATION  = VLOCATION,                                  &
                 NUM_SUBTILES=NUM_SUBTILES,                               &
                 STAT       = STAT,                                       &
                 ACCMLT_INTERVAL= ACCMLT_INTERVAL,                     &
                 COUPLE_INTERVAL= COUPLE_INTERVAL,                       &
                 RC=STATUS  )
          _VERIFY(STATUS)

          call MAPL_VarSpecGet(SPECS(I), FIELDPTR = FIELD, RC=STATUS  )
          _VERIFY(STATUS)


          call MAPL_VarSpecCreateInList(MYSPEC,                         &
                 SHORT_NAME=trim(NAME)//'_from_' // trim(GCNAME),         &
                 LONG_NAME  = LONG_NAME,                                  &
                 UNITS      = UNITS,                                      &
                 FIELD_TYPE = FIELD_TYPE,                                 &
                 STAGGERING = STAGGERING,                                 &
                 ROTATION = ROTATION,                                     &
                 DIMS       = DIMS,                                       &
                 VLOCATION  = VLOCATION,                                  &
                 NUM_SUBTILES=NUM_SUBTILES,                               &
                 STAT       = STAT,                                       &
                 ACCMLT_INTERVAL= ACCMLT_INTERVAL,                     &
                 COUPLE_INTERVAL= COUPLE_INTERVAL,                       &
                 RC=STATUS  )
          _VERIFY(STATUS)

          call MAPL_VarSpecSet(MYSPEC(1), FIELDPTR = FIELD, RC=STATUS  )
          _VERIFY(STATUS)

          call MAPL_VarSpecAddRefToList(STATE%COMPONENT_SPEC%EXPORT, MYSPEC(1), RC=STATUS)
          if (STATUS /= MAPL_DuplicateEntry) then
             _VERIFY(STATUS)
          else
             _RETURN(ESMF_FAILURE) ! ALT this needs to be revisited
          endif

          NULLIFY(MYSPEC)
!ALT specDestroy ?; otherwise we have small mem leak
!ALT    call MAPL_AddConnectivity ???


       end do

    end do

    _RETURN(ESMF_SUCCESS)
  end subroutine MAPL_StateAddExportSpecFrmAll



!BOPI
! !IROUTINE: MAPL_AddInternalSpec
! !IIROUTINE: MAPL_StateAddInternalSpec --- Sets specifications for an item in the \texttt{INTERNAL} state

! !INTERFACE:
  subroutine MAPL_StateAddInternalSpec(GC,                 &
                                       SHORT_NAME,         &
                                       LONG_NAME,          &
                                       UNITS,              &
                                       DIMS,               &
                                       VLOCATION,          &
                                       DATATYPE,           &
                                       NUM_SUBTILES,       &
                                       REFRESH_INTERVAL,   &
                                       AVERAGING_INTERVAL, &
                                       DEFAULT,            &
                                       RESTART,            &
                                       HALOWIDTH,          &
                                       PRECISION,          &
                                       FRIENDLYTO,         &
                                       ADD2EXPORT,         &
                                       ATTR_RNAMES,        &
                                       ATTR_INAMES,        &
                                       ATTR_RVALUES,       &
                                       ATTR_IVALUES,       &
                                       UNGRIDDED_DIMS,     &
                                       FIELD_TYPE,         &
                                       STAGGERING,         &
                                       ROTATION,           &
                                       RC)

! !ARGUMENTS:

    type (ESMF_GridComp)            , intent(INOUT)   :: GC
    character (len=*)               , intent(IN)      :: SHORT_NAME
    character (len=*)  , optional   , intent(IN)      :: LONG_NAME
    character (len=*)  , optional   , intent(IN)      :: UNITS
    integer            , optional   , intent(IN)      :: DIMS
    integer            , optional   , intent(IN)      :: DATATYPE
    integer            , optional   , intent(IN)      :: VLOCATION
    integer            , optional   , intent(IN)      :: NUM_SUBTILES
    integer            , optional   , intent(IN)      :: REFRESH_INTERVAL
    integer            , optional   , intent(IN)      :: AVERAGING_INTERVAL
    integer            , optional   , intent(IN)      :: PRECISION
    real               , optional   , intent(IN)      :: DEFAULT
    integer            , optional   , intent(IN)      :: RESTART
    character (len=*)  , optional   , intent(IN)      :: HALOWIDTH
    character (len=*)  , optional   , intent(IN)      :: FRIENDLYTO
    logical            , optional   , intent(IN)      :: ADD2EXPORT
    character (len=*)  , optional   , intent(IN)      :: ATTR_INAMES(:)
    character (len=*)  , optional   , intent(IN)      :: ATTR_RNAMES(:)
    integer            , optional   , intent(IN)      :: ATTR_IVALUES(:)
    real               , optional   , intent(IN)      :: ATTR_RVALUES(:)
    integer            , optional   , intent(IN)      :: UNGRIDDED_DIMS(:)
    integer            , optional   , intent(IN)      :: FIELD_TYPE
    integer            , optional   , intent(IN)      :: STAGGERING
    integer            , optional   , intent(IN)      :: ROTATION
    integer            , optional   , intent(OUT)     :: RC

! !DESCRIPTION:

!  Sets the specifications for an item in the {\tt INTERNAL} state.

!EOPI

    character(len=ESMF_MAXSTR), parameter :: IAm="MAPL_StateAddInternalSpec"
    integer                               :: STATUS
    integer                               :: usable_RS
    integer                               :: usable_HW
    integer                               :: I
    type (MAPL_MetaComp), pointer         :: STATE
    type (MAPL_VarSpec),  pointer         :: SPEC
    integer                               :: default_dt
    integer                               :: interval
    real                                  :: dt

    call MAPL_InternalStateRetrieve(GC, STATE, RC=status)
    _VERIFY(STATUS)

    if (present(Restart)) then
       usable_RS  = Restart
    else
       usable_RS = MAPL_RestartOptional
    endif

    if (present(HALOWIDTH)) then
       read(HALOWIDTH,'(I1)') usable_HW
    else
       usable_HW = 0
    endif

    call MAPL_VarSpecCreateInListNew(STATE%COMPONENT_SPEC%INTERNAL,                       &
       LONG_NAME  = LONG_NAME,                                               &
       UNITS      = UNITS,                                                   &
       SHORT_NAME = SHORT_NAME,                                              &
       DIMS       = DIMS,                                                    &
       STAT       = DATATYPE,                                                &
       NUM_SUBTILES=NUM_SUBTILES,                                            &
       ACCMLT_INTERVAL= AVERAGING_INTERVAL,                                  &
       COUPLE_INTERVAL= REFRESH_INTERVAL,                                    &
       VLOCATION  = VLOCATION,                                               &
       DEFAULT    = DEFAULT, FRIENDLYTO = FRIENDLYTO,                        &
       HALOWIDTH  = usable_HW, PRECISION=PRECISION,                          &
       RESTART    = usable_RS,                                               &
       ATTR_RNAMES=ATTR_RNAMES, ATTR_INAMES=ATTR_INAMES,                     &
       ATTR_RVALUES=ATTR_RVALUES, ATTR_IVALUES=ATTR_IVALUES,                 &
       UNGRIDDED_DIMS=UNGRIDDED_DIMS,                                        &
       FIELD_TYPE = FIELD_TYPE,                                              &
       STAGGERING = STAGGERING,                                              &
       ROTATION   = ROTATION,                                                &
       RC=STATUS  )
    _VERIFY(STATUS)

!ALT: the next section is added here upon the request of Arlindo:
!     if FRIENDLYTO is set, we automatically 
!     add the spec/field to the export

    if (present(FRIENDLYTO) .or. present(ADD2EXPORT)) then

       I=MAPL_VarSpecGetIndex(STATE%COMPONENT_SPEC%INTERNAL%OLD_VAR_SPECS, SHORT_NAME, RC=STATUS)
       if (I == -1) then
          _RETURN(ESMF_FAILURE)
       endif
       _VERIFY(STATUS)

       call ESMF_ConfigGetAttribute( STATE%CF, dt,  Label="RUN_DT:", RC=STATUS)
       _VERIFY(STATUS)
       default_dt = nint(dt)

       SPEC => STATE%COMPONENT_SPEC%INTERNAL%OLD_VAR_SPECS(I)
       call MAPL_VarSpecGet(SPEC, ACCMLT_INTERVAL=interval, RC=STATUS)
       _VERIFY(STATUS)
       if (interval == 0) then ! this was not supplied
          call MAPL_VarSpecSet(SPEC, ACCMLT_INTERVAL=default_dt, RC=STATUS)
          _VERIFY(STATUS)
       endif

       call MAPL_VarSpecGet(SPEC, COUPLE_INTERVAL=interval, RC=STATUS)
       _VERIFY(STATUS)
       if (interval == 0) then ! this was not supplied
          call MAPL_VarSpecSet(SPEC, COUPLE_INTERVAL=default_dt, RC=STATUS)
          _VERIFY(STATUS)
       endif
    
       call MAPL_VarSpecAddRefToList(STATE%COMPONENT_SPEC%EXPORT, SPEC, RC=STATUS)
       _VERIFY(STATUS)

    endif

    _RETURN(ESMF_SUCCESS)
  end subroutine MAPL_StateAddInternalSpec

  !BOPI
  ! !IROUTINE: MAPL_DoNotDeferExport

  !INTERFACE:
  subroutine MAPL_DoNotDeferExport(GC, NAMES, RC)
! !ARGUMENTS:

    type (ESMF_GridComp)            , intent(INOUT)   :: GC
    character (len=*)               , intent(IN)      :: NAMES(:)
    integer            , optional   , intent(OUT)     :: RC

! !DESCRIPTION:

!  For each entry in {\tt NAMES} marks the export spec 
!  to not be deferred during {\tt MAPL\_GenericInitialize}.

!EOPI

    character(len=ESMF_MAXSTR), parameter :: IAm="MAPL_DoNotDeferExport"
    integer                               :: STATUS
    integer                               :: I, J, N, K
    type (MAPL_MetaComp), pointer         :: STATE

    call MAPL_InternalStateRetrieve(GC, STATE, RC=status)
    _VERIFY(STATUS)

    if (associated(STATE%COMPONENT_SPEC%EXPORT%OLD_VAR_SPECS)) then
       N = size(STATE%COMPONENT_SPEC%EXPORT%OLD_VAR_SPECS)
       K = size(NAMES)

       DO I=1,K

          J = MAPL_VarSpecGetIndex(STATE%COMPONENT_SPEC%EXPORT%OLD_VAR_SPECS,NAMES(I))
          _ASSERT(J > 0 .and. J <= N,'needs informative message')

          call MAPL_VarSpecSet(STATE%COMPONENT_SPEC%EXPORT%OLD_VAR_SPECS(J),                         &
               alwaysAllocate = .true.,                                     &
               RC=STATUS  )
          _VERIFY(STATUS)
       END DO
    end if

    _RETURN(ESMF_SUCCESS)
  end subroutine MAPL_DoNotDeferExport


  !BOPI
  ! !IROUTINE: MAPL_GridCompSetEntryPoint

  !INTERFACE:
  subroutine MAPL_GridCompSetEntryPoint(GC, registeredMethod, usersRoutine, RC)

    !ARGUMENTS:
    type(ESMF_GridComp),                  intent(INOUT) :: GC         ! Gridded component
    type(ESMF_Method_Flag),               intent(IN   ) :: registeredMethod
    external                                            :: usersRoutine
    integer,                    optional, intent(  OUT) :: RC         ! Return code
    !EOPI

    integer                               :: status

    type (MAPL_MetaComp),     pointer     :: META 
    integer                               :: phase

    call MAPL_InternalStateRetrieve( GC, META, RC=STATUS)
    _VERIFY(STATUS)

    if (registeredMethod == ESMF_METHOD_INITIALIZE) then
       phase = MAPL_AddMethod(META%phase_init, RC=STATUS)
    else if (registeredMethod == ESMF_METHOD_RUN) then
       phase = MAPL_AddMethod(META%phase_run, RC=STATUS)
    else if (registeredMethod == ESMF_METHOD_FINALIZE) then
       phase = MAPL_AddMethod(META%phase_final, RC=STATUS)
    else if (registeredMethod == ESMF_METHOD_WRITERESTART) then
       phase = MAPL_AddMethod(META%phase_record, RC=STATUS)
    else if (registeredMethod == ESMF_METHOD_READRESTART) then
       phase = MAPL_AddMethod(META%phase_coldstart, RC=STATUS)
    else
       _RETURN(ESMF_FAILURE)
    endif
    _VERIFY(STATUS)

    if (phase > MAPL_MAX_PHASES) then
       print *, 'ERROR: exceeded maximum number of run phases. Increase MAPL_MAX_PHASES and recompile'
    end if

    call ESMF_GridCompSetEntryPoint(GC, registeredMethod, MAPL_GenericWrapper, &
         phase=phase, rc=status)
    _VERIFY(STATUS)
    call ESMF_GridCompSetEntryPoint(GC, registeredMethod,  usersRoutine, &
         phase=MAPL_MAX_PHASES+phase, rc=status)
    _VERIFY(STATUS)

    _RETURN(ESMF_SUCCESS)
  end subroutine MAPL_GridCompSetEntryPoint


  !BOPI
  ! !IROUTINE: MAPL_GetObjectFromGC
  ! !IIROUTINE: MAPL_InternalStateGet

  !INTERFACE:
  subroutine MAPL_InternalStateGet ( GC, MAPLOBJ, RC)

    !ARGUMENTS:
    type(ESMF_GridComp),            intent(INout) :: GC ! Gridded component
    type (MAPL_MetaComp),                 pointer :: MAPLOBJ
    integer,              optional, intent(  OUT) :: RC ! Return code

! !DESCRIPTION:
! This is the recommended way of getting the opaque MAPL Generic
! state object from the gridded component (GC). It can be called at any time
! {\em after} {\tt MAPL\_GenericSetServices} has been called on GC.
! Note that you get a pointer to the object.
    !EOPI

! ErrLog Variables

    character(len=ESMF_MAXSTR)        :: IAm
    character(len=ESMF_MAXSTR)        :: COMP_NAME
    integer                           :: STATUS

! Local variables
! ---------------

    type (MAPL_GenericWrap )          :: WRAP
#if defined(ABSOFT) || defined(sysIRIX64)
    type(MAPL_MetaComp      ), target :: DUMMY
#endif

!=============================================================================

! Begin...

! Get the target components name and set-up traceback handle.
! -----------------------------------------------------------

    Iam = "MAPL_InternalStateGet"
    call ESMF_GridCompGet( GC, name=COMP_NAME, RC=STATUS )
    _VERIFY(STATUS)
    Iam = trim(COMP_NAME) // trim(Iam)

#if defined(ABSOFT) || defined(sysIRIX64)
    WRAP%MAPLOBJ => DUMMY
#endif

    call ESMF_UserCompGetInternalState(GC, "MAPL_GenericInternalState", WRAP, STATUS)
    IF (STATUS /= ESMF_SUCCESS) then
       if (present(RC)) then
          RC = ESMF_FAILURE
       end if
       return
    END IF

    MAPLOBJ => WRAP%MAPLOBJ


    _RETURN(ESMF_SUCCESS)
  end subroutine MAPL_InternalStateGet




!=============================================================================
!=============================================================================
!=============================================================================
!=============================================================================



  !BOPI
  ! !IROUTINE: MAPL_Get
  ! !IIROUTINE: MAPL_GenericStateGet

  !INTERFACE:
  subroutine MAPL_GenericStateGet (STATE, IM, JM, LM, VERTDIM,                &
                                   NX, NY, NX0, NY0, LAYOUT,                  &
                                   GCNames,                                   &
                                   LONS, LATS, ORBIT, RUNALARM,               &
                                   IMPORTspec, EXPORTspec, INTERNALspec,      &
                                   INTERNAL_ESMF_STATE,                       &
                                   TILETYPES, TILEKIND,                       &
                                   TILELATS,TILELONS,TILEAREA,LOCSTREAM,      &
                                   EXCHANGEGRID,                              &
                                   CLOCK,                                     &
                                   NumInitPhases,                             &
                                   NumRunPhases,                              &
                                   GCS, CCS, GIM, GEX, CF, HEARTBEAT,         &
                                   childrens_names, childrens_gridcomps,      &
                                   childrens_import_states, childrens_export_states, &
                                   RC )


    !ARGUMENTS:
    type (MAPL_MetaComp), target,   intent(INOUT) :: STATE
    type (ESMF_Alarm),    optional, intent(  OUT) :: RUNALARM
    type (MAPL_SunOrbit), optional, intent(  OUT) :: ORBIT
    integer,              optional, intent(  OUT) :: IM, JM, LM
    integer,              optional, intent(  OUT) :: VERTDIM
    integer,              optional, intent(  OUT) :: NX, NY, NX0, NY0
    type (ESMF_DELayout), optional, intent(  OUT) :: LAYOUT
    real, pointer,        optional                :: LONS(:,:)
    real, pointer,        optional                :: LATS(:,:)
    integer,              optional, intent(  OUT) :: RC     ! Error code:
    type (MAPL_VarSpec),  optional, pointer       :: IMPORTspec(:)
    type (MAPL_VarSpec),  optional, pointer       :: EXPORTspec(:)
    type (MAPL_VarSpec),  optional, pointer       :: INTERNALspec(:)
    type (ESMF_State),    optional, intent(  OUT) :: INTERNAL_ESMF_STATE
    integer,              optional, pointer       :: TILETYPES(:)
    integer,              optional, pointer       :: TILEKIND(:)
    real, pointer,        optional                :: TILELONS(:)
    real, pointer,        optional                :: TILELATS(:)
    real, pointer,        optional                :: TILEAREA(:)
    type (MAPL_LocStream),optional, intent(  OUT) :: LOCSTREAM
    type (MAPL_LocStream),optional, intent(  OUT) :: EXCHANGEGRID
    type (ESMF_CLOCK)    ,optional, intent(  OUT) :: CLOCK
    type (ESMF_CplComp),  optional, pointer       :: CCS(:,:)

    ! Next four are deprecated (now have memory leak)
    character(len=ESMF_MAXSTR), optional, pointer :: GCNames(:)
    type (ESMF_GridComp),       optional, pointer :: GCS(:)
    type (ESMF_State),          optional, pointer :: GIM(:)
    type (ESMF_State),          optional, pointer :: GEX(:)

    character(len=ESMF_MAXSTR), optional, allocatable :: childrens_names(:)
    type (ESMF_GridComp),       optional, allocatable :: childrens_gridcomps(:)
    type (ESMF_State),          optional, allocatable :: childrens_import_states(:)
    type (ESMF_State),          optional, allocatable :: childrens_export_states(:)

    real                 ,optional, intent(  OUT) :: HEARTBEAT
    integer,              optional, intent(  OUT) :: NumInitPhases
    integer,              optional, intent(  OUT) :: NumRunPhases
    type (ESMF_Config),   optional, intent(  OUT) :: CF

! !DESCRIPTION:
! This is the way of querying the opaque {\em MAPL\_Generic}
! state object. The arguments are:
!  \begin{description}
!   \item[STATE]
!    The MAPL object to be queried.     
!   \item[IM]
!      Size of the first horizontal dimension (X) of local arrays.
!   \item[JM]
!      Size of the second horizontal dimension (Y) of local arrays.
!   \item[LM]
!      Size of the vertical dimension.
!   \item[VERTDIM]
!      Position of the vertical dimension of 2 or higher dimensional arrays.
!   \item[NX]
!      Size of the DE array dimension aligned with the first horizontal dimension of arrays 
!   \item[NY]
!      Size of the DE array dimension aligned with the second horizontal dimension of arrays 
!   \item[NX0, NY0]
!      Coordinates of current DE.
!   \item[LONS]
!      X coordinates of array locations. Currently longitude in radians.
!   \item[LATS]
!      Y coordinates of array locations. Currently latitude in radians.
!   \item[INTERNAL\_ESMF\_STATE]
!      The gridded component's INTERNAL state.
!   \item[GCNames]
!      Names of the children.
!   \item[GCS]
!      The child gridded components.
!   \item[GIM]
!      The childrens' IMPORT states.
!   \item[GEX]
!      The childrens' EXPORT states.
!   \item[CCS]
!      Array of child-to-child couplers.
!   \end{description}
    !EOPI

    character(len=ESMF_MAXSTR), parameter :: IAm = "MAPL_GenericStateGet"
    integer                               :: STATUS

    real                                  :: ECC
    real                                  :: OB
    real                                  :: PER
    integer                               :: EQNX
    logical                               :: FIX_SUN
    character(len=ESMF_MAXSTR)            :: gname

    logical :: EOT, ORBIT_ANAL2B
    integer :: ORB2B_REF_YYYYMMDD, ORB2B_REF_HHMMSS, &
      ORB2B_EQUINOX_YYYYMMDD, ORB2B_EQUINOX_HHMMSS
    real :: ORB2B_YEARLEN, &
      ORB2B_ECC_REF, ORB2B_ECC_RATE, &
      ORB2B_OBQ_REF, ORB2B_OBQ_RATE, &
      ORB2B_LAMBDAP_REF, ORB2B_LAMBDAP_RATE

     if(present(IM)) then
      IM=STATE%GRID%IM
     endif

     if(present(JM)) then
      JM=STATE%GRID%JM
     endif

     if(present(LM)) then
      LM=STATE%GRID%LM
     endif

     if(present(VERTDIM)) then
      VERTDIM=STATE%GRID%VERTDIM
     endif

     if(present(NX)) then
      NX=STATE%GRID%NX
     endif

     if(present(NY)) then
      NY=STATE%GRID%NY
     endif

     if(present(NX0)) then
      NX0=STATE%GRID%NX0
     endif

     if(present(NY0)) then
      NY0=STATE%GRID%NY0
     endif

     if(present(LAYOUT)) then
      LAYOUT=STATE%GRID%LAYOUT
     endif

     if(present(CF)) then
      CF=STATE%CF
     endif

     ! pmn: There is one orbit is per STATE, so, for example, the MAPL states of the
     ! solar and land gridded components can potentially have independent solar orbits.
     ! Usually these "independent orbits" will be IDENTICAL because the configuration
     ! resources such as "ECCENTRICITY:" or "EOT:" will not be qualified by the name
     ! of the gridded component. But for example, if the resource file specifies
     !   "EOT: .FALSE."
     ! but
     !   "SOLAR_EOT: .TRUE."
     ! then only SOLAR will have an EOT correction. The same goes for the new orbital
     ! system choice ORBIT_ANAL2B.
     !   A state's orbit is actually created in this routine by requesting the ORBIT
     ! object. If its not already created then it will be made below. GridComps that
     ! don't needed an orbit and dont request one will not have one.

     if(present(ORBIT)) then

        if(.not.MAPL_SunOrbitCreated(STATE%ORBIT)) then

           call ESMF_GridGet(STATE%GRID%ESMFGRID,name=gname,rc=status)
           _VERIFY(STATUS)
           if (index(gname,"DP")>0) then
              FIX_SUN=.true.
           else
              FIX_SUN=.false.
           end if 

           ! Fixed parameters of standard orbital system (tabularized intercalation cycle)
           ! -----------------------------------------------------------------------------

           call MAPL_GetResource(STATE, ECC, Label="ECCENTRICITY:", default=0.0167, &
                  RC=STATUS)
           _VERIFY(STATUS)

           call MAPL_GetResource(STATE, OB, Label="OBLIQUITY:", default=23.45, &
                RC=STATUS)
           _VERIFY(STATUS)

           call MAPL_GetResource(STATE, PER, Label="PERIHELION:", default=102.0, &
                RC=STATUS)
           _VERIFY(STATUS)

           call MAPL_GetResource(STATE, EQNX, Label="EQUINOX:", default=80, &
                RC=STATUS)
           _VERIFY(STATUS)

           ! Apply Equation of Time correction?
           ! ----------------------------------
           call MAPL_GetResource(STATE, EOT, Label="EOT:", default=.FALSE., &
                RC=STATUS)
           _VERIFY(STATUS)

           ! New orbital system (analytic two-body) allows some time-varying
           ! behavior, namely, linear variation in LAMBDAP, ECC, and OBQ.
           ! ---------------------------------------------------------------

           call MAPL_GetResource(STATE, &
                ORBIT_ANAL2B, Label="ORBIT_ANAL2B:", default=.FALSE., &
                RC=STATUS)
           _VERIFY(STATUS)

           ! Fixed anomalistic year length in mean solar days
           call MAPL_GetResource(STATE, &
                ORB2B_YEARLEN, Label="ORB2B_YEARLEN:", default=365.2596, &
                RC=STATUS)
           _VERIFY(STATUS)

           ! Reference date and time for orbital parameters
           ! (defaults to J2000 = 01Jan2000 12:00:00 TT = 11:58:56 UTC)
           call MAPL_GetResource(STATE, &
                ORB2B_REF_YYYYMMDD, Label="ORB2B_REF_YYYYMMDD:", default=20000101, &
                RC=STATUS)
           _VERIFY(STATUS)
           call MAPL_GetResource(STATE, &
                ORB2B_REF_HHMMSS, Label="ORB2B_REF_HHMMSS:", default=115856, &
                RC=STATUS)
           _VERIFY(STATUS)

           ! Orbital eccentricity at reference date
           call MAPL_GetResource(STATE, &
                ORB2B_ECC_REF, Label="ORB2B_ECC_REF:", default=0.016710, &
                RC=STATUS)
           _VERIFY(STATUS)

           ! Rate of change of orbital eccentricity per Julian century
           call MAPL_GetResource(STATE, &
                ORB2B_ECC_RATE, Label="ORB2B_ECC_RATE:", default=-4.2e-5, &
                RC=STATUS)
           _VERIFY(STATUS)

           ! Earth's obliquity (axial tilt) at reference date [degrees]
           call MAPL_GetResource(STATE, &
                ORB2B_OBQ_REF, Label="ORB2B_OBQ_REF:", default=23.44, &
                RC=STATUS)
           _VERIFY(STATUS)

           ! Rate of change of obliquity [degrees per Julian century]
           call MAPL_GetResource(STATE, &
                ORB2B_OBQ_RATE, Label="ORB2B_OBQ_RATE:", default=-1.3e-2, &
                RC=STATUS)
           _VERIFY(STATUS)

           ! Longitude of perihelion at reference date [degrees]
           !   (from March equinox to perihelion in direction of earth's motion)
           call MAPL_GetResource(STATE, &
                ORB2B_LAMBDAP_REF, Label="ORB2B_LAMBDAP_REF:", default=282.947, &
                RC=STATUS)
           _VERIFY(STATUS)

           ! Rate of change of LAMBDAP [degrees per Julian century]
           !   (Combines both equatorial and ecliptic precession)
           call MAPL_GetResource(STATE, &
                ORB2B_LAMBDAP_RATE, Label="ORB2B_LAMBDAP_RATE:", default=1.7195, &
                RC=STATUS)
           _VERIFY(STATUS)

           ! March Equinox date and time
           ! (defaults to March 20, 2000 at 07:35:00 UTC)
           call MAPL_GetResource(STATE, &
                ORB2B_EQUINOX_YYYYMMDD, Label="ORB2B_EQUINOX_YYYYMMDD:", default=20000320, &
                RC=STATUS)
           _VERIFY(STATUS)
           call MAPL_GetResource(STATE, &
                ORB2B_EQUINOX_HHMMSS, Label="ORB2B_EQUINOX_HHMMSS:", default=073500, &
                RC=STATUS)
           _VERIFY(STATUS)

           ! create the orbit object
           STATE%ORBIT = MAPL_SunOrbitCreate(STATE%CLOCK, ECC, OB, PER, EQNX, &
                                             EOT, ORBIT_ANAL2B, ORB2B_YEARLEN, &
                                             ORB2B_REF_YYYYMMDD, ORB2B_REF_HHMMSS, &
                                             ORB2B_ECC_REF, ORB2B_ECC_RATE, &
                                             ORB2B_OBQ_REF, ORB2B_OBQ_RATE, &
                                             ORB2B_LAMBDAP_REF, ORB2B_LAMBDAP_RATE, &
                                             ORB2B_EQUINOX_YYYYMMDD, ORB2B_EQUINOX_HHMMSS, &
                                             FIX_SUN=FIX_SUN,RC=STATUS)
           _VERIFY(STATUS)

        end if
        ORBIT=STATE%ORBIT
     end if

     if(present(RUNALARM)) then
      RUNALARM=STATE%ALARM(0)
     endif

     if(present(LONS    )) then
      LONS   =>STATE%GRID%LONS
     endif

     if(present(LATS    )) then
      LATS   =>STATE%GRID%LATS
     endif

     if(present(IMPORTspec)) then
      IMPORTspec =>STATE%COMPONENT_SPEC%IMPORT%OLD_VAR_SPECS
     endif

     if(present(EXPORTspec)) then
      EXPORTspec =>STATE%COMPONENT_SPEC%EXPORT%OLD_VAR_SPECS
     endif

     if(present(INTERNALspec)) then
      INTERNALspec =>STATE%COMPONENT_SPEC%INTERNAL%OLD_VAR_SPECS
     endif

     if(present(INTERNAL_ESMF_STATE)) then
      INTERNAL_ESMF_STATE = STATE%get_internal_state()
     endif

     if(present(TILETYPES)) then
        call MAPL_LocStreamGet(STATE%LocStream, TILETYPE=TILETYPES, RC=STATUS)
        _VERIFY(STATUS)
     end if

     if(present(TILEKIND)) then
        call MAPL_LocStreamGet(STATE%LocStream, TILEKIND=TILEKIND, RC=STATUS)
        _VERIFY(STATUS)
     end if

     if(present(TILELONS)) then
        call MAPL_LocStreamGet(STATE%LocStream, TILELONS=TILELONS, RC=STATUS)
        _VERIFY(STATUS)
     end if

     if(present(TILELATS)) then
        call MAPL_LocStreamGet(STATE%LocStream, TILELATS=TILELATS, RC=STATUS)
        _VERIFY(STATUS)
     end if

     if(present(TILEAREA)) then
        call MAPL_LocStreamGet(STATE%LocStream, TILEAREA=TILEAREA, RC=STATUS)
        _VERIFY(STATUS)
     end if

     if(present(LOCSTREAM)) then
      LOCSTREAM = STATE%LOCSTREAM
     endif

     if(present(EXCHANGEGRID)) then
      EXCHANGEGRID = STATE%EXCHANGEGRID
     endif

     if(present(CLOCK)) then
      CLOCK = STATE%CLOCK
     endif

     if(present(CCS)) then
      CCS => STATE%CCS
     endif

     if(present(GCNames )) then
        if (.not. allocated(STATE%GCNamelist)) allocate(STATE%GCNamelist(0))
        GCNames => STATE%GCNameList
     endif

     if(present(childrens_names )) then
        if (.not. allocated(STATE%GCNamelist)) allocate(STATE%GCNamelist(0))
        childrens_names = STATE%GCNameList
     endif

     if(present(GCS)) then
        block
          integer i, nc
          nc = STATE%get_num_children()
          allocate(GCS(nc))
          do i = 1, nc
             GCS(i) = STATE%get_child_gridcomp(i)
          end do
        end block
     endif

     if(present(childrens_gridcomps)) then
        block
          integer i, nc
          nc = STATE%get_num_children()
          allocate(childrens_gridcomps(nc))
          do i = 1, nc
             childrens_gridcomps(i) = STATE%get_child_gridcomp(i)
          end do
        end block
     endif

     if(present(GIM)) then
        block
          integer i, nc
          nc = STATE%get_num_children()
          allocate(GIM(nc))
          do i = 1, nc
             GIM(i) = state%get_child_import_state(i)
          end do
        end block
     endif

     if(present(childrens_import_states)) then
        block
          integer i, nc
          nc = STATE%get_num_children()
          allocate(childrens_import_states(nc))
          do i = 1, nc
             childrens_import_states(i) = state%get_child_import_state(i)
          end do
        end block
     endif

     if(present(GEX)) then
        block
          integer i, nc
          nc = STATE%get_num_children()
          allocate(GEX(nc))
          do i = 1, nc
             GEX(i) = state%get_child_export_state(i)
          end do
        end block
     endif

     if(present(childrens_export_states)) then
        block
          integer i, nc
          nc = STATE%get_num_children()
          allocate(childrens_export_states(nc))
          do i = 1, nc
             childrens_export_states(i) = state%get_child_export_state(i)
          end do
        end block
     endif
     if(present(HEARTBEAT)) then
      HEARTBEAT = STATE%HEARTBEAT
     endif

     if(present(NumInitPhases)) then
        NumInitPhases = SIZE(STATE%PHASE_INIT)
     endif

     if(present(NumRunPhases)) then
        NumRunPhases = SIZE(STATE%PHASE_RUN)
     endif

    _RETURN(ESMF_SUCCESS)
  end subroutine MAPL_GenericStateGet

  

  !BOPI
  ! !IROUTINE: MAPL_Set
  ! !IIROUTINE: MAPL_GenericStateSet

  !INTERFACE:
  subroutine MAPL_GenericStateSet (STATE, ORBIT, LM, RUNALARM, CHILDINIT, &
                                   LOCSTREAM, EXCHANGEGRID, CLOCK, NAME,  &
                                   CF, ConfigFile, component, RC)

    use mapl_AbstractComposite
    use mapl_ConcreteComposite
    !ARGUMENTS:
    type (MAPL_MetaComp),            intent(INOUT) :: STATE
    type (ESMF_Alarm),     optional, intent(IN   ) :: RUNALARM
    type (MAPL_SunOrbit),  optional, intent(IN   ) :: ORBIT
    integer,               optional, intent(IN   ) :: LM
    logical,               optional, intent(IN   ) :: CHILDINIT
    type (MAPL_LocStream), optional, intent(IN   ) :: LOCSTREAM
    type (MAPL_LocStream), optional, intent(IN   ) :: EXCHANGEGRID
    type (ESMF_Clock)    , optional, intent(IN   ) :: CLOCK
    type (ESMF_Config)   , optional, intent(IN   ) :: CF
    character(len=*)     , optional, intent(IN   ) :: NAME
    character(len=*)     , optional, intent(IN   ) :: ConfigFile
    class(AbstractComponent), optional, intent(in) :: component
    integer,               optional, intent(  OUT) :: RC
    !EOPI

    character(len=ESMF_MAXSTR), parameter :: IAm = "MAPL_GenericStateSet"
    integer :: STATUS

    class(AbstractComposite), pointer :: composite
    type(ConcreteComposite), pointer :: t_composite
    ! Fixup uninitialized METAs


    ! Fixup uninitialized META objs.
    composite => STATE%get_composite()
    if (.not. associated(composite)) then
       allocate(t_composite)
       call t_composite%initialize(STATE)
       composite => t_composite
       call STATE%set_composite(composite)
    end if
    
     if(present(LM)) then
      STATE%GRID%LM=LM
     endif

     if(present(ORBIT)) then
      STATE%ORBIT=ORBIT
     endif

     if(present(RUNALARM)) then
      STATE%ALARM(0)=RUNALARM
     endif

     if(present(CHILDINIT)) then
      STATE%CHILDINIT=CHILDINIT
     endif

     if(present(LOCSTREAM)) then
      STATE%LOCSTREAM=LOCSTREAM
     endif

     if(present(EXCHANGEGRID)) then
      STATE%EXCHANGEGRID=EXCHANGEGRID
     endif

     if(present(CLOCK)) then
      STATE%CLOCK=CLOCK
     endif

     if(present(NAME)) then
        STATE%COMPNAME=NAME
        if (.not. allocated(state%full_name)) then
           state%full_name = trim(name)
        end if
     endif

     if(present(Cf)) then
        STATE%CF=CF
     endif

     if(present(ConfigFile)) then
        State%CF = ESMF_ConfigCreate(rc=status)
        _VERIFY(STATUS)
        call ESMF_ConfigLoadFile(State%CF,ConfigFile,rc=STATUS)
        _VERIFY(STATUS)
     endif

     if (present(component)) then
        call state%set_component(component)
        call state%set_logger(logging%get_logger(state%full_name))
     end if

    _RETURN(ESMF_SUCCESS)
  end subroutine MAPL_GenericStateSet


  !BOPI
  ! !IIROUTINE: MAPL_GenericStateSetFromGC

  !INTERFACE:
  subroutine MAPL_GenericStateSetFromGC (GC, ORBIT, LM, RUNALARM, CHILDINIT, &
                                         LOCSTREAM, EXCHANGEGRID, CLOCK, RC)
    !ARGUMENTS:
    type (ESMF_GridComp),            intent(INout) :: GC
    type (ESMF_Alarm),     optional, intent(IN   ) :: RUNALARM
    type (MAPL_SunOrbit),  optional, intent(IN   ) :: ORBIT
    integer,               optional, intent(IN   ) :: LM
    logical,               optional, intent(IN   ) :: CHILDINIT
    type (MAPL_LocStream), optional, intent(IN   ) :: LOCSTREAM
    type (MAPL_LocStream), optional, intent(IN   ) :: EXCHANGEGRID
    type (ESMF_Clock)    , optional, intent(IN   ) :: CLOCK
    integer,               optional, intent(  OUT) :: RC
    !EOPI

    character(len=ESMF_MAXSTR), parameter :: IAm = "MAPL_GenericStateSetFromGC"
    integer :: STATUS

    type (MAPL_MetaComp), pointer         :: STATE

    call MAPL_InternalStateGet ( GC, STATE, RC=STATUS )
    _VERIFY(STATUS)

    call MAPL_GenericStateSet (STATE, ORBIT, LM, RUNALARM, CHILDINIT, &
                               LOCSTREAM, EXCHANGEGRID, CLOCK, RC=STATUS)
    _VERIFY(STATUS)

    _RETURN(ESMF_SUCCESS)
  end subroutine MAPL_GenericStateSetFromGC



  !BOPI
  ! !IROUTINE: MAPL_GenericRunCouplers
  
  !INTERFACE:
  subroutine MAPL_GenericRunCouplers( STATE, CHILD, CLOCK, RC )
    
    !ARGUMENTS:
    type (MAPL_MetaComp),    intent(INOUT) :: STATE
    integer,                 intent(IN   ) :: CHILD  ! Child Id
    type(ESMF_Clock),        intent(INOUT) :: CLOCK  ! The clock
    integer, optional,       intent(  OUT) :: RC     ! Error code
    !EOPI

    ! ErrLog Variables
    character(len=ESMF_MAXSTR), parameter :: IAm="MAPL_GenericRunCouplers"
    integer                               :: STATUS
    integer                               :: userRC
    integer                               :: J
    type(ESMF_State), pointer :: child_import_state
    type(ESMF_State), pointer :: child_export_state
    
       do J=1,STATE%get_num_children()
          if(STATE%CCcreated(CHILD,J)) then
             child_export_state => STATE%get_child_export_state(child)
             child_import_state => STATE%get_child_import_state(j)
           call ESMF_CplCompRun (STATE%CCS(CHILD,J), &
                importState=child_export_state, &
                exportState=child_import_state, &
                clock=CLOCK, userRC=userRC, RC=STATUS )
             _ASSERT(userRC==ESMF_SUCCESS .and. STATUS==ESMF_SUCCESS,'needs informative message')
          endif
       enddo
    
    _RETURN(ESMF_SUCCESS)
    
  end subroutine MAPL_GenericRunCouplers



  !BOPI
  ! !IROUTINE: MAPL_StatePrintSpecCSV

  !INTERFACE: 
  recursive subroutine MAPL_StatePrintSpecCSV(GC, printSpec, RC)
    
    !ARGUMENTS:
    type(ESMF_GridComp),           intent(INOUT)  :: GC
    integer,                       intent(IN   )  :: printSpec
    integer,             optional, intent(  OUT)  :: RC
    !EOPI

!=============================================================================
!
! ErrLog Variables

    character(len=ESMF_MAXSTR)            :: IAm='MAPL_StatePrintSpecCSV'
    integer                               :: STATUS
    character(len=ESMF_MAXSTR)            :: COMP_NAME

! Local variables
! ---------------

    type (MAPL_MetaComp),              pointer  :: MAPLOBJ 

    type (MAPL_VarSpec),               pointer  :: IMPORT_SPEC(:)
    type (MAPL_VarSpec),               pointer  :: EXPORT_SPEC(:)
    integer                                     :: I
    type(ESMF_GridComp), pointer :: gridcomp

!EOP

! Get my name and set-up traceback handle
! ---------------------------------------

    call ESMF_GridCompGet( GC, NAME=COMP_NAME, RC=STATUS )
    _VERIFY(STATUS)
    Iam = trim(COMP_NAME) // trim(Iam)

! Retrieve the pointer to the internal state
! --------------------------------------------

    call MAPL_InternalStateRetrieve ( GC, MAPLOBJ, RC=STATUS )
    _VERIFY(STATUS)

    IMPORT_SPEC => MAPLOBJ%COMPONENT_SPEC%IMPORT%OLD_VAR_SPECS
    EXPORT_SPEC => MAPLOBJ%COMPONENT_SPEC%EXPORT%OLD_VAR_SPECS

    if (printSpec == 1) then
       if (associated(IMPORT_SPEC)) then
          call WRITE_PARALLEL("#IMPORT spec for " // trim(COMP_NAME))
          call WRITE_PARALLEL("#COMPONENT, SHORT_NAME, LONG_NAME, UNIT, DIMS")
          if (associated(IMPORT_SPEC)) then
             call MAPL_VarSpecPrintCSV(IMPORT_SPEC, COMP_NAME, RC=STATUS)
             _VERIFY(STATUS)
          end if
       end if
       if (associated(EXPORT_SPEC)) then
          call WRITE_PARALLEL("#EXPORT spec for " // trim(COMP_NAME))
          call WRITE_PARALLEL("#COMPONENT, SHORT_NAME, LONG_NAME, UNIT, DIMS")
          if (associated(EXPORT_SPEC)) then
             call MAPL_VarSpecPrintCSV(EXPORT_SPEC, COMP_NAME, RC=STATUS)
             _VERIFY(STATUS)
          end if
       end if
    else if (printSpec == 2) then
       if (associated(IMPORT_SPEC)) then
          call WRITE_PARALLEL("#IMPORT spec for " // trim(COMP_NAME))
          call WRITE_PARALLEL("#COMPONENT, SHORT_NAME, LONG_NAME, UNIT, DIMS")
          if (associated(IMPORT_SPEC)) then
             call MAPL_VarSpecPrintCSV(IMPORT_SPEC, COMP_NAME, RC=STATUS)
             _VERIFY(STATUS)
          end if
       end if
    else if (printSpec == 3) then
       if (associated(EXPORT_SPEC)) then
          call WRITE_PARALLEL("#EXPORT spec for " // trim(COMP_NAME))
          call WRITE_PARALLEL("#COMPONENT, SHORT_NAME, LONG_NAME, UNIT, DIMS")
          if (associated(EXPORT_SPEC)) then
             call MAPL_VarSpecPrintCSV(EXPORT_SPEC, COMP_NAME, RC=STATUS)
             _VERIFY(STATUS)
          end if
       end if
    end if

       do I = 1, MAPLOBJ%get_num_children()
          gridcomp => MAPLOBJ%GET_CHILD_GRIDCOMP(I)
          call MAPL_StatePrintSpecCSV(gridcomp, printSpec, RC=STATUS)
          _VERIFY(STATUS)
       end do

    _RETURN(ESMF_SUCCESS)
  end subroutine MAPL_StatePrintSpecCSV


  !BOPI
  ! !IROUTINE: MAPL_AddChild
  ! !IIROUTINE: MAPL_AddChildFromMeta --- From Meta

  !INTERFACE:
  recursive integer function MAPL_AddChildFromMeta(META, NAME, GRID, &
                                                   CONFIGFILE, SS, PARENTGC, &
                                                   petList, RC)

    !ARGUMENTS:
    type(MAPL_MetaComp), target,   intent(INOUT) :: META
    character(len=*),              intent(IN   ) :: NAME
    type(ESMF_Grid),  optional,    intent(INout) :: GRID
    character(len=*), optional,    intent(IN   ) :: CONFIGFILE
    external                                     :: SS
    type(ESMF_GridComp), optional, intent(IN   ) :: parentGC
    integer,           optional  , intent(IN   ) :: petList(:)
    integer,           optional  , intent(  OUT) :: rc
    !EOPI

  integer                                     :: STATUS

  integer                                     :: I
  type(MAPL_MetaComp), pointer                :: CHILD_META, tmp_meta
  class(AbstractFrameworkComponent), pointer :: tmp_framework
  character(len=ESMF_MAXSTR), allocatable     :: TMPNL(:)
  character(len=ESMF_MAXSTR)                  :: FNAME, PNAME
  type(ESMF_GridComp)                         :: pGC
  type(ESMF_Context_Flag)                     :: contextFlag
  class(BaseProfiler), pointer                :: t_p

  class(Logger), pointer :: lgr
  type(ESMF_GridComp), pointer :: gridcomp
  type(ESMF_State), pointer :: child_import_state
  type(ESMF_State), pointer :: child_export_state
  type(StubComponent) :: stub_component
  type(ESMF_VM) :: vm
  integer :: comm 

  lgr => logging%get_logger('MAPL.GENERIC')


  if (.not.allocated(META%GCNameList)) then
     ! this is the first child to be added
     allocate(META%GCNameList(0), stat=status)
     _VERIFY(STATUS)
  end if

  I = META%get_num_children() + 1
  MAPL_AddChildFromMeta = I
! realloc gcnamelist 
  allocate(TMPNL(I), stat=status)
  _VERIFY(STATUS)
  TMPNL(1:I-1) = META%GCNameList
  deallocate(META%GCNameList)
  META%GCNameList = TMPNL

  FNAME = trim(NAME)
  if (index(NAME,":") == 0) then
     if (present(parentGC)) then
        pGC = parentGC
        call ESMF_GridCompGet(pGC, name = PNAME, RC=STATUS)
        _VERIFY(STATUS)
        FNAME = PNAME(1:index(PNAME,":"))//trim(NAME)
     end if
  end if

  allocate(tmp_meta, __STAT__)
  tmp_framework => META%add_child(FNAME, tmp_meta)
  deallocate(tmp_meta)
  _ASSERT(associated(tmp_framework),'add_child() failed')

  select type (tmp_framework)
  class is (MAPL_MetaComp)
     child_meta => tmp_framework
     call child_meta%set_component(stub_component)
     
     if (present(petList)) then
        contextFlag = ESMF_CONTEXT_OWN_VM ! this is default
     else
        contextFlag = ESMF_CONTEXT_PARENT_VM ! more efficient
     end if
     
     META%GCNameList(I) = trim(FNAME)

     gridcomp => child_meta%gridcomp
     if (present(configfile)) then
        gridcomp = ESMF_GridCompCreate   (     &
             NAME   = trim(FNAME),                 &
             CONFIGFILE = configfile,             &
             grid = grid,                         &
             petList = petList,                   &
             contextFlag = contextFlag,           &
             RC=STATUS )
        _VERIFY(STATUS)
     else
        gridcomp = ESMF_GridCompCreate   (     &
             NAME   = trim(FNAME),                 &
             CONFIG = META%CF,                    &
             grid = grid,                         &
             petList = petList,                   &
             contextFlag = contextFlag,           &
             RC=STATUS )
        _VERIFY(STATUS)
     end if

! Create each child's import/export state
! ----------------------------------

     child_import_state => META%get_child_import_state(i)
     child_import_state = ESMF_StateCreate (                         & 
          NAME = trim(META%GCNameList(I)) // '_Imports', &
          stateIntent = ESMF_STATEINTENT_IMPORT, &
          RC=STATUS )
     _VERIFY(STATUS)

     child_export_state => META%get_child_export_state(i)
     child_export_state = ESMF_StateCreate (                         &
          NAME = trim(META%GCNameList(I)) // '_Exports', &
          stateIntent = ESMF_STATEINTENT_EXPORT, &
       RC=STATUS )
  _VERIFY(STATUS)

! create MAPL_Meta
  call MAPL_InternalStateCreate ( gridcomp, CHILD_META, RC=STATUS)
  _VERIFY(STATUS)

  end select

! put parentGC there
  if (present(parentGC)) then
     allocate(CHILD_META%parentGC, stat=status)
     _VERIFY(STATUS)
     CHILD_META%parentGC = parentGC
  end if

  call lgr%debug('Adding logger for component %a ',trim(fname))
  child_meta%full_name = meta%full_name // SEPARATOR // trim(fname)
  child_meta%compname = trim(fname)
  call child_meta%set_logger(logging%get_logger(child_meta%full_name))

  ! copy communicator to childs mapl_metacomp
  call ESMF_VMGetCurrent(vm, __RC__)
  call ESMF_VMGet(vm, mpiCommunicator=comm, __RC__)
  CHILD_META%t_profiler = TimeProfiler(trim(NAME), comm_world=comm)

  t_p => get_global_time_profiler()

  call t_p%start(trim(NAME),__RC__)
  call CHILD_META%t_profiler%start(__RC__)
  call CHILD_META%t_profiler%start('SetService',__RC__)
  gridcomp => META%GET_CHILD_GRIDCOMP(I)
  call ESMF_GridCompSetServices ( gridcomp, SS, RC=status )
  call CHILD_META%t_profiler%stop('SetService',__RC__)
  call CHILD_META%t_profiler%stop(__RC__)
  call t_p%stop(trim(NAME),__RC__)
  
  _VERIFY(STATUS)

  _RETURN(ESMF_SUCCESS)
end function MAPL_AddChildFromMeta


!BOPI
! !IIROUTINE: MAPL_AddChildFromGC --- From gc

!INTERFACE:
recursive integer function MAPL_AddChildFromGC(GC, NAME, SS, petList, configFile, RC)

  !ARGUMENTS:
  type(ESMF_GridComp), intent(INOUT) :: GC
  character(len=*),    intent(IN   ) :: NAME
  external                           :: SS
  integer, optional  , intent(IN   ) :: petList(:)
  character(len=*), optional, intent(IN   ) :: configFile
  integer, optional  , intent(  OUT) :: rc
  !EOPI
  
  character(len=ESMF_MAXSTR)                  :: IAm
  integer                                     :: STATUS

  type(MAPL_MetaComp), pointer                :: META

  Iam = "MAPL_AddChildFromGC"
  call MAPL_InternalStateRetrieve(GC, META, RC=status)
  _VERIFY(STATUS)

  MAPL_AddChildFromGC = MAPL_AddChildFromMeta(Meta, NAME, SS=SS, PARENTGC = GC, petList=petList, configFile=configFile, RC=status)
  _VERIFY(STATUS)

  _RETURN(ESMF_SUCCESS)
end function MAPL_AddChildFromGC


!INTERFACE:
recursive integer function MAPL_AddChildFromDSO(NAME, userRoutine, grid, ParentGC, SharedObj, petList, configFile, RC)

  !ARGUMENTS:
  character(len=*), intent(IN)    :: NAME
  character(len=*), intent(in)    :: userRoutine
  type(ESMF_Grid),  optional,    intent(INout) :: grid
  type(ESMF_GridComp), optional, intent(INOUT) :: ParentGC
  character(len=*), optional, intent(in)       :: sharedObj

  integer, optional  , intent(IN   ) :: petList(:)
  character(len=*), optional, intent(IN   ) :: configFile
  integer, optional  , intent(  OUT) :: rc
  !EOP
  
  character(len=ESMF_MAXSTR)                  :: IAm
  integer                                     :: STATUS

  type(MAPL_MetaComp), pointer                :: META

  integer                                     :: I
  type(MAPL_MetaComp), pointer                :: CHILD_META, tmp_meta
  class(AbstractFrameworkComponent), pointer :: tmp_framework
  type(ESMF_GridComp), pointer                :: TMPGCS(:)
  type(ESMF_State)   , pointer                :: TMPGIM(:)
  type(ESMF_State)   , pointer                :: TMPGEX(:)
  character(len=ESMF_MAXSTR), pointer         :: TMPNL(:)
  character(len=ESMF_MAXSTR)                  :: FNAME, PNAME
  type(ESMF_GridComp)                         :: pGC
  type(ESMF_Context_Flag)                     :: contextFlag
  class(BaseProfiler), pointer                :: t_p

  class(Logger), pointer :: lgr
  type(ESMF_GridComp), pointer :: gridcomp
  type(ESMF_State), pointer :: child_import_state
  type(ESMF_State), pointer :: child_export_state
  type(StubComponent) :: stub_component
  type(ESMF_VM) :: vm
  integer :: comm 

  lgr => logging%get_logger('MAPL.GENERIC')

  Iam = "MAPL_AddChildFromDSO"

  if (present(ParentGC)) then
     call MAPL_InternalStateRetrieve(ParentGC, META, RC=status)
     _VERIFY(STATUS)
  endif

  if (.not. allocated(META%GCNamelist)) then
     ! this is the first child to be added
     allocate(META%GCNameList(0), stat=status)
     _VERIFY(STATUS)
  end if

  I = META%get_num_children() + 1
  MAPL_AddChildFromDSO = I
  allocate(TMPNL(I), stat=status)
  _VERIFY(STATUS)
  TMPNL(1:I-1) = META%GCNameList
  deallocate(META%GCNameList)

  FNAME = trim(NAME)
  if (index(NAME,":") == 0) then
     if (present(parentGC)) then
        pGC = parentGC
        call ESMF_GridCompGet(pGC, name = PNAME, RC=STATUS)
        _VERIFY(STATUS)
        FNAME = PNAME(1:index(PNAME,":"))//trim(NAME)
     end if
  end if

  allocate(tmp_meta, __STAT__)
  tmp_framework => META%add_child(FNAME, tmp_meta)
  deallocate(tmp_meta)
  _ASSERT(associated(tmp_framework),'add_child() failed')

  if (present(petList)) then
     contextFlag = ESMF_CONTEXT_OWN_VM ! this is default
  else
     contextFlag = ESMF_CONTEXT_PARENT_VM ! more efficient
  end if

  META%GCNameList(I) = trim(FNAME)

  gridcomp => child_meta%gridcomp
  if (present(configfile)) then
     gridcomp = ESMF_GridCompCreate   (     &
          NAME   = trim(FNAME),                 &
          CONFIGFILE = configfile,             &
          grid = grid,                         &
          petList = petList,                   &
          contextFlag = contextFlag,           &
          RC=STATUS )
     _VERIFY(STATUS)
  else
     gridcomp = ESMF_GridCompCreate   (     &
          NAME   = trim(FNAME),                 &
          CONFIG = META%CF,                    &
          grid = grid,                         &
          petList = petList,                   &
          contextFlag = contextFlag,           &
          RC=STATUS )
     _VERIFY(STATUS)
  end if

! Create each child's import/export state
! ----------------------------------

! Create each child's import/export state
! ----------------------------------

     child_import_state => META%get_child_import_state(i)
     child_import_state = ESMF_StateCreate (                         & 
          NAME = trim(META%GCNameList(I)) // '_Imports', &
          stateIntent = ESMF_STATEINTENT_IMPORT, &
          RC=STATUS )
     _VERIFY(STATUS)

     child_export_state => META%get_child_export_state(i)
     child_export_state = ESMF_StateCreate (                         &
          NAME = trim(META%GCNameList(I)) // '_Exports', &
          stateIntent = ESMF_STATEINTENT_EXPORT, &
       RC=STATUS )
  _VERIFY(STATUS)

! create MAPL_Meta
  call MAPL_InternalStateCreate ( gridcomp, CHILD_META, RC=STATUS)
  _VERIFY(STATUS)

! put parentGC there
  if (present(parentGC)) then
     allocate(CHILD_META%parentGC, stat=status)
     _VERIFY(STATUS)
     CHILD_META%parentGC = parentGC
  end if

  call lgr%debug('Adding logger for component %a ',trim(fname))
  child_meta%full_name = meta%full_name // SEPARATOR // trim(fname)
  child_meta%compname = trim(fname)
  call child_meta%set_logger(logging%get_logger(child_meta%full_name))

  ! copy communicator to childs mapl_metacomp
  call ESMF_VMGetCurrent(vm, __RC__)
  call ESMF_VMGet(vm, mpiCommunicator=comm, __RC__)
  CHILD_META%t_profiler = TimeProfiler(trim(NAME), comm_world=comm)

  t_p => get_global_time_profiler()
  call t_p%start(trim(NAME),__RC__)
  call CHILD_META%t_profiler%start(__RC__)
  call CHILD_META%t_profiler%start('SetService',__RC__)
  gridcomp => META%GET_CHILD_GRIDCOMP(I)
  call ESMF_GridCompSetServices ( gridcomp, userRoutine, sharedObj=sharedObj,__RC__)
  call CHILD_META%t_profiler%stop('SetService',__RC__)
  call CHILD_META%t_profiler%stop(__RC__)
  call t_p%stop(trim(NAME),__RC__)
  
  _VERIFY(STATUS)

  _RETURN(ESMF_SUCCESS)
end function MAPL_AddChildFromDSO



  subroutine MAPL_AddConnectivityOld ( GC, SHORT_NAME, TO_NAME, &
       FROM_IMPORT, FROM_EXPORT, TO_IMPORT, TO_EXPORT, RC  )

    type(ESMF_GridComp),            intent(INOUT) :: GC ! Gridded component
    character (len=*)             , intent(IN   ) :: SHORT_NAME
    character (len=*),    optional, intent(IN   ) :: TO_NAME
    integer,              optional, intent(IN   ) :: FROM_IMPORT
    integer,              optional, intent(IN   ) :: FROM_EXPORT
    integer,              optional, intent(IN   ) :: TO_IMPORT
    integer,              optional, intent(IN   ) :: TO_EXPORT
    integer,              optional, intent(  OUT) :: RC     ! Error code:

    character(len=ESMF_MAXSTR), parameter :: IAm="MAPL_AddConnectivity"
    integer                               :: STATUS
    type (MAPL_Connectivity), pointer     :: conn


    call MAPL_ConnectivityGet(gc, connectivityPtr=conn, RC=status)
    _VERIFY(STATUS)

    call MAPL_VarConnCreate(CONN%CONNECT, SHORT_NAME, TO_NAME=TO_NAME,        &
                            FROM_IMPORT=FROM_IMPORT, FROM_EXPORT=FROM_EXPORT, &
                            TO_IMPORT  =TO_IMPORT,   TO_EXPORT  =TO_EXPORT, RC=STATUS  )
    _VERIFY(STATUS)

    _RETURN(ESMF_SUCCESS)
  end subroutine MAPL_AddConnectivityOld

  subroutine MAPL_AddConnectivityE2E ( GC, SHORT_NAME, &
       SRC_ID, TO_EXPORT, RC  )

    type(ESMF_GridComp),            intent(INOUT) :: GC ! Gridded component
    character (len=*),              intent(IN   ) :: SHORT_NAME
    integer,                        intent(IN   ) :: SRC_ID !FROM_EXPORT
    integer,                        intent(IN   ) :: TO_EXPORT
    integer,              optional, intent(  OUT) :: RC     ! Error code:

    character(len=ESMF_MAXSTR), parameter :: IAm="MAPL_AddConnectivityE2E"
    integer                               :: STATUS
    type (MAPL_Connectivity), pointer     :: conn

    call MAPL_ConnectivityGet(gc, connectivityPtr=conn, RC=status)
    _VERIFY(STATUS)

    call MAPL_VarConnCreate(CONN%CONNECT, SHORT_NAME, &
                            FROM_EXPORT=SRC_ID, &
                            TO_EXPORT=TO_EXPORT, RC=STATUS  )
    _VERIFY(STATUS)

    _RETURN(ESMF_SUCCESS)
  end subroutine MAPL_AddConnectivityE2E



  !BOPI
  ! !IROUTINE: MAPL_AddConnectivity
  ! !IIROUTINE: MAPL_AddConnectivityRename --- Rename

  !INTERFACE:
  subroutine MAPL_AddConnectivityRename ( GC, SRC_NAME, SRC_ID, &
                                          DST_NAME, DST_ID, RC  )

    !ARGUMENTS:
    type(ESMF_GridComp),            intent(INOUT) :: GC ! Gridded component
    character (len=*),              intent(IN   ) :: SRC_NAME !FROM_NAME==SHORT_NAME
    character (len=*),              intent(IN   ) :: DST_NAME !TO_NAME
    integer,                        intent(IN   ) :: SRC_ID !FROM_EXPORT
    integer,                        intent(IN   ) :: DST_ID !TO_IMPORT
    integer,              optional, intent(  OUT) :: RC     ! Error code:
    !EOPI

    character(len=ESMF_MAXSTR), parameter :: IAm="MAPL_AddConnectivityRename"
    integer                               :: STATUS

    type (MAPL_Connectivity), pointer     :: conn


    call MAPL_ConnectivityGet(gc, connectivityPtr=conn, RC=status)
    _VERIFY(STATUS)

    call MAPL_VarConnCreate(CONN%CONNECT, SHORT_NAME=SRC_NAME, TO_NAME=DST_NAME,        &
                            FROM_EXPORT=SRC_ID, TO_IMPORT=DST_ID, RC=STATUS  )
    _VERIFY(STATUS)

    _RETURN(ESMF_SUCCESS)
  end subroutine MAPL_AddConnectivityRename

  !BOPI
  ! !IIROUTINE: MAPL_AddConnectivityRenameMany --- Rename many

  !INTERFACE:
  subroutine MAPL_AddConnectivityRenameMany ( GC, SRC_NAME, SRC_ID, &
                                              DST_NAME, DST_ID, RC  )

    !ARGUMENTS:
    type(ESMF_GridComp),            intent(INOUT) :: GC ! Gridded component
    character (len=*),              intent(IN   ) :: SRC_NAME(:)
    character (len=*),              intent(IN   ) :: DST_NAME(:)
    integer,                        intent(IN   ) :: SRC_ID !FROM_EXPORT
    integer,                        intent(IN   ) :: DST_ID !TO_IMPORT
    integer,              optional, intent(  OUT) :: RC     ! Error code:
    !EOPI

    character(len=ESMF_MAXSTR), parameter :: IAm="MAPL_AddConnectivityRename"
    integer                               :: STATUS
    integer                               :: I

    DO I = 1, size(SRC_NAME)
       call MAPL_AddConnectivity ( GC, SRC_NAME=SRC_NAME(I), DST_NAME=DST_NAME(I), &
                                   SRC_ID=SRC_ID, DST_ID=DST_ID, RC=status  )
       _VERIFY(STATUS)
    END DO

    _RETURN(ESMF_SUCCESS)
  end subroutine MAPL_AddConnectivityRenameMany


  !BOPI
  ! !IIROUTINE: MAPL_AddConnectivityMany --- Many

  !INTERFACE:
  subroutine MAPL_AddConnectivityMany ( GC, SHORT_NAME, SRC_ID, DST_ID, RC  )

    !ARGUMENTS:
    type(ESMF_GridComp),            intent(INOUT) :: GC ! Gridded component
    character (len=*)             , intent(IN   ) :: SHORT_NAME(:)
    integer,                        intent(IN   ) :: SRC_ID
    integer,                        intent(IN   ) :: DST_ID
    integer,              optional, intent(  OUT) :: RC     ! Error code:
    !EOPI

    character(len=ESMF_MAXSTR), parameter :: IAm="MAPL_AddConnectivityMany"
    integer                               :: STATUS
    integer                               :: I

    DO I = 1, size(SHORT_NAME)
!ALT attention: DST_NAME needs to be revisited once we remove the old style interface to
!               MAPL_AddConnectivity
       call MAPL_AddConnectivityRENAME ( GC, SRC_NAME=SHORT_NAME(I), DST_NAME=SHORT_NAME(I), &
                                   SRC_ID=SRC_ID, DST_ID=DST_ID, RC=status  )
       _VERIFY(STATUS)
    END DO


    _RETURN(ESMF_SUCCESS)
  end subroutine MAPL_AddConnectivityMany


  !BOPI
  ! !IROUTINE: MAPL_TerminateImport
  ! !IIROUTINE: MAPL_DoNotConnect --- Do not connect

  !INTERFACE:
  subroutine MAPL_DoNotConnect ( GC, SHORT_NAME, CHILD, RC )

    !ARGUMENTS:
    type(ESMF_GridComp),            intent(INOUT) :: GC ! Gridded component
    character (len=*)             , intent(IN   ) :: SHORT_NAME
    integer,                        intent(IN   ) :: CHILD
    integer,              optional, intent(  OUT) :: RC     ! Error code:
    !EOPI

    character(len=ESMF_MAXSTR), parameter :: IAm="MAPL_DoNotConnect"
    integer                               :: STATUS
    type (MAPL_Connectivity), pointer     :: conn

    call MAPL_ConnectivityGet(gc, connectivityPtr=conn, RC=status)
    _VERIFY(STATUS)

    call MAPL_VarConnCreate(CONN%DONOTCONN, SHORT_NAME, &
       FROM_IMPORT=CHILD, RC=STATUS  )
    _VERIFY(STATUS)


    _RETURN(ESMF_SUCCESS)
  end subroutine MAPL_DoNotConnect



  !BOPI
  ! !IIROUTINE: MAPL_DoNotConnectMany --- Do not connect many

  !INTERFACE:
  subroutine MAPL_DoNotConnectMany ( GC, SHORT_NAME, CHILD, RC )

    !ARGUMENTS:
    type(ESMF_GridComp),            intent(INOUT) :: GC ! Gridded component
    character (len=*)             , intent(IN   ) :: SHORT_NAME(:)
    integer,                        intent(IN   ) :: CHILD
    integer,              optional, intent(  OUT) :: RC     ! Error code:
    !EOPI

    character(len=ESMF_MAXSTR), parameter :: IAm="MAPL_DoNotConnectMany"
    integer                               :: STATUS
    integer                               :: I


    DO I = 1, size(SHORT_NAME)
       call MAPL_DoNotConnect(GC, SHORT_NAME(I), CHILD, RC=status)
       _VERIFY(STATUS)
    END DO

    _RETURN(ESMF_SUCCESS)
  end subroutine MAPL_DoNotConnectMany


  !BOPI
  ! !IIROUTINE: MAPL_DoNotConnectAnyImport --- Do not connect any import

  !INTERFACE:
  subroutine MAPL_DoNotConnectAnyImport ( GC, CHILD, RC )

    !ARGUMENTS:
    type(ESMF_GridComp),            intent(INOUT) :: GC ! Gridded component
    integer,                        intent(IN   ) :: CHILD
    integer,              optional, intent(  OUT) :: RC     ! Error code:
    !EOPI

    character(len=ESMF_MAXSTR), parameter :: IAm="MAPL_DoNotConnectAnyImport"
    integer                               :: STATUS

    call MAPL_DoNotConnect ( GC, SHORT_NAME="MAPL_AnyChildImport", &
                             CHILD=CHILD, RC=STATUS )
    _VERIFY(STATUS)

    _RETURN(ESMF_SUCCESS)
  end subroutine MAPL_DoNotConnectAnyImport

  !BOPI
  ! !IIROUTINE: MAPL_TerminateImportAll --- Terminate import all except those specified

  !INTERFACE:
  subroutine MAPL_TerminateImportAllBut ( GC, SHORT_NAMES, CHILD_IDS, RC )

    !ARGUMENTS:
    type(ESMF_GridComp),            intent(INOUT) :: GC ! Gridded component
    character(len=*),               intent(IN   ) :: SHORT_NAMES(:)
    integer,                        intent(IN   ) :: CHILD_IDS(:)
    integer,              optional, intent(  OUT) :: RC     ! Error code:
    !EOPI

    character(len=ESMF_MAXSTR), parameter :: IAm="MAPL_TerminateImportAllBut"
    integer                               :: STATUS
    type(MAPL_MetaComp), pointer          :: META
    type(MAPL_MetaComp), pointer          :: META_CHILD
    character(len=ESMF_MAXSTR)            :: SHORT_NAME
    integer                               :: I,J
    logical                               :: SKIP
    character(len=ESMF_MAXSTR), allocatable :: SNAMES(:)
    type (MAPL_Connectivity), pointer     :: conn
    type (MAPL_VarConn), pointer          :: CONNECT(:)
    logical :: isConnected
    type(ESMF_GridComp), pointer :: gridcomp

    _ASSERT(size(SHORT_NAMES)==size(CHILD_IDS),'needs informative message')

    call MAPL_GetObjectFromGC(GC, META, RC=STATUS)
    _VERIFY(STATUS)

    allocate(SNAMES(size(SHORT_NAMES)))
    do I=1, size(SHORT_NAMES(:))
       SNAMES(I) = trim(SHORT_NAMES(I))
    enddo

    call MAPL_ConnectivityGet(gc, connectivityPtr=conn, RC=status)
    CONNECT => CONN%CONNECT
       do I=1, META%get_num_children()
          gridcomp => META%GET_CHILD_GRIDCOMP(I)
          call MAPL_GetObjectFromGC(gridcomp, META_CHILD, RC=STATUS)
          _VERIFY(STATUS)
          do J=1 ,size(META_CHILD%component_spec%import%old_var_specs)
             call MAPL_VarSpecGet(META_CHILD%component_spec%import%old_var_specs(J),SHORT_NAME=SHORT_NAME,RC=STATUS)
             isConnected = MAPL_VarIsConnected(connect,short_name,I,rc=status)
             SKIP = ANY(SNAMES==TRIM(SHORT_NAME)) .and. (ANY(CHILD_IDS==I))
             if ((.not.isConnected) .and. (.not.skip)) then
                call MAPL_DoNotConnect(GC, SHORT_NAME, I, RC=status)
                _VERIFY(STATUS)
             end if
          enddo
       end do

    deallocate(SNAMES)
    _RETURN(ESMF_SUCCESS)
  end subroutine MAPL_TerminateImportAllBut

  !BOPI
  ! !IIROUTINE: MAPL_TerminateImportAll --- Terminate import all

  !INTERFACE:
  subroutine MAPL_TerminateImportAll ( GC, ALL, RC )

    !ARGUMENTS:
    type(ESMF_GridComp),            intent(INOUT) :: GC ! Gridded component
    logical,                        intent(IN   ) :: ALL
    integer,              optional, intent(  OUT) :: RC     ! Error code:
    !EOPI

    character(len=ESMF_MAXSTR), parameter :: IAm="MAPL_TerminateImportAll"
    integer                               :: STATUS
    type(MAPL_MetaComp), pointer          :: META
    integer                               :: I

    _UNUSED_DUMMY(ALL)

    call MAPL_GetObjectFromGC(GC, META, RC=STATUS)
    _VERIFY(STATUS)

       do I=1, META%get_num_children()
          call MAPL_TerminateImport ( GC, CHILD=I, RC=STATUS )
          _VERIFY(STATUS)
       end do

    _RETURN(ESMF_SUCCESS)
  end subroutine MAPL_TerminateImportAll



  !BOPI
  ! !IROUTINE: MAPL_TimerOn
  ! !IIROUTINE: MAPL_GenericStateClockOn

  !INTERFACE:
  subroutine MAPL_GenericStateClockOn(STATE,NAME,RC)

    !ARGUMENTS:
    type (MAPL_MetaComp),        intent(INOUT) :: STATE
    character(len=*),            intent(IN   ) :: NAME
    integer, optional,           intent(  OUT) :: RC     ! Error code:
    !EOPI

    character(len=ESMF_MAXSTR), parameter :: IAm = "MAPL_GenericStateClockOn"
    integer :: STATUS !, n
   
    call MAPL_ProfClockOn(STATE%TIMES,NAME,RC=STATUS)
    _VERIFY(STATUS)
    
    !n = index(NAME,'-',.true.) + 1
    !call state%t_profiler%start(trim(Name(n:)))

    _RETURN(ESMF_SUCCESS)
  end subroutine MAPL_GenericStateClockOn

  subroutine MAPL_StateAlarmAdd(STATE,ALARM,RC)

    type (MAPL_MetaComp),        intent(INOUT) :: STATE
    type (ESMF_Alarm),           intent(IN   ) :: ALARM
    integer, optional,           intent(  OUT) :: RC     ! Error code

    character(len=ESMF_MAXSTR), parameter :: IAm = "MAPL_StateAlarmAdd"

    STATE%ALARMLAST = STATE%ALARMLAST + 1
    _ASSERT(STATE%ALARMLAST <= LAST_ALARM,'needs informative message')

    STATE%ALARM(STATE%ALARMLAST) = ALARM

    _RETURN(ESMF_SUCCESS)
  end subroutine MAPL_StateAlarmAdd

  subroutine MAPL_StateAlarmGet(STATE,ALARM,NAME,RC)
    type (MAPL_MetaComp),        intent(INOUT) :: STATE
    type (ESMF_Alarm),           intent(  OUT) :: ALARM
    character(len=*),            intent(IN   ) :: NAME
    integer, optional,           intent(  OUT) :: RC     ! Error code:

    character(len=ESMF_MAXSTR), parameter :: IAm = "MAPL_StateAlarmGet"
    integer :: STATUS, I
    character(len=ESMF_MAXSTR) :: ANAME

    do I=0,STATE%ALARMLAST
       call ESMF_AlarmGet(STATE%ALARM(I), name=ANAME, RC=STATUS)
       _VERIFY(STATUS)
       if(trim(NAME)/=trim(ANAME)) cycle
       ALARM=STATE%ALARM(I)
       _RETURN(ESMF_SUCCESS)
    end do

    _RETURN(ESMF_FAILURE)
  end subroutine MAPL_StateAlarmGet



!=============================================================================
!=============================================================================
!=============================================================================
!=============================================================================


  !BOPI
  ! !IROUTINE: MAPL_TimerOff
  ! !IIROUTINE: MAPL_GenericStateClockOff

  !INTERFACE:
  subroutine MAPL_GenericStateClockOff(STATE,NAME,RC)

    !ARGUMENTS:
    type (MAPL_MetaComp),        intent(INOUT) :: STATE
    character(len=*),            intent(IN   ) :: NAME
    integer, optional,           intent(  OUT) :: RC     ! Error code:
    !EOPI

    character(len=ESMF_MAXSTR), parameter :: IAm = "MAPL_GenericStateClockOff"
    integer :: STATUS

    call MAPL_ProfClockOff(STATE%TIMES,NAME,RC=STATUS)
    _VERIFY(STATUS)

    _RETURN(ESMF_SUCCESS)
  end subroutine MAPL_GenericStateClockOff


!=============================================================================
!=============================================================================
!=============================================================================
!=============================================================================

  !BOPI
  ! !IROUTINE: MAPL_TimerAdd
  ! !IIROUTINE: MAPL_GenericStateClockAdd

  !INTERFACE:
  subroutine MAPL_GenericStateClockAdd(GC, NAME, RC)
    
    !ARGUMENTS:
    type (ESMF_GridComp),        intent(INOUT) :: GC
    character(len=*),            intent(IN   ) :: NAME
    integer, optional,           intent(  OUT) :: RC     ! Error code:
    !EOPI

    character(len=ESMF_MAXSTR), parameter :: IAm = "MAPL_GenericStateClockAdd"
    integer :: STATUS
    type (MAPL_MetaComp), pointer         :: STATE

    call MAPL_InternalStateRetrieve(GC, STATE, RC=status)
    _VERIFY(STATUS)

    call MAPL_ProfSet(STATE%TIMES,NAME=NAME,RC=STATUS)
    _VERIFY(STATUS)

    _RETURN(ESMF_SUCCESS)
  end subroutine MAPL_GenericStateClockAdd


!=============================================================================
!=============================================================================
!=============================================================================
!=============================================================================


!=============================================================================
!=============================================================================
!=============================================================================
!=============================================================================

  subroutine MAPL_ESMFStateWriteToFile(STATE,CLOCK,FILENAME,FILETYPE,MPL,HDR, oClients,RC)
    type(ESMF_State),                 intent(INOUT) :: STATE
    type(ESMF_Clock),                 intent(IN   ) :: CLOCK
    character(len=*),                 intent(IN   ) :: FILENAME
    character(LEN=*),                 intent(INout) :: FILETYPE
    type(MAPL_MetaComp),              intent(INOUT) :: MPL
    logical,                          intent(IN   ) :: HDR
    type (ClientManager), optional,   intent(inout) :: oClients
    integer, optional,                intent(  OUT) :: RC

    character(len=ESMF_MAXSTR), parameter :: IAm="MAPL_ESMFStateWriteToFile"
    integer                               :: STATUS

    integer                               :: UNIT
    integer                               :: YYYY, MM, DD, H, M, S
    type(ESMF_Time)                       :: currentTime
!    integer                               :: IM_WORLD
!    integer                               :: JM_WORLD
!    integer                               :: MONTH, DAY, HOUR, MINUTE
!    integer                               :: YEAR, SECOND
!    type (ESMF_Time)                      :: CURRENTTIME
    integer                               :: HEADER(6), DimCount
    logical                               :: AmWriter
    type(ArrDescr)                        :: ArrDes
    integer(kind=MPI_OFFSET_KIND)         :: offset
    integer                               :: info

    type(ESMF_Grid)                       :: TILEGRID
    integer                               :: COUNTS(2)
    integer                               :: io_nodes, io_rank
    integer                               :: attr
    character(len=MPI_MAX_INFO_VAL )      :: romio_cb_write
    logical                               :: nwrgt1
    !real(kind=ESMF_KIND_R8) :: itime_beg, itime_end
    !real(kind=ESMF_KIND_R8),save ::  total_time = 0.0d0
    !logical                               :: amIRoot
    !type (ESMF_VM)                        :: vm

! Open file
!----------
!I/O
    if (index(filename,'*') /= 0) then
       !ALT: this is a special, MAPL_Write2RAM type
       filetype = 'binary'
    end if

    if (mpl%grid%num_writers == 1 .and. filetype == 'pbinary') then
       !ALT: this is a special case, we will treat the same as BINARY
       filetype = 'binary'
    end if
    ! do we have more than 1 reader or writer
    nwrgt1 = (mpl%grid%num_writers > 1)

    if (filetype == 'binary' .or. filetype == 'BINARY') then
       UNIT = GETFILE(FILENAME, form="unformatted", all_pes=.true., rc=status)
       _VERIFY(STATUS)
    elseif(filetype=="formatted".or.filetype=="FORMATTED") then
       UNIT = GETFILE(FILENAME, form="formatted", rc=status)
       _VERIFY(STATUS)
    elseif(filetype=='pbinary') then
       call ESMF_GridGet(MPL%GRID%ESMFGRID, dimCount=dimCount, RC=STATUS)
       _VERIFY(STATUS)

       AmWriter = mpl%grid%writers_comm/=MPI_COMM_NULL

       call ESMF_AttributeGet(STATE, NAME = "MAPL_GridTypeBits", VALUE=ATTR, RC=STATUS)
       _VERIFY(STATUS)
       TILE: if(IAND(ATTR, MAPL_AttrTile) /= 0) then
          _ASSERT(IAND(ATTR, MAPL_AttrGrid) == 0,'needs informative message') ! no hybrid allowed
          _ASSERT(MAPL_LocStreamIsAssociated(MPL%LOCSTREAM,RC=STATUS),'needs informative message')

          call MAPL_LocStreamGet(mpl%LocStream, TILEGRID=TILEGRID, RC=STATUS)
          _VERIFY(STATUS)
          
          call MAPL_GridGet(TILEGRID, globalCellCountPerDim=COUNTS, RC=STATUS)
          _VERIFY(STATUS)

          call ArrDescrSet(arrdes,                   &
               writers_comm = mpl%grid%writers_comm,&
               iogathercomm = mpl%grid%comm )

          if(AmWriter) then

             call MPI_COMM_SIZE(mpl%grid%writers_comm, io_nodes, STATUS)
             _VERIFY(STATUS)
             call MPI_COMM_RANK(mpl%grid%writers_comm, io_rank, STATUS)
             _VERIFY(STATUS)

          endif

          call ArrDescrSet(arrdes,                   &
               i1 = mpl%grid%i1, in = mpl%grid%in,   &
               j1 = mpl%grid%j1, jn = mpl%grid%jn,   &
               im_world = COUNTS(1),                 &
               jm_world = COUNTS(2)                  )

       else
          
          AmWriter = mpl%grid%writers_comm/=MPI_COMM_NULL

          if (AmWriter) then
             call MPI_COMM_SIZE(mpl%grid%writers_comm, io_nodes, STATUS)
             _VERIFY(STATUS)
             call MPI_COMM_RANK(mpl%grid%writers_comm, io_rank, STATUS)
             _VERIFY(STATUS)
          endif

          call ArrDescrSet(arrdes, offset, &
               writers_comm = mpl%grid%writers_comm,  &
               iogathercomm = mpl%grid%iogathercomm,  &
               i1 = mpl%grid%i1, in = mpl%grid%in,    &
               j1 = mpl%grid%j1, jn = mpl%grid%jn,    &
               im_world = mpl%grid%im_world,          &
               jm_world = mpl%grid%jm_world)

       end if TILE

!@       call MPI_Barrier(mpl%grid%comm, status)
!@       _VERIFY(STATUS)
       arrdes%offset = 0
       if (AmWriter) then

          call MPI_Info_create(info, STATUS)
          _VERIFY(STATUS)
! disable works best on GPFS but remains TBD for Lustre
          call MAPL_GetResource(MPL, romio_cb_write, Label="ROMIO_CB_WRITE:", default="disable", RC=STATUS) 
          _VERIFY(STATUS)
          call MPI_Info_set(info, "romio_cb_write", trim(romio_cb_write), STATUS)
          _VERIFY(STATUS)
          if (io_rank == 0) then
             print *,'Using parallel IO for writing file: ',trim(FILENAME)
             ! make sure file exists
             call MPI_FILE_OPEN(MPI_COMM_SELF, FILENAME, MPI_MODE_WRONLY+MPI_MODE_CREATE, &
                                info, UNIT, STATUS)
             _VERIFY(STATUS)
             call MPI_FILE_CLOSE(UNIT, status)
             _VERIFY(STATUS)
          end if
          call MPI_Barrier(mpl%grid%writers_comm, status)
          _VERIFY(STATUS)
          call MPI_FILE_OPEN(mpl%grid%writers_comm, FILENAME, MPI_MODE_WRONLY, &
                                info, UNIT, STATUS)
          _VERIFY(STATUS)
!$$            call MPI_Barrier(mpl%grid%writers_comm, status)
!$$            _VERIFY(STATUS)
       else
          UNIT=0
       endif

    else if (filetype=='pnc4') then
#ifndef H5_HAVE_PARALLEL
       if (nwrgt1) then
          print*,trim(Iam),': num_readers and number_writers must be 1 with pnc4 unless HDF5 was built with -enable-parallel'
          _ASSERT(.false.,'needs informative message')
       end if
#endif
       AmWriter = mpl%grid%writers_comm/=MPI_COMM_NULL
       call ESMF_AttributeGet(STATE, NAME = "MAPL_GridTypeBits", VALUE=ATTR, RC=STATUS)
       _VERIFY(STATUS)
       PNC4_TILE: if(IAND(ATTR, MAPL_AttrTile) /= 0) then
          _ASSERT(IAND(ATTR, MAPL_AttrGrid) == 0,'needs informative message') ! no hybrid allowed
          call ArrDescrSetNCPar(arrdes,MPL,tile=.TRUE.,num_writers=mpl%grid%num_writers,RC=STATUS)
          _VERIFY(STATUS)
       else
          call ArrDescrSetNCPar(arrdes,MPL,num_writers=mpl%grid%num_writers,RC=STATUS)
          _VERIFY(STATUS)
       end if PNC4_TILE
       arrdes%filename = trim(FILENAME)
       if (AmWriter) then
          call MPI_COMM_RANK(mpl%grid%writers_comm, io_rank, STATUS)
          _VERIFY(STATUS)
          if (io_rank == 0) then
            print *,'Using parallel NetCDF for file: ',trim(FILENAME)
          end if
       endif
    else
       UNIT=0
    end if

! Write data
!-----------

    if (HDR .and. filetype/='pnc4') then
       call ESMF_ClockGet (clock, currTime=currentTime, rc=status)
       _VERIFY(STATUS)
       call ESMF_TimeGet(CurrentTime, &
                        YY=YYYY, MM=MM, DD=DD, &
                        H=H, M=M, S=S, rc=status)
       _VERIFY(STATUS)

       HEADER(1) = YYYY
       HEADER(2) = MM
       HEADER(3) = DD
       HEADER(4) = H
       HEADER(5) = M
       HEADER(6) = S

       if(filetype=='pbinary' ) then
          arrdes%offset=0
          call Write_Parallel(HEADER, UNIT, ARRDES=ARRDES, RC=status)
          _VERIFY(STATUS)
       else
          call Write_Parallel(HEADER, UNIT, RC=status)
          _VERIFY(STATUS)
       endif

       HEADER(1) = MPL%GRID%IM_WORLD
       HEADER(2) = MPL%GRID%JM_WORLD
       HEADER(3) = MPL%GRID%LM
       HEADER(4) = 0
       HEADER(5) = 0

       if(filetype=='pbinary' ) then
          call Write_Parallel(HEADER(1:5), UNIT, ARRDES=ARRDES, RC=status)
          _VERIFY(STATUS)
       else
          call Write_Parallel(HEADER(1:5), UNIT, RC=status)
          _VERIFY(STATUS)
       endif

    end if


    if(filetype=='pbinary' ) then
       arrdes%ycomm = mpl%grid%Ycomm
       call MAPL_VarWrite(UNIT=UNIT, STATE=STATE, arrdes=arrdes, rc=status)
       _VERIFY(STATUS)

       if (AmWriter) then
!$$          call MPI_Barrier(mpl%grid%writers_comm, status)
!$$          _VERIFY(STATUS)
          call MPI_FILE_CLOSE(UNIT, status)
          _VERIFY(STATUS)
          call MPI_Info_free(info, status)
          _VERIFY(STATUS)
       endif
!@       call MPI_Barrier(mpl%grid%comm, status)
!@       _VERIFY(STATUS)

    elseif(filetype=='pnc4') then
  
       !call MPI_Barrier(mpl%grid%comm, status)
       !_VERIFY(STATUS)
       !itime_beg = MPI_Wtime(STATUS)
       !_VERIFY(STATUS)

       call MAPL_VarWriteNCPar(filename,STATE,ArrDes,CLOCK, oClients=oClients, RC=STATUS)
       _VERIFY(STATUS)

       !call MPI_Barrier(mpl%grid%comm, status)
       !_VERIFY(STATUS)
       !itime_end = MPI_Wtime(STATUS)
       !total_time = total_time + itime_end - itime_beg
       !_VERIFY(STATUS)
       !call MPI_COMM_RANK(mpl%grid%comm, io_rank, STATUS)
       !   _VERIFY(STATUS)
       !if (io_rank == 0) then
       !   print *,'Time using writing filename: '//trim(filename), '  ', itime_end - itime_beg
       !   print *,'Total time writing checkpoint: ', total_time
       !end if

    elseif(UNIT/=0) then

       call MAPL_VarWrite(UNIT=UNIT, STATE=STATE, rc=status)
       _VERIFY(STATUS)

       call FREE_FILE(UNIT)

    else
       STATUS = -1  ! not yet
       _VERIFY(STATUS)
    endif

    _RETURN(ESMF_SUCCESS)
  end subroutine MAPL_ESMFStateWriteToFile

!=============================================================================
!=============================================================================
!=============================================================================
!=============================================================================


  subroutine MAPL_ESMFStateReadFromFile(STATE,CLOCK,FILENAME,MPL,HDR,RC)
    type(ESMF_State),                 intent(INOUT) :: STATE
    type(ESMF_Clock),                 intent(IN   ) :: CLOCK
    character(LEN=*),                 intent(IN   ) :: FILENAME
    type(MAPL_MetaComp),              intent(INOUT) :: MPL
    logical,                          intent(IN   ) :: HDR
    integer, optional,                intent(  OUT) :: RC

    character(len=ESMF_MAXSTR), parameter :: IAm="MAPL_ESMFStateReadFromFile"
    integer                               :: STATUS
    integer                               :: UNIT
    character(len=ESMF_MAXSTR)            :: FNAME
    character(len=ESMF_MAXSTR)            :: TMP
    type(ArrDescr)                        :: ArrDes
    integer(kind=MPI_OFFSET_KIND)         :: offset
    integer                               :: dimcount
    integer                               :: info
    logical                               :: AmReader
    logical                               :: FileExists

    type(ESMF_Grid) :: TILEGRID
    integer :: COUNTS(2)
    integer :: io_nodes, io_rank
    integer :: attr
    character(len=MPI_MAX_INFO_VAL )      :: romio_cb_read
    logical                               :: bootstrapable
    logical                               :: restartRequired
    logical                               :: nwrgt1
    character(len=ESMF_MAXSTR)            :: rstBoot
    integer                               :: rstReq
    logical                               :: amIRoot
    type (ESMF_VM)                        :: vm
    character(len=1)                      :: firstChar
    character(len=ESMF_MAXSTR)            :: FileType
    integer                               :: isNC4
    logical                               :: isPresent

    _UNUSED_DUMMY(CLOCK)

! Implemented a suggestion by Arlindo to allow files beginning with "-" (dash)
! to be skipped if file does not exist and values defaulted

! Implemented a suggestion by Larry to allow files beginning with "+" (plus)
! to not fail if they do not contain enough variables or skipped alltogether
! if file does not exist. In both case values are defaulted

    FNAME = adjustl(FILENAME)
    bootstrapable = .false.

    ! check resource for restart mode (strict would require restarts regardless of the specs)
    call MAPL_GetResource( MPL, rstBoot, Label='MAPL_ENABLE_BOOTSTRAP:', &
         Default='NO', RC=STATUS)
    _VERIFY(STATUS)

    rstBoot = ESMF_UtilStringUpperCase(rstBoot,rc=STATUS)
    _VERIFY(STATUS)

    bootstrapable = (rstBoot /= 'NO')

    firstChar = FNAME(1:1)

    if (firstChar == "-" .or. firstChar == '+') then
       TMP = FNAME(2:)
       FNAME = TMP
       if (.not. bootstrapable) then
          call WRITE_PARALLEL("WARNING: use of '+' or '-' in the restart name '"//&
               trim(FNAME)//"' allows bootstrapping!")
       end if
       bootstrapable = .true.
    end if

    ! get the "required restart" attribute from the state
    call ESMF_AttributeGet(STATE, NAME="MAPL_RestartRequired", isPresent=isPresent, RC=STATUS)
    _VERIFY(STATUS) 
    if (isPresent) then
       call ESMF_AttributeGet(STATE, NAME="MAPL_RestartRequired", VALUE=rstReq, RC=STATUS)
       _VERIFY(STATUS) 
    else
       rstReq = 0
    end if
    restartRequired = (rstReq /= 0)

    call ESMF_VmGetCurrent(vm, rc=status)
    _VERIFY(status)
    
    amIRoot = MAPL_AM_I_Root(vm)

    nwrgt1 = (mpl%grid%num_readers > 1) 

   
    if(INDEX(FNAME,'*') == 0) then
       if (AmIRoot) then
          block
             character(len=:), allocatable :: fname_by_face
             logical :: fexist
             integer :: i

             FileExists = .false.
             if (mpl%grid%read_restart_by_face) then
                FileExists = .true.
                do i = 1,6 ! 6 faces
                   fname_by_face = get_fname_by_face(trim(fname), i)
                   inquire(FILE = trim(fname_by_face), EXIST=fexist)
                   FileExists = FileExists .and. fexist
                enddo
                if ( .not. FileExists) then
                   _VERIFY(-1)
                else
                   ! just pick one face to deduce filetype, only in root
                   call MAPL_NCIOGetFileType(trim(fname_by_face),isNC4,rc=status)
                   _VERIFY(STATUS)
                endif
                deallocate(fname_by_face)
             endif
          end block
          if( .not. FileExists) then
             inquire(FILE = FNAME, EXIST=FileExists)
             if (FileExists) then
                call MAPL_NCIOGetFileType(FNAME,isNC4,rc=status)
                _VERIFY(STATUS)
             endif
          endif
       end if

       call MAPL_CommsBcast(vm, fileExists, n=1, ROOT=MAPL_Root, rc=status)
       _VERIFY(status)

       if (FileExists) then
          !if (AmIRoot) then
          !   call MAPL_NCIOGetFileType(FNAME,isNC4,rc=status)
          !   _VERIFY(STATUS)
          !end if
          call MAPL_CommsBcast(vm,isNC4,n=1,ROOT=MAPL_Root,rc=status)
          _VERIFY(STATUS)
          if (isNC4 == 0) then
             filetype = 'pnc4'
          else
             if (.not.nwrgt1) then
                filetype='binary'
             else
                filetype='pbinary'
             end if
          end if
       end if
    else
       FileExists = MAPL_MemFileInquire(NAME=FNAME)
    end if

    if (.not. FileExists) then
       if (.not. bootstrapable .or. restartRequired) then
          call WRITE_PARALLEL('ERROR: Required restart '//trim(FNAME)//' does not exist!')
          _RETURN(ESMF_FAILURE)
       else
          call WRITE_PARALLEL("Bootstrapping " // trim(FNAME))
          _RETURN(ESMF_SUCCESS)
       end if
    end if

!    if (ignoreEOF) then
!       if (filetype == 'pbinary' .or. filetype == 'PBINARY') then
!          filetype = 'binary'
!       end if
!    end if

! Open file
!----------

!   Test if is a memory unit, if not must be real file
    if (index(filename,'*') /= 0) then
       !ALT: this is a special, MAPL_Write2RAM type
       filetype = 'binary'
    end if
    
    if (filetype == 'binary' .or. filetype == 'BINARY') then
       UNIT = GETFILE(FNAME, form="unformatted", all_pes=.true., rc=status)
       _VERIFY(STATUS)

    elseif(filetype=="formatted".or.filetype=="FORMATTED") then
       UNIT = GETFILE(FNAME, form="formatted", all_pes=.true., rc=status)
       _VERIFY(STATUS)

    elseif(filetype=='pbinary') then
       call ESMF_GridGet(MPL%GRID%ESMFGRID, dimCount=dimCount, RC=STATUS)
       _VERIFY(STATUS)

       AmReader = mpl%grid%readers_comm/=MPI_COMM_NULL

       call ESMF_AttributeGet(STATE, NAME = "MAPL_GridTypeBits", VALUE=ATTR, RC=STATUS)
       _VERIFY(STATUS)
       TILE: if(IAND(ATTR, MAPL_AttrTile) /= 0) then
          _ASSERT(IAND(ATTR, MAPL_AttrGrid) == 0,'needs informative message') ! no hybrid allowed
          _ASSERT(MAPL_LocStreamIsAssociated(MPL%LOCSTREAM,RC=STATUS),'needs informative message')

          call MAPL_LocStreamGet(mpl%LocStream, TILEGRID=TILEGRID, RC=STATUS)
          _VERIFY(STATUS)
 
          call MAPL_GridGet(TILEGRID, globalCellCountPerDim=COUNTS, RC=STATUS)
          _VERIFY(STATUS)

          call ArrDescrSet(arrdes,                    &
               readers_comm  = mpl%grid%readers_comm, &
               ioscattercomm = mpl%grid%comm )

          if(AmReader) then

             call MPI_COMM_SIZE(mpl%grid%readers_comm, io_nodes, STATUS)
             _VERIFY(STATUS)
             call MPI_COMM_RANK(mpl%grid%readers_comm, io_rank, STATUS)
             _VERIFY(STATUS)

          endif

          call ArrDescrSet(arrdes,                   &
               i1 = mpl%grid%i1, in = mpl%grid%in,   &
               j1 = mpl%grid%j1, jn = mpl%grid%jn,   &
               im_world = COUNTS(1),                 &
               jm_world = COUNTS(2)                  )

       else

          if (AmReader) then
             call MPI_COMM_SIZE(mpl%grid%readers_comm, io_nodes, STATUS)
             _VERIFY(STATUS)
             call MPI_COMM_RANK(mpl%grid%readers_comm, io_rank, STATUS)
             _VERIFY(STATUS)
          endif

          call ArrDescrSet(arrdes, offset, &
                      readers_comm  = mpl%grid%readers_comm,  &
                      ioscattercomm = mpl%grid%ioscattercomm, &
                      i1 = mpl%grid%i1, in = mpl%grid%in,     &
                      j1 = mpl%grid%j1, jn = mpl%grid%jn,     &
                      im_world = mpl%grid%im_world,           &
                      jm_world = mpl%grid%jm_world)

       end if TILE

       UNIT=-999

       offset = 0
       if (AmReader) then
             call MPI_Info_create(info, STATUS)
             _VERIFY(STATUS)
! This need to be tested on GPFS and Lustre to determine best performance
             call MAPL_GetResource(MPL, romio_cb_read, Label="ROMIO_CB_READ:", default="automatic", RC=STATUS)
             _VERIFY(STATUS)
             call MPI_Info_set(info, "romio_cb_read", trim(romio_cb_read), STATUS)
             _VERIFY(STATUS)
             if (io_rank == 0) then
                print *,'Using parallel IO for reading file: ',trim(FNAME)
             end if
             call MPI_Barrier(mpl%grid%readers_comm, status)
             _VERIFY(STATUS)
             call MPI_FILE_OPEN(mpl%grid%readers_comm, FNAME, MPI_MODE_RDONLY, &
                                MPI_INFO_NULL, UNIT, STATUS)
             _VERIFY(STATUS)
             call MPI_Barrier(mpl%grid%readers_comm, status)
             _VERIFY(STATUS)
       else
          UNIT=0
       endif ! AmReader
       
    else if (filetype=='pnc4') then
#ifndef H5_HAVE_PARALLEL
       if (nwrgt1) then
          print*,trim(Iam),': num_readers and number_writers must be 1 with pnc4 unless HDF5 was built with -enable-parallel'
          _ASSERT(.false.,'needs informative message')
       end if
#endif
       AmReader = mpl%grid%readers_comm/=MPI_COMM_NULL
       call ESMF_AttributeGet(STATE, NAME = "MAPL_GridTypeBits", VALUE=ATTR, RC=STATUS)
       _VERIFY(STATUS)
       PNC4_TILE: if(IAND(ATTR, MAPL_AttrTile) /= 0) then
          _ASSERT(IAND(ATTR, MAPL_AttrGrid) == 0,'needs informative message') ! no hybrid allowed
          call ArrDescrSetNCPar(arrdes,MPL,tile=.TRUE.,num_readers=mpl%grid%num_readers,RC=STATUS)
          _VERIFY(STATUS)
       else
          call ArrDescrSetNCPar(arrdes,MPL,num_readers=mpl%grid%num_readers,RC=STATUS)
          _VERIFY(STATUS)
       end if PNC4_TILE
       if (mpl%grid%readers_comm/=MPI_COMM_NULL) then
          call MPI_COMM_RANK(mpl%grid%readers_comm, io_rank, STATUS)
          _VERIFY(STATUS)
          if (io_rank == 0) then
            print *,'Using parallel NetCDF for file: ',trim(FNAME)
          end if
       endif
    else
       UNIT=0
    end if


! Skip Header
!------------

    if (HDR .and. filetype/='pnc4') then
       if(filetype=='pbinary') then
          offset = 16*4 ! + aks and bks ????
       else
          call MAPL_Skip(UNIT, MPL%GRID%LAYOUT, COUNT=2, RC=STATUS)
          _VERIFY(STATUS)
       endif
    end if

! Read data
! ---------

    if(filetype=='pbinary') then
       call ArrDescrSet(arrdes, offset)
       arrdes%Ycomm = mpl%grid%Ycomm
       call MAPL_VarRead(UNIT=UNIT, STATE=STATE, arrdes=arrdes, RC=STATUS)
       _VERIFY(STATUS)
       if (AmReader) then
          call MPI_Barrier(mpl%grid%readers_comm, status)
          _VERIFY(STATUS)
          call MPI_FILE_CLOSE(UNIT, status)
          _VERIFY(STATUS)
          call MPI_Barrier(mpl%grid%readers_comm, status)
          _VERIFY(STATUS)
       endif
    elseif(filetype=='pnc4') then

       call MAPL_VarReadNCPar(fname,STATE,ArrDes,bootstrapable,RC=STATUS)
       _VERIFY(STATUS)

    elseif(UNIT/=0) then
       call MAPL_VarRead(UNIT=UNIT, STATE=STATE, bootstrapable=bootstrapable, RC=STATUS)
       _VERIFY(STATUS)
       call FREE_FILE(UNIT)
    else
       STATUS = -1 ! not yet
       _VERIFY(STATUS)
    endif

    call ESMF_AttributeSet(STATE,'MAPL_Initialized', .TRUE.,RC=STATUS)
    _VERIFY(STATUS)

    call MAPL_AttributeSet(STATE, NAME="MAPL_InitStatus", VALUE=MAPL_InitialRestart, RC=STATUS)
    _VERIFY(STATUS)      

    _RETURN(ESMF_SUCCESS)
  end subroutine MAPL_ESMFStateReadFromFile

!=============================================================================
!=============================================================================
!=============================================================================
!=============================================================================

  subroutine MAPL_StateCreateFromVarSpecNew(STATE,SPEC,GRID,TILEGRID,DEFER,range,RC)
    type(ESMF_State),                 intent(INOUT) :: STATE
    type(StateSpecification),               intent(INOUT) :: SPEC
    type(ESMF_Grid),                  intent(INout) :: GRID
    logical, optional,                intent(IN   ) :: DEFER
    type(ESMF_Grid), optional,        intent(INout) :: TILEGRID
    integer, optional, intent(in) :: range(2)
    integer, optional,                intent(  OUT) :: RC

    character(len=ESMF_MAXSTR), parameter :: IAm="MAPL_StateCreateFromVarSpec"
    integer                               :: STATUS

    integer               :: L
    integer               :: DIMS
    integer               :: STAT
    integer               :: LOCATION
    type(ESMF_Grid)       :: GRD

    integer :: range_(2)
    type(MAPL_VarSpec), pointer :: varspec

    if (present(range)) then
       range_ = range
    else
       range_(1) = 1
       range_(2) = spec%var_specs%size()
    end if
    do L = range_(1), range_(2)

       call MAPL_VarSpecGet(SPEC%var_specs%of(L), DIMS=DIMS, VLOCATION=LOCATION, STAT=STAT, RC=STATUS )
       _VERIFY(STATUS)
!ALT we should also check if we have a valid grid in the spec so we do not overwrite it

       if (IAND(STAT, MAPL_BundleItem) /= 0) then
          GRD = GRID
       else
! choose the grid         
          Dimensionality: select case(DIMS)
          
          case(MAPL_DimsHorzVert)
             select case(LOCATION)
             case(MAPL_VLocationCenter)
                GRD = GRID
             case(MAPL_VLocationEdge  )
                GRD = GRID
             case default
                _RETURN(ESMF_FAILURE)
             end select
          case(MAPL_DimsHorzOnly)
             GRD = GRID
          case(MAPL_DimsVertOnly)
             GRD = GRID
          case(MAPL_DimsNone)
             GRD = GRID
          case(MAPL_DimsTileOnly)
             if (.not. present(TILEGRID)) then
                _RETURN(ESMF_FAILURE)
             endif
             GRD = TILEGRID
          case(MAPL_DimsTileTile)
             if (.not. present(TILEGRID)) then
                _RETURN(ESMF_FAILURE)
             endif
             GRD = TILEGRID
          case default 
             _RETURN(ESMF_FAILURE)
          end select Dimensionality
       end if

       varspec => SPEC%var_specs%of(L)
       call MAPL_VarSpecSet(varspec, GRID=GRD, RC=STATUS )
       _VERIFY(STATUS)

    end do

    call MAPL_StateCreateFromSpecNew(STATE, SPEC, DEFER, range=range, RC=STATUS  )
    _VERIFY(STATUS)

   _RETURN(ESMF_SUCCESS)

end subroutine MAPL_StateCreateFromVarSpecNew


  subroutine MAPL_StateCreateFromSpec(STATE,SPEC,DEFER,RC)
    type(ESMF_State),                 intent(INOUT) :: STATE
    type(MAPL_VarSpec), target,       intent(INOUT) :: SPEC(:)
    logical, optional,                intent(IN   ) :: DEFER
    integer, optional,                intent(  OUT) :: RC

    character(len=ESMF_MAXSTR), parameter :: IAm="MAPL_StateCreateFromSpec"
    integer                               :: STATUS

    type (StateSpecification) :: state_spec


    state_spec%old_var_specs => spec
    call state_spec%update_vector()

    call MAPL_StateCreateFromSpecNew(state, state_spec, defer,rc=status)
    _VERIFY(status)

    _RETURN(_SUCCESS)
  end subroutine MAPL_StateCreateFromSpec


  subroutine MAPL_StateCreateFromSpecNew(STATE,SPEC,DEFER,range, RC)
     type(ESMF_State),                 intent(INOUT) :: STATE
     type(StateSpecification), target, intent(inout) :: spec
    logical, optional,                intent(IN   ) :: DEFER
    integer, optional, intent(in) :: range(2)
    integer, optional,                intent(  OUT) :: RC

    character(len=ESMF_MAXSTR), parameter :: IAm="MAPL_StateCreateFromSpecNew"
    integer                               :: STATUS

    integer               :: L
    type (ESMF_Grid)      :: GRID
    type (ESMF_Array)     :: Array
    type (ESMF_Field)     :: FIELD
    type (ESMF_FieldBundle) :: BUNDLE
    type (ESMF_Field)       :: SPEC_FIELD
    type (ESMF_FieldBundle) :: SPEC_BUNDLE
    real(kind=ESMF_KIND_R4), pointer         :: VAR_1D(:), VAR_2D(:,:), VAR_3D(:,:,:), VAR_4d(:,:,:,:)
    real(kind=ESMF_KIND_R8), pointer         :: VR8_1D(:), VR8_2D(:,:), VR8_3D(:,:,:), VR8_4D(:,:,:,:)
    logical               :: usableDEFER
    logical               :: deferAlloc
    integer               :: DIMS
    integer               :: STAT
    integer               :: KND
    integer               :: LOCATION
    character(ESMF_MAXSTR):: SHORT_NAME
    character(ESMF_MAXSTR):: LONG_NAME
    character(ESMF_MAXSTR):: UNITS
    character(ESMF_MAXSTR):: FRIENDLYTO
    integer               :: REFRESH
    integer               :: AVGINT
    real                  :: DEFAULT_VALUE
    integer               :: I
    logical               ::  done
    integer               :: N, N1, N2, NE
    integer               :: HW
    integer               :: RESTART
    character(len=ESMF_MAXSTR), pointer     :: ATTR_INAMES(:)
    character(len=ESMF_MAXSTR), pointer     :: ATTR_RNAMES(:)
    integer,                    pointer     :: ATTR_IVALUES(:)
    real,                       pointer     :: ATTR_RVALUES(:)
    integer,                    pointer     :: UNGRD(:)
    integer                                 :: attr
    integer                                 :: initStatus
    logical                                 :: defaultProvided
    integer                                 :: fieldRank
    real(kind=ESMF_KIND_R8)                 :: def_val_8
    type(ESMF_TypeKind_Flag)                :: typekind
    logical                                 :: has_ungrd
    logical                                 :: doNotAllocate
    logical                                 :: alwaysAllocate
    integer                                 :: field_type
    integer                                 :: staggering
    integer                                 :: rotation
    type(ESMF_State)                        :: SPEC_STATE
    type(ESMF_State)                        :: nestSTATE
    character(ESMF_MAXSTR)                  :: ungridded_unit
    character(ESMF_MAXSTR)                  :: ungridded_name
    real,                    pointer        :: ungridded_coords(:)
    integer                                 :: szUngrd
    integer                                 :: rstReq
    logical                                 :: isPresent
    logical                                 :: isCreated

    integer :: range_(2)
    type(MAPL_VarSpec), pointer :: varspec

    if (present(range)) then
       range_ = range
    else
       range_(1) = 1
       range_(2) = spec%var_specs%size()
    end if

   if (present(DEFER)) then
      usableDEFER = DEFER
   else
      usableDEFER = .false.
   end if

   attr = 0
   rstReq = 0
   do L = range_(1), range_(2)

      call MAPL_VarSpecGet(SPEC%var_specs%of(L),DIMS=DIMS,VLOCATION=LOCATION,   &
                           SHORT_NAME=SHORT_NAME, LONG_NAME=LONG_NAME, UNITS=UNITS,&
                           FIELD=SPEC_FIELD, &
                           BUNDLE=SPEC_BUNDLE, &
                           STATE=SPEC_STATE, &
                           STAT=STAT, DEFAULT = DEFAULT_VALUE, &
                           defaultProvided = defaultProvided, &
                           FRIENDLYTO=FRIENDLYTO, &
                           COUPLE_INTERVAL=REFRESH, &
                           ACCMLT_INTERVAL=AVGINT, &
                           HALOWIDTH=HW, &
                           RESTART=RESTART, &
                           PRECISION=KND, &
                           ATTR_RNAMES=ATTR_RNAMES, &
                           ATTR_INAMES=ATTR_INAMES, &
                           ATTR_RVALUES=ATTR_RVALUES, &
                           ATTR_IVALUES=ATTR_IVALUES, &
                           UNGRIDDED_DIMS=UNGRD, &
                           UNGRIDDED_UNIT=UNGRIDDED_UNIT, &
                           UNGRIDDED_NAME=UNGRIDDED_NAME, &
                           UNGRIDDED_COORDS=UNGRIDDED_COORDS, &
                           GRID=GRID, &
                           doNotAllocate=doNotAllocate, &
                           alwaysAllocate=alwaysAllocate, &
                           FIELD_TYPE=FIELD_TYPE, &
                           STAGGERING=STAGGERING, &
                           ROTATION=ROTATION, &
                           RC=STATUS )
      _VERIFY(STATUS)

      I=MAPL_VarSpecGetIndex(SPEC%old_var_specs(range_(1):range_(2)), SHORT_NAME, RC=STATUS)
      if (I + (range_(1)-1) /= L) then
         CALL WRITE_PARALLEL("===================>")
         CALL WRITE_PARALLEL(trim(Iam) //": var "// trim(SHORT_NAME) // " already exists. Skipping ...")
         cycle
      endif

      if (RESTART == MAPL_RestartRequired) then
         rstReq = 1
      end if

      if (DIMS == MAPL_DimsTileOnly .OR. DIMS == MAPL_DimsTileTile) then
         ATTR = IOR(ATTR, MAPL_AttrTile)
      else
         ATTR = IOR(ATTR, MAPL_AttrGrid)
      end if
      
      if (IAND(STAT, MAPL_StateItem) /= 0) then
         isCreated = ESMF_StateIsCreated(SPEC_STATE, rc=status)
         _VERIFY(STATUS)
         if (.not. isCreated) then
! Create an empty state
! ---------------------
            nestState = ESMF_StateCreate(NAME=SHORT_NAME, RC=STATUS)
            _VERIFY(STATUS)
         else
            nestState = SPEC_STATE
         end if
         varspec => spec%var_specs%of(L)
         call MAPL_VarSpecSet(varspec,STATE=nestState,RC=STATUS)
         _VERIFY(STATUS)

         call ESMF_AttributeSet(nestState, NAME='RESTART', VALUE=RESTART, RC=STATUS)
         _VERIFY(STATUS)
      
! Put the BUNDLE in the state
! --------------------------
         call ESMF_StateAdd(STATE, (/nestState/), rc=status)
         _VERIFY(STATUS)

         GOTO 10         
      endif

      if (IAND(STAT, MAPL_BundleItem) /= 0) then
!ALT: logic needed for putting bundleptr (like bundle validate)
         isCreated = ESMF_FieldBundleIsCreated(SPEC_BUNDLE, rc=status)
         _VERIFY(STATUS)
         if (.not. isCreated) then
! Create an empty BUNDLE
! ----------------------
            bundle = ESMF_FieldBundleCreate(NAME=SHORT_NAME, RC=STATUS)
            _VERIFY(STATUS)
            call ESMF_FieldBundleSet(bundle, GRID=GRID, RC=STATUS)
            _VERIFY(STATUS)
         else
            BUNDLE = SPEC_BUNDLE
         end if
         varspec => SPEC%var_specs%of(L)
         call MAPL_VarSpecSet(varspec,BUNDLE=BUNDLE,RC=STATUS)
         _VERIFY(STATUS)

         call ESMF_AttributeSet(BUNDLE, NAME='RESTART', VALUE=RESTART, RC=STATUS)
         _VERIFY(STATUS)
      
! Put the BUNDLE in the state
! --------------------------
         call MAPL_StateAdd(STATE, bundle, rc=status)
         _VERIFY(STATUS)

         GOTO 10
!!!         cycle
      endif

      deferAlloc = usableDefer
      if (usableDefer) deferAlloc = .not. alwaysAllocate

!ALTcheck this      call ESMF_FieldGet(SPEC_FIELD, Array=array, rc=status)
      isCreated = ESMF_FieldIsCreated(SPEC_FIELD, rc=status)
      _VERIFY(STATUS)
      if (isCreated) then
         call MAPL_AllocateCoupling( SPEC_FIELD, RC=STATUS ) ! if 'DEFER' this allocates the data
         _VERIFY(STATUS)
         

!ALT we are creating new field so that we can optionally change the name of the field;
!    the important thing is that the data (ESMF_Array) is the SAME as the one in SPEC_Field
         FIELD = MAPL_FieldCreate(SPEC_FIELD, name=SHORT_NAME, RC=STATUS )
         _VERIFY(STATUS)

         call ESMF_FieldGet(field, Array=array, rc=status)
         _VERIFY(STATUS)
         call ESMF_AttributeGet(field, NAME="MAPL_InitStatus", isPresent=isPresent, RC=STATUS)
         _VERIFY(STATUS)
         if (isPresent) then
            call ESMF_AttributeGet(field, NAME="MAPL_InitStatus", VALUE=initStatus, RC=STATUS)
            _VERIFY(STATUS)
         else
            initStatus = MAPL_UnInitialized
         end if
         if (defaultProvided) then
! if the "original" field was initialized by reading a restart file do not overwrite
            if (initStatus /= MAPL_InitialRestart) then
               call ESMF_FieldGet(FIELD, typeKind=typeKind, dimCount=fieldRank, RC=status)
               _VERIFY(STATUS)
               if (typeKind == ESMF_TYPEKIND_R4) then
                  if (fieldRank == 1) then
                     call ESMF_FieldGet(field, farrayPtr=var_1d, rc=status)
                     _VERIFY(STATUS)
                     if (initStatus == MAPL_InitialDefault) then
                        if (any(var_1d /= default_value)) then
                           _RETURN(ESMF_FAILURE)
                        endif
                     end if
                     var_1d = default_value
                     initStatus = MAPL_InitialDefault
                  else if (fieldRank == 2) then
                     call ESMF_FieldGet(field, farrayPtr=var_2d, rc=status)
                     _VERIFY(STATUS)
                     if (initStatus == MAPL_InitialDefault) then
                        if (any(var_2d /= default_value)) then
                           _RETURN(ESMF_FAILURE)
                        endif
                     end if
                     var_2d = default_value
                     initStatus = MAPL_InitialDefault
                  else if (fieldRank == 3) then
                     call ESMF_FieldGet(field, farrayPtr=var_3d, rc=status)
                     _VERIFY(STATUS)
                     if (initStatus == MAPL_InitialDefault) then
                        if (any(var_3d /= default_value)) then
                           _RETURN(ESMF_FAILURE)
                        endif
                     end if
                     var_3d = default_value
                     initStatus = MAPL_InitialDefault
                  else if (fieldRank == 4) then
                     call ESMF_FieldGet(field, farrayPtr=var_4d, rc=status)
                     _VERIFY(STATUS)
                     if (initStatus == MAPL_InitialDefault) then
                        if (any(var_4d /= default_value)) then
                           _RETURN(ESMF_FAILURE)
                        endif
                     end if
                     var_4d = default_value
                     initStatus = MAPL_InitialDefault
                  end if
               else if (typeKind == ESMF_TYPEKIND_R8) then
                  def_val_8 = real(default_value,kind=ESMF_KIND_R8)
                  if (fieldRank == 1) then
                     call ESMF_FieldGet(field, farrayPtr=vr8_1d, rc=status)
                     _VERIFY(STATUS)
                     if (initStatus == MAPL_InitialDefault) then
                        if (any(vr8_1d /= def_val_8)) then
                           _RETURN(ESMF_FAILURE)
                        endif
                     end if
                     vr8_1d = def_val_8
                     initStatus = MAPL_InitialDefault
                  else if (fieldRank == 2) then
                     call ESMF_FieldGet(field, farrayPtr=vr8_2d, rc=status)
                     _VERIFY(STATUS)
                     if (initStatus == MAPL_InitialDefault) then
                        if (any(vr8_2d /= def_val_8)) then
                           _RETURN(ESMF_FAILURE)
                        endif
                     end if
                     vr8_2d = def_val_8
                     initStatus = MAPL_InitialDefault
                  else if (fieldRank == 3) then
                     call ESMF_FieldGet(field, farrayPtr=vr8_3d, rc=status)
                     _VERIFY(STATUS)
                     if (initStatus == MAPL_InitialDefault) then
                        if (any(vr8_3d /= def_val_8)) then
                           _RETURN(ESMF_FAILURE)
                        endif
                     end if
                     vr8_3d = def_val_8
                     initStatus = MAPL_InitialDefault
                  else if (fieldRank == 4) then
                     call ESMF_FieldGet(field, farrayPtr=vr8_4d, rc=status)
                     _VERIFY(STATUS)
                     if (initStatus == MAPL_InitialDefault) then
                        if (any(vr8_4d /= default_value)) then
                           _RETURN(ESMF_FAILURE)
                        endif
                     end if
                     vr8_4d = default_value
                     initStatus = MAPL_InitialDefault
                  end if
               end if
               call MAPL_AttributeSet(field, NAME="MAPL_InitStatus", &
                                      VALUE=initStatus, RC=STATUS)
               _VERIFY(STATUS)      
            end if
         end if
      else

! Create the appropriate ESMF FIELD
! ---------------------------------

         field = MAPL_FieldCreateEmpty(name=SHORT_NAME, grid=grid, rc=status)
         _VERIFY(STATUS)

         has_ungrd = associated(UNGRD)

         if (.not. deferAlloc) then

!ALT we check if doNotAllocate is set only for fields that are not deferred
            if (.not. doNotAllocate) then
               if (has_ungrd) then
                  if (defaultProvided) then
                     call MAPL_FieldAllocCommit(field, dims=dims, location=location, typekind=knd, &
                          hw=hw, ungrid=ungrd, default_value=default_value, rc=status)
                     _VERIFY(STATUS)
                  else
                     call MAPL_FieldAllocCommit(field, dims=dims, location=location, typekind=knd, &
                          hw=hw, ungrid=ungrd, rc=status)
                     _VERIFY(STATUS)
                  endif
               else
                  if (defaultProvided) then
                     call MAPL_FieldAllocCommit(field, dims=dims, location=location, typekind=knd, &
                          hw=hw, default_value=default_value, rc=status)
                     _VERIFY(STATUS)
                  else
                     call MAPL_FieldAllocCommit(field, dims=dims, location=location, typekind=knd, &
                          hw=hw, rc=status)
                     _VERIFY(STATUS)
                  end if

               end if
            else
               call ESMF_AttributeSet(FIELD, NAME='doNotAllocate', VALUE=1, RC=STATUS)
               _VERIFY(STATUS)
            end if
         else
            call ESMF_AttributeSet(FIELD, NAME='PRECISION', VALUE=KND, RC=STATUS)
            _VERIFY(STATUS)
            call ESMF_AttributeSet(FIELD, NAME='DEFAULT_PROVIDED', &
                 value=defaultProvided, RC=STATUS)
            _VERIFY(STATUS)
            if (defaultProvided) then
               call ESMF_AttributeSet(FIELD, NAME='DEFAULT_VALUE', &
                    value=default_value, RC=STATUS)
               _VERIFY(STATUS)
            end if
            if (has_ungrd) then
               call ESMF_AttributeSet(FIELD, NAME='UNGRIDDED_DIMS', valueList=UNGRD, RC=STATUS)
               _VERIFY(STATUS)
            end if
         end if

! Put the FIELD in the MAPL FIELD (VAR SPEC)
! --------------------------------

!         call MAPL_VarSpecSet(SPEC(L),FIELD=FIELD,RC=STATUS)
!         _VERIFY(STATUS)

      endif
      varspec => SPEC%var_specs%of(L)
      call MAPL_VarSpecSet(varspec,FIELD=FIELD,RC=STATUS)
      _VERIFY(STATUS)
! and in the FIELD in the state
! --------------------------

      call MAPL_StateAdd(STATE, field, rc=status)
      _VERIFY(STATUS)

      if (deferAlloc) then
         initStatus = MAPL_Uninitialized
      else
         if (defaultProvided) initStatus = MAPL_InitialDefault
      end if

! Add SPECs to the FIELD

      call ESMF_AttributeSet(FIELD, NAME='STAT', VALUE=STAT, RC=STATUS)
      _VERIFY(STATUS)
      call ESMF_AttributeSet(FIELD, NAME='DIMS', VALUE=DIMS, RC=STATUS)
      _VERIFY(STATUS)
      call ESMF_AttributeSet(FIELD, NAME='VLOCATION', VALUE=LOCATION, RC=STATUS)
      _VERIFY(STATUS)
      call ESMF_AttributeSet(FIELD, NAME='LONG_NAME', VALUE=LONG_NAME, RC=STATUS)
      _VERIFY(STATUS)
      call ESMF_AttributeSet(FIELD, NAME='UNITS', VALUE=UNITS, RC=STATUS)
      _VERIFY(STATUS)

      call ESMF_AttributeSet(FIELD, NAME='REFRESH_INTERVAL', VALUE=REFRESH, RC=STATUS)
      _VERIFY(STATUS)
      call ESMF_AttributeSet(FIELD, NAME='AVERAGING_INTERVAL', VALUE=AVGINT, RC=STATUS)
      _VERIFY(STATUS)
      call ESMF_AttributeSet(FIELD, NAME='HALOWIDTH', VALUE=HW, RC=STATUS)
      _VERIFY(STATUS)
      call ESMF_AttributeSet(FIELD, NAME='RESTART', VALUE=RESTART, RC=STATUS)
      _VERIFY(STATUS)
      call ESMF_AttributeSet(FIELD, NAME='FIELD_TYPE', VALUE=FIELD_TYPE, RC=STATUS)
      _VERIFY(STATUS)
      call ESMF_AttributeSet(FIELD, NAME='STAGGERING', VALUE=STAGGERING, RC=STATUS)
      _VERIFY(STATUS)
      call ESMF_AttributeSet(FIELD, NAME='ROTATION', VALUE=ROTATION, RC=STATUS)
      _VERIFY(STATUS)
      if (associated(UNGRD)) Then
         call ESMF_AttributeSet(FIELD, NAME='UNGRIDDED_NAME', VALUE=UNGRIDDED_NAME, RC=STATUS)
         _VERIFY(STATUS)
         call ESMF_AttributeSet(FIELD, NAME='UNGRIDDED_UNIT', VALUE=UNGRIDDED_UNIT, RC=STATUS)
         _VERIFY(STATUS)
         if (associated(UNGRIDDED_COORDS)) then
            szUngrd = size(ungridded_coords)
            call ESMF_AttributeSet(FIELD, NAME='UNGRIDDED_COORDS', itemCount=szUngrd, &
                                   valuelist=ungridded_coords, rc=status)
            _VERIFY(STATUS)
         end if      
      end if

      if (associated(ATTR_RNAMES)) then
         DO N = 1, size(ATTR_RNAMES) 
            call ESMF_AttributeSet(FIELD, NAME=trim(ATTR_RNAMES(N)), &
                                        VALUE=ATTR_RVALUES(N), RC=STATUS)
            _VERIFY(STATUS)
         END DO
      end if

      if (associated(ATTR_INAMES)) then
         DO N = 1, size(ATTR_INAMES) 
            call ESMF_AttributeSet(FIELD, NAME=trim(ATTR_INAMES(N)), &
                                        VALUE=ATTR_IVALUES(N), RC=STATUS)
            _VERIFY(STATUS)
         END DO
      end if

10    if (FRIENDLYTO /= "") then

! parse the string for ":" word delimiters
         done = .false.
         n1 = 1
         NE = len(FRIENDLYTO)

         DO WHILE(.not. DONE)
            N = INDEX(FRIENDLYTO(N1:NE), ':')
            IF (N == 0) then
               DONE = .TRUE.
               N2 = NE
            ELSE
               N2 = N1 + N - 2
            END IF
            if (N1 <= N2 .and. N2 > 0) then
               if (IAND(STAT, MAPL_BundleItem) /= 0) then
                  call ESMF_AttributeSet(BUNDLE, &
                       NAME='FriendlyTo'//trim(FRIENDLYTO(N1:N2)), &
                       VALUE=.TRUE., RC=STATUS)
                  _VERIFY(STATUS)
               else
!print *,"DEBUG: setting FieldAttr:FriendlyTo"//trim(FRIENDLYTO(N1:N2))
                  call ESMF_AttributeSet(FIELD, &
                       NAME='FriendlyTo'//trim(FRIENDLYTO(N1:N2)), &
                       VALUE=.TRUE., RC=STATUS)
                  _VERIFY(STATUS)
               end if
            end if

            N1 = N1 + N
         END DO

      end if

   enddo
   call ESMF_AttributeSet(STATE, NAME="MAPL_GridTypeBits", VALUE=ATTR, RC=STATUS)
   _VERIFY(STATUS)
   call ESMF_AttributeSet(STATE, NAME="MAPL_RestartRequired", VALUE=rstReq, RC=STATUS)
   _VERIFY(STATUS)

   _RETURN(ESMF_SUCCESS)

end subroutine MAPL_StateCreateFromSpecNew


!=============================================================================
!=============================================================================
!=============================================================================
!=============================================================================


! !IROUTINE: MAPL_GridCompGetVarSpec

! !INTERFACE:

subroutine MAPL_GridCompGetVarSpecs(GC,IMPORT,EXPORT,INTERNAL,RC)

! !ARGUMENTS:

    type(ESMF_GridComp),    intent(INOUT)  :: GC
    type(MAPL_VarSpec ), pointer, optional :: IMPORT(:)
    type(MAPL_VarSpec ), pointer, optional :: EXPORT(:)
    type(MAPL_VarSpec ), pointer, optional :: INTERNAL(:)
    integer,             optional, intent(OUT) :: RC


!=============================================================================
!
! ErrLog Variables

    character(len=ESMF_MAXSTR)            :: IAm='MAPL_GridCompGetVarSpec'
    integer                               :: STATUS
    character(len=ESMF_MAXSTR)            :: COMP_NAME

    type (MAPL_MetaComp ), pointer        :: STATE

! Begin

! Get my name and set-up traceback handle
! ---------------------------------------

    call ESMF_GridCompGet( GC, NAME=COMP_NAME, RC=STATUS )
    _VERIFY(STATUS)
    Iam = trim(COMP_NAME) // trim(Iam)

! Get the private state
! ---------------------

    call MAPL_InternalStateRetrieve ( GC, STATE, RC=STATUS )
    _VERIFY(STATUS)

! Get the specs for the 3 ESMF states
! -----------------------------------

    call MAPL_StateGetVarSpecs(STATE,IMPORT,EXPORT,INTERNAL,RC=STATUS)
    _VERIFY(STATUS)

    _RETURN(ESMF_SUCCESS)

end subroutine MAPL_GridCompGetVarSpecs



! !IROUTINE: MAPL_StateGetVarSpec

! !INTERFACE:

subroutine MAPL_StateGetVarSpecs(STATE,IMPORT,EXPORT,INTERNAL,RC)

! !ARGUMENTS:

    type(MAPL_MetaComp),       intent(IN)  :: STATE
    type(MAPL_VarSpec ), pointer, optional :: IMPORT(:)
    type(MAPL_VarSpec ), pointer, optional :: EXPORT(:)
    type(MAPL_VarSpec ), pointer, optional :: INTERNAL(:)
    integer,             optional, intent(OUT) :: RC


!=============================================================================
!
! ErrLog Variables

! Begin

! Get the specs for the 3 ESMF states
! -----------------------------------

    if(present(IMPORT)) then
     IMPORT => STATE%COMPONENT_SPEC%IMPORT%OLD_VAR_SPECS
    endif

    if(present(EXPORT)) then
     EXPORT => STATE%COMPONENT_SPEC%EXPORT%OLD_VAR_SPECS
    endif

    if(present(INTERNAL)) then
     INTERNAL => STATE%COMPONENT_SPEC%INTERNAL%OLD_VAR_SPECS
    endif

    _RETURN(ESMF_SUCCESS)

  end subroutine MAPL_StateGetVarSpecs



!=============================================================================
!=============================================================================
!=============================================================================
!=============================================================================
!=============================================================================



! !IROUTINE: MAPL_WireComponent

! !INTERFACE:

recursive subroutine MAPL_WireComponent(GC, RC)

! !ARGUMENTS:

    type(ESMF_GridComp), intent(INOUT) :: GC
    integer,   optional                :: RC      ! return code

! !DESCRIPTION: This connects the child components, creates the couplers,
!               and adds child info to GC's import and export specs.


!=============================================================================
!
! ErrLog Variables


    character(len=ESMF_MAXSTR)            :: IAm
    integer                               :: STATUS
    character(len=ESMF_MAXSTR)            :: COMP_NAME

! Local types
! -----------

    type SpecWrapper
     type (MAPL_VarSpec),              pointer :: SPEC(:)
    end type SpecWrapper

! Local variables
! ---------------

    type (MAPL_MetaComp),              pointer  :: STATE 
    type (ESMF_CplComp),               pointer  :: CCS(:,:)
    type (MAPL_VarSpec),               pointer  :: IMPORT_SPECS(:)
    type (MAPL_VarSpec),               pointer  :: EXPORT_SPECS(:)
    type (MAPL_VarSpec),               pointer  :: IM_SPECS(:)
    type (MAPL_VarSpec),               pointer  :: EX_SPECS(:)
  
    type (SpecWrapper), pointer, dimension(:,:) :: SRCS
    type (SpecWrapper), pointer, dimension(:,:) :: DSTS

    character(len=ESMF_MAXSTR)                  :: SRCNAME
    character(len=ESMF_MAXSTR)                  :: DSTNAME
    character(len=ESMF_MAXSTR)                  :: SHORT_NAME
    character(len=ESMF_MAXSTR)                  :: ENAME
    integer                                     :: I, J, K, N
    integer                                     :: NC
    integer                                     :: STAT
    logical                                     :: SATISFIED
    logical                                     :: PARENTIMPORT
    type (MAPL_Connectivity), pointer           :: conn
    type (MAPL_VarConn), pointer                :: CONNECT(:)
    type (MAPL_VarConn), pointer                :: DONOTCONN(:)
    type(ESMF_GridComp), pointer :: gridcomp
! Begin

! Get my name and set-up traceback handle
! ---------------------------------------

    Iam = 'MAPL_WireComponent'
    call ESMF_GridCompGet( GC, NAME=COMP_NAME, RC=STATUS )
    _VERIFY(STATUS)
    Iam = trim(COMP_NAME) // trim(Iam)

! Retrieve the pointer to the child components
! --------------------------------------------

    call MAPL_InternalStateRetrieve ( GC, STATE, RC=STATUS )
    _VERIFY(STATUS)

    IMPORT_SPECS => STATE%COMPONENT_SPEC%IMPORT%OLD_VAR_SPECS
    EXPORT_SPECS => STATE%COMPONENT_SPEC%EXPORT%OLD_VAR_SPECS

! First look for import couplings that are satisfied internally 
! -------------------------------------------------------------

    if (STATE%get_num_children() == 0) then
       _RETURN(ESMF_SUCCESS)
    end if

    call MAPL_ConnectivityGet(gc, connectivityPtr=conn, RC=status)
    _VERIFY(STATUS)

    CONNECT => CONN%CONNECT
    DONOTCONN => CONN%DONOTCONN

    NC = STATE%get_num_children()

    allocate(SRCS(NC,NC), DSTS(NC,NC), STAT=STATUS)
    _VERIFY(STATUS)
    DO I=1,NC
       DO J=1,NC
          NULLIFY(SRCS(J,I)%SPEC)
          NULLIFY(DSTS(J,I)%SPEC)
       END DO
    END DO

! first check if we need to add Exports from children
    do I=1,NC       !  Cycle thru children's imports
       
       gridcomp => STATE%get_child_gridcomp(I)
       call MAPL_GridCompGetVarSpecs(gridcomp, EXPORT=EX_SPECS, RC=STATUS)
       _VERIFY(STATUS)

       if (.not. associated(EX_SPECS)) then
          cycle
       end if

       do K=1,size(EX_SPECS)
          call MAPL_VarSpecGet(EX_SPECS(K), SHORT_NAME=SHORT_NAME, RC=STATUS) 
          if (MAPL_VarIsConnected(CONNECT, SHORT_NAME=SHORT_NAME, &
                                  FROM_EXPORT=I, TO_EXPORT=MAPL_Self, &
                                  RC=STATUS)) then

             _VERIFY(STATUS)
             call MAPL_VarSpecAddRefToList(STATE%COMPONENT_SPEC%EXPORT, EX_SPECS(K), &
                                           RC=STATUS)
             if (STATUS /= MAPL_DuplicateEntry) then
                _VERIFY(STATUS)
             else
                print *,'ERROR: duplicate entry for ',trim(SHORT_NAME)
                _RETURN(ESMF_FAILURE)
             endif
          end if
          _VERIFY(STATUS)
       end do

    end do

! try to satisfy imports internally
    do I=1,NC       !  Cycle thru children's imports

! check "do not connect" list for 
       PARENTIMPORT = .true.
       if (MAPL_VarIsListed(DONOTCONN, SHORT_NAME="MAPL_AnyChildImport", &
                            IMPORT=I, RC=STATUS)) then
          _VERIFY(STATUS)
          PARENTIMPORT = .false.
       end if

       gridcomp => STATE%get_child_gridcomp(I)
       call MAPL_GridCompGetVarSpecs(gridcomp, IMPORT=IM_SPECS, RC=STATUS)
       _VERIFY(STATUS)

       if (.not. associated(IM_SPECS)) then
          cycle
       end if

       do K=1,size(IM_SPECS)

          call MAPL_VarSpecGet(IM_SPECS(K), SHORT_NAME=SHORT_NAME, &
                               STAT=STAT, RC=STATUS) 
          _VERIFY(STATUS)

! do not connect Friendly bundles
#if 0
          IF (IAND(STAT, MAPL_BundleItem) /= 0) then
             cycle
          end if
#endif

! check "do not connect" list 
          if (MAPL_VarIsListed(DONOTCONN, SHORT_NAME=SHORT_NAME, &
                               IMPORT=I, RC=STATUS)) then
             _VERIFY(STATUS)
             cycle
          end if
          _VERIFY(STATUS)

!  Cycle thru all exports
!------------------------
          call MAPL_VarSpecGet(IM_SPECS(K), STAT=STAT, RC=STATUS)
          _VERIFY(STATUS) 

          SATISFIED = .false.
          do J=1,NC      
             if(I==J) cycle
! then check if this is internally satisfied
             if (MAPL_VarIsConnected(CONNECT, IMPORT_NAME=SHORT_NAME, &
                                     IMPORT=I, EXPORT=J, RC=STATUS)) then
                
                _VERIFY(STATUS) 
                SATISFIED = .true.
                cycle
             end if
          end do

          if (SATISFIED) then
             STAT = ior(STAT,MAPL_CplSATISFIED)
             call MAPL_VarSpecSet(IM_SPECS(K), STAT=STAT, RC=STATUS)
             _VERIFY(STATUS)
          end if


          do J=1,NC      
             gridcomp => STATE%get_child_gridcomp(J)
             call MAPL_GridCompGetVarSpecs(gridcomp, EXPORT=EX_SPECS, RC=STATUS)
             _VERIFY(STATUS)

! Trying to satisfy I's imports from J's exports
! ----------------------------------------------


! then check if this is internally satisfied
             if (MAPL_VarIsConnected(CONNECT, &
                                     IMPORT_NAME=SHORT_NAME, EXPORT_NAME=ENAME, &
                                     IMPORT=I, EXPORT=J, RC=STATUS)) then
! If a match is found, add it to that coupler's src and dst specs
! ?? Mark the import satisfied and the export needed.
! -----------------------------------------------------------------
                _VERIFY(STATUS) 
                N =  MAPL_VarSpecGetIndex(EX_SPECS,ENAME,RC=STATUS) 
                if(N /= -1) then
                   _VERIFY(STATUS)
                else
                   print *,'ERROR: cannot find export ',trim(ENAME)
                   _RETURN(ESMF_FAILURE)
                endif
!ALT: currently the function comparing the SPECS assumes SAME names;
!     so we temporarily change the SHORT_NAME, and restore the name after comparison
                call MAPL_VarSpecSet(EX_SPECS(N), SHORT_NAME=SHORT_NAME, RC=STATUS)
                _VERIFY(STATUS)
                if (EX_SPECS(N) == IM_SPECS(K)) then
                   call MAPL_VarSpecSet(EX_SPECS(N), SHORT_NAME=ENAME, RC=STATUS)
                   _VERIFY(STATUS)
! this a direct connection 
! SPECS are the same, no additional averaging is needed
                   call MAPL_Reconnect(STATE,       &
                        STATE%get_child_gridcomp(I), MAPL_Import, K,         &
                        STATE%get_child_gridcomp(J), MAPL_Export, N, RC=STATUS)
                   _VERIFY(STATUS)

                else
                   _ASSERT(MAPL_VarSpecSamePrec(EX_SPECS(N), IM_SPECS(K)),'needs informative message')
                   call MAPL_VarSpecSet(EX_SPECS(N), SHORT_NAME=ENAME, RC=STATUS)
                   _VERIFY(STATUS)
! coupler is needed
                   call MAPL_VarSpecAddRefToList(DSTS(J,I)%SPEC,IM_SPECS(K), RC=STATUS)
                   _VERIFY(STATUS)

                   call MAPL_VarSpecAddRefToList(SRCS(J,I)%SPEC,EX_SPECS(N), RC=STATUS)
                   _VERIFY(STATUS) 
                end if

             else
! Imports that are not internally satisfied have their specs put in the GC's
! import spec to be externally satisfied.  Their status is left unaltered. 
! --------------------------------------------------------------------------
                if (.not. SATISFIED .and. PARENTIMPORT) then
                   _VERIFY(STATUS) 
                   call MAPL_VarSpecGet(IM_SPECS(K), STAT=STAT, RC=STATUS)
                   _VERIFY(STATUS) 
                   if (iand(STAT,MAPL_CplSATISFIED) /= 0) then
                      cycle
                   end if
                   call MAPL_VarSpecAddRefToList(STATE%COMPONENT_SPEC%IMPORT, IM_SPECS(K), RC=STATUS)
                   if (STATUS /= MAPL_DuplicateEntry) then
                      _VERIFY(STATUS)
                   else
                   
                      N =  MAPL_VarSpecGetIndex(STATE%COMPONENT_SPEC%IMPORT%OLD_VAR_SPECS, IM_SPECS(K),RC=STATUS) 
                      if(N /= -1) then
                         _VERIFY(STATUS)
                      else
                         _RETURN(ESMF_FAILURE)
                      endif

                      call MAPL_Reconnect(STATE,       &
                           GC, MAPL_Import, N,         &
                           STATE%get_child_gridcomp(I), MAPL_Import, K, RC=STATUS)
                      _VERIFY(STATUS)
                   end if
                endif
             end if
          enddo
       enddo
    enddo
 
    allocate(STATE%CCS(NC,NC),STAT=STATUS)
    _VERIFY(STATUS)
    allocate(STATE%CIM(NC,NC),STAT=STATUS)
    _VERIFY(STATUS)
    allocate(STATE%CEX(NC,NC),STAT=STATUS)
    _VERIFY(STATUS)
    allocate(STATE%CCcreated(NC,NC),STAT=STATUS)
    _VERIFY(STATUS)
    STATE%CCcreated = .false.

    CCS          => STATE%CCS

    do I=1,NC
       do J=1,NC

          if(associated(DSTS(J,I)%SPEC)) then
             if(I/=J) then
                call ESMF_GridCompGet( STATE%get_child_gridcomp(J), NAME=SRCNAME, RC=STATUS )
                _VERIFY(STATUS)

                call ESMF_GridCompGet( STATE%get_child_gridcomp(I), NAME=DSTNAME, RC=STATUS )
                _VERIFY(STATUS)

                CCS(J,I) = ESMF_CplCompCreate (                        &
                     NAME       = trim(SRCNAME)//'_2_'//trim(DSTNAME), & 
!                     LAYOUT     = STATE%GRID%LAYOUT,                   &
                     contextFlag = ESMF_CONTEXT_PARENT_VM,              &
                     CONFIG     = STATE%CF,                  RC=STATUS )
                _VERIFY(STATUS)

!                STATE%CIM(J,I) = ESMF_StateCreate (     &
!                     STATENAME = trim(SRCNAME)//'_2_'//trim(DSTNAME) // '_Imports', &
!                     STATETYPE = ESMF_STATEEXPORT,               &
!                     RC=STATUS )
!                _VERIFY(STATUS)

!                STATE%CEX(J,I) = ESMF_StateCreate (     &
!                     STATENAME = trim(SRCNAME)//'_2_'//trim(DSTNAME) // '_Exports', &
!                     STATETYPE = ESMF_STATEEXPORT,               &
!                     RC=STATUS )
!                _VERIFY(STATUS)

                STATE%CCcreated(J,I) = .true.

                call WRITE_PARALLEL("Coupler needed for "//trim(SRCNAME)// ' and ' //&
                                    trim(DSTNAME))
                call ESMF_CplCompSetServices (CCS(J,I), GenericCplSetServices, RC=STATUS )
                _VERIFY(STATUS)

                call MAPL_CplCompSetVarSpecs(CCS(J,I),SRCS(J,I)%SPEC,DSTS(J,I)%SPEC,RC=STATUS)
                _VERIFY(STATUS)

             endif
          endif
       enddo
    enddo


! Add my children's exports specs to mine
! ---------------------------------------

! currently disabled (they will be explicitly added to the EXPORT as nested)
!    call MAPL_StateAddExportSpecFrmAll ( STATE, RC=STATUS  )
!    _VERIFY(STATUS)
     

    deallocate(SRCS, DSTS, STAT=STATUS)
    _VERIFY(STATUS)

! Wire my children
! ---------------------------------------

    do I=1,NC
!!!ALT      call MAPL_WireComponent(GCS(I), RC=STATUS)
      _VERIFY(STATUS)
    end do

    _RETURN(ESMF_SUCCESS)
  
  end subroutine MAPL_WireComponent

  subroutine MAPL_BundleInit(GC,STATE,BUNDLENAME,RC)

! !ARGUMENTS:

    type(ESMF_GridComp),           intent(INOUT)  :: GC
    type(ESMF_State),              intent(IN   )  :: STATE
    character (len=*),             intent(IN   )  :: BUNDLENAME
    integer,             optional, intent(  OUT)  :: RC

!=============================================================================
!
! ErrLog Variables

    character(len=ESMF_MAXSTR)            :: IAm='MAPL_BundleInit'
    integer                               :: STATUS
    character(len=ESMF_MAXSTR)            :: COMP_NAME

! Local variables
! ---------------

    type (MAPL_MetaComp),              pointer  :: MAPLOBJ 

    type (ESMF_FieldBundle)                          :: BUNDLE
    type (ESMF_Field)                           :: FIELD
    type (MAPL_VarSpec),               pointer  :: INTERNAL_SPEC(:)
    integer                                     :: I
    integer                                     :: STAT

!EOP

! Get my name and set-up traceback handle
! ---------------------------------------

    call ESMF_GridCompGet( GC, NAME=COMP_NAME, RC=STATUS )
    _VERIFY(STATUS)
    Iam = trim(COMP_NAME) // trim(Iam)

! Retrieve the pointer to the internal state
! --------------------------------------------

    call MAPL_InternalStateRetrieve(GC, MAPLOBJ, RC=STATUS)
    _VERIFY(STATUS)

    call ESMF_StateGet(STATE, BUNDLENAME, BUNDLE, RC=STATUS)
    _VERIFY(STATUS)

    INTERNAL_SPEC => MAPLOBJ%COMPONENT_SPEC%INTERNAL%OLD_VAR_SPECS
    if (.not. associated(INTERNAL_SPEC)) then
       _RETURN(ESMF_FAILURE)
    end if
    do I = 1, size(INTERNAL_SPEC)
       call MAPL_VarSpecGet(INTERNAL_SPEC(I), FIELD=FIELD, STAT=STAT, RC=STATUS)
       _VERIFY(STATUS)

       if (ior(STAT, MAPL_FriendlyVariable) /= 0) then
          cycle
       end if

!ALT: alternatevly, we could get the field from the INTERNAL_ESMF_STATE
       call MAPL_FieldBundleAdd(bundle, field, rc=status)
       _VERIFY(STATUS)
    end do

    _RETURN(ESMF_SUCCESS)
  end subroutine MAPL_BundleInit


! !IROUTINE: MAPL_StatePrintSpec

! !INTERFACE: 

  recursive subroutine MAPL_StatePrintSpec(GC, RC)

! !ARGUMENTS:

    type(ESMF_GridComp),           intent(INOUT)  :: GC
    integer,             optional, intent(  OUT)  :: RC


!=============================================================================
!
! ErrLog Variables

    character(len=ESMF_MAXSTR)            :: IAm='MAPL_StatePrintSpec'
    integer                               :: STATUS
    character(len=ESMF_MAXSTR)            :: COMP_NAME

! Local variables
! ---------------

    type (MAPL_MetaComp),              pointer  :: MAPLOBJ 

    type (MAPL_VarSpec),               pointer  :: IMPORT_SPEC(:)
    type (MAPL_VarSpec),               pointer  :: EXPORT_SPEC(:)
    integer                                     :: I
    type(ESMF_GridComp), pointer :: gridcomp

!EOP

! Get my name and set-up traceback handle
! ---------------------------------------

    call ESMF_GridCompGet( GC, NAME=COMP_NAME, RC=STATUS )
    _VERIFY(STATUS)
    Iam = trim(COMP_NAME) // trim(Iam)

! Retrieve the pointer to the internal state
! --------------------------------------------

    call MAPL_InternalStateRetrieve ( GC, MAPLOBJ, RC=STATUS )
    _VERIFY(STATUS)

    IMPORT_SPEC => MAPLOBJ%COMPONENT_SPEC%IMPORT%OLD_VAR_SPECS
    EXPORT_SPEC => MAPLOBJ%COMPONENT_SPEC%EXPORT%OLD_VAR_SPECS

    if (associated(IMPORT_SPEC)) then
       call WRITE_PARALLEL("==========================")
       call WRITE_PARALLEL("IMPORT spec for " // trim(COMP_NAME))
       call WRITE_PARALLEL("==========================")
       if (associated(IMPORT_SPEC)) then
          do I = 1, size(IMPORT_SPEC)
             call MAPL_VarSpecPrint(IMPORT_SPEC(I), RC=STATUS)
             _VERIFY(STATUS)
          end do
       end if
    end if


    if (associated(EXPORT_SPEC)) then
       call WRITE_PARALLEL("==========================")
       call WRITE_PARALLEL("EXPORT spec for " // trim(COMP_NAME))
       call WRITE_PARALLEL("==========================")
       if (associated(EXPORT_SPEC)) then
          do I = 1, size(EXPORT_SPEC)
             call MAPL_VarSpecPrint(EXPORT_SPEC(I), RC=STATUS)
             _VERIFY(STATUS)
          end do
       end if
    end if

       do I = 1, MAPLOBJ%get_num_children()
          gridcomp => MAPLOBJ%GET_CHILD_GRIDCOMP(I)
          call MAPL_StatePrintSpec(gridcomp, RC=STATUS)
          _VERIFY(STATUS)
       end do

    _RETURN(ESMF_SUCCESS)
  end subroutine MAPL_StatePrintSpec



  recursive subroutine MAPL_GenericSpecEnum(GC, SPECS, RC)
! !ARGUMENTS:

     type(ESMF_GridComp),           intent(INout)  :: GC
     type(StateSpecification), intent(inout) :: specs
    integer,             optional, intent(  OUT)  :: RC

!=============================================================================
!
! ErrLog Variables

    character(len=ESMF_MAXSTR)            :: IAm
    integer                               :: STATUS
    character(len=ESMF_MAXSTR)            :: COMP_NAME

! Local variables
! ---------------

    type (MAPL_MetaComp),              pointer  :: STATE 
    type (MAPL_VarSpec),               pointer  :: IMPORT_SPEC(:)
    type (MAPL_VarSpec),               pointer  :: EXPORT_SPEC(:)
    integer                                     :: I, K
    integer                                     :: LBL
    type(ESMF_GridComp), pointer :: gridcomp
!EOP

! Get my name and set-up traceback handle
!----------------------------------------

    call ESMF_GridCompGet( GC, NAME=COMP_NAME, RC=STATUS )
    _VERIFY(STATUS)
    Iam = trim(COMP_NAME) // 'MAPL_GenericSpecEnum'

! Retrieve the pointer to the internal state
!--------------------------------------------

    call MAPL_InternalStateGet ( GC, STATE, RC=STATUS )
    _VERIFY(STATUS)

    IMPORT_SPEC => STATE%COMPONENT_SPEC%IMPORT%OLD_VAR_SPECS
    EXPORT_SPEC => STATE%COMPONENT_SPEC%EXPORT%OLD_VAR_SPECS

! Add all import specs to the list
!  and label by place on list.
!---------------------------------

    if (associated(IMPORT_SPEC)) then
       do I = 1, size(IMPORT_SPEC)
          call MAPL_VarSpecAddRefToList(SPECS, IMPORT_SPEC(I), &
                                        ALLOW_DUPLICATES=.true., RC=STATUS)
          if (STATUS /= MAPL_DuplicateEntry) then
             _VERIFY(STATUS)
          end if
          LBL = SPECS%var_specs%size()
          call MAPL_VarSpecSet(SPECS%old_var_specs(LBL), LABEL=LBL, RC=STATUS )
          _VERIFY(STATUS)

       end do
    end if

! Add all export specs to the list
!  and label in a funny way.
!---------------------------------

    if (associated(EXPORT_SPEC)) then
       do I = 1, size(EXPORT_SPEC)
          call MAPL_VarSpecAddRefToList(SPECS, EXPORT_SPEC(I), &
                                        ALLOW_DUPLICATES=.true., RC=STATUS)
          if (STATUS /= MAPL_DuplicateEntry) then
             _VERIFY(STATUS)
          end if
          K = specs%var_specs%size()
          call MAPL_VarSpecGet(SPECS%old_var_specs(K), LABEL=LBL, RC=STATUS )
          _VERIFY(STATUS)
          if (LBL == 0) then
             call MAPL_VarSpecSet(SPECS%old_var_specs(K), LABEL=K, RC=STATUS )
             _VERIFY(STATUS)
          end if
       end do
    end if

! Do the same for the children
!-----------------------------

       do I = 1, STATE%get_num_children()
          gridcomp => STATE%GET_CHILD_GRIDCOMP(I)
          call MAPL_GenericSpecEnum(gridcomp, SPECS, RC=STATUS)
          _VERIFY(STATUS)
       end do

    _RETURN(ESMF_SUCCESS)

  end subroutine MAPL_GenericSpecEnum




  subroutine MAPL_LinkCreate(LINK,              &
       GC_FROM, STATETYPE_FROM, SPECINDEX_FROM, &
       GC_TO,   STATETYPE_TO,   SPECINDEX_TO,   &
       RC )

    type (MAPL_Link),              pointer     :: LINK(:)
    type(ESMF_GridComp),        intent(IN   )  :: GC_FROM
    integer,                    intent(IN   )  :: STATETYPE_FROM
    integer,                    intent(IN   )  :: SPECINDEX_FROM
    type(ESMF_GridComp),        intent(IN   )  :: GC_TO
    integer,                    intent(IN   )  :: STATETYPE_TO
    integer,                    intent(IN   )  :: SPECINDEX_TO
    integer,          optional, intent(  OUT)  :: RC     ! Error code:
    


    character(len=ESMF_MAXSTR), parameter :: IAm="MAPL_LinkCreate"
    integer                               :: STATUS

    integer                               :: I

    type (MAPL_Link ), pointer         :: TMP(:)
    type (MAPL_LinkType) :: FROM
    type (MAPL_LinkType) :: TO

      if(.not. associated(LINK)) then
       allocate(LINK(0),stat=STATUS)
       _VERIFY(STATUS)
      else
!ALT: check for duplicates ???
      endif
    
        
      I = size(LINK)

      allocate(TMP(I+1),stat=STATUS)
      _VERIFY(STATUS)
      
      TMP(1:I) = LINK
      deallocate(LINK, stat=STATUS)
      _VERIFY(STATUS)

      allocate(TMP(I+1)%Ptr,stat=STATUS)
      _VERIFY(STATUS)

      FROM = MAPL_LinkType(GC_FROM, STATETYPE_FROM, SPECINDEX_FROM)
      TO   = MAPL_LinkType(GC_TO,   STATETYPE_TO,   SPECINDEX_TO  )

      TMP(I+1)%Ptr = MAPL_LinkForm(FROM, TO)

      LINK => TMP

      _RETURN(ESMF_SUCCESS)

  end subroutine MAPL_LinkCreate


  recursive subroutine MAPL_Reconnect(STATE,              &
       GC_FROM, STATETYPE_FROM, SPECINDEX_FROM, &
       GC_TO,   STATETYPE_TO,   SPECINDEX_TO,   &
       RC )

    type (MAPL_MetaComp)                       :: STATE
    type(ESMF_GridComp),        intent(IN   )  :: GC_FROM
    integer,                    intent(IN   )  :: STATETYPE_FROM
    integer,                    intent(IN   )  :: SPECINDEX_FROM
    type(ESMF_GridComp),        intent(IN   )  :: GC_TO
    integer,                    intent(IN   )  :: STATETYPE_TO
    integer,                    intent(IN   )  :: SPECINDEX_TO
    integer,          optional, intent(  OUT)  :: RC     ! Error code:
    


    character(len=ESMF_MAXSTR), parameter :: IAm="MAPL_Reconnect"
    integer                               :: STATUS


! Local variables
! ---------------

    type (MAPL_MetaComp),          pointer  :: PSTATE 


! Retrieve the pointer to the internal state of Root. 
! ----------------------------------------------------

    call MAPL_InternalStateRetrieve ( STATE%RootGC, PSTATE, RC=STATUS )
    _VERIFY(STATUS)

    call MAPL_LinkCreate(PSTATE%LINK,              &
       GC_FROM, STATETYPE_FROM, SPECINDEX_FROM, &
       GC_TO,   STATETYPE_TO,   SPECINDEX_TO,   &
       RC=STATUS )
    _VERIFY(STATUS)


    _RETURN(ESMF_SUCCESS)
  end subroutine MAPL_Reconnect

  integer function MAPL_LabelGet(LINK, RC)
    type (MAPL_LinkType)                       :: LINK
    integer,          optional, intent(  OUT)  :: RC     ! Error code:
    


    character(len=ESMF_MAXSTR), parameter :: IAm="MAPL_GetLabel"
    integer                               :: STATUS


! Local variables
! ---------------

    type (MAPL_MetaComp),              pointer  :: STATE 
    type (MAPL_VarSpec),               pointer  :: SPEC(:)


! Retrieve the pointer to the internal state of Root. 
! ----------------------------------------------------

    call MAPL_InternalStateRetrieve ( LINK%GC, STATE, RC=STATUS )
    _VERIFY(STATUS)

! Local aliases to the state
! ---------------------------------------------------

    if (LINK%StateType == MAPL_Import) then
       SPEC => STATE%COMPONENT_SPEC%IMPORT%OLD_VAR_SPECS
    else if (LINK%StateType == MAPL_Export) then
       SPEC => STATE%COMPONENT_SPEC%EXPORT%OLD_VAR_SPECS
    else
       _RETURN(ESMF_FAILURE)
    end if



    call MAPL_VarSpecGet(SPEC(LINK%SpecId), LABEL = MAPL_LabelGet, RC = STATUS)
    _VERIFY(STATUS)

    _RETURN(ESMF_SUCCESS)
  end function MAPL_LabelGet

  subroutine MAPL_FriendlyGet ( GC, NAME, FIELD, REQUESTOR, RC )

! !ARGUMENTS:

    type(ESMF_GridComp),           intent(INOUT)  :: GC
    character(len=*),              intent(IN   )  :: NAME
    character(len=*),    optional, intent(IN   )  :: REQUESTOR !ALT (set to optional TEMPORARY)
    type(ESMF_Field),              intent(  OUT)  :: FIELD
    integer,             optional, intent(  OUT)  :: RC

!=============================================================================
!
! ErrLog Variables

    integer                               :: STATUS

! Local variables
! ---------------

    type (MAPL_MetaComp),              pointer  :: STATE 
    logical                                     :: FRIENDLY

    integer                                     :: N, STAT


! Retrieve the pointer to the internal state of Root. 
! ----------------------------------------------------

    call MAPL_InternalStateRetrieve ( GC, STATE, RC=STATUS )
    _VERIFY(STATUS)

    N =  MAPL_VarSpecGetIndex(STATE%COMPONENT_SPEC%INTERNAL%OLD_VAR_SPECS, NAME, RC=STATUS) 
    if(N /= -1) then
       _VERIFY(STATUS)
    else
       _RETURN(ESMF_FAILURE)
    endif

    call MAPL_VarSpecGet(STATE%COMPONENT_SPEC%INTERNAL%OLD_VAR_SPECS(N), STAT=STAT, RC=STATUS)
    _VERIFY(STATUS)

    _ASSERT(iand(STAT, MAPL_FriendlyVariable) /= 0,'needs informative message')

    call ESMF_StateGet(STATE%get_internal_state(), NAME, FIELD, RC=STATUS)
    _VERIFY(STATUS)

    if (present(REQUESTOR)) then
       call ESMF_AttributeGet  (FIELD, NAME="FriendlyTo"//trim(REQUESTOR),VALUE=FRIENDLY, RC=STATUS)
       _VERIFY(STATUS)
       _ASSERT(FRIENDLY,'needs informative message')
    end if

    _RETURN(ESMF_SUCCESS)
  end subroutine MAPL_FriendlyGet





 subroutine MAPL_CopyFriendlinessInState(STATEOUT, NAMEOUT,STATEIN,NAMEIN,RC)
    type(ESMF_STATE),      intent(INOUT) :: STATEOUT
    character(len=*),      intent(IN   ) :: nameOUT
    type(ESMF_STATE),      intent(IN   ) :: STATEIN
    character(len=*),      intent(IN   ) :: nameIN
    integer,    optional,  intent(  OUT) :: rc
    
    character(len=ESMF_MAXSTR), parameter :: IAm="MAPL_CopyFriendlinessInState"
    integer                               :: status 
    type(ESMF_FIELD)                      :: FIELDIN
    type(ESMF_FIELD)                      :: FIELDOUT

    call ESMF_StateGet(STATEIN ,NAMEIN ,FIELDIN ,RC=STATUS)
    _VERIFY(STATUS)
    call ESMF_StateGet(STATEOUT,NAMEOUT,FIELDOUT,RC=STATUS)
    _VERIFY(STATUS)
    call  MAPL_CopyFriendlinessInField(FIELDOUT,FIELDIN,RC=STATUS)
    _VERIFY(STATUS)

    _RETURN(ESMF_SUCCESS)
  end subroutine MAPL_CopyFriendlinessInState



 subroutine MAPL_CopyFriendlinessInField(FIELDOUT,FIELDIN,RC)
    type(ESMF_FIELD),      intent(INOUT) :: FIELDOUT
    type(ESMF_FIELD),      intent(INout) :: FIELDIN
    integer,    optional,  intent(  OUT) :: rc
    
    character(len=ESMF_MAXSTR), parameter :: IAm="MAPL_CopyFriendlinessInField"
    integer                               :: status 
    integer                               :: I, NF
    character(len=ESMF_MAXSTR)            :: NAME
    logical                               :: VALUE

    call ESMF_AttributeGet(FIELDIN, count=NF, RC=STATUS)
    _VERIFY(STATUS)

    do I=1,NF
       call ESMF_AttributeGet(FIELDIN,attributeIndex=I,NAME=NAME,RC=STATUS)
       _VERIFY(STATUS)
       NAME = trim(NAME)
       if(NAME(1:10)=='FriendlyTo') then
          call ESMF_AttributeGet(FIELDIN , NAME=NAME, VALUE=VALUE, RC=STATUS)
          _VERIFY(STATUS)
          call ESMF_AttributeSet(FIELDOUT, NAME=NAME, VALUE=VALUE, RC=STATUS)
          _VERIFY(STATUS)
       end if
    end do

    _RETURN(ESMF_SUCCESS)
  end subroutine MAPL_CopyFriendlinessInField


!....................................................................................

  recursive subroutine MAPL_GridCompGetFriendlies0 ( GC, TO, BUNDLE, AddGCPrefix, RC )

! !ARGUMENTS:

    type(ESMF_GridComp),           intent(INOUT)  :: GC
    character(len=*),              intent(IN   )  :: TO(:)
    type(ESMF_FieldBundle  ),           intent(INOUT)  :: BUNDLE
    logical, optional,             intent(IN   )  :: AddGCPrefix
    integer,             optional, intent(  OUT)  :: RC

!=============================================================================
!
! ErrLog Variables

    integer                               :: STATUS

! Local variables
! ---------------

    type (MAPL_MetaComp),        pointer  :: STATE 
    type (ESMF_State), pointer                     :: INTERNAL
    type (ESMF_Field)                     :: FIELD, TempField
    character (len=ESMF_MAXSTR), allocatable  :: itemNameList(:)
    type(ESMF_StateItem_Flag),   allocatable  :: itemtypeList(:)
    type(ESMF_FieldBundle)                    :: B
 
    integer                               :: I, N
    integer                               :: J, NF
    integer, allocatable :: orderlist(:)
    integer :: jj
    character(len=ESMF_MAXSTR)           :: attrName
    character(len=ESMF_MAXSTR), allocatable :: currList(:)
    integer                                 :: natt
    logical                                 :: haveAttr

    logical                                 :: AddPrefix_
    character(len=ESMF_MAXSTR)              :: GC_NAME, fieldname
    type(ESMF_GridComp), pointer :: gridcomp

! Get my MAPL_Generic state
!--------------------------
    
    AddPrefix_ = .false.
    if (present(AddGCPrefix) ) then
       AddPrefix_ = AddGCPrefix
    end if

    call MAPL_InternalStateGet ( GC, STATE, RC=STATUS)
    _VERIFY(STATUS)

    ! Call recursively the children
    !==============================OB
    ! as a consequence of some assuptions in CHEM
    ! we are going to allow recursing ONLY
    ! when addGCPrefix is passed in and it is set to .true.
    ! Physics does not pass this agrument

    if (AddPrefix_ .and. allocated(state%GCnamelist)) then
       do I = 1, size(state%GCnamelist)
          call write_parallel("Executing getFriendlies for " // &
               trim(state%GCnamelist(I)))
          gridcomp => state%GET_CHILD_GRIDCOMP(I)
          call MAPL_GridCompGetFriendlies( gridcomp , TO, BUNDLE, &
               AddGCPrefix, RC=status)
          _VERIFY(status)
       end do
    end if

!    _RETURN(0)
! now call itself

    if (.not.associated(state%component_spec%internal%old_var_specs)) then
       _RETURN(ESMF_SUCCESS)
    end if
    INTERNAL => STATE%get_internal_state()

    call ESMF_StateGet(INTERNAL, ITEMCOUNT=N,  RC=STATUS)
    _VERIFY(STATUS)

    allocate(itemNameList(N)     ,STAT=STATUS)
    _VERIFY(STATUS)
    allocate(itemtypeList(N),STAT=STATUS)
    _VERIFY(STATUS)

    call ESMF_StateGet(INTERNAL,ITEMNAMELIST=itemNamelist,ITEMTYPELIST=itemtypeList,RC=STATUS)
    _VERIFY(STATUS)

    attrName = MAPL_StateItemOrderList
    call ESMF_AttributeGet(internal, NAME=attrName, isPresent=haveAttr, RC=STATUS)
    _VERIFY(STATUS)
    if (haveAttr) then
       call ESMF_AttributeGet(internal, NAME=attrName, itemcount=natt, RC=STATUS)
       _VERIFY(STATUS)
    else
       natt = N
    end if

    if (natt == 0) then
       _RETURN(ESMF_SUCCESS)
    endif
    allocate(orderlist(natt), stat=status)
    _VERIFY(STATUS)

    if (haveAttr) then
       allocate(currList(natt), stat=status)
       _VERIFY(STATUS)

       ! get the current list
       call ESMF_AttributeGet(internal, NAME=attrName, VALUELIST=currList, rc=status)
       _VERIFY(STATUS)

       orderList = -1 ! not found
       do i = 1, natt
          ! search loop
          do jj = 1, N
             if (itemNameList(jj) == currList(i)) then
                orderList(i) = jj
                exit
             end if
          end do

       end do

       deallocate(currList)
    else
       do i = 1, natt
          orderList(i) = i
       end do
    end if

    do JJ = 1, natt

       I = ORDERLIST(JJ)
!    do I=1,N
       if(itemtypeList(I)==ESMF_STATEITEM_FIELD) then
          call ESMF_StateGet(INTERNAL,itemNameList(I),FIELD,RC=STATUS)
          _VERIFY(STATUS)
          call Am_I_Friendly_ ( FIELD, TO, RC=STATUS ) 
          if(STATUS==ESMF_SUCCESS) then
            if (AddPrefix_) then
              call ESMF_GridCompGet(GC, NAME=GC_NAME, RC=status)
              _VERIFY(STATUS)
                if (scan(itemNameList(I),"::")==0) then
                  TempField = MAPL_FieldCreate(FIELD, name=(trim(GC_NAME)//'::'//trim(itemNameList(I))), RC=STATUS)
                  _VERIFY(STATUS)
                  call PutFieldInBundle__(BUNDLE, TempField, RC=STATUS)
                  _VERIFY(STATUS)
                else
                  call PutFieldInBundle__(BUNDLE, FIELD, RC=STATUS )
                  _VERIFY(STATUS)
                end if
              else
                call PutFieldInBundle__(BUNDLE, FIELD, RC=STATUS )
                _VERIFY(STATUS)
            end if ! (AddPrefix_)
          end if
       else if(itemtypeList(I)==ESMF_STATEITEM_FieldBundle) then
          call ESMF_StateGet(INTERNAL,itemNameList(I), B, RC=STATUS)
          _VERIFY(STATUS)
          call ESMF_FieldBundleGet(B,FieldCount=NF, RC=STATUS)
          _VERIFY(STATUS)
          call Am_I_Friendly__ ( B, TO, RC=STATUS ) 
          if(STATUS==ESMF_SUCCESS) then
! if the bundle is "friendly", copy every single field
             DO J=1,NF
               if (AddPrefix_) then
                 call ESMF_GridCompGet(GC, NAME=GC_NAME, RC=status)
                 _VERIFY(STATUS)
                 call MAPL_FieldBundleGet(B,   J,   FIELD,  RC=STATUS)
                 _VERIFY(STATUS)
                 call ESMF_FieldGet (FIELD, name=fieldname, RC=STATUS)
                 _VERIFY(STATUS)
                 if (scan(fieldname,"::")==0) then
                   TempField = MAPL_FieldCreate(FIELD, name=(trim(GC_NAME)//'::'//trim(fieldname)), RC=STATUS)
                   _VERIFY(STATUS)
                   call PutFieldInBundle__(BUNDLE, TempField, RC=STATUS)
                   _VERIFY(STATUS)
                 else
                   call MAPL_FieldBundleGet(B,   J,   FIELD,  RC=STATUS)
                   _VERIFY(STATUS)
                   call PutFieldInBundle__(BUNDLE, FIELD, RC=STATUS )
                   _VERIFY(STATUS)
                 end if
               else
                 call MAPL_FieldBundleGet(B,   J,   FIELD,  RC=STATUS)
                 _VERIFY(STATUS)
                 call PutFieldInBundle__ (BUNDLE, FIELD, RC=STATUS )
                 _VERIFY(STATUS)
               end if ! (AddPrefix_)
             END DO
          else
! check the fields for "friendliness"
             DO J=1,NF
               if (AddPrefix_) then
                 call ESMF_GridCompGet(GC, NAME=GC_NAME, RC=status)
                 _VERIFY(STATUS)
                 call MAPL_FieldBundleGet(B,   J,   FIELD,  RC=STATUS)
                 _VERIFY(STATUS)
                 call ESMF_FieldGet (FIELD, name=fieldname, RC=STATUS)
                 _VERIFY(STATUS)
                 if (scan(fieldname,"::")==0) then
                   TempField = MAPL_FieldCreate(FIELD, name=(trim(GC_NAME)//'::'//trim(fieldname)), RC=STATUS)
                   _VERIFY(STATUS)
                   call PutFieldInBundle__(BUNDLE, TempField, RC=STATUS)
                   _VERIFY(STATUS)
                 else
                   call MAPL_FieldBundleGet(B,   J,   FIELD,  RC=STATUS)
                   _VERIFY(STATUS)
                    call Am_I_Friendly_ ( FIELD, TO, RC=STATUS )
                    if(STATUS==ESMF_SUCCESS) then
                      call PutFieldInBundle__  (BUNDLE, FIELD, RC=STATUS )
                      _VERIFY(STATUS)
                    end if
                 end if
               else
                 call MAPL_FieldBundleGet(B,   J,   FIELD,  RC=STATUS)
                 _VERIFY(STATUS)
                 call Am_I_Friendly_ ( FIELD, TO, RC=STATUS )
                 if(STATUS==ESMF_SUCCESS) then
                   call PutFieldInBundle__  (BUNDLE, FIELD, RC=STATUS )
                   _VERIFY(STATUS)
                 END if
               end if ! (AddPrefix_)
             END DO
          end if
       end if
    end do

    deallocate(orderlist, stat=status)
    _VERIFY(STATUS)
    deallocate(itemNameList     ,STAT=STATUS)
    _VERIFY(STATUS)
    deallocate(itemtypeList,STAT=STATUS)
    _VERIFY(STATUS)

    _RETURN(ESMF_SUCCESS)

  end subroutine MAPL_GridCompGetFriendlies0

  subroutine PutFieldInBundle__(Bundle, Field, multiflag, RC)
    type(ESMF_FieldBundle),  intent(inout) :: Bundle
    type(ESMF_Field),  intent(in   ) :: Field
    logical, optional, intent(in   ) :: multiflag
    integer, optional, intent(  out) :: rc

! ErrLog vars
    integer                   :: STATUS

    ! Local var
    integer                   :: DIMS, I
    integer                   :: fieldRank
    type(ESMF_Field), pointer :: splitFields(:) => null()

    _UNUSED_DUMMY(multiflag)
    call ESMF_FieldGet(FIELD, dimCount=fieldRank, rc=status)
    _VERIFY(status)
    if (fieldRank == 4) then
       call ESMF_AttributeGet(FIELD, NAME='DIMS', VALUE=DIMS, rc=status)
       _VERIFY(status)
       if (DIMS == MAPL_DimsHorzVert) then
          call MAPL_FieldSplit(field, splitFields, RC=status)
          _VERIFY(STATUS)

          do I=1, size(splitFields)
             call MAPL_FieldBundleAdd(BUNDLE, splitFields(I), rc=status )
             _VERIFY(status)
          end do
          deallocate(splitFields)
       else
          call MAPL_FieldBundleAdd(BUNDLE, FIELD, rc=status )
          _VERIFY(status)
       end if
    else
       call MAPL_FieldBundleAdd(BUNDLE, FIELD, rc=status )
       _VERIFY(status)
    end if

    _RETURN(ESMF_SUCCESS)
  end subroutine PutFieldInBundle__
  
   subroutine Am_I_Friendly_ ( FIELD, TO, RC ) 
     type(ESMF_Field),  intent(INout)  :: FIELD
     character(len=*),  intent(IN)  :: TO(:)
     integer,           intent(OUT) :: RC
     logical            :: FRIENDLY, isPresent
     integer            :: I, STATUS
     RC = ESMF_FAILURE    
     do I = 1, size(TO)
        call ESMF_AttributeGet  (FIELD, NAME="FriendlyTo"//trim(TO(I)), &
                                 isPresent=isPresent, RC=STATUS)
        if (isPresent) then
           call ESMF_AttributeGet  (FIELD, NAME="FriendlyTo"//trim(TO(I)), &
                                    VALUE=FRIENDLY, RC=STATUS)
           RC = ESMF_SUCCESS
        endif
     end do
     return 
   end subroutine Am_I_Friendly_

   subroutine Am_I_Friendly__ ( BUNDLE, TO, RC ) 
     type(ESMF_FieldBundle),  intent(INout)  :: BUNDLE
     character(len=*),  intent(IN)  :: TO(:)
     integer,           intent(OUT) :: RC
     logical            :: FRIENDLY, isPresent
     integer            :: I, STATUS
     RC = ESMF_FAILURE    
     do I = 1, size(TO)
        FRIENDLY = .false.
        call ESMF_AttributeGet (BUNDLE, NAME="FriendlyTo"//trim(TO(I)), &
                                isPresent=isPresent, RC=STATUS)
        if (isPresent) then
           call ESMF_AttributeGet (BUNDLE, NAME="FriendlyTo"//trim(TO(I)), &
                                   VALUE=FRIENDLY, RC=STATUS)
           if (FRIENDLY) RC = ESMF_SUCCESS
        endif
     end do
     return 
   end subroutine Am_I_Friendly__


   subroutine MAPL_GridCompGetFriendlies1 ( GC, TO, BUNDLE, AddGCPrefix, RC )

! !ARGUMENTS:

    type(ESMF_GridComp),           intent(INOUT)  :: GC
    character(len=*),              intent(IN   )  :: TO
    type(ESMF_FieldBundle  ),      intent(INOUT)  :: BUNDLE
    logical, optional,             intent(IN   )  :: AddGCPrefix
    integer,             optional, intent(  OUT)  :: RC

!=============================================================================
!
! ErrLog Variables

!    character(len=ESMF_MAXSTR)            :: IAm='MAPL_GridCompGetFriendlies1'
    character(len=ESMF_MAXSTR)            :: TO_(1)

    TO_(1) = TO
    call MAPL_GridCompGetFriendlies0 ( GC, TO_, BUNDLE, AddGCPrefix, RC )

  end subroutine MAPL_GridCompGetFriendlies1

  subroutine MAPL_GridCompGetFriendlies2 ( GC, TO, BUNDLE, AddGCPrefix, RC )

! !ARGUMENTS:

    type(ESMF_GridComp),           intent(INOUT)  :: GC(:)
    character(len=*),              intent(IN   )  :: TO
    type(ESMF_FieldBundle  ),      intent(INOUT)  :: BUNDLE
    logical, optional,             intent(IN   )  :: AddGCPrefix
    integer,             optional, intent(  OUT)  :: RC

!=============================================================================
!
! ErrLog Variables

    integer                               :: STATUS, I
    character(len=ESMF_MAXSTR)            :: TO_(1)

    TO_(1) = TO
    do I=1,size(GC)
       call MAPL_GridCompGetFriendlies0(GC(I), TO_, BUNDLE, AddGCPrefix, RC=STATUS)
       _VERIFY(STATUS)
    end do

    _RETURN(ESMF_SUCCESS)
  end subroutine MAPL_GridCompGetFriendlies2

  subroutine MAPL_GridCompGetFriendlies3 ( GC, TO, BUNDLE, AddGCPrefix, RC )

! !ARGUMENTS:

    type(ESMF_GridComp),           intent(INOUT)  :: GC(:)
    character(len=*),              intent(IN   )  :: TO(:)
    type(ESMF_FieldBundle  ),      intent(INOUT)  :: BUNDLE
    logical, optional,             intent(IN   )  :: AddGCPrefix
    integer,             optional, intent(  OUT)  :: RC

!=============================================================================
!
! ErrLog Variables

    integer                               :: STATUS, I

    do I=1,size(GC)
       call MAPL_GridCompGetFriendlies0(GC(I), TO, BUNDLE, AddGCPrefix, RC=STATUS)
       _VERIFY(STATUS)
    end do

    _RETURN(ESMF_SUCCESS)
  end subroutine MAPL_GridCompGetFriendlies3


!================================

   subroutine MAPL_SetVarSpecForCC(gcA, gcB, ccAxB, rc)
    type(ESMF_GridComp), intent(inout) :: GCA 
    type(ESMF_GridComp), intent(inout) :: GCB 
    type(ESMF_CplComp) , intent(inout) :: CCAxB 
    integer, optional,   intent(  out) :: RC     ! Error code:

! Local vars
    character(len=ESMF_MAXSTR)   :: NAME
    integer                      :: STATUS
    integer                      :: I, N, STAT
    type (MAPL_VarSpec), pointer :: SRCS(:)
    type (MAPL_VarSpec), pointer :: DSTS(:)
    type (MAPL_VarSpec), pointer :: IM_SPECS(:), EX_SPECS(:)

! Begin

    NULLIFY(SRCS)
    NULLIFY(DSTS)

    call MAPL_GridCompGetVarSpecs(gcA, EXPORT=EX_SPECS, RC=STATUS)
    _VERIFY(STATUS)

    call MAPL_GridCompGetVarSpecs(gcB, IMPORT=IM_SPECS,  RC=STATUS)
    _VERIFY(STATUS)

    DO I = 1, size(IM_SPECS)
       call MAPL_VarSpecGet(IM_SPECS(I), STAT=STAT, RC=STATUS)
       _VERIFY(STATUS) 

       IF (IAND(STAT, MAPL_BundleItem) /= 0) then
          cycle
       END IF

       call MAPL_VarSpecAddRefToList(DSTS, IM_SPECS(I), RC=STATUS)
       _VERIFY(STATUS)
    END DO

    IF (.not. associated(DSTS)) then
       _RETURN(ESMF_FAILURE)
    END IF

    DO I = 1, size(DSTS)
       call MAPL_VarSpecGet(DSTS(I), STAT=STAT, SHORT_NAME=NAME, RC=STATUS)
       _VERIFY(STATUS) 

       N =  MAPL_VarSpecGetIndex(EX_SPECS, NAME, RC=STATUS) 
       if(N /= -1) then
          _VERIFY(STATUS)
       else
          call WRITE_PARALLEL("ERROR: cannot match spec:")
          call MAPL_VarSpecPrint(DSTS(I))
          _RETURN(ESMF_FAILURE)
       endif

       call MAPL_VarSpecAddRefToList(SRCS, DSTS(I), RC=STATUS)
       _VERIFY(STATUS)
    END DO

    call MAPL_CplCompSetVarSpecs(ccAxB, SRCS, DSTS, RC=STATUS)
    _VERIFY(STATUS)

    _RETURN(ESMF_SUCCESS)

  end subroutine MAPL_SetVarSpecForCC


  recursive subroutine MAPL_GenericConnCheck(GC, RC)
! !ARGUMENTS:

    type(ESMF_GridComp),           intent(INOUT)  :: GC
    integer,             optional, intent(  OUT)  :: RC

!=============================================================================
!
! ErrLog Variables

    character(len=ESMF_MAXSTR)            :: IAm
    integer                               :: STATUS
    character(len=ESMF_MAXSTR)            :: COMP_NAME

! Local variables
! ---------------

    type (MAPL_MetaComp),              pointer  :: STATE 

    integer                                     :: I
    logical                                     :: err
    type (MAPL_Connectivity), pointer           :: conn
    type(ESMF_GridComp), pointer :: gridcomp

!EOP

! Get my name and set-up traceback handle
! ---------------------------------------

    call ESMF_GridCompGet( GC, NAME=COMP_NAME, RC=STATUS )
    _VERIFY(STATUS)
    Iam = trim(COMP_NAME) // 'MAPL_GenericConnCheck'

! Retrieve the pointer to the internal state
! --------------------------------------------

    call MAPL_InternalStateRetrieve ( GC, STATE, RC=STATUS )
    _VERIFY(STATUS)

    conn => state%connectList
      
    err = .false.
    if (.not. MAPL_ConnCheckUnused(CONN%CONNECT)) then
       err = .true.
       CALL WRITE_PARALLEL("CONNECT ERRORS FOUND in " // trim(COMP_NAME))
    end if

    if (.not. MAPL_ConnCheckUnused(CONN%DONOTCONN)) then
       err = .true.
       CALL WRITE_PARALLEL("DO_NOT_CONNECT ERRORS FOUND in " // trim(COMP_NAME))
    end if

       do I = 1, STATE%get_num_children()
          gridcomp => STATE%GET_CHILD_GRIDCOMP(I)
          call MAPL_GenericConnCheck(gridcomp, RC=STATUS)
          if (status /= ESMF_SUCCESS) then
             err = .true.
          end if
       end do

    if (err) then
       _RETURN(ESMF_FAILURE)
    end if

    _RETURN(ESMF_SUCCESS)
  end subroutine MAPL_GenericConnCheck
  
    
  ! MAPL searches for labels with certain prefixes as well as just the label itself
  pure function get_labels_with_prefix(component_name, label) result(labels_with_prefix)
    character(len=*), intent(in) :: component_name, label
    character(len=ESMF_MAXSTR) :: labels_with_prefix(4), component_type
    
    component_type = component_name(index(component_name, ":") + 1:)

    ! The order to search for labels in resource files
    labels_with_prefix(1) = trim(component_name)//"_"//trim(label)
    labels_with_prefix(2) = trim(component_type)//"_"//trim(label)
    labels_with_prefix(3) = trim(label)
    labels_with_prefix(4) = trim(component_name)//CF_COMPONENT_SEPARATOR//trim(label)
  end function get_labels_with_prefix  


  subroutine MAPL_GetResource_scalar(state, val, label, default, rc)
    type(MAPL_MetaComp), intent(inout) :: state
    character(len=*), intent(in) :: label
    class(*), intent(inout) :: val
    class(*), optional, intent(in) :: default
    integer, optional, intent(out) :: rc

    character(len=ESMF_MAXSTR), allocatable :: labels_to_try(:)
    character(len=:), allocatable :: label_to_print, label_to_use
    integer :: i, status, printrc
    logical :: label_is_present, default_is_present

    default_is_present = present(default)
    
    if (default_is_present) then
       _ASSERT(same_type_as(val, default), "Value and default must have same type")
    end if

    label_is_present = .false.
    labels_to_try = get_labels_with_prefix(state%compname, label)
    
    do i = 1, size(labels_to_try)
       label_to_use = trim(labels_to_try(i))
       call ESMF_ConfigFindLabel(state%cf, label = label_to_use, isPresent = label_is_present, rc = status)
       _VERIFY(status)

       if (label_is_present) then
          exit
       end if
    end do

    if (.not. label_is_present .and. .not. default_is_present) then
       if (present(rc)) rc = ESMF_FAILURE
       return
    end if

    select type(val)
    type is(integer(int32))
       if (default_is_present .and. .not. label_is_present) then
          select type(default)
          type is(integer(int32))
             val = default             
          end select
       else
          call ESMF_ConfigGetAttribute(state%cf, val, label = label_to_use, rc = status)
          _VERIFY(status)
       end if
    type is(integer(int64))
       if (default_is_present .and. .not. label_is_present) then
          select type(default)
          type is(integer(int64))
             val = default             
          end select
       else
          call ESMF_ConfigGetAttribute(state%cf, val, label = label_to_use, rc = status)
          _VERIFY(status)
       end if
    type is(real(real32))
       if (default_is_present .and. .not. label_is_present) then
          select type(default)
          type is(real(real32))
             val = default             
          end select
       else
          call ESMF_ConfigGetAttribute(state%cf, val, label = label_to_use, rc = status)
          _VERIFY(status)
       end if
    type is (real(real64))
       if (default_is_present .and. .not. label_is_present) then
          select type(default)
          type is(real(real64))
             val = default             
          end select
       else
          call ESMF_ConfigGetAttribute(state%cf, val, label = label_to_use, rc = status)
          _VERIFY(status)
       end if
    type is(character(len=*))      
       if (default_is_present .and. .not. label_is_present) then
          select type(default)
          type is(character(len=*))
             val = trim(default)
          end select
       else
          call ESMF_ConfigGetAttribute(state%cf, val, label = label_to_use, rc = status)
          _VERIFY(status)
       end if
    type is(logical)
       if (default_is_present .and. .not. label_is_present) then
          select type(default)
          type is(logical)
             val = default             
          end select
       else
          call ESMF_ConfigGetAttribute(state%cf, val, label = label_to_use, rc = status)
          _VERIFY(status)
       end if
       class default       
       _ASSERT(.false., "Unupported type")
    end select

    call ESMF_ConfigGetAttribute(state%cf, printrc, label = 'PRINTRC:', default = 0, rc = status)
    _VERIFY(status)

    ! Can set printrc to negative to not print at all
    if (MAPL_AM_I_Root() .and. printrc >= 0) then
       if (label_is_present) then
          label_to_print = label_to_use
       else
          label_to_print = trim(label)
       end if
       call print_resource(printrc, label_to_print, val, default)
    end if
    
    _RETURN(ESMF_SUCCESS)

  end subroutine MAPL_GetResource_scalar
  

  subroutine MAPL_GetResource_array(state, vals, label, default, rc)
    type(MAPL_MetaComp), intent(inout) :: state
    character(len=*), intent(in) :: label
    class(*), intent(inout) :: vals(:)
    class(*), optional, intent(in) :: default(:)
    integer, optional, intent(out) :: rc

    character(len=ESMF_MAXSTR), allocatable :: labels_to_try(:)
    character(len=:), allocatable :: label_to_use
    integer :: i, status, count
    logical :: label_is_present, default_is_present  

    default_is_present = present(default)

    if (default_is_present) then
       _ASSERT(same_type_as(vals, default), "Value and default must have same type")
    end if

    labels_to_try = get_labels_with_prefix(state%compname, label)
    label_is_present = .false.
    
    ! Try out the label variations to see which one exists in the ESMF_Config
    do i = 1, size(labels_to_try)  
       label_to_use = trim(labels_to_try(i))

       call ESMF_ConfigFindLabel(state%cf, label = label_to_use, isPresent = label_is_present, rc = status)
       _VERIFY(status)

       if (label_is_present) then
          exit
       end if
    end do

    ! No default and not in config, error
    if (.not. label_is_present .and. .not. default_is_present) then
       if (present(rc)) rc = ESMF_FAILURE
       return
    end if

    count = size(vals)

    select type(vals)
    type is(integer(int32))
       if (default_is_present .and. .not. label_is_present) then
          select type(default)
          type is(integer(int32))
             if (.not. label_is_present) vals = default
          end select
       else
          call ESMF_ConfigGetAttribute(state%cf, valuelist = vals, count = count, label = label_to_use, rc = status)
          _VERIFY(status)
       end if
    type is(integer(int64))
       if (default_is_present .and. .not. label_is_present) then
          select type(default)
          type is(integer(int64))
             vals = default
          end select
       else
          call ESMF_ConfigGetAttribute(state%cf, valuelist = vals, count = count, label = label_to_use, rc = status)
          _VERIFY(status)
       end if
    type is(real(real32))
       if (default_is_present .and. .not. label_is_present) then
          select type(default)
          type is(integer(real32))
             vals = default
          end select
       else
          call ESMF_ConfigGetAttribute(state%cf, valuelist = vals, count = count, label = label_to_use, rc = status)
          _VERIFY(status)
       end if
    type is (real(real64))
       if (default_is_present .and. .not. label_is_present) then
          select type(default)
          type is(integer(real64))
             vals = default
          end select
       else
          call ESMF_ConfigGetAttribute(state%cf, valuelist = vals, count = count, label = label_to_use, rc = status)
          _VERIFY(status)
       end if
    type is(character(len=*))
       if (default_is_present .and. .not. label_is_present) then
          select type(default)
          type is(character(*))
             vals = default
          end select
       else
          call ESMF_ConfigGetAttribute(state%cf, valuelist = vals, count = count, label = label_to_use, rc = status)
          _VERIFY(status)
       end if
    type is(logical)
       if (default_is_present .and. .not. label_is_present) then
          select type(default)
          type is(logical)
             vals = default
          end select
       else
          call ESMF_ConfigGetAttribute(state%cf, valuelist = vals, count = count, label = label_to_use, rc = status)
          _VERIFY(status)
       end if
       class default       
       _ASSERT(.false., "Unsupported type")
    end select

    _RETURN(ESMF_SUCCESS)

  end subroutine MAPL_GetResource_array

  
  subroutine print_resource(printrc, label, val, default)
    integer, intent(in) :: printrc
    character(len=*), intent(in) :: label
    class(*), intent(in) :: val
    class(*), optional, intent(in) :: default

    character(len=:), allocatable :: val_str, default_str, output_format, type_str, type_format
    type(StringVector), pointer, save :: already_printed_labels => null()

    if (.not. associated(already_printed_labels)) then
       allocate(already_printed_labels)
    end if

    ! Do not print label more than once
    if (.not. vector_contains_str(already_printed_labels, trim(label))) then
       call already_printed_labels%push_back(trim(label))
    else
       return
    end if

    select type(val)
    type is(integer(int32))
       type_str = "'Integer*4 '"
       type_format = '(i0.1)'
       val_str = intrinsic_to_string(val, type_format)
       if (present(default)) then
          default_str = intrinsic_to_string(default, type_format)
       end if
    type is(integer(int64))
       type_str = "'Integer*8 '"
       type_format = '(i0.1)'
       val_str = intrinsic_to_string(val, type_format)
       if (present(default)) then
          default_str = intrinsic_to_string(default, type_format)
       end if
    type is(real(real32))
       type_str = "'Real*4 '"
       type_format = '(f0.6)'
       val_str = intrinsic_to_string(val, type_format)
       if (present(default)) then
          default_str = intrinsic_to_string(default, type_format)
       end if
    type is(real(real64))
       type_str = "'Real*8 '"
       type_format = '(f0.6)'
       val_str = intrinsic_to_string(val, type_format)
       if (present(default)) then
          default_str = intrinsic_to_string(default, type_format)         
       end if
    type is(logical)
       type_str = "'Logical '"
       type_format = '(l1)'
       val_str = intrinsic_to_string(val, type_format)
       if (present(default)) then
          default_str = intrinsic_to_string(default, type_format)
       end if
    type is(character(len=*))
       type_str = "'Character '"
       val_str = trim(val)
       if (present(default)) then
          default_str = intrinsic_to_string(default, 'a')
       end if
    end select

    output_format = "(1x, " // type_str // ", 'Resource Parameter: '" // ", a"// ", a)"

    ! printrc = 0 - Only print non-default values
    ! printrc = 1 - Print all values
    if (present(default)) then
       if (trim(val_str) /= trim(default_str) .or. printrc == 1) then
          print output_format, trim(label), trim(val_str)
       end if
    else
       print output_format, trim(label), trim(val_str)
    end if

  contains

    logical function vector_contains_str(vector, string)
      type(StringVector), intent(in) :: vector
      character(len=*), intent(in) :: string
      type(StringVectorIterator) :: iter

      iter = vector%begin()

      vector_contains_str = .false.

      if (vector%size() /= 0) then
         do while (iter /= vector%end())
            if (trim(string) == iter%get()) then
               vector_contains_str = .true.
               return
            end if
            call iter%next()
         end do
      end if

    end function vector_contains_str

  end subroutine print_resource


  function intrinsic_to_string(val, str_format, rc) result(formatted_str)    
    class(*), intent(in) :: val
    character(len=*), intent(in) :: str_format
    character(len=256) :: formatted_str
    integer, optional, intent(out) :: rc

    select type(val)
    type is(integer(int32))
       write(formatted_str, str_format) val
    type is(integer(int64))
       write(formatted_str, str_format) val
    type is(real(real32))
       write(formatted_str, str_format) val
    type is(real(real64))
       write(formatted_str, str_format) val
    type is(logical)
       write(formatted_str, str_format) val
    type is(character(len=*))
       formatted_str = trim(val)
    class default
       _ASSERT(.false., "Unsupported type in intrinsic_to_string")
    end select

  end function intrinsic_to_string



 integer function MAPL_GetNumSubtiles(STATE, RC)
    type (MAPL_MetaComp),       intent(INOUT)    :: STATE
    integer  , optional,        intent(  OUT)    :: RC

    character(len=ESMF_MAXSTR), parameter :: IAm="MAPL_GetNumSubtiles"
    integer                               :: STATUS
    integer                               :: I
    integer                               :: DIMS
    integer                               :: NUM_SUBTILES

    MAPL_GetNumSubtiles = 1
    if (associated(STATE%COMPONENT_SPEC%INTERNAL%OLD_VAR_SPECS)) then
       DO I = 1, size(STATE%COMPONENT_SPEC%INTERNAL%OLD_VAR_SPECS)
          call MAPL_VarSpecGet(STATE%COMPONENT_SPEC%INTERNAL%OLD_VAR_SPECS(I), DIMS = DIMS,       &
                 NUM_SUBTILES=NUM_SUBTILES,                 &
                 RC=STATUS  )
          _VERIFY(STATUS)
          if (DIMS == MAPL_DimsTileTile) then
             MAPL_GetNumSubtiles = NUM_SUBTILES
             _RETURN(ESMF_SUCCESS)
          end if
       END DO
    end if
    if (associated(STATE%COMPONENT_SPEC%IMPORT%OLD_VAR_SPECS)) then
       DO I = 1, size(STATE%COMPONENT_SPEC%IMPORT%OLD_VAR_SPECS)
          call MAPL_VarSpecGet(STATE%COMPONENT_SPEC%IMPORT%OLD_VAR_SPECS(I), DIMS = DIMS,       &
                 NUM_SUBTILES=NUM_SUBTILES,                 &
                 RC=STATUS  )
          _VERIFY(STATUS)
          if (DIMS == MAPL_DimsTileTile) then
             MAPL_GetNumSubtiles = NUM_SUBTILES
             _RETURN(ESMF_SUCCESS)
          end if
       END DO
    end if
    _RETURN(ESMF_SUCCESS)
  end function MAPL_GetNumSubtiles


  recursive subroutine MAPL_AdjustIsNeeded ( GC, EXPORT, RC )

! !ARGUMENTS:

    type(ESMF_GridComp), intent(INOUT) :: GC     ! Gridded component 
    type(ESMF_State),    intent(INOUT) :: EXPORT ! Export state
    integer, optional,   intent(  OUT) :: RC     ! Error code:

!EOPI

! ErrLog Variables

    character(len=ESMF_MAXSTR)    :: IAm
    character(len=ESMF_MAXSTR)    :: COMP_NAME
    integer                       :: STATUS

! Local derived type aliases

    type (MAPL_MetaComp),pointer     :: STATE 
    integer                          :: I
    integer                          :: ITEMCOUNT
    type (ESMF_StateItem_Flag), pointer :: ITEMTYPES(:)
    character(len=ESMF_MAXSTR ), pointer  :: ITEMNAMES(:)
    type(ESMF_GridComp), pointer :: gridcomp
    type(ESMF_State), pointer :: child_export_state

    Iam = "MAPL_AdjustIsNeeded"
    call ESMF_GridCompGet( GC, NAME=COMP_NAME, RC=STATUS )
    _VERIFY(STATUS)
    Iam = trim(COMP_NAME) // trim(Iam)

! Retrieve the pointer to the internal state.
! -------------------------------------------

    call MAPL_InternalStateGet ( GC, STATE, RC=STATUS)
    _VERIFY(STATUS)

    call ESMF_StateGet(EXPORT,ITEMCOUNT=ITEMCOUNT,RC=STATUS)
    _VERIFY(STATUS)

    IF (ITEMCOUNT>0) then

       allocate(ITEMNAMES(ITEMCOUNT),STAT=STATUS)
       _VERIFY(STATUS)
       allocate(ITEMTYPES(ITEMCOUNT),STAT=STATUS)
       _VERIFY(STATUS)

       call ESMF_StateGet(EXPORT,ITEMNAMELIST=ITEMNAMES,ITEMTYPELIST=ITEMTYPES,RC=STATUS)
       _VERIFY(STATUS)

       deallocate(ITEMNAMES)
       deallocate(ITEMTYPES)
    end IF

       do I = 1, STATE%get_num_children()
          gridcomp => STATE%GET_CHILD_GRIDCOMP(I)
          child_export_state => state%get_child_export_state(i)
          call MAPL_AdjustIsNeeded(gridcomp, child_export_state, RC=STATUS)
          _VERIFY(STATUS)
       end do

    _RETURN(ESMF_SUCCESS)
  end subroutine MAPL_AdjustIsNeeded

!BOPI

! !INTERFACE:

  logical function MAPL_IsFieldAllocated(FIELD, RC)

! !ARGUMENTS:

    type(ESMF_Field),    intent(INout) :: FIELD  ! Field
    integer, optional,   intent(  OUT) :: RC     ! Error code:

! !DESCRIPTION:  Shortcut for checking that field is allocated
 
!EOPI

! ErrLog Variables

    character(len=ESMF_MAXSTR), parameter :: IAm='MAPL_IsFieldAllocated'
    integer                               :: STATUS
    
    real(kind=REAL32), dimension(:)        , pointer :: r4d1
    real(kind=REAL32), dimension(:,:)      , pointer :: r4d2
    real(kind=REAL32), dimension(:,:,:)    , pointer :: r4d3
    real(kind=REAL32), dimension(:,:,:,:)  , pointer :: r4d4

    type(ESMF_Array)                        :: array
    type(ESMF_TypeKind_Flag)                :: tk
    integer                                 :: rank
    type(ESMF_FieldStatus_Flag)             :: fieldStatus

    call ESMF_FieldGet(field, status=fieldStatus, rc=status)
    _VERIFY(STATUS)

    if(fieldStatus /= ESMF_FIELDSTATUS_COMPLETE) then
       MAPL_IsFieldAllocated = .false.
    else
       call ESMF_FieldGet (FIELD, Array=ARRAY, RC=STATUS)
       _VERIFY(STATUS)

       MAPL_IsFieldAllocated = .true.
       
       call ESMF_ArrayGet(array, typekind=tk, rank=rank, rc=status)
       if (tk .eq. ESMF_TYPEKIND_R4) then
          if (rank .eq. 1) then
             call ESMF_ArrayGet(array, localDE=0, farrayptr=r4d1, rc=status)
             _VERIFY(STATUS)
             if (.not. associated(r4d1)) then
                MAPL_IsFieldAllocated = .false.
                _RETURN(ESMF_SUCCESS)
             endif
          else if (rank .eq. 2) then
             call ESMF_ArrayGet(array, localDE=0, farrayptr=r4d2, rc=status)
             _VERIFY(STATUS)
             if (.not. associated(r4d2)) then
                MAPL_IsFieldAllocated = .false.
                _RETURN(ESMF_SUCCESS)
             endif
          else if (rank .eq. 3) then
             call ESMF_ArrayGet(array, localDE=0, farrayptr=r4d3, rc=status)
             _VERIFY(STATUS)
             if (.not. associated(r4d3)) then
                MAPL_IsFieldAllocated = .false.
                _RETURN(ESMF_SUCCESS)
             endif
          else if (rank .eq. 4) then
             call ESMF_ArrayGet(array, localDE=0, farrayptr=r4d4, rc=status)
             _VERIFY(STATUS)
             if (.not. associated(r4d4)) then
                MAPL_IsFieldAllocated = .false.
                _RETURN(ESMF_SUCCESS)
             endif
          else
             _RETURN(ESMF_FAILURE)
          end if
       end if
    end if

    _RETURN(ESMF_SUCCESS)
  end function MAPL_IsFieldAllocated

  subroutine MAPL_ExchangeGridGet ( GC, EXCH, RC )

! !ARGUMENTS:

    type(ESMF_GridComp), intent(INout) :: GC     ! Gridded component 
    type(MAPL_LocStream),intent(INOUT) :: EXCH   ! Exchange grid
    integer, optional,   intent(  OUT) :: RC     ! Error code:

!EOPI

! ErrLog Variables

    character(len=ESMF_MAXSTR)    :: IAm
    character(len=ESMF_MAXSTR)    :: COMP_NAME
    integer                       :: STATUS

! Local derived type aliases

    type (MAPL_MetaComp),pointer     :: STATE 

    Iam = "MAPL_ExchangeGridGet"
    call ESMF_GridCompGet( GC, NAME=COMP_NAME, RC=STATUS )
    _VERIFY(STATUS)
    Iam = trim(COMP_NAME) // trim(Iam)

! Retrieve the pointer to the internal state.
! -------------------------------------------

    call MAPL_InternalStateGet ( GC, STATE, RC=STATUS)
    _VERIFY(STATUS)

    EXCH = STATE%EXCHANGEGRID

    _RETURN(ESMF_SUCCESS)
  end subroutine MAPL_ExchangeGridGet
    
  recursive subroutine MAPL_ExchangeGridSet ( GC, EXCH, RC )

! !ARGUMENTS:

    type(ESMF_GridComp), intent(INOUT) :: GC     ! Gridded component 
    type(MAPL_LocStream),intent(IN   ) :: EXCH   ! Exchange grid
    integer, optional,   intent(  OUT) :: RC     ! Error code:

!EOPI

! ErrLog Variables

    character(len=ESMF_MAXSTR)    :: IAm
    character(len=ESMF_MAXSTR)    :: COMP_NAME
    integer                       :: STATUS

! Local derived type aliases

    type (MAPL_MetaComp),pointer     :: STATE 
    integer                          :: I
    type(ESMF_GridComp), pointer :: gridcomp
    Iam = "MAPL_ExchangeGridSet"
    call ESMF_GridCompGet( GC, NAME=COMP_NAME, RC=STATUS )
    _VERIFY(STATUS)
    Iam = trim(COMP_NAME) // trim(Iam)

! Retrieve the pointer to the internal state.
! -------------------------------------------

    call MAPL_InternalStateGet ( GC, STATE, RC=STATUS)
    _VERIFY(STATUS)

    STATE%EXCHANGEGRID=EXCH

       do I = 1, STATE%get_num_children()
          gridcomp => STATE%GET_CHILD_GRIDCOMP(I)
          call MAPL_ExchangeGridSet(gridcomp, exch, RC=STATUS)
          _VERIFY(STATUS)
       end do

    _RETURN(ESMF_SUCCESS)
  end subroutine MAPL_ExchangeGridSet
   
  recursive subroutine MAPL_GCGet(GC,name,result,rc)
    type(ESMF_GridComp), intent(inout) :: GC
    character(len=*)   , intent(in   ) :: name
    type(ESMF_GridComp), intent(inout) :: result
    integer, optional  , intent(  out) :: rc

    character(len=ESMF_MAXSTR), parameter :: IAm = "MAPL_GCGet"
    integer                               :: status
    character(len=ESMF_MAXSTR)            :: comp_name
    type(MAPL_MetaComp), pointer          :: state
    type(ESMF_VM)                         :: vm
    integer                               :: i
    type(ESMF_GridComp), pointer :: gridcomp

    call ESMF_GridCompGet(GC,name=comp_name,vm=vm,rc=status)
    _VERIFY(STATUS)
    if (trim(comp_name) == trim(name)) then
       result = GC
       _RETURN(ESMF_SUCCESS)
    end if

    call MAPL_InternalStateGet ( GC, STATE, RC=STATUS)
    _VERIFY(STATUS)
       do I = 1, STATE%get_num_children()
          gridcomp => STATE%GET_CHILD_GRIDCOMP(I)
          call MAPL_GCGet(gridcomp,name,result,rc=status)
          if (status==ESMF_SUCCESS) then
             _RETURN(ESMF_SUCCESS)
          end if
       enddo

    _RETURN(ESMF_FAILURE)
    return

  end subroutine MAPL_GCGet
 
  recursive subroutine MAPL_ExportStateGet(export, name, result, rc)
    type (ESMF_State), intent(IN   ) :: export(:)
    character(len=*),  intent(IN   ) :: name
    type (ESMF_State), intent(  OUT) :: result
    integer,           intent(  OUT) :: rc
    
    character(len=ESMF_MAXSTR), parameter :: IAm="MAPL_ExportStateGet"
    integer                               :: status 
    
    integer                               :: n, i, ni, k, j
    logical                               :: have_ens
    character(len=ESMF_MAXSTR)            :: sname
    type (ESMF_StateItem_Flag), pointer   :: ITEMTYPES(:)
    character(len=ESMF_MAXSTR ), pointer  :: ITEMNAMES(:)
    type (ESMF_State), pointer            :: exptmp(:)
   
    n = size(export)
    
    have_ens = index(name,":") /= 0

    do i = 1, n
       call ESMF_StateGet(export(i), name=sname, itemcount = ni, rc=status)
       _VERIFY(STATUS)
       if (have_ens) then
          if (sname == trim(name) // '_Exports') then
             result = export(i)
             _RETURN(ESMF_SUCCESS)
          end if
       else
          if (sname(index(sname,":")+1:) == trim(name) // '_Exports') then
             result = export(i)
             _RETURN(ESMF_SUCCESS)
          end if
       end if
       
       allocate(itemtypes(ni), itemnames(ni), stat=status)
       _VERIFY(STATUS)
       
       call ESMF_StateGet(export(i), ITEMNAMELIST=ITEMNAMES,ITEMTYPELIST=ITEMTYPES,RC=STATUS)
       _VERIFY(STATUS)
      
       j = 0
       do k = 1, ni
          if (itemtypes(k) == ESMF_StateItem_State) then
             j = j+1
          end if
       end do
      
       allocate(exptmp(j), stat=status)
       _VERIFY(STATUS)
      
       j = 0
       do k = 1, ni
          if (itemtypes(k) == ESMF_StateItem_State) then
             j = j+1
             call ESMF_StateGet(export(i), itemnames(k), exptmp(j) , rc=status)
             _VERIFY(STATUS)
          end if
       end do
      
       call MAPL_ExportStateGet(exptmp, name, result, rc=status)
       deallocate(exptmp)
       deallocate(itemtypes, itemnames)
       if (status == ESMF_SUCCESS) then
          _RETURN(ESMF_SUCCESS)
       end if              
   end do

   rc = ESMF_FAILURE
   return
 end subroutine MAPL_ExportStateGet

 recursive subroutine MAPL_ImportStateGet ( GC, import, name, result, rc )
    type(ESMF_GridComp), intent(INout)  :: GC
    type(ESMF_State),    intent(IN)  :: import
    character(len=*),  intent(IN   ) :: name
    type (ESMF_State), intent(  OUT) :: result
    integer,           intent(  OUT) :: rc
    
    character(len=ESMF_MAXSTR), parameter :: IAm="MAPL_ImportStateGet"
    integer                               :: status 
    integer                               :: I
    logical                               :: have_ens
    character(len=ESMF_MAXSTR)            :: sname
    type (MAPL_MetaComp),pointer          :: STATE 
    type(ESMF_GridComp), pointer :: gridcomp
    type(ESMF_State), pointer :: child_import_state

    have_ens = index(name,":") /= 0
    call ESMF_StateGet(import, name=sname, rc=status)
    _VERIFY(STATUS)
    if (have_ens) then
       if (sname == trim(name) // '_Imports') then
          result = import
          _RETURN(ESMF_SUCCESS)
       end if
    else
       if (sname(index(sname,":")+1:) == trim(name) // '_Imports') then
          result = import
          _RETURN(ESMF_SUCCESS)
       end if
    end if

    call MAPL_InternalStateGet ( GC, STATE, RC=STATUS)
    _VERIFY(STATUS)

       do I = 1, STATE%get_num_children()
          gridcomp => STATE%GET_CHILD_GRIDCOMP(I)
          child_import_state => STATE%get_child_import_state(i)
          call MAPL_ImportStateGet(gridcomp, child_import_state, name, result, RC=STATUS)
          if (status == ESMF_SUCCESS) then
             _RETURN(ESMF_SUCCESS)
          end if
       end do

   rc = ESMF_FAILURE
   return
 end subroutine MAPL_ImportStateGet

 recursive subroutine MAPL_InternalESMFStateGet ( GC, name, result, rc )
    type(ESMF_GridComp), intent(INout)  :: GC
    character(len=*),  intent(IN   ) :: name
    type (ESMF_State), intent(  OUT) :: result
    integer,           intent(  OUT) :: rc
    
    character(len=ESMF_MAXSTR), parameter :: IAm="MAPL_InternalESMFStateGet"
    integer                               :: status 
    integer                               :: I
    logical                               :: have_ens
    character(len=ESMF_MAXSTR)            :: sname
    type (MAPL_MetaComp),pointer          :: STATE 
    type(ESMF_State)                      :: internal
    type(ESMF_GridComp), pointer :: gridcomp

    have_ens = index(name,":") /= 0
   
    call MAPL_InternalStateGet ( GC, STATE, RC=STATUS)
    _VERIFY(STATUS)
    call MAPL_Get(STATE,INTERNAL_ESMF_STATE=internal,rc=status)
    _VERIFY(status)
    call ESMF_StateGet(internal, name=sname, rc=status)
    _VERIFY(STATUS)
    if (have_ens) then
       if (sname == trim(name) // '_INTERNAL') then
          result = internal
          _RETURN(ESMF_SUCCESS)
       end if
    else
       if (sname(index(sname,":")+1:) == trim(name) // '_INTERNAL') then
          result = internal
          _RETURN(ESMF_SUCCESS)
       end if
    end if

       do I = 1, STATE%get_num_children()
          gridcomp => STATE%GET_CHILD_GRIDCOMP(I)
          call MAPL_InternalESMFStateGet(gridcomp, name, result, RC=STATUS)
          if (status == ESMF_SUCCESS) then
             _RETURN(ESMF_SUCCESS)
          end if
       end do
   rc = ESMF_FAILURE
   return
 end subroutine MAPL_InternalESMFStateGet

 recursive subroutine MAPL_GetChildLocstream(GC, result, name, rc)
    type(ESMF_GridComp),   intent(INout) :: GC
    type (MAPL_LocStream), intent(  OUT) :: result
    character(len=*),      intent(IN   ) :: name
    integer,               intent(  OUT) :: rc
    
    character(len=ESMF_MAXSTR), parameter :: IAm="MAPL_GetChildLocstream"
    integer                               :: status 
    integer                               :: I
    logical                               :: have_ens
    character(len=ESMF_MAXSTR)            :: comp_name
    type (MAPL_MetaComp),pointer          :: STATE 
    type(ESMF_GridComp), pointer :: gridcomp

    call ESMF_GridCompGet(GC, name=comp_name, rc=status)
    _VERIFY(STATUS)

    call MAPL_InternalStateGet ( GC, STATE, RC=STATUS)
    _VERIFY(STATUS)

    have_ens = index(name,":") /= 0
    if (have_ens) then ! the name contains ensemble #
       if (name == comp_name) then
          result = state%locstream
          _RETURN(STATUS)
       end if
    else
       if (name == comp_name(index(comp_name,":")+1:)) then
          result = state%locstream
          _RETURN(STATUS)
       end if
    end if


       do I = 1, STATE%get_num_children()
          gridcomp => STATE%GET_CHILD_GRIDCOMP(I)
          call MAPL_GetChildLocstream(gridcomp, result, name, rc=STATUS)
          if (status == ESMF_SUCCESS) then
             _RETURN(ESMF_SUCCESS)
          end if
       end do

    rc = ESMF_FAILURE
    return
  end subroutine MAPL_GetChildLocstream


  function MAPL_VerifyFriendlyInField(FIELD,FRIEND2COMP,RC) result(FRIENDLY)
    type (ESMF_field),          intent(INout)    :: FIELD
    character(len=*),           intent(IN)    :: FRIEND2COMP
    integer  , optional,        intent(OUT)   :: RC
    logical                                   :: FRIENDLY

    character(len=ESMF_MAXSTR), parameter :: IAm="MAPL_VerifyFriendlyField"
    integer                               :: STATUS
    logical                               :: isPresent

    call ESMF_AttributeGet  (FIELD, NAME="FriendlyTo"//trim(FRIEND2COMP), &
                                        isPresent=isPresent, RC=STATUS)
    _VERIFY(STATUS)
    if(isPresent) then
       call ESMF_AttributeGet  (FIELD, NAME="FriendlyTo"//trim(FRIEND2COMP), &
                                        VALUE=FRIENDLY, RC=STATUS)
       _VERIFY(STATUS)
    else
       FRIENDLY = .false.
    end if

    _RETURN(ESMF_SUCCESS)

  end function MAPL_VerifyFriendlyInField

  function MAPL_VerifyFriendlyInState(STATE,NAME,FRIEND2COMP,RC) result(FRIENDLY)
    type (ESMF_State),          intent(IN)    :: STATE
    character(len=*),           intent(IN)    :: NAME
    character(len=*),           intent(IN)    :: FRIEND2COMP
    integer  , optional,        intent(OUT)   :: RC
    logical                                   :: FRIENDLY

    character(len=ESMF_MAXSTR), parameter :: IAm="MAPL_VerifyFriendlyState"
    integer                               :: STATUS
    type (ESMF_field)                     :: FIELD

    call ESMF_StateGet(STATE,NAME,FIELD,RC=STATUS)
    _VERIFY(STATUS)
    FRIENDLY=MAPL_VerifyFriendly(FIELD,FRIEND2COMP,RC=STATUS)
    _VERIFY(STATUS)

    _RETURN(ESMF_SUCCESS)

  end function MAPL_VerifyFriendlyInState

!==================================================================


  !BOPI
  ! !IROUTINE: MAPL_ReadForcing
  ! !IIROUTINE: MAPL_ReadForcing1 --- 1

  !INTERFACE:
  subroutine MAPL_ReadForcing1(STATE,NAME,DATAFILE,CURRENTTIME,    &
                               FORCING,INIT_ONLY,ON_TILES,RC )

    !ARGUMENTS:
    type (MAPL_MetaComp),     intent(INOUT)   :: STATE
    character(len=*),         intent(IN   )   :: NAME
    character(len=*),         intent(IN   )   :: DATAFILE
    type (ESMF_Time),         intent(INout)   :: CURRENTTIME
    real,                     intent(  OUT)   :: FORCING(:)   
    logical, optional,        intent(IN   )   :: INIT_ONLY
    logical, optional,        intent(IN   )   :: ON_TILES
    integer, optional,        intent(  OUT)   :: RC
    !EOPI

    integer                           :: STATUS
    
    call MAPL_ReadForcingX(STATE,NAME,DATAFILE,CURRENTTIME,      &
         FORCING1=FORCING,INIT_ONLY=INIT_ONLY, &
         ON_TILES=ON_TILES,                    &
         RC=STATUS )
    
    _RETURN(STATUS)
  end subroutine MAPL_ReadForcing1

!==================================================================

  !BOPI
  ! !IIROUTINE: MAPL_ReadForcing2 --- 2
  
  !INTERFACE:
  subroutine MAPL_ReadForcing2(STATE,NAME,DATAFILE,CURRENTTIME,    &
                               FORCING,INIT_ONLY,RC )

    !ARGUMENTS:
    type (MAPL_MetaComp),     intent(INOUT)   :: STATE
    character(len=*),         intent(IN   )   :: NAME
    character(len=*),         intent(IN   )   :: DATAFILE
    type (ESMF_Time),         intent(INout)   :: CURRENTTIME
    real,                     intent(  OUT)   :: FORCING(:,:)   
    logical, optional,        intent(IN   )   :: INIT_ONLY
    integer, optional,        intent(  OUT)   :: RC
    !EOPI

   integer                           :: STATUS

   call MAPL_ReadForcingX(STATE,NAME,DATAFILE,CURRENTTIME,      &
                          FORCING2=FORCING,INIT_ONLY=INIT_ONLY, &
                          ON_TILES=.FALSE.,                     &
                          RC=STATUS )
   _RETURN(STATUS)

end subroutine MAPL_ReadForcing2

!==================================================================

subroutine MAPL_ReadForcingX(MPL,NAME,DATAFILE,CURRTIME,  &
                             FORCING1,FORCING2,INIT_ONLY,ON_TILES,RC )

   type (MAPL_MetaComp),     intent(INOUT)   :: MPL
   character(len=*),         intent(IN   )   :: NAME
   character(len=*),         intent(IN   )   :: DATAFILE
   type (ESMF_Time),         intent(INout)   :: CURRTIME
   real, optional,           intent(  OUT)   :: FORCING1(:)   
   real, optional,           intent(  OUT)   :: FORCING2(:,:)   
   logical, optional,        intent(IN   )   :: INIT_ONLY
   logical, optional,        intent(IN   )   :: ON_TILES
   integer, optional,        intent(  OUT)   :: RC

! ErrLog Variables

   integer                           :: STATUS

! Locals

   type (MAPL_LocStream)             :: LOCSTREAM
   type (ESMF_Time)                  :: DATE1
   type (ESMF_Time)                  :: DATEN
   type (ESMF_Calendar)              :: CAL
   type (ESMF_DELayout)              :: LAYOUT
   type (ESMF_Time)                  :: MIDT1
   type (ESMF_Time)                  :: MIDT2
   type (ESMF_Time)                  :: CURRENTTIME
   type (ESMF_Grid)                  :: GRID
   type (ESMF_Field)                 :: FIELD
   real                              :: FAC
   real, pointer                     :: PRE1(:  ), NEX1(:  )
   real, pointer                     :: PRE2(:,:), NEX2(:,:)
   real, pointer                     :: LONS(:,:), VAR2(:,:)
   type(ESMF_TimeInterval)           :: TIMEDIFF
   integer, parameter                :: FILE_HEADER_SIZE=14
   real                              :: REAL_HEADER(FILE_HEADER_SIZE)
   integer                           :: HEADER(FILE_HEADER_SIZE)
   integer                           :: UNIT
   integer                           :: DATE_PREV, YEAR
   logical                           :: USE_INIT_ONLY
   logical                           :: USE_ON_TILES
   logical                           :: TRANSFORM
   logical                           :: ONED
   logical                           :: CLIMATOLOGY
   integer                           :: YY, MM, DD
   integer                           :: H, M, S
   integer                           :: NSUBTILES
   integer                           :: L1, L2
   type(ESMF_Grid)                   :: TILEGRID
   integer                           :: datetime(2)
   logical                           :: FileExists
   logical                           :: AM_I_Root_
   character(len=ESMF_MAXSTR)        :: FNAME
   type (ESMF_StateItem_Flag)        :: itemType

! Process arguments
!------------------

   if(present(INIT_ONLY)) then
      USE_INIT_ONLY = INIT_ONLY
   else
      USE_INIT_ONLY = .false.
   end if

   if(present(ON_TILES)) then
      USE_ON_TILES = ON_TILES
   else
      USE_ON_TILES = .false.
   end if

   if    (present(FORCING1)) then
      ONED = .TRUE.
   elseif(present(FORCING2)) then
      ONED = .FALSE.
   else
      _ASSERT(.FALSE.,'needs informative message')
   end if

! Get parameters from generic state.
!-----------------------------------

   call MAPL_GenericStateGet(MPL,             &
        LOCSTREAM=LOCSTREAM,                    &
        LONS=LONS,                              &
        LAYOUT=LAYOUT,                          &
                                      RC=STATUS )
   _VERIFY(STATUS)

! Get the calendar from the current time
!---------------------------------------

   call ESMF_TimeGet(CurrTime, calendar=cal, rc=status)
   _VERIFY(STATUS)

! Scratch space for reading data files
!-------------------------------------

    TRANSFORM = ONED .and. .not.USE_ON_TILES

    if(TRANSFORM) then
       allocate(VAR2(size(LONS,1),size(LONS,2)),STAT=STATUS)
       _VERIFY(STATUS)
    end if

! Get the grid form generic state 
!-------------------------------------------------------

   GRID=MPL%GRID%ESMFGRID

! Check if FRCSTATE already has previous and next vars. If not, create them
!---------------------------------------------------------------------------
   call ESMF_StateGet(MPL%FORCING, trim(NAME)//'_PREV', itemType=itemType, RC=STATUS)
   _VERIFY(STATUS)

   if (itemType == ESMF_STATEITEM_FIELD) then
      call ESMF_StateGet(MPL%FORCING, trim(NAME)//'_PREV', FIELD, RC=STATUS)
      _VERIFY(STATUS)
   else
      if(ONED) then
         NSUBTILES = MAPL_GetNumSubtiles(MPL, RC=STATUS)
         _VERIFY(STATUS)
         call MAPL_LocStreamGet(MPL%LocStream, TILEGRID=TILEGRID, RC=STATUS)
         _VERIFY(STATUS)
         
         ! create a spec
         call MAPL_VarSpecCreateInListNew(MPL%COMPONENT_SPEC%FORCING,                   &
              SHORT_NAME = trim(NAME) // '_PREV',                       &
              LONG_NAME  = 'previous value of ' // trim(NAME),          &
              NUM_SUBTILES=NSUBTILES,                                   &
              DIMS       = MAPL_DimsTileOnly,                           &
              VLOCATION  = MAPL_VLocationNone,                          &
              COUPLE_INTERVAL = -1,                                     & ! time not set
              RC=STATUS  )
         _VERIFY(STATUS)
         
         call MAPL_VarSpecCreateInListNew(MPL%COMPONENT_SPEC%FORCING,                   &
              SHORT_NAME = trim(NAME) // '_NEXT',                       &
              LONG_NAME  = 'next value of ' // trim(NAME),              &
              NUM_SUBTILES=NSUBTILES,                                   &
              DIMS       = MAPL_DimsTileOnly,                           &
              VLOCATION  = MAPL_VLocationNone,                          &
              COUPLE_INTERVAL = -1,                                     & ! time not set
              RC=STATUS  )
         _VERIFY(STATUS)

         L1 = MAPL_VarSpecGetIndex(MPL%COMPONENT_SPEC%FORCING%OLD_VAR_SPECS,trim(NAME)//'_PREV')
         L2 = MAPL_VarSpecGetIndex(MPL%COMPONENT_SPEC%FORCING%OLD_VAR_SPECS,trim(NAME)//'_NEXT')
         _ASSERT(L2==L1+1,'needs informative message')

         ! create field and put it in FRCSTATE
         call MAPL_StateCreateFromVarSpecNew(MPL%FORCING,MPL%COMPONENT_SPEC%FORCING, &
              GRID,TILEGRID=TILEGRID,range=[L1,L2],RC=STATUS)
         _VERIFY(STATUS)

      else
         call MAPL_VarSpecCreateInListNew(MPL%COMPONENT_SPEC%FORCING,                   &
              SHORT_NAME = trim(NAME) // '_PREV',                       &
              LONG_NAME  = 'previous value of ' // trim(NAME),          &
              DIMS       = MAPL_DimsHorzOnly,                           &
              VLOCATION  = MAPL_VLocationNone,                          &
              COUPLE_INTERVAL = -1,                                     & ! time not set
              RC=STATUS  )
         _VERIFY(STATUS)
         
         call MAPL_VarSpecCreateInListNew(MPL%COMPONENT_SPEC%FORCING,                   &
              SHORT_NAME = trim(NAME) // '_NEXT',                       &
              LONG_NAME  = 'next value of ' // trim(NAME),              &
              DIMS       = MAPL_DimsHorzOnly,                           &
              VLOCATION  = MAPL_VLocationNone,                          &
              COUPLE_INTERVAL = -1,                                     & ! time not set
              RC=STATUS  )
         _VERIFY(STATUS)

         L1 = MAPL_VarSpecGetIndex(MPL%COMPONENT_SPEC%FORCING%OLD_VAR_SPECS,trim(NAME)//'_PREV')
         L2 = MAPL_VarSpecGetIndex(MPL%COMPONENT_SPEC%FORCING%OLD_VAR_SPECS,trim(NAME)//'_NEXT')
         _ASSERT(L2==L1+1,'needs informative message')

            ! create field and put it in FRCSTATE
         call MAPL_StateCreateFromVarSpecNew(MPL%FORCING,MPL%COMPONENT_SPEC%FORCING, &
              GRID, range=[L1,L2],RC=STATUS)
         _VERIFY(STATUS)

      end if
   end if

! Get pointers to the endpoints in the forcing state
!----------------------------------------------------

   if(ONED) then
      call GET_POINTER(MPL%FORCING, PRE1, trim(NAME)//'_PREV', RC=STATUS)
      _VERIFY(STATUS)
      call GET_POINTER(MPL%FORCING, NEX1, trim(NAME)//'_NEXT', RC=STATUS)
      _VERIFY(STATUS)
   else
      call GET_POINTER(MPL%FORCING, PRE2, trim(NAME)//'_PREV', RC=STATUS)
      _VERIFY(STATUS)
      call GET_POINTER(MPL%FORCING, NEX2, trim(NAME)//'_NEXT', RC=STATUS)
      _VERIFY(STATUS)
   end if

! Set time endpoints. These are 0000-01-01:000000 if uninitialized
!----------------------------------------------------

   call MAPL_StateGetTimeStamp(MPL,trim(NAME)//'_NEXT',MIDT2, RC=STATUS )
   _VERIFY(STATUS)
   call MAPL_StateGetTimeStamp(MPL,trim(NAME)//'_PREV',MIDT1, RC=STATUS )
   _VERIFY(STATUS)

! Check if the input file is a climatology
!-----------------------------------------

   call MAPL_StateGetSpecAttrib(MPL,trim(NAME)//'_PREV',forcing=.true., &
        REFRESH_INTERVAL=DATE_PREV, rc=STATUS)
   _VERIFY(STATUS)

   if ( DATE_PREV < 0 ) then
      UNIT=GETFILE(DATAFILE,form='unformatted', rc=status)
      _VERIFY(STATUS)
      call READ_PARALLEL(LAYOUT, REAL_HEADER, unit=UNIT, rc=status)
      _VERIFY(STATUS)
      HEADER = nint(REAL_HEADER)
      call MAPL_BACKSPACE(UNIT,LAYOUT,RC=STATUS)
      _VERIFY(STATUS)
      CLIMATOLOGY = HEADER(1)<3
   else
      call ESMF_TimeGet(MIDT1, YY=YEAR, rc=status)
      _VERIFY(STATUS)
      CLIMATOLOGY = YEAR < 3
   end if

! Make a local copy of the current time
!--------------------------------------

!ALT clock are shallow objects!!!    CurrentTime = CurrTime
      call ESMF_TimeGet(CurrTime, &
                        YY=YY, MM=MM, DD=DD, &
                        H=H, M=M, S=S, rc=status)
      _VERIFY(STATUS)

   if(CLIMATOLOGY) then

      YY = 1

!ALT: In Climatology mode we should not have leap year
!     so if the date is Feb.29, move it to Feb.28

      if (MM==2 .and. DD==29) DD=28

   end if

   call ESMF_TimeSet(CurrentTime, &
                     YY=YY, MM=MM, DD=DD, &
                     H=H, M=M, S=S, rc=status)
   _VERIFY(STATUS)

   AM_I_Root_ = MAPL_AM_I_Root(layout)

! Make sure current time is between the current endpoints;
!  if not, refresh endpoints. This also works initially.
!-------------------------------------------------------

   if (CurrentTime < MIDT1 .or. CurrentTime > MIDT2) then
      call UPDATE_ENDPOINTS
      _VERIFY(STATUS)
   endif

! Interpolate ocean and ice temp and ice fraction
!   and update the forcing state on the tile grid.
!--------------------------------------------------

   if(USE_INIT_ONLY) then
      FAC=1.0
   else
      call MAPL_Interp_Fac (CurrentTime,MIDT1,MIDT2,FAC,RC=STATUS )
      _VERIFY(STATUS)
   end if

   _ASSERT(FAC >= 0.0,'needs informative message')
   _ASSERT(FAC <= 1.0,'needs informative message')

!  Update the friendly skin values
!---------------------------------

   if(ONED) then
      FORCING1 = MAPL_UNDEF
      where(PRE1/=MAPL_UNDEF .and. NEX1/=MAPL_UNDEF)
         FORCING1 = FAC * PRE1  +  (1.0-FAC) * NEX1
      end where
      where(PRE1==MAPL_UNDEF .and. NEX1/=MAPL_UNDEF)
         FORCING1 = NEX1
      end where
      where(PRE1/=MAPL_UNDEF .and. NEX1==MAPL_UNDEF)
         FORCING1 = PRE1
      end where
   else
      FORCING2 = MAPL_UNDEF
      where(PRE2/=MAPL_UNDEF .and. NEX2/=MAPL_UNDEF)
         FORCING2 = FAC * PRE2  +  (1.0-FAC) * NEX2
      end where
      where(PRE2==MAPL_UNDEF .and. NEX2/=MAPL_UNDEF)
         FORCING2 = NEX2
      end where
      where(PRE2/=MAPL_UNDEF .and. NEX2==MAPL_UNDEF)
         FORCING2 = PRE2
      end where
   end if

   if(TRANSFORM) deallocate(VAR2)

!  All done
!-----------

   _RETURN(ESMF_SUCCESS)

 contains

   subroutine UPDATE_ENDPOINTS

   logical :: DataAlreadyRead

! Get the previous date
!----------------------

    call MAPL_StateGetSpecAttrib(MPL,trim(NAME)//'_PREV',forcing=.true., &
        REFRESH_INTERVAL=DATE_PREV, rc=STATUS)
    _VERIFY(STATUS)

    DataAlreadyRead = .not. (DATE_PREV < 0)

! In forecast mode (init_only=.true.) we ready only once!
!-------------------------------------------------------
    if (use_init_only) then
       if (DataAlreadyRead) return

       ! check if a bcs_restart file exists
       ! if YES, use its content to overwrite current clock

       if (AM_I_Root_) then
          call MAPL_GetResource( MPL, FNAME, 'BCS_RESTART:', &
               default='bcs_restart', rc = status )
          _VERIFY(STATUS)
          inquire(FILE = FNAME, EXIST=FileExists)
          if (FileExists) then
             UNIT = GETFILE ( FNAME, form="formatted", rc=status )
             _VERIFY(STATUS)
             read(UNIT,'(i8.8,1x,i6.6)',iostat=status) datetime
             _VERIFY(STATUS)
             call FREE_FILE(UNIT)
          else
             call MAPL_PackDateTime(DATETIME, YY, MM, DD, H, M, S)
             !write bcs checkpoint
             UNIT = GETFILE ( FNAME, form="formatted", rc=status )
             _VERIFY(STATUS)
             write(UNIT,'(i8.8,1x,i6.6)',iostat=status) datetime
             _VERIFY(STATUS)
             call FREE_FILE(UNIT)
          end if
       end if

       call MAPL_CommsBcast(layout, datetime, n=2, ROOT=MAPL_Root, rc=status)
       _VERIFY(STATUS)

       CALL MAPL_UnpackDateTime(DATETIME, YY, MM, DD, H, M, S)

       call ESMF_TimeSet(CurrentTime, &
                         YY=YY, MM=MM, DD=DD, &
                         H=H, M=M, S=S, rc=status)
       _VERIFY(STATUS)
    end if

! Get forcing fortran unit
!-------------------------

    UNIT=GETFILE(DATAFILE,form='unformatted',all_pes=.true.)

! Check to see if forcing state buffers have been initialized
!-------------------------------------------------------------

    if ( DATE_PREV < 0 .or. CLIMATOLOGY .or. CurrentTime < MIDT1 .or. CurrentTime > MIDT2 ) then

! If not, initialize them, by looping until correct times are found
!-----------------------------------------------------------------

       if(AM_I_ROOT_) then
          rewind(UNIT, iostat=status)
          _VERIFY(STATUS)
          do
             read(UNIT, iostat=status) REAL_HEADER
             _VERIFY(STATUS)
             HEADER = nint(REAL_HEADER)
             call ESMF_TimeSet(DATEN, &
                              YY=HEADER(7), MM=HEADER(8), DD=HEADER(9), &
                              H=HEADER(10), M=HEADER(11), S=HEADER(12), &
                              calendar=cal, rc=status)
             _VERIFY(STATUS)
    
! If the current time is beyond the item's final time, skip it.
!--------------------------------------------------------------

             if (DATEN < CurrentTime) then
                read(UNIT, iostat=status)
                _VERIFY(STATUS)
                cycle
             else
                backspace (UNIT, iostat=status)
                _VERIFY(STATUS)
                exit
             end if
          end do
       end if

! We have found an interval that contains Current. Now get its initial time
!--------------------------------------------------------------------------

       call READ_PARALLEL(Layout, REAL_HEADER, unit=UNIT, rc=status)
       _VERIFY(STATUS)
       HEADER = nint(REAL_HEADER)

       call ESMF_TimeSet(DATEN, &
                         YY=HEADER(7), MM=HEADER(8), DD=HEADER(9), &
                         H=HEADER(10), M=HEADER(11), S=HEADER(12), &
                         calendar=cal, rc=status)
       _VERIFY(STATUS)
    
       call ESMF_TimeSet(DATE1, &
                         YY=HEADER(1), MM=HEADER(2), DD=HEADER(3), &
                         H =HEADER(4), M =HEADER(5), S =HEADER(6), &
                         calendar=cal, rc=status)
       _VERIFY(STATUS)

! compute its central time
!-------------------------

       TimeDiff=DATEN-DATE1
       MIDT2=DATE1 + TimeDiff/2.0D0

       if(MIDT2<=CurrentTime .or. use_init_only) then ! The item we found is PREV
          MIDT1 = MIDT2
          
          call WRITE_PARALLEL("Previous time for"//trim(NAME)//"s is:", RC=STATUS)
          _VERIFY(STATUS)
          call WRITE_PARALLEL(HEADER(1:12), RC=STATUS)
          _VERIFY(STATUS)

! Read PREV
!----------

          call READIT('_PREV')

          call MAPL_StateSetTimeStamp(MPL,trim(NAME)//'_PREV',MIDT1, RC=STATUS )
          _VERIFY(STATUS)

          if (use_init_only) then
             if(ONED) then
                NEX1   = PRE1
             else
                NEX2   = PRE2
             end if
             return
          end if

! Read the header for NEXT
!-------------------------

          call READ_PARALLEL(Layout, REAL_HEADER, unit=UNIT, rc=status)
          _VERIFY(STATUS)
          HEADER = nint(REAL_HEADER)

! Get NEXT's initial and final times
!-----------------------------------

          call ESMF_TimeSet(DATEN, &
                            YY=HEADER(7), MM=HEADER(8), DD=HEADER(9), &
                            H=HEADER(10), M=HEADER(11), S=HEADER(12), &
                            calendar=cal, rc=status)
          _VERIFY(STATUS)

          call ESMF_TimeSet(DATE1, &
                            YY=HEADER(1), MM=HEADER(2), DD=HEADER(3), &
                            H =HEADER(4), M =HEADER(5), S =HEADER(6), &
                            calendar=cal, rc=status)
          _VERIFY(STATUS)

! compute its central time
!-------------------------

          TimeDiff=DATEN-DATE1
          MIDT2=DATE1 + TimeDiff/2.0D0

! and read NEXT
!--------------

          call READIT('_NEXT')
          _VERIFY(STATUS)

          call MAPL_StateSetTimeStamp(MPL,trim(NAME)//'_NEXT',MIDT2, RC=STATUS )
          _VERIFY(STATUS)

       else

! Read NEXT
!----------

          call READIT('_NEXT')
          _VERIFY(STATUS)

          call MAPL_StateSetTimeStamp(MPL,trim(NAME)//'_NEXT',MIDT2, RC=STATUS )
          _VERIFY(STATUS)

! Back up to get PREV
!--------------------

          call MAPL_Backspace(UNIT, LAYOUT, COUNT=4, RC=STATUS); _VERIFY(STATUS)

! Read the header of PREV item
!-----------------------------

          call READ_PARALLEL(Layout, REAL_HEADER, unit=UNIT, rc=status)
          _VERIFY(STATUS)
          HEADER = nint(REAL_HEADER)

! Get the item's initial and final times
!---------------------------------------

          call ESMF_TimeSet(DATEN, &
                            YY=HEADER(7), MM=HEADER(8), DD=HEADER(9), &
                            H=HEADER(10), M=HEADER(11), S=HEADER(12), &
                            calendar=cal, rc=status)
          _VERIFY(STATUS)

          call ESMF_TimeSet(DATE1, &
                            YY=HEADER(1), MM=HEADER(2), DD=HEADER(3), &
                            H =HEADER(4), M =HEADER(5), S =HEADER(6), &
                            calendar=cal, rc=status)
          _VERIFY(STATUS)

! compute its central time
!-------------------------

          TimeDiff=DATEN-DATE1
          MIDT1=DATE1 + TimeDiff/2.0D0

! Read PREV
!----------

          call READIT('_PREV')
          _VERIFY(STATUS)
          
          call MAPL_StateSetTimeStamp(MPL,trim(NAME)//'_PREV',MIDT1, RC=STATUS )
          _VERIFY(STATUS)

! Skip over NEXT to be positioned for subsequent reads
!-----------------------------------------------------

          call MAPL_Skip(UNIT, LAYOUT, COUNT=2, RC=STATUS ); _VERIFY(STATUS)

       end if
       
    elseif(.not.use_init_only) then ! Just need to update NEXT,  PREV is old NEXT

       if(ONED) then
          PRE1   = NEX1
       else
          PRE2   = NEX2
       end if

       MIDT1  = MIDT2

! Move time stamp from NEXT to PREV
!------------------------------------------

       call MAPL_StateGetTimeStamp(MPL,trim(NAME)//'_NEXT',MIDT2, RC=STATUS )
       _VERIFY(STATUS)
       call MAPL_StateSetTimeStamp(MPL,trim(NAME)//'_PREV',MIDT2, RC=STATUS )
       _VERIFY(STATUS)

! Read the header of next item
!-----------------------------

       call READ_PARALLEL(Layout, REAL_HEADER, unit=UNIT, rc=status)
       _VERIFY(STATUS)
       HEADER = nint(REAL_HEADER)

! Get the item's initial and final time
!--------------------------------------
 
       call ESMF_TimeSet(DATE1, &
                        YY=HEADER(1), MM=HEADER(2), DD=HEADER(3), &
                        H =HEADER(4), M =HEADER(5), S =HEADER(6), &
                        calendar=cal, rc=status)
       _VERIFY(STATUS)
 
       call ESMF_TimeSet(DATEN, &
                        YY=HEADER(7), MM=HEADER(8), DD=HEADER(9), &
                        H =HEADER(10),M =HEADER(11),S =HEADER(12),&
                        calendar=cal, rc=status)
       _VERIFY(STATUS)

       TimeDiff=DATEN-DATE1
       MIDT2=DATE1 + TimeDiff/2.0D0

! Verify that it is the next item
!--------------------------------

       _ASSERT(MIDT2 >= CurrentTime,'needs informative message')

! Read NEXT
!----------

       call READIT('_NEXT')
       _VERIFY(STATUS)

       call MAPL_StateSetTimeStamp(MPL,trim(NAME)//'_NEXT',MIDT2, RC=STATUS )
       _VERIFY(STATUS)

    endif

  end subroutine UPDATE_ENDPOINTS

  subroutine READIT(WHICH)
    character(len=5), intent(IN) :: WHICH
    real, pointer :: VAR1(:)
    integer       :: PRF
    integer       :: MUNIT
    integer       :: io_rank
    integer(kind=MPI_OFFSET_KIND)         :: offset
    logical       :: AmReader
    type(ArrDescr):: ArrDes
    integer(kind=MPI_OFFSET_KIND)         :: _FTELL
#ifndef __GFORTRAN__
    external      :: _FTELL
#endif
    type(ESMF_Grid) :: TILEGRID
    integer       :: COUNTS(2)
  
    call MAPL_GetResource( MPL, PRF, 'PARALLEL_READFORCING:', default=0, rc = status )
    _VERIFY(STATUS)
    MUNIT = 0 ! to avoid un-init problems

    AmReader = .false.
    if (PRF /= 0) then
       AmReader = mpl%grid%readers_comm /= MPI_COMM_NULL

       if (use_ON_TILES) then
          call MAPL_LocStreamGet(LocStream, TILEGRID=TILEGRID, RC=STATUS)
          _VERIFY(STATUS)
 
          call MAPL_GridGet(TILEGRID, globalCellCountPerDim=COUNTS, RC=STATUS)
          _VERIFY(STATUS)
          call ArrDescrSet(arrdes,                     &
               readers_comm  = mpl%grid%readers_comm,  &
               ioscattercomm = mpl%grid%comm,          &
               i1 = mpl%grid%i1, in = mpl%grid%in,     &
               j1 = mpl%grid%j1, jn = mpl%grid%jn,     &
               im_world = COUNTS(1),                   &
               jm_world = COUNTS(2)                  )
       else
          call ArrDescrSet(arrdes,                     &
               readers_comm  = mpl%grid%readers_comm,  &
               ioscattercomm = mpl%grid%ioscattercomm, &
               i1 = mpl%grid%i1, in = mpl%grid%in,     &
               j1 = mpl%grid%j1, jn = mpl%grid%jn,     &
               im_world = mpl%grid%im_world,           &
               jm_world = mpl%grid%jm_world)
       endif
       arrdes%Ycomm = mpl%grid%Ycomm

       if (AmReader) then
          call MPI_COMM_RANK(mpl%grid%readers_comm, io_rank, STATUS)
          _VERIFY(STATUS)
          if (io_rank == 0) then
             print *,'Using parallel IO for reading file: ',trim(DATAFILE)

#ifdef __NAG_COMPILER_RELEASE
             _FAIL('NAG does not provide ftell. Convert to stream I/O')
#else
             offset = _FTELL(UNIT)+4
#endif

             ! MAT: Here we help protect against use of the 32-bit
             !      ftell from the macro at top. If we read a file
             !      > 2 gb, ftell returns a negative number, so
             !      offset will be less than 4.

             _ASSERT(offset >= 4,'needs informative message')
          end if
!ALT: disclaimer
! This code assumes the reader with i0_rank == 0 is also Root in the Components VM
! This is the only processor that actually could check the offset in the file
          call MPI_Bcast(offset, 1, MPI_INTEGER8, 0, mpl%grid%readers_comm, STATUS)
          _VERIFY(STATUS)
          call ArrDescrSet(arrdes, offset)

          call MPI_Barrier(mpl%grid%readers_comm, status)
          _VERIFY(STATUS)
          call MPI_FILE_OPEN(mpl%grid%readers_comm, DATAFILE, MPI_MODE_RDONLY, &
               MPI_INFO_NULL, MUNIT, STATUS)
          _VERIFY(STATUS)
          call MPI_Barrier(mpl%grid%readers_comm, status)
          _VERIFY(STATUS)
       end if
    end if

    if(TRANSFORM) then
       if (PRF /= 0) then
          _ASSERT(.false.,'needs informative message') ! for now
       else
! ALT this LOOKS WRONG. MAPL_VarRead needs a mask for tiles!!!
          call MAPL_VarRead(UNIT, GRID, VAR2, RC=STATUS )
          _VERIFY(STATUS)
       end if
       call GET_POINTER(MPL%FORCING, VAR1, trim(NAME)//WHICH, RC=STATUS)
       _VERIFY(STATUS)
       call MAPL_LocStreamTransform(LOCSTREAM, VAR1, VAR2, interp=.true.,RC=STATUS )
       _VERIFY(STATUS)
    else
       if (PRF /= 0) then
          call MAPL_VarRead(UNIT=MUNIT, STATE=MPL%FORCING, NAME=trim(NAME)//WHICH, &
               arrdes=arrdes, RC=STATUS)
          _VERIFY(STATUS)
       else
          call MAPL_VarRead(UNIT, MPL%FORCING, trim(NAME)//WHICH, RC=STATUS)
          _VERIFY(STATUS)
       end if
    end if

    if (AmReader) then
! we need to advance the file pointer properly (to the end of the current record) on root 
! by using a blank Fortran read
       if (io_rank == 0) then
          read(UNIT, iostat=status)
          _VERIFY(STATUS)
       end if

       call MPI_Barrier(mpl%grid%readers_comm, status)
       _VERIFY(STATUS)
       call MPI_FILE_CLOSE(MUNIT, status)
       _VERIFY(STATUS)
       call MPI_Barrier(mpl%grid%readers_comm, status)
       _VERIFY(STATUS)

    endif

  end subroutine READIT

end subroutine MAPL_READFORCINGX

!==================================================================

  subroutine MAPL_StateGetTimeStamp(STATE,NAME,TIME,RC)

    type (MAPL_MetaComp),     intent(INOUT)   :: STATE
    character(len=*),         intent(IN   )   :: NAME
    type (ESMF_Time),         intent(  OUT)   :: TIME
    integer, optional,        intent(  OUT)   :: RC

! ErrLog Variables

    integer                           :: STATUS

! Locals

    integer                           :: HOUR
    integer                           :: DATE
    integer                           :: IYR, IMM, IDD, IHR, IMN, ISC


    call MAPL_StateGetSpecAttrib(STATE,NAME,forcing=.true., &
        refresh_interval=DATE, averaging_interval=HOUR, RC=STATUS )
    _VERIFY(STATUS)

    if (DATE <= 0) then
       IYR=0; IMM = 1; IDD = 1
       IHR=0; IMN = 0; ISC = 0
    else
       call MAPL_UNPACKTIME(DATE,IYR,IMM,IDD)
       call MAPL_UNPACKTIME(HOUR,IHR,IMN,ISC)
    endif

    call ESMF_TimeSet(TIME, YY=IYR, MM=IMM, DD=IDD, H=IHR, M=IMN, S=ISC, RC=STATUS )
    _VERIFY(STATUS)

    _RETURN(ESMF_SUCCESS)

  end subroutine MAPL_StateGetTimeStamp

!==================================================================

  subroutine MAPL_StateSetTimeStamp(STATE,NAME,TIME,RC)

    type (MAPL_MetaComp),     intent(INOUT)   :: STATE
    character(len=*),         intent(IN   )   :: NAME
    type (ESMF_Time),         intent(INout)   :: TIME
    integer, optional,        intent(  OUT)   :: RC

! ErrLog Variables

    integer                           :: STATUS

! Locals

    integer                           :: HOUR
    integer                           :: DATE
    integer                           :: IYR, IMM, IDD, IHR, IMN, ISC

    call ESMF_TimeGet(TIME ,YY=IYR, MM=IMM, DD=IDD, H=IHR, M=IMN, S=ISC, rc=STATUS)
    _VERIFY(STATUS)

    call MAPL_PackTime(DATE,IYR,IMM,IDD)
    call MAPL_PackTime(HOUR,IHR,IMN,ISC)

    call MAPL_StateSetSpecAttrib(STATE,NAME,forcing=.true., &
         refresh_interval=DATE, &
         averaging_interval=HOUR, rc=STATUS)
    _VERIFY(STATUS)

    _RETURN(ESMF_SUCCESS)

  end subroutine MAPL_StateSetTimeStamp

  subroutine MAPL_GenericMakeXchgNatural(STATE, RC)
    type (MAPL_MetaComp),     intent(INOUT)   :: STATE
    integer, optional,        intent(  OUT)   :: RC

! ErrLog Variables


    STATE%LOCSTREAM = STATE%ExchangeGrid

    _RETURN(ESMF_SUCCESS)
  end subroutine MAPL_GenericMakeXchgNatural

  
  subroutine MAPL_GridCreate(GC, MAPLOBJ, ESMFGRID, srcGC, rc)
    use MAPL_GridManagerMod, only: grid_manager
    type(ESMF_GridComp), optional,         intent(INOUT) :: GC
    type (MAPL_MetaComp),optional, target, intent(INOUT) :: MAPLOBJ
    type (ESMF_Grid),    optional,         intent(  OUT) :: ESMFGRID
    type(ESMF_GridComp), optional,         intent(INout) :: srcGC
    integer,             optional,         intent(  OUT) :: rc

    integer                               :: status
    character(len=ESMF_MAXSTR)            :: Comp_Name
    character(len=ESMF_MAXSTR)            :: IAm

    type (ESMF_VM)                        :: VM
    type (MAPL_MetaComp), pointer         :: STATE
    type (ESMF_Grid)                      :: GRID
    integer                               :: nn,ny
    character(len=ESMF_MAXSTR)            :: GridName
    character(len=2)                      :: dateline
#ifdef CREATE_REGULAR_GRIDS
    logical                               :: isRegular
#endif

    ! Query GC
    !---------

    Iam='MAPL_GridCreate'
    if(present(GC)) then
       call ESMF_GridCompGet( GC, name=Comp_Name,   rc = status )
       _VERIFY(STATUS)
       Iam = trim(Comp_Name)//Iam
    endif

    ! New option to get grid from existing component
    !-----------------------------------------------

    if(present(srcGC)) then
       call ESMF_GridCompGet ( srcGC, grid=Grid, RC=STATUS )
       _VERIFY(STATUS)
       if(present(GC)) then
          call ESMF_GridCompSet(GC, GRID=GRID, RC=STATUS)
          _VERIFY(STATUS)
       end if
       if(present(ESMFGRID)) then
          ESMFGRID=GRID
       end if
       _RETURN(ESMF_SUCCESS)
    end if


    
    call ESMF_VMGetCurrent(vm, rc=status)
    _VERIFY(STATUS)

    ! Get MAPL object
    !----------------

    if(present(GC)) then
       _ASSERT(.not. present(MAPLOBJ),'needs informative message')
       call MAPL_InternalStateGet(GC, STATE, RC=STATUS)
       _VERIFY(STATUS)
    elseif(present(MAPLOBJ)) then
       STATE => MAPLOBJ
    else
       _ASSERT(.false.,'needs informative message')
    endif

    call MAPL_ConfigPrepend(state%cf,trim(comp_name),CF_COMPONENT_SEPARATOR,'NX:',rc=status)
    _VERIFY(status)
    call MAPL_ConfigPrepend(state%cf,trim(comp_name),CF_COMPONENT_SEPARATOR,'NY:',rc=status)
    _VERIFY(status)
   
    call ESMF_ConfigGetAttribute(state%cf,gridname,label=trim(comp_name)//CF_COMPONENT_SEPARATOR//'GRIDNAME:',rc=status)
    _VERIFY(status)
    nn = len_trim(gridname)
    dateline = gridname(nn-1:nn)
    if (dateline == 'CF') then
       call ESMF_ConfigGetAttribute(state%CF,ny,label=trim(COMP_Name)//CF_COMPONENT_SEPARATOR//'NY:',rc=status)
       _VERIFY(status)
       call MAPL_ConfigSetAttribute(state%CF, value=ny/6, label=trim(COMP_Name)//CF_COMPONENT_SEPARATOR//'NY:',rc=status)
       _VERIFY(status)
    end if

    grid = grid_manager%make_grid(state%CF, prefix=trim(COMP_Name)//CF_COMPONENT_SEPARATOR, rc=status)
    _VERIFY(status)

    call state%grid%set(grid, __RC__)

    if(present(GC)) then
       call ESMF_GridCompSet(GC, GRID=GRID, RC=STATUS)
       _VERIFY(STATUS)
    end if

    if(present(ESMFGRID)) then
       ESMFGRID=GRID
    end if

    _RETURN(ESMF_SUCCESS)

    contains

       subroutine MAPL_ConfigPrepend(cf,prefix,separator,label,rc)
          type(ESMF_Config), intent(inout) :: cf
          character(len=*) , intent(in   ) :: prefix
          character(len=*) , intent(in   ) :: separator
          character(len=*) , intent(in   ) :: label
          integer, optional , intent(out  ) :: rc

          integer  :: status
          character(len=ESMF_MAXSTR) :: Iam = "MAPL_ConfigPrepend"
          integer  :: val

              call ESMF_ConfigGetAttribute( cf, val, label=trim(prefix)//trim(separator)//trim(label), rc = status )
              if (status /= ESMF_SUCCESS) then
                 call ESMF_ConfigGetAttribute(CF,val,label=trim(label),rc=status)
                 _VERIFY(status)
                 call MAPL_ConfigSetAttribute(CF, val, label=trim(prefix)//trim(separator)//trim(label),rc=status)
                 _VERIFY(status)
              end if

          _RETURN(ESMF_SUCCESS)

       end subroutine MAPL_ConfigPrepend

  end subroutine MAPL_GridCreate

  subroutine MAPL_GridCoordAdjustFromFile(GRID, GRIDSPECFILE, RC)
    type(ESMF_Grid),               intent(INout ) :: Grid
    character(len=*),              intent(IN    ) :: GRIDSPECFILE
    integer, optional,             intent(   OUT) :: RC  

! local vars
!------------ 
    integer                    :: STATUS
    integer :: UNIT
    integer :: IM, JM
    integer :: IMSTART, JMSTART
    integer :: IM_WORLD, JM_WORLD
    integer :: DUMMYI, DUMMYJ

    integer :: COUNTS(3), DIMS(3)
    type(ESMF_DELayout) :: LAYOUT
    type(ESMF_DistGrid) :: DISTGRID
    real(ESMF_KIND_R8), allocatable :: x(:,:), y(:,:)
    real(ESMF_KIND_R8), pointer :: gridx(:,:), gridy(:,:)


! get IM, JM and IM_WORLD, JM_WORLD
    call MAPL_GridGet(GRID, localCellCountPerDim=COUNTS, globalCellCountPerDim=DIMS, RC=STATUS)
    _VERIFY(STATUS)

    IM = COUNTS(1)
    JM = COUNTS(2)
    IM_WORLD = DIMS(1)
    JM_WORLD = DIMS(2)

! get global index of the lower left corner
!------------------------------------------
    call MAPL_GRID_INTERIOR(GRID,IMSTART,DUMMYI,JMSTART,DUMMYJ)
  
    call ESMF_GridGetCoord(grid, localDE=0, coordDim=1, &
         staggerloc=ESMF_STAGGERLOC_CENTER, &
         farrayPtr=gridx, rc=status)
    _VERIFY(STATUS) 

    call ESMF_GridGetCoord(grid, localDE=0, coordDim=2, &
         staggerloc=ESMF_STAGGERLOC_CENTER, &
         farrayPtr=gridy, rc=status)
    _VERIFY(STATUS) 

    allocate(x(IM_WORLD, JM_WORLD), stat=status)
    _VERIFY(STATUS)
    allocate(y(IM_WORLD, JM_WORLD), stat=status)
    _VERIFY(STATUS)

    call ESMF_GridGet    (GRID,   distGrid=distGrid, rc=STATUS)
    _VERIFY(STATUS)
    call ESMF_DistGridGet(distGRID, delayout=layout, rc=STATUS)
    _VERIFY(STATUS)

    UNIT = GETFILE(GRIDSPECFILE, form="formatted", rc=status)
    call READ_PARALLEL(LAYOUT, X, unit=UNIT)
    call READ_PARALLEL(LAYOUT, Y, unit=UNIT)
    call FREE_FILE(UNIT)


! Make sure the longitudes are between -180 and 180 degrees
!ALT disable this for AR5    X = mod(X + 72180._REAL64,360._REAL64) - 180._REAL64 ! -180<= lon0 <180
! Convert to radians
    X = X * (MAPL_PI_R8)/180._REAL64
    Y = Y * (MAPL_PI_R8)/180._REAL64
     

! Modify grid coordinates
!------------------------
    GRIDX = X(IMSTART:IMSTART+IM-1,JMSTART:JMSTART+JM-1)
    GRIDY = Y(IMSTART:IMSTART+IM-1,JMSTART:JMSTART+JM-1)

! Clean-up
!---------
    deallocate(y)
    deallocate(x)

! All done
!---------
    _RETURN(ESMF_SUCCESS)
  end subroutine MAPL_GridCoordAdjustFromFile


  recursive subroutine MAPL_GetRootGC(GC, rootGC, RC)
    type(ESMF_GridComp),    intent(INout) :: GC
    type(ESMF_GridComp),    intent(  OUT) :: rootGC
    integer, optional,      intent(OUT)   :: rc

    integer                               :: status
    type (MAPL_MetaComp),     pointer     :: META 

    call MAPL_GetObjectFromGC(GC, META, RC=STATUS)
    _VERIFY(STATUS)

    if (.not. associated(META%parentGC)) then
       rootGC = GC
    else
       call MAPL_GetRootGC(META%parentGC, rootGC, RC=status)
       _VERIFY(STATUS)
    end if

    _RETURN(ESMF_SUCCESS)
  end subroutine MAPL_GetRootGC



  integer function MAPL_AddMethod(PHASE, RC)
    integer, pointer               :: PHASE(:)
    integer, optional, intent(out) :: rc

    integer :: I
    integer, pointer :: tmp(:)
    integer :: status
    character(len=ESMF_MAXSTR), parameter :: Iam="MAPL_AddMethod"

    MAPL_AddMethod = MAPL_FirstPhase
    if (.not.associated(PHASE)) then
     ! this is the method to be added
       I = 1
       allocate(PHASE(I), stat=status)
       _VERIFY(STATUS)
       PHASE(I) = MAPL_AddMethod

    else
       I = size(PHASE) + 1
       allocate(TMP(I), stat=status)
       _VERIFY(STATUS)
       TMP(1:I-1) = PHASE
       TMP(I) = TMP(I-1)+1

       deallocate(PHASE)
       PHASE => TMP
    end if
    MAPL_AddMethod = PHASE(I)

    _RETURN(ESMF_SUCCESS)
  end function MAPL_AddMethod


  recursive subroutine MAPL_SetStateSave(state,filetype,rc)
    type(MAPL_MetaComp), intent(inout) :: state
    integer,             intent(in ) :: filetype
    integer, optional,   intent(out) :: rc
    type(MAPL_MetaComp), pointer :: CMAPL => null()
    integer :: k, status

    type(ESMF_GridComp), pointer :: gridcomp

       do k=1,state%get_num_children()
          gridcomp => state%GET_CHILD_GRIDCOMP(K)
          call MAPL_GetObjectFromGC ( gridcomp, CMAPL, RC=STATUS)
          _VERIFY(STATUS)
          call MAPL_SetStateSave(CMAPL,filetype,RC=STATUS)
          _VERIFY(STATUS)
       enddo

    state%initial_state%filetype = filetype

  end subroutine MAPL_SetStateSave

  recursive subroutine MAPL_GenericStateSave( GC, IMPORT, EXPORT, CLOCK, RC )
    type(ESMF_GridComp), intent(inout) :: GC     ! composite gridded component
    type(ESMF_State),    intent(inout) :: IMPORT ! import state
    type(ESMF_State),    intent(inout) :: EXPORT ! export state
    type(ESMF_Clock),    intent(inout) :: CLOCK  ! the clock
    integer, optional,   intent(  out) :: RC     ! Error code:

    type(mapl_metacomp), pointer :: state
    integer :: i,filetype

    character(len=14)                           :: datestamp
    character(len=1)                            :: separator

    character(len=ESMF_MAXSTR)                  :: filetypechar
    character(len=4)                            :: extension
    integer                                     :: hdr
    integer :: status
    character(len=:), allocatable :: tmpstr
    character(len=ESMF_MAXSTR) :: filename
    character(len=ESMF_MAXSTR)                  :: CFILETYPE
    type(ESMF_GridComp), pointer :: gridcomp
    type(ESMF_State), pointer :: child_import_state
    type(ESMF_State), pointer :: child_export_state
    type (ESMF_State), pointer :: internal_state

    _UNUSED_DUMMY(EXPORT)
    call MAPL_InternalStateRetrieve(GC, STATE, RC=STATUS)
    _VERIFY(STATUS)

    call MAPL_GetResource( STATE, FILENAME,         &
                          LABEL="IMPORT_CHECKPOINT_FILE:", &
                          RC=STATUS)
    if(STATUS==ESMF_SUCCESS) then
       _ASSERT(.not.allocated(state%initial_state%imp_fname),"can only save one state")
       STATE%initial_state%IMP_FNAME = FILENAME
    end if
    call MAPL_GetResource( STATE   , filename,  &
                        LABEL="INTERNAL_CHECKPOINT_FILE:", &
                         RC=STATUS)
    if(STATUS==ESMF_SUCCESS) then
       _ASSERT(.not.allocated(state%initial_state%int_fname),"can only save one state")
       STATE%initial_state%INT_FNAME = FILENAME
    end if

       do I=1,STATE%get_num_children()
          gridcomp => STATE%GET_CHILD_GRIDCOMP(I)
          child_import_state => STATE%get_child_import_state(i)
          child_export_state => STATE%get_child_export_state(i)
          call MAPL_GenericStateSave (gridcomp, &
               child_import_state, &
               child_export_state, &
               CLOCK, RC=STATUS )
          _VERIFY(status)
       enddo

    call MAPL_DateStampGet(clock, datestamp, rc=status)
    _VERIFY(STATUS)
    filetype=state%initial_state%filetype
    if (FILETYPE /= MAPL_Write2Disk) then
       separator = '*'
    else
       separator = '.'
    end if

    if (allocated(state%initial_state%imp_fname)) then
       call    MAPL_GetResource( STATE, filetypechar, LABEL="IMPORT_CHECKPOINT_TYPE:",                  RC=STATUS )
       if ( STATUS/=ESMF_SUCCESS  .or.  filetypechar == "default" ) then
      call MAPL_GetResource( STATE, filetypechar, LABEL="DEFAULT_CHECKPOINT_TYPE:", default='pnc4', RC=STATUS )
                    _VERIFY(STATUS)
    end if
    filetypechar = ESMF_UtilStringLowerCase(filetypechar,rc=STATUS)
    _VERIFY(STATUS)
    if (filetypechar == 'pnc4') then
       extension = '.nc4'
    else
       extension = '.bin'
     end if
      tmpstr=trim(state%initial_state%imp_fname)
      deallocate(state%initial_state%imp_fname)
      STATE%initial_state%IMP_FNAME = tmpstr // separator // DATESTAMP // extension
      deallocate(tmpstr)
    end if

    if (allocated(state%initial_state%int_fname)) then
       call    MAPL_GetResource( STATE, hdr,      LABEL="INTERNAL_HEADER:",         default=0,      RC=STATUS )
       _VERIFY(STATUS)
       call    MAPL_GetResource( STATE, filetypechar, LABEL="INTERNAL_CHECKPOINT_TYPE:",                RC=STATUS )
       if ( STATUS/=ESMF_SUCCESS  .or.  filetypechar == "default" ) then
          call MAPL_GetResource( STATE, filetypechar, LABEL="DEFAULT_CHECKPOINT_TYPE:", default='pnc4', RC=STATUS )
          _VERIFY(STATUS)
       end if
       filetypechar = ESMF_UtilStringLowerCase(filetypechar,rc=STATUS)
       _VERIFY(STATUS)
       if (filetypechar == 'pnc4') then
          extension = '.nc4'
       else
          extension = '.bin'
       end if
       tmpstr=trim(state%initial_state%int_fname)
       deallocate(state%initial_state%int_fname)
       STATE%initial_state%INT_FNAME = tmpstr // separator // DATESTAMP // extension
       deallocate(tmpstr)
    end if

    if (allocated(state%initial_state%imp_fname)) then
       call    MAPL_GetResource( STATE, CFILETYPE, LABEL="IMPORT_CHECKPOINT_TYPE:",                  RC=STATUS )
       if ( STATUS/=ESMF_SUCCESS  .or.  CFILETYPE == "default" ) then
          call MAPL_GetResource( STATE, CFILETYPE, LABEL="DEFAULT_CHECKPOINT_TYPE:", default='pnc4', RC=STATUS )
          _VERIFY(STATUS)
       end if
       call MAPL_ESMFStateWriteToFile(IMPORT, CLOCK, &
                                      STATE%initial_state%IMP_FNAME, &
                                      CFILETYPE, STATE, .FALSE., oClients = o_Clients, &
                                    RC=STATUS)
       _VERIFY(STATUS)
    end if

    if (allocated(state%initial_state%int_fname)) then
       call    MAPL_GetResource( STATE, hdr,      LABEL="INTERNAL_HEADER:",         default=0,      RC=STATUS )
       _VERIFY(STATUS)
       call    MAPL_GetResource( STATE, CFILETYPE, LABEL="INTERNAL_CHECKPOINT_TYPE:",                RC=STATUS )
       if ( STATUS/=ESMF_SUCCESS  .or.  CFILETYPE == "default" ) then
          call MAPL_GetResource( STATE, CFILETYPE, LABEL="DEFAULT_CHECKPOINT_TYPE:", default='pnc4', RC=STATUS )
          _VERIFY(STATUS)
       end if
       internal_state => STATE%get_internal_state()
       call MAPL_ESMFStateWriteToFile(internal_state, CLOCK, &
                                      STATE%initial_state%INT_FNAME, &
                                      CFILETYPE, STATE, hdr/=0, oClients = o_Clients, &
                                    RC=STATUS)
       _VERIFY(STATUS)
    end if

    _RETURN(ESMF_SUCCESS)
  end subroutine MAPL_GenericStateSave


  recursive subroutine MAPL_GenericStateRestore ( GC, IMPORT, EXPORT, CLOCK, RC )

! !ARGUMENTS:

    type(ESMF_GridComp), intent(inout) :: GC     ! composite gridded component
    type(ESMF_State),    intent(inout) :: IMPORT ! import state
    type(ESMF_State),    intent(inout) :: EXPORT ! export state
    type(ESMF_Clock),    intent(inout) :: CLOCK  ! the clock
    integer, optional,   intent(  out) :: RC     ! Error code:
                                                 ! = 0 all is well
                                                 ! otherwise, error
!EOPI

! LOCAL VARIABLES

  character(len=ESMF_MAXSTR)                  :: IAm
  character(len=ESMF_MAXSTR)                  :: COMP_NAME
  character(len=ESMF_MAXSTR)                  :: CHILD_NAME
  integer                                     :: STATUS
  integer                                     :: I
  type (MAPL_MetaComp), pointer               :: STATE
  integer                                     :: hdr, unit
  type(ESMF_GridComp), pointer :: gridcomp
  type(ESMF_State), pointer :: child_import_state
  type(ESMF_State), pointer :: child_export_state
  type(ESMF_State), pointer :: internal_state
!=============================================================================

!  Begin...

  _UNUSED_DUMMY(EXPORT)
  Iam = "MAPL_GenericStateRestore"
  call ESMF_GridCompGet(GC, name=COMP_NAME, RC=STATUS )
  _VERIFY(STATUS)
  Iam = trim(COMP_NAME) // Iam

! Retrieve the pointer to the state
!----------------------------------

  call MAPL_InternalStateRetrieve(GC, STATE, RC=STATUS)
  _VERIFY(STATUS)

  call MAPL_GenericStateClockOn(STATE,"TOTAL")
! Refresh the children
! ---------------------
     do I=1,STATE%get_num_children()
        gridcomp => STATE%GET_CHILD_GRIDCOMP(I)
        call ESMF_GridCompGet( gridcomp, NAME=CHILD_NAME, RC=STATUS )
        _VERIFY(STATUS)
        child_import_state => STATE%get_child_import_state(i)
        child_export_state => STATE%get_child_export_state(i)
        call MAPL_GenericStateRestore (gridcomp, child_import_state, child_export_state, CLOCK, &
             RC=STATUS )
        _VERIFY(STATUS)
     enddo

! Do my "own" refresh
! ------------------
  call MAPL_GenericStateClockOn(STATE,"--GenRefreshMine")

  if (allocated(STATE%initial_state%imp_fname)) then
     call MAPL_ESMFStateReadFromFile(IMPORT, CLOCK, &
                                     STATE%initial_state%IMP_FNAME, &
                                     STATE, .FALSE., RC=STATUS)
     _VERIFY(STATUS)
  end if

  if (allocated(state%initial_state%int_fname)) then
     call MAPL_GetResource( STATE   , hdr,         &
                            default=0, &
                            LABEL="INTERNAL_HEADER:", &
                            RC=STATUS)
     _VERIFY(STATUS)
     internal_state => state%get_internal_state()
     call MAPL_ESMFStateReadFromFile(internal_state, CLOCK, &
                                     STATE%initial_state%INT_FNAME, &
                                     STATE, hdr/=0, RC=STATUS)
     _VERIFY(STATUS)
     UNIT = GETFILE(STATE%initial_state%INT_FNAME, RC=STATUS)
     _VERIFY(STATUS)
  end if

  _RETURN(ESMF_SUCCESS)
end subroutine MAPL_GenericStateRestore

  recursive subroutine MAPL_DestroyStateSave(gc,rc)
    type(ESMF_GridComp), intent(inout) :: GC    
    integer, optional,   intent(out) :: rc
    type(MAPL_MetaComp), pointer :: state
    integer :: unit, i, status

    type (ESMF_GridComp), pointer :: gridcomp

     call MAPL_InternalStateRetrieve(GC, STATE, RC=STATUS)
     _VERIFY(STATUS)
        do I=1,STATE%get_num_children()
           gridcomp => STATE%GET_CHILD_GRIDCOMP(I)
           call MAPL_DestroyStateSave (gridcomp, RC=STATUS )
           _VERIFY(STATUS)
        enddo

     if (allocated(STATE%initial_state%imp_fname)) then
        UNIT = GETFILE(STATE%initial_state%IMP_FNAME, RC=STATUS)
        _VERIFY(STATUS)
        call MAPL_DestroyFile(unit = UNIT, rc=STATUS)
        _VERIFY(STATUS)
        deallocate(STATE%initial_state%imp_fname)
     end if

     if (allocated(state%initial_state%int_fname)) then
        UNIT = GETFILE(STATE%initial_state%INT_FNAME, RC=STATUS)
        _VERIFY(STATUS)
        call MAPL_DestroyFile(unit = UNIT, rc=STATUS)
        _VERIFY(STATUS)
        deallocate(state%initial_state%int_fname)
     end if

  end subroutine MAPL_DestroyStateSave

  subroutine MAPL_AddRecord(MAPLOBJ, ALARM, FILETYPE, RC)
    type(MAPL_MetaComp), intent(inout) :: MAPLOBJ
    type(ESMF_Alarm),    intent(INout) :: ALARM(:)
    integer,             intent(in ) :: filetype(:)
    integer, optional,   intent(out) :: rc

    integer :: NRA
    integer :: NR
    integer :: NRT
    integer :: I
    integer :: J
    integer :: K
    logical :: found
    type(ESMF_Alarm), allocatable :: R_ALARM(:)
    integer         , allocatable :: R_FILETYPE(:)
    character(len=ESMF_MAXSTR) :: ANAME
    character(len=ESMF_MAXSTR) :: NAME

    integer :: status
    character(len=ESMF_MAXSTR), parameter :: Iam="MAPL_AddRecord"

    NRA = size(ALARM)
    _ASSERT(size(filetype) == NRA,'needs informative message')

    if (.not. associated(MAPLOBJ%RECORD)) then
       allocate(MAPLOBJ%RECORD, stat=status)
       _VERIFY(STATUS)
       MAPLOBJ%RECORD%IMP_LEN = 0
       MAPLOBJ%RECORD%INT_LEN = 0
       allocate(MAPLOBJ%RECORD%alarm(NRA),stat=status)
       _VERIFY(STATUS)
       allocate(MAPLOBJ%RECORD%filetype(NRA),stat=status)
       _VERIFY(STATUS)
       MAPLOBJ%RECORD%alarm = alarm
       MAPLOBJ%RECORD%filetype = filetype
    else
       NR = size(MAPLOBJ%RECORD%alarm)
       NRT = NR + NRA ! maximum size
       allocate(r_alarm(NRT), r_filetype(NRT), stat=status)
       _VERIFY(STATUS)
       K = NR
       r_alarm(1:K) = MAPLOBJ%RECORD%alarm
       r_filetype(1:K) = MAPLOBJ%RECORD%filetype
       found = .false.
       do I = 1, NRA
          call ESMF_AlarmGet(ALARM(I), name=ANAME, RC=STATUS)
          _VERIFY(STATUS)
          do J = 1, NR
             call ESMF_AlarmGet(MAPLOBJ%RECORD%ALARM(J), name=NAME, RC=STATUS)
             _VERIFY(STATUS)
             if(ANAME == NAME .and. &
                filetype(I) == MAPLOBJ%RECORD%FILETYPE(J)) then
                found = .true.
                exit
             end if
          end do
          if (.not. found) then
             K = K+1
             r_alarm(K) = ALARM(I)
             r_filetype(K) = FILETYPE(I)
          end if
       end do
       if (K /= NR) then
          deallocate(MAPLOBJ%RECORD%filetype)
          deallocate(MAPLOBJ%RECORD%alarm)
          allocate(MAPLOBJ%RECORD%alarm(K),stat=status)
          _VERIFY(STATUS)
          allocate(MAPLOBJ%RECORD%filetype(K),stat=status)
          _VERIFY(STATUS)
          MAPLOBJ%RECORD%alarm = r_alarm(1:K)
          MAPLOBJ%RECORD%filetype = r_filetype(1:K)
       end if
       deallocate(r_filetype, r_alarm)
    end if
    _RETURN(ESMF_SUCCESS)
  end subroutine MAPL_AddRecord

  recursive subroutine MAPL_DisableRecord(MAPLOBJ, ALARM_NAME, RC)
    type(MAPL_MetaComp), intent(inout) :: MAPLOBJ
    character(len=*),    intent(in   ) :: ALARM_NAME
    integer, optional,   intent(out) :: rc

    integer :: k
    character(len=ESMF_MAXSTR) :: ANAME
    type(MAPL_MetaComp), pointer :: CMAPL => null()

    integer :: status
    character(len=ESMF_MAXSTR), parameter :: Iam="MAPL_DisableRecord"
    type(ESMF_GridComp), pointer :: gridcomp

    do k=1,MAPLOBJ%get_num_children()
       gridcomp => MAPLOBJ%GET_CHILD_GRIDCOMP(K)
       call MAPL_GetObjectFromGC ( gridcomp, CMAPL, RC=STATUS)
       _VERIFY(STATUS)
       call MAPL_DisableRecord(CMAPL,ALARM_NAME,RC=STATUS)
       _VERIFY(STATUS)
    enddo

    do k=1,size(MAPLOBJ%record%alarm)
       call ESMF_AlarmGet(MAPLOBJ%RECORD%ALARM(K), name=ANAME, RC=STATUS)
       if (trim(ANAME)==trim(ALARM_NAME)) then
          call ESMF_AlarmDisable(MAPLOBJ%RECORD%ALARM(K))
          _RETURN(ESMF_SUCCESS)
       end if
    enddo
    _RETURN(ESMF_FAILURE)

  end subroutine MAPL_DisableRecord

  function  MAPL_GridGetSection(Grid, SectionMap, GridName, RC) result(SECTION)
    type(ESMF_Grid),   intent(IN   ) :: GRID
    integer,           intent(IN   ) :: SectionMap(:)
    character (Len=*), optional, intent(IN   ) :: GridName
    integer, optional, intent(  OUT) :: RC
    type(ESMF_Grid) :: SECTION

    integer                          :: ndes
    integer                          :: dimcount
    integer                          :: nx, ny
    integer, allocatable             :: minindex(:,:)
    integer, allocatable             :: maxindex(:,:)
    integer, pointer                 :: ims(:) => null()
    integer, pointer                 :: jms(:) => null()
    real(ESMF_KIND_R8), pointer      :: centerX(:,:)
    real(ESMF_KIND_R8), pointer      :: centerY(:,:)
    real(ESMF_KIND_R8), pointer      :: coordX(:,:)
    real(ESMF_KIND_R8), pointer      :: coordY(:,:)
    type(ESMF_DistGrid)              :: distgrid
    type(ESMF_VM)                    :: vm
    character(len=ESMF_MAXSTR)       :: name

    integer              :: status

    call ESMF_GridGet(GRID, Name=Name, DistGrid=distgrid, dimCount=dimCount, RC=STATUS)
    _VERIFY(STATUS)

    if (present(GridName)) then
       name = GridName
    else
       name = name // "horz_section"
    end if

    _ASSERT(dimcount == size(SectionMap),'needs informative message')

    call ESMF_VmGetCurrent(VM, rc=status)
    _VERIFY(STATUS)
    call ESMF_VmGet(VM, petCount=ndes, rc=status)
    _VERIFY(STATUS)

    allocate(minindex(dimCount,ndes), maxindex(dimCount,ndes), stat=status)
    _VERIFY(STATUS)

! Processors in each direction
!-----------------------------

    call MAPL_DistGridGet(distgrid, &
         minIndex=minindex, &
         maxIndex=maxindex, rc=status)
    _VERIFY(STATUS)

    call MAPL_GetImsJms(Imins=minindex(1,:),Imaxs=maxindex(1,:),&
         Jmins=minindex(2,:),Jmaxs=maxindex(2,:),Ims=ims,Jms=jms,rc=status)
    _VERIFY(STATUS)

    deallocate(maxindex, minindex)

    NX = size(ims)
    NY = size(jms)
  
  
! Retrieve the coordinates so we can put them in the newly create SECTION
    call ESMF_GridGetCoord(GRID, coordDim=1, localDE=0, &
         staggerloc=ESMF_STAGGERLOC_CENTER, &
         farrayPtr=centerX, rc=status)
    _VERIFY(STATUS)

    call ESMF_GridGetCoord(GRID, coordDim=2, localDE=0, &
         staggerloc=ESMF_STAGGERLOC_CENTER, &
         farrayPtr=centerY, rc=status)
    _VERIFY(STATUS)

    SECTION = ESMF_GridCreate(          &
         name=name,                     &
         countsPerDEDim1=ims,           &
         countsPerDEDim2=jms,           &
         !       indexFlag = ESMF_INDEX_GLOBAL, &
         coordDep1 = (/1,2/),           &
         coordDep2 = (/1,2/),           &
         gridEdgeLWidth = (/0,0/),      &
         gridEdgeUWidth = (/0,0/),      &
         rc=status)
    _VERIFY(STATUS)
    
! Allocate coords at default stagger location
    call ESMF_GridAddCoord(SECTION, rc=status)
    _VERIFY(STATUS)

    call ESMF_GridGetCoord(SECTION, coordDim=1, localDE=0, &
         staggerloc=ESMF_STAGGERLOC_CENTER, &
         farrayPtr=coordX, rc=status)
    _VERIFY(STATUS)
    
    call ESMF_GridGetCoord(SECTION, coordDim=2, localDE=0, &
         staggerloc=ESMF_STAGGERLOC_CENTER, &
         farrayPtr=coordY, rc=status)
    _VERIFY(STATUS)

    coordX = centerX
    coordY = centerY

    deallocate(jms, ims)

    _RETURN(ESMF_SUCCESS)
  end function MAPL_GridGetSection


  logical function MAPL_RecordAlarmIsRinging(META, unusable, MODE, RC)

! !ARGUMENTS:

    type (MAPL_MetaComp), intent(inout) :: META
    class (KeywordEnforcer), optional, intent(in) :: unusable
    integer, optional,    intent(in   ) :: MODE  ! Writing file mode: disk or ram
    integer, optional,    intent(  out) :: RC     ! Error code:
                                                 ! = 0 all is well
                                                 ! otherwise, error
!EOPI

! LOCAL VARIABLES

    character(len=ESMF_MAXSTR)                  :: IAm
    integer                                     :: STATUS

    integer                                     :: I
    integer                                     :: mode_
    logical                                     :: modePresent
!=============================================================================

!  Begin...

    _UNUSED_DUMMY(unusable)
    Iam = "MAPL_RecordIsAlarmRinging"

    MAPL_RecordAlarmIsRinging  = .false.
    if (present(MODE)) then
       mode_ = mode
       modePresent = .true.
    else
       mode_ = MAPL_Write2Disk
       modePresent = .false.
    end if

! ------------------
    if (associated(META%RECORD)) then

       RECORDLOOP: DO I = 1, size(META%RECORD%ALARM)
          if ( ESMF_AlarmIsRinging(META%RECORD%ALARM(I), RC=status) ) then
             _VERIFY(status)
             if (.not. modePresent) then
                MAPL_RecordAlarmIsRinging = .true.
                exit RECORDLOOP
             end if
             if (META%RECORD%FILETYPE(I) == mode_) then
                MAPL_RecordAlarmIsRinging = .true.
                exit RECORDLOOP
             end if
          end if
       end DO RECORDLOOP
    end if
    _RETURN(ESMF_SUCCESS)
  end function MAPL_RecordAlarmIsRinging

   recursive subroutine MAPL_GetAllExchangeGrids ( GC, LSADDR, RC )


     type(ESMF_GridComp),  intent(INOUT) :: GC         ! Gridded component
     integer(kind=INT64),            pointer       :: LSADDR(:)
     integer,              intent(  OUT) :: RC         ! Return code


     integer :: status

     type (MAPL_MetaComp),              pointer  :: MAPLOBJ 
     type (MAPL_LocStream)                       :: LocStream

     character(len=ESMF_MAXSTR)   :: CNAME
     integer(kind=INT64)                    :: ADDR
     integer                      :: I
     integer                      :: N
     integer(kind=INT64), pointer           :: TMP(:)
     logical                      :: found

     type(ESMF_GridComp), pointer :: gridcomp
! Retrieve the pointer to the internal state
! --------------------------------------------

     call MAPL_GetObjectFromGC ( GC, MAPLOBJ, RC=STATUS)
     _VERIFY(STATUS)
     
     LocStream = MAPLOBJ%LocStream
     call c_MAPL_LocStreamRetrievePtr(LocStream, ADDR)

     call ESMF_GridCompGet(GC, NAME = cname, rc=status)
     _VERIFY(STATUS)

     if (ADDR /= 0) then
        N = 0
        if (associated(LSADDR)) then
           N = SIZE(LSADDR)
        end if
     
        found = .false.
        do I = 1, N
           if (addr == LSADDR(I)) then
              found = .true.
              exit
           end if
        end do

        if (.not. found) then
           allocate(tmp(N+1), stat=status)
           _VERIFY(STATUS)
           if (N > 0) then
              tmp(1:N) = LSADDR
              deallocate(LSADDR)
           end if
           N = N + 1
           tmp(N) = addr
           LSADDR => TMP
        end if
     end if

        do I = 1, MAPLOBJ%get_num_children()
           gridcomp => MAPLOBJ%GET_CHILD_GRIDCOMP(I)
           call MAPL_GetAllExchangeGrids(gridcomp, LSADDR, RC=STATUS)
           _VERIFY(STATUS)
        end do
     
     _RETURN(ESMF_SUCCESS)
   end subroutine MAPL_GetAllExchangeGrids

   subroutine MAPL_DoNotAllocateImport(GC, NAME, notFoundOK, RC)
     type(ESMF_GridComp),  intent(INOUT) :: GC         ! Gridded component
     character(len=*),     intent(IN   ) :: NAME
     logical, optional,    intent(IN   ) :: notFoundOK
     integer, optional,    intent(  OUT) :: RC         ! Return code

     integer                       :: status

     type (MAPL_MetaComp), pointer :: MAPLOBJ 
     type (MAPL_VarSpec),  pointer :: SPEC(:) => null()

     call MAPL_GetObjectFromGC(GC, MAPLOBJ, RC=STATUS)
     _VERIFY(STATUS)

     call MAPL_Get (MAPLOBJ, ImportSpec=spec, RC=STATUS )
     _VERIFY(STATUS)

     if (associated(spec)) then
        call MAPL_DoNotAllocateVar(SPEC, NAME, notFoundOK, RC=STATUS)
        _VERIFY(STATUS)
     end if

     _RETURN(ESMF_SUCCESS)
   end subroutine MAPL_DoNotAllocateImport

   subroutine MAPL_DoNotAllocateInternal(GC, NAME, notFoundOK, RC)
     type(ESMF_GridComp),  intent(INOUT) :: GC         ! Gridded component
     character(len=*),     intent(IN   ) :: NAME
     logical, optional,    intent(IN   ) :: notFoundOK
     integer,              intent(  OUT) :: RC         ! Return code

     integer                       :: status

     type (MAPL_MetaComp), pointer :: MAPLOBJ 
     type (MAPL_VarSpec),  pointer :: SPEC(:)

     call MAPL_GetObjectFromGC(GC, MAPLOBJ, RC=STATUS)
     _VERIFY(STATUS)

     call MAPL_Get (MAPLOBJ, InternalSpec=spec, RC=STATUS )
     _VERIFY(STATUS)

     call MAPL_DoNotAllocateVar(SPEC, NAME, notFoundOK, RC=STATUS)
     _VERIFY(STATUS)

     _RETURN(ESMF_SUCCESS)
   end subroutine MAPL_DoNotAllocateInternal

   subroutine MAPL_DoNotAllocateVar(SPEC, NAME, notFoundOK, RC)
     type (MAPL_VarSpec),  pointer       :: SPEC(:)
     character(len=*),     intent(IN   ) :: NAME
     logical, optional,    intent(IN   ) :: notFoundOK
     integer, optional,    intent(  OUT) :: RC         ! Return code

     integer                       :: status

     integer                       :: I
     logical                       :: notFoundOK_

     notFoundOK_ = .false.
     if (present(notFoundOK)) then
        notFoundOK_ = notFoundOK
     end if
     I = MAPL_VarSpecGetIndex(Spec, NAME)
     if (I<=0 .and. notFoundOK_) then
        _RETURN(ESMF_SUCCESS)
     end if
        
     _ASSERT(I>0,'needs informative message') ! make sure NAME is in the SPEC

     call MAPL_VarSpecSet(SPEC(I), doNotAllocate = .true., RC=status )
     _VERIFY(STATUS)

     _RETURN(ESMF_SUCCESS)
   end subroutine MAPL_DoNotAllocateVar

   subroutine ArrDescrSetNCPar(ArrDes, MPL, tile, offset, num_readers, num_writers, rc)

     type(ArrDescr),                 intent(INOUT) :: ArrDes
     type(MAPL_MetaComp),            intent(INOUT) :: MPL
     logical, optional,              intent(in   ) :: tile
     integer(kind=MPI_OFFSET_KIND), &
                          optional,  intent(IN   ) :: offset
     integer, optional,              intent(IN   ) :: num_readers
     integer, optional,              intent(IN   ) :: num_writers
     integer, optional,              intent(  OUT) :: RC

     integer                       :: COUNTS(2),CCPD(3), km_world
     integer                       :: status
     integer(kind=MPI_OFFSET_KIND) :: offset_local
     logical                       :: tile_loc
     type(ESMF_Grid)               :: TILEGRID
     character(len=MPI_MAX_INFO_VAL) :: romio_cb_read,cb_buffer_size,romio_cb_write
 
     if (present(tile)) then
        tile_loc=tile
     else
        tile_loc=.false.
     endif

     if (present(offset)) then
        offset_local = offset
     else
        offset_local = 0
     endif
      
     if (tile_loc) then
        call MAPL_LocStreamGet(MPL%LocStream, TILEGRID=TILEGRID,RC=STATUS)
        _VERIFY(STATUS)
        call MAPL_GridGet(TILEGRID,globalCellCountPerDim=COUNTS,RC=STATUS)
        _VERIFY(STATUS)
        call ArrDescrSet(arrdes, offset_local, &
                    readers_comm  = mpl%grid%readers_comm,  &
                    ioscattercomm = mpl%grid%comm, &
                    writers_comm = mpl%grid%writers_comm, &
                    iogathercomm = mpl%grid%comm, &
                    i1 = mpl%grid%i1, in = mpl%grid%in,     &
                    j1 = mpl%grid%j1, jn = mpl%grid%jn,     &
                    im_world = COUNTS(1),                   &
                    jm_world = COUNTS(2))
        arrdes%ycomm = mpl%grid%Ycomm
        arrdes%xcomm = mpl%grid%Xcomm
        arrdes%NY0   = mpl%grid%NY0
        arrdes%NX0   = mpl%grid%NX0
        arrdes%tile=.true.
        arrdes%grid=tilegrid
     else
        call MAPL_GridGet(mpl%grid%ESMFGRID,globalCellCountPerDim=CCPD,RC=STATUS)
        _VERIFY(STATUS)
        km_world = CCPD(3)
        call ArrDescrSet(arrdes, offset_local, &
                    readers_comm  = mpl%grid%readers_comm,  &
                    ioscattercomm = mpl%grid%ioscattercomm, &
                    writers_comm = mpl%grid%writers_comm, &
                    iogathercomm = mpl%grid%iogathercomm, &
                    i1 = mpl%grid%i1, in = mpl%grid%in,     &
                    j1 = mpl%grid%j1, jn = mpl%grid%jn,     &
                    im_world = mpl%grid%im_world,           &
                    jm_world = mpl%grid%jm_world,           &
                    lm_world = km_world)
        arrdes%ycomm = mpl%grid%Ycomm
        arrdes%xcomm = mpl%grid%Xcomm
        arrdes%NY0   = mpl%grid%NY0
        arrdes%NX0   = mpl%grid%NX0
        arrdes%tile=.false.
        arrdes%grid=MPL%GRID%ESMFGRID
        call set_arrdes_by_face()
     endif
     call MAPL_GetResource(MPL, romio_cb_read, Label="ROMIO_CB_READ:", default="automatic", RC=STATUS)
     _VERIFY(STATUS)
     arrdes%romio_cb_read = romio_cb_read
     call MAPL_GetResource(MPL, romio_cb_write, Label="ROMIO_CB_WRITE:", default="enable", RC=STATUS)
     _VERIFY(STATUS)
     arrdes%romio_cb_write = romio_cb_write
     call MAPL_GetResource(MPL, cb_buffer_size, Label="CB_BUFFER_SIZE:", default="16777216", RC=STATUS)
     _VERIFY(STATUS)
     arrdes%cb_buffer_size = cb_buffer_size
     if (present(num_readers)) arrdes%num_readers=num_readers
     if (present(num_writers)) arrdes%num_writers=num_writers
     arrdes%write_restart_by_oserver = mpl%grid%write_restart_by_oserver 

     _RETURN(ESMF_SUCCESS)

     contains
        subroutine set_arrdes_by_face()
           integer :: color, key
           character(len=ESMF_MAXSTR)    :: Iam="ArrDescrSetNCPar_face"
           integer :: status
     
           if (mpl%grid%im_world /= mpl%grid%jm_world/6) return ! only apply to cube

           color = (arrdes%j1(mpl%grid%NY0)-1)/mpl%grid%im_world + 1
           arrdes%face_index = color
           key   = 1
           if ( mpl%grid%write_restart_by_face) then
              arrdes%write_restart_by_face = .true.    
              if (mpl%grid%writers_comm /= MPI_COMM_NULL) then
                 call MPI_COMM_SPLIT( mpl%grid%writers_comm,color,key,arrdes%face_writers_comm, STATUS)
                 _VERIFY(STATUS) 
              endif
           endif

           if ( mpl%grid%read_restart_by_face) then
              arrdes%read_restart_by_face = .true.    
              if (mpl%grid%readers_comm /= MPI_COMM_NULL) then
                 call MPI_COMM_SPLIT( mpl%grid%readers_comm,color,key,arrdes%face_readers_comm, STATUS)
                 _VERIFY(STATUS) 
              endif
           endif

        end subroutine set_arrdes_by_face

   end subroutine ArrDescrSetNCPar

   subroutine MAPL_GetLogger_meta(meta, lgr, rc)
      type (MAPL_MetaComp), intent(in) :: meta
      class(Logger), pointer :: lgr
      integer, optional, intent(out) :: rc

!!$      class(Logger), pointer :: meta_lgr
!!$
!!$      meta_lgr => logging%get_logger('MAPL.GENERIC')
!!$      call meta_lgr%warning('obsolete interface MAPL_GetLogger()')

      lgr => meta%get_logger()

      _RETURN(_SUCCESS)
   end subroutine MAPL_GetLogger_meta

   subroutine MAPL_GetLogger_gc(gc, lgr, rc)
      type(ESMF_GridComp), intent(inout) :: gc
      class(Logger), pointer :: lgr
      integer, optional, intent(out) :: rc
      type (MAPL_MetaComp), pointer :: meta

      integer :: status
      
      call MAPL_GetObjectFromGC(gc, meta, rc=status)
      _VERIFY(status)

      lgr => meta%get_logger()

      _RETURN(_SUCCESS)
   end subroutine MAPL_GetLogger_gc

   subroutine MAPL_ConnectivityGet(gc, connectivityPtr, RC)
      type(ESMF_GridComp), intent(inout) :: gc
      integer, optional, intent(out) :: rc
      type (MAPL_Connectivity), pointer :: connectivityPtr

      type (MAPL_MetaComp), pointer :: meta
      integer                       :: status
      
      call MAPL_GetObjectFromGC(gc, meta, rc=status)
      _VERIFY(status)

      connectivityPtr => meta%connectList
      
      _RETURN(_SUCCESS)
   end subroutine MAPL_ConnectivityGet


   ! Type-bound procedures

   function get_ith_child(this, i) result(child)
      class(MaplGenericComponent), pointer :: child
      class(MAPL_MetaComp), target, intent(in) :: this
      integer, intent(in) :: i

      class(AbstractFrameworkComponent), pointer :: child_node

      child_node => this%get_child(trim(this%GCnamelist(i)))
      select type (child_node)
      class is (MaplGenericComponent)
         child => child_node
      end select

   end function get_ith_child



   function get_child_gridcomp(this, i) result(gridcomp)
      type(ESMF_GridComp), pointer :: gridcomp
      class(MAPL_MetaComp), target, intent(in) :: this
      integer, intent(in) :: i

      class(MaplGenericComponent), pointer :: child
      
      child => this%get_ith_child(i)
      gridcomp => child%gridcomp
      
   end function get_child_gridcomp

   function get_child_import_state(this, i) result(state)
      type(ESMF_State), pointer :: state
      class(MAPL_MetaComp), target, intent(in) :: this
      integer, intent(in) :: i

      class(MaplGenericComponent), pointer :: child
      
      child => this%get_ith_child(i)
      state => child%import_state

   end function get_child_import_state

   function get_child_export_state(this, i) result(state)
      type(ESMF_State), pointer :: state
      class(MAPL_MetaComp), target, intent(in) :: this
      integer, intent(in) :: i

      class(MaplGenericComponent), pointer :: child
      
      child => this%get_ith_child(i)
      state => child%export_state

   end function get_child_export_state

      
end module MAPL_GenericMod
