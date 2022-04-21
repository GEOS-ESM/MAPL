#include "MAPL_Exceptions.h"
#include "MAPL_Generic.h"
#include "unused_dummy.H"

!-------------------------------------------------------------------------
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 910.1     !
!-------------------------------------------------------------------------

   MODULE MAPL_ExtDataGridComp2G

!BOP
! !MODULE: MAPL_ExtDataGridCompMod - Implements Interface to External Data
!
! !DESCRIPTION: 
!
!  {\tt MAPL\_ExtDataGridComp} is an ESMF gridded component implementing
!  an interface to boundary conditions and other types of external data
!  files.
!
!  Developed for GEOS-5 release Fortuna 2.0 and later.
!
! !USES:
!
   USE ESMF
   use gFTL_StringVector
   use gFTL_IntegerVector
   use MAPL_BaseMod
   use MAPL_CommsMod
   use MAPL_ShmemMod
   use ESMFL_Mod
   use MAPL_GenericMod
   use MAPL_VarSpecMod
   use MAPL_CFIOMod
   use MAPL_NewArthParserMod
   use MAPL_ConstantsMod, only: MAPL_PI,MAPL_PI_R8,MAPL_RADIANS_TO_DEGREES
   use MAPL_IOMod, only: MAPL_NCIOParseTimeUnits
   use, intrinsic :: iso_fortran_env, only: REAL64
   use linearVerticalInterpolation_mod
   use ESMF_CFIOCollectionVectorMod
   use ESMF_CFIOCollectionMod
   use MAPL_ConfigMod
   use MAPL_GridManagerMod
   use MAPL_ExtDataNG_IOBundleMod
   use MAPL_ExtDataNG_IOBundleVectorMod
   use MAPL_ExceptionHandling
   use MAPL_DataCollectionMod
   use MAPL_CollectionVectorMod
   use MAPL_DataCollectionManagerMod
   use MAPL_FileMetadataUtilsMod
   use pFIO_ClientManagerMod, only : i_Clients
   use MAPL_GriddedIOItemMod
   use MAPL_GriddedIOItemVectorMod
   use MAPL_ExtDataConfig
   use MAPL_ExtDataTypeDef
   use MAPL_ExtDataOldTypesCreator
   use MAPL_StringTemplate
   use pflogger, only: logging, Logger
   use MAPL_ExtDataLogger
   use MAPL_ExtDataConstants

   IMPLICIT NONE
   PRIVATE
!
! !PUBLIC MEMBER FUNCTIONS:

   PUBLIC SetServices
!EOP
!
! !REVISION HISTORY:
!
!  12Dec2009  da Silva  Design and first implementation.
!
!-------------------------------------------------------------------------

  integer, parameter         :: MAPL_ExtDataLeft          = 1
  integer, parameter         :: MAPL_ExtDataRight         = 2
  logical                    :: hasRun
  character(len=ESMF_MAXSTR) :: error_msg_str

  type PrimaryExports
     PRIVATE
     integer :: nItems = 0
     type(integerVector) :: export_id_start
     type(integerVector) :: number_of_rules
     type(stringVector)  :: import_names
     type(PrimaryExport), pointer :: item(:) => null()
     contains 
        procedure :: get_item_index 
  end type PrimaryExports

  type DerivedExports
     PRIVATE
     integer :: nItems = 0
     type(stringVector)  :: import_names
     type(DerivedExport), pointer :: item(:) => null()
  end type DerivedExports

! Legacy state
! ------------
  type MAPL_ExtData_State
     PRIVATE
     type(PrimaryExports) :: Primary
     type(DerivedExports) :: Derived
     ! will add fields from export state to this state
     ! will also add new fields that could be mask
     ! or primary exports that were not in the export
     ! state recieved by ExtData, i.e. fields that are
     ! needed by a derived field where the primary fields
     ! are not actually required
     type(ESMF_State)     :: ExtDataState
     type(ESMF_Config)    :: CF
     logical              :: active
  end type MAPL_ExtData_State

! Hook for the ESMF
! -----------------
  type MAPL_ExtData_Wrap
     type (MAPL_ExtData_State), pointer :: PTR => null()
  end type MAPL_ExtData_WRAP

CONTAINS


!-------------------------------------------------------------------------
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 910.1     !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE: SetServices --- Sets IRF services for the MAPL_ExtData
!
! !INTERFACE:

   SUBROUTINE SetServices ( GC, RC )

! !ARGUMENTS:

    type(ESMF_GridComp), intent(INOUT) :: GC  ! gridded component
    integer, optional                  :: RC  ! return code

! !DESCRIPTION: Sets Initialize, Run and Finalize services. 
!
! !REVISION HISTORY:
!
!  12Dec2009  da Silva  Design and first implementation.
!
!EOP
!-------------------------------------------------------------------------

!   Local derived type aliases
!   --------------------------
    type (MAPL_ExtData_State), pointer  :: self   ! internal, that is
    type (MAPL_ExtData_wrap)            :: wrap

    character(len=ESMF_MAXSTR)          :: comp_name
    character(len=ESMF_MAXSTR)          :: Iam
    integer                             :: status

!                              ------------

!   Get my name and set-up traceback handle
!   ---------------------------------------
    Iam = 'SetServices'
    call ESMF_GridCompGet( GC, name=comp_name, __RC__ )
    Iam = trim(comp_name) // '::' // trim(Iam)

!   Wrap internal state for storing in GC; rename legacyState
!   -------------------------------------
    allocate ( self, stat=STATUS )
    _VERIFY(STATUS)
    wrap%ptr => self
 
!                       ------------------------
!                       ESMF Functional Services
!                       ------------------------

!   Set the Initialize, Run, Finalize entry points
!   ----------------------------------------------
    call MAPL_GridCompSetEntryPoint ( GC, ESMF_METHOD_INITIALIZE,  Initialize_, __RC__ )
    call MAPL_GridCompSetEntryPoint ( GC, ESMF_METHOD_RUN,   Run_,        __RC__ )
    call MAPL_GridCompSetEntryPoint ( GC, ESMF_METHOD_FINALIZE, Finalize_,   __RC__ )
        
!   Store internal state in GC
!   --------------------------
    call ESMF_UserCompSetInternalState ( GC, 'MAPL_ExtData_state', wrap, STATUS )
    _VERIFY(STATUS)
  
    call MAPL_TimerAdd(gc,name="Initialize", rc=status)
    _VERIFY(STATUS)
    call MAPL_TimerAdd(gc,name="Run", rc=status)
    _VERIFY(STATUS)
    call MAPL_TimerAdd(gc,name="-Read_Loop", rc=status)
    _VERIFY(STATUS)
    call MAPL_TimerAdd(gc,name="--CheckUpd", rc=status)
    _VERIFY(STATUS)
    call MAPL_TimerAdd(gc,name="--Read", rc=status)
    _VERIFY(STATUS)
    call MAPL_TimerAdd(gc,name="--GridCreate", rc=status)
    _VERIFY(STATUS)
    call MAPL_TimerAdd(gc,name="--IclientWait", rc=status)
    _VERIFY(STATUS)
    call MAPL_TimerAdd(gc,name="--PRead", rc=status)
    _VERIFY(STATUS)
    call MAPL_TimerAdd(gc,name="---CreateCFIO", rc=status)
    _VERIFY(STATUS)
    call MAPL_TimerAdd(gc,name="---prefetch", rc=status)
    _VERIFY(STATUS)
    call MAPL_TimerAdd(gc,name="----add-collection", rc=status)
    _VERIFY(STATUS)
    call MAPL_TimerAdd(gc,name="----make-reference", rc=status)
    _VERIFY(STATUS)
    call MAPL_TimerAdd(gc,name="----RegridStore", rc=status)
    _VERIFY(STATUS)
    call MAPL_TimerAdd(gc,name="----request", rc=status)
    _VERIFY(STATUS)
    call MAPL_TimerAdd(gc,name="---IclientDone", rc=status)
    _VERIFY(STATUS)
    call MAPL_TimerAdd(gc,name="----RegridApply", rc=status)
    _VERIFY(STATUS)
    call MAPL_TimerAdd(gc,name="---read-prefetch", rc=status)
    _VERIFY(STATUS)
    call MAPL_TimerAdd(gc,name="--Swap", rc=status)
    _VERIFY(STATUS)
    call MAPL_TimerAdd(gc,name="--Bracket", rc=status)
    _VERIFY(STATUS)
    call MAPL_TimerAdd(gc,name="-Interpolate", rc=status)
    _VERIFY(STATUS)
!   Generic Set Services
!   --------------------
    call MAPL_GenericSetServices ( GC, __RC__ )

!   All done
!   --------

    _RETURN(ESMF_SUCCESS)

  END SUBROUTINE SetServices


!-------------------------------------------------------------------------
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 610.1     !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  Initialize_ --- Initialize MAPL_ExtData
!
! !INTERFACE:
!

   SUBROUTINE Initialize_ ( GC, IMPORT, EXPORT, CLOCK, rc )

! !USES:

   implicit NONE

! !INPUT PARAMETERS:

   type(ESMF_Clock),  intent(inout)   :: CLOCK   ! The clock

! !OUTPUT PARAMETERS:

   type(ESMF_GridComp), intent(inout) :: GC      ! Grid Component
   type(ESMF_State), intent(inout)    :: IMPORT  ! Import State
   type(ESMF_State), intent(inout)    :: EXPORT  ! Export State
   integer, intent(out)               :: rc      ! Error return code:
                                                 !  0 - all is well
                                                 !  1 - 

! !DESCRIPTION: This is a simple ESMF wrapper.
!
! !REVISION HISTORY:
!
!  12Dec2009  da Silva  Design and first implementation.
!
!EOP
!-------------------------------------------------------------------------

   type(MAPL_ExtData_state), pointer :: self        ! Legacy state
   type(ESMF_Config)                 :: CF_master          ! Universal Config 

   character(len=ESMF_MAXSTR)        :: comp_name
   character(len=ESMF_MAXSTR)        :: Iam
   integer                           :: Status

   type(PrimaryExport), pointer      :: item
   integer                           :: i,j
   integer                           :: ItemCount
   integer                           :: PrimaryItemCount, DerivedItemCount

   type(ESMF_Time)                   :: time

   type (ESMF_Field)                 :: field
   type (ESMF_StateItem_Flag), pointer    :: ITEMTYPES(:)
   character(len=ESMF_MAXSTR), allocatable   :: ITEMNAMES(:)

   integer           :: idx
   type(MAPL_MetaComp),pointer :: MAPLSTATE

   type(ExtDataOldTypesCreator),target :: config_yaml
   character(len=ESMF_MAXSTR) :: new_rc_file
   logical :: found_in_config
   integer :: num_primary,num_derived,num_rules
   integer :: item_type
   type(StringVector) :: unsatisfied_imports,extra_variables_needed
   type(StringVectorIterator) :: siter
   character(len=:), pointer :: current_base_name,extra_var
   character(len=:), allocatable :: primary_var_name,derived_var_name
   type(ESMF_Time), allocatable :: time_ranges(:)
   character(len=1) :: sidx
   type(ESMF_VM) :: vm
   type(ESMF_Field) :: new_field,existing_field
   type(ESMF_StateItem_Flag) :: state_item_type
   !class(logger), pointer :: lgr

!  Get my name and set-up traceback handle
!  ---------------------------------------
   Iam = 'Initialize_'
   call ESMF_GridCompGet( GC, name=comp_name, config=CF_master, vm=vm, __RC__ )
   Iam = trim(comp_name) // '::' // trim(Iam)
   call MAPL_GetLogger(gc, extdata_lgr, __RC__)

!  Extract relevant runtime information
!  ------------------------------------
   call extract_ ( GC, self, CF_master, __RC__)
   self%CF = CF_master

!  Start Some Timers
!  -----------------
   call MAPL_GetObjectFromGC ( gc, MAPLSTATE, RC=STATUS)
   _VERIFY(STATUS) 
   call MAPL_TimerOn(MAPLSTATE,"TOTAL")
   call MAPL_TimerOn(MAPLSTATE,"Initialize")

   call ESMF_ConfigGetAttribute(cf_master,new_rc_file,label="EXTDATA_YAML_FILE:",default="extdata.yaml",_RC)
   self%active = am_i_running(new_rc_file)

   call ESMF_ClockGet(CLOCK, currTIME=time, __RC__)
! Get information from export state
!----------------------------------
    call ESMF_StateGet(EXPORT, ITEMCOUNT=ItemCount, RC=STATUS)
    _VERIFY(STATUS)

    ! no need to run ExtData if there are no imports to fill
    if (ItemCount == 0) then
       self%active = .false.
    end if

    if (.not.self%active) then
       call MAPL_TimerOff(MAPLSTATE,"Initialize")
       call MAPL_TimerOff(MAPLSTATE,"TOTAL")
       _RETURN(ESMF_SUCCESS)
    end if

    config_yaml = ExtDataOldTypesCreator(new_rc_file,time,__RC__)
!   Greetings
!   ---------
    if (MAPL_am_I_root()) then
         print *, TRIM(Iam)//': ACTIVE'
         print *
    end if

    allocate(ITEMNAMES(ITEMCOUNT), STAT=STATUS)
    _VERIFY(STATUS)
    allocate(ITEMTYPES(ITEMCOUNT), STAT=STATUS)
    _VERIFY(STATUS)

    call ESMF_StateGet(EXPORT, ITEMNAMELIST=ITEMNAMES, &
                       ITEMTYPELIST=ITEMTYPES, RC=STATUS)
    _VERIFY(STATUS)

!                               --------
!  Initialize MAPL Generic
!  -----------------------
   call MAPL_GenericInitialize ( GC, IMPORT, EXPORT, clock,  __RC__ )

   call extdata_lgr%info("Using ExtData2G, note this is still in BETA stage")

!                         ---------------------------
!                         Parse ExtData Resource File
!                         ---------------------------
   self%ExtDataState = ESMF_StateCreate(Name="ExtDataNameSpace",__RC__)
   num_primary=0
   num_derived=0
   primaryitemcount=0
   deriveditemcount=0
   do i=1,size(itemnames)
      item_type = config_yaml%get_item_type(trim(itemnames(i)),rc=status)
      _VERIFY(status)
      found_in_config = (item_type/= ExtData_not_found)
      if (.not.found_in_config) call unsatisfied_imports%push_back(itemnames(i))
      if (item_type == derived_type) then
         call self%derived%import_names%push_back(trim(itemnames(i)))
         deriveditemcount=deriveditemcount+1
      else if (item_type==Primary_Type_Scalar .or. item_type==Primary_Type_Vector_comp1) then
         call self%primary%import_names%push_back(trim(itemnames(i)))
         primaryitemcount=primaryitemcount+config_yaml%count_rules_for_item(trim(itemnames(i)),_RC)
      end if
   enddo
   extra_variables_needed = config_yaml%get_extra_derived_items(self%primary%import_names,self%derived%import_names,_RC)
   siter = extra_variables_needed%begin() 
   do while(siter/=extra_variables_needed%end())
      extra_var => siter%get()
      idx = index(extra_var,",")
      primary_var_name = extra_var(:idx-1) 
      derived_var_name = extra_var(idx+1:)
      call self%primary%import_names%push_back(primary_var_name)
      primaryItemCount=primaryItemCount+config_yaml%count_rules_for_item(primary_var_name,_RC)
      call ESMF_StateGet(self%ExtDataState,primary_var_name,state_item_type,_RC)
      if (state_item_type == ESMF_STATEITEM_NOTFOUND) then
         call ESMF_StateGet(export,derived_var_name,existing_field,_RC)
         new_field = MAPL_FieldCreate(existing_field,primary_var_name,doCOpy=.true.,_RC)
         call MAPL_StateAdd(self%ExtDataState,new_field,__RC__)
      end if
      call siter%next()
   enddo 
   call ESMF_VMBarrier(vm,_RC)
   if (unsatisfied_imports%size() > 0) then
      do i=1,unsatisfied_imports%size()
         call extdata_lgr%error("In ExtData resource file, could not find: "//trim(unsatisfied_imports%at(i)))
      enddo
      _FAIL("Unsatisfied imports in ExtData")
   end if
      
   allocate(self%primary%item(PrimaryItemCount),__STAT__)
   allocate(self%derived%item(DerivedItemCount),__STAT__)
   self%primary%nitems = PrimaryItemCount
   self%derived%nitems = DerivedItemCount

   num_primary=0
   num_derived=0 
   do i=1,self%primary%import_names%size()
      current_base_name => self%primary%import_names%at(i)
      num_rules = config_yaml%count_rules_for_item(current_base_name)
      _ASSERT(num_rules > 0,"no rule found for "//trim(current_base_name))
      call self%primary%number_of_rules%push_back(num_rules)
      call self%primary%export_id_start%push_back(num_primary+1)
      if (num_rules > 1) then
         if (allocated(time_ranges)) deallocate(time_ranges)
         allocate(time_ranges(num_rules))
         time_ranges = config_yaml%get_time_range(current_base_name,_RC)
         do j=1,num_rules
            num_primary=num_primary+1
            write(sidx,'(I1)')j
            call config_yaml%fillin_primary(current_base_name//"+"//sidx,current_base_name,self%primary%item(num_primary),time,clock,__RC__)
            allocate(self%primary%item(num_primary)%start_end_time(2))
            self%primary%item(num_primary)%start_end_time(1)=time_ranges(j)
            self%primary%item(num_primary)%start_end_time(2)=time_ranges(j+1)
         enddo
      else
         num_primary=num_primary+1
         call config_yaml%fillin_primary(current_base_name,current_base_name,self%primary%item(num_primary),time,clock,__RC__)
      end if
      call ESMF_StateGet(Export,current_base_name,state_item_type,_RC)
      if (state_item_type /= ESMF_STATEITEM_NOTFOUND) then
         call ESMF_StateGet(Export,current_base_name,field,__RC__)
         call MAPL_StateAdd(self%ExtDataState,field,__RC__)
         item_type = config_yaml%get_item_type(current_base_name)
         if (item_type == Primary_Type_Vector_comp1) then
            call ESMF_StateGet(Export,self%primary%item(num_primary)%vcomp2,field,_RC)
            call MAPL_StateAdd(self%ExtDataState,field,_RC)
         end if
      end if
   enddo
   do i=1,self%derived%import_names%size()
      current_base_name => self%derived%import_names%at(i)
      num_derived=num_derived+1
      call config_yaml%fillin_derived(current_base_name,self%derived%item(num_derived),time,clock,__RC__)
      call ESMF_StateGet(Export,current_base_name,field,__RC__)
      call MAPL_StateAdd(self%ExtDataState,field,__RC__)
   enddo
   
   PrimaryLoop: do i=1,self%primary%import_names%size()

      current_base_name => self%primary%import_names%at(i)
      idx = self%primary%get_item_index(current_base_name,time,_RC)
      item => self%primary%item(idx)
      item%initialized = .true.

      item%pfioCollection_id = MAPL_DataAddCollection(item%file_template)
      if (item%isConst) then
         call set_constant_field(item,self%extDataState,_RC)
         cycle
      end if
      call create_bracketing_fields(item,self%ExtDataState,cf_master,_RC) 

   end do PrimaryLoop

! Check if we have any files that would need to be vertically interpolated
! if so ensure that PS is done first
!!  check for PS
   !idx = -1
   !if (any(self%primary%item%do_VertInterp .eqv. .true.)) then
      !do i=1,size(self%primary%item)
         !if (self%primary%item(i)%name=='PS') then
            !idx =i
         !end if
      !enddo
      !_ASSERT(idx/=-1,'Surface pressure not present for vertical interpolation')
      !self%primary%item(idx)%units = ESMF_UtilStringUppercase(self%primary%item(idx)%units,rc=status)
      !_ASSERT(trim(self%primary%item(idx)%units)=="PA",'PS must be in units of PA')
   !end if

   call extdata_lgr%info('*******************************************************')
   call extdata_lgr%info('** Variables to be provided by the ExtData Component **')
   call extdata_lgr%info('*******************************************************')
   do i = 1, ItemCount
      call extdata_lgr%info('---- %i0.5~: %a', i, trim(ItemNames(i)))
   end do
   call extdata_lgr%info('*******************************************************\n')

! Clean up
! --------
   deallocate(ItemTypes)
   deallocate(ItemNames)

!  Set has run to false to we know when we first go to run method it is first call
   hasRun = .false.

   call MAPL_TimerOff(MAPLSTATE,"Initialize")
   call MAPL_TimerOff(MAPLSTATE,"TOTAL")
!  All done
!  --------

   call extdata_lgr%debug('ExtData Initialize_(): End')

   _RETURN(ESMF_SUCCESS)

   END SUBROUTINE Initialize_


!-------------------------------------------------------------------------
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 610.1     !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  Run_ --- Runs MAPL_ExtData
!
! !INTERFACE:
!

   SUBROUTINE Run_ ( GC, IMPORT, EXPORT, CLOCK, rc )

! !USES:

  implicit NONE

! !INPUT PARAMETERS:

   type(ESMF_Clock),  intent(inout) :: CLOCK     ! The clock

! !OUTPUT PARAMETERS:

   type(ESMF_GridComp), intent(inout)  :: GC     ! Grid Component
   type(ESMF_State), intent(inout) :: IMPORT     ! Import State
   type(ESMF_State), intent(inout) :: EXPORT     ! Export State
   integer, intent(out) ::  rc                   ! Error return code:
                                                 !  0 - all is well
                                                 !  1 - 

! !DESCRIPTION: This is a simple ESMF wrapper.
!
! !REVISION HISTORY:
!
!  12Dec2009  da Silva  Design and first implementation.
!
!EOP
!-------------------------------------------------------------------------

   type(MAPL_ExtData_state), pointer :: self        ! Legacy state
   type(ESMF_Config)                 :: CF          ! Universal Config 

   character(len=ESMF_MAXSTR)        :: comp_name
   character(len=ESMF_MAXSTR)        :: Iam
   integer                           :: status

   type(PrimaryExport), pointer      :: item
   type(DerivedExport), pointer      :: derivedItem
   integer                           :: i

   type(ESMF_Time)                   :: time, time0
   type(MAPL_MetaComp), pointer      :: MAPLSTATE

   logical                           :: doUpdate_
   character(len=ESMF_MAXPATHLEN)    :: file_processed
   logical, allocatable              :: doUpdate(:)
   type(ESMF_Time), allocatable      :: useTime(:)

   integer                           :: bracket_side
   integer                           :: entry_num
   type(IOBundleNGVector), target     :: IOBundles
   type(IOBundleNGVectorIterator) :: bundle_iter
   type(ExtDataNG_IOBundle), pointer :: io_bundle
   character(len=:), pointer :: current_base_name
   integer :: idx,nitems
   type(ESMF_Config) :: cf_master

   _UNUSED_DUMMY(IMPORT)
   _UNUSED_DUMMY(EXPORT)

!  Declare pointers to IMPORT/EXPORT/INTERNAL states 
!  -------------------------------------------------
!  #include "MAPL_ExtData_DeclarePointer___.h"
  
!  Get my name and set-up traceback handle
!  ---------------------------------------
   Iam = 'Run_'
   call ESMF_GridCompGet( GC, name=comp_name, config=CF_master, __RC__ )
   Iam = trim(comp_name) // '::' // trim(Iam)

!  Extract relevant runtime information
!  ------------------------------------
   call extract_ ( GC, self, CF, __RC__ )

   if (.not. self%active) then
      _RETURN(ESMF_SUCCESS)
   end if

   call MAPL_GetObjectFromGC ( gc, MAPLSTATE, RC=STATUS)
   _VERIFY(STATUS) 
   call MAPL_TimerOn(MAPLSTATE,"TOTAL")
   call MAPL_TimerOn(MAPLSTATE,"Run")

   call ESMF_ClockGet(CLOCK, currTIME=time0, __RC__)

!  Fill in the internal state with data from the files 
!  ---------------------------------------------------

   allocate(doUpdate(self%primary%nitems),stat=status)
   _VERIFY(STATUS)
   doUpdate = .false.
   allocate(useTime(self%primary%nitems),stat=status)
   _VERIFY(STATUS)

   call MAPL_TimerOn(MAPLSTATE,"-Read_Loop")

   call extdata_lgr%debug('ExtData Rune_(): Start')
   call extdata_lgr%debug('ExtData Run_(): READ_LOOP: Start')
 
   READ_LOOP: do i=1,self%primary%import_names%size()

      current_base_name => self%primary%import_names%at(i)
      idx = self%primary%get_item_index(current_base_name,time0,_RC)
      item => self%primary%item(idx)
      if (.not.item%initialized) then
         item%pfioCollection_id = MAPL_DataAddCollection(item%file_template)
         if (item%isConst) then
            call set_constant_field(item,self%extDataState,_RC)
            cycle
         end if
         call create_bracketing_fields(item,self%ExtDataState,cf_master, _RC)
         item%initialized=.true.
      end if

      nitems = self%primary%import_names%size()
      !call extdata_lgr%debug('ExtData Run_(): READ_LOOP: variable %i0 of %i0~: %a', i, nitems, trim(current_base_name))
      !call extdata_lgr%debug('   ==> file: %a', trim(item%file_template))
      !call extdata_lgr%debug('   ==> isConst:: %l1', item%isConst)

      if (item%isConst) then
         call extdata_lgr%debug('   ==> Break loop since isConst is true')
         cycle
      endif

      call MAPL_TimerOn(MAPLSTATE,"--CheckUpd")

      call item%update_freq%check_update(doUpdate(i),time,time0,.not.hasRun,__RC__)
      call MAPL_TimerOff(MAPLSTATE,"--CheckUpd")

      DO_UPDATE: if (doUpdate(i)) then

         !call extdata_lgr%info('Going to update %a with file template: %a ',current_base_name, item%file_template) 
         call item%modelGridFields%comp1%reset()
         call item%filestream%get_file_bracket(time,item%source_time, item%modelGridFields%comp1,__RC__)
         if (item%vartype == MAPL_VectorField) then
            call item%filestream%get_file_bracket(time,item%source_time, item%modelGridFields%comp2,__RC__)
         end if
         call IOBundle_Add_Entry(IOBundles,item,idx)
         useTime(i)=time

      end if DO_UPDATE

   end do READ_LOOP

   call extdata_lgr%debug('ExtData Run_: READ_LOOP: Done')

   bundle_iter = IOBundles%begin()
   do while (bundle_iter /= IoBundles%end())
      io_bundle => bundle_iter%get()
      bracket_side = io_bundle%bracket_side
      entry_num = io_bundle%entry_index
      file_Processed = io_bundle%file_name
      item => self%primary%item(entry_num)

      io_bundle%pbundle = ESMF_FieldBundleCreate(rc=status)
      _VERIFY(STATUS)

      call MAPL_ExtDataPopulateBundle(item,bracket_side,io_bundle%pbundle,rc=status)
      _VERIFY(status)
      call bundle_iter%next()
   enddo

   call MAPL_TimerOn(MAPLSTATE,"--PRead")
   call MAPL_TimerOn(MAPLSTATE,"---CreateCFIO")
   call MAPL_ExtDataCreateCFIO(IOBundles, rc=status)
   _VERIFY(status)
   call MAPL_TimerOff(MAPLSTATE,"---CreateCFIO")

   call MAPL_TimerOn(MAPLSTATE,"---prefetch")
   call MAPL_ExtDataPrefetch(IOBundles, rc=status)
   _VERIFY(status)
   call MAPL_TimerOff(MAPLSTATE,"---prefetch")
   _VERIFY(STATUS)
   call MAPL_TimerOn(MAPLSTATE,"---IclientDone")

   call i_Clients%done_collective_prefetch()
   call i_Clients%wait()

   call MAPL_TimerOff(MAPLSTATE,"---IclientDone")
   _VERIFY(STATUS)
  
   call MAPL_TimerOn(MAPLSTATE,"---read-prefetch")
   call MAPL_ExtDataReadPrefetch(IOBundles,rc=status) 
   _VERIFY(status)
   call MAPL_TimerOff(MAPLSTATE,"---read-prefetch")
   call MAPL_TimerOff(MAPLSTATE,"--PRead")

   bundle_iter = IOBundles%begin()
   do while (bundle_iter /= IOBundles%end())
      io_bundle => bundle_iter%get()
      bracket_side = io_bundle%bracket_side
      entry_num = io_bundle%entry_index
      item => self%primary%item(entry_num)
      call MAPL_ExtDataVerticalInterpolate(self,item,bracket_side,time0,rc=status)
      _VERIFY(status)
      call bundle_iter%next()
   enddo
   call MAPL_ExtDataDestroyCFIO(IOBundles,rc=status)
   _VERIFY(status)

   call MAPL_TimerOff(MAPLSTATE,"-Read_Loop")

   call MAPL_TimerOn(MAPLSTATE,"-Interpolate")

   call extdata_lgr%debug('ExtData Run_: INTERP_LOOP: Start') 

   INTERP_LOOP: do i=1,self%primary%import_names%size()

      current_base_name => self%primary%import_names%at(i)
      idx = self%primary%get_item_index(current_base_name,time0,_RC)
      item => self%primary%item(idx)

      if (doUpdate(i)) then

         call extdata_lgr%debug('ExtData Run_: INTERP_LOOP: interpolating between bracket times, variable: %a, file: %a', &
              & trim(current_base_name), trim(item%file_template))
        
         call MAPL_ExtDataInterpField(item,self%ExtDataState,useTime(i),__RC__)

      endif

      nullify(item) 

   end do INTERP_LOOP

   call extdata_lgr%debug('ExtData Run_: INTERP_LOOP: Done')

   call MAPL_TimerOff(MAPLSTATE,"-Interpolate")

   ! now take care of derived fields
   do i=1,self%derived%nItems

      derivedItem => self%derived%item(i)

      call derivedItem%update_freq%check_update(doUpdate_,time,time0,.not.hasRun,__RC__)

      if (doUpdate_) then

         call derivedItem%evaluate_derived_field(self%ExtDataState,_RC)

      end if

   end do

   call extdata_lgr%debug('ExtData Run_: End')

!  All done
!  --------
   deallocate(doUpdate)
   deallocate(useTime)

   if (hasRun .eqv. .false.) hasRun = .true.
   call MAPL_TimerOff(MAPLSTATE,"Run")
   call MAPL_TimerOff(MAPLSTATE,"TOTAL")

   _RETURN(ESMF_SUCCESS)

   END SUBROUTINE Run_

!-------------------------------------------------------------------------
!     NASA/GSFC, Global Modeling and Assimilation Office, Code 610.1     !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  Finalize_ --- Finalize MAPL_ExtData
!
! !INTERFACE:
!

   SUBROUTINE Finalize_ ( GC, IMPORT, EXPORT, CLOCK, rc )

! !USES:

  implicit NONE

! !INPUT PARAMETERS:

   type(ESMF_Clock),  intent(inout) :: CLOCK      ! The clock

! !OUTPUT PARAMETERS:

   type(ESMF_GridComp), intent(inout)  :: GC     ! Grid Component
   type(ESMF_State), intent(inout) :: IMPORT     ! Import State
   type(ESMF_State), intent(inout) :: EXPORT     ! Export State
   integer, intent(out) ::  rc                   ! Error return code:
                                                 !  0 - all is well
                                                 !  1 - 

! !DESCRIPTION: This is a simple ESMF wrapper.
!
! !REVISION HISTORY:
!
!  12Dec2009  da Silva  Design and first implementation.
!
!EOP
!-------------------------------------------------------------------------

   type(MAPL_ExtData_state), pointer :: self        ! Legacy state
   type(ESMF_Config)                 :: CF          ! Universal Config 

   character(len=ESMF_MAXSTR)        :: comp_name
   character(len=ESMF_MAXSTR)        :: Iam
   integer                           :: status


!  Get my name and set-up traceback handle
!  ---------------------------------------
   Iam = 'Finalize_'
   call ESMF_GridCompGet( GC, name=comp_name, __RC__ )
   Iam = trim(comp_name) // trim(Iam)

!  Finalize MAPL Generic
!  ---------------------
   call MAPL_GenericFinalize ( GC, IMPORT, EXPORT, CLOCK,  __RC__ )

!  Extract relevant runtime information
!  ------------------------------------
   call extract_ ( GC, self, CF, __RC__)

!  Free the memory used to hold the primary export items
!  -----------------------------------------------------
   if (associated(self%primary%item)) then
      deallocate(self%primary%item)
   end if


!  All done
!  --------
   _RETURN(ESMF_SUCCESS)

 end SUBROUTINE Finalize_

!.......................................................................

 subroutine extract_ ( GC, self, CF, rc)

    type(ESMF_GridComp), intent(INout)  :: GC           ! Grid Comp object

    type(MAPL_ExtData_state), pointer   :: self         ! Legacy state
    type(ESMF_Config),   intent(out)    :: CF           ! Universal Config 

    integer, intent(out), optional      :: rc

!                            ---

    character(len=ESMF_MAXSTR) :: comp_name
    character(len=ESMF_MAXSTR) :: Iam
    integer                    :: status

    type(MAPL_ExtData_Wrap)  :: wrap

!   Get my name and set-up traceback handle
!   ---------------------------------------
    Iam = 'extract_'
    call ESMF_GridCompGet( GC, NAME=comp_name, __RC__ )
    Iam = trim(COMP_NAME) // '::' // trim(Iam)

    If (present(rc))  rc=ESMF_SUCCESS

!   Get my internal state
!   ---------------------
    call ESMF_UserCompGetInternalState(gc, 'MAPL_ExtData_state', WRAP, STATUS)
    _VERIFY(STATUS)
    self => wrap%ptr

!   Get the configuration
!   ---------------------
    call ESMF_GridCompGet ( GC, config=CF, __RC__ )

    
    _RETURN(ESMF_SUCCESS)

  end subroutine extract_
   
! ............................................................................

   logical function PrimaryExportIsConstant_(item)
   
      type(PrimaryExport), intent(in) :: item

      if ( item%update_freq%is_single_shot() .or. &
           trim(item%file_template) == '/dev/null' ) then
          PrimaryExportIsConstant_ = .true. 
      else
          PrimaryExportIsConstant_ = .false.
      end if

   end function PrimaryExportIsConstant_

! ............................................................................

   logical function DerivedExportIsConstant_(item)
   
      type(DerivedExport), intent(in) :: item

      if ( item%update_freq%is_disabled() ) then
          DerivedExportIsConstant_ = .true. 
      else
          DerivedExportIsConstant_ = .false.
      end if

   end function DerivedExportIsConstant_

   ! ............................................................................

     type (ESMF_Time) function timestamp_(time, template, rc)
        type(ESMF_Time), intent(inout)         :: time
        character(len=ESMF_MAXSTR), intent(in) :: template
        integer, optional, intent(inout)       :: rc 

        ! locals
        integer, parameter :: DATETIME_MAXSTR_ = 32
        integer :: yy, mm, dd, hs, ms, ss
        character(len=DATETIME_MAXSTR_) :: buff, buff_date, buff_time
        character(len=DATETIME_MAXSTR_) :: str_yy, str_mm, str_dd
        character(len=DATETIME_MAXSTR_) :: str_hs, str_ms, str_ss

        integer :: i, il, ir
        integer :: status
       
        ! test the length of the timestamp template
        _ASSERT(len_trim(template) < DATETIME_MAXSTR_,'Timestamp template is greater than Maximum allowed len')

        buff = trim(template)
        buff = ESMF_UtilStringLowerCase(buff, __RC__)
         
        ! test if the template is empty and return the current time as result
        if (buff == '-'  .or. buff == '--'   .or. buff == '---' .or. &
            buff == 'na' .or. buff == 'none' .or. buff == 'n/a') then

           timestamp_ = time
        else   
           ! split the time stamp template into a date and time strings
           i = scan(buff, 't')
           If (.not.(i > 3)) Then
              _FAIL('ERROR: Time stamp ' // trim(template) // ' uses the fixed format, and must therefore contain a T')
           End If

           buff_date = buff(1:i-1)
           buff_time = buff(i+1:)

           ! parse the date string
           il = scan(buff_date, '-', back=.false.)
           ir = scan(buff_date, '-', back=.true. )
           str_yy = trim(buff_date(1:il-1))
           str_mm = trim(buff_date(il+1:ir-1))
           str_dd = trim(buff_date(ir+1:))

           ! parse the time string
           il = scan(buff_time, ':', back=.false.)
           ir = scan(buff_time, ':', back=.true. )
           str_hs = trim(buff_time(1:il-1))
           str_ms = trim(buff_time(il+1:ir-1))
           str_ss = trim(buff_time(ir+1:))
        
           ! remove the trailing 'Z' from the seconds string
           i = scan(str_ss, 'z')
           if (i > 0) then
              str_ss = trim(str_ss(1:i-1))
           end if

           ! apply the timestamp template
           call ESMF_TimeGet(time, yy=yy, mm=mm, dd=dd, h=hs, m=ms, s=ss, __RC__)

           i = scan(str_yy, '%'); if (i == 0) read (str_yy, '(I4)') yy
           i = scan(str_mm, '%'); if (i == 0) read (str_mm, '(I2)') mm
           i = scan(str_dd, '%'); if (i == 0) read (str_dd, '(I2)') dd
           i = scan(str_hs, '%'); if (i == 0) read (str_hs, '(I2)') hs
           i = scan(str_ms, '%'); if (i == 0) read (str_ms, '(I2)') ms
           i = scan(str_ss, '%'); if (i == 0) read (str_ss, '(I2)') ss

           call ESMF_TimeSet(timestamp_, yy=yy, mm=mm, dd=dd, h=hs, m=ms, s=ss, __RC__)
        end if

        _RETURN(ESMF_SUCCESS)

     end function timestamp_
    
     subroutine GetLevs(item, rc)

        type(PrimaryExport)      , intent(inout) :: item
        integer, optional        , intent(out  ) :: rc

        integer :: status

        real, allocatable          :: levFile(:) 
        character(len=ESMF_MAXSTR) :: levunits,tlevunits
        character(len=:), allocatable :: levname
        character(len=:), pointer :: positive 
        type(Variable), pointer :: var
        integer :: i

        positive=>null()
        var => null()
        if (item%isVector) then
           var=>item%file_metadata%get_variable(trim(item%fcomp1))
           _ASSERT(associated(var),"Variable "//TRIM(item%fcomp1)//" not found in file "//TRIM(item%file_template))
           var => null()
           var=>item%file_metadata%get_variable(trim(item%fcomp2))
           _ASSERT(associated(var),"Variable "//TRIM(item%fcomp2)//" not found in file "//TRIM(item%file_template))
        else
           var=>item%file_metadata%get_variable(trim(item%var))
           _ASSERT(associated(var),"Variable "//TRIM(item%var)//" not found in file "//TRIM(item%file_template))
        end if
   
        levName = item%file_metadata%get_level_name(rc=status)
        _VERIFY(status)
        if (trim(levName) /='') then
           call item%file_metadata%get_coordinate_info(levName,coordSize=item%lm,coordUnits=tLevUnits,coords=levFile,__RC__)
           levUnits=MAPL_TrimString(tlevUnits)
           ! check if pressure
           item%levUnit = ESMF_UtilStringLowerCase(levUnits)
           if (trim(item%levUnit) == 'hpa' .or. trim(item%levUnit)=='pa') then
              item%havePressure = .true.
           end if
           if (item%havePressure) then
              if (levFile(1)>levFile(size(levFile))) item%fileVDir="up"
           else
              positive => item%file_metadata%get_variable_attribute(levName,'positive',__RC__)
              if (associated(positive)) then
                 if (MAPL_TrimString(positive)=='up') item%fileVDir="up"
              end if
           end if

           allocate(item%levs(item%lm),__STAT__)
           item%levs=levFile
           if (trim(item%fileVDir)/=trim(item%importVDir)) then
              do i=1,size(levFile)
                 item%levs(i)=levFile(size(levFile)-i+1)
              enddo
           end if
           if (trim(item%levunit)=='hpa') item%levs=item%levs*100.0
           if (item%isVector) then
              item%units = item%file_metadata%get_variable_attribute(trim(item%fcomp1),"units",rc=status)
              _VERIFY(status)
           else
              item%units = item%file_metadata%get_variable_attribute(trim(item%var),"units",rc=status)
              _VERIFY(status)
           end if

        else
           item%LM=0
        end if

        _RETURN(ESMF_SUCCESS)

     end subroutine GetLevs

  subroutine MAPL_ExtDataInterpField(item,state,time,rc)
     type(PrimaryExport), intent(inout) :: item
     type(ESMF_State),    intent(in)    :: state
     type(ESMF_Time),     intent(in   ) :: time
     integer, optional,   intent(out  ) :: rc

     integer                    :: status
     type(ESMF_Field) :: field

     call ESMF_StateGet(state,item%vcomp1,field,__RC__)
     call item%modelGridFields%comp1%interpolate_to_time(field,time,__RC__)
     if (item%vartype == MAPL_VectorField) then
        call ESMF_StateGet(state,item%vcomp2,field,__RC__)
        call item%modelGridFields%comp2%interpolate_to_time(field,time,__RC__)
     end if        
     _RETURN(ESMF_SUCCESS)
  end subroutine MAPL_ExtDataInterpField

  subroutine MAPL_ExtDataVerticalInterpolate(ExtState,item,filec,current_time,rc)
     type(MAPL_ExtData_State), intent(inout) :: ExtState
     type(PrimaryExport), intent(inout)     :: item
     integer,             intent(in   )     :: filec
     type(ESMF_Time),     intent(in   )     :: current_time
     integer, optional,   intent(out  )     :: rc

     integer :: status
     integer :: id_ps
     type(ESMF_Field) :: field, newfield,psF

     if (item%do_VertInterp) then
        if (trim(item%importVDir)/=trim(item%fileVDir)) then
           call MAPL_ExtDataFlipVertical(item,filec,rc=status)
           _VERIFY(status)
        end if 
        if (item%vartype == MAPL_fieldItem) then
           call MAPL_ExtDataGetBracket(item,filec,newField,getRL=.true.,rc=status)
           _VERIFY(STATUS)
           call MAPL_ExtDataGetBracket(item,filec,Field,rc=status)
           _VERIFY(STATUS)
           id_ps = ExtState%primary%get_item_index("PS",current_time,_RC)
           call MAPL_ExtDataGetBracket(ExtState%primary%item(id_ps),filec,field=psF,rc=status)
           _VERIFY(STATUS)
           call vertInterpolation_pressKappa(field,newfield,psF,item%levs,MAPL_UNDEF,rc=status)
           _VERIFY(STATUS)
  
        else if (item%vartype == MAPL_VectorField) then

           id_ps = ExtState%primary%get_item_index("PS",current_time,_RC)
           call MAPL_ExtDataGetBracket(ExtState%primary%item(id_ps),filec,field=psF,rc=status)
           _VERIFY(STATUS)
           call MAPL_ExtDataGetBracket(item,filec,newField,getRL=.true.,vcomp=1,rc=status)
           _VERIFY(STATUS)
           call MAPL_ExtDataGetBracket(item,filec,Field,vcomp=1,rc=status)
           _VERIFY(STATUS)
           call vertInterpolation_pressKappa(field,newfield,psF,item%levs,MAPL_UNDEF,rc=status)
           _VERIFY(STATUS)
           call MAPL_ExtDataGetBracket(item,filec,newField,getRL=.true.,vcomp=2,rc=status)
           _VERIFY(STATUS)
           call MAPL_ExtDataGetBracket(item,filec,Field,vcomp=2,rc=status)
           _VERIFY(STATUS)
           call vertInterpolation_pressKappa(field,newfield,psF,item%levs,MAPL_UNDEF,rc=status)
           _VERIFY(STATUS)

        end if

     else if (item%do_Fill) then
        if (item%vartype == MAPL_fieldItem) then
           call MAPL_ExtDataGetBracket(item,filec,newField,getRL=.true.,rc=status)
           _VERIFY(STATUS)
           call MAPL_ExtDataGetBracket(item,filec,Field,rc=status)
           _VERIFY(STATUS)
           call MAPL_ExtDataFillField(item,field,newfield,rc=status)
           _VERIFY(STATUS)
        else if (item%vartype == MAPL_VectorField) then
           call MAPL_ExtDataGetBracket(item,filec,newField,getRL=.true.,vcomp=1,rc=status)
           _VERIFY(STATUS)
           call MAPL_ExtDataGetBracket(item,filec,Field,vcomp=1,rc=status)
           _VERIFY(STATUS)
           call MAPL_ExtDataFillField(item,field,newfield,rc=status)
           _VERIFY(STATUS)
           call MAPL_ExtDataGetBracket(item,filec,newField,getRL=.true.,vcomp=2,rc=status)
           _VERIFY(STATUS)
           call MAPL_ExtDataGetBracket(item,filec,Field,vcomp=2,rc=status)
           _VERIFY(STATUS)
           call MAPL_ExtDataFillField(item,field,newfield,rc=status)
           _VERIFY(STATUS)
        end if
     else
        if (trim(item%importVDir)/=trim(item%fileVDir)) then
           call MAPL_ExtDataFlipVertical(item,filec,rc=status)
           _VERIFY(status)
        end if
     end if
 
     _RETURN(ESMF_SUCCESS)
  end subroutine MAPL_ExtDataVerticalInterpolate

  function MAPL_ExtDataGridChangeLev(Grid,CF,lm,rc) result(NewGrid)

     type(ESMF_Grid), intent(inout) :: Grid
     type(ESMF_Config), intent(inout) :: CF
     integer,         intent(in)    :: lm
     integer, optional, intent(out) :: rc

     integer :: status
     character(len=ESMF_MAXSTR) :: Iam

     character(len=ESMF_MAXSTR) :: gname, comp_name
     integer :: counts(3)
     integer :: NX,NY
     type(ESMF_Grid)           :: newGrid
     type(ESMF_Config)         :: cflocal
     character(len=*), parameter :: CF_COMPONENT_SEPARATOR = '.'
     real :: temp_real

     IAM = "MAPL_ExtDataGridChangeLev"

     call MAPL_GridGet(grid,globalCellCountPerDim=counts,__RC__)
     call ESMF_GridGet(grid,name=gName,__RC__)
     call ESMF_ConfigGetAttribute(CF, value = NX, Label="NX:", __RC__)
     call ESMF_ConfigGetAttribute(CF, value = NY, Label="NY:", __RC__)

     comp_name = "ExtData"
     cflocal = MAPL_ConfigCreate(rc=status)
     _VERIFY(status)
     call MAPL_ConfigSetAttribute(cflocal,value=NX, label=trim(COMP_Name)//CF_COMPONENT_SEPARATOR//"NX:",rc=status)
     _VERIFY(status)
     call MAPL_ConfigSetAttribute(cflocal,value=lm, label=trim(COMP_Name)//CF_COMPONENT_SEPARATOR//"LM:",rc=status)
     _VERIFY(status)

     if (counts(2) == 6*counts(1)) then
        call MAPL_ConfigSetAttribute(cflocal,value="Cubed-Sphere", label=trim(COMP_Name)//CF_COMPONENT_SEPARATOR//"GRID_TYPE:",rc=status)
        _VERIFY(status)
        call MAPL_ConfigSetAttribute(cflocal,value=6, label=trim(COMP_Name)//CF_COMPONENT_SEPARATOR//"NF:",rc=status)
        _VERIFY(status)
        call MAPL_ConfigSetAttribute(cflocal,value=counts(1), label=trim(COMP_Name)//CF_COMPONENT_SEPARATOR//"IM_WORLD:",rc=status)
        _VERIFY(status)
        call MAPL_ConfigSetAttribute(cflocal,value=ny/6, label=trim(COMP_Name)//CF_COMPONENT_SEPARATOR//"NY:",rc=status)
        _VERIFY(status)
        call MAPL_ConfigSetAttribute(cflocal,value=trim(gname), label=trim(COMP_Name)//CF_COMPONENT_SEPARATOR//"GRIDNAME:",rc=status)
        _VERIFY(status)
        call ESMF_AttributeGet(grid, name='STRETCH_FACTOR', value=temp_real, rc=status)
        if (status == ESMF_SUCCESS) then
           call MAPL_ConfigSetAttribute(cflocal,value=temp_real, label=trim(COMP_Name)//CF_COMPONENT_SEPARATOR//"STRETCH_FACTOR:",rc=status)
           _VERIFY(status)
        endif
        call ESMF_AttributeGet(grid, name='TARGET_LON', value=temp_real, rc=status)
        if (status == ESMF_SUCCESS) then
           call MAPL_ConfigSetAttribute(cflocal,value=temp_real*MAPL_RADIANS_TO_DEGREES, label=trim(COMP_Name)//CF_COMPONENT_SEPARATOR//"TARGET_LON:",rc=status)
           _VERIFY(status)
        endif
        call ESMF_AttributeGet(grid, name='TARGET_LAT', value=temp_real, rc=status)
        if (status == ESMF_SUCCESS) then
           call MAPL_ConfigSetAttribute(cflocal,value=temp_real*MAPL_RADIANS_TO_DEGREES, label=trim(COMP_Name)//CF_COMPONENT_SEPARATOR//"TARGET_LAT:",rc=status)
           _VERIFY(status)
        endif
     else
        call MAPL_ConfigSetAttribute(cflocal,value=counts(1), label=trim(COMP_Name)//CF_COMPONENT_SEPARATOR//"IM_WORLD:",rc=status)
        _VERIFY(status)
        call MAPL_ConfigSetAttribute(cflocal,value=counts(2), label=trim(COMP_Name)//CF_COMPONENT_SEPARATOR//"JM_WORLD:",rc=status)
        _VERIFY(status)
        call MAPL_ConfigSetAttribute(cflocal,value=ny, label=trim(COMP_Name)//CF_COMPONENT_SEPARATOR//"NY:",rc=status)
        _VERIFY(status)
        call MAPL_ConfigSetAttribute(cflocal,value=trim(gname), label=trim(COMP_Name)//CF_COMPONENT_SEPARATOR//"GRIDNAME:",rc=status)
        _VERIFY(status)
     end if
     newgrid = grid_manager%make_grid(cflocal, prefix=trim(COMP_Name)//".", rc=status)
     _VERIFY(status)

     _RETURN(ESMF_SUCCESS)

  end function MAPL_ExtDataGridChangeLev

  subroutine MAPL_ExtDataGetBracket(item,Bside,field,bundle,getRL,vcomp,rc)

     type(PrimaryExport),              intent(inout) :: item
     integer,                          intent(in   ) :: bside
     type(ESMF_Field),       optional, intent(inout) :: field
     type(ESMF_FieldBundle), optional, intent(inout) :: bundle
     logical,                optional, intent(in   ) :: getRL
     integer,                optional, intent(in   ) :: vcomp
     integer,                optional, intent(out  ) :: rc

     character(len=ESMF_MAXSTR) :: Iam
     integer :: status

     logical :: getRL_
     
     Iam = "MAPL_ExtDataGetBracket"

     if (present(getRL)) then
        getRL_=getRL
     else
        getRL_=.false.
     end if

     if (present(vcomp)) then

        if (present(field)) then

           if (Bside == MAPL_ExtDataLeft .and. vcomp == 1) then 
              if (getRL_) then
                 call item%modelGridFields%auxiliary1%get_parameters('L',field=field,__RC__)
                 _RETURN(ESMF_SUCCESS)
              else
                 call item%modelGridFields%comp1%get_parameters('L',field=field,__RC__)
                 _RETURN(ESMF_SUCCESS)
              end if
           else if (Bside == MAPL_ExtDataLeft .and. vcomp == 2) then 
              if (getRL_) then
                 call item%modelGridFields%auxiliary2%get_parameters('L',field=field,__RC__)
                 _RETURN(ESMF_SUCCESS)
              else
                 call item%modelGridFields%comp2%get_parameters('L',field=field,__RC__)
                 _RETURN(ESMF_SUCCESS)
              end if
           else if (Bside == MAPL_ExtDataRight .and. vcomp == 1) then 
              if (getRL_) then
                 call item%modelGridFields%auxiliary1%get_parameters('R',field=field,__RC__)
                 _RETURN(ESMF_SUCCESS)
              else
                 call item%modelGridFields%comp1%get_parameters('R',field=field,__RC__)
                 _RETURN(ESMF_SUCCESS)
              end if
           else if (Bside == MAPL_ExtDataRight .and. vcomp == 2) then 
              if (getRL_) then
                 call item%modelGridFields%auxiliary2%get_parameters('R',field=field,__RC__)
                 _RETURN(ESMF_SUCCESS)
              else
                 call item%modelGridFields%comp2%get_parameters('R',field=field,__RC__)
                 _RETURN(ESMF_SUCCESS)
              end if
           end if

        else if (present(bundle)) then
           _RETURN(ESMF_FAILURE)
        end if

     else

        if (present(field)) then
           if (Bside == MAPL_ExtDataLeft) then
              if (getRL_) then
                 call item%modelGridFields%auxiliary1%get_parameters('L',field=field,__RC__)
                 _RETURN(ESMF_SUCCESS)
              else
                 call item%modelGridFields%comp1%get_parameters('L',field=field,__RC__)
                 _RETURN(ESMF_SUCCESS)
              end if
           else if (Bside == MAPL_ExtDataRight) then
              if (getRL_) then
                 call item%modelGridFields%auxiliary1%get_parameters('R',field=field,__RC__)
                 _RETURN(ESMF_SUCCESS)
              else
                 call item%modelGridFields%comp1%get_parameters('R',field=field,__RC__)
                 _RETURN(ESMF_SUCCESS)
              end if
           end if
        else if (present(bundle)) then
           !if (Bside == MAPL_ExtDataLeft) then 
              !bundle = item%binterp1
              !_RETURN(ESMF_SUCCESS)
           !else if (Bside == MAPL_ExtDataRight) then 
              !bundle = item%binterp2
              !_RETURN(ESMF_SUCCESS)
           !end if

        end if

     end if
     _RETURN(ESMF_FAILURE)

  end subroutine MAPL_ExtDataGetBracket

  subroutine MAPL_ExtDataFillField(item,FieldF,FieldR,rc)

  type(PrimaryExport), intent(inout) :: item
  type(ESMF_Field), intent(inout) :: FieldF
  type(ESMF_Field), intent(inout) :: FieldR
  integer, optional, intent(out)  :: rc

  character(len=ESMF_MAXSTR) :: Iam
  integer :: status

  real, pointer :: ptrF(:,:,:),ptrR(:,:,:)
  integer :: lm_in,lm_out,i

  Iam = "MAPL_ExtDataFillField"

  call ESMF_FieldGet(FieldF,0,farrayPtr=ptrF,rc=status)
  _VERIFY(STATUS)
  call ESMF_FieldGet(FieldR,0,farrayPtr=ptrR,rc=status)
  _VERIFY(STATUS)
  ptrF = 0.0
  lm_in= size(ptrR,3)
  lm_out = size(ptrF,3)
  if (trim(item%importVDir)=="down") then

     if (trim(item%fileVDir)=="down") then
        do i=1,lm_in
           ptrF(:,:,lm_out-lm_in+i)=ptrR(:,:,i)
        enddo
     else if (trim(item%fileVDir)=="up") then
        do i=1,lm_in
           ptrF(:,:,lm_out-i+1)=ptrR(:,:,i)
        enddo
     end if
  else if (trim(item%importVDir)=="up") then
     if (trim(item%fileVDir)=="down") then
        do i=1,lm_in
           ptrF(:,:,lm_in-i+1)=ptrR(:,:,i)
        enddo
     else if (trim(item%fileVDir)=="up") then
        do i=1,lm_in
           ptrF(:,:,i)=ptrR(:,:,i)
        enddo
     end if
  end if

  _RETURN(ESMF_SUCCESS)
  
  end subroutine MAPL_ExtDataFillField

  subroutine MAPL_ExtDataFlipVertical(item,filec,rc)
      type(PrimaryExport), intent(inout)      :: item
      integer,                  intent(in)    :: filec
      integer, optional, intent(out)          :: rc
 
      integer :: status
     
      type(ESMF_Field) :: Field,field1,field2
      real, pointer    :: ptr(:,:,:)
      real, allocatable :: ptemp(:,:,:)
      integer :: ls, le

      if (item%isVector) then

         if (item%do_Fill .or. item%do_VertInterp) then
            call MAPL_ExtDataGetBracket(item,filec,field=Field1,vcomp=1,getRL=.true.,__RC__)
            call MAPL_ExtDataGetBracket(item,filec,field=Field2,vcomp=2,getRL=.true.,__RC__)
         else
            call MAPL_ExtDataGetBracket(item,filec,field=Field1,vcomp=1,__RC__)
            call MAPL_ExtDataGetBracket(item,filec,field=Field2,vcomp=2,__RC__)
         end if

         call ESMF_FieldGet(Field1,0,farrayPtr=ptr,rc=status)
         _VERIFY(STATUS)
         allocate(ptemp,source=ptr,stat=status)
         _VERIFY(status)
         ls = lbound(ptr,3)
         le = ubound(ptr,3)
         ptr(:,:,le:ls:-1) = ptemp(:,:,ls:le:+1)

         call ESMF_FieldGet(Field2,0,farrayPtr=ptr,rc=status)
         _VERIFY(STATUS)
         ptemp=ptr
         ptr(:,:,le:ls:-1) = ptemp(:,:,ls:le:+1)

         deallocate(ptemp)

      else

         if (item%do_Fill .or. item%do_VertInterp) then
            call MAPL_ExtDataGetBracket(item,filec,field=Field,getRL=.true.,__RC__)
         else
            call MAPL_ExtDataGetBracket(item,filec,field=Field,__RC__)
         end if

         call ESMF_FieldGet(Field,0,farrayPtr=ptr,rc=status)
         _VERIFY(STATUS)
         allocate(ptemp,source=ptr,stat=status)
         _VERIFY(status)
         ls = lbound(ptr,3)
         le = ubound(ptr,3)
         ptr(:,:,le:ls:-1) = ptemp(:,:,ls:le:+1)
         deallocate(ptemp)
      end if

      _RETURN(ESMF_SUCCESS)

  end subroutine MAPL_ExtDataFlipVertical
  subroutine MAPL_ExtDataPopulateBundle(item,filec,pbundle,rc)
      type(PrimaryExport), intent(inout)      :: item
      integer,                  intent(in)    :: filec
      type(ESMF_FieldBundle), intent(inout)   :: pbundle
      integer, optional, intent(out)          :: rc
 
      integer :: status
     
      type(ESMF_Field) :: Field,field1,field2
      type(ESMF_Grid)  :: grid

      if (item%isVector) then

         if (item%do_Fill .or. item%do_VertInterp) then
            call MAPL_ExtDataGetBracket(item,filec,field=Field1,vcomp=1,getRL=.true.,__RC__)
            call MAPL_ExtDataGetBracket(item,filec,field=Field2,vcomp=2,getRL=.true.,__RC__)
         else
            call MAPL_ExtDataGetBracket(item,filec,field=Field1,vcomp=1,__RC__)
            call MAPL_ExtDataGetBracket(item,filec,field=Field2,vcomp=2,__RC__)
         end if

         call ESMF_FieldGet(Field1,grid=grid,rc=status)
         _VERIFY(STATUS)
         call ESMF_FieldBundleSet(pbundle,grid=grid,rc=status)
         _VERIFY(STATUS)
         call MAPL_FieldBundleAdd(pbundle,Field1,rc=status)
         _VERIFY(STATUS)
         call MAPL_FieldBundleAdd(pbundle,Field2,rc=status)
         _VERIFY(STATUS)

      else

         if (item%do_Fill .or. item%do_VertInterp) then
            call MAPL_ExtDataGetBracket(item,filec,field=Field,getRL=.true.,__RC__)
         else
            call MAPL_ExtDataGetBracket(item,filec,field=Field,__RC__)
         end if

         call ESMF_FieldGet(Field,grid=grid,rc=status)
         _VERIFY(STATUS)
         call ESMF_FieldBundleSet(pbundle,grid=grid,rc=status)
         _VERIFY(STATUS)
         call MAPL_FieldBundleAdd(pbundle,Field,rc=status)
         _VERIFY(STATUS)

      end if

      _RETURN(ESMF_SUCCESS)

  end subroutine MAPL_ExtDataPopulateBundle

  subroutine MAPL_ExtDataCreateCFIO(IOBundles, rc)
    type(IOBundleNGVector), target, intent(inout) :: IOBundles
    integer, optional,      intent(out  ) :: rc

     type (IOBundleNGVectorIterator) :: bundle_iter
     type (ExtDataNG_IOBundle), pointer :: io_bundle
     integer :: status
    
     bundle_iter = IOBundles%begin()
     do while (bundle_iter /= IOBundles%end())
        io_bundle => bundle_iter%get()
        call io_bundle%make_cfio(__RC__)
        call bundle_iter%next()
     enddo

     _RETURN(ESMF_SUCCESS)

  end subroutine MAPL_ExtDataCreateCFIO

  subroutine MAPL_ExtDataDestroyCFIO(IOBundles,rc)
     type(IOBundleNGVector), target, intent(inout) :: IOBundles
     integer, optional,      intent(out  ) :: rc

     type(IOBundleNGVectorIterator) :: bundle_iter
     type (ExtDataNG_IOBundle), pointer :: io_bundle
     integer :: status

     bundle_iter = IOBundles%begin()
     do while (bundle_iter /= IOBundles%end())
        io_bundle => bundle_iter%get()
        call io_bundle%clean(__RC__)
        call bundle_iter%next
     enddo
     call IOBundles%clear()

     _RETURN(ESMF_SUCCESS)

  end subroutine MAPL_ExtDataDestroyCFIO

  subroutine MAPL_ExtDataPrefetch(IOBundles,rc)
     type(IOBundleNGVector), target, intent(inout) :: IOBundles
     integer, optional,      intent(out  ) :: rc

     integer :: n,nfiles
     type(ExtDataNG_IOBundle), pointer :: io_bundle => null()
     integer :: status

     nfiles = IOBundles%size()

     do n = 1, nfiles
        io_bundle => IOBundles%at(n)
        call io_bundle%cfio%request_data_from_file(io_bundle%file_name,io_bundle%time_index,rc=status)
        _VERIFY(status)
     enddo

     _RETURN(ESMF_SUCCESS)

  end subroutine MAPL_ExtDataPrefetch

  subroutine MAPL_ExtDataReadPrefetch(IOBundles,rc)
     type(IOBundleNGVector), target, intent(inout) :: IOBundles
     integer, optional,      intent(out  ) :: rc

     integer :: nfiles, n
     type (ExtDataNG_IOBundle), pointer :: io_bundle
     integer :: status


     nfiles = IOBundles%size()
     do n=1, nfiles
        io_bundle => IOBundles%at(n)
        call io_bundle%cfio%process_data_from_file(rc=status)
        _VERIFY(status)
     enddo

     _RETURN(ESMF_SUCCESS)

  end subroutine MAPL_ExtDataReadPrefetch

  subroutine createFileLevBracket(item,cf,rc)
     type(PrimaryExport), intent(inout) :: item
     type(ESMF_Config), intent(inout) :: cf
     integer, optional, intent(out) :: rc

     integer :: status
     type (ESMF_Grid) :: grid, newgrid
     type(ESMF_Field) :: field,new_field

     call item%modelGridFields%comp1%get_parameters('L',field=field,__RC__)
     newGrid = MAPL_ExtDataGridChangeLev(grid,cf,item%lm,__RC__)
     new_field = MAPL_FieldCreate(field,newGrid,lm=item%lm,newName=trim(item%fcomp1),__RC__)
     call item%modelGridFields%auxiliary1%set_parameters(left_field=new_field,__RC__)
     new_field = MAPL_FieldCreate(field,newGrid,lm=item%lm,newName=trim(item%fcomp1),__RC__)
     call item%modelGridFields%auxiliary1%set_parameters(right_field=new_field,__RC__)
     if (item%vartype==MAPL_VectorField) then
        new_field = MAPL_FieldCreate(field,newGrid,lm=item%lm,newName=trim(item%fcomp2),__RC__)
        call item%modelGridFields%auxiliary2%set_parameters(left_field=new_field,__RC__)
        new_field = MAPL_FieldCreate(field,newGrid,lm=item%lm,newName=trim(item%fcomp2),__RC__)
        call item%modelGridFields%auxiliary2%set_parameters(right_field=new_field,__RC__)
     end if
     _RETURN(_SUCCESS)

  end subroutine createFileLevBracket


  subroutine IOBundle_Add_Entry(IOBundles,item,entry_num,rc)
     type(IOBundleNGVector), intent(inout) :: IOBundles
     type(primaryExport), intent(inout)        :: item 
     integer, intent(in)                    :: entry_num
     integer, intent(out), optional         :: rc

     integer :: status

     type (ExtDataNG_IOBundle) :: io_bundle
     type (GriddedIOItemVector) :: itemsL, itemsR
     logical :: update
     character(len=ESMF_MAXPATHLEN) :: current_file
     integer :: time_index

     call item%modelGridFields%comp1%get_parameters('L',update=update,file=current_file,time_index=time_index)
     if (update) then    
        call itemsL%push_back(item%fileVars)
        io_bundle = ExtDataNG_IOBundle(MAPL_ExtDataLeft, entry_num, current_file, time_index, item%trans, item%fracval, item%file_template, &
            item%pfioCollection_id,item%iclient_collection_id,itemsL,rc=status)
        _VERIFY(status)
        call IOBundles%push_back(io_bundle)
        call extdata_lgr%info('%a updated L bracket with: %a at time index %i2 ',item%name, current_file, time_index)
     end if
     call item%modelGridFields%comp1%get_parameters('R',update=update,file=current_file,time_index=time_index)
     if (update) then    
        call itemsR%push_back(item%fileVars)
        io_bundle = ExtDataNG_IOBundle(MAPL_ExtDataRight, entry_num, current_file, time_index, item%trans, item%fracval, item%file_template, &
            item%pfioCollection_id,item%iclient_collection_id,itemsR,rc=status)
        _VERIFY(status)
        call IOBundles%push_back(io_bundle)
        call extdata_lgr%info('%a updated R bracket with: %a at time index %i2 ',item%name,current_file, time_index)
     end if

     _RETURN(ESMF_SUCCESS)

  end subroutine IOBundle_Add_Entry

  subroutine set_constant_field(item,ExtDataState,rc)
     type(PrimaryExport), intent(inout) :: item
     type(ESMF_State), intent(inout) :: extDataState
     integer, intent(out), optional :: rc

     integer :: status,fieldRank
     real(kind=REAL32), pointer :: ptr2d(:,:),ptr3d(:,:,:)
     type(ESMF_Field) :: field

     if (item%vartype == MAPL_FieldItem) then
        call ESMF_StateGet(ExtDataState,trim(item%name),field,__RC__)
        call ESMF_FieldGet(field,dimCount=fieldRank,__RC__)
        if (fieldRank == 2) then
           call MAPL_GetPointer(ExtDataState, ptr2d, trim(item%name),__RC__)
           ptr2d = item%const
        else if (fieldRank == 3) then
           call MAPL_GetPointer(ExtDataState, ptr3d, trim(item%name), __RC__)
           ptr3d = item%const
         endif
     else if (item%vartype == MAPL_VectorField) then
        call ESMF_StateGet(ExtDataState,trim(item%vcomp1),field,__RC__)
        call ESMF_FieldGet(field,dimCount=fieldRank,__RC__)
         if (fieldRank == 2) then
               call MAPL_GetPointer(ExtDataState, ptr2d, trim(item%vcomp1),__RC__)
               ptr2d = item%const
         else if (fieldRank == 3) then
               call MAPL_GetPointer(ExtDataState, ptr3d, trim(item%vcomp1), __RC__)
               ptr3d = item%const
         endif
         call ESMF_StateGet(ExtDataState,trim(item%vcomp2),field,__RC__)
         call ESMF_FieldGet(field,dimCount=fieldRank,__RC__)
         if (fieldRank == 2) then
               call MAPL_GetPointer(ExtDataState, ptr2d, trim(item%vcomp2),__RC__)
               ptr2d = item%const
         else if (fieldRank == 3) then
               call MAPL_GetPointer(ExtDataState, ptr3d, trim(item%vcomp2), __RC__)
               ptr3d = item%const
         endif
      end if

     _RETURN(_SUCCESS)
  end subroutine set_constant_field

  subroutine create_bracketing_fields(item,ExtDataState,cf,rc)
     type(PrimaryExport), intent(inout) :: item
     type(ESMF_State), intent(inout) :: extDataState
     type(ESMF_Config), intent(inout) :: cf
     integer, intent(out), optional :: rc

     integer :: status,lm,fieldRank
     type(ESMF_Field) :: field,left_field,right_field
     type(ESMF_Grid)  :: grid
     real(kind=REAL32), pointer :: ptr3d(:,:,:)

     call GetLevs(item,__RC__)
     item%iclient_collection_id=i_clients%add_ext_collection(trim(item%file_template))
     if (item%vartype == MAPL_FieldItem) then

        call ESMF_StateGet(ExtDataState, trim(item%name), field,__RC__)
        call ESMF_FieldGet(field,grid=grid,rank=fieldRank,__RC__)

        lm=0
        if (fieldRank==3) then
           call ESMF_FieldGet(field,0,farrayPtr=ptr3d,__RC__)
           lm = size(ptr3d,3)
        end if
        if (item%lm /= lm .and. lm /= 0 .and. item%havePressure) then
           item%do_VertInterp = .true.
        else if (item%lm /= lm .and. lm /= 0) then
           item%do_Fill = .true.
        end if
        left_field = MAPL_FieldCreate(field,item%var,doCopy=.true.,__RC__)
        right_field = MAPL_FieldCreate(field,item%var,doCopy=.true.,__RC__)
        call item%modelGridFields%comp1%set_parameters(left_field=left_field,right_field=right_field, __RC__)
        if (item%do_fill .or. item%do_vertInterp) then
           call createFileLevBracket(item,cf,__RC__)
        end if

     else if (item%vartype == MAPL_VectorField) then

        if (item%Trans /= REGRID_METHOD_BILINEAR) then
           _FAIL('No conservative re-gridding with vectors')
        end if

        call ESMF_StateGet(ExtDataState, trim(item%vcomp1), field,__RC__)
        call ESMF_FieldGet(field,grid=grid,rank=fieldRank,__RC__)

        lm = 0
        if (fieldRank==3) then
           call ESMF_FieldGet(field,0,farrayPtr=ptr3d,__RC__)
           lm = size(ptr3d,3)
        end if
        if (item%lm /= lm .and. item%havePressure) then
           item%do_VertInterp = .true.
        else if (item%lm /= lm .and. lm /= 0) then
           item%do_Fill = .true.
        end if

        left_field = MAPL_FieldCreate(field,item%fcomp1,doCopy=.true.,__RC__)
        right_field = MAPL_FieldCreate(field,item%fcomp1,doCopy=.true.,__RC__)
        call item%modelGridFields%comp1%set_parameters(left_field=left_field,right_field=right_field, __RC__)
        call ESMF_StateGet(ExtDataState, trim(item%vcomp2), field,__RC__)
        left_field = MAPL_FieldCreate(field,item%fcomp2,doCopy=.true.,__RC__)
        right_field = MAPL_FieldCreate(field,item%fcomp2,doCopy=.true.,__RC__)
        call item%modelGridFields%comp2%set_parameters(left_field=left_field,right_field=right_field, __RC__)

        if (item%do_fill .or. item%do_vertInterp) then
           call createFileLevBracket(item,cf,__RC__)
        end if

     end if

     _RETURN(_SUCCESS)
  end subroutine create_bracketing_fields

  function get_item_index(this,base_name,current_time,rc) result(item_index)
     integer :: item_index
     class(primaryExports), intent(in) :: this
     type(ESMF_Time) :: current_time
     character(len=*),intent(in) :: base_name
     integer, optional, intent(out) :: rc

     integer :: status
     character(len=:), pointer :: cname
     integer :: i
     integer, pointer :: num_rules,i_start
     logical :: found

     found = .false.
     do i=1,this%import_names%size()
        cname => this%import_names%at(i)
        if (cname == base_name) then 
           found = .true.
           i_start => this%export_id_start%at(i)
           num_rules => this%number_of_rules%at(i)
           exit
        end if
     enddo
     _ASSERT(found,"no item with that basename found")

     item_index = -1
     if (num_rules == 1) then
        item_index = i_start
     else if (num_rules > 1) then
        do i=1,num_rules
           if (current_time >= this%item(i_start+i-1)%start_end_time(1) .and. &
               current_time <  this%item(i_start+i-1)%start_end_time(2)) then
              item_index = i_start + i -1
              exit
           endif
        enddo
     end if
     _ASSERT(item_index/=-1,"did not find item")
     _RETURN(_SUCCESS)
  end function get_item_index

  function am_i_running(yaml_file) result(am_running)
     logical :: am_running
     character(len=*), intent(in) :: yaml_file

      type(Parser)              :: p
      class(YAML_Node), allocatable :: config

      p = Parser('core')
      config = p%load(yaml_file)

      if (config%has("USE_EXTDATA")) then
         am_running = config%of("USE_EXTDATA")
      else
         am_running = .true.
      end if
   end function am_i_running

 END MODULE MAPL_ExtDataGridComp2G
