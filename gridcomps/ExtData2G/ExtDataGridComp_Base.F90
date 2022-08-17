#include "MAPL_Exceptions.h"
#include "MAPL_Generic.h"
#include "unused_dummy.H"

   MODULE MAPL_ExtDataGridComp_Base

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

   PUBLIC SetServices

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

  type MAPL_ExtData_State
     PRIVATE
     type(PrimaryExports) :: Primary
     type(DerivedExports) :: Derived
     type(ESMF_Config)    :: CF
     logical              :: active
  end type MAPL_ExtData_State

  type MAPL_ExtData_Wrap
     type (MAPL_ExtData_State), pointer :: PTR => null()
  end type MAPL_ExtData_WRAP

CONTAINS

   SUBROUTINE SetServices ( GC, RC )

    type(ESMF_GridComp), intent(INOUT) :: GC  ! gridded component
    integer, optional                  :: RC  ! return code

    type (MAPL_ExtData_State), pointer  :: self   ! internal, that is
    type (MAPL_ExtData_wrap)            :: wrap

    character(len=ESMF_MAXSTR)          :: comp_name
    integer                             :: status


    call ESMF_GridCompGet( GC, name=comp_name, _RC )

    allocate ( self, stat=STATUS )
    _VERIFY(STATUS)
    wrap%ptr => self
 
    call MAPL_GridCompSetEntryPoint ( GC, ESMF_METHOD_INITIALIZE,  Initialize_, _RC )
    call MAPL_GridCompSetEntryPoint ( GC, ESMF_METHOD_RUN,   Run_,        _RC )
    call MAPL_GridCompSetEntryPoint ( GC, ESMF_METHOD_FINALIZE, Finalize_,   _RC )
        
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
    call MAPL_GenericSetServices ( GC, _RC )

    _RETURN(ESMF_SUCCESS)

  END SUBROUTINE SetServices

   SUBROUTINE Initialize_ ( GC, IMPORT, EXPORT, CLOCK, rc )

   implicit NONE

   type(ESMF_Clock),  intent(inout)   :: CLOCK   ! The clock

   type(ESMF_GridComp), intent(inout) :: GC      ! Grid Component
   type(ESMF_State), intent(inout)    :: IMPORT  ! Import State
   type(ESMF_State), intent(inout)    :: EXPORT  ! Export State
   integer, intent(out)               :: rc      ! Error return code:
                                                 !  0 - all is well
   type(MAPL_ExtData_state), pointer :: self        ! Legacy state
   type(ESMF_Config)                 :: CF_master          ! Universal Config 

   character(len=ESMF_MAXSTR)        :: comp_name
   integer                           :: Status

   type(PrimaryExport), pointer      :: item
   integer                           :: i,j
   integer                           :: ItemCount
   integer                           :: PrimaryItemCount, DerivedItemCount

   type(ESMF_Time)                   :: time

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
   type(ESMF_Field) :: new_field
   type(ESMF_StateItem_Flag) :: state_item_type
   !class(logger), pointer :: lgr

!  Get my name and set-up traceback handle
!  ---------------------------------------
   call ESMF_GridCompGet( GC, name=comp_name, config=CF_master, vm=vm, _RC )
   call MAPL_GetLogger(gc, extdata_lgr, _RC)

!  Extract relevant runtime information
!  ------------------------------------
   call extract_ ( GC, self, CF_master, _RC)
   self%CF = CF_master

!  Start Some Timers
!  -----------------
   call MAPL_GetObjectFromGC ( gc, MAPLSTATE, RC=STATUS)
   _VERIFY(STATUS) 
   call MAPL_TimerOn(MAPLSTATE,"TOTAL")
   call MAPL_TimerOn(MAPLSTATE,"Initialize")

   call ESMF_ConfigGetAttribute(cf_master,new_rc_file,label="EXTDATA_YAML_FILE:",default="extdata.yaml",_RC)
   self%active = am_i_running(new_rc_file)

   call ESMF_ClockGet(CLOCK, currTIME=time, _RC)
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

    config_yaml = ExtDataOldTypesCreator(new_rc_file,time,_RC)

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
   call MAPL_GenericInitialize ( GC, IMPORT, EXPORT, clock,  _RC )

   call extdata_lgr%info("Using ExtData2G, note this is still in BETA stage")

!                         ---------------------------
!                         Parse ExtData Resource File
!                         ---------------------------
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
            call config_yaml%fillin_primary(current_base_name//"+"//sidx,current_base_name,self%primary%item(num_primary),time,clock,_RC)
            allocate(self%primary%item(num_primary)%start_end_time(2))
            self%primary%item(num_primary)%start_end_time(1)=time_ranges(j)
            self%primary%item(num_primary)%start_end_time(2)=time_ranges(j+1)
         enddo
      else
         num_primary=num_primary+1
         call config_yaml%fillin_primary(current_base_name,current_base_name,self%primary%item(num_primary),time,clock,_RC)
      end if
   enddo
   do i=1,self%derived%import_names%size()
      current_base_name => self%derived%import_names%at(i)
      num_derived=num_derived+1
      call config_yaml%fillin_derived(current_base_name,self%derived%item(num_derived),time,clock,_RC)
   enddo
   
   PrimaryLoop: do i=1,self%primary%import_names%size()

      current_base_name => self%primary%import_names%at(i)
      idx = self%primary%get_item_index(current_base_name,time,_RC)
      item => self%primary%item(idx)
      item%initialized = .true.

      item%pfioCollection_id = MAPL_DataAddCollection(item%file_template)
      ! now if we haven't found field must be extra one we need to derive
      call ESMF_StateGet(export,item%name,state_item_type,_RC)
      if (state_item_type == ESMF_STATEITEM_NOTFOUND) then
         new_field = ESMF_FieldEmptyCreate(name=trim(item%name),_RC)
         call MAPL_StateAdd(export,new_field,_RC)   
      end if
      if (item%isConst) then
         call mark_constant_field(item,export,_RC)
      else
         call create_bracketing_fields(item,export,_RC) 
      end if
      call mark_primary_fields(item,export,_RC)

   end do PrimaryLoop

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

   SUBROUTINE Run_ ( GC, IMPORT, EXPORT, CLOCK, rc )

  implicit NONE

   type(ESMF_Clock),  intent(inout) :: CLOCK     ! The clock

   type(ESMF_GridComp), intent(inout)  :: GC     ! Grid Component
   type(ESMF_State), intent(inout) :: IMPORT     ! Import State
   type(ESMF_State), intent(inout) :: EXPORT     ! Export State
   integer, intent(out) ::  rc                   ! Error return code:
                                                 !  0 - all is well

   type(MAPL_ExtData_state), pointer :: self        ! Legacy state
   type(ESMF_Config)                 :: CF          ! Universal Config 

   character(len=ESMF_MAXSTR)        :: comp_name
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

   call ESMF_GridCompGet( GC, name=comp_name, config=CF_master, _RC )

   call extract_ ( GC, self, CF, _RC )

   if (.not. self%active) then
      _RETURN(ESMF_SUCCESS)
   end if

   call MAPL_GetObjectFromGC ( gc, MAPLSTATE, RC=STATUS)
   _VERIFY(STATUS) 
   call MAPL_TimerOn(MAPLSTATE,"TOTAL")
   call MAPL_TimerOn(MAPLSTATE,"Run")

   call ESMF_ClockGet(CLOCK, currTIME=time0, _RC)

   allocate(doUpdate(self%primary%nitems),stat=status)
   _VERIFY(STATUS)
   doUpdate = .false.
   allocate(useTime(self%primary%nitems),stat=status)
   _VERIFY(STATUS)

   call MAPL_TimerOn(MAPLSTATE,"-Read_Loop")

   call extdata_lgr%debug('ExtData Run_(): Start')
   call extdata_lgr%debug('ExtData Run_(): READ_LOOP: Start')
 
   READ_LOOP: do i=1,self%primary%import_names%size()

      current_base_name => self%primary%import_names%at(i)
      idx = self%primary%get_item_index(current_base_name,time0,_RC)
      item => self%primary%item(idx)

      if (.not.item%initialized) then
         item%pfioCollection_id = MAPL_DataAddCollection(item%file_template)
         if (item%isConst) then
            call mark_constant_field(item,export,_RC)
         else
            call create_bracketing_fields(item,export, _RC)
         end if   
         call mark_primary_fields(item,export,_RC)
         item%initialized=.true.
      end if

      nitems = self%primary%import_names%size()

      if (item%isConst) then
         call extdata_lgr%debug('   ==> Break loop since isConst is true')
         cycle
      endif

      call MAPL_TimerOn(MAPLSTATE,"--CheckUpd")

      call item%update_freq%check_update(doUpdate(i),time,time0,.not.hasRun,_RC)
      call MAPL_TimerOff(MAPLSTATE,"--CheckUpd")

      DO_UPDATE: if (doUpdate(i)) then

         call item%modelGridFields%comp1%reset()
         call item%filestream%get_file_bracket(time,item%source_time, item%modelGridFields%comp1,_RC)
         if (item%vartype == MAPL_VectorField) then
            call item%filestream%get_file_bracket(time,item%source_time, item%modelGridFields%comp2,_RC)
         end if
         call IOBundle_Add_Entry(IOBundles,item,idx)
         useTime(i)=time
         call mark_update(item,export,.true.,_RC)
      else
         call mark_update(item,export,.false.,_RC)
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
        
         call MAPL_ExtDataInterpField(item,export,useTime(i),_RC)

      endif

      nullify(item) 

   end do INTERP_LOOP

   call extdata_lgr%debug('ExtData Run_: INTERP_LOOP: Done')

   call MAPL_TimerOff(MAPLSTATE,"-Interpolate")

   do i=1,self%derived%nItems

      derivedItem => self%derived%item(i)

      call derivedItem%update_freq%check_update(doUpdate_,time,time0,.not.hasRun,_RC)
      call derivedItem%mark_derived_field(export,doUpdate_,_RC)

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

   SUBROUTINE Finalize_ ( GC, IMPORT, EXPORT, CLOCK, rc )

  implicit NONE

   type(ESMF_Clock),  intent(inout) :: CLOCK      ! The clock

   type(ESMF_GridComp), intent(inout)  :: GC     ! Grid Component
   type(ESMF_State), intent(inout) :: IMPORT     ! Import State
   type(ESMF_State), intent(inout) :: EXPORT     ! Export State
   integer, intent(out) ::  rc                   ! Error return code:
                                                 !  0 - all is well

   type(MAPL_ExtData_state), pointer :: self        ! Legacy state
   type(ESMF_Config)                 :: CF          ! Universal Config 

   character(len=ESMF_MAXSTR)        :: comp_name
   integer                           :: status


   call ESMF_GridCompGet( GC, name=comp_name, _RC )

   call MAPL_GenericFinalize ( GC, IMPORT, EXPORT, CLOCK,  _RC )

   call extract_ ( GC, self, CF, _RC)

   if (associated(self%primary%item)) then
      deallocate(self%primary%item)
   end if

   _RETURN(ESMF_SUCCESS)

 end SUBROUTINE Finalize_

 subroutine extract_ ( GC, self, CF, rc)

    type(ESMF_GridComp), intent(INout)  :: GC           ! Grid Comp object

    type(MAPL_ExtData_state), pointer   :: self         ! Legacy state
    type(ESMF_Config),   intent(out)    :: CF           ! Universal Config 

    integer, intent(out), optional      :: rc

!                            ---

    character(len=ESMF_MAXSTR) :: comp_name
    integer                    :: status

    type(MAPL_ExtData_Wrap)  :: wrap

    call ESMF_GridCompGet( GC, NAME=comp_name, _RC )

    If (present(rc))  rc=ESMF_SUCCESS

!   Get my internal state
!   ---------------------
    call ESMF_UserCompGetInternalState(gc, 'MAPL_ExtData_state', WRAP, STATUS)
    _VERIFY(STATUS)
    self => wrap%ptr

    call ESMF_GridCompGet ( GC, config=CF, _RC )

    
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

     subroutine GetLevs(item, rc)

        type(PrimaryExport)      , intent(inout) :: item
        integer, optional        , intent(out  ) :: rc

        integer :: status

        real, allocatable          :: levFile(:) 
        character(len=ESMF_MAXSTR) :: levunits,tlevunits
        character(len=:), allocatable :: levname
        character(len=:), pointer :: positive 
        type(Variable), pointer :: var
        type(StringVector), pointer :: dimensions
        type(StringVectorIterator) :: iter
        character(len=:), pointer :: dim_name
        logical :: is_3d
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
           call item%file_metadata%get_coordinate_info(levName,coordSize=item%lm,coordUnits=tLevUnits,coords=levFile,_RC)
           dimensions => var%get_dimensions()
           is_3d=.false.
           iter = dimensions%begin()
           do while(iter/=dimensions%end())
              dim_name => iter%get()
              if (levName == dim_name) is_3d=.true.
              call iter%next()
           enddo
           if (is_3d) then           
              levUnits=MAPL_TrimString(tlevUnits)
              ! check if pressure
              item%levUnit = ESMF_UtilStringLowerCase(levUnits)
              if (trim(item%levUnit) == 'hpa' .or. trim(item%levUnit)=='pa') then
                 item%havePressure = .true.
              end if
              if (item%havePressure) then
                 if (levFile(1)>levFile(size(levFile))) item%fileVDir="up"
              else
                 positive => item%file_metadata%get_variable_attribute(levName,'positive',_RC)
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
              item%lm=0
           end if
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

     call ESMF_StateGet(state,item%vcomp1,field,_RC)
     call item%modelGridFields%comp1%interpolate_to_time(field,time,_RC)
     if (item%vartype == MAPL_VectorField) then
        call ESMF_StateGet(state,item%vcomp2,field,_RC)
        call item%modelGridFields%comp2%interpolate_to_time(field,time,_RC)
     end if        
     _RETURN(ESMF_SUCCESS)
  end subroutine MAPL_ExtDataInterpField

  subroutine MAPL_ExtDataGetBracket(item,Bside,field,bundle,vcomp,rc)

     type(PrimaryExport),              intent(inout) :: item
     integer,                          intent(in   ) :: bside
     type(ESMF_Field),       optional, intent(inout) :: field
     type(ESMF_FieldBundle), optional, intent(inout) :: bundle
     integer,                optional, intent(in   ) :: vcomp
     integer,                optional, intent(out  ) :: rc

     integer :: status

     if (present(vcomp)) then

        if (present(field)) then

           if (Bside == MAPL_ExtDataLeft .and. vcomp == 1) then 
              call item%modelGridFields%comp1%get_parameters('L',field=field,_RC)
              _RETURN(ESMF_SUCCESS)
           else if (Bside == MAPL_ExtDataLeft .and. vcomp == 2) then 
              call item%modelGridFields%comp2%get_parameters('L',field=field,_RC)
              _RETURN(ESMF_SUCCESS)
           else if (Bside == MAPL_ExtDataRight .and. vcomp == 1) then 
              call item%modelGridFields%comp1%get_parameters('R',field=field,_RC)
              _RETURN(ESMF_SUCCESS)
           else if (Bside == MAPL_ExtDataRight .and. vcomp == 2) then 
              call item%modelGridFields%comp2%get_parameters('R',field=field,_RC)
              _RETURN(ESMF_SUCCESS)
           end if

        else if (present(bundle)) then
           _RETURN(ESMF_FAILURE)
        end if

     else

        if (present(field)) then
           if (Bside == MAPL_ExtDataLeft) then
              call item%modelGridFields%comp1%get_parameters('L',field=field,_RC)
              _RETURN(ESMF_SUCCESS)
           else if (Bside == MAPL_ExtDataRight) then
              call item%modelGridFields%comp1%get_parameters('R',field=field,_RC)
              _RETURN(ESMF_SUCCESS)
           end if
        end if

     end if
     _RETURN(ESMF_FAILURE)

  end subroutine MAPL_ExtDataGetBracket

  subroutine MAPL_ExtDataPopulateBundle(item,filec,pbundle,rc)
      type(PrimaryExport), intent(inout)      :: item
      integer,                  intent(in)    :: filec
      type(ESMF_FieldBundle), intent(inout)   :: pbundle
      integer, optional, intent(out)          :: rc
 
      integer :: status
     
      type(ESMF_Field) :: Field,field1,field2
      type(ESMF_Grid)  :: grid

      if (item%isVector) then

         call MAPL_ExtDataGetBracket(item,filec,field=Field1,vcomp=1,_RC)
         call MAPL_ExtDataGetBracket(item,filec,field=Field2,vcomp=2,_RC)

         call ESMF_FieldGet(Field1,grid=grid,rc=status)
         _VERIFY(STATUS)
         call ESMF_FieldBundleSet(pbundle,grid=grid,rc=status)
         _VERIFY(STATUS)
         call MAPL_FieldBundleAdd(pbundle,Field1,rc=status)
         _VERIFY(STATUS)
         call MAPL_FieldBundleAdd(pbundle,Field2,rc=status)
         _VERIFY(STATUS)

      else

         call MAPL_ExtDataGetBracket(item,filec,field=Field,_RC)

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
        call io_bundle%make_cfio(_RC)
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
        call io_bundle%clean(_RC)
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

  subroutine mark_update(item,ExtDataState,do_update,rc)
     type(PrimaryExport), intent(inout) :: item
     type(ESMF_State), intent(inout) :: extDataState
     logical, intent(in) :: do_update
     integer, intent(out), optional :: rc
  
     integer :: status
     type(ESMF_Info) :: info
     type(ESMF_Field) :: field

     if (item%vartype == MAPL_FieldItem) then
        call ESMF_StateGet(ExtDataState,trim(item%name),field,_RC)
        call ESMF_InfoGetFromHost(field,info,_RC)
        call ESMF_InfoSet(info,extdata_update,value=do_update,_RC)
     else if (item%vartype == MAPL_VectorField) then
        call ESMF_StateGet(ExtDataState,trim(item%vcomp1),field,_RC)
        call ESMF_InfoGetFromHost(field,info,_RC)
        call ESMF_InfoSet(info,extdata_update,value=do_update,_RC)
        call ESMF_StateGet(ExtDataState,trim(item%vcomp2),field,_RC)
        call ESMF_InfoGetFromHost(field,info,_RC)
        call ESMF_InfoSet(info,extdata_update,value=do_update,_RC)
     end if
     _RETURN(_SUCCESS)
  end subroutine

  subroutine mark_constant_field(item,ExtDataState,rc)
     type(PrimaryExport), intent(inout) :: item
     type(ESMF_State), intent(inout) :: extDataState
     integer, intent(out), optional :: rc
  
     integer :: status
     type(ESMF_Info) :: info
     type(ESMF_Field) :: field

     if (item%vartype == MAPL_FieldItem) then
        call ESMF_StateGet(ExtDataState,trim(item%name),field,_RC)
        call ESMF_InfoGetFromHost(field,info,_RC)
        call ESMF_InfoSet(info,extdata_constant_value,value=item%const,_RC)
     else if (item%vartype == MAPL_VectorField) then
        call ESMF_StateGet(ExtDataState,trim(item%vcomp1),field,_RC)
        call ESMF_InfoGetFromHost(field,info,_RC)
        call ESMF_InfoSet(info,extdata_constant_value,value=item%const,_RC)
        call ESMF_StateGet(ExtDataState,trim(item%vcomp2),field,_RC)
        call ESMF_InfoGetFromHost(field,info,_RC)
        call ESMF_InfoSet(info,extdata_constant_value,value=item%const,_RC)
     end if

  end subroutine mark_constant_field

  subroutine mark_primary_fields(item,state,rc)
     type(PrimaryExport), intent(inout) :: item
     type(ESMF_State), intent(inout) :: state
     integer, intent(out), optional :: rc

     integer :: status
     type(ESMF_Field) :: field
     type(ESMF_Info) :: info

     if (item%vartype == MAPL_FieldItem) then
        write(*,*)"mark scalar"
        call ESMF_StateGet(state,item%name,field,_RC)
        call ESMF_InfoGetFromHost(field,info,_RC)
        call ESMF_InfoSet(info,extdata_regridding_method,value=item%trans,_RC)
     else if (item%vartype == MAPL_VectorField) then
        write(*,*)"mark vector"

        call ESMF_StateGet(state,item%vcomp1,field,_RC)
        call ESMF_InfoGetFromHost(field,info,_RC)
        call ESMF_InfoSet(info,extdata_regridding_method,value=item%trans,_RC)
        call ESMF_InfoSet(info,extdata_vector_partner,value=item%vcomp2,_RC)
        call ESMF_InfoSet(info,extdata_vector_comp,value=1,_RC)

        call ESMF_StateGet(state,item%vcomp2,field,_RC)
        call ESMF_InfoGetFromHost(field,info,_RC)
        call ESMF_InfoSet(info,extdata_regridding_method,value=item%trans,_RC)
        call ESMF_InfoSet(info,extdata_vector_partner,value=item%vcomp1,_RC)
        call ESMF_InfoSet(info,extdata_vector_comp,value=2,_RC)

     end if
     _RETURN(_SUCCESS)
  end subroutine

  subroutine create_bracketing_fields(item,ExtDataState,rc)
     type(PrimaryExport), intent(inout) :: item
     type(ESMF_State), intent(inout) :: extDataState
     integer, intent(out), optional :: rc

     integer :: status
     type(ESMF_Field) :: left_field,right_field
     type(ESMF_Grid)  :: grid
     type(MAPLDataCOllection), pointer :: collection

     collection => DataCollections%at(item%pfiocollection_id)
     grid = collection%get_source_grid(_RC)
     ! here is where we could detech all metadata for non-gridded dimensions and add for now
     ! just do lm
     call GetLevs(item,_RC)
     
     item%iclient_collection_id=i_clients%add_ext_collection(trim(item%file_template))
     if (item%vartype == MAPL_FieldItem) then

        call update_field_in_state(ExtDataState,trim(item%name),grid,item%lm,_RC)

        left_field = create_simple_field(item%var,grid,item%lm,_RC)
        right_field = create_simple_field(item%var,grid,item%lm,_RC)
        call item%modelGridFields%comp1%set_parameters(left_field=left_field,right_field=right_field, _RC)

     else if (item%vartype == MAPL_VectorField) then

        call update_field_in_state(ExtDataState, trim(item%vcomp1),grid,item%lm,_RC)
        call update_field_in_state(ExtDataState, trim(item%vcomp2),grid,item%lm,_RC)

        left_field = create_simple_field(item%fcomp1,grid,item%lm,_RC)
        right_field = create_simple_field(item%fcomp1,grid,item%lm,_RC)
        call item%modelGridFields%comp1%set_parameters(left_field=left_field,right_field=right_field, _RC)

        left_field = create_simple_field(item%fcomp2,grid,item%lm,_RC)
        right_field = create_simple_field(item%fcomp2,grid,item%lm,_RC)
        call item%modelGridFields%comp2%set_parameters(left_field=left_field,right_field=right_field, _RC)

     end if

     _RETURN(_SUCCESS)

  end subroutine create_bracketing_fields

  function create_simple_field(name,grid,lm,rc) result(field)
     type(ESMF_Field) :: field
     character(len=*), intent(in) :: name
     type(ESMF_Grid), intent(in) :: grid
     integer, intent(in) :: lm
     integer, intent(out), optional :: rc
     integer :: status

     if (lm ==0) then
        field=ESMF_FieldCreate(grid,name=name,typekind=ESMF_TYPEKIND_R4,_RC)
     else 
        field=ESMF_FieldCreate(grid,name=name,typekind=ESMF_TYPEKIND_R4,ungriddedLbound=[1],ungriddedUbound=[lm],_RC)
     endif
     _RETURN(_SUCCESS)
  end function

  subroutine update_field_in_state(state,field_name,grid,lm,rc)
     type(ESMF_State), intent(inout)  :: state
     character(len=*), intent(in) :: field_name
     type(ESMF_Grid), intent(in) :: grid
     integer, intent(in) :: lm
     integer, intent(out), optional :: rc
     integer :: status

     type(ESMF_Field) :: field 

     call ESMF_StateGet(state,trim(field_name),field,_RC)
     call ESMF_StateRemove(state,[field_name],_RC)
     call ESMF_FieldDestroy(field,nogarbage=.true.,_RC)
     if (lm ==0) then
        field=ESMF_FieldCreate(grid,name=field_name,typekind=ESMF_TYPEKIND_R4,_RC)
     else 
        field=ESMF_FieldCreate(grid,name=field_name,typekind=ESMF_TYPEKIND_R4,ungriddedLbound=[1],ungriddedUbound=[lm],_RC)
     endif
     call MAPL_StateAdd(state,field,_RC)
     _RETURN(_SUCCESS)
  end subroutine 

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
      type(FileStream) :: fstream
      type(Configuration) :: config

      p = Parser('core')
      fstream=FileStream(yaml_file)
      config = p%load(fstream)
      call fstream%close()

      if (config%has("USE_EXTDATA")) then
         am_running = config%of("USE_EXTDATA")
      else
         am_running = .true.
      end if
   end function am_i_running

 END MODULE MAPL_ExtDataGridComp_Base
