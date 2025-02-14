!------------------------------------------------------------------------------
!               Global Modeling and Assimilation Office (GMAO)                !
!                    Goddard Earth Observing System (GEOS)                    !
!                                 MAPL Component                              !
!------------------------------------------------------------------------------
!
#include "MAPL_Exceptions.h"
#include "MAPL_Generic.h"
#include "unused_dummy.H"
!
!>
!### MODULE: `MAPL_ExtDataGridComp2G`
!
! Author: GMAO SI-Team
!
! `MAPL_ExtDataGridComp` is an ESMF gridded component implementing
!  an interface to boundary conditions and other types of external data
!  files.
!
!  Developed for GEOS-5 release Fortuna 2.0 and later.
!
!#### History
!- 12Dec2009:  da Silva  Design and first implementation.
!
   MODULE MAPL_ExtDataGridComp2G
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
   use MAPL_ConstantsMod, only: MAPL_RADIANS_TO_DEGREES
   use, intrinsic :: iso_fortran_env, only: REAL32
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
   use gFTL_StringIntegerMap
   use MAPL_FieldUtils
   use MAPL_ExtDataPrimaryExportVectorMod
   use MAPL_ExtDataDerivedExportVectorMod

   IMPLICIT NONE
   PRIVATE
!
! !PUBLIC MEMBER FUNCTIONS:

   PUBLIC SetServices
!-------------------------------------------------------------------------

  integer, parameter         :: MAPL_ExtDataLeft          = 1
  integer, parameter         :: MAPL_ExtDataRight         = 2
  logical                    :: hasRun
  character(len=ESMF_MAXSTR) :: error_msg_str

  type PrimaryExports
     PRIVATE
     type(integerVector) :: export_id_start
     type(integerVector) :: number_of_rules
     type(stringVector)  :: import_names
     type(PrimaryExportVector) :: item_vec
     contains
        procedure :: get_item_index
  end type PrimaryExports

  type DerivedExports
     PRIVATE
     type(stringVector)  :: import_names
     type(DerivedExportVector) :: item_vec
  end type DerivedExports

! Legacy state
! ------------
  type MAPL_ExtData_State
     PRIVATE
     type(PrimaryExports) :: Primary
     type(DerivedExports) :: Derived
     type(ESMF_State)     :: ExtDataState !! will add fields from export state to this state
                                          !! will also add new fields that could be mask
                                          !! or primary exports that were not in the export
                                          !! state recieved by ExtData, i.e. fields that are
                                          !! needed by a derived field where the primary fields
                                          !! are not actually required
     type(ESMF_Config)    :: CF
     logical              :: active = .true.
     logical              :: file_weights = .false.
  end type MAPL_ExtData_State

! Hook for the ESMF
! -----------------
  type MAPL_ExtData_Wrap
     type (MAPL_ExtData_State), pointer :: PTR => null()
  end type MAPL_ExtData_WRAP

CONTAINS


!-------------------------------------------------------------------------
!>
! Sets Initialize, Run and Finalize services for the `MAPL_ExtData` component.
!
   SUBROUTINE SetServices ( GC, RC )

    type(ESMF_GridComp), intent(INOUT) :: GC  !! gridded component
    integer, optional,   intent(OUT)   :: RC  !! return code

!-------------------------------------------------------------------------

!   Local derived type aliases
!   --------------------------
    type (MAPL_ExtData_State), pointer  :: self   ! internal, that is
    type (MAPL_ExtData_wrap)            :: wrap

    character(len=ESMF_MAXSTR)          :: comp_name
    integer                             :: status

!                              ------------

!   Get my name and set-up traceback handle
!   ---------------------------------------
    call ESMF_GridCompGet( GC, name=comp_name, _RC )

!   Wrap internal state for storing in GC; rename legacyState
!   -------------------------------------
    allocate ( self, _STAT )
    wrap%ptr => self

!                       ------------------------
!                       ESMF Functional Services
!                       ------------------------

!   Set the Initialize, Run, Finalize entry points
!   ----------------------------------------------
    call MAPL_GridCompSetEntryPoint ( GC, ESMF_METHOD_INITIALIZE,  Initialize_, _RC )
    call MAPL_GridCompSetEntryPoint ( GC, ESMF_METHOD_RUN,   Run_,        _RC )
    call MAPL_GridCompSetEntryPoint ( GC, ESMF_METHOD_FINALIZE, Finalize_,   _RC )

!   Store internal state in GC
!   --------------------------
    call ESMF_UserCompSetInternalState ( GC, 'MAPL_ExtData_state', wrap, STATUS )

    call MAPL_TimerAdd(gc,name="Initialize", _RC)
    call MAPL_TimerAdd(gc,name="Run", _RC)
    call MAPL_TimerAdd(gc,name="-Read_Loop", _RC)
    call MAPL_TimerAdd(gc,name="--CheckUpd", _RC)
    call MAPL_TimerAdd(gc,name="--Read", _RC)
    call MAPL_TimerAdd(gc,name="--GridCreate", _RC)
    call MAPL_TimerAdd(gc,name="--IclientWait", _RC)
    call MAPL_TimerAdd(gc,name="--PRead", _RC)
    call MAPL_TimerAdd(gc,name="---CreateCFIO", _RC)
    call MAPL_TimerAdd(gc,name="---prefetch", _RC)
    call MAPL_TimerAdd(gc,name="----add-collection", _RC)
    call MAPL_TimerAdd(gc,name="----make-reference", _RC)
    call MAPL_TimerAdd(gc,name="----RegridStore", _RC)
    call MAPL_TimerAdd(gc,name="----request", _RC)
    call MAPL_TimerAdd(gc,name="---IclientDone", _RC)
    call MAPL_TimerAdd(gc,name="----RegridApply", _RC)
    call MAPL_TimerAdd(gc,name="---read-prefetch", _RC)
    call MAPL_TimerAdd(gc,name="--Swap", _RC)
    call MAPL_TimerAdd(gc,name="--Bracket", _RC)
    call MAPL_TimerAdd(gc,name="-Interpolate", _RC)
!   Generic Set Services
!   --------------------
    call MAPL_GenericSetServices ( GC, _RC )

!   All done
!   --------

    _RETURN(ESMF_SUCCESS)

  END SUBROUTINE SetServices


!-------------------------------------------------------------------------
!>
! Initialize the `MAPL_ExtData` component.
!
   SUBROUTINE Initialize_ ( GC, IMPORT, EXPORT, CLOCK, rc )

   implicit NONE

   type(ESMF_Clock),  intent(inout)   :: CLOCK   !! The clock

   type(ESMF_GridComp), intent(inout) :: GC      !! Grid Component
   type(ESMF_State), intent(inout)    :: IMPORT  !! Import State
   type(ESMF_State), intent(inout)    :: EXPORT  !! Export State
   integer, intent(out)               :: rc      !! Error return code:
                                                 !!  0 - all is well
                                                 !!  1 -

!-------------------------------------------------------------------------

   type(MAPL_ExtData_state), pointer :: self        ! Legacy state
   type(ESMF_Config)                 :: CF_master          ! Universal Config

   character(len=ESMF_MAXSTR)        :: comp_name
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

   type(ExtDataOldTypesCreator), target :: config_yaml
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
   type(ESMF_StateItem_Flag) :: state_item_type
   type(PrimaryExport), allocatable :: temp_item
   type(DerivedExport), allocatable :: derived_item
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
   call MAPL_GetObjectFromGC ( gc, MAPLSTATE, _RC)
   call MAPL_TimerOn(MAPLSTATE,"TOTAL")
   call MAPL_TimerOn(MAPLSTATE,"Initialize")

   call ESMF_ConfigGetAttribute(cf_master,new_rc_file,label="EXTDATA_YAML_FILE:",default="extdata.yaml",_RC)
   call get_global_options(new_rc_file,self%active,self%file_weights,_RC)

   call ESMF_ClockGet(CLOCK, currTIME=time, _RC)
! Get information from export state
!----------------------------------
    call ESMF_StateGet(EXPORT, ITEMCOUNT=ItemCount, _RC)

    ! no need to run ExtData if there are no imports to fill
    if (ItemCount == 0) then
       self%active = .false.
    end if

    if (.not.self%active) then
       call MAPL_TimerOff(MAPLSTATE,"Initialize")
       call MAPL_TimerOff(MAPLSTATE,"TOTAL")
       _RETURN(ESMF_SUCCESS)
    end if

    call new_ExtDataOldTypesCreator(config_yaml, new_rc_file, time, _RC)

    allocate(ITEMNAMES(ITEMCOUNT), _STAT)
    allocate(ITEMTYPES(ITEMCOUNT), _STAT)

    call ESMF_StateGet(EXPORT, ITEMNAMELIST=ITEMNAMES, ITEMTYPELIST=ITEMTYPES, _RC)

!                               --------
!  Initialize MAPL Generic
!  -----------------------
   call MAPL_GenericInitialize ( GC, IMPORT, EXPORT, clock,  _RC )

!                         ---------------------------
!                         Parse ExtData Resource File
!                         ---------------------------
   self%ExtDataState = ESMF_StateCreate(Name="ExtDataNameSpace",_RC)
   num_primary=0
   num_derived=0
   primaryitemcount=0
   deriveditemcount=0
   do i=1,size(itemnames)
      item_type = config_yaml%get_item_type(trim(itemnames(i)), _RC)
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
      if (.not.string_in_stringVector(primary_var_name,self%primary%import_names)) then
         call create_holding_field(self%ExtDataState,primary_var_name,derived_var_name,_RC)
         call self%primary%import_names%push_back(primary_var_name)
         primaryItemCount=primaryItemCount+config_yaml%count_rules_for_item(primary_var_name,_RC)
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
            allocate(temp_item)
            call config_yaml%fillin_primary(current_base_name//"+"//sidx,current_base_name,temp_item,time,clock,_RC)
            _ASSERT(status==0, "ExtData multi-rule problem with BASE NAME "//TRIM(current_base_name))
            allocate(temp_item%start_end_time(2))
            temp_item%start_end_time(1)=time_ranges(j)
            temp_item%start_end_time(2)=time_ranges(j+1)
            call self%primary%item_vec%push_back(temp_item)
            deallocate(temp_item)
         enddo
      else
         num_primary=num_primary+1
         allocate(temp_item)
         call config_yaml%fillin_primary(current_base_name,current_base_name,temp_item,time,clock,_RC)
         call self%primary%item_vec%push_back(temp_item)
         deallocate(temp_item)
         _ASSERT(status==0, "ExtData single-rule problem with BASE NAME "//TRIM(current_base_name))
      end if
      call ESMF_StateGet(Export,current_base_name,state_item_type,_RC)
      if (state_item_type /= ESMF_STATEITEM_NOTFOUND) then
         call ESMF_StateGet(Export,current_base_name,field,_RC)
         call MAPL_StateAdd(self%ExtDataState,field,_RC)
         item_type = config_yaml%get_item_type(current_base_name)
         if (item_type == Primary_Type_Vector_comp1) then
            item => self%primary%item_vec%at(num_primary)
            call ESMF_StateGet(Export,item%vcomp2,field,_RC)
            call MAPL_StateAdd(self%ExtDataState,field,_RC)
         end if
      end if
   enddo
   do i=1,self%derived%import_names%size()
      current_base_name => self%derived%import_names%at(i)
      num_derived=num_derived+1
      allocate(derived_item)
      call config_yaml%fillin_derived(current_base_name,derived_item,time,clock,_RC)
      call self%derived%item_vec%push_back(derived_item)
      call ESMF_StateGet(Export,current_base_name,field,_RC)
      call MAPL_StateAdd(self%ExtDataState,field,_RC)
      deallocate(derived_item)
   enddo

   ! now see if we have to allocate any primary fields due to a derived item
   ! also see if we have to allocate any primary fields due to PS
   PrimaryLoop: do i=1,self%primary%import_names%size()

      current_base_name => self%primary%import_names%at(i)
      idx = self%primary%get_item_index(current_base_name,time,_RC)
      item => self%primary%item_vec%at(idx)

      item%pfioCOllection_id = MAPL_DataAddCollection(item%file_template)
      call create_primary_field(item,self%ExtDataState,time,_RC)
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
      !self%primary%item(idx)%units = ESMF_UtilStringUppercase(self%primary%item(idx)%units,_RC)
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
!>
! Run the `MAPL_ExtData` component.
!
   SUBROUTINE Run_ ( GC, IMPORT, EXPORT, CLOCK, rc )

   implicit NONE

   type(ESMF_Clock),  intent(inout) :: CLOCK     !! The clock

   type(ESMF_GridComp), intent(inout)  :: GC     !! Grid Component
   type(ESMF_State), intent(inout) :: IMPORT     !! Import State
   type(ESMF_State), intent(inout) :: EXPORT     !! Export State
   integer, intent(out) ::  rc                   !! Error return code:
                                                 !!  0 - all is well
                                                 !!  1 -

!-------------------------------------------------------------------------

   type(MAPL_ExtData_state), pointer :: self        ! Legacy state
   type(ESMF_Config)                 :: CF          ! Universal Config

   character(len=ESMF_MAXSTR)        :: comp_name
   integer                           :: status

   type(PrimaryExport), pointer      :: item
   type(DerivedExport), pointer      :: derivedItem
   integer                           :: i

   type(ESMF_Time)                   :: use_time, current_time
   type(MAPL_MetaComp), pointer      :: MAPLSTATE

   logical                           :: doUpdate_
   character(len=ESMF_MAXPATHLEN)    :: file_processed
   logical, allocatable              :: do_pointer_update(:)
   type(ESMF_Time), allocatable      :: useTime(:)

   integer                           :: bracket_side
   integer                           :: entry_num
   type(IOBundleNGVector), target     :: IOBundles
   type(IOBundleNGVectorIterator) :: bundle_iter
   type(ExtDataNG_IOBundle), pointer :: io_bundle
   character(len=:), pointer :: current_base_name
   integer :: idx,nitems
   type(ESMF_Config) :: cf_master
   type(ESMF_Time) :: adjusted_time

   _UNUSED_DUMMY(IMPORT)
   _UNUSED_DUMMY(EXPORT)

!  Get my name and set-up traceback handle
!  ---------------------------------------
   call ESMF_GridCompGet( GC, name=comp_name, config=CF_master, _RC )

!  Extract relevant runtime information
!  ------------------------------------
   call extract_ ( GC, self, CF, _RC )

   if (.not. self%active) then
      _RETURN(ESMF_SUCCESS)
   end if

   call MAPL_GetObjectFromGC ( gc, MAPLSTATE, _RC)
   call MAPL_TimerOn(MAPLSTATE,"TOTAL")
   call MAPL_TimerOn(MAPLSTATE,"Run")

   call ESMF_ClockGet(CLOCK, currTIME=current_time, _RC)

!  Fill in the internal state with data from the files
!  ---------------------------------------------------

   allocate(do_pointer_update(self%primary%item_vec%size()),_STAT)
   do_pointer_update = .false.
   allocate(useTime(self%primary%item_vec%size()),_STAT)

   call MAPL_TimerOn(MAPLSTATE,"-Read_Loop")

   call extdata_lgr%debug('ExtData Rune_(): Start')
   call extdata_lgr%debug('ExtData Run_(): READ_LOOP: Start')

   READ_LOOP: do i=1,self%primary%import_names%size()

      current_base_name => self%primary%import_names%at(i)
      idx = self%primary%get_item_index(current_base_name,current_time,_RC)
      item => self%primary%item_vec%at(idx)

      if (.not.item%initialized) then
         item%pfioCollection_id = MAPL_DataAddCollection(item%file_template)
         if (item%isConst) then
            call set_constant_field(item,self%extDataState,_RC)
            cycle
         end if
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

      call item%update_freq%check_update(do_pointer_update(i),use_time,current_time,.not.hasRun,_RC)
      adjusted_time = item%update_freq%get_adjusted_time(current_time)
      call MAPL_TimerOff(MAPLSTATE,"--CheckUpd")

      !call extdata_lgr%info('Going to update %a with file template: %a ',current_base_name, item%file_template)
      call item%modelGridFields%comp1%reset()
      call item%filestream%get_file_bracket(use_time,item%source_time, item%modelGridFields%comp1,item%fail_on_missing_file, _RC)
      if (item%vartype == MAPL_VectorField) then
         call item%filestream%get_file_bracket(use_time,item%source_time, item%modelGridFields%comp2, item%fail_on_missing_file,_RC)
      end if
      call create_bracketing_fields(item,self%ExtDataState,cf_master, _RC)
      call IOBundle_Add_Entry(IOBundles,item,idx)
      useTime(i)=use_time

   end do READ_LOOP

   call extdata_lgr%debug('ExtData Run_: READ_LOOP: Done')

   bundle_iter = IOBundles%begin()
   do while (bundle_iter /= IoBundles%end())
      io_bundle => bundle_iter%get()
      bracket_side = io_bundle%bracket_side
      entry_num = io_bundle%entry_index
      file_Processed = io_bundle%file_name
      item => self%primary%item_vec%at(entry_num)

      io_bundle%pbundle = ESMF_FieldBundleCreate(_RC)

      call MAPL_ExtDataPopulateBundle(item,bracket_side,io_bundle%pbundle,_RC)
      call bundle_iter%next()
   enddo

   call MAPL_TimerOn(MAPLSTATE,"--PRead")
   call MAPL_TimerOn(MAPLSTATE,"---CreateCFIO")
   call MAPL_ExtDataCreateCFIO(IOBundles, _RC)
   call MAPL_TimerOff(MAPLSTATE,"---CreateCFIO")

   call MAPL_TimerOn(MAPLSTATE,"---prefetch")
   call MAPL_ExtDataPrefetch(IOBundles, file_weights=self%file_weights, _RC)
   call MAPL_TimerOff(MAPLSTATE,"---prefetch")
   call MAPL_TimerOn(MAPLSTATE,"---IclientDone")

   call i_Clients%done_collective_prefetch(_RC)
   call i_Clients%wait(_RC)

   call MAPL_TimerOff(MAPLSTATE,"---IclientDone")

   call MAPL_TimerOn(MAPLSTATE,"---read-prefetch")
   call MAPL_ExtDataReadPrefetch(IOBundles,_RC)
   call MAPL_TimerOff(MAPLSTATE,"---read-prefetch")
   call MAPL_TimerOff(MAPLSTATE,"--PRead")

   bundle_iter = IOBundles%begin()
   do while (bundle_iter /= IOBundles%end())
      io_bundle => bundle_iter%get()
      bracket_side = io_bundle%bracket_side
      entry_num = io_bundle%entry_index
      item => self%primary%item_vec%at(entry_num)
      call MAPL_ExtDataVerticalInterpolate(self,item,bracket_side,current_time,_RC)
      call bundle_iter%next()
   enddo
   call MAPL_ExtDataDestroyCFIO(IOBundles,_RC)

   call MAPL_TimerOff(MAPLSTATE,"-Read_Loop")

   call MAPL_TimerOn(MAPLSTATE,"-Interpolate")

   call extdata_lgr%debug('ExtData Run_: INTERP_LOOP: Start')

   INTERP_LOOP: do i=1,self%primary%import_names%size()

      current_base_name => self%primary%import_names%at(i)
      idx = self%primary%get_item_index(current_base_name,current_time,_RC)
      item => self%primary%item_vec%at(idx)

      if (do_pointer_update(i)) then

         call extdata_lgr%debug('ExtData Run_: INTERP_LOOP: interpolating between bracket times, variable: %a, file: %a', &
              & trim(current_base_name), trim(item%file_template))

         call MAPL_ExtDataInterpField(item,self%ExtDataState,useTime(i),_RC)

      endif

      nullify(item)

   end do INTERP_LOOP

   call extdata_lgr%debug('ExtData Run_: INTERP_LOOP: Done')

   call MAPL_TimerOff(MAPLSTATE,"-Interpolate")

   ! now take care of derived fields
   do i=1,self%derived%item_vec%size()

      derivedItem => self%derived%item_vec%at(i)

      call derivedItem%update_freq%check_update(doUpdate_,use_time,current_time,.not.hasRun,_RC)

      if (doUpdate_) then

         call derivedItem%evaluate_derived_field(self%ExtDataState,_RC)

      end if

   end do

   call extdata_lgr%debug('ExtData Run_: End')

!  All done
!  --------
   deallocate(useTime)

   if (hasRun .eqv. .false.) hasRun = .true.
   call MAPL_TimerOff(MAPLSTATE,"Run")
   call MAPL_TimerOff(MAPLSTATE,"TOTAL")

   _RETURN(ESMF_SUCCESS)

   END SUBROUTINE Run_

!-------------------------------------------------------------------------
!>
! Finalize the `MAPL_ExtData` component.
!
   SUBROUTINE Finalize_ ( GC, IMPORT, EXPORT, CLOCK, rc )

   implicit NONE

   type(ESMF_Clock),  intent(inout) :: CLOCK     !! The clock

   type(ESMF_GridComp), intent(inout)  :: GC     !! Grid Component
   type(ESMF_State), intent(inout) :: IMPORT     !! Import State
   type(ESMF_State), intent(inout) :: EXPORT     !! Export State
   integer, intent(out) ::  rc                   !! Error return code:
                                                 !!  0 - all is well
                                                 !!  1 -

!-------------------------------------------------------------------------

   type(MAPL_ExtData_state), pointer :: self        ! Legacy state
   type(ESMF_Config)                 :: CF          ! Universal Config

   character(len=ESMF_MAXSTR)        :: comp_name
   integer                           :: status


!  Get my name and set-up traceback handle
!  ---------------------------------------
   call ESMF_GridCompGet( GC, name=comp_name, _RC )

!  Finalize MAPL Generic
!  ---------------------
   call MAPL_GenericFinalize ( GC, IMPORT, EXPORT, CLOCK,  _RC )

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
    integer                    :: status

    type(MAPL_ExtData_Wrap)  :: wrap

!   Get my name and set-up traceback handle
!   ---------------------------------------
    call ESMF_GridCompGet( GC, NAME=comp_name, _RC )

    If (present(rc))  rc=ESMF_SUCCESS

!   Get my internal state
!   ---------------------
    call ESMF_UserCompGetInternalState(gc, 'MAPL_ExtData_state', WRAP, STATUS)
    self => wrap%ptr

!   Get the configuration
!   ---------------------
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
        integer :: i

        positive=>null()
        var => null()
        if (item%vartype == MAPL_VectorField) then
           var=>item%file_metadata%get_variable(trim(item%fcomp1))
           _ASSERT(associated(var),"Variable "//TRIM(item%fcomp1)//" not found in file "//TRIM(item%file_template))
           var => null()
           var=>item%file_metadata%get_variable(trim(item%fcomp2))
           _ASSERT(associated(var),"Variable "//TRIM(item%fcomp2)//" not found in file "//TRIM(item%file_template))
        else
           var=>item%file_metadata%get_variable(trim(item%var))
           _ASSERT(associated(var),"Variable "//TRIM(item%var)//" not found in file "//TRIM(item%file_template))
        end if

        levName = item%file_metadata%get_level_name(_RC)
        if (trim(levName) /='') then
           call item%file_metadata%get_coordinate_info(levName,coordSize=item%lm,coordUnits=tLevUnits,coords=levFile,_RC)
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

           if (.not.allocated(item%levs)) allocate(item%levs(item%lm),__STAT__)
           item%levs=levFile
           if (trim(item%fileVDir)/=trim(item%importVDir)) then
              do i=1,size(levFile)
                 item%levs(i)=levFile(size(levFile)-i+1)
              enddo
           end if
           if (trim(item%levunit)=='hpa') item%levs=item%levs*100.0
           if (item%vartype == MAPL_VectorField) then
              item%units = item%file_metadata%get_variable_attribute(trim(item%fcomp1),"units",_RC)
           else
              item%units = item%file_metadata%get_variable_attribute(trim(item%var),"units",_RC)
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

     call ESMF_StateGet(state,item%vcomp1,field,_RC)
     call item%modelGridFields%comp1%interpolate_to_time(field,time,_RC)
     if (item%vartype == MAPL_VectorField) then
        call ESMF_StateGet(state,item%vcomp2,field,_RC)
        call item%modelGridFields%comp2%interpolate_to_time(field,time,_RC)
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
     type(PrimaryExport), pointer      :: ps_item

     if (item%do_VertInterp) then
        if (trim(item%importVDir)/=trim(item%fileVDir)) then
           call MAPL_ExtDataFlipVertical(item,filec,_RC)
        end if
        if (item%vartype == MAPL_fieldItem) then
           call MAPL_ExtDataGetBracket(item,filec,newField,getRL=.true.,_RC)
           call MAPL_ExtDataGetBracket(item,filec,Field,_RC)
           id_ps = ExtState%primary%get_item_index("PS",current_time,_RC)
           ps_item => ExtState%primary%item_vec%at(id_ps)
           call MAPL_ExtDataGetBracket(ps_item,filec,field=psF,_RC)
           call vertInterpolation_pressKappa(field,newfield,psF,item%levs,MAPL_UNDEF,_RC)

        else if (item%vartype == MAPL_VectorField) then

           id_ps = ExtState%primary%get_item_index("PS",current_time,_RC)
           ps_item => ExtState%primary%item_vec%at(id_ps)
           call MAPL_ExtDataGetBracket(ps_item,filec,field=psF,_RC)
           call MAPL_ExtDataGetBracket(item,filec,newField,getRL=.true.,vcomp=1,_RC)
           call MAPL_ExtDataGetBracket(item,filec,Field,vcomp=1,_RC)
           call vertInterpolation_pressKappa(field,newfield,psF,item%levs,MAPL_UNDEF,_RC)
           call MAPL_ExtDataGetBracket(item,filec,newField,getRL=.true.,vcomp=2,_RC)
           call MAPL_ExtDataGetBracket(item,filec,Field,vcomp=2,_RC)
           call vertInterpolation_pressKappa(field,newfield,psF,item%levs,MAPL_UNDEF,_RC)

        end if

     else if (item%do_Fill) then
        if (item%vartype == MAPL_fieldItem) then
           call MAPL_ExtDataGetBracket(item,filec,newField,getRL=.true.,_RC)
           call MAPL_ExtDataGetBracket(item,filec,Field,_RC)
           call MAPL_ExtDataFillField(item,field,newfield,_RC)
        else if (item%vartype == MAPL_VectorField) then
           call MAPL_ExtDataGetBracket(item,filec,newField,getRL=.true.,vcomp=1,_RC)
           call MAPL_ExtDataGetBracket(item,filec,Field,vcomp=1,_RC)
           call MAPL_ExtDataFillField(item,field,newfield,_RC)
           call MAPL_ExtDataGetBracket(item,filec,newField,getRL=.true.,vcomp=2,_RC)
           call MAPL_ExtDataGetBracket(item,filec,Field,vcomp=2,_RC)
           call MAPL_ExtDataFillField(item,field,newfield,_RC)
        end if
     else
        if (trim(item%importVDir)/=trim(item%fileVDir)) then
           call MAPL_ExtDataFlipVertical(item,filec,_RC)
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

     character(len=ESMF_MAXSTR) :: gname, comp_name
     integer :: counts(3)
     integer :: NX,NY
     type(ESMF_Grid)           :: newGrid
     type(ESMF_Config)         :: cflocal
     character(len=*), parameter :: CF_COMPONENT_SEPARATOR = '.'
     real :: temp_real
     logical :: isPresent
     type(ESMF_Info) :: infoh

     call MAPL_GridGet(grid,globalCellCountPerDim=counts,_RC)
     call ESMF_GridGet(grid,name=gName,_RC)
     call ESMF_ConfigGetAttribute(CF, value = NX, Label="NX:", _RC)
     call ESMF_ConfigGetAttribute(CF, value = NY, Label="NY:", _RC)

     comp_name = "ExtData"
     cflocal = MAPL_ConfigCreate(_RC)
     call MAPL_ConfigSetAttribute(cflocal,value=NX, label=trim(COMP_Name)//CF_COMPONENT_SEPARATOR//"NX:",_RC)
     call MAPL_ConfigSetAttribute(cflocal,value=lm, label=trim(COMP_Name)//CF_COMPONENT_SEPARATOR//"LM:",_RC)

     if (counts(2) == 6*counts(1)) then
        call MAPL_ConfigSetAttribute(cflocal,value="Cubed-Sphere", label=trim(COMP_Name)//CF_COMPONENT_SEPARATOR//"GRID_TYPE:",_RC)
        call MAPL_ConfigSetAttribute(cflocal,value=6, label=trim(COMP_Name)//CF_COMPONENT_SEPARATOR//"NF:",_RC)
        call MAPL_ConfigSetAttribute(cflocal,value=counts(1), label=trim(COMP_Name)//CF_COMPONENT_SEPARATOR//"IM_WORLD:",_RC)
        call MAPL_ConfigSetAttribute(cflocal,value=ny/6, label=trim(COMP_Name)//CF_COMPONENT_SEPARATOR//"NY:",_RC)
        call MAPL_ConfigSetAttribute(cflocal,value=trim(gname), label=trim(COMP_Name)//CF_COMPONENT_SEPARATOR//"GRIDNAME:",_RC)

        call ESMF_InfoGetFromHost(grid,infoh,_RC)
        if (isPresent) then
           call ESMF_InfoGet(infoh,'STRETCH_FACTOR',temp_real,_RC)
           call MAPL_ConfigSetAttribute(cflocal,value=temp_real, label=trim(COMP_Name)//MAPL_CF_COMPONENT_SEPARATOR//"STRETCH_FACTOR:",_RC)
        endif

        isPresent = ESMF_InfoIsPresent(infoh,'TARGET_LON',_RC)
        if (isPresent) then
           call ESMF_InfoGet(infoh,'TARGET_LON',temp_real,_RC)
           call MAPL_ConfigSetAttribute(cflocal,value=temp_real*MAPL_RADIANS_TO_DEGREES, label=trim(COMP_Name)//MAPL_CF_COMPONENT_SEPARATOR//"TARGET_LON:",_RC)
        endif

        isPresent = ESMF_InfoIsPresent(infoh,'TARGET_LAT',_RC)
        if (isPresent) then
           call ESMF_InfoGet(infoh,'TARGET_LAT',temp_real,_RC)
           call MAPL_ConfigSetAttribute(cflocal,value=temp_real*MAPL_RADIANS_TO_DEGREES, label=trim(COMP_Name)//MAPL_CF_COMPONENT_SEPARATOR//"TARGET_LAT:",_RC)
        endif
     else
        call MAPL_ConfigSetAttribute(cflocal,value=counts(1), label=trim(COMP_Name)//CF_COMPONENT_SEPARATOR//"IM_WORLD:",_RC)
        call MAPL_ConfigSetAttribute(cflocal,value=counts(2), label=trim(COMP_Name)//CF_COMPONENT_SEPARATOR//"JM_WORLD:",_RC)
        call MAPL_ConfigSetAttribute(cflocal,value=ny, label=trim(COMP_Name)//CF_COMPONENT_SEPARATOR//"NY:",_RC)
        call MAPL_ConfigSetAttribute(cflocal,value=trim(gname), label=trim(COMP_Name)//CF_COMPONENT_SEPARATOR//"GRIDNAME:",_RC)
     end if
     newgrid = grid_manager%make_grid(cflocal, prefix=trim(COMP_Name)//".", _RC)

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

     integer :: status

     logical :: getRL_

     if (present(getRL)) then
        getRL_=getRL
     else
        getRL_=.false.
     end if

     if (present(vcomp)) then

        if (present(field)) then

           if (Bside == MAPL_ExtDataLeft .and. vcomp == 1) then
              if (getRL_) then
                 call item%modelGridFields%auxiliary1%get_parameters('L',field=field,_RC)
                 _RETURN(ESMF_SUCCESS)
              else
                 call item%modelGridFields%comp1%get_parameters('L',field=field,_RC)
                 _RETURN(ESMF_SUCCESS)
              end if
           else if (Bside == MAPL_ExtDataLeft .and. vcomp == 2) then
              if (getRL_) then
                 call item%modelGridFields%auxiliary2%get_parameters('L',field=field,_RC)
                 _RETURN(ESMF_SUCCESS)
              else
                 call item%modelGridFields%comp2%get_parameters('L',field=field,_RC)
                 _RETURN(ESMF_SUCCESS)
              end if
           else if (Bside == MAPL_ExtDataRight .and. vcomp == 1) then
              if (getRL_) then
                 call item%modelGridFields%auxiliary1%get_parameters('R',field=field,_RC)
                 _RETURN(ESMF_SUCCESS)
              else
                 call item%modelGridFields%comp1%get_parameters('R',field=field,_RC)
                 _RETURN(ESMF_SUCCESS)
              end if
           else if (Bside == MAPL_ExtDataRight .and. vcomp == 2) then
              if (getRL_) then
                 call item%modelGridFields%auxiliary2%get_parameters('R',field=field,_RC)
                 _RETURN(ESMF_SUCCESS)
              else
                 call item%modelGridFields%comp2%get_parameters('R',field=field,_RC)
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
                 call item%modelGridFields%auxiliary1%get_parameters('L',field=field,_RC)
                 _RETURN(ESMF_SUCCESS)
              else
                 call item%modelGridFields%comp1%get_parameters('L',field=field,_RC)
                 _RETURN(ESMF_SUCCESS)
              end if
           else if (Bside == MAPL_ExtDataRight) then
              if (getRL_) then
                 call item%modelGridFields%auxiliary1%get_parameters('R',field=field,_RC)
                 _RETURN(ESMF_SUCCESS)
              else
                 call item%modelGridFields%comp1%get_parameters('R',field=field,_RC)
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

  integer :: status

  real, pointer :: ptrF(:,:,:),ptrR(:,:,:)
  integer :: lm_in,lm_out,i

  call ESMF_FieldGet(FieldF,0,farrayPtr=ptrF,_RC)
  call ESMF_FieldGet(FieldR,0,farrayPtr=ptrR,_RC)
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

      if (item%vartype == MAPL_VectorField) then

         call MAPL_ExtDataGetBracket(item,filec,field=Field1,vcomp=1,_RC)
         call MAPL_ExtDataGetBracket(item,filec,field=Field2,vcomp=2,_RC)

         call ESMF_FieldGet(Field1,0,farrayPtr=ptr,_RC)
         allocate(ptemp,source=ptr,_STAT)
         ls = lbound(ptr,3)
         le = ubound(ptr,3)
         ptr(:,:,le:ls:-1) = ptemp(:,:,ls:le:+1)

         call ESMF_FieldGet(Field2,0,farrayPtr=ptr,_RC)
         ptemp=ptr
         ptr(:,:,le:ls:-1) = ptemp(:,:,ls:le:+1)

         deallocate(ptemp)

      else

         call MAPL_ExtDataGetBracket(item,filec,field=Field,_RC)

         call ESMF_FieldGet(Field,0,farrayPtr=ptr,_RC)
         allocate(ptemp,source=ptr,_STAT)
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

      if (item%vartype == MAPL_VectorField) then

         call MAPL_ExtDataGetBracket(item,filec,field=Field1,vcomp=1,_RC)
         call MAPL_ExtDataGetBracket(item,filec,field=Field2,vcomp=2,_RC)

         call ESMF_FieldGet(Field1,grid=grid,_RC)
         call ESMF_FieldBundleSet(pbundle,grid=grid,_RC)
         call MAPL_FieldBundleAdd(pbundle,Field1,_RC)
         call MAPL_FieldBundleAdd(pbundle,Field2,_RC)

      else

         call MAPL_ExtDataGetBracket(item,filec,field=Field,_RC)

         call ESMF_FieldGet(Field,grid=grid,_RC)
         call ESMF_FieldBundleSet(pbundle,grid=grid,_RC)
         call MAPL_FieldBundleAdd(pbundle,Field,_RC)

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
        call io_bundle%make_io(_RC)
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

  subroutine MAPL_ExtDataPrefetch(IOBundles,file_weights,rc)
     type(IOBundleNGVector), target, intent(inout) :: IOBundles
     logical, intent(in) :: file_weights
     integer, optional,      intent(out  ) :: rc

     integer :: n,nfiles,regrid_hints
     type(ExtDataNG_IOBundle), pointer :: io_bundle => null()
     integer :: status

     nfiles = IOBundles%size()

     regrid_hints = 0
     if (file_weights) regrid_hints = IOR(regrid_hints,REGRID_HINT_FILE_WEIGHTS)

     do n = 1, nfiles
        io_bundle => IOBundles%at(n)
        if (io_bundle%on_tiles) then
           call io_bundle%tile_io%request_data_from_file(io_bundle%file_name,io_bundle%time_index,_RC)
        else
           call io_bundle%grid_io%set_param(regrid_hints=regrid_hints)
           call io_bundle%grid_io%request_data_from_file(io_bundle%file_name,io_bundle%time_index,_RC)
        end if
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
        if (io_bundle%on_tiles) then
           call io_bundle%tile_io%process_data_from_file(_RC)
        else
           call io_bundle%grid_io%process_data_from_file(_RC)
        end if
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

     call item%modelGridFields%comp1%get_parameters('L',field=field,_RC)
     newGrid = MAPL_ExtDataGridChangeLev(grid,cf,item%lm,_RC)
     new_field = MAPL_FieldCreate(field,newGrid,lm=item%lm,newName=trim(item%fcomp1),_RC)
     call item%modelGridFields%auxiliary1%set_parameters(left_field=new_field,_RC)
     new_field = MAPL_FieldCreate(field,newGrid,lm=item%lm,newName=trim(item%fcomp1),_RC)
     call item%modelGridFields%auxiliary1%set_parameters(right_field=new_field,_RC)
     if (item%vartype==MAPL_VectorField) then
        new_field = MAPL_FieldCreate(field,newGrid,lm=item%lm,newName=trim(item%fcomp2),_RC)
        call item%modelGridFields%auxiliary2%set_parameters(left_field=new_field,_RC)
        new_field = MAPL_FieldCreate(field,newGrid,lm=item%lm,newName=trim(item%fcomp2),_RC)
        call item%modelGridFields%auxiliary2%set_parameters(right_field=new_field,_RC)
     end if
     _RETURN(_SUCCESS)

  end subroutine createFileLevBracket


  subroutine IOBundle_Add_Entry(IOBundles,item,entry_num,rc)
     type(IOBundleNGVector), intent(inout) :: IOBundles
     type(primaryExport), target, intent(inout)        :: item
     integer, intent(in)                    :: entry_num
     integer, intent(out), optional         :: rc

     integer :: status

     type (ExtDataNG_IOBundle) :: io_bundle
     type (GriddedIOItemVector) :: itemsL, itemsR
     logical :: update
     character(len=ESMF_MAXPATHLEN) :: current_file
     integer :: time_index
     type(StringIntegerMap), pointer :: dimensions
     integer, pointer :: tile_size
     logical :: on_tiles

     dimensions => item%file_metadata%get_dimensions()
     tile_size => dimensions%at("tile_index")
     on_tiles = associated(tile_size)
     call item%modelGridFields%comp1%get_parameters('L',update=update,file=current_file,time_index=time_index)
     if (update) then
        if (trim(current_file)/=file_not_found) then
           call itemsL%push_back(item%fileVars)
           io_bundle = ExtDataNG_IOBundle(MAPL_ExtDataLeft, entry_num, current_file, time_index, item%trans, item%fracval, item%file_template, &
               item%pfioCollection_id,item%iclient_collection_id,itemsL,on_tiles,_RC)
           call IOBundles%push_back(io_bundle)
           call extdata_lgr%info('%a updated L bracket with: %a at time index %i0 ',item%name, current_file, time_index)
        end if
     end if
     call item%modelGridFields%comp1%get_parameters('R',update=update,file=current_file,time_index=time_index)
     if (update) then
        if (trim(current_file)/=file_not_found) then
           call itemsR%push_back(item%fileVars)
           io_bundle = ExtDataNG_IOBundle(MAPL_ExtDataRight, entry_num, current_file, time_index, item%trans, item%fracval, item%file_template, &
               item%pfioCollection_id,item%iclient_collection_id,itemsR,on_tiles,_RC)
           call IOBundles%push_back(io_bundle)
           call extdata_lgr%info('%a updated R bracket with: %a at time index %i0 ',item%name,current_file, time_index)
        end if
     end if

     _RETURN(ESMF_SUCCESS)

  end subroutine IOBundle_Add_Entry

  subroutine set_constant_field(item,ExtDataState,rc)
     type(PrimaryExport), intent(inout) :: item
     type(ESMF_State), intent(inout) :: extDataState
     integer, intent(out), optional :: rc

     integer :: status
     type(ESMF_Field) :: field

     if (item%vartype == MAPL_FieldItem) then
        call ESMF_StateGet(ExtDataState,trim(item%name),field,_RC)
        call FieldSet(field, item%const, _RC)
     else if (item%vartype == MAPL_VectorField) then
        call ESMF_StateGet(ExtDataState,trim(item%vcomp1),field,_RC)
        call FieldSet(field, item%const, _RC)
        call ESMF_StateGet(ExtDataState,trim(item%vcomp2),field,_RC)
        call FieldSet(field, item%const, _RC)
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
     character(len=ESMF_MAXPATHLEN) :: file_left, file_right, filename
     logical :: found_file
     type(FileMetadataUtils), pointer :: metadata
     type(MAPLDataCollection), pointer :: collection

     if (item%modelGridFields%initialized) then
        _RETURN(_SUCCESS)
     else
        found_file = .false.
        call item%modelGridFields%comp1%get_parameters('L',file=file_left)
        if (trim(file_left) /= file_not_found) then
           filename = file_left
           found_file = .true.
        else
           call item%modelGridFields%comp1%get_parameters('R',file=file_right)
           if (trim(file_right) /= file_not_found) then
              filename = file_right
              found_file = .true.
           end if
        end if
        if (found_file) then
           collection => DataCollections%at(item%pfioCollection_id)
           metadata => collection%find(filename,_RC)
           item%file_metadata = metadata
           item%modelGridFields%initialized = .true.
        end if
     end if

     if (found_file) then
        call GetLevs(item,_RC)
        item%iclient_collection_id=i_clients%add_ext_collection(trim(item%file_template))
        if (item%vartype == MAPL_FieldItem) then

           call ESMF_StateGet(ExtDataState, trim(item%name), field,_RC)
           call ESMF_FieldGet(field,grid=grid,rank=fieldRank,_RC)

           lm=0
           if (fieldRank==3) then
              call ESMF_FieldGet(field,0,farrayPtr=ptr3d,_RC)
              lm = size(ptr3d,3)
           end if
           if (item%lm /= lm .and. lm /= 0 .and. item%havePressure) then
              item%do_VertInterp = .true.
           else if (item%lm /= lm .and. lm /= 0 .and. item%lm /= 0) then
              item%do_Fill = .true.
           end if
           left_field = MAPL_FieldCreate(field,item%var,doCopy=.true.,_RC)
           right_field = MAPL_FieldCreate(field,item%var,doCopy=.true.,_RC)
           call item%modelGridFields%comp1%set_parameters(left_field=left_field,right_field=right_field, _RC)
           if (item%do_fill .or. item%do_vertInterp) then
              call createFileLevBracket(item,cf,_RC)
           end if

        else if (item%vartype == MAPL_VectorField) then

           call ESMF_StateGet(ExtDataState, trim(item%vcomp1), field,_RC)
           call ESMF_FieldGet(field,grid=grid,rank=fieldRank,_RC)

           lm = 0
           if (fieldRank==3) then
              call ESMF_FieldGet(field,0,farrayPtr=ptr3d,_RC)
              lm = size(ptr3d,3)
           end if
           if (item%lm /= lm .and. item%havePressure) then
              item%do_VertInterp = .true.
           else if (item%lm /= lm .and. lm /= 0) then
              item%do_Fill = .true.
           end if

           left_field = MAPL_FieldCreate(field,item%fcomp1,doCopy=.true.,_RC)
           right_field = MAPL_FieldCreate(field,item%fcomp1,doCopy=.true.,_RC)
           call item%modelGridFields%comp1%set_parameters(left_field=left_field,right_field=right_field, _RC)
           call ESMF_StateGet(ExtDataState, trim(item%vcomp2), field,_RC)
           left_field = MAPL_FieldCreate(field,item%fcomp2,doCopy=.true.,_RC)
           right_field = MAPL_FieldCreate(field,item%fcomp2,doCopy=.true.,_RC)
           call item%modelGridFields%comp2%set_parameters(left_field=left_field,right_field=right_field, _RC)

           if (item%do_fill .or. item%do_vertInterp) then
              call createFileLevBracket(item,cf,_RC)
           end if

        end if

     end if

     _RETURN(_SUCCESS)
  end subroutine create_bracketing_fields

  subroutine create_holding_field(state,primary_name,derived_name,rc)
     type(ESMF_State), intent(inout) :: state
     character(len=*), intent(in) :: primary_name
     character(len=*), intent(in) :: derived_name
     integer, optional, intent(out) :: rc

     integer :: status
     type(ESMF_Field) :: field

     field = ESMF_FieldEmptyCreate(name=primary_name,_RC)
     call ESMF_AttributeSet(field,name="derived_source",value=derived_name,_RC)
     call MAPL_StateAdd(state,field,_RC)

     _RETURN(_SUCCESS)
  end subroutine

  subroutine create_primary_field(item,ExtDataState,current_time,rc)
     type(PrimaryExport), intent(inout) :: item
     type(ESMF_State), intent(inout) :: extDataState
     type(ESMF_Time), intent(in) :: current_time
     integer, intent(out), optional :: rc

     integer :: status
     type(ESMF_Field) :: field,derived_field
     type(ESMF_Grid)  :: grid
     logical :: must_create
     character(len=ESMF_MAXSTR) :: derived_field_name
     type(FileMetadataUtils), pointer :: metadata
     type(MAPLDataCollection), pointer :: collection
     character(len=ESMF_MAXPATHLEN) :: filename
     logical :: file_found

     call ESMF_StateGet(ExtDataState,trim(item%name),field,_RC)
     call ESMF_FieldValidate(field,_RC)
     call ESMF_AttributeGet(field,name="derived_source",isPresent=must_create,_RC)
     if (.not.must_create) then
        _RETURN(_SUCCESS)
     end if
     if (index(item%file_template,"/dev/null")/=0) then
        _FAIL("Asking for ExtData to allocate a field when no file is provided")
     end if


     call ESMF_AttributeGet(field,name="derived_source",value=derived_field_name,_RC)
     call ESMF_StateGet(ExtDataState,trim(derived_field_name),derived_field,_RC)
     call ESMF_FieldGet(derived_field,grid=grid,_RC)

     call ESMF_StateRemove(ExtDataState,[trim(item%name)],_RC)
     call ESMF_FieldDestroy(field,noGarbage=.true.,_RC)

     call fill_grads_template(filename,item%file_template,time=current_time,_RC )
     inquire(file=trim(filename),exist=file_found)
     _ASSERT(file_found,"Forcing extdata to allocate primary field but have gaps in data, not implemented currently")
     collection => DataCollections%at(item%pfioCollection_id)
     metadata => collection%find(filename,_RC)
     item%file_metadata = metadata

     call GetLevs(item,_RC)
     if (item%vartype == MAPL_FieldItem) then
        field = create_simple_field(item%name,grid,item%lm,_RC)
        call MAPL_StateAdd(ExtDataState,field,_RC)
     else if (item%vartype == MAPL_VectorField) then
        field = create_simple_field(item%vcomp1,grid,item%lm,_RC)
        call MAPL_StateAdd(ExtDataState,field,_RC)
        field = create_simple_field(item%vcomp2,grid,item%lm,_RC)
        call MAPL_StateAdd(ExtDataState,field,_RC)
     end if

     _RETURN(_SUCCESS)

     contains

     function create_simple_field(field_name,grid,num_levels,rc) result(new_field)
        type(ESMF_Field) :: new_field
        character(len=*), intent(in) :: field_name
        type(ESMF_Grid), intent(in) :: grid
        integer, intent(in) :: num_levels
        integer, optional, intent(out) :: rc

        integer :: status
        real, pointer :: ptr2d(:,:), ptr3d(:,:,:)
        if (num_levels ==0) then
           new_field=ESMF_FieldCreate(grid,name=field_name,typekind=ESMF_TYPEKIND_R4,_RC)
           call ESMF_FieldGet(new_field,0,farrayPtr=ptr2d,_RC)
           ptr2d=0.0
        else
           new_field=ESMF_FieldCreate(grid,name=field_name,typekind=ESMF_TYPEKIND_R4,ungriddedLBound=[1],ungriddedUBound=[num_levels],_RC)
           call ESMF_FieldGet(new_field,0,farrayPtr=ptr3d,_RC)
           ptr3d=0.0
        end if
        _RETURN(_SUCCESS)
     end function

  end subroutine create_primary_field


  function get_item_index(this,base_name,current_time,rc) result(item_index)
     integer :: item_index
     class(primaryExports), intent(in) :: this
     type(ESMF_Time) :: current_time
     character(len=*),intent(in) :: base_name
     integer, optional, intent(out) :: rc

     character(len=:), pointer :: cname
     integer :: i
     integer, pointer :: num_rules,i_start
     logical :: found
     type(PrimaryExport), pointer :: item

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
     _ASSERT(found,"ExtData no item with basename '"//TRIM(base_name)//"' found")

     item_index = -1
     if (num_rules == 1) then
        item_index = i_start
     else if (num_rules > 1) then
        do i=1,num_rules
           item => this%item_vec%at(i_start+i-1)
           if (current_time >= item%start_end_time(1) .and. &
               current_time <  item%start_end_time(2)) then
              item_index = i_start + i -1
              exit
           endif
        enddo
     end if
     _ASSERT(item_index/=-1,"ExtData did not find item index for basename "//TRIM(base_name))
     _RETURN(_SUCCESS)
  end function get_item_index

  subroutine get_global_options(yaml_file,am_running,use_file_weights,rc)
     character(len=*), intent(in) :: yaml_file
     logical,intent(out) :: am_running
     logical,intent(out) :: use_file_weights
     integer, intent(out), optional :: rc
     type(ESMF_HConfig), allocatable :: config
     integer :: status

     am_running=.true.
     use_file_weights=.false.
     config = ESMF_HConfigCreate(filename = trim(yaml_file),_RC)
     if (ESMF_HConfigIsDefined(config,keyString="USE_EXTDATA")) then
        am_running  = ESMF_HConfigAsLogical(config,keyString="USE_EXTDATA",_RC)
     end if
     if (ESMF_HConfigIsDefined(config,keyString="file_weights")) then
        use_file_weights  = ESMF_HConfigAsLogical(config,keyString="file_weights",_RC)
     end if
     call ESMF_HConfigDestroy(config)
     _RETURN(_SUCCESS)
  end subroutine get_global_options


 END MODULE MAPL_ExtDataGridComp2G
