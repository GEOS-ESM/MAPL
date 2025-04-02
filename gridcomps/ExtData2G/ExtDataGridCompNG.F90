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
   use gFTL2_StringVector
   use pfio_StringVectorUtilMod
   use pFIO_StringVariableMapMod
   use gFTL_IntegerVector
   use MAPL_BaseMod
   use MAPL_CommsMod
   use MAPL_ShmemMod
   use ESMFL_Mod
   use MAPL_GenericMod
   use MAPL_VarSpecMod
   use MAPL_CFIOMod
   use MAPL_NewArthParserMod
   use MAPL_ConstantsMod, only: MAPL_RADIANS_TO_DEGREES, MAPL_GRAV
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
   use pFIO_VariableMod
   use MAPL_GriddedIOItemMod
   use MAPL_GriddedIOItemVectorMod
   use MAPL_ExtDataConfig
   use MAPL_ExtDataTypeDef
   use MAPL_ExtDataOldTypesCreator
   use MAPL_StringTemplate
   use pflogger, only: logging, Logger
   use MAPL_ExtDataLogger
   use MAPL_ExtDataConstants
   use gFTL2_StringIntegerMap
   use MAPL_FieldUtils
   use MAPL_ExtDataPrimaryExportVectorMod
   use MAPL_ExtDataDerivedExportVectorMod
   use VerticalCoordinateMod
   use VerticalRegridConserveInterfaceMod
   use MAPL_AbstractGridFactoryMod

   IMPLICIT NONE
   PRIVATE
!
! !PUBLIC MEMBER FUNCTIONS:

   PUBLIC SetServices
!-------------------------------------------------------------------------

  integer, parameter         :: MAPL_ExtDataLeft          = 1
  integer, parameter         :: MAPL_ExtDataRight         = 2
  integer, parameter         :: MAPL_ExtDataResult        = 3
  character(len=*), parameter :: mol_per_mol = 'mol mol-1'
  character(len=*), parameter :: kg_per_kg = 'kg kg-1'
  character(len=*), parameter :: emission_units = 'kg m-2 s-1'
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
   type(PrimaryExport)               :: new_item
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
   type(StringVector), target :: unsatisfied_imports,extra_variables_needed
   type(StringVectorIterator) :: siter
   character(len=:), pointer :: current_base_name,extra_var,import_name
   character(len=:), allocatable :: primary_var_name,derived_var_name
   type(ESMF_Time), allocatable :: time_ranges(:)
   character(len=1) :: sidx
   type(ESMF_VM) :: vm
   type(ESMF_StateItem_Flag) :: state_item_type
   type(PrimaryExport), allocatable :: temp_item
   type(DerivedExport), allocatable :: derived_item
   integer, pointer :: i_start
   integer :: new_size
   logical, allocatable :: rules_with_ps(:), rules_with_q(:)
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
      extra_var => siter%of()
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

!  now lets establish the horizonal and vertical grid for each component, replaces getlevs
   do i=1,self%primary%import_names%size()

      i_start => self%primary%export_id_start%at(i)
      do j=1,self%primary%number_of_rules%at(i)
         item => self%primary%item_vec%at(i_start+j-1)
         item%pfioCOllection_id = MAPL_DataAddCollection(item%file_template)
         call GetLevs(item, time, _RC)
      enddo

   enddo
   ! done establishing grid and levels

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
   PrimaryLoop: do i=1,self%primary%import_names%size()
      current_base_name => self%primary%import_names%at(i)
      idx = self%primary%get_item_index(current_base_name,time,_RC)
      item => self%primary%item_vec%at(idx)

      call create_primary_field(item,self%ExtDataState,time,_RC)
   end do PrimaryLoop

   ! also see if we have to allocate any primary fields due to PS
   num_primary = self%primary%import_names%size()
   do i=1,num_primary

      i_start => self%primary%export_id_start%at(i)
      num_rules = self%primary%number_of_rules%at(i)
      if (allocated(rules_with_ps)) deallocate(rules_with_ps)
      allocate(rules_with_ps(num_rules), source=.false.)
      if (allocated(rules_with_q)) deallocate(rules_with_q)
      allocate(rules_with_q(num_rules), source=.false.)

      do j=1,self%primary%number_of_rules%at(i)
         item => self%primary%item_vec%at(i_start+j-1)
         rules_with_ps(j) = allocated(item%aux_ps)
         rules_with_q(j) = allocated(item%aux_q)
      enddo

      if (any(rules_with_ps)) then

         i_start => self%primary%export_id_start%at(i)
         num_rules = self%primary%number_of_rules%at(i)
         import_name => self%primary%import_names%at(i)
         call self%primary%import_names%push_back("PS_"//import_name)
         new_size = self%primary%item_vec%size()
         do j=1,self%primary%number_of_rules%at(i)
            item => self%primary%item_vec%at(i_start+j-1)
            call copy_primary(item,new_item,'PS')
            call self%primary%item_vec%push_back(new_item)
            ! make a new name ps_importname, if that's not already in import names
            call create_aux_field(new_item, self%ExtDataState, item, 2, _RC)
         enddo

         num_rules = self%primary%number_of_rules%of(i)
         call self%primary%export_id_start%push_back(new_size+1)
         call self%primary%number_of_rules%push_back(num_rules)

      end if

      if (any(rules_with_q)) then

         i_start => self%primary%export_id_start%at(i)
         num_rules = self%primary%number_of_rules%at(i)
         import_name => self%primary%import_names%at(i)
         call self%primary%import_names%push_back("Q_"//import_name)
         new_size = self%primary%item_vec%size()
         do j=1,self%primary%number_of_rules%at(i)
            item => self%primary%item_vec%at(i_start+j-1)
            call copy_primary(item,new_item,'Q')
            call self%primary%item_vec%push_back(new_item)
            ! make a new name ps_importname, if that's not already in import names
            call create_aux_field(new_item, self%ExtDataState, item, 3, _RC)
         enddo

         num_rules = self%primary%number_of_rules%of(i)
         call self%primary%export_id_start%push_back(new_size+1)
         call self%primary%number_of_rules%push_back(num_rules)

      end if
   enddo

   call confirm_imports_for_vregrid(self%primary, import, _RC)

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
      call create_bracketing_fields(item,self%ExtDataState, _RC)
      call IOBundle_Add_Entry(IOBundles,item,idx)
      useTime(i)=use_time

   end do READ_LOOP

   call extdata_lgr%debug('ExtData Run_: READ_LOOP: Done')

   bundle_iter = IOBundles%begin()
   do while (bundle_iter /= IoBundles%end())
      io_bundle => bundle_iter%of()
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
      io_bundle => bundle_iter%of()
      bracket_side = io_bundle%bracket_side
      entry_num = io_bundle%entry_index
      item => self%primary%item_vec%at(entry_num)
      call MAPL_ExtDataFlipBracketSide(item,bracket_side,_RC)
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

   VINTERP_LOOP: do i=1,self%primary%import_names%size()

      current_base_name => self%primary%import_names%at(i)
      idx = self%primary%get_item_index(current_base_name,current_time,_RC)
      item => self%primary%item_vec%at(idx)

      if (do_pointer_update(i)) then

         call MAPL_ExtDataVerticalInterpolate(self,item,import,_RC)

      endif

      nullify(item)

   end do VINTERP_LOOP

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

   integer                           :: status

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

     subroutine GetLevs(item, current_time, rc)

        type(PrimaryExport)      , intent(inout) :: item
        type(ESMF_Time)          , intent(in) :: current_time
        integer, optional        , intent(out  ) :: rc
        integer :: status

        type(Variable), pointer :: var

        type(FileMetadataUtils), pointer :: metadata
        type(MAPLDataCollection), pointer :: collection
        character(len=:), allocatable :: filename, q_name
        real :: molecular_weight

        if (trim(item%file_template) == "/dev/null") then
           _RETURN(_SUCCESS)
        end if
        filename = item%filestream%find_any_file(current_time, _RC)
        collection => DataCollections%at(item%pfioCollection_id)
        metadata => collection%find(filename,_RC)
        item%file_metadata = metadata
        item%units = metadata%get_var_attr_string(item%var,'units',_RC)

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

        item%vcoord = verticalCoordinate(metadata, item%var, _RC)
        if (item%vcoord%vertical_type /= NO_COORD .and. item%vcoord%vertical_type /= SIMPLE_COORD .and. &
            (item%enable_vertical_regrid .eqv. .true.)) item%allow_vertical_regrid = .true.

        if (item%allow_vertical_regrid) then
           item%aux_ps = item%vcoord%surf_name
           if (item%units == mol_per_mol) then
              molecular_weight = metadata%get_var_attr_real32(item%var, 'molecular_weight', _RC)
              allocate(item%molecular_weight,source=molecular_weight)
              q_name = find_q(metadata, _RC)
              item%aux_q = q_name
           end if
        end if
        _RETURN(ESMF_SUCCESS)

     contains

     function find_q(metadata, rc) result(q_name)
        character(len=:), allocatable :: q_name
        type(FileMetadataUtils), intent(inout) :: metadata
        integer, optional, intent(out) :: rc
        type (StringVariableMap), pointer :: vars
        type (StringVariableMapIterator) :: var_iter
        character(len=:), pointer :: var_name
        character(len=:), allocatable :: units
        character(len=:), allocatable :: long_name
        integer :: status
        logical :: has_units, has_longname

        vars => metadata%get_variables()
        var_iter = vars%begin()
        do while (var_iter /= vars%end())
           var_name => var_iter%first()
           has_longname = metadata%var_has_attr(var_name,'long_name',_RC)
           has_units = metadata%var_has_attr(var_name,'units',_RC)
           if (has_longname .and. has_units) then
              long_name = metadata%get_var_attr_string(var_name,'long_name',_RC)
              units = metadata%get_var_attr_string(var_name,'units',_RC)
              if (long_name == "specific_humidity" .and. units == "kg kg-1") q_name = var_name
           end if
           call var_iter%next()
        enddo
        _ASSERT(allocated(q_name), "could not find specific humidity in source file needed for volume mixing regridding")
        _RETURN(_SUCCESS)
     end function find_q

     end subroutine GetLevs

  subroutine MAPL_ExtDataInterpField(item,state,time,rc)
     type(PrimaryExport), intent(inout) :: item
     type(ESMF_State),    intent(in)    :: state
     type(ESMF_Time),     intent(in   ) :: time
     integer, optional,   intent(out  ) :: rc

     integer                    :: status
     type(ESMF_Field) :: field

     ! if we didn't actually create bracketing fields we had a gap in the data
     ! in this case, get the ultimate pointer to fill
     call ESMF_FieldBundleValidate(item%t_interp_bundle, rc=status)
     if (status /= _SUCCESS) then
        call ESMF_StateGet(state,item%vcomp1,field,_RC)
     else
        call ESMF_FieldBundleGet(item%t_interp_bundle, item%vcomp1, field=field, _RC)
     end if
     call item%modelGridFields%comp1%interpolate_to_time(field,time,_RC)
     if (item%vartype == MAPL_VectorField) then
        call ESMF_FieldBundleValidate(item%t_interp_bundle, rc=status)
        if (status /= _SUCCESS) then
           call ESMF_StateGet(state,item%vcomp2,field,_RC)
        else
           call ESMF_FieldBundleGet(item%t_interp_bundle, item%vcomp2, field=field, _RC)
        end if
        call item%modelGridFields%comp2%interpolate_to_time(field,time,_RC)
     end if
     _RETURN(ESMF_SUCCESS)
  end subroutine MAPL_ExtDataInterpField

  subroutine MAPL_ExtDataFlipBracketSide(item,bracket_side,rc)
     type(PrimaryExport), intent(inout)     :: item
     integer, intent(in)                    :: bracket_side
     integer, optional,   intent(out  )     :: rc

     integer :: status

     if (item%vcoord%vertical_type == NO_COORD) then
        _RETURN(_SUCCESS)
     end if

     if (item%vcoord%positive /= item%importVDir) then
        call MAPL_ExtDataFlipVertical(item,bracket_side,_RC)
     end if
     _RETURN(_SUCCESS)
  end subroutine MAPL_ExtDataFlipBracketSide

  subroutine MAPL_ExtDataVerticalInterpolate(MAPLExtState,item,import,rc)
     type(MAPL_ExtData_State), intent(inout) :: MAPLExtState
     type(PrimaryExport), intent(inout)     :: item
     type(ESMF_State), intent(in)           :: import
     integer, optional,   intent(out  )     :: rc

     integer :: status
     type(ESMF_Field) :: src_field, dst_field, src_ps, dst_ple, q_field
     character(len=:), allocatable :: src_ps_name, src_q_name

     integer :: fieldRank
     real, pointer :: dst_ptr3d(:,:,:), src_ptr3d(:,:,:), src_ps_ptr(:,:), dst_ple_ptr(:,:,:)
     real, pointer :: src_q(:,:,:), dst_q(:,:,:)
     real, allocatable :: src_ple_ptr(:,:,:)
     real :: molecular_weight
     character(len=:), allocatable :: units_in, units_out
     integer :: constituent_type
     type(ESMF_Info) :: infoh


     if (item%vcoord%vertical_type == NO_COORD &
        .or. (.not.item%delivered_item)) then
        _RETURN(_SUCCESS)
     end if

     if (item%allow_vertical_regrid .and. (item%vcoord%vertical_type == model_pressure)) then

        call extdata_lgr%info('ExtData vertical conservative regridding of '//trim(item%name))
        call ESMF_StateGet(import, "PLE", dst_ple, _RC)
        call ESMF_FieldGet(dst_ple,farrayPtr=dst_ple_ptr,_RC)
        src_ps_name = item%vcoord%surf_name//"_"//trim(item%vcomp1)
        call ESMF_StateGet(MAPLExtState%ExtDataState, src_ps_name, src_ps, _RC)
        call ESMF_FieldGet(src_ps, farrayPtr=src_ps_ptr, _RC)
        src_ple_ptr = item%vcoord%compute_ple(src_ps_ptr, _RC)
        call ESMF_StateGet(MAPLExtState%ExtDataState,trim(item%vcomp1),dst_field,_RC)
        call ESMF_FieldGet(dst_field,rank=fieldRank,_RC)
        _ASSERT(fieldRank==3, "Trying to regrid non 3D field")
        call ESMF_FieldGet(dst_field,farrayPtr=dst_ptr3d,_RC)
        call ESMF_FieldBundleGet(item%t_interp_bundle, trim(item%vcomp1), field=src_field, _RC)
        call ESMF_FieldGet(src_field,farrayPtr=src_ptr3d,_RC)

        units_in = get_field_units(src_field, _RC)
        units_out = get_field_units(dst_field, _RC)
        _ASSERT(units_in == units_out, "Going to vertical regrid and units of source and destination do not match")

        if (units_in == mol_per_mol) constituent_type = volume_mixing
        if (units_in == kg_per_kg) constituent_type = mass_mixing
        if (units_in == emission_units) constituent_type = emission

        select case (constituent_type)
        case(mass_mixing)
           call vremap_conserve_mass_mixing(src_ple_ptr,src_ptr3d,dst_ple_ptr,dst_ptr3d)
        case(emission)
           call vremap_conserve_emission(src_ple_ptr,src_ptr3d,dst_ple_ptr,dst_ptr3d)
        case (volume_mixing)
           call ESMF_InfoGetFromHost(src_field,infoh,_RC)
           call ESMF_InfoGet(infoh,key='molecular_weight',value=molecular_weight, _RC)
           call ESMF_StateGet(import, 'Q', q_field, _RC)
           call ESMF_FieldGet(q_field,0, farrayPtr=dst_q, _RC)
           src_q_name = item%aux_q//"_"//trim(item%vcomp1)
           call ESMF_StateGet(MAPLExtState%ExtDataState, src_q_name, q_field, _RC)
           call ESMF_FieldGet(q_field,0, farrayPtr=src_q, _RC)
           call vremap_conserve_vol_mixing(src_ple_ptr, src_q, molecular_weight, src_ptr3d, dst_ple_ptr, dst_q, dst_ptr3d, _RC)
        case default
           _FAIL(trim(units_in)//" not supported for vertical regridding")
        end select
     else if (item%vcoord%vertical_type == simple_coord .and. item%do_fill) then
        call extdata_lgr%info('ExtData filling destination with available layers of source for '//trim(item%name))
        call ESMF_FieldBundleGet(item%t_interp_bundle, trim(item%vcomp1), field=src_field, _RC)
        call ESMF_StateGet(MAPLExtState%ExtDataState,trim(item%vcomp1),dst_field,_RC)
        call MAPL_ExtDataFillField(item, dst_field, src_field, _RC)
     end if
     _RETURN(ESMF_SUCCESS)

    contains

    function get_field_units(field, rc) result(field_units)
       character(len=:), allocatable :: field_units
       type(ESMF_Field), intent(in) :: field
       integer, optional, intent(out) :: rc
       integer :: status
       character(len=ESMF_MAXSTR) :: temp_char
       type(ESMF_Info) :: infoh

       call ESMF_InfoGetFromHost(field,infoh,_RC)
       call ESMF_InfoGet(infoh,key='UNITS',value=temp_char,_RC)
       field_units = temp_char
    end function get_field_units

  end subroutine MAPL_ExtDataVerticalInterpolate

  function MAPL_ExtDataGridChangeLev(Grid,lm,rc) result(NewGrid)

     type(ESMF_Grid), intent(inout) :: Grid
     integer,         intent(in)    :: lm
     integer, optional, intent(out) :: rc

     integer :: status

     type(ESMF_Grid)           :: newGrid
     type(ESMF_Info) :: infoh_grid, infoh_NewGrid
     class (AbstractGridFactory), pointer :: factory
     integer :: factory_id

     factory => get_factory(grid, _RC)
     NewGrid = factory%make_grid(force_new_grid=.true., _RC)

     call ESMF_InfoGetFromHost(grid,infoh_Grid,_RC)
     call ESMF_InfoGetFromHost(NewGrid,infoh_NewGrid,_RC)

     call ESMF_InfoSet(infoh_NewGrid, key='GRID_LM', value=lm, _RC)
     call ESMF_InfoGet(infoh_Grid, key=factory_id_attribute_public,value=factory_id,_RC)
     call ESMF_InfoSet(infoh_NewGrid, key=factory_id_attribute_public,value=factory_id,_RC)

     _RETURN(ESMF_SUCCESS)

  end function MAPL_ExtDataGridChangeLev

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
        else if (present(bundle)) then
           _RETURN(ESMF_FAILURE)
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
  _ASSERT(lm_out > lm_in, "trying to fillin but destination has less levels than source")
  if (trim(item%importVDir)=="down") then

     if (trim(item%vcoord%positive)=="down") then
        do i=1,lm_in
           ptrF(:,:,lm_out-lm_in+i)=ptrR(:,:,i)
        enddo
     else if (trim(item%vcoord%positive)=="up") then
        do i=1,lm_in
           ptrF(:,:,lm_out-i+1)=ptrR(:,:,i)
        enddo
     end if
  else if (trim(item%importVDir)=="up") then
     if (trim(item%vcoord%positive)=="down") then
        do i=1,lm_in
           ptrF(:,:,lm_in-i+1)=ptrR(:,:,i)
        enddo
     else if (trim(item%vcoord%positive)=="up") then
        do i=1,lm_in
           ptrF(:,:,i)=ptrR(:,:,i)
        enddo
     end if
  end if

  _RETURN(ESMF_SUCCESS)

  end subroutine MAPL_ExtDataFillField

  subroutine MAPL_ExtDataFlipVertical(item,filec,rc)
      type(PrimaryExport), intent(inout)      :: item
      integer, optional, intent(in)           :: filec
      integer, optional, intent(out)          :: rc

      integer :: status

      type(ESMF_Field) :: Field,field1,field2
      real, pointer    :: ptr(:,:,:)
      real, allocatable :: ptemp(:,:,:)
      integer :: ls, le, local_filec

      local_filec = MAPL_ExtDataResult
      if (present(filec)) local_filec = filec

      if (item%vartype == MAPL_VectorField) then

         if (local_filec /= MAPL_ExtDataResult) then
            call MAPL_ExtDataGetBracket(item,local_filec,field=Field1,vcomp=1,_RC)
            call MAPL_ExtDataGetBracket(item,local_filec,field=Field2,vcomp=2,_RC)
         else
            _FAIL("not yet implemented")
         end if

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

         if (local_filec /= MAPL_ExtDataResult) then
            call MAPL_ExtDataGetBracket(item,local_filec,field=Field,_RC)
         else
            call ESMF_FieldBundleGet(item%t_interp_bundle, item%vcomp1, field=field, _RC)
         end if

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
        io_bundle => bundle_iter%of()
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
        io_bundle => bundle_iter%of()
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

  subroutine IOBundle_Add_Entry(IOBundles,item,entry_num,rc)
     type(IOBundleNGVector), target, intent(inout) :: IOBundles
     type(primaryExport), target, intent(inout)  :: item
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

  subroutine create_bracketing_fields(item,ExtDataState,rc)
     type(PrimaryExport), intent(inout) :: item
     type(ESMF_State), intent(inout) :: extDataState
     integer, intent(out), optional :: rc

     integer :: status,lm,fieldRank
     type(ESMF_Field) :: field,left_field,right_field
     type(ESMF_Grid)  :: grid,bracket_grid
     real(kind=REAL32), pointer :: ptr3d(:,:,:)
     character(len=ESMF_MAXPATHLEN) :: file_left, file_right, filename
     logical :: found_file
     type(FileMetadataUtils), pointer :: metadata
     type(MAPLDataCollection), pointer :: collection
     type(ESMF_Field) :: temp_field

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
        item%iclient_collection_id=i_clients%add_data_collection(trim(item%file_template))
        item%t_interp_bundle = ESMF_FieldBundleCreate(_RC)
        if (item%vartype == MAPL_FieldItem) then

           call ESMF_StateGet(ExtDataState, trim(item%name), field,_RC)
           call ESMF_FieldGet(field,grid=grid,rank=fieldRank,_RC)

           lm=0
           if (fieldRank==3) then
              call ESMF_FieldGet(field,0,farrayPtr=ptr3d,_RC)
              lm = size(ptr3d,3)
           end if
           if (item%vcoord%num_levels /= lm .and. lm /= 0 .and. (item%vcoord%vertical_type == model_pressure)) then
              item%do_VertInterp = .true.
           else if (item%vcoord%num_levels /= lm .and. lm /= 0 .and. item%vcoord%num_levels /= 0) then
              item%do_Fill = .true.
           end if

           bracket_grid = MAPL_ExtDataGridChangeLev(grid,item%vcoord%num_levels,_RC)
           left_field = MAPL_FieldCreate(field,bracket_grid,lm=item%vcoord%num_levels,newName=trim(item%fcomp1),_RC)
           call set_field_units(left_field, item%units, _RC)
           call set_mw(left_field, item, _RC)
           right_field = MAPL_FieldCreate(field,bracket_grid,lm=item%vcoord%num_levels,newName=trim(item%fcomp1),_RC)
           call set_field_units(right_field, item%units, _RC)
           call set_mw(right_field, item, _RC)


           if ((item%vcoord%num_levels /= lm) .and. (lm > 0)) then
              temp_field = MAPL_FieldCreate(field,bracket_grid,lm=item%vcoord%num_levels,newName=trim(item%vcomp1),_RC)
              call set_field_units(temp_field, item%units, _RC)
              call set_mw(temp_field, item, _RC)
              call MAPL_FieldBundleAdd(item%t_interp_bundle, temp_field, _RC)
           else
              call MAPL_FieldBundleAdd(item%t_interp_bundle, field, _RC)
           end if

           call item%modelGridFields%comp1%set_parameters(left_field=left_field,right_field=right_field, _RC)

        else if (item%vartype == MAPL_VectorField) then

           call ESMF_StateGet(ExtDataState, trim(item%vcomp1), field,_RC)
           call ESMF_FieldGet(field,grid=grid,rank=fieldRank,_RC)

           lm = 0
           if (fieldRank==3) then
              call ESMF_FieldGet(field,0,farrayPtr=ptr3d,_RC)
              lm = size(ptr3d,3)
           end if
           if (item%vcoord%num_levels /= lm .and. lm /= 0 .and. (item%vcoord%vertical_type == model_pressure)) then
              item%do_VertInterp = .true.
           else if (item%vcoord%num_levels /= lm .and. lm /= 0 .and. item%vcoord%num_levels /= 0) then
              item%do_Fill = .true.
           end if

           bracket_grid = MAPL_ExtDataGridChangeLev(grid,item%vcoord%num_levels,_RC)
           left_field = MAPL_FieldCreate(field,bracket_grid,lm=item%vcoord%num_levels,newName=trim(item%fcomp1),_RC)
           call set_field_units(left_field, item%units, _RC)
           _HERE
           call set_mw(left_field, item, _RC)
           right_field = MAPL_FieldCreate(field,bracket_grid,lm=item%vcoord%num_levels,newName=trim(item%fcomp1),_RC)
           call set_field_units(right_field, item%units, _RC)
           _HERE
           call set_mw(right_field, item, _RC)
           call item%modelGridFields%comp1%set_parameters(left_field=left_field,right_field=right_field, _RC)
           if (item%vcoord%num_levels /= lm) then
              temp_field = MAPL_FieldCreate(field,bracket_grid,lm=item%vcoord%num_levels,newName=trim(item%vcomp1),_RC)
              call set_field_units(temp_field, item%units, _RC)
           _HERE
              call set_mw(temp_field, item, _RC)
              call MAPL_FieldBundleAdd(item%t_interp_bundle, temp_field, _RC)
           else
              call MAPL_FieldBundleAdd(item%t_interp_bundle, field, _RC)
           end if


           call ESMF_StateGet(ExtDataState, trim(item%vcomp2), field,_RC)
           left_field = MAPL_FieldCreate(field,item%fcomp2,doCopy=.true.,_RC)
           call set_field_units(left_field, item%units, _RC)
           call set_mw(left_field, item, _RC)
           right_field = MAPL_FieldCreate(field,item%fcomp2,doCopy=.true.,_RC)
           call set_field_units(right_field, item%units, _RC)
           call set_mw(right_field, item, _RC)
           call item%modelGridFields%comp2%set_parameters(left_field=left_field,right_field=right_field, _RC)
           if (item%vcoord%num_levels /= lm) then
              temp_field = MAPL_FieldCreate(field,bracket_grid,lm=item%vcoord%num_levels,newName=trim(item%vcomp2),_RC)
              call set_field_units(temp_field, item%units, _RC)
              call set_mw(temp_field, item, _RC)
              call MAPL_FieldBundleAdd(item%t_interp_bundle, temp_field, _RC)
           else
              call MAPL_FieldBundleAdd(item%t_interp_bundle, field, _RC)
           end if

        end if

     end if

     _RETURN(_SUCCESS)

     contains

     subroutine set_field_units(field, units, rc)
        type(ESMF_Field), intent(inout) :: field
        character(len=*), intent(in) :: units
        integer, optional, intent(out) :: rc
        integer :: status
        type(ESMF_Info) :: infoh

        call ESMF_InfoGetFromHost(field,infoh,_RC)
        call ESMF_InfoSet(infoh,key='UNITS',value=units, _RC)
        _RETURN(_SUCCESS)
     end subroutine set_field_units

     subroutine set_mw(field, item, rc)
        type(ESMF_Field), intent(inout) :: field
        type(PrimaryExport), intent(inout) :: item
        integer, optional, intent(out) :: rc
        integer :: status
        type(ESMF_Info) :: infoh

        if (allocated(item%molecular_weight)) then
           call ESMF_InfoGetFromHost(field,infoh,_RC)
           call ESMF_InfoSet(infoh,key='molecular_weight',value=item%molecular_weight, _RC)
        end if
        _RETURN(_SUCCESS)
     end subroutine set_mw

  end subroutine create_bracketing_fields

  subroutine create_holding_field(state,primary_name,derived_name,rc)
     type(ESMF_State), intent(inout) :: state
     character(len=*), intent(in) :: primary_name
     character(len=*), intent(in) :: derived_name
     integer, optional, intent(out) :: rc

     integer :: status
     type(ESMF_Field) :: field
     type(ESMF_Info) :: infoh

     field = ESMF_FieldEmptyCreate(name=primary_name,_RC)
     call ESMF_InfoGetFromHost(field,infoh,_RC)
     call ESMF_InfoSet(infoh,key="derived_source",value=derived_name,_RC)
     call MAPL_StateAdd(state,field,_RC)

     _RETURN(_SUCCESS)
  end subroutine

  subroutine create_aux_field(item,ExtDataState,old_item,rank,rc)
     type(PrimaryExport), intent(inout) :: item
     type(ESMF_State), intent(inout) :: extDataState
     type(PrimaryExport), intent(inout) :: old_item
     integer, intent(in) :: rank
     integer, intent(out), optional :: rc

     integer :: status
     type(ESMF_Field) :: field,new_field
     type(ESMF_Grid)  :: grid
     type(ESMF_StateItem_Flag) :: item_type

     call ESMF_StateGet(ExtDataState,trim(old_item%name),field,_RC)
     if (index(old_item%file_template,"/dev/null")/=0) then
        _RETURN(_SUCCESS)
     end if

     call ESMF_StateGet(ExtDataState, itemName=item%name, itemType=item_type, _RC)
     if (item_type == ESMF_STATEITEM_FIELD) then
        _RETURN(_SUCCESS)
     end if

     call ESMF_FieldGet(field,grid=grid,_RC)

     if (rank==2) then
        new_field=ESMF_FieldCreate(grid,name=item%name,typekind=ESMF_TYPEKIND_R4,_RC)
     else if (rank==3) then
        new_field=ESMF_FieldCreate(grid,name=item%name,typekind=ESMF_TYPEKIND_R4, &
         ungriddedLBound=[1], ungriddedUBound=[item%vcoord%num_levels],_RC)
     end if
     call MAPL_StateAdd(ExtDataState,new_field,_RC)

     _RETURN(_SUCCESS)

  end subroutine create_aux_field

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
     type(ESMF_Info) :: infoh

     call ESMF_StateGet(ExtDataState,trim(item%name),field,_RC)
     call ESMF_FieldValidate(field,rc=status)
     call ESMF_InfoGetFromHost(field,infoh,_RC)
     must_create = ESMF_InfoIsPresent(infoh,key="derived_source",_RC)
     if (.not.must_create) then
        _RETURN(_SUCCESS)
     end if
     if (index(item%file_template,"/dev/null")/=0) then
        _FAIL("Asking for ExtData to allocate a field when no file is provided")
     end if


     call ESMF_InfoGet(infoh,key="derived_source",value=derived_field_name,_RC)
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

     if (item%vartype == MAPL_FieldItem) then
        field = create_simple_field(item%name,grid,item%vcoord%num_levels,_RC)
        call MAPL_StateAdd(ExtDataState,field,_RC)
     else if (item%vartype == MAPL_VectorField) then
        field = create_simple_field(item%vcomp1,grid,item%vcoord%num_levels,_RC)
        call MAPL_StateAdd(ExtDataState,field,_RC)
        field = create_simple_field(item%vcomp2,grid,item%vcoord%num_levels,_RC)
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

  subroutine confirm_imports_for_vregrid(primary_exports, import_state, rc)
     type(PrimaryExports), intent(in) :: primary_exports
     type(ESMF_State), intent(in) :: import_state
     integer, intent(out), optional :: rc

     integer :: status, i, num_items
     logical :: found_allowed
     type(PrimaryExport), pointer      :: item
     character(len=*), parameter :: PLE_IMPORT = 'PLE'
     ! for now only required import is PLE, but this will grow, hence array
     character(len=3), parameter :: required_imports(1) = [character(len=3) :: &
          PLE_IMPORT]
     type(ESMF_StateItem_Flag) :: item_type

     found_allowed = .false.
     num_items = primary_exports%item_vec%size()
     do i=1,num_items
        item => primary_exports%item_Vec%at(i)
        found_allowed = item%allow_vertical_regrid
        if (found_allowed) exit
     end do
     if (found_allowed) then
        do i=1,size(required_imports)
           call ESMF_StateGet(import_state, required_imports(i), item_type, _RC)
           _ASSERT(item_type == ESMF_STATEITEM_FIELD, "Vertically regridding in extdata is allowed but required import "//trim(required_imports(i))//" not present, modify cap to import PLE")
        enddo
     end if
     _RETURN(_SUCCESS)

  end subroutine confirm_imports_for_vregrid



 END MODULE MAPL_ExtDataGridComp2G
