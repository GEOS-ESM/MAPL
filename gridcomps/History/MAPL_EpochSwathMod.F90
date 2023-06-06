!
! __ parallel to GriddedIO.F90 with twist for Epoch Swath grid
!
#include "MAPL_Generic.h"

module MAPL_EpochSwathMod
  use ESMF
  use ESMFL_Mod
  use MAPL_AbstractGridFactoryMod
  use MAPL_AbstractRegridderMod
  use MAPL_GridManagerMod
  use MAPL_BaseMod
  use MAPL_NewRegridderManager
  use MAPL_RegridMethods
  use MAPL_TimeDataMod
  use MAPL_VerticalDataMod
  use MAPL_Constants
  use pFIO
  use MAPL_GriddedIOItemVectorMod
  use MAPL_GriddedIOItemMod
  use MAPL_ExceptionHandling
  use pFIO_ClientManagerMod
  use MAPL_DataCollectionMod
  use MAPL_DataCollectionManagerMod
  use gFTL_StringVector
  use gFTL_StringStringMap
  use MAPL_StringGridMapMod
  use MAPL_FileMetadataUtilsMod
  use MAPL_DownbitMod
  use MAPL_plain_netCDF_Time
  use, intrinsic :: ISO_C_BINDING
  use, intrinsic :: iso_fortran_env, only: REAL64
  use ieee_arithmetic, only: isnan => ieee_is_nan
  implicit none

  private

  type, public :: samplerHQ 
     type(ESMF_Clock)         :: clock
     type(ESMF_Alarm)         :: alarm
     type(ESMF_Time)          :: RingTime
     type(ESMF_TimeInterval)  :: Frequency_epoch
     type(ESMF_config)        :: config_grid_save
     type(ESMF_grid)          :: ogrid
     character(len=ESMF_MAXSTR) :: grid_type
     real*8 :: arr(2)

   contains
     procedure ::  create_grid
     !!procedure ::  create_sampler => create_epoch_sampler
     procedure ::  regrid_accumulate => regrid_accumulate_on_xysubset
     procedure ::  regen_grid => destroy_regen_ogrid_rh
     procedure ::  write_2_oserver
  end type samplerHQ

  interface samplerHQ
     module procedure new_samplerHQ
  end interface samplerHQ

  type, public :: sampler
     type(FileMetaData) :: metadata
     type(fileMetadataUtils), pointer :: current_file_metadata
     integer :: write_collection_id
     integer :: read_collection_id
     integer :: metadata_collection_id
     class (AbstractRegridder), pointer :: regrid_handle => null()
     type(ESMF_Grid) :: output_grid
     logical :: doVertRegrid = .false.
     type(ESMF_FieldBundle) :: output_bundle
     type(ESMF_FieldBundle) :: input_bundle
     type(ESMF_FieldBundle) :: acc_bundle     
     type(ESMF_Time) :: startTime
     integer :: regrid_method = REGRID_METHOD_BILINEAR
     integer :: nbits_to_keep = MAPL_NBITS_NOT_SET
     real, allocatable :: lons(:,:),lats(:,:)
     real, allocatable :: corner_lons(:,:),corner_lats(:,:)
     real, allocatable :: times(:)
     type(TimeData) :: timeInfo
     type(VerticalData) :: vdata
     type(GriddedIOitemVector) :: items
     integer :: deflateLevel = 0
     integer :: quantizeAlgorithm = 1
     integer :: quantizeLevel = 0
     integer, allocatable :: chunking(:)
     logical :: itemOrderAlphabetical = .true.
     integer :: fraction
   contains
     procedure :: CreateFileMetaData
     procedure :: CreateVariable
     procedure :: modifyTime
     procedure :: modifyTimeIncrement
     procedure :: bundlePost
     procedure :: stageData
     procedure :: stage2DLatLon
     procedure :: regridScalar
     procedure :: regridVector
     procedure :: set_param
     procedure :: set_default_chunking
     procedure :: check_chunking
     procedure :: alphabatize_variables
     procedure :: request_data_from_file
     procedure :: process_data_from_file
     procedure :: swap_undef_value
     procedure :: addVariable_to_acc_bundle
     procedure :: addVariable_to_output_bundle     
     procedure :: interp_accumulate_fields
  end type sampler

  interface sampler
     module procedure new_sampler
  end interface sampler

contains

  function new_samplerHQ(clock, config, key, rc) result(hq)
    implicit none
    type(samplerHQ) :: hq
    type(ESMF_Clock),  intent(in) :: clock
    type(ESMF_Config), intent(inout) :: config
    character(len=*), intent(in) :: key
    integer, optional, intent(out) :: rc

    character(len=ESMF_MAXSTR) :: time_string
    integer :: status
    integer :: time_integer
    type(ESMF_Time)            :: RingTime_epoch
    type(ESMF_Time)            :: startTime
    type(ESMF_Time)         :: currTime
    type(ESMF_TimeInterval) :: timeStep
    type(ESMF_TimeInterval)  :: Frequency_epoch

    integer :: sec, second
    integer :: n1
    type(ESMF_Config) :: cf

    hq%clock= clock
    hq%config_grid_save= config
    hq%arr(1:2) = -2.d0
    call ESMF_ClockGet ( clock, CurrTime=currTime, _RC )
    call ESMF_ClockGet ( clock, timestep=timestep, _RC )
    call ESMF_ClockGet ( clock, startTime=startTime, _RC )
    call ESMF_ConfigGetAttribute(config, value=time_integer, label=trim(key)//'.Epoch:', default=0, _RC)
    _ASSERT(time_integer /= 0, 'Epoch value in config wrong')
    call hms_2_s (time_integer, second, _RC)
    call ESMF_TimeIntervalSet(frequency_epoch, s=second, _RC)
    hq%frequency_epoch = frequency_epoch
    hq%RingTime  = currTime
    hq%alarm = ESMF_AlarmCreate( clock=clock, RingInterval=Frequency_epoch, &
         RingTime=hq%RingTime, sticky=.false., _RC )

    write(6,*) 'ck 1: epoch:  second=', second
    _RETURN(_SUCCESS)

  end function new_samplerHQ
    

  !--------------------------------------------------!
  ! __ set
  !    - ogrid via grid_manager%make_grid
  !      using currTime and HQ%config_grid_save
  !--------------------------------------------------!
  function create_grid(this, key, currTime, grid_type, rc)  result(ogrid)
    class(samplerHQ)  :: this
    character(len=*), intent(in) :: key
    type(ESMF_Time), intent(inout) :: currTime
    character(len=*), optional, intent(in) :: grid_type
    integer, intent(out), optional :: rc
    type (ESMF_Grid)  :: ogrid

    character(len=ESMF_MAXSTR) :: time_string
    integer :: status
    type(ESMF_Config) :: config_grid

    if (present(grid_type)) this%grid_type = trim(grid_type)
    config_grid = this%config_grid_save
    call ESMF_TimeGet(currTime, timeString=time_string, _RC)
    call ESMF_ConfigSetAttribute( config_grid, time_string, label=trim(key)//'.Epoch_init:', _RC)
    ogrid = grid_manager%make_grid(config_grid, prefix=trim(key)//'.', _RC )
    this%ogrid = ogrid
    _RETURN(_SUCCESS)
    
  end function create_grid


  subroutine regrid_accumulate_on_xysubset (this, sp, rc)
    class(samplerHQ) :: this
    class(sampler), intent(inout)  :: sp
    integer, intent(out), optional :: rc

    class(AbstractGridFactory), pointer :: factory
    integer                        :: xy_subset(2,2)
    type(ESMF_Time)                :: timeset(2)
    type(ESMF_Time) :: current_time
    type(ESMF_TimeInterval) :: dur
    character(len=ESMF_MAXSTR) :: time_string
    integer :: status
    integer, allocatable :: global_xy_mask(:,:)
    integer, allocatable :: local_xy_mask(:,:)    

    integer :: counts(5)
    integer :: dims(3)
    integer :: m1, m2

    
    ! __ s1.  get xy_subset

    factory => grid_manager%get_factory(this%ogrid,_RC)
    call ESMF_ClockGet(this%clock,currTime=current_time,_RC)
    call ESMF_ClockGet(this%clock,timeStep=dur, _RC )
    timeset(1) = current_time - dur
    timeset(2) = current_time
    call factory%get_xy_subset( timeset, xy_subset, _RC)

    
    ! __ s2.  get xy_mask

    call MAPL_GridGet( this%ogrid, globalCellCountPerDim=COUNTS, &
         localCellCountPerDim=DIMS, _RC)
    m1=COUNTS(1); m2=COUNTS(2)
    allocate (global_xy_mask(m1, m2))
    m1=DIMS(1); m2=DIMS(2)
    allocate (local_xy_mask(m1, m2))    

    write(6,*) 'global_counts(1:5)', COUNTS(1:3)
    write(6,*) 'local_counts(1:3)', DIMS(1:3)
    write(6,*) 'xy_subset(:,1)_x', xy_subset(:,1)    ! LB
    write(6,*) 'xy_subset(:,2)_a', xy_subset(:,2), xy_subset(2,2)-xy_subset(1,2)+1  ! UB
    
!!    call get_xy_mask( this%ogrid, xy_subset, global_xy_mask, local_xy_mask, _RC )    
  

    ! __ s3.  interpolate then save data using xy_mask

    write(6,*) 'bf sp%interp_accumulate_fields'
    !    call sp%interp_accumulate_fields (xy_subset, xy_mask, _RC)
    call sp%interp_accumulate_fields (xy_subset, _RC)
    write(6,*) 'af sp%interp_accumulate_fields'



    
    _RETURN(ESMF_SUCCESS)
    
  end subroutine regrid_accumulate_on_xysubset  
  


  subroutine destroy_regen_ogrid_rh (this, key_grid_label, pt_output_grids, sp, rc)
    implicit none
    class(samplerHQ) :: this
    class(sampler) :: sp
    type (StringGridMap), pointer, intent(inout) :: pt_output_grids
    character(len=*), intent(in)  :: key_grid_label
    integer, intent(out), optional :: rc 
!!    class(sampler), intent(inout)  :: sp
    type (StringGridMap) :: output_grids
    
    class(AbstractGridFactory), pointer :: factory
    type(ESMF_Time) :: currTime
    type(ESMF_TimeInterval) :: dur
    character(len=ESMF_MAXSTR) :: time_string
    integer :: status
    
    type(ESMF_Grid), pointer :: pgrid
    type(ESMF_Grid) :: ogrid
    type(ESMF_Grid) :: input_grid
    character(len=ESMF_MAXSTR) :: key_str
    type (StringGridMapIterator) :: iter
    character(len=:), pointer :: key
    type (ESMF_Config) ::  config_grid
    

    integer :: i, numVars
     character(len=ESMF_MAXSTR), allocatable :: names(:)
     type(ESMF_Field) :: field


!!    type(Abstract) :: route_handle

    if ( .NOT. ESMF_AlarmIsRinging(this%alarm) ) then
       write(6,*) 'ck: regen,  not in alarming'
       rc=0
       return
    endif

    write(6,*)  'ck: yes in alarming , destroy_regen_ogrid_rh'    
    

    !__ s1. destroy ogrid + regen ogrid

    output_grids = pt_output_grids
    key_str=trim(key_grid_label)
    pgrid => output_grids%at(trim(key_str))
    ogrid = pgrid
    call grid_manager%destroy(ogrid,_RC)
    write(6,*) 'ck: key_str ', trim(key_str)
    write(6,*) 'ck: done grid_manager%destroy'    

    call output_grids%insert(trim(key_str), ogrid)
    call ESMF_ClockGet ( this%clock, CurrTime=currTime, _RC )
    iter = output_grids%begin()
    do while (iter /= output_grids%end())
       key => iter%key()
       if (trim(key)==trim(key_str)) then
          ogrid = this%create_grid (key_str, currTime, _RC)
          call output_grids%set(key, ogrid)
       endif
       call iter%next()
    enddo
    write(6,*) 'ck: done adding iter to output_grids'

    
    !
    !-- ygyu: this still fails
    !

    !__ s2.  destroy RH  +  regen RH

!    ! -- destroy route_handle
!    route_handle = sp%regrid_handle
!    call route_handle%destroy(_RC)
        
    call ESMF_FieldBundleGet(sp%input_bundle,grid=input_grid,_RC)
    sp%regrid_handle => new_regridder_manager%make_regridder(input_grid,ogrid,sp%regrid_method,_RC)
    write(6,*) 'ck: done adding sp%regrid_handle'

    
    !__ s3.  destroy + regen acc_bundle
    
   call ESMF_FieldBundleGet(sp%acc_bundle,fieldCount=numVars,_RC)
   allocate(names(numVars),stat=status)
   call ESMF_FieldBundleGet(sp%acc_bundle,fieldNameList=names,_RC)
   do i=1,numVars
        call ESMF_FieldBundleGet(sp%acc_bundle,trim(names(i)),field=field,_RC)
        call ESMF_FieldDestroy(field,noGarbage=.true., _RC)
     enddo
     call ESMF_FieldBundleDestroy(sp%acc_bundle,noGarbage=.true.,_RC)

     


    _RETURN(ESMF_SUCCESS)
    
  end subroutine destroy_regen_ogrid_rh
    
  
     function new_sampler(metadata,input_bundle,output_bundle,write_collection_id,read_collection_id, &
             metadata_collection_id,regrid_method,fraction,items,rc) result(GriddedIO)
        type(sampler) :: GriddedIO
        type(Filemetadata), intent(in), optional :: metadata
        type(ESMF_FieldBundle), intent(in), optional :: input_bundle
        type(ESMF_FieldBundle), intent(in), optional :: output_bundle
        integer, intent(in), optional :: write_collection_id
        integer, intent(in), optional :: read_collection_id
        integer, intent(in), optional :: metadata_collection_id
        integer, intent(in), optional :: regrid_method
        integer, intent(in), optional :: fraction
        type(GriddedIOitemVector), intent(in), optional :: items
        integer, intent(out), optional :: rc

        if (present(metadata)) GriddedIO%metadata=metadata
        if (present(input_bundle)) GriddedIO%input_bundle=input_bundle
        if (present(output_bundle)) GriddedIO%output_bundle=output_bundle
        if (present(regrid_method)) GriddedIO%regrid_method=regrid_method
        if (present(write_collection_id)) GriddedIO%write_collection_id=write_collection_id
        if (present(read_collection_id)) GriddedIO%read_collection_id=read_collection_id
        if (present(metadata_collection_id)) GriddedIO%metadata_collection_id=metadata_collection_id
        if (present(items)) GriddedIO%items=items
        if (present(fraction)) GriddedIO%fraction=fraction
        _RETURN(ESMF_SUCCESS)
     end function new_sampler

     subroutine CreateFileMetaData(this,items,bundle,timeInfo,vdata,ogrid,global_attributes,rc)
        class (sampler), intent(inout) :: this
        type(GriddedIOitemVector), target, intent(inout) :: items
        type(ESMF_FieldBundle), intent(inout) :: bundle
        type(TimeData), optional, intent(inout) :: timeInfo
        type(VerticalData), intent(inout), optional :: vdata
        type (ESMF_Grid), intent(inout), pointer, optional :: ogrid
        type(StringStringMap), intent(in), optional :: global_attributes
        integer, intent(out), optional :: rc

        type(ESMF_Grid) :: input_grid
        class (AbstractGridFactory), pointer :: factory

        type(ESMF_Field) :: new_field
        type(GriddedIOitemVectorIterator) :: iter
        type(GriddedIOitem), pointer :: item
        type(stringVector) :: order
        integer :: metadataVarsSize
        type(StringStringMapIterator) :: s_iter
        character(len=:), pointer :: attr_name, attr_val
        integer :: status

        this%items = items
        this%input_bundle = bundle
        this%output_bundle = ESMF_FieldBundleCreate(rc=status)
        _VERIFY(status)
        if(present(timeInfo)) this%timeInfo = timeInfo
        call ESMF_FieldBundleGet(this%input_bundle,grid=input_grid,rc=status)
        _VERIFY(status)
        if (present(ogrid)) then
           this%output_grid=ogrid
        else
           call ESMF_FieldBundleGet(this%input_bundle,grid=this%output_grid,rc=status)
           _VERIFY(status)
        end if
        this%regrid_handle => new_regridder_manager%make_regridder(input_grid,this%output_grid,this%regrid_method,rc=status)
        _VERIFY(status)

        ! We get the regrid_method here because in the case of Identity, we set it to
        ! REGRID_METHOD_IDENTITY in the regridder constructor if identity. Now we need
        ! to change the regrid_method in the GriddedIO object to be the same as the
        ! the regridder object.
        this%regrid_method = this%regrid_handle%get_regrid_method()

        call ESMF_FieldBundleSet(this%output_bundle,grid=this%output_grid,rc=status)
        _VERIFY(status)
        factory => get_factory(this%output_grid,rc=status)
        _VERIFY(status)
        call factory%append_metadata(this%metadata)

        if (present(vdata)) then
           this%vdata=vdata
        else
           this%vdata=VerticalData(rc=status)
           _VERIFY(status)
        end if
        call this%vdata%append_vertical_metadata(this%metadata,this%input_bundle,rc=status)
        _VERIFY(status)
        this%doVertRegrid = (this%vdata%regrid_type /= VERTICAL_METHOD_NONE)
        if (this%vdata%regrid_type == VERTICAL_METHOD_ETA2LEV) call this%vdata%get_interpolating_variable(this%input_bundle,rc=status)
        _VERIFY(status)

        if(present(timeInfo)) call this%timeInfo%add_time_to_metadata(this%metadata,_RC)

        iter = this%items%begin()
        if (.not.allocated(this%chunking)) then
           call this%set_default_chunking(rc=status)
           _VERIFY(status)
        else
           call this%check_chunking(this%vdata%lm,_RC)
        end if

        order = this%metadata%get_order(rc=status)
        _VERIFY(status)
        metadataVarsSize = order%size()

        do while (iter /= this%items%end())
           item => iter%get()
           if (item%itemType == ItemTypeScalar) then
              call this%CreateVariable(item%xname,rc=status)
              _VERIFY(status)
           else if (item%itemType == ItemTypeVector) then
              call this%CreateVariable(item%xname,rc=status)
              _VERIFY(status)
              call this%CreateVariable(item%yname,rc=status)
              _VERIFY(status)
           end if
           call iter%next()
        enddo

        if (this%itemOrderAlphabetical) then
           call this%alphabatize_variables(metadataVarsSize,rc=status)
           _VERIFY(status)
        end if

        if (present(global_attributes)) then
           s_iter = global_attributes%begin()
           do while(s_iter /= global_attributes%end())
              attr_name => s_iter%key()
              attr_val => s_iter%value()
              call this%metadata%add_attribute(attr_name,attr_val,_RC)
              call s_iter%next()
           enddo
        end if


!        input_bundle/lat-lon AGCM1
!        output_bundle /    for       regrid at 2 min each:  field
!        acc_bundle  for   save field --> 

        ! __ add acc_bundle and output_bundle
        !
        this%acc_bundle = ESMF_FieldBundleCreate(_RC)
        call ESMF_FieldBundleSet(this%acc_bundle,grid=this%output_grid,_RC)
        iter = this%items%begin()
        do while (iter /= this%items%end())
           item => iter%get()
           !!call this%addVariable_to_output_bundle(item%xname,_RC)
           call this%addVariable_to_acc_bundle(item%xname,_RC)
           if (item%itemType == ItemTypeVector) then
              !!call this%addVariable_to_output_bundle(item%yname,_RC)
              call this%addVariable_to_acc_bundle(item%yname,_RC)              
           end if
           call iter%next()
        enddo

        new_field = ESMF_FieldCreate(this%output_grid ,name='time', &
               typekind=ESMF_TYPEKIND_R4,_RC)
        call MAPL_FieldBundleAdd( this%acc_bundle, new_field, _RC )
        !unit
        
        _RETURN(_SUCCESS)        
     end subroutine CreateFileMetaData


     subroutine set_param(this,deflation,quantize_algorithm,quantize_level,chunking,nbits_to_keep,regrid_method,itemOrder,write_collection_id,rc)
        class (sampler), intent(inout) :: this
        integer, optional, intent(in) :: deflation
        integer, optional, intent(in) :: quantize_algorithm
        integer, optional, intent(in) :: quantize_level
        integer, optional, intent(in) :: chunking(:)
        integer, optional, intent(in) :: nbits_to_keep
        integer, optional, intent(in) :: regrid_method
        logical, optional, intent(in) :: itemOrder
        integer, optional, intent(in) :: write_collection_id
        integer, optional, intent(out) :: rc

        integer :: status

        if (present(regrid_method)) this%regrid_method=regrid_method
        if (present(nbits_to_keep)) this%nbits_to_keep=nbits_to_keep
        if (present(deflation)) this%deflateLevel = deflation
        if (present(quantize_algorithm)) this%quantizeAlgorithm = quantize_algorithm
        if (present(quantize_level)) this%quantizeLevel = quantize_level
        if (present(chunking)) then
           allocate(this%chunking,source=chunking,stat=status)
           _VERIFY(status)
        end if
        if (present(itemOrder)) this%itemOrderAlphabetical = itemOrder
        if (present(write_collection_id)) this%write_collection_id=write_collection_id
        _RETURN(ESMF_SUCCESS)

     end subroutine set_param

     subroutine set_default_chunking(this,rc)
        class (sampler), intent(inout) :: this
        integer, optional, intent(out) :: rc

        integer ::  global_dim(3)
        integer :: status

        call MAPL_GridGet(this%output_grid,globalCellCountPerDim=global_dim,rc=status)
        _VERIFY(status)
        if (global_dim(1)*6 == global_dim(2)) then
           allocate(this%chunking(5))
           this%chunking(1) = global_dim(1)
           this%chunking(2) = global_dim(1)
           this%chunking(3) = 1
           this%chunking(4) = 1
           this%chunking(5) = 1
        else
           allocate(this%chunking(4))
           this%chunking(1) = global_dim(1)
           this%chunking(2) = global_dim(2)
           this%chunking(3) = 1
           this%chunking(4) = 1
        endif
        _RETURN(ESMF_SUCCESS)

     end subroutine set_default_chunking

     subroutine check_chunking(this,lev_size,rc)
        class (sampler), intent(inout) :: this
        integer, intent(in) :: lev_size
        integer, optional, intent(out) :: rc

        integer ::  global_dim(3)
        integer :: status
        character(len=5) :: c1,c2

        call MAPL_GridGet(this%output_grid,globalCellCountPerDim=global_dim,rc=status)
        _VERIFY(status)
        if (global_dim(1)*6 == global_dim(2)) then
           write(c2,'(I5)')global_dim(1)
           write(c1,'(I5)')this%chunking(1)
           _ASSERT(this%chunking(1) <= global_dim(1), "Chunk for Xdim "//c1//" must be less than or equal to "//c2)
           write(c1,'(I5)')this%chunking(2)
           _ASSERT(this%chunking(2) <= global_dim(1), "Chunk for Ydim "//c1//" must be less than or equal to "//c2)
           _ASSERT(this%chunking(3) <= 6, "Chunksize for face dimension must be 6 or less")
           if (lev_size > 0) then
              write(c2,'(I5)')lev_size
              write(c1,'(I5)')this%chunking(4)
              _ASSERT(this%chunking(4) <= lev_size, "Chunk for level size "//c1//" must be less than or equal to "//c2)
           end if
           _ASSERT(this%chunking(5) == 1, "Time must have chunk size of 1")
        else
           write(c2,'(I5)')global_dim(1)
           write(c1,'(I5)')this%chunking(1)
           _ASSERT(this%chunking(1) <= global_dim(1), "Chunk for lon "//c1//" must be less than or equal to "//c2)
           write(c2,'(I5)')global_dim(2)
           write(c1,'(I5)')this%chunking(2)
           _ASSERT(this%chunking(2) <= global_dim(2), "Chunk for lat "//c1//" must be less than or equal to "//c2)
           if (lev_size > 0) then
              write(c2,'(I5)')lev_size
              write(c1,'(I5)')this%chunking(3)
              _ASSERT(this%chunking(3) <= lev_size, "Chunk for level size "//c1//" must be less than or equal to "//c2)
           end if
           _ASSERT(this%chunking(4) == 1, "Time must have chunk size of 1")
        endif
        _RETURN(ESMF_SUCCESS)

     end subroutine check_chunking

     subroutine CreateVariable(this,itemName,rc)
        class (sampler), intent(inout) :: this
        character(len=*), intent(in) :: itemName
        integer, optional, intent(out) :: rc

        integer :: status

        type(ESMF_Field) :: field,newField
        class (AbstractGridFactory), pointer :: factory
        integer :: fieldRank
        logical :: isPresent
        character(len=ESMF_MAXSTR) :: varName,longName,units
        character(len=:), allocatable :: grid_dims
        character(len=:), allocatable :: vdims
        type(Variable) :: v

        call ESMF_FieldBundleGet(this%input_bundle,itemName,field=field,rc=status)
        _VERIFY(status)
        factory => get_factory(this%output_grid,rc=status)
        _VERIFY(status)

        call ESMF_FieldGet(field,rank=fieldRank,rc=status)
        _VERIFY(status)
        call ESMF_FieldGet(field,name=varName,rc=status)
        _VERIFY(status)
        call ESMF_AttributeGet(field,name="LONG_NAME",isPresent=isPresent,rc=status)
        _VERIFY(status)
        if ( isPresent ) then
           call ESMF_AttributeGet  (FIELD, NAME="LONG_NAME",VALUE=LongName, RC=STATUS)
           _VERIFY(STATUS)
        else
           LongName = varName
        endif
        call ESMF_AttributeGet(field,name="UNITS",isPresent=isPresent,rc=status)
        _VERIFY(status)
        if ( isPresent ) then
           call ESMF_AttributeGet  (FIELD, NAME="UNITS",VALUE=units, RC=STATUS)
           _VERIFY(STATUS)
        else
           units = 'unknown'
        endif
        grid_dims = factory%get_grid_vars()
        if (this%timeInfo%is_initialized) then
           if (fieldRank==2) then
              vdims=grid_dims//",time"
           else if (fieldRank==3) then
              vdims=grid_dims//",lev,time"
           else
              _FAIL( 'Unsupported field rank')
           end if
        else
           if (fieldRank==2) then
              vdims=grid_dims
           else if (fieldRank==3) then
              vdims=grid_dims//",lev"
           else
              _FAIL( 'Unsupported field rank')
           end if
        end if

        write(6,*) 'in createVar: vdims=', trim(vdims)

        v = Variable(type=PFIO_REAL32,dimensions=vdims,chunksizes=this%chunking,deflation=this%deflateLevel,quantize_algorithm=this%quantizeAlgorithm,quantize_level=this%quantizeLevel)
        call v%add_attribute('units',trim(units))
        call v%add_attribute('long_name',trim(longName))
        call v%add_attribute('standard_name',trim(longName))
        call v%add_attribute('missing_value',MAPL_UNDEF)
        call v%add_attribute('fmissing_value',MAPL_UNDEF)
        call v%add_attribute('vmax',MAPL_UNDEF)
        call v%add_attribute('vmin',-MAPL_UNDEF)
        call v%add_attribute('scale_factor',1.0)
        call v%add_attribute('add_offset',0.0)
        call v%add_attribute('_FillValue',MAPL_UNDEF)
        call v%add_attribute('valid_range',(/-MAPL_UNDEF,MAPL_UNDEF/))
        ! Weird workaround for NAG 7.1.113
#ifdef __NAG_COMPILER_RELEASE
        associate (s => regrid_method_int_to_string(this%regrid_method))
          call v%add_attribute('regrid_method', s)
        end associate
#else
        call v%add_attribute('regrid_method', regrid_method_int_to_string(this%regrid_method))
#endif
        call factory%append_variable_metadata(v)
        call this%metadata%add_variable(trim(varName),v,rc=status)
        _VERIFY(status)
        ! finally make a new field if neccessary
        if (this%doVertRegrid .and. (fieldRank ==3) ) then
           newField = MAPL_FieldCreate(field,this%output_grid,lm=this%vData%lm,rc=status)
           _VERIFY(status)
           call MAPL_FieldBundleAdd(this%output_bundle,newField,rc=status)
           _VERIFY(status)
        else
           newField = MAPL_FieldCreate(field,this%output_grid,rc=status)
           _VERIFY(status)
           call MAPL_FieldBundleAdd(this%output_bundle,newField,rc=status)
           _VERIFY(status)
        end if
        _RETURN(_SUCCESS)

     end subroutine CreateVariable

     subroutine modifyTime(this, oClients, rc)
        class(sampler), intent(inout) :: this
        type (ClientManager), optional, intent(inout) :: oClients
        integer, optional, intent(out) :: rc

        type(Variable) :: v
        type(StringVariableMap) :: var_map
        integer :: status

        if (this%timeInfo%is_initialized) then
           v = this%timeInfo%define_time_variable(_RC)
           call this%metadata%modify_variable('time',v,rc=status)
           _VERIFY(status)
           if (present(oClients)) then
              call var_map%insert('time',v)
              call oClients%modify_metadata(this%write_collection_id, var_map=var_map, rc=status)
              _VERIFY(status)
           end if
        else
           _FAIL("Time was not initialized for the GriddedIO class instance")
        end if
        _RETURN(ESMF_SUCCESS)

     end subroutine modifyTime

     subroutine modifyTimeIncrement(this, frequency, rc)
        class(sampler), intent(inout) :: this
        integer, intent(in) :: frequency
        integer, optional, intent(out) :: rc

        integer :: status

        if (this%timeInfo%is_initialized) then
           call this%timeInfo%setFrequency(frequency, rc=status)
           _VERIFY(status)
        else
           _FAIL("Time was not initialized for the GriddedIO class instance")
        end if

        _RETURN(ESMF_SUCCESS)

      end subroutine modifyTimeIncrement

     subroutine bundlepost(this,filename,oClients,rc)
        class (sampler), intent(inout) :: this
        character(len=*), intent(in) :: filename
        type (ClientManager), optional, intent(inout) :: oClients
        integer, optional, intent(out) :: rc

        integer :: status
        type(ESMF_Field) :: outField
        integer :: tindex
        type(ArrayReference) :: ref

        type(GriddedIOitemVectorIterator) :: iter
        type(GriddedIOitem), pointer :: item
        logical :: have_time
  
        have_time = this%timeInfo%am_i_initialized()

        if (have_time) then
           this%times = this%timeInfo%compute_time_vector(this%metadata,rc=status)
           _VERIFY(status)
           ref = ArrayReference(this%times)
           call oClients%stage_nondistributed_data(this%write_collection_id,trim(filename),'time',ref)

           tindex = size(this%times)
           if (tindex==1) then
              call this%stage2DLatLon(filename,oClients=oClients,_RC)
           end if
        else
           tindex = -1
        end if

        if (this%vdata%regrid_type==VERTICAL_METHOD_ETA2LEV) then
           call this%vdata%setup_eta_to_pressure(regrid_handle=this%regrid_handle,output_grid=this%output_grid,rc=status)
           _VERIFY(status)
        end if

        iter = this%items%begin()
        do while (iter /= this%items%end())
           item => iter%get()
           if (item%itemType == ItemTypeScalar) then
              call this%RegridScalar(item%xname,rc=status)
              _VERIFY(status)
              call ESMF_FieldBundleGet(this%output_bundle,item%xname,field=outField,rc=status)
              _VERIFY(status)

              
              if (this%vdata%regrid_type==VERTICAL_METHOD_ETA2LEV) then
                 call this%vdata%correct_topo(outField,rc=status)
                 _VERIFY(status)
              end if
              call this%stageData(outField,filename,tIndex, oClients=oClients,rc=status)
              _VERIFY(status)


           else if (item%itemType == ItemTypeVector) then
              call this%RegridVector(item%xname,item%yname,rc=status)
              _VERIFY(status)
              call ESMF_FieldBundleGet(this%output_bundle,item%xname,field=outField,rc=status)
              _VERIFY(status)
              if (this%vdata%regrid_type==VERTICAL_METHOD_ETA2LEV) then
                 call this%vdata%correct_topo(outField,rc=status)
                 _VERIFY(status)
              end if
              call this%stageData(outField,filename,tIndex,oClients=oClients,rc=status)
              _VERIFY(status)
              call ESMF_FieldBundleGet(this%output_bundle,item%yname,field=outField,rc=status)
              _VERIFY(status)
              if (this%vdata%regrid_type==VERTICAL_METHOD_ETA2LEV) then
                 call this%vdata%correct_topo(outField,rc=status)
                 _VERIFY(status)
              end if
              call this%stageData(outField,filename,tIndex,oClients=oClients,rc=status)
              _VERIFY(status)
           end if
           call iter%next()
        enddo

        _RETURN(ESMF_SUCCESS)

     end subroutine bundlepost

     subroutine RegridScalar(this,itemName,rc)
        class (sampler), intent(inout) :: this
        character(len=*), intent(in) :: itemName
        integer, optional, intent(out) :: rc

        integer :: status

        type(ESMF_Field) :: field,outField
        integer :: fieldRank
        real, pointer :: ptr3d(:,:,:),outptr3d(:,:,:)
        real, pointer :: ptr2d(:,:), outptr2d(:,:)
        real, allocatable, target :: ptr3d_inter(:,:,:)
        type(ESMF_Grid) :: gridIn,gridOut
        logical :: hasDE_in, hasDE_out

        call ESMF_FieldBundleGet(this%output_bundle,itemName,field=outField,rc=status)
        _VERIFY(status)
        call ESMF_FieldBundleGet(this%input_bundle,grid=gridIn,rc=status)
        _VERIFY(status)
        call ESMF_FieldBundleGet(this%output_bundle,grid=gridOut,rc=status)
        _VERIFY(status)
        hasDE_in = MAPL_GridHasDE(gridIn,rc=status)
        _VERIFY(status)
        hasDE_out = MAPL_GridHasDE(gridOut,rc=status)
        _VERIFY(status)

        if (this%doVertRegrid) then
           call ESMF_FieldBundleGet(this%input_bundle,itemName,field=field,rc=status)
           _VERIFY(status)
           call ESMF_FieldGet(Field,rank=fieldRank,rc=status)
           _VERIFY(status)
           if (fieldRank==3) then
              if (hasDE_in) then
                 call ESMF_FieldGet(field,farrayPtr=ptr3d,rc=status)
                 _VERIFY(status)
              else
                 allocate(ptr3d(0,0,0))
              end if
              allocate(ptr3d_inter(size(ptr3d,1),size(ptr3d,2),this%vdata%lm),stat=status)
              _VERIFY(status)
              if (this%vdata%regrid_type==VERTICAL_METHOD_SELECT) then
                 call this%vdata%regrid_select_level(ptr3d,ptr3d_inter,rc=status)
                 _VERIFY(status)
              else if (this%vdata%regrid_type==VERTICAL_METHOD_ETA2LEV) then
                 call this%vdata%regrid_eta_to_pressure(ptr3d,ptr3d_inter,rc=status)
                 _VERIFY(status)
              else if (this%vdata%regrid_type==VERTICAL_METHOD_FLIP) then
                 call this%vdata%flip_levels(ptr3d,ptr3d_inter,rc=status)
                 _VERIFY(status)
              end if
              ptr3d => ptr3d_inter
           end if
        else
           if (associated(ptr3d)) nullify(ptr3d)
        end if

        call ESMF_FieldBundleGet(this%input_bundle,itemName,field=field,rc=status)
        _VERIFY(status)
        call ESMF_FieldGet(field,rank=fieldRank,rc=status)
        _VERIFY(status)
        if (fieldRank==2) then
           if (hasDE_in) then
              call MAPL_FieldGetPointer(field,ptr2d,rc=status)
              _VERIFY(status)
           else
              allocate(ptr2d(0,0))
           end if
           if (hasDE_out) then
              call MAPL_FieldGetPointer(OutField,outptr2d,rc=status)
              _VERIFY(status)
           else
              allocate(outptr2d(0,0))
           end if
           if (gridIn==gridOut) then
              outPtr2d=ptr2d
           else
              if (this%regrid_method==REGRID_METHOD_FRACTION) ptr2d=ptr2d-this%fraction
              call this%regrid_handle%regrid(ptr2d,outPtr2d,rc=status)
              _VERIFY(status)
           end if

           print *, maxval(ptr2d)
           print *, minval(ptr2d)
           print *, maxval(outptr2d)
           print *, minval(outptr2d)           

           
        else if (fieldRank==3) then
           if (.not.associated(ptr3d)) then
              if (hasDE_in) then
                 call ESMF_FieldGet(field,farrayPtr=ptr3d,rc=status)
                 _VERIFY(status)
              else
                 allocate(ptr3d(0,0,0))
              end if
           end if
           if (hasDE_out) then
              call MAPL_FieldGetPointer(OutField,outptr3d,rc=status)
              _VERIFY(status)
           else
              allocate(outptr3d(0,0,0))
           end if
           if (gridIn==gridOut) then
              outPtr3d=Ptr3d
           else
              if (this%regrid_method==REGRID_METHOD_FRACTION) ptr3d=ptr3d-this%fraction
              call this%regrid_handle%regrid(ptr3d,outPtr3d,rc=status)
              _VERIFY(status)
           end if
        else
           _FAIL('rank not supported')
        end if

        if (allocated(ptr3d_inter)) deallocate(ptr3d_inter)
        _RETURN(_SUCCESS)

     end subroutine RegridScalar

     subroutine RegridVector(this,xName,yName,rc)
        class (sampler), intent(inout) :: this
        character(len=*), intent(in) :: xName
        character(len=*), intent(in) :: yName
        integer, optional, intent(out) :: rc

        integer :: status

        type(ESMF_Field) :: xfield,xoutField
        type(ESMF_Field) :: yfield,youtField
        integer :: fieldRank
        real, pointer :: xptr3d(:,:,:),xoutptr3d(:,:,:)
        real, pointer :: xptr2d(:,:), xoutptr2d(:,:)
        real, allocatable, target :: xptr3d_inter(:,:,:)
        real, pointer :: yptr3d(:,:,:),youtptr3d(:,:,:)
        real, pointer :: yptr2d(:,:), youtptr2d(:,:)
        real, allocatable, target :: yptr3d_inter(:,:,:)
        type(ESMF_Grid) :: gridIn, gridOut
        logical :: hasDE_in, hasDE_out

        call ESMF_FieldBundleGet(this%output_bundle,xName,field=xoutField,rc=status)
        _VERIFY(status)
        call ESMF_FieldBundleGet(this%output_bundle,yName,field=youtField,rc=status)
        _VERIFY(status)
        call ESMF_FieldBundleGet(this%input_bundle,grid=gridIn,rc=status)
        _VERIFY(status)
        call ESMF_FieldBundleGet(this%output_bundle,grid=gridOut,rc=status)
        _VERIFY(status)
        hasDE_in = MAPL_GridHasDE(gridIn,rc=status)
        _VERIFY(status)
        hasDE_out = MAPL_GridHasDE(gridOut,rc=status)
        _VERIFY(status)

        if (this%doVertRegrid) then
           call ESMF_FieldBundleGet(this%input_bundle,xName,field=xfield,rc=status)
           _VERIFY(status)
           call ESMF_FieldGet(xField,rank=fieldRank,rc=status)
           _VERIFY(status)
           if (fieldRank==3) then
              if (hasDE_in) then
                 call ESMF_FieldGet(xfield,farrayPtr=xptr3d,rc=status)
                 _VERIFY(status)
              else
                 allocate(xptr3d(0,0,0))
              end if
              allocate(xptr3d_inter(size(xptr3d,1),size(xptr3d,2),this%vdata%lm),stat=status)
              _VERIFY(status)
              if (this%vdata%regrid_type==VERTICAL_METHOD_SELECT) then
                 call this%vdata%regrid_select_level(xptr3d,xptr3d_inter,rc=status)
                 _VERIFY(status)
              else if (this%vdata%regrid_type==VERTICAL_METHOD_ETA2LEV) then
                 call this%vdata%regrid_eta_to_pressure(xptr3d,xptr3d_inter,rc=status)
                 _VERIFY(status)
              else if (this%vdata%regrid_type==VERTICAL_METHOD_FLIP) then
                 call this%vdata%flip_levels(xptr3d,xptr3d_inter,rc=status)
                 _VERIFY(status)
              end if
              xptr3d => xptr3d_inter
           end if
           call ESMF_FieldBundleGet(this%input_bundle,yName,field=yfield,rc=status)
           _VERIFY(status)
           call ESMF_FieldGet(yField,rank=fieldRank,rc=status)
           _VERIFY(status)
           if (fieldRank==3) then
              if (hasDE_in) then
                 call ESMF_FieldGet(yfield,farrayPtr=yptr3d,rc=status)
                 _VERIFY(status)
              else
                 allocate(yptr3d(0,0,0))
              end if
              allocate(yptr3d_inter(size(yptr3d,1),size(yptr3d,2),this%vdata%lm),stat=status)
              _VERIFY(status)
              if (this%vdata%regrid_type==VERTICAL_METHOD_SELECT) then
                 call this%vdata%regrid_select_level(yptr3d,yptr3d_inter,rc=status)
                 _VERIFY(status)
              else if (this%vdata%regrid_type==VERTICAL_METHOD_ETA2LEV) then
                 call this%vdata%regrid_eta_to_pressure(yptr3d,yptr3d_inter,rc=status)
                 _VERIFY(status)
              else if (this%vdata%regrid_type==VERTICAL_METHOD_FLIP) then
                 call this%vdata%flip_levels(yptr3d,yptr3d_inter,rc=status)
                 _VERIFY(status)
              end if
              yptr3d => yptr3d_inter
           end if
        else
           if (associated(xptr3d)) nullify(xptr3d)
           if (associated(yptr3d)) nullify(yptr3d)
        end if

        call ESMF_FieldBundleGet(this%input_bundle,xname,field=xfield,rc=status)
        _VERIFY(status)
        call ESMF_FieldBundleGet(this%input_bundle,yname,field=yfield,rc=status)
        _VERIFY(status)
        call ESMF_FieldGet(xfield,rank=fieldRank,rc=status)
        _VERIFY(status)
        if (fieldRank==2) then
           if (hasDE_in) then
              call MAPL_FieldGetPointer(xfield,xptr2d,rc=status)
              _VERIFY(status)
              call MAPL_FieldGetPointer(yfield,yptr2d,rc=status)
              _VERIFY(status)
           else
              allocate(xptr2d(0,0))
              allocate(yptr2d(0,0))
           end if

           if (hasDE_in) then
              call MAPL_FieldGetPointer(xOutField,xoutptr2d,rc=status)
              _VERIFY(status)
              call MAPL_FieldGetPointer(yOutField,youtptr2d,rc=status)
              _VERIFY(status)
           else
              allocate(xoutptr2d(0,0))
              allocate(youtptr2d(0,0))
           end if


           if (gridIn==gridOut) then
              xoutPtr2d=xptr2d
              youtPtr2d=yptr2d
           else
              call this%regrid_handle%regrid(xptr2d,yptr2d,xoutPtr2d,youtPtr2d,rc=status)
              _VERIFY(status)
           end if
        else if (fieldRank==3) then
           if (.not.associated(xptr3d)) then
              if (hasDE_in) then
                 call MAPL_FieldGetPointer(xfield,xptr3d,rc=status)
                 _VERIFY(status)
              else
                 allocate(xptr3d(0,0,0))
              end if
           end if
           if (.not.associated(yptr3d)) then
              if (hasDE_in) then
                 call MAPL_FieldGetPointer(yfield,yptr3d,rc=status)
                 _VERIFY(status)
              else
                 allocate(yptr3d(0,0,0))
              end if
           end if

           if (hasDE_out) then
              call MAPL_FieldGetPointer(xOutField,xoutptr3d,rc=status)
              _VERIFY(status)
              call MAPL_FieldGetPointer(yOutField,youtptr3d,rc=status)
              _VERIFY(status)
           else
              allocate(xoutptr3d(0,0,0))
              allocate(youtptr3d(0,0,0))
           end if

           if (gridIn==gridOut) then
              xoutPtr3d=xptr3d
              youtPtr3d=yptr3d
           else
              call this%regrid_handle%regrid(xptr3d,yptr3d,xoutPtr3d,youtPtr3d,rc=status)
              _VERIFY(status)
           end if
        end if

        if (allocated(xptr3d_inter)) deallocate(xptr3d_inter)
        if (allocated(yptr3d_inter)) deallocate(yptr3d_inter)
        _RETURN(_SUCCESS)

     end subroutine RegridVector

  subroutine stage2DLatLon(this, fileName, oClients, rc)
     class (sampler), intent(inout) :: this
     character(len=*), intent(in) :: fileName
     type (ClientManager), optional, intent(inout) :: oClients
     integer, optional, intent(out) :: rc

     integer :: status
     real(REAL64), pointer :: ptr2d(:,:)
     type(ArrayReference) :: ref
     class (AbstractGridFactory), pointer :: factory
     integer, allocatable :: localStart(:),globalStart(:),globalCount(:)
     logical :: hasll
     class(Variable), pointer :: var_lat,var_lon

     var_lon => this%metadata%get_variable('lons')
     var_lat => this%metadata%get_variable('lats')

     hasll = associated(var_lon) .and. associated(var_lat)
     if (hasll) then
        factory => get_factory(this%output_grid,rc=status)
        _VERIFY(status)

        call factory%generate_file_bounds(this%output_grid,LocalStart,GlobalStart,GlobalCount,rc=status)
        _VERIFY(status)
        call ESMF_GridGetCoord(this%output_grid, localDE=0, coordDim=1, &
        staggerloc=ESMF_STAGGERLOC_CENTER, &
        farrayPtr=ptr2d, rc=status)
        _VERIFY(STATUS)
        this%lons=ptr2d*MAPL_RADIANS_TO_DEGREES
        ref = ArrayReference(this%lons)
         call oClients%collective_stage_data(this%write_collection_id,trim(filename),'lons', &
              ref,start=localStart, global_start=GlobalStart, global_count=GlobalCount)
        call ESMF_GridGetCoord(this%output_grid, localDE=0, coordDim=2, &
        staggerloc=ESMF_STAGGERLOC_CENTER, &
        farrayPtr=ptr2d, rc=status)
        _VERIFY(STATUS)
        if (.not.allocated(this%lats)) allocate(this%lats(size(ptr2d,1),size(ptr2d,2)))
        this%lats=ptr2d*MAPL_RADIANS_TO_DEGREES
        ref = ArrayReference(this%lats)
         call oClients%collective_stage_data(this%write_collection_id,trim(filename),'lats', &
              ref,start=localStart, global_start=GlobalStart, global_count=GlobalCount)
        deallocate(LocalStart,GlobalStart,GlobalCount)
     end if

     var_lon => this%metadata%get_variable('corner_lons')
     var_lat => this%metadata%get_variable('corner_lats')

     hasll = associated(var_lon) .and. associated(var_lat)
     if (hasll) then
        factory => get_factory(this%output_grid,rc=status)
        _VERIFY(status)

        call factory%generate_file_corner_bounds(this%output_grid,LocalStart,GlobalStart,GlobalCount,rc=status)
        _VERIFY(status)
        call ESMF_GridGetCoord(this%output_grid, localDE=0, coordDim=1, &
        staggerloc=ESMF_STAGGERLOC_CORNER, &
        farrayPtr=ptr2d, rc=status)
        _VERIFY(STATUS)
        this%corner_lons=ptr2d*MAPL_RADIANS_TO_DEGREES
        ref = ArrayReference(this%corner_lons)
         call oClients%collective_stage_data(this%write_collection_id,trim(filename),'corner_lons', &
              ref,start=localStart, global_start=GlobalStart, global_count=GlobalCount)
        call ESMF_GridGetCoord(this%output_grid, localDE=0, coordDim=2, &
        staggerloc=ESMF_STAGGERLOC_CORNER, &
        farrayPtr=ptr2d, rc=status)
        _VERIFY(STATUS)
        if (.not.allocated(this%corner_lats)) allocate(this%corner_lats(size(ptr2d,1),size(ptr2d,2)))
        this%corner_lats=ptr2d*MAPL_RADIANS_TO_DEGREES
        ref = ArrayReference(this%corner_lats)
         call oClients%collective_stage_data(this%write_collection_id,trim(filename),'corner_lats', &
              ref,start=localStart, global_start=GlobalStart, global_count=GlobalCount)
     end if
     _RETURN(_SUCCESS)

  end subroutine stage2DLatLon

  subroutine stageData(this, field, fileName, tIndex, oClients, rc)
     class (sampler), intent(inout) :: this
     type(ESMF_Field), intent(inout) :: field
     character(len=*), intent(in) :: fileName
     integer, intent(in) :: tIndex
     type (ClientManager), optional, intent(inout) :: oClients
     integer, optional, intent(out) :: rc

     integer :: status
     integer :: fieldRank
     character(len=ESMF_MAXSTR) :: fieldName
     real, pointer :: ptr3d(:,:,:) => null()
     real, pointer :: ptr2d(:,:) => null()
     type(ArrayReference) :: ref
     integer :: lm
     logical :: hasDE
     integer, allocatable :: localStart(:),globalStart(:),globalCount(:)
     integer, allocatable :: gridLocalStart(:),gridGlobalStart(:),gridGlobalCount(:)
     class (AbstractGridFactory), pointer :: factory
     real, allocatable :: temp_2d(:,:), temp_3d(:,:,:)
     type(ESMF_VM) :: vm
     integer :: mpi_comm

     call ESMF_VMGetCurrent(vm,_RC)
     call ESMF_VMGet(vm,mpiCommunicator=mpi_comm,_RC)

     factory => get_factory(this%output_grid,rc=status)
     _VERIFY(status)
     hasDE = MAPL_GridHasDE(this%output_grid,rc=status)
     _VERIFY(status)
     lm = this%vdata%lm
     call ESMF_FieldGet(field,rank=fieldRank,name=fieldName,rc=status)
     _VERIFY(status)

     call factory%generate_file_bounds(this%output_grid,gridLocalStart,gridGlobalStart,gridGlobalCount,rc=status)
     _VERIFY(status)
     if (fieldRank==2) then
        if (hasDE) then
           call ESMF_FieldGet(Field,farrayPtr=ptr2d,rc=status)
           _VERIFY(status)
           if (this%nbits_to_keep < MAPL_NBITS_UPPER_LIMIT) then
              allocate(temp_2d,source=ptr2d)
              call DownBit(temp_2d,ptr2d,this%nbits_to_keep,undef=MAPL_undef,mpi_comm=mpi_comm,rc=status)
              _VERIFY(status)
           end if
        else
           allocate(ptr2d(0,0))
        end if
        ref = factory%generate_file_reference2D(Ptr2D)
        allocate(localStart,source=[gridLocalStart,1])
        if (tindex > -1) then
           allocate(globalStart,source=[gridGlobalStart,tindex])
           allocate(globalCount,source=[gridGlobalCount,1])
        else
           allocate(globalStart,source=gridGlobalStart)
           allocate(globalCount,source=gridGlobalCount)
        end if
      else if (fieldRank==3) then
         if (HasDE) then
            call ESMF_FieldGet(field,farrayPtr=ptr3d,rc=status)
            _VERIFY(status)
            if (this%nbits_to_keep < MAPL_NBITS_UPPER_LIMIT) then
               allocate(temp_3d,source=ptr3d)
               call DownBit(temp_3d,ptr3d,this%nbits_to_keep,undef=MAPL_undef,mpi_comm=mpi_comm,rc=status)
               _VERIFY(status)
            end if
         else
            allocate(ptr3d(0,0,0))
         end if
         ref = factory%generate_file_reference3D(Ptr3D)
         allocate(localStart,source=[gridLocalStart,1,1])
         if (tindex > -1) then
            allocate(globalStart,source=[gridGlobalStart,1,tindex])
            allocate(globalCount,source=[gridGlobalCount,lm,1])
         else
            allocate(globalStart,source=[gridGlobalStart,1])
            allocate(globalCount,source=[gridGlobalCount,lm])
         end if
      else
         _FAIL( "Rank not supported")
      end if
      call oClients%collective_stage_data(this%write_collection_id,trim(filename),trim(fieldName), &
           ref,start=localStart, global_start=GlobalStart, global_count=GlobalCount)
      _RETURN(_SUCCESS)

  end subroutine stageData

  subroutine alphabatize_variables(this,nfixedVars,rc)
     class (sampler), intent(inout) :: this
     integer, intent(in) :: nFixedVars
     integer, optional, intent(out) :: rc

     type(StringVector) :: order
     type(StringVector) :: newOrder
     character(len=:), pointer :: v1
     character(len=ESMF_MAXSTR) :: c1,c2
     character(len=ESMF_MAXSTR), allocatable :: temp(:)
     logical :: swapped
     integer :: n,i
     integer :: status

     order = this%metadata%get_order(rc=status)
     _VERIFY(status)
     n = Order%size()
     allocate(temp(nFixedVars+1:n))
     do i=1,n
        v1 => order%at(i)
        if ( i > nFixedVars) temp(i)=trim(v1)
     enddo

     swapped = .true.
     do while(swapped)
        swapped = .false.
        do i=nFixedVars+1,n-1
           c1 = temp(i)
           c2 = temp(i+1)
           if (c1 > c2) then
              temp(i+1)=c1
              temp(i)=c2
              swapped =.true.
           end if
        enddo
     enddo

     do i=1,nFixedVars
        v1 => Order%at(i)
        call newOrder%push_back(v1)
     enddo
     do i=nFixedVars+1,n
        call newOrder%push_back(trim(temp(i)))
     enddo
     call this%metadata%set_order(newOrder,rc=status)
     _VERIFY(status)
     deallocate(temp)

     _RETURN(_SUCCESS)

  end subroutine alphabatize_variables

  subroutine request_data_from_file(this,filename,timeindex,rc)
     class(sampler), intent(inout) :: this
     character(len=*), intent(in) :: filename
     integer, intent(in) :: timeindex
     integer, intent(out), optional :: rc

     integer :: status
     type(esmf_grid) :: filegrid
     type(maplDatacollection), pointer :: collection
     integer :: i,numVars
     character(len=ESMF_MAXSTR), allocatable :: names(:)
     type(ESMF_Field) :: output_field
     type(ESMF_Field), allocatable :: input_fields(:)
     integer :: ub(1),lb(1),dims(3),lm,rank
     type(ArrayReference) :: ref
     real, pointer :: ptr2d(:,:) => null()
     real, pointer :: ptr3d(:,:,:) => null()
     integer, allocatable :: localStart(:), globalStart(:), globalCount(:)
     integer, allocatable :: gridLocalStart(:), gridGlobalStart(:), gridGlobalCount(:)
     type(ESMF_Grid) :: output_grid
     logical :: hasDE
     class(AbstractGridFactory), pointer :: factory
     real(REAL32) :: missing_value

     collection => Datacollections%at(this%metadata_collection_id)
     this%current_file_metadata => collection%find(filename, _RC)
     filegrid = collection%src_grid
     factory => get_factory(filegrid)
     hasDE=MAPL_GridHasDE(filegrid,rc=status)
     _VERIFY(status)
     call ESMF_FieldBundleGet(this%output_bundle,grid=output_grid,rc=status)
     _VERIFY(status)
     if (filegrid/=output_grid) then
        this%regrid_handle => new_regridder_manager%make_regridder(filegrid,output_grid,this%regrid_method,rc=status)
        _VERIFY(status)
     end if
     call MAPL_GridGet(filegrid,globalCellCountPerdim=dims,rc=status)
     _VERIFY(status)
     call factory%generate_file_bounds(fileGrid,gridLocalStart,gridGlobalStart,gridGlobalCount,metadata=this%current_file_metadata%fileMetadata,rc=status)
     _VERIFY(status)
     ! create input bundle
     call ESMF_FieldBundleGet(this%output_bundle,fieldCount=numVars,rc=status)
     _VERIFY(status)
     allocate(names(numVars),stat=status)
     _VERIFY(status)
     allocate(input_fields(numVars),stat=status)
     _VERIFY(status)
     call ESMF_FieldBundleGet(this%output_bundle,fieldNameList=names,rc=status)
     _VERIFY(status)
     do i=1,numVars
        call ESMF_FieldBundleGet(this%output_bundle,names(i),field=output_field,rc=status)
        _VERIFY(status)
        call ESMF_FieldGet(output_field,rank=rank,rc=status)
        _VERIFY(status)
        if (rank==2) then
           input_fields(i) = ESMF_FieldCreate(filegrid,typekind=ESMF_TYPEKIND_R4,gridToFieldMap=[1,2],name=trim(names(i)),rc=status)
           _VERIFY(status)
           if (hasDE) then
              call ESMF_FieldGet(input_fields(I),0,farrayPtr=ptr2d,rc=status)
              _VERIFY(status)
           else
              allocate(ptr2d(0,0),stat=status)
              _VERIFY(status)
           end if
           ref=factory%generate_file_reference2D(ptr2d)
           allocate(localStart,source=[gridLocalStart,timeIndex])
           allocate(globalStart,source=[gridGlobalStart,timeIndex])
           allocate(globalCount,source=[gridGlobalCount,1])
        else if (rank==3) then
           call ESMF_FieldGet(output_field,ungriddedLBound=lb,ungriddedUBound=ub,rc=status)
           _VERIFY(status)
           lm=ub(1)-lb(1)+1
           input_fields(i) = ESMF_FieldCreate(filegrid,typekind=ESMF_TYPEKIND_R4,gridToFieldMap=[1,2], &
              ungriddedLBound=lb,ungriddedUBound=ub,name=trim(names(i)),rc=status)
           _VERIFY(status)
           if (hasDE) then
              call ESMF_FieldGet(input_fields(I),0,farrayPtr=ptr3d,rc=status)
              _VERIFY(status)
           else
              allocate(ptr3d(0,0,0),stat=status)
              _VERIFY(status)
           end if
           ref=factory%generate_file_reference3D(ptr3d,metadata=this%current_file_metadata%filemetadata)
           allocate(localStart,source=[gridLocalStart,1,timeIndex])
           allocate(globalStart,source=[gridGlobalStart,1,timeIndex])
           allocate(globalCount,source=[gridGlobalCount,lm,1])
        end if
        call i_Clients%collective_prefetch_data( &
             this%read_collection_id, fileName, trim(names(i)), &
             & ref, start=localStart, global_start=globalStart, global_count=globalCount)
        deallocate(localStart,globalStart,globalCount)
     enddo
     deallocate(gridLocalStart,gridGlobalStart,gridGlobalCount)
     this%input_bundle = ESMF_FieldBundleCreate(fieldList=input_fields,rc=status)
     _VERIFY(status)
     _RETURN(_SUCCESS)

  end subroutine request_data_from_file

  subroutine process_data_from_file(this,rc)
     class(sampler), intent(inout) :: this
     integer, intent(out), optional :: rc

     integer :: status
     integer :: i,numVars
     character(len=ESMF_MAXSTR), allocatable :: names(:)
     type(ESMF_Field) :: field
     type(GriddedIOitem), pointer :: item
     type(GriddedIOitemVectorIterator) :: iter

     call ESMF_FieldBundleGet(this%output_bundle,fieldCount=numVars,rc=status)
     _VERIFY(status)
     allocate(names(numVars),stat=status)
     _VERIFY(status)
     call ESMF_FieldBundleGet(this%output_bundle,fieldNameList=names,rc=status)
     _VERIFY(status)
     iter = this%items%begin()
     do while(iter /= this%items%end())
        item => iter%get()
        if (item%itemType == ItemTypeScalar) then
           call this%swap_undef_value(trim(item%xname),_RC)
           call this%regridScalar(trim(item%xname),rc=status)
           _VERIFY(status)
        else if (item%itemType == ItemTypeVector) then
           call this%swap_undef_value(trim(item%xname),_RC)
           call this%swap_undef_value(trim(item%yname),_RC)
           call this%regridVector(trim(item%xname),trim(item%yname),rc=status)
           _VERIFY(status)
        end if
        call iter%next()
     enddo

     do i=1,numVars
        call ESMF_FieldBundleGet(this%input_bundle,trim(names(i)),field=field,rc=status)
        _VERIFY(status)
        call ESMF_FieldDestroy(field,noGarbage=.true., rc=status)
        _VERIFY(status)
     enddo
     call ESMF_FieldBundleDestroy(this%input_bundle,noGarbage=.true.,rc=status)
     _VERIFY(status)
     _RETURN(_SUCCESS)

  end subroutine process_data_from_file

  subroutine swap_undef_value(this,fname,rc)
     class (sampler), intent(inout) :: this
     character(len=*), intent(in) :: fname
     integer, optional, intent(out) :: rc

     integer :: status

     type(ESMF_Field) :: field
     integer :: fieldRank
     real, pointer :: ptr3d(:,:,:)
     real, pointer :: ptr2d(:,:)
     type(ESMF_Grid) :: gridIn
     logical :: hasDE_in
     real(REAL32) :: fill_value

     if ( .not. this%current_file_metadata%var_has_missing_value(fname) ) then
        _RETURN(_SUCCESS)
     endif

     fill_value = this%current_file_metadata%var_get_missing_value(fname,_RC)

     call ESMF_FieldBundleGet(this%input_bundle,fname,field=field,_RC)
     call ESMF_FieldBundleGet(this%input_bundle,grid=gridIn,_RC)
     call ESMF_FieldGet(field,rank=fieldRank,_RC)
     hasDE_in = MAPL_GridHasDE(gridIn,_RC)

     if (fieldRank==2) then
        if (hasDE_in) then
           call MAPL_FieldGetPointer(field,ptr2d,_RC)
        else
           allocate(ptr2d(0,0))
        end if

        if (isnan(fill_value)) then
           where(isnan(ptr2d)) ptr2d=MAPL_UNDEF
        else
           where(ptr2d==fill_value) ptr2d=MAPL_UNDEF
        endif

     else if (fieldRank==3) then
        if (hasDE_in) then
           call ESMF_FieldGet(field,farrayPtr=ptr3d,_RC)
        else
           allocate(ptr3d(0,0,0))
        end if

        if (isnan(fill_value)) then
           where(isnan(ptr3d)) ptr3d=MAPL_UNDEF
        else
           where(ptr3d==fill_value) ptr3d=MAPL_UNDEF
        endif

     else
        _FAIL('rank not supported')
     end if

     _RETURN(_SUCCESS)

  end subroutine swap_undef_value

  
  subroutine addVariable_to_acc_bundle(this,itemName,rc)
    class (sampler), intent(inout) :: this
    character(len=*), intent(in) :: itemName
    integer, optional, intent(out) :: rc

    type(ESMF_Field) :: field,newField
    type(ESMF_Array) :: array1
    real(KIND=ESMF_KIND_R4), pointer :: pt2d(:,:)
    class (AbstractGridFactory), pointer :: factory
    integer :: fieldRank
    logical :: isPresent
    integer :: status

    call ESMF_FieldBundleGet(this%input_bundle,itemName,field=field,_RC)
    call ESMF_FieldGet(field,rank=fieldRank,rc=status)
    if (this%doVertRegrid .and. (fieldRank ==3) ) then
       newField = MAPL_FieldCreate(field,this%output_grid,lm=this%vData%lm,_RC)
    else
       newField = MAPL_FieldCreate(field,this%output_grid,_RC)
    end if
    
    call ESMF_FieldGet(newField, Array=array1, _RC)
    call ESMF_ArrayGet(array1, farrayptr=pt2d, _RC)
    pt2d=55.d0
    call MAPL_FieldBundleAdd(this%acc_bundle,newField,_RC)

    _RETURN(_SUCCESS)
  end subroutine addVariable_to_acc_bundle


  subroutine addVariable_to_output_bundle(this,itemName,rc)
    class (sampler), intent(inout) :: this
    character(len=*), intent(in) :: itemName
    integer, optional, intent(out) :: rc

    type(ESMF_Field) :: field,newField
    class (AbstractGridFactory), pointer :: factory
    integer :: fieldRank
    logical :: isPresent
    integer :: status

    call ESMF_FieldBundleGet(this%input_bundle,itemName,field=field,_RC)
    call ESMF_FieldGet(field,rank=fieldRank,rc=status)
    if (this%doVertRegrid .and. (fieldRank ==3) ) then
       newField = MAPL_FieldCreate(field,this%output_grid,lm=this%vData%lm,_RC)
    else
       newField = MAPL_FieldCreate(field,this%output_grid,_RC)
    end if
    call MAPL_FieldBundleAdd(this%output_bundle,newField,_RC)

    _RETURN(_SUCCESS)
  end subroutine addVariable_to_output_bundle
  
        

  !! -- based on subroutine bundlepost(this,filename,oClients,rc)
  !  subroutine interp_accumulate_fields (this,xy_subset,xy_mask,rc)
  subroutine interp_accumulate_fields (this,xy_subset,rc)
    implicit none
    class (sampler) :: this
    integer, intent(in) :: xy_subset(2,2)
    !!integer, intent(in) :: xy_mask(:,:)
    integer, optional, intent(out) :: rc

    integer :: status
    type(ESMF_Field) :: outField
    type(ESMF_Field) :: new_outField
    type(ESMF_Grid)  :: grid
    integer :: tindex
    type(ArrayReference) :: ref

    type(GriddedIOitemVectorIterator) :: iter
    type(GriddedIOitem), pointer :: item
    logical :: have_time


    type(ESMF_Array) :: array1, array2
    integer :: is,ie,js,je

    integer :: rank, rank1, rank2
    real(KIND=ESMF_KIND_R4), pointer :: pt2d(:,:), pt2d_(:,:)
    real(KIND=ESMF_KIND_R4), pointer :: pt3d(:,:,:), pt3d_(:,:,:)

    integer :: localDe, localDECount
    integer, dimension(:), allocatable :: LB, UB, exclusiveCount
    integer, dimension(:), allocatable :: compLB, compUB, compCount    
    integer :: dimCount
    integer :: y1, y2
    integer :: j, jj
    integer :: ii1, iin, jj1, jjn
    integer, dimension(:), allocatable :: j1, j2

    is=xy_subset(1,1); ie=xy_subset(2,1)
    js=xy_subset(1,2); je=xy_subset(2,2)

    if (this%vdata%regrid_type==VERTICAL_METHOD_ETA2LEV) then
       call this%vdata%setup_eta_to_pressure(regrid_handle=this%regrid_handle,output_grid=this%output_grid,rc=status)
       _VERIFY(status)
    end if
    
    call ESMF_FieldBundleGet(this%output_bundle, grid=grid, _RC)
    call ESMF_GridGet(grid, localDECount=localDECount, dimCount=dimCount, _RC)
    allocate ( LB(dimCount), UB(dimCount), exclusiveCount(dimCount) )
    allocate ( compLB(dimCount), compUB(dimCount), compCount(dimCount) )    
    
    allocate ( j1(0:localDEcount-1) )  ! start
    allocate ( j2(0:localDEcount-1) )  ! end

    _ASSERT ( localDEcount == 1, 'failed, due to localDEcount > 1')
    call MAPL_GridGetInterior(grid,ii1,iin,jj1,jjn)
    write(6,*) 'MAPL_GridGetInterior, ii1,iin,jj1,jjn', ii1,iin,jj1,jjn

    LB(1)=ii1; LB(2)=jj1
    UB(1)=iin; UB(2)=jjn
    
!    do localDe=0, localDEcount-1
    do localDe=0, 0    
       !call ESMF_GridGet (grid, ESMF_STAGGERLOC_CENTER, localDE, &
       !     exclusiveLBound=LB, exclusiveUBound=UB, exclusiveCount=exclusiveCount,&
       !     computationalLBound=compLB, computationalUBound=compUB,&
       !     computationalCount=compCount,_RC)
       !write(6,*) 'exclusiveLBound, exclusiveUBound, exclusiveCount', &
       !     LB, UB, exclusiveCount
       !write(6,*) 'computationalLBound, computationalUBound, computationalCount', &
       !     compLB, compUB, compCount
       

       ! is/ie, js/je,  [LB, UB]
       !
       !
       y1=jj1; y2=jjn
       if (y1 < js) then
          if (y2 < js) then
             j1(localDe)=-1
             j2(localDe)=-1
          elseif (y2 < je) then
             j1(localDe)=js
             j2(localDe)=y2
          else
             j1(localDe)=js
             j2(localDe)=je
          endif
       elseif (y1 <= je) then
          j1(localDe)=y1
          if (y2 < je) then
             j2(localDe)=y2
          else
             j2(localDe)=je
          endif
       else
          j1(localDe)=-1
          j2(localDe)=-1
       endif
    enddo

    write(6,*) 'ck bundlepost_acc'
    write(6,*) 'j1(localDe)', j1(0:localDeCount-1)
    write(6,*) 'j2(localDe)', j2(0:localDeCount-1)


    
    iter = this%items%begin()
    do while (iter /= this%items%end())
       item => iter%get()
       if (item%itemType == ItemTypeScalar) then

          write(6,*) 'ck bundlepost_acc, item%xname ', item%xname

          call this%RegridScalar(item%xname,rc=status)
          _VERIFY(status)
          call ESMF_FieldBundleGet(this%output_bundle,item%xname,field=outField, _RC)
          _VERIFY(status)
          if (this%vdata%regrid_type==VERTICAL_METHOD_ETA2LEV) then
             call this%vdata%correct_topo(outField,rc=status)
             _VERIFY(status)
          end if

          ! -- mask the time interval
          !    store the time interval fields into new bundle
          call ESMF_FieldGet(outField, Array=array1, _RC)
          call ESMF_FieldBundleGet(this%acc_bundle,item%xname,field=new_outField,_RC)
          call ESMF_FieldGet(new_outField, Array=array2, _RC)
          call ESMF_ArrayGet(array1, rank=rank, _RC)
          if (rank==2) then
             call ESMF_ArrayGet(array1, farrayptr=pt2d, _RC)
             write(6,*) 'shape(pt2d)', shape(pt2d)
             write(6,*) 'in_pt2d', pt2d(10,1:100:20)
             
             call ESMF_ArrayGet(array2, farrayptr=pt2d_, _RC)
             do localDe=0, localDEcount-1
                if (j1(localDe)>0) then
                   do j= j1(localDe), j2(localDe)
                      jj= j-jj1+1     ! j_local
!!                      write(6,*) 'j, jj', j, jj
                      pt2d_(:,jj) = pt2d(:,jj)
                   enddo
                endif
             enddo
             write(6,*) 'out_pt2d', pt2d_(10,1:10:2)

          elseif (rank==3) then
             call ESMF_ArrayGet(array1, farrayptr=pt3d, _RC)
             write(6,*) 'shape(pt3d)', shape(pt3d)
             call ESMF_ArrayGet(array2, farrayptr=pt3d_, _RC)
             do localDe=0, localDEcount-1
                if (j1(localDe)>0) then
                   do j= j1(localDe), j2(localDe)
                      jj= j-jj1+1
                      pt3d_(:,jj,:) = pt3d(:,jj,:)
                   enddo
                endif
             enddo
          else
             stop 'failed GriddedIO.F90'
          endif

       else if (item%itemType == ItemTypeVector) then
          _FAIL('ItemTypeVector not implemented')
       end if
       call iter%next()
    enddo

    _RETURN(ESMF_SUCCESS)

  end subroutine interp_accumulate_fields


  
  subroutine write_2_oserver(this,sp,filename,oClients,rc)
    class (samplerHQ), intent(inout) :: this
    class (sampler), intent(inout) :: sp
    character(len=*), intent(in) :: filename
    type (ClientManager), optional, intent(inout) :: oClients
    integer, optional, intent(out) :: rc

    integer :: status
    type(ESMF_Field) :: outField
    integer :: tindex
    type(ArrayReference) :: ref

    type(GriddedIOitemVectorIterator) :: iter
    type(GriddedIOitem), pointer :: item
    logical :: have_time


    if ( .NOT. ESMF_AlarmIsRinging(this%alarm) ) then
       rc=0
       return
    endif

    write(6,*) 'ck: bf ESMF_AlarmIsRinging(this%alarm)  write_2_oserver '
    have_time = sp%timeInfo%am_i_initialized()

    if (have_time) then
       sp%times = sp%timeInfo%compute_time_vector(sp%metadata,rc=status)
       _VERIFY(status)
       ref = ArrayReference(sp%times)
       call oClients%stage_nondistributed_data(sp%write_collection_id,trim(filename),'time',ref)

       tindex = size(sp%times)
       if (tindex==1) then
          call sp%stage2DLatLon(filename,oClients=oClients,_RC)
       end if
    else
       tindex = -1
    end if

    if (sp%vdata%regrid_type==VERTICAL_METHOD_ETA2LEV) then
       call sp%vdata%setup_eta_to_pressure(regrid_handle=sp%regrid_handle,output_grid=sp%output_grid,rc=status)
       _VERIFY(status)
    end if

    iter = sp%items%begin()
    do while (iter /= sp%items%end())
       item => iter%get()
       if (item%itemType == ItemTypeScalar) then
          call ESMF_FieldBundleGet(sp%acc_bundle,item%xname,field=outField,rc=status)
          _VERIFY(status)
          if (sp%vdata%regrid_type==VERTICAL_METHOD_ETA2LEV) then
             call sp%vdata%correct_topo(outField,rc=status)
             _VERIFY(status)
          end if
          call sp%stageData(outField,filename,tIndex, oClients=oClients,rc=status)
          _VERIFY(status)
       else if (item%itemType == ItemTypeVector) then
          call ESMF_FieldBundleGet(sp%acc_bundle,item%xname,field=outField,rc=status)
          _VERIFY(status)
          if (sp%vdata%regrid_type==VERTICAL_METHOD_ETA2LEV) then
             call sp%vdata%correct_topo(outField,rc=status)
             _VERIFY(status)
          end if
          call sp%stageData(outField,filename,tIndex,oClients=oClients,rc=status)
          _VERIFY(status)
          call ESMF_FieldBundleGet(sp%acc_bundle,item%yname,field=outField,rc=status)
          _VERIFY(status)
          if (sp%vdata%regrid_type==VERTICAL_METHOD_ETA2LEV) then
             call sp%vdata%correct_topo(outField,rc=status)
             _VERIFY(status)
          end if
          call sp%stageData(outField,filename,tIndex,oClients=oClients,rc=status)
          _VERIFY(status)
       end if
       call iter%next()
    enddo

    write(6,*) 'ck: af ESMF_AlarmIsRinging(this%alarm)  write_2_oserver '
    
    _RETURN(ESMF_SUCCESS)

  end subroutine write_2_oserver


!! -- todo:  delete
!!
!!  subroutine regenerate_routehandle (this, rc)
!!    type(sampler) :: this
!!    type(ESMF_RouteHandle) :: route_handle
!!    integer, intent(out), optional     :: rc
!!    integer :: status
!!    
!!    ! -- destroy regrid handle
!!!???
!!!    route_handle = this%regrid_handle
!!!    call ESMF_RouteHandleDestroy(route_handle, noGarbage=.true.)
!!    
!!    write(6,*)
!!    _RETURN(_SUCCESS)
!!  end subroutine regenerate_routehandle
!!
  
  subroutine get_xy_mask(grid, xy_subset, xy_mask, rc)
    implicit none
    type(ESMF_Grid), intent(in) :: grid
    integer, intent(in)  :: xy_subset(2,2)
    integer, intent(out) :: xy_mask(:,:)
    integer, optional, intent(out) :: rc

    integer :: status
    integer :: ii1, iin, jj1, jjn  ! local box for localDE
    integer :: is, ie, js, je  ! global box for each time-interval
    integer :: j1p, jnp        ! local y-index for each time-interval


    integer :: localDe, localDECount
    integer, dimension(:), allocatable :: LB, UB, exclusiveCount, clbnd, cubnd
    integer :: dimCount
    integer :: y1, y2
    integer :: j, jj
    !    integer, dimension(:), allocatable :: j1, j2
    integer :: j1, j2

    is=xy_subset(1,1); ie=xy_subset(2,1)
    js=xy_subset(1,2); je=xy_subset(2,2)


    !      call ESMF_GridGet(grid, localDECount=localDECount, dimCount=dimCount, _RC)
    !      allocate ( LB(dimCount), UB(dimCount), exclusiveCount(dimCount) )
    !      allocate ( j1(0:localDEcount-1) )  ! start
    !      allocate ( j2(0:localDEcount-1) )  ! end
    !      write(6,*) 'localDECount, dimCount', localDECount, dimCount
    !      localDe=0
    !      do localDe=0, localDEcount-1
    !         call ESMF_GridGet (grid, ESMF_STAGGERLOC_CENTER, localDE, &
    !              exclusiveLBound=LB, exclusiveUBound=UB, &
    !              computationalLBound=clbnd, computationalUBound=cubnd,  _RC)
    !         write(6,*) 'exclusiveLBound, exclusiveUBound, clbnd, cubnd', &
    !              LB, UB, clbnd, cubnd
    !      enddo
    !
    !!         call ESMF_GridGet (grid, ESMF_STAGGERLOC_CENTER, localDE, &
    !!              exclusiveLBound=LB, exclusiveUBound=UB, exclusiveCount=exclusiveCount, &
    !!              computationalLBound=clbnd, computationalUBound=cubnd,  _RC)      

    
    call MAPL_GridGetInterior(grid,ii1,iin,jj1,jjn)
    write(6,*) 'MAPL_GridGetInterior, ii1,iin,jj1,jjn', ii1,iin,jj1,jjn

    y1=jj1; y2=jjn
    if (y1 < js) then
       if (y2 < js) then
          j1=-1
          j2=-1
       elseif (y2 < je) then
          j1=js
          j2=y2
       else
          j1=js
          j2=je
       endif
    elseif (y1 <= je) then
       j1=y1
       if (y2 < je) then
          j2=y2
       else
          j2=je
       endif
    else
       j1=-1
       j2=-1
    endif

    write(6,*) 'get_xy_mask: j1,j2=', j1, j2
    xy_mask(:,:) = 0
    if (j1 > 0) then
       do jj = j1, j2
          xy_mask(:, jj) = 1
       enddo
    end if

    if(present(rc)) rc=0

  end subroutine get_xy_mask

  
end module MAPL_EpochSwathMod
 
