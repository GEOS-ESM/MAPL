!
!__  meta code
!    1.  samplerHQ deals with
!        clock, alarm, make_grid
!    2.  sampler  deals with
!        time depdent epoch, extract xy_subset, save_copy_subset
!        create metadata
!        create output_bundle,  acc_bundle
!

#include "MAPL_Generic.h"
module MAPL_EpochSwathMod
  use ESMF
  use MAPL_ErrorHandlingMod
  use netcdf
!  use MAPL_CommsMod
!  use mapl_MaplGrid, only : MAPL_GridGet
!  use MAPL_Base, only : MAPL_GetHorzIJIndex, MAPL_GetGlobalHorzIJIndex
!  use pFIO_NetCDF4_FileFormatterMod
!  use MAPL_GridManagerMod   
!  use MAPL_GriddedIOMod
!  use mapl_RegridMethods
!  use MAPL_GriddedIOitemVectorMod
!  use MAPL_VerticalDataMod

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
  use MAPL_GriddedIOMod
  use MAPL_ExceptionHandling
  use pFIO_ClientManagerMod
  use MAPL_DataCollectionMod
  use MAPL_DataCollectionManagerMod
  use gFTL_StringVector
  use gFTL_StringStringMap
  use MAPL_FileMetadataUtilsMod
  use MAPL_DownbitMod
  use, intrinsic :: ISO_C_BINDING
  use, intrinsic :: iso_fortran_env, only: REAL64
  use ieee_arithmetic, only: isnan => ieee_is_nan
  use  MAPL_SwathGridFactoryMod
  use MAPL_plain_netCDF_Time
  
  implicit none
!!  private

  public

  !__ clct_sampler for each collection
  !
  type :: sampler
     class(*), pointer     :: ptr => null()
     type(MAPL_GriddedIO)  :: io_only
     integer :: regrid_method = REGRID_METHOD_BILINEAR
     integer :: nbits_to_keep = MAPL_NBITS_NOT_SET

     class (AbstractRegridder), pointer :: regrid_handle => null()
     type(ESMF_FieldBundle)  :: input_bundle
     type(ESMF_FieldBundle)  :: output_bundle     
     type(ESMF_FieldBundle)  :: acc_bundle
     type(ESMF_grid) :: input_grid
     type(ESMF_grid) :: output_grid     
     type(GriddedIOitemVector) :: items
     type(VerticalData) :: vdata
     type(FileMetaData)      :: metadata
     integer, allocatable :: chunking(:)
     logical :: doVertRegrid = .false.
     integer :: fraction = 0

     integer :: write_collection_id = 11
     real, allocatable :: lons(:,:),lats(:,:)
     real, allocatable :: corner_lons(:,:),corner_lats(:,:)


   contains
     procedure :: set_default_chunking
     procedure :: check_chunking
!     procedure :: stageData
!     procedure :: stage2DLatLon
     procedure :: regridScalar
     procedure :: regridVector
     procedure :: createFilemetadata
     procedure :: createVariable
     procedure :: interp_accumulate_fields
  end type sampler



!  type :: epoch
!     type(ESMF_Time)         :: currTime
!     type(ESMF_TimeInterval) :: timeStep
!     integer                 :: xy_subset(2,2)
!     type(ESMF_FieldBundle)  :: acc_bundle
!     character(len=ESMF_MAXSTR) :: grid_type
!     type(ESMF_grid) :: ogrid     
!     type (ESMF_Config) :: config_grid
!  end type epoch


  type :: samplerHQ   ! generic_sample
     !!type(time_n_alarm)    :: time_n_alarm_for_epoch
     type(ESMF_Clock)         :: clock
     type(ESMF_Alarm)         :: alarm
     type(ESMF_Time)          :: RingTime
     type(ESMF_TimeInterval)  :: Frequency_epoch
     type(ESMF_config) :: config_grid_save
     type(ESMF_grid) :: ogrid
     character(len=ESMF_MAXSTR) :: grid_type

   contains

     ! generic ::  init => init_epoch
     !     procedure ::  destroy_n_regenerate_epoch
     
     procedure ::  create_grid
     procedure ::  create_sampler => create_epoch_sampler
     procedure ::  regrid_accumulate => regrid_accumulate_on_xysubset
     procedure ::  write_2_oserver

!     procedure ::  destroy_ogrid
!     procedure ::  gen_regrid_RH

!    
  end type samplerHQ   ! generic_sampler


  interface samplerHQ
     module procedure new_samplerHQ
  end interface samplerHQ

!  interface create_sampler
!     module procedure create_epoch_sampler
!  end interface create_sampler
     

contains


  !--------------------------------------------------!
  ! __ set HQ: headquarter
  !    - save clock, config_grid_save
  !    - samlper alarm
  !
  !--------------------------------------------------!
  
  !!  function new_sampler(ext_currTime, timestep, config) result(mysampler)
  function new_samplerHQ(clock, config, key, rc) result(hq)  
    implicit none
    type(samplerHQ) :: hq
    type(ESMF_Clock),  intent(in) :: clock
    type(ESMF_Config), intent(inout) :: config
    character(len=100), intent(in) :: key
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

    call ESMF_ClockGet ( clock, CurrTime=currTime, _RC )
    call ESMF_ClockGet ( clock, timestep=timestep, _RC )
    call ESMF_ClockGet ( clock, startTime=startTime, _RC )

    !__ note a bug in  ESMF_ConfigGetAttribute
    !     call fails with config = intent(in)
    !
    call ESMF_ConfigGetAttribute(config, value=time_integer, label=trim(key)//'Epoch:', default=0, _RC)
!!    call ESMF_ConfigGetAttribute( cf, value=n1 ,label='one_number:',default=1, rc=rc)
!!    call ESMF_ConfigGetAttribute ( config, value=time_integer, label ='AvoidRootNodeThreshold:', default=1, rc=rc)    

    _ASSERT(time_integer /= 0, 'Epoch value in config wrong')
    call hms_2_s (time_integer, second, _RC)
    call ESMF_TimeIntervalSet(frequency_epoch, s=second, _RC)
    hq%frequency_epoch = frequency_epoch
    hq%RingTime  = currTime
    hq%alarm = ESMF_AlarmCreate( clock=clock, RingInterval=Frequency_epoch, &
         RingTime=hq%RingTime, sticky=.false., _RC )

    _RETURN(_SUCCESS)
    
  end function new_samplerHQ



  !--------------------------------------------------!
  ! __ set
  !    - ogrid via grid_manager%make_grid 
  !      using currTime and HQ%config_grid_save
  !
  !--------------------------------------------------!
  function create_grid(this, grid_type, key, currTime, rc)  result(ogrid)  
    class(samplerHQ)  :: this
    character(len=*), intent(in) :: grid_type
    character(len=*), intent(in) :: key
    type(ESMF_Time), intent(inout) :: currTime
    integer, intent(out), optional :: rc
    type (ESMF_Grid)  :: ogrid

    character(len=ESMF_MAXSTR) :: time_string
    integer :: status
    type(ESMF_Config) :: config_grid

    this%grid_type = trim(grid_type)
    config_grid = this%config_grid_save
    call ESMF_TimeSet(currTime, time_string, _RC)
    call ESMF_ConfigSetAttribute( config_grid, time_string, label=trim(key)//'.Epoch_init:', _RC)
    ogrid = grid_manager%make_grid(config_grid, prefix=key//'.', _RC )
    this%ogrid = ogrid
  end function create_grid



  !--------------------------------------------------!
  ! __ set
  !    - output_bundle, acc_bundle
  !    - regrid RH
  !
  !--------------------------------------------------!
  function create_epoch_sampler (this, regrid_method, input_bundle, items, vdata, rc) result(sp)
    class(samplerHQ)  :: this
    integer, intent(in) :: regrid_method
    type(ESMF_FieldBundle), intent(in) :: input_bundle
    type(sampler) :: sp
    type(GriddedIOitemVector), intent(in) :: items
    type(VerticalData), intent(in), optional :: vdata
    integer, intent(out), optional :: rc

    integer :: status
    type(ESMF_grid) :: input_grid

    sp%input_bundle = input_bundle
    sp%regrid_method = regrid_method
    sp%items = items    

    if (present(vdata)) then
       sp%vdata=vdata
    else
       sp%vdata=VerticalData(_RC)
    end if
    
    call ESMF_FieldBundleGet(input_bundle,grid=input_grid,rc=status)
    sp%input_grid = input_grid
    sp%output_grid = this%ogrid
    sp%output_bundle = ESMF_FieldBundleCreate(_RC)
    call ESMF_FieldBundleSet(sp%output_bundle,grid=sp%output_grid, _RC)
    sp%acc_bundle = ESMF_FieldBundleCreate(_RC)
    call ESMF_FieldBundleSet(sp%acc_bundle,grid=sp%output_grid, _RC)

    sp%regrid_handle => new_regridder_manager%make_regridder(sp%input_grid,sp%output_grid,regrid_method,_RC)
    
  end function create_epoch_sampler
    

!  
!!  subroutine gen_regrid_RH (this, bundle_in, rc)
!!    type(sampler), intent(inout)  :: this
!!    type (ESMF_Bundle), intent(in)  :: bundle_in
!!    type (ESMF_Bundle), intent(out) :: bundle_out
!!
!!    character(len=:), intent(in) :: grid_type
!!    character(len=:), intent(in) :: key
!!    type(ESMF_Time), intent(in) :: currTime
!!    integer, intent(out), optional :: rc
!!
!!    character(len=ESMF_MAXSTR) :: time_string
!!    integer :: status
!!    class (AbstractRegridder), pointer, intent(out) :: regrid_handle    
!!
!!  end subroutine gen_regrid_RH
!

  !!regrid_accumulate_on_xysubset
  subroutine regrid_accumulate_on_xysubset (this, sp, rc)
    !bundle_in, bundle_out, RH, rc)
    class(samplerHQ) :: this
    class(sampler), intent(inout)  :: sp
    integer, intent(out), optional :: rc
    
    class(AbstractGridFactory), pointer :: factory    
    integer                        :: xy_subset(2,2)
    type(ESMF_Time)                :: timeset(2)
    type(ESMF_Time) :: current_time
    type(ESMF_TimeInterval) :: dur

!    type (ESMF_FieldBundle), intent(in)  :: bundle_in
!    type (ESMF_FieldBundle), intent(out) :: bundle_out

    character(len=ESMF_MAXSTR) :: time_string
    integer :: status

    
    ! __ s1.  get xy_subset

    factory => grid_manager%get_factory(this%ogrid,_RC)
    call ESMF_ClockGet(this%clock,currTime=current_time,_RC)
    call ESMF_ClockGet(this%clock,timeStep=dur, _RC )
    timeset(1) = current_time - dur
    timeset(2) = current_time
    call factory%get_subset( timeset, xy_subset, _RC)
    write(6,*) 'xy_subset(:,1)_x', xy_subset(:,1)    ! LB
    write(6,*) 'xy_subset(:,2)_a', xy_subset(:,2), xy_subset(2,2)-xy_subset(1,2)+1  ! UB

    ! __ s2.  interpolate then save data on xy_subset

    call sp%interp_accumulate_fields (xy_subset, _RC)

  end subroutine regrid_accumulate_on_xysubset


  subroutine write_2_oserver (this, sp, rc)
    class(samplerHQ) :: this
    class(sampler), intent(inout)  :: sp
    integer, intent(out), optional :: rc
    
    type(ESMF_Time)                :: timeset(2)
    type(ESMF_Time) :: current_time
    type(ESMF_TimeInterval) :: dur

!    type (ESMF_FieldBundle), intent(in)  :: bundle_in
!    type (ESMF_FieldBundle), intent(out) :: bundle_out

    character(len=ESMF_MAXSTR) :: time_string
    integer :: status


    if (.NOT. ESMF_AlarmIsRinging (sp%alarm)) then
       rc=0
       return
    endif


    

    
  end subroutine write_2_oserver

  


  !-----------------------------------------------------
  ! __ given [is,ie,js,je] + output_bundle / output_grid
  !    do
  !      - regrid and get output_field
  !      - copy xy_subset from
  !            out_field in output_bundle  into
  !            new_out_field in new_bundle
  !-----------------------------------------------------  
  subroutine interp_accumulate_fields (this,xy_subset,rc)
    class (sampler) :: this
    integer, intent(in) :: xy_subset(2,2)
    integer, optional, intent(out) :: rc

    type(ESMF_FieldBundle) :: new_output_bundle
    integer :: status
    type(ESMF_Field) :: outField
    type(ESMF_Field) :: new_outField
    type(ESMF_Grid)  :: grid
    integer :: tindex
    type(ArrayReference) :: ref
    type(ESMF_Array) :: array1, array2

    type(GriddedIOitemVectorIterator) :: iter
    type(GriddedIOitem), pointer :: item
    integer :: is,ie,js,je

    integer :: rank, rank1, rank2
    real(KIND=ESMF_KIND_R4), pointer :: pt2d(:,:), pt2d_(:,:)
    real(KIND=ESMF_KIND_R4), pointer :: pt3d(:,:,:), pt3d_(:,:,:)


    integer :: localDe, localDECount
    integer, dimension(:), allocatable :: LB, UB, exclusiveCount
    integer :: dimCount
    integer :: y1, y2
    integer :: j, jj
    integer, dimension(:), allocatable :: j1, j2


    is=xy_subset(1,1); ie=xy_subset(2,1)
    js=xy_subset(1,2); je=xy_subset(2,2)        

    call ESMF_FieldBundleGet(this%output_bundle, grid=grid, _RC)
    if (this%vdata%regrid_type==VERTICAL_METHOD_ETA2LEV) then
       call this%vdata%setup_eta_to_pressure(regrid_handle=this%regrid_handle,output_grid=grid, _RC)
    end if    
    call ESMF_GridGet(grid, localDECount=localDECount, dimCount=dimCount, _RC)
    allocate ( LB(dimCount), UB(dimCount), exclusiveCount(dimCount) )
    allocate ( j1(0:localDEcount-1) )  ! start
    allocate ( j2(0:localDEcount-1) )  ! end

    do localDe=0, localDEcount-1
       call ESMF_GridGet (grid, ESMF_STAGGERLOC_CENTER, localDE, &
            exclusiveLBound=LB, exclusiveUBound=UB, exclusiveCount=exclusiveCount, _RC)
       write(6,*) 'exclusiveLBound, exclusiveUBound, exclusiveCount', &
            LB, UB, exclusiveCount
       ! is/ie, js/je,  [LB, UB]
       y1=LB(2); y2=UB(2)
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
          call ESMF_FieldBundleGet(new_output_bundle,item%xname,field=new_outField,_RC)
          call ESMF_FieldGet(new_outField, Array=array2, _RC)              
          call ESMF_ArrayGet(array1, rank=rank, _RC)
          if (rank==2) then
             call ESMF_ArrayGet(array1, farrayptr=pt2d, _RC)
             write(6,*) 'shape(pt2d)', shape(pt2d)
             call ESMF_ArrayGet(array2, farrayptr=pt2d_, _RC)
             do localDe=0, localDEcount-1
                if (j1(localDe)>0) then
                   do j= j1(localDe), j2(localDe)
                      jj= j-LB(2)     ! j_local
                      pt2d_(:,jj) = pt2d(:,jj)
                   enddo
                endif
             enddo
          elseif (rank==3) then
             call ESMF_ArrayGet(array1, farrayptr=pt3d, _RC)
             write(6,*) 'shape(pt3d)', shape(pt3d)
             call ESMF_ArrayGet(array2, farrayptr=pt3d_, _RC)
             do localDe=0, localDEcount-1
                if (j1(localDe)>0) then
                   do j= j1(localDe), j2(localDe)
                      jj= j-LB(2)
                      pt3d_(:,jj,:) = pt3d(:,jj,:)
                   enddo
                endif
             enddo
          else
             _ASSERT(.false., 'rank > 3 fails')
             stop 'failed'
          endif

       else if (item%itemType == ItemTypeVector) then
          _ASSERT(.false., 'item%itemType == ItemTypeVector not implemented')
          call this%RegridVector(item%xname,item%yname,rc=status)
          _VERIFY(status)

          call ESMF_FieldBundleGet(this%output_bundle,item%xname,field=outField,rc=status)
          _VERIFY(status)
          if (this%vdata%regrid_type==VERTICAL_METHOD_ETA2LEV) then
             call this%vdata%correct_topo(outField,rc=status)
             _VERIFY(status)
          end if

       end if
       call iter%next()
    enddo

    this%acc_bundle = new_output_bundle
    
    _RETURN(ESMF_SUCCESS)

  end subroutine interp_accumulate_fields





  !
  !
  !--  adopted from griddedio.F90
  !
  !
  subroutine CreateFileMetaData(this,rc)
        class (sampler), intent(inout) :: this
          integer, intent(out), optional :: rc

        type(ESMF_Grid) :: input_grid
        class (AbstractGridFactory), pointer :: factory

        type(GriddedIOitemVectorIterator) :: iter
        type(GriddedIOitem), pointer :: item
        type(stringVector) :: order
        integer :: metadataVarsSize
        type(StringStringMapIterator) :: s_iter
        character(len=:), pointer :: attr_name, attr_val
        integer :: status

        
        factory => get_factory(this%output_grid,_RC)
        call factory%append_metadata(this%metadata)

        call this%vdata%append_vertical_metadata(this%metadata,this%input_bundle,rc=status)
        _VERIFY(status)
        this%doVertRegrid = (this%vdata%regrid_type /= VERTICAL_METHOD_NONE)
        if (this%vdata%regrid_type == VERTICAL_METHOD_ETA2LEV) call this%vdata%get_interpolating_variable(this%input_bundle,rc=status)
        _VERIFY(status)

        !!call this%timeInfo%add_time_to_metadata(this%metadata,rc=status)
        !!_VERIFY(status)

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
              call this%createVariable(item%xname,rc=status)
              _VERIFY(status)
           else if (item%itemType == ItemTypeVector) then
              call this%createVariable(item%xname,rc=status)
              _VERIFY(status)
              call this%createVariable(item%yname,rc=status)
              _VERIFY(status)
           end if
           call iter%next()
        enddo

!
!        ! Add newFields to new bundle
!        if (present(new_output_bundle)) then
!           write(6,*) 'ck inside if (present(new_output_bundle))'
!           iter = this%items%begin()
!           do while (iter /= this%items%end())
!              item => iter%get()
!              if (item%itemType == ItemTypeScalar) then
!                 call this%createVariable_newbundle(item%xname,new_output_bundle,_RC)
!              else if (item%itemType == ItemTypeVector) then
!                 call this%createVariable_newbundle(item%xname,new_output_bundle,_RC)
!                 call this%createVariable_newbundle(item%yname,new_output_bundle,_RC)
!              end if
!              call iter%next()
!           enddo
!        endif
           
        
        _RETURN(_SUCCESS)

     end subroutine CreateFileMetaData
     
     
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


     subroutine createVariable(this,itemName,rc)
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

!!        if (this%timeInfo%is_initialized) then
!!           if (fieldRank==2) then
!!              vdims=grid_dims//",time"
!!           else if (fieldRank==3) then
!!              vdims=grid_dims//",lev,time"
!!           else
!!              _FAIL( 'Unsupported field rank')
!!           end if
!!        else
           if (fieldRank==2) then
              vdims=grid_dims
           else if (fieldRank==3) then
              vdims=grid_dims//",lev"
           else
              _FAIL( 'Unsupported field rank')
           end if
!!        end if
        !!v = Variable(type=PFIO_REAL32,dimensions=vdims,chunksizes=this%chunking,deflation=this%deflateLevel,quantize_algorithm=this%quantizeAlgorithm,quantize_level=this%quantizeLevel)
        v = Variable(type=PFIO_REAL32,dimensions=vdims,chunksizes=this%chunking)
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

     end subroutine createVariable


     
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

     
     
end module MAPL_EpochSwathMod








  !    
  !
  !    subroutine init_epoch (this, epoch, ext_current_time)
  !    type (sampler), intent(inout) :: this
  !    type(ESMF_Time)             :: ext_currTime
  !    !
  !    type(epoch) :: epoch_new
  !    type (ESMF_Config) :: config_swath_new
  !
  !    character(len=ESMF_MAXSTR) :: time_string
  !    integer :: status
  !    this%ptr => epoch_new
  !  end subroutine init_epoch
  !
  !
  !  subroutine destroy_n_regenerate_epoch (this, ext_current_time)
  !    type (epoch), intent(inout) :: this
  !    type(ESMF_Time)             :: ext_currTime
  !    !
  !    type (ESMF_Config) :: config_swath_new
  !
  !
  !
  !   call ESMF_TimeGet  ( current_time, TimeString=string  ,_RC )
  !   write(6,*) 'ck string: current_time: ', trim(string)
  !
  !
  !   regen_swath_grid: do n=1,nlist
  !      if (epoching(n) .AND. trim(list(n)%output_grid_label)=='swathGrid') then
  !
  !         key_str=trim(list(n)%output_grid_label)
  !
  !!- need imlpement
  !!         call list(n)%mGriddedIO%write_oserver ( list(n)%acc_bundle )     ! missing time
  !!         call list(n)%mGriddedIO%delte_regridhandle (list(n)%bundle,_RC)
  !
  !         ! -- destroy, regenerate swath grid
  !         write(6,*) 'I am inside list(n)%epoching', epoching(n)
  !         pgrid => IntState%output_grids%at(trim(key_str))
  !!         call grid_manager%destroy(pgrid,_RC)
  !! backup
  !         grid_out = pgrid
  !         call grid_manager%destroy(grid_out,_RC)
  !
  !         call IntState%output_grids%insert(trim(key_str), output_grid)
  !         call ESMF_ConfigSetAttribute( config_swath, trim(string), label=trim(key_str)//'.Epoch_init:', _RC)
  !
  !         iter = IntState%output_grids%begin()
  !         do while (iter /= IntState%output_grids%end())
  !            key => iter%key()
  !            if (trim(key)==trim(key_str)) then
  !               output_grid = grid_manager%make_grid( config_swath, prefix=trim(key_str)//'.', _RC)
  !               call IntState%output_grids%set(key, output_grid)
  !            endif
  !            call iter%next()
  !         enddo
  !      endif
  !      write(6,*) 'end of regen_swath_grid'
  !   end do regen_swath_grid
  !
  !   write(6,*) 'end hist run'
  !
  !
