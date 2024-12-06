!
! __ Analogy to GriddedIO.F90 with a twist for Epoch Swath grid
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
  use Plain_netCDF_Time
  use, intrinsic :: ISO_C_BINDING
  use MAPL_CommsMod, only : MAPL_Am_I_Root
  use pflogger, only: Logger, logging
  implicit none
  private

  integer, parameter       :: ngrid_max = 10

  type, private :: K_V_CF
     character(len=ESMF_MAXSTR) :: key
     type(ESMF_config) :: cf
  end type K_V_CF

  type, public :: samplerHQ
     type(ESMF_Clock)         :: clock
     type(ESMF_Alarm)         :: alarm
     type(ESMF_Time)          :: RingTime
     type(ESMF_TimeInterval)  :: Frequency_epoch
     integer                  :: ngrid = 0
     character(len=ESMF_MAXSTR) :: grid_type
     character(len=ESMF_MAXSTR) :: tunit
     type (K_V_CF)            :: CF_loc(ngrid_max)
     real*8 :: arr(2)

   contains
     procedure :: create_grid
     procedure :: regrid_accumulate => regrid_accumulate_on_xysubset
     procedure :: destroy_rh_regen_ogrid
     procedure :: fill_time_in_bundle
     procedure :: find_config
     procedure :: config_accumulate
     procedure :: verify_epoch_equals_freq
  end type samplerHQ

  interface samplerHQ
     module procedure new_samplerHQ
  end interface samplerHQ

  type, public :: sampler
     type(FileMetaData), allocatable :: metadata
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
     integer :: zstandardLevel = 0
     integer, allocatable :: chunking(:)
     logical :: itemOrderAlphabetical = .true.
     integer :: fraction
     logical :: have_initalized
     integer :: epoch_sec
   contains
     procedure :: Create_bundle_RH
     procedure :: CreateVariable
     procedure :: regridScalar
     procedure :: regridVector
     procedure :: set_param
     procedure :: set_default_chunking
     procedure :: check_chunking
     procedure :: alphabatize_variables
     procedure :: addVariable_to_acc_bundle
     procedure :: interp_accumulate_fields
  end type sampler

  interface sampler
     module procedure new_sampler
  end interface sampler

contains

  !
  ! in MAPL_HistoryGridComp.F90,  Hsampler get its config and key
  ! from the first SwathGrid entry in HISTORY.rc
  ! thus
  ! there is only one frequency_epoch for all the SwathGrid usage
  !
  function new_samplerHQ(clock, key, config, rc) result(hq)
    implicit none
    type(samplerHQ) :: hq
    type(ESMF_Clock),  intent(in)    :: clock
    character(len=*),  intent(in)    :: key
    type(ESMF_Config), intent(inout) :: config
    integer, optional, intent(out)   :: rc

    integer :: status
    integer :: second
    integer :: time_integer
    type(ESMF_Time)          :: startTime
    type(ESMF_Time)          :: currTime
    type(ESMF_TimeInterval)  :: timeStep
    type(ESMF_TimeInterval)  :: Frequency_epoch


    hq%clock= clock
    hq%arr(1:2) = -2.d0
    call ESMF_ClockGet ( clock, CurrTime=currTime, _RC )
    call ESMF_ClockGet ( clock, timestep=timestep, _RC )
    call ESMF_ClockGet ( clock, startTime=startTime, _RC )
    call ESMF_ConfigGetAttribute(config, value=time_integer, label=trim(key)//'.Epoch:', default=0, _RC)
    call ESMF_ConfigGetAttribute(config, value=hq%tunit,     label=trim(key)//'.tunit:', default="", _RC)
    _ASSERT(time_integer /= 0, 'Epoch value in config wrong')
    second = hms_2_s (time_integer)
    call ESMF_TimeIntervalSet(frequency_epoch, s=second, _RC)
    hq%frequency_epoch = frequency_epoch
    hq%RingTime  = currTime
    hq%alarm = ESMF_AlarmCreate( clock=clock, RingInterval=Frequency_epoch, &
         RingTime=hq%RingTime, sticky=.false., _RC )

    _RETURN(_SUCCESS)

  end function new_samplerHQ


  function find_config (this, key, rc) result(cf)
    class(samplerHQ)  :: this
    character(len=*) , intent(in) :: key
    type(ESMF_Config) :: cf
    integer, intent(out), optional :: rc
    integer :: status
    integer :: i, j

    j=0
    do i=1, this%ngrid
       if ( trim(key) == trim(this%CF_loc(i)%key) ) then
          cf = this%CF_loc(i)%cf
          j=j+1
          exit
       end if
    end do

    _ASSERT( j>0 , trim(key)//' is not found in Hsampler CF_loc(:)')

    _RETURN(_SUCCESS)
  end function find_config


  subroutine config_accumulate (this, key, cf, rc)
    class(samplerHQ)  :: this
    type(ESMF_Config), intent(in) :: cf
    character(len=*) , intent(in) :: key
    integer, intent(out), optional :: rc
    integer :: status

    this%ngrid = this%ngrid + 1
    this%CF_loc(this%ngrid)%key = trim(key)
    this%CF_loc(this%ngrid)%cf = cf
    _RETURN(_SUCCESS)
  end subroutine config_accumulate


  subroutine verify_epoch_equals_freq (this, frequency_from_list, swath_grid_label, rc)
    class(samplerHQ)  :: this
    integer, intent(in) :: frequency_from_list
    character(len=*) , intent(in) :: swath_grid_label
    integer, intent(out), optional :: rc
    type(ESMF_Config) :: config_grid
    integer :: hq_epoch_sec
    integer :: freq_sec
    integer :: local_swath_epoch_sec
    integer :: time_integer
    logical :: con
    integer :: status
    type(Logger), pointer          :: lgr

    call ESMF_TimeIntervalGet(this%Frequency_epoch, s=hq_epoch_sec, _RC)
    freq_sec = MAPL_nsecf( frequency_from_list )
    config_grid = this%find_config( swath_grid_label )
    call ESMF_ConfigGetAttribute(config_grid, value=time_integer, &
         label=trim(swath_grid_label)//'.Epoch:', default=0, _RC)
    local_swath_epoch_sec = MAPL_nsecf( time_integer )

    lgr => logging%get_logger('HISTORY.sampler')
    con = (hq_epoch_sec == local_swath_epoch_sec) .AND. (hq_epoch_sec == freq_sec)

    if (.not. con) then
       call lgr%debug('%a %i', 'hq_epoch_sec', hq_epoch_sec)
       call lgr%debug('%a %i', 'local_swath_epoch_sec', local_swath_epoch_sec)
       call lgr%debug('%a %i', 'freq_sec', freq_sec)
    end if

    _ASSERT(con, 'Error in '//trim(swath_grid_label)//' related swath and list in History.rc: Epoch in all swath grids must be equal, and equal to list%freq')
    _RETURN(_SUCCESS)
  end subroutine verify_epoch_equals_freq


  !--------------------------------------------------!
  ! __ set
  !    - ogrid via grid_manager%make_grid
  !      using currTime and HQ%config_grid_save
  !--------------------------------------------------!
  function create_grid(this, key, currTime, grid_type, rc)  result(ogrid)
    type (ESMF_Grid) :: ogrid
    class(samplerHQ)  :: this
    character(len=*), intent(in) :: key
    type(ESMF_Time), intent(inout) :: currTime
    character(len=*), optional, intent(in) :: grid_type
    integer, intent(out), optional :: rc
    integer :: status

    type(ESMF_Config) :: config_grid
    character(len=ESMF_MAXSTR) :: time_string


    if (present(grid_type)) this%grid_type = trim(grid_type)
    config_grid = this%find_config(key)
    call ESMF_TimeGet(currTime, timeString=time_string, _RC)

    !
    ! -- the `ESMF_ConfigSetAttribute` shows a risk
    !    to overwrite the nextline in config
    !
    call ESMF_ConfigSetAttribute( config_grid, trim(time_string), label=trim(key)//'.Epoch_init:', _RC)

    ogrid = grid_manager%make_grid(config_grid, prefix=trim(key)//'.', _RC )
    !!    call grid_validate (ogrid,)

    _RETURN(_SUCCESS)

  end function create_grid


  subroutine regrid_accumulate_on_xysubset (this, sp, rc)
    class(samplerHQ) :: this
    class(sampler), intent(inout)  :: sp
    integer, intent(out), optional :: rc
    integer :: status

    class(AbstractGridFactory), pointer :: factory
    type(ESMF_Time)            :: timeset(2)
    type(ESMF_Time)            :: current_time
    type(ESMF_TimeInterval)    :: dur
    integer                    :: xy_subset(2,2)

    ! __ s1.  get xy_subset

    call ESMF_ClockGet(this%clock,currTime=current_time,_RC)
    call ESMF_ClockGet(this%clock,timeStep=dur, _RC )
    timeset(1) = current_time - dur
    timeset(2) = current_time

    factory => grid_manager%get_factory(sp%output_grid,_RC)
    call factory%get_xy_subset( timeset, xy_subset, _RC)

    ! __ s2.  interpolate then save data using xy_mask

    call sp%interp_accumulate_fields (xy_subset, _RC)

    _RETURN(ESMF_SUCCESS)

  end subroutine regrid_accumulate_on_xysubset


  subroutine destroy_rh_regen_ogrid (this, key_grid_label, output_grids, sp, rc)
    implicit none
    class(samplerHQ) :: this
    class(sampler) :: sp
    type (StringGridMap), target, intent(inout) :: output_grids
    character(len=*), intent(in)  :: key_grid_label
    integer, intent(out), optional :: rc
    integer :: status

    type(ESMF_Time) :: currTime
    type(ESMF_Grid), pointer :: pgrid
    type(ESMF_Grid) :: ogrid
    character(len=ESMF_MAXSTR) :: key_str
    type (StringGridMapIterator) :: iter
    character(len=:), pointer :: key

    integer :: i, numVars
    character(len=ESMF_MAXSTR), allocatable :: names(:)
    type(ESMF_Field) :: field

    if ( .NOT. ESMF_AlarmIsRinging(this%alarm) ) then
       _RETURN(ESMF_SUCCESS)
    endif


    !__ s1. destroy ogrid + RH, regen ogrid

    key_str = trim(key_grid_label)
    pgrid => output_grids%at(key_str)

    call grid_manager%destroy(pgrid,_RC)

    call ESMF_ClockGet (this%clock, CurrTime=currTime, _RC )
    iter = output_grids%begin()
    do while (iter /= output_grids%end())
       key => iter%key()
       if (trim(key)==trim(key_str)) then
          ogrid = this%create_grid (key_str, currTime, _RC)
          call output_grids%set(key, ogrid)
       endif
       call iter%next()
    enddo


    !__ s2. destroy RH
    call sp%regrid_handle%destroy(_RC)



    !__ s3. destroy acc_bundle / output_bundle

   call ESMF_FieldBundleGet(sp%acc_bundle,fieldCount=numVars,_RC)
   allocate(names(numVars),_STAT)
   call ESMF_FieldBundleGet(sp%acc_bundle,fieldNameList=names,_RC)
   do i=1,numVars
      call ESMF_FieldBundleGet(sp%acc_bundle,trim(names(i)),field=field,_RC)
      call ESMF_FieldDestroy(field,noGarbage=.true., _RC)
   enddo
   call ESMF_FieldBundleDestroy(sp%acc_bundle,noGarbage=.true.,_RC)
   deallocate(names,_STAT)

   call ESMF_FieldBundleGet(sp%output_bundle,fieldCount=numVars,_RC)
   allocate(names(numVars),_STAT)
   call ESMF_FieldBundleGet(sp%output_bundle,fieldNameList=names,_RC)
   do i=1,numVars
      call ESMF_FieldBundleGet(sp%output_bundle,trim(names(i)),field=field,_RC)
      call ESMF_FieldDestroy(field,noGarbage=.true., _RC)
   enddo
   call ESMF_FieldBundleDestroy(sp%output_bundle,noGarbage=.true.,_RC)
   deallocate(names,_STAT)

   _RETURN(ESMF_SUCCESS)

  end subroutine destroy_rh_regen_ogrid


  subroutine fill_time_in_bundle (this, xname, bundle, ogrid, rc)
    implicit none
    class(samplerHQ) :: this
    character(len=*), intent(in) :: xname
    type(ESMF_FieldBundle), intent(inout) :: bundle
    integer, optional, intent(out) :: rc
    integer :: status

    type(ESMF_Grid), intent(in) :: ogrid
    class(AbstractGridFactory), pointer :: factory
    type(ESMF_Field) :: field
    real(kind=ESMF_KIND_R4), pointer :: ptr2d(:,:)

    ! __ get field xname='time'
    call ESMF_FieldBundleGet (bundle, xname, field=field, _RC)
    call ESMF_FieldGet (field, farrayptr=ptr2d, _RC)

    ! __ obs_time from swath factory
    factory => grid_manager%get_factory(ogrid,_RC)
    call factory%get_obs_time (ogrid, ptr2d, _RC)

    _RETURN(ESMF_SUCCESS)

  end subroutine fill_time_in_bundle


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


     subroutine Create_bundle_RH(this,items,bundle,tunit,timeInfo,vdata,ogrid,rc)
        class (sampler), intent(inout) :: this
        type(GriddedIOitemVector), target, intent(inout) :: items
        type(ESMF_FieldBundle), intent(inout) :: bundle
        character(len=*), intent(in) :: tunit
        type(TimeData), optional, intent(inout) :: timeInfo
        type(VerticalData), intent(inout), optional :: vdata
        type (ESMF_Grid), intent(inout), pointer, optional :: ogrid
        integer, intent(out), optional :: rc

        type(ESMF_Grid) :: input_grid
        class (AbstractGridFactory), pointer :: factory

        type(ESMF_Field) :: new_field
        type(GriddedIOitemVectorIterator) :: iter
        type(GriddedIOitem), pointer :: item
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

        ! __ please note, metadata in this section is not used in put_var to netCDF
        !    the design used mGriddedIO%metadata in MAPL_HistoryGridComp.F90
        !    In other words, factory%append_metadata appeared here and in GriddedIO.F90
        !
        if (allocated(this%metadata)) then
           deallocate (this%metadata)
        end if
        allocate(this%metadata,_STAT)
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

        ! __ add field to output_bundle
        !
        iter = this%items%begin()
        do while (iter /= this%items%end())
           item => iter%get()
           if (item%itemType == ItemTypeScalar) then
              call this%CreateVariable(item%xname,_RC)
           else if (item%itemType == ItemTypeVector) then
              call this%CreateVariable(item%xname,_RC)
              call this%CreateVariable(item%yname,_RC)
           end if
           call iter%next()
        enddo


        ! __ add field to acc_bundle
        !
        this%acc_bundle = ESMF_FieldBundleCreate(_RC)
        call ESMF_FieldBundleSet(this%acc_bundle,grid=this%output_grid,_RC)
        iter = this%items%begin()
        do while (iter /= this%items%end())
           item => iter%get()
           call this%addVariable_to_acc_bundle(item%xname,_RC)
           if (item%itemType == ItemTypeVector) then
              call this%addVariable_to_acc_bundle(item%yname,_RC)
           end if
           call iter%next()
        enddo


        ! __ add time to acc_bundle
        !
        new_field = ESMF_FieldCreate(this%output_grid ,name='time', &
               typekind=ESMF_TYPEKIND_R4,_RC)
        !
        ! add attribute
        !
        call ESMF_AttributeSet(new_field,'UNITS',trim(tunit),_RC)
        call MAPL_FieldBundleAdd( this%acc_bundle, new_field, _RC )

        _RETURN(_SUCCESS)
      end subroutine Create_Bundle_RH


     subroutine set_param(this,deflation,quantize_algorithm,quantize_level,zstandard_level,chunking,nbits_to_keep,regrid_method,itemOrder,write_collection_id,rc)
        class (sampler), intent(inout) :: this
        integer, optional, intent(in) :: deflation
        integer, optional, intent(in) :: quantize_algorithm
        integer, optional, intent(in) :: quantize_level
        integer, optional, intent(in) :: zstandard_level
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
        if (present(zstandard_level)) this%zstandardLevel = zstandard_level
        if (present(chunking)) then
           allocate(this%chunking,source=chunking,_STAT)
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
           allocate(this%chunking(5),_STAT)
           this%chunking(1) = global_dim(1)
           this%chunking(2) = global_dim(1)
           this%chunking(3) = 1
           this%chunking(4) = 1
           this%chunking(5) = 1
        else
           allocate(this%chunking(4),_STAT)
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

        call MAPL_GridGet(this%output_grid,globalCellCountPerDim=global_dim,_RC)
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
        logical :: first_entry

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
        first_entry = .true.
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
                 allocate(ptr3d(0,0,0),_STAT)
              end if
              allocate(ptr3d_inter(size(ptr3d,1),size(ptr3d,2),this%vdata%lm),_STAT)
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
           if (first_entry) then
              nullify(ptr3d)
              first_entry = .false.
           end if
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
              allocate(ptr2d(0,0),_STAT)
           end if
           if (hasDE_out) then
              call MAPL_FieldGetPointer(OutField,outptr2d,rc=status)
              _VERIFY(status)
           else
              allocate(outptr2d(0,0),_STAT)
           end if
           if (gridIn==gridOut) then
              outPtr2d=ptr2d
           else
              if (this%regrid_method==REGRID_METHOD_FRACTION) ptr2d=ptr2d-this%fraction
              call this%regrid_handle%regrid(ptr2d,outPtr2d,rc=status)
              _VERIFY(status)
           end if

!!           print *, maxval(ptr2d)
!!           print *, minval(ptr2d)
!!           print *, maxval(outptr2d)
!!           print *, minval(outptr2d)

        else if (fieldRank==3) then
           if (.not.associated(ptr3d)) then
              if (hasDE_in) then
                 call ESMF_FieldGet(field,farrayPtr=ptr3d,rc=status)
                 _VERIFY(status)
              else
                 allocate(ptr3d(0,0,0),_STAT)
              end if
           end if
           if (hasDE_out) then
              call MAPL_FieldGetPointer(OutField,outptr3d,rc=status)
              _VERIFY(status)
           else
              allocate(outptr3d(0,0,0),_STAT)
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
                 allocate(xptr3d(0,0,0),_STAT)
              end if
              allocate(xptr3d_inter(size(xptr3d,1),size(xptr3d,2),this%vdata%lm),_STAT)
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
                 allocate(yptr3d(0,0,0),_STAT)
              end if
              allocate(yptr3d_inter(size(yptr3d,1),size(yptr3d,2),this%vdata%lm),_STAT)
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
           nullify(xptr3d, yptr3d)
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
              allocate(xptr2d(0,0),_STAT)
              allocate(yptr2d(0,0),_STAT)
           end if

           if (hasDE_in) then
              call MAPL_FieldGetPointer(xOutField,xoutptr2d,rc=status)
              _VERIFY(status)
              call MAPL_FieldGetPointer(yOutField,youtptr2d,rc=status)
              _VERIFY(status)
           else
              allocate(xoutptr2d(0,0),_STAT)
              allocate(youtptr2d(0,0),_STAT)
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
                 allocate(xptr3d(0,0,0),_STAT)
              end if
           end if
           if (.not.associated(yptr3d)) then
              if (hasDE_in) then
                 call MAPL_FieldGetPointer(yfield,yptr3d,rc=status)
                 _VERIFY(status)
              else
                 allocate(yptr3d(0,0,0),_STAT)
              end if
           end if

           if (hasDE_out) then
              call MAPL_FieldGetPointer(xOutField,xoutptr3d,rc=status)
              _VERIFY(status)
              call MAPL_FieldGetPointer(yOutField,youtptr3d,rc=status)
              _VERIFY(status)
           else
              allocate(xoutptr3d(0,0,0),_STAT)
              allocate(youtptr3d(0,0,0),_STAT)
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
     allocate(temp(nFixedVars+1:n),_STAT)
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
     deallocate(temp,_STAT)

     _RETURN(_SUCCESS)

  end subroutine alphabatize_variables


  subroutine addVariable_to_acc_bundle(this,itemName,rc)
    class (sampler), intent(inout) :: this
    character(len=*), intent(in) :: itemName
    integer, optional, intent(out) :: rc

    type(ESMF_Field) :: field,newField
    integer :: fieldRank
    integer :: status

    call ESMF_FieldBundleGet(this%input_bundle,itemName,field=field,_RC)
    call ESMF_FieldGet(field,rank=fieldRank,rc=status)
    if (this%doVertRegrid .and. (fieldRank ==3) ) then
       newField = MAPL_FieldCreate(field,this%output_grid,lm=this%vData%lm,_RC)
    else
       newField = MAPL_FieldCreate(field,this%output_grid,_RC)
    end if
    call MAPL_FieldBundleAdd(this%acc_bundle,newField,_RC)

    _RETURN(_SUCCESS)

  end subroutine addVariable_to_acc_bundle



  !! -- based on subroutine bundlepost(this,filename,oClients,rc)
  !!
  subroutine interp_accumulate_fields (this,xy_subset,rc)
    implicit none
    class (sampler) :: this
    integer, intent(in) :: xy_subset(2,2)
    !!integer, intent(in) :: xy_mask(:,:)
    integer, optional, intent(out) :: rc

    integer :: status
    type(ESMF_Field) :: outField, outField2
    type(ESMF_Field) :: new_outField
    type(ESMF_Grid)  :: grid

    type(GriddedIOitemVectorIterator) :: iter
    type(GriddedIOitem), pointer :: item

    type(ESMF_Array) :: array1, array2
    integer :: is,ie,js,je

    integer :: rank
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

    if (js > je) then
       ! no valid points are found on swath grid for this time step
       _RETURN(ESMF_SUCCESS)
    end if

    if (this%vdata%regrid_type==VERTICAL_METHOD_ETA2LEV) then
       call this%vdata%setup_eta_to_pressure(regrid_handle=this%regrid_handle,output_grid=this%output_grid,rc=status)
       _VERIFY(status)
    end if

    call ESMF_FieldBundleGet(this%output_bundle, grid=grid, _RC)
    call ESMF_GridGet(grid, localDECount=localDECount, dimCount=dimCount, _RC)
    allocate ( LB(dimCount), UB(dimCount), exclusiveCount(dimCount) ,_STAT)
    allocate ( compLB(dimCount), compUB(dimCount), compCount(dimCount) ,_STAT)

    allocate ( j1(0:localDEcount-1) ,_STAT)  ! start
    allocate ( j2(0:localDEcount-1) ,_STAT)  ! end

    _ASSERT ( localDEcount == 1, 'failed, due to localDEcount > 1')
    call MAPL_GridGetInterior(grid,ii1,iin,jj1,jjn)
!!    write(6,*) 'MAPL_GridGetInterior, ii1,iin,jj1,jjn', ii1,iin,jj1,jjn
!!    print*, 'js,je ', js, je

    LB(1)=ii1; LB(2)=jj1
    UB(1)=iin; UB(2)=jjn

    do localDe=0, localDEcount-1
       !
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

!!    write(6,*) 'ck bundlepost_acc'
!!    write(6,*) 'j1(localDe)', j1(0:localDeCount-1)
!!    write(6,*) 'j2(localDe)', j2(0:localDeCount-1)


    iter = this%items%begin()
    do while (iter /= this%items%end())
       item => iter%get()
       if (item%itemType == ItemTypeScalar) then
          call this%RegridScalar(item%xname,_RC)
          call ESMF_FieldBundleGet(this%output_bundle,item%xname,field=outField, _RC)
          if (this%vdata%regrid_type==VERTICAL_METHOD_ETA2LEV) then
             call this%vdata%correct_topo(outField,_RC)
          end if
       elseif (item%itemType == ItemTypeVector) then
          call this%RegridVector(item%xname,item%yname,_RC)
          call ESMF_FieldBundleGet(this%output_bundle,item%xname,field=outField, _RC)
          if (this%vdata%regrid_type==VERTICAL_METHOD_ETA2LEV) then
             call this%vdata%correct_topo(outField,_RC)
          end if
          call ESMF_FieldBundleGet(this%output_bundle,item%yname,field=outField2, _RC)
          if (this%vdata%regrid_type==VERTICAL_METHOD_ETA2LEV) then
             call this%vdata%correct_topo(outField2,_RC)
          end if
       end if


       ! -- mask the time interval
       !    store the time interval fields into new bundle
       !    xname
       call ESMF_FieldGet(outField, Array=array1, _RC)
       call ESMF_FieldBundleGet(this%acc_bundle,item%xname,field=new_outField,_RC)
       call ESMF_FieldGet(new_outField, Array=array2, _RC)
       call ESMF_ArrayGet(array1, rank=rank, _RC)
       if (rank==2) then
          call ESMF_ArrayGet(array1, farrayptr=pt2d, _RC)
          call ESMF_ArrayGet(array2, farrayptr=pt2d_, _RC)
          localDe=0
          if (j1(localDe)>0) then
             do j= j1(localDe), j2(localDe)
                jj= j-jj1+1     ! j_local
                !!                      write(6,*) 'j, jj', j, jj
                pt2d_(:,jj) = pt2d(:,jj)
             enddo
          endif
       elseif (rank==3) then
          call ESMF_ArrayGet(array1, farrayptr=pt3d, _RC)
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
          _FAIL('failed interp_accumulate_fields')
       endif

       ! __ additional step for yname if vector
       if (item%itemType == ItemTypeScalar) then
          ! already done
       elseif (item%itemType == ItemTypeVector) then
          !
          ! add yname
          !
          call ESMF_FieldGet(outField2, Array=array1, _RC)
          call ESMF_FieldBundleGet(this%acc_bundle,item%yname,field=new_outField,_RC)
          call ESMF_FieldGet(new_outField, Array=array2, _RC)
          call ESMF_ArrayGet(array1, rank=rank, _RC)
          if (rank==2) then
             call ESMF_ArrayGet(array1, farrayptr=pt2d, _RC)
             call ESMF_ArrayGet(array2, farrayptr=pt2d_, _RC)
             localDe=0
                if (j1(localDe)>0) then
                   do j= j1(localDe), j2(localDe)
                      jj= j-jj1+1     ! j_local
!!                      write(6,*) 'j, jj', j, jj
                      pt2d_(:,jj) = pt2d(:,jj)
                   enddo
                endif
          elseif (rank==3) then
             call ESMF_ArrayGet(array1, farrayptr=pt3d, _RC)
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
             _FAIL('failed interp_accumulate_fields')
          endif
       end if
       call iter%next()
    enddo

    _RETURN(ESMF_SUCCESS)

  end subroutine interp_accumulate_fields


  subroutine get_xy_mask(grid, xy_subset, xy_mask, rc)
    implicit none
    type(ESMF_Grid), intent(in) :: grid
    integer, intent(in)  :: xy_subset(2,2)
    integer, intent(out) :: xy_mask(:,:)
    integer, optional, intent(out) :: rc

    integer :: status
    integer :: ii1, iin, jj1, jjn  ! local box for localDE
    integer :: is,ie, js, je       ! global box for each time-interval

    integer :: y1, y2
    integer :: jj
    integer :: j1, j2

    is=xy_subset(1,1); ie=xy_subset(2,1)
    js=xy_subset(1,2); je=xy_subset(2,2)

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

!!    write(6,*) 'get_xy_mask: j1,j2=', j1, j2
    xy_mask(:,:) = 0
    if (j1 > 0) then
       do jj = j1, j2
          xy_mask(:, jj) = 1
       enddo
    end if

    if(present(rc)) rc=0

  end subroutine get_xy_mask


end module MAPL_EpochSwathMod
