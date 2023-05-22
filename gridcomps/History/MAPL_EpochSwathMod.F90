#include "MAPL_Generic.h"
module MAPL_EpochSwathMod
  use ESMF
  use MAPL_ErrorHandlingMod
  use netcdf
  use MAPL_CommsMod
  use mapl_MaplGrid, only : MAPL_GridGet
  use MAPL_Base, only : MAPL_GetHorzIJIndex, MAPL_GetGlobalHorzIJIndex
  use pFIO_NetCDF4_FileFormatterMod
  use MAPL_GridManagerMod   

  implicit none
  private

  public :: generic_sampler

  
  !____ Using generic epoch
  !
!!  type :: time_n_alarm
!!     type(ESMF_Clock)         :: clock
!!     type(ESMF_Alarm)         :: alarm
!!     type(ESMF_Time)          :: RingTime
!!     type(ESMF_TimeInterval)  :: Frequency
!!  end type time_n_alarm



  !
  !__ clct_sampler for each collection
  !
  type :: sampler
     class(*), pointer     :: ptr => null()
     type(MAPL_GriddedIO)  :: io_only
     integer :: regrid_method = REGRID_METHOD_BILINEAR
     class (AbstractRegridder), pointer :: regrid_handle => null()
  end type sampler
  
  type :: epoch
     type(ESMF_Time)         :: currTime
     type(ESMF_TimeInterval) :: timeStep
     integer                 :: xy_subset(2,2)
     type(ESMF_FieldBundle)  :: bundle_acc
     type(FileMetaData)      :: metadata
     character(len=ESMF_MAXSTR) :: grid_type
     type(ESMF_grid) :: ogrid     
     type (ESMF_Config) :: config_grid
  end type epoch


  type :: samplerHQ   ! generic_sample
     !!type(time_n_alarm)    :: time_n_alarm_for_epoch
     type(ESMF_Clock)         :: clock
     type(ESMF_Alarm)         :: alarm
     type(ESMF_Time)          :: RingTime
     type(ESMF_TimeInterval)  :: Frequency_epoch
     type(ESMF_config) :: conf_grid_save
     type(ESMF_grid) :: ogrid
     
     contains
     procedure ::  init => init_epoch
     procedure ::  create_sampler => create_epoch_sampler
     procedure ::  destroy_n_regenerate_epoch
     procedure ::  create_grid => gen_ogrid
     procedure ::  destroy_ogrid
     procedure ::  createFilemetadata
     procedure ::  gen_regrid_RH
     procedure ::  regrid_accumulate => regrid_accumulate_on_xysubset
     procedure ::  write_2_oserver     
  end type samplerHQ   ! generic_sampler

  interface samplerHQ
     module procedure new_samplerHQ
  end interface samplerHQ

  
contains

  !!  function new_sampler(ext_currTime, timestep, config) result(mysampler)
  function new_samplerHQ(clock, config, rc) result(hq)  
    type(ESMF_Time), intent(in) :: ext_currTime
    type(ESMF_TimeInterval), intent(in) :: timestep
    type(ESMF_Clock),  intent(in) :: clock
    type(ESMF_Config), intent(in) :: config
    type(samplerHQ), intent(out)  :: hq
    type(epoch) :: epoch_new
    integer, intent(out), optional :: rc    
    character(len=ESMF_MAXSTR) :: time_string
    integer :: status
    integer :: time_integer
    type(ESMF_Time)            :: RingTime_epoch
    type(ESMF_Time)            :: startTime    
    
    hq%clock= clock
    hq%config_grid_save= config

    call ESMF_ClockGet ( clock, CurrTime=epoch_new%currTime, _RC )
    call ESMF_ClockGet ( clock, timestep=epoch_new%timestep, _RC )
    call ESMF_ClockGet ( clock, startTime=startTime, _RC )

    call ESMF_ConfigGetAttribute(config, time_integer, label=prefix//'Epoch:', default=0, _RC)
    _ASSERT(time_integer /= 0, 'Epoch value in config wrong')
    call hms_2_s (time_integer, sec, _RC)
    call ESMF_TimeIntervalSet(frequency_epoch, s=second, _RC)
    hq%frequency_epoch = frequency_epoch
    hq%RingTime_epoch  = currTime
    hq%alarm = ESMF_AlarmCreate( clock=clock, RingInterval=Frequency_epoch, &
         RingTime=hq%RingTime_epoch, sticky=.false., _RC )
    hq%ptr => epoch_new
    
  end function new_samplerHQ


  !!  function create_grid(this, grid_type, config_grid, key, currTime, _RC)  result(orgid)
  function gen_ogrid(this, grid_type, key, currTime, rc)  result(ogrid)  
    type(samplerHQ), intent(inout)  :: this
    character(len=:), intent(in) :: grid_type
    character(len=:), intent(in) :: key
    type(ESMF_Time), intent(in) :: currTime
    integer, intent(out), optional :: rc
    type (ESMF_Grid), intent(out) :: ogrid

    character(len=ESMF_MAXSTR) :: time_string
    integer :: status

    this%ptr%grid_type = trim(grid_type)
    config_grid = this%config_grid_save
    call ESMF_TimeSet(currTime, time_string, _RC)
    call ESMF_ConfigSetAttribute( config_grid, time_string, label=trim(key)//'.Epoch_init:', _RC)
    ogrid = grid_manager%make_grid(config_grid, prefix=key//'.', _RC )
    this%ptr%ogrid = ogrid
  end subroutine gen_ogrid


  function create_epoch_sampler (this, regrid_method, input_bundle, rc) result(mysampler)
    type(samplerHQ), intent(inout)  :: this
    integer, intent(in) :: regrid_method
    type(ESMF_bundle), intent(in) :: input_bundle
    type(sampler), intent(out) :: mysampler
    integer, intent(out), optional :: rc

    integer :: status
    type(ESMF_grid) :: input_grid
    
    mysampler%input_bundle = input_bundle
    mysampler%regrid_method = regrid_method    
    
    call ESMF_FieldBundleGet(input_bundle,grid=input_grid,rc=status)
    mysampler%regrid_handle => new_regridder_manager%make_regridder(input_grid,this%ogrid,regrid_method,_RC)

  end function create_epoch_sampler

    
  subroutine gen_regrid_RH (this, bundle_in, rc)
    type(sampler), intent(inout)  :: this
    type (ESMF_Bundle), intent(in)  :: bundle_in
    type (ESMF_Bundle), intent(out) :: bundle_out
    
    character(len=:), intent(in) :: grid_type
    character(len=:), intent(in) :: key
    type(ESMF_Time), intent(in) :: currTime
    integer, intent(out), optional :: rc


    character(len=ESMF_MAXSTR) :: time_string
    integer :: status
    class (AbstractRegridder), pointer, intent(out) :: regrid_handle    
    
  end subroutine gen_regrid_RH

  


  subroutine regrid_accumulate_on_xysubset (this, sp, rc)
    !bundle_in, bundle_out, RH, rc)
    type(samplerHQ), intent(inout)  :: this
    type(sampler), intent(inout)  :: sp

    integer                        :: xy_subset(2,2)
    type(ESMF_Time)                :: timeset(2)

    
    ! __ s1.  get xy_subset
    
    factory => grid_manager%get_factory(this%ogrid,_RC)
    call ESMF_ClockGet(this%clock,currTime=current_time,_RC)
    call ESMF_ClockGet(this%clock,timeStep=dur, _RC )
    timeset(1) = current_time - dur
    timeset(2) = current_time
    call factory%get_subset( timeset, xy_subset, _RC)
    write(6,*) 'xy_subset(:,1)_x', xy_subset(:,1)    ! LB
    write(6,*) 'xy_subset(:,2)_a', xy_subset(:,2), xy_subset(2,2)-xy_subset(1,2)+1  ! UB


    


    type (ESMF_Bundle), intent(in)  :: bundle_in
    type (ESMF_Bundle), intent(out) :: bundle_out

    
    character(len=:), intent(in) :: grid_type
    character(len=:), intent(in) :: key
    type(ESMF_Time), intent(in) :: currTime
    integer, intent(out), optional :: rc


    character(len=ESMF_MAXSTR) :: time_string
    integer :: status
  end subroutine regrid_accumulate

  

    

    subroutine init_epoch (this, epoch, ext_current_time)
    type (sampler), intent(inout) :: this
    type(ESMF_Time)             :: ext_currTime
    !
    type(epoch) :: epoch_new
    type (ESMF_Config) :: config_swath_new

    character(len=ESMF_MAXSTR) :: time_string
    integer :: status
    this%ptr => epoch_new
  end subroutine init_epoch



    
    

    
  
  subroutine destroy_n_regenerate_epoch (this, ext_current_time)
    type (epoch), intent(inout) :: this
    type(ESMF_Time)             :: ext_currTime
    !
    type (ESMF_Config) :: config_swath_new



   call ESMF_TimeGet  ( current_time, TimeString=string  ,_RC )
   write(6,*) 'ck string: current_time: ', trim(string)


   regen_swath_grid: do n=1,nlist
      if (epoching(n) .AND. trim(list(n)%output_grid_label)=='swathGrid') then

         key_str=trim(list(n)%output_grid_label)

!- need imlpement
!         call list(n)%mGriddedIO%write_oserver ( list(n)%bundle_acc )     ! missing time
!         call list(n)%mGriddedIO%delte_regridhandle (list(n)%bundle,_RC)

         ! -- destroy, regenerate swath grid
         write(6,*) 'I am inside list(n)%epoching', epoching(n)
         pgrid => IntState%output_grids%at(trim(key_str))
!         call grid_manager%destroy(pgrid,_RC)
! backup
         grid_out = pgrid
         call grid_manager%destroy(grid_out,_RC)

         call IntState%output_grids%insert(trim(key_str), output_grid)
         call ESMF_ConfigSetAttribute( config_swath, trim(string), label=trim(key_str)//'.Epoch_init:', _RC)

         iter = IntState%output_grids%begin()
         do while (iter /= IntState%output_grids%end())
            key => iter%key()
            if (trim(key)==trim(key_str)) then
               output_grid = grid_manager%make_grid( config_swath, prefix=trim(key_str)//'.', _RC)
               call IntState%output_grids%set(key, output_grid)
            endif
            call iter%next()
         enddo
      endif
      write(6,*) 'end of regen_swath_grid'
   end do regen_swath_grid

   write(6,*) 'end hist run'


    
end module MAPL_EpochSwathMod
