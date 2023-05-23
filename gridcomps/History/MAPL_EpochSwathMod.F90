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
     type(ESMF_FieldBundle)  :: input_bundle
     type(ESMF_FieldBundle)  :: output_bundle     
     type(ESMF_FieldBundle)  :: bundle_acc
     type(ESMF_grid) :: input_grid
     type(ESMF_grid) :: output_grid     
     type(GriddedIOitemVector) :: items
     type(VerticalData) :: vdata
     type(FileMetaData)      :: metadata
     integer, allocatable :: chunking(:)

     procedure :: set_default_chunking
     procedure :: check_chunking
  end type sampler

  type :: epoch
     type(ESMF_Time)         :: currTime
     type(ESMF_TimeInterval) :: timeStep
     integer                 :: xy_subset(2,2)
     type(ESMF_FieldBundle)  :: bundle_acc
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


  function create_epoch_sampler (this, regrid_method, input_bundle, items, vdata, rc) result(sp)
    type(samplerHQ), intent(inout)  :: this
    integer, intent(in) :: regrid_method
    type(ESMF_bundle), intent(in) :: input_bundle
    type(sampler), intent(out) :: sp
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
    sp%bundle_acc = ESMF_FieldBundleCreate(_RC)
    call ESMF_FieldBundleSet(sp%bundle_acc,grid=this%output_grid, _RC)

    sp%regrid_handle => new_regridder_manager%make_regridder(input_grid,this%ogrid,regrid_method,_RC)
    
  end function create_epoch_sampler
  

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



     
  
!  subroutine gen_regrid_RH (this, bundle_in, rc)
!    type(sampler), intent(inout)  :: this
!    type (ESMF_Bundle), intent(in)  :: bundle_in
!    type (ESMF_Bundle), intent(out) :: bundle_out
!
!    character(len=:), intent(in) :: grid_type
!    character(len=:), intent(in) :: key
!    type(ESMF_Time), intent(in) :: currTime
!    integer, intent(out), optional :: rc
!
!    character(len=ESMF_MAXSTR) :: time_string
!    integer :: status
!    class (AbstractRegridder), pointer, intent(out) :: regrid_handle    
!
!  end subroutine gen_regrid_RH


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
  end subroutine regrid_accumulate_on_xysubset



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
  !
  !    
  !    
  !
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
  !!         call list(n)%mGriddedIO%write_oserver ( list(n)%bundle_acc )     ! missing time
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

  subroutine interp_accumulate_fields (this,new_output_bundle,xy_subset,rc)
    class (sampler), intent(inout) :: this

    type(ESMF_FieldBundle), intent(inout) :: new_output_bundle
    integer, intent(in) :: xy_subset(2,2)
    integer, optional, intent(out) :: rc

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
             stop 'failed GriddedIO.F90'
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

    _RETURN(ESMF_SUCCESS)

  end subroutine interp_accumulate_fields



  subroutine CreateFileMetaData(this,new_output_bundle,rc)
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
              call this%CreateVariable_default(item%xname,rc=status)
              _VERIFY(status)
           else if (item%itemType == ItemTypeVector) then
              call this%CreateVariable_default(item%xname,rc=status)
              _VERIFY(status)
              call this%CreateVariable_default(item%yname,rc=status)
              _VERIFY(status)
           end if
           call iter%next()
        enddo


        ! Add newFields to new bundle
        if (present(new_output_bundle)) then
           write(6,*) 'ck inside if (present(new_output_bundle))'
           iter = this%items%begin()
           do while (iter /= this%items%end())
              item => iter%get()
              if (item%itemType == ItemTypeScalar) then
                 call this%CreateVariable_newbundle(item%xname,new_output_bundle,_RC)
              else if (item%itemType == ItemTypeVector) then
                 call this%CreateVariable_newbundle(item%xname,new_output_bundle,_RC)
                 call this%CreateVariable_newbundle(item%yname,new_output_bundle,_RC)
              end if
              call iter%next()
           enddo
        endif
           
        
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


     

     
     
end module MAPL_EpochSwathMod

