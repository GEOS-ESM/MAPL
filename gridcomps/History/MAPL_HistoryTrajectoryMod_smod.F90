#include "MAPL_ErrLog.h"
#include "unused_dummy.H"


submodule (HistoryTrajectoryMod)  HistoryTrajectory_implement
  use ESMF
  use MAPL_ErrorHandlingMod
  use MAPL_KeywordEnforcerMod
  use LocStreamFactoryMod
  use MAPL_LocstreamRegridderMod
  use MAPL_FileMetadataUtilsMod
  use pFIO
  use MAPL_GriddedIOItemMod
  use MAPL_GriddedIOItemVectorMod
  use MAPL_TimeDataMod
  use MAPL_VerticalDataMod
  use MAPL_BaseMod
  use MAPL_CommsMod
  use MAPL_SortMod
  use MAPL_NetCDF
  use MAPL_StringTemplate
  use MAPL_plain_netCDF_Time
  use MAPL_ISO8601_DateTime_ESMF
  use, intrinsic :: iso_fortran_env, only: REAL32
  use, intrinsic :: iso_fortran_env, only: REAL64
  implicit none

   contains

     module procedure HistoryTrajectory_from_config
         character(len=ESMF_MAXSTR) :: filename
         character(len=ESMF_MAXSTR) :: grp_name
         character(len=ESMF_MAXSTR) :: dim_name(10)
         character(len=ESMF_MAXSTR) :: var_name_lon
         character(len=ESMF_MAXSTR) :: var_name_lat
         character(len=ESMF_MAXSTR) :: var_name_time
         type(ESMF_TimeInterval)    :: epoch_frequency
         type(ESMF_Time)            :: currTime
         integer                    :: time_integer, second
         integer                    :: ncid, grpid, ncid0
         integer                    :: len, status
         integer                    :: itime(2), nymd, nhms
         character(len=ESMF_MAXSTR) :: STR1
         character(len=ESMF_MAXSTR) :: symd, shms
         integer                    :: i, j, k
         ! __ parse variables, set alarm
         !
         !!call ESMF_ConfigGetAttribute(config, value=traj%obsFile, default="", &
         !!     label=trim(string) // 'obs_file:', _RC)


         call ESMF_ConfigGetAttribute(config, value=traj%nc_index, default="", &
              label=trim(string) // 'nc_Index:', _RC)
         call ESMF_ConfigGetAttribute(config, value=traj%nc_time, default="", &
              label=trim(string) // 'nc_Time:', _RC)
         call ESMF_ConfigGetAttribute(config, value=traj%nc_longitude, default="", &
              label=trim(string) // 'nc_Longitude:', _RC)
         call ESMF_ConfigGetAttribute(config, value=traj%nc_latitude, default="", &
              label=trim(string) // 'nc_Latitude:', _RC)

         traj%clock=clock
         call ESMF_ClockGet ( clock, CurrTime=currTime, _RC )
         call ESMF_ConfigGetAttribute(config, value=time_integer, label=trim(string)//'Epoch:', default=0, _RC)
         _ASSERT(time_integer /= 0, 'Epoch value in config wrong')
         call hms_2_s (time_integer, second, _RC)
         call ESMF_TimeIntervalSet(epoch_frequency, s=second, _RC)
         traj%Epoch = time_integer
         traj%epoch_frequency = epoch_frequency
         traj%RingTime  = currTime
         traj%alarm = ESMF_AlarmCreate( clock=clock, RingInterval=epoch_frequency, &
              RingTime=traj%RingTime, sticky=.false., _RC )

         call ESMF_ConfigGetAttribute(config, value=traj%obsfile_template, default="", &
              label=trim(string) // 'obs_file:', _RC)
         call ESMF_ConfigGetAttribute(config, value=STR1, default="", &
              label=trim(string) // 'obs_file_begin:', _RC)
         if (mapl_am_I_root()) write(6,*) 'obs_file_begin:', trim(STR1)
         call ESMF_TimeSet(traj%obsfile_start_time, STR1, _RC)

         call ESMF_ConfigGetAttribute(config, value=STR1, default="", &
              label=trim(string) // 'obs_file_end:', _RC)
         if (mapl_am_I_root()) write(6,*) 'obs_file_end:', trim(STR1)
         call ESMF_TimeSet(traj%obsfile_end_time, STR1, _RC)

         call ESMF_ConfigGetAttribute(config, value=STR1, default="", &
              label=trim(string) // 'obs_file_interval:', _RC)
         if (mapl_am_I_root()) write(6,*) 'obs_file_interval:', trim(STR1)


         i= index( trim(STR1), ' ' )
         if (i>0) then
            symd=STR1(1:i-1)
            shms=STR1(i+1:)
         else
            symd=''
            shms=trim(STR1)
         endif
         call convert_twostring_2_esmfinterval (symd, shms,  traj%obsfile_interval, _RC)

         _RETURN(_SUCCESS)

       end procedure


       module procedure initialize
         integer :: status,nobs
         type(ESMF_Grid) :: grid
         type(variable) :: v
         type(GriddedIOitemVectorIterator) :: iter
         type(GriddedIOitem), pointer :: item
         type(ESMF_Time)            :: currTime
         ! __ s.1   create LS grid, RH, output/acc bundle
         ! __ s.2   metadata
         ! __ s.3   misc

         this%bundle=bundle
         this%items=items
         this%epoch_index(1:2)=0

         if (present(vdata)) then
            this%vdata=vdata
         else
            this%vdata=VerticalData(_RC)
         end if
         call this%vdata%append_vertical_metadata(this%metadata,this%bundle,_RC)
         this%do_vertical_regrid = (this%vdata%regrid_type /= VERTICAL_METHOD_NONE)
         if (this%vdata%regrid_type == VERTICAL_METHOD_ETA2LEV) call this%vdata%get_interpolating_variable(this%bundle,_RC)

         call ESMF_ClockGet ( this%clock, CurrTime=currTime, _RC )
         call this%get_obsfile_Tbracket_from_epoch(currTime, _RC)
         call this%create_grid(_RC)
         call ESMF_FieldBundleGet(this%bundle,grid=grid,_RC)
         this%regridder = LocStreamRegridder(grid,this%LS_ds,_RC)
         this%output_bundle = this%create_new_bundle(_RC)
         this%acc_bundle    = this%create_new_bundle(_RC)


         this%time_info = timeInfo

         call this%metadata%add_dimension('time', this%nobs_epoch)
         if (this%time_info%integer_time) then
            v = Variable(type=PFIO_INT32,dimensions='time')
         else
            v = Variable(type=PFIO_REAL32,dimensions='time')
         end if
         call v%add_attribute('units', this%datetime_units)
         call v%add_attribute('long_name', this%nc_time)
         call this%metadata%add_variable(this%var_name_time,v)

         v = variable(type=PFIO_REAL64,dimensions="time")
         call v%add_attribute('units','degrees_east')
         call v%add_attribute('long_name','longitude')
         call this%metadata%add_variable(this%var_name_lon,v)

         v = variable(type=PFIO_REAL64,dimensions="time")
         call v%add_attribute('units','degrees_east')
         call v%add_attribute('long_name','latitude')
         call this%metadata%add_variable(this%var_name_lat,v)

         iter = this%items%begin()
         do while (iter /= this%items%end())
            item => iter%get()
            if (item%itemType == ItemTypeScalar) then
               call this%create_variable(item%xname,_RC)
            else if (item%itemType == ItemTypeVector) then
               call this%create_variable(item%xname,_RC)
               call this%create_variable(item%yname,_RC)
            end if
            call iter%next()
         enddo

         this%file_name = ''

         this%recycle_track=.false.
         if (present(recycle_track)) then
            this%recycle_track=recycle_track
         end if
         if (this%recycle_track) then
            call this%reset_times_to_current_day(_RC)
         end if
         _RETURN(_SUCCESS)

      end procedure


      module procedure create_metadata_variable
        type(ESMF_Field) :: field
        type(variable) :: v
        logical :: is_present
        integer :: field_rank, status
        character(len=ESMF_MAXSTR) :: var_name,long_name,units,vdims
        type(ESMF_Info) :: infoh

        call ESMF_FieldBundleGet(this%bundle,vname,field=field,_RC)
        call ESMF_FieldGet(field,name=var_name,rank=field_rank,_RC)
        call ESMF_InfoGetFromHost(field,infoh,_RC)
        is_present = ESMF_InfoIsPresent(infoh,"LONG_NAME",_RC)
        if ( is_present ) then
           call ESMF_InfoGet(infoh,"LONG_NAME",long_name,_RC)
        else
           long_name = var_name
        endif
        is_present = ESMF_InfoIsPresent(infoh,"UNITS",_RC)
        if ( is_present ) then
           call ESMF_InfoGet(infoh,"UNITS",units,_RC)
        else
           units = 'unknown'
        endif
        if (field_rank==2) then
           vdims = "time"
        else if (field_rank==3) then
           vdims = "time,lev"
        end if
        v = variable(type=PFIO_REAL32,dimensions=trim(vdims))
        call v%add_attribute('units',trim(units))
        call v%add_attribute('long_name',trim(long_name))
        call v%add_attribute('missing_value',MAPL_UNDEF)
        call v%add_attribute('_FillValue',MAPL_UNDEF)
        call v%add_attribute('valid_range',(/-MAPL_UNDEF,MAPL_UNDEF/))
        call this%metadata%add_variable(trim(var_name),v,_RC)

         _RETURN(_SUCCESS)
      end procedure create_metadata_variable


      module procedure create_new_bundle
        type(GriddedIOitemVectorIterator) :: iter
        type(GriddedIOitem), pointer :: item
        type(ESMF_Field) :: src_field,dst_field
        integer :: rank,lb(1),ub(1)
        real(kind=REAL32), pointer :: p_acc_3d(:,:),p_acc_2d(:)
        integer :: status

        new_bundle = ESMF_FieldBundleCreate(_RC)
        iter = this%items%begin()
        do while (iter /= this%items%end())
           item => iter%get()
           if (item%itemType == ItemTypeScalar) then
              call ESMF_FieldBundleGet(this%bundle,trim(item%xname),field=src_field,_RC)
              call ESMF_FieldGet(src_field,rank=rank,_RC)
              if (rank==2) then
                 dst_field = ESMF_FieldCreate(this%LS_ds,name=trim(item%xname), &
                      typekind=ESMF_TYPEKIND_R4,_RC)
              else if (rank==3) then
                 call ESMF_FieldGet(src_field,ungriddedLBound=lb,ungriddedUBound=ub,_RC)
                 if (this%vdata%lm/=(ub(1)-lb(1)+1)) then
                    lb(1)=1
                    ub(1)=this%vdata%lm
                 end if
                 dst_field = ESMF_FieldCreate(this%LS_ds,name=trim(item%xname), &
                      typekind=ESMF_TYPEKIND_R4,ungriddedLBound=lb,ungriddedUBound=ub,_RC)
              end if
              call MAPL_FieldBundleAdd(new_bundle,dst_field,_RC)
           else if (item%itemType == ItemTypeVector) then
              _FAIL("ItemTypeVector not yet supported")
           end if
           call iter%next()
        enddo
        _RETURN(_SUCCESS)

      end procedure create_new_bundle


      module procedure create_file_handle
         type(variable) :: v
         integer :: status

         this%file_name = trim(filename)
         ! __ update metadata
         call this%metadata%modify_dimension('time', this%nobs_epoch)
         if (mapl_am_I_root()) then
            call this%file_handle%create(trim(filename),_RC)
            call this%file_handle%write(this%metadata,_RC)
         end if

        _RETURN(_SUCCESS)
      end procedure create_file_handle


       module procedure close_file_handle
          integer :: status

          if (trim(this%file_name) /= '') then
             if (mapl_am_i_root()) then
                call this%file_handle%close(_RC)
             end if
          end if
          _RETURN(_SUCCESS)
       end procedure close_file_handle


      module procedure append_file
         type(GriddedIOitemVectorIterator) :: iter
         type(GriddedIOitem), pointer :: item
         type(ESMF_RouteHandle) :: RH

         type(ESMF_Field) :: src_field, dst_field
         type(ESMF_Field) :: acc_field
         type(ESMF_Field) :: acc_field_2d_rt, acc_field_3d_rt
         real(kind=REAL32), allocatable :: p_new_lev(:,:,:)
         real(kind=REAL32), pointer :: p_src_3d(:,:,:),p_src_2d(:,:)
         real(kind=REAL32), pointer :: p_dst_3d(:,:),p_dst_2d(:)
         real(kind=REAL32), pointer :: p_acc_3d(:,:),p_acc_2d(:)
         real(kind=REAL32), pointer :: p_acc_rt_3d(:,:),p_acc_rt_2d(:)
         real(kind=ESMF_KIND_R8), allocatable :: rtimes(:)

         integer :: is, ie, nx
         integer :: lb(1),ub(1)
         integer :: lm
         integer :: rank
         integer :: status

         nx=this%nobs_epoch
         is=1
         ie=nx
         print*, 'begin append_file, nobs_epoch=', nx

         if (this%nobs_epoch_sum==0) then
            rc=0
            return
         endif

         if (mapl_am_i_root()) then
            _ASSERT (nx /= 0, 'wrong, we should never have zero obs here!')
            call this%file_handle%put_var(this%var_name_time, real(this%times_R8), &
                 start=[is], count=[nx], _RC)
            call this%file_handle%put_var(this%var_name_lon, this%lons, &
                 start=[is], count=[nx], _RC)
            call this%file_handle%put_var(this%var_name_lat, this%lats, &
                 start=[is], count=[nx], _RC)
         end if

         ! get RH from 2d field
         src_field = ESMF_FieldCreate(this%LS_ds,typekind=ESMF_TYPEKIND_R4,gridToFieldMap=[1],_RC)
         dst_field = ESMF_FieldCreate(this%LS_rt,typekind=ESMF_TYPEKIND_R4,gridToFieldMap=[1],_RC)
         call ESMF_FieldRedistStore(src_field,dst_field,RH,_RC)
         call ESMF_FieldDestroy(src_field,noGarbage=.true.,_RC)
         call ESMF_FieldDestroy(dst_field,noGarbage=.true.,_RC)

         ! redist and put_var
         lm = this%vdata%lm
         acc_field_2d_rt = ESMF_FieldCreate (this%LS_rt, name='field_2d_rt', typekind=ESMF_TYPEKIND_R4, _RC)
         acc_field_3d_rt = ESMF_FieldCreate (this%LS_rt, name='field_3d_rt', typekind=ESMF_TYPEKIND_R4, &
              gridToFieldMap=[1],ungriddedLBound=[1],ungriddedUBound=[lm],_RC)
         iter = this%items%begin()
         do while (iter /= this%items%end())
            item => iter%get()
            if (item%itemType == ItemTypeScalar) then
               call ESMF_FieldBundleGet(this%acc_bundle,trim(item%xname),field=acc_field,_RC)
               call ESMF_FieldGet(acc_field,rank=rank,_RC)
               if (rank==1) then
                  call ESMF_FieldGet( acc_field, localDE=0, farrayPtr=p_acc_2d, _RC)
                  call ESMF_FieldGet( acc_field_2d_rt, localDE=0, farrayPtr=p_acc_rt_2d, _RC)
                  call ESMF_FieldRedist( acc_field,  acc_field_2d_rt, RH, _RC)
                  if (mapl_am_i_root()) then
                     call this%file_handle%put_var(trim(item%xname),p_acc_rt_2d(1:nx),&
                          start=[is],count=[nx])
                  end if
               else if (rank==2) then
                  call ESMF_FieldGet( acc_field, localDE=0, farrayPtr=p_acc_3d, _RC)
                  call ESMF_FieldGet( acc_field_3d_rt, localDE=0, farrayPtr=p_acc_rt_3d, _RC)
                  call ESMF_FieldRedist( acc_field,  acc_field_3d_rt, RH, _RC)
                  if (mapl_am_i_root()) then
                     !!write(6,'(10f8.2)') p_acc_rt_3d(:,:)
                     !!write(6,*) 'here in append_file:  put_var 3d'
                     call this%file_handle%put_var(trim(item%xname),p_acc_rt_3d(:,:),&
                          start=[is,1],count=[nx,size(p_acc_3d,2)])
                  end if
               endif
            else if (item%itemType == ItemTypeVector) then
               _FAIL("ItemTypeVector not yet supported")
            end if
            call iter%next()
         enddo
         call ESMF_FieldDestroy(acc_field_2d_rt, noGarbage=.true., _RC)
         call ESMF_FieldDestroy(acc_field_3d_rt, noGarbage=.true., _RC)
         call ESMF_FieldRedistRelease(RH, noGarbage=.true., _RC)

         print*, 'end append_file, nobs_epoch=', nx
         print*, __LINE__, __FILE__
         write(6,'(//)')

         _RETURN(_SUCCESS)

       end procedure append_file


      module procedure reset_times_to_current_day

         integer :: i,status,h,m,yp,mp,dp,s,ms,us,ns
         type(ESMF_Clock) :: clock
         type(ESMF_Time) :: current_time
         integer :: year,month,day

         call this%time_info%get(clock=clock,_RC)
         call ESMF_ClockGet(clock,currtime=current_time,_RC)
         call ESMF_TimeGet(current_time,yy=year,mm=month,dd=day,_RC)
         do i=1,size(this%times)
            call ESMF_TimeGet(this%times(i),yy=yp,mm=mp,dd=dp,h=h,m=m,s=s,ms=ms,us=us,ns=ns,_RC)
            call ESMF_TimeSet(this%times(i),yy=year,mm=month,dd=day,h=h,m=m,s=s,ms=ms,us=us,ns=ns,_RC)
         enddo

      end procedure reset_times_to_current_day


      module procedure sort_three_arrays_by_time
        integer :: status
        integer :: i, len
        integer, allocatable :: IA(:)
        integer(ESMF_KIND_I8), allocatable :: IX(:)
        real(ESMF_KIND_R8), allocatable :: X(:)

        _ASSERT (size(U)==size(V), 'U,V different dimension')
        _ASSERT (size(U)==size(T), 'U,T different dimension')
        len = size (T)

         allocate (IA(len), IX(len), X(len))
         do i=1, len
            IX(i)=T(i)
            IA(i)=i
         enddo
         call MAPL_Sort(IX,IA)

         X = U
         do i=1, len
            U(i) = X(IA(i))
         enddo
         X = V
         do i=1, len
            V(i) = X(IA(i))
         enddo
         X = T
         do i=1, len
            T(i) = X(IA(i))
         enddo

         _RETURN(_SUCCESS)
       end procedure sort_three_arrays_by_time



       module procedure time_real_to_ESMF
         type(ESMF_TimeInterval) :: interval
         type(ESMF_Time) :: time0
         type(ESMF_Time) :: time1
         character(len=:), allocatable :: tunit
         character(len=ESMF_MAXSTR) :: datetime_units
         integer :: i, len
         integer :: int_time
         integer :: status

         datetime_units = this%datetime_units
         len = size (this%times_R8)
         do i=1, len
            int_time = this%times_R8(i)
            call convert_NetCDF_DateTime_to_ESMF(int_time, datetime_units, interval, time0, time1=time1, tunit=tunit, _RC)
            this%times(i) = time1
         enddo

         _RETURN(_SUCCESS)
       end procedure time_real_to_ESMF



        module procedure create_grid
         character(len=ESMF_MAXSTR) :: filename
         type(NetCDF4_FileFormatter) :: formatter
         type(FileMetadataUtils) :: metadata_utils
         type(FileMetadata) :: fmd
         !!integer(ESMF_KIND_I8) :: num_times
         integer(ESMF_KIND_I4) :: num_times
         integer :: ncid, ncid0
         integer :: dimid(10),  dimlen(10)
         integer :: len
         integer :: len_full
         integer :: status

         character(len=ESMF_MAXSTR) :: grp_name
         character(len=ESMF_MAXSTR) :: dim_name(10)
         character(len=ESMF_MAXSTR) :: var_name_lon
         character(len=ESMF_MAXSTR) :: var_name_lat
         character(len=ESMF_MAXSTR) :: var_name_time

         type(ESMF_Config) :: config_grid
         character(len=ESMF_MAXSTR) :: time_string

         real(kind=REAL64), allocatable :: lons_full(:), lats_full(:)
         real(kind=REAL64), allocatable :: times_R8_full(:)
         real(kind=REAL64), allocatable :: XA(:)

         integer(ESMF_KIND_I4), pointer :: ptAI(:), ptBI(:)
         real(ESMF_KIND_R8), pointer :: ptAT(:), ptBT(:)
         type(ESMF_routehandle) :: RH
         type(ESMF_Time) :: timeset(2)
         type(ESMF_Time) :: current_time
         type(ESMF_Field) :: src_fld, dst_fld
         type(ESMF_Field) :: src_fld2, dst_fld2
         type(ESMF_Grid) :: grid

         type(ESMF_VM) :: vm
         integer :: mypet, petcount

         integer :: i, j, k, L
         integer :: is, ie
         integer(kind=ESMF_KIND_I8) :: j0, j1
         integer(kind=ESMF_KIND_I8) :: jt1, jt2
         integer(kind=ESMF_KIND_I8) :: nstart, nend
         real(kind=ESMF_KIND_R8) :: jx0, jx1
         integer :: nx, nx_sum
         integer :: arr(1)
         integer :: sec

         ! TOBE removed: hard coded tunits
         this%datetime_units = "seconds since 1970-01-01 00:00:00"


         if (this%nc_index == '') then
            filename=trim(this%obsFile)
            !!         print*, 'obs file name=', trim(filename)
            call formatter%open(trim(filename),pFIO_READ,_RC)
            fmd = formatter%read(_RC)
            call metadata_utils%create(fmd,trim(filename))
            num_times = metadata_utils%get_dimension("time",_RC)
            allocate(this%lons(num_times),this%lats(num_times),_STAT)
            if (metadata_utils%is_var_present("longitude")) then
               call formatter%get_var("longitude",this%lons,_RC)
            end if
            if (metadata_utils%is_var_present("latitude")) then
               call formatter%get_var("latitude",this%lats,_RC)
            end if
            call metadata_utils%get_time_info(timeVector=this%times,_RC)
         else
            i=index(this%nc_longitude, '/')
            _ASSERT (i>0, 'group name not found')
            grp_name = this%nc_longitude(1:i-1)
            this%var_name_lat = this%nc_latitude(i+1:)
            this%var_name_lon = this%nc_longitude(i+1:)
            this%var_name_time= this%nc_time(i+1:)


            ! __ loop obsfile_index for epoch
            !    get max len, allocate array, concatenate , get_var
            !
            L=0
            is=this%obsfile_Ts_index   ! from call get_obsfile_Tbracket_from_epoch
            ie=this%obsfile_Te_index
            if(ie < L) then
               allocate(this%lons(0),this%lats(0),_STAT)
               allocate(this%times_R8(0),_STAT)
               this%epoch_index(1:2)=0
               this%nobs_epoch = 0
               rc=0
               return
            end if

            ! -- this is all  ie >= L case
            !    get bounds, get_var
            j = max (is, L)
            len = 0
            do while (j<=ie)
               filename = this%get_filename_from_template_use_index(j, _RC)
               !!call get_ncfile_dimension_I8(filename, tdim=num_times, key_time=this%nc_index, _RC)
               call get_ncfile_dimension(filename, tdim=num_times, key_time=this%nc_index, _RC)
               len = len + num_times
               j=j+1
               if (mapl_am_I_root()) write(6,*) 'input filename=', trim(filename)
            enddo
            len_full = len
            write(6,*) 'len_full=', len_full
            allocate(lons_full(len),lats_full(len),_STAT)
            allocate(times_R8_full(len),_STAT)

            j = max (is, L)
            len = 0
            do while (j<=ie)
               filename = this%get_filename_from_template_use_index(j, _RC)
               call get_ncfile_dimension(trim(filename), tdim=num_times, key_time=this%nc_index, _RC)
               call get_v1d_netcdf_R8 (filename, this%var_name_lon,  lons_full(len+1:), num_times, group_name=grp_name)
               call get_v1d_netcdf_R8 (filename, this%var_name_lat,  lats_full(len+1:), num_times, group_name=grp_name)
               call get_v1d_netcdf_R8 (filename, this%var_name_time, times_R8_full(len+1:), num_times, group_name=grp_name)
               len = len + num_times
               j=j+1
            enddo

            call ESMF_VMGetGlobal(vm,_RC)
            call ESMF_VMGet(vm, localPet=mypet, petCount=petCount, _RC)


            !__ epoch grid on root
            !
            if (mapl_am_I_root()) then
               call sort_three_arrays_by_time(lons_full, lats_full, times_R8_full, _RC)
               call ESMF_ClockGet(this%clock,currTime=current_time,_RC)
               timeset(1) = current_time
               timeset(2) = current_time + this%epoch_frequency
               call time_esmf_2_nc_int (timeset(1), this%datetime_units, j0, _RC)
               call hms_2_s (this%Epoch, sec, _RC)
               j1 = j0 + sec
               jx0 = real ( j0, kind=ESMF_KIND_R8)
               jx1 = real ( j1, kind=ESMF_KIND_R8)
               !!call lgr%debug ('%a %i4 %i4', 'jx0, jx1', jx0, jx1)
               write(6,*) 'jx0, jx1', jx0, jx1

               nstart=1; nend=size(times_R8_full)
               call bisect( times_R8_full, jx0, jt1, n_LB=int(nstart, ESMF_KIND_I8), n_UB=int(nend, ESMF_KIND_I8), rc=rc)
               call bisect( times_R8_full, jx1, jt2, n_LB=int(nstart, ESMF_KIND_I8), n_UB=int(nend, ESMF_KIND_I8), rc=rc)
               if (jt1==jt2) then
                  _FAIL('Epoch Time is too small, empty swath grid is generated, increase Epoch')
               endif
               ! (x1, x2]  design in bisect
               if (this%epoch_index(1) == 0) then
                  if (jt1==0) then
                     this%epoch_index(1)= 1
                  else
                     this%epoch_index(1)= jt1
                  endif
               else
                  this%epoch_index(1)= this%epoch_index(2)  !  previous save
               end if
!               if (jt2<=nend) then
                  this%epoch_index(2)= jt2
!               else
!                  this%epoch_index(2)= nend
!               endif

               nx= this%epoch_index(2) - this%epoch_index(1) + 1
               this%nobs_epoch = nx
               allocate(this%lons(nx),this%lats(nx),_STAT)
               allocate(this%times_R8(nx),_STAT)


               write(6,*) 'jx0, jx1', jx0, jx1
               write(6,*) 'full time array, nstart, nend', nstart, nend
               write(6,*) 'epoch_index(1:2), nx', this%epoch_index(1:2), this%nobs_epoch

               j=this%epoch_index(1)
               do i=1, nx
                  this%lons(i) = lons_full(j)
                  this%lats(i) = lats_full(j)
                  this%times_R8(i) = times_R8_full(j)
                  j=j+1
               enddo
               arr(1)=nx
            else
               allocate(this%lons(0),this%lats(0),_STAT)
               allocate(this%times_R8(0),_STAT)
               this%epoch_index(1:2)=0
               this%nobs_epoch = 0
               nx=0
               arr(1)=nx
            endif


            call ESMF_VMAllFullReduce(vm, sendData=arr, recvData=nx_sum, &
                 count=1, reduceflag=ESMF_REDUCE_SUM, rc=rc)
            write(6,*) 'epoch_index(1:2), nx', this%epoch_index(1:2), this%nobs_epoch
            write(6,*) 'nx_sum', nx_sum
            this%nobs_epoch_sum = nx_sum


            this%locstream_factory = LocStreamFactory(this%lons,this%lats,_RC)
            this%LS_rt = this%locstream_factory%create_locstream(_RC)
            call ESMF_FieldBundleGet(this%bundle,grid=grid,_RC)
            this%LS_ds = this%locstream_factory%create_locstream(grid=grid,_RC)   ! use background grid

            this%fieldA = ESMF_FieldCreate (this%LS_rt, name='A_time', typekind=ESMF_TYPEKIND_R8, _RC)
            this%fieldB = ESMF_FieldCreate (this%LS_ds, name='B_time', typekind=ESMF_TYPEKIND_R8, _RC)

            call ESMF_FieldGet( this%fieldA, localDE=0, farrayPtr=ptAT)
            call ESMF_FieldGet( this%fieldB, localDE=0, farrayPtr=this%obsTime)
            if (mypet == 0) then
               ptAT(:) = this%times_R8(:)
            end if
            this%obsTime= -1.d0

            call ESMF_FieldRedistStore (this%fieldA, this%fieldB, RH, _RC)
            call ESMF_FieldRedist      (this%fieldA, this%fieldB, RH, _RC)

            !!write(6,'(2x,a,10E20.11)') 'obstime bf destroy'
            !!write(6,'(2x,a,i5,2x,10E20.11)')  'pet=', mypet, this%obsTime(1:10)

            call ESMF_FieldRedistRelease(RH, noGarbage=.true., _RC)
            call ESMF_FieldDestroy(this%fieldA,nogarbage=.true.,_RC)
            ! defer destroy fieldB at regen_grid step

            !!write(6,'(2x,a,10E20.11)') 'obstime af destroy'
            !!write(6,'(2x,a,i5,2x,10E20.11)')  'pet=', mypet, this%obsTime(1:10)

            print*, 'end create_grid'

         end if

         deallocate(lons_full, lats_full, times_R8_full)


         _RETURN(_SUCCESS)
       end procedure create_grid
            !! debug
            !!write(6, '(a)')  'lons_full(1:3*num_times:num_times) and  times_R8_full'
            !!write(6, '(10f10.1)')  lons_full(1:3*num_times:num_times/2)
            !!write(6, '(10E25.12)')  times_R8_full(1:3*num_times:num_times/2)
            !!write(6, '(10E25.12)')  times_R8_full(1:len:20)





         module procedure regrid_accumulate_on_xsubset
           integer                 :: x_subset(2)
           type(ESMF_Time)         :: timeset(2)
           type(ESMF_Time)         :: current_time
           type(ESMF_TimeInterval) :: dur
           type(GriddedIOitemVectorIterator) :: iter
           type(GriddedIOitem), pointer :: item
           type(ESMF_Field) :: src_field,dst_field,acc_field
           integer :: rank
           real(kind=REAL32), allocatable :: p_new_lev(:,:,:)
           real(kind=REAL32), pointer :: p_src_3d(:,:,:),p_src_2d(:,:)
           real(kind=REAL32), pointer :: p_dst_3d(:,:),p_dst_2d(:)
           real(kind=REAL32), pointer :: p_acc_3d(:,:),p_acc_2d(:)
           integer :: is, ie
           integer :: status

           ! __ s1.  get x_subset for each model time step interval on each pet
           call ESMF_ClockGet(this%clock,currTime=current_time,_RC)
           call ESMF_ClockGet(this%clock,timeStep=dur, _RC )
           timeset(1) = current_time - dur
           timeset(2) = current_time
           call this%get_x_subset(timeset, x_subset, _RC)
           is=x_subset(1)
           ie=x_subset(2)

           if (this%vdata%regrid_type==VERTICAL_METHOD_ETA2LEV) then
              call this%vdata%setup_eta_to_pressure(_RC)
           endif


           ! __ s2. regrid & accumulate
           if (is > 0 .AND. is <= ie ) then
           iter = this%items%begin()
           do while (iter /= this%items%end())
              item => iter%get()
              if (item%itemType == ItemTypeScalar) then
                 call ESMF_FieldBundleGet(this%bundle,trim(item%xname),field=src_field,_RC)
                 call ESMF_FieldBundleGet(this%output_bundle,trim(item%xname),field=dst_field,_RC)
                 call ESMF_FieldBundleGet(this%acc_bundle,trim(item%xname),field=acc_field,_RC)
                 call ESMF_FieldGet(src_field,rank=rank,_RC)
                 if (rank==2) then
                    call ESMF_FieldGet(src_field,farrayptr=p_src_2d,_RC)
                    call ESMF_FieldGet(dst_field,farrayptr=p_dst_2d,_RC)
                    call ESMF_FieldGet(acc_field,farrayptr=p_acc_2d,_RC)

                    print*, 'size(src,dst,acc)', size(p_src_2d), size(p_dst_2d), size(p_acc_2d)
                    call this%regridder%regrid(p_src_2d,p_dst_2d,_RC)
                    p_acc_2d(is:ie) = p_dst_2d(is:ie)

                    !!if (is>0) write(6,'(a)')  'regrid_accu:  p_dst_2d'
                    !!if (is>0) write(6,'(10f7.1)')  p_dst_2d

                 else if (rank==3) then
                    call ESMF_FieldGet(src_field,farrayptr=p_src_3d,_RC)
                    call ESMF_FieldGet(dst_field,farrayptr=p_dst_3d,_RC)
                    call ESMF_FieldGet(acc_field,farrayptr=p_acc_3d,_RC)
                    if (this%vdata%regrid_type==VERTICAL_METHOD_ETA2LEV) then
                       allocate(p_new_lev(size(p_src_3d,1),size(p_src_3d,2),this%vdata%lm),_STAT)
                       call this%vdata%regrid_eta_to_pressure(p_src_3d,p_new_lev,_RC)
                       call this%regridder%regrid(p_new_lev,p_dst_3d,_RC)
                       p_acc_3d(is:ie,:) = p_dst_3d(is:ie,:)
                    else
                       call this%regridder%regrid(p_src_3d,p_dst_3d,_RC)
                       p_acc_3d(is:ie,:) = p_dst_3d(is:ie,:)
                    end if
                 end if
              else if (item%itemType == ItemTypeVector) then
                 _FAIL("ItemTypeVector not yet supported")
              end if
              call iter%next()
           enddo
        endif
           _RETURN(ESMF_SUCCESS)

         end procedure regrid_accumulate_on_xsubset


         module procedure get_x_subset
           type   (ESMF_Time)    :: T1,  T2
           real   (ESMF_KIND_R8) :: rT1, rT2

           integer(ESMF_KIND_I8) :: i1,  i2
           integer(ESMF_KIND_I8) :: jt1, jt2, lb, ub
           integer               :: jlo, jhi
           integer               :: status

           T1= interval(1)
           T2= interval(2)

           ! __ s1. find index on each PET
           !
           !              T1 <--> T2
           !      --------------------------->
           !         |                |
           !   epoch_index(1)        (2)

             call time_esmf_2_nc_int (T1, this%datetime_units, i1, _RC)
             call time_esmf_2_nc_int (T2, this%datetime_units, i2, _RC)
             rT1=real(i1, kind=ESMF_KIND_R8)
             rT2=real(i2, kind=ESMF_KIND_R8)
             jlo = 1
             jhi= size(this%obstime)
             if (jhi==0) then
                x_subset(1:2)=0
                _RETURN(_SUCCESS)
             endif

             !!write(6,*) 'jlo, jhi in obstime', jlo, jhi
             !!write(6,'(2x,a,2i15)') 'time/sec: i1, i2', i1, i2
             !!write(6,'(2x,a,2f22.11)') 'obstime(1:n) in get_x_subset'
             !!write(6,'(2x,5E22.11)') this%obstime(jlo), this%obstime((jhi+jhi)/2), this%obstime(jhi)

             !
             ! bisect design
             !    y(n)  < x(key) <=  y(n+1)
             !
             !     (x1, x2 ]
             !   o--o-o-o-o--o--o--o--
             !     |          |
             !
             !

             lb=int(jlo, ESMF_KIND_I8)
             ub=int(jhi, ESMF_KIND_I8)
             call bisect( this%obstime, rT1, jt1, n_LB=lb, n_UB=ub, rc=rc)
             call bisect( this%obstime, rT2, jt2, n_LB=lb, n_UB=ub, rc=rc)
!             if (jt1<lb .OR. jt1>=ub) jt1=0   ! out of bounds
!             if (jt2<lb .OR. jt2>=ub) jt2=0   ! out of bounds


             x_subset(1) = jt1
             !
             !  correction: for inclusive end
             !
             if (jt1<lb) then
                if (jt2<lb) then
                   x_subset(1) = 0
                   x_subset(2) = 0
                elseif (jt2>=ub) then
                   x_subset(1) = lb
                   x_subset(2) = ub
                else
                   x_subset(1) = lb
                   x_subset(2) = jt2
                endif
             elseif (jt1>=ub) then
                x_subset(1) = 0
                x_subset(2) = 0
             else
                x_subset(1) = jt1
                if (jt2>=ub) then
                   x_subset(2) = ub
                else
                   x_subset(2) = jt2
                endif
             endif

             print*, 'x_subset(1:2)', x_subset(1:2)

           _RETURN(_SUCCESS)
         end procedure get_x_subset


         module procedure destroy_rh_regen_LS
           integer :: status

           integer :: numVars, i
           character(len=ESMF_MAXSTR), allocatable :: names(:)
           type(ESMF_Field) :: field
           type(ESMF_Grid)  :: grid
           type(ESMF_Time)  :: currTime

           ! __ s1. destroy RH, LS, acc_bundle / output_bundle
           call ESMF_FieldDestroy(this%fieldB,nogarbage=.true.,_RC)
           call this%regridder%destroy(_RC)
           call this%locstream_factory%destroy_locstream(this%LS_rt, _RC)
           call this%locstream_factory%destroy_locstream(this%LS_ds, _RC)
           deallocate (this%lons, this%lats, this%times_R8)

           call ESMF_FieldBundleGet(this%acc_bundle,fieldCount=numVars,_RC)
           allocate(names(numVars),stat=status)
           call ESMF_FieldBundleGet(this%acc_bundle,fieldNameList=names,_RC)
           do i=1,numVars
              call ESMF_FieldBundleGet(this%acc_bundle,trim(names(i)),field=field,_RC)
              call ESMF_FieldDestroy(field,noGarbage=.true., _RC)
           enddo
           call ESMF_FieldBundleDestroy(this%acc_bundle,noGarbage=.true.,_RC)

           call ESMF_FieldBundleGet(this%output_bundle,fieldCount=numVars,_RC)
           allocate(names(numVars),stat=status)
           call ESMF_FieldBundleGet(this%output_bundle,fieldNameList=names,_RC)
           do i=1,numVars
              call ESMF_FieldBundleGet(this%output_bundle,trim(names(i)),field=field,_RC)
              call ESMF_FieldDestroy(field,noGarbage=.true., _RC)
           enddo
           call ESMF_FieldBundleDestroy(this%output_bundle,noGarbage=.true.,_RC)

           ! __ s2. regenerate LS, RH, bundles
           call ESMF_ClockGet ( this%clock, CurrTime=currTime, _RC )
           call this%get_obsfile_Tbracket_from_epoch(currTime, _RC)
           call this%create_grid(_RC)  !  setup LS_rt, LS_ds
           call ESMF_FieldBundleGet(this%bundle,grid=grid,_RC)
           this%regridder = LocStreamRegridder(grid,this%LS_ds,_RC)
           this%output_bundle = this%create_new_bundle(_RC)
           this%acc_bundle    = this%create_new_bundle(_RC)

           ! __ s3. Epoch reset
           this%epoch_index(1:2)=0

           _RETURN(ESMF_SUCCESS)

         end procedure destroy_rh_regen_LS


         module procedure get_obsfile_Tbracket_from_epoch
           implicit none
           integer :: status
           integer :: numVars, i
           character(len=ESMF_MAXSTR), allocatable :: names(:)
           type(ESMF_Field) :: field
           type(ESMF_Grid)  :: grid

           type(ESMF_Time)  :: T1, Tn
           type(ESMF_Time)  :: cT1, cTn
           type(ESMF_Time)  :: Ts, Te
           type(ESMF_TimeInterval)  :: dT1, dT2, dT3, dTs, dTe
           real(ESMF_KIND_R8) :: Tint_r8
           real(ESMF_KIND_R8) :: Tint2_r8
           real(ESMF_KIND_R8) :: dT0_s, dT1_s, dT2_s, dT3_s
           real(ESMF_KIND_R8) :: s1, s2
           integer :: n1, n2
           integer :: K

         ! get obs file index:  n1, n2
         ! get obs file content:
         !   given  traj%obsfile_Template
         !          traj%obsfile_begin
         !          traj%obsfile_interval
         !          currTime
         !          EpochTime
         !   output:
         !          nfile, filename(nfile) : time_esmf(nfile)

           T1 = this%obsfile_start_time
           Tn = this%obsfile_end_time

           cT1 = currTime
           dT1 = currTime - T1
           dT2 = currTime + this%epoch_frequency - T1

           call ESMF_TimeIntervalGet(this%obsfile_interval, s_r8=dT0_s, rc=status)
           call ESMF_TimeIntervalGet(dT1, s_r8=dT1_s, rc=status)
           call ESMF_TimeIntervalGet(dT2, s_r8=dT2_s, rc=status)
           n1 = floor (dT1_s / dT0_s)
           n2 = floor (dT2_s / dT0_s)
           s1 = n1 * dT0_s
           s2 = n2 * dT0_s
           call ESMF_TimeIntervalSet(dTs, s_r8=s1, rc=status)
           call ESMF_TimeIntervalSet(dTe, s_r8=s2, rc=status)
           Ts = T1 + dTs
           Te = T1 + dTe

           !! debug
           !!write(6,*) 'ck: due to epoch,  at curr_time, time bracket for files:  '
           !!!write(6,*) 'ck: due to epoch,  integer multiple of obsfile_interval', n1, n2

           this%obsfile_Ts_index = n1
           this%obsfile_Te_index = n2

           !! -- prone to error
           !!dT3 = Tn - T1
           !!call ESMF_TimeIntervalGet(dT3, s_r8=dT3_s, rc=status)
           !!K   = floor (dT3_s / dT0_s)
           !!if (n1>=0 .AND. n2<=K) then
           !!   this%obsfile_is_available = .true.
           !!else
           !!   this%obsfile_is_available = .false.
           !!end if

           _RETURN(ESMF_SUCCESS)
         end procedure get_obsfile_Tbracket_from_epoch


         module procedure  get_filename_from_template
            integer :: itime(2)
            integer :: nymd, nhms
            integer :: status

            stop 'DO not use get_filename_from_template'
            call ESMF_time_to_two_integer(time, itime, _RC)
            print*, 'two integer time,  itime(:)', itime(1:2)
            nymd = itime(1)
            nhms = itime(2)
            call fill_grads_template ( filename, file_template, &
                 experiment_id='', nymd=nymd, nhms=nhms, _RC )
           print*, 'ck: this%obsFile_T=', trim(filename)
           _RETURN(ESMF_SUCCESS)
         end procedure


         module procedure  get_filename_from_template_use_index
            integer :: itime(2)
            integer :: nymd, nhms
            integer :: status
            real(ESMF_KIND_R8) :: dT0_s
            real(ESMF_KIND_R8) :: s
            type(ESMF_TimeInterval) :: dT
            type(ESMF_Time)         :: time


            call ESMF_TimeIntervalGet(this%obsfile_interval, s_r8=dT0_s, rc=status)
            s = dT0_s * f_index
            call ESMF_TimeIntervalSet(dT, s_r8=s, rc=status)
            time = this%obsfile_start_time + dT

            call ESMF_time_to_two_integer(time, itime, _RC)
            !!print*, 'two integer time,  itime(:)', itime(1:2)
            nymd = itime(1)
            nhms = itime(2)
            call fill_grads_template ( filename, this%obsfile_template, &
                 experiment_id='', nymd=nymd, nhms=nhms, _RC )
            !!print*, 'ck: this%obsFile_T=', trim(filename)

            _RETURN(ESMF_SUCCESS)

         end procedure


end submodule HistoryTrajectory_implement
