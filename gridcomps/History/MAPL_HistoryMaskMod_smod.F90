#include "MAPL_ErrLog.h"
#include "unused_dummy.H"

submodule (MaskSamplerMod)  MaskSampler_implement
  implicit none
contains
     module procedure MaskSampler_from_config
         use BinIOMod
         use pflogger, only         :  Logger, logging
         type(ESMF_Time)            :: currTime
         type(ESMF_TimeInterval)    :: epoch_frequency
         type(ESMF_TimeInterval)    :: obs_time_span
         integer                    :: time_integer, second
         integer                    :: status
         character(len=ESMF_MAXSTR) :: STR1, line
         character(len=ESMF_MAXSTR) :: symd, shms
         integer                    :: nline, col
         integer, allocatable       :: ncol(:)
         character(len=ESMF_MAXSTR), allocatable :: word(:)
         integer                    :: nobs, head, jvar
         logical                    :: tend
         integer                    :: i, j, k, M
         integer                    :: count
         integer                    :: unitr, unitw
         type(Logger), pointer :: lgr

         mask%clock=clock
         call ESMF_ClockGet ( clock, CurrTime=currTime, _RC )
         call ESMF_ConfigGetAttribute(config, value=time_integer, label=trim(string)//'Epoch:', default=0, _RC)
         _ASSERT(time_integer /= 0, 'Epoch value in config wrong')
         second = hms_2_s(time_integer)
         call ESMF_TimeIntervalSet(epoch_frequency, s=second, _RC)
         mask%Epoch = time_integer
         mask%RingTime = currTime
         mask%epoch_frequency = epoch_frequency
         mask%alarm = ESMF_AlarmCreate( clock=clock, RingInterval=epoch_frequency, &
              RingTime=mask%RingTime, sticky=.false., _RC )

         call ESMF_ConfigGetAttribute(config, value=mask%index_name_lon, default="", &
              label=trim(string) // 'index_name_lon:', rc=rc)
         call ESMF_ConfigGetAttribute(config, value=mask%index_name_lat, default="", &
              label=trim(string) // 'index_name_lat:', rc=rc)
         call ESMF_ConfigGetAttribute(config, value=mask%index_name_loc, default="", &
              label=trim(string) // 'index_name_loc:', rc=rc)
         call ESMF_ConfigGetAttribute(config, value=mask%var_name_time, default="", &
              label=trim(string) // 'var_name_time:', rc=rc)
         call ESMF_ConfigGetAttribute(config, value=mask%var_name_lon, default="", &
              label=trim(string) // 'var_name_lon:', rc=rc)
         call ESMF_ConfigGetAttribute(config, value=mask%var_name_lat, default="", &
              label=trim(string) // 'var_name_lat:', rc=rc)
         call ESMF_ConfigGetAttribute(config, value=mask%datetime_units, default="", &
              label=trim(string) // 'tunit:', _RC)

         call ESMF_ConfigGetAttribute(config, value=STR1, default="", &
              label=trim(string) // 'obs_file_begin:', _RC)
         if (trim(STR1)=='') then
            mask%obsfile_start_time = currTime
            call ESMF_TimeGet(currTime, timestring=STR1, _RC)
            if (mapl_am_I_root()) then
               write(6,105) 'obs_file_begin missing, default = currTime :', trim(STR1)
            endif
         else
            call ESMF_TimeSet(mask%obsfile_start_time, STR1, _RC)
            if (mapl_am_I_root()) then
               write(6,105) 'obs_file_begin provided: ', trim(STR1)
            end if
         end if

         call ESMF_ConfigGetAttribute(config, value=STR1, default="", &
              label=trim(string) // 'obs_file_end:', _RC)
         if (trim(STR1)=='') then
            call ESMF_TimeIntervalSet(obs_time_span, d=14, _RC)
            mask%obsfile_end_time = mask%obsfile_start_time + obs_time_span
            call ESMF_TimeGet(mask%obsfile_end_time, timestring=STR1, _RC)
            if (mapl_am_I_root()) then
               write(6,105) 'obs_file_end   missing, default = begin+14D:', trim(STR1)
            endif
         else
            call ESMF_TimeSet(mask%obsfile_end_time, STR1, _RC)
            if (mapl_am_I_root()) then
               write(6,105) 'obs_file_end provided:', trim(STR1)
            end if
         end if

         call ESMF_ConfigGetAttribute(config, value=STR1, default="", &
              label=trim(string) // 'obs_file_interval:', _RC)
         _ASSERT(STR1/='', 'fatal error: obs_file_interval not provided in RC file')
         if (mapl_am_I_root()) write(6,105) 'obs_file_interval:', trim(STR1)
         if (mapl_am_I_root()) write(6,106) 'Epoch (second)   :', second

         i= index( trim(STR1), ' ' )
         if (i>0) then
            symd=STR1(1:i-1)
            shms=STR1(i+1:)
         else
            symd=''
            shms=trim(STR1)
         endif
         call convert_twostring_2_esmfinterval (symd, shms,  mask%obsfile_interval, _RC)
         mask%is_valid = .true.

         ! __ s1. overall print
         call ESMF_ConfigGetAttribute(config, mask%input_template, &
              label=trim(string)//'obs_files:', default='unknown.txt', _RC)

         ! __ s2. metadata and file_handle
         allocate (mask%metadata)
         if (mapl_am_i_root()) then
            allocate (mask%file_handle)
         end if

         _RETURN(_SUCCESS)

105      format (1x,a,2x,a)
106      format (1x,a,2x,i8)
       end procedure MaskSampler_from_config


       !
       !-- integrate both initialize and reinitialize
       !
       module procedure initialize
         integer :: status
         type(ESMF_Grid) :: grid
         type(variable) :: v
         type(GriddedIOitemVectorIterator) :: iter
         type(GriddedIOitem), pointer :: item
         type(ESMF_Time)            :: currTime
         integer :: k

         if (.not. present(reinitialize)) then
            if(present(bundle))   this%bundle=bundle
            if(present(items))    this%items=items
            if(present(timeInfo)) this%time_info=timeInfo
            if (present(vdata)) then
               this%vdata=vdata
            else
               this%vdata=VerticalData(_RC)
            end if
         else
            if (reinitialize) then
               allocate (this%metadata)
               if (mapl_am_i_root()) then
                  allocate (this%file_handle)
               end if
            end if
         end if

!-- wrong, fix it later
         call this%vdata%append_vertical_metadata(this%metadata,this%bundle,_RC)

         this%do_vertical_regrid = (this%vdata%regrid_type /= VERTICAL_METHOD_NONE)
         if (this%vdata%regrid_type == VERTICAL_METHOD_ETA2LEV) call this%vdata%get_interpolating_variable(this%bundle,_RC)

         call ESMF_ClockGet (this%clock, CurrTime=currTime, _RC)
         call get_obsfile_Tbracket_from_epoch(currTime, &
              this%obsfile_start_time, this%obsfile_end_time, &
              this%obsfile_interval, this%epoch_frequency, &
              this%obsfile_Ts_index, this%obsfile_Te_index, _RC)
         if (this%obsfile_Te_index < 0) then
            if (mapl_am_I_root()) then
               write(6,*) "model start time is earlier than obsfile_start_time"
               write(6,*) "solution: adjust obsfile_start_time and Epoch in rc file"
            end if
            _FAIL("obs file not found at init time")
         endif


         call this%create_grid(_RC)
!
!!!         call ESMF_FieldBundleGet(this%bundle,grid=grid,_RC)
!!!         this%regridder = LocStreamRegridder(grid,this%LS_ds,_RC)
!         this%output_bundle = this%create_new_bundle(_RC)
!         this%acc_bundle    = this%create_new_bundle(_RC)
!
!
!         do k=1, this%nobs_type
!            call this%obs(k)%metadata%add_dimension(this%index_name_loc, this%obs(k)%nobs_epoch)
!            if (this%time_info%integer_time) then
!               v = Variable(type=PFIO_INT32,dimensions=this%index_name_loc)
!            else
!               v = Variable(type=PFIO_REAL32,dimensions=this%index_name_loc)
!            end if
!            call v%add_attribute('units', this%datetime_units)
!            call v%add_attribute('long_name', 'dateTime')
!            call this%obs(k)%metadata%add_variable(this%var_name_time,v)
!
!            v = variable(type=PFIO_REAL64,dimensions=this%index_name_loc)
!            call v%add_attribute('units','degrees_east')
!            call v%add_attribute('long_name','longitude')
!            call this%obs(k)%metadata%add_variable(this%var_name_lon,v)
!
!            v = variable(type=PFIO_REAL64,dimensions=this%index_name_loc)
!            call v%add_attribute('units','degrees_north')
!            call v%add_attribute('long_name','latitude')
!            call this%obs(k)%metadata%add_variable(this%var_name_lat,v)
!         end do
!
!         ! push varible names down to each obs(k); see create_metadata_variable
!         iter = this%items%begin()
!         do while (iter /= this%items%end())
!            item => iter%get()
!            if (item%itemType == ItemTypeScalar) then
!               call this%create_variable(item%xname,_RC)
!            else if (item%itemType == ItemTypeVector) then
!               call this%create_variable(item%xname,_RC)
!               call this%create_variable(item%yname,_RC)
!            end if
!            call iter%next()
!         enddo

         
         _RETURN(_SUCCESS)

       end procedure initialize




       ! __ this code does not handle zero observations
       !
       !
        module procedure create_grid
        use pflogger, only: Logger, logging
         character(len=ESMF_MAXSTR) :: filename
         integer(ESMF_KIND_I4) :: num_times
         integer :: len
         integer :: len_full
         integer :: status
         type(Logger), pointer :: lgr

         character(len=ESMF_MAXSTR) :: grp_name
         character(len=ESMF_MAXSTR) :: timeunits_file

         real(kind=REAL64), allocatable :: lons_full(:), lats_full(:)
         real(kind=REAL64), allocatable :: times_R8_full(:)
         integer,           allocatable :: obstype_id_full(:)

         real, allocatable :: lon_true(:,:)
         real, allocatable :: lat_true(:,:)
         real, allocatable :: scanTime(:,:)         
         
         real(ESMF_KIND_R8), pointer :: ptAT(:)
         type(ESMF_routehandle) :: RH
         type(ESMF_Time) :: timeset(2)
         type(ESMF_Time) :: currTime
         type(ESMF_Grid) :: grid

         type(ESMF_VM) :: vm
         integer :: mypet, petcount

         integer :: i, j, k, L
         integer :: fid_s, fid_e
         integer :: M_file

         integer :: nx, ny, nx_sum
         integer :: nlon, nlat
         integer :: sec


         type(ESMF_TimeInterval) :: Toff


         lgr => logging%get_logger('HISTORY.sampler')

         call ESMF_VMGetGlobal(vm,_RC)
         call ESMF_VMGet(vm, localPet=mypet, petCount=petCount, _RC)

         ! __ read in swath grid (at high Res) as LS

         if (mypet==0) then
            write(6,'(10(2x,a20,2x,a40,/))') &
                 'index_name_lon:', trim(this%index_name_lon), &
                 'index_name_lat:', trim(this%index_name_lat), &
                 'var_name_lon:',   trim(this%var_name_lon), &
                 'var_name_lat:',   trim(this%var_name_lat), &
                 'var_name_time:',  trim(this%var_name_time), &
                 'tunit:',          trim(this%datetime_units)
         end if

         if (mypet==0) then
            call ESMF_TimeIntervalSet(Toff, h=0, m=0, s=0, _RC)
            call ESMF_ClockGet (this%clock, CurrTime=currTime, _RC)
            call Find_M_files_for_currTime (currTime, &
                 this%obsfile_start_time, this%obsfile_end_time, this%obsfile_interval, &
                 this%epoch_frequency,  this%input_template, M_file, this%filenames, &
                 T_offset_in_file_content = Toff,  _RC)
            this%M_file = M_file
            write(6,'(10(2x,a20,2x,i40))') &
                 'M_file:', M_file
            do i=1, M_file
               write(6,'(10(2x,a14,i4,a2,2x,a))') &
                    'filenames(', i, '):', trim(this%filenames(i))
            end do

            ! __ s1. find obs files
            !
            !    QC for obs files:
            !
            !    --  redefine nstart to skip un-defined time value
            !    --  Scan_Start_Time =  -9999, -9999, -9999,
            !        ::  eliminate this row of data
            !

            allocate(lon_true(0,0), lat_true(0,0), scanTime(0,0))
            call read_M_files_4_swath (this%filenames(1:M_file), nx, ny, &
                 this%index_name_lon, this%index_name_lat, &
                 var_name_lon=this%var_name_lon, &
                 var_name_lat=this%var_name_lat, &
                 var_name_time=this%var_name_time, &
                 lon=lon_true, lat=lat_true, time=scanTime, &
                 Tfilter=.true., _RC)

            nlon=nx
            nlat=ny
            allocate(this%t_alongtrack(nlat))
            do j=1, nlat
               this%t_alongtrack(j) = scanTime(1,j)
            end do

            this%lons_2d = lon_true
            this%lats_2d = lat_true            

            write(6,'(a)')  'this%t_alongtrack(::50)='
            write(6,'(5f20.2)')  this%t_alongtrack(::50)

         end if
         
         _RETURN(_SUCCESS)
       end procedure create_grid



      module procedure create_new_bundle
        type(GriddedIOitemVectorIterator) :: iter
        type(GriddedIOitem), pointer :: item
        type(ESMF_Field) :: src_field,dst_field
        integer :: rank,lb(1),ub(1)
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



      module procedure create_metadata_variable
        type(ESMF_Field) :: field
        type(variable) :: v
        logical :: is_present
        integer :: field_rank, status
        character(len=ESMF_MAXSTR) :: var_name,long_name,units,vdims
        integer :: k, ig

        call ESMF_FieldBundleGet(this%bundle,vname,field=field,_RC)
        call ESMF_FieldGet(field,name=var_name,rank=field_rank,_RC)
        call ESMF_AttributeGet(field,name="LONG_NAME",isPresent=is_present,_RC)
        if ( is_present ) then
           call ESMF_AttributeGet  (FIELD, NAME="LONG_NAME",VALUE=long_name, _RC)
        else
           long_name = var_name
        endif
        call ESMF_AttributeGet(field,name="UNITS",isPresent=is_present,_RC)
        if ( is_present ) then
           call ESMF_AttributeGet  (FIELD, NAME="UNITS",VALUE=units, _RC)
        else
           units = 'unknown'
        endif
        if (field_rank==2) then
           vdims = this%index_name_loc
        else if (field_rank==3) then
           vdims = trim(this%index_name_loc)//",lev"
        end if
        v = variable(type=PFIO_REAL32,dimensions=trim(vdims))
        call v%add_attribute('units',trim(units))
        call v%add_attribute('long_name',trim(long_name))
        call v%add_attribute('missing_value',MAPL_UNDEF)
        call v%add_attribute('_FillValue',MAPL_UNDEF)
        call v%add_attribute('valid_range',(/-MAPL_UNDEF,MAPL_UNDEF/))

        do k = 1, this%nobs_type
           do ig = 1, this%obs(k)%ngeoval
              if (trim(var_name) == trim(this%obs(k)%geoval_name(ig))) then
                 call this%obs(k)%metadata%add_variable(trim(var_name),v,_RC)
              endif
           enddo
        enddo

         _RETURN(_SUCCESS)
      end procedure create_metadata_variable


      module procedure create_file_handle
         integer :: status
         integer :: k
         character(len=ESMF_MAXSTR) :: filename

         if (.NOT. this%is_valid) then
            _RETURN(ESMF_SUCCESS)
         endif

         do k=1, this%nobs_type
            call this%obs(k)%metadata%modify_dimension(this%index_name_loc, this%obs(k)%nobs_epoch)
         enddo
         if (mapl_am_I_root()) then
            do k=1, this%nobs_type
               if (this%obs(k)%nobs_epoch > 0) then
                  filename=trim(this%obs(k)%name)//trim(filename_suffix)
                  call this%obs(k)%file_handle%create(trim(filename),_RC)
                  call this%obs(k)%file_handle%write(this%obs(k)%metadata,_RC)
                  write(6,*) "Sampling to new file : ",trim(filename)
               end if
            enddo
         end if

        _RETURN(_SUCCESS)
      end procedure create_file_handle


       module procedure close_file_handle
          integer :: status
          integer :: k

          if (.NOT. this%is_valid) then
             _RETURN(ESMF_SUCCESS)
          endif

         if (mapl_am_I_root()) then
            do k=1, this%nobs_type
               if (this%obs(k)%nobs_epoch > 0) then
                  call this%obs(k)%file_handle%close(_RC)
               end if
            end do
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
         real(kind=REAL32), pointer :: p_acc_3d(:,:),p_acc_2d(:)
         real(kind=REAL32), pointer :: p_acc_rt_3d(:,:),p_acc_rt_2d(:)
         real(kind=REAL32), pointer :: p_src(:,:),p_dst(:,:)

         integer :: is, ie, nx
         integer :: lm
         integer :: rank
         integer :: status
         integer :: j, k, ig
         integer, allocatable :: ix(:)

         if (.NOT. this%is_valid) then
            _RETURN(ESMF_SUCCESS)
         endif

         if (this%nobs_dur_sum==0) then
            rc=0
            return
         endif
!
!         is=1
!         do k = 1, this%nobs_type
!            !-- limit  nx < 2**32 (integer*4)
!            nx=this%obs(k)%nobs_epoch
!            if (nx >0) then
!               if (mapl_am_i_root()) then
!                  call this%obs(k)%file_handle%put_var(this%var_name_time, real(this%obs(k)%times_R8), &
!                       start=[is], count=[nx], _RC)
!                  call this%obs(k)%file_handle%put_var(this%var_name_lon, this%obs(k)%lons, &
!                       start=[is], count=[nx], _RC)
!                  call this%obs(k)%file_handle%put_var(this%var_name_lat, this%obs(k)%lats, &
!                       start=[is], count=[nx], _RC)
!               end if
!            end if
!         enddo
!
!         ! get RH from 2d field
!         src_field = ESMF_FieldCreate(this%LS_ds,typekind=ESMF_TYPEKIND_R4,gridToFieldMap=[1],_RC)
!         dst_field = ESMF_FieldCreate(this%LS_rt,typekind=ESMF_TYPEKIND_R4,gridToFieldMap=[1],_RC)
!         call ESMF_FieldRedistStore(src_field,dst_field,RH,_RC)
!         call ESMF_FieldDestroy(src_field,noGarbage=.true.,_RC)
!         call ESMF_FieldDestroy(dst_field,noGarbage=.true.,_RC)
!
!         ! redist and put_var
!         lm = this%vdata%lm
!         acc_field_2d_rt = ESMF_FieldCreate (this%LS_rt, name='field_2d_rt', typekind=ESMF_TYPEKIND_R4, _RC)
!         acc_field_3d_rt = ESMF_FieldCreate (this%LS_rt, name='field_3d_rt', typekind=ESMF_TYPEKIND_R4, &
!              gridToFieldMap=[1],ungriddedLBound=[1],ungriddedUBound=[lm],_RC)
!
!         iter = this%items%begin()
!         do while (iter /= this%items%end())
!            item => iter%get()
!            !!write(6, '(2x,a,2x,a)') 'item%xname', trim(item%xname)
!
!            if (item%itemType == ItemTypeScalar) then
!               call ESMF_FieldBundleGet(this%acc_bundle,trim(item%xname),field=acc_field,_RC)
!               call ESMF_FieldGet(acc_field,rank=rank,_RC)
!               if (rank==1) then
!                  call ESMF_FieldGet( acc_field, localDE=0, farrayPtr=p_acc_2d, _RC)
!                  call ESMF_FieldGet( acc_field_2d_rt, localDE=0, farrayPtr=p_acc_rt_2d, _RC)
!                  call ESMF_FieldRedist( acc_field,  acc_field_2d_rt, RH, _RC)
!                  if (mapl_am_i_root()) then
!                     !
!                     !-- pack fields to obs(k)%p2d and put_var
!                     !
!                     is=1
!                     ie=this%epoch_index(2)-this%epoch_index(1)+1
!                     do k=1, this%nobs_type
!                        nx = this%obs(k)%nobs_epoch
!                        allocate (this%obs(k)%p2d(nx))
!                     enddo
!
!                     allocate(ix(this%nobs_type))
!                     ix(:)=0
!                     do j=is, ie
!                        k = this%obstype_id(j)
!                        ix(k) = ix(k) + 1
!                        this%obs(k)%p2d(ix(k)) = p_acc_rt_2d(j)
!                     enddo
!
!                     do k=1, this%nobs_type
!                        if (ix(k) /= this%obs(k)%nobs_epoch) then
!                           print*, 'obs_', k, ' : ix(k) /= this%obs(k)%nobs_epoch'
!                           print*, 'obs_', k, ' : this%obs(k)%nobs_epoch, ix(k) =', this%obs(k)%nobs_epoch, ix(k)
!                           _FAIL('test ix(k) failed')
!                        endif
!                     enddo
!                     deallocate(ix)
!                     do k=1, this%nobs_type
!                        is = 1
!                        nx = this%obs(k)%nobs_epoch
!                        if (nx>0) then
!                           do ig = 1, this%obs(k)%ngeoval
!                              if (trim(item%xname) == trim(this%obs(k)%geoval_name(ig))) then
!                                 call this%obs(k)%file_handle%put_var(trim(item%xname), this%obs(k)%p2d(1:nx), &
!                                      start=[is],count=[nx])
!                              end if
!                           enddo
!                        endif
!                     enddo
!                  end if
!               else if (rank==2) then
!                  call ESMF_FieldGet( acc_field, localDE=0, farrayPtr=p_acc_3d, _RC)
!                  call ESMF_FieldGet( acc_field_3d_rt, localDE=0, farrayPtr=p_acc_rt_3d, _RC)
!
!                  dst_field=ESMF_FieldCreate(this%LS_rt,typekind=ESMF_TYPEKIND_R4, &
!                       gridToFieldMap=[2],ungriddedLBound=[1],ungriddedUBound=[lm],_RC)
!                  src_field=ESMF_FieldCreate(this%LS_ds,typekind=ESMF_TYPEKIND_R4, &
!                       gridToFieldMap=[2],ungriddedLBound=[1],ungriddedUBound=[lm],_RC)
!
!                  call ESMF_FieldGet(src_field,localDE=0,farrayPtr=p_src,_RC)
!                  call ESMF_FieldGet(dst_field,localDE=0,farrayPtr=p_dst,_RC)
!
!                  p_src= reshape(p_acc_3d,shape(p_src), order=[2,1])
!                  call ESMF_FieldRegrid(src_field,dst_field,RH,_RC)
!                  p_acc_rt_3d=reshape(p_dst, shape(p_acc_rt_3d), order=[2,1])
!
!                  call ESMF_FieldDestroy(dst_field,noGarbage=.true.,_RC)
!                  call ESMF_FieldDestroy(src_field,noGarbage=.true.,_RC)
!
!                  if (mapl_am_i_root()) then
!                     !
!                     !-- pack fields to obs(k)%p3d and put_var
!                     !
!                     is=1
!                     ie=this%epoch_index(2)-this%epoch_index(1)+1
!                     do k=1, this%nobs_type
!                        nx = this%obs(k)%nobs_epoch
!                        allocate (this%obs(k)%p3d(nx, size(p_acc_rt_3d,2)))
!                     enddo
!                     allocate(ix(this%nobs_type))
!                     ix(:)=0
!                     do j=is, ie
!                        k = this%obstype_id(j)
!                        ix(k) = ix(k) + 1
!                        this%obs(k)%p3d(ix(k),:) = p_acc_rt_3d(j,:)
!                     enddo
!                     deallocate(ix)
!                     do k=1, this%nobs_type
!                        is = 1
!                        nx = this%obs(k)%nobs_epoch
!                        if (nx>0) then
!                           do ig = 1, this%obs(k)%ngeoval
!                              if (trim(item%xname) == trim(this%obs(k)%geoval_name(ig))) then
!                                 call this%obs(k)%file_handle%put_var(trim(item%xname), this%obs(k)%p3d(:,:), &
!                                      start=[is,1],count=[nx,size(p_acc_rt_3d,2)])
!                              end if
!                           end do
!                        endif
!                     enddo
!                     !!write(6,'(10f8.2)') p_acc_rt_3d(:,:)
!                     !!write(6,*) 'here in append_file:  put_var 3d'
!                     !!call this%obs(k)%file_handle%put_var(trim(item%xname),p_acc_rt_3d(:,:),&
!                     !!     start=[is,1],count=[nx,size(p_acc_rt_3d,2)])
!                  end if
!               endif
!            else if (item%itemType == ItemTypeVector) then
!               _FAIL("ItemTypeVector not yet supported")
!            end if
!            call iter%next()
!         enddo
!         call ESMF_FieldDestroy(acc_field_2d_rt, noGarbage=.true., _RC)
!         call ESMF_FieldDestroy(acc_field_3d_rt, noGarbage=.true., _RC)
!         call ESMF_FieldRedistRelease(RH, noGarbage=.true., _RC)
!
         _RETURN(_SUCCESS)
       end procedure append_file



       
       module procedure regrid_accumulate_on_xsubset
       use pflogger, only: Logger, logging     
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

       real(ESMF_KIND_R8), pointer :: ptA(:)
       real(ESMF_KIND_R8), pointer :: ptB(:)       
       type(ESMF_routehandle) :: RH
       type(ESMF_grid) :: grid
       integer :: i, j, k, L
       integer :: fid_s, fid_e
       integer :: M_file
       integer(kind=ESMF_KIND_I8) :: j0, j1
       integer(kind=ESMF_KIND_I8) :: jt1, jt2
       integer(kind=ESMF_KIND_I8) :: nstart, nend
       real(kind=ESMF_KIND_R8) :: jx0, jx1
       integer :: nx, ny, nx_sum
       integer :: nlon, nlat
       integer :: arr(1)
       integer :: sec

       integer :: mypet, petcount
       integer :: len
       type(Logger), pointer          :: lgr


       integer :: IM, JM, LM, COUNTS(3)
       type(ESMF_DistGrid) :: distGrid
       type(ESMF_DElayout) :: layout
       type(ESMF_VM) :: VM
       integer :: myid
       integer :: ndes
       integer :: dimCount
       integer, allocatable :: II(:)
       integer, allocatable :: JJ(:)
       real, allocatable :: obs_lons(:)
       real, allocatable :: obs_lats(:)


       lgr => logging%get_logger('HISTORY.sampler')

       if (.NOT. this%is_valid) then
          _RETURN(ESMF_SUCCESS)
       endif

       ! __ Ab. cut swath by time step, convert rectangular to LS
       !        find on each DE:
       !        - nearest CS points for swath points,  define as index1
       !        - n.n. around these index1 pts, define as index2 
       !        - merge and accumulate


       !           call this%get_x_subset(timeset, x_subset, _RC)
       !           is=x_subset(1)
       !           ie=x_subset(2)


       call ESMF_VMGetGlobal(vm,_RC)
       call ESMF_VMGet(vm, localPet=mypet, petCount=petCount, _RC)

       !            
       if (mypet == 0) then                      
          call ESMF_ClockGet(this%clock,currTime=current_time,_RC)
          call ESMF_ClockGet(this%clock,timeStep=dur, _RC )
          timeset(1) = current_time - dur
          timeset(2) = current_time

          ! __ s1. cut swath by Epoch time, convert rectangular to LS
          !
          call time_esmf_2_nc_int (timeset(1), this%datetime_units, j0, _RC)
          call ESMF_TimeIntervalGet(dur, s=sec, _RC)
          j1= j0 + sec
          jx0= j0
          jx1= j1

          !! write(6,*) 'dur in s', sec

          nstart = 1
          nend = size(this%t_alongtrack)
          call bisect( this%t_alongtrack, jx0, jt1, n_LB=int(nstart, ESMF_KIND_I8), n_UB=int(nend, ESMF_KIND_I8), rc=rc)
          call bisect( this%t_alongtrack, jx1, jt2, n_LB=int(nstart, ESMF_KIND_I8), n_UB=int(nend, ESMF_KIND_I8), rc=rc)

          call lgr%debug ('%a %i20 %i20', 'nstart, nend', nstart, nend)
          call lgr%debug ('%a %f20.1 %f20.1', 'j0[currT]    j1[T+Dur]    w.r.t. timeunit ', jx0, jx1)
          call lgr%debug ('%a %f20.1 %f20.1', 'x0[times(1)] xn[times(N)] w.r.t. timeunit ', &
               this%t_alongtrack(1), this%t_alongtrack(nend))
          call lgr%debug ('%a %i20 %i20', 'jt1, jt2 [final intercepted position]', jt1, jt2)

          if (jt1==jt2) then
             _FAIL('Epoch Time is too small, empty swath grid is generated, increase Epoch')
          endif
          jt1 = jt1 + 1               ! (x1,x2]  design


          ! use nx as 1d
          nlon = size (this%lons_2d, dim=1)
          len =  nlon * (jt2 - jt1 + 1)
          allocate(this%lons(len),this%lats(len),_STAT)
          nx = 0
          do j = jt1, jt2
             do i = 1, nlon
                nx = nx + 1
                this%lons(nx) = this%lons_2d(i, j)
                this%lats(nx) = this%lats_2d(i, j)
             end do
          end do

          this%nobs_dur = nx
          arr(1)=nx

          call lgr%debug('%a %i12 %i12 %i12', 'this%nobs_dur, nlon, nlat', this%nobs_dur, nlon, len)

       else
          allocate(this%lons(0),this%lats(0),_STAT)
          this%nobs_dur = 0
          nx=0
          arr(1)=0
       endif
       

       call ESMF_VMAllFullReduce(vm, sendData=arr, recvData=nx_sum, &
            count=1, reduceflag=ESMF_REDUCE_SUM, rc=rc)
       this%nobs_dur_sum = nx_sum
       if (mapl_am_I_root()) write(6,*) 'nobs in dur(heartbeat)  :', nx_sum

       
       if ( nx_sum == 0 ) then
          this%is_valid = .false.
          _RETURN(ESMF_SUCCESS)
          !
          ! no valid obs points are found
          !
       end if


       ! __ s2. set distributed LS
       !

       this%locstream_factory = LocStreamFactory(this%lons,this%lats,_RC)
       this%LS_rt = this%locstream_factory%create_locstream(_RC)
       call ESMF_FieldBundleGet(this%bundle,grid=grid,_RC)
       this%LS_ds = this%locstream_factory%create_locstream(grid=grid,_RC)

       this%fieldA = ESMF_FieldCreate (this%LS_rt, name='A', typekind=ESMF_TYPEKIND_R8, _RC)
       this%fieldB = ESMF_FieldCreate (this%LS_ds, name='B', typekind=ESMF_TYPEKIND_R8, _RC)

!!       ptA => null()
       call ESMF_FieldGet( this%fieldA, localDE=0, farrayPtr=ptA)
       call ESMF_FieldGet( this%fieldB, localDE=0, farrayPtr=ptB)
       if (mypet == 0) then
          ptA(:) = this%lons(:)
       end if
       call ESMF_FieldRedistStore (this%fieldA, this%fieldB, RH, _RC)
       call ESMF_FieldRedist      (this%fieldA, this%fieldB, RH, _RC)
       this%lons_ds = ptB

       
       if (mypet == 0) then
          ptA(:) = this%lats(:)
       end if
       call ESMF_FieldRedist      (this%fieldA, this%fieldB, RH, _RC)
       this%lats_ds = ptB
       
       call ESMF_FieldRedistRelease(RH, noGarbage=.true., _RC)
       call ESMF_FieldDestroy(this%fieldA,nogarbage=.true.,_RC)
       call ESMF_FieldDestroy(this%fieldB,nogarbage=.true.,_RC)       

       !- debug
!       write(6,'(2x,a,i5,100f10.1)') 'lons_ds pet=', mypet, this%lons_ds(::200)
!       write(6,'(2x,a,i5,100f10.1)') 'lats_ds pet=', mypet, this%lats_ds(::200)
       
       write(6,'(2x,a,i5,100f10.1)') 'lons_rt pet=', mypet, this%lons(::200)
       write(6,'(2x,a,i5,100f10.1)') 'lats_rt pet=', mypet, this%lats(::200)       


       _FAIL ('nail xx')
       ! __ s3. round-1: find n.n. CS pts for LS_ds
       !
       obs_lons = this%lons_ds
       obs_lats = this%lats_ds
       
       nx = size ( this%lons_ds )
       allocate ( II(nx), JJ(nx) )
       !!       call MAPL_GetGlobalHorzIJIndex(nx,II,JJ,lon=this%lons_ds,lat=this%lats_ds,grid=Grid,_RC)
       call MAPL_GetGlobalHorzIJIndex(nx,II,JJ,lon=obs_lons,lat=obs_lats,grid=Grid,_RC)              

       call ESMF_GridGet(grid, DistGrid=distgrid, dimCount=dimCount, _RC)
       call ESMF_DistGridGet(distgrid, deLayout=LAYOUT, _RC)
       call ESMF_DELayoutGet(layout, VM=vm, _RC)
       call ESMF_VmGet(VM, localPet=myid, petCount=ndes, _RC)
       call MAPL_GridGet(grid, localCellCountPerDim=COUNTS, _RC)
       IM= COUNTS(1)
       JM= COUNTS(2)
       LM= COUNTS(3)
       allocate( this%mask(IM, JM))
       this%mask = 0
       do i=1, nx
          if ( II(i)>0 .AND. JJ(i)>0 ) then
             this%mask( II(i), JJ(i) ) = 1
          endif
       enddo
       
       write(6,'(2x,a,i5,100i5)') 'lats_rt pet=', mypet, this%mask(::5,::5)       
       
       _FAIL('nail 2')


       _RETURN(ESMF_SUCCESS)

     end procedure regrid_accumulate_on_xsubset
         

         module procedure destroy_rh_regen_LS
           integer :: status
           integer :: numVars, i, k
           character(len=ESMF_MAXSTR), allocatable :: names(:)
           type(ESMF_Field) :: field
           type(ESMF_Time)  :: currTime

          if (.NOT. this%is_valid) then
             _RETURN(ESMF_SUCCESS)
          endif
!
!           call ESMF_FieldDestroy(this%fieldB,nogarbage=.true.,_RC)
!           call this%locstream_factory%destroy_locstream(this%LS_rt, _RC)
!           call this%locstream_factory%destroy_locstream(this%LS_ds, _RC)
!           call this%regridder%destroy(_RC)
!           deallocate (this%lons, this%lats, &
!                this%times_R8, this%obstype_id)
!
!           do k=1, this%nobs_type
!              deallocate (this%obs(k)%metadata)
!              if (mapl_am_i_root()) then
!                 deallocate (this%obs(k)%file_handle)
!              end if
!           end do
!
!           if (mapl_am_i_root()) then
!              do k=1, this%nobs_type
!                 deallocate (this%obs(k)%lons)
!                 deallocate (this%obs(k)%lats)
!                 deallocate (this%obs(k)%times_R8)
!                 if (allocated(this%obs(k)%p2d)) then
!                    deallocate (this%obs(k)%p2d)
!                 endif
!                 if (allocated(this%obs(k)%p3d)) then
!                    deallocate (this%obs(k)%p3d)
!                 endif
!              end do
!           end if
!
!           call ESMF_FieldBundleGet(this%acc_bundle,fieldCount=numVars,_RC)
!           allocate(names(numVars),stat=status)
!           call ESMF_FieldBundleGet(this%acc_bundle,fieldNameList=names,_RC)
!           do i=1,numVars
!              call ESMF_FieldBundleGet(this%acc_bundle,trim(names(i)),field=field,_RC)
!              call ESMF_FieldDestroy(field,noGarbage=.true., _RC)
!           enddo
!           call ESMF_FieldBundleDestroy(this%acc_bundle,noGarbage=.true.,_RC)
!
!           call ESMF_FieldBundleGet(this%output_bundle,fieldCount=numVars,_RC)
!           allocate(names(numVars),stat=status)
!           call ESMF_FieldBundleGet(this%output_bundle,fieldNameList=names,_RC)
!           do i=1,numVars
!              call ESMF_FieldBundleGet(this%output_bundle,trim(names(i)),field=field,_RC)
!              call ESMF_FieldDestroy(field,noGarbage=.true., _RC)
!           enddo
!           call ESMF_FieldBundleDestroy(this%output_bundle,noGarbage=.true.,_RC)
!
!
!           call ESMF_ClockGet ( this%clock, CurrTime=currTime, _RC )
!           if (currTime > this%obsfile_end_time) then
!              this%is_valid = .false.
!              _RETURN(ESMF_SUCCESS)
!           end if
!
!           this%epoch_index(1:2)=0
!
!           call this%initialize(reinitialize=.true., _RC)
!
           _RETURN(ESMF_SUCCESS)

         end procedure destroy_rh_regen_LS


         module procedure get_x_subset
           type   (ESMF_Time)    :: T1,  T2
           real   (ESMF_KIND_R8) :: rT1, rT2

           integer(ESMF_KIND_I8) :: i1,  i2
           integer(ESMF_KIND_I8) :: jt1, jt2, lb, ub
           integer               :: jlo, jhi
           integer               :: status

           T1= interval(1)
           T2= interval(2)
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

           lb=int(jlo, ESMF_KIND_I8)
           ub=int(jhi, ESMF_KIND_I8)
           call bisect( this%obstime, rT1, jt1, n_LB=lb, n_UB=ub, rc=rc)
           call bisect( this%obstime, rT2, jt2, n_LB=lb, n_UB=ub, rc=rc)
           x_subset(1) = jt1

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

           _RETURN(_SUCCESS)
         end procedure get_x_subset


end submodule MaskSampler_implement
