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
  use Plain_netCDF_Time
  use MAPL_ObsUtilMod
  use MPI, only : MPI_INTEGER, MPI_REAL, MPI_REAL8
  use, intrinsic :: iso_fortran_env, only: REAL32
  use, intrinsic :: iso_fortran_env, only: REAL64
  use, intrinsic :: iso_fortran_env, only: INT64
  implicit none
   contains

     module procedure HistoryTrajectory_from_config
       integer :: status

       if (.not. present(GENSTATE)) then
          __FAIL('GENSTATE is not provided')
       end if
       if (schema_version == 1) then
          traj = HistoryTrajectory_from_config_schema_version_1 &
               (config,string,clock,schema_version,genstate=GENSTATE,__RC)
       elseif (schema_version == 2) then
          traj = HistoryTrajectory_from_config_schema_version_2 &
               (config,string,clock,schema_version,genstate=GENSTATE,__RC)
       end if
       __RETURN(__SUCCESS)

     end procedure HistoryTrajectory_from_config


     ! case : schema_version = 1
     !        read collection from .rcx config
     !        read grid_label
     module procedure HistoryTrajectory_from_config_schema_version_1
         use BinIOMod
         use  MAPL_scan_pattern_in_file
         use pflogger, only         :  Logger, logging
         type(ESMF_Time)            :: currTime
         type(ESMF_TimeInterval)    :: epoch_frequency
         type(ESMF_TimeInterval)    :: obs_time_span
         integer                    :: time_integer, second
         integer                    :: status
         character(len=ESMF_MAXSTR) :: STR1, line, splitter, STR_KW
         character(len=ESMF_MAXSTR) :: symd, shms
         character(len=ESMF_MAXSTR) :: key_grid
         integer                    :: nline, col
         integer, allocatable       :: ncol(:)
         character(len=ESMF_MAXSTR), allocatable :: word(:)
         character(len=ESMF_MAXSTR), allocatable :: str_piece(:)
         integer                    :: nobs, head, jvar
         logical                    :: tend, ispresent
         integer                    :: i, j, k, k2, M
         integer                    :: count, idx
         integer                    :: unitr, unitw
         integer                    :: length_mx, mxseg, nseg
         type(GriddedIOitem)        :: item
         character(len=3)           :: output_leading_dim
         type(Logger), pointer      :: lgr

         traj%clock=clock
         traj%schema_version=schema_version
         lgr => logging%get_logger('HISTORY.sampler')
         if (present(GENSTATE)) traj%GENSTATE => GENSTATE

         call ESMF_ClockGet ( clock, CurrTime=currTime, __RC )
         call ESMF_ConfigGetAttribute(config, value=time_integer, label=trim(string)//'Epoch:', default=0, __RC)
         __ASSERT(time_integer /= 0, 'Epoch value in config wrong')
         second = hms_2_s(time_integer)
         call ESMF_TimeIntervalSet(epoch_frequency, s=second, __RC)
         traj%Epoch = time_integer
         traj%RingTime = currTime
         traj%epoch_frequency = epoch_frequency
         traj%alarm = ESMF_AlarmCreate( clock=clock, RingInterval=epoch_frequency, &
              RingTime=traj%RingTime, sticky=.false., __RC )

         call ESMF_ConfigGetAttribute(config, value=traj%use_NWP_1_file, default=.false., &
              label=trim(string)//'use_NWP_1_file:', __RC)
         call ESMF_ConfigGetAttribute(config, value=traj%restore_2_obs_vector, default=.false., &
              label=trim(string)//'restore_2_obs_vector:', __RC)
         if (mapl_am_I_root()) then
            if (traj%use_NWP_1_file) then
               write(6,105) 'WARNING: Traj sampler: use_NWP_1_file is true'
               write(6,105) 'WARNING: USER needs to check if observation file is fetched correctly'
            end if
            if (traj%restore_2_obs_vector) then
               write(6,105) 'WARNING: Traj sampler: restore_2_obs_vector is true'
            end if
         end if
         if (.NOT. traj%use_NWP_1_file .AND. traj%restore_2_obs_vector) then
            __FAIL('use_NWP_1_file=.false. and restore_2_obs_vector=.true. is not allowed')
         end if

         call ESMF_ConfigGetAttribute ( config, key_grid, default='' , &
              label=trim(string) // 'grid_label:' ,__RC )
         key_grid = trim(adjustl(key_grid))//'.'
         __ASSERT (key_grid /= '', 'GRID_LABELS is empty')

         call ESMF_ConfigFindLabel(config, trim(key_grid)//'schema:',  isPresent=ispresent, rc=status)
         if (isPresent) then
            call ESMF_ConfigGetAttribute(config, value=STR1, default="", &
                 label=trim(key_grid) // 'schema:', __RC)
            call lgr%debug('%a %a', 'schema: ', trim(STR1))
            STR_KW = trim(STR1)//'.'
         else
            STR_KW = key_grid
         end if
         call ESMF_ConfigFindLabel(config, trim(key_grid)//'index:',  isPresent=ispresent, rc=status)
         __ASSERT(.not.ispresent, 'conflict: '//trim(key_grid)//'schema:'//' with '//trim(key_grid)//'index:')

         call ESMF_ConfigGetAttribute(config, value=traj%index_name_x, default="", &
              label=trim(STR_KW) // 'index:', __RC)
         call ESMF_ConfigGetAttribute(config, value=traj%var_name_lon_full, default="", &
              label=trim(STR_KW) // 'lon:', __RC)
         call ESMF_ConfigGetAttribute(config, value=traj%var_name_lat_full, default="", &
              label=trim(STR_KW) // 'lat:', __RC)
         call ESMF_ConfigGetAttribute(config, value=traj%var_name_time_full, default="", &
              label=trim(STR_KW) // 'time:', __RC)
         call ESMF_ConfigGetAttribute(config, value=STR1, default="", &
              label=trim(STR_KW) // 'obs_reftime:', __RC)
         if (trim(STR1)=='') then
            traj%obsfile_start_time = currTime
            call ESMF_TimeGet(currTime, timestring=STR1, __RC)
            if (mapl_am_I_root()) then
               write(6,105) 'obs reftime missing, default = currTime :', trim(STR1)
            endif
         else
            call ESMF_TimeSet(traj%obsfile_start_time, STR1, __RC)
            if (mapl_am_I_root()) then
               write(6,105) 'obs reftime provided: ', trim(STR1)
            end if
         end if
         call ESMF_ConfigGetAttribute(config, value=STR1, default="", &
              label=trim(STR_KW)//'obs_frequency:', __RC)
         __ASSERT(STR1/='', 'fatal error: obs_file_interval not provided in RC file')

         if (mapl_am_I_root()) write(6,105) 'obs_file frequency:', trim(STR1)
         if (mapl_am_I_root()) write(6,106) 'Epoch (second)   :', second
         i= index( trim(STR1), ' ' )
         if (i>0) then
            symd=STR1(1:i-1)
            shms=STR1(i+1:)
         else
            symd=''
            shms=trim(STR1)
         endif
         call convert_twostring_2_esmfinterval (symd, shms,  traj%obsfile_interval, __RC)
         traj%active = .true.

         k=1
         traj%nobs_type = k
         allocate (traj%obs(k), __STAT)
         allocate (traj%obs(k)%metadata, __STAT)
         call ESMF_ConfigGetAttribute(config, value=STR1, default="", &
              label=trim(key_grid) // 'file_name_template:', __RC)
         traj%obs(k)%input_template = trim(STR1)
         if (mapl_am_i_root()) then
            allocate (traj%obs(k)%file_handle, __STAT)
         end if
         traj%obs(k)%name = ''

         call lgr%debug('%a %i8', 'nobs_type=', traj%nobs_type)

         call ESMF_ConfigGetAttribute(config, value=output_leading_dim, default='lev', &
              label=trim(string)//'output_leading_dim:', __RC)
         traj%write_lev_first = ( output_leading_dim == 'lev' )

         __RETURN(__SUCCESS)

105      format (1x,a,2x,a)
106      format (1x,a,2x,i8)
     end procedure HistoryTrajectory_from_config_schema_version_1



     ! case : schema_version = 2
     !        read from .rcx config
     !        read DEFINE_OBS_PLATFORM:  supercollection
     !
     module procedure HistoryTrajectory_from_config_schema_version_2
         use BinIOMod
         use  MAPL_scan_pattern_in_file
         use pflogger, only         :  Logger, logging
         type(ESMF_Time)            :: currTime
         type(ESMF_TimeInterval)    :: epoch_frequency
         type(ESMF_TimeInterval)    :: obs_time_span
         integer                    :: time_integer, second
         integer                    :: status
         character(len=ESMF_MAXSTR) :: STR1, line, splitter, STR_KW
         character(len=ESMF_MAXSTR) :: symd, shms
         integer                    :: nline, col
         integer, allocatable       :: ncol(:)
         character(len=ESMF_MAXSTR), allocatable :: word(:)
         character(len=ESMF_MAXSTR), allocatable :: str_piece(:)
         integer                    :: nobs, head, jvar
         logical                    :: tend, ispresent, ispresent2
         integer                    :: i, j, k, k2, M
         integer                    :: count, idx
         integer                    :: unitr, unitw
         integer                    :: length_mx, mxseg, nseg
         type(GriddedIOitem)        :: item
         type(Logger), pointer      :: lgr

         traj%schema_version=schema_version
         traj%clock=clock
         if (present(GENSTATE)) traj%GENSTATE => GENSTATE

         call ESMF_ClockGet ( clock, CurrTime=currTime, __RC )
         call ESMF_ConfigGetAttribute(config, value=time_integer, label=trim(string)//'Epoch:', default=0, __RC)
         __ASSERT(time_integer /= 0, 'Epoch value in config wrong')
         second = hms_2_s(time_integer)
         call ESMF_TimeIntervalSet(epoch_frequency, s=second, __RC)
         traj%Epoch = time_integer
         traj%RingTime = currTime
         traj%epoch_frequency = epoch_frequency
         traj%alarm = ESMF_AlarmCreate( clock=clock, RingInterval=epoch_frequency, &
              RingTime=traj%RingTime, sticky=.false., __RC )


         call ESMF_ConfigFindLabel(config, trim(string)//'schema:',  isPresent=ispresent, rc=status)
         if (isPresent) then
            call ESMF_ConfigGetAttribute(config, value=STR1, default="", &
                 label=trim(string) // 'schema:', __RC)
            STR_KW = trim(STR1)//'.'
         else
            STR_KW = string
         end if

         call ESMF_ConfigFindLabel(config, trim(string)//'index:',  isPresent=ispresent2, rc=status)
         ispresent = .not. (ispresent .and. ispresent2)
         __ASSERT(ispresent, 'conflict: '//trim(string)//'schema:'//' with '//trim(string)//'index_name_x:')


         call ESMF_ConfigGetAttribute(config, value=traj%index_name_x, default="", &
              label=trim(STR_KW) // 'index:', __RC)
         call ESMF_ConfigGetAttribute(config, value=traj%var_name_lon_full, default="", &
              label=trim(STR_KW) // 'lon:', __RC)
         call ESMF_ConfigGetAttribute(config, value=traj%var_name_lat_full, default="", &
              label=trim(STR_KW) // 'lat:', __RC)
         call ESMF_ConfigGetAttribute(config, value=traj%var_name_time_full, default="", &
              label=trim(STR_KW) // 'time:', __RC)
         call ESMF_ConfigGetAttribute(config, value=traj%use_NWP_1_file, default=.false., &
              label=trim(string)//'use_NWP_1_file:', __RC)
         call ESMF_ConfigGetAttribute(config, value=traj%restore_2_obs_vector, default=.false., &
              label=trim(string)//'restore_2_obs_vector:', __RC)
         if (mapl_am_I_root()) then
            if (traj%use_NWP_1_file) then
               write(6,105) 'WARNING: Traj sampler: use_NWP_1_file is true'
               write(6,105) 'WARNING: USER needs to check if observation file is fetched correctly'
            end if
            if (traj%restore_2_obs_vector) then
               write(6,105) 'WARNING: Traj sampler: restore_2_obs_vector is true'
            end if
         end if
         if (.NOT. traj%use_NWP_1_file .AND. traj%restore_2_obs_vector) then
            __FAIL('use_NWP_1_file=.false. and restore_2_obs_vector=.true. is not allowed')
         end if

         call ESMF_ConfigGetAttribute(config, value=STR1, default="", &
              label=trim(STR_KW) // 'obs_reftime:', __RC)
         if (trim(STR1)=='') then
            traj%obsfile_start_time = currTime
            call ESMF_TimeGet(currTime, timestring=STR1, __RC)
            if (mapl_am_I_root()) then
               write(6,105) 'obs reftime missing, default = currTime :', trim(STR1)
            endif
         else
            call ESMF_TimeSet(traj%obsfile_start_time, STR1, __RC)
            if (mapl_am_I_root()) then
               write(6,105) 'obs_reftime provided: ', trim(STR1)
            end if
         end if

         call ESMF_ConfigGetAttribute(config, value=STR1, default="", &
              label=trim(STR_KW) // 'obs_refquency:', __RC)
         __ASSERT(STR1/='', 'fatal error: obs_file_interval not provided in RC file')
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
         call convert_twostring_2_esmfinterval (symd, shms,  traj%obsfile_interval, __RC)
         traj%active = .true.



         ! __ s1. overall print
         call ESMF_ConfigGetDim(config, nline, col, label=trim(string)//'obs_files:', rc=rc)
         __ASSERT(rc==0 .AND. nline > 0, 'obs_files not found')
         !! write(6,*) 'nline, col', nline, col
         allocate(ncol(1:nline), __STAT)

         call ESMF_ConfigFindLabel( config, trim(string)//'obs_files:', __RC )
         do i = 1, nline
            call ESMF_ConfigNextLine(config, __RC)
            ncol(i) = ESMF_ConfigGetLen(config, __RC)
!!            write(6,*) 'line', i, 'ncol(i)', ncol(i)
         enddo


         ! __ s2. find nobs  &&  distinguish design with vs wo  '------'
         nobs=0
         call ESMF_ConfigFindLabel( config, trim(string)//'obs_files:', __RC)
         do i=1, nline
            call ESMF_ConfigNextLine( config, tableEnd=tend, __RC)
            call ESMF_ConfigGetAttribute( config, STR1, __RC)
            if ( index(trim(STR1), '-----') > 0 ) nobs=nobs+1
         enddo


         ! __ s3. retrieve template and geoval, set metadata file_handle
         lgr => logging%get_logger('HISTORY.sampler')
         length_mx = ESMF_MAXSTR
         mxseg = 100
         allocate (str_piece(mxseg))
         if ( nobs == 0 ) then
            !
            !   treatment-1:
            !
            __FAIL('this setting in HISTORY.rc obs_files: is not supported, stop')
            traj%nobs_type = nline         ! here .rc format cannot have empty spaces
            allocate (traj%obs(nline), __STAT)
            call ESMF_ConfigFindLabel( config, trim(string)//'obs_files:', __RC)
            do i=1, nline
               call ESMF_ConfigNextLine( config, tableEnd=tend, __RC)
               call ESMF_ConfigGetAttribute( config, traj%obs(i)%input_template, __RC)
               traj%obs(i)%export_all_geoval = .true.
            enddo
         else
            !
            !-- selectively output geovals
            !   treatment-2:
            !
            traj%nobs_type = nobs
            allocate (traj%obs(nobs), __STAT)
            !
            nobs=0   ! reuse counter
            head=1
            jvar=0
            !
            !   count '------' in history.rc as special markers for ngeoval
            !
            call ESMF_ConfigFindLabel(config, trim(string)//'obs_files:', __RC)
            do i=1, nline
               call ESMF_ConfigNextLine(config, tableEnd=tend, __RC)
               M = ncol(i)
               __ASSERT(M>=1, '# of columns should be >= 1')
               allocate (word(M), __STAT)
               count=0
               do col=1, M
                  call ESMF_ConfigGetAttribute(config, STR1, __RC)
                  if (trim(STR1)/=',') then
                     count=count+1
                     word(count) =  extract_unquoted_item(STR1)
                  end if
               enddo
               if (count ==1 .or. count==2) then
                  ! 1-item case:  file template or one-var
                  ! 2-item     :  var1 , 'root' case
                  STR1=trim(word(1))
               elseif ( count == 3 ) then
                  ! the Alias case + the splitField case
                  ! 3-item     :  var1 , 'root',  var1_alias case
                  ! 3-item     :  var1 , 'root', 'TOTEXTTAU470;TOTEXTTAU550;TOTEXTTAU870',
                  ! 3-item     :  'u;v' vector interpolation is not handled
                  STR1=trim(word(3))
               else
                  STR1=trim(word(3))
                  call lgr%debug('%a %i8', 'WARNING: there are more than 3 field_names in platform rcx' )
               end if
               deallocate(word, __STAT)

               if ( index(trim(STR1), '-----') == 0 ) then
                  if (head==1 .AND. trim(STR1)/='') then
                     nobs=nobs+1
                     traj%obs(nobs)%input_template = trim(STR1)
                     traj%obs(nobs)%export_all_geoval = .false.
                     head=0
                  else
                     if (trim(STR1)/='') then
                        splitter=';,'
                        call split_string_by_seperator (STR1, length_mx, splitter, mxseg, &
                             nseg, str_piece, status)
                        if (count < 3) then
                           ! case
                           ! 'var1'
                           ! 'var1' , 'ROOT'
                           ! 'u;v'  , 'ROOT'
                           jvar=jvar+1
                           if (nseg==1) then
                              traj%obs(nobs)%geoval_xname(jvar) = STR1
                           else
                              traj%obs(nobs)%geoval_xname(jvar) = trim(str_piece(1))
                              traj%obs(nobs)%geoval_yname(jvar) = trim(str_piece(2))
                           end if
                        else
                           ! case
                           ! 'var1' , 'ROOT' , alias
                           ! 'var1' , 'ROOT' , 'TOTEXTTAU470;TOTEXTTAU550;TOTEXTTAU870,'  split_field
                           do j=1, nseg
                              jvar=jvar+1
                              traj%obs(nobs)%geoval_xname(jvar) = trim(str_piece(j))
                           end do
                        end if
                     end if
                  end if
               else
                  traj%obs(nobs)%ngeoval=jvar
                  head=1
                  jvar=0
               endif
            enddo
         end if

         !!if (mapl_am_i_root()) then
         !!   do k=1, nobs
         !!      do j=1, traj%obs(k)%ngeoval
         !!         write(6, '(2x,a,2x,2i10,2x,a)') &
         !!              'traj%obs(k)%geoval_xname(j),  k, j, xname ', k, j, trim(traj%obs(k)%geoval_xname(j))
         !!      end do
         !!   end do
         !!end if


         do k=1, traj%nobs_type
            allocate (traj%obs(k)%metadata, __STAT)
            if (mapl_am_i_root()) then
               allocate (traj%obs(k)%file_handle, __STAT)
            end if
         end do


         call lgr%debug('%a %i8', 'nobs_type=', traj%nobs_type)
         do i=1, traj%nobs_type
            call lgr%debug('%a %i4 %a  %a', 'obs(', i, ') input_template =', &
                 trim(traj%obs(i)%input_template))
            k=index(traj%obs(i)%input_template , '/', back=.true.)
            j=index(traj%obs(i)%input_template(k+1:), '%')
            if (j>0) then
               ! normal case:  geos_atmosphere/aircraft.%y4%m2%d2T%h2%n2%S2Z.nc4
               traj%obs(i)%name = traj%obs(i)%input_template(k+1:k+j-1)
            else
               ! different case:  Y%y4/M%m2/.../this.nc or ./this
               k2=index(traj%obs(i)%input_template(k+1:), '.')
               if (k2>0) then
                  traj%obs(i)%name = traj%obs(i)%input_template(k+1:k+k2)
               else
                  traj%obs(i)%name = trim(traj%obs(i)%input_template(k+1:))
               end if
            end if
         end do

         call ESMF_ConfigGetAttribute(config, value=traj%write_lev_first, default=.true., &
              label=trim(string)//'write_lev_first:', __RC)

         __RETURN(__SUCCESS)

105      format (1x,a,2x,a)
106      format (1x,a,2x,i8)
       end procedure HistoryTrajectory_from_config_schema_version_2



       !
       !-- integrate both initialize and reinitialize
       !
       module procedure initialize_
         integer :: status
         type(ESMF_Grid) :: grid
         type(variable) :: v
         type(GriddedIOitemVectorIterator) :: iter
         type(GriddedIOitem), pointer :: item
         type(ESMF_Time)            :: currTime
         integer :: k

!         if (mapl_am_i_root())  write(6,'(2x,a,10(2x,L5))') &
!              'traj initialize_ :  present(reinitialize), reinitialize =',  &
!              present(reinitialize), reinitialize
         if (.not. present(reinitialize)) then
            if(present(bundle))   this%bundle=bundle
            if(present(items))    this%items=items
            if(present(timeInfo)) this%time_info=timeInfo
            if (present(vdata)) then
               this%vdata=vdata
            else
               this%vdata=VerticalData(__RC)
            end if
            !if (mapl_am_i_root())  write(6,'(2x,a,10(2x,L5))') &
            !      'traj initialize_ : initialize : not present '
         else
            if (reinitialize) then
               do k=1, this%nobs_type
                  allocate (this%obs(k)%metadata, __STAT)
                  if (mapl_am_i_root()) then
                     allocate (this%obs(k)%file_handle, __STAT)
                  end if
               end do
            !if (mapl_am_i_root())  write(6,'(2x,a,10(2x,L5))') &
            !     'traj initialize_ : initialize : TRUE'
            end if
         end if

         do k=1, this%nobs_type
            call this%vdata%append_vertical_metadata(this%obs(k)%metadata,this%bundle,__RC)
         end do
         this%do_vertical_regrid = (this%vdata%regrid_type /= VERTICAL_METHOD_NONE)
         if (this%vdata%regrid_type == VERTICAL_METHOD_ETA2LEV) call this%vdata%get_interpolating_variable(this%bundle,__RC)

         call ESMF_ClockGet (this%clock, CurrTime=currTime, __RC)
         call get_obsfile_Tbracket_from_epoch(currTime, this%obsfile_start_time, &
              this%obsfile_interval, this%epoch_frequency, &
              this%obsfile_Ts_index, this%obsfile_Te_index, __RC)
         if (this%obsfile_Te_index < 0) then
            if (mapl_am_I_root()) then
               write(6,*) "model start time is earlier than obsfile_start_time"
               write(6,*) "solution: adjust obsfile_start_time and Epoch in rc file"
            end if
            __FAIL("obs file not found at init time")
         endif
         call this%create_grid(__RC)

         call ESMF_FieldBundleGet(this%bundle,grid=grid,__RC)
         this%regridder = LocStreamRegridder(grid,this%LS_ds,__RC)
         this%output_bundle = this%create_new_bundle(__RC)
         this%acc_bundle    = this%create_new_bundle(__RC)

         do k=1, this%nobs_type
            call this%obs(k)%metadata%add_dimension(this%index_name_x, this%obs(k)%nobs_epoch)
            if (this%time_info%integer_time) then
               v = Variable(type=PFIO_INT32,dimensions=this%index_name_x)
            else
               v = Variable(type=PFIO_REAL64,dimensions=this%index_name_x)
            end if
            call v%add_attribute('units', this%datetime_units)
            call v%add_attribute('long_name', 'time')
            call this%obs(k)%metadata%add_variable(this%var_name_time,v)

            if (.NOT. this%restore_2_obs_vector) then
               v = Variable(type=PFIO_INT32,dimensions=this%index_name_x)
               call v%add_attribute('units', '1')
               call v%add_attribute('long_name', 'Location index in corresponding IODA file')
               call this%obs(k)%metadata%add_variable(this%location_index_name,v)
            end if

            v = variable(type=PFIO_REAL64,dimensions=this%index_name_x)
            call v%add_attribute('units','degrees_east')
            call v%add_attribute('long_name','longitude')
            call this%obs(k)%metadata%add_variable(this%var_name_lon,v)

            v = variable(type=PFIO_REAL64,dimensions=this%index_name_x)
            call v%add_attribute('units','degrees_north')
            call v%add_attribute('long_name','latitude')
            call this%obs(k)%metadata%add_variable(this%var_name_lat,v)
         end do

         ! push varible names down to each obs(k); see create_metadata_variable
         iter = this%items%begin()
         do while (iter /= this%items%end())
            item => iter%get()

!!            print*, 'list item%xname', trim(item%xname)

            if (item%itemType == ItemTypeScalar) then
               call this%create_variable(item%xname,__RC)
            else if (item%itemType == ItemTypeVector) then
               call this%create_variable(item%xname,__RC)
               call this%create_variable(item%yname,__RC)
            end if
            call iter%next()
         enddo

         __RETURN(__SUCCESS)

       end procedure initialize_



      module procedure create_metadata_variable
        type(ESMF_Field) :: field
        type(variable) :: v
        logical :: is_present
        integer :: field_rank, status
        character(len=ESMF_MAXSTR) :: var_name,long_name,units,vdims
        integer :: k, ig

        call ESMF_FieldBundleGet(this%bundle,vname,field=field,__RC)
        call ESMF_FieldGet(field,name=var_name,rank=field_rank,__RC)
        call ESMF_AttributeGet(field,name="LONG_NAME",isPresent=is_present,__RC)
        if ( is_present ) then
           call ESMF_AttributeGet  (FIELD, NAME="LONG_NAME",VALUE=long_name, __RC)
        else
           long_name = var_name
        endif
        call ESMF_AttributeGet(field,name="UNITS",isPresent=is_present,__RC)
        if ( is_present ) then
           call ESMF_AttributeGet  (FIELD, NAME="UNITS",VALUE=units, __RC)
        else
           units = 'unknown'
        endif
        if (field_rank==2) then
           vdims = this%index_name_x
        else if (field_rank==3) then
           if (this%write_lev_first) then
              vdims = "lev,"//trim(this%index_name_x)
           else
              vdims = trim(this%index_name_x)//",lev"
           end if
        end if
        v = variable(type=PFIO_REAL32,dimensions=trim(vdims))
        call v%add_attribute('units',trim(units))
        call v%add_attribute('long_name',trim(long_name))
        call v%add_attribute('missing_value',MAPL_UNDEF)
        call v%add_attribute('_FillValue',MAPL_UNDEF)
        call v%add_attribute('valid_range',(/-MAPL_UNDEF,MAPL_UNDEF/))

        do k = 1, this%nobs_type
           if (this%schema_version == 1) then
              call this%obs(k)%metadata%add_variable(trim(var_name),v,__RC)
           else
              do ig = 1, this%obs(k)%ngeoval
                 if (trim(var_name) == trim(this%obs(k)%geoval_xname(ig))) then
                    call this%obs(k)%metadata%add_variable(trim(var_name),v,__RC)

!!              if (mapl_am_i_root()) write(6, '(2x,a,/,10(2x,a))') &
!!                   'Traj: create_metadata_variable: vname, var_name, this%obs(k)%geoval_xname(ig)', &
!!                   trim(vname), trim(var_name), trim(this%obs(k)%geoval_xname(ig))

                 endif
              enddo
           end if
        enddo

         __RETURN(__SUCCESS)
      end procedure create_metadata_variable


      module procedure create_new_bundle
        type(GriddedIOitemVectorIterator) :: iter
        type(GriddedIOitem), pointer :: item
        type(ESMF_Field) :: src_field,dst_field
        integer :: rank,lb(1),ub(1)
        integer :: status

        new_bundle = ESMF_FieldBundleCreate(__RC)
        iter = this%items%begin()
        do while (iter /= this%items%end())
           item => iter%get()
           !!if (mapl_am_I_root()) print*, 'create new bundle, this%items%xname= ', trim(item%xname)
           if (item%itemType == ItemTypeScalar) then
              call ESMF_FieldBundleGet(this%bundle,trim(item%xname),field=src_field,__RC)
              call ESMF_FieldGet(src_field,rank=rank,__RC)
              if (rank==2) then
                 dst_field = ESMF_FieldCreate(this%LS_ds,name=trim(item%xname), &
                      typekind=ESMF_TYPEKIND_R4,__RC)
              else if (rank==3) then
                 call ESMF_FieldGet(src_field,ungriddedLBound=lb,ungriddedUBound=ub,__RC)
                 if (this%vdata%lm/=(ub(1)-lb(1)+1)) then
                    lb(1)=1
                    ub(1)=this%vdata%lm
                 end if
                 dst_field = ESMF_FieldCreate(this%LS_ds,name=trim(item%xname), &
                      typekind=ESMF_TYPEKIND_R4,ungriddedLBound=lb,ungriddedUBound=ub,__RC)
              end if
              call MAPL_FieldBundleAdd(new_bundle,dst_field,__RC)
           else if (item%itemType == ItemTypeVector) then
!!              __FAIL("ItemTypeVector not yet supported")
           end if
           call iter%next()
        enddo
        __RETURN(__SUCCESS)

      end procedure create_new_bundle


      module procedure create_file_handle
      use pflogger, only         :  Logger, logging
         integer :: status
         integer :: k
         character(len=ESMF_MAXSTR) :: filename
         type(Logger), pointer :: lgr

         if (.NOT. this%active) then
            __RETURN(ESMF_SUCCESS)
         endif

         if (this%nobs_epoch_sum==0) then
            rc=0
            return
         endif

         lgr => logging%get_logger('HISTORY.sampler')
         do k=1, this%nobs_type
            call this%obs(k)%metadata%modify_dimension(this%index_name_x, this%obs(k)%nobs_epoch)
         enddo
         if (mapl_am_I_root()) then
            do k=1, this%nobs_type
               if (this%obs(k)%nobs_epoch > 0) then
                  filename=trim(this%obs(k)%name)//trim(filename_suffix)
                  write(6,'(1x,a,2x,a)')  "Sampling to new file :",trim(filename)
                  call this%obs(k)%file_handle%create(trim(filename),__RC)
                  call this%obs(k)%file_handle%write(this%obs(k)%metadata,__RC)
               end if
            enddo
         end if

        __RETURN(__SUCCESS)
      end procedure create_file_handle


       module procedure close_file_handle
          integer :: status
          integer :: k

          if (.NOT. this%active) then
             __RETURN(ESMF_SUCCESS)
          endif

         if (this%nobs_epoch_sum==0) then
            rc=0
            return
         endif

         if (mapl_am_I_root()) then
            do k=1, this%nobs_type
               if (this%obs(k)%nobs_epoch > 0) then
                  call this%obs(k)%file_handle%close(__RC)
               end if
            end do
         end if
          __RETURN(__SUCCESS)
       end procedure close_file_handle


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
         character :: new_char(ESMF_MAXSTR)

         real(REAL64), allocatable :: lons_full(:), lats_full(:)
         real(REAL64), allocatable :: times_R8_full(:)
         real(REAL64)              :: t_shift
         integer,           allocatable :: obstype_id_full(:)
         integer,           allocatable :: location_index_ioda_full(:)
         integer,           allocatable :: location_index_ioda_full_aux(:)
         integer,           allocatable :: IA_full(:)


         real(ESMF_KIND_R8), pointer :: ptAT(:)
         type(ESMF_routehandle) :: RH
         type(ESMF_Time) :: timeset(2)
         type(ESMF_Time) :: current_time
         type(ESMF_Time) :: time0
         type(ESMF_TimeInterval) :: dt
         type(ESMF_Grid) :: grid

         type(ESMF_VM) :: vm
         integer :: mypet, petcount, mpic

         integer :: i, j, k, L, ii, jj, jj2
         integer :: fid_s, fid_e
         integer(kind=ESMF_KIND_I8) :: j0, j1
         integer(kind=ESMF_KIND_I8) :: jt1, jt2
         integer(kind=ESMF_KIND_I8) :: nstart, nend
         real(kind=ESMF_KIND_R8) :: jx0, jx1
         integer :: nx, nx_sum
         integer :: n0
         integer :: arr(1)
         integer :: sec
         integer, allocatable :: ix(:) !  counter for each obs(k)%nobs_epoch
         integer :: nx2
         logical :: EX ! file
         logical :: zero_obs
         integer, allocatable :: sendcount(:), displs(:)
         integer :: recvcount
         integer :: is, ie, ierr
         integer :: M, N, ip

         real(REAL64), allocatable :: lons_chunk(:)
         real(REAL64), allocatable :: lats_chunk(:)
         real(REAL64), allocatable :: times_R8_chunk(:)


         lgr => logging%get_logger('HISTORY.sampler')

         call ESMF_VMGetCurrent(vm,__RC)
         call ESMF_VMGet(vm, mpiCommunicator=mpic, petCount=petCount, localPet=mypet, __RC)

         if (this%index_name_x == '') then
            !
            !-- non IODA case / non netCDF
            !
            __FAIL('non-IODA format is not implemented here')
         end if

         !
         !-- IODA case
         !
         i=index(this%var_name_lon_full, '/')
         if (i==0) then
            grp_name = ''
            call lgr%debug('%a', 'grp_name not found')
         else
            grp_name = this%var_name_lon_full(1:i-1)
         end if
         this%var_name_lon = this%var_name_lon_full(i+1:)
         i=index(this%var_name_lat_full, '/')
         this%var_name_lat = this%var_name_lat_full(i+1:)
         i=index(this%var_name_time_full, '/')
         this%var_name_time= this%var_name_time_full(i+1:)
         this%location_index_name = 'location_index_in_iodafile'

         call lgr%debug('%a', 'grp_name,this%index_name_x,this%var_name_lon,this%var_name_lat,this%var_name_time')
         call lgr%debug('%a %a %a %a %a', &
              trim(grp_name),trim(this%index_name_x),trim(this%var_name_lon),&
              trim(this%var_name_lat),trim(this%var_name_time))

         L=0
         if (this%use_NWP_1_file) then
            ! NWP IODA 1 file case
            fid_s=this%obsfile_Ts_index+1    ! index is downshifted by 1 in MAPL_ObsUtil.F90
            fid_e=fid_s
         else
            ! regular case for any trajectory
            fid_s=this%obsfile_Ts_index    ! index is downshifted by 1 in MAPL_ObsUtil.F90
            fid_e=this%obsfile_Te_index
         end if

         call lgr%debug('%a %i10 %i10', &
              'fid_s,  fid_e', fid_s,  fid_e)

         arr(1)=0     ! len_full
         if (mapl_am_I_root()) then
            !
            ! __ s1. scan 1st time: determine len
            len = 0
            do k=1, this%nobs_type
               j = max (fid_s, L)
               jj2 = 0           ! obs(k) count location
               this%obs(k)%count_location_until_matching_file = 0   ! init
               this%obs(k)%count_location_in_matching_file = 0      ! init
               do while (j<=fid_e)
                  filename = get_filename_from_template_use_index( &
                       this%obsfile_start_time, this%obsfile_interval, &
                       j, this%obs(k)%input_template, EX, __RC)
                  if (EX) then
                     call lgr%debug('%a %i10', 'exist: filename fid j      :', j)
                     call lgr%debug('%a %a',   'exist: true filename       :', trim(filename))
                     call get_ncfile_dimension(filename, tdim=num_times, key_time=this%index_name_x, __RC)
                     len = len + num_times
                     jj2 = jj2 + num_times
                     if (j==this%obsfile_Ts_index) then
                        this%obs(k)%count_location_until_matching_file = jj2
                     elseif (j==this%obsfile_Ts_index+1) then
                        this%obs(k)%count_location_in_matching_file = num_times
                     end if
                  else
                     call lgr%debug('%a %i10', 'non-exist: filename fid j  :', j)
                     call lgr%debug('%a %a',   'non-exist: missing filename:', trim(filename))
                  end if
                  j=j+1
               enddo
            enddo
            arr(1)=len

            !
            ! __ s2. scan 2nd time: read time ect. into array,
            !        set location_index starting with the 1st element of the closest matched obs file
            if (len>0) then
               allocate(lons_full(len),lats_full(len),__STAT)
               allocate(times_R8_full(len),__STAT)
               allocate(obstype_id_full(len),__STAT)
               allocate(location_index_ioda_full(len),__STAT)
               allocate(IA_full(len),__STAT)
               call lgr%debug('%a %i12', 'nobs from input file:', len)
               len = 0
               ii = 0
               do k=1, this%nobs_type
                  j = max (fid_s, L)
                  jj2 = 0           ! obs(k) count location
                  do while (j<=fid_e)
                     filename = get_filename_from_template_use_index( &
                          this%obsfile_start_time, this%obsfile_interval, &
                          j, this%obs(k)%input_template, EX, __RC)
                     if (EX) then
                        ii = ii + 1
                        call get_ncfile_dimension(trim(filename), tdim=num_times, key_time=this%index_name_x, __RC)
                        call get_v1d_netcdf_R8 (filename, this%var_name_lon,  lons_full(len+1:), num_times, group_name=grp_name)
                        call get_v1d_netcdf_R8 (filename, this%var_name_lat,  lats_full(len+1:), num_times, group_name=grp_name)
                        call get_v1d_netcdf_R8 (filename, this%var_name_time, times_R8_full(len+1:), num_times, group_name=grp_name)
                        call get_attribute_from_group (filename, grp_name, this%var_name_time, "units", timeunits_file)
                        if (ii == 1) then
                           this%datetime_units = trim(timeunits_file)
                           call lgr%debug('%a %a', 'datetime_units from 1st file:', trim(timeunits_file))
                        end if
                        call diff_two_timeunits (this%datetime_units, timeunits_file, t_shift, __RC)
                        times_R8_full(len+1:len+num_times) = times_R8_full(len+1:len+num_times) + t_shift
                        obstype_id_full(len+1:len+num_times) = k
                        do jj = 1, num_times
                           jj2 = jj2 + 1
                           ! for each obs type use the correct starting point
                           ! if: use_nwp_1_file:  index_ioda = [ 1, Nobs ] : restore_2_obs_vector is exact
                           ! else                 index_ioda = [ -M, 0 ] + [1, Nobs1] + [Nob1+1, Nobs2] : restore_2_obs_vector may fail
                           !                                      File1     File(center)         File3
                           ! why: bc we have no restriction on observation file name vs content, hence unexpected things can happen
                           !      use use_nwp_1_file + restore_2_obs_vector only when filename and content are systematic
                           location_index_ioda_full(len+jj) = jj2 - this%obs(k)%count_location_until_matching_file
                        end do
                        len = len + num_times
                     end if
                     j=j+1
                  enddo
               enddo
            end if
         end if

         call ESMF_VMAllFullReduce(vm, sendData=arr, recvData=nx_sum, &
              count=1, reduceflag=ESMF_REDUCE_SUM, rc=rc)
         if (nx_sum == 0) then
            allocate(this%lons(0),this%lats(0),__STAT)
            allocate(this%times_R8(0),__STAT)
            allocate(this%obstype_id(0),__STAT)
            allocate(this%location_index_ioda(0),__STAT)
            this%epoch_index(1:2) = 0
            this%nobs_epoch = 0
            this%nobs_epoch_sum = 0
            !
            ! empty shell to keep regridding and destroy_RH_LS to work
            !
            this%locstream_factory = LocStreamFactory(this%lons,this%lats,__RC)
            this%LS_rt = this%locstream_factory%create_locstream(__RC)
            call ESMF_FieldBundleGet(this%bundle,grid=grid,__RC)
            this%LS_ds = this%locstream_factory%create_locstream(grid=grid,__RC)
            this%fieldB = ESMF_FieldCreate (this%LS_ds, name='B_time', typekind=ESMF_TYPEKIND_R8, __RC)
            call ESMF_FieldGet( this%fieldB, localDE=0, farrayPtr=this%obsTime)
            this%obsTime= -1.d0
            rc = 0
            return
         end if
         call MAPL_CommsBcast(vm, this%datetime_units, N=ESMF_MAXSTR, ROOT=MAPL_Root, __RC)


         if (mapl_am_I_root()) then
            call sort_index (times_R8_full, IA_full, __RC)
            !!   use index to sort togehter multiple arrays
            allocate(location_index_ioda_full_aux, source=location_index_ioda_full, __STAT)
            do jj = 1, nx_sum
               ii = IA_full(jj)
               location_index_ioda_full(jj) = location_index_ioda_full_aux(ii)
            end do
            deallocate(location_index_ioda_full_aux)
            ! NVHPC dies with NVFORTRAN-S-0155-Could not resolve generic procedure sort_multi_arrays_by_time
            call sort_four_arrays_by_time(lons_full, lats_full, times_R8_full, obstype_id_full, __RC)
            call ESMF_ClockGet(this%clock,currTime=current_time,__RC)
            timeset(1) = current_time
            timeset(2) = current_time + this%epoch_frequency
            call time_esmf_2_nc_int (timeset(1), this%datetime_units, j0, __RC)
            sec = hms_2_s(this%Epoch)
            j1 = j0 + int(sec, kind=ESMF_KIND_I8)
            jx0 = real ( j0, kind=ESMF_KIND_R8)
            if (this%use_NWP_1_file) then
               ! IODA case:
               ! Upper bound time is set at 'Epoch + 1 second' to get the right index from bisect
               !
               jx1 = real ( j1 + 1, kind=ESMF_KIND_R8)
            else
               ! normal case:
               jx1 = real ( j1, kind=ESMF_KIND_R8)
            end if

            nstart=1; nend=size(times_R8_full)
            call bisect( times_R8_full, jx0, jt1, n_LB=int(nstart, ESMF_KIND_I8), n_UB=int(nend, ESMF_KIND_I8), rc=rc)
            call bisect( times_R8_full, jx1, jt2, n_LB=int(nstart, ESMF_KIND_I8), n_UB=int(nend, ESMF_KIND_I8), rc=rc)
            call lgr%debug ('%a %i20 %i20', 'nstart, nend', nstart, nend)
            call lgr%debug ('%a %f20.1 %f20.1', 'j0[currT]    j1[T+Epoch]  w.r.t. timeunit ', jx0, jx1)
            call lgr%debug ('%a %f20.1 %f20.1', 'x0[times(1)] xn[times(N)] w.r.t. timeunit ', &
                 times_R8_full(1), times_R8_full(nend))
            call lgr%debug ('%a %i20 %i20', 'jt1, jt2 [final intercepted position]', jt1, jt2)

            if (jt1/=jt2) then
               zero_obs = .false.
            else
               ! at most one obs point exist, set it .true.
               zero_obs = .true.
            end if

            !
            !-- exclude the out-of-range case
            !
            if ( zero_obs ) then
               allocate(this%lons(0),this%lats(0),__STAT)
               allocate(this%times_R8(0),__STAT)
               allocate(this%obstype_id(0),__STAT)
               allocate(this%location_index_ioda(0),__STAT)
               this%epoch_index(1:2)=0
               this%nobs_epoch = 0
               nx=0
               arr(1)=nx
            else
               !! doulbe check
               ! (x1, x2]  design in bisect :  y(n) < x <= y(n+1), n is intercept index
               this%epoch_index(1)= jt1 + 1

               __ASSERT(jt2<=len, 'bisect index for this%epoch_index(2) failed')
               if (jt2==0) then
                  this%epoch_index(2)= 1
               else
                  this%epoch_index(2)= jt2
               endif

               nx= this%epoch_index(2) - this%epoch_index(1) + 1
               this%nobs_epoch = nx


               allocate(this%lons(nx),this%lats(nx),__STAT)
               allocate(this%times_R8(nx),__STAT)
               allocate(this%obstype_id(nx),__STAT)
               allocate(this%location_index_ioda(nx),__STAT)

               j=this%epoch_index(1)
               do i=1, nx
                  this%lons(i) = lons_full(j)
                  this%lats(i) = lats_full(j)
                  this%times_R8(i) = times_R8_full(j)
                  this%obstype_id(i) = obstype_id_full(j)
                  this%location_index_ioda(i) = location_index_ioda_full(j)
                  j=j+1
               enddo
               arr(1)=nx

               do k=1, this%nobs_type
                  this%obs(k)%nobs_epoch = 0
               enddo
               do j = this%epoch_index(1), this%epoch_index(2)
                  k = obstype_id_full(j)
                  this%obs(k)%nobs_epoch = this%obs(k)%nobs_epoch + 1
               enddo

               do k=1, this%nobs_type
                  nx2 = this%obs(k)%nobs_epoch
                  allocate (this%obs(k)%lons(nx2), __STAT)
                  allocate (this%obs(k)%lats(nx2), __STAT)
                  allocate (this%obs(k)%times_R8(nx2), __STAT)
                  allocate (this%obs(k)%location_index_ioda(nx2), __STAT)
                  if (this%use_NWP_1_file) then
                     allocate (this%obs(k)%restore_index(nx2), __STAT)
                  end if
               enddo

               allocate(ix(this%nobs_type), __STAT)
               ix(:)=0
               j=this%epoch_index(1)
               do i=1, nx
                  k = obstype_id_full(j)
                  ix(k) = ix(k) + 1
                  this%obs(k)%lons(ix(k)) = lons_full(j)
                  this%obs(k)%lats(ix(k)) = lats_full(j)
                  this%obs(k)%times_R8(ix(k)) = times_R8_full(j)
                  this%obs(k)%location_index_ioda(ix(k)) = location_index_ioda_full(j)
                  if (this%use_NWP_1_file) then
                     ! only this case, we have exact obs in 1_file <-> sampling match
                     this%obs(k)%restore_index(location_index_ioda_full(j)) = ix(k)
                  end if
                  !
                  !if (mod(k,10**8)==1) then
                  !   write(6,*) 'this%obs(k)%times_R8(ix(k))', this%obs(k)%times_R8(ix(k))
                  !endif
                  j=j+1
               enddo
               deallocate(ix, __STAT)
               deallocate(lons_full, lats_full, times_R8_full, obstype_id_full, location_index_ioda_full, __STAT)

               call lgr%debug('%a %i12 %i12 %i12', &
                    'epoch_index(1:2), nx', this%epoch_index(1), &
                    this%epoch_index(2), this%nobs_epoch)
               !
               !    Note: the time boundary issue can appear when we use python convention [T1, T2) but obs files donot
               !    ioda file split [1/2data 15Z  :  1/2data 21Z ]  [ 1/2data 21Z :  1/2data 3Z]   (aircraft)
               !        ___x  x  x x x ___ ---------------------------------- o --o ---o -- o --
               !    debug:  negative index (extra) at Tmin                    missing Tmax
               !    debug shows: overcount at Tmin and missing points at Tmax
               !    use_NPW_1_file=.true. solves this issue, due to special treatment at time boundaries
               !
            end if
         else
            allocate(this%lons(0),this%lats(0),__STAT)
            allocate(this%times_R8(0),__STAT)
            allocate(this%obstype_id(0),__STAT)
            allocate(this%location_index_ioda(0),__STAT)
            this%epoch_index(1:2)=0
            this%nobs_epoch = 0
            nx=0
            arr(1)=nx
         endif

         call ESMF_VMAllFullReduce(vm, sendData=arr, recvData=nx_sum, &
              count=1, reduceflag=ESMF_REDUCE_SUM, rc=rc)
         this%nobs_epoch_sum = nx_sum
         call lgr%debug('%a %i20', 'nobservation points=', nx_sum)

         !
         !__ s1. distrubute data chunk for the locstream points : mpi_scatterV
         !__ s2. create LS on parallel processors
         !       caution about zero-sized array for MPI
         !
         ip = mypet    ! 0 to M-1
         N = nx_sum
         M = petCount
         recvcount = int(ip+1, INT64) * int(N, INT64) / int(M, INT64) - &
                     int(ip  , INT64) * int(N, INT64) / int(M, INT64)
         call lgr%debug('%a %i12 %i12', 'ip, recvcount', ip, recvcount)

         allocate ( sendcount (petCount) )
         allocate ( displs    (petCount) )
         do ip=0, M-1
            sendcount(ip+1) = int(ip+1, INT64) * int(N, INT64) / int(M, INT64) - &
                              int(ip  , INT64) * int(N, INT64) / int(M, INT64)
         end do
         displs(1)=0
         do i = 2, petCount
            displs(i) = displs(i-1) + sendcount(i-1)
         end do

         allocate ( lons_chunk (recvcount) )
         allocate ( lats_chunk (recvcount) )
         allocate ( times_R8_chunk (recvcount) )

         arr(1) = recvcount
         call ESMF_VMAllFullReduce(vm, sendData=arr, recvData=nx2, &
              count=1, reduceflag=ESMF_REDUCE_SUM, rc=rc)
         __ASSERT( nx2 == nx_sum, 'Erorr in recvcount' )

         call MPI_Scatterv( this%lons, sendcount, &
              displs, MPI_REAL8,  lons_chunk, &
              recvcount, MPI_REAL8, 0, mpic, ierr)
         __VERIFY(ierr)

         call MPI_Scatterv( this%lats, sendcount, &
              displs, MPI_REAL8,  lats_chunk, &
              recvcount, MPI_REAL8, 0, mpic, ierr)
         __VERIFY(ierr)

         call MPI_Scatterv( this%times_R8, sendcount, &
              displs, MPI_REAL8,  times_R8_chunk, &
              recvcount, MPI_REAL8, 0, mpic, ierr)
         __VERIFY(ierr)

         ! -- root
         this%locstream_factory = LocStreamFactory(this%lons,this%lats,__RC)
         this%LS_rt = this%locstream_factory%create_locstream(__RC)

         ! -- proc
         this%locstream_factory = LocStreamFactory(lons_chunk,lats_chunk,__RC)
         this%LS_chunk = this%locstream_factory%create_locstream_on_proc(__RC)

         call ESMF_FieldBundleGet(this%bundle,grid=grid,__RC)
         this%LS_ds = this%locstream_factory%create_locstream_on_proc(grid=grid,__RC)

         this%fieldA = ESMF_FieldCreate (this%LS_chunk, name='A_time', typekind=ESMF_TYPEKIND_R8, __RC)
         this%fieldB = ESMF_FieldCreate (this%LS_ds, name='B_time', typekind=ESMF_TYPEKIND_R8, __RC)

         call ESMF_FieldGet( this%fieldA, localDE=0, farrayPtr=ptAT)
         call ESMF_FieldGet( this%fieldB, localDE=0, farrayPtr=this%obsTime)
         ptAT(:) = times_R8_chunk(:)
         this%obsTime= -1.d0

         call ESMF_FieldRedistStore (this%fieldA, this%fieldB, RH, __RC)
         call ESMF_FieldRedist      (this%fieldA, this%fieldB, RH, __RC)

         call ESMF_FieldRedistRelease(RH, noGarbage=.true., __RC)
         call ESMF_FieldDestroy(this%fieldA,nogarbage=.true.,__RC)
         ! defer destroy fieldB at regen_grid step
         !

         __RETURN(__SUCCESS)
       end procedure create_grid



      module procedure append_file
      use pflogger, only: Logger, logging
         type(GriddedIOitemVectorIterator) :: iter
         type(GriddedIOitem), pointer :: item
         type(ESMF_RouteHandle) :: RH
         type(Logger), pointer :: lgr

         type(ESMF_Field) :: src_field, dst_field
         type(ESMF_Field) :: acc_field
         type(ESMF_Field) :: acc_field_2d_rt, acc_field_3d_rt
         real(REAL32), pointer :: p_acc_3d(:,:),p_acc_2d(:)
         real(REAL32), pointer :: p_acc_rt_2d(:)
         real(REAL32), pointer :: p_src(:,:),p_dst(:,:), p_dst_t(:,:)   ! _t: transpose
         real(REAL32), pointer :: p_dst_rt(:,:), p_acc_rt_3d(:,:)
         real(REAL32), pointer :: pt1(:), pt2(:)
         real(REAL32), pointer :: p_rt_3d(:,:)
         real(REAL64), allocatable :: aux_R8(:)
         real(REAL64), allocatable :: aux_R4(:)
         integer, allocatable :: vec(:)

         type(ESMF_Field) :: acc_field_2d_chunk, acc_field_3d_chunk, chunk_field
         real(REAL32), pointer :: p_acc_chunk_3d(:,:),p_acc_chunk_2d(:)

         integer :: is, ie, nx
         integer :: lm
         integer :: rank
         integer :: status
         integer :: j, j2, k, kz, ig
         integer, allocatable :: ix(:)
         type(ESMF_VM) :: vm
         integer :: mypet, petcount, mpic, iroot

         integer :: na, nb, nx_sum, nsend
         integer, allocatable :: RecvCount(:), displs(:)
         integer :: i, ierr
         integer :: nsend_v
         integer, allocatable :: recvcount_v(:), displs_v(:)
         integer :: ip, M, N

         if (.NOT. this%active) then
            __RETURN(ESMF_SUCCESS)
         endif

         if (this%nobs_epoch_sum==0) then
            rc=0
            return
         endif
         lgr => logging%get_logger('HISTORY.sampler')

         is=1
         do k = 1, this%nobs_type
            !-- limit  nx < 2**32 (integer*4)
            nx=this%obs(k)%nobs_epoch
            if (nx >0) then
               if (mapl_am_i_root()) then
                  if (this%restore_2_obs_vector) then
                     ! restore back to obs vector
                     allocate (aux_R8(nx), vec(nx))
                     vec(1:nx) = this%obs(k)%restore_index(1:nx)
                     aux_R8(1:nx) = this%obs(k)%times_R8(vec(1:nx))
                     call this%obs(k)%file_handle%put_var(this%var_name_time, aux_R8, &
                          start=[is], count=[nx], __RC)
                     aux_R8(1:nx) = this%obs(k)%lons(vec(1:nx))
                     call this%obs(k)%file_handle%put_var(this%var_name_lon, aux_R8, &
                          start=[is], count=[nx], __RC)
                     aux_R8(1:nx) = this%obs(k)%lats(vec(1:nx))
                     call this%obs(k)%file_handle%put_var(this%var_name_lat, aux_R8, &
                          start=[is], count=[nx], __RC)
                     deallocate (aux_R8, vec)
                  else
                     ! default:  location in time sequence
                     call this%obs(k)%file_handle%put_var(this%var_name_time, this%obs(k)%times_R8, &
                          start=[is], count=[nx], __RC)
                     call this%obs(k)%file_handle%put_var(this%var_name_lon, this%obs(k)%lons, &
                          start=[is], count=[nx], __RC)
                     call this%obs(k)%file_handle%put_var(this%var_name_lat, this%obs(k)%lats, &
                          start=[is], count=[nx], __RC)
                     call this%obs(k)%file_handle%put_var(this%location_index_name, this%obs(k)%location_index_ioda, &
                          start=[is], count=[nx], __RC)
                  end if
               end if
            end if
         enddo

         ! get RH from 2d field
         src_field = ESMF_FieldCreate(this%LS_ds,typekind=ESMF_TYPEKIND_R4,gridToFieldMap=[1],__RC)
         chunk_field = ESMF_FieldCreate(this%LS_chunk,typekind=ESMF_TYPEKIND_R4,gridToFieldMap=[1],__RC)
         call ESMF_FieldGet( src_field, localDE=0, farrayPtr=pt1, __RC )
         call ESMF_FieldGet( chunk_field, localDE=0, farrayPtr=pt2, __RC )
         pt1=0.0
         pt2=0.0
         call ESMF_FieldRedistStore(src_field,chunk_field,RH,__RC)
         call ESMF_FieldDestroy(src_field,noGarbage=.true.,__RC)
         call ESMF_FieldDestroy(chunk_field,noGarbage=.true.,__RC)

         ! redist and put_var
         lm = this%vdata%lm
         acc_field_2d_rt = ESMF_FieldCreate (this%LS_rt, name='field_2d_rt', typekind=ESMF_TYPEKIND_R4, __RC)
         acc_field_3d_rt = ESMF_FieldCreate (this%LS_rt, name='field_3d_rt', typekind=ESMF_TYPEKIND_R4, &
              gridToFieldMap=[1],ungriddedLBound=[1],ungriddedUBound=[lm],__RC)

         acc_field_2d_chunk = ESMF_FieldCreate (this%LS_chunk, name='field_2d_chunk', typekind=ESMF_TYPEKIND_R4, __RC)
         acc_field_3d_chunk = ESMF_FieldCreate (this%LS_chunk, name='field_3d_chunk', typekind=ESMF_TYPEKIND_R4, &
              gridToFieldMap=[1],ungriddedLBound=[1],ungriddedUBound=[lm],__RC)

         !
         !   caution about zero-sized array for MPI
         !
         nx_sum = this%nobs_epoch_sum
         call ESMF_VMGetCurrent(vm,__RC)
         call ESMF_VMGet(vm, mpiCommunicator=mpic, petCount=petCount, localPet=mypet, __RC)

         iroot = 0
         ip = mypet
         N = nx_sum
         M = petCount
         nsend = int(ip+1, INT64) * int(N, INT64) / int(M, INT64) - &
                 int(ip  , INT64) * int(N, INT64) / int(M, INT64)
         allocate ( recvcount (petCount) )
         allocate ( displs    (petCount) )
         do ip=0, M-1
            recvcount(ip+1) =  int(ip+1, INT64) * int(N, INT64) / int(M, INT64) - &
                               int(ip  , INT64) * int(N, INT64) / int(M, INT64)
         end do
         displs(1)=0
         do i = 2, petCount
            displs(i) = displs(i-1) + recvcount(i-1)
         end do

         nsend_v = nsend * lm      ! vertical
         allocate (recvcount_v, source = recvcount * lm )
         allocate (displs_v, source = displs * lm )

         if (mapl_am_i_root()) then
            allocate ( p_acc_rt_2d(nx_sum) )
         else
            allocate ( p_acc_rt_2d(1) )
         end if
         !
         ! p_dst (lm, nx)
         if (mapl_am_i_root()) then
            allocate ( p_acc_rt_3d(nx_sum,lm) )
            allocate ( p_dst_rt(lm, nx_sum) )
         else
            allocate ( p_acc_rt_3d(1,lm) )
            allocate ( p_dst_rt(lm, 1) )
         end if


         iter = this%items%begin()
         do while (iter /= this%items%end())
            item => iter%get()
            !!if (mapl_am_I_root())  print*, 'regrid: item%xname= ', trim(item%xname)

            if (item%itemType == ItemTypeScalar) then
               call ESMF_FieldBundleGet(this%acc_bundle,trim(item%xname),field=acc_field,__RC)
               call ESMF_FieldGet(acc_field,rank=rank,__RC)
               if (rank==1) then
                  call ESMF_FieldGet( acc_field, localDE=0, farrayPtr=p_acc_2d, __RC )
                  call ESMF_FieldGet( acc_field_2d_chunk, localDE=0, farrayPtr=p_acc_chunk_2d, __RC )
                  call ESMF_FieldRedist( acc_field,  acc_field_2d_chunk, RH, __RC )
                  call MPI_gatherv ( p_acc_chunk_2d, nsend, MPI_REAL, &
                       p_acc_rt_2d, recvcount, displs, MPI_REAL,&
                       iroot, mpic, ierr )
                  __VERIFY(ierr)

                  if (mapl_am_i_root()) then
                     !
                     !-- pack fields to obs(k)%p2d and put_var
                     !
                     is=1
                     ie=this%epoch_index(2)-this%epoch_index(1)+1
                     do k=1, this%nobs_type
                        nx = this%obs(k)%nobs_epoch
                        allocate (this%obs(k)%p2d(nx), __STAT)
                     enddo

                     allocate(ix(this%nobs_type), __STAT)
                     ix(:)=0
                     do j=is, ie
                        k = this%obstype_id(j)
                        ix(k) = ix(k) + 1
                        this%obs(k)%p2d(ix(k)) = p_acc_rt_2d(j)
                     enddo

                     do k=1, this%nobs_type
                        if (ix(k) /= this%obs(k)%nobs_epoch) then
                           print*, 'obs_', k, ' : ix(k) /= this%obs(k)%nobs_epoch'
                           print*, 'obs_', k, ' : this%obs(k)%nobs_epoch, ix(k) =', this%obs(k)%nobs_epoch, ix(k)
                           __FAIL('test ix(k) failed')
                        endif
                     enddo
                     deallocate(ix, __STAT)

                     ! rotate 2d field
                     if (this%restore_2_obs_vector) then
                        do k=1, this%nobs_type
                           nx = this%obs(k)%nobs_epoch
                           if (nx>0) then
                              allocate (aux_R4(nx), vec(nx))
                              vec(1:nx) = this%obs(k)%restore_index(1:nx)
                              aux_R4(1:nx) = this%obs(k)%p2d(vec(1:nx))
                              this%obs(k)%p2d(1:nx) = aux_R4(1:nx)
                              deallocate (aux_R4, vec)
                           end if
                        end do
                     end if

                     do k=1, this%nobs_type
                        is = 1
                        nx = this%obs(k)%nobs_epoch
                        if (nx>0) then
                           if (this%schema_version==1) then
                              call this%obs(k)%file_handle%put_var(trim(item%xname), this%obs(k)%p2d(1:nx), &
                                   start=[is],count=[nx])
                           else
                              do ig = 1, this%obs(k)%ngeoval
                                 !! print*, 'this%obs(k)%geoval_xname(ig)= ', this%obs(k)%geoval_xname(ig)
                                 if (trim(item%xname) == trim(this%obs(k)%geoval_xname(ig))) then
                                    call this%obs(k)%file_handle%put_var(trim(item%xname), this%obs(k)%p2d(1:nx), &
                                         start=[is],count=[nx])
                                 end if
                              end do
                           end if
                        endif
                     enddo

                     do k=1, this%nobs_type
                        deallocate (this%obs(k)%p2d, __STAT)
                     enddo
                  end if
               else if (rank==2) then

                  call ESMF_FieldGet( acc_field, localDE=0, farrayPtr=p_acc_3d, __RC)
                  dst_field=ESMF_FieldCreate(this%LS_chunk,typekind=ESMF_TYPEKIND_R4, &
                       gridToFieldMap=[2],ungriddedLBound=[1],ungriddedUBound=[lm],__RC)
                  src_field=ESMF_FieldCreate(this%LS_ds,typekind=ESMF_TYPEKIND_R4, &
                       gridToFieldMap=[2],ungriddedLBound=[1],ungriddedUBound=[lm],__RC)

                  call ESMF_FieldGet(src_field,localDE=0,farrayPtr=p_src,__RC)
                  call ESMF_FieldGet(dst_field,localDE=0,farrayPtr=p_dst,__RC)
                  p_src= reshape(p_acc_3d,shape(p_src), order=[2,1])
                  call ESMF_FieldRegrid(src_field,dst_field,RH,__RC)

                  if (this%level_by_level) then
                     ! p_dst (lm, nx)
                     allocate ( p_dst_t, source = reshape ( p_dst, [size(p_dst,2),size(p_dst,1)], order=[2,1] ) )
                     do k = 1, lm
                        call MPI_gatherv ( p_dst_t(1,k), nsend, MPI_REAL, &
                             p_acc_rt_3d(1,k), recvcount, displs, MPI_REAL,&
                             iroot, mpic, ierr )
                        __VERIFY(ierr)
                     end do
                     deallocate (p_dst_t)
                  else
                     call MPI_gatherv ( p_dst, nsend_v, MPI_REAL, &
                          p_dst_rt, recvcount_v, displs_v, MPI_REAL,&
                          iroot, mpic, ierr )
                     __VERIFY(ierr)
                     p_acc_rt_3d = reshape ( p_dst_rt, shape(p_acc_rt_3d), order=[2,1] )
                  end if

                  call ESMF_FieldDestroy(dst_field,noGarbage=.true.,__RC)
                  call ESMF_FieldDestroy(src_field,noGarbage=.true.,__RC)

                  if (mapl_am_i_root()) then
                     !
                     !-- pack fields to obs(k)%p3d and put_var
                     !
                     is=1
                     ie=this%epoch_index(2)-this%epoch_index(1)+1
                     do k=1, this%nobs_type
                        nx = this%obs(k)%nobs_epoch
                        allocate (this%obs(k)%p3d(nx, size(p_acc_rt_3d,2)), __STAT)
                     enddo
                     allocate(ix(this%nobs_type), __STAT)
                     ix(:)=0
                     do j=is, ie
                        k = this%obstype_id(j)
                        ix(k) = ix(k) + 1
                        this%obs(k)%p3d(ix(k),:) = p_acc_rt_3d(j,:)
                     enddo
                     deallocate(ix, __STAT)

                     ! rotate 3d field
                     if (this%restore_2_obs_vector) then
                        do k=1, this%nobs_type
                           nx = this%obs(k)%nobs_epoch
                           if (nx>0) then
                              allocate (aux_R4(nx), vec(nx))
                              vec(1:nx) = this%obs(k)%restore_index(1:nx)
                              do kz=1, lm
                                 aux_R4(1:nx) = this%obs(k)%p3d(vec(1:nx), kz)
                                 this%obs(k)%p3d(1:nx, kz) = aux_R4(1:nx)
                              end do
                              deallocate (aux_R4, vec)
                           end if
                        end do
                     end if

                     do k=1, this%nobs_type
                        is = 1
                        nx = this%obs(k)%nobs_epoch
                        if (this%write_lev_first) then
                           ! p_rt_3d  --> [nz,nx] for output nc4
                           allocate(p_rt_3d, source=reshape(this%obs(k)%p3d, &
                                [size(this%obs(k)%p3d,2),size(this%obs(k)%p3d,1)], order=[2,1]))
                        endif
                        if (nx>0) then
                           if (this%schema_version==1) then
                              if (this%write_lev_first) then
                                 call this%obs(k)%file_handle%put_var(trim(item%xname), p_rt_3d(:,:), &
                                      start=[1,is],count=[size(p_acc_rt_3d,2),nx])
                                 !     lev,nx
                              else
                                 call this%obs(k)%file_handle%put_var(trim(item%xname), this%obs(k)%p3d(:,:), &
                                      start=[is,1],count=[nx,size(p_acc_rt_3d,2)])
                              end if
                           else
                              do ig = 1, this%obs(k)%ngeoval
                                 if (trim(item%xname) == trim(this%obs(k)%geoval_xname(ig))) then
                                    if (this%write_lev_first) then
                                       call this%obs(k)%file_handle%put_var(trim(item%xname), p_rt_3d(:,:), &
                                            start=[1,is],count=[size(p_acc_rt_3d,2),nx])
                                       !      lev,nx
                                    else
                                       call this%obs(k)%file_handle%put_var(trim(item%xname), this%obs(k)%p3d(:,:), &
                                            start=[is,1],count=[nx,size(p_acc_rt_3d,2)])
                                    end if
                                 end if
                              end do
                           end if
                        endif
                        if (this%write_lev_first) then
                           deallocate(p_rt_3d, __STAT)
                        end if
                        deallocate (this%obs(k)%p3d, __STAT)
                     enddo
                  end if
               endif

            else if (item%itemType == ItemTypeVector) then
               __FAIL("ItemTypeVector not yet supported")
            end if
            call iter%next()
         enddo
         call ESMF_FieldDestroy(acc_field_2d_chunk, noGarbage=.true., __RC)
         call ESMF_FieldDestroy(acc_field_3d_chunk, noGarbage=.true., __RC)
         call ESMF_FieldRedistRelease(RH, noGarbage=.true., __RC)

         __RETURN(__SUCCESS)
       end procedure append_file


         module procedure regrid_accumulate_on_xsubset
           integer                 :: x_subset(2)
           type(ESMF_Time)         :: timeset(2)
           type(ESMF_Time)         :: current_time
           type(ESMF_TimeInterval) :: dur, delT
           type(GriddedIOitemVectorIterator) :: iter
           type(GriddedIOitem), pointer :: item
           type(ESMF_Field) :: src_field,dst_field,acc_field
           integer :: rank
           real(REAL32), allocatable :: p_new_lev(:,:,:)
           real(REAL32), pointer :: p_src_3d(:,:,:),p_src_2d(:,:)
           real(REAL32), pointer :: p_dst_3d(:,:),p_dst_2d(:)
           real(REAL32), pointer :: p_acc_3d(:,:),p_acc_2d(:)
           type(ESMF_VM) :: vm
           integer :: mypet, petcount
           integer :: is, ie, nx_sum
           integer :: status
           integer :: arr(1)


           if (.NOT. this%active) then
              __RETURN(ESMF_SUCCESS)
           endif

           if (this%nobs_epoch_sum==0) then
              __RETURN(ESMF_SUCCESS)
           endif

           if (this%nobs_epoch_sum==0) then
              rc=0
              return
           endif

           call ESMF_ClockGet(this%clock,currTime=current_time,__RC)
           call ESMF_ClockGet(this%clock,timeStep=dur, __RC )
           timeset(1) = current_time - dur
           timeset(2) = current_time
           if (this%use_NWP_1_file) then
              !
              ! change UB to Epoch + 1 s to be inclusive for IODA
              if ( ESMF_AlarmIsRinging (this%alarm) ) then
                 call ESMF_TimeIntervalSet(delT, s=1, __RC)
                 timeset(2) = current_time + delT
              end if
           end if
           call this%get_x_subset(timeset, x_subset, __RC)
           is=x_subset(1)
           ie=x_subset(2)

           !
           ! __ I designed a method to return from regridding if no valid points exist
           !    in reality for 29 ioda platforms and dt > 20 sec, we donot need this
           !
           !!arr(1)=1
           !!if (.NOT. (is > 0 .AND. is <= ie ))  arr(1)=0
           !!call ESMF_VMGetCurrent(vm,__RC)
           !!call ESMF_VMGet(vm, localPet=mypet, petCount=petCount, __RC)
           !!call ESMF_VMAllFullReduce(vm, sendData=arr, recvData=nx_sum, &
           !!   count=1, reduceflag=ESMF_REDUCE_SUM, rc=rc)
           !!if ( nx_sum == 0 ) then
           !!   write(6, '(2x,a,2x,3i10)')  'invalid points, mypet, is, ie =', mypet, is, ie
           !!   ! no valid points to regrid
           !!   __RETURN(ESMF_SUCCESS)
           !!else
           !!   write(6, '(2x,a,2x,3i10)')  '  valid points, mypet, is, ie =', mypet, is, ie
           !!end if


           if (this%vdata%regrid_type==VERTICAL_METHOD_ETA2LEV) then
              call this%vdata%setup_eta_to_pressure(__RC)
           endif

           iter = this%items%begin()
           do while (iter /= this%items%end())
              item => iter%get()
              if (item%itemType == ItemTypeScalar) then
                 call ESMF_FieldBundleGet(this%bundle,trim(item%xname),field=src_field,__RC)
                 call ESMF_FieldBundleGet(this%output_bundle,trim(item%xname),field=dst_field,__RC)
                 call ESMF_FieldBundleGet(this%acc_bundle,trim(item%xname),field=acc_field,__RC)
                 call ESMF_FieldGet(src_field,rank=rank,__RC)
                 if (rank==2) then
                    call ESMF_FieldGet(src_field,farrayptr=p_src_2d,__RC)
                    call ESMF_FieldGet(dst_field,farrayptr=p_dst_2d,__RC)
                    call ESMF_FieldGet(acc_field,farrayptr=p_acc_2d,__RC)

                    !! print*, 'size(src,dst,acc)', size(p_src_2d), size(p_dst_2d), size(p_acc_2d)
                    call this%regridder%regrid(p_src_2d,p_dst_2d,__RC)
                    if (is > 0 .AND. is <= ie ) then
                       p_acc_2d(is:ie) = p_dst_2d(is:ie)
                    endif

                    !!if (is>0) write(6,'(a)')  'regrid_accu:  p_dst_2d'
                    !!if (is>0) write(6,'(10f7.1)')  p_dst_2d

                 else if (rank==3) then
                    call ESMF_FieldGet(src_field,farrayptr=p_src_3d,__RC)
                    call ESMF_FieldGet(dst_field,farrayptr=p_dst_3d,__RC)
                    call ESMF_FieldGet(acc_field,farrayptr=p_acc_3d,__RC)
                    if (this%vdata%regrid_type==VERTICAL_METHOD_ETA2LEV) then
                       allocate(p_new_lev(size(p_src_3d,1),size(p_src_3d,2),this%vdata%lm),__STAT)
                       call this%vdata%regrid_eta_to_pressure(p_src_3d,p_new_lev,__RC)
                       call this%regridder%regrid(p_new_lev,p_dst_3d,__RC)
                       if (is > 0 .AND. is <= ie ) then
                          p_acc_3d(is:ie,:) = p_dst_3d(is:ie,:)
                       end if
                    else
                       call this%regridder%regrid(p_src_3d,p_dst_3d,__RC)
                       if (is > 0 .AND. is <= ie ) then
                          p_acc_3d(is:ie,:) = p_dst_3d(is:ie,:)
                       end if
                    end if
                 end if
              else if (item%itemType == ItemTypeVector) then
                 __FAIL("ItemTypeVector not yet supported")
              end if
              call iter%next()
           enddo

           __RETURN(ESMF_SUCCESS)

         end procedure regrid_accumulate_on_xsubset


         module procedure destroy_rh_regen_LS
           integer :: status
           integer :: numVars, i, k
           character(len=ESMF_MAXSTR), allocatable :: names(:)
           type(ESMF_Field) :: field
           type(ESMF_Time)  :: currTime

          if (.NOT. this%active) then
             __RETURN(ESMF_SUCCESS)
          endif

           call ESMF_FieldDestroy(this%fieldB,nogarbage=.true.,__RC)
           call this%locstream_factory%destroy_locstream(this%LS_rt, __RC)
           call this%locstream_factory%destroy_locstream(this%LS_ds, __RC)
           call this%regridder%destroy(__RC)
           deallocate (this%lons, this%lats, &
                this%times_R8, this%obstype_id, this%location_index_ioda, __STAT)

           do k=1, this%nobs_type
              deallocate (this%obs(k)%metadata, __STAT)
              if (mapl_am_i_root()) then
                 deallocate (this%obs(k)%file_handle, __STAT)
              end if
           end do

           if (mapl_am_i_root()) then
              do k=1, this%nobs_type
                 if (allocated (this%obs(k)%lons)) then
                    deallocate (this%obs(k)%lons)
                 end if
                 if (allocated (this%obs(k)%lats)) then
                    deallocate (this%obs(k)%lats)
                 end if
                 if (allocated (this%obs(k)%times_R8)) then
                    deallocate (this%obs(k)%times_R8)
                 end if
                 if (allocated (this%obs(k)%location_index_ioda)) then
                    deallocate (this%obs(k)%location_index_ioda)
                 end if
                 if (allocated (this%obs(k)%restore_index)) then
                    deallocate (this%obs(k)%restore_index)
                 end if
                 if (allocated(this%obs(k)%p2d)) then
                    deallocate (this%obs(k)%p2d)
                 endif
                 if (allocated(this%obs(k)%p3d)) then
                    deallocate (this%obs(k)%p3d)
                 endif
              end do
           end if

           call ESMF_FieldBundleGet(this%acc_bundle,fieldCount=numVars,__RC)
           allocate(names(numVars), __STAT)
           call ESMF_FieldBundleGet(this%acc_bundle,fieldNameList=names,__RC)
           do i=1,numVars
              call ESMF_FieldBundleGet(this%acc_bundle,trim(names(i)),field=field,__RC)
              call ESMF_FieldDestroy(field,noGarbage=.true., __RC)
           enddo
           call ESMF_FieldBundleDestroy(this%acc_bundle,noGarbage=.true.,__RC)
           deallocate(names, __STAT)

           call ESMF_FieldBundleGet(this%output_bundle,fieldCount=numVars,__RC)
           allocate(names(numVars), __STAT)
           call ESMF_FieldBundleGet(this%output_bundle,fieldNameList=names,__RC)
           do i=1,numVars
              call ESMF_FieldBundleGet(this%output_bundle,trim(names(i)),field=field,__RC)
              call ESMF_FieldDestroy(field,noGarbage=.true., __RC)
           enddo
           call ESMF_FieldBundleDestroy(this%output_bundle,noGarbage=.true.,__RC)
           deallocate(names, __STAT)

           this%epoch_index(1:2)=0

           call this%initialize(reinitialize=.true., __RC)

           __RETURN(ESMF_SUCCESS)

         end procedure destroy_rh_regen_LS


         module procedure get_x_subset
           type   (ESMF_Time)    :: T1,  T2
           real   (ESMF_KIND_R8) :: rT1, rT2

           integer(ESMF_KIND_I8) :: i1,  i2
           integer(ESMF_KIND_I8) :: index1, index2, lb, ub
           integer               :: jlo, jhi
           integer               :: status

           T1= interval(1)
           T2= interval(2)
           call time_esmf_2_nc_int (T1, this%datetime_units, i1, __RC)
           call time_esmf_2_nc_int (T2, this%datetime_units, i2, __RC)
           rT1=real(i1, kind=ESMF_KIND_R8)
           rT2=real(i2, kind=ESMF_KIND_R8)
           jlo = 1
           !!
           !! I choose UB = N+1 not N, because my sub. bisect find n: Y(n)<x<=Y(n+1)
           jhi= size(this%obstime) + 1

           !write(6,*) 'size(this%obstime)=', size(this%obstime)

           if (jhi==1) then
              x_subset(1:2)=0
              __RETURN(__SUCCESS)
           endif

           lb=int(jlo, ESMF_KIND_I8)
           ub=int(jhi, ESMF_KIND_I8)
           call bisect( this%obstime, rT1, index1, n_LB=lb, n_UB=ub, rc=rc)
           call bisect( this%obstime, rT2, index2, n_LB=lb, n_UB=ub, rc=rc)

           ! (x1, x2]  design in bisect:  y(n) < x <= y(n+1),  n is output index

           x_subset(1) = index1+1
           x_subset(2) = index2

!!           write(6,'(2x,a,2x,2i10)')  'mod vers. get_x_subset, LB,UB=', x_subset(1:2)
           __RETURN(__SUCCESS)
         end procedure get_x_subset


         function extract_unquoted_item(string_list) result(item)
           character(:), allocatable :: item
           character(*), intent(in) :: string_list

           integer :: i
           integer :: j

           character(1) :: QUOTE = "'"

           i = index(string_list(  1:), QUOTE)
           j = index(string_list(i+1:), QUOTE)+i
           if( i.ne.0 ) then
              item = adjustl( string_list(i+1:j-1) )
           else
              item = adjustl( string_list)
           endif
         end function extract_unquoted_item


end submodule HistoryTrajectory_implement
