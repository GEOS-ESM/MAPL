#include "MAPL_ErrLog.h"
#include "unused_dummy.H"

module HistoryTrajectoryMod

   use ESMF
   use MAPL_ErrorHandlingMod
   use MAPL_KeywordEnforcerMod
   use MAPL_FileMetadataUtilsMod
   use LocStreamFactoryMod
   use pFIO
   use MAPL_GriddedIOItemVectorMod
   use MAPL_GriddedIOItemMod
   use MAPL_TimeDataMod
   use MAPL_VerticalDataMod
   use MAPL_BaseMod
   use MAPL_CommsMod
   use MAPL_SortMod
   use MAPL_NetCDF
   use MAPL_plain_netCDF_Time
   use MAPL_LocstreamRegridderMod
   use, intrinsic :: iso_fortran_env, only: REAL32
   use, intrinsic :: iso_fortran_env, only: REAL64
   implicit none
   private

   public :: HistoryTrajectory

   type :: HistoryTrajectory
      private
      type(ESMF_LocStream) :: LS_rt
      type(ESMF_LocStream) :: LS_ds
      type(LocStreamFactory) :: locstream_factory
      type(ESMF_Time), allocatable :: times(:)
      real(kind=REAL64), allocatable :: times_R8(:)
      real(kind=REAL64), allocatable :: lons(:),lats(:)
      type(ESMF_FieldBundle) :: bundle
      type(ESMF_FieldBundle) :: output_bundle
      integer :: number_written
      type(GriddedIOitemVector) :: items
      type(FileMetadata) :: metadata
      type(VerticalData) :: vdata
      logical :: do_vertical_regrid
      type(NetCDF4_FileFormatter) :: file_handle
      type(LocstreamRegridder) :: regridder
      integer :: previous_index
      type(ESMF_Time) :: previous_time
      character(LEN=ESMF_MAXPATHLEN) :: file_name
      type(TimeData) :: time_info
      logical :: recycle_track
      type(ESMF_Clock)         :: clock
      type(ESMF_Alarm), public :: alarm
      type(ESMF_Time)          :: RingTime
      type(ESMF_TimeInterval)  :: Frequency_epoch

      character(len=ESMF_MAXSTR)     :: obsFile
      character(len=ESMF_MAXSTR)     :: nc_index
      character(len=ESMF_MAXSTR)     :: nc_time
      character(len=ESMF_MAXSTR)     :: nc_latitude
      character(len=ESMF_MAXSTR)     :: nc_longitude
      character(len=ESMF_MAXSTR)     :: var_name_time
      character(len=ESMF_MAXSTR)     :: var_name_lat
      character(len=ESMF_MAXSTR)     :: var_name_lon
      character(len=ESMF_MAXSTR)     :: datetime_units
      integer :: epoch                         ! unit: second
      integer(kind=ESMF_KIND_I8)     :: epoch_index(2)
      integer(ESMF_KIND_I4), pointer :: seqIndex(:)
      real(kind=ESMF_KIND_R8), pointer:: obsTime(:)

      !!integer(ESMF_KIND_I4), pointer :: ptAI(:), ptBI(:)
      
      contains
         procedure :: initialize
         procedure :: create_variable
         procedure :: create_file_handle
         procedure :: close_file_handle
         procedure :: append_file
         procedure :: get_current_interval
         procedure :: compute_times_for_interval
         procedure :: create_output_bundle
         procedure :: get_file_start_time
         procedure :: get
         procedure :: reset_times_to_current_day
         procedure :: sort_arrays_by_time
         procedure :: time_real_to_ESMF

         procedure :: create_grid
         procedure :: regrid_accumulate => regrid_accumulate_on_xsubset
!         procedure :: destroy_rh_regen_ogrid
         procedure :: get_x_subset
   end type

   interface HistoryTrajectory
      module procedure HistoryTrajectory_from_config
   end interface HistoryTrajectory

   contains

      function HistoryTrajectory_from_config(config,string,clock,unusable,rc) result(traj)
         use pflogger, only : Logger, logging
         type(HistoryTrajectory) :: traj
         type(ESMF_Config), intent(inout) :: config
         character(len=*), intent(in)     :: string
         type(ESMF_Clock),  intent(in)    :: clock
         class (KeywordEnforcer), optional, intent(in) :: unusable
         integer, optional, intent(out) :: rc
         integer :: status

         character(len=ESMF_MAXSTR) :: filename
         type(NetCDF4_FileFormatter) :: formatter
         type(FileMetadataUtils) :: metadata_utils
         type(FileMetadata) :: basic_metadata
         integer(ESMF_KIND_I8) :: num_times

         integer :: ncid, grpid, ncid0
         integer :: dimid(10),  dimlen(10)
         integer :: len
         integer :: i
         character(len=ESMF_MAXSTR) :: grp_name
         character(len=ESMF_MAXSTR) :: dim_name(10)
         character(len=ESMF_MAXSTR) :: var_name_lon
         character(len=ESMF_MAXSTR) :: var_name_lat
         character(len=ESMF_MAXSTR) :: var_name_time
         integer :: time_integer, second
         type(ESMF_TimeInterval)    :: Frequency_epoch
         type(ESMF_Time)            :: currTime

         type(Logger), pointer :: lgr

         _UNUSED_DUMMY(unusable)

         ! _ get variable, set alarm
         !
         call ESMF_ConfigGetAttribute(config, value=traj%obsFile, default="", &
              label=trim(string) // 'track_file:', _RC)
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
         call ESMF_TimeIntervalSet(frequency_epoch, s=second, _RC)
         traj%Epoch = time_integer
         traj%frequency_epoch = frequency_epoch
         traj%RingTime  = currTime
         traj%alarm = ESMF_AlarmCreate( clock=clock, RingInterval=Frequency_epoch, &
              RingTime=traj%RingTime, sticky=.false., _RC )         

         _RETURN(_SUCCESS)

      end function HistoryTrajectory_from_config


      subroutine initialize(this,items,bundle,timeInfo,unusable,vdata,recycle_track,rc)
         class(HistoryTrajectory), intent(inout) :: this
         type(GriddedIOitemVector), target, intent(inout) :: items
         type(ESMF_FieldBundle), intent(inout) :: bundle
         type(TimeData), intent(inout) :: timeInfo
         class (KeywordEnforcer), optional, intent(in) :: unusable
         type(VerticalData), optional, intent(inout) :: vdata
         logical, optional, intent(inout) :: recycle_track
         integer, optional, intent(out) :: rc

         integer :: status,nobs
         type(ESMF_Grid) :: grid
         type(ESMF_Clock) :: clock
         type(variable) :: v
         type(GriddedIOitemVectorIterator) :: iter
         type(GriddedIOitem), pointer :: item

         _UNUSED_DUMMY(unusable)


         ! __ s.1   create LS grid
         ! __ s.2   metadata, output / acc field
         ! __ s.3   RH



         call this%create_grid(_RC)

         
         this%bundle=bundle
         this%items=items
         
         if (present(vdata)) then
            this%vdata=vdata
         else
            this%vdata=VerticalData(_RC)
         end if
         call this%vdata%append_vertical_metadata(this%metadata,this%bundle,_RC)
         this%do_vertical_regrid = (this%vdata%regrid_type /= VERTICAL_METHOD_NONE)
         if (this%vdata%regrid_type == VERTICAL_METHOD_ETA2LEV) call this%vdata%get_interpolating_variable(this%bundle,_RC)

         call timeInfo%add_time_to_metadata(this%metadata,_RC)
         this%time_info = timeInfo
         !-- no use -- ! nobs = size(this%times)

         
         v = variable(type=PFIO_REAL64,dimensions="time")
         call v%add_attribute('units','degrees_east')
         call v%add_attribute('long_name','longitude')
         call this%metadata%add_variable(trim('longitude'),v)
         v = variable(type=PFIO_REAL64,dimensions="time")
         call v%add_attribute('units','degrees_east')
         call v%add_attribute('long_name','latitude')
         call this%metadata%add_variable(trim('latitude'),v)

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


!!         STOP -1
         this%epoch_index(1:2)=0
         this%number_written = 0
         this%previous_index = lbound(this%times,1)-1
         call timeInfo%get(clock=clock,_RC)
         call ESMF_ClockGet(clock,currTime=this%previous_time,_RC)

!         this%regridder = LocStreamRegridder(grid,this%LS_rt,_RC)

         !! -- bug YGYU here -- solve later
         !! call this%create_output_bundle(_RC)
         this%file_name = ''

         ! swath
         ! time-indepent -- simple
         ! donot have root -- distribute -- on MPI

         !  
         !   [ 1, 10 min ]
         !   send time index to distrute
         !   collect
         !   each PET, check and collect.
         !
         
         ! heartbeat

         
         this%recycle_track=.false.
         if (present(recycle_track)) then
            this%recycle_track=recycle_track
         end if
         if (this%recycle_track) then
            call this%reset_times_to_current_day(_RC)
         end if
         _RETURN(_SUCCESS)

      end subroutine initialize

      

      subroutine create_variable(this,vname,rc)
         class(HistoryTrajectory), intent(inout) :: this
         character(len=*), intent(in) :: vname
         integer, optional, intent(out) :: rc

         integer :: status,field_rank
         type(ESMF_Field) :: field
         character(len=ESMF_MAXSTR) :: var_name,long_name,units,vdims
         type(variable) :: v
         logical :: is_present

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

      end subroutine create_variable

      subroutine create_output_bundle(this,rc)
         class(HistoryTrajectory), intent(inout) :: this
         integer, optional, intent(out) :: rc

         integer :: status
         type(GriddedIOitemVectorIterator) :: iter
         type(GriddedIOitem), pointer :: item
         type(ESMF_Field) :: src_field,dst_field
         integer :: rank,lb(1),ub(1)

         this%output_bundle = ESMF_FieldBundleCreate(_RC)
         iter = this%items%begin()
         do while (iter /= this%items%end())
            item => iter%get()
            if (item%itemType == ItemTypeScalar) then
               call ESMF_FieldBundleGet(this%bundle,trim(item%xname),field=src_field,_RC)
               call ESMF_FieldGet(src_field,rank=rank,_RC)
               if (rank==2) then
                  dst_field = ESMF_FieldCreate(this%LS_rt,name=trim(item%xname), &
                     typekind=ESMF_TYPEKIND_R4,_RC)
               else if (rank==3) then
                  call ESMF_FieldGet(src_field,ungriddedLBound=lb,ungriddedUBound=ub,_RC)
                  if (this%vdata%lm/=(ub(1)-lb(1)+1)) then
                     lb(1)=1
                     ub(1)=this%vdata%lm
                  end if

                  dst_field = ESMF_FieldCreate(this%LS_rt,name=trim(item%xname), &
                       typekind=ESMF_TYPEKIND_R4,ungriddedLBound=lb,ungriddedUBound=ub,_RC)
               end if
               call MAPL_FieldBundleAdd(this%output_bundle,dst_field,_RC)
            else if (item%itemType == ItemTypeVector) then
               _FAIL("ItemTypeVector not yet supported")
            end if
            call iter%next()
         enddo

      end subroutine create_output_bundle

      subroutine create_file_handle(this,filename,rc)
         class(HistoryTrajectory), intent(inout) :: this
         character(len=*), intent(inout) :: filename
         integer, optional, intent(out) :: rc

         type(variable) :: v
         integer :: status

         this%file_name = trim(filename)
         v = this%time_info%define_time_variable(_RC)
         call this%metadata%modify_variable('time',v,_RC)
         if (mapl_am_I_root()) then
            call this%file_handle%create(trim(filename),_RC)
            call this%file_handle%write(this%metadata,_RC)
         end if
         this%number_written = 0

      end subroutine create_file_handle

      subroutine close_file_handle(this,rc)
         class(HistoryTrajectory), intent(inout) :: this
         integer, optional, intent(out) :: rc
         integer :: status

         if (trim(this%file_name) /= '') then
            if (mapl_am_i_root()) then
               call this%file_handle%close(_RC)
            end if
         end if

         _RETURN(_SUCCESS)

      end subroutine close_file_handle

      subroutine append_file(this,current_time,rc)
         class(HistoryTrajectory), intent(inout) :: this
         type(ESMF_Time), intent(inout) :: current_time
         integer, optional, intent(out) :: rc

         integer :: status
         type(GriddedIOitemVectorIterator) :: iter
         type(GriddedIOitem), pointer :: item
         type(ESMF_Field) :: src_field,dst_field
         integer :: rank,interval(2),number_to_write,previous_day,current_day
         real(kind=REAL32), allocatable :: p_new_lev(:,:,:)
         real(kind=REAL32), pointer :: p_src_3d(:,:,:),p_src_2d(:,:)
         real(kind=REAL32), pointer :: p_dst_3d(:,:),p_dst_2d(:)
         real(kind=ESMF_KIND_R8), allocatable :: rtimes(:)

         interval = this%get_current_interval(current_time)
         if (all(interval==0)) then
            number_to_write = 0
         else
            number_to_write = interval(2)-interval(1)+1
         end if
         if (number_to_write>0) then
            rtimes = this%compute_times_for_interval(interval,_RC)
            if (this%vdata%regrid_type==VERTICAL_METHOD_ETA2LEV) then
               call this%vdata%setup_eta_to_pressure(_RC)
            end if
            if (mapl_am_i_root()) then
               call this%file_handle%put_var('time',rtimes,&
                 start=[this%number_written+1],count=[number_to_write],_RC)
               call this%file_handle%put_var('longitude',this%lons(interval(1):interval(2)),&
                 start=[this%number_written+1],count=[number_to_write],_RC)
               call this%file_handle%put_var('latitude',this%lats(interval(1):interval(2)),&
                 start=[this%number_written+1],count=[number_to_write],_RC)
            end if
            deallocate(rtimes)
            iter = this%items%begin()
            do while (iter /= this%items%end())
               item => iter%get()
               if (item%itemType == ItemTypeScalar) then
                  call ESMF_FieldBundleGet(this%bundle,trim(item%xname),field=src_field,_RC)
                  call ESMF_FieldBundleGet(this%output_bundle,trim(item%xname),field=dst_field,_RC)
                  call ESMF_FieldGet(src_field,rank=rank,_RC)
                  if (rank==2) then
                     call ESMF_FieldGet(src_field,farrayptr=p_src_2d,_RC)
                     call ESMF_FieldGet(dst_field,farrayptr=p_dst_2d,_RC)
                     call this%regridder%regrid(p_src_2d,p_dst_2d,_RC)
                     if (mapl_am_i_root()) then
                        call this%file_handle%put_var(trim(item%xname),p_dst_2d(interval(1):interval(2)),&
                          start=[this%number_written+1],count=[number_to_write])
                     end if
                  else if (rank==3) then
                     call ESMF_FieldGet(src_field,farrayptr=p_src_3d,_RC)
                     call ESMF_FieldGet(dst_field,farrayptr=p_dst_3d,_RC)
                     if (this%vdata%regrid_type==VERTICAL_METHOD_ETA2LEV) then
                        allocate(p_new_lev(size(p_src_3d,1),size(p_src_3d,2),this%vdata%lm),_STAT)
                        call this%vdata%regrid_eta_to_pressure(p_src_3d,p_new_lev,_RC)
                        call this%regridder%regrid(p_new_lev,p_dst_3d,_RC)
                     else
                        call this%regridder%regrid(p_src_3d,p_dst_3d,_RC)
                     end if
                     if (mapl_am_i_root()) then
                        call this%file_handle%put_var(trim(item%xname),p_dst_3d(interval(1):interval(2),:),&
                          start=[this%number_written+1,1],count=[number_to_write,size(p_dst_3d,2)])
                     end if
                  end if
               else if (item%itemType == ItemTypeVector) then
                  _FAIL("ItemTypeVector not yet supported")
               end if
               call iter%next()
            enddo
            this%number_written=this%number_written+number_to_write
         endif

         call ESMF_TimeGet(this%previous_time,dd=previous_day,_RC)
         call ESMF_TimeGet(current_time,dd=current_day,_RC)
         if (this%recycle_track .and. (current_day/=previous_day)) then
            call this%reset_times_to_current_day(_RC)
            this%previous_index = lbound(this%times,1)-1
         end if
         this%previous_time=current_time

      end subroutine append_file

      subroutine get_file_start_time(this,start_time,time_units,rc)
         class(HistoryTrajectory), intent(inout) :: this
         type(ESMF_Time), intent(inout) :: start_time
         character(len=*), intent(inout) :: time_units
         integer, optional, intent(out) :: rc

         integer :: status
         class(Variable), pointer :: var
         type(Attribute), pointer :: attr
         class(*), pointer :: pTimeUnits
         character(len=ESMF_MAXSTR) :: timeUnits

         integer ypos(2), mpos(2), dpos(2), hpos(2), spos(2)
         integer strlen
         integer firstdash, lastdash
         integer firstcolon, lastcolon
         integer lastspace,since_pos
         integer year,month,day,hour,min,sec

         var => this%metadata%get_variable('time',_RC)
         attr => var%get_attribute('units')
         ptimeUnits => attr%get_value()
         select type(pTimeUnits)
         type is (character(*))
            timeUnits = pTimeUnits
            strlen = LEN_TRIM (TimeUnits)

            since_pos = index(TimeUnits, 'since')
            time_units = trim(TimeUnits(:since_pos-1))
            time_units = trim(time_units)

            firstdash = index(TimeUnits, '-')
            lastdash  = index(TimeUnits, '-', BACK=.TRUE.)

            if (firstdash .LE. 0 .OR. lastdash .LE. 0) then
              rc = -1
              return
            endif
            ypos(2) = firstdash - 1
            mpos(1) = firstdash + 1
            ypos(1) = ypos(2) - 3

            mpos(2) = lastdash - 1
            dpos(1) = lastdash + 1
            dpos(2) = dpos(1) + 1

            read ( TimeUnits(ypos(1):ypos(2)), * ) year
            read ( TimeUnits(mpos(1):mpos(2)), * ) month
            read ( TimeUnits(dpos(1):dpos(2)), * ) day

            firstcolon = index(TimeUnits, ':')
            if (firstcolon .LE. 0) then

              ! If no colons, check for hour.

              ! Logic below assumes a null character or something else is after the hour
              ! if we do not find a null character add one so that it correctly parses time
              if (TimeUnits(strlen:strlen) /= char(0)) then
                 TimeUnits = trim(TimeUnits)//char(0)
                 strlen=len_trim(TimeUnits)
              endif
              lastspace = index(TRIM(TimeUnits), ' ', BACK=.TRUE.)
              if ((strlen-lastspace).eq.2 .or. (strlen-lastspace).eq.3) then
                hpos(1) = lastspace+1
                hpos(2) = strlen-1
                read (TimeUnits(hpos(1):hpos(2)), * ) hour
                min  = 0
                sec  = 0
              else
                hour = 0
                min  = 0
                sec  = 0
              endif

            else
              hpos(1) = firstcolon - 2
              hpos(2) = firstcolon - 1
              lastcolon =  index(TimeUnits, ':', BACK=.TRUE.)
              if ( lastcolon .EQ. firstcolon ) then
                mpos(1) = firstcolon + 1
                mpos(2) = firstcolon + 2
                read (TimeUnits(hpos(1):hpos(2)), * ) hour
                read (TimeUnits(mpos(1):mpos(2)), * ) min
                sec = 0
              else
                mpos(1) = firstcolon + 1
                mpos(2) = lastcolon - 1
                spos(1) = lastcolon + 1
                spos(2) = lastcolon + 2
                read (TimeUnits(hpos(1):hpos(2)), * ) hour
                read (TimeUnits(mpos(1):mpos(2)), * ) min
                read (TimeUnits(spos(1):spos(2)), * ) sec
              endif
            endif
         class default
            _FAIL("Time unit must be character")
         end select
         call ESMF_TimeSet(start_time,yy=year,mm=month,dd=day,h=hour,m=min,s=sec,_RC)
         _RETURN(_SUCCESS)

      end subroutine get_file_start_time

      subroutine get(this, file_name, rc)
         class(HistoryTrajectory), intent(inout) :: this
         character(len=*), intent(inout), optional :: file_name
         integer, intent(out), optional :: rc

         if (present(file_name)) file_name = trim(this%file_name)
         _RETURN(_SUCCESS)
      end subroutine get

      subroutine reset_times_to_current_day(this,rc)
         class(HistoryTrajectory), intent(Inout) :: this
         integer, intent(out), optional :: rc

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

      end subroutine reset_times_to_current_day


      subroutine sort_arrays_by_time(this,rc)
         class(HistoryTrajectory), intent(inout) :: this
         integer, optional, intent(out) :: rc
         integer :: status

         integer :: i, len
         integer, allocatable :: IA(:)
         real(ESMF_KIND_R8), allocatable :: X(:), Y(:)
         integer(ESMF_KIND_I8), allocatable :: IX(:)

         len = size (this%times_R8)
         allocate (IA(len), IX(len), X(len))
         do i=1, len
            IX(i)=this%times_R8(i)
            IA(i)=i
         enddo
         call MAPL_Sort(IX,IA)

         X = this%lons
         do i=1, len
            this%lons(i) = X(IA(i))
         enddo
         X = this%lats
         do i=1, len
            this%lats(i) = X(IA(i))
         enddo
         X = this%times_R8
         do i=1, len
            this%times_R8(i) = X(IA(i))
         enddo

         _RETURN(_SUCCESS)
       end subroutine sort_arrays_by_time


      subroutine sort_three_arrays_by_time(U,V,T,rc)
        real(ESMF_KIND_R8) :: U(:), V(:), T(:)
        integer, optional, intent(out) :: rc
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
       end subroutine sort_three_arrays_by_time
       
       

       subroutine time_real_to_ESMF (this,rc)
         class(HistoryTrajectory), intent(inout) :: this
         integer, optional, intent(out) :: rc
         integer :: status

         integer :: i, len
         integer :: int_time
         type(ESMF_TimeInterval) :: interval
         type(ESMF_Time) :: time0
         type(ESMF_Time) :: time1
         character(len=:), allocatable :: tunit
         character(len=ESMF_MAXSTR) :: datetime_units

         datetime_units = this%datetime_units
         len = size (this%times_R8)

         do i=1, len
            int_time = this%times_R8(i)
            call convert_NetCDF_DateTime_to_ESMF(int_time, datetime_units, interval, time0, time1=time1, tunit=tunit, _RC)
            this%times(i) = time1
         enddo

         _RETURN(_SUCCESS)
       end subroutine time_real_to_ESMF


       subroutine create_grid(this, rc)
         class(HistoryTrajectory)  :: this
         integer, intent(out), optional :: rc
         integer :: status

         character(len=ESMF_MAXSTR) :: filename
         type(NetCDF4_FileFormatter) :: formatter
         type(FileMetadataUtils) :: metadata_utils
         type(FileMetadata) :: basic_metadata
         integer(ESMF_KIND_I8) :: num_times
         integer :: ncid, ncid0
         integer :: dimid(10),  dimlen(10)
         integer :: len

         character(len=ESMF_MAXSTR) :: grp_name
         character(len=ESMF_MAXSTR) :: dim_name(10)
         character(len=ESMF_MAXSTR) :: var_name_lon
         character(len=ESMF_MAXSTR) :: var_name_lat
         character(len=ESMF_MAXSTR) :: var_name_time
         
         type(ESMF_Config) :: config_grid
         character(len=ESMF_MAXSTR) :: time_string
         
         real(kind=REAL64), allocatable :: lons_full(:), lats_full(:)
         real(kind=REAL64), allocatable :: times_R8_full(:)
         !!real(kind=ESMF_KIND_R8), allocatable :: times_R8_full2(:)         


         integer(ESMF_KIND_I4), pointer :: ptAI(:), ptBI(:)
         real(ESMF_KIND_R8), pointer :: ptAT(:)
         type(ESMF_routehandle) :: RH
         type(ESMF_Time) :: timeset(2)
         type(ESMF_Time) :: current_time         
         type(ESMF_Field) :: src_fld, dst_fld
         type(ESMF_Field) :: src_fld2, dst_fld2         
         type(ESMF_Grid) :: grid         
         
         type(ESMF_VM) :: vm
         integer :: mypet, petcount

         integer :: i, j, k
         integer(kind=ESMF_KIND_I8) :: j0, j1
         integer(kind=ESMF_KIND_I8) :: jt1, jt2         
         integer(kind=ESMF_KIND_I8) :: nstart, nend
         real(kind=ESMF_KIND_R8) :: jx0, jx1
         integer :: nx
         integer :: sec

         this%datetime_units = "seconds since 1970-01-01 00:00:00"
         
         filename=trim(this%obsFile)
         call formatter%open(trim(filename),pFIO_READ,_RC)
         if (this%nc_index == '') then
            basic_metadata = formatter%read(_RC)
            call metadata_utils%create(basic_metadata,trim(filename))
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

            call formatter%open(trim(filename),pFIO_READ,_RC)
            basic_metadata = formatter%read(_RC)
            call metadata_utils%create(basic_metadata,trim(filename))
            num_times = basic_metadata%get_dimension(trim(this%nc_index),_RC)
            len = num_times

            allocate(lons_full(len),lats_full(len),_STAT)
            allocate(times_R8_full(len),_STAT)
            call formatter%get_var(this%var_name_lon,  lons_full, group_name=grp_name, count=[len], rc=status)
            call formatter%get_var(this%var_name_lat,  lats_full, group_name=grp_name, count=[len], rc=status)
            call formatter%get_var(this%var_name_time, times_R8_full, group_name=grp_name, count=[len], rc=status)

            ! get pet info
            call ESMF_VMGetGlobal(vm,_RC)
            call ESMF_VMGet(vm, localPet=mypet, petCount=petCount, _RC)
            write(6,*) 'mypet, petcount', mypet, petcount


            !__ epoch grid on root
            !
            if (mapl_am_I_root()) then

               call sort_three_arrays_by_time(lons_full, lats_full, times_R8_full, _RC)
               call ESMF_ClockGet(this%clock,currTime=current_time,_RC)
               timeset(1) = current_time 
               timeset(2) = current_time + this%frequency_epoch
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
               jt1 = jt1 + 1               ! (x1,x2]  design
               if (this%epoch_index(1) == 0) then
                  this%epoch_index(1)= jt1
               else
                  this%epoch_index(1)= this%epoch_index(2)
               end if
               this%epoch_index(2)= jt2

               nx= this%epoch_index(2) - this%epoch_index(1) + 1
               allocate(this%lons(nx),this%lats(nx),_STAT)
               allocate(this%times_R8(nx),_STAT)
               j=this%epoch_index(1)
               do i=1, nx
                  this%lons(i) = lons_full(j)
                  this%lats(i) = lats_full(j)
                  this%times_R8(i) = times_R8_full(j)
                  j=j+1
               enddo
            else
               allocate(this%lons(0),this%lats(0),_STAT)
               allocate(this%times_R8(0),_STAT)
               this%epoch_index(1:2)=0
               nx=0
            endif

            write(6,121) 'epoch_index(1:2), nx', this%epoch_index(1:2), nx
            this%locstream_factory = LocStreamFactory(this%lons,this%lats,_RC)
            this%LS_rt = this%locstream_factory%create_locstream(_RC)
            call ESMF_FieldBundleGet(this%bundle,grid=grid,_RC)
            this%LS_ds = this%locstream_factory%create_locstream(grid=grid,_RC)         

            src_fld = ESMF_FieldCreate (this%LS_rt, name='A_index', typekind=ESMF_TYPEKIND_I4, _RC)
            dst_fld = ESMF_FieldCreate (this%LS_ds, name='B_index', typekind=ESMF_TYPEKIND_I4, _RC)
               
            call ESMF_FieldGet( src_fld, localDE=0, farrayPtr=ptAI)
            call ESMF_FieldGet( dst_fld, localDE=0, farrayPtr=this%seqIndex)
            if (mypet == 0) then
               do i=1, nx
                  ptAI(i)=i
               enddo
            end if
            write(10+mypet,121)  'pet, ptAI(:)', mypet, ptAI(:)

            call ESMF_FieldRedistStore (src_fld, dst_fld, RH, _RC)
            call ESMF_FieldRedist      (src_fld, dst_fld, RH, _RC)
            write(10+mypet,121)  'af redist pet, size(this%seqIndex), this%seqIndex(:)', mypet, size(this%seqIndex), this%seqIndex(:)


            src_fld2 = ESMF_FieldCreate (this%LS_rt, name='A_time', typekind=ESMF_TYPEKIND_R8, _RC)
            dst_fld2 = ESMF_FieldCreate (this%LS_ds, name='B_time', typekind=ESMF_TYPEKIND_R8, _RC)
               
            call ESMF_FieldGet( src_fld2, localDE=0, farrayPtr=ptAT)
            call ESMF_FieldGet( dst_fld2, localDE=0, farrayPtr=this%obsTime)
            if (mypet == 0) then
               ptAT(:) = this%times_R8(:)
            end if
            write(10+mypet,143)  'pet, ptAT(:)', mypet, ptAT(:)

            call ESMF_FieldRedistStore (src_fld2, dst_fld2, RH, _RC)
            call ESMF_FieldRedist      (src_fld2, dst_fld2, RH, _RC)
            write(10+mypet,148)  'af redist pet, size(this%obsTime), this%obsTime(:)', mypet, size(this%obsTime), this%obsTime(:)
         end if
         
         _RETURN(_SUCCESS)
         
         include '/Users/yyu11/sftp/myformat.inc'         
       end subroutine create_grid


  subroutine regrid_accumulate_on_xsubset (this, rc)
    class(HistoryTrajectory), intent(inout) :: this
    integer, intent(out), optional :: rc
    integer :: status
    
    class(AbstractGridFactory), pointer :: factory
    integer                        :: x_subset(2)
    type(ESMF_Time)                :: timeset(2)
    type(ESMF_Time) :: current_time
    type(ESMF_TimeInterval) :: dur
    character(len=ESMF_MAXSTR) :: time_string

    integer, allocatable :: global_xy_mask(:,:)
    integer, allocatable :: local_xy_mask(:,:)    

    integer :: counts(5)
    integer :: dims(3)
    integer :: m1, m2

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


    ! __ s1.  get x_subset for each model time step interval on each pet    
    call ESMF_ClockGet(this%clock,currTime=current_time,_RC)
    call ESMF_ClockGet(this%clock,timeStep=dur, _RC )
    timeset(1) = current_time - dur
    timeset(2) = current_time    

    call this%get_x_subset(timeset, x_subset, _RC)



!    x_subset(1)= this%epoch_index(1)
!    x_subset(2)= this%epoch_index(2)

    _RETURN(ESMF_SUCCESS)

  end subroutine regrid_accumulate_on_xsubset


  subroutine get_x_subset(this, interval, x_subset, rc)
    class(HistoryTrajectory), intent(inout) :: this
    type(ESMF_Time), intent(in) :: interval(2)
    integer, intent(out) :: x_subset(2)
    integer, optional, intent(out) :: rc
    
    integer :: status
    type(ESMF_Time) :: T1, T2
    integer(ESMF_KIND_I8) :: i1, i2
    real(ESMF_KIND_R8) :: iT1, iT2
    integer(ESMF_KIND_I8) :: jt1, jt2
    integer :: jlo, jhi
    
    T1= interval(1)
    T2= interval(2)
    

    ! __ s1. find index on each PET

!              T1 <--> T2    
!      --------------------------->
!         |                |     
!   epoch_index(1)        (2)

      call time_esmf_2_nc_int (T1, this%datetime_units, i1, _RC)
      call time_esmf_2_nc_int (T2, this%datetime_units, i2, _RC)
      iT1=i1; iT2=i2   ! int to real*8
      jlo = 1 ; jhi= size(this%obstime)
      call bisect( this%obstime, iT1, jt1, n_LB=int(jlo, ESMF_KIND_I8), n_UB=int(jhi, ESMF_KIND_I8), rc=rc)
      call bisect( this%obstime, iT2, jt2, n_LB=int(jlo, ESMF_KIND_I8), n_UB=int(jhi, ESMF_KIND_I8), rc=rc)

      if(jt1==jt2) then
         !
         !- empty bracket
         !  
         x_subset(1)=0
         x_subset(2)=0
      else
         x_subset(1)=jt1+1
         x_subset(2)=jt2
      endif
      
      print*, 'x_subset(1:2)', x_subset(1:2)
      
    _RETURN(_SUCCESS)
  end subroutine get_x_subset



end module HistoryTrajectoryMod  
