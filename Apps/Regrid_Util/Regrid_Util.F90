#include "MAPL.h"

   module regrid_util_support_mod

   use ESMF
   use MAPL
   use gFTL2_StringVector

   implicit NONE

   public

   real, parameter :: uninit = MAPL_UNDEF

   type regrid_support
      type(ESMF_Grid)     :: new_grid
      type(StringVector) :: filenames,outputfiles
      integer :: Nx,Ny
      integer :: itime(2)
      logical :: onlyVars, allTimes
      character(len=512) :: vars
      character(len=:), allocatable :: tripolar_file_in,tripolar_file_out
      integer :: regridMethod
      real :: cs_stretch_param(3)
      real :: lon_range(2), lat_range(2)
      integer :: deflate, shave
      integer :: quantize_algorithm
      integer :: quantize_level
      integer :: zstandard_level
      logical :: use_weights
   contains
      procedure :: create_grid
      procedure :: process_command_line
      procedure :: has_level
   end type regrid_support

   contains

   subroutine UnpackGridName(gridName,im,jm,date,pole)
     character(len=*), intent(in) :: gridName
     integer,          intent(out) :: im
     integer,          intent(out) :: jm
     character(len=2), intent(out) :: date
     character(len=2), intent(out) :: pole

     integer :: nn
     character(len=5) :: imsz,jmsz

     nn   = len_trim(Gridname)
     imsz = Gridname(3:index(Gridname,'x')-1)
     jmsz = Gridname(index(Gridname,'x')+1:nn-3)
     pole = Gridname(1:2)
     date = Gridname(nn-1:nn)
     read(IMSZ,*) IM
     read(JMSZ,*) JM

    end subroutine

    function split_string(input_string,separator) result(output_string_vec)
       character(len=*), intent(in) :: input_string
       character(len=1), intent(in) :: separator
       type(StringVector)  :: output_string_vec
       character(len=:), allocatable :: tstring
       integer :: i

       tstring = input_string
       i = 1
       do while(i /=0)
          i = index(tstring,separator)
          if (i > 0) then
             call output_string_vec%push_back(tstring(1:i-1))
             tstring = tstring(i+1:)
          else
             call output_string_vec%push_back(trim(tstring))
          end if
       enddo
    end function split_string

    subroutine process_command_line(this,rc)
    class(regrid_support) :: this
    integer, optional, intent(out) :: rc

    character(len=ESMF_MAXSTR) :: RegridMth
    integer :: nargs, i, status
    character(len=ESMF_MAXPATHLEN) :: str,astr
    character(len=ESMF_MAXPATHLEN) :: tp_filein,tp_fileout
    character(len=ESMF_MAXPATHLEN*100) :: cfileNames,coutputFiles
    character(len=ESMF_MAXSTR) :: gridname

    this%nx=1
    this%ny=6
    this%onlyvars=.false.
    this%alltimes=.true.
    regridMth='bilinear'
    this%cs_stretch_param=uninit
    this%lon_range=uninit
    this%lat_range=uninit
    this%shave=64
    this%deflate=0
    this%quantize_algorithm=0
    this%quantize_level=0
    this%zstandard_level=0
    this%use_weights = .false.
    nargs = command_argument_count()
    do i=1,nargs
      call get_command_argument(i,str)
      select case(trim(str))
      case ('-o')
         call get_command_argument(i+1,coutputfiles)
      case('-i')
         call get_command_argument(i+1,cfilenames)
      case('-ogrid')
         call get_command_argument(i+1,Gridname)
      case('-nx')
         call get_command_argument(i+1,astr)
         read(astr,*)this%nx
      case('-ny')
         call get_command_argument(i+1,astr)
         read(astr,*)this%ny
      case('-vars')
         call get_command_argument(i+1,this%vars)
         this%onlyVars = .true.
      case('-t')
         call get_command_argument(i+1,astr)
         read(astr,*)this%itime(1)
         call get_command_argument(i+2,astr)
         read(astr,*)this%itime(2)
         this%alltimes=.false.
      case('-stretch_factor')
         call get_command_argument(i+1,astr)
         read(astr,*)this%cs_stretch_param(1)
         call get_command_argument(i+2,astr)
         read(astr,*)this%cs_stretch_param(2) ! target_lon in degree
         call get_command_argument(i+3,astr)
         read(astr,*)this%cs_stretch_param(3) ! target_lat in degree
      case('-lon_range')
         call get_command_argument(i+1,astr)
         read(astr,*)this%lon_range(1)
         call get_command_argument(i+2,astr)
         read(astr,*)this%lon_range(2)
      case('-lat_range')
         call get_command_argument(i+1,astr)
         read(astr,*)this%lat_range(1)
         call get_command_argument(i+2,astr)
         read(astr,*)this%lat_range(2)
      case('-method')
         call get_command_argument(i+1,RegridMth)
      case('-tp_in')
         call get_command_argument(i+1,tp_filein)
         this%tripolar_file_in = tp_filein
      case('-tp_out')
         call get_command_argument(i+1,tp_fileout)
         this%tripolar_file_out = tp_fileout
      case('-shave')
         call get_command_argument(i+1,astr)
         read(astr,*)this%shave
      case('-deflate')
         call get_command_argument(i+1,astr)
         read(astr,*)this%deflate
      case('-quantize_algorithm')
         call get_command_argument(i+1,astr)
         read(astr,*)this%quantize_algorithm
      case('-quantize_level')
         call get_command_argument(i+1,astr)
         read(astr,*)this%quantize_level
      case('-zstandard_level')
         call get_command_argument(i+1,astr)
         read(astr,*)this%zstandard_level
      case('-file_weights')
         this%use_weights = .true.
      case('--help')
         if (mapl_am_I_root()) then

         end if
         call MPI_Finalize(status)
         return
      end select
    enddo

    if (.not.allocated(this%tripolar_file_out)) then
       this%tripolar_file_out = "empty"
    end if
    this%regridMethod = regrid_method_string_to_int(regridMth)
    _ASSERT(this%regridMethod/=UNSPECIFIED_REGRID_METHOD,"improper regrid method chosen")

    this%filenames = split_string(cfilenames,',')
    this%outputfiles = split_string(coutputfiles,',')
    _ASSERT(this%filenames%size() > 0, 'no input files')
    _ASSERT(this%outputfiles%size() >0, 'no ouput files specified')
    _ASSERT(this%filenames%size() == this%outputfiles%size(), 'different number of input and output files')
    if (.not.this%alltimes) then
       _ASSERT(this%filenames%size() == 1,'if selecting time from file, can only regrid a single file')
    end if

    call this%create_grid(gridname,_RC)
    _RETURN(_SUCCESS)

    end subroutine process_command_line

    subroutine create_grid(this,grid_name,rc)
    class(regrid_support) :: this
    character(len=*), intent(in) :: grid_name
    integer, optional, intent(out) :: rc

    type (FileMetaDataUtils) :: metadata
    type (FileMetaData) :: basic_metadata
    character(len=:),allocatable :: lev_name
    integer :: im_world,jm_world,lm_world
    type(NetCDF4_FileFormatter) :: formatter
    character(len=:), allocatable :: filename
    character(len=2) :: dateline,pole
    integer :: status
    type(ESMF_CONFIG) :: cfoutput

    filename = this%filenames%at(1)

    call formatter%open(trim(filename),pFIO_Read,_RC)
    basic_metadata=formatter%read(_RC)
    call metadata%create(basic_metadata,trim(filename))

    call formatter%close(_RC)

    lm_world=0
    lev_name=metadata%get_level_name()
    if (trim(lev_name)/='') then
       lm_world = metadata%get_dimension(lev_name,_RC)
    end if
    call UnpackGridName(Grid_name,im_world,jm_world,dateline,pole)

    cfoutput = create_cf(grid_name,im_world,jm_world,this%nx,this%ny,lm_world,this%cs_stretch_param,this%lon_range,this%lat_range,this%tripolar_file_out,_RC)
    this%new_grid=grid_manager%make_grid(cfoutput,prefix=trim(grid_name)//".",_RC)

    _RETURN(_SUCCESS)
    end subroutine create_grid

    function has_level(this,rc) result(file_has_level)
       logical :: file_has_level
       class(regrid_support), intent(in) :: this
       integer, intent(out), optional :: rc
       integer :: global_dims(3),status
       call MAPL_GridGet(this%new_grid,globalCellCountPerDim=global_dims,_RC)
       file_has_level = (global_dims(3) /= 0)
       _RETURN(_SUCCESS)
    end function

    function create_cf(grid_name,im_world,jm_world,nx,ny,lm,cs_stretch_param,lon_range,lat_range,tripolar_file,rc) result(cf)
       use MAPL_ConfigMod
       type(ESMF_Config)              :: cf
       character(len=*), intent(in) :: grid_name
       integer, intent(in)          :: im_world,jm_world
       integer, intent(in)          :: nx,ny
       integer, intent(in)          :: lm
       real, intent(in)             :: cs_stretch_param(3)
       real, intent(in)             :: lon_range(2)
       real, intent(in)             :: lat_range(2)
       character(len=*), intent(in) :: tripolar_file
       integer, optional, intent(out) :: rc

       integer :: status
       character(len=2) :: pole,dateline
       integer :: nn

       nn = len_trim(grid_name)
       dateline=grid_name(nn-1:nn)
       pole=grid_name(1:2)

       cf = MAPL_ConfigCreate(_RC)
       call MAPL_ConfigSetAttribute(cf,value=NX, label=trim(grid_name)//".NX:",_RC)
       call MAPL_ConfigSetAttribute(cf,value=lm, label=trim(grid_name)//".LM:",_RC)
       if (dateline=='CF') then
          call MAPL_ConfigSetAttribute(cf,value="Cubed-Sphere", label=trim(grid_name)//".GRID_TYPE:",_RC)
          call MAPL_ConfigSetAttribute(cf,value=6, label=trim(grid_name)//".NF:",_RC)
          call MAPL_ConfigSetAttribute(cf,value=im_world,label=trim(grid_name)//".IM_WORLD:",_RC)
          call MAPL_ConfigSetAttribute(cf,value=ny/6, label=trim(grid_name)//".NY:",_RC)
          if (any(cs_stretch_param/=uninit)) then
             call MAPL_ConfigSetAttribute(cf,value=cs_stretch_param(1),label=trim(grid_name)//".STRETCH_FACTOR:",_RC)
             call MAPL_ConfigSetAttribute(cf,value=cs_stretch_param(2),label=trim(grid_name)//".TARGET_LON:",_RC)
             call MAPL_ConfigSetAttribute(cf,value=cs_stretch_param(3),label=trim(grid_name)//".TARGET_LAT:",_RC)
          end if
       else if (dateline=='TM') then
          call MAPL_ConfigSetAttribute(cf,value="Tripolar", label=trim(grid_name)//".GRID_TYPE:",_RC)
          call MAPL_ConfigSetAttribute(cf,value=im_world,label=trim(grid_name)//".IM_WORLD:",_RC)
          call MAPL_ConfigSetAttribute(cf,value=jm_world,label=trim(grid_name)//".JM_WORLD:",_RC)
          call MAPL_ConfigSetAttribute(cf,value=ny, label=trim(grid_name)//".NY:",_RC)
          _ASSERT(tripolar_file /= "empty","asked for tripolar output but did not specify the coordinate file")
          call MAPL_ConfigSetAttribute(cf,value=tripolar_file,label=trim(grid_name)//".GRIDSPEC:",_RC)
       else
          call MAPL_ConfigSetAttribute(cf,value="LatLon", label=trim(grid_name)//".GRID_TYPE:",_RC)
          call MAPL_ConfigSetAttribute(cf,value=im_world,label=trim(grid_name)//".IM_WORLD:",_RC)
          call MAPL_ConfigSetAttribute(cf,value=jm_world,label=trim(grid_name)//".JM_WORLD:",_RC)
          call MAPL_ConfigSetAttribute(cf,value=ny, label=trim(grid_name)//".NY:",_RC)
          call MAPL_ConfigSetAttribute(cf,value=pole, label=trim(grid_name)//".POLE:",_RC)
          call MAPL_ConfigSetAttribute(cf,value=dateline, label=trim(grid_name)//".DATELINE:",_RC)
          if (pole=='XY' .and. dateline=='XY') then
             _ASSERT(all(lon_range/=uninit),'if regional must specify lon_range')
             _ASSERT(all(lat_range/=uninit),'if regional must specify lat_range')
             call MAPL_ConfigSetAttribute(cf,value=lon_range,label=trim(grid_name)//".LON_RANGE:",_RC)
             call MAPL_ConfigSetAttribute(cf,value=lat_range,label=trim(grid_name)//".LAT_RANGE:",_RC)
          end if
       end if

     end function create_cf

   end module regrid_util_support_mod

   Program Regrid_Util

   use ESMF
   use ESMFL_Mod
   use MAPL_ExceptionHandling
   use MAPL_Profiler
   use MAPL_BaseMod
   use MAPL_MemUtilsMod
   use MAPL_CFIOMod
   use MAPL_CommsMod
   use MAPL_ShmemMod
   use ESMF_CFIOMod
   use ESMF_CFIOUtilMod
   use ESMF_CFIOFileMod
   use MAPL_NewRegridderManager
   use MAPL_AbstractRegridderMod
   use mapl_RegridMethods
   use MAPL_GridManagerMod
   use MAPL_LatLonGridFactoryMod, only: LatLonGridFactory
   use MAPL_CubedSphereGridFactoryMod, only: CubedSphereGridFactory
   use MAPL_TripolarGridFactoryMod, only: TripolarGridFactory
   use MAPL_Constants, only: MAPL_PI_R8
   use MAPL_ExceptionHandling
   use MAPL_ApplicationSupport
   use pFIO
   use MAPL_ESMFFieldBundleWrite
   use MAPL_ESMFFieldBundleRead
   use MAPL_ServerManager
   use MAPL_FileMetadataUtilsMod
   use gFTL_StringVector
   use regrid_util_support_mod
   use mpi

   implicit NONE

   type(DistributedProfiler), target :: t_prof
   type (ProfileReporter) :: reporter

   call main()

CONTAINS

    subroutine main()

   type(regrid_support), target :: support

   type(ESMF_VM)       :: vm             ! ESMF Virtual Machine

   character(len=ESMF_MAXPATHLEN) ::  Filename,OutputFile

   integer :: myPET   ! The local PET number
   integer :: nPET    ! The total number of PETs you are running on

   integer :: status, rc

   type(ESMF_FieldBundle) :: bundle
   type(ESMF_Time) :: time
   type(ESMF_Time), allocatable :: tSeries(:)
   type(ESMF_TimeInterval) :: timeInterval
   type(ESMF_Clock) :: clock


   logical :: fileCreated,file_exists

   integer :: tsteps,i,j,tint
   type(VerticalData) :: vertical_data

   type(FieldBundleWriter) :: newWriter
   logical :: writer_created, has_vertical_level
   type(ServerManager) :: io_server


   call ESMF_Initialize (LogKindFlag=ESMF_LOGKIND_NONE, vm=vm, _RC)
   call ESMF_VMGet(vm, localPET=myPET, petCount=nPet)
   call MAPL_Initialize(_RC)
   call MAPL_GetNodeInfo (comm=MPI_COMM_WORLD, _RC)
   call ESMF_CalendarSetDefault ( ESMF_CALKIND_GREGORIAN, _RC )

   call support%process_command_line(_RC)

   t_prof=DistributedProfiler('Regrid_Util',MpiTimerGauge(),MPI_COMM_WORLD)
   call t_prof%start(_RC)

   call io_server%initialize(mpi_comm_world)

   filename = support%filenames%at(1)
   if (allocated(tSeries)) deallocate(tSeries)
   call get_file_times(filename,support%itime,support%allTimes,tseries,timeInterval,tint,tsteps,_RC)
   has_vertical_level = support%has_level(_RC)
   if (has_vertical_level) then
      call get_file_levels(filename,vertical_data,_RC)
   end if

   Clock = ESMF_ClockCreate ( name="Eric", timeStep=TimeInterval, &
                               startTime=tSeries(1), _RC )

   bundle=ESMF_FieldBundleCreate(name="cfio_bundle",_RC)
   call ESMF_FieldBundleSet(bundle,grid=support%new_grid,_RC)

   writer_created=.false.
   do j=1,support%filenames%size()

      filename = support%filenames%at(j)
      if (j>1) then
         if (allocated(tSeries)) deallocate(tSeries)
         call get_file_times(filename,support%itime,support%allTimes,tseries,timeInterval,tint,tsteps,_RC)
      end if
      outputfile = support%outputfiles%at(j)

      inquire(file=trim(outputfile),exist=file_exists)
      _ASSERT(.not.file_exists,"output file already exists: exiting!")

      fileCreated=.false.
      do i=1,tsteps

         call t_prof%start("Read")
         if (mapl_am_i_root()) write(*,*)'processing timestep from '//trim(filename)
         time = tSeries(i)
         if (support%onlyvars) then
            call MAPL_Read_bundle(bundle,trim(filename),time=time,regrid_method=support%regridMethod,only_vars=support%vars,file_weights=support%use_weights, _RC)
         else
            call MAPL_Read_bundle(bundle,trim(filename),time=time,regrid_method=support%regridMethod,file_weights=support%use_weights, _RC)
         end if
         call t_prof%stop("Read")

         call MPI_BARRIER(MPI_COMM_WORLD,STATUS)
         _VERIFY(status)

         call t_prof%start("write")

         if (mapl_am_I_root()) write(*,*) "moving on to writing "//trim(outputfile)

         call ESMF_ClockSet(clock,currtime=time,_RC)
         if (.not. writer_created) then
            call newWriter%create_from_bundle(bundle,clock,n_steps=tsteps,time_interval=tint,nbits_to_keep=support%shave,deflate=support%deflate,vertical_data=vertical_data,quantize_algorithm=support%quantize_algorithm,quantize_level=support%quantize_level,zstandard_level=support%zstandard_level,_RC)
            writer_created=.true.
         end if

         if (.not.fileCreated) then
            call newWriter%start_new_file(outputFile,_RC)
            fileCreated=.true.
         end if
         call newWriter%write_to_file(_RC)
         call t_prof%stop("write")

      end do
   enddo
!   All done
!   --------
   call ESMF_VMBarrier(VM,_RC)

   call io_server%finalize()
   call t_prof%stop()
   call t_prof%reduce()
   call t_prof%finalize()
   call generate_report()
   call MAPL_Finalize(_RC)
   call ESMF_Finalize ( _RC )

   end subroutine main

   subroutine get_file_levels(filename,vertical_data,rc)
      character(len=*), intent(in) :: filename
      type(VerticalData), intent(inout) :: vertical_data
      integer, intent(out), optional :: rc

      integer :: status
      type(NetCDF4_fileFormatter) :: formatter
      type(FileMetadata) :: basic_metadata
      type(FileMetadataUtils) :: metadata
      character(len=:), allocatable :: lev_name
      character(len=ESMF_MAXSTR) :: long_name
      character(len=ESMF_MAXSTR) :: standard_name
      character(len=ESMF_MAXSTR) :: vcoord
      character(len=ESMF_MAXSTR) :: lev_units
      real, allocatable, target :: levs(:)
      real, pointer :: plevs(:)

      call formatter%open(trim(filename),pFIO_Read,_RC)
      basic_metadata=formatter%read(_RC)
      call metadata%create(basic_metadata,trim(filename))
      lev_name = metadata%get_level_name(_RC)
      call metadata%get_coordinate_info(lev_name,coords=levs,coordUnits=lev_units,long_name=long_name,&
           standard_name=standard_name,coordinate_attr=vcoord,_RC)
      plevs => levs
      vertical_data = VerticalData(levels=plevs,vunit=lev_units,vcoord=vcoord,standard_name=standard_name,long_name=long_name, &
                      force_no_regrid=.true.,_RC)
      nullify(plevs)

      _RETURN(_SUCCESS)

   end subroutine get_file_levels

   subroutine get_file_times(filename,itime,alltimes,tseries,timeInterval,tint,tsteps,rc)
      character(len=*), intent(in) :: filename
      integer, intent(in) :: itime(2)
      logical, intent(in) :: alltimes
      type(ESMF_Time), allocatable, intent(inout) :: tseries(:)
      type(ESMF_TimeInterval), intent(inout) :: timeInterval
      integer, intent(out) :: tint
      integer, intent(out) :: tsteps
      integer, intent(out), optional :: rc

      integer :: status
      integer :: second,minute,hour,day,month,year
      type(NetCDF4_fileFormatter) :: formatter
      type(FileMetadata) :: basic_metadata
      type(FileMetadataUtils) :: metadata

      call formatter%open(trim(filename),pFIO_Read,_RC)
      basic_metadata=formatter%read(_RC)
      call metadata%create(basic_metadata,trim(filename))

      call formatter%close(_RC)

      tsteps = metadata%get_dimension('time',_RC)
      call metadata%get_time_info(timeVector=tSeries,_RC)

      if (.not.allTimes) then
         tSteps=1
         call UnpackDateTIme(itime,year,month,day,hour,minute,second)
         deallocate(tSeries)
         allocate(tSeries(1))
         call ESMF_TimeSet(tSeries(1), yy=year, mm=month, dd=day,  h=hour,  m=minute, s=second,_RC)
      end if
      if (tSteps == 1) then
         call ESMF_TimeIntervalSet( TimeInterval, h=6, m=0, s=0, _RC )
      else
         TimeInterval=tSeries(2)-tSeries(1)
      end if
      call ESMF_TimeIntervalGet(TimeInterval,h=hour,m=minute,s=second,_RC)
      tint=hour*10000+minute*100+second

      _RETURN(_SUCCESS)

   end subroutine get_file_times

   subroutine UnpackDateTime(DATETIME, YY, MM, DD, H, M, S)
     integer, intent(IN   ) :: DATETIME(:)
     integer, intent(  OUT) :: YY, MM, DD, H, M, S

     YY =     datetime(1)/10000
     MM = mod(datetime(1),10000)/100
     DD = mod(datetime(1),100)
     H  =     datetime(2)/10000
     M  = mod(datetime(2),10000)/100
     S  = mod(datetime(2),100)
     return
   end subroutine UnpackDateTime

    subroutine generate_report()

         character(:), allocatable :: report_lines(:)
         integer :: i
         character(1) :: empty(0)

         reporter = ProfileReporter(empty)
         call reporter%add_column(NameColumn(20))
         call reporter%add_column(FormattedTextColumn('Inclusive','(f9.6)', 9, InclusiveColumn('MEAN')))
         call reporter%add_column(FormattedTextColumn('% Incl','(f6.2)', 6, PercentageColumn(InclusiveColumn('MEAN'),'MAX')))
         call reporter%add_column(FormattedTextColumn('Exclusive','(f9.6)', 9, ExclusiveColumn('MEAN')))
         call reporter%add_column(FormattedTextColumn('% Excl','(f6.2)', 6, PercentageColumn(ExclusiveColumn('MEAN'))))
         call reporter%add_column(FormattedTextColumn(' Max Excl)','(f9.6)', 9, ExclusiveColumn('MAX')))
         call reporter%add_column(FormattedTextColumn(' Min Excl)','(f9.6)', 9, ExclusiveColumn('MIN')))
         call reporter%add_column(FormattedTextColumn('Max PE)','(1x,i5.5,1x)', 7, ExclusiveColumn('MAX_PE')))
         call reporter%add_column(FormattedTextColumn('Min PE)','(1x,i5.5,1x)', 7, ExclusiveColumn('MIN_PE')))
        report_lines = reporter%generate_report(t_prof)
         if (mapl_am_I_root()) then
            write(*,'(a)')'Final profile'
            write(*,'(a)')'============='
            do i = 1, size(report_lines)
               write(*,'(a)') report_lines(i)
            end do
            write(*,'(a)') ''
         end if
    end subroutine generate_report

    end program Regrid_Util
