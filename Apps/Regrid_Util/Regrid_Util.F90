#include "MAPL.h"

   module regrid_util_support_mod

   use ESMF
   use MAPL
   
   use mapl3g_RegridderMethods
   use gFTL2_StringVector

   implicit NONE
   private

   public :: regrid_support
   public :: uninit
   public :: UnpackGridName
   public :: split_string

   real, parameter :: uninit = MAPL_UNDEF

   type regrid_support
      type(ESMF_Grid)     :: new_grid
      class(VerticalGrid), pointer :: new_vgrid ! same as old for now...
      type(StringVector) :: filenames,outputfiles
      type(CompressionSettings) :: compression_settings
      integer :: Nx,Ny
      integer :: itime(2)
      logical :: onlyVars, allTimes
      character(len=512) :: vars
      character(len=:), allocatable :: tripolar_file_in,tripolar_file_out
      integer :: regridMethod
      real :: cs_stretch_param(3)
      real :: lon_range(2), lat_range(2)
      integer :: deflate
      integer :: shave
      character(len=:), allocatable :: quantize_algorithm
      integer :: quantize_level
      integer :: zstandard_level
      logical :: use_weights
   contains
      procedure :: create_grid
      procedure :: create_vgrid
      procedure :: process_command_line
      procedure :: sync_compression_to_bundle
      procedure :: fill_in_compression_hconfig
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
    type(ESMF_HConfig) :: hconfig_compression

    this%nx=1
    this%ny=1
    this%onlyvars=.false.
    this%alltimes=.true.
    regridMth='bilinear'
    this%cs_stretch_param=uninit
    this%lon_range=uninit
    this%lat_range=uninit
    this%shave=-1
    this%deflate=0
    this%quantize_algorithm='NONE'
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
         this%quantize_algorithm=astr
      case('-quantize_level')
         call get_command_argument(i+1,astr)
         read(astr,*)this%quantize_level
      case('-zstandard_level')
         call get_command_argument(i+1,astr)
         read(astr,*)this%zstandard_level
      case('-file_weights')
         this%use_weights = .true.
      case('--help')
         if (local_am_i_root()) then

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
    call this%create_vgrid(_RC)
    hconfig_compression = this%fill_in_compression_hconfig(_RC)
    this%compression_settings = CompressionSettings(hconfig_compression, _RC)
    _RETURN(_SUCCESS)

    end subroutine process_command_line

    subroutine create_vgrid(this,rc)
    class(regrid_support) :: this
    integer, optional, intent(out) :: rc

    type(NetCDF4_FileFormatter)     :: file_formatter
    type(FileMetaData)              :: metadata
    class(VerticalGridManager), pointer :: vgrid_manager
    character(len=:), pointer :: file_name
    integer :: status

    file_name => this%filenames%at(1)
    call file_formatter%open(trim(file_name), PFIO_READ, _RC)
    metadata = file_formatter%read(_RC)
    call file_formatter%close(_RC)
    vgrid_manager => get_vertical_grid_manager(_RC)
    this%new_vgrid => vgrid_manager%create_grid_from_file_metadata(metadata, _RC)

    _RETURN(_SUCCESS)

    end subroutine create_vgrid

    subroutine create_grid(this,grid_name,rc)
    class(regrid_support) :: this
    character(len=*), intent(in) :: grid_name
    integer, optional, intent(out) :: rc

    integer :: im_world,jm_world
    character(len=2) :: dateline,pole
    integer :: status
    type(ESMF_HConfig) :: geom_hconfig
    type(MAPLGeom), pointer :: mapl_geom
    type(ESMF_Geom) :: geom
    type(GeomManager), pointer :: geom_mgr

    call UnpackGridName(Grid_name,im_world,jm_world,dateline,pole)

    geom_hconfig = create_output_geom_hconfig(grid_name,im_world,jm_world,this%nx,this%ny,this%cs_stretch_param,this%lon_range,this%lat_range,this%tripolar_file_out,_RC)
    geom_mgr => get_geom_manager()
    mapl_geom => geom_mgr%get_mapl_geom(geom_hconfig, _RC)
    geom = mapl_geom%get_geom()
    call ESMF_GeomGet(geom, grid=this%new_grid, _RC)

    _RETURN(_SUCCESS)
    end subroutine create_grid

    function create_output_geom_hconfig(grid_name,im_world,jm_world,nx,ny,cs_stretch_param,lon_range,lat_range,tripolar_file,rc) result(output_geom_hconfig)
       type(ESMF_HConfig)              :: output_geom_hconfig
       character(len=*), intent(in) :: grid_name
       integer, intent(in)          :: im_world,jm_world
       integer, intent(in)          :: nx,ny
       real, intent(in)             :: cs_stretch_param(3)
       real, intent(in)             :: lon_range(2)
       real, intent(in)             :: lat_range(2)
       character(len=*), intent(in) :: tripolar_file
       integer, optional, intent(out) :: rc

       integer :: status
       character(len=2) :: pole,dateline
       integer :: nn
       character(len=:), allocatable :: grid_class

       grid_class = 'latlon'

       nn = len_trim(grid_name)
       dateline=grid_name(nn-1:nn)
       pole=grid_name(1:2)

       if (dateline=='CF') grid_class = 'CubedSphere'
       if (dateline=='TM') then
          _FAIL('tripolar grid not supported')
       end if

       output_geom_hconfig = ESMF_HConfigCreate(content='{}', _RC)
       if (grid_class=='CubedSphere') then
          call ESMF_HConfigAdd(output_geom_hconfig, 'CubedSphere', addKeyString='class', _RC)
          call ESMF_HConfigAdd(output_geom_hconfig, im_world, addKeyString='im_world', _RC)
          call ESMF_HConfigAdd(output_geom_hconfig, nx, addKeyString='nx_face', _RC)
          call ESMF_HConfigAdd(output_geom_hconfig, ny/6, addKeyString='ny_face', _RC)

          if (any(cs_stretch_param/=uninit)) then
             call ESMF_HConfigAdd(output_geom_hconfig,cs_stretch_param(1),addKeyString='stretch_factor',_RC)
             call ESMF_HConfigAdd(output_geom_hconfig,cs_stretch_param(2),addKeyString='target_lon',_RC)
             call ESMF_HConfigAdd(output_geom_hconfig,cs_stretch_param(3),addKeyString='target_lat',_RC)
          end if
       else if (grid_class=='latlon') then
          call ESMF_HConfigAdd(output_geom_hconfig, 'latlon', addKeyString='class', _RC)
          call ESMF_HConfigAdd(output_geom_hconfig, im_world, addKeyString='im_world', _RC)
          call ESMF_HConfigAdd(output_geom_hconfig, jm_world, addKeyString='jm_world', _RC)
          call ESMF_HConfigAdd(output_geom_hconfig, pole, addKeyString='pole', _RC)
          call ESMF_HConfigAdd(output_geom_hconfig, dateline, addKeyString='dateline', _RC)
          call ESMF_HConfigAdd(output_geom_hconfig, nx, addKeyString='nx', _RC)
          call ESMF_HConfigAdd(output_geom_hconfig, ny, addKeyString='ny', _RC)
          if (pole=='XY' .and. dateline=='XY') then
             _ASSERT(all(lon_range/=uninit),'if regional must specify lon_range')
             _ASSERT(all(lat_range/=uninit),'if regional must specify lat_range')
             call ESMF_HConfigAdd(output_geom_hconfig, lon_range, addKeyString='lon_range',_RC)
             call ESMF_HConfigAdd(output_geom_hconfig, lat_range, addKeyString='lat_range',_RC)
          end if
       end if
       _RETURN(_SUCCESS)
     end function create_output_geom_hconfig

     subroutine sync_compression_to_bundle(this, bundle, rc)
        class(regrid_support), intent(inout) :: this
        type(ESMF_FieldBundle), intent(inout) :: bundle
        integer, optional, intent(out) :: rc

        integer :: status, i
        type(ESMF_Info) :: infoh
        type(ESMF_Field), allocatable :: field_list(:)

        call MAPL_FieldBundleGet(bundle, fieldList=field_list, _RC)
        do i=1,size(field_list)
           call ESMF_InfoGetFromHost(field_list(i), infoh, _RC)
           call this%compression_settings%sync_to_info(infoh, _RC)
        enddo

        _RETURN(_SUCCESS)
      end subroutine sync_compression_to_bundle

      function fill_in_compression_hconfig(this, rc) result(hconfig)
         type(ESMF_HConfig) :: hconfig
         class(regrid_support), intent(inout) :: this
         integer, optional, intent(out) :: rc

         integer :: status
         hconfig = ESMF_HConfigCreate(content='{}', _RC)
         if (this%deflate > 0) then
            call ESMF_HConfigAdd(hconfig, this%deflate, AddKeyString='deflate', _RC)
         end if
         if (this%zstandard_level > 0) then
            call ESMF_HConfigAdd(hconfig, this%zstandard_level, AddKeyString='zstandard', _RC)
         end if
         if (this%quantize_algorithm /= 'NONE') then
            call ESMF_HConfigAdd(hconfig, this%quantize_level, AddKeyString='quantize_level', _RC)
            call ESMF_HConfigAdd(hconfig, this%quantize_algorithm, AddKeyString='quantize_algorithm', _RC)
         end if
         if (this%shave > 0) then
            call ESMF_HConfigAdd(hconfig, this%shave, AddKeyString='nbits', _RC)
         end if
         _RETURN(_SUCCESS)

      end function fill_in_compression_hconfig

      function local_am_i_root(rc) result(am_i_root)
         logical :: am_i_root
         integer, optional, intent(out) :: rc

         type(ESMF_VM) :: vm
         integer :: localPet, status

         call ESMF_VMGetCurrent(vm, _RC)
         call ESMF_VMGet(vm, localPet=localPet, _RC)
         am_i_root = localPet == 0
         _RETURN(_SUCCESS)
      end function local_am_i_root

   end module regrid_util_support_mod

   Program Regrid_Util

   use ESMF
   use MAPL
   use MAPLBase_Mod, only: FileMetadataUtils
   use mapl_Profiler
   use regrid_util_support_mod
   use mpi

   implicit NONE

   type(DistributedProfiler), target :: t_prof
   type (ProfileReporter) :: reporter

   call main()

CONTAINS

    subroutine main()

   type(regrid_support), target :: support

   character(len=ESMF_MAXPATHLEN) ::  Filename,OutputFile

   integer :: status, rc

   type(ESMF_FieldBundle) :: bundle
   type(ESMF_Time) :: time
   type(ESMF_Time), allocatable :: tSeries(:)
   type(ESMF_TimeInterval) :: timeInterval
   type(ESMF_Clock) :: clock


   logical :: fileCreated,file_exists

   integer :: tsteps,i,j,tint

   type(FieldBundleWriter) :: newWriter
   logical :: writer_created


   call MAPL_Initialize()
   call ESMF_CalendarSetDefault ( ESMF_CALKIND_GREGORIAN, _RC )

   call support%process_command_line(_RC)

   t_prof=DistributedProfiler('Regrid_Util',MpiTimerGauge(),MPI_COMM_WORLD)
   call t_prof%start(_RC)

   filename = support%filenames%at(1)
   if (allocated(tSeries)) deallocate(tSeries)
   call get_file_times(filename,support%itime,support%allTimes,tseries,timeInterval,tint,tsteps,_RC)

   Clock = ESMF_ClockCreate ( name="Eric", timeStep=TimeInterval, &
                               startTime=tSeries(1), _RC )

   bundle=ESMF_FieldBundleCreate(name="cfio_bundle",_RC)
   call MAPL_FieldBundleSet(bundle, fieldBundleType=FIELDBUNDLETYPE_BASIC, _RC)
   call ESMF_FieldBundleSet(bundle,grid=support%new_grid,_RC)
   call MAPL_FieldBundleSet(bundle, vgrid=support%new_vgrid, _RC)

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
         if (local_am_i_root()) write(*,*)'processing timestep from '//trim(filename)
         time = tSeries(i)
         if (support%onlyvars) then
            call MAPL_Read_bundle(bundle,trim(filename),time,only_vars=support%vars, regrid_method=support%regridMethod, _RC)
         else
            call MAPL_Read_bundle(bundle,trim(filename),time,regrid_method=support%regridMethod, _RC)
         end if
         call t_prof%stop("Read")

         call MPI_BARRIER(MPI_COMM_WORLD,STATUS)
         _VERIFY(status)

         call t_prof%start("write")

         if (local_am_i_root()) write(*,*) "moving on to writing "//trim(outputfile)

         call ESMF_ClockSet(clock,currtime=time,_RC)
         if (.not. writer_created) then
            call support%sync_compression_to_bundle(bundle, _RC)
            call newWriter%create_from_bundle(bundle,clock,_RC)
            writer_created=.true.
         end if

         if (.not.fileCreated) then
            call newWriter%start_new_file(outputFile, time, _RC)
            fileCreated=.true.
         end if
         call newWriter%write_to_file(bundle, time, _RC)
         call t_prof%stop("write")

      end do
   enddo
!   All done
!   --------

   call t_prof%stop()
   call t_prof%reduce()
   call t_prof%finalize()
   call generate_report()
   call MAPL_Finalize()

   end subroutine main

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

         type(StringVector) :: report_lines
         type(StringVectorIterator) :: iter
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
         if (local_am_i_root()) then
            write(*,'(a)')'Final profile'
            write(*,'(a)')'============='
            iter = report_lines%begin()
            do while (iter /= report_lines%end())
               write(*,'(a)') iter%of()
               call iter%next()
            end do
            write(*,'(a)') ''
         end if
    end subroutine generate_report

    end program Regrid_Util
