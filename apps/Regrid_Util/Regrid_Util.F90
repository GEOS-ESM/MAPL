#include "MAPL.h"

   module regrid_util_support_mod

   use ESMF
   use MAPL
   use fargparse
   use gFTL2_StringVector
   use gFTL2_IntegerVector
   use gFTL2_RealVector

   implicit NONE
   private

   public :: regrid_support
   public :: uninit
   public :: UnpackGridName
   public :: split_string
   public :: local_am_i_root

   real, parameter :: uninit = MAPL_UNDEF

   type regrid_support
      type(ESMF_Grid)     :: new_grid
      class(mapl_VerticalGrid), pointer :: new_vgrid ! same as old for now...
      type(StringVector) :: filenames,outputfiles
      type(mapl_CompressionSettings) :: compression_settings
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

     subroutine process_command_line(this, rc)
     class(regrid_support) :: this
     integer, optional, intent(out) :: rc

     character(len=ESMF_MAXSTR) :: RegridMth
     integer :: status
     character(len=ESMF_MAXPATHLEN*100) :: cfileNames, coutputFiles
     character(len=ESMF_MAXSTR) :: gridname
     type(ESMF_HConfig) :: hconfig_compression

      type(ArgParser) :: parser
      type(StringUnlimitedMap) :: options
      class(*), pointer :: option
      type(IntegerVector) :: int_vec
      type(RealVector) :: real_vec
      character(len=:), allocatable :: tmp_str

     ! Defaults for options not covered by fargparse default= keyword
     this%cs_stretch_param = uninit
     this%lon_range = uninit
     this%lat_range = uninit
     this%onlyvars = .false.
     this%alltimes = .true.

     parser = ArgParser()

     call parser%add_argument('-i', '--input', &
          help='Comma-separated list of input NetCDF files', &
          action='store', type='string')
     call parser%add_argument('-o', '--output', &
          help='Comma-separated list of output NetCDF files', &
          action='store', type='string')
     call parser%add_argument('--ogrid', &
          help='Output grid name (e.g. PE180x1080-CF)', &
          action='store', type='string')
     call parser%add_argument('--nx', &
          help='MPI decomposition in x (default: 1)', &
          action='store', type='integer', default=1)
     call parser%add_argument('--ny', &
          help='MPI decomposition in y (default: 1)', &
          action='store', type='integer', default=1)
     call parser%add_argument('--vars', &
          help='Comma-separated list of variable names to regrid', &
          action='store', type='string')
     call parser%add_argument('--t', &
          help='Time range as two integers: YYYYMMDD HHMMSS (start and end)', &
          action='store', type='integer', n_arguments=2)
     call parser%add_argument('--stretch_factor', &
          help='Cubed-sphere stretch parameters: stretch_factor target_lon target_lat', &
          action='store', type='real', n_arguments=3)
     call parser%add_argument('--lon_range', &
          help='Longitude range for regional grid: lon_min lon_max', &
          action='store', type='real', n_arguments=2)
     call parser%add_argument('--lat_range', &
          help='Latitude range for regional grid: lat_min lat_max', &
          action='store', type='real', n_arguments=2)
     call parser%add_argument('--method', &
          help='Regrid method (default: bilinear)', &
          action='store', type='string', default='bilinear')
     call parser%add_argument('--tp_in', &
          help='Tripolar input grid descriptor file', &
          action='store', type='string')
     call parser%add_argument('--tp_out', &
          help='Tripolar output grid descriptor file', &
          action='store', type='string')
     call parser%add_argument('--shave', &
          help='Number of bits to shave for compression (default: -1, disabled)', &
          action='store', type='integer', default=-1)
     call parser%add_argument('--deflate', &
          help='Deflate compression level 0-9 (default: 0, disabled)', &
          action='store', type='integer', default=0)
     call parser%add_argument('--quantize_algorithm', &
          help='Quantize algorithm name (default: NONE)', &
          action='store', type='string', default='NONE')
     call parser%add_argument('--quantize_level', &
          help='Quantize level (default: 0)', &
          action='store', type='integer', default=0)
     call parser%add_argument('--zstandard_level', &
          help='Zstandard compression level (default: 0, disabled)', &
          action='store', type='integer', default=0)
     call parser%add_argument('--file_weights', &
          help='Use weight files for regridding', &
          action='store_true', default=.false.)

     options = parser%parse_args()

     ! --- Required arguments ---

      option => options%at('input')
      _ASSERT(associated(option), 'required argument --input / -i not provided')
      call cast(option, tmp_str)
      cfilenames = tmp_str

      option => options%at('output')
      _ASSERT(associated(option), 'required argument --output / -o not provided')
      call cast(option, tmp_str)
      coutputfiles = tmp_str

      option => options%at('ogrid')
      _ASSERT(associated(option), 'required argument --ogrid not provided')
      call cast(option, tmp_str)
      gridname = tmp_str

     ! --- Arguments with defaults (always associated) ---

     option => options%at('nx')
     if (associated(option)) call cast(option, this%nx)

     option => options%at('ny')
     if (associated(option)) call cast(option, this%ny)

      option => options%at('method')
      if (associated(option)) then
         call cast(option, tmp_str)
         RegridMth = tmp_str
      end if

     option => options%at('shave')
     if (associated(option)) call cast(option, this%shave)

     option => options%at('deflate')
     if (associated(option)) call cast(option, this%deflate)

     option => options%at('quantize_algorithm')
     if (associated(option)) call cast(option, this%quantize_algorithm)

     option => options%at('quantize_level')
     if (associated(option)) call cast(option, this%quantize_level)

     option => options%at('zstandard_level')
     if (associated(option)) call cast(option, this%zstandard_level)

     option => options%at('file_weights')
     if (associated(option)) call cast(option, this%use_weights)

     ! --- Optional arguments (not associated when absent) ---

      option => options%at('vars')
      if (associated(option)) then
         call cast(option, tmp_str)
         this%vars = tmp_str
         this%onlyVars = .true.
      end if

     option => options%at('t')
     if (associated(option)) then
        call cast(option, int_vec)
        this%itime(1) = int_vec%at(1)
        this%itime(2) = int_vec%at(2)
        this%alltimes = .false.
     end if

     option => options%at('stretch_factor')
     if (associated(option)) then
        call cast(option, real_vec)
        this%cs_stretch_param(1) = real_vec%at(1)
        this%cs_stretch_param(2) = real_vec%at(2)  ! target_lon in degrees
        this%cs_stretch_param(3) = real_vec%at(3)  ! target_lat in degrees
     end if

     option => options%at('lon_range')
     if (associated(option)) then
        call cast(option, real_vec)
        this%lon_range(1) = real_vec%at(1)
        this%lon_range(2) = real_vec%at(2)
     end if

     option => options%at('lat_range')
     if (associated(option)) then
        call cast(option, real_vec)
        this%lat_range(1) = real_vec%at(1)
        this%lat_range(2) = real_vec%at(2)
     end if

     option => options%at('tp_in')
     if (associated(option)) call cast(option, this%tripolar_file_in)

     option => options%at('tp_out')
     if (associated(option)) call cast(option, this%tripolar_file_out)

     ! --- Post-parse validation and setup ---

     if (.not. allocated(this%tripolar_file_out)) then
        this%tripolar_file_out = "empty"
     end if
     this%regridMethod = mapl_regrid_method_string_to_int(RegridMth)
     _ASSERT(this%regridMethod /= MAPL_UNSPECIFIED_REGRID_METHOD, "improper regrid method chosen")

     this%filenames = split_string(cfilenames, ',')
     this%outputfiles = split_string(coutputfiles, ',')
     _ASSERT(this%filenames%size() > 0, 'no input files')
     _ASSERT(this%outputfiles%size() > 0, 'no output files specified')
     _ASSERT(this%filenames%size() == this%outputfiles%size(), 'different number of input and output files')
     if (.not. this%alltimes) then
        _ASSERT(this%filenames%size() == 1, 'if selecting time from file, can only regrid a single file')
     end if

     call this%create_grid(gridname, _RC)
     call this%create_vgrid(_RC)
     hconfig_compression = this%fill_in_compression_hconfig(_RC)
     this%compression_settings = mapl_CompressionSettings(hconfig_compression, _RC)
     _RETURN(_SUCCESS)

     end subroutine process_command_line

     subroutine create_vgrid(this,rc)
     class(regrid_support) :: this
    integer, optional, intent(out) :: rc

    type(mapl_NetCDF4_FileFormatter)     :: file_formatter
    type(mapl_FileMetaData)              :: metadata
    class(mapl_VerticalGridManager), pointer :: vgrid_manager
    character(len=:), pointer :: file_name
    integer :: status

    file_name => this%filenames%at(1)
    call file_formatter%open(trim(file_name), MAPL_PFIO_READ, _RC)
    metadata = file_formatter%read(_RC)
    call file_formatter%close(_RC)
    vgrid_manager => mapl_get_vertical_grid_manager(_RC)
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
    type(mapl_MAPLGeom), pointer :: mapl_geom
    type(ESMF_Geom) :: geom
    type(mapl_GeomManager), pointer :: geom_mgr

    call UnpackGridName(Grid_name,im_world,jm_world,dateline,pole)

    geom_hconfig = create_output_geom_hconfig(grid_name,im_world,jm_world,this%nx,this%ny,this%cs_stretch_param,this%lon_range,this%lat_range,this%tripolar_file_out,_RC)
    geom_mgr => mapl_get_geom_manager()
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
   use regrid_util_support_mod
   use mpi
   use gFTL2_StringVector

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

   type(mapl_FieldBundleWriter) :: newWriter
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
   call MAPL_FieldBundleSet(bundle, fieldBundleType=MAPL_FIELDBUNDLETYPE_BASIC, _RC)
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
      type(mapl_NetCDF4_fileFormatter) :: formatter
      type(mapl_FileMetadata) :: basic_metadata
      type(FileMetadataUtils) :: metadata

      call formatter%open(trim(filename),MAPL_PFIO_Read,_RC)
      basic_metadata=formatter%read(_RC)
      call metadata%create(basic_metadata,trim(filename))

      call formatter%close(_RC)

      tsteps = metadata%get_dimension('time',_RC)
      call metadata%get_time_info(timeVector=tSeries,_RC)

      if (.not.allTimes) then
         tSteps=1
         call mapl_UnpackDateTIme(itime,year,month,day,hour,minute,second)
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
