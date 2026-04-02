#include "MAPL.h"

   module regrid_util_support_mod

   use ESMF
   use mapl3
   use pfio
   use gFTL2_StringVector

   implicit NONE

   public

   real, parameter :: uninit = MAPL_UNDEF

   type regrid_support
      character(len=:), allocatable :: gridname
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
         call get_command_argument(i+1,gridname)
         this%gridname = gridname
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
         call MPI_Finalize(status)
         return
      end select
    enddo

    if (.not.allocated(this%tripolar_file_out)) then
       this%tripolar_file_out = "empty"
    end if
    !this%regridMethod = regrid_method_string_to_int(regridMth)
    !_ASSERT(this%regridMethod/=UNSPECIFIED_REGRID_METHOD,"improper regrid method chosen")

    this%filenames = split_string(cfilenames,',')
    this%outputfiles = split_string(coutputfiles,',')
    _ASSERT(this%filenames%size() > 0, 'no input files')
    _ASSERT(this%outputfiles%size() >0, 'no ouput files specified')
    _ASSERT(this%filenames%size() == this%outputfiles%size(), 'different number of input and output files')
    if (.not.this%alltimes) then
       _ASSERT(this%filenames%size() == 1,'if selecting time from file, can only regrid a single file')
    end if

    _RETURN(_SUCCESS)

    end subroutine process_command_line

    subroutine create_grid(this,grid_name,geom_hconfig,rc)
    class(regrid_support) :: this
    character(len=*), intent(in) :: grid_name
    type(ESMF_HConfig), intent(inout) :: geom_hconfig
    integer, optional, intent(out) :: rc

    integer :: im_world,jm_world,lm_world
    character(len=2) :: dateline,pole
    integer :: status

    call UnpackGridName(Grid_name,im_world,jm_world,dateline,pole)

    lm_world=1
    geom_hconfig = create_output_geom_hconfig(grid_name,im_world,jm_world,this%nx,this%ny,lm_world,this%cs_stretch_param,this%lon_range,this%lat_range,this%tripolar_file_out,_RC)

    _RETURN(_SUCCESS)
    end subroutine create_grid

    function create_output_geom_hconfig(grid_name,im_world,jm_world,nx,ny,lm,cs_stretch_param,lon_range,lat_range,tripolar_file,rc) result(output_geom_hconfig)
       type(ESMF_HConfig)              :: output_geom_hconfig
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
       character(len=:), allocatable :: grid_class

       grid_class = 'latlon'

       nn = len_trim(grid_name)
       dateline=grid_name(nn-1:nn)
       pole=grid_name(1:2)

       if (dateline=='CF') grid_class = 'CubedSphere'
       if (dateline=='TM') then
          _FAIL('tripolar grid not supported')
       end if

       output_geom_hconfig = ESMF_HConfigCreate(_RC)
       !call MAPL_ConfigSetAttribute(cf,value=NX, label=trim(grid_name)//".NX:",_RC)
       !call MAPL_ConfigSetAttribute(cf,value=lm, label=trim(grid_name)//".LM:",_RC)
       if (grid_class=='CubedSphere') then
          call ESMF_HConfigAdd(output_geom_hconfig, 'CubedSphere', keyString='class', _RC)
          call ESMF_HConfigAdd(output_geom_hconfig, im_world, keyString='im_world', _RC)
          call ESMF_HConfigAdd(output_geom_hconfig, nx, keyString='nx_face', _RC)
          call ESMF_HConfigAdd(output_geom_hconfig, ny/6, keyString='ny_face', _RC)
 
          if (any(cs_stretch_param/=uninit)) then
             call ESMF_HConfigAdd(output_geom_hconfig,cs_stretch_param(1),keyString='stretch_factor',_RC)
             call ESMF_HConfigAdd(output_geom_hconfig,cs_stretch_param(2),keyString='target_lon',_RC)
             call ESMF_HConfigAdd(output_geom_hconfig,cs_stretch_param(3),keyString='target_lat',_RC)
          end if
       else if (grid_class=='latlon') then
          call ESMF_HConfigAdd(output_geom_hconfig, 'latlon', keyString='class', _RC)
          call ESMF_HConfigAdd(output_geom_hconfig, im_world, keyString='im_world', _RC)
          call ESMF_HConfigAdd(output_geom_hconfig, jm_world, keyString='jm_world', _RC)
          call ESMF_HConfigAdd(output_geom_hconfig, pole, keyString='pole', _RC)
          call ESMF_HConfigAdd(output_geom_hconfig, dateline, keyString='dateline', _RC)
          call ESMF_HConfigAdd(output_geom_hconfig, nx, keyString='nx', _RC)
          call ESMF_HConfigAdd(output_geom_hconfig, ny, keyString='ny', _RC)
          if (pole=='XY' .and. dateline=='XY') then
             _ASSERT(all(lon_range/=uninit),'if regional must specify lon_range')
             _ASSERT(all(lat_range/=uninit),'if regional must specify lat_range')
             call ESMF_HConfigAdd(output_geom_hconfig, lon_range, keyString='lon_range',_RC)
             call ESMF_HConfigAdd(output_geom_hconfig, lat_range, keyString='lat_range',_RC)
          end if
       end if
       _RETURN(_SUCCESS)
     end function create_output_geom_hconfig

   subroutine add_varspecs_from_file(gridcomp, data_file, state_intent, rc)
      type(ESMF_GridComp), intent(inout) :: gridcomp
      character(len=*), intent(in) :: data_file
      type(ESMF_STATEINTENT_FLAG) :: state_intent
      integer, optional, intent(out) :: rc

      integer :: status
      type(FileMetadata) :: metadata, file_variables
      type(NetCDF4_FileFormatter) :: formatter
      type(StringVariableMap), pointer :: variables
      type(MAPLGeom), pointer :: mapl_geom
      class(GeomFactory), allocatable :: factory
      class(GeomSpec), allocatable :: geom_spec
      type(StringVector) :: file_variables_vec
      type(StringVariableMapIterator) :: var_iter
      type(VariableSpec) :: varspec
      character(len=:), pointer :: var_name
      type(GeomManager), pointer :: geom_mgr

      _HERE,' reading bmaa: '//data_file
      call formatter%open(data_file, pFIO_READ, _RC)
      metadata = formatter%read(_RC)
      call formatter%close(_RC)
      geom_mgr => get_geom_manager()
      mapl_geom => geom_mgr%get_mapl_geom_from_metadata(metadata, _RC)

      factory = mapl_geom%get_factory()
      geom_spec = mapl_geom%get_spec()
      file_variables = factory%make_file_metadata(geom_spec, _RC)
      file_variables_vec = string_vector_from_metadata(file_variables)
      call file_variables_vec%push_back('time')
      variables => metadata%get_variables()
      var_iter = variables%ftn_begin()
      do while (var_iter /= variables%ftn_end())
         call var_iter%next()
         var_name => var_iter%first()
         if (string_in_vector(file_variables_vec, var_name)) cycle
         varspec = make_VariableSpec(state_intent, var_name, &
            typekind=ESMF_TYPEKIND_R4,  itemType=ESMF_STATEITEM_FIELD, _RC)
      enddo

      _RETURN(_SUCCESS)
   end subroutine add_varspecs_from_file

   function string_in_vector(string_vector, string) result(in_vector)
      logical :: in_vector
      type(StringVector), intent(in) :: string_vector
      character(len=*), intent(in) :: string

      integer :: i

      in_vector = .false.
      do i=1,string_vector%size()
         if (trim(string) == string_vector%of(i)) then
            in_vector = .true.
            exit
         end if
      enddo
   end function string_in_vector

   function string_vector_from_metadata(metadata) result(string_vector)
      type(StringVector) :: string_vector
      type(FileMetadata), intent(in) :: metadata
      type(StringVariableMap), pointer :: variables
      type(StringVariableMapIterator) :: var_iter
      character(len=:), pointer :: var_name

      variables =>  metadata%get_variables()
      var_iter = variables%ftn_begin()
      do while (var_iter /= variables%ftn_end())
         call var_iter%next()
         var_name => var_iter%first()
         call string_vector%push_back(var_name)
      enddo

   end function string_vector_from_metadata

   subroutine add_geom_from_file(gridcomp, data_file, rc)
      type(ESMF_GridComp), intent(inout) :: gridcomp
      character(len=*), intent(in) :: data_file
      integer, optional, intent(out) :: rc

      integer :: status
      type(FileMetadata) :: metadata
      type(NetCDF4_FileFormatter) :: formatter
      type(MAPLGeom), pointer :: mapl_geom
      type(GeomManager), pointer :: geom_mgr
      type(ESMF_Geom) :: geom

      call formatter%open(data_file, pFIO_READ, _RC)
      metadata = formatter%read(_RC)
      call formatter%close(_RC)
      geom_mgr => get_geom_manager()
      mapl_geom => geom_mgr%get_mapl_geom_from_metadata(metadata, _RC)
      geom = mapl_geom%get_geom()
      call MAPL_GridCompSetGeom(gridcomp, geom, _RC)

      _RETURN(_SUCCESS)
   end subroutine add_geom_from_file

   end module regrid_util_support_mod
