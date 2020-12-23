#include "MAPL_Generic.h"

!---------------------------
! Note - this module abuses global variables as a simple mechanism for
! sharing data among procedures.  The module is only intended to be
! used with this one driver, so the use of globals is (barely)
! forgiveable.  Any further extension should involve creating a data
! type (or types) to encapsulate these and pass them between the
! various procedures.
!---------------------------

module SupportMod
   use ESMF
   use MAPL_ExceptionHandling
   use MAPL_BaseMod
   use pFIO
   use MAPL_ConstantsMod
   use MAPL_RangeMod
   use MAPL_StringRouteHandleMapMod
   use gFTL_StringVector
   use gFTL_StringIntegerMap
   use, intrinsic :: iso_fortran_env, only: REAL32, REAL64
   use mpi
   implicit none
   public



   integer :: local_pet
   integer :: pet_count
   integer :: npx, npy, px, py
   integer :: ierror
   integer, parameter :: N_TILES = 6


   type (StringRouteHandleMap) :: route_handles
   integer :: srcTerm
   type (ESMF_RouteHandle), target :: default_route_handle
   

   type RegridSupport
     ! Cubed-sphere
     integer :: Xdim, Ydim
     integer :: nx_loc, ny_loc
     integer :: x_1, x_n
     integer :: y_1, y_n
     integer :: my_tile
     character(len=:), allocatable :: in_file
     type (FileMetadata) :: cfio_cubed_sphere
     type (ESMF_Grid) :: grid_cubed_sphere
     type (Netcdf4_Fileformatter) :: formatter_cubed_sphere
     
     ! Lat-lon
     integer :: IM
     integer :: JM
     character(len=:), allocatable :: out_file
     type (FileMetadata) :: cfio_lat_lon
     type (ESMF_Grid) :: grid_lat_lon
     integer :: i_1, i_n
     integer :: j_1, j_n
     type (Netcdf4_Fileformatter) :: formatter_lat_lon
     real(kind=ESMF_KIND_R8), allocatable :: longitudes(:)
     real(kind=ESMF_KIND_R8), allocatable :: latitudes(:)
     
     ! Both
     logical :: debug = .false.
     integer :: LM
     integer :: NT
     
     ! Misc
     type (StringVector) :: requested_variables
     type (StringVector) :: scalar_variables
     type (StringVector) :: vector_variables(2)
   contains
     procedure :: process_command_line
     procedure :: read_metadata
     procedure :: create_esmf_grids
     procedure :: create_cubed_sphere_grid
     procedure :: create_lat_lon_grid
     procedure :: transfer_metadata
     procedure :: write_metadata
     procedure :: write_data

   end type RegridSupport

contains

   ! The following procedure parses the command line to find various
   ! arguments for file names, target grid resolution, etc.
   subroutine process_command_line(regridder, rc)
      class (RegridSupport) :: regridder
      integer, optional, intent(out) :: rc

      integer :: n_args
      integer :: i_arg
      character(len=:), allocatable :: argument
      character(len=:), allocatable :: buffer

      n_args = command_argument_count()

      i_arg = 0
      do
         if (i_arg > n_args) exit

         argument = get_next_argument()

         select case (argument)
         case ('-i', '--ifile')
            regridder%in_file = get_next_argument()
            _ASSERT(regridder%in_file(1:1) /= '-','bad format')
         case ('-o', '--ofile')
             regridder%out_file = get_next_argument()
            _ASSERT(regridder%in_file(1:1) /= '-','bad format')
         case ('--nlats')
            buffer  = get_next_argument()
            _ASSERT(buffer(1:1) /= '-','bad format')
            read(buffer,*) regridder%JM
         case ('--nlons')
            buffer  = get_next_argument()
            _ASSERT(buffer(1:1) /= '-','bad format')
            read(buffer,*) regridder%IM
         case ('--vars')
            buffer = get_next_argument()
            _ASSERT(buffer(1:1) /= '-','bad format')
            regridder%requested_variables = parse_vars(buffer)
         case ('-d', '--debug')
            regridder%debug = .true.
         case default
            ! ignore
         end select

      end do

   contains
      
      function get_next_argument() result(argument)
         character(len=:), allocatable :: argument
         
         integer :: length
         
         i_arg = i_arg + 1
         
         call get_command_argument(i_arg, length=length)
         allocate(character(len=length) :: argument)
         call get_command_argument(i_arg, value=argument)
         
      end function get_next_argument


      function parse_vars(buffer) result(vars)
         type (StringVector) :: vars
         character(len=*), intent(in) :: buffer

         integer :: idx
         character(len=1), parameter :: COMMA = ','
         character(len=:), allocatable :: string

         string = buffer // COMMA
         do
            if (len(string) == 0) exit
            idx = index(string,COMMA)
            call vars%push_back(string(1:idx-1))
            string = string(idx+1:)
         end do

      end function parse_vars

   end subroutine process_command_line


   ! Read the NetCDF metadata from the input file and store
   ! in a FileMetadata object.
   subroutine read_metadata(this)
      class (RegridSupport), intent(inout) :: this

      associate (formatter => this%formatter_cubed_sphere )
         call formatter%open(this%in_file, mode=pFIO_READ)
         this%cfio_cubed_sphere = formatter%read()
         this%nt = formatter%inq_dim('time')
         call formatter%close()
      end associate

    end subroutine read_metadata

   ! Create the metadata for a lat-lon grid, and transfer non-grid
   ! metadata from the CS FileMetadata object into the new one.  I.e. copy
   ! what you can/should and create the rest from input parameters.
   subroutine transfer_metadata(this)!vars)
      class (RegridSupport), intent(inout) :: this

      integer :: status

      call add_grid_dimensions()
      call add_grid_variables()

      call add_global_attributes()
      call add_variables()

   contains      

      subroutine add_grid_dimensions()
         integer :: status
         type (StringIntegerMap), pointer :: dims

         dims => this%cfio_cubed_sphere%get_dimensions()

         associate ( ll => this%cfio_lat_lon )
            call ll%add_dimension('lon', this%IM, rc=status)
            call ll%add_dimension('lat', this%JM, rc=status)
            if (associated(dims%at('lev'))) then
               call ll%add_dimension('lev', this%LM, rc=status)
            end if
            call ll%add_dimension('time', this%nt, rc=status)
        end associate

      end subroutine add_grid_dimensions


      subroutine add_grid_variables()
         type (Variable), pointer :: v

         type (Variable) :: tmp
         integer :: status

         associate ( ll => this%cfio_lat_lon, cs => this%cfio_cubed_sphere )

            tmp = Variable(type=pFIO_REAL32, dimensions='lat')
            !call tmp%add_attribute('long_name', Attribute('latitude'))
            call tmp%add_attribute('long_name', 'latitude')
            !call tmp%add_attribute('units', Attribute('degrees_north'))
            call tmp%add_attribute('units', 'degrees_north')
            call ll%add_variable('lat', tmp, rc=status)
         
            tmp = Variable(type=pFIO_REAL32, dimensions='lon')
            !call tmp%add_attribute('long_name', Attribute('longitudes'))
            call tmp%add_attribute('long_name', 'longitudes')
            !call tmp%add_attribute('units', Attribute('degrees_east'))
            call tmp%add_attribute('units', 'degrees_east')
            call ll%add_variable('lon', tmp, rc=status)

            v => cs%get_variable('lev')
            if (associated(v)) call ll%add_variable('lev', v, rc=status)
            v => cs%get_variable('time')
            call ll%add_variable('time', v, rc=status)

         end associate

      end subroutine add_grid_variables


      subroutine add_global_attributes()
         type (StringAttributeMapIterator) :: iter
         type (StringAttributeMap), pointer :: attributes
         character(len=:), pointer :: name

         type (Attribute), pointer :: attr

         associate ( ll => this%cfio_lat_lon, cs => this%cfio_cubed_sphere )

            attributes => cs%get_attributes()
            iter = attributes%begin()
            do while (iter /= attributes%end())
               name => iter%key()
               attr => iter%value()

               call ll%add_attribute(name, attr)
   
               call iter%next()
            end do

         end associate

      end subroutine add_global_attributes


      subroutine add_variables()
         type (StringVariableMapIterator) :: var_iter
         type (StringVariableMap), pointer :: variables

         character(len=:), allocatable :: ll_var_dimensions
         character(len=:), pointer :: var_name

         type (Variable), pointer :: cs_variable
         type (StringVector), pointer :: cs_var_dimensions
         type (Variable) :: ll_variable

         associate ( ll => this%cfio_lat_lon, cs => this%cfio_cubed_sphere )

         variables => cs%get_variables()
         var_iter = variables%begin()
         do while (var_iter /= variables%end())
            var_name => var_iter%key()
            select case (var_name)
               ! CS specific variables
            case ('nf', 'ncontact', 'cubed_sphere', &
                 & 'Xdim', 'Ydim', 'lons', 'lats', &
                 & 'contacts', 'orientation', 'anchor', &
                 & 'lev', 'time')
               ! skip CS specific variables
            case default

               if (keep_var(var_name, this%requested_variables)) then

                  cs_variable => var_iter%value()

                  cs_var_dimensions => cs_variable%get_dimensions()
                  ll_var_dimensions = make_dim_string(cs_var_dimensions)

                  if (associated(this%cfio_cubed_sphere%get_variable('lev'))) then
                     ll_variable = Variable(type=cs_variable%get_type(), dimensions=ll_var_dimensions, &
                          & chunksizes = [this%IM/npx,this%JM/npy,1,1,1])
                  else
                     ll_variable = Variable(type=cs_variable%get_type(), dimensions=ll_var_dimensions, &
                          & chunksizes = [this%IM/npx,this%JM/npy,1,1])
                  end if

                  call transfer_attributes(from=cs_variable, to=ll_variable)
                  call ll%add_variable(var_name, ll_variable)

                  call categorize(cs_variable, var_name, variables)

               end if

            end select
               
            call var_iter%next()
         end do

         end associate

      end subroutine add_variables

      subroutine transfer_attributes(from, to)
         type (Variable), target, intent(in) :: from
         type (Variable), target, intent(inout) :: to
         
         type (StringAttributeMap), pointer :: attributes
         type (StringAttributeMapIterator) :: attr_iter
         character(len=:), pointer :: attr_name

         attributes => from%get_attributes()
         attr_iter = attributes%begin()
         do while (attr_iter /= attributes%end())
            attr_name => attr_iter%key()
            select case (attr_name)
            case ('grid_mapping','coordinates') ! CS specific attributes
               ! skip
            case default
               call to%add_attribute(attr_name, attr_iter%value())
            end select
            call attr_iter%next()
         end do

      end subroutine transfer_attributes


      ! Is variable a scalar or vector?
      subroutine categorize(var, var_name, vars, rc)
         type (Variable), target, intent(in) :: var
         character(len=*), intent(in) :: var_name
         type (StringVariableMap), target, intent(in) :: vars
         integer, optional, intent(out) :: rc

         type (StringAttributeMap), pointer :: attributes
         type (Attribute), pointer :: long_name_attr
         character(len=:), allocatable :: long_name
         character(len=:), allocatable :: north_component
         integer :: status

         class(*), pointer :: a

         attributes => var%get_attributes()
         long_name_attr => attributes%at('long_name')
         if (.not. associated(long_name_attr)) then
            _RETURN(_SUCCESS)
         end if

         a => long_name_attr%get_value()
         _ASSERT(associated(a),'invalid pointer')
         select type (a)
         type is (character(len=*))
            long_name = ESMF_UtilStringLowerCase(a, rc=STATUS)
         class default
            _FAIL('incorrect type')
         end select

         if (index(long_name, 'east') > 0) then ! East component of a vector
            north_component = find_north_component(vars, long_name)
            _ASSERT(north_component /= '','needs informative message')
            call this%vector_variables(1)%push_back(var_name)
            call this%vector_variables(2)%push_back(north_component)
         elseif (index(long_name, 'north') == 0) then ! 
            call this%scalar_variables%push_back(var_name)
         end if
         
      end subroutine categorize


      ! For variables that have 'north' in their long name, we need to
      ! find the corresponding 'east' variable to properly regrid
      ! vector quantities.  The function returns the name of the
      ! corresponding 'north' variable if it exists, otherwise an 0
      ! length string is returned.
      !
      ! NOTE: This routine is called for all variables, not just those
      ! with 'north' in the long name.
      !
      ! The logic is a bit complicated due to the use of unlimited polymorphic
      ! entities to store CFIO attributes.   This means we need to do
      ! SELECT TYPE on each quantity to cast it as a string before we can
      ! compare.
      !
      function find_north_component(vars, long_name, rc) result(north_component)
         character(len=:), allocatable :: north_component
         type (StringVariableMap), target, intent(in) :: vars
         character(len=*), intent(in) :: long_name
         integer, optional, intent(out) :: rc

         type (StringVariableMapIterator) :: var_iter
         type (Variable), pointer :: var
         type (StringAttributeMap), pointer :: attrs
         type (Attribute), pointer :: attr
         character(len=:), allocatable :: trial
         integer :: idx
         class (*), pointer :: a

         north_component = '' ! unless
         var_iter = vars%begin()
         do while (var_iter /= vars%end())
            var => var_iter%value()
            attrs => var%get_attributes()
            attr => attrs%at('long_name')

            if (associated(attr)) then
               a => attr%get_value()
               _ASSERT(associated(a),'invalid pointer')
               select type (a)
               type is (character(len=*))
                  trial = ESMF_UtilStringLowerCase(a, rc=status)
               class default
                  _FAIL('incorrect type')
               end select

               idx = index(trial, 'north')
               if (idx /= 0) then
                  trial = trial(1:idx-1) // 'east' // trial(idx+5:)
                  if (trial == long_name) then ! success
                     north_component = var_iter%key()
                  end if
               end if
            end if
            call var_iter%next()
         end do
         
      end function find_north_component

      logical function keep_var(var_name, requested_vars)
         character(len=*), intent(in) :: var_name
         type (StringVector), intent(in) :: requested_vars
         
         integer :: idx
         
         if (requested_vars%size() == 0) then
            keep_var = .true.
         else
            keep_var = (requested_vars%get_index(var_name) /= 0)
         end if
         
      end function keep_var


      ! Convert CS dimensions of a varible into Lat-Lon dimensions.
      ! The former arrive as a StringVector, but this function produces
      ! a string of the form '<d1>,<d2>,...' to mimic the convenient form
      ! for the Variable() constructor.
      function make_dim_string(cs_dims) result(ll_dims)
         character(len=:), allocatable :: ll_dims
         type (StringVector), target, intent(in) :: cs_dims

         type (StringVectorIterator) :: dim_iter
         character(len=:), pointer :: d
         
         ll_dims = ''
         dim_iter = cs_dims%begin()
         do while (dim_iter /= cs_dims%end())
            d => dim_iter%get()
            select case (d)
            case ('Ydim')
               ll_dims = ll_dims // 'lat' // pFIO_DIMENSION_SEPARATOR
            case ('Xdim')
               ll_dims = ll_dims // 'lon' // pFIO_DIMENSION_SEPARATOR
            case ('nf')
               ! skip
            case default
               ll_dims = ll_dims // d // pFIO_DIMENSION_SEPARATOR
            end select
            call dim_iter%next()
         end do
      end function make_dim_string
               
   end subroutine transfer_metadata

   function run_length_encode(missing) result(str)
      character(len=:), allocatable :: str
      logical, intent(in) :: missing(:)

      integer :: i
      integer :: count
      logical :: value

      if (size(missing) == 0) then
         str = ''
         return
      end if
      
      count = 1
      value = missing(1)
      str = to_string_bool(value)

      do i = 2, size(missing)
        if (value .eqv. missing(i)) then
           count = count + 1
        else
           value = missing(i)
           str = str // to_string(count) // to_string_bool(value)
           count = 1
        end if
     end do

     str = str // to_string(count)

   contains

      function to_string(count) result(str)
         character(len=:), allocatable :: str
         integer, intent(in) :: count
         character(len=8) :: buffer

         write(buffer,'(i0)') count
         str = trim(buffer)
      end function to_string

      function to_string_bool(bool) result(str)
         character(len=1) :: str
         logical, intent(in) :: bool
         if (bool) then
            str = 'T'
         else
            str = 'F'
          end if
       end function to_string_bool

   end function run_length_encode

   function all_gather(local) result(global)
      character(len=*), intent(in) :: local
      character(len=:), allocatable :: global

      integer :: p
      integer, allocatable :: counts(:)
      integer, allocatable :: displs(:)
      integer :: ierror

      allocate(counts(0:pet_count-1))
      allocate(displs(0:pet_count-1))

      call mpi_allgather(len(local), 1, MPI_INTEGER, &
           & counts, 1, MPI_INTEGER, MPI_COMM_WORLD, ierror)

      displs(0) = 0
      do p = 1, pet_count - 1
          displs(p) = displs(p-1) + counts(p-1)
      end do

      allocate(character(len=sum(counts)) :: global)
      call mpi_allgatherv(local, len(local), MPI_CHAR, &
           global, counts, displs, MPI_CHAR, MPI_COMM_WORLD, ierror)

   end function all_gather


   subroutine regrid(srcField, dstField, missing, rc)
     type (ESMF_Field), intent(in) :: srcField
     type (ESMF_Field), intent(inout) :: dstField
     real (kind=REAL32), optional, intent(in) :: missing
     integer, optional, intent(out) :: rc

     integer :: status
     real(kind=REAL32), pointer :: src_array(:,:)
     real(kind=REAL32), pointer :: dst_array(:,:)
     type (ESMF_RouteHandle), pointer :: handle
     character(len=:), allocatable :: local_key
     character(len=:), allocatable :: global_key
     integer, pointer :: mask(:,:)
     type (ESMF_Array) :: mask_array
     type (ESMF_Grid) :: grid
     logical :: have_missing, any_missing

     call ESMF_FieldGet(srcField, 0, src_array)
     if (present(missing)) then
        have_missing = any(missing == src_array)
        call MPI_AllReduce(have_missing, any_missing, 1, MPI_LOGICAL, MPI_LOR, MPI_COMM_WORLD, ierror)
        if (any_missing) then
           local_key = run_length_encode(reshape(src_array,[size(src_array)]) == missing)
           global_key = all_gather(local_key)
           
           handle => route_handles%at(global_key)
           if (.not. associated(handle)) then
              allocate(handle)

              call ESMF_FieldGet(srcfield, grid=grid, rc=status)
              _VERIFY(status)
              call ESMF_GridGetItem(grid, staggerLoc=ESMF_STAGGERLOC_CENTER, &
                   & itemflag=ESMF_GRIDITEM_MASK, array=mask_array, rc=status)
              _VERIFY(status)
              call ESMF_ArrayGet(mask_array, farrayptr=mask, rc=status)
              _VERIFY(status)

              where (src_array == missing)
                 mask = 0
              elsewhere
                 mask = 1
              end where

              call ESMF_FieldRegridStore(srcField, dstField, &
                   & regridmethod=ESMF_REGRIDMETHOD_BILINEAR, lineType=ESMF_LINETYPE_GREAT_CIRCLE, &
                   & srcTermProcessing=srcTerm, &
                   & srcMaskValues = [0], &
                   & unmappedAction=ESMF_UNMAPPEDACTION_IGNORE, &
                   & routehandle=handle, rc=status)
              _VERIFY(status)

              call route_handles%insert(global_key, handle)

           endif

           call ESMF_FieldGet(dstField, 0, dst_array)
           dst_array = missing
           call ESMF_FieldRegrid(srcField, dstField, routeHandle=handle, &
                & termorderflag=ESMF_TERMORDER_SRCSEQ, &
                & zeroregion=ESMF_REGION_SELECT, &
                & rc=status)
           _VERIFY(status)
           
           _RETURN(_SUCCESS)
        else
           handle => default_route_handle
        end if
     else
        handle => default_route_handle
     end if
 
     call ESMF_FieldRegrid(srcField, dstField, routeHandle=handle, &
          & termorderflag=ESMF_TERMORDER_SRCSEQ, rc=status)
     _VERIFY(status)

     _RETURN(_SUCCESS)
   end subroutine regrid

   ! This routine does the big work of reading data from one file, regridding, and
   ! then writing to the other file.   Gets a bit messy to handle  2D vs 3D and scalar
   ! vs vector cases.  Certainly could be cleaned further in next pass.

   subroutine write_data(this, rc)
      class (RegridSupport), target, intent(inout) :: this
      integer, optional, intent(out) :: rc
      

      type (StringVariableMapIterator) :: var_iter
      type (StringVariableMap), pointer :: variables

      real(kind=REAL32), target, allocatable :: cs_scalar_patch(:,:)
      real(kind=REAL32), target, allocatable :: cs_vector_patch(:,:,:)
      real(kind=REAL32), target, allocatable :: cs_uvw(:,:,:)
      real(kind=REAL32), target, allocatable :: ll_uvw(:,:,:)
      real(kind=REAL32), target, allocatable :: ll_scalar_patch(:,:)
      real(kind=REAL32), target, allocatable :: ll_vector_patch(:,:,:)

      character(len=:), pointer :: var_name
      type (ESMF_Field) :: cs_scalar_field
      type (ESMF_Field) :: ll_scalar_field
      type (ESMF_Field) :: cs_uvw_field(3)
      type (ESMF_Field) :: ll_uvw_field(3)
      integer :: level
      integer :: num_levels
      integer :: status
      integer :: time
      type (StringVector), pointer :: dims
      type (Variable), pointer :: var

      integer, allocatable :: cs_start(:), cs_count(:)
      integer, allocatable :: ll_start(:), ll_count(:)

      real(kind=REAL64), pointer :: lat(:,:)
      real(kind=REAL64), pointer :: lon(:,:)

      type (Attribute), pointer :: missing_attr
      class(*), pointer :: missing_ptr(:)
      real(kind=REAL32), pointer :: missing

      integer :: d
      logical :: is_scalar
      logical :: is_east_vector_component
      integer :: idx
      character(len=:), allocatable :: north_component
      integer :: c0, c1,crate

      associate (cs_fmtr => this%formatter_cubed_sphere, ll_fmtr => this%formatter_lat_lon)
      call cs_fmtr%open(this%in_file, mode=pFIO_READ, rc=status)
      if (status /= pFIO_SUCCESS) then
         if (local_pet == 0) then
            print*, 'Unable to open input file: ',this%in_file
          end if
      end if
      call ll_fmtr%open(this%out_file, mode=pFIO_WRITE, rc=status)
      if (status /= pFIO_SUCCESS) then
         if (local_pet == 0) then
            print*, 'Unable to open new output file: ',this%out_file
            print*, 'Possibly it already exists?'
         end if
      end if

      call ll_fmtr%put_var('lat', this%latitudes)
      call ll_fmtr%put_var('lon', this%longitudes)

      block
        real(kind=REAL64) :: lev(this%LM)
        if (associated(this%cfio_cubed_sphere%get_variable('lev'))) then
           call cs_fmtr%get_var('lev', lev)
           call ll_fmtr%put_var('lev', lev)
        end if
      end block
      block
        integer :: time(this%NT)
        call cs_fmtr%get_var('time', time)
        call ll_fmtr%put_var('time', time)
      end block

      allocate(ll_scalar_patch(this%i_1:this%i_n, this%j_1:this%j_n))
      allocate(ll_vector_patch(this%i_1:this%i_n, this%j_1:this%j_n,2))
      allocate(ll_uvw(this%i_1:this%i_n, this%j_1:this%j_n,3))

      
      allocate(cs_scalar_patch(this%nx_loc,this%ny_loc))
      allocate(cs_vector_patch(this%nx_loc,this%ny_loc,2))

      allocate(cs_uvw(this%nx_loc,this%ny_loc,3))

      ! Create fields
      ll_scalar_field = ESMF_FieldCreate(this%grid_lat_lon, ll_scalar_patch(:,:), ESMF_INDEX_DELOCAL, &
           & datacopyflag=ESMF_DATACOPY_REFERENCE, rc=status)
      _VERIFY(status)
      cs_scalar_field = ESMF_FieldCreate(this%grid_cubed_sphere, cs_scalar_patch(:,:), ESMF_INDEX_DELOCAL, &
           & datacopyflag=ESMF_DATACOPY_REFERENCE, rc=status)
      _VERIFY(status)
      do d = 1, 3
         ll_uvw_field(d) = ESMF_FieldCreate(this%grid_lat_lon, ll_uvw(:,:,d), ESMF_INDEX_DELOCAL, &
              & datacopyflag=ESMF_DATACOPY_REFERENCE, rc=status)
         _VERIFY(status)
         cs_uvw_field(d) = ESMF_FieldCreate(this%grid_cubed_sphere, cs_uvw(:,:,d), ESMF_INDEX_DELOCAL, &
              & datacopyflag=ESMF_DATACOPY_REFERENCE, rc=status)
         _VERIFY(status)
      end do

      ! Create regrid
      srcTerm = 1
      call system_clock(c0,crate)
      call ESMF_FieldRegridStore(cs_scalar_field, ll_scalar_field, &
           & regridmethod=ESMF_REGRIDMETHOD_BILINEAR, lineType=ESMF_LINETYPE_GREAT_CIRCLE, &
           & srcTermProcessing=srcTerm, &
           & routehandle=default_route_handle, rc=status)
      _VERIFY(status)
      call system_clock(c1,crate)
      if (local_pet == 0) then
         print*,'regrid store: ', real(c1-c0)/crate
      end if

      block
        type (ESMF_VM) :: global
        call ESMF_VMGetGlobal(global, rc=status)
        call ESMF_VMBarrier(global, rc=status)
      end block
      variables => this%cfio_cubed_sphere%get_variables()
      var_iter = variables%begin()
      do while (var_iter /= variables%end())
         var_name => var_iter%key()

         select case (var_name)
         case ('nf', 'ncontact', 'cubed_sphere', &
              & 'Xdim', 'Ydim', 'lons', 'lats', &
              & 'contacts', 'orientation', 'anchor', &
              & 'lev','time')
            ! skip
         case default

            if ( local_pet == 0 .and. this%debug) then
               print*, 'var = ', var_name
            end if

            var => var_iter%value()
            missing_attr => var%get_attribute('missing_value')
            missing_ptr => missing_attr%get_values()

            select type (missing_ptr)
            type is (real(kind=REAL64))
               allocate(missing) ! memory leak!
               missing = missing_ptr(1)
            type is (real(kind=REAL32))
               missing => missing_ptr(1)
            class default
               ! no missing value?
            end select
            num_levels = 1
            dims => var%get_dimensions()
            if (dims%get_index('lev') /= 0) then
               num_levels = this%LM
            else
               num_levels = 1
            end if

            is_scalar = .false.
            is_east_vector_component = .false.
            
            do idx = 1, this%scalar_variables%size()
               if (this%scalar_variables%at(idx) == var_name) then
                  is_scalar = .true.
                  exit
               end if
            end do
            
            if (.not. is_scalar) then
               do idx = 1, this%vector_variables(1)%size()
                  if (this%vector_variables(1)%at(idx) == var_name) then
                     is_east_vector_component = .true.
                     north_component = this%vector_variables(2)%at(idx)
                     exit
                  end if
               end do
            end if
            
            if (.not. (is_scalar .or. is_east_vector_component)) then
               call var_iter%next()
               cycle
            end if

            
            do time = 1, this%nt
               do level = 1, num_levels

                  if (num_levels == 1) then
                     cs_start = [this%x_1,this%y_1,this%my_tile,time]
                     ll_start = [this%i_1,this%j_1,time]
                     cs_count = [this%nx_loc,this%ny_loc,1,1]
                     ll_count = [this%i_n-this%i_1+1, this%j_n-this%j_1+1,1]
                  else
                     cs_start = [this%x_1,this%y_1,this%my_tile,level,time]
                     ll_start = [this%i_1,this%j_1,level,time]
                     cs_count = [this%nx_loc,this%ny_loc,1,1,1]
                     ll_count = [this%i_n-this%i_1+1, this%j_n-this%j_1+1,1,1]
                  end if

                  if (is_east_vector_component) then ! vector

                     call cs_fmtr%get_var(var_name, cs_vector_patch(:,:,1), start=cs_start,count=cs_count, rc=status)
                     _VERIFY(status)
                     call cs_fmtr%get_var(north_component, cs_vector_patch(:,:,2), start=cs_start,count=cs_count, rc=status)
                     _VERIFY(status)

                     call ESMF_GridGetCoord(this%grid_cubed_sphere, coordDim=1, localDE=0, &
                          & staggerLoc=ESMF_STAGGERLOC_CENTER, farrayPtr=lon,rc=status)
                     call ESMF_GridGetCoord(this%grid_cubed_sphere, coordDim=2, localDE=0, &
                          & staggerLoc=ESMF_STAGGERLOC_CENTER, farrayPtr=lat,rc=status)

                     associate (u => cs_vector_patch(:,:,1), v => cs_vector_patch(:,:,2))
                       where (u == missing)
                          cs_uvw(:,:,1) = missing
                          cs_uvw(:,:,2) = missing
                          cs_uvw(:,:,3) = missing
                       elsewhere
                          cs_uvw(:,:,1) = - u * sind(lon) - v * sind(lat)*cosd(lon)
                          cs_uvw(:,:,2) = + u * cosd(lon) - v * sind(lat)*sind(lon)
                          cs_uvw(:,:,3) = + v * cosd(lat)
                      end where
                     end associate


                     do d = 1, 3
                        if (associated(missing)) then
                           call regrid(srcField=cs_uvw_field(d), dstField=ll_uvw_field(d), missing=missing, rc=status)
                           _VERIFY(status)
                        else
                           call regrid(srcField=cs_uvw_field(d), dstField=ll_uvw_field(d), rc=status)
                           _VERIFY(status)
                        end if
                     end do

                     call ESMF_GridGetCoord(this%grid_lat_lon, coordDim=1, localDE=0, &
                          & staggerLoc=ESMF_STAGGERLOC_CENTER, farrayPtr=lon,rc=status)
                     call ESMF_GridGetCoord(this%grid_lat_lon, coordDim=2, localDE=0, &
                          & staggerLoc=ESMF_STAGGERLOC_CENTER, farrayPtr=lat,rc=status)

                     associate (u => ll_vector_patch(:,:,1), v => ll_vector_patch(:,:,2))
                       where (ll_uvw(:,:,1) == missing)
                          u = missing
                          v = missing
                       elsewhere
                          u = -ll_uvw(:,:,1) * sind(lon) + ll_uvw(:,:,2) * cosd(lon)
                          v = -ll_uvw(:,:,1) * sind(lat)*cosd(lon) - ll_uvw(:,:,2) * sind(lat)*sind(lon) +  ll_uvw(:,:,3)*cosd(lat)
                       end where
                     end associate

                     call ll_fmtr%put_var(var_name, ll_vector_patch(:,:,1), start=ll_start, count=ll_count, rc=status)
                     _VERIFY(status)
                     call ll_fmtr%put_var(north_component, ll_vector_patch(:,:,2), start=ll_start, count=ll_count, rc=status)
                     _VERIFY(status)

                  else ! scalar
                     call cs_fmtr%get_var(var_name, cs_scalar_patch, start=cs_start,count=cs_count, rc=status)
                     _VERIFY(status)
                     if (associated(missing)) then
                        call regrid(srcField=cs_scalar_field, dstField=ll_scalar_field, missing=missing, rc=status)
                        _VERIFY(status)
                     else
                        call regrid(srcField=cs_scalar_field, dstField=ll_scalar_field, rc=status)
                        _VERIFY(status)
                     end if
                     call ll_fmtr%put_var(var_name, ll_scalar_patch, start=ll_start, count=ll_count, rc=status)
                     _VERIFY(status)

                  end if

               end do
            end do
         end select
         call var_iter%next()
      end do


      call ll_fmtr%close()
      call cs_fmtr%close()

      end associate

   end subroutine write_data


   subroutine write_metadata(this, rc)
      class (RegridSupport), intent(inout) :: this
      integer, optional, intent(out) :: rc
      type (ESMF_VM) :: vm_global
      integer :: status
      
      include 'mpif.h'

!$$      if (local_pet == 0) then
         call this%formatter_lat_lon%create_par(this%out_file, comm=MPI_COMM_WORLD, rc=status)
         _VERIFY(status)
         call this%formatter_lat_lon%write(this%cfio_lat_lon, rc=status)
         _VERIFY(status)
         call this%formatter_lat_lon%close(rc=status)
         _VERIFY(status)
!$$       end if

      call ESMF_VMGetGlobal(vm_global, rc=status)
      _VERIFY(status)
      call ESMF_VMBarrier(vm_global, rc=status)
      _VERIFY(status)

   end subroutine write_metadata


   ! Trig functions to work with angles in degrees.
   ! Current ESMF CS grid is hardwired to produce coordinates
   ! in degrees.  (Good and bad.)
   elemental function sind(x) result(s)
      real(kind=REAL64), intent(in) :: x
      real(kind=REAL64) :: s

      s = sin(x * MAPL_DEGREES_TO_RADIANS)
      
   end function sind

   elemental function cosd(x) result(c)
      real(kind=REAL64), intent(in) :: x
      real(kind=REAL64) :: c

      c = cos(x * MAPL_DEGREES_TO_RADIANS)
      
   end function cosd

   subroutine create_esmf_grids(this, rc)
      class (RegridSupport), intent(inout) :: this
      integer, optional, intent(out) :: rc
      integer :: status


      call this%create_cubed_sphere_grid(rc=status)
      _VERIFY(status)
      call this%create_lat_lon_grid(rc=status)
      _VERIFY(status)

      _RETURN(_SUCCESS)


   end subroutine create_esmf_grids


   subroutine create_cubed_sphere_grid(this, rc)
      class (RegridSupport), intent(inout) :: this
      integer, optional, intent(out) :: rc
      type (ESMF_DistGrid):: distgrid

      integer :: deToTileMap(0:pet_count-1)
      integer :: minIndex(2,0:pet_count-1), maxIndex(2,0:pet_count-1)
      integer :: ijms(2,6),NX,NY,i,lcnts(2)
      integer :: nPetPerTile
      integer :: status

      this%Xdim = this%cfio_cubed_sphere%get_dimension('Xdim')
      this%LM = this%cfio_cubed_sphere%get_dimension('lev',rc=status)
      ! ignore status
      if (status /= pFIO_SUCCESS) then
        this%LM = 1
        status = pFIO_SUCCESS
      end if

      nPetPerTile = pet_count/n_tiles
      nx = nint(sqrt(float(nPetPerTile*this%Xdim)/this%Xdim))
      nx = max(nx,1)
      do while( mod(nPetPerTile,nx).NE.0) 
         nx = nx - 1
      enddo
      ny=nPetPerTile/nx
      ijms(1,1)=NX
      ijms(2,1)=NY
      do i=2,6
         ijms(:,i)=ijms(:,1)
      enddo

      this%grid_cubed_sphere = ESMF_GridCreateCubedSphere(this%Xdim, regDecompPTile=ijms, rc=status)
      _VERIFY(status)

      call ESMF_GridGet(this%grid_cubed_sphere, distgrid=distgrid,rc=status)
      _VERIFY(status)
      call ESMF_DistGridGet(distgrid, deToTileMap=deToTileMap, rc=status)
      _VERIFY(status)
      call MAPL_DistGridGet(distgrid, MaxIndex=maxIndex, MinIndex=minIndex, rc=status)
      _VERIFY(status)
      this%my_tile = deToTileMap(local_pet)

      call ESMF_GridGet(this%grid_cubed_sphere,localDE=0,staggerloc=ESMF_STAGGERLOC_CENTER, &
           exclusiveCount=lcnts,rc=status)
      _VERIFY(status)
      call ESMF_GridAddItem(this%grid_cubed_sphere,itemflag=ESMF_GRIDITEM_MASK,staggerloc=ESMF_STAGGERLOC_CENTER, &
           & itemTypeKind=ESMF_TYPEKIND_I4, rc=status)
      _VERIFY(status)
      this%nx_loc=lcnts(1)
      this%ny_loc=lcnts(2)
      select case (this%my_tile)
         case(1)
            this%x_1=minIndex(1,local_pet)
            this%x_n=maxIndex(1,local_pet)
            this%y_1=minIndex(2,local_pet)
            this%y_n=maxIndex(2,local_pet)
         case(2)
            this%x_1=minIndex(1,local_pet) - this%Xdim 
            this%x_n=maxIndex(1,local_pet) - this%Xdim
            this%y_1=minIndex(2,local_pet)
            this%y_n=maxIndex(2,local_pet)
         case(3)
            this%x_1=minIndex(1,local_pet) - this%Xdim 
            this%x_n=maxIndex(1,local_pet) - this%Xdim
            this%y_1=minIndex(2,local_pet) - this%Xdim
            this%y_n=maxIndex(2,local_pet) - this%Xdim
         case(4)
            this%x_1=minIndex(1,local_pet) - 2*this%Xdim
            this%x_n=maxIndex(1,local_pet) - 2*this%Xdim
            this%y_1=minIndex(2,local_pet) - this%Xdim
            this%y_n=maxIndex(2,local_pet) - this%Xdim
         case(5)
            this%x_1=minIndex(1,local_pet) - 2*this%Xdim
            this%x_n=maxIndex(1,local_pet) - 2*this%Xdim
            this%y_1=minIndex(2,local_pet) - 2*this%Xdim
            this%y_n=maxIndex(2,local_pet) - 2*this%Xdim
         case(6)
            this%x_1=minIndex(1,local_pet) - 3*this%Xdim
            this%x_n=maxIndex(1,local_pet) - 3*this%Xdim
            this%y_1=minIndex(2,local_pet) - 2*this%Xdim
            this%y_n=maxIndex(2,local_pet) - 2*this%Xdim
      end select

      _RETURN(_SUCCESS)
   end subroutine create_cubed_sphere_grid



   subroutine create_lat_lon_grid(this, rc)
      class (RegridSupport), intent(inout) :: this
      integer, optional, intent(out) :: rc

      real(kind=ESMF_KIND_R8), pointer :: centers(:,:)

      integer :: status
      integer :: i, j

      integer, allocatable :: jms(:)
      integer, allocatable :: ims(:)
      integer :: np

      np = floor(sqrt(real(pet_count)))
      do npx = np, 1, -1
         ! will always be true in the final iteration
         if (mod(pet_count, npx) == 0) exit
      end do
      npy = pet_count / npx

!$$      npx = 2
!$$      npy = pet_count/2
      npx = 1
      npy = pet_count

      if (local_pet == 0) print*,'Topology: ', npx, npy
      ims = [((((j+1)*this%IM)/npx) - ((j*this%IM)/npx),j=0,npx-1)]
      jms = [((((j+1)*this%JM)/npy) - ((j*this%JM)/npy),j=0,npy-1)]

      this%grid_lat_lon = ESMF_GridCreate( &
           & countsPerDEDim1 = ims, &
           & countsPerDEDim2 = jms, &
           & indexFlag=ESMF_INDEX_DELOCAL, &
           & gridEdgeLWidth=[0,0], &
           & gridEdgeUWidth=[0,0], &
           & coordDep1=[1,2], &
           & coordDep2=[1,2], &
           & rc=status &
           & )

      call ESMF_GridAddCoord(this%grid_lat_lon, rc=status)
      _VERIFY(status)

      px = mod(local_pet, npx)
      py = local_pet / npx

      this%i_1 = 1 + (px*this%IM)/npx
      this%i_n = ((px+1)*this%IM)/npx
      this%j_1 = 1 + (py*this%JM)/npy
      this%j_n = ((py+1)*this%JM)/npy

      call ESMF_GridGetCoord(this%grid_lat_lon, coordDim=1, localDE=0, &
           staggerloc=ESMF_STAGGERLOC_CENTER, &
           farrayPtr=centers, rc=status)
      _VERIFY(status)

      this%longitudes = MAPL_RANGE(-180.d0, 180.d0 - (360.d0/this%IM), this%IM, rc=status)
      _VERIFY(status)

      centers(:,1) = this%longitudes(this%i_1:this%i_n)
      do j = 2, size(centers,2)
        centers(:,j) = centers(:,1)
      end do

      ! Now latitudes
      call ESMF_GridGetCoord(this%grid_lat_lon, coordDim=2, localDE=0, &
           staggerloc=ESMF_STAGGERLOC_CENTER, &
           farrayPtr=centers, rc=status)
      _VERIFY(status)

      this%latitudes = MAPL_RANGE(-90.d0, +90.d0, this%JM, rc=status)
      _VERIFY(status)

      centers(1,:) = this%latitudes(this%j_1:this%j_n)
      do i = 2, size(centers,1)
        centers(i,:) = centers(1,:)
      end do

      _RETURN(_SUCCESS)
   end subroutine create_lat_lon_grid



end module SupportMod


! Macros for main program should STOP.  Macros for module above
! throw an exception and RETURN.

! The main program.   Misleadingly simple.
#undef MAPL_ErrLog_DONE
#define I_AM_MAIN
#include "MAPL_Generic.h"
program main
   use ESMF
   use SupportMod
   use pFIO
   implicit none

   integer :: c00, c0, c1, crate

   integer :: status
   type (RegridSupport) :: regridder

   call ESMF_Initialize(logKindFlag=ESMF_LOGKIND_NONE)

   call check_resources(rc=status)
   _VERIFY(status)

   call regridder%process_command_line(rc=status)
   _VERIFY(status)

   call system_clock(c0,crate)
   c00 = c0
   call regridder%read_metadata()
   call system_clock(c1)
   if (local_pet == 0) then
     print*,'read_metadata()', real(c1-c0)/crate
   end if

   call system_clock(c0)
   call regridder%create_esmf_grids()
   call system_clock(c1)
   if (local_pet == 0) then
     print*,'create_esmf_grids()', real(c1-c0)/crate
   end if

   call system_clock(c0)
   call regridder%transfer_metadata()
   call system_clock(c1)
   if (local_pet == 0) then
     print*,'transfer_metadata()', real(c1-c0)/crate
   end if

   call system_clock(c0)
   call regridder%write_metadata() ! to out_file
   call system_clock(c1)
   if (local_pet == 0) then
     print*,'write_metadata()', real(c1-c0)/crate
   end if


   call system_clock(c0)
   call regridder%write_data(__RC__)
   call system_clock(c1)
   if (local_pet == 0) then
     print*,'write_data()', real(c1-c0)/crate
     print*,'total: ', real(c1-c00)/crate
     print*,'num regridders = ',  1 + route_handles%size()

   end if
   
   call ESMF_finalize()

contains



   subroutine check_resources(rc)
      use SupportMod
      integer, optional, intent(out) :: rc
      integer:: status

      type (ESMF_VM) :: vm_global

      call ESMF_VMGetGlobal(vm_global, rc=status)
      call ESMF_VMGet(vm_global, petcount=pet_count, localpet=local_pet, rc=status)

      if (mod(pet_count,N_TILES) /= 0) then ! should just require a multiple, but ...
         rc = -1
         if (local_pet == 0) then
            write(*,*)'The number of mpi processes must be a multile of 6.'
         end if
         _RETURN(_FAILURE)
      end if

      _RETURN(_SUCCESS)

   end subroutine check_resources


end program
