!usage
!mpirun -np 8 ./pfio_collective_demo.x -nc 6 -ns 2 -f1 xxx1.nc4 -f2 xxx2.nc4 -v T -s mpi
!The variable should be 4d with lavel>=20
#include "MAPL_ErrLog.h"
#include "unused_dummy.H"

module collective_demo_CLI
   use MAPL_ExceptionHandling
   use gFTL_StringVector
   implicit none
   private

   public :: CommandLineOptions
   public :: process_command_line
   
   type CommandLineOptions
      character(len=:), allocatable :: file_1, file_2
      type (StringVector) :: requested_variables

      integer :: npes_client
      integer :: npes_server
      logical :: debug
      character(len=:),allocatable :: server_type ! 'mpi' or 'openmp'
   end type CommandLineOptions


contains

   ! The following procedure parses the command line to find various
   ! arguments for file names, target grid resolution, etc.
   subroutine process_command_line(options, rc)
      type (CommandLineOptions), intent(inout) :: options
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
         case ('-nc', '--npes_client')
            buffer = get_next_argument()
            _ASSERT(buffer /= '-', "no -")
            read(buffer,*) options%npes_client
         case ('-ns', '--npes_server')
            buffer = get_next_argument()
            _ASSERT(buffer /= '-', "no -")
            read(buffer,*) options%npes_server
         case ('-f1', '--file_1')
            options%file_1 = get_next_argument()
            _ASSERT(options%file_1(1:1) /= '-', "too many ")
         case ('-f2', '--file_2')
            options%file_2 = get_next_argument()
            _ASSERT(options%file_2(1:1) /= '-', "too many -")
         case ('-v', '--var')
            buffer = get_next_argument()
            _ASSERT(buffer(1:1) /= '-', "too many -")
            options%requested_variables = parse_vars(buffer)
         case ('-s', '--server_type')
            options%server_type = get_next_argument()
            _ASSERT(options%server_type /= '-', "too many -")
         case ('-d', '--debug')
            options%debug = .true.
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
   

end module collective_demo_CLI

module FakeExtDataMod_collective
   use MAPL_ExceptionHandling
   use collective_demo_CLI
   use pFIO
   use gFTL_StringVector
   use, intrinsic :: iso_fortran_env, only: REAL32
   implicit none
   private

   public :: FakeExtData

   type FakeBundle
      real(kind=REAL32), allocatable :: x(:,:,:,:)
      integer :: request_id
   end type FakeBundle

   type FakeExtData
      type (ClientThread) :: c

      character(len=:), allocatable :: file_1
      character(len=:), allocatable :: file_2

      type (StringVector) :: vars
      type (FakeBundle), allocatable :: bundle(:)

      integer :: comm
      integer :: rank
      integer :: npes

      integer :: nlat
      integer :: nlon

   contains
      procedure :: init
      procedure :: run
      procedure :: finalize

   end type FakeExtData

contains
   

   subroutine init(this, options, comm, d_s, port_name)
      use gFTL_StringIntegerMap
      class (FakeExtData),target, intent(inout) :: this
      type (CommandLineOptions), intent(in) :: options
      integer, intent(in) :: comm
      class (AbstractDirectoryService), target,intent(inout) :: d_s
      character(*), intent(in) :: port_name

      integer :: ierror
      type (FileMetadata) :: file_metadata
      type (NetCDF4_FileFormatter) :: formatter
      type (StringIntegerMap) :: dims

      this%c = ClientThread()
      call d_s%connect_to_server(port_name, this%c, comm)

      this%file_1 = options%file_1
      this%file_2 = options%file_2
      this%vars = options%requested_variables
      !call this%vars%push_back('T')
      !call this%vars%push_back('U')
      !call this%vars%push_back('V')


      this%comm = comm
      call MPI_Comm_rank(comm,this%rank,ierror)
      call MPI_Comm_size(comm,this%npes,ierror)

      allocate(this%bundle(this%vars%size()))

      call formatter%open(this%file_1, pFIO_READ)
      file_metadata = formatter%read()
      call formatter%close()

      dims = file_metadata%get_dimensions()
      this%nlat = dims%at('lat')
      this%nlon = dims%at('lon')
      
   end subroutine init

   subroutine run(this, step)
      class (FakeExtData), target, intent(inout) :: this
      integer, intent(in) :: step
      
      type (ArrayReference) :: ref

      integer :: i_var,i
      integer :: lat0, lat1, nlats
      integer :: collection_id
      character(len=5) :: tmp
      integer :: c1,c2,num_request
      integer,allocatable :: request_ids(:,:)

      lat0 = 1 + (this%rank*this%nlat)/this%npes
      lat1 = (this%rank+1)*this%nlat/this%npes
      nlats = (lat1 - lat0 + 1)

      ! Establish the collection
      ! In a real use case the collection name would be the ExtData template.
      ! But the actual name does not matter - it is just used to identify
      ! a group of files that have identical metadata (except for time)
      !num_request = 1000
      num_request = 3
      call system_clock(c1)

      do i = 1,num_request
         tmp= ''
         write(tmp,'(I5.5)') i
         collection_id = this%c%add_ext_collection('collection-name'//tmp)
         !print*,"collection_id: ",collection_id
      enddo
      call system_clock(c2)
      !print*," step  1 : add_ext_collection"

      allocate(request_ids(this%vars%size(),num_request))      

      select case (step)
      case (1) ! read 1st file; prefetch 2nd
       
        ! call system_clock(c1)
         do i_var = 1, this%vars%size()
            allocate(this%bundle(i_var)%x(this%nlon,lat0:lat1,1,1))
            this%bundle(i_var)%x = -1
            ref = ArrayReference(this%bundle(i_var)%x)
            !this%bundle(i_var)%request_id = &
                 !& this%c%request_subset_data_reference(collection_id, this%file_1, this%vars%at(i_var), ref, start=[1,lat0,20,1])
            do i =1, num_request
            request_ids(i_var,i) = &
                 & this%c%collective_prefetch_data(collection_id, this%file_1, this%vars%at(i_var), ref,&
                 & start=[1,lat0,20,1], &
                 & global_start=[1,1,20,1],global_count=[this%nlon,this%nlat,1,1])
            enddo

         end do
         !call system_clock(c2)
        ! print*," step 1 send collective message"
         call this%c%done_collective_prefetch()
         !print*," step 1 first done"

         do i_var = 1, this%vars%size()
            do i = 1, num_request
              call this%c%wait(request_ids(i_var,i))
            enddo
         end do

         do i_var = 1, this%vars%size()
            this%bundle(i_var)%x = -1
            ref = ArrayReference(this%bundle(i_var)%x)
            this%bundle(i_var)%request_id = &
                 !& this%c%request_subset_data_reference(collection_id, this%file_2, this%vars%at(i_var), ref, start=[1,lat0,20,1])
                 & this%c%collective_prefetch_data(collection_id, this%file_1, this%vars%at(i_var), ref,&
                 & start=[1,lat0,20,1], &
                 & global_start=[1,1,20,1],global_count=[this%nlon,this%nlat,1,1])
         end do
         call this%c%done_collective_prefetch()

      case (2) ! wait for 2nd file to complete

         do i_var = 1, this%vars%size()
            call this%c%wait(this%bundle(i_var)%request_id)
         end do

      end select
      
   end subroutine run


   subroutine finalize(this)
      class (FakeExtData), intent(inout) :: this
      deallocate(this%bundle)
      print*,"client sent terminate signal" 
      call this%c%terminate()
   end subroutine finalize

end module FakeExtDataMod_collective

program main
   use, intrinsic :: iso_fortran_env, only: REAL32
   use mpi
   use pFIO
   use MAPL_ExceptionHandling
   use collective_demo_CLI
   use FakeExtDataMod_collective
   implicit none

   integer :: rank, npes, ierror, provided,required
   integer :: status, color, key
   class(AbstractServer),pointer :: server
   class(AbstractDirectoryService), pointer :: d_s => null()

   type (CommandLineOptions) :: options
   integer, parameter :: NO_COLOR     = 0
   integer, parameter :: SERVER_COLOR = 1
   integer, parameter :: CLIENT_COLOR = 2
   integer, parameter :: BOTH_COLOR   = 3

   integer :: comm,num_threads
   type (FakeExtData), target :: extData

   required = MPI_THREAD_MULTIPLE
   call MPI_init_thread(required, provided, ierror)
   call MPI_Comm_rank(MPI_COMM_WORLD, rank, ierror)
   call MPI_Comm_size(MPI_COMM_WORLD, npes, ierror)

   call process_command_line(options, rc=status)

!! sanity check

!$   if(options%server_type == 'openmp') then
!$     if (required > provided) stop "provided thread is not enough for openmp"
!$     num_threads = 10
!$     call omp_set_num_threads(num_threads) 
!$   endif

   d_s => get_directory_service(options%server_type)

   color = split_color(options%server_type,options%npes_server)
   key = 0

   call MPI_Comm_split(MPI_COMM_WORLD, color, key, comm, ierror)

   if (color == SERVER_COLOR .or. color == BOTH_COLOR) then ! server
      
      server=>get_server(options%server_type,comm,d_s,'i_server')
      if (color == SERVER_COLOR) call server%start()

   endif

   if (color == CLIENT_COLOR .or. color == BOTH_COLOR) then ! client

      call extData%init(options, comm, d_s, 'i_server')
      call extData%run(step=1)
      call extData%run(step=2)
      call extData%finalize()
 
   end if

   call MPI_finalize(ierror)

contains

   function get_directory_service(stype) result(d_s)
      character(*),intent(in) :: stype
      class(AbstractDirectoryService),pointer :: d_s

      allocate(d_s, source=DirectoryService(MPI_COMM_WORLD))

      _UNUSED_DUMMY(stype)

   end function

   function split_color(stype,split_rank) result(color)
      character(*),intent(in) :: stype
      integer,intent(in) :: split_rank  
      integer :: color

      select case (stype)
      case ('openmp','mpi')
         if (rank < split_rank) then
            color = SERVER_COLOR
         else
            color = CLIENT_COLOR
         end if
      case ('simple')
         color = BOTH_COLOR
      case default
         stop "not known server type"
      end select 

   end function

   function get_server(stype, comm, d_s, port_name) result(server)
      character(*),intent(in) :: stype
      integer,intent(in) :: comm
      class (AbstractDirectoryService), target, intent(inout) :: d_s
      character(*), intent(in) :: port_name

      class(BaseServer), pointer :: server

      select case (stype)
      case('mpi')
         allocate(server,source=MpiServer(comm, port_name))
         call d_s%publish(PortInfo(port_name, server),server)
         call d_s%connect_to_client(port_name, server)
         print*,"using MpiServer"
      case('openmp')
!!$        allocate(server,source=OpenmpServer(comm,d_s))
!!$        print*,"using OpenMpServer"
      case('simple')
         allocate(server,source=MpiServer(comm, port_name))
         call d_s%publish(PortInfo(port_name, server), server)
!!         call d_s%connect_to_client(port_name, server)
         print*,"using simple server"
      end select

     

    end function

end program main
