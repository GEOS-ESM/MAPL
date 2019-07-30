!usage
!mpirun -np 8 ./pfio_server_demo.x -nc 6 -ns 2 -f1 xxx1.nc4 -f2 xxx2.nc4 -v T -s mpi
!The variable should be 4d with lavel>=20

#define _SUCCESS      0
#define _FAILURE     1
#define _VERIFY(A)   if(  (A)/=0) then; call pFIO_throw_exception(__FILE__,__LINE__); return; endif
#define _ASSERT(A)   if(.not.(A)) then; if(present(rc)) rc=_FAILURE; call pFIO_throw_exception(__FILE__,__LINE__); return; endif
#define _RETURN(A)   if(present(rc)) rc=A; return
#include "unused_dummy.H"

module CLI
   use pFIO_ThrowMod
   use pFIO_StringVectorMod
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
      character(len=6) :: server_type ! 'mpi' or 'openmp'
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
            _ASSERT(buffer /= '-')
            read(buffer,*) options%npes_client
         case ('-ns', '--npes_server')
            buffer = get_next_argument()
            _ASSERT(buffer /= '-')
            read(buffer,*) options%npes_server
         case ('-f1', '--file_1')
            options%file_1 = get_next_argument()
            _ASSERT(options%file_1(1:1) /= '-')
         case ('-f2', '--file_2')
            options%file_2 = get_next_argument()
            _ASSERT(options%file_2(1:1) /= '-')
         case ('-v', '--var')
            buffer = get_next_argument()
            _ASSERT(buffer(1:1) /= '-')
            options%requested_variables = parse_vars(buffer)
         case ('-s', '--server_type')
            options%server_type = get_next_argument()
            _ASSERT(options%server_type /= '-')
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
   

end module CLI

module FakeExtDataMod
   use CLI
   use pFIO
   use pFIO_StringVectorMod
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
   

   subroutine init(this, options, comm, d_s)
      use pFIO_StringIntegerMapMod
      class (FakeExtData), intent(inout) :: this
      type (CommandLineOptions), intent(in) :: options
      integer, intent(in) :: comm
      class (AbstractDirectoryService), target,intent(inout) :: d_s

      integer :: ierror
      type (FileMetadata) :: file_metadata
      type (NetCDF4_FileFormatter) :: formatter
      type (StringIntegerMap) :: dims

      this%c = ClientThread()
      call d_s%connect_to_server('i_server', this%c, comm)

      this%file_1 = options%file_1
      this%file_2 = options%file_2
      this%vars = options%requested_variables

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
      character(len=4) :: tmp    
 
      lat0 = 1 + (this%rank*this%nlat)/this%npes
      lat1 = (this%rank+1)*this%nlat/this%npes
      nlats = (lat1 - lat0 + 1)

      ! Establish the collection
      ! In a real use case the collection name would be the ExtData template.
      ! But the actual name does not matter - it is just used to identify
      ! a group of files that have identical metadata (except for time)
      !do i = 1,9999
      !   tmp= ''
      !   write(tmp,'(I4.4)') i
      !collection_id = this%c%add_ext_collection('collection-name'//tmp)
      !enddo
      collection_id = this%c%add_ext_collection('collection-name')

      select case (step)
      case (1) ! read 1st file; prefetch 2nd

         do i_var = 1, this%vars%size()
            allocate(this%bundle(i_var)%x(this%nlon,lat0:lat1,1,1))
            this%bundle(i_var)%x = -1
            ref = ArrayReference(this%bundle(i_var)%x)
            this%bundle(i_var)%request_id = &
                 & this%c%prefetch_data(collection_id, this%file_1, this%vars%at(i_var), ref, start=[1,lat0,20,1])
         end do
         call this%c%done()

         do i_var = 1, this%vars%size()
            call this%c%wait(this%bundle(i_var)%request_id)
         end do

         do i_var = 1, this%vars%size()
            this%bundle(i_var)%x = -1
            ref = ArrayReference(this%bundle(i_var)%x)
            this%bundle(i_var)%request_id = &
                 & this%c%prefetch_data(collection_id, this%file_2, this%vars%at(i_var), ref, start=[1,lat0,20,1])
         end do
         call this%c%done()

      case (2) ! wait for 2nd file to complete

         do i_var = 1, this%vars%size()
            call this%c%wait(this%bundle(i_var)%request_id)
         end do

      end select
      
   end subroutine run


   subroutine finalize(this)
      class (FakeExtData), intent(inout) :: this
      integer :: ierror
      deallocate(this%bundle)
      call this%c%terminate()
   end subroutine finalize

end module FakeExtDataMod

program main
   use, intrinsic :: iso_fortran_env, only: REAL32
   use mpi
   use pFIO
   use CLI
   use FakeExtDataMod
   use pFIO_ThrowMod
   implicit none

   integer :: rank, npes, ierror, provided
   integer :: status, color, key
   class(BaseServer),allocatable :: s


   type (CommandLineOptions) :: options
   integer, parameter :: SERVER_COLOR = 1
   integer, parameter :: CLIENT_COLOR = 2

   integer :: comm,num_threads
   type (FakeExtData), target :: extData
   class(AbstractDirectoryService), pointer :: d_s=>null()

   call MPI_init_thread(MPI_THREAD_MULTIPLE, provided, ierror)
   call MPI_Comm_rank(MPI_COMM_WORLD, rank, ierror)
   call MPI_Comm_size(MPI_COMM_WORLD, npes, ierror)

   call process_command_line(options, rc=status)


   if (rank < options%npes_server) then
      color = SERVER_COLOR
   else
      color = CLIENT_COLOR
   end if
   key = 0

   call MPI_Comm_split(MPI_COMM_WORLD, color, key, comm, ierror)

   num_threads = 20
   allocate(d_s, source = DirectoryService(MPI_COMM_WORLD))

   if (color == SERVER_COLOR) then
      if(trim(options%server_type) == 'mpi') then
         allocate(s, source=MpiServer(comm, 'i_server'))
         call d_s%publish(PortInfo('i_server', s),s)
         call d_s%connect_to_client('i_server', s)
         print*, "using MpiServer"
      else if(trim(options%server_type) == 'openmp') then
!!$         call omp_set_num_threads(num_threads)
!!$         allocate(s, source=OpenMPServer(comm,d_s))
!!$         print*, "using OpenMPServer"
      else
         print*, options%server_type // '  not implemented'
         stop
      endif    
      call s%start()
   else ! client
      call extData%init(options, comm, d_s)
      call extData%run(step=1)
      call extData%run(step=2)
      call extData%finalize()

      !print*,"terminate_servers"
      !call global_directory_service%terminate_servers(comm)
   end if

   call MPI_finalize(ierror)

end program main
   
