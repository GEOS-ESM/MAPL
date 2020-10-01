!usage
!mpirun -np 8 ./pfio_collective_demo.x -nc 4 -nsi 2 -nso 2  -f1 xxx1.nc4 -f2 xxx2.nc4 -v T -s mpi
!The variable should be 4d with lavel>=20
#include "MAPL_ErrLog.h"
#include "unused_dummy.H"

module io_demo_CLI
   use MAPL_ExceptionHandling
   use pFIO_StringVectorMod
   implicit none
   private

   public :: CommandLineOptions
   public :: process_command_line
   
   type CommandLineOptions
      character(len=:), allocatable :: file_1, file_2
      type (StringVector) :: requested_variables

      integer :: npes_client
      integer :: npes_iserver
      integer :: npes_oserver
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
            _ASSERT(buffer /= '-')
            read(buffer,*) options%npes_client
         case ('-nsi', '--npes_iserver')
            buffer = get_next_argument()
            _ASSERT(buffer /= '-')
            read(buffer,*) options%npes_iserver
         case ('-nso', '--npes_oserver')
            buffer = get_next_argument()
            _ASSERT(buffer /= '-')
            read(buffer,*) options%npes_oserver
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
   

end module io_demo_CLI

module FakeExtDataMod
   use io_demo_CLI
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
      type (ClientThread) :: i_c
      type (ClientThread) :: o_c
      integer, allocatable :: hist_collection_ids(:)

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
      class (FakeExtData),target, intent(inout) :: this
      type (CommandLineOptions), intent(in) :: options
      integer, intent(in) :: comm
      class (AbstractDirectoryService), target,intent(inout) :: d_s

      integer :: ierror
      type (FileMetadata) :: file_metadata
      type (NetCDF4_FileFormatter) :: formatter
      type (StringIntegerMap) :: dims

      this%i_c = ClientThread()
      call d_s%connect_to_server('i_server', this%i_c, comm)

      this%o_c = ClientThread()
      call d_s%connect_to_server('o_server', this%o_c, comm)


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
      
      allocate(this%hist_collection_ids(10))
      
   end subroutine init

   subroutine run(this, step)
      class (FakeExtData), target, intent(inout) :: this
      integer, intent(in) :: step
      
      type(ArrayReference) :: ref
      type(FileMetadata) :: fmd,fmd2
      Type(Variable) :: T1,T2

      integer :: i_var,i
      integer :: lat0, lat1, nlats
      integer :: collection_id, file_md_id
      character(len=3) :: tmp
      integer :: c1,c2,num_request
      integer,allocatable :: pull_ids(:,:)
      integer,allocatable :: push_ids(:,:)

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
         write(tmp,'(I3.3)') i
         collection_id = this%i_c%add_ext_collection('collection-i'//tmp)
         collection_id = this%o_c%add_ext_collection('collection-o'//tmp)
      enddo
      call system_clock(c2)

      allocate(pull_ids(this%vars%size(),num_request))      
      allocate(push_ids(this%vars%size(),num_request))      

      select case (step)
      case (1) ! read 1st file; prefetch 2nd
       
        ! call system_clock(c1)
         do i_var = 1, this%vars%size()
            allocate(this%bundle(i_var)%x(this%nlon,lat0:lat1,1,1))
            this%bundle(i_var)%x = -1
            ref = ArrayReference(this%bundle(i_var)%x)

            do i =1, num_request
               pull_ids(i_var,i) = &
                 & this%i_c%collective_prefetch_data(collection_id, this%file_1, this%vars%at(i_var), ref,&
                 & start=[1,lat0,20,1], &
                 & global_start=[1,1,20,1],global_count=[this%nlon,this%nlat,1,1])
            enddo

         end do
         !call system_clock(c2)
         call this%i_c%done_collective_prefetch()

         do i_var = 1, this%vars%size()
            do i = 1, num_request
              call this%i_c%wait(pull_ids(i_var,i))
            enddo
         end do
         
         do i_var = 1, this%vars%size()
            this%bundle(i_var)%x = -1
            ref = ArrayReference(this%bundle(i_var)%x)
            this%bundle(i_var)%request_id = &
                 & this%i_c%collective_prefetch_data(collection_id, this%file_1, this%vars%at(i_var), ref,&
                 & start=[1,lat0,20,1], &
                 & global_start=[1,1,20,1],global_count=[this%nlon,this%nlat,1,1])
         end do
         call this%i_c%done_collective_prefetch()


      case (2) ! wait for 2nd file to complete

         do i_var = 1, this%vars%size()
            call this%i_c%wait(this%bundle(i_var)%request_id)
         end do

!!!!!!!!!!!!!!!!!
! individual write
         call fmd%add_dimension('lon',this%nlon)
         call fmd%add_dimension('lat',nlats)
         call fmd%add_dimension('level',1)
         call fmd%add_dimension('time',1)
         T1 = Variable(type=pFIO_REAL32, dimensions='lon,lat,level,time')
         call fmd%add_variable('T',T1)
         this%hist_collection_ids(1) = this%o_c%add_hist_collection(fmd)

         call fmd2%add_dimension('lon',this%nlon)
         call fmd2%add_dimension('lat',this%nlat)
         call fmd2%add_dimension('level',1)
         call fmd2%add_dimension('time',1)
         T2 = Variable(type=pFIO_REAL32, dimensions='lon,lat,level,time')
         call fmd2%add_variable('T',T2)
         this%hist_collection_ids(2) = this%o_c%add_hist_collection(fmd2)

         file_md_id = this%hist_collection_ids(1) 
         write(tmp,'(I3.3)') this%rank

         do i_var = 1, this%vars%size()

            ref = ArrayReference(this%bundle(i_var)%x)

            do i =1, 1
               push_ids(i_var,i) = &
                 & this%o_c%stage_data(file_md_id,trim(this%file_1)//'.rank_'//tmp//'.nc4', this%vars%at(i_var), ref,&
                 & start=[1,lat0,20,1])

            enddo

         end do
         !call system_clock(c2)
         call this%o_c%done_stage()
         do i_var = 1, this%vars%size()
            do i = 1, 1
              call this%o_c%wait(push_ids(i_var,i))
            enddo
         end do

! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         ! writing
         file_md_id = this%hist_collection_ids(2) 

         do i_var = 1, this%vars%size()

            ref = ArrayReference(this%bundle(i_var)%x)
            do i =1, 1
               push_ids(i_var,i) = &
                 & this%o_c%collective_stage_data(file_md_id, trim(this%file_1)//'.new.nc4', this%vars%at(i_var), ref,&
                 & start=[1,lat0,1,1], &
                 & global_start=[1,1,1,1],global_count=[this%nlon,this%nlat,1,1])

            enddo

         end do
         !call system_clock(c2)
         call this%o_c%done_collective_stage()
         do i_var = 1, this%vars%size()
            do i = 1, 1
              call this%o_c%wait(push_ids(i_var,i))
            enddo
         end do

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!1         
      end select
      
   end subroutine run


   subroutine finalize(this)
      class (FakeExtData), intent(inout) :: this
      integer :: ierror
      deallocate(this%bundle)
      call Mpi_Barrier(this%comm,ierror)
      print*,"iclient sent terminate signal" 
      call this%i_c%terminate()
      call Mpi_Barrier(this%comm,ierror)
      print*,"oclient sent terminate signal" 
      call this%o_c%terminate()
   end subroutine finalize

end module FakeExtDataMod

program main
   use, intrinsic :: iso_fortran_env, only: REAL32
   use mpi
   use pFIO
   use io_demo_CLI
   use FakeExtDataMod
   use MAPL_ExceptionHandling
   implicit none

   integer :: rank, npes, ierror, provided,required
   integer :: status, color, key

   class(AbstractServer),pointer :: iserver,oserver
   class(AbstractDirectoryService), pointer :: d_s => null()

   type (CommandLineOptions) :: options
   integer, parameter :: NO_COLOR     = 0
   integer, parameter :: iSERVER_COLOR = 1
   integer, parameter :: oSERVER_COLOR = 4
   integer, parameter :: CLIENT_COLOR  = 2
   integer, parameter :: BOTH_COLOR    = 3

   integer :: comm,num_threads
   type (FakeExtData), target :: extData


   required = MPI_THREAD_MULTIPLE
   call MPI_init_thread(required, provided, ierror)
   call MPI_Comm_rank(MPI_COMM_WORLD, rank, ierror)
   call MPI_Comm_size(MPI_COMM_WORLD, npes, ierror)

   call process_command_line(options, rc=status)

!! sanity check

   if(options%server_type == 'openmp') then
     if (required > provided) stop "provided thread is not enough for openmp"
     num_threads = 10
     call omp_set_num_threads(num_threads) 
   endif

   d_s => get_directory_service(options%server_type)

   color = split_color(options%server_type,options%npes_iserver,options%npes_oserver)
   key = 0

   call MPI_Comm_split(MPI_COMM_WORLD, color, key, comm, ierror)

   if (color == iSERVER_COLOR .or. color == BOTH_COLOR ) then ! i_server
      
      iserver=>get_server(options%server_type,comm, d_s,'i_server')
      print*,"start i_server"
      if (color == iSERVER_COLOR ) call iserver%start()

   endif

   if (color == oSERVER_COLOR .or. color == BOTH_COLOR ) then ! o_server
      
      oserver=>get_server(options%server_type,comm,d_s,'o_server')

      print*,"start o_server"
      if (color == oSERVER_COLOR ) call oserver%start()

   endif

   if (color == CLIENT_COLOR .or. color == BOTH_COLOR) then ! client

      call extData%init(options, comm, d_s)
      call extData%run(step=1)
      call extData%run(step=2)
      call extData%finalize()
 
   end if

   call Mpi_Barrier(MPI_COMM_WORLD,ierror)

   call MPI_finalize(ierror)

contains

  function get_directory_service(stype) result(d_s)
      character(*),intent(in) :: stype
      class(AbstractDirectoryService),pointer :: d_s

      allocate(d_s, source = DirectoryService(MPI_COMM_WORLD))

   end function

   function split_color(stype,split_irank,split_orank) result(color)
      character(*),intent(in) :: stype
      integer,intent(in) :: split_irank  
      integer,intent(in) :: split_orank  
      integer :: color

      select case (stype)
      case ('openmp','mpi')
         if (rank < split_irank) then
            color = iSERVER_COLOR
         elseif (rank < split_orank+split_irank ) then
            color = oSERVER_COLOR
         else 
            color = CLIENT_COLOR
         end if
      case ('simple')
         color = BOTH_COLOR
      case default
         stop "not known server type"
      end select 

   end function

   function get_server(stype,comm,d_s,port_name) result(server)
      character(*),intent(in) :: stype
      integer,intent(in) :: comm
      class (AbstractDirectoryService), target,intent(inout) :: d_s 
      character(*), intent(in) :: port_name

      class(BaseServer),pointer :: server

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
!         call d_s%connect_to_client(port_name, server)
         print*,"using simple server"
      end select

    end function  
       
end program main
