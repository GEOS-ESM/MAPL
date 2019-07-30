!usage
!mpirun -np 8 ./pfio_collective_demo.x -nc 4 -nsi 2 -nso 2  -f1 xxx1.nc4 -f2 xxx2.nc4 -v T -s mpi
!The variable should be 4d with lavel>=20
#include "pFIO_ErrLog.h"
#include "unused_dummy.H"

module CLI
   use pFIO_ErrorHandlingMod
   use pFIO_ThrowMod
   use pFIO
   implicit none
   private

   public :: CommandLineOptions
   public :: process_command_line
   public :: DirectoryServicePointer
 
   type CommandLineOptions
      character(len=:), allocatable :: file_1, file_2
      type (StringVector) :: requested_variables

      integer :: npes_client
      integer :: npes_iserver
      integer :: npes_oserver

      integer :: N_ig ! number of isever group
      integer :: N_og ! nuber of  osever group

      logical :: debug
      character(len=:),allocatable :: server_type ! 'mpi' or 'openmp'
   end type CommandLineOptions

   type DirectoryServicePointer
     class(AbstractDirectoryService),pointer :: dsPtr=>null()
   end type

   integer, public :: c1,c2,c3,c4,c0

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
            _ASSERT(buffer /= '-', "no extrea - ")
            read(buffer,*) options%npes_client
         case ('-nsi', '--npes_iserver')
            buffer = get_next_argument()
            _ASSERT(buffer /= '-', "no extrea - ")
            read(buffer,*) options%npes_iserver
         case ('-ngi', '--ng_iserver')
            buffer = get_next_argument()
            _ASSERT(buffer /= '-', "no extrea - ")
            read(buffer,*) options%N_ig
         case ('-nso', '--npes_oserver')
            buffer = get_next_argument()
            _ASSERT(buffer /= '-', "no extrea - ")
            read(buffer,*) options%npes_oserver
         case ('-ngo', '--ng_oserver')
            buffer = get_next_argument()
            _ASSERT(buffer /= '-', "no extrea - ")
            read(buffer,*) options%N_og
         case ('-f1', '--file_1')
            options%file_1 = get_next_argument()
            _ASSERT(options%file_1(1:1) /= '-', "no extrea - ")
         case ('-f2', '--file_2')
            options%file_2 = get_next_argument()
            _ASSERT(options%file_2(1:1) /= '-', "no extrea - ")
         case ('-v', '--var')
            buffer = get_next_argument()
            _ASSERT(buffer(1:1) /= '-', "no extrea - ")
            options%requested_variables = parse_vars(buffer)
         case ('-s', '--server_type')
            options%server_type = get_next_argument()
            _ASSERT(options%server_type /= '-', "no extrea - ")
         case ('-d', '--debug')
            options%debug = .true.
         case default
            ! ignore
         end select

      end do
      _RETURN(_SUCCESS)
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

module FakeHistDataMod
   use pFIO_ErrorHandlingMod
   use CLI
   use pFIO
   use, intrinsic :: iso_c_binding, only: c_f_pointer, c_loc
   use, intrinsic :: iso_fortran_env, only: REAL32
   implicit none
   private

   public :: FakeHistData

   type FakeBundle
      real(kind=REAL32), allocatable :: x(:,:,:,:,:)
      integer :: request_id
   end type FakeBundle

   type FakeHistData
      type (ClientThread) :: i_c
      type (ClientThread) :: o_c
      type (ClientThreadVector) :: ic_vec
      type (ClientThreadVector) :: oc_vec

      integer, allocatable :: hist_collection_ids(:)

      character(len=:), allocatable :: file_1
      character(len=:), allocatable :: file_2

      type (StringVector) :: vars
      type (FakeBundle), allocatable :: bundle(:)

      integer :: comm
      integer,allocatable :: comms(:)
      integer :: rank
      integer :: npes

      integer :: Xdim
      integer :: Ydim
      integer :: nf
      integer :: lev
      integer :: time

   contains
      procedure :: init
      procedure :: run
      procedure :: finalize

   end type FakeHistData

contains
   

   subroutine init(this, options, comms,app_ds, N_iclient_g, N_oclient_g, rc)
      use pFIO_StringIntegerMapMod
      class (FakeHistData),target, intent(inout) :: this
      type (CommandLineOptions), intent(in) :: options
      integer, intent(in), dimension(:) :: comms
      class(AbstractDirectoryService), target,intent(inout) :: app_ds
      integer, intent(in) :: N_iclient_g, N_oclient_g
      integer, optional, intent(out) :: rc

      integer :: ierror,k, i,status
      type (FileMetadata) :: file_metadata
      type (NetCDF4_FileFormatter) :: formatter
      type (NetCDF4_FileFormatter) :: test_formatter
      type (FileMetadata) :: test_metadata
      real, target,allocatable :: testValues(:,:,:,:,:)
      real, pointer :: testPtr(:)

      type (StringIntegerMap) :: dims
      class(ClientThread), pointer :: threadPtr=>null()     

      this%ic_vec = ClientThreadVector()
      this%oc_vec = ClientThreadVector()

      do i = 1, N_iclient_g
         allocate(threadPtr, source = ClientThread())
         call app_ds%connect_to_server('iserver',threadPtr, comms(i), rc=status)
         _VERIFY(status)
         !call threadPtr%init_connection(app_ds(i)%dsPtr, comms(i),'i_server')
         call this%ic_vec%push_back(threadPtr)
         nullify(threadPtr)
      enddo

      do i = 1 + N_iclient_g, N_iclient_g + N_oclient_g
         allocate(threadPtr, source = ClientThread())
         call app_ds%connect_to_server('oserver',threadPtr, comms(i), rc=status)
         _VERIFY(status)
         !call threadPtr%init_connection(app_ds(i)%dsPtr, comms(i),'o_server')
         call this%oc_vec%push_back(threadPtr)
         nullify(threadPtr)
      enddo

      !this%i_c = ClientThread()
      !call this%i_c%init_connection(app_ds(1)%dsPtr,comms(1),'i_server')

      !this%o_c = ClientThread()
      !call this%o_c%init_connection(app_ds(2)%dsPtr,comms(2),'o_server')

      this%file_1 = options%file_1
      this%vars = options%requested_variables

      this%comm = comms(1)
      allocate(this%comms, source = comms)
      call MPI_Comm_rank(this%comms(1),this%rank,ierror)
      call MPI_Comm_size(this%comms(1),this%npes,ierror)

      allocate(this%bundle(this%vars%size()))
      allocate(this%hist_collection_ids(10))
  
      if ( this%rank == 0) then
         call test_metadata%add_dimension('Xdim',48)
         call test_metadata%add_dimension('Ydim',48)
         call test_metadata%add_dimension('nf',6)
         call test_metadata%add_dimension('lev',72)
         call test_metadata%add_dimension('time',1)
 
         do k = 1, this%vars%size()
            call test_metadata%add_variable(this%vars%at(k),Variable(pFIO_REAL32, dimensions='Xdim,Ydim,nf,lev,time'), rc=status)
            _VERIFY(status)
         enddo

         call test_formatter%create('test_in.nc4', rc=status)
         _VERIFY(status)
         call test_formatter%write(test_metadata, rc=status)
         _VERIFY(status)

         allocate(testValues(48,48,6,72,1))

         do k = 1, this%vars%size()
            do i = 1,6
               testValues(:,:,i,:,:) = 1.0*i*10**k
            enddo
            call c_f_pointer(c_loc(testValues), testPtr,[48*48*72*6*1])
            call test_formatter%put_var(this%vars%at(k),testPtr, start=[1,1,1,1,1], count = [48,48,6,72,1])

         enddo
         call test_formatter%close(rc=status)
      endif

      call Mpi_Barrier(this%comms(1),ierror)

      call formatter%open('test_in.nc4', pFIO_READ)
      file_metadata = formatter%read()
      call formatter%close()

      dims = file_metadata%get_dimensions()

      this%Xdim = dims%at('Xdim')
      this%Ydim = dims%at('Ydim')
      this%nf   = dims%at('nf')
      this%lev  = dims%at('lev')
      this%time = 1

   end subroutine init

   subroutine run(this, step, rc)
      class (FakeHistData), target, intent(inout) :: this
      integer, intent(in) :: step
      integer, optional, intent(out) :: rc

      type(ArrayReference) :: ref
      type(FileMetadata) :: fmd
      Type(Variable) :: T

      integer :: i_var,i,md_id
      integer :: collection_id, file_md_id, collection_num
      integer,allocatable :: prefetch_ids(:)
      integer,allocatable :: stage_ids(:,:)
      integer :: nx, nf, width, k, ith, jth, Xdim0, Xdim1, Ydim0, Ydim1
      integer :: status

      class(ClientThread), pointer :: icPtr=>null()     
      class(ClientThread), pointer :: ocPtr=>null()     


      i = mod(this%npes, 6)
      if(i /=0 ) then; print*, " make sure the number of reading cores  is multiple of 6"; stop 1;endif

      nx = nint(sqrt(this%npes/6.))
      
      if( nx*nx*6 /= this%npes ) then; print*, " make sure 6*M^2 cores to read"; stop 1; endif

      width = this%Xdim/nx

      nf  = this%rank/(nx*nx) + 1    ! nf th face

      k   = this%rank - (nf-1)*nx*nx ! rank within the face
      ith = k/nx
      jth = mod(k,nx)

      Xdim0 = 1 + ith*width
      Xdim1 = (ith+1)*width
      Ydim0 = 1 + jth*width
      Ydim1 = (jth+1)*width

      ! Establish the collection
      ! In a real use case the collection name would be the ExtData template.
      ! But the actual name does not matter - it is just used to identify
      ! a group of files that have identical metadata (except for time)
      !num_request = 1000

      ! get the input first

      icPtr => this%ic_vec%at(1)
      collection_id = icPtr%add_ext_collection('collection-i')
      !collection_id = this%i_c%add_ext_collection('collection-i')

      allocate(prefetch_ids(this%vars%size()))

      select case (step)
      case (1) ! read the file


         call system_clock(c0)
         do i_var = 1, this%vars%size()
            allocate(this%bundle(i_var)%x(Xdim0:Xdim1,Ydim0:Ydim1,nf:nf,this%lev,1))
            ref = ArrayReference(this%bundle(i_var)%x)
            prefetch_ids(i_var) = &
                 & icPtr%collective_prefetch_data(collection_id,'test_in.nc4', this%vars%at(i_var), ref,&
                 !& this%i_c%collective_prefetch_data(collection_id, this%file_1, this%vars%at(i_var), ref,&
                 & start=[Xdim0,Ydim0,nf,1,1], &
                 & global_start=[1,1,1,1,1],global_count=[this%Xdim,this%Ydim,this%nf, this%lev,1], rc=status)
            _VERIFY(status)

         end do
         call icPtr%done()
         !call this%i_c%done()

         do i_var = 1, this%vars%size()
            call icPtr%wait(prefetch_ids(i_var))
            !call this%i_c%wait(prefetch_ids(i_var))
         end do

      case (2) ! history out

         call system_clock(c1)
         call fmd%add_dimension('Xdim',this%Xdim)
         call fmd%add_dimension('Ydim',this%Ydim)
         call fmd%add_dimension('lev',this%lev)
         call fmd%add_dimension('nf',this%nf)
         call fmd%add_dimension('time',1)

         T = Variable(pFIO_REAL32, dimensions='Xdim,Ydim,nf,lev,time')

         do i_var = 1, this%vars%size() 
            call fmd%add_variable( this%vars%at(i_var),T, rc=status)
            _VERIFY(status)
         enddo

         ocPtr=> this%oc_vec%at(1)
         this%hist_collection_ids(1) = ocPtr%add_hist_collection(fmd)
         this%hist_collection_ids(2) = ocPtr%add_hist_collection(fmd)

         !this%hist_collection_ids(1) = this%o_c%add_hist_collection(fmd)
         collection_num = 2
         allocate(stage_ids(this%vars%size(),collection_num))

         do md_id = 1, collection_num
            ! writing
            file_md_id = this%hist_collection_ids(md_id) 
            do i_var = 1, this%vars%size()
               ref = ArrayReference(this%bundle(i_var)%x)
               stage_ids(i_var,md_id) = &
                 !& this%o_c%collective_stage_data(file_md_id, trim(this%file_1)//'.new.nc4', this%vars%at(i_var), ref,&
                 & ocPtr%collective_stage_data(file_md_id, 'test_out'//i_to_string(md_id)//'.nc4', this%vars%at(i_var), ref,&
                 & start=[Xdim0,Ydim0,nf,1, 1], &
                 & global_start=[1,1,1,1,1],global_count=[this%Xdim,this%Ydim, this%nf, this%lev,1], rc=status)
               _VERIFY(status)
            end do
         enddo

         call ocPtr%done()
         !call this%o_c%done()
         do md_id = 1, collection_num
            do i_var = 1, this%vars%size()
               call ocPtr%wait(stage_ids(i_var,md_id))
            enddo
         end do
         call system_clock(c2)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!1         
      end select
      _RETURN(_SUCCESS)      
   end subroutine run


   subroutine finalize(this)
      class (FakeHistData), intent(inout) :: this
      integer :: ierror
      class(ClientThread), pointer :: icPtr=>null()
      class(ClientThread), pointer :: ocPtr=>null()
      deallocate(this%bundle)
      call Mpi_Barrier(this%comms(1),ierror)
      print*,"iclient sent terminate signal" 
      icPtr=>this%ic_vec%at(1)
      call icPtr%terminate()
     ! call this%i_c%terminate()
      call Mpi_Barrier(this%comms(2),ierror)
      ocPtr=>this%oc_vec%at(1)
      call ocPtr%terminate()
      print*,"oclient sent terminate signal" 
      !call this%o_c%terminate()
   end subroutine finalize

end module FakeHistDataMod

program main
   use, intrinsic :: iso_fortran_env, only: REAL32
   use mpi
   use pFIO
   use CLI
   use pFIO_ErrorHandlingMod
   use FakeHistDataMod
   use pFIO_ThrowMod
   implicit none

   integer :: rank, npes, ierror, provided,required
   integer :: status, color, key

   class(BaseServer),allocatable :: iserver,oserver
   class(AbstractDirectoryService), allocatable, target :: directory_service

   type (CommandLineOptions) :: options
   integer, parameter :: NO_COLOR     = 0
   integer, parameter :: iSERVER_COLOR = 1
   integer, parameter :: oSERVER_COLOR = 4
   integer, parameter :: CLIENT_COLOR  = 2
   integer, parameter :: BOTH_COLOR    = 3

   integer :: comm,num_threads
   type (FakeHistData), target :: HistData

   integer :: my_comm_world, my_iComm, my_oComm, my_appcomm

   integer :: client_start, size_group,low_rank,up_rank
   integer :: local_rank, local_size, i,k, size_iclient, size_oclient
   integer :: app_start_rank, app_end_rank
   character(len = 20) :: out_file
   integer :: N_iclient_group, N_oclient_group,N_groups
   integer,allocatable :: local_comm_world(:), app_comms(:)
   integer :: md_id

   required = MPI_THREAD_MULTIPLE
   call MPI_init_thread(required, provided, ierror)
   call MPI_Comm_rank(MPI_COMM_WORLD, rank, ierror)
   call MPI_Comm_size(MPI_COMM_WORLD, npes, ierror)

   call process_command_line(options, rc=status)
   ! split comm_world to local_world

   N_iclient_group = options%N_ig
   N_oclient_group = options%N_og

   N_groups = N_iclient_group + N_oclient_group
 
   allocate(local_comm_world(N_groups))
   allocate(app_comms(N_groups))

   key = 0

   if (options%server_type == 'simple') then
      options%npes_iserver = npes
      options%npes_oserver = npes
      N_iclient_group = 1
      N_oclient_group = 1
      size_iclient = 0
      client_start = 0
      app_start_rank = 0
      app_end_rank = npes-1
   else if (options%server_type == 'mpi') then 
      size_iclient = N_iclient_group*options%npes_iserver
      size_oclient = N_oclient_group*options%npes_oserver
      client_start = npes - size_iclient-size_oclient
      app_start_rank = 0
      app_end_rank   = npes - size_iclient-size_oclient -1

  elseif(options%server_type == 'hybrid') then
      options%npes_iserver = npes - options%npes_oserver
      N_iclient_group = 1
      N_oclient_group = 1
      size_iclient =  N_iclient_group*options%npes_iserver
      client_start = 0
      app_start_rank = 0
      app_end_rank   = size_iclient - 1 
   endif

   directory_service = DirectoryService(MPI_COMM_WORLD)

   ! app + icilent comm
   my_icomm = MPI_COMM_NULL
   my_appcomm = MPI_COMM_NULL

   do i = 1, N_iclient_group
      low_rank = client_start + (i-1) * options%npes_iserver
      up_rank  = client_start + i*options%npes_iserver
      color = MPI_UNDEFINED
      if (( app_start_rank<= rank .and. rank <= app_end_rank ) .or. ( low_rank <= rank .and. rank < up_rank) ) then
        color = 1
      endif

      call MPI_comm_split(MPI_COMM_WORLD,color,key,local_comm_world(i), ierror)

      if (low_rank <= rank .and. rank < up_rank)  then
         my_comm_world = local_comm_world(i)
      endif

      color = MPI_UNDEFINED
      if ( app_start_rank<= rank .and. rank <= app_end_rank ) then
         color = 1
      endif
      if (low_rank <= rank .and. rank < up_rank)  then
         color = 2
      endif

      app_comms(i) = MPI_COMM_NULL
      if (local_comm_world(i) /= MPI_COMM_NULL) then 
         call MPI_comm_split(local_comm_world(i),color,key,app_comms(i), ierror)
      endif
      
      if ( app_start_rank<= rank .and. rank <= app_end_rank ) then
         my_appcomm = app_comms(i)
      endif

      if (low_rank <= rank .and. rank < up_rank) then
         my_icomm = app_comms(i)
      endif

   enddo
 
   ! app + ocilent comm
   my_ocomm = MPI_COMM_NULL
   do k = 1, N_oclient_group
      i = k + N_iclient_group
      low_rank = client_start+size_iclient + (k-1) * options%npes_oserver
      up_rank  = client_start+size_iclient + k*options%npes_oserver

      color = MPI_UNDEFINED

      if (( app_start_rank<= rank .and. rank <= app_end_rank ) .or. ( low_rank <= rank .and. rank < up_rank) ) then
        color = i
       endif

      call MPI_comm_split(MPI_COMM_WORLD,color,key,local_comm_world(i), ierror)

      if ( low_rank <= rank .and. rank < up_rank)  then
         my_comm_world = local_comm_world(i)
      endif

      color = MPI_UNDEFINED
      if ( app_start_rank<= rank .and. rank <= app_end_rank ) then
        color = i
      endif
      if (low_rank <= rank .and. rank < up_rank) then
        color = i+1
      endif

      app_comms(i) = MPI_COMM_NULL
      if (local_comm_world(i) /= MPI_COMM_NULL) then 
         call MPI_comm_split(local_comm_world(i),color,key,app_comms(i), ierror)
      endif
      
      if (low_rank <= rank .and. rank < up_rank) then
        my_ocomm = app_comms(i)
      endif
   enddo

   if (my_icomm /= MPI_COMM_NULL) then  
      allocate(iserver, source = MpiServer(my_icomm, 'iserver'))
      call directory_service%publish(PortInfo('iserver',iserver), iserver, rc=status)
      if( my_appcomm == MPI_COMM_NULL) then ! mpi server
         call directory_service%connect_to_client('iserver', iserver, rc=status)
         call iserver%start()
      endif
   endif

   if( my_ocomm /= MPI_COMM_NULl) then

      allocate(oserver, source = MpiServer(my_ocomm, 'oserver'))
      call directory_service%publish(PortInfo('oserver',oserver), oserver, rc=status)
      if (my_appcomm == MPI_COMM_NULL) then 
         call directory_service%connect_to_client('oserver', oserver, rc=status)
         call oserver%start()
      endif
   endif

   if ( my_appcomm /= MPI_COMM_NULL) then
      
      print*,"start app rank:", rank

      call histData%init(options,app_comms,directory_service, N_iclient_group, N_oclient_group)
      call histData%run(step=1)
      call histData%run(step=2)
      call histData%finalize()

   end if
  
   call Mpi_Barrier(MPI_COMM_WORLD,ierror)
   call system_clock(c3)

   if ( rank == 0) then
      do md_id = 1, 2
         out_file = 'test_out'//i_to_string(md_id)//'.nc4'
         call execute_command_line('diff test_in.nc4 '//trim(out_file), exitstat = status )
         if (status == 0) then
            print*, 'test_in.nc4 and '//trim(out_file)//' are the same and thus removed'
            call execute_command_line('rm -f '//trim(out_file))
         else
            print*, 'test_in.nc4 and '//trim(out_file)//' differ'
            stop 1
         endif
      enddo
      call execute_command_line('rm -f test_in.nc4')
   endif

   call MPI_finalize(ierror)
end program main
