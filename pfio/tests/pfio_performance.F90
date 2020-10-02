!usage
!mpirun -np 84 ./pfio_performace.x -nc 54 -nso 1 -ncol 4 -v T,U,V -s mpi
!  54 + 1*28 < 84 . The left over if for i-server
!The variable should be 4d with lavel>=20
#include "MAPL_ErrLog.h"
#include "unused_dummy.H"

module performace_CLI
   use MAPL_ExceptionHandling
   use pFIO
   use gFTL_StringVector
   use gFTL_StringIntegerMap
   implicit none
   private

   public :: CommandLineOptions
   public :: process_command_line
   public :: DirectoryServicePointer
 
   type CommandLineOptions
      character(len=:), allocatable :: file_1, file_2
      type (StringVector) :: requested_variables

      integer :: npes_client
      integer :: nodes_oserver
      integer :: num_collection

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
            _ASSERT(buffer /= '-', "too many -")
            read(buffer,*) options%npes_client
         case ('-nso', '--nodes_oserver')
            buffer = get_next_argument()
            _ASSERT(buffer /= '-',"too many -")
            read(buffer,*) options%nodes_oserver
         case ('-ncol', '--num_collection')
            buffer = get_next_argument()
            _ASSERT(buffer /= '-',"too many -")
            read(buffer,*) options%num_collection
         case ('-f1', '--file_1')
            options%file_1 = get_next_argument()
            _ASSERT(options%file_1(1:1) /= '-',"too many -")
         case ('-f2', '--file_2')
            options%file_2 = get_next_argument()
            _ASSERT(options%file_2(1:1) /= '-',"too many -")
         case ('-v', '--var')
            buffer = get_next_argument()
            _ASSERT(buffer(1:1) /= '-',"too many -")
            options%requested_variables = parse_vars(buffer)
         case ('-s', '--server_type')
            options%server_type = get_next_argument()
            _ASSERT(options%server_type /= '-',"too many -")
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
   

end module performace_CLI

module FakeHistDataMod
   use performace_CLI
   use pFIO
   use gFTL_StringVector
   use, intrinsic :: iso_c_binding, only: c_f_pointer, c_loc
   use, intrinsic :: iso_fortran_env, only: REAL32, REAL64
   use mpi
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
      integer :: num_collection

      integer :: comm
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
   

   subroutine init(this, options, comm,app_ds)
      use gFTL_StringIntegerMap
      class (FakeHistData),target, intent(inout) :: this
      type (CommandLineOptions), intent(in) :: options
      integer, intent(in) :: comm
      class(AbstractDirectoryService), target,intent(inout) :: app_ds

      integer :: ierror

      class(ClientThread), pointer :: threadPtr=>null()     

      this%ic_vec = ClientThreadVector()
      this%oc_vec = ClientThreadVector()

      allocate(threadPtr, source = ClientThread())
      call app_ds%connect_to_server('iserver',threadPtr, comm)
      call this%ic_vec%push_back(threadPtr)
      nullify(threadPtr)

      allocate(threadPtr, source = ClientThread())
      call app_ds%connect_to_server('oserver',threadPtr, comm)
      call this%oc_vec%push_back(threadPtr)
      nullify(threadPtr)

      this%file_1 = options%file_1
      this%vars = options%requested_variables
      this%num_collection = options%num_collection


      this%comm = comm
      call MPI_Comm_rank(this%comm,this%rank,ierror)
      call MPI_Comm_size(this%comm,this%npes,ierror)

      allocate(this%bundle(this%vars%size()))
      allocate(this%hist_collection_ids(10))
  
      !call formatter%open(this%file_1, pFIO_READ)
      !file_metadata = formatter%read()
      !call formatter%close()

      !dims = file_metadata%get_dimensions()

      this%Xdim = 24 !720!dims%at('Xdim')
      this%Ydim = 24 !720!dims%at('Ydim')
      this%nf   = 6  !dims%at('nf')
      this%lev  = 72 !dims%at('lev')
      this%time = 1

   end subroutine init

   subroutine run(this, step)
      class (FakeHistData), target, intent(inout) :: this
      integer, intent(in) :: step
      
      type(ArrayReference) :: ref
      type(FileMetadata) :: fmd
      Type(Variable) :: T, v, const_v

      integer :: i_var,i
      integer :: collection_id, file_md_id
      integer,allocatable :: prefetch_ids(:)
      integer,allocatable :: stage_ids(:,:)
      integer,allocatable :: nondistributed_ids(:,:)
      integer :: nx, nf, width, k, ith, jth, Xdim0, Xdim1, Ydim0, Ydim1
      integer :: ierr, status
      real(kind=REAL64) :: t0, t1
      type (StringVariableMap) :: var_map
      real(kind=REAL64), allocatable :: lons(:,:,:),lats(:,:,:)

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

      select case (step)
      case (1) ! read the file
         icPtr => this%ic_vec%at(1)
         collection_id = icPtr%add_ext_collection('collection-i')
         allocate(prefetch_ids(this%vars%size()))

         call MPI_barrier(this%comm,ierr)

         t0 = MPI_wtime()

         do i_var = 1, this%vars%size()
            allocate(this%bundle(i_var)%x(Xdim0:Xdim1,Ydim0:Ydim1,nf:nf,this%lev,1))
            ref = ArrayReference(this%bundle(i_var)%x)
            prefetch_ids(i_var) = &
                 & icPtr%collective_prefetch_data(collection_id,this%file_1, this%vars%at(i_var), ref,&
                 !& this%i_c%collective_prefetch_data(collection_id, this%file_1, this%vars%at(i_var), ref,&
                 & start=[Xdim0,Ydim0,nf,1,1], &
                 & global_start=[1,1,1,1,1],global_count=[this%Xdim,this%Ydim,this%nf, this%lev,1])

         end do
         call icPtr%done_collective_prefetch()
         call MPI_barrier(this%comm,ierr)
        
         t1 = MPI_wtime()

         if( this%rank == 0) then
            print*, "seconds issuing prefech: ", t1-t0
            print*
         endif

         call MPI_barrier(this%comm,ierr)

         t0 = MPI_wtime()

         do i_var = 1, this%vars%size()
            call icPtr%wait(prefetch_ids(i_var))
         end do

         call MPI_barrier(this%comm,ierr)

         t1 = MPI_wtime()

         if( this%rank == 0) then
            print*, "seconds fetching the data: ",t1-t0
         endif

      case (2) ! history out

         allocate(stage_ids(this%vars%size(),this%num_collection))
         fmd = FileMetadata()
         call fmd%add_dimension('Xdim',this%Xdim)
         call fmd%add_dimension('Ydim',this%Ydim)
         call fmd%add_dimension('lev',this%lev)
         call fmd%add_dimension('nf',this%nf)
         call fmd%add_dimension('time',1)

         call fmd%add_dimension('ncontact', 4, rc=status)
         call fmd%add_dimension('orientationStrLen', 5, rc=status)

          v = Variable(type=PFIO_REAL32, dimensions='Xdim')
          call v%add_attribute('long_name', 'Fake Longitude for GrADS Compatibility')
          call v%add_attribute('units', 'degrees_east')
          call fmd%add_variable('Xdim', v)

          const_v = Variable(type=PFIO_REAL64, dimensions='Xdim,Ydim,nf')
          call const_v%add_attribute('long_name', 'longitude with 6 faces')
          call const_v%add_attribute('units', 'degrees_east')
          allocate(lats(this%Xdim,this%Ydim, this%nf))
          lats = 1.0_real64
          call const_v%add_const_value(UnlimitedEntity(lats))
          call fmd%add_variable('lats', const_v)

          v = Variable(type=PFIO_REAL64, dimensions='Xdim,Ydim,nf')
          call const_v%add_attribute('long_name', 'longitude with 6 faces')
          call const_v%add_attribute('units', 'degrees_east')
          call fmd%add_variable('lons', v)

           v = Variable(type=PFIO_REAL64, dimensions='Ydim')
          call v%add_attribute('long_name', 'Fake Latitude for GrADS Compatibility')
          call v%add_attribute('units', 'degrees_north')
          call fmd%add_variable('Ydim', v)

           v = Variable(type=PFIO_REAL32, dimensions='time')
          call v%add_attribute('long_name', 'time sine')
          call v%add_attribute('units', 'time since 0000000')
          call fmd%add_variable('time', v)

           T = Variable(type=pFIO_REAL32, dimensions='Xdim,Ydim,nf,lev,time')

          do i_var = 1, this%vars%size() 
             call fmd%add_variable( this%vars%at(i_var),T)
             if (.not. allocated(this%bundle(i_var)%x)) allocate(this%bundle(i_var)%x(Xdim0:Xdim1,Ydim0:Ydim1,nf:nf,this%lev,1))
            
         enddo

         ocPtr=> this%oc_vec%at(1)

         do i = 1, this%num_collection
            this%hist_collection_ids(i) = ocPtr%add_hist_collection(fmd)
         enddo

         ! create file and put changes into var_map
         v = Variable(type=PFIO_REAL32, dimensions='time')
         call v%add_attribute('long_name', 'time sine')
         call v%add_attribute('units', 'time since 1111111')
         call var_map%insert('time',v)     

         do i = 1, this%num_collection
            file_md_id = this%hist_collection_ids(i)
            call ocPtr%modify_metadata(file_md_id, var_map = var_map)
         end do

         ! send non-distributed-data
         allocate(nondistributed_ids(1,this%num_collection)) 
         allocate(lons(this%Xdim,this%Ydim, this%nf))
         lons = 10.0_real64
         do i = 1, this%num_collection
            file_md_id = this%hist_collection_ids(i)
            ref = ArrayReference(lons)
            nondistributed_ids(1,i) = ocPtr%stage_nondistributed_data(file_md_id, 'test_out'//i_to_string(i)//'.nc4', 'lons', ref)
         end do

         t0 = MPI_wtime()

         do i = 1, this%num_collection
            file_md_id = this%hist_collection_ids(i)
            do i_var = 1, this%vars%size()
               this%bundle(i_var)%x = 1.0*i*i*i_var*i_var
               ref = ArrayReference(this%bundle(i_var)%x)
               stage_ids(i_var,i) = &
                 & ocPtr%collective_stage_data(file_md_id, 'test_out'//i_to_string(i)//'.nc4', this%vars%at(i_var), ref,&
                 & start=[Xdim0,Ydim0,nf,1, 1], &
                 & global_start=[1,1,1,1,1],global_count=[this%Xdim,this%Ydim, this%nf, this%lev,1])
            enddo
         end do
         call ocPtr%done_collective_stage()

         call MPI_barrier(this%comm,ierr)
         t1 = MPI_wtime()
         if( this%rank == 0) then
            print*, "seconds issuing staging data: ", t1-t0
         endif

         call MPI_barrier(this%comm,ierr)

         t0 = MPI_wtime()

         do i = 1, this%num_collection
           call ocPtr%wait(nondistributed_ids(1,i))
         end do

         do i = 1, this%num_collection
            do i_var = 1, this%vars%size()
               call ocPtr%wait(stage_ids(i_var,i))
            enddo
         end do

         call MPI_barrier(this%comm,ierr)
         t1 = MPI_wtime()
         if( this%rank == 0) then
            print*, "seconds offloading data ", t1-t0
         endif

         deallocate(stage_ids)
         do i_var = 1, this%vars%size()
            if (allocated(this%bundle(i_var)%x)) deallocate(this%bundle(i_var)%x)
         enddo
      end select
      
   end subroutine run


   subroutine finalize(this)
      class (FakeHistData), intent(inout) :: this
      class(ClientThread), pointer :: icPtr=>null()
      class(ClientThread), pointer :: ocPtr=>null()

      deallocate(this%bundle)
      icPtr=>this%ic_vec%at(1)
      call icPtr%terminate()
      ocPtr=>this%oc_vec%at(1)
      call ocPtr%terminate()

   end subroutine finalize

end module FakeHistDataMod

program main
   use, intrinsic :: iso_fortran_env, only: REAL32, REAL64
   use mpi
   use pFIO
   use performace_CLI
   use FakeHistDataMod
   use MAPL_ExceptionHandling
   implicit none

   integer :: rank, npes, ierror
   integer :: status, key

   class(BaseServer),allocatable :: iserver,oserver
   class(AbstractDirectoryService), allocatable, target :: directory_service

   type (CommandLineOptions) :: options
   type (FakeHistData), target :: HistData

   integer :: my_icomm, my_ocomm, my_appcomm, o_comm, i_comm
   integer :: InNode_Comm,innode_rank, node_rank, node_num, NodeRoot_Comm
   integer :: root_color,o_color,i_color, app_color
   integer :: tmp_rank, i_size, i_rank, o_size, o_rank, app_size, app_rank
   integer :: i
   real(kind=REAL64) :: t0, t1   

   call MPI_init(ierror)
   call MPI_Comm_rank(MPI_COMM_WORLD, rank, ierror)
   call MPI_Comm_size(MPI_COMM_WORLD, npes, ierror)

   call process_command_line(options, rc=status)

   directory_service = DirectoryService(MPI_COMM_WORLD)

   my_icomm   = MPI_COMM_NULL
   my_appcomm = MPI_COMM_NULL
   my_ocomm   = MPI_COMM_NULL

   ! split into node
   call MPI_Comm_split_type(MPI_COMM_WORLD, MPI_COMM_TYPE_SHARED, 0, MPI_INFO_NULL, InNode_Comm,ierror)
   call MPI_Comm_rank(InNode_Comm, InNode_Rank, ierror)

   root_color = 0
   key = 0
   if ( InNode_rank ==0) root_color = 1
   call MPI_COMM_SPLIT( MPI_COMM_WORLD, root_color, key, NodeRoot_Comm, ierror)
   
   if (root_color == 1) then ! they are all roots of  nodes
      call MPI_COMM_SIZE(NodeRoot_Comm, Node_Num, ierror)  
      call MPI_COMM_RANK(NodeRoot_Comm, Node_Rank, ierror)
   endif

   ! now each process knows its node_rank
   call Mpi_Bcast(Node_Rank, 1, MPI_INTEGER, 0, InNode_Comm, ierror)
   call Mpi_Bcast(Node_Num,  1, MPI_INTEGER, 0, InNode_Comm, ierror)
   if (rank ==0) print*, "total node number: ", node_num 

   if (rank ==0) call execute_command_line('rm -f test_out1.nc4')
 
    o_color   = 0 
    i_color   = 0
    app_color = 0
    my_icomm  = MPI_COMM_NULL
    my_appcomm= MPI_COMM_NULL
    my_ocomm  = MPI_COMM_NULL
    ! o-sever
    if (Node_rank < options%nodes_oserver) o_color = 1
    call MPI_COMM_SPLIT( MPI_COMM_WORLD, o_color, key, o_Comm, ierror)   
    if (o_color == 1) my_ocomm = o_comm 

    if (o_color == 0) then
       call MPI_COMM_RANK(o_comm, tmp_Rank, ierror)
       if (tmp_rank >= options%npes_client) then 
          i_color = 1
       else
          app_color = 1
       endif
       call MPI_COMM_SPLIT( o_comm, i_color, key, i_Comm, ierror)
       
       if(i_color == 1) my_icomm = i_comm
       if(app_color ==1) my_appcomm = i_comm
  
    endif
    
    call Mpi_Barrier(MPI_COMM_WORLD,ierror)
    t0 = MPI_wtime()

    if (my_icomm /= MPI_COMM_NULL) then

      call MPI_Comm_rank(my_icomm, i_rank, ierror)
      call MPI_Comm_size(my_icomm, i_size, ierror)
      if(i_rank ==0) print*, "i_server size:", i_size

      allocate(iserver, source = MpiServer(my_icomm, 'iserver'))
      call directory_service%publish(PortInfo('iserver',iserver), iserver)
      if( my_appcomm == MPI_COMM_NULL) then ! mpi server
         call directory_service%connect_to_client('iserver', iserver)
         call iserver%start()
      endif
   endif

   if( my_ocomm /= MPI_COMM_NULl) then

      call MPI_Comm_rank(my_ocomm, o_rank, ierror)
      call MPI_Comm_size(my_ocomm, o_size, ierror)
      if(o_rank ==0) print*, "o_server size:", o_size

      allocate(oserver, source = MpiServer(my_ocomm, 'oserver'))
      call directory_service%publish(PortInfo('oserver',oserver), oserver)
      if (my_appcomm == MPI_COMM_NULL) then 
         call directory_service%connect_to_client('oserver', oserver)
         call oserver%start()
      endif
   endif

   if ( my_appcomm /= MPI_COMM_NULL) then
     
      call MPI_Comm_rank(my_appcomm, app_rank, ierror)
      call MPI_Comm_size(my_appcomm, app_size, ierror)
      if(app_rank ==0) print*, "app client size:", app_size

      call histData%init(options,my_appcomm,directory_service)
      !call histData%run(step=1)
      call histData%run(step=2)
      call histData%finalize()

   end if
  
   call Mpi_Barrier(MPI_COMM_WORLD,ierror)
   t1 = MPI_wtime()
   if( rank == 0) then
      print*, "seconds wall time : ", t1-t0
   endif

   if ( rank == 0) then
   !   call execute_command_line('diff test_out_TUV.nc4 test_out.nc4', exitstat = status )
   !   if (status == 0) then
   !      print*, 'test_out.nc4 and test_out_TUV.nc4 are the same and thus removed'
   !      call execute_command_line('rm -f test_out.nc4')
   !   else
   !      print*, 'test_out.nc4 and test_out_TUV.nc4 differ'
   !      stop 1
   !   endif

      do i = 1, options%num_collection
!         call execute_command_line('rm -f test_out'//i_to_string(i)//'.nc4')
      enddo
   endif
   call directory_service%free_directory_resources()
   call MPI_finalize(ierror)

end program main
