#include "MAPL_ErrLog.h"
#include "unused_dummy.H"

module MAPL_ServerManager

   use MAPL_ExceptionHandling
   use MAPL_KeywordEnforcerMod
   use pFIO_ClientManagerMod
   use PFIO
   use MAPL_ioClientsMod
   use MAPL_SimpleCommSplitterMod
   use MAPL_SplitCommunicatorMod
   implicit none
   private


   type, public :: ServerManager
      type(SplitCommunicator)  :: split_comm
      type(MpiServer), pointer :: i_server=>null()
      type(MpiServer), pointer :: o_server=>null()
      type(DirectoryService) :: directory_service
      contains
         procedure :: initialize
         procedure :: finalize
         procedure :: get
   end type

contains

   subroutine get(this, unusable, split_comm, rc)
      class (ServerManager), intent(inout) :: this
      class (KeywordEnforcer),  optional, intent(in) :: unusable
      type(SplitCommunicator), intent(out), optional :: split_comm
      integer, intent(out), optional :: rc

      _UNUSED_DUMMY(unusable)

      if (present(split_comm)) split_comm=this%split_comm
      _RETURN(_SUCCESS)

   end subroutine get

   subroutine initialize(this, comm, unusable, application_size, nodes_input_server, nodes_output_server, npes_input_server,npes_output_server, rc)
      class (ServerManager), intent(inout) :: this
      integer, intent(in) :: comm
      class (KeywordEnforcer),  optional, intent(in) :: unusable
      integer, optional, intent(in) :: application_size
      integer, optional, intent(in) :: nodes_input_server(:),nodes_output_server(:)
      integer, optional, intent(in) :: npes_input_server(:),npes_output_server(:)
      integer, optional, intent(out) :: rc
      integer, allocatable :: npes_in(:),npes_out(:),nodes_in(:),nodes_out(:)

      type (SimpleCommSplitter) :: splitter
      integer :: status, i, rank,npes_model,n_oserver_group, n_iserver_group
      character(len=:), allocatable :: s_name
      type(ClientThread), pointer :: clientPtr

      _UNUSED_DUMMY(unusable)

      if (present(application_size)) then
         npes_model = application_size
      else
         call MPI_COMM_Size(comm,npes_model,status)
         _VERIFY(status)
      end if
      if (present(npes_input_server)) then
         npes_in = npes_input_server
      else
         npes_in = [0]
      end if

      if (present(npes_output_server)) then
         npes_out = npes_output_server
      else
         npes_out = [0]
      end if

      if (present(nodes_input_server)) then
         nodes_in = nodes_input_server
      else
         nodes_in = [0]
      end if

      if (present(nodes_output_server)) then
         nodes_out = nodes_output_server
      else
         nodes_out = [0]
      end if

     n_iserver_group = max(size(npes_in),size(nodes_in))
     n_oserver_group = max(size(npes_out),size(nodes_out))
     this%directory_service = DirectoryService(comm)
     splitter = SimpleCommSplitter(comm)
     call splitter%add_group(npes=npes_model, name='model', isolate_nodes=.true.)

     if (npes_in(1) > 0) then
        do i = 1, size(npes_in)
           s_name ='i_server'//trim(i_to_string(i))
           call splitter%add_group(npes=npes_in(i), name=s_name, isolate_nodes=.true.)
        enddo
     elseif (nodes_in(1) > 0) then
        do i = 1, size(nodes_in)
           s_name ='i_server'//trim(i_to_string(i))
           call splitter%add_group(nnodes=nodes_in(i), name=s_name, isolate_nodes=.true.)
        enddo
     end if

     if (npes_out(1) > 0 ) then
        do i = 1, size(npes_out)
          s_name ='o_server'//trim(i_to_string(i))
          call splitter%add_group(npes=npes_out(i), name=s_name, isolate_nodes=.true.)
        enddo
     else if(nodes_out(1) > 0) then
        do i = 1, size(nodes_out)
          s_name ='o_server'//trim(i_to_string(i))
          call splitter%add_group(nnodes=nodes_out(i), name=s_name, isolate_nodes=.true.)
        enddo
     endif

     this%split_comm = splitter%split(rc=status); _VERIFY(status)

     s_name = this%split_comm%get_name()

     if ( index(s_name, 'model') /=0 ) then
        if (npes_in(1) == 0 .and. nodes_in(1) == 0) then
           allocate(this%i_server, source = MpiServer(this%split_comm%get_subcommunicator(), 'i_server'//trim(i_to_string(1))))
           call this%directory_service%publish(PortInfo('i_server'//trim(i_to_string(1)), this%i_server), this%i_server)
        end if
        if (npes_out(1) == 0 .and. nodes_out(1) == 0) then
           allocate(this%o_server, source = MpiServer(this%split_comm%get_subcommunicator(), 'o_server'//trim(i_to_string(1))))
           call this%directory_service%publish(PortInfo('o_server'//trim(i_to_string(1)), this%o_server), this%o_server)
        end if
        call io_client%init_io_clients(ni = n_oserver_group, no = n_iserver_group )
     endif

     ! establish i_server group one by one
     do i = 1, n_iserver_group

        if ( trim(s_name) =='i_server'//trim(i_to_string(i)) ) then
           allocate(this%i_server, source = MpiServer(this%split_comm%get_subcommunicator(), s_name))
           call this%directory_service%publish(PortInfo(s_name,this%i_server), this%i_server)
           call this%directory_service%connect_to_client(s_name, this%i_server)
           call MPI_Comm_Rank(this%split_comm%get_subcommunicator(),rank,status)
           if (rank == 0 .and. nodes_in(i) /=0 ) then
              write(*,'(A,I0,A)')"Starting pFIO input server on ",nodes_in(i)," nodes"
           else if (rank==0 .and. npes_in(i) /=0 ) then
              write(*,'(A,I0,A)')"Starting pFIO input server on ",npes_in(i)," pes"
           end if
        endif

        if ( index(s_name, 'model') /=0 ) then
           clientPtr => i_Clients%current()
           call this%directory_service%connect_to_server('i_server'//trim(i_to_string(i)), clientPtr, this%split_comm%get_subcommunicator())
           call i_Clients%next()
        endif

        call mpi_barrier(comm, status)

     enddo

     ! establish o_server group one by one
     do i = 1, n_oserver_group

        if ( trim(s_name) =='o_server'//trim(i_to_string(i)) ) then
           allocate(this%o_server, source = MpiServer(this%split_comm%get_subcommunicator(), s_name))
           call this%directory_service%publish(PortInfo(s_name,this%o_server), this%o_server)
           call this%directory_service%connect_to_client(s_name, this%o_server)
           call MPI_Comm_Rank(this%split_comm%get_subcommunicator(),rank,status)
           if (rank == 0 .and. nodes_out(i) /=0 ) then
              write(*,'(A,I0,A)')"Starting pFIO output server on ",nodes_out," nodes"
           else if (rank==0 .and. npes_out(i) /=0 ) then
              write(*,'(A,I0,A)')"Starting pFIO output server on ",npes_out," pes"
           end if
        endif

        if ( index(s_name, 'model') /=0 ) then
           clientPtr => o_Clients%current()
           call this%directory_service%connect_to_server('o_server'//trim(i_to_string(i)), clientPtr, this%split_comm%get_subcommunicator())
           call o_Clients%next()
        endif

        call mpi_barrier(comm, status)

     enddo

     if ( index(s_name, 'o_server') /=0 ) then
        call this%o_server%start()
     endif

     if ( index(s_name, 'i_server') /=0 ) then
        call this%i_server%start()
     endif

     if ( index(s_name, 'model') /=0 ) then
        call i_Clients%set_current(1) ! set current to be the first
        call o_Clients%set_current(1) ! set current to be the first
        if (npes_out(1) >0) then
           call io_client%set_size(no = npes_out,rc=status)
        else if (nodes_out(1)>0) then
           call io_client%set_size(no = nodes_out,rc=status)
        endif
        _VERIFY(status)
     end if

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(unusable)
   end subroutine initialize

   subroutine finalize(this,rc)
      class(ServerManager), intent(inout) :: this
      integer, optional, intent(out) :: rc
 
      call this%directory_service%free_directory_resources()
      _RETURN(_SUCCESS)
   end subroutine finalize

end module MAPL_ServerManager
