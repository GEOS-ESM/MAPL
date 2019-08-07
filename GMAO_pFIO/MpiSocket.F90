#include "pFIO_ErrLog.h"
#include "unused_dummy.H"

module pFIO_MpiSocketMod
   use iso_c_binding
   use, intrinsic :: iso_fortran_env, only: REAL32
   use, intrinsic :: iso_fortran_env, only: INT64
   use pFIO_ErrorHandlingMod
   use pFIO_AbstractSocketMod
   use pFIO_AbstractRequestHandleMod
   use pFIO_AbstractMessageMod
   use pFIO_ProtocolParserMod
   use pFIO_AbstractDataReferenceMod
   use pFIO_ConstantsMod
   use pFIO_UtilitiesMod, only: word_size, i_to_string
   use mpi
   implicit none
   private

   public :: MpiSocket

   type, extends(AbstractSocket) :: MpiSocket
      private
      type (ProtocolParser), pointer :: parser
      integer :: world_comm
      integer :: pair_comm
      integer :: world_remote_rank
      integer :: world_local_rank
      integer :: pair_local_rank
      integer :: pair_remote_rank
   contains
      procedure :: receive
      procedure :: send
      procedure :: put
      procedure :: get
      procedure :: to_string
   end type MpiSocket

   interface MpiSocket
      module procedure new_MpiSocket
   end interface MpiSocket

   integer, parameter :: PAIR_TAG = 10
   integer, parameter :: MESSAGE_TAG = 11
   integer, parameter :: TAG_TAG = 12

   integer, parameter :: MIN_NONBLOCKING_TAG = 100
   integer, parameter :: MAX_NONBLOCKING_TAG = 199

   ! private type
   type, extends(AbstractRequestHandle) :: MpiRequestHandle
     private
     integer :: mpi_request
   contains
      procedure :: wait
   end type MpiRequestHandle
   
   interface MpiRequestHandle
      module procedure new_MpiRequestHandle
   end interface MpiRequestHandle

contains

   function new_MpiSocket(comm, remote_rank, parser, rc) result(s)
      type (MpiSocket) :: s
      integer, intent(in) :: comm
      integer, intent(in) :: remote_rank
      type (ProtocolParser), target, intent(in) :: parser
      integer, optional, intent(out) :: rc

      integer :: ierror
      integer :: local_rank
      integer :: pair_group
      integer :: world_group
      integer :: ranks(2)

      s%parser => parser
      s%world_comm = comm
      s%world_remote_rank = remote_rank

      call MPI_Comm_rank(comm, local_rank, ierror)
      s%world_local_rank = local_rank

      call MPI_Comm_group(comm, world_group, ierror)

      ! Enforce consistent ordering in new communicator/group
      if (local_rank < remote_rank) then
         ranks = [local_rank, remote_rank]
         s%pair_local_rank = 0
         s%pair_remote_rank = 1
      else
         ranks = [remote_rank, local_rank]
         s%pair_local_rank = 1
         s%pair_remote_rank = 0
      end if
      call MPI_Group_incl(world_group, 2, ranks, pair_group, ierror)
      call MPI_Comm_create_group(comm, pair_group, PAIR_TAG, s%pair_comm, ierror)
      _RETURN(_SUCCESS)
   end function new_MpiSocket

   function receive(this, rc) result(message)
      class (AbstractMessage), pointer :: message
      class (MpiSocket), intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer, allocatable :: buffer(:)
      integer :: ierror
      integer :: status(MPI_STATUS_SIZE)
      integer :: count

      call MPI_Probe(this%pair_remote_rank, MESSAGE_TAG, this%pair_comm, status, ierror)
      call MPI_Get_count(status, MPI_INTEGER, count, ierror)

      allocate(buffer(count))
      call MPI_Recv(buffer, count, MPI_INTEGER, this%pair_remote_rank, MESSAGE_TAG, this%pair_comm, &
           & status, ierror)

      allocate(message, source=this%parser%decode(buffer))
      _RETURN(_SUCCESS)
   end function receive

   subroutine send(this, message, rc)
      class (MpiSocket), intent(inout) :: this
      class (AbstractMessage), intent(in) :: message
      integer, optional, intent(out) :: rc

      integer, allocatable :: buffer(:)
      integer :: ierror

      buffer = this%parser%encode(message)
      call MPI_Send(buffer, size(buffer), MPI_INTEGER, this%pair_remote_rank, MESSAGE_TAG, this%pair_comm, &
           & ierror)
      _RETURN(_SUCCESS)      
   end subroutine send


   function new_MpiRequestHandle(data_reference, mpi_request) result(handle)
      type (MpiRequestHandle) :: handle
      class (AbstractDatareference), intent(in) :: data_reference
      integer, intent(in) :: mpi_request

      allocate(handle%data_reference,source = data_reference)
      handle%mpi_request = mpi_request
   end function new_MpiRequestHandle

   function put(this, request_id, local_reference, rc) result(handle)
      class (AbstractRequestHandle), allocatable :: handle
      class (MpiSocket), intent(inout) :: this
      integer, intent(in) :: request_id
      class (AbstractDatareference), intent(in) :: local_reference
      integer, optional, intent(out) :: rc

      integer :: request
      integer :: ierror
      integer :: tag

      integer, pointer :: data(:)
      integer :: n_words
      
      tag = make_tag(request_id)

      n_words = product(local_reference%shape) * word_size(local_reference%type_kind)
      call c_f_pointer(local_reference%base_address, data, shape=[n_words])
      call MPI_Isend(data, n_words, MPI_INTEGER, this%pair_remote_rank, tag, this%pair_comm, request, ierror)
      allocate(handle, source=MpiRequestHandle(local_reference, request))
      _RETURN(_SUCCESS)
   end function put

   function get(this, request_id, local_reference, rc) result(handle)
      class (AbstractRequestHandle), allocatable :: handle
      class (MpiSocket), intent(inout) :: this
      integer, intent(in) :: request_id
      class (AbstractDataReference), intent(in) :: local_reference
      integer, optional, intent(out) :: rc

      integer :: tag
      integer :: ierror
      integer :: request

      integer, pointer :: data(:)
      integer :: n_words

      tag = make_tag(request_id)

      n_words = product(local_reference%shape) * word_size(local_reference%type_kind)
      call c_f_pointer(local_reference%base_address, data, shape=[n_words])
      call MPI_Irecv(data, n_words, MPI_INTEGER, this%pair_remote_rank, tag, this%pair_comm, request, ierror)
      allocate(handle, source=MpiRequestHandle(local_reference, request))
      _RETURN(_SUCCESS)
   end function get

   subroutine wait(this, rc)
      class (MpiRequestHandle), intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: ierror
      integer :: status(MPI_STATUS_SIZE)
      integer :: save_request

      save_request = this%mpi_request
      call MPI_Wait(this%mpi_request, status, ierror)
      _VERIFY(ierror)
      _RETURN(_SUCCESS)
   end subroutine wait

   integer function get_next_tag() result(tag)
      integer, save :: global_tag = MIN_NONBLOCKING_TAG

      tag = global_tag
      global_tag = MIN_NONBLOCKING_TAG + mod(global_tag + 1 - MIN_NONBLOCKING_TAG, MAX_NONBLOCKING_TAG - MIN_NONBLOCKING_TAG + 1)

   end function get_next_tag

   integer function make_tag(request_id) result(tag)
      integer, intent(in) :: request_id

      tag = request_id

   end function make_tag
      
   function to_string(this) result(string)
      class (MpiSocket), intent(in) :: this
      character(len=:), allocatable :: string
      
      string = 'MpiSocket::info' // new_line('a')
      string = string // '... world local rank:  ' // i_to_string(this%world_local_rank) // new_line('a')
      string = string // '... world remote rank: ' // i_to_string(this%world_remote_rank) // new_line('a')

   end function to_string

end module pFIO_MpiSocketMod
