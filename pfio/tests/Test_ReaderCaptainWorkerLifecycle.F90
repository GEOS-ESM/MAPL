program Test_ReaderCaptainWorkerLifecycle
   use mpi
   implicit none

   integer, parameter :: CMD_READY = 1
   integer, parameter :: CMD_WORK = 2
   integer, parameter :: CMD_STOP = -1
   integer, parameter :: TAG_MODEL_CMD = 8801
   integer, parameter :: TAG_MODEL_DONE = 8802
   integer, parameter :: TAG_WORK_CMD = 8803
   integer, parameter :: TAG_WORK_DONE = 8804

   integer :: ierr, world_rank, world_size, reader_comm, reader_rank
   integer :: color, command, value, status(MPI_STATUS_SIZE)

   call MPI_Init(ierr)
   call MPI_Comm_rank(MPI_COMM_WORLD, world_rank, ierr)
   call MPI_Comm_size(MPI_COMM_WORLD, world_size, ierr)
   if (world_size /= 3) then
      if (world_rank == 0) write(*, '(A,I0)') 'expected 3 ranks, got ', world_size
      call MPI_Abort(MPI_COMM_WORLD, 2, ierr)
   end if

   color = merge(1, MPI_UNDEFINED, world_rank > 0)
   call MPI_Comm_split(MPI_COMM_WORLD, color, world_rank, reader_comm, ierr)
   call check(ierr)

   if (world_rank == 0) then
      command = CMD_READY
      call MPI_Send(command, 1, MPI_INTEGER, 1, TAG_MODEL_CMD, MPI_COMM_WORLD, ierr)
      call check(ierr)
      call MPI_Recv(value, 1, MPI_INTEGER, 1, TAG_MODEL_DONE, MPI_COMM_WORLD, status, ierr)
      call check(ierr)
      if (value /= CMD_READY) call MPI_Abort(MPI_COMM_WORLD, 3, ierr)

      command = CMD_WORK
      call MPI_Send(command, 1, MPI_INTEGER, 1, TAG_MODEL_CMD, MPI_COMM_WORLD, ierr)
      call check(ierr)
      call MPI_Recv(value, 1, MPI_INTEGER, 1, TAG_MODEL_DONE, MPI_COMM_WORLD, status, ierr)
      call check(ierr)
      if (value /= CMD_WORK) call MPI_Abort(MPI_COMM_WORLD, 4, ierr)

      command = CMD_STOP
      call MPI_Send(command, 1, MPI_INTEGER, 1, TAG_MODEL_CMD, MPI_COMM_WORLD, ierr)
      call check(ierr)
   else
      call MPI_Comm_rank(reader_comm, reader_rank, ierr)
      call check(ierr)
      if (reader_rank == 0) then
         call captain_loop()
      else
         call worker_loop()
      end if
      call MPI_Comm_free(reader_comm, ierr)
      call check(ierr)
   end if

   call MPI_Finalize(ierr)
   call check(ierr)

contains

   subroutine captain_loop()
      integer :: worker_command

      call MPI_Recv(command, 1, MPI_INTEGER, 0, TAG_MODEL_CMD, MPI_COMM_WORLD, status, ierr)
      call check(ierr)
      worker_command = CMD_READY
      call MPI_Send(worker_command, 1, MPI_INTEGER, 1, TAG_WORK_CMD, reader_comm, ierr)
      call check(ierr)
      call MPI_Recv(value, 1, MPI_INTEGER, 1, TAG_WORK_DONE, reader_comm, status, ierr)
      call check(ierr)
      call MPI_Send(value, 1, MPI_INTEGER, 0, TAG_MODEL_DONE, MPI_COMM_WORLD, ierr)
      call check(ierr)

      call MPI_Recv(command, 1, MPI_INTEGER, 0, TAG_MODEL_CMD, MPI_COMM_WORLD, status, ierr)
      call check(ierr)
      worker_command = CMD_WORK
      call MPI_Send(worker_command, 1, MPI_INTEGER, 1, TAG_WORK_CMD, reader_comm, ierr)
      call check(ierr)
      call MPI_Recv(value, 1, MPI_INTEGER, 1, TAG_WORK_DONE, reader_comm, status, ierr)
      call check(ierr)
      call MPI_Send(value, 1, MPI_INTEGER, 0, TAG_MODEL_DONE, MPI_COMM_WORLD, ierr)
      call check(ierr)

      call MPI_Recv(command, 1, MPI_INTEGER, 0, TAG_MODEL_CMD, MPI_COMM_WORLD, status, ierr)
      call check(ierr)
      if (command /= CMD_STOP) call MPI_Abort(MPI_COMM_WORLD, 5, ierr)
      call MPI_Send(CMD_STOP, 1, MPI_INTEGER, 1, TAG_WORK_CMD, reader_comm, ierr)
      call check(ierr)
      call MPI_Recv(value, 1, MPI_INTEGER, 1, TAG_WORK_DONE, reader_comm, status, ierr)
      call check(ierr)
   end subroutine captain_loop

   subroutine worker_loop()
      do while (.true.)
         call MPI_Recv(command, 1, MPI_INTEGER, 0, TAG_WORK_CMD, reader_comm, status, ierr)
         call check(ierr)
         if (command == CMD_STOP) then
            call MPI_Send(CMD_STOP, 1, MPI_INTEGER, 0, TAG_WORK_DONE, reader_comm, ierr)
            call check(ierr)
            exit
         end if
         if (command /= CMD_READY .and. command /= CMD_WORK) &
            call MPI_Abort(MPI_COMM_WORLD, 6, ierr)
         call MPI_Send(command, 1, MPI_INTEGER, 0, TAG_WORK_DONE, reader_comm, ierr)
         call check(ierr)
      end do
   end subroutine worker_loop

   subroutine check(error_code)
      integer, intent(in) :: error_code
      if (error_code /= MPI_SUCCESS) call MPI_Abort(MPI_COMM_WORLD, error_code, ierr)
   end subroutine check

end program Test_ReaderCaptainWorkerLifecycle
