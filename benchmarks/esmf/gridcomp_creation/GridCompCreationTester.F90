program grid_comp_creation_tester

   use grid_comp_creation_shared
   use grid_comp_creator
   use grid_comp_creator_memory_profiler

   implicit none

   integer, parameter :: SUCCESS = 0
   integer, parameter :: GENERAL_ERROR = 1
   integer, parameter :: NGC_NOT_SET = 2*GENERAL_ERROR
   integer, parameter :: RUN_FAILED  = 2*NGC_NOT_SET

   character(len=*), parameter :: OPTION_USE_OWN_VM = '--use-own-vm'

   integer :: rc, status
   integer :: ngc
   character(len=MAXSTR) :: raw
   integer :: i, n
   logical :: is_silent = .TRUE.
   logical :: use_own_vm = .FALSE.

   rc = SUCCESS
   ngc = -1
   do i = 1, command_argument_count()
      call get_command_argument(i, value=raw, status=status)
      if(status /= SUCCESS) cycle
      raw = adjustl(raw)
      if(raw == OPTION_USE_OWN_VM) then
         use_own_vm = .TRUE.
         cycle
      end if
      read(raw, fmt='(I32)', iostat=status) n
      if(status == SUCCESS) ngc = n
   end do
   
   if(ngc < 0) then
      rc = rc + NGC_NOT_SET
      error stop rc, QUIET=is_silent
   end if
   
   call run(ngc, use_own_vm, status)

   if(status /= SUCCESS) rc = rc + RUN_FAILED
   rc = finalize()
   if(rc == SUCCESS) stop rc, QUIET=is_silent
   error stop rc, QUIET=is_silent

end program grid_comp_creation_tester
