module mapl3g_Framework

!  USE STATEMENTS

   implicit none (type, external)

   private
   public :: MAPL_initialize
   public :: MAPL_finalize

contains


  subroutine MAPL_initialize(config_filename, mpi_communicator, rc)
      character(*), intent(in) :: config_filename
      integer, intent(in) :: mpi_communicator
      integer, optional, intent(out) :: rc

      integer :: status

      ! Cannot process config file until ESMF is initialized, so this is first.
      
      call profiler_init(...)
      call pflogger_init(...)

      _RETURN(_SUCCESS)
   end subroutine MAPL_initialize
      
   subroutine MAPL_finalize(rc)
      integer, optional, intent(out) :: rc

      integer :: status

      ! Cannot process config file until ESMF is initialized, so this is first.

      call profiler_finalize(...)
      call pflogger_finalize(...)
      call ESMF_Finalize(_RC)

      _RETURN(_SUCCESS)
   end subroutine MAPL_finalize
      
end module mapl3g_Framework

