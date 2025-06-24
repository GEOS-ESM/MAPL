!------------------------------------------------------------------------------
!># Standalone Program for Testing fargparse
!
!------------------------------------------------------------------------------
#define I_AM_MAIN
#include "MAPL_ErrLog.h"
#include "unused_dummy.H"

program main
      use MAPL
      use mpi
      use fargparse

      implicit none

      type(MAPL_FargparseCLI) :: cli
      type(MAPL_CapOptions)   :: cap_options
      integer :: status

!------------------------------------------------------------------------------

      call run(__RC)

   contains

#undef I_AM_MAIN
#include "MAPL_ErrLog.h"
      subroutine run(rc)

         integer, intent(out), optional :: rc

         integer :: status
         character(len=:), allocatable :: input_file

         call MPI_Init(status)
         __VERIFY(status)

         ! Read and parse the command line, and set parameters
         ! If you have extra options you make a procedure as seen below and add arguments
         ! there and pass in here
         cli = MAPL_FargparseCLI(extra=extra_options)

         ! This does the casting of arguments into cap_options for CAP
         cap_options = MAPL_CapOptions(cli, __RC)

         write(*,*) "done with MAPL_FargparseCLI"
         write(*,*) "  cap_options%with_esmf_moab = ", cap_options%with_esmf_moab
         write(*,*) "  cap_options%npes_input_server = ", cap_options%npes_input_server
         write(*,*) "  cap_options%nodes_input_server = ", cap_options%nodes_input_server
         write(*,*) "  cap_options%npes_output_server = ", cap_options%npes_output_server
         write(*,*) "  cap_options%nodes_output_server = ", cap_options%nodes_output_server
         write(*,*) "  cap_options%egress_file = ", cap_options%egress_file

         ! For our extra options we have to explicitly cast them
         call cast(cli%options%at('file'), input_file, __RC)

         write(*,*) ""
         write(*,*) "Extra arguments"
         write(*,*) "  input file = ", input_file

         __RETURN(__SUCCESS)

      end subroutine run

      subroutine extra_options(parser, rc)
         type (ArgParser), intent(inout) :: parser
         integer, intent(out), optional :: rc

         call parser%add_argument('-f', '--file', &
              help='A file to read', &
              type='string', &
              default='default.config', &
              action='store')

         !__RETURN(__SUCCESS)
         if (present(rc)) rc = 0

      end subroutine extra_options

end program main
