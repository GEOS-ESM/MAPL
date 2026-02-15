!------------------------------------------------------------------------------
!># Standalone Program for Testing fargparse
!
!------------------------------------------------------------------------------

! We use a module here because we need two levels of contains

#include "MAPL_ErrLog.h"
module main_mod

   use MAPL
   use mpi
   use fargparse

   implicit none

   contains

      subroutine run(rc)

         integer, intent(out), optional :: rc

         type(FargparseCLI_Type) :: cli
         type(MAPL_CapOptions)   :: cap_options

         integer :: status
         character(len=:), allocatable :: input_file

         call MPI_Init(status)
         _VERIFY(status)

         ! Read and parse the command line, and set parameters
         ! If you have extra options, you need to make two procedures as seen below:
         ! 1. a procedure to declare the options
         ! 2. a procedure to cast the options
         cap_options = FargparseCLI(extra_options=extra_options, cast_extras=cast_extras)

         write(*,*) "done with FargparseCLI"
         write(*,*) "  cap_options%with_esmf_moab = ", cap_options%with_esmf_moab
         write(*,*) "  cap_options%npes_input_server = ", cap_options%npes_input_server
         write(*,*) "  cap_options%nodes_input_server = ", cap_options%nodes_input_server
         write(*,*) "  cap_options%npes_output_server = ", cap_options%npes_output_server
         write(*,*) "  cap_options%nodes_output_server = ", cap_options%nodes_output_server
         write(*,*) "  cap_options%egress_file = ", cap_options%egress_file
         write(*,*) ""
         write(*,*) "Extra arguments"
         write(*,*) "  input file = ", input_file

         _RETURN(_SUCCESS)

         contains

            subroutine extra_options(parser, rc)
               type (ArgParser), intent(inout) :: parser
               integer, intent(out), optional :: rc

               call parser%add_argument('-f', '--file', &
                  help='A file to read', &
                  type='string', &
                  default='default.config', &
                  action='store')

               !_RETURN(_SUCCESS)
               if (present(rc)) rc = 0

            end subroutine extra_options

            subroutine cast_extras(cli, rc)
               type(FargparseCLI_Type), intent(inout) :: cli
               integer, intent(out), optional :: rc

               class(*), pointer :: option

               option => cli%options%at('file')
               if (associated(option)) then
                  call cast(option, input_file, _RC)
               end if

               !_RETURN(_SUCCESS)
               if (present(rc)) rc = 0

            end subroutine cast_extras

      end subroutine run

end module main_mod

#define I_AM_MAIN
#include "MAPL_ErrLog.h"
#include "unused_dummy.H"

program main
      use main_mod

      implicit none

      integer :: status

!------------------------------------------------------------------------------

      call run(_RC)

end program main
