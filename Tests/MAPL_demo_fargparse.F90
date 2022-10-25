!------------------------------------------------------------------------------
!># Standalone Program for Testing fargparse
!
!------------------------------------------------------------------------------
#define I_AM_MAIN
#include "MAPL_ErrLog.h"
#include "unused_dummy.H"

program main
      use, intrinsic :: iso_fortran_env, only: REAL32
      use MAPL
      use ESMF
      use mpi

      implicit none

      type(MAPL_FargparseCLI) :: cli
      type(MAPL_CapOptions)   :: cap_options
      integer :: status

!------------------------------------------------------------------------------

      call run(_RC)

   contains

      subroutine run(rc)

#undef I_AM_MAIN
#include "MAPL_ErrLog.h"
         integer, intent(out), optional :: rc

         integer :: status

         call MPI_Init(status)
         _VERIFY(status)

         ! Read and parse the command line, and set parameters
         cli = MAPL_FargparseCLI()
         cap_options = MAPL_CapOptions(cli, rc=status)
         write (*,*) "rc = ", status
         _VERIFY(STATUS)

         write(*,*) "done with MAPL_FargparseCLI"
         write(*,*) "  cap_options%with_esmf_moab = ", cap_options%with_esmf_moab
         write(*,*) "  cap_options%npes_input_server = ", cap_options%npes_input_server
         write(*,*) "  cap_options%nodes_input_server = ", cap_options%nodes_input_server
         write(*,*) "  cap_options%npes_output_server = ", cap_options%npes_output_server
         write(*,*) "  cap_options%nodes_output_server = ", cap_options%nodes_output_server
         write(*,*) "  cap_options%egress_file = ", cap_options%egress_file

         _RETURN(_SUCCESS)

      end subroutine run

end program main
