#include "MAPL_ErrLog.h"
#include "unused_dummy.H"

module MAPL_FlapCLIMod
   use MPI
   use ESMF
   use FLAP
   use MAPL_KeywordEnforcerMod
   use MAPL_ExceptionHandling
   implicit none
   private

   public :: MAPL_FlapCLI

   type :: MAPL_FlapCLI
     type(command_line_interface) :: cli_options
   contains
      procedure, nopass :: add_command_line_options
   end type MAPL_FlapCLI

   interface MAPL_FlapCLI
      module procedure new_FlapCLI
   end interface

contains

   function new_FlapCLI(unusable, description, authors, rc) result (flapcap)
      type (MAPL_FlapCLI) :: flapcap
      class (KeywordEnforcer), optional, intent(in) :: unusable
      character(*), optional, intent(in) :: description
      character(*), optional, intent(in) :: authors
      integer, optional, intent(out) :: rc
      integer :: status
 
      _UNUSED_DUMMY(unusable)

      call flapcap%cli_options%init( &
        description = trim(description), &
        authors     = trim(authors))

      call flapcap%add_command_line_options(flapcap%cli_options, rc=status)
      _VERIFY(status)   

      call flapcap%cli_options%parse(error=status); _VERIFY(status)

      _RETURN(_SUCCESS)

   end function   

   ! Static method
   subroutine add_command_line_options(options, unusable, rc)
      type (command_line_interface), intent(inout) :: options
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status
      _UNUSED_DUMMY(unusable)

      call options%add(switch='--esmf_logtype',                   &
           help='ESMF Logging type',                   &
           required=.false.,                           &
           choices='none,single,multi,multi_on_error', &
           def='none',                                 &
           act='store',                                &
           error=status)
      _VERIFY(status)
      call options%add(switch='--egress_file', &
                  help='Egress file name', &
                  required=.false.,        &
                  def='EGRESS',            &
                  act='store',             &
                  hidden=.true.,           &
                  error=status)
      _VERIFY(status)
      call options%add(switch='--cap_rc',            &
           help='CAP resource file name', &
           required=.false.,              &
           def='CAP.rc',                  &
           act='store',                   &
           error=status)
      _VERIFY(status)


      call options%add(switch='--npes_model', &
           help='# MPI processes used by model CapGridComp', &
           required=.false., &
           act='store', &
           def='-1', &
           error=status)
      _VERIFY(status)

      call options%add(switch='--n_members', &
           help='# MPI processes used by model CapGridComp1', &
           required=.false., &
           act='store', &
           def='1', &
           error=status)
      _VERIFY(status)

      call options%add(switch='--use_sub_comm', &
           help='# The model by default is using MPI_COMM_WORLD : .true. or .false.', &
           required=.false., &
           def='.false.',     &
           act='store_true',      &
           error=status)
      _VERIFY(status)

      call options%add(switch='--comm_model', &
           help='# The model will use the communitator passed in', &
           required=.false., &
           act='store',      &
           def='*',     &
           error=status)
      _VERIFY(status)

      call options%add(switch='--prefix', &
           help='prefix for ensemble subdirectories', &
           required=.false., &
           act='store', &
           def='mem', &
           error=status)
      _VERIFY(status)

      call options%add(switch='--npes_input_server', &
           help='# MPI processes used by input server', &
           required=.false., &
           def='0', &
           nargs ='*', &
           exclude = '--nodes_input_server', &
           act='store', &
           error=status)
      _VERIFY(status)
      
      call options%add(switch='--npes_output_server', &
           help='# MPI processes used by output server', &
           required=.false., &
           def='0', &
           nargs ='*', &
           exclude = '--nodes_output_server', &
           act='store', &
           error=status)
      _VERIFY(status)

      call options%add(switch='--nodes_input_server', &
           help='# NCCS nodes (28 or more processors ) used by input server', &
           required=.false., &
           def='0', &
           nargs ='*', &
           exclude = '--npes_input_server', &
           act='store', &
           error=status)
      _VERIFY(status)
      
      call options%add(switch='--nodes_output_server', &
           help='# NCCS nodes (28 or more processors) used by output server', &
           required=.false., &
           def='0', &
           nargs ='*', &
           exclude = '--npes_output_server', &
           act='store', &
           error=status)
      _VERIFY(status)

      call options%add(switch='--logging_config', &
           help='Configuration file for logging', &
           required=.false., &
           def='', &
           act='store', &
           error=status)
      _VERIFY(status)

      call options%add(switch='--oserver_type', &
           help='Output Server Type', &
           required=.false., &
           def='single', &
           act='store', &
           error=status)
      _VERIFY(status)

      call options%add(switch='--npes_backend_pernode', &
           help='# MPI processes used by the backend output', &
           required=.false., &
           def='0', &
           act='store', &
           error=status)
      _VERIFY(status)

      call options%add(switch='--compress_nodes', &
           help='MPI processes continue on the nodes even MPI communicator is divided', &
           required=.false., &
           def='.false.', &
           act='store_true', &
           error=status)
      _VERIFY(status)

      call options%add(switch='--fast_oclient', &
           help='Copying data before isend. Client would wait until it is re-used', &
           required=.false., &
           def='.false.', &
           act='store_true', &
           error=status)
      _VERIFY(status)

     call options%add(switch='--one_node_output', &
           help='Specify if each output server has only one nodes', &
           required=.false., &
           def='.false.', &
           act='store_true', &
           error=status)

      call options%add(switch='--with_io_profiler', &
           help='Turning on io_profler', &
           required=.false., &
           def='.false.', &
           act='store_true', &
           error=status)
      _VERIFY(status)

      call options%add(switch='--with_esmf_moab', &
           help='Enables use of MOAB library for ESMF meshes', &
           required=.false., &
           def='.false.', &
           act='store_true', &
           error=status)
      _VERIFY(status)

      _RETURN(_SUCCESS)

   end subroutine add_command_line_options

end module MAPL_FlapCLIMod
