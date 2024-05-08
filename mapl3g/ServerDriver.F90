#include "MAPL_Generic.h"

module mapl3g_ServerDriver
   use mapl_ErrorHandling
   use mpi
   use esmf
!#   use dll
   implicit none
   private

   public :: ServerDriver

   type :: ServerDriver
      type(ESMF_HConfig) :: hconfig
      integer :: world_comm
      integer :: model_comm
      integer :: server_comm
   contains
      procedure :: run
   end type ServerDriver

   interface ServerDriver
      procedure :: new_ServerDriver
   end interface ServerDriver

contains

   function new_ServerDriver(hconfig, world_comm, model_comm, server_comm) result(driver)
      type(ServerDriver) :: driver
      type(ESMF_HConfig), optional, intent(in) :: hconfig
      integer, intent(in) :: world_comm
      integer, intent(in) :: model_comm
      integer, intent(in) :: server_comm
      
   end function new_ServerDriver


   subroutine run(this, rc)
      class(ServerDriver), intent(in) :: this
      integer, optional, intent(out) :: rc

      integer :: status
      character(:), allocatable :: dso_name, dso_procedure
      
      _RETURN_IF(this%server_comm == MPI_COMM_NULL)

      dso_name = ESMF_HConfigAsString(this%hconfig, keystring="dso_name", _RC)
      dso_procedure = ESMF_HConfigAsString(this%hconfig, keystring="dso_procedure", _RC)
      
!#      call dlopen(dso_name,...)
!#      call dlload(dso_procedure ...)
!#
!#      call server_initialize(this%hconfig, this%world_comm, this%model_comm, this%server_comm, _RC)

      _RETURN(_SUCCESS)
   end subroutine run


end module mapl3g_ServerDriver
