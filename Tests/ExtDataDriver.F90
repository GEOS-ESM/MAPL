#include "MAPL_Generic.h"

program ExtData_Driver
  use MPI
  use ESMF
  use MAPL_BaseMod
  use MAPL_GenericMod
  use ExtData_DriverGridCompMod, only: ExtData_DriverGridComp, new_ExtData_DriverGridComp
  use ExtDataUtRoot_GridCompMod, only:  ROOT_SetServices => SetServices
  use FLAP
  use MAPL_KeywordEnforcerMod
  use ExtDataDriverMod

  implicit none

  integer :: status
  character(len=*), parameter :: Iam="ExtData_Driver"
  type(ExtDataDriver) :: Driver 
  type (command_line_interface) :: options

  call options%init( &
       description = 'ExtData driver', &
       authors     = 'GMAO')

  driver = ExtDataDriver('ExtDataApp',Root_SetServices)
  call driver%add_command_line_options(options,rc=status)
  call driver%run(options,rc=STATUS)

  call exit(0)

end program ExtData_Driver
