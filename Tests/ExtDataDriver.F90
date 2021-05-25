#define I_AM_MAIN

#include "MAPL_Generic.h"

program ExtData_Driver
  use MPI
  use ESMF
  use ExtData_DriverGridCompMod, only: ExtData_DriverGridComp, new_ExtData_DriverGridComp
  use ExtDataUtRoot_GridCompMod, only:  ROOT_SetServices => SetServices
  use FLAP
  use ExtDataDriverMod
  use MAPL

  implicit none

  integer :: status
  character(len=*), parameter :: Iam="ExtData_Driver"
  type(ExtDataDriver) :: Driver 
  type (MAPL_CapOptions) :: cap_options
  type (MAPL_FlapCLI) :: cli

  cli = MAPL_FlapCLI(description='extdata driver',authors='gmao')
  cap_options=MAPL_CapOptions(cli)

  driver = ExtDataDriver('ExtDataApp',Root_SetServices,cap_options=cap_options,_RC)
  call driver%run(_RC)

  call exit(0)

end program ExtData_Driver
