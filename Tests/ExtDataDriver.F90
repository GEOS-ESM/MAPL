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
  type (MAPL_FlapCapOptions) :: cap_options

  cap_options=MAPL_FlapCapOptions(description='extdata driver',authors='gmao')

  driver = ExtDataDriver('ExtDataApp',Root_SetServices,cap_options=cap_options)
  call driver%run(rc=STATUS)

  call exit(0)

end program ExtData_Driver
