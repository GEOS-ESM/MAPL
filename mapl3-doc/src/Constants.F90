module MAPL_Constants

use, intrinsic :: iso_fortran_env, only: REAL64, REAL32
use MAPL_InternalConstantsMod
use MAPL_MathConstantsMod
use MAPL_PhysicalConstantsMod

contains

subroutine initialize_constants()
   implicit none
end subroutine initialize_constants

end module MAPL_Constants

! For backwards compatibility
module MAPL_ConstantsMod
use MAPL_Constants
end module MAPL_ConstantsMod


