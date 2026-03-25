module MAPL_MathConstants

   use, intrinsic :: iso_fortran_env, only: REAL64, REAL32

   implicit none

   real(kind=REAL64), parameter :: MAPL_PI_R8                 = 3.14159265358979323846d0
   real(kind=REAL32), parameter :: MAPL_PI                    = MAPL_PI_R8
   real(kind=REAL64), parameter :: MAPL_DEGREES_TO_RADIANS_R8 = MAPL_PI_R8 / 180.d0
   real(kind=REAL32), parameter :: MAPL_DEGREES_TO_RADIANS    = MAPL_PI / 180.
   real(kind=REAL64), parameter :: MAPL_RADIANS_TO_DEGREES    = 180.d0 / MAPL_PI_R8

end module MAPL_MathConstants

! Backwards compatibility alias
module MAPL_MathConstantsMod
   use MAPL_MathConstants
end module MAPL_MathConstantsMod
