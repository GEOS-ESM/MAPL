module MAPL_MathConstantsMod

   use, intrinsic :: iso_fortran_env, only: REAL64, REAL32

   implicit none

!=============================================================================
!BOP

! !MODULE: -- A container module for MAPL mathematical constants

! !PUBLIC VARIABLES:
   real(kind=REAL64), parameter :: MAPL_PI_R8              = 3.14159265358979323846d0
   real(kind=REAL32), parameter :: MAPL_PI                 = MAPL_PI_R8
   real(kind=REAL64), parameter :: MAPL_DEGREES_TO_RADIANS_R8 = MAPL_PI_R8 / 180.
   real(kind=REAL32), parameter :: MAPL_DEGREES_TO_RADIANS = MAPL_PI / 180.
   real(kind=REAL64), parameter :: MAPL_RADIANS_TO_DEGREES = 180. / MAPL_PI_R8

!EOP

end module MAPL_MathConstantsMod
