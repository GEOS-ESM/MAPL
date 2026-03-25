module MAPL_Constants

   use MAPL_InternalConstants
   use MAPL_MathConstants
   use MAPL_PhysicalConstants
   use MAPL_EarthConstants
   use MAPL_EarthAtmosphericConstants

contains

   subroutine initialize_constants()
      implicit none
   end subroutine initialize_constants

end module MAPL_Constants

! Backwards compatibility alias
module MAPL_ConstantsMod
   use MAPL_Constants
end module MAPL_ConstantsMod
