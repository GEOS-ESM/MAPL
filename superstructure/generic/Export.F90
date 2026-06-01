! Export umbrella for the MAPL superstructure/generic layer.
! Public API exposed to external consumers.
module mapl_generic_export

   use mapl_UserSetServices_mod, only: user_setservices, AbstractUserSetServices, DSOSetServices

   implicit none
   private

   public :: user_setservices
   public :: AbstractUserSetServices
   public :: DSOSetServices

end module mapl_generic_export
