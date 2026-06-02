! Export umbrella for the MAPL superstructure/generic layer.
! Public API exposed to external consumers.
module mapl_generic_export

   use mapl_UserSetServices_mod, only: user_setservices, AbstractUserSetServices, DSOSetServices
   use mapl_OpenMP_Support_mod, only: mapl_find_bounds => find_bounds
   use mapl_OpenMP_Support_mod, only: mapl_get_num_threads => get_num_threads
   use mapl_OpenMP_Support_mod, only: mapl_get_current_thread => get_current_thread
   
   implicit none
   private

   public :: user_setservices
   public :: AbstractUserSetServices
   public :: DSOSetServices

   public :: mapl_find_bounds
   public :: mapl_get_num_threads
   public :: mapl_get_current_thread

end module mapl_generic_export
