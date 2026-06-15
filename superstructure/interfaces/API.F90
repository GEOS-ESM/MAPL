! Export umbrella for the MAPL.ESMF_Interfaces library.
module mapl_esmf_interfaces_api

   use mapl_esmf_interfaces_mod, only: MAPL_UserCompGetInternalState => UserCompGetInternalState
   use mapl_esmf_interfaces_mod, only: MAPL_UserCompSetInternalState => UserCompSetInternalState

   implicit none
   private

   public :: MAPL_UserCompGetInternalState
   public :: MAPL_UserCompSetInternalState

end module mapl_esmf_interfaces_api
