module mapl3g_ServiceProviderSpec
   use mapl3g_AbstractStateItemSpec
   implicit none
   private

   public :: ServiceProviderSpec

   type, extends(AbstractStateItemSpec) :: ServiceProviderSpec
      character(:), allocatable :: service_name
      character(:), allocatable :: bundle_name  ! provider side
   end type ServiceProviderSpec

end module mapl3g_ServiceProviderSpec
