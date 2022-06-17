module mapl3g_ServiceRequesterSpec
   use mapl3g_AbstractStateItemSpec
   use gftl2_StringVector
   implicit none
   private

   public :: ServiceRequesterSpec

   type, extends(AbstractStateItemSpec) :: ServiceRequesterSpec
      character(:), allocatable :: service_name
      type(StringVector) :: field_names         ! requester side (maybe bundle ...)
   end type ServiceRequesterSpec
   
end module mapl3g_ServiceRequesterSpec
