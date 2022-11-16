module mapl3g_RegistryPtr
   use mapl3g_AbstractRegistry
   implicit none
   private
  
   public :: RegistryPtr
  
   type :: RegistryPtr
      class(AbstractRegistry), pointer :: registry
   end type RegistryPtr
  
contains
  
end module mapl3g_RegistryPtr
