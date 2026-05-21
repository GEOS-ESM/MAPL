module mapl_RegistryPtr
   use mapl_AbstractRegistry
   implicit none(type,external)
   private
  
   public :: RegistryPtr
  
   type :: RegistryPtr
      class(AbstractRegistry), pointer :: registry
   end type RegistryPtr
  
contains
  
end module mapl_RegistryPtr
