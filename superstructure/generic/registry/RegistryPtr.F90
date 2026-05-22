module mapl_RegistryPtr_mod
   use mapl_AbstractRegistry_mod
   implicit none(type,external)
   private
  
   public :: RegistryPtr
  
   type :: RegistryPtr
      class(AbstractRegistry), pointer :: registry
   end type RegistryPtr
  
contains
  
end module mapl_RegistryPtr_mod
