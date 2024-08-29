module MAPL_RunEntryPoint

   implicit none (type, external)
   private 
   public :: runEntryPoint

   type :: runEntryPoint
      procedure(), pointer, nopass :: run_entry_point => null()
   end type

end module
