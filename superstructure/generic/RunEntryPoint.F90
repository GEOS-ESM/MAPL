module mapl_RunEntryPoint_mod
   implicit none(type,external)
   private

   public :: runEntryPoint

   type :: runEntryPoint
      procedure(), pointer, nopass :: run_entry_point => null()
   end type runEntryPoint

end module mapl_RunEntryPoint_mod
