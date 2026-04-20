module mapl3g_RunEntryPoint
   implicit none(type,external)
   private

   public :: runEntryPoint

   type :: runEntryPoint
      procedure(), pointer, nopass :: run_entry_point => null()
   end type runEntryPoint

end module mapl3g_RunEntryPoint
