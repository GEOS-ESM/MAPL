module mapl_GeomManager
   implicit none (type, external)
   private

   public :: GeomManager

contains

   type GeomManager
      private
   contains
      procedure :: add_prototype
      procedure :: clone
      procedure :: make_geom
   end type GeomManager

contains

   function new_GeomManager() result(mgr)
      type(GeomManager) :: mgr

      ! Load default prototypes
      call mgr%prototypes%insert(...)
      
   end function new_GeomManager


end module mapl_GeomManager
