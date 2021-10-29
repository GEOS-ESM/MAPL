module Mapl_VarSpecPtrMod
   use MAPL_VarSpecMod
   implicit none
   private

   public :: MAPL_VarSpecPtr
   
   type :: MAPL_VarSpecPtr
      type(MAPL_VarSpec), pointer :: Spec(:) => null()
   end type MAPL_VarSpecPtr

end module Mapl_VarSpecPtrMod
