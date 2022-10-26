module mapl3g_StateItemSpecPtr
   use mapl3g_AbstractStateItemSpec
   implicit none
   private
  
   public :: StateItemSpecPtr
  
   type :: StateItemSpecPtr
      class(AbstractStateItemSpec), pointer :: ptr
   end type StateItemSpecPtr

   interface StateItemSpecPtr
      module procedure new_StateItemSpecPtr
   end interface StateItemSpecPtr
      
contains

   function new_StateItemSpecPtr(state_item) result(wrap)
      type(StateItemSpecPtr) :: wrap
      class(AbstractStateItemSpec), target :: state_item

      wrap%ptr => state_item
   end function new_StateItemSpecPtr
  
end module mapl3g_StateItemSpecPtr
