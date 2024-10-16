module mapl3g_AbstractActionSpec
   implicit none (type, external)
   private
  
   public :: AbstractActionSpec
  
   type, abstract :: AbstractActionSpec
     private
  contains
!!$     procedure :: make_task
   end type AbstractActionSpec
  
contains
  
end module mapl3g_AbstractActionSpec
