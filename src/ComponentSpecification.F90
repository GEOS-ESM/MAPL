module mapl_ComponentSpecification
   use mapl_VariableSpecification
   use mapl_StateSpecification
   implicit none
   private

   public :: ComponentSpecification
   

   type :: ComponentSpecification
      type(StateSpecification) :: import
      type(StateSpecification) :: export
      type(StateSpecification) :: internal
      type(StateSpecification) :: forcing
   end type ComponentSpecification
   
end module mapl_ComponentSpecification
