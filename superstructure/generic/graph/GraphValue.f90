module mapl_GraphValue_mod
   use mapl_GraphValueSpec_mod, only: GraphValueSpec
   implicit none(type, external)
   private

   public :: GraphValue

   type, abstract :: GraphValue
   contains
      procedure(graph_value_spec_ifc), deferred :: spec
   end type GraphValue

   abstract interface
      function graph_value_spec_ifc(this) result(spec)
         import :: GraphValue, GraphValueSpec
         class(GraphValue), intent(in) :: this
         type(GraphValueSpec) :: spec
      end function graph_value_spec_ifc
   end interface

end module mapl_GraphValue_mod
