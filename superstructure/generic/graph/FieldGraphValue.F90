module mapl_FieldGraphValue_mod
   use mapl_GraphValue_mod, only: GraphValue
   use mapl_GraphValueSpec_mod, only: GraphValueSpec
   use mapl_ItemSpec_mod, only: ItemSpec
   implicit none(type, external)
   private

   public :: FieldGraphValue

   type, extends(GraphValue) :: FieldGraphValue
      private
      character(:), allocatable :: name_
      type(ItemSpec) :: item_spec_
   contains
      procedure :: name
      procedure :: item_spec
      procedure :: geom_name
      procedure :: spec
   end type FieldGraphValue

   interface FieldGraphValue
      module procedure new_field_graph_value
   end interface FieldGraphValue

contains

   function new_field_graph_value(name, item_spec) result(value)
      character(*), intent(in) :: name
      type(ItemSpec), intent(in) :: item_spec
      type(FieldGraphValue) :: value

      value%name_ = name
      value%item_spec_ = item_spec
   end function new_field_graph_value

   function name(this) result(value)
      class(FieldGraphValue), intent(in) :: this
      character(:), allocatable :: value

      value = this%name_
   end function name

   function item_spec(this) result(value)
      class(FieldGraphValue), intent(in) :: this
      type(ItemSpec) :: value

      value = this%item_spec_
   end function item_spec

   function geom_name(this) result(value)
      class(FieldGraphValue), intent(in) :: this
      character(:), allocatable :: value

      value = this%item_spec_%grid_name()
   end function geom_name

   function spec(this) result(value)
      class(FieldGraphValue), intent(in) :: this
      type(GraphValueSpec) :: value

      value = GraphValueSpec(this%item_spec_%category(), this%name_)
   end function spec

end module mapl_FieldGraphValue_mod
