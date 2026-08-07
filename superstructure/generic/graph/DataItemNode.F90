module mapl_DataItemNode_mod
   use mapl_GraphNode_mod, only: GraphNode
   use mapl_ItemSpec_mod, only: ItemSpec
   implicit none(type, external)
   private

   public :: DataItemNode

   type, extends(GraphNode) :: DataItemNode
      private
      character(:), allocatable :: name_
      type(ItemSpec) :: spec_
   contains
      procedure :: name
      procedure :: spec
   end type DataItemNode

   interface DataItemNode
      procedure new_data_item
      procedure new_data_item_from_spec
   end interface

contains

   function new_data_item(name, spec) result(node)
      character(*), intent(in) :: name
      type(ItemSpec), optional, intent(in) :: spec
      type(DataItemNode) :: node

      node%name_ = name
      if (present(spec)) node%spec_ = spec
   end function new_data_item

   function new_data_item_from_spec(spec) result(node)
      type(ItemSpec), intent(in) :: spec
      type(DataItemNode) :: node

      node%name_ = ''
      node%spec_ = spec
   end function new_data_item_from_spec

   function name(this) result(value)
      class(DataItemNode), intent(in) :: this
      character(:), allocatable :: value

      value = this%name_
   end function name

   function spec(this) result(value)
      class(DataItemNode), intent(in) :: this
      type(ItemSpec) :: value

      value = this%spec_
   end function spec

end module mapl_DataItemNode_mod
