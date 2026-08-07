module mapl_GraphTransform_mod
   use mapl_GraphValue_mod, only: GraphValue
   use mapl_GraphValueSpec_mod, only: GraphValueSpec
   implicit none(type, external)
   private

   public :: GraphTransform, GraphPortSpec, GraphValueRef

   type :: GraphPortSpec
      private
      character(:), allocatable :: name_
      logical :: required_ = .true.
      type(GraphValueSpec) :: value_spec_
   contains
      procedure :: name
      procedure :: is_required
      procedure :: value_spec
   end type GraphPortSpec

   interface GraphPortSpec
      module procedure new_graph_port_spec
   end interface GraphPortSpec

   type :: GraphValueRef
      class(GraphValue), pointer :: value => null()
   end type GraphValueRef

   type, abstract :: GraphTransform
   contains
      procedure(graph_port_specs_ifc), deferred :: input_specs
      procedure(graph_port_specs_ifc), deferred :: output_specs
      procedure(execute_graph_transform_ifc), deferred :: execute
   end type GraphTransform

   abstract interface
      function graph_port_specs_ifc(this) result(specs)
         import :: GraphPortSpec, GraphTransform
         class(GraphTransform), intent(in) :: this
         type(GraphPortSpec), allocatable :: specs(:)
      end function graph_port_specs_ifc

      subroutine execute_graph_transform_ifc(this, inputs, outputs, rc)
         import :: GraphTransform, GraphValueRef
         class(GraphTransform), intent(inout) :: this
         type(GraphValueRef), intent(in) :: inputs(:)
         type(GraphValueRef), intent(inout) :: outputs(:)
         integer, optional, intent(out) :: rc
      end subroutine execute_graph_transform_ifc
   end interface

contains

   function new_graph_port_spec(name, value_spec, required) result(spec)
      character(*), intent(in) :: name
      type(GraphValueSpec), intent(in) :: value_spec
      logical, optional, intent(in) :: required
      type(GraphPortSpec) :: spec

      spec%name_ = name
      spec%value_spec_ = value_spec
      if (present(required)) spec%required_ = required
   end function new_graph_port_spec

   function name(this) result(value)
      class(GraphPortSpec), intent(in) :: this
      character(:), allocatable :: value

      value = this%name_
   end function name

   pure logical function is_required(this)
      class(GraphPortSpec), intent(in) :: this

      is_required = this%required_
   end function is_required

   pure function value_spec(this) result(spec)
      class(GraphPortSpec), intent(in) :: this
      type(GraphValueSpec) :: spec

      spec = this%value_spec_
   end function value_spec

end module mapl_GraphTransform_mod
