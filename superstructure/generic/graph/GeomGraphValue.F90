module mapl_GeomGraphValue_mod
   use mapl_GeomSpec_mod, only: GeomSpec
   use mapl_GraphValue_mod, only: GraphValue
   use mapl_GraphValueSpec_mod, only: GraphValueSpec
   implicit none(type, external)
   private

   public :: GeomGraphValue

   type, extends(GraphValue) :: GeomGraphValue
      private
      character(:), allocatable :: name_
      class(GeomSpec), allocatable :: geom_spec_
   contains
      procedure :: name
      procedure :: geom_spec
      procedure :: spec
   end type GeomGraphValue

   interface GeomGraphValue
      module procedure new_geom_graph_value
   end interface GeomGraphValue

contains

   function new_geom_graph_value(geom_spec, name) result(value)
      class(GeomSpec), intent(in) :: geom_spec
      character(*), optional, intent(in) :: name
      type(GeomGraphValue) :: value

      if (present(name)) then
         value%name_ = name
      else
         value%name_ = geom_spec%get_name()
      end if
      allocate(value%geom_spec_, source=geom_spec)
   end function new_geom_graph_value

   function name(this) result(value)
      class(GeomGraphValue), intent(in) :: this
      character(:), allocatable :: value

      value = this%name_
   end function name

   function geom_spec(this) result(value)
      class(GeomGraphValue), intent(in) :: this
      class(GeomSpec), allocatable :: value

      allocate(value, source=this%geom_spec_)
   end function geom_spec

   function spec(this) result(value)
      class(GeomGraphValue), intent(in) :: this
      type(GraphValueSpec) :: value

      value = GraphValueSpec('geom', this%name())
   end function spec

end module mapl_GeomGraphValue_mod
