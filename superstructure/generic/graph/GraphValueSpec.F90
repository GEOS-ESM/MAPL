module mapl_GraphValueSpec_mod
   implicit none(type, external)
   private

   public :: GraphValueSpec
   public :: operator(==), operator(/=)

   type :: GraphValueSpec
      private
      character(:), allocatable :: kind_
      character(:), allocatable :: metadata_
   contains
      procedure :: kind
      procedure :: metadata
   end type GraphValueSpec

   interface GraphValueSpec
      module procedure new_graph_value_spec
   end interface GraphValueSpec

   interface operator(==)
      module procedure equal
   end interface

   interface operator(/=)
      module procedure not_equal
   end interface

contains

   function new_graph_value_spec(kind, metadata) result(spec)
      character(*), intent(in) :: kind
      character(*), optional, intent(in) :: metadata
      type(GraphValueSpec) :: spec

      spec%kind_ = kind
      if (present(metadata)) spec%metadata_ = metadata
   end function new_graph_value_spec

   function kind(this) result(value)
      class(GraphValueSpec), intent(in) :: this
      character(:), allocatable :: value

      value = this%kind_
   end function kind

   function metadata(this) result(value)
      class(GraphValueSpec), intent(in) :: this
      character(:), allocatable :: value

      if (allocated(this%metadata_)) then
         value = this%metadata_
      else
         value = ''
      end if
   end function metadata

   logical function equal(lhs, rhs)
      type(GraphValueSpec), intent(in) :: lhs, rhs

      equal = lhs%kind() == rhs%kind() .and. lhs%metadata() == rhs%metadata()
   end function equal

   logical function not_equal(lhs, rhs)
      type(GraphValueSpec), intent(in) :: lhs, rhs

      not_equal = .not. lhs == rhs
   end function not_equal

end module mapl_GraphValueSpec_mod
