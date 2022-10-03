module mapl_ComponentRegistry
   implicit none
   private
  
   public :: ComponentRegistry
  
   type :: ComponentRegistry
      private
      type(StringComponentSpecMap) :: map
   contains
      procedure :: add_component
      procedure :: get_spec
   end type ComponentRegistry
  
contains

   function add_component(this, name) result(spec)
      type(ComponentSpec), pointer :: comp_spec
      class(ComponentRegistry), intent(inout) :: this
      character(len=*), intent(in) :: name

      type(ComponentSpec) :: stub
      
      call this%map%insert(name, stub)
      spec => this%get_spec(name)

   end function add_component

   pure function get_spec(this, name) result(spec)
      type(ComponentSpec), pointer :: comp_spec
      class(ComponentRegistry), intent(in) :: this
      character(len=*), intent(in) :: name

      spec => this%map%of(name)
   end function get_spec
      
  
end module mapl_ComponentRegistry

