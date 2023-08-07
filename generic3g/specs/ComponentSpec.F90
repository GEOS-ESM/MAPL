#include "MAPL_Generic.h"

module mapl3g_ComponentSpec
   use mapl3g_AbstractStateItemSpec
   use mapl3g_ConnectionVector
   use mapl3g_HierarchicalRegistry, only: Connection
   use mapl3g_VariableSpec
   use mapl3g_VariableSpecVector
   use mapl3g_ChildSpecMap
   use mapl_ErrorHandling
   use ESMF
   implicit none
   private

   public :: ComponentSpec

   type :: ComponentSpec
!!$      private
      type(VariableSpecVector) :: var_specs
      type(ConnectionVector) :: connections
      type(ChildSpecMap) :: children
   contains
      procedure :: add_var_spec
      procedure :: add_connection
   end type ComponentSpec

   interface ComponentSpec
      module procedure new_ComponentSpec
   end interface ComponentSpec

contains

   function new_ComponentSpec(var_specs, connections) result(spec)
      type(ComponentSpec) :: spec
      type(VariableSpecVector), optional, intent(in) :: var_specs
      type(ConnectionVector), optional, intent(in) :: connections

      if (present(var_specs)) spec%var_specs = var_specs
      if (present(connections)) spec%connections = connections
   end function new_ComponentSpec


   subroutine add_var_spec(this, var_spec)
      class(ComponentSpec), intent(inout) :: this
      class(VariableSpec), intent(in) :: var_spec
      call this%var_specs%push_back(var_spec)
   end subroutine add_var_spec


   subroutine add_connection(this, conn)
      class(ComponentSpec), intent(inout) :: this
      class(Connection), intent(in) :: conn
      call this%connections%push_back(conn)
   end subroutine add_connection




end module mapl3g_ComponentSpec

