#include "MAPL_Generic.h"

module mapl3g_ComponentSpec
   use mapl3g_ConnectionVector
   use mapl3g_Connection
   use mapl3g_VariableSpec
   use mapl3g_VariableSpecVector
   use mapl3g_ChildSpecMap
   use mapl3g_GeometrySpec
   use mapl_ErrorHandling
   use ESMF
   implicit none
   private

   public :: ComponentSpec

   type :: ComponentSpec
      !!$      private
      type(GeometrySpec) :: geometry_spec
      type(VariableSpecVector) :: var_specs
      type(ConnectionVector) :: connections
      type(ChildSpecMap) :: children
      type(ESMF_HConfig), allocatable :: geom_hconfig ! optional
      type(ESMF_TimeInterval), allocatable :: timestep
      type(ESMF_Time), allocatable :: reference_time
      logical :: activate_all_exports = .false. ! used for testing in isolation
      logical :: activate_all_imports = .false. ! used for testing in isolation

   contains
      procedure :: has_geom_hconfig
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

   logical function has_geom_hconfig(this)
      class(ComponentSpec), intent(in) :: this
      has_geom_hconfig = allocated(this%geom_hconfig)
   end function has_geom_hconfig

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
