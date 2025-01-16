#include "MAPL_ErrLog.h"

module mapl3g_ComponentSpecParser

   use mapl3g_ComponentSpec
   use mapl3g_ChildSpec
   use mapl3g_ChildSpecMap
   use mapl3g_UserSetServices
   use mapl_ErrorHandling
   use mapl3g_VariableSpec
   use mapl3g_Connection
   use mapl3g_ConnectionPt
   use mapl3g_VirtualConnectionPt
   use mapl3g_VariableSpecVector
   use mapl3g_SimpleConnection
   use mapl3g_MatchConnection
   use mapl3g_ReexportConnection
   use mapl3g_ConnectionVector
   use mapl3g_VerticalDimSpec
   use mapl3g_UngriddedDims
   use mapl3g_UngriddedDim
   use mapl3g_GeometrySpec
   use mapl3g_geom_mgr
   use mapl3g_Stateitem
   use mapl3g_ESMF_Utilities
   use mapl3g_UserSetServices
   use mapl3g_StateRegistry
   use gftl2_StringVector, only: StringVector
   use esmf

   implicit none
   private

   public :: parse_component_spec

   ! The following interfaces are public only for testing purposes.
   public :: parse_children
   public :: parse_child
   public :: parse_SetServices
   public :: parse_geometry_spec
   public :: parse_timestep

!!$   public :: parse_ChildSpecMap
!!$   public :: parse_ChildSpec

   character(*), parameter :: MAPL_SECTION = 'mapl'
   character(*), parameter :: COMPONENT_GEOMETRY_SECTION = 'geometry'
   character(*), parameter :: COMPONENT_ESMF_GEOM_SECTION = 'esmf_geom'
   character(*), parameter :: COMPONENT_VERTICAL_GRID_SECTION = 'vertical_grid'
   character(*), parameter :: COMPONENT_VERTGEOM_SECTION = 'vert_geom'
   character(*), parameter :: COMPONENT_STATES_SECTION = 'states'
   character(*), parameter :: COMPONENT_IMPORT_STATE_SECTION = 'import'
   character(*), parameter :: COMPONENT_EXPORT_STATE_SECTION = 'export'
   character(*), parameter :: COMPONENT_INTERNAL_STATE_SECTION = 'internal'
   character(*), parameter :: COMPONENT_CONNECTIONS_SECTION = 'connections'
   character(*), parameter :: COMPONENT_CHILDREN_SECTION = 'children'

   character(*), parameter :: KEY_DEFAULT_VALUE = 'default_value'
   character(*), parameter :: KEY_UNGRIDDED_DIMS = 'ungridded_dims'
   character(*), parameter :: KEY_UNGRIDDED_DIM_NAME = 'dim_name'
   character(*), parameter :: KEY_UNGRIDDED_DIM_UNITS = 'dim_units'
   character(*), parameter :: KEY_UNGRIDDED_DIM_EXTENT = 'extent'
   character(*), parameter :: KEY_UNGRIDDED_DIM_COORDINATES = 'coordinates'
   character(*), parameter :: KEY_VERTICAL_DIM_SPEC = 'vertical_dim_spec'
   character(*), parameter :: KEY_ACCUMULATION_TYPE = 'accumulation_type'
   character(*), parameter :: KEY_TIMESTEP = 'timestep'

   !>
   ! Submodule declarations
   INTERFACE
      module function parse_component_spec(hconfig, registry, refTime, timeStep, rc) result(spec)
         type(ComponentSpec) :: spec
         type(ESMF_HConfig), target, intent(inout) :: hconfig
         type(StateRegistry), target, intent(in) :: registry
         type(ESMF_Time), intent(in) :: refTime
         type(ESMF_TimeInterval), intent(in) :: timeStep
         integer, optional, intent(out) :: rc
      end function parse_component_spec

      module function parse_geometry_spec(mapl_cfg, registry, rc) result(geometry_spec)
         type(GeometrySpec) :: geometry_spec
         type(ESMF_HConfig), intent(in) :: mapl_cfg
         type(StateRegistry), optional, target, intent(in) :: registry
         integer, optional, intent(out) :: rc
      end function parse_geometry_spec

      module function parse_var_specs(hconfig, timestep, rc) result(var_specs)
         type(VariableSpecVector) :: var_specs
         type(ESMF_HConfig), intent(in) :: hconfig
         type(ESMF_TimeInterval), intent(in) :: timestep
         integer, optional, intent(out) :: rc
      end function parse_var_specs

      module function parse_connections(hconfig, rc) result(connections)
         type(ConnectionVector) :: connections
         type(ESMF_HConfig), optional, intent(in) :: hconfig
         integer, optional, intent(out) :: rc
      end function parse_connections

      module function parse_setservices(config, rc) result(user_ss)
         type(DSOSetServices) :: user_ss
         type(ESMF_HConfig), target, intent(in) :: config
         integer, optional, intent(out) :: rc
      end function parse_setservices

      module function parse_children(hconfig, rc) result(children)
         type(ChildSpecMap) :: children
         type(ESMF_HConfig), intent(in) :: hconfig
         integer, optional, intent(out) :: rc
      end function parse_children

      module function parse_child(hconfig, rc) result(child)
         type(ChildSpec) :: child
         type(ESMF_HConfig), intent(in) :: hconfig
         integer, optional, intent(out) :: rc
      end function parse_child

      module subroutine parse_timestep(hconfig, timestep, rc)
         type(ESMF_HConfig), intent(in) :: hconfig
         type(ESMF_TimeInterval),intent(inout) :: timestep
         integer, optional, intent(out) :: rc
      end subroutine parse_timestep

   END INTERFACE

end module mapl3g_ComponentSpecParser
