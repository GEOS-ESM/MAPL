#include "MAPL.h"

module mapl3g_StateRegistry
   use mapl3g_Field_API
   use mapl3g_AbstractRegistry
   use mapl3g_RegistryPtr
   use mapl3g_RegistryPtrMap
   use mapl3g_VirtualConnectionPt
   use mapl3g_VirtualConnectionPtVector
   use mapl3g_ConnectionPt
   use mapl3g_StateItemExtension
   use mapl3g_StateItemExtensionVector
   use mapl3g_StateItemExtensionPtrVector
   use mapl3g_ExtensionFamily
   use mapl3g_VirtualPtFamilyMap
   use mapl3g_StateItemVector
   use mapl3g_StateItemSpec
   use mapl3g_ComponentDriver
   use mapl3g_ComponentDriverVector
   use mapl3g_ComponentDriverPtrVector
   use mapl3g_GriddedComponentDriver
   use mapl3g_VerticalGrid
   use mapl_ErrorHandling
   use esmf, only: ESMF_Geom, ESMF_TimeInterval

   implicit none(type,external)
   private

   public :: StateRegistry

   type, extends(AbstractRegistry) :: StateRegistry
      private
      character(:), allocatable :: name
      type(StateItemExtensionVector) :: owned_items ! specs and couplers
      type(RegistryPtrMap) :: subregistries

      type(VirtualPtFamilyMap) :: family_map

   contains

      procedure :: add_subregistry
      procedure :: add_virtual_pt
      procedure :: add_primary_spec
      procedure :: link_extension
      procedure :: add_extension
      procedure :: add_spec
      procedure :: add_family

      procedure :: propagate_unsatisfied_imports_all
      procedure :: propagate_unsatisfied_imports_subregistry
      procedure :: propagate_unsatisfied_imports_virtual_pt
      generic :: propagate_unsatisfied_imports => propagate_unsatisfied_imports_all
      generic :: propagate_unsatisfied_imports => propagate_unsatisfied_imports_subregistry
      generic :: propagate_unsatisfied_imports => propagate_unsatisfied_imports_virtual_pt

      procedure :: propagate_exports_all
      procedure :: propagate_exports_subregistry
      procedure :: propagate_exports_virtual_pt
      generic :: propagate_exports => propagate_exports_all
      generic :: propagate_exports => propagate_exports_subregistry
      generic :: propagate_exports => propagate_exports_virtual_pt

      procedure :: get_name
      procedure :: has_virtual_pt
      procedure :: num_owned_items
      procedure :: get_extension_family
      procedure :: get_extensions
      procedure :: get_primary_extension

      procedure :: has_subregistry
      procedure :: get_subregistry_by_name
      procedure :: get_subregistry_by_conn_pt
      generic :: get_subregistry => get_subregistry_by_name
      generic :: get_subregistry => get_subregistry_by_conn_pt

      ! Actions on specs
      procedure :: allocate_items
      procedure :: add_to_states

      procedure :: filter ! for MatchConnection

      procedure :: get_export_couplers
      procedure :: get_import_couplers

      procedure :: write_formatted
      generic :: write(formatted) => write_formatted

      procedure :: extend
      procedure :: item_is_deferred

   end type StateRegistry

   interface StateRegistry
      module function new_StateRegistry(name) result(r)
         type(StateRegistry) :: r
         character(*), intent(in) :: name
      end function new_StateRegistry
   end interface StateRegistry

   character(*), parameter :: SELF = "<self>"

   interface
      ! Lifecycle procedures
      module function get_name(this) result(name)
         character(:), allocatable :: name
         class(StateRegistry), intent(in) :: this
      end function get_name

      module function has_virtual_pt(this, virtual_pt) result(has_pt)
         logical :: has_pt
         class(StateRegistry), intent(in) :: this
         type(VirtualConnectionPt), intent(in) :: virtual_pt
      end function has_virtual_pt

      module function num_owned_items(this) result(num_items)
         integer :: num_items
         class(StateRegistry), intent(in) :: this
      end function num_owned_items

      module subroutine write_formatted(this, unit, iotype, v_list, iostat, iomsg)
         class(StateRegistry), intent(in) :: this
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in) :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg
      end subroutine write_formatted

      ! Hierarchy procedures
      module subroutine add_subregistry(this, subregistry, rc)
         class(StateRegistry), target, intent(inout) :: this
         class(StateRegistry), target, intent(in) :: subregistry
         integer, optional, intent(out) :: rc
      end subroutine add_subregistry

      module function has_subregistry(this, name) result(has_sub)
         logical :: has_sub
         class(StateRegistry), intent(in) :: this
         character(len=*), intent(in) :: name
      end function has_subregistry

      module function get_subregistry_by_name(this, name, rc) result(subregistry)
         type(StateRegistry), pointer :: subregistry
         class(StateRegistry), target, intent(in) :: this
         character(len=*), intent(in) :: name
         integer, optional, intent(out) :: rc
      end function get_subregistry_by_name

      module function get_subregistry_by_conn_pt(this, conn_pt, rc) result(subregistry)
         type(StateRegistry), pointer :: subregistry
         class(StateRegistry), target, intent(in) :: this
         type(ConnectionPt), intent(in) :: conn_pt
         integer, optional, intent(out) :: rc
      end function get_subregistry_by_conn_pt

      ! Extensions procedures
      module subroutine add_virtual_pt(this, virtual_pt, rc)
         class(StateRegistry), target, intent(inout) :: this
         type(VirtualConnectionPt), intent(in) :: virtual_pt
         integer, optional, intent(out) :: rc
      end subroutine add_virtual_pt

      module subroutine add_family(this, virtual_pt, family, rc)
         class(StateRegistry), target, intent(inout) :: this
         type(VirtualConnectionPt), intent(in) :: virtual_pt
         type(ExtensionFamily), intent(in) :: family
         integer, optional, intent(out) :: rc
      end subroutine add_family

      module subroutine add_primary_spec(this, virtual_pt, spec, rc)
         class(StateRegistry), target, intent(inout) :: this
         type(VirtualConnectionPt), intent(in) :: virtual_pt
         type(StateItemSpec), intent(in) :: spec
         integer, optional, intent(out) :: rc
      end subroutine add_primary_spec

      module function get_primary_extension(this, virtual_pt, rc) result(primary)
         type(StateItemExtension), pointer :: primary
         class(StateRegistry), target, intent(in) :: this
         type(VirtualConnectionPt), intent(in) :: virtual_pt
         integer, optional, intent(out) :: rc
      end function get_primary_extension

      module function add_extension(this, virtual_pt, extension, rc) result(new_extension)
         type(StateItemExtension), pointer :: new_extension
         class(StateRegistry), target, intent(inout) :: this
         type(VirtualConnectionPt), intent(in) :: virtual_pt
         type(StateItemExtension), intent(in) :: extension
         integer, optional, intent(out) :: rc
      end function add_extension

      module subroutine add_spec(this, virtual_pt, spec, rc)
         class(StateRegistry), target, intent(inout) :: this
         type(VirtualConnectionPt), intent(in) :: virtual_pt
         type(StateItemSpec), intent(in) :: spec
         integer, optional, intent(out) :: rc
      end subroutine add_spec

      module subroutine link_extension(this, virtual_pt, extension, rc)
         class(StateRegistry), target, intent(inout) :: this
         type(VirtualConnectionPt), intent(in) :: virtual_pt
         type(StateItemExtension), pointer, intent(in) :: extension
         integer, optional, intent(out) :: rc
      end subroutine link_extension

      module function get_extension_family(this, virtual_pt, rc) result(family)
         type(ExtensionFamily), pointer :: family
         class(StateRegistry), target, intent(in) :: this
         type(VirtualConnectionPt), intent(in) :: virtual_pt
         integer, optional, intent(out) :: rc
      end function get_extension_family

      module function get_extensions(this, virtual_pt, rc) result(extensions)
         type(StateItemExtensionPtr), allocatable :: extensions(:)
         class(StateRegistry), target, intent(in) :: this
         type(VirtualConnectionPt), intent(in) :: virtual_pt
         integer, optional, intent(out) :: rc
      end function get_extensions

      recursive module function extend(registry, v_pt, goal_spec, rc) result(extension)
         type(StateItemExtension), pointer :: extension
         class(StateRegistry), target, intent(inout) :: registry
         type(VirtualConnectionPt), intent(in) :: v_pt
         type(StateItemSpec), intent(in) :: goal_spec
         integer, optional, intent(out) :: rc
      end function extend

      module function item_is_deferred(this, v_pt, rc) result(is_deferred)
         logical :: is_deferred
         class(StateRegistry), target, intent(in) :: this
         type(VirtualConnectionPt), intent(in) :: v_pt
         integer, optional, intent(out) :: rc
      end function item_is_deferred

      ! Propagation procedures
      module subroutine propagate_unsatisfied_imports_all(this, rc)
         class(StateRegistry), target, intent(inout) :: this
         integer, optional, intent(out) :: rc
      end subroutine propagate_unsatisfied_imports_all

      module subroutine propagate_unsatisfied_imports_subregistry(this, subregistry, rc)
         class(StateRegistry), target, intent(inout) :: this
         class(StateRegistry), target, intent(in) :: subregistry
         integer, optional, intent(out) :: rc
      end subroutine propagate_unsatisfied_imports_subregistry

      module subroutine propagate_unsatisfied_imports_virtual_pt(this, virtual_pt, family, rc)
         class(StateRegistry), target, intent(inout) :: this
         type(VirtualConnectionPt), intent(in) :: virtual_pt
         type(ExtensionFamily), intent(in) :: family
         integer, optional, intent(out) :: rc
      end subroutine propagate_unsatisfied_imports_virtual_pt

      module subroutine propagate_exports_all(this, rc)
         class(StateRegistry), target, intent(inout) :: this
         integer, optional, intent(out) :: rc
      end subroutine propagate_exports_all

      module subroutine propagate_exports_subregistry(this, subregistry, rc)
         class(StateRegistry), target, intent(inout) :: this
         type(StateRegistry), target, intent(in) :: subregistry
         integer, optional, intent(out) :: rc
      end subroutine propagate_exports_subregistry

      module subroutine propagate_exports_virtual_pt(this, subregistry_name, iter, rc)
         use mapl3g_VirtualPtFamilyMap, only: VirtualPtFamilyMapIterator
         class(StateRegistry), target, intent(inout) :: this
         character(*), intent(in) :: subregistry_name
         type(VirtualPtFamilyMapIterator), intent(in) :: iter
         integer, optional, intent(out) :: rc
      end subroutine propagate_exports_virtual_pt

      ! Actions procedures
      module subroutine allocate_items(this, rc)
         class(StateRegistry), target, intent(inout) :: this
         integer, optional, intent(out) :: rc
      end subroutine allocate_items

      module subroutine add_to_states(this, multi_state, mode, rc)
         use mapl3g_MultiState
         class(StateRegistry), target, intent(inout) :: this
         type(MultiState), intent(inout) :: multi_state
         character(*), intent(in) :: mode
         integer, optional, intent(out) :: rc
      end subroutine add_to_states

      module function filter(this, pattern) result(matches)
         type(VirtualConnectionPtVector) :: matches
         class(StateRegistry), target, intent(in) :: this
         type(VirtualConnectionPt), intent(in) :: pattern
      end function filter

      module function get_export_couplers(this) result(export_couplers)
         type(ComponentDriverPtrVector) :: export_couplers
         class(StateRegistry), target, intent(in) :: this
      end function get_export_couplers

      module function get_import_couplers(this) result(import_couplers)
         type(ComponentDriverPtrVector) :: import_couplers
         class(StateRegistry), target, intent(in) :: this
      end function get_import_couplers

   end interface

end module mapl3g_StateRegistry
