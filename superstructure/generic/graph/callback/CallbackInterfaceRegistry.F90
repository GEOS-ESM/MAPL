#include "MAPL.h"

!------------------------------------------------------------------------------
! CallbackInterfaceRegistry: the one shared, cross-ComponentGraph lookup
! point for CallbackInterface values (spec/15-callbacks.md REQ-CB-008/
! 009/010) - "callback interfaces MUST be shared across component
! graphs... a root-owned registry with pointers threaded down to
! descendants is awkward" (REQ-CB-008).
!
! A private derived type (CallbackInterfaceRegistry) owns a
! CallbackInterfaceId -> CallbackInterface map, a service name ->
! CallbackInterfaceId map, and one CallbackInterfaceIdGenerator (already
! available for free from CallbackInterfaceId's own IdTemplate.inc-
! generated CallbackInterfaceIdGenerator), backed by a single private
! module-level singleton instance. Callers never see the type itself -
! only the public wrapper procedures below (REQ-CB-010's explicit
! "narrower wrapper procedures that hide the singleton" option) -
! generalizing ExtensionResolution.F90's own single-flag module-level-
! state precedent to a full owned-state singleton
! (callback-data-model-registry design.md Decision 4).
!
! register_callback_interface rejects a duplicate service_name
! (no-silent-replace, the same convention used throughout this module
! family), leaving the original registration's interface/id unchanged.
!------------------------------------------------------------------------------
module mapl_CallbackInterfaceRegistry_mod
   use mapl_CallbackInterfaceId_mod, only: CallbackInterfaceId, CallbackInterfaceIdGenerator
   use mapl_CallbackInterface_mod, only: CallbackInterface
   use mapl_CallbackInterfaceIdInterfaceMap_mod, only: CallbackInterfaceIdInterfaceMap
   use mapl_CallbackServiceNameIdMap_mod, only: CallbackServiceNameIdMap
   use mapl_ErrorHandling_mod
   implicit none(type, external)
   private

   public :: register_callback_interface
   public :: get_callback_interface
   public :: get_callback_interface_by_name
   public :: lookup_callback_interface_id

   type :: CallbackInterfaceRegistryType
      private
      type(CallbackInterfaceIdInterfaceMap) :: interfaces
      type(CallbackServiceNameIdMap) :: service_ids
      type(CallbackInterfaceIdGenerator) :: id_generator
   end type CallbackInterfaceRegistryType

   ! The one shared instance (REQ-CB-008). Not exposed outside this
   ! module - every external interaction goes through the wrapper
   ! procedures below.
   type(CallbackInterfaceRegistryType), save, target :: the_registry

contains

   function get_registry() result(registry)
      type(CallbackInterfaceRegistryType), pointer :: registry

      registry => the_registry
   end function get_registry

   ! REQ-CB-009/010: assigns a new, unique CallbackInterfaceId and
   ! records iface under both maps atomically. Rejects a duplicate
   ! service_name (spec scenario "Duplicate service name is rejected").
   subroutine register_callback_interface(service_name, iface, id, rc)
      character(*), intent(in) :: service_name
      type(CallbackInterface), intent(in) :: iface
      type(CallbackInterfaceId), intent(out) :: id
      integer, optional, intent(out) :: rc

      type(CallbackInterfaceRegistryType), pointer :: registry
      integer :: status

      registry => get_registry()

      _ASSERT(registry%service_ids%count(service_name) == 0, 'CallbackInterfaceRegistry: service name already registered')

      id = registry%id_generator%next(status)
      _VERIFY(status)

      call registry%interfaces%insert(id, iface)
      call registry%service_ids%insert(service_name, id)

      _RETURN(_SUCCESS)
   end subroutine register_callback_interface

   ! Spec scenario: "Registered interface is retrievable ... by the
   ! identity assigned at registration".
   function get_callback_interface(id, rc) result(iface)
      type(CallbackInterfaceId), intent(in) :: id
      integer, optional, intent(out) :: rc
      type(CallbackInterface) :: iface

      type(CallbackInterfaceRegistryType), pointer :: registry
      type(CallbackInterface), pointer :: found

      registry => get_registry()
      found => registry%interfaces%at(id)
      _ASSERT(associated(found), 'CallbackInterfaceRegistry: get_callback_interface - id not registered')
      iface = found

      _RETURN(_SUCCESS)
   end function get_callback_interface

   ! Spec scenario: "Lookup of an unregistered service name fails
   ! explicitly".
   function lookup_callback_interface_id(service_name, rc) result(id)
      character(*), intent(in) :: service_name
      integer, optional, intent(out) :: rc
      type(CallbackInterfaceId) :: id

      type(CallbackInterfaceRegistryType), pointer :: registry
      type(CallbackInterfaceId), pointer :: found

      registry => get_registry()
      found => registry%service_ids%at(service_name)
      _ASSERT(associated(found), 'CallbackInterfaceRegistry: lookup_callback_interface_id - service name not registered')
      id = found

      _RETURN(_SUCCESS)
   end function lookup_callback_interface_id

   ! Spec scenario: "Registered interface is retrievable ... by the
   ! service name it was registered under" - composes the two lookups
   ! above.
   function get_callback_interface_by_name(service_name, rc) result(iface)
      character(*), intent(in) :: service_name
      integer, optional, intent(out) :: rc
      type(CallbackInterface) :: iface

      type(CallbackInterfaceId) :: id
      integer :: status

      id = lookup_callback_interface_id(service_name, status)
      _VERIFY(status)

      iface = get_callback_interface(id, status)
      _VERIFY(status)

      _RETURN(_SUCCESS)
   end function get_callback_interface_by_name

end module mapl_CallbackInterfaceRegistry_mod
