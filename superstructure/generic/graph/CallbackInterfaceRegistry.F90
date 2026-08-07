#include "MAPL.h"
module mapl_CallbackInterfaceRegistry_mod

   use mapl_CallbackInterface, only: CallbackInterface
   use mapl_CallbackInterfaceId_mod, only: &
      CallbackInterfaceId, &
      CallbackInterfaceIdGenerator, &
      INVALID_CALLBACK_INTERFACE_ID

   ! Illustrative gFTL-generated containers:
   !
   !   CallbackInterfaceId -> CallbackInterface
   !   character(*)        -> CallbackInterfaceId
   use mapl_CallbackInterfaceId_CallbackInterfaceMap
   use mapl_String_CallbackInterfaceIdMap

   implicit none(type, external)
   private

   public :: InterfaceRegistry
   public :: get_interface_registry

   type :: InterfaceRegistry
      private

      type(CallbackInterfaceId_CallbackInterfaceMap) :: interfaces_
      type(String_CallbackInterfaceIdMap)            :: names_
      type(CallbackInterfaceIdGenerator)             :: id_generator_

   contains

      procedure :: add
      procedure :: contains
      procedure :: find
      procedure :: get
      procedure :: validate

   end type InterfaceRegistry

   type(InterfaceRegistry), target :: registry_

contains

   function add(this, service_name, interface, rc) result(interface_id)
      class(InterfaceRegistry), intent(inout) :: this
      character(*), intent(in) :: service_name
      type(CallbackInterface), intent(in) :: interface
      integer, optional, intent(out) :: rc

      type(CallbackInterfaceId) :: interface_id
      integer :: status

      ! Always define the result on failure.
      interface_id = INVALID_CALLBACK_INTERFACE_ID

      _ASSERT(len_trim(service_name) > 0, &
         'Callback interface service name cannot be empty.')

      _ASSERT(.not. this%contains(service_name), &
         'Callback interface is already registered: ' // trim(service_name))

      interface_id = this%id_generator_%next(_RC)

      ! Insert authoritative storage first.
      call this%interfaces_%insert(interface_id, interface)

      ! Add the human-readable lookup index.
      call this%names_%insert(trim(service_name), interface_id)

      _RETURN(_SUCCESS)
   end function add


   pure logical function contains(this, service_name) result(found)
      class(InterfaceRegistry), intent(in) :: this
      character(*), intent(in) :: service_name

      found = this%names_%count(trim(service_name)) > 0
   end function contains


   function find(this, service_name, rc) result(interface_id)
      class(InterfaceRegistry), target, intent(in) :: this
      character(*), intent(in) :: service_name
      integer, optional, intent(out) :: rc

      type(CallbackInterfaceId) :: interface_id
      integer :: status

      ! Return an invalid ID when lookup fails.
      interface_id = INVALID_CALLBACK_INTERFACE_ID

      _ASSERT(this%contains(service_name), &
         'Unknown callback interface: ' // trim(service_name))

      interface_id = this%names_%at(trim(service_name))

      _RETURN(_SUCCESS)
   end function find


   function get(this, interface_id, rc) result(interface)
      class(InterfaceRegistry), target, intent(in) :: this
      type(CallbackInterfaceId), intent(in) :: interface_id
      integer, optional, intent(out) :: rc

      type(CallbackInterface), pointer :: interface
      integer :: status

      ! This pointer is borrowed and should only be used temporarily.
      nullify(interface)

      _ASSERT(interface_id%is_valid(), &
         'Invalid CallbackInterfaceId.')

      _ASSERT(this%interfaces_%count(interface_id) > 0, &
         'CallbackInterfaceId is not registered.')

      interface => this%interfaces_%at(interface_id)

      _RETURN(_SUCCESS)
   end function get


   subroutine validate(this, rc)
      class(InterfaceRegistry), target, intent(in) :: this
      integer, optional, intent(out) :: rc

      type(String_CallbackInterfaceIdMapIterator) :: iter
      type(CallbackInterfaceId) :: interface_id
      integer :: status

      ! Verify that every service-name index entry identifies an
      ! interface in the authoritative ID-keyed map.
      associate (map_end => this%names_%ftn_end())
         iter = this%names_%ftn_begin()

         do while (iter /= map_end)
            interface_id = iter%second()

            _ASSERT(interface_id%is_valid(), &
               'Interface registry contains an invalid ID.')

            _ASSERT(this%interfaces_%count(interface_id) > 0, &
               'Interface name index refers to an unknown ID.')

            call iter%next()
         end do
      end associate

      ! Because add() inserts exactly one entry into each map, equal
      ! sizes establish the reverse direction as well.
      _ASSERT(this%names_%size() == this%interfaces_%size(), &
         'Callback interface registry indexes are inconsistent.')

      _RETURN(_SUCCESS)
   end subroutine validate

end module mapl_CallbackInterfaceRegistry_mod
