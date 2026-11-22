#include "MAPL.h"

!------------------------------------------------------------------------------
! CallbackInterfaceRegistry: the one shared, cross-ComponentGraph lookup
! point for CallbackInterface values (spec/15-callbacks.md) -
! "callback interfaces MUST be shared across component
! graphs... a root-owned registry with pointers threaded down to
! descendants is awkward".
!
! A private derived type (CallbackInterfaceRegistry) owns a
! CallbackInterfaceId -> CallbackInterface map, a service name ->
! CallbackInterfaceId map, and one CallbackInterfaceIdGenerator (already
! available for free from CallbackInterfaceId's own IdTemplate.inc-
! generated CallbackInterfaceIdGenerator), backed by a single private
! module-level singleton instance. Callers never see the type itself -
! only the public wrapper procedures below (narrower wrapper
! procedures that hide the singleton) -
! generalizing ExtensionResolution.F90's own single-flag module-level-
! state precedent to a full owned-state singleton.
!
! register_callback_interface rejects a duplicate service_name
! (no-silent-replace, the same convention used throughout this module
! family), leaving the original registration's interface/id unchanged,
! and a service_name containing '/' (the interface's own name has no
! other home, so its slash-free rule is checked here).
! freeze_callback_registry ends registration once the interface set is
! settled; list_callback_interface_names enumerates what was registered.
!
! mark_callback_registry_no_file/is_callback_registry_no_file record
! whether the registration file was found at load time: the loader
! calls mark_callback_registry_no_file when no file exists, so an
! empty registry that never had a file to load can be told apart from
! one a file loaded with zero entries into.
!
! mark_callback_registry_file_loaded records the path and CRC-32 of the
! file the loader read; make_callback_registry_report turns that (or the
! "no file" mark) plus every registered interface, in sorted name order
! with its version and summary, into the lines of the startup report.
!
! lookup_callback_interface_id gives a distinct message when it is
! about to fail, the registry is marked "no file", and nothing at all
! is registered: "no callback_interfaces.yaml found in <cwd>". Both
! conditions are required, not the "no file" mark alone, so a file that
! was found (even an empty one, or one missing this particular name)
! always gets the plain "service name not registered" message instead
! - the specific message names a missing file as the cause, which is
! only true when no entries came from anywhere.
!------------------------------------------------------------------------------
module mapl_CallbackInterfaceRegistry_mod
   use mapl_CallbackInterfaceId_mod, only: CallbackInterfaceId, CallbackInterfaceIdGenerator
   use mapl_CallbackInterface_mod, only: CallbackInterface
   use mapl_CallbackInterfaceIdInterfaceMap_mod, only: CallbackInterfaceIdInterfaceMap
   use mapl_CallbackServiceNameIdMap_mod, only: CallbackServiceNameIdMap, &
        CallbackServiceNameIdMapIterator, operator(/=)
   use mapl_StringSortUtilities_mod, only: sorted_names
   use mapl_os_mod, only: GetCurrentWorkingDirectory
   use mapl_ErrorHandling_mod
   use gFTL2_StringVector, only: StringVector
   implicit none(type, external)
   private

   public :: register_callback_interface
   public :: get_callback_interface
   public :: get_callback_interface_by_name
   public :: lookup_callback_interface_id
   public :: freeze_callback_registry
   public :: list_callback_interface_names
   public :: reset_callback_registry_for_testing
   public :: mark_callback_registry_no_file
   public :: is_callback_registry_no_file
   public :: mark_callback_registry_file_loaded
   public :: make_callback_registry_report

   type :: CallbackInterfaceRegistryType
      private
      type(CallbackInterfaceIdInterfaceMap) :: interfaces
      type(CallbackServiceNameIdMap) :: service_ids
      type(CallbackInterfaceIdGenerator) :: id_generator
      logical :: frozen = .false.
      logical :: no_file = .false.
      character(:), allocatable :: file_path
      character(:), allocatable :: file_checksum
   end type CallbackInterfaceRegistryType

   ! The one shared instance. Not exposed outside this
   ! module - every external interaction goes through the wrapper
   ! procedures below.
   type(CallbackInterfaceRegistryType), save, target :: the_registry

contains

   function get_registry() result(registry)
      type(CallbackInterfaceRegistryType), pointer :: registry

      registry => the_registry
   end function get_registry

   ! Assigns a new, unique CallbackInterfaceId and
   ! records iface under both maps atomically. Rejects a duplicate
   ! service_name (spec scenario "Duplicate service name is rejected").
   subroutine register_callback_interface(service_name, iface, id, rc)
      character(*), intent(in) :: service_name
      type(CallbackInterface), intent(in) :: iface
      type(CallbackInterfaceId), intent(out) :: id
      integer, optional, intent(out) :: rc

      type(CallbackInterfaceRegistryType), pointer :: registry
      integer :: status
      character(:), allocatable :: message

      registry => get_registry()

      if (registry%frozen) then
         message = 'CallbackInterfaceRegistry: registry is frozen, ' // &
              'cannot register interface ' // service_name
         _FAIL(message)
      end if

      message = 'CallbackInterfaceRegistry: interface name must not ' // &
           'contain "/": ' // service_name
      _ASSERT(index(service_name, '/') == 0, message)

      _ASSERT(registry%service_ids%count(service_name) == 0, 'CallbackInterfaceRegistry: service name already registered')

      id = registry%id_generator%next(status)
      _VERIFY(status)

      call registry%interfaces%insert(id, iface)
      call registry%service_ids%insert(service_name, id)

      _RETURN(_SUCCESS)
   end subroutine register_callback_interface

   ! Registration fails from now on; lookups and enumeration keep working.
   ! Freezing a frozen registry changes nothing.
   subroutine freeze_callback_registry()
      type(CallbackInterfaceRegistryType), pointer :: registry

      registry => get_registry()
      registry%frozen = .true.
   end subroutine freeze_callback_registry

   ! Every registered service name in sorted order. Entries are blank-padded
   ! to the longest name; callers trim.
   function list_callback_interface_names() result(names)
      character(:), allocatable :: names(:)

      type(CallbackInterfaceRegistryType), pointer :: registry
      type(StringVector) :: unsorted
      type(CallbackServiceNameIdMap), pointer :: ids
      type(CallbackServiceNameIdMapIterator) :: iter

      registry => get_registry()
      ids => registry%service_ids
      associate (e => ids%ftn_end())
         iter = ids%ftn_begin()
         do while (iter /= e)
            call iter%next()
            call unsorted%push_back(iter%first())
         end do
      end associate

      names = sorted_names(unsorted)
   end function list_callback_interface_names

   ! Test-only: clears every registration and unfreezes, so tests that
   ! register or freeze in code stay independent. No production caller.
   subroutine reset_callback_registry_for_testing()
      type(CallbackInterfaceRegistryType), pointer :: registry

      registry => get_registry()
      call registry%interfaces%clear()
      call registry%service_ids%clear()
      registry%id_generator = CallbackInterfaceIdGenerator()
      registry%frozen = .false.
      registry%no_file = .false.
      if (allocated(registry%file_path)) deallocate(registry%file_path)
      if (allocated(registry%file_checksum)) deallocate(registry%file_checksum)
   end subroutine reset_callback_registry_for_testing

   ! Records that the registration file was not found, so an empty
   ! registry that never had a file to load can be told apart from one
   ! a file loaded with zero entries into. Set by the loader;
   ! registering an interface does not clear it.
   subroutine mark_callback_registry_no_file()
      type(CallbackInterfaceRegistryType), pointer :: registry

      registry => get_registry()
      registry%no_file = .true.
      if (allocated(registry%file_path)) deallocate(registry%file_path)
      if (allocated(registry%file_checksum)) deallocate(registry%file_checksum)
   end subroutine mark_callback_registry_no_file

   logical function is_callback_registry_no_file() result(no_file)
      type(CallbackInterfaceRegistryType), pointer :: registry

      registry => get_registry()
      no_file = registry%no_file
   end function is_callback_registry_no_file

   ! Records the file the loader read and its checksum, for the startup
   ! report. Set by the loader after a successful load. Clears the "no
   ! file" mark, as mark_callback_registry_no_file clears this one.
   subroutine mark_callback_registry_file_loaded(path, checksum)
      character(*), intent(in) :: path
      character(*), intent(in) :: checksum

      type(CallbackInterfaceRegistryType), pointer :: registry

      registry => get_registry()
      registry%file_path = path
      registry%file_checksum = checksum
      registry%no_file = .false.
   end subroutine mark_callback_registry_file_loaded

   ! The startup report, one line per element: first the file and its
   ! CRC-32 (or that no file was found, worded as in
   ! lookup_callback_interface_id), then one line per registered
   ! interface in sorted name order with its version and summary.
   ! Interfaces registered in code are listed too, file or no file.
   function make_callback_registry_report(rc) result(lines)
      integer, optional, intent(out) :: rc
      type(StringVector) :: lines

      type(CallbackInterfaceRegistryType), pointer :: registry
      type(CallbackInterface) :: iface
      character(:), allocatable :: names(:), cwd, header, name
      character(16) :: count_text, version_text
      integer :: i, status

      registry => get_registry()
      names = list_callback_interface_names()
      write(count_text, '(i0)') size(names)

      if (allocated(registry%file_path)) then
         header = 'Callback registration file ' // registry%file_path // &
              ', CRC-32 ' // registry%file_checksum
      else if (registry%no_file) then
         cwd = GetCurrentWorkingDirectory(_RC)
         header = 'Callback registration: no callback_interfaces.yaml found in ' // cwd
      else
         header = 'Callback registration: no file loaded'
      end if
      call lines%push_back(header // ', ' // trim(count_text) // ' interface(s) registered')

      do i = 1, size(names)
         name = trim(names(i))
         iface = get_callback_interface_by_name(name, _RC)
         write(version_text, '(i0)') iface%get_version()
         call lines%push_back('  ' // name // ' (version ' // trim(version_text) // '): ' // &
              iface%get_summary())
      end do

      _RETURN(_SUCCESS)
   end function make_callback_registry_report

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
   ! explicitly". When no interface at all is registered and the
   ! registry was marked "no file" (see mark_callback_registry_no_file),
   ! the failure names the missing file and the current working
   ! directory instead of the generic message (module header comment).
   function lookup_callback_interface_id(service_name, rc) result(id)
      character(*), intent(in) :: service_name
      integer, optional, intent(out) :: rc
      type(CallbackInterfaceId) :: id

      type(CallbackInterfaceRegistryType), pointer :: registry
      type(CallbackInterfaceId), pointer :: found
      character(:), allocatable :: cwd, message
      integer :: status

      registry => get_registry()
      found => registry%service_ids%at(service_name)

      if (.not. associated(found)) then
         if (registry%no_file .and. size(list_callback_interface_names()) == 0) then
            cwd = GetCurrentWorkingDirectory(_RC)
            message = 'CallbackInterfaceRegistry: lookup_callback_interface_id - ' // &
                 'no callback_interfaces.yaml found in ' // cwd
            _FAIL(message)
         end if
         _FAIL('CallbackInterfaceRegistry: lookup_callback_interface_id - service name not registered')
      end if
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
