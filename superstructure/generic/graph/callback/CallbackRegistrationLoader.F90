#include "MAPL.h"

!------------------------------------------------------------------------------
! CallbackRegistrationLoader: reads a callback registration file (an
! ESMF HConfig mapping of interface name -> interface entry) and
! registers every entry into CallbackInterfaceRegistry, via
! load_callback_registration_file(path, found, rc).
!
! A missing file is not an error: found is set .false., rc is
! success, and the registry is marked "no file" (see
! mark_callback_registry_no_file) so a later lookup against an empty
! registry can tell that case apart from a file that loaded with no
! entries. A found file is validated entry by entry: every entry is
! built (every declare_* call on a fresh CallbackInterface) before any
! entry is registered, so one bad entry leaves the registry untouched
! for the whole file. Loading stops at the first invalid entry.
!
! An unknown key at any level (entry, argument, method-argument option
! map, attribute, call scalar, rule) is a load error naming the file,
! the interface and the offending key. Most other validation (legal
! names, weight consistency, rule legality, ...) is already done by
! the declare_* calls on CallbackInterface itself; this module does
! not repeat those checks, it only adds file and interface context: a
! failed declare_* call already raised one message (no file context),
! and this module raises a second one naming the file and interface,
! so every failure names both without this module re-deriving the
! content of the first message.
!
! Arguments are declared in two passes so that a weight_for argument
! may appear before or after the argument it weights in the file: pass
! one declares every argument without weight_for, pass two declares
! every argument with weight_for (which needs its target already
! declared).
!
! A rule trigger value (when: {attribute: value}) is passed to
! declare_rule as its YAML source text, unchanged, with one exception.
! A logical attribute value is normalized to the literal lowercase
! word declare_rule expects, because the HConfig bool tag can come
! from several spellings (yes/no/True/on/...), not just true/false. A
! canonical textual form for numeric comparison at rule-matching time
! (not yet built) is left for whoever builds that; it is not decided
! here.
!------------------------------------------------------------------------------
module mapl_CallbackRegistrationLoader_mod
   use mapl_CallbackInterface_mod, only: CallbackInterface, make_CallbackInterface
   use mapl_CallbackInterfaceRegistry_mod, only: register_callback_interface
   use mapl_CallbackInterfaceRegistry_mod, only: mark_callback_registry_no_file
   use mapl_CallbackInterfaceRegistry_mod, only: list_callback_interface_names
   use mapl_CallbackInterfaceId_mod, only: CallbackInterfaceId
   use mapl_CallbackValueType_mod, only: CallbackValueType, operator(==)
   use mapl_CallbackValueType_mod, only: CALLBACK_VALUE_INTEGER
   use mapl_CallbackValueType_mod, only: CALLBACK_VALUE_REAL
   use mapl_CallbackValueType_mod, only: CALLBACK_VALUE_LOGICAL
   use mapl_CallbackValueType_mod, only: CALLBACK_VALUE_STRING
   use mapl_CallbackAttributeSpec_mod, only: CallbackAttributeScope
   use mapl_CallbackAttributeSpec_mod, only: CALLBACK_SCOPE_MEMBER
   use mapl_CallbackAttributeSpec_mod, only: CALLBACK_SCOPE_STATE
   use mapl_CallbackCallScalarSpec_mod, only: CallbackCallScalarWriter
   use mapl_CallbackCallScalarSpec_mod, only: CALLBACK_WRITER_INVOKER
   use mapl_CallbackCallScalarSpec_mod, only: CALLBACK_WRITER_HANDLER
   use mapl_CallbackArgumentOptions_mod, only: CallbackArgumentOptions
   use mapl_AccessSpec_mod, only: AccessSpec
   use mapl_AccessSpec_mod, only: MAPL_ACCESS_IN, MAPL_ACCESS_OUT, MAPL_ACCESS_INOUT
   use mapl_StateItemFlag_mod, only: MAPL_StateItem_Flag
   use mapl_StateItemFlag_mod, only: MAPL_STATEITEM_FIELD
   use mapl_StateItemFlag_mod, only: MAPL_STATEITEM_FIELDBUNDLE
   use mapl_StateItemFlag_mod, only: MAPL_STATEITEM_STATE
   use ESMF, only: ESMF_HConfig, ESMF_HConfigCreate, ESMF_HConfigDestroy
   use ESMF, only: ESMF_HConfigIsDefined, ESMF_HConfigCreateAt, ESMF_HConfigCreateAtMapVal
   use ESMF, only: ESMF_HConfigIter, ESMF_HConfigIterBegin, ESMF_HConfigIterEnd
   use ESMF, only: ESMF_HConfigIterLoop, ESMF_HConfigAsStringMapKey, ESMF_HConfigGetSize
   use ESMF, only: ESMF_HConfigIsScalar
   use ESMF, only: ESMF_HConfigAsString, ESMF_HConfigAsLogical, ESMF_HConfigAsI4
   use ESMF, only: ESMF_HConfigAsI8, ESMF_HConfigAsR8, ESMF_HConfigGetTag
   use ESMF, only: ESMF_HConfigAsStringSeq, ESMF_HConfigAsI8Seq
   use ESMF, only: ESMF_HConfigAsR8Seq, ESMF_HConfigAsLogicalSeq
   use ESMF, only: ESMF_MAXSTR
   use, intrinsic :: iso_fortran_env, only: INT64, REAL64
   use mapl_ErrorHandling_mod
   implicit none(type, external)
   private

   public :: load_callback_registration_file

   ! The core-schema bool tag ESMF_HConfigGetTag reports for a scalar
   ! spelled as a YAML bool (yes/no/True/on/...). Not public from
   ! ESMF_HConfigUtilities, so the literal tag string is repeated here.
   character(*), parameter :: TAG_BOOL = 'tag:yaml.org,2002:bool'

   integer, parameter :: MAX_KEY_LEN = 13
   character(MAX_KEY_LEN), parameter :: ENTRY_LEGAL_KEYS(8) = [ &
        'version      ', 'summary      ', 'description  ', 'arguments    ', &
        'methods      ', 'attributes   ', 'call_scalars ', 'rules        ']
   character(MAX_KEY_LEN), parameter :: ARGUMENT_LEGAL_KEYS(6) = [ &
        'kind         ', 'dims         ', 'optional     ', 'extensive    ', &
        'weight_for   ', 'description  ']
   character(MAX_KEY_LEN), parameter :: METHOD_ARG_LEGAL_KEYS(2) = [ &
        'access       ', 'put_flavour  ']
   character(MAX_KEY_LEN), parameter :: ATTRIBUTE_LEGAL_KEYS(7) = [ &
        'type         ', 'scope        ', 'host_argument', 'required     ', &
        'default      ', 'allowed      ', 'description  ']
   character(MAX_KEY_LEN), parameter :: CALL_SCALAR_LEGAL_KEYS(3) = [ &
        'type         ', 'written_by   ', 'description  ']
   character(MAX_KEY_LEN), parameter :: RULE_LEGAL_KEYS(2) = [ &
        'when         ', 'requires     ']

   ! Legal values for fields whose value is checked against a fixed
   ! set rather than against a key list, quoted verbatim into "unknown
   ! ..." error messages.
   character(*), parameter :: KIND_LEGAL_VALUES = 'field, bundle, state'
   character(*), parameter :: ACCESS_LEGAL_VALUES = 'in, out, inout'
   character(*), parameter :: SCOPE_LEGAL_VALUES = 'member, state'
   character(*), parameter :: VALUE_TYPE_LEGAL_VALUES = &
        'integer, real, logical, string (optionally prefixed with "list of ")'
   character(*), parameter :: CALL_SCALAR_TYPE_LEGAL_VALUES = &
        'integer, real, logical, string (lists are not supported)'
   character(*), parameter :: WRITER_LEGAL_VALUES = 'invoker, handler'

   ! One entry built from the file, kept until every entry in the file
   ! has built clean, so nothing is registered until then.
   type :: NamedCallbackInterface
      character(:), allocatable :: name
      type(CallbackInterface) :: iface
   end type NamedCallbackInterface

contains

   ! Loads path (an ESMF HConfig mapping of interface name -> entry). A
   ! missing file sets found .false., marks the registry "no file" (see
   ! mark_callback_registry_no_file), and returns success - not an
   ! error. Existence is checked with inquire before parsing, so a file
   ! that exists but fails to parse is a real error naming the file,
   ! never mistaken for "no file". A found
   ! file is validated whole before anything is registered; loading
   ! stops at the first invalid entry and nothing from the file is
   ! registered.
   subroutine load_callback_registration_file(path, found, rc)
      character(*), intent(in) :: path
      logical, intent(out) :: found
      integer, optional, intent(out) :: rc

      type(ESMF_HConfig) :: file_hconfig
      integer :: status

      inquire(file=path, exist=found)
      if (.not. found) then
         call mark_callback_registry_no_file()
         _RETURN(_SUCCESS)
      end if

      file_hconfig = ESMF_HConfigCreate(filename=path, rc=status)
      if (status /= 0) then
         _FAIL(trim(path) // ': could not parse YAML')
      end if

      call process_all_entries(path, file_hconfig, _RC)
      call ESMF_HConfigDestroy(file_hconfig, _RC)

      _RETURN(_SUCCESS)
   end subroutine load_callback_registration_file

   ! Builds every entry (none registered yet); only once every entry
   ! has built clean are all of them registered.
   subroutine process_all_entries(path, file_hconfig, rc)
      character(*), intent(in) :: path
      type(ESMF_HConfig), intent(in) :: file_hconfig
      integer, optional, intent(out) :: rc

      type(NamedCallbackInterface), allocatable :: built(:)
      type(ESMF_HConfigIter) :: iter, b, e
      type(ESMF_HConfig) :: entry_cfg
      type(CallbackInterface) :: iface
      type(CallbackInterfaceId) :: id
      character(:), allocatable :: iface_name, msg
      character(:), allocatable :: already_registered(:)
      integer :: status, i, j, n

      ! Allocated to its final size up front and filled by index below,
      ! rather than appended to with built = [built, ...], which would
      ! deep-copy every already-built CallbackInterface (each carrying
      ! its own gFTL-backed maps) on every new entry.
      n = ESMF_HConfigGetSize(file_hconfig, _RC)
      allocate(built(n))

      b = ESMF_HConfigIterBegin(file_hconfig, _RC)
      e = ESMF_HConfigIterEnd(file_hconfig, _RC)
      iter = b
      i = 0
      do while (ESMF_HConfigIterLoop(iter, b, e, rc=status))
         _VERIFY(status)
         i = i + 1
         iface_name = ESMF_HConfigAsStringMapKey(iter, _RC)

         msg = trim(path) // ': interface ' // iface_name // &
              ': name must not contain "/"'
         _ASSERT(index(iface_name, '/') == 0, msg)

         entry_cfg = ESMF_HConfigCreateAtMapVal(iter, _RC)
         iface = build_one_interface(path, iface_name, entry_cfg, _RC)
         call ESMF_HConfigDestroy(entry_cfg, _RC)

         built(i) = NamedCallbackInterface(iface_name, iface)
      end do

      ! Every name this file would register is checked against the
      ! registry, and against the other names from this same file,
      ! before anything is registered - so a name already registered
      ! in code, or duplicated within this file, cannot register some
      ! of the interfaces in this file and leave the rest out. A frozen
      ! registry is not checked here: nothing has registered yet at
      ! this point, so the registration loop below still rejects every
      ! entry, all or nothing, the first time it calls
      ! register_callback_interface.
      already_registered = list_callback_interface_names()
      do i = 1, size(built)
         do j = 1, size(already_registered)
            if (trim(already_registered(j)) == built(i)%name) then
               msg = trim(path) // ': interface ' // built(i)%name // &
                    ': already registered'
               _FAIL(msg)
            end if
         end do
         do j = 1, i - 1
            if (built(j)%name == built(i)%name) then
               msg = trim(path) // ': interface ' // built(i)%name // &
                    ': declared more than once in this file'
               _FAIL(msg)
            end if
         end do
      end do

      do i = 1, size(built)
         call register_callback_interface(built(i)%name, built(i)%iface, &
              id, status)
         if (status /= 0) then
            msg = trim(path) // ': interface ' // built(i)%name // &
                 ': could not register interface'
            _FAIL(msg)
         end if
      end do

      _RETURN(_SUCCESS)
   end subroutine process_all_entries

   function build_one_interface(path, iface_name, entry_cfg, rc) &
        result(iface)
      character(*), intent(in) :: path
      character(*), intent(in) :: iface_name
      type(ESMF_HConfig), intent(in) :: entry_cfg
      integer, optional, intent(out) :: rc
      type(CallbackInterface) :: iface

      character(:), allocatable :: summary, description, msg
      logical :: has_description
      integer :: version, status

      call check_no_unknown_keys(entry_cfg, ENTRY_LEGAL_KEYS, path, &
           iface_name, 'entry', _RC)

      call require_key(entry_cfg, 'version', path, iface_name, _RC)
      version = ESMF_HConfigAsI4(entry_cfg, keyString='version', _RC)

      call require_key(entry_cfg, 'summary', path, iface_name, _RC)
      summary = ESMF_HConfigAsString(entry_cfg, keyString='summary', _RC)

      has_description = ESMF_HConfigIsDefined(entry_cfg, &
           keyString='description', _RC)
      if (has_description) then
         description = ESMF_HConfigAsString(entry_cfg, &
              keyString='description', _RC)
      end if

      iface = make_CallbackInterface(version, summary, &
           description=description, rc=status)
      if (status /= 0) then
         msg = trim(path) // ': interface ' // iface_name // &
              ': could not build interface'
         _FAIL(msg)
      end if

      call load_arguments(entry_cfg, iface, path, iface_name, _RC)

      call load_methods(entry_cfg, iface, path, iface_name, _RC)

      call load_attributes(entry_cfg, iface, path, iface_name, _RC)

      call load_call_scalars(entry_cfg, iface, path, iface_name, _RC)

      call load_rules(entry_cfg, iface, path, iface_name, _RC)

      _RETURN(_SUCCESS)
   end function build_one_interface

   ! Fails (one message naming the file, interface and key) unless
   ! hconfig is a mapping holding no key outside legal_keys.
   subroutine check_no_unknown_keys(hconfig, legal_keys, path, iface_name, &
        context, rc)
      type(ESMF_HConfig), intent(in) :: hconfig
      character(*), intent(in) :: legal_keys(:)
      character(*), intent(in) :: path
      character(*), intent(in) :: iface_name
      character(*), intent(in) :: context
      integer, optional, intent(out) :: rc

      type(ESMF_HConfigIter) :: iter, b, e
      character(:), allocatable :: key, msg
      logical :: is_legal
      integer :: status, i

      b = ESMF_HConfigIterBegin(hconfig, _RC)
      e = ESMF_HConfigIterEnd(hconfig, _RC)
      iter = b
      do while (ESMF_HConfigIterLoop(iter, b, e, rc=status))
         _VERIFY(status)
         key = ESMF_HConfigAsStringMapKey(iter, _RC)

         is_legal = .false.
         do i = 1, size(legal_keys)
            if (trim(legal_keys(i)) == key) is_legal = .true.
         end do

         if (.not. is_legal) then
            msg = trim(path) // ': interface ' // iface_name // ': ' // &
                 context // ': unknown key ' // key // &
                 '; legal keys: ' // join_keys(legal_keys)
            _FAIL(msg)
         end if
      end do

      _RETURN(_SUCCESS)
   end subroutine check_no_unknown_keys

   ! Joins a legal_keys array (trimming the padding of each entry) into
   ! one comma-separated string, for an error message.
   function join_keys(legal_keys) result(joined)
      character(*), intent(in) :: legal_keys(:)
      character(:), allocatable :: joined

      integer :: i

      joined = ''
      do i = 1, size(legal_keys)
         if (i > 1) joined = joined // ', '
         joined = joined // trim(legal_keys(i))
      end do
   end function join_keys

   ! Fails (naming the file, interface and key, plus context when given,
   ! e.g. which argument/method/attribute/call scalar/rule) unless key
   ! is defined on hconfig.
   subroutine require_key(hconfig, key, path, iface_name, rc, context)
      type(ESMF_HConfig), intent(in) :: hconfig
      character(*), intent(in) :: key
      character(*), intent(in) :: path
      character(*), intent(in) :: iface_name
      integer, optional, intent(out) :: rc
      character(*), optional, intent(in) :: context

      character(:), allocatable :: msg, where
      logical :: present_
      integer :: status

      present_ = ESMF_HConfigIsDefined(hconfig, keyString=key, _RC)
      where = ''
      if (present(context)) where = ': ' // context
      msg = trim(path) // ': interface ' // iface_name // where // &
           ': missing required key ' // key
      _ASSERT(present_, msg)

      _RETURN(_SUCCESS)
   end subroutine require_key

   !--------------------------------------------------------------------
   ! Arguments
   !--------------------------------------------------------------------

   subroutine load_arguments(entry_cfg, iface, path, iface_name, rc)
      type(ESMF_HConfig), intent(in) :: entry_cfg
      type(CallbackInterface), intent(inout) :: iface
      character(*), intent(in) :: path, iface_name
      integer, optional, intent(out) :: rc

      type(ESMF_HConfig) :: args_cfg
      integer :: status

      call require_key(entry_cfg, 'arguments', path, iface_name, _RC)

      args_cfg = ESMF_HConfigCreateAt(entry_cfg, keyString='arguments', _RC)

      ! Pass 1: arguments without weight_for; pass 2: arguments with
      ! weight_for, so the argument a weight names is always already
      ! declared, regardless of the order the file gives them in.
      call declare_arguments_pass(args_cfg, iface, path, iface_name, &
           .false., _RC)

      call declare_arguments_pass(args_cfg, iface, path, iface_name, &
           .true., _RC)

      call ESMF_HConfigDestroy(args_cfg, _RC)

      _RETURN(_SUCCESS)
   end subroutine load_arguments

   subroutine declare_arguments_pass(args_cfg, iface, path, iface_name, &
        weights_only, rc)
      type(ESMF_HConfig), intent(in) :: args_cfg
      type(CallbackInterface), intent(inout) :: iface
      character(*), intent(in) :: path, iface_name
      logical, intent(in) :: weights_only
      integer, optional, intent(out) :: rc

      type(ESMF_HConfigIter) :: iter, b, e
      type(ESMF_HConfig) :: one_cfg
      character(:), allocatable :: name
      logical :: has_weight
      integer :: status

      b = ESMF_HConfigIterBegin(args_cfg, _RC)
      e = ESMF_HConfigIterEnd(args_cfg, _RC)
      iter = b
      do while (ESMF_HConfigIterLoop(iter, b, e, rc=status))
         _VERIFY(status)
         name = ESMF_HConfigAsStringMapKey(iter, _RC)
         one_cfg = ESMF_HConfigCreateAtMapVal(iter, _RC)
         has_weight = ESMF_HConfigIsDefined(one_cfg, keyString='weight_for', &
              _RC)

         if (has_weight .eqv. weights_only) then
            call declare_one_argument(one_cfg, iface, path, iface_name, &
                 name, _RC)
         end if

         call ESMF_HConfigDestroy(one_cfg, _RC)
      end do

      _RETURN(_SUCCESS)
   end subroutine declare_arguments_pass

   subroutine declare_one_argument(one_cfg, iface, path, iface_name, name, rc)
      type(ESMF_HConfig), intent(in) :: one_cfg
      type(CallbackInterface), intent(inout) :: iface
      character(*), intent(in) :: path, iface_name, name
      integer, optional, intent(out) :: rc

      type(MAPL_StateItem_Flag) :: expected_kind
      character(:), allocatable :: kind_text, dims, weight_for, msg
      logical, allocatable :: optional_value, extensive_value
      logical :: kind_legal, is_defined
      integer :: status

      call check_no_unknown_keys(one_cfg, ARGUMENT_LEGAL_KEYS, path, &
           iface_name, 'argument ' // name, _RC)

      kind_text = 'field'
      is_defined = ESMF_HConfigIsDefined(one_cfg, keyString='kind', _RC)
      if (is_defined) then
         kind_text = ESMF_HConfigAsString(one_cfg, keyString='kind', _RC)
      end if
      call parse_argument_kind(kind_text, expected_kind, kind_legal)
      if (.not. kind_legal) then
         msg = trim(path) // ': interface ' // iface_name // &
              ': argument ' // name // ': unknown kind ' // kind_text // &
              '; legal kinds: ' // KIND_LEGAL_VALUES
         _FAIL(msg)
      end if

      is_defined = ESMF_HConfigIsDefined(one_cfg, keyString='dims', _RC)
      if (is_defined) then
         dims = ESMF_HConfigAsString(one_cfg, keyString='dims', _RC)
      end if

      is_defined = ESMF_HConfigIsDefined(one_cfg, keyString='optional', _RC)
      if (is_defined) then
         allocate(optional_value)
         optional_value = ESMF_HConfigAsLogical(one_cfg, &
              keyString='optional', _RC)
      end if

      is_defined = ESMF_HConfigIsDefined(one_cfg, keyString='extensive', _RC)
      if (is_defined) then
         allocate(extensive_value)
         extensive_value = ESMF_HConfigAsLogical(one_cfg, &
              keyString='extensive', _RC)
      end if

      is_defined = ESMF_HConfigIsDefined(one_cfg, keyString='weight_for', &
           _RC)
      if (is_defined) then
         weight_for = ESMF_HConfigAsString(one_cfg, keyString='weight_for', &
              _RC)
      end if

      call iface%declare_argument(name, expected_kind, dims=dims, &
           optional=optional_value, extensive=extensive_value, &
           weight_for=weight_for, rc=status)
      if (status /= 0) then
         msg = trim(path) // ': interface ' // iface_name // &
              ': could not declare argument ' // name
         _FAIL(msg)
      end if

      _RETURN(_SUCCESS)
   end subroutine declare_one_argument

   subroutine parse_argument_kind(kind_text, expected_kind, legal)
      character(*), intent(in) :: kind_text
      type(MAPL_StateItem_Flag), intent(out) :: expected_kind
      logical, intent(out) :: legal

      legal = .true.
      select case (trim(kind_text))
      case ('field')
         expected_kind = MAPL_STATEITEM_FIELD
      case ('bundle')
         expected_kind = MAPL_STATEITEM_FIELDBUNDLE
      case ('state')
         expected_kind = MAPL_STATEITEM_STATE
      case default
         legal = .false.
      end select
   end subroutine parse_argument_kind

   !--------------------------------------------------------------------
   ! Methods
   !--------------------------------------------------------------------

   subroutine load_methods(entry_cfg, iface, path, iface_name, rc)
      type(ESMF_HConfig), intent(in) :: entry_cfg
      type(CallbackInterface), intent(inout) :: iface
      character(*), intent(in) :: path, iface_name
      integer, optional, intent(out) :: rc

      type(ESMF_HConfig) :: methods_cfg, method_cfg
      type(ESMF_HConfigIter) :: iter, b, e
      character(:), allocatable :: method_name, msg
      integer :: status, n

      call require_key(entry_cfg, 'methods', path, iface_name, _RC)

      methods_cfg = ESMF_HConfigCreateAt(entry_cfg, keyString='methods', _RC)
      n = ESMF_HConfigGetSize(methods_cfg, _RC)
      if (n < 1) then
         msg = trim(path) // ': interface ' // iface_name // &
              ': methods must declare at least one method'
         _FAIL(msg)
      end if

      b = ESMF_HConfigIterBegin(methods_cfg, _RC)
      e = ESMF_HConfigIterEnd(methods_cfg, _RC)
      iter = b
      do while (ESMF_HConfigIterLoop(iter, b, e, rc=status))
         _VERIFY(status)
         method_name = ESMF_HConfigAsStringMapKey(iter, _RC)
         method_cfg = ESMF_HConfigCreateAtMapVal(iter, _RC)

         call iface%declare_method(method_name, rc=status)
         if (status /= 0) then
            msg = trim(path) // ': interface ' // iface_name // &
                 ': could not declare method ' // method_name
            _FAIL(msg)
         end if

         call load_method_arguments(method_cfg, iface, path, iface_name, &
              method_name, _RC)

         call ESMF_HConfigDestroy(method_cfg, _RC)
      end do

      call ESMF_HConfigDestroy(methods_cfg, _RC)

      _RETURN(_SUCCESS)
   end subroutine load_methods

   subroutine load_method_arguments(method_cfg, iface, path, iface_name, &
        method_name, rc)
      type(ESMF_HConfig), intent(in) :: method_cfg
      type(CallbackInterface), intent(inout) :: iface
      character(*), intent(in) :: path, iface_name, method_name
      integer, optional, intent(out) :: rc

      type(ESMF_HConfigIter) :: iter, b, e
      type(ESMF_HConfig) :: val_cfg
      type(AccessSpec) :: access
      type(CallbackArgumentOptions) :: options
      character(:), allocatable :: arg_name, access_text, put_flavour_text, msg
      logical :: is_scalar, access_legal, has_options, has_put_flavour
      integer :: status

      b = ESMF_HConfigIterBegin(method_cfg, _RC)
      e = ESMF_HConfigIterEnd(method_cfg, _RC)
      iter = b
      do while (ESMF_HConfigIterLoop(iter, b, e, rc=status))
         _VERIFY(status)
         arg_name = ESMF_HConfigAsStringMapKey(iter, _RC)
         val_cfg = ESMF_HConfigCreateAtMapVal(iter, _RC)
         is_scalar = ESMF_HConfigIsScalar(val_cfg, _RC)
         has_options = .false.

         if (is_scalar) then
            access_text = ESMF_HConfigAsString(val_cfg, _RC)
         else
            call check_no_unknown_keys(val_cfg, METHOD_ARG_LEGAL_KEYS, &
                 path, iface_name, &
                 'method ' // method_name // ' argument ' // arg_name, &
                 _RC)

            call require_key(val_cfg, 'access', path, iface_name, &
                 context='method ' // method_name // ' argument ' // &
                 arg_name, _RC)
            access_text = ESMF_HConfigAsString(val_cfg, keyString='access', &
                 _RC)

            has_put_flavour = ESMF_HConfigIsDefined(val_cfg, &
                 keyString='put_flavour', _RC)
            if (has_put_flavour) then
               put_flavour_text = ESMF_HConfigAsString(val_cfg, &
                    keyString='put_flavour', _RC)
               call options%set_option('put_flavour', put_flavour_text, &
                    rc=status)
               if (status /= 0) then
                  msg = trim(path) // ': interface ' // iface_name // &
                       ': method ' // method_name // ': argument ' // &
                       arg_name // ': could not set put_flavour ' // &
                       put_flavour_text
                  _FAIL(msg)
               end if
               has_options = .true.
            end if
         end if

         call parse_access(access_text, access, access_legal)
         if (.not. access_legal) then
            msg = trim(path) // ': interface ' // iface_name // &
                 ': method ' // method_name // ': argument ' // arg_name // &
                 ': unknown access ' // access_text // &
                 '; legal access modes: ' // ACCESS_LEGAL_VALUES
            _FAIL(msg)
         end if

         if (has_options) then
            call iface%set_method_argument_access(method_name, arg_name, &
                 access, options=options, rc=status)
         else
            call iface%set_method_argument_access(method_name, arg_name, &
                 access, rc=status)
         end if
         if (status /= 0) then
            msg = trim(path) // ': interface ' // iface_name // &
                 ': method ' // method_name // &
                 ': could not set access for argument ' // arg_name
            _FAIL(msg)
         end if

         call ESMF_HConfigDestroy(val_cfg, _RC)
      end do

      _RETURN(_SUCCESS)
   end subroutine load_method_arguments

   subroutine parse_access(text, access, legal)
      character(*), intent(in) :: text
      type(AccessSpec), intent(out) :: access
      logical, intent(out) :: legal

      legal = .true.
      select case (trim(text))
      case ('in')
         access = MAPL_ACCESS_IN
      case ('out')
         access = MAPL_ACCESS_OUT
      case ('inout')
         access = MAPL_ACCESS_INOUT
      case default
         legal = .false.
      end select
   end subroutine parse_access

   !--------------------------------------------------------------------
   ! Attributes
   !--------------------------------------------------------------------

   subroutine load_attributes(entry_cfg, iface, path, iface_name, rc)
      type(ESMF_HConfig), intent(in) :: entry_cfg
      type(CallbackInterface), intent(inout) :: iface
      character(*), intent(in) :: path, iface_name
      integer, optional, intent(out) :: rc

      type(ESMF_HConfig) :: attrs_cfg, one_cfg
      type(ESMF_HConfigIter) :: iter, b, e
      character(:), allocatable :: name
      logical :: has_attributes
      integer :: status

      has_attributes = ESMF_HConfigIsDefined(entry_cfg, &
           keyString='attributes', _RC)
      _RETURN_UNLESS(has_attributes)

      attrs_cfg = ESMF_HConfigCreateAt(entry_cfg, keyString='attributes', _RC)

      b = ESMF_HConfigIterBegin(attrs_cfg, _RC)
      e = ESMF_HConfigIterEnd(attrs_cfg, _RC)
      iter = b
      do while (ESMF_HConfigIterLoop(iter, b, e, rc=status))
         _VERIFY(status)
         name = ESMF_HConfigAsStringMapKey(iter, _RC)
         one_cfg = ESMF_HConfigCreateAtMapVal(iter, _RC)

         call declare_one_attribute(one_cfg, iface, path, iface_name, name, &
              _RC)

         call ESMF_HConfigDestroy(one_cfg, _RC)
      end do

      call ESMF_HConfigDestroy(attrs_cfg, _RC)

      _RETURN(_SUCCESS)
   end subroutine load_attributes

   ! type (integer|real|logical|string, optionally "list of <type>") and
   ! legal is .false. for anything else.
   subroutine parse_value_type(type_text, value_type, is_list, legal)
      character(*), intent(in) :: type_text
      type(CallbackValueType), intent(out) :: value_type
      logical, intent(out) :: is_list
      logical, intent(out) :: legal

      character(:), allocatable :: base
      character(*), parameter :: LIST_PREFIX = 'list of '

      legal = .true.
      is_list = .false.
      base = trim(type_text)
      if (len(base) > len(LIST_PREFIX)) then
         if (base(1:len(LIST_PREFIX)) == LIST_PREFIX) then
            is_list = .true.
            base = trim(base(len(LIST_PREFIX) + 1:))
         end if
      end if

      select case (base)
      case ('integer')
         value_type = CALLBACK_VALUE_INTEGER
      case ('real')
         value_type = CALLBACK_VALUE_REAL
      case ('logical')
         value_type = CALLBACK_VALUE_LOGICAL
      case ('string')
         value_type = CALLBACK_VALUE_STRING
      case default
         legal = .false.
      end select
   end subroutine parse_value_type

   subroutine declare_one_attribute(one_cfg, iface, path, iface_name, name, &
        rc)
      type(ESMF_HConfig), intent(in) :: one_cfg
      type(CallbackInterface), intent(inout) :: iface
      character(*), intent(in) :: path, iface_name, name
      integer, optional, intent(out) :: rc

      type(CallbackValueType) :: value_type
      type(CallbackAttributeScope), allocatable :: scope
      character(:), allocatable :: type_text, scope_text, host_argument, msg
      logical :: is_list, type_legal, is_defined
      logical, allocatable :: required
      integer :: status

      integer(INT64), allocatable :: default_i, default_i_list(:), &
           allowed_i(:)
      real(REAL64), allocatable :: default_r, default_r_list(:)
      logical, allocatable :: default_l, default_l_list(:)
      character(:), allocatable :: default_s
      character(ESMF_MAXSTR), allocatable :: default_s_list(:), allowed_s(:)

      call check_no_unknown_keys(one_cfg, ATTRIBUTE_LEGAL_KEYS, path, &
           iface_name, 'attribute ' // name, _RC)

      call require_key(one_cfg, 'type', path, iface_name, &
           context='attribute ' // name, _RC)
      type_text = ESMF_HConfigAsString(one_cfg, keyString='type', _RC)
      call parse_value_type(type_text, value_type, is_list, type_legal)
      if (.not. type_legal) then
         msg = trim(path) // ': interface ' // iface_name // &
              ': attribute ' // name // ': unknown type ' // type_text // &
              '; legal types: ' // VALUE_TYPE_LEGAL_VALUES
         _FAIL(msg)
      end if

      is_defined = ESMF_HConfigIsDefined(one_cfg, keyString='scope', _RC)
      if (is_defined) then
         scope_text = ESMF_HConfigAsString(one_cfg, keyString='scope', _RC)
         select case (trim(scope_text))
         case ('member')
            allocate(scope, source=CALLBACK_SCOPE_MEMBER)
         case ('state')
            allocate(scope, source=CALLBACK_SCOPE_STATE)
         case default
            msg = trim(path) // ': interface ' // iface_name // &
                 ': attribute ' // name // ': unknown scope ' // &
                 scope_text // '; legal scopes: ' // SCOPE_LEGAL_VALUES
            _FAIL(msg)
         end select
      end if

      is_defined = ESMF_HConfigIsDefined(one_cfg, keyString='host_argument', &
           _RC)
      if (is_defined) then
         host_argument = ESMF_HConfigAsString(one_cfg, &
              keyString='host_argument', _RC)
      end if

      is_defined = ESMF_HConfigIsDefined(one_cfg, keyString='required', _RC)
      if (is_defined) then
         allocate(required)
         required = ESMF_HConfigAsLogical(one_cfg, keyString='required', &
              _RC)
      end if

      is_defined = ESMF_HConfigIsDefined(one_cfg, keyString='default', _RC)
      if (is_defined) then
         if (is_list) then
            if (value_type == CALLBACK_VALUE_INTEGER) then
               default_i_list = ESMF_HConfigAsI8Seq(one_cfg, &
                    keyString='default', _RC)
            else if (value_type == CALLBACK_VALUE_REAL) then
               default_r_list = ESMF_HConfigAsR8Seq(one_cfg, &
                    keyString='default', _RC)
            else if (value_type == CALLBACK_VALUE_LOGICAL) then
               default_l_list = ESMF_HConfigAsLogicalSeq(one_cfg, &
                    keyString='default', _RC)
            else
               default_s_list = ESMF_HConfigAsStringSeq(one_cfg, &
                    ESMF_MAXSTR, keyString='default', _RC)
            end if
         else
            if (value_type == CALLBACK_VALUE_INTEGER) then
               allocate(default_i)
               default_i = ESMF_HConfigAsI8(one_cfg, keyString='default', &
                    _RC)
            else if (value_type == CALLBACK_VALUE_REAL) then
               allocate(default_r)
               default_r = ESMF_HConfigAsR8(one_cfg, keyString='default', &
                    _RC)
            else if (value_type == CALLBACK_VALUE_LOGICAL) then
               allocate(default_l)
               default_l = ESMF_HConfigAsLogical(one_cfg, &
                    keyString='default', _RC)
            else
               default_s = ESMF_HConfigAsString(one_cfg, &
                    keyString='default', _RC)
            end if
         end if
      end if

      is_defined = ESMF_HConfigIsDefined(one_cfg, keyString='allowed', _RC)
      if (is_defined) then
         if (value_type == CALLBACK_VALUE_INTEGER) then
            allowed_i = ESMF_HConfigAsI8Seq(one_cfg, keyString='allowed', &
                 _RC)
         else
            allowed_s = ESMF_HConfigAsStringSeq(one_cfg, ESMF_MAXSTR, &
                 keyString='allowed', _RC)
         end if
      end if

      if (value_type == CALLBACK_VALUE_INTEGER) then
         if (is_list) then
            call iface%declare_attribute(name, value_type, is_list=is_list, &
                 scope=scope, host_argument=host_argument, &
                 required=required, default_list=default_i_list, &
                 allowed_values=allowed_i, rc=status)
         else
            call iface%declare_attribute(name, value_type, is_list=is_list, &
                 scope=scope, host_argument=host_argument, &
                 required=required, default=default_i, &
                 allowed_values=allowed_i, rc=status)
         end if
      else if (value_type == CALLBACK_VALUE_REAL) then
         if (is_list) then
            call iface%declare_attribute(name, value_type, is_list=is_list, &
                 scope=scope, host_argument=host_argument, &
                 required=required, default_list=default_r_list, &
                 allowed_values=allowed_s, rc=status)
         else
            call iface%declare_attribute(name, value_type, is_list=is_list, &
                 scope=scope, host_argument=host_argument, &
                 required=required, default=default_r, &
                 allowed_values=allowed_s, rc=status)
         end if
      else if (value_type == CALLBACK_VALUE_LOGICAL) then
         if (is_list) then
            call iface%declare_attribute(name, value_type, is_list=is_list, &
                 scope=scope, host_argument=host_argument, &
                 required=required, default_list=default_l_list, &
                 allowed_values=allowed_s, rc=status)
         else
            call iface%declare_attribute(name, value_type, is_list=is_list, &
                 scope=scope, host_argument=host_argument, &
                 required=required, default=default_l, &
                 allowed_values=allowed_s, rc=status)
         end if
      else
         if (is_list) then
            call iface%declare_attribute(name, value_type, is_list=is_list, &
                 scope=scope, host_argument=host_argument, &
                 required=required, default_list=default_s_list, &
                 allowed_values=allowed_s, rc=status)
         else
            call iface%declare_attribute(name, value_type, is_list=is_list, &
                 scope=scope, host_argument=host_argument, &
                 required=required, default=default_s, &
                 allowed_values=allowed_s, rc=status)
         end if
      end if
      if (status /= 0) then
         msg = trim(path) // ': interface ' // iface_name // &
              ': could not declare attribute ' // name
         _FAIL(msg)
      end if

      _RETURN(_SUCCESS)
   end subroutine declare_one_attribute

   !--------------------------------------------------------------------
   ! Call scalars
   !--------------------------------------------------------------------

   subroutine load_call_scalars(entry_cfg, iface, path, iface_name, rc)
      type(ESMF_HConfig), intent(in) :: entry_cfg
      type(CallbackInterface), intent(inout) :: iface
      character(*), intent(in) :: path, iface_name
      integer, optional, intent(out) :: rc

      type(ESMF_HConfig) :: cs_cfg, one_cfg
      type(ESMF_HConfigIter) :: iter, b, e
      type(CallbackValueType) :: value_type
      type(CallbackCallScalarWriter) :: writer
      character(:), allocatable :: name, type_text, writer_text, msg
      logical :: has_call_scalars, is_list, type_legal, writer_legal
      integer :: status

      has_call_scalars = ESMF_HConfigIsDefined(entry_cfg, &
           keyString='call_scalars', _RC)
      _RETURN_UNLESS(has_call_scalars)

      cs_cfg = ESMF_HConfigCreateAt(entry_cfg, keyString='call_scalars', _RC)

      b = ESMF_HConfigIterBegin(cs_cfg, _RC)
      e = ESMF_HConfigIterEnd(cs_cfg, _RC)
      iter = b
      do while (ESMF_HConfigIterLoop(iter, b, e, rc=status))
         _VERIFY(status)
         name = ESMF_HConfigAsStringMapKey(iter, _RC)
         one_cfg = ESMF_HConfigCreateAtMapVal(iter, _RC)

         call check_no_unknown_keys(one_cfg, CALL_SCALAR_LEGAL_KEYS, path, &
              iface_name, 'call scalar ' // name, _RC)

         call require_key(one_cfg, 'type', path, iface_name, &
              context='call scalar ' // name, _RC)
         type_text = ESMF_HConfigAsString(one_cfg, keyString='type', _RC)
         call parse_value_type(type_text, value_type, is_list, type_legal)
         if (.not. type_legal .or. is_list) then
            msg = trim(path) // ': interface ' // iface_name // &
                 ': call scalar ' // name // ': unsupported type ' // &
                 type_text // '; legal types: ' // &
                 CALL_SCALAR_TYPE_LEGAL_VALUES
            _FAIL(msg)
         end if

         call require_key(one_cfg, 'written_by', path, iface_name, &
              context='call scalar ' // name, _RC)
         writer_text = ESMF_HConfigAsString(one_cfg, keyString='written_by', &
              _RC)
         writer_legal = .true.
         select case (trim(writer_text))
         case ('invoker')
            writer = CALLBACK_WRITER_INVOKER
         case ('handler')
            writer = CALLBACK_WRITER_HANDLER
         case default
            writer_legal = .false.
         end select
         if (.not. writer_legal) then
            msg = trim(path) // ': interface ' // iface_name // &
                 ': call scalar ' // name // ': unknown written_by ' // &
                 writer_text // '; legal values: ' // WRITER_LEGAL_VALUES
            _FAIL(msg)
         end if

         call iface%declare_call_scalar(name, value_type, writer, &
              rc=status)
         if (status /= 0) then
            msg = trim(path) // ': interface ' // iface_name // &
                 ': could not declare call scalar ' // name
            _FAIL(msg)
         end if

         call ESMF_HConfigDestroy(one_cfg, _RC)
      end do

      call ESMF_HConfigDestroy(cs_cfg, _RC)

      _RETURN(_SUCCESS)
   end subroutine load_call_scalars

   !--------------------------------------------------------------------
   ! Rules
   !--------------------------------------------------------------------

   subroutine load_rules(entry_cfg, iface, path, iface_name, rc)
      type(ESMF_HConfig), intent(in) :: entry_cfg
      type(CallbackInterface), intent(inout) :: iface
      character(*), intent(in) :: path, iface_name
      integer, optional, intent(out) :: rc

      type(ESMF_HConfig) :: rules_cfg, rule_cfg, when_cfg, value_cfg
      type(ESMF_HConfigIter) :: when_iter, wb, we
      character(:), allocatable :: attribute_name, value_text, msg
      character(ESMF_MAXSTR), allocatable :: requires(:)
      logical :: has_rules, looped
      integer :: n, i, when_size, status

      has_rules = ESMF_HConfigIsDefined(entry_cfg, keyString='rules', _RC)
      _RETURN_UNLESS(has_rules)

      rules_cfg = ESMF_HConfigCreateAt(entry_cfg, keyString='rules', _RC)
      n = ESMF_HConfigGetSize(rules_cfg, _RC)

      do i = 1, n
         rule_cfg = ESMF_HConfigCreateAt(rules_cfg, index=i, _RC)

         call check_no_unknown_keys(rule_cfg, RULE_LEGAL_KEYS, path, &
              iface_name, 'rule', _RC)

         call require_key(rule_cfg, 'when', path, iface_name, &
              context='rule', _RC)
         when_cfg = ESMF_HConfigCreateAt(rule_cfg, keyString='when', _RC)
         when_size = ESMF_HConfigGetSize(when_cfg, _RC)
         if (when_size /= 1) then
            msg = trim(path) // ': interface ' // iface_name // &
                 ': rule: when must have exactly one entry'
            _FAIL(msg)
         end if

         ! when_size == 1 above already guarantees the loop below finds
         ! exactly one entry; its status is still checked.
         wb = ESMF_HConfigIterBegin(when_cfg, _RC)
         we = ESMF_HConfigIterEnd(when_cfg, _RC)
         when_iter = wb
         looped = ESMF_HConfigIterLoop(when_iter, wb, we, rc=status)
         _VERIFY(status)
         attribute_name = ESMF_HConfigAsStringMapKey(when_iter, _RC)
         value_cfg = ESMF_HConfigCreateAtMapVal(when_iter, _RC)
         value_text = normalize_rule_value(value_cfg, _RC)
         call ESMF_HConfigDestroy(value_cfg, _RC)
         call ESMF_HConfigDestroy(when_cfg, _RC)

         call require_key(rule_cfg, 'requires', path, iface_name, &
              context='rule', _RC)
         requires = ESMF_HConfigAsStringSeq(rule_cfg, ESMF_MAXSTR, &
              keyString='requires', _RC)

         call iface%declare_rule(attribute_name, value_text, requires, &
              rc=status)
         if (status /= 0) then
            msg = trim(path) // ': interface ' // iface_name // &
                 ': could not declare rule on ' // attribute_name
            _FAIL(msg)
         end if

         call ESMF_HConfigDestroy(rule_cfg, _RC)
      end do

      call ESMF_HConfigDestroy(rules_cfg, _RC)

      _RETURN(_SUCCESS)
   end subroutine load_rules

   ! The rule trigger value as the YAML source text, unchanged, except
   ! logical, which is normalized to the literal lowercase true/false
   ! declare_rule expects (the HConfig bool tag can come from
   ! yes/no/True/on/... spellings). A canonical textual form for
   ! numeric comparison at rule-matching time (not yet built) is left
   ! for whoever builds that; it is not decided here.
   function normalize_rule_value(value_cfg, rc) result(text)
      type(ESMF_HConfig), intent(in) :: value_cfg
      integer, optional, intent(out) :: rc
      character(:), allocatable :: text

      character(:), allocatable :: tag
      logical :: as_logical
      integer :: status

      tag = ESMF_HConfigGetTag(value_cfg, _RC)

      select case (tag)
      case (TAG_BOOL)
         as_logical = ESMF_HConfigAsLogical(value_cfg, _RC)
         if (as_logical) then
            text = 'true'
         else
            text = 'false'
         end if
      case default
         text = ESMF_HConfigAsString(value_cfg, _RC)
      end select

      _RETURN(_SUCCESS)
   end function normalize_rule_value

end module mapl_CallbackRegistrationLoader_mod
