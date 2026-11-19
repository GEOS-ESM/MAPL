#include "MAPL.h"

! Parses and validates the YAML description of a configurable callback State.
! Kept separate from the State-building code so that a later expansion step
! (for example a registered interface) can produce the same HConfig shape
! without touching the builder.
module mapl_ConfigurableCallbackSpec_mod
   use ESMF
   use mapl_ErrorHandling_mod
   implicit none(type,external)
   private

   public :: CallbackStateSpec
   public :: CallbackFieldDecl
   public :: CallbackMethodDecl
   public :: parse_callback_state_spec
   public :: destroy_callback_state_spec
   public :: get_map_keys
   public :: to_flow_yaml
   public :: MAX_CALLBACK_SLOTS
   public :: MAX_NAME_LEN
   public :: ERR_PREFIX
   public :: MOCK_INTERNAL_ERROR_RC
   public :: TAG_STR, TAG_INT, TAG_FLOAT, TAG_BOOL

   integer, parameter :: MAX_CALLBACK_SLOTS = 8
   integer, parameter :: MAX_NAME_LEN = 256
   character(*), parameter :: ERR_PREFIX = 'ConfigurableCallback: '
   ! userRc of a call that failed inside the mock itself; reserved, so a
   ! client can tell it apart from any fail_on_call code.
   integer, parameter :: MOCK_INTERNAL_ERROR_RC = -999901

   character(*), parameter :: TAG_INT = 'tag:yaml.org,2002:int'
   character(*), parameter :: TAG_FLOAT = 'tag:yaml.org,2002:float'
   character(*), parameter :: TAG_STR = 'tag:yaml.org,2002:str'
   character(*), parameter :: TAG_BOOL = 'tag:yaml.org,2002:bool'

   ! Method labels become Info keys under mapl_mock/; these collide with
   ! the mock's own top-level keys.
   character(len=10), parameter :: RESERVED_LABELS(3) = [character(len=10) :: 'calls', 'labels', 'num_levels']

   integer, parameter :: FORM_FIELD = 1
   integer, parameter :: FORM_ATTRIBUTE = 2

   type :: CallbackFieldDecl
      character(:), allocatable :: name
      character(:), allocatable :: dims
      type(ESMF_TypeKind_Flag) :: typekind = ESMF_TYPEKIND_R4
   end type CallbackFieldDecl

   type :: CallbackMethodDecl
      character(:), allocatable :: label
      type(ESMF_HConfig) :: hconfig
      logical :: records_fields = .false.
   end type CallbackMethodDecl

   type :: CallbackStateSpec
      logical :: has_state_attributes = .false.
      type(ESMF_HConfig) :: state_attributes
      type(CallbackFieldDecl), allocatable :: fields(:)
      type(CallbackMethodDecl), allocatable :: methods(:)
   end type CallbackStateSpec

contains

   ! On a configuration error, errmsg is allocated (naming the key) and rc is
   ! still success; rc is non-zero only when ESMF itself fails.
   subroutine parse_callback_state_spec(hconfig, spec, errmsg, num_levels, rc)
      type(ESMF_HConfig), intent(in) :: hconfig
      type(CallbackStateSpec), intent(out) :: spec
      character(:), allocatable, intent(out) :: errmsg
      integer, optional, intent(in) :: num_levels
      integer, optional, intent(out) :: rc

      integer :: status
      logical :: is_map, has_geometry
      integer :: nlev

      nlev = 0
      if (present(num_levels)) nlev = num_levels
      allocate(spec%fields(0), spec%methods(0))

      is_map = ESMF_HConfigIsMap(hconfig, _RC)
      if (.not. is_map) then
         errmsg = ERR_PREFIX // 'configuration must be a mapping'
         _RETURN(_SUCCESS)
      end if

      has_geometry = ESMF_HConfigIsDefined(hconfig, keyString='geometry', _RC)
      if (has_geometry) then
         errmsg = ERR_PREFIX // 'key <geometry> is not supported; pass the grid to ConfigurableCallbackCreate'
         _RETURN(_SUCCESS)
      end if

      call check_keys(hconfig, [character(len=16) :: 'state_attributes', 'fields', 'methods'], &
           'the callback state', errmsg, _RC)
      _RETURN_IF(allocated(errmsg))

      call parse_state_attributes(hconfig, spec, errmsg, _RC)
      _RETURN_IF(allocated(errmsg))
      call parse_fields(hconfig, spec, nlev, errmsg, _RC)
      _RETURN_IF(allocated(errmsg))
      call parse_methods(hconfig, spec, nlev, errmsg, _RC)

      _RETURN(_SUCCESS)
   end subroutine parse_callback_state_spec

   ! Releases the HConfig handles spec owns; safe on a partly parsed spec.
   subroutine destroy_callback_state_spec(spec, rc)
      type(CallbackStateSpec), intent(inout) :: spec
      integer, optional, intent(out) :: rc

      integer :: status, i

      if (spec%has_state_attributes) then
         call ESMF_HConfigDestroy(spec%state_attributes, _RC)
         spec%has_state_attributes = .false.
      end if
      if (allocated(spec%methods)) then
         do i = 1, size(spec%methods)
            call ESMF_HConfigDestroy(spec%methods(i)%hconfig, _RC)
         end do
         deallocate(spec%methods)
      end if

      _RETURN(_SUCCESS)
   end subroutine destroy_callback_state_spec

   subroutine parse_state_attributes(hconfig, spec, errmsg, rc)
      type(ESMF_HConfig), intent(in) :: hconfig
      type(CallbackStateSpec), intent(inout) :: spec
      character(:), allocatable, intent(inout) :: errmsg
      integer, optional, intent(out) :: rc

      integer :: status, i
      logical :: has_key, is_map
      type(ESMF_HConfig) :: node, val
      character(len=MAX_NAME_LEN), allocatable :: names(:)

      has_key = ESMF_HConfigIsDefined(hconfig, keyString='state_attributes', _RC)
      _RETURN_UNLESS(has_key)

      node = ESMF_HConfigCreateAt(hconfig, keyString='state_attributes', _RC)
      body: block
         is_map = ESMF_HConfigIsMap(node, _RC)
         if (.not. is_map) then
            errmsg = ERR_PREFIX // 'key <state_attributes> must be a mapping'
            exit body
         end if

         names = get_map_keys(node, _RC)
         do i = 1, size(names)
            call check_name(trim(names(i)), 'state_attributes', errmsg)
            if (allocated(errmsg)) exit body
            val = ESMF_HConfigCreateAt(node, keyString=trim(names(i)), _RC)
            call validate_literal(val, 'state_attributes/'//trim(names(i)), errmsg, _RC)
            call ESMF_HConfigDestroy(val, _RC)
            if (allocated(errmsg)) exit body
         end do
      end block body
      if (allocated(errmsg)) then
         call ESMF_HConfigDestroy(node, _RC)
         _RETURN(_SUCCESS)
      end if

      ! Ownership of node passes to spec.
      spec%state_attributes = node
      spec%has_state_attributes = .true.

      _RETURN(_SUCCESS)
   end subroutine parse_state_attributes

   subroutine parse_fields(hconfig, spec, nlev, errmsg, rc)
      type(ESMF_HConfig), intent(in) :: hconfig
      type(CallbackStateSpec), intent(inout) :: spec
      integer, intent(in) :: nlev
      character(:), allocatable, intent(inout) :: errmsg
      integer, optional, intent(out) :: rc

      integer :: status, i
      logical :: has_key, is_map
      type(ESMF_HConfig) :: node, val
      character(len=MAX_NAME_LEN), allocatable :: names(:)
      type(CallbackFieldDecl) :: decl

      has_key = ESMF_HConfigIsDefined(hconfig, keyString='fields', _RC)
      _RETURN_UNLESS(has_key)

      node = ESMF_HConfigCreateAt(hconfig, keyString='fields', _RC)
      body: block
         is_map = ESMF_HConfigIsMap(node, _RC)
         if (.not. is_map) then
            errmsg = ERR_PREFIX // 'key <fields> must be a mapping'
            exit body
         end if

         names = get_map_keys(node, _RC)
         do i = 1, size(names)
            call check_name(trim(names(i)), 'fields', errmsg)
            if (allocated(errmsg)) exit body
            val = ESMF_HConfigCreateAt(node, keyString=trim(names(i)), _RC)
            call parse_field_decl(val, trim(names(i)), nlev, decl, errmsg, _RC)
            call ESMF_HConfigDestroy(val, _RC)
            if (allocated(errmsg)) exit body
            spec%fields = [spec%fields, decl]
         end do
      end block body
      call ESMF_HConfigDestroy(node, _RC)

      _RETURN(_SUCCESS)
   end subroutine parse_fields

   subroutine parse_field_decl(val, name, nlev, decl, errmsg, rc)
      type(ESMF_HConfig), intent(in) :: val
      character(*), intent(in) :: name
      integer, intent(in) :: nlev
      type(CallbackFieldDecl), intent(out) :: decl
      character(:), allocatable, intent(inout) :: errmsg
      integer, optional, intent(out) :: rc

      integer :: status
      logical :: has_key, is_map
      character(:), allocatable :: ctx, str

      ctx = 'fields/' // name
      is_map = ESMF_HConfigIsMap(val, _RC)
      if (.not. is_map) then
         errmsg = ERR_PREFIX // 'key <' // ctx // '> must be a mapping with key <dims>'
         _RETURN(_SUCCESS)
      end if
      call check_keys(val, [character(len=16) :: 'dims', 'typekind'], ctx, errmsg, _RC)
      _RETURN_IF(allocated(errmsg))

      decl%name = name
      decl%typekind = ESMF_TYPEKIND_R4

      has_key = ESMF_HConfigIsDefined(val, keyString='dims', _RC)
      if (.not. has_key) then
         errmsg = ERR_PREFIX // 'key <' // ctx // '/dims> is required (xy or xyz)'
         _RETURN(_SUCCESS)
      end if
      call get_scalar_string(val, 'dims', str, errmsg, ctx, _RC)
      _RETURN_IF(allocated(errmsg))
      select case (str)
      case ('xy', 'xyz')
         decl%dims = str
      case default
         errmsg = ERR_PREFIX // 'key <' // ctx // '/dims> must be xy or xyz, not <' // str // '>'
         _RETURN(_SUCCESS)
      end select
      if (decl%dims == 'xyz' .and. nlev <= 0) then
         errmsg = ERR_PREFIX // 'field <' // decl%name // &
              '> has dims xyz but no positive num_levels was passed to ConfigurableCallbackCreate'
         _RETURN(_SUCCESS)
      end if

      has_key = ESMF_HConfigIsDefined(val, keyString='typekind', _RC)
      if (has_key) then
         call get_scalar_string(val, 'typekind', str, errmsg, ctx, _RC)
         _RETURN_IF(allocated(errmsg))
         select case (str)
         case ('R4')
            decl%typekind = ESMF_TYPEKIND_R4
         case ('R8')
            decl%typekind = ESMF_TYPEKIND_R8
         case default
            errmsg = ERR_PREFIX // 'key <' // ctx // '/typekind> must be R4 or R8, not <' // str // '>'
            _RETURN(_SUCCESS)
         end select
      end if

      _RETURN(_SUCCESS)
   end subroutine parse_field_decl

   subroutine parse_methods(hconfig, spec, nlev, errmsg, rc)
      type(ESMF_HConfig), intent(in) :: hconfig
      type(CallbackStateSpec), intent(inout) :: spec
      integer, intent(in) :: nlev
      character(:), allocatable, intent(inout) :: errmsg
      integer, optional, intent(out) :: rc

      integer :: status, i
      logical :: has_key, is_map, is_null
      type(ESMF_HConfig) :: node, val
      character(len=MAX_NAME_LEN), allocatable :: labels(:)
      character(:), allocatable :: label
      type(CallbackMethodDecl) :: decl
      character(len=16) :: buf

      has_key = ESMF_HConfigIsDefined(hconfig, keyString='methods', _RC)
      _RETURN_UNLESS(has_key)

      node = ESMF_HConfigCreateAt(hconfig, keyString='methods', _RC)
      body: block
         is_map = ESMF_HConfigIsMap(node, _RC)
         if (.not. is_map) then
            errmsg = ERR_PREFIX // 'key <methods> must be a mapping from label to behavior'
            exit body
         end if

         labels = get_map_keys(node, _RC)
         if (size(labels) > MAX_CALLBACK_SLOTS) then
            write(buf, '(i0)') size(labels)
            errmsg = ERR_PREFIX // trim(buf) // ' methods configured, but at most '
            write(buf, '(i0)') MAX_CALLBACK_SLOTS
            errmsg = errmsg // trim(buf) // ' are supported'
            exit body
         end if

         do i = 1, size(labels)
            label = trim(labels(i))
            call check_name(label, 'methods', errmsg)
            if (allocated(errmsg)) exit body
            if (any(RESERVED_LABELS == label)) then
               errmsg = ERR_PREFIX // 'method label <' // label // '> is reserved'
               exit body
            end if

            val = ESMF_HConfigCreateAt(node, keyString=label, _RC)
            is_null = ESMF_HConfigIsNull(val, _RC)
            if (is_null) then
               call ESMF_HConfigDestroy(val, _RC)
               val = ESMF_HConfigCreate(content='{}', _RC)
            end if
            call parse_method_decl(val, label, spec, nlev, decl, errmsg, _RC)
            if (allocated(errmsg)) then
               call ESMF_HConfigDestroy(val, _RC)
               exit body
            end if
            ! Ownership of val passes to spec.
            decl%hconfig = val
            spec%methods = [spec%methods, decl]
         end do
      end block body
      call ESMF_HConfigDestroy(node, _RC)

      _RETURN(_SUCCESS)
   end subroutine parse_methods

   subroutine parse_method_decl(val, label, spec, nlev, decl, errmsg, rc)
      type(ESMF_HConfig), intent(in) :: val
      character(*), intent(in) :: label
      type(CallbackStateSpec), intent(in) :: spec
      integer, intent(in) :: nlev
      type(CallbackMethodDecl), intent(out) :: decl
      character(:), allocatable, intent(inout) :: errmsg
      integer, optional, intent(out) :: rc

      integer :: status
      logical :: is_map
      character(:), allocatable :: ctx

      ctx = 'methods/' // label
      is_map = ESMF_HConfigIsMap(val, _RC)
      if (.not. is_map) then
         errmsg = ERR_PREFIX // 'key <' // ctx // '> must be a mapping'
         _RETURN(_SUCCESS)
      end if
      call check_keys(val, [character(len=16) :: 'supply', 'record', 'fail_on_call'], ctx, errmsg, _RC)
      _RETURN_IF(allocated(errmsg))

      decl%label = label
      decl%records_fields = .false.
      call parse_supply(val, spec, nlev, ctx, errmsg, _RC)
      _RETURN_IF(allocated(errmsg))
      call parse_record(val, spec, ctx, decl%records_fields, errmsg, _RC)
      _RETURN_IF(allocated(errmsg))
      call parse_fail_on_call(val, ctx, errmsg, _RC)

      _RETURN(_SUCCESS)
   end subroutine parse_method_decl

   subroutine parse_supply(method, spec, nlev, method_ctx, errmsg, rc)
      type(ESMF_HConfig), intent(in) :: method
      type(CallbackStateSpec), intent(in) :: spec
      integer, intent(in) :: nlev
      character(*), intent(in) :: method_ctx
      character(:), allocatable, intent(inout) :: errmsg
      integer, optional, intent(out) :: rc

      integer :: status
      logical :: has_key, is_map
      type(ESMF_HConfig) :: supply
      character(:), allocatable :: ctx

      has_key = ESMF_HConfigIsDefined(method, keyString='supply', _RC)
      _RETURN_UNLESS(has_key)

      ctx = method_ctx // '/supply'
      supply = ESMF_HConfigCreateAt(method, keyString='supply', _RC)
      body: block
         is_map = ESMF_HConfigIsMap(supply, _RC)
         if (.not. is_map) then
            errmsg = ERR_PREFIX // 'key <' // ctx // '> must be a mapping'
            exit body
         end if
         call check_keys(supply, [character(len=16) :: 'attributes', 'fields'], ctx, errmsg, _RC)
         if (allocated(errmsg)) exit body
         call parse_supply_section(supply, 'attributes', FORM_ATTRIBUTE, spec, nlev, ctx, errmsg, _RC)
         if (allocated(errmsg)) exit body
         call parse_supply_section(supply, 'fields', FORM_FIELD, spec, nlev, ctx, errmsg, _RC)
      end block body
      call ESMF_HConfigDestroy(supply, _RC)

      _RETURN(_SUCCESS)
   end subroutine parse_supply

   subroutine parse_supply_section(supply, section, form_kind, spec, nlev, supply_ctx, errmsg, rc)
      type(ESMF_HConfig), intent(in) :: supply
      character(*), intent(in) :: section
      integer, intent(in) :: form_kind
      type(CallbackStateSpec), intent(in) :: spec
      integer, intent(in) :: nlev
      character(*), intent(in) :: supply_ctx
      character(:), allocatable, intent(inout) :: errmsg
      integer, optional, intent(out) :: rc

      integer :: status, i, idx
      logical :: has_key, is_map
      type(ESMF_HConfig) :: node, val
      character(len=MAX_NAME_LEN), allocatable :: names(:)
      character(:), allocatable :: ctx, dims

      has_key = ESMF_HConfigIsDefined(supply, keyString=section, _RC)
      _RETURN_UNLESS(has_key)

      ctx = supply_ctx // '/' // section
      node = ESMF_HConfigCreateAt(supply, keyString=section, _RC)
      body: block
         is_map = ESMF_HConfigIsMap(node, _RC)
         if (.not. is_map) then
            errmsg = ERR_PREFIX // 'key <' // ctx // '> must be a mapping'
            exit body
         end if
         names = get_map_keys(node, _RC)
         do i = 1, size(names)
            dims = ''
            if (form_kind == FORM_ATTRIBUTE) then
               call check_name(trim(names(i)), ctx, errmsg)
               if (allocated(errmsg)) exit body
            else
               idx = find_field(spec, trim(names(i)))
               if (idx == 0) then
                  errmsg = ERR_PREFIX // 'key <' // ctx // '/' // trim(names(i)) // &
                       '> names a field not declared under <fields>'
                  exit body
               end if
               dims = spec%fields(idx)%dims
            end if
            val = ESMF_HConfigCreateAt(node, keyString=trim(names(i)), _RC)
            call validate_form(val, form_kind, dims, nlev, ctx // '/' // trim(names(i)), errmsg, _RC)
            call ESMF_HConfigDestroy(val, _RC)
            if (allocated(errmsg)) exit body
         end do
      end block body
      call ESMF_HConfigDestroy(node, _RC)

      _RETURN(_SUCCESS)
   end subroutine parse_supply_section

   subroutine parse_record(method, spec, method_ctx, records_fields, errmsg, rc)
      type(ESMF_HConfig), intent(in) :: method
      type(CallbackStateSpec), intent(in) :: spec
      character(*), intent(in) :: method_ctx
      logical, intent(out) :: records_fields
      character(:), allocatable, intent(inout) :: errmsg
      integer, optional, intent(out) :: rc

      integer :: status, i
      logical :: has_key, is_map
      type(ESMF_HConfig) :: record
      character(len=MAX_NAME_LEN), allocatable :: names(:)
      character(:), allocatable :: ctx

      records_fields = .false.
      has_key = ESMF_HConfigIsDefined(method, keyString='record', _RC)
      _RETURN_UNLESS(has_key)

      ctx = method_ctx // '/record'
      record = ESMF_HConfigCreateAt(method, keyString='record', _RC)
      body: block
         is_map = ESMF_HConfigIsMap(record, _RC)
         if (.not. is_map) then
            errmsg = ERR_PREFIX // 'key <' // ctx // '> must be a mapping'
            exit body
         end if
         call check_keys(record, [character(len=16) :: 'attributes', 'fields'], ctx, errmsg, _RC)
         if (allocated(errmsg)) exit body

         has_key = ESMF_HConfigIsDefined(record, keyString='attributes', _RC)
         if (has_key) then
            call get_string_sequence(record, 'attributes', names, ctx, errmsg, _RC)
            if (allocated(errmsg)) exit body
            do i = 1, size(names)
               call check_name(trim(names(i)), ctx // '/attributes', errmsg)
               if (allocated(errmsg)) exit body
            end do
         end if

         has_key = ESMF_HConfigIsDefined(record, keyString='fields', _RC)
         if (has_key) then
            call get_string_sequence(record, 'fields', names, ctx, errmsg, _RC)
            if (allocated(errmsg)) exit body
            do i = 1, size(names)
               if (find_field(spec, trim(names(i))) == 0) then
                  errmsg = ERR_PREFIX // 'key <' // ctx // '/fields> names field <' // trim(names(i)) // &
                       '> not declared under <fields>'
                  exit body
               end if
            end do
            records_fields = size(names) > 0
         end if
      end block body
      call ESMF_HConfigDestroy(record, _RC)

      _RETURN(_SUCCESS)
   end subroutine parse_record

   subroutine parse_fail_on_call(method, method_ctx, errmsg, rc)
      type(ESMF_HConfig), intent(in) :: method
      character(*), intent(in) :: method_ctx
      character(:), allocatable, intent(inout) :: errmsg
      integer, optional, intent(out) :: rc

      integer :: status, i, call_no, ios, user_rc
      logical :: has_key, is_map
      type(ESMF_HConfig) :: node
      character(len=MAX_NAME_LEN), allocatable :: keys(:)
      character(:), allocatable :: ctx, tag

      has_key = ESMF_HConfigIsDefined(method, keyString='fail_on_call', _RC)
      _RETURN_UNLESS(has_key)

      ctx = method_ctx // '/fail_on_call'
      node = ESMF_HConfigCreateAt(method, keyString='fail_on_call', _RC)
      body: block
         is_map = ESMF_HConfigIsMap(node, _RC)
         if (.not. is_map) then
            errmsg = ERR_PREFIX // 'key <' // ctx // '> must be a mapping {call_number: rc}'
            exit body
         end if

         keys = get_map_keys(node, _RC)
         do i = 1, size(keys)
            read(keys(i), *, iostat=ios) call_no
            if (ios /= 0 .or. verify(trim(keys(i)), '0123456789') /= 0) call_no = 0
            if (call_no <= 0) then
               errmsg = ERR_PREFIX // 'key <' // ctx // '/' // trim(keys(i)) // '> must be a positive call number'
               exit body
            end if
            tag = ESMF_HConfigGetTag(node, keyString=trim(keys(i)), _RC)
            if (tag /= TAG_INT) then
               errmsg = ERR_PREFIX // 'value of <' // ctx // '/' // trim(keys(i)) // '> must be an integer rc'
               exit body
            end if
            user_rc = ESMF_HConfigAsI4(node, keyString=trim(keys(i)), _RC)
            if (user_rc == 0) then
               errmsg = ERR_PREFIX // 'value of <' // ctx // '/' // trim(keys(i)) // '> must be a non-zero rc'
               exit body
            end if
            if (user_rc == MOCK_INTERNAL_ERROR_RC) then
               errmsg = ERR_PREFIX // 'value of <' // ctx // '/' // trim(keys(i)) // &
                    '> is reserved for errors inside the mock'
               exit body
            end if
         end do
      end block body
      call ESMF_HConfigDestroy(node, _RC)

      _RETURN(_SUCCESS)
   end subroutine parse_fail_on_call

   ! A field form is a mapping with exactly one of constant, profile,
   ! by_attribute, by_call.  An attribute form is a literal value (scalar or
   ! list), or a mapping with exactly one of by_attribute, by_call.
   recursive subroutine validate_form(node, form_kind, dims, nlev, ctx, errmsg, rc)
      type(ESMF_HConfig), intent(in) :: node
      integer, intent(in) :: form_kind
      character(*), intent(in) :: dims
      integer, intent(in) :: nlev
      character(*), intent(in) :: ctx
      character(:), allocatable, intent(inout) :: errmsg
      integer, optional, intent(out) :: rc

      integer :: status, i, n
      logical :: is_map, is_seq, is_scalar, has_key, has_sibling_default
      character(len=MAX_NAME_LEN), allocatable :: keys(:)
      character(:), allocatable :: form, tag, str, forms_list
      type(ESMF_HConfig) :: sub, child
      character(len=16) :: buf

      if (form_kind == FORM_FIELD) then
         forms_list = 'constant, profile, by_attribute, by_call'
      else
         forms_list = 'by_attribute, by_call'
      end if

      is_map = ESMF_HConfigIsMap(node, _RC)
      if (.not. is_map) then
         if (form_kind == FORM_ATTRIBUTE) then
            call validate_literal(node, ctx, errmsg, _RC)
         else
            errmsg = ERR_PREFIX // 'key <' // ctx // '> must be a mapping with one of: ' // forms_list
         end if
         _RETURN(_SUCCESS)
      end if

      ! default may sit beside by_attribute instead of inside it.
      keys = get_map_keys(node, _RC)
      has_sibling_default = (size(keys) == 2 .and. any(keys == 'by_attribute') .and. any(keys == 'default'))
      if (size(keys) /= 1 .and. .not. has_sibling_default) then
         errmsg = ERR_PREFIX // 'key <' // ctx // '> must have exactly one of: ' // forms_list
         _RETURN(_SUCCESS)
      end if

      form = trim(keys(1))
      if (has_sibling_default) form = 'by_attribute'
      select case (form)
      case ('constant')
         if (form_kind /= FORM_FIELD) then
            call set_unknown_form()
            _RETURN(_SUCCESS)
         end if
         is_scalar = ESMF_HConfigIsScalar(node, keyString='constant', _RC)
         tag = ''
         if (is_scalar) tag = ESMF_HConfigGetTag(node, keyString='constant', _RC)
         if (.not. is_numeric_tag(tag)) then
            errmsg = ERR_PREFIX // 'key <' // ctx // '/constant> must be a number'
         end if

      case ('profile')
         if (form_kind /= FORM_FIELD) then
            call set_unknown_form()
            _RETURN(_SUCCESS)
         end if
         if (dims /= 'xyz') then
            errmsg = ERR_PREFIX // 'key <' // ctx // '/profile> requires a field with dims xyz'
            _RETURN(_SUCCESS)
         end if
         sub = ESMF_HConfigCreateAt(node, keyString='profile', _RC)
         profile: block
            is_seq = ESMF_HConfigIsSequence(sub, _RC)
            n = 0
            if (is_seq) n = ESMF_HConfigGetSize(sub, _RC)
            if (n /= nlev) then
               write(buf, '(i0)') nlev
               errmsg = ERR_PREFIX // 'key <' // ctx // '/profile> must be a list of ' // trim(buf) // ' numbers'
               exit profile
            end if
            do i = 1, n
               is_scalar = ESMF_HConfigIsScalar(sub, index=i, _RC)
               tag = ''
               if (is_scalar) tag = ESMF_HConfigGetTag(sub, index=i, _RC)
               if (.not. is_numeric_tag(tag)) then
                  errmsg = ERR_PREFIX // 'key <' // ctx // '/profile> must contain only numbers'
                  exit profile
               end if
            end do
         end block profile
         call ESMF_HConfigDestroy(sub, _RC)

      case ('by_attribute')
         sub = ESMF_HConfigCreateAt(node, keyString='by_attribute', _RC)
         by_attribute: block
            is_map = ESMF_HConfigIsMap(sub, _RC)
            if (.not. is_map) then
               errmsg = ERR_PREFIX // 'key <' // ctx // '/by_attribute> must be a mapping'
               exit by_attribute
            end if
            has_key = ESMF_HConfigIsDefined(sub, keyString='key', _RC)
            if (.not. has_key) then
               errmsg = ERR_PREFIX // 'key <' // ctx // '/by_attribute/key> is required'
               exit by_attribute
            end if
            call get_scalar_string(sub, 'key', str, errmsg, ctx // '/by_attribute', _RC)
            if (allocated(errmsg)) exit by_attribute
            has_key = ESMF_HConfigIsDefined(sub, keyString='default', _RC)
            if (has_key .and. has_sibling_default) then
               errmsg = ERR_PREFIX // 'key <' // ctx // '> has <default> both inside and beside <by_attribute>'
               exit by_attribute
            end if
            if (.not. (has_key .or. has_sibling_default)) then
               errmsg = ERR_PREFIX // 'key <' // ctx // '/by_attribute/default> is required (error, or a form)'
               exit by_attribute
            end if
            if (has_sibling_default) then
               child = ESMF_HConfigCreateAt(node, keyString='default', _RC)
               if (.not. is_error_marker(child)) then
                  call validate_form(child, form_kind, dims, nlev, ctx // '/default', errmsg, _RC)
               end if
               call ESMF_HConfigDestroy(child, _RC)
               if (allocated(errmsg)) exit by_attribute
            end if
            keys = get_map_keys(sub, _RC)
            do i = 1, size(keys)
               if (trim(keys(i)) == 'key') cycle
               child = ESMF_HConfigCreateAt(sub, keyString=trim(keys(i)), _RC)
               if (trim(keys(i)) /= 'default' .or. .not. is_error_marker(child)) then
                  call validate_form(child, form_kind, dims, nlev, ctx // '/by_attribute/' // trim(keys(i)), errmsg, _RC)
               end if
               call ESMF_HConfigDestroy(child, _RC)
               if (allocated(errmsg)) exit by_attribute
            end do
         end block by_attribute
         call ESMF_HConfigDestroy(sub, _RC)

      case ('by_call')
         sub = ESMF_HConfigCreateAt(node, keyString='by_call', _RC)
         by_call: block
            is_seq = ESMF_HConfigIsSequence(sub, _RC)
            n = 0
            if (is_seq) n = ESMF_HConfigGetSize(sub, _RC)
            if (n == 0) then
               errmsg = ERR_PREFIX // 'key <' // ctx // '/by_call> must be a non-empty list of forms'
               exit by_call
            end if
            do i = 1, n
               child = ESMF_HConfigCreateAt(sub, index=i, _RC)
               write(buf, '(i0)') i
               call validate_form(child, form_kind, dims, nlev, ctx // '/by_call[' // trim(buf) // ']', errmsg, _RC)
               call ESMF_HConfigDestroy(child, _RC)
               if (allocated(errmsg)) exit by_call
            end do
         end block by_call
         call ESMF_HConfigDestroy(sub, _RC)

      case ('function')
         errmsg = ERR_PREFIX // 'key <' // ctx // '/function>: the function supply form is not supported'

      case default
         call set_unknown_form()
      end select

      _RETURN(_SUCCESS)

   contains

      subroutine set_unknown_form()
         errmsg = ERR_PREFIX // 'unknown supply form <' // form // '> in <' // ctx // '>; expected one of: ' // forms_list
      end subroutine set_unknown_form

      logical function is_error_marker(hc)
         type(ESMF_HConfig), intent(in) :: hc
         integer :: local_status
         character(:), allocatable :: t
         is_error_marker = .false.
         if (.not. ESMF_HConfigIsScalar(hc, rc=local_status)) return
         t = ESMF_HConfigGetTag(hc, rc=local_status)
         if (t /= TAG_STR) return
         is_error_marker = (ESMF_HConfigAsString(hc, rc=local_status) == 'error')
      end function is_error_marker

   end subroutine validate_form

   ! A literal attribute value: a scalar, or a non-empty list of scalars of
   ! one kind (strings, logicals, or numbers).
   subroutine validate_literal(node, ctx, errmsg, rc)
      type(ESMF_HConfig), intent(in) :: node
      character(*), intent(in) :: ctx
      character(:), allocatable, intent(inout) :: errmsg
      integer, optional, intent(out) :: rc

      integer :: status, i, n
      logical :: is_scalar, is_seq
      character(:), allocatable :: tag, first_kind

      is_scalar = ESMF_HConfigIsScalar(node, _RC)
      if (is_scalar) then
         tag = ESMF_HConfigGetTag(node, _RC)
         if (tag_kind(tag) == '') then
            errmsg = ERR_PREFIX // 'key <' // ctx // '> has unsupported YAML type <' // tag // '>'
         end if
         _RETURN(_SUCCESS)
      end if

      is_seq = ESMF_HConfigIsSequence(node, _RC)
      if (.not. is_seq) then
         errmsg = ERR_PREFIX // 'key <' // ctx // '> must be a scalar or a list of scalars'
         _RETURN(_SUCCESS)
      end if

      n = ESMF_HConfigGetSize(node, _RC)
      if (n == 0) then
         errmsg = ERR_PREFIX // 'key <' // ctx // '> must not be an empty list'
         _RETURN(_SUCCESS)
      end if
      do i = 1, n
         is_scalar = ESMF_HConfigIsScalar(node, index=i, _RC)
         tag = ''
         if (is_scalar) tag = ESMF_HConfigGetTag(node, index=i, _RC)
         if (i == 1) first_kind = tag_kind(tag)
         if (tag_kind(tag) == '' .or. tag_kind(tag) /= first_kind) then
            errmsg = ERR_PREFIX // 'key <' // ctx // '> must be a list of strings, logicals, or numbers (not mixed)'
            _RETURN(_SUCCESS)
         end if
      end do

      _RETURN(_SUCCESS)
   end subroutine validate_literal

   function tag_kind(tag) result(kind)
      character(:), allocatable :: kind
      character(*), intent(in) :: tag
      select case (tag)
      case (TAG_STR)
         kind = 'string'
      case (TAG_BOOL)
         kind = 'logical'
      case (TAG_INT, TAG_FLOAT)
         kind = 'number'
      case default
         kind = ''
      end select
   end function tag_kind

   logical function is_numeric_tag(tag)
      character(*), intent(in) :: tag
      is_numeric_tag = (tag == TAG_INT .or. tag == TAG_FLOAT)
   end function is_numeric_tag

   integer function find_field(spec, name) result(idx)
      type(CallbackStateSpec), intent(in) :: spec
      character(*), intent(in) :: name
      integer :: i
      idx = 0
      do i = 1, size(spec%fields)
         if (spec%fields(i)%name == name) then
            idx = i
            return
         end if
      end do
   end function find_field

   ! Names become ESMF Info keys (JSON pointers, where / and ~ are special)
   ! and State item names.
   subroutine check_name(name, ctx, errmsg)
      character(*), intent(in) :: name
      character(*), intent(in) :: ctx
      character(:), allocatable, intent(inout) :: errmsg
      if (len_trim(name) == 0 .or. scan(name, '/~') > 0) then
         errmsg = ERR_PREFIX // 'name <' // name // '> in <' // ctx // '> must be non-empty and must not contain / or ~'
      end if
   end subroutine check_name

   subroutine check_keys(node, allowed, ctx, errmsg, rc)
      type(ESMF_HConfig), intent(in) :: node
      character(*), intent(in) :: allowed(:)
      character(*), intent(in) :: ctx
      character(:), allocatable, intent(inout) :: errmsg
      integer, optional, intent(out) :: rc

      integer :: status, i
      character(len=MAX_NAME_LEN), allocatable :: keys(:)
      character(:), allocatable :: allowed_list

      keys = get_map_keys(node, _RC)
      do i = 1, size(keys)
         if (any(allowed == trim(keys(i)))) cycle
         allowed_list = trim(allowed(1))
         block
           integer :: j
           do j = 2, size(allowed)
              allowed_list = allowed_list // ', ' // trim(allowed(j))
           end do
         end block
         errmsg = ERR_PREFIX // 'unknown key <' // trim(keys(i)) // '> in <' // ctx // '>; allowed: ' // allowed_list
         _RETURN(_SUCCESS)
      end do

      _RETURN(_SUCCESS)
   end subroutine check_keys

   subroutine get_scalar_string(node, key, value, errmsg, ctx, rc)
      type(ESMF_HConfig), intent(in) :: node
      character(*), intent(in) :: key
      character(:), allocatable, intent(out) :: value
      character(:), allocatable, intent(inout) :: errmsg
      character(*), intent(in) :: ctx
      integer, optional, intent(out) :: rc

      integer :: status
      logical :: is_scalar
      character(:), allocatable :: tag

      is_scalar = ESMF_HConfigIsScalar(node, keyString=key, _RC)
      tag = ''
      if (is_scalar) tag = ESMF_HConfigGetTag(node, keyString=key, _RC)
      if (tag /= TAG_STR) then
         errmsg = ERR_PREFIX // 'key <' // ctx // '/' // key // '> must be a string'
         value = ''
         _RETURN(_SUCCESS)
      end if
      value = ESMF_HConfigAsString(node, keyString=key, _RC)

      _RETURN(_SUCCESS)
   end subroutine get_scalar_string

   subroutine get_string_sequence(node, key, values, ctx, errmsg, rc)
      type(ESMF_HConfig), intent(in) :: node
      character(*), intent(in) :: key
      character(len=MAX_NAME_LEN), allocatable, intent(out) :: values(:)
      character(*), intent(in) :: ctx
      character(:), allocatable, intent(inout) :: errmsg
      integer, optional, intent(out) :: rc

      integer :: status, i, n
      logical :: is_seq, is_scalar
      type(ESMF_HConfig) :: seq
      character(:), allocatable :: tag

      allocate(values(0))
      seq = ESMF_HConfigCreateAt(node, keyString=key, _RC)
      body: block
         is_seq = ESMF_HConfigIsSequence(seq, _RC)
         if (.not. is_seq) then
            errmsg = ERR_PREFIX // 'key <' // ctx // '/' // key // '> must be a list of names'
            exit body
         end if
         n = ESMF_HConfigGetSize(seq, _RC)
         deallocate(values)
         allocate(values(n))
         do i = 1, n
            is_scalar = ESMF_HConfigIsScalar(seq, index=i, _RC)
            tag = ''
            if (is_scalar) tag = ESMF_HConfigGetTag(seq, index=i, _RC)
            if (tag /= TAG_STR) then
               errmsg = ERR_PREFIX // 'key <' // ctx // '/' // key // '> must be a list of names'
               exit body
            end if
            values(i) = ESMF_HConfigAsString(seq, index=i, _RC)
         end do
      end block body
      call ESMF_HConfigDestroy(seq, _RC)

      _RETURN(_SUCCESS)
   end subroutine get_string_sequence

   function get_map_keys(node, rc) result(keys)
      character(len=MAX_NAME_LEN), allocatable :: keys(:)
      type(ESMF_HConfig), intent(in) :: node
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_HConfigIter) :: iter, iter_begin, iter_end
      character(len=MAX_NAME_LEN) :: key

      allocate(keys(0))
      iter_begin = ESMF_HConfigIterBegin(node, _RC)
      iter_end = ESMF_HConfigIterEnd(node, _RC)
      iter = iter_begin
      do while (ESMF_HConfigIterLoop(iter, iter_begin, iter_end, rc=status))
         _VERIFY(status)
         key = ESMF_HConfigAsStringMapKey(iter, _RC)
         keys = [keys, key]
      end do

      _RETURN(_SUCCESS)
   end function get_map_keys

   ! Flow-style YAML with every string quoted, so reparsing keeps scalar
   ! types (ESMF_InfoSet cannot store a sequence of mappings).  Child text
   ! goes through a local because gfortran truncates the result otherwise.
   recursive function to_flow_yaml(node, rc) result(text)
      character(:), allocatable :: text
      type(ESMF_HConfig), intent(in) :: node
      integer, optional, intent(out) :: rc

      integer :: status, i, n
      logical :: is_map, is_seq, is_null
      type(ESMF_HConfig) :: child
      character(len=MAX_NAME_LEN), allocatable :: keys(:)
      character(:), allocatable :: tag, child_text

      text = ''
      is_map = ESMF_HConfigIsMap(node, _RC)
      if (is_map) then
         keys = get_map_keys(node, _RC)
         text = '{'
         do i = 1, size(keys)
            child = ESMF_HConfigCreateAt(node, keyString=trim(keys(i)), _RC)
            child_text = to_flow_yaml(child, rc=status)
            _VERIFY(status)
            if (i > 1) text = text // ', '
            text = text // quote(trim(keys(i))) // ': ' // child_text
            call ESMF_HConfigDestroy(child, _RC)
         end do
         text = text // '}'
         _RETURN(_SUCCESS)
      end if

      is_seq = ESMF_HConfigIsSequence(node, _RC)
      if (is_seq) then
         n = ESMF_HConfigGetSize(node, _RC)
         text = '['
         do i = 1, n
            child = ESMF_HConfigCreateAt(node, index=i, _RC)
            child_text = to_flow_yaml(child, rc=status)
            _VERIFY(status)
            if (i > 1) text = text // ', '
            text = text // child_text
            call ESMF_HConfigDestroy(child, _RC)
         end do
         text = text // ']'
         _RETURN(_SUCCESS)
      end if

      is_null = ESMF_HConfigIsNull(node, _RC)
      if (is_null) then
         text = 'null'
         _RETURN(_SUCCESS)
      end if

      tag = ESMF_HConfigGetTag(node, _RC)
      if (tag == TAG_STR) then
         text = quote(ESMF_HConfigAsString(node, rc=status))
         _VERIFY(status)
      else
         text = ESMF_HConfigAsString(node, _RC)
      end if

      _RETURN(_SUCCESS)

   contains

      function quote(str) result(quoted)
         character(:), allocatable :: quoted
         character(*), intent(in) :: str
         character(*), parameter :: BACKSLASH = achar(92)
         integer :: j
         quoted = '"'
         do j = 1, len(str)
            if (str(j:j) == '"' .or. str(j:j) == BACKSLASH) quoted = quoted // BACKSLASH
            quoted = quoted // str(j:j)
         end do
         quoted = quoted // '"'
      end function quote

   end function to_flow_yaml

end module mapl_ConfigurableCallbackSpec_mod
