#include "MAPL.h"

! A configurable callback State: an ESMF_State whose methods are attached by
! fixed slot procedures and whose behavior (supply, record, fail) is read
! from the State's own Info, so several mock States can coexist.
!
! Client-visible attributes use the legacy ESMF_Attribute calls, which live
! under the Info key ESMF/General/; mock bookkeeping lives under mapl_mock/.
module mapl_ConfigurableCallback_mod
   use ESMF
   use mapl_ErrorHandling_mod
   use mapl_KeywordEnforcer_mod
   use mapl_StateAddMethod_mod, only: StateAddMethod
   use mapl_FieldPointerUtilities_mod, only: FieldClone, FieldCopy
   use mapl_ConfigurableCallbackSpec_mod
   implicit none(type,external)
   private

   public :: ConfigurableCallbackCreate
   public :: ConfigurableCallbackCallCount
   public :: ConfigurableCallbackCalls
   public :: ConfigurableCallbackAttrSeen
   public :: ConfigurableCallbackFieldSeen
   public :: ConfigurableCallbackRecordName
   public :: MAX_CALLBACK_SLOTS
   public :: MOCK_INTERNAL_ERROR_RC

   character(*), parameter :: MOCK_NS = 'mapl_mock'
   character(*), parameter :: KEY_LABELS = MOCK_NS // '/labels'
   character(*), parameter :: KEY_CALLS = MOCK_NS // '/calls'
   character(*), parameter :: KEY_NUM_LEVELS = MOCK_NS // '/num_levels'
   character(*), parameter :: RECORD_STATE_NAME = 'mapl_mock_record'

contains

   ! Builds a new State from hconfig: creates fields on grid, sets
   ! state_attributes, and attaches one slot procedure per configured method.
   subroutine ConfigurableCallbackCreate(state, hconfig, unusable, grid, num_levels, name, rc)
      type(ESMF_State), intent(out) :: state
      type(ESMF_HConfig), intent(in) :: hconfig
      class(KeywordEnforcer), optional, intent(in) :: unusable
      type(ESMF_Grid), optional, intent(in) :: grid
      integer, optional, intent(in) :: num_levels
      character(*), optional, intent(in) :: name
      integer, optional, intent(out) :: rc

      integer :: status, destroy_status, i, nlev
      type(CallbackStateSpec) :: spec
      character(:), allocatable :: errmsg
      type(ESMF_Info) :: info
      type(ESMF_State) :: record_state
      character(len=MAX_NAME_LEN), allocatable :: labels(:)
      character(:), allocatable :: config_text

      nlev = 0
      if (present(num_levels)) nlev = num_levels

      call parse_callback_state_spec(hconfig, spec, errmsg, num_levels=nlev, rc=status)
      if (status == _SUCCESS .and. .not. allocated(errmsg)) then
         if (size(spec%fields) > 0 .and. .not. present(grid)) then
            errmsg = ERR_PREFIX // 'fields are configured, so a grid must be passed'
         end if
      end if
      if (status /= _SUCCESS .or. allocated(errmsg)) then
         call destroy_callback_state_spec(spec, rc=destroy_status)
         _VERIFY(status)
         _VERIFY(destroy_status)
         _FAIL(errmsg)
      end if

      if (present(name)) then
         state = ESMF_StateCreate(name=name, _RC)
      else
         state = ESMF_StateCreate(_RC)
      end if

      call ESMF_InfoGetFromHost(state, info, _RC)
      call ESMF_InfoSet(info, key=KEY_NUM_LEVELS, value=nlev, _RC)

      do i = 1, size(spec%fields)
         call add_field(state, spec%fields(i), grid, nlev, _RC)
      end do

      if (spec%has_state_attributes) then
         call set_attributes_from_map(state, spec%state_attributes, _RC)
      end if

      ! Only added when needed, so clients that list State items see no extra item.
      if (any(spec%methods%records_fields)) then
         record_state = ESMF_StateCreate(name=RECORD_STATE_NAME, _RC)
         call ESMF_StateAdd(state, [record_state], _RC)
      end if

      allocate(labels(size(spec%methods)))
      do i = 1, size(spec%methods)
         labels(i) = spec%methods(i)%label
      end do
      if (size(labels) > 0) then
         call ESMF_InfoSet(info, key=KEY_LABELS, values=labels, _RC)
      end if

      do i = 1, size(spec%methods)
         associate (label => spec%methods(i)%label)
           config_text = to_flow_yaml(spec%methods(i)%hconfig, _RC)
           call ESMF_InfoSet(info, key=method_key(label, 'config'), value=config_text, _RC)
           call ESMF_InfoSet(info, key=method_key(label, 'slot'), value=i, _RC)
           call ESMF_InfoSet(info, key=method_key(label, 'call_count'), value=0, _RC)
           call attach_slot(state, label, i, _RC)
         end associate
      end do
      call destroy_callback_state_spec(spec, _RC)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine ConfigurableCallbackCreate

   integer function ConfigurableCallbackCallCount(state, label, rc) result(count)
      type(ESMF_State), intent(in) :: state
      character(*), intent(in) :: label
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_Info) :: info

      count = -1
      call get_mock_info(state, info, _RC)
      call check_label(info, label, _RC)
      call ESMF_InfoGet(info, key=method_key(label, 'call_count'), value=count, _RC)

      _RETURN(_SUCCESS)
   end function ConfigurableCallbackCallCount

   ! Labels of all mock methods in the order they were called.
   subroutine ConfigurableCallbackCalls(state, labels, rc)
      type(ESMF_State), intent(in) :: state
      character(:), allocatable, intent(out) :: labels(:)
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_Info) :: info

      call get_mock_info(state, info, _RC)
      call get_string_list(info, KEY_CALLS, labels, _RC)

      _RETURN(_SUCCESS)
   end subroutine ConfigurableCallbackCalls

   ! Values of attribute name seen at each call of label, as strings; an
   ! attribute that was absent at a call is recorded as an empty string.
   subroutine ConfigurableCallbackAttrSeen(state, label, name, values, rc)
      type(ESMF_State), intent(in) :: state
      character(*), intent(in) :: label
      character(*), intent(in) :: name
      character(:), allocatable, intent(out) :: values(:)
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_Info) :: info
      type(ESMF_HConfig) :: config
      character(len=MAX_NAME_LEN), allocatable :: recorded(:)

      call get_mock_info(state, info, _RC)
      call check_label(info, label, _RC)
      call load_method_config(info, label, config, _RC)
      call get_record_names(config, 'attributes', recorded, _RC)
      call ESMF_HConfigDestroy(config, _RC)
      _ASSERT(any(recorded == name), ERR_PREFIX // 'method <' // label // '> does not record attribute <' // name // '>')

      call get_string_list(info, method_key(label, 'attr/' // name), values, _RC)

      _RETURN(_SUCCESS)
   end subroutine ConfigurableCallbackAttrSeen

   ! Copy of field name as it was at call call_no of label.
   subroutine ConfigurableCallbackFieldSeen(state, label, name, call_no, field, rc)
      type(ESMF_State), intent(in) :: state
      character(*), intent(in) :: label
      character(*), intent(in) :: name
      integer, intent(in) :: call_no
      type(ESMF_Field), intent(out) :: field
      integer, optional, intent(out) :: rc

      integer :: status, count
      type(ESMF_Info) :: info
      type(ESMF_HConfig) :: config
      character(len=MAX_NAME_LEN), allocatable :: recorded(:)
      type(ESMF_State) :: record_state
      character(len=16) :: buf

      call get_mock_info(state, info, _RC)
      call check_label(info, label, _RC)
      call load_method_config(info, label, config, _RC)
      call get_record_names(config, 'fields', recorded, _RC)
      call ESMF_HConfigDestroy(config, _RC)
      _ASSERT(any(recorded == name), ERR_PREFIX // 'method <' // label // '> does not record field <' // name // '>')

      call ESMF_InfoGet(info, key=method_key(label, 'call_count'), value=count, _RC)
      write(buf, '(i0)') call_no
      _ASSERT(call_no >= 1 .and. call_no <= count, ERR_PREFIX // 'method <' // label // '> has no call ' // trim(buf))

      call ESMF_StateGet(state, itemName=RECORD_STATE_NAME, nestedState=record_state, _RC)
      call ESMF_StateGet(record_state, itemName=ConfigurableCallbackRecordName(label, name, call_no), field=field, _RC)

      _RETURN(_SUCCESS)
   end subroutine ConfigurableCallbackFieldSeen

   ! Name of the recorded copy of field name at call call_no of label, in the
   ! nested State mapl_mock_record.
   function ConfigurableCallbackRecordName(label, name, call_no) result(record_name)
      character(:), allocatable :: record_name
      character(*), intent(in) :: label
      character(*), intent(in) :: name
      integer, intent(in) :: call_no
      character(len=16) :: buf
      write(buf, '(i0)') call_no
      record_name = label // ':' // name // ':' // trim(buf)
   end function ConfigurableCallbackRecordName

   ! ESMF does not pass the label to the attached routine, so each slot is a
   ! distinct procedure that knows its own index.
   subroutine slot_1(state, rc)
      type(ESMF_State) :: state
      integer, intent(out) :: rc
      call run_slot(state, 1, rc)
   end subroutine slot_1

   subroutine slot_2(state, rc)
      type(ESMF_State) :: state
      integer, intent(out) :: rc
      call run_slot(state, 2, rc)
   end subroutine slot_2

   subroutine slot_3(state, rc)
      type(ESMF_State) :: state
      integer, intent(out) :: rc
      call run_slot(state, 3, rc)
   end subroutine slot_3

   subroutine slot_4(state, rc)
      type(ESMF_State) :: state
      integer, intent(out) :: rc
      call run_slot(state, 4, rc)
   end subroutine slot_4

   subroutine slot_5(state, rc)
      type(ESMF_State) :: state
      integer, intent(out) :: rc
      call run_slot(state, 5, rc)
   end subroutine slot_5

   subroutine slot_6(state, rc)
      type(ESMF_State) :: state
      integer, intent(out) :: rc
      call run_slot(state, 6, rc)
   end subroutine slot_6

   subroutine slot_7(state, rc)
      type(ESMF_State) :: state
      integer, intent(out) :: rc
      call run_slot(state, 7, rc)
   end subroutine slot_7

   subroutine slot_8(state, rc)
      type(ESMF_State) :: state
      integer, intent(out) :: rc
      call run_slot(state, 8, rc)
   end subroutine slot_8

   subroutine attach_slot(state, label, slot, rc)
      type(ESMF_State), intent(inout) :: state
      character(*), intent(in) :: label
      integer, intent(in) :: slot
      integer, optional, intent(out) :: rc

      integer :: status

      select case (slot)
      case (1)
         call StateAddMethod(state, label, slot_1, _RC)
      case (2)
         call StateAddMethod(state, label, slot_2, _RC)
      case (3)
         call StateAddMethod(state, label, slot_3, _RC)
      case (4)
         call StateAddMethod(state, label, slot_4, _RC)
      case (5)
         call StateAddMethod(state, label, slot_5, _RC)
      case (6)
         call StateAddMethod(state, label, slot_6, _RC)
      case (7)
         call StateAddMethod(state, label, slot_7, _RC)
      case (8)
         call StateAddMethod(state, label, slot_8, _RC)
      case default
         _FAIL(ERR_PREFIX // 'no slot procedure for method <' // label // '>')
      end select

      _RETURN(_SUCCESS)
   end subroutine attach_slot

   ! Any failure other than a configured fail_on_call is reported as
   ! MOCK_INTERNAL_ERROR_RC, not as the generic rc of the error macros.
   subroutine run_slot(state, slot, rc)
      type(ESMF_State) :: state
      integer, intent(in) :: slot
      integer, intent(out) :: rc

      integer :: status, user_rc

      call execute_slot(state, slot, user_rc, rc=status)
      _ASSERT_RC(status == _SUCCESS, ERR_PREFIX // 'mock method failed', MOCK_INTERNAL_ERROR_RC)
      rc = user_rc
   end subroutine run_slot

   ! user_rc is the fail_on_call code configured for this call, or 0.
   subroutine execute_slot(state, slot, user_rc, rc)
      type(ESMF_State) :: state
      integer, intent(in) :: slot
      integer, intent(out) :: user_rc
      integer, optional, intent(out) :: rc

      integer :: status, destroy_status, call_no, nlev
      type(ESMF_Info) :: info
      type(ESMF_HConfig) :: config
      character(:), allocatable :: labels(:), label

      user_rc = 0
      call ESMF_InfoGetFromHost(state, info, _RC)
      call get_string_list(info, KEY_LABELS, labels, _RC)
      _ASSERT(slot <= size(labels), ERR_PREFIX // 'slot procedure called on a State with no method in that slot')
      label = trim(labels(slot))
      call ESMF_InfoGet(info, key=KEY_NUM_LEVELS, value=nlev, _RC)

      call ESMF_InfoGet(info, key=method_key(label, 'call_count'), value=call_no, _RC)
      call_no = call_no + 1
      call ESMF_InfoSet(info, key=method_key(label, 'call_count'), value=call_no, _RC)
      call append_string(info, KEY_CALLS, label, _RC)

      call load_method_config(info, label, config, _RC)
      ! config is destroyed before any status is acted on, so no path leaks it.
      call record_inputs(state, info, label, config, call_no, rc=status)
      if (status == _SUCCESS) user_rc = get_fail_rc(config, call_no, rc=status)
      if (status == _SUCCESS .and. user_rc == 0) then
         call supply_outputs(state, label, config, call_no, nlev, rc=status)
      end if
      call ESMF_HConfigDestroy(config, rc=destroy_status)
      _VERIFY(status)
      _VERIFY(destroy_status)

      _RETURN(_SUCCESS)
   end subroutine execute_slot

   subroutine record_inputs(state, info, label, config, call_no, rc)
      type(ESMF_State), intent(inout) :: state
      type(ESMF_Info), intent(inout) :: info
      character(*), intent(in) :: label
      type(ESMF_HConfig), intent(in) :: config
      integer, intent(in) :: call_no
      integer, optional, intent(out) :: rc

      integer :: status, i
      character(len=MAX_NAME_LEN), allocatable :: names(:)
      character(:), allocatable :: value
      logical :: is_present
      type(ESMF_State) :: record_state
      type(ESMF_Field) :: field, copy

      call get_record_names(config, 'attributes', names, _RC)
      do i = 1, size(names)
         call get_attribute_as_string(state, trim(names(i)), value, is_present, _RC)
         call append_string(info, method_key(label, 'attr/' // trim(names(i))), value, _RC)
      end do

      call get_record_names(config, 'fields', names, _RC)
      if (size(names) > 0) then
         call ESMF_StateGet(state, itemName=RECORD_STATE_NAME, nestedState=record_state, _RC)
      end if
      do i = 1, size(names)
         call ESMF_StateGet(state, itemName=trim(names(i)), field=field, _RC)
         call FieldClone(field, copy, name=ConfigurableCallbackRecordName(label, trim(names(i)), call_no), _RC)
         call FieldCopy(field, copy, _RC)
         call ESMF_StateAdd(record_state, [copy], _RC)
      end do

      _RETURN(_SUCCESS)
   end subroutine record_inputs

   integer function get_fail_rc(config, call_no, rc) result(user_rc)
      type(ESMF_HConfig), intent(in) :: config
      integer, intent(in) :: call_no
      integer, optional, intent(out) :: rc

      integer :: status
      logical :: has_key
      type(ESMF_HConfig) :: node
      character(len=16) :: buf

      user_rc = 0
      has_key = ESMF_HConfigIsDefined(config, keyString='fail_on_call', _RC)
      _RETURN_UNLESS(has_key)

      node = ESMF_HConfigCreateAt(config, keyString='fail_on_call', _RC)
      write(buf, '(i0)') call_no
      has_key = ESMF_HConfigIsDefined(node, keyString=trim(buf), _RC)
      if (has_key) then
         user_rc = ESMF_HConfigAsI4(node, keyString=trim(buf), _RC)
      end if
      call ESMF_HConfigDestroy(node, _RC)

      _RETURN(_SUCCESS)
   end function get_fail_rc

   ! Every form is resolved against the client inputs before any output is
   ! written, so a supplied attribute cannot change another selection.
   subroutine supply_outputs(state, label, config, call_no, nlev, rc)
      type(ESMF_State), intent(inout) :: state
      character(*), intent(in) :: label
      type(ESMF_HConfig), intent(in) :: config
      integer, intent(in) :: call_no
      integer, intent(in) :: nlev
      integer, optional, intent(out) :: rc

      integer :: status, i, n_fields, n_attrs
      logical :: has_key
      type(ESMF_HConfig) :: supply
      type(ESMF_HConfig), allocatable :: field_forms(:), attr_forms(:)
      character(len=MAX_NAME_LEN), allocatable :: field_names(:), attr_names(:)

      has_key = ESMF_HConfigIsDefined(config, keyString='supply', _RC)
      _RETURN_UNLESS(has_key)
      supply = ESMF_HConfigCreateAt(config, keyString='supply', _RC)

      ! Handles created below are released by release_handles on every path.
      n_fields = 0
      n_attrs = 0
      call resolve_section('fields', field_names, field_forms, n_fields, status)
      if (status == _SUCCESS) then
         call resolve_section('attributes', attr_names, attr_forms, n_attrs, status)
      end if
      if (status == _SUCCESS) then
         do i = 1, n_fields
            call fill_field(state, trim(field_names(i)), field_forms(i), nlev, rc=status)
            if (status /= _SUCCESS) exit
         end do
      end if
      if (status == _SUCCESS) then
         do i = 1, n_attrs
            call set_attribute(state, trim(attr_names(i)), attr_forms(i), rc=status)
            if (status /= _SUCCESS) exit
         end do
      end if
      call release_handles(status)
      _VERIFY(status)

      _RETURN(_SUCCESS)

   contains

      ! On return, forms(1:n_resolved) are owned handles, also on failure.
      subroutine resolve_section(section, names, forms, n_resolved, stat)
         character(*), intent(in) :: section
         character(len=MAX_NAME_LEN), allocatable, intent(out) :: names(:)
         type(ESMF_HConfig), allocatable, intent(out) :: forms(:)
         integer, intent(out) :: n_resolved
         integer, intent(out) :: stat

         integer :: j, destroy_stat
         logical :: has_section
         type(ESMF_HConfig) :: node, form

         allocate(names(0), forms(0))
         n_resolved = 0
         has_section = ESMF_HConfigIsDefined(supply, keyString=section, rc=stat)
         if (stat /= _SUCCESS .or. .not. has_section) return
         node = ESMF_HConfigCreateAt(supply, keyString=section, rc=stat)
         if (stat /= _SUCCESS) return

         names = get_map_keys(node, rc=stat)
         if (stat == _SUCCESS) then
            deallocate(forms)
            allocate(forms(size(names)))
            do j = 1, size(names)
               form = ESMF_HConfigCreateAt(node, keyString=trim(names(j)), rc=stat)
               if (stat /= _SUCCESS) exit
               call resolve_form(state, label, call_no, form, 'supply/' // section // '/' // trim(names(j)), &
                    forms(j), rc=stat)
               call ESMF_HConfigDestroy(form, rc=destroy_stat)
               if (stat /= _SUCCESS) exit
               n_resolved = j
               stat = destroy_stat
               if (stat /= _SUCCESS) exit
            end do
         end if

         call ESMF_HConfigDestroy(node, rc=destroy_stat)
         if (stat == _SUCCESS) stat = destroy_stat
      end subroutine resolve_section

      ! Keeps the first failure in stat.
      subroutine release_handles(stat)
         integer, intent(inout) :: stat
         integer :: j, destroy_stat
         do j = 1, n_fields
            call ESMF_HConfigDestroy(field_forms(j), rc=destroy_stat)
            if (stat == _SUCCESS) stat = destroy_stat
         end do
         do j = 1, n_attrs
            call ESMF_HConfigDestroy(attr_forms(j), rc=destroy_stat)
            if (stat == _SUCCESS) stat = destroy_stat
         end do
         call ESMF_HConfigDestroy(supply, rc=destroy_stat)
         if (stat == _SUCCESS) stat = destroy_stat
      end subroutine release_handles

   end subroutine supply_outputs

   ! Reduces by_attribute and by_call to a terminal form: a constant or
   ! profile mapping for a field, a literal for an attribute.  resolved is a
   ! new handle owned by the caller; form is left untouched.
   recursive subroutine resolve_form(state, label, call_no, form, ctx, resolved, rc)
      type(ESMF_State), intent(inout) :: state
      character(*), intent(in) :: label
      integer, intent(in) :: call_no
      type(ESMF_HConfig), intent(in) :: form
      character(*), intent(in) :: ctx
      type(ESMF_HConfig), intent(out) :: resolved
      integer, optional, intent(out) :: rc

      integer :: status, destroy_status
      logical :: is_map, by_attribute, by_call
      type(ESMF_HConfig) :: sub, child
      character(:), allocatable :: where

      by_attribute = .false.
      by_call = .false.
      is_map = ESMF_HConfigIsMap(form, _RC)
      if (is_map) then
         by_attribute = ESMF_HConfigIsDefined(form, keyString='by_attribute', _RC)
         if (.not. by_attribute) by_call = ESMF_HConfigIsDefined(form, keyString='by_call', _RC)
      end if
      if (.not. (by_attribute .or. by_call)) then
         resolved = ESMF_HConfigCreateAt(form, _RC)
         _RETURN(_SUCCESS)
      end if

      where = ERR_PREFIX // 'method <' // label // '>, <' // ctx // '>: '
      if (by_attribute) then
         sub = ESMF_HConfigCreateAt(form, keyString='by_attribute', _RC)
         call select_by_attribute(state, form, sub, where, child, rc=status)
      else
         sub = ESMF_HConfigCreateAt(form, keyString='by_call', _RC)
         call select_by_call(sub, call_no, where, child, rc=status)
      end if
      call ESMF_HConfigDestroy(sub, rc=destroy_status)
      _VERIFY(status)
      _VERIFY(destroy_status)

      call resolve_form(state, label, call_no, child, ctx, resolved, rc=status)
      call ESMF_HConfigDestroy(child, rc=destroy_status)
      _VERIFY(status)
      _VERIFY(destroy_status)

      _RETURN(_SUCCESS)
   end subroutine resolve_form

   ! child is created only on success.  default may sit inside by_attribute
   ! (sub) or beside it (form).
   subroutine select_by_attribute(state, form, sub, where, child, rc)
      type(ESMF_State), intent(inout) :: state
      type(ESMF_HConfig), intent(in) :: form
      type(ESMF_HConfig), intent(in) :: sub
      character(*), intent(in) :: where
      type(ESMF_HConfig), intent(out) :: child
      integer, optional, intent(out) :: rc

      integer :: status
      logical :: has_key, is_present
      character(:), allocatable :: key, value

      key = ESMF_HConfigAsString(sub, keyString='key', _RC)
      call get_attribute_as_string(state, key, value, is_present, _RC)
      _ASSERT(is_present, where // 'by_attribute key <' // key // '> is not set on the State')

      ! key and default are the form keywords, never attribute values.
      has_key = (value /= 'key' .and. value /= 'default' .and. len(value) > 0)
      if (has_key) has_key = ESMF_HConfigIsDefined(sub, keyString=value, _RC)
      if (has_key) then
         child = ESMF_HConfigCreateAt(sub, keyString=value, _RC)
         _RETURN(_SUCCESS)
      end if

      has_key = ESMF_HConfigIsDefined(sub, keyString='default', _RC)
      if (has_key) then
         child = ESMF_HConfigCreateAt(sub, keyString='default', _RC)
      else
         child = ESMF_HConfigCreateAt(form, keyString='default', _RC)
      end if
      if (is_error_string(child)) then
         call ESMF_HConfigDestroy(child, _RC)
         _FAIL(where // 'by_attribute key <' // key // '> has unlisted value <' // value // '>')
      end if

      _RETURN(_SUCCESS)
   end subroutine select_by_attribute

   ! child is created only on success.
   subroutine select_by_call(sub, call_no, where, child, rc)
      type(ESMF_HConfig), intent(in) :: sub
      integer, intent(in) :: call_no
      character(*), intent(in) :: where
      type(ESMF_HConfig), intent(out) :: child
      integer, optional, intent(out) :: rc

      integer :: status, n
      character(len=16) :: buf, buf2

      n = ESMF_HConfigGetSize(sub, _RC)
      write(buf, '(i0)') call_no
      write(buf2, '(i0)') n
      _ASSERT(call_no <= n, where // 'by_call has ' // trim(buf2) // ' forms but this is call ' // trim(buf))
      child = ESMF_HConfigCreateAt(sub, index=call_no, _RC)

      _RETURN(_SUCCESS)
   end subroutine select_by_call

   logical function is_error_string(node)
      type(ESMF_HConfig), intent(in) :: node
      integer :: status
      is_error_string = .false.
      if (.not. ESMF_HConfigIsScalar(node, rc=status)) return
      if (ESMF_HConfigGetTag(node, rc=status) /= TAG_STR) return
      is_error_string = (ESMF_HConfigAsString(node, rc=status) == 'error')
   end function is_error_string

   subroutine fill_field(state, name, form, nlev, rc)
      type(ESMF_State), intent(inout) :: state
      character(*), intent(in) :: name
      type(ESMF_HConfig), intent(in) :: form
      integer, intent(in) :: nlev
      integer, optional, intent(out) :: rc

      integer :: status, rank, k
      logical :: has_key
      type(ESMF_Field) :: field
      type(ESMF_TypeKind_Flag) :: typekind
      real(kind=ESMF_KIND_R8), allocatable :: levels(:)
      real(kind=ESMF_KIND_R4), pointer :: r4_2d(:,:), r4_3d(:,:,:)
      real(kind=ESMF_KIND_R8), pointer :: r8_2d(:,:), r8_3d(:,:,:)
      character(:), allocatable :: mismatch

      mismatch = ERR_PREFIX // 'level count mismatch for field <' // name // '>'
      has_key = ESMF_HConfigIsDefined(form, keyString='constant', _RC)
      if (has_key) then
         allocate(levels(max(nlev, 1)))
         levels = ESMF_HConfigAsR8(form, keyString='constant', _RC)
      else
         levels = ESMF_HConfigAsR8Seq(form, keyString='profile', _RC)
      end if

      call ESMF_StateGet(state, itemName=name, field=field, _RC)
      call ESMF_FieldGet(field, rank=rank, typekind=typekind, _RC)

      if (typekind == ESMF_TYPEKIND_R4 .and. rank == 2) then
         call ESMF_FieldGet(field, farrayPtr=r4_2d, _RC)
         r4_2d = real(levels(1), kind=ESMF_KIND_R4)
      else if (typekind == ESMF_TYPEKIND_R4 .and. rank == 3) then
         call ESMF_FieldGet(field, farrayPtr=r4_3d, _RC)
         _ASSERT(size(r4_3d, 3) == size(levels) .or. size(levels) == 1, mismatch)
         do k = 1, size(r4_3d, 3)
            r4_3d(:,:,lbound(r4_3d,3)+k-1) = real(levels(min(k, size(levels))), kind=ESMF_KIND_R4)
         end do
      else if (typekind == ESMF_TYPEKIND_R8 .and. rank == 2) then
         call ESMF_FieldGet(field, farrayPtr=r8_2d, _RC)
         r8_2d = levels(1)
      else if (typekind == ESMF_TYPEKIND_R8 .and. rank == 3) then
         call ESMF_FieldGet(field, farrayPtr=r8_3d, _RC)
         _ASSERT(size(r8_3d, 3) == size(levels) .or. size(levels) == 1, mismatch)
         do k = 1, size(r8_3d, 3)
            r8_3d(:,:,lbound(r8_3d,3)+k-1) = levels(min(k, size(levels)))
         end do
      else
         _FAIL(ERR_PREFIX // 'field <' // name // '> must be R4 or R8 of rank 2 or 3')
      end if

      _RETURN(_SUCCESS)
   end subroutine fill_field

   subroutine add_field(state, decl, grid, nlev, rc)
      type(ESMF_State), intent(inout) :: state
      type(CallbackFieldDecl), intent(in) :: decl
      type(ESMF_Grid), intent(in) :: grid
      integer, intent(in) :: nlev
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_Field) :: field
      real(kind=ESMF_KIND_R4), pointer :: r4_2d(:,:), r4_3d(:,:,:)
      real(kind=ESMF_KIND_R8), pointer :: r8_2d(:,:), r8_3d(:,:,:)

      if (decl%dims == 'xyz') then
         field = ESMF_FieldCreate(grid, typekind=decl%typekind, name=decl%name, &
              ungriddedLBound=[1], ungriddedUBound=[nlev], _RC)
      else
         field = ESMF_FieldCreate(grid, typekind=decl%typekind, name=decl%name, _RC)
      end if

      ! Start from zero so an unsupplied field reads predictably.
      if (decl%typekind == ESMF_TYPEKIND_R4) then
         if (decl%dims == 'xyz') then
            call ESMF_FieldGet(field, farrayPtr=r4_3d, _RC)
            r4_3d = 0
         else
            call ESMF_FieldGet(field, farrayPtr=r4_2d, _RC)
            r4_2d = 0
         end if
      else
         if (decl%dims == 'xyz') then
            call ESMF_FieldGet(field, farrayPtr=r8_3d, _RC)
            r8_3d = 0
         else
            call ESMF_FieldGet(field, farrayPtr=r8_2d, _RC)
            r8_2d = 0
         end if
      end if

      call ESMF_StateAdd(state, [field], _RC)

      _RETURN(_SUCCESS)
   end subroutine add_field

   subroutine set_attributes_from_map(state, map, rc)
      type(ESMF_State), intent(inout) :: state
      type(ESMF_HConfig), intent(in) :: map
      integer, optional, intent(out) :: rc

      integer :: status, i
      character(len=MAX_NAME_LEN), allocatable :: names(:)
      type(ESMF_HConfig) :: node

      names = get_map_keys(map, _RC)
      do i = 1, size(names)
         node = ESMF_HConfigCreateAt(map, keyString=trim(names(i)), _RC)
         call set_attribute(state, trim(names(i)), node, _RC)
         call ESMF_HConfigDestroy(node, _RC)
      end do

      _RETURN(_SUCCESS)
   end subroutine set_attributes_from_map

   ! Integers become I4, reals R8, strings and lists of strings character.
   ! An existing attribute is removed first because ESMF refuses to change
   ! the type of an attribute in place.
   subroutine set_attribute(state, name, node, rc)
      type(ESMF_State), intent(inout) :: state
      character(*), intent(in) :: name
      type(ESMF_HConfig), intent(in) :: node
      integer, optional, intent(out) :: rc

      integer :: status, i, n
      logical :: is_scalar, is_present, all_int
      character(:), allocatable :: tag, str_value
      integer(kind=ESMF_KIND_I4) :: int_value
      real(kind=ESMF_KIND_R8) :: real_value
      logical :: logical_value
      character(len=MAX_NAME_LEN), allocatable :: strings(:)
      logical, allocatable :: logicals(:)
      integer(kind=ESMF_KIND_I4), allocatable :: ints(:)
      real(kind=ESMF_KIND_R8), allocatable :: reals(:)

      call ESMF_AttributeGet(state, name=name, isPresent=is_present, _RC)
      if (is_present) then
         call ESMF_AttributeRemove(state, name=name, _RC)
      end if

      is_scalar = ESMF_HConfigIsScalar(node, _RC)
      if (is_scalar) then
         tag = ESMF_HConfigGetTag(node, _RC)
         select case (tag)
         case (TAG_STR)
            str_value = ESMF_HConfigAsString(node, _RC)
            call ESMF_AttributeSet(state, name=name, value=str_value, _RC)
         case (TAG_INT)
            int_value = ESMF_HConfigAsI4(node, _RC)
            call ESMF_AttributeSet(state, name=name, value=int_value, _RC)
         case (TAG_FLOAT)
            real_value = ESMF_HConfigAsR8(node, _RC)
            call ESMF_AttributeSet(state, name=name, value=real_value, _RC)
         case (TAG_BOOL)
            logical_value = ESMF_HConfigAsLogical(node, _RC)
            call ESMF_AttributeSet(state, name=name, value=logical_value, _RC)
         case default
            _FAIL(ERR_PREFIX // 'attribute <' // name // '> has unsupported YAML type <' // tag // '>')
         end select
         _RETURN(_SUCCESS)
      end if

      n = ESMF_HConfigGetSize(node, _RC)
      _ASSERT(n > 0, ERR_PREFIX // 'attribute <' // name // '> must not be an empty list')
      tag = ESMF_HConfigGetTag(node, index=1, _RC)
      select case (tag)
      case (TAG_STR)
         allocate(strings(n))
         do i = 1, n
            strings(i) = ESMF_HConfigAsString(node, index=i, _RC)
         end do
         call ESMF_AttributeSet(state, name=name, valueList=strings, _RC)
      case (TAG_BOOL)
         allocate(logicals(n))
         do i = 1, n
            logicals(i) = ESMF_HConfigAsLogical(node, index=i, _RC)
         end do
         call ESMF_AttributeSet(state, name=name, valueList=logicals, _RC)
      case (TAG_INT, TAG_FLOAT)
         all_int = .true.
         do i = 1, n
            str_value = ESMF_HConfigGetTag(node, index=i, _RC)
            all_int = all_int .and. (str_value == TAG_INT)
         end do
         if (all_int) then
            allocate(ints(n))
            do i = 1, n
               ints(i) = ESMF_HConfigAsI4(node, index=i, _RC)
            end do
            call ESMF_AttributeSet(state, name=name, valueList=ints, _RC)
         else
            allocate(reals(n))
            do i = 1, n
               reals(i) = ESMF_HConfigAsR8(node, index=i, _RC)
            end do
            call ESMF_AttributeSet(state, name=name, valueList=reals, _RC)
         end if
      case default
         _FAIL(ERR_PREFIX // 'attribute <' // name // '> has unsupported YAML type <' // tag // '>')
      end select

      _RETURN(_SUCCESS)
   end subroutine set_attribute

   ! Lists are joined with commas; numbers use list-directed (g0) format.
   subroutine get_attribute_as_string(state, name, value, is_present, rc)
      type(ESMF_State), intent(inout) :: state
      character(*), intent(in) :: name
      character(:), allocatable, intent(out) :: value
      logical, intent(out) :: is_present
      integer, optional, intent(out) :: rc

      integer :: status, n, i
      type(ESMF_TypeKind_Flag) :: typekind
      character(len=ESMF_MAXSTR), allocatable :: strings(:)
      integer(kind=ESMF_KIND_I4), allocatable :: i4s(:)
      integer(kind=ESMF_KIND_I8), allocatable :: i8s(:)
      real(kind=ESMF_KIND_R4), allocatable :: r4s(:)
      real(kind=ESMF_KIND_R8), allocatable :: r8s(:)
      logical, allocatable :: logicals(:)
      character(len=64) :: buf
      character(len=64), allocatable :: items(:)

      value = ''
      call ESMF_AttributeGet(state, name=name, isPresent=is_present, _RC)
      _RETURN_UNLESS(is_present)

      call ESMF_AttributeGet(state, name=name, typekind=typekind, itemCount=n, _RC)
      allocate(items(n))
      if (typekind == ESMF_TYPEKIND_CHARACTER) then
         allocate(strings(n))
         if (n == 1) then
            call ESMF_AttributeGet(state, name=name, value=strings(1), _RC)
         else
            call ESMF_AttributeGet(state, name=name, valueList=strings, _RC)
         end if
         value = trim(strings(1))
         do i = 2, n
            value = value // ',' // trim(strings(i))
         end do
         _RETURN(_SUCCESS)
      else if (typekind == ESMF_TYPEKIND_I4) then
         allocate(i4s(n))
         if (n == 1) then
            call ESMF_AttributeGet(state, name=name, value=i4s(1), _RC)
         else
            call ESMF_AttributeGet(state, name=name, valueList=i4s, _RC)
         end if
         do i = 1, n
            write(buf, '(i0)') i4s(i)
            items(i) = buf
         end do
      else if (typekind == ESMF_TYPEKIND_I8) then
         allocate(i8s(n))
         if (n == 1) then
            call ESMF_AttributeGet(state, name=name, value=i8s(1), _RC)
         else
            call ESMF_AttributeGet(state, name=name, valueList=i8s, _RC)
         end if
         do i = 1, n
            write(buf, '(i0)') i8s(i)
            items(i) = buf
         end do
      else if (typekind == ESMF_TYPEKIND_R4) then
         allocate(r4s(n))
         if (n == 1) then
            call ESMF_AttributeGet(state, name=name, value=r4s(1), _RC)
         else
            call ESMF_AttributeGet(state, name=name, valueList=r4s, _RC)
         end if
         do i = 1, n
            write(buf, '(g0)') r4s(i)
            items(i) = adjustl(buf)
         end do
      else if (typekind == ESMF_TYPEKIND_R8) then
         allocate(r8s(n))
         if (n == 1) then
            call ESMF_AttributeGet(state, name=name, value=r8s(1), _RC)
         else
            call ESMF_AttributeGet(state, name=name, valueList=r8s, _RC)
         end if
         do i = 1, n
            write(buf, '(g0)') r8s(i)
            items(i) = adjustl(buf)
         end do
      else if (typekind == ESMF_TYPEKIND_LOGICAL) then
         allocate(logicals(n))
         if (n == 1) then
            call ESMF_AttributeGet(state, name=name, value=logicals(1), _RC)
         else
            call ESMF_AttributeGet(state, name=name, valueList=logicals, _RC)
         end if
         do i = 1, n
            items(i) = merge('true ', 'false', logicals(i))
         end do
      else
         _FAIL(ERR_PREFIX // 'attribute <' // name // '> has an unsupported type')
      end if

      value = trim(items(1))
      do i = 2, n
         value = value // ',' // trim(items(i))
      end do

      _RETURN(_SUCCESS)
   end subroutine get_attribute_as_string

   subroutine get_mock_info(state, info, rc)
      type(ESMF_State), intent(in) :: state
      type(ESMF_Info), intent(out) :: info
      integer, optional, intent(out) :: rc

      integer :: status
      logical :: is_mock

      call ESMF_InfoGetFromHost(state, info, _RC)
      is_mock = ESMF_InfoIsPresent(info, key=KEY_NUM_LEVELS, _RC)
      _ASSERT(is_mock, ERR_PREFIX // 'State was not built by ConfigurableCallbackCreate')

      _RETURN(_SUCCESS)
   end subroutine get_mock_info

   subroutine check_label(info, label, rc)
      type(ESMF_Info), intent(in) :: info
      character(*), intent(in) :: label
      integer, optional, intent(out) :: rc

      integer :: status
      logical :: is_present

      is_present = (len_trim(label) > 0 .and. scan(label, '/~') == 0)
      if (is_present) is_present = ESMF_InfoIsPresent(info, key=method_key(label, 'slot'), _RC)
      _ASSERT(is_present, ERR_PREFIX // 'no method configured with label <' // label // '>')

      _RETURN(_SUCCESS)
   end subroutine check_label

   subroutine load_method_config(info, label, config, rc)
      type(ESMF_Info), intent(in) :: info
      character(*), intent(in) :: label
      type(ESMF_HConfig), intent(out) :: config
      integer, optional, intent(out) :: rc

      integer :: status
      character(:), allocatable :: text

      call ESMF_InfoGetCharAlloc(info, key=method_key(label, 'config'), value=text, _RC)
      config = ESMF_HConfigCreate(content=text, _RC)

      _RETURN(_SUCCESS)
   end subroutine load_method_config

   subroutine get_record_names(config, kind, names, rc)
      type(ESMF_HConfig), intent(in) :: config
      character(*), intent(in) :: kind
      character(len=MAX_NAME_LEN), allocatable, intent(out) :: names(:)
      integer, optional, intent(out) :: rc

      integer :: status, i, n
      logical :: has_key
      type(ESMF_HConfig) :: record, seq

      allocate(names(0))
      has_key = ESMF_HConfigIsDefined(config, keyString='record', _RC)
      _RETURN_UNLESS(has_key)
      record = ESMF_HConfigCreateAt(config, keyString='record', _RC)
      has_key = ESMF_HConfigIsDefined(record, keyString=kind, _RC)
      if (has_key) then
         seq = ESMF_HConfigCreateAt(record, keyString=kind, _RC)
         n = ESMF_HConfigGetSize(seq, _RC)
         deallocate(names)
         allocate(names(n))
         do i = 1, n
            names(i) = ESMF_HConfigAsString(seq, index=i, _RC)
         end do
         call ESMF_HConfigDestroy(seq, _RC)
      end if
      call ESMF_HConfigDestroy(record, _RC)

      _RETURN(_SUCCESS)
   end subroutine get_record_names

   subroutine get_string_list(info, key, values, rc)
      type(ESMF_Info), intent(in) :: info
      character(*), intent(in) :: key
      character(:), allocatable, intent(out) :: values(:)
      integer, optional, intent(out) :: rc

      integer :: status, i, max_len
      logical :: is_present
      character(len=MAX_NAME_LEN), allocatable :: raw(:)

      is_present = ESMF_InfoIsPresent(info, key=key, _RC)
      if (.not. is_present) then
         allocate(character(len=1) :: values(0))
         _RETURN(_SUCCESS)
      end if

      call ESMF_InfoGetAlloc(info, key=key, values=raw, _RC)
      max_len = 1
      do i = 1, size(raw)
         max_len = max(max_len, len_trim(raw(i)))
      end do
      allocate(character(len=max_len) :: values(size(raw)))
      do i = 1, size(raw)
         values(i) = raw(i)
      end do

      _RETURN(_SUCCESS)
   end subroutine get_string_list

   subroutine append_string(info, key, value, rc)
      type(ESMF_Info), intent(inout) :: info
      character(*), intent(in) :: key
      character(*), intent(in) :: value
      integer, optional, intent(out) :: rc

      integer :: status
      logical :: is_present
      character(len=MAX_NAME_LEN), allocatable :: raw(:)
      character(len=MAX_NAME_LEN) :: item

      _ASSERT(len(value) <= MAX_NAME_LEN, ERR_PREFIX // 'recorded value too long for key <' // key // '>')
      is_present = ESMF_InfoIsPresent(info, key=key, _RC)
      if (is_present) then
         call ESMF_InfoGetAlloc(info, key=key, values=raw, _RC)
      else
         allocate(raw(0))
      end if
      item = value
      raw = [raw, item]
      call ESMF_InfoSet(info, key=key, values=raw, _RC)

      _RETURN(_SUCCESS)
   end subroutine append_string

   function method_key(label, suffix) result(key)
      character(:), allocatable :: key
      character(*), intent(in) :: label
      character(*), intent(in) :: suffix
      key = MOCK_NS // '/' // label // '/' // suffix
   end function method_key

end module mapl_ConfigurableCallback_mod
