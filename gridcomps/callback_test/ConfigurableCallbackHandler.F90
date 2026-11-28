#include "MAPL.h"

!------------------------------------------------------------------------------
! ConfigurableCallbackHandler: a YAML-driven mock callback Handler. Given
! a registered CallbackInterface (looked up by name, already loaded into
! CallbackInterfaceRegistry) and a small HConfig section of its own,
! build_configurable_handler builds a real ESMF_State shaped like what a
! Handler would hand an Invoker: one member per declared argument (so it
! conforms by construction), Info values under `state_attributes` and
! `member_attributes`, and (if `record` is configured) placeholder
! `mapl_mock_<method>_<argument>` export fields in a separate mock/export
! State. No Invoker is involved and no value is actually recorded here;
! this only proves a YAML-described callback State can be built and
! checked with check_callback_state_conformance.
!
! `non_negative` is reported back as StateItemDescriptors rather than
! written into cb_state: NonNegativeCharacteristic writes no Info key, so
! it cannot be observed on a realized State the way the other sections
! can.
!
! `geometry` is required: a real component's ADVERTISE/GEOM/REALIZE
! phases supply a default geometry an Invoker can fall back on, but
! this is a plain Fortran module, not an ESMF_GridComp - no component
! lifecycle calls it, so there is no such default here. Tests call
! build_configurable_handler directly, the same way
! Test_CallbackStateConformance.pf builds its States by hand.
!------------------------------------------------------------------------------
module mapl_ConfigurableCallbackHandler_mod
   use mapl_CallbackInterface_mod, only: CallbackInterface
   use mapl_CallbackInterfaceRegistry_mod, only: get_callback_interface_by_name
   use mapl_CallbackArgumentSpec_mod, only: CallbackArgumentSpec
   use mapl_CallbackArgumentSpecMap_mod, only: CallbackArgumentSpecMap, CallbackArgumentSpecMapIterator, &
        operator(/=)
   use mapl_CallbackMethodSpec_mod, only: CallbackMethodSpec
   use mapl_CallbackMethodSpecMap_mod, only: CallbackMethodSpecMap, CallbackMethodSpecMapIterator, &
        operator(/=)
   use mapl_AccessSpec_mod, only: AccessSpec, MAPL_ACCESS_OUT, MAPL_ACCESS_INOUT, MAPL_ACCESS_IN, &
        operator(==)
   use mapl_CallbackAttributeSpec_mod, only: CallbackAttributeSpec, &
        CALLBACK_SCOPE_STATE, CALLBACK_SCOPE_MEMBER, operator(==)
   use mapl_CallbackValueType_mod, only: CallbackValueType, operator(==)
   use mapl_CallbackValueType_mod, only: CALLBACK_VALUE_INTEGER, CALLBACK_VALUE_REAL
   use mapl_CallbackValueType_mod, only: CALLBACK_VALUE_LOGICAL, CALLBACK_VALUE_STRING
   use mapl_StateItemFlag_mod, only: MAPL_StateItem_Flag, MAPL_STATEITEM_FIELD, operator(==)
   use mapl_InfoUtilities_mod, only: MAPL_InfoSetShared
   use mapl_geom_api, only: mapl_get_geom_manager, mapl_GeomManager, mapl_MaplGeom, &
        mapl_GridGetCoordinates
   use mapl_field_api, only: MAPL_FieldCreate
   use mapl_vertical_grid_api, only: mapl_VerticalGrid, mapl_get_vertical_grid_manager, &
        mapl_VerticalGridManager
   use mapl_VerticalStaggerLoc_mod, only: VERTICAL_STAGGER_CENTER
   use mapl_VariableSpec_mod, only: VariableSpec, make_VariableSpec
   use mapl_StateItemDescriptor_mod, only: StateItemDescriptor
   use mapl_StateArithmeticParser_mod, only: StateEval
   use mapl_MathConstants_mod, only: MAPL_RADIANS_TO_DEGREES
   use esmf
   use mapl_ErrorHandling_mod
   implicit none(type, external)
   private

   public :: build_configurable_handler

   character(len=20), parameter :: TOP_LEVEL_KEYS(7) = [character(len=20) :: &
        'interface', 'geometry', 'supply', 'non_negative', 'record', &
        'state_attributes', 'member_attributes']

contains

   ! Builds cb_state (the callback State proper) and mock_state (this
   ! Handler's own export State, holding only the record placeholders)
   ! from hconfig, whose top-level keys are all optional except
   ! `interface` and `geometry`:
   !   interface: <registered interface name>
   !   geometry: {esmf_geom: <a geometry-manager HConfig>, vertical_grid: <optional, for xyz args>}
   !   supply: {argument: {constant: V} | {profile: [v1, ..., vK]}}
   !   non_negative: {argument: {fill: column}}
   !   state_attributes: {attribute: value}
   !   member_attributes: {argument: {attribute: value}}
   !   record: {method: [argument, ...]}
   ! interface_name is returned because CallbackInterface does not carry
   ! its own name (see CallbackStateConformance, which needs it too).
   ! non_negative_names/non_negative_descriptors report one
   ! StateItemDescriptor per `non_negative` entry (empty when the section
   ! is absent); cb_state carries no trace of them.
   subroutine build_configurable_handler(hconfig, handler_name, interface_name, &
        cb_state, mock_state, non_negative_names, non_negative_descriptors, rc)
      type(ESMF_HConfig), intent(in) :: hconfig
      character(*), intent(in) :: handler_name
      character(:), allocatable, intent(out) :: interface_name
      type(ESMF_State), intent(out) :: cb_state
      type(ESMF_State), intent(out) :: mock_state
      character(ESMF_MAXSTR), allocatable, intent(out) :: non_negative_names(:)
      type(StateItemDescriptor), allocatable, intent(out) :: non_negative_descriptors(:)
      integer, optional, intent(out) :: rc

      type(CallbackInterface) :: iface
      type(ESMF_HConfig) :: geometry_cfg, non_negative_cfg
      type(ESMF_Geom) :: geom
      class(mapl_VerticalGrid), pointer :: vgrid
      character(:), allocatable :: msg
      logical :: is_defined, has_vgrid
      integer :: status

      call check_top_level_keys(hconfig, handler_name, _RC)

      is_defined = ESMF_HConfigIsDefined(hconfig, keyString='interface', _RC)
      msg = 'ConfigurableCallbackHandler ' // handler_name // ': missing required key interface'
      _ASSERT(is_defined, msg)
      interface_name = ESMF_HConfigAsString(hconfig, keyString='interface', _RC)
      iface = get_callback_interface_by_name(interface_name, _RC)

      cb_state = ESMF_StateCreate(name='cb', _RC)
      mock_state = ESMF_StateCreate(name=trim(handler_name) // '_mock_export', _RC)

      is_defined = ESMF_HConfigIsDefined(hconfig, keyString='geometry', _RC)
      msg = 'ConfigurableCallbackHandler ' // handler_name // ': missing required key geometry'
      _ASSERT(is_defined, msg)
      geometry_cfg = ESMF_HConfigCreateAt(hconfig, keyString='geometry', _RC)
      call build_geometry(geometry_cfg, handler_name, geom, vgrid, has_vgrid, _RC)
      call ESMF_HConfigDestroy(geometry_cfg, _RC)

      call build_members(iface, handler_name, geom, vgrid, has_vgrid, cb_state, _RC)
      call build_supply(hconfig, iface, handler_name, geom, vgrid, has_vgrid, cb_state, _RC)
      call build_state_attributes(hconfig, iface, interface_name, handler_name, cb_state, _RC)
      call build_member_attributes(hconfig, iface, interface_name, handler_name, cb_state, _RC)
      call build_record_fields(hconfig, iface, handler_name, geom, vgrid, has_vgrid, mock_state, _RC)

      allocate(non_negative_names(0))
      allocate(non_negative_descriptors(0))
      is_defined = ESMF_HConfigIsDefined(hconfig, keyString='non_negative', _RC)
      if (is_defined) then
         non_negative_cfg = ESMF_HConfigCreateAt(hconfig, keyString='non_negative', _RC)
         call build_non_negative_descriptors(non_negative_cfg, iface, handler_name, &
              non_negative_names, non_negative_descriptors, _RC)
         call ESMF_HConfigDestroy(non_negative_cfg, _RC)
      end if

      _RETURN(_SUCCESS)
   end subroutine build_configurable_handler

   ! Rejects a key at hconfig's top level that is not one of the
   ! documented ones (a typo such as `supplly:` would otherwise build
   ! silently and be missed).
   subroutine check_top_level_keys(hconfig, handler_name, rc)
      type(ESMF_HConfig), intent(in) :: hconfig
      character(*), intent(in) :: handler_name
      integer, optional, intent(out) :: rc

      type(ESMF_HConfigIter) :: iter, b, e
      character(:), allocatable :: key, msg
      integer :: status

      b = ESMF_HConfigIterBegin(hconfig, _RC)
      e = ESMF_HConfigIterEnd(hconfig, _RC)
      iter = b
      do while (ESMF_HConfigIterLoop(iter, b, e, rc=status))
         _VERIFY(status)
         key = ESMF_HConfigAsStringMapKey(iter, _RC)
         msg = 'ConfigurableCallbackHandler ' // handler_name // ': unknown top-level key ' // &
              key // '; legal keys: ' // join_csv(TOP_LEVEL_KEYS)
         _ASSERT(any(TOP_LEVEL_KEYS == key), msg)
      end do

      _RETURN(_SUCCESS)
   end subroutine check_top_level_keys

   ! Comma-and-space-joined text of keys, for an error message.
   function join_csv(keys) result(csv)
      character(*), intent(in) :: keys(:)
      character(:), allocatable :: csv

      integer :: i

      csv = trim(keys(1))
      do i = 2, size(keys)
         csv = csv // ', ' // trim(keys(i))
      end do
   end function join_csv

   !--------------------------------------------------------------------
   ! geometry
   !--------------------------------------------------------------------

   ! esmf_geom and vertical_grid are the same two sub-keys the generic
   ! layer's own `geometry:` section uses (ComponentSpecParser's
   ! parse_geometry_spec), so a Handler's config needs no bespoke
   ! geometry shape. vertical_grid is optional and only needed when the
   ! interface has an xyz argument.
   subroutine build_geometry(geometry_cfg, handler_name, geom, vgrid, has_vgrid, rc)
      type(ESMF_HConfig), intent(inout) :: geometry_cfg
      character(*), intent(in) :: handler_name
      type(ESMF_Geom), intent(out) :: geom
      class(mapl_VerticalGrid), pointer, intent(out) :: vgrid
      logical, intent(out) :: has_vgrid
      integer, optional, intent(out) :: rc

      type(mapl_GeomManager), pointer :: geom_mgr
      type(mapl_MaplGeom), pointer :: mapl_geom
      type(mapl_VerticalGridManager), pointer :: vgrid_mgr
      type(ESMF_HConfig) :: esmf_geom_cfg, vertical_grid_cfg
      logical :: has_esmf_geom
      character(:), allocatable :: msg
      integer :: status

      has_esmf_geom = ESMF_HConfigIsDefined(geometry_cfg, keyString='esmf_geom', _RC)
      msg = 'ConfigurableCallbackHandler ' // handler_name // ': geometry needs esmf_geom'
      _ASSERT(has_esmf_geom, msg)
      esmf_geom_cfg = ESMF_HConfigCreateAt(geometry_cfg, keyString='esmf_geom', _RC)
      geom_mgr => mapl_get_geom_manager()
      mapl_geom => geom_mgr%get_mapl_geom(esmf_geom_cfg, _RC)
      geom = mapl_geom%get_geom()
      call ESMF_HConfigDestroy(esmf_geom_cfg, _RC)

      has_vgrid = ESMF_HConfigIsDefined(geometry_cfg, keyString='vertical_grid', _RC)
      vgrid => null()
      if (has_vgrid) then
         vertical_grid_cfg = ESMF_HConfigCreateAt(geometry_cfg, keyString='vertical_grid', _RC)
         vgrid_mgr => mapl_get_vertical_grid_manager(_RC)
         vgrid => vgrid_mgr%create_grid(vertical_grid_cfg, _RC)
         call ESMF_HConfigDestroy(vertical_grid_cfg, _RC)
      end if

      _RETURN(_SUCCESS)
   end subroutine build_geometry

   !--------------------------------------------------------------------
   ! members: one real field per declared argument, built whether or not
   ! it is configured under `supply` - an Invoker's `put` side would set
   ! an unconfigured one later, out of scope here.
   !--------------------------------------------------------------------

   subroutine build_members(iface, handler_name, geom, vgrid, has_vgrid, cb_state, rc)
      type(CallbackInterface), intent(in) :: iface
      character(*), intent(in) :: handler_name
      type(ESMF_Geom), intent(in) :: geom
      class(mapl_VerticalGrid), pointer, intent(in) :: vgrid
      logical, intent(in) :: has_vgrid
      type(ESMF_State), intent(inout) :: cb_state
      integer, optional, intent(out) :: rc

      type(CallbackArgumentSpecMap), target :: arguments
      type(CallbackArgumentSpecMapIterator) :: iter
      character(:), allocatable :: arg_name
      type(CallbackArgumentSpec) :: arg_spec
      type(ESMF_Field) :: field
      integer :: status

      arguments = iface%get_arguments()
      iter = arguments%ftn_begin()
      do while (iter /= arguments%ftn_end())
         call iter%next()
         if (allocated(arg_name)) deallocate(arg_name)
         allocate(arg_name, source=iter%first())
         arg_spec = iter%second()
         field = build_field_for_argument(arg_spec, handler_name, geom, vgrid, has_vgrid, &
              arg_name, _RC)
         call ESMF_StateAdd(cb_state, [field], _RC)
      end do

      _RETURN(_SUCCESS)
   end subroutine build_members

   ! A field on geom, 2-D for dims 'xy' or 3-D for 'xyz' (vgrid required),
   ! named field_name (which may differ from the argument's own name, for
   ! the record placeholders below). Unfilled: ESMF/MAPL's own default
   ! fill applies.
   function build_field_for_argument(arg_spec, handler_name, geom, vgrid, has_vgrid, &
        field_name, rc) result(field)
      type(CallbackArgumentSpec), intent(in) :: arg_spec
      character(*), intent(in) :: handler_name
      type(ESMF_Geom), intent(in) :: geom
      class(mapl_VerticalGrid), pointer, intent(in) :: vgrid
      logical, intent(in) :: has_vgrid
      character(*), intent(in) :: field_name
      integer, optional, intent(out) :: rc
      type(ESMF_Field) :: field

      type(MAPL_StateItem_Flag) :: expected_kind
      character(:), allocatable :: name, dims, msg
      logical :: is_xyz
      integer :: status

      name = arg_spec%get_name()
      expected_kind = arg_spec%get_expected_kind()
      msg = 'ConfigurableCallbackHandler ' // handler_name // ': argument ' // name // &
           ': only field-kind arguments are supported'
      _ASSERT(expected_kind == MAPL_STATEITEM_FIELD, msg)

      dims = arg_spec%get_dims()
      msg = 'ConfigurableCallbackHandler ' // handler_name // ': argument ' // name // &
           ': needs a declared dims (xy or xyz)'
      _ASSERT(len(dims) > 0, msg)
      is_xyz = dims == 'xyz'

      if (is_xyz) then
         msg = 'ConfigurableCallbackHandler ' // handler_name // ': argument ' // name // &
              ': dims xyz needs geometry.vertical_grid'
         _ASSERT(has_vgrid, msg)
         field = MAPL_FieldCreate(geom, ESMF_TYPEKIND_R8, name=field_name, vgrid=vgrid, &
              vert_staggerloc=VERTICAL_STAGGER_CENTER, _RC)
      else
         field = MAPL_FieldCreate(geom, ESMF_TYPEKIND_R8, name=field_name, _RC)
      end if

      _RETURN(_SUCCESS)
   end function build_field_for_argument

   !--------------------------------------------------------------------
   ! supply
   !--------------------------------------------------------------------

   ! Whether argument_name has access OUT or INOUT in at least one of
   ! iface's methods - the spec's condition for being legally
   ! configurable under `supply`.
   logical function argument_is_suppliable(iface, argument_name) result(suppliable)
      type(CallbackInterface), intent(in) :: iface
      character(*), intent(in) :: argument_name

      type(CallbackMethodSpecMap), target :: methods
      type(CallbackMethodSpecMapIterator) :: iter
      character(:), allocatable :: method_name
      type(CallbackMethodSpec) :: method_spec
      type(AccessSpec) :: access

      suppliable = .false.
      methods = iface%get_methods()
      iter = methods%ftn_begin()
      do while (iter /= methods%ftn_end())
         call iter%next()
         if (allocated(method_name)) deallocate(method_name)
         allocate(method_name, source=iter%first())
         method_spec = iter%second()
         if (method_spec%is_argument_access(argument_name)) then
            access = method_spec%get_argument_access(argument_name)
            if (access == MAPL_ACCESS_OUT .or. access == MAPL_ACCESS_INOUT) suppliable = .true.
         end if
      end do
   end function argument_is_suppliable

   subroutine build_supply(hconfig, iface, handler_name, geom, vgrid, has_vgrid, cb_state, rc)
      type(ESMF_HConfig), intent(in) :: hconfig
      type(CallbackInterface), intent(in) :: iface
      character(*), intent(in) :: handler_name
      type(ESMF_Geom), intent(in) :: geom
      class(mapl_VerticalGrid), pointer, intent(in) :: vgrid
      logical, intent(in) :: has_vgrid
      type(ESMF_State), intent(inout) :: cb_state
      integer, optional, intent(out) :: rc

      type(ESMF_HConfig) :: supply_cfg, one_cfg
      type(ESMF_HConfigIter) :: iter, b, e
      type(CallbackArgumentSpec) :: arg_spec
      type(ESMF_Field) :: field
      character(:), allocatable :: arg_name, msg
      logical :: has_supply
      integer :: status

      has_supply = ESMF_HConfigIsDefined(hconfig, keyString='supply', _RC)
      _RETURN_UNLESS(has_supply)

      supply_cfg = ESMF_HConfigCreateAt(hconfig, keyString='supply', _RC)
      b = ESMF_HConfigIterBegin(supply_cfg, _RC)
      e = ESMF_HConfigIterEnd(supply_cfg, _RC)
      iter = b
      do while (ESMF_HConfigIterLoop(iter, b, e, rc=status))
         _VERIFY(status)
         arg_name = ESMF_HConfigAsStringMapKey(iter, _RC)
         msg = 'ConfigurableCallbackHandler ' // handler_name // &
              ': supply names unknown argument ' // arg_name
         _ASSERT(iface%is_argument(arg_name), msg)
         msg = 'ConfigurableCallbackHandler ' // handler_name // ': supply argument ' // arg_name // &
              ' is not out or inout in any method'
         _ASSERT(argument_is_suppliable(iface, arg_name), msg)
         arg_spec = iface%get_argument(arg_name, _RC)
         one_cfg = ESMF_HConfigCreateAtMapVal(iter, _RC)
         call ESMF_StateGet(cb_state, itemName=arg_name, field=field, _RC)
         call fill_supplied_field(field, arg_spec, handler_name, geom, vgrid, has_vgrid, one_cfg, _RC)
         call ESMF_HConfigDestroy(one_cfg, _RC)
      end do
      call ESMF_HConfigDestroy(supply_cfg, _RC)

      _RETURN(_SUCCESS)
   end subroutine build_supply

   ! Fills field (already a member of cb_state) per its supply entry's
   ! single key: constant (one R8 value everywhere), profile (xyz only:
   ! one value per level, broadcast over every column), or function (an
   ! arithmetic-parser expression of lon, lat, and - xyz only - level,
   ! evaluated at every cell/column).
   subroutine fill_supplied_field(field, arg_spec, handler_name, geom, vgrid, has_vgrid, &
        supply_entry_cfg, rc)
      type(ESMF_Field), intent(inout) :: field
      type(CallbackArgumentSpec), intent(in) :: arg_spec
      character(*), intent(in) :: handler_name
      type(ESMF_Geom), intent(in) :: geom
      class(mapl_VerticalGrid), pointer, intent(in) :: vgrid
      logical, intent(in) :: has_vgrid
      type(ESMF_HConfig), intent(in) :: supply_entry_cfg
      integer, optional, intent(out) :: rc

      character(:), allocatable :: name, dims, msg, function_expr
      logical :: is_xyz, has_constant, has_profile, has_function
      real(kind=ESMF_KIND_R8) :: const_value
      real(kind=ESMF_KIND_R8), allocatable :: profile_values(:)
      real(kind=ESMF_KIND_R8), pointer :: ptr2d(:,:), ptr3d(:,:,:)
      integer :: entry_size, status

      name = arg_spec%get_name()
      dims = arg_spec%get_dims()
      is_xyz = dims == 'xyz'

      entry_size = ESMF_HConfigGetSize(supply_entry_cfg, _RC)
      msg = 'ConfigurableCallbackHandler ' // handler_name // ': argument ' // name // &
           ': supply must name exactly one of constant, profile, function'
      _ASSERT(entry_size == 1, msg)

      has_constant = ESMF_HConfigIsDefined(supply_entry_cfg, keyString='constant', _RC)
      has_profile = ESMF_HConfigIsDefined(supply_entry_cfg, keyString='profile', _RC)
      has_function = ESMF_HConfigIsDefined(supply_entry_cfg, keyString='function', _RC)

      if (has_constant) then
         const_value = ESMF_HConfigAsR8(supply_entry_cfg, keyString='constant', _RC)
         if (is_xyz) then
            call ESMF_FieldGet(field, farrayPtr=ptr3d, _RC)
            ptr3d = const_value
         else
            call ESMF_FieldGet(field, farrayPtr=ptr2d, _RC)
            ptr2d = const_value
         end if
      else if (has_profile) then
         msg = 'ConfigurableCallbackHandler ' // handler_name // ': argument ' // name // &
              ': supply profile needs dims xyz'
         _ASSERT(is_xyz, msg)
         profile_values = ESMF_HConfigAsR8Seq(supply_entry_cfg, keyString='profile', _RC)
         msg = 'ConfigurableCallbackHandler ' // handler_name // ': argument ' // name // &
              ': supply profile length does not match the vertical grid'
         _ASSERT(has_vgrid, msg)
         _ASSERT(size(profile_values) == vgrid%get_num_layers(), msg)
         call ESMF_FieldGet(field, farrayPtr=ptr3d, _RC)
         call broadcast_profile(ptr3d, profile_values)
      else if (has_function) then
         function_expr = ESMF_HConfigAsString(supply_entry_cfg, keyString='function', _RC)
         call evaluate_function_field(field, handler_name, name, geom, vgrid, is_xyz, function_expr, _RC)
      else
         msg = 'ConfigurableCallbackHandler ' // handler_name // ': argument ' // name // &
              ': supply needs one of constant, profile, function'
         _FAIL(msg)
      end if

      _RETURN(_SUCCESS)
   end subroutine fill_supplied_field

   subroutine broadcast_profile(ptr3d, profile_values)
      real(kind=ESMF_KIND_R8), intent(inout) :: ptr3d(:,:,:)
      real(kind=ESMF_KIND_R8), intent(in) :: profile_values(:)

      integer :: ii, jj

      do concurrent (ii = 1:size(ptr3d, 1), jj = 1:size(ptr3d, 2))
         ptr3d(ii, jj, :) = profile_values
      end do
   end subroutine broadcast_profile

   ! Evaluates function_expr with the state arithmetic parser (StateEval)
   ! and writes the result into field. lon and lat are always available
   ! (degrees, one real value per column, built once as 2-D fields on
   ! geom); level (1-based level index, xyz only) is built as a full 3-D
   ! field up front via broadcast_profile. A 2-D field sharing the
   ! horizontal dims of an xyz target is promoted by StateEval to every
   ! level automatically, which is why lon/lat need not be 3-D, but that
   ! promotion does not apply to a field that varies only in the
   ! vertical, which is why level is built as a full 3-D field instead.
   ! Any other name in function_expr is not in this scratch state, so
   ! StateEval's own CheckSyntax rejects it ('Invalid element: ...'); a
   ! syntactically malformed function_expr is rejected the same way
   ! ('Missing ) in ...', 'Missing operand in ...', etc.). Those parser
   ! diagnostics are left on the exception stack as raised; the _ASSERT
   ! below additionally reports the failure under this file's own
   ! handler/argument-naming convention.
   subroutine evaluate_function_field(field, handler_name, name, geom, vgrid, is_xyz, &
        function_expr, rc)
      type(ESMF_Field), intent(inout) :: field
      character(*), intent(in) :: handler_name
      character(*), intent(in) :: name
      type(ESMF_Geom), intent(in) :: geom
      class(mapl_VerticalGrid), pointer, intent(in) :: vgrid
      logical, intent(in) :: is_xyz
      character(*), intent(in) :: function_expr
      integer, optional, intent(out) :: rc

      type(ESMF_Grid) :: grid
      type(ESMF_State) :: scratch_state
      type(ESMF_Field) :: lon_field, lat_field, level_field
      real(kind=ESMF_KIND_R8), pointer :: longitudes(:,:), latitudes(:,:)
      real(kind=ESMF_KIND_R8), pointer :: lon_ptr(:,:), lat_ptr(:,:), level_ptr(:,:,:)
      real(kind=ESMF_KIND_R8), allocatable :: level_values(:)
      character(:), allocatable :: msg
      integer :: k, status, eval_status

      call ESMF_GeomGet(geom, grid=grid, _RC)
      call mapl_GridGetCoordinates(grid, longitudes, latitudes, _RC)

      lon_field = MAPL_FieldCreate(geom, ESMF_TYPEKIND_R8, name='lon', _RC)
      lat_field = MAPL_FieldCreate(geom, ESMF_TYPEKIND_R8, name='lat', _RC)
      call ESMF_FieldGet(lon_field, farrayPtr=lon_ptr, _RC)
      call ESMF_FieldGet(lat_field, farrayPtr=lat_ptr, _RC)
      lon_ptr = longitudes * MAPL_RADIANS_TO_DEGREES
      lat_ptr = latitudes * MAPL_RADIANS_TO_DEGREES

      scratch_state = ESMF_StateCreate(name='configurable_callback_handler_function', _RC)
      call ESMF_StateAdd(scratch_state, [lon_field], _RC)
      call ESMF_StateAdd(scratch_state, [lat_field], _RC)

      if (is_xyz) then
         level_field = MAPL_FieldCreate(geom, ESMF_TYPEKIND_R8, name='level', vgrid=vgrid, &
              vert_staggerloc=VERTICAL_STAGGER_CENTER, _RC)
         call ESMF_FieldGet(level_field, farrayPtr=level_ptr, _RC)
         level_values = [(real(k, kind=ESMF_KIND_R8), k = 1, vgrid%get_num_layers())]
         call broadcast_profile(level_ptr, level_values)
         call ESMF_StateAdd(scratch_state, [level_field], _RC)
      end if

      call StateEval(scratch_state, function_expr, field, rc=eval_status)

      call ESMF_FieldDestroy(lon_field, noGarbage=.true., _RC)
      call ESMF_FieldDestroy(lat_field, noGarbage=.true., _RC)
      if (is_xyz) then
         call ESMF_FieldDestroy(level_field, noGarbage=.true., _RC)
      end if
      call ESMF_StateDestroy(scratch_state, _RC)

      msg = 'ConfigurableCallbackHandler ' // handler_name // ': argument ' // name // &
           ': supply function "' // function_expr // '" failed to evaluate'
      _ASSERT(eval_status == 0, msg)

      _RETURN(_SUCCESS)
   end subroutine evaluate_function_field

   !--------------------------------------------------------------------
   ! state_attributes / member_attributes
   !--------------------------------------------------------------------

   subroutine build_state_attributes(hconfig, iface, interface_name, handler_name, cb_state, rc)
      type(ESMF_HConfig), intent(in) :: hconfig
      type(CallbackInterface), intent(in) :: iface
      character(*), intent(in) :: interface_name
      character(*), intent(in) :: handler_name
      type(ESMF_State), intent(inout) :: cb_state
      integer, optional, intent(out) :: rc

      type(ESMF_HConfig) :: attrs_cfg
      type(ESMF_HConfigIter) :: iter, b, e
      type(CallbackAttributeSpec) :: attr_spec
      character(:), allocatable :: attr_name, key, msg
      logical :: has_section
      integer :: status

      has_section = ESMF_HConfigIsDefined(hconfig, keyString='state_attributes', _RC)
      _RETURN_UNLESS(has_section)

      attrs_cfg = ESMF_HConfigCreateAt(hconfig, keyString='state_attributes', _RC)
      b = ESMF_HConfigIterBegin(attrs_cfg, _RC)
      e = ESMF_HConfigIterEnd(attrs_cfg, _RC)
      iter = b
      do while (ESMF_HConfigIterLoop(iter, b, e, rc=status))
         _VERIFY(status)
         attr_name = ESMF_HConfigAsStringMapKey(iter, _RC)
         msg = 'ConfigurableCallbackHandler ' // handler_name // &
              ': state_attributes names unknown attribute ' // attr_name
         _ASSERT(iface%is_attribute(attr_name), msg)
         attr_spec = iface%get_attribute(attr_name, _RC)
         msg = 'ConfigurableCallbackHandler ' // handler_name // ': state_attributes names ' // &
              attr_name // ', which is a member-scoped attribute; put it under member_attributes'
         _ASSERT(attr_spec%get_scope() == CALLBACK_SCOPE_STATE, msg)
         key = interface_name // '/' // attr_name
         call set_info_value(cb_state, '', key, attr_spec, attrs_cfg, attr_name, _RC)
      end do
      call ESMF_HConfigDestroy(attrs_cfg, _RC)

      _RETURN(_SUCCESS)
   end subroutine build_state_attributes

   subroutine build_member_attributes(hconfig, iface, interface_name, handler_name, cb_state, rc)
      type(ESMF_HConfig), intent(in) :: hconfig
      type(CallbackInterface), intent(in) :: iface
      character(*), intent(in) :: interface_name
      character(*), intent(in) :: handler_name
      type(ESMF_State), intent(inout) :: cb_state
      integer, optional, intent(out) :: rc

      type(ESMF_HConfig) :: members_cfg, member_cfg
      type(ESMF_HConfigIter) :: iter, b, e, miter, mb, me
      type(CallbackAttributeSpec) :: attr_spec
      character(:), allocatable :: member_name, attr_name, key, msg
      logical :: has_section
      integer :: status

      has_section = ESMF_HConfigIsDefined(hconfig, keyString='member_attributes', _RC)
      _RETURN_UNLESS(has_section)

      members_cfg = ESMF_HConfigCreateAt(hconfig, keyString='member_attributes', _RC)
      b = ESMF_HConfigIterBegin(members_cfg, _RC)
      e = ESMF_HConfigIterEnd(members_cfg, _RC)
      iter = b
      do while (ESMF_HConfigIterLoop(iter, b, e, rc=status))
         _VERIFY(status)
         member_name = ESMF_HConfigAsStringMapKey(iter, _RC)
         msg = 'ConfigurableCallbackHandler ' // handler_name // &
              ': member_attributes names unknown argument ' // member_name
         _ASSERT(iface%is_argument(member_name), msg)
         member_cfg = ESMF_HConfigCreateAtMapVal(iter, _RC)

         mb = ESMF_HConfigIterBegin(member_cfg, _RC)
         me = ESMF_HConfigIterEnd(member_cfg, _RC)
         miter = mb
         do while (ESMF_HConfigIterLoop(miter, mb, me, rc=status))
            _VERIFY(status)
            attr_name = ESMF_HConfigAsStringMapKey(miter, _RC)
            msg = 'ConfigurableCallbackHandler ' // handler_name // &
                 ': member_attributes names unknown attribute ' // attr_name
            _ASSERT(iface%is_attribute(attr_name), msg)
            attr_spec = iface%get_attribute(attr_name, _RC)
            msg = 'ConfigurableCallbackHandler ' // handler_name // ': member_attributes names ' // &
                 attr_name // ', which is a state-scoped attribute; put it under state_attributes'
            _ASSERT(attr_spec%get_scope() == CALLBACK_SCOPE_MEMBER, msg)
            msg = 'ConfigurableCallbackHandler ' // handler_name // ': member_attributes puts ' // &
                 attr_name // ' on ' // member_name // ', but its declared host argument is ' // &
                 attr_spec%get_host_argument()
            _ASSERT(member_name == attr_spec%get_host_argument(), msg)
            key = interface_name // '/' // attr_name
            call set_info_value(cb_state, member_name, key, attr_spec, member_cfg, attr_name, _RC)
         end do

         call ESMF_HConfigDestroy(member_cfg, _RC)
      end do
      call ESMF_HConfigDestroy(members_cfg, _RC)

      _RETURN(_SUCCESS)
   end subroutine build_member_attributes

   ! Writes one attribute's value (read from value_cfg at value_key) to
   ! cb_state's own Info (host == '') or a member's Info (host ==
   ! member name), using the type set%is_list() declares. A logical
   ! list has no MAPL_InfoSetShared list form and is rejected.
   subroutine set_info_value(cb_state, host, key, attr_spec, value_cfg, value_key, rc)
      type(ESMF_State), intent(inout) :: cb_state
      character(*), intent(in) :: host
      character(*), intent(in) :: key
      type(CallbackAttributeSpec), intent(in) :: attr_spec
      type(ESMF_HConfig), intent(in) :: value_cfg
      character(*), intent(in) :: value_key
      integer, optional, intent(out) :: rc

      type(CallbackValueType) :: value_type
      logical :: has_host
      integer(kind=ESMF_KIND_I4) :: ival
      integer(kind=ESMF_KIND_I4), allocatable :: ivals(:)
      real(kind=ESMF_KIND_R8) :: rval
      real(kind=ESMF_KIND_R8), allocatable :: rvals(:)
      logical :: lval
      character(:), allocatable :: sval, msg
      character(ESMF_MAXSTR), allocatable :: svals(:)
      integer :: status

      value_type = attr_spec%get_value_type()
      has_host = len(host) > 0

      if (attr_spec%is_list()) then
         if (value_type == CALLBACK_VALUE_INTEGER) then
            ivals = ESMF_HConfigAsI4Seq(value_cfg, keyString=value_key, _RC)
            if (has_host) then
               call MAPL_InfoSetShared(cb_state, host, key, ivals, _RC)
            else
               call MAPL_InfoSetShared(cb_state, key, ivals, _RC)
            end if
         else if (value_type == CALLBACK_VALUE_REAL) then
            rvals = ESMF_HConfigAsR8Seq(value_cfg, keyString=value_key, _RC)
            if (has_host) then
               call MAPL_InfoSetShared(cb_state, host, key, rvals, _RC)
            else
               call MAPL_InfoSetShared(cb_state, key, rvals, _RC)
            end if
         else if (value_type == CALLBACK_VALUE_STRING) then
            svals = ESMF_HConfigAsStringSeq(value_cfg, ESMF_MAXSTR, keyString=value_key, _RC)
            if (has_host) then
               call MAPL_InfoSetShared(cb_state, host, key, svals, _RC)
            else
               call MAPL_InfoSetShared(cb_state, key, svals, _RC)
            end if
         else
            msg = 'ConfigurableCallbackHandler: ' // key // &
                 ': a logical list attribute cannot be written to Info'
            _FAIL(msg)
         end if
      else
         if (value_type == CALLBACK_VALUE_INTEGER) then
            ival = ESMF_HConfigAsI4(value_cfg, keyString=value_key, _RC)
            if (has_host) then
               call MAPL_InfoSetShared(cb_state, host, key, ival, _RC)
            else
               call MAPL_InfoSetShared(cb_state, key, ival, _RC)
            end if
         else if (value_type == CALLBACK_VALUE_REAL) then
            rval = ESMF_HConfigAsR8(value_cfg, keyString=value_key, _RC)
            if (has_host) then
               call MAPL_InfoSetShared(cb_state, host, key, rval, _RC)
            else
               call MAPL_InfoSetShared(cb_state, key, rval, _RC)
            end if
         else if (value_type == CALLBACK_VALUE_LOGICAL) then
            lval = ESMF_HConfigAsLogical(value_cfg, keyString=value_key, _RC)
            if (has_host) then
               call MAPL_InfoSetShared(cb_state, host, key, lval, _RC)
            else
               call MAPL_InfoSetShared(cb_state, key, lval, _RC)
            end if
         else
            sval = ESMF_HConfigAsString(value_cfg, keyString=value_key, _RC)
            if (has_host) then
               call MAPL_InfoSetShared(cb_state, host, key, sval, _RC)
            else
               call MAPL_InfoSetShared(cb_state, key, sval, _RC)
            end if
         end if
      end if

      _RETURN(_SUCCESS)
   end subroutine set_info_value

   !--------------------------------------------------------------------
   ! record: placeholder export fields only, in a separate mock_state.
   ! Recording an actual value under execution needs a real Invoker to
   ! drive invoke_callback_method, which is out of scope here.
   !--------------------------------------------------------------------

   subroutine build_record_fields(hconfig, iface, handler_name, geom, vgrid, has_vgrid, mock_state, rc)
      type(ESMF_HConfig), intent(in) :: hconfig
      type(CallbackInterface), intent(in) :: iface
      character(*), intent(in) :: handler_name
      type(ESMF_Geom), intent(in) :: geom
      class(mapl_VerticalGrid), pointer, intent(in) :: vgrid
      logical, intent(in) :: has_vgrid
      type(ESMF_State), intent(inout) :: mock_state
      integer, optional, intent(out) :: rc

      type(ESMF_HConfig) :: record_cfg
      type(ESMF_HConfigIter) :: iter, b, e
      type(CallbackArgumentSpec) :: arg_spec
      type(CallbackMethodSpec) :: method_spec
      type(AccessSpec) :: access
      type(ESMF_Field) :: field
      character(ESMF_MAXSTR), allocatable :: arg_names(:)
      character(:), allocatable :: method_name, arg_name, field_name, msg
      logical :: has_section
      integer :: i, status

      has_section = ESMF_HConfigIsDefined(hconfig, keyString='record', _RC)
      _RETURN_UNLESS(has_section)

      record_cfg = ESMF_HConfigCreateAt(hconfig, keyString='record', _RC)
      b = ESMF_HConfigIterBegin(record_cfg, _RC)
      e = ESMF_HConfigIterEnd(record_cfg, _RC)
      iter = b
      do while (ESMF_HConfigIterLoop(iter, b, e, rc=status))
         _VERIFY(status)
         method_name = ESMF_HConfigAsStringMapKey(iter, _RC)
         msg = 'ConfigurableCallbackHandler ' // handler_name // &
              ': record names unknown method ' // method_name
         _ASSERT(iface%is_method(method_name), msg)
         method_spec = iface%get_method(method_name, _RC)
         arg_names = ESMF_HConfigAsStringSeq(record_cfg, ESMF_MAXSTR, keyString=method_name, _RC)
         do i = 1, size(arg_names)
            arg_name = trim(arg_names(i))
            msg = 'ConfigurableCallbackHandler ' // handler_name // ': record: method ' // &
                 method_name // ': unknown argument ' // arg_name
            _ASSERT(iface%is_argument(arg_name), msg)
            msg = 'ConfigurableCallbackHandler ' // handler_name // ': record: method ' // &
                 method_name // ': argument ' // arg_name // ' has no declared access in this method'
            _ASSERT(method_spec%is_argument_access(arg_name), msg)
            access = method_spec%get_argument_access(arg_name)
            msg = 'ConfigurableCallbackHandler ' // handler_name // ': record: method ' // &
                 method_name // ': argument ' // arg_name // ' is out-only in this method'
            _ASSERT(access == MAPL_ACCESS_IN .or. access == MAPL_ACCESS_INOUT, msg)
            arg_spec = iface%get_argument(arg_name, _RC)
            field_name = 'mapl_mock_' // trim(method_name) // '_' // arg_name
            field = build_field_for_argument(arg_spec, handler_name, geom, vgrid, has_vgrid, &
                 field_name, _RC)
            call ESMF_StateAdd(mock_state, [field], _RC)
         end do
      end do
      call ESMF_HConfigDestroy(record_cfg, _RC)

      _RETURN(_SUCCESS)
   end subroutine build_record_fields

   !--------------------------------------------------------------------
   ! non_negative
   !--------------------------------------------------------------------

   ! non_negative_cfg is a mapping {argument_name: {fill: column}}, one
   ! entry per interface argument to mark non-negative (the spec's own
   ! scenario names mixed_quantity, a declared argument, not some other
   ! mock-owned field - there is no such separate class here);
   ! names(i)/descriptors(i) is the pair built from its i-th entry,
   ! descriptors(i) carrying a NonNegativeCharacteristic.
   subroutine build_non_negative_descriptors(non_negative_cfg, iface, handler_name, names, &
        descriptors, rc)
      type(ESMF_HConfig), intent(in) :: non_negative_cfg
      type(CallbackInterface), intent(in) :: iface
      character(*), intent(in) :: handler_name
      character(ESMF_MAXSTR), allocatable, intent(out) :: names(:)
      type(StateItemDescriptor), allocatable, intent(out) :: descriptors(:)
      integer, optional, intent(out) :: rc

      type(ESMF_HConfigIter) :: iter, b, e
      type(ESMF_HConfig) :: entry_cfg
      type(CallbackArgumentSpec) :: arg_spec
      type(VariableSpec) :: var_spec
      character(:), allocatable :: name, fill, msg
      logical :: has_fill
      integer :: n, i, entry_size, status

      n = ESMF_HConfigGetSize(non_negative_cfg, _RC)
      allocate(names(n))
      allocate(descriptors(n))

      b = ESMF_HConfigIterBegin(non_negative_cfg, _RC)
      e = ESMF_HConfigIterEnd(non_negative_cfg, _RC)
      iter = b
      i = 0
      do while (ESMF_HConfigIterLoop(iter, b, e, rc=status))
         _VERIFY(status)
         i = i + 1
         name = ESMF_HConfigAsStringMapKey(iter, _RC)
         msg = 'ConfigurableCallbackHandler ' // handler_name // &
              ': non_negative names unknown argument ' // name
         _ASSERT(iface%is_argument(name), msg)
         arg_spec = iface%get_argument(name, _RC)
         msg = 'ConfigurableCallbackHandler ' // handler_name // ': non_negative argument ' // &
              name // ': column fill needs dims xyz'
         _ASSERT(arg_spec%get_dims() == 'xyz', msg)
         entry_cfg = ESMF_HConfigCreateAtMapVal(iter, _RC)
         entry_size = ESMF_HConfigGetSize(entry_cfg, _RC)
         msg = 'ConfigurableCallbackHandler ' // handler_name // ': non_negative argument ' // &
              name // ': only fill is a legal key'
         _ASSERT(entry_size == 1, msg)
         has_fill = ESMF_HConfigIsDefined(entry_cfg, keyString='fill', _RC)
         _ASSERT(has_fill, msg)
         fill = ESMF_HConfigAsString(entry_cfg, keyString='fill', _RC)
         call ESMF_HConfigDestroy(entry_cfg, _RC)
         var_spec = make_VariableSpec(ESMF_STATEINTENT_EXPORT, short_name=name, &
              non_negative=fill, rc=status)
         _VERIFY(status)
         names(i) = name
         call var_spec%make_characteristics(descriptors(i), _RC)
      end do

      _RETURN(_SUCCESS)
   end subroutine build_non_negative_descriptors

end module mapl_ConfigurableCallbackHandler_mod
