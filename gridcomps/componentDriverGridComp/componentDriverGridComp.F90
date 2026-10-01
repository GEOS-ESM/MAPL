#include "MAPL.h"

module mapl_ComponentDriverDriverGridComp_mod

   use MAPL
   use esmf
   use gFTL2_StringStringMap
   use gFTL2_StringVector, only: StringVector, StringVectorIterator, operator(/=)
   use timeSupport

   implicit none(type,external)
   private

   public :: setServices

   type :: Comp_Driver_Support
      type(StringStringMap) :: fillDefs
      type(StringVector) :: import_testing_expressions
      character(len=:), allocatable :: runMode
      type(timeVar) :: tFunc
      real :: delay ! in seconds
   end type Comp_Driver_Support

   character(*), parameter :: PRIVATE_STATE = "Comp_Driver_Support"
   character(*), parameter :: MAPL_SECTION = "mapl"
   character(*), parameter :: COMPONENT_STATES_SECTION = "states"
   character(*), parameter :: COMPONENT_EXPORT_STATE_SECTION = "export"
   character(*), parameter :: KEY_DEFAULT_VERT_PROFILE = "default_vertical_profile"
   character(len=*), parameter :: runModeGenerateExports = "GenerateExports"
   character(len=*), parameter :: runModeFillExportsFromImports = "FillExportsFromImports"
   character(len=*), parameter :: runModeFillImports = "FillImports"
   character(len=*), parameter :: runModeCompareImportsToReference = "CompareImportsToReference"
   character(len=*), parameter :: runModeCompareImportsToExpression = "CompareImportsToExpression"

contains

   subroutine setServices(gridcomp, rc)

      type(ESMF_GridComp) :: gridcomp
      integer, intent(out) :: rc

      integer :: status

      call MAPL_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_INITIALIZE, init, _RC)
      call MAPL_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_RUN, run, phase_name="run", _RC)
      ! Attach private state
      _SET_NAMED_PRIVATE_STATE(gridcomp, Comp_Driver_Support, PRIVATE_STATE)
      call add_internal_specs(gridcomp, _RC)

      _RETURN(_SUCCESS)

   contains

      subroutine add_internal_specs(gridcomp, rc)
         type(ESMF_GridComp), intent(inout) :: gridcomp
         integer, intent(out), optional :: rc
         integer :: status
         call MAPL_GridCompAddSpec(gridcomp, ESMF_STATEINTENT_INTERNAL, &
              'time_interval', &
              standard_name='unknown', &
              units='unknown', &
              vertical_stagger=MAPL_VERTICAL_STAGGER_NONE, &
              fill_value=0.0, _RC)
         call MAPL_GridCompAddSpec(gridcomp, ESMF_STATEINTENT_INTERNAL, &
              'rand', &
              standard_name='randomnumber', &
              units='unknown', &
              vertical_stagger=MAPL_VERTICAL_STAGGER_NONE, &
              fill_value=0.0, _RC)
         call MAPL_GridCompAddSpec(gridcomp, ESMF_STATEINTENT_INTERNAL, &
              'grid_lons', &
              standard_name='longitude', &
              units='degrees_east', &
              vertical_stagger=MAPL_VERTICAL_STAGGER_NONE, &
              fill_value=0.0, _RC)
         call MAPL_GridCompAddSpec(gridcomp, ESMF_STATEINTENT_INTERNAL, &
              'grid_lats', &
              standard_name='latitude', &
              units='degrees_north', &
              vertical_stagger=MAPL_VERTICAL_STAGGER_NONE, &
              fill_value=0.0, _RC)
         call MAPL_GridCompAddSpec(gridcomp, ESMF_STATEINTENT_INTERNAL, &
              'quarter_grid', &
              standard_name='quarter_grid', &
              units='NA', &
              vertical_stagger=MAPL_VERTICAL_STAGGER_NONE, &
              fill_value=0.0, _RC)
         call MAPL_GridCompAddSpec(gridcomp, ESMF_STATEINTENT_INTERNAL, &
              'fixed_columns', &
              standard_name='fixed_columns', &
              units='NA', &
              vertical_stagger=MAPL_VERTICAL_STAGGER_CENTER, &
              fill_value=0.0, _RC)
         _RETURN(_SUCCESS)

      end subroutine add_internal_specs
   end subroutine setServices

   subroutine init(gridcomp, importState, exportState, clock, rc)
      type(ESMF_GridComp) :: gridcomp
      type(ESMF_State) :: importState
      type(ESMF_State) :: exportState
      type(ESMF_Clock) :: clock
      integer, intent(out) :: rc

      character(:), allocatable :: field_name
      type(ESMF_HConfig) :: hconfig, mapl_cfg, states_cfg, export_cfg, field_cfg, fill_def, import_comp_expressions
      logical :: has_export_section, has_default_vert_profile
      real(kind=ESMF_KIND_R4), allocatable :: default_vert_profile(:)
      real(kind=ESMF_KIND_R4), pointer :: ptr3d(:, :, :)
      integer :: ii, jj, shape_(3), status
      type(ESMF_State) :: internal_state
      type(Comp_Driver_Support), pointer :: support
      type(ESMF_HConfigIter) :: iter, e, b
      logical :: is_present
      character(len=:), allocatable :: key, keyVal, vector_val
      type(ESMF_Time) :: current_time

      _GET_NAMED_PRIVATE_STATE(gridcomp, Comp_Driver_Support, PRIVATE_STATE, support)
      call MAPL_GridCompGet(gridcomp, hconfig=hconfig, _RC)

      call MAPL_GridCompGetInternalState(gridcomp, internal_state, _RC)

      support%runMode = ESMF_HConfigAsString(hconfig, keyString='RUN_MODE', _RC)
      support%delay = -1.0
      is_present = ESMF_HConfigIsDefined(hconfig, keyString='delay', _RC)
      if (is_present) then
         support%delay = ESMF_HConfigAsR4(hconfig, keyString='delay', _RC)
      end if
      fill_def = ESMF_HConfigCreateAt(hconfig, keyString='FILL_DEF', _RC)
      b = ESMF_HConfigIterBegin(fill_def, _RC)
      e = ESMF_HConfigIterEnd(fill_def, _RC)
      iter = b
      do while (ESMF_HConfigIterLoop(iter, b, e))
         key = ESMF_HConfigAsStringMapKey(iter, _RC)
         keyVal = ESMF_HConfigAsStringMapVal(iter, _RC)
         call support%fillDefs%insert(key, keyVal)
      end do

      is_present = ESMF_HConfigIsDefined(hconfig, keyString='import_comparison_expressions', _RC)
      if (is_present) then
         import_comp_expressions = ESMF_HConfigCreateAt(hconfig, keyString='import_comparison_expressions', _RC)
         b = ESMF_HConfigIterBegin(import_comp_expressions, _RC)
         e = ESMF_HConfigIterEnd(import_comp_expressions, _RC)
         iter = b
         do while (ESMF_HConfigIterLoop(iter, b, e))
            vector_val = ESMF_HConfigAsString(iter, _RC)
            call support%import_testing_expressions%push_back(vector_val)
         end do
      end if

      call ESMF_ClockGet(clock, currTime=current_time, _RC)
      call support%tFunc%init_time(hconfig, current_time, _RC)

      call initialize_internal_state(internal_state, support, hconfig, _RC)
      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(importState)
      _UNUSED_DUMMY(clock)
   end subroutine init

   recursive subroutine run(gridcomp, importState, exportState, clock, rc)
      type(ESMF_GridComp) :: gridcomp
      type(ESMF_State) :: importState
      type(ESMF_State) :: exportState
      type(ESMF_Clock) :: clock
      integer, intent(out) :: rc

      integer :: status
      type(ESMF_State) :: internal_state
      type(Comp_Driver_Support), pointer :: support
      type(ESMF_Time) :: current_time
      type(ESMF_Grid) :: grid
      type(ESMF_HConfig) :: hconfig
      logical :: is_present, print_min_max

      _GET_NAMED_PRIVATE_STATE(gridcomp, Comp_Driver_Support, PRIVATE_STATE, support)
      call ESMF_ClockGet(clock, currTime=current_time, _RC)
      call MAPL_GridCompGetInternalState(gridcomp, internal_state, _RC)
      call MAPL_GridCompGet(gridcomp, hconfig=hconfig, _RC)
      call update_internal_state(internal_state, current_time, support, hconfig, _RC)

      if (support%runMode == "GenerateExports") then
         call fill_state_from_internal(exportState, internal_state, support, _RC)
      else if (support%runMode == "FillExportsFromImports") then
         call copy_state(exportState, importState, _RC)
      else if (support%runMode == "FillImports") then
         ! there's nothing to do here
      else if (support%runMode == "CompareImportsToReference") then
         call fill_state_from_internal(exportState, internal_state, support, _RC)
         ! fill internal or export state
         ! compare import state to reference state
         call compare_states(importState, exportState, 0.001, _RC)
      else if (support%runMode == "CompareImportsToExpression") then
         call MAPL_GridCompGet(gridcomp, grid=grid, _RC)
         call compare_state_to_expressions(importState, internal_state, grid, support, 0.001, _RC)
      else
         _FAIL("no run mode selected")
      end if

      ! DEBUG: optionally print min/max of every field in the export state
      ! (top-level hconfig key 'print_export_min_max', default false)
      print_min_max = .false.
      is_present = ESMF_HConfigIsDefined(hconfig, keyString='print_export_min_max', _RC)
      if (is_present) then
         print_min_max = ESMF_HConfigAsLogical(hconfig, keyString='print_export_min_max', _RC)
      end if
      if (print_min_max) call print_state_min_max(exportState, "exportState", _RC)

      _UNUSED_DUMMY(importState)
      _UNUSED_DUMMY(exportState)
      _UNUSED_DUMMY(clock)
      _RETURN(_SUCCESS)

   end subroutine run

   subroutine initialize_internal_state(internal_state, support, hconfig, rc)
      type(ESMF_State), intent(inout) :: internal_state
      type(Comp_Driver_Support), intent(inout) :: support
      type(ESMF_HConfig), intent(in) :: hconfig
      integer, optional, intent(out) :: rc

      real, pointer :: ptr_2d(:, :)
      real(kind=ESMF_KIND_R8), pointer :: coords(:, :)
      integer :: status, seed_size, mypet, i, j
      integer, allocatable :: seeds(:)
      type(ESMF_Field) :: field
      type(ESMF_Grid) :: grid
      type(ESMF_VM) :: vm
      logical :: is_present
      real :: quarter_grid_fac1, quarter_grid_fac2

      ! rand
      call MAPL_StateGetPointer(internal_state, ptr_2d, 'rand', _RC)
      call random_seed(size=seed_size)
      allocate(seeds(seed_size))
      call ESMF_VMGetCurrent(vm, _RC)
      call ESMF_VMGet(vm, localPet=mypet, _RC)
      seeds = mypet
      call random_seed(put=seeds)
      call random_number(ptr_2d)
      ! lons and lats
      call MAPL_StateGetPointer(internal_state, ptr_2d, 'grid_lons', _RC)
      call ESMF_StateGet(internal_state, 'grid_lons', field, _RC)
      call ESMF_FieldGet(field, grid=grid, _RC)
      call ESMF_GridGetCoord(grid, coordDim=1, localDE=0, &
           staggerloc=ESMF_STAGGERLOC_CENTER, &
           farrayPtr=coords, _RC)
      ptr_2d = coords
      call MAPL_StateGetPointer(internal_state, ptr_2d, 'grid_lats', _RC)
      call ESMF_GridGetCoord(grid, coordDim=2, localDE=0, &
           staggerloc=ESMF_STAGGERLOC_CENTER, &
           farrayPtr=coords, _RC)
      ptr_2d = coords

      quarter_grid_fac1 = 1.0
      quarter_grid_fac2 = 2.0
      is_present = ESMF_HConfigIsDefined(hconfig, keyString='quarter_grid_fac1', _RC)
      if (is_present) then
         quarter_grid_fac1 = ESMF_HConfigAsR4(hconfig, keyString='quarter_grid_fac1', _RC)
      end if
      is_present = ESMF_HConfigIsDefined(hconfig, keyString='quarter_grid_fac2', _RC)
      if (is_present) then
         quarter_grid_fac2 = ESMF_HConfigAsR4(hconfig, keyString='quarter_grid_fac2', _RC)
      end if
      call MAPL_StateGetPointer(internal_state, ptr_2d, 'quarter_grid', _RC)
      ptr_2d = quarter_grid_fac2
      do i = 1, size(ptr_2d, 1), 2
         do j = 1, size(ptr_2d, 2), 2
            ptr_2d(i, j) = quarter_grid_fac1
         end do
      end do

      call fill_vertical_levels_from_config(internal_state, hconfig, _RC)

      _RETURN(_SUCCESS)

   end subroutine initialize_internal_state

   subroutine update_internal_state(internal_state, current_time, support, hconfig, rc)
      type(ESMF_State), intent(inout) :: internal_state
      type(ESMF_Time), intent(inout) :: current_time
      type(Comp_Driver_Support), intent(inout) :: support
      type(ESMF_HConfig), intent(in) :: hconfig
      integer, optional, intent(out) :: rc

      integer :: status
      real, pointer :: ptr_2d(:, :)
      logical :: is_present, apply_perturbation

      call MAPL_StateGetPointer(internal_state, ptr_2d, 'time_interval', _RC)
      ptr_2d = support%tFunc%evaluate_time(current_time, _RC)

      ! Re-apply the configured vertical profiles and, unless the top-level
      ! hconfig key 'apply_perturbation' is set to false (default: true),
      ! layer on a fresh, independent random whole-number perturbation
      ! (in [-100, 100]) for each field listed under 'vertical_levels', so the
      ! perturbation varies each time this routine is called without
      ! drifting/accumulating.
      apply_perturbation = .true.
      is_present = ESMF_HConfigIsDefined(hconfig, keyString='apply_perturbation', _RC)
      if (is_present) then
         apply_perturbation = ESMF_HConfigAsLogical(hconfig, keyString='apply_perturbation', _RC)
      end if
      call fill_vertical_levels_from_config(internal_state, hconfig, apply_perturbation=apply_perturbation, _RC)

      _RETURN(_SUCCESS)

   end subroutine update_internal_state

   ! Fills fields named under the 'vertical_levels' hconfig map with their
   ! configured per-level values (broadcast across all columns). When
   ! apply_perturbation is .true., each such field additionally receives its
   ! own independently-drawn random whole number in [-100, 100], added
   ! uniformly to every point in that field.
   subroutine fill_vertical_levels_from_config(internal_state, hconfig, apply_perturbation, rc)
      type(ESMF_State), intent(inout) :: internal_state
      type(ESMF_HConfig), intent(in) :: hconfig
      logical, optional, intent(in) :: apply_perturbation
      integer, optional, intent(out) :: rc

      integer :: status, ii, jj, shape_(3)
      logical :: is_present, do_perturb
      type(ESMF_HConfig) :: vertical_levels_cfg, level_val_cfg
      type(ESMF_HConfigIter) :: iter, b, e
      character(len=:), allocatable :: level_field_name
      real(kind=ESMF_KIND_R4), allocatable :: level_values(:)
      real(kind=ESMF_KIND_R4), pointer :: ptr3d(:, :, :)
      real(kind=ESMF_KIND_R8), allocatable :: level_values_r8(:)
      real(kind=ESMF_KIND_R8), pointer :: ptr3d_r8(:, :, :)
      type(ESMF_Field) :: level_field
      type(ESMF_TypeKind_Flag) :: level_typekind
      real :: harvest
      integer :: perturbation

      do_perturb = .false.
      if (present(apply_perturbation)) do_perturb = apply_perturbation

      is_present = ESMF_HConfigIsDefined(hconfig, keyString='vertical_levels', _RC)
      if (is_present) then
         vertical_levels_cfg = ESMF_HConfigCreateAt(hconfig, keyString='vertical_levels', _RC)
         b = ESMF_HConfigIterBegin(vertical_levels_cfg, _RC)
         e = ESMF_HConfigIterEnd(vertical_levels_cfg, _RC)
         iter = b
         do while (ESMF_HConfigIterLoop(iter, b, e))
            level_field_name = ESMF_HConfigAsStringMapKey(iter, _RC)
            level_val_cfg = ESMF_HConfigCreateAtMapVal(iter, _RC)
            call ESMF_StateGet(internal_state, trim(level_field_name), level_field, _RC)
            call ESMF_FieldGet(level_field, typekind=level_typekind, _RC)
            if (do_perturb) then
               call random_number(harvest)
               perturbation = floor(harvest * 201.0) - 100  ! whole number in [-100, 100]
            end if
            if (level_typekind == ESMF_TYPEKIND_R4) then
               level_values = ESMF_HConfigAsR4Seq(level_val_cfg, _RC)
               call MAPL_StateGetPointer(internal_state, ptr3d, trim(level_field_name), _RC)
               shape_ = shape(ptr3d)
               _ASSERT(shape_(3) == size(level_values), &
                    "vertical_levels size mismatch for field " // trim(level_field_name))
               do concurrent(ii = 1:shape_(1), jj = 1:shape_(2))
                  ptr3d(ii, jj, :) = level_values
               end do
               if (do_perturb) ptr3d = ptr3d + real(perturbation, kind=ESMF_KIND_R4)
            else if (level_typekind == ESMF_TYPEKIND_R8) then
               level_values_r8 = ESMF_HConfigAsR8Seq(level_val_cfg, _RC)
               call MAPL_StateGetPointer(internal_state, ptr3d_r8, trim(level_field_name), _RC)
               shape_ = shape(ptr3d_r8)
               _ASSERT(shape_(3) == size(level_values_r8), &
                    "vertical_levels size mismatch for field " // trim(level_field_name))
               do concurrent(ii = 1:shape_(1), jj = 1:shape_(2))
                  ptr3d_r8(ii, jj, :) = level_values_r8
               end do
               if (do_perturb) ptr3d_r8 = ptr3d_r8 + real(perturbation, kind=ESMF_KIND_R8)
            else
               _FAIL("unsupported typekind for vertical_levels field " // trim(level_field_name))
            end if
         end do
         call ESMF_HConfigDestroy(vertical_levels_cfg, _RC)
      end if

      _RETURN(_SUCCESS)

   end subroutine fill_vertical_levels_from_config

   ! Debug utility: loops over every field in a state (including fields
   ! nested in field bundles) and prints its min/max value. 'label' is
   ! printed alongside each field name to identify which state/call the
   ! output came from.
   subroutine print_state_min_max(state, label, rc)
      type(ESMF_State), intent(inout) :: state
      character(*), intent(in) :: label
      integer, optional, intent(out) :: rc

      integer :: status, item_count, i, j
      character(len=ESMF_MAXSTR), allocatable :: name_list(:)
      type(ESMF_StateItem_Flag), allocatable :: itemTypeList(:)
      type(ESMF_Field) :: field
      type(ESMF_FieldBundle) :: bundle
      type(ESMF_Field), allocatable :: field_list(:)
      character(len=ESMF_MAXSTR) :: component_name

      call ESMF_StateGet(state, itemCount=item_count, _RC)
      allocate(name_list(item_count), _STAT)
      allocate(itemTypeList(item_count), _STAT)
      call ESMF_StateGet(state, itemTypeList=itemTypeList, itemNameList=name_list, _RC)
      do i = 1, item_count
         if (itemTypeList(i) == ESMF_STATEITEM_FIELD) then
            call ESMF_StateGet(state, trim(name_list(i)), field, _RC)
            call print_field_min_max(label, trim(name_list(i)), field, _RC)
         else if (itemTypeList(i) == ESMF_STATEITEM_FIELDBUNDLE) then
            call ESMF_StateGet(state, trim(name_list(i)), bundle, _RC)
            call MAPL_FieldBundleGet(bundle, fieldList=field_list, _RC)
            do j = 1, size(field_list)
               call ESMF_FieldGet(field_list(j), name=component_name, _RC)
               call print_field_min_max(label, trim(name_list(i)) // ':' // trim(component_name), field_list(j), _RC)
            end do
         end if
      end do

      _RETURN(_SUCCESS)

   end subroutine print_state_min_max

   ! Prints the min/max value of a single field, prefixed with a caller
   ! supplied label and the field name.
   subroutine print_field_min_max(label, field_name, field, rc)
      character(*), intent(in) :: label
      character(*), intent(in) :: field_name
      type(ESMF_Field), intent(inout) :: field
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_TypeKind_Flag) :: typekind
      real(kind=ESMF_KIND_R4), pointer :: ptr_r4(:)
      real(kind=ESMF_KIND_R8), pointer :: ptr_r8(:)

      call ESMF_FieldGet(field, typekind=typekind, _RC)
      if (typekind == ESMF_TYPEKIND_R4) then
         call mapl_assignFptr(field, ptr_r4, _RC)
         write(*,'(A,": ",A," min=",ES14.6," max=",ES14.6)') trim(label), trim(field_name), minval(ptr_r4), maxval(ptr_r4)
      else if (typekind == ESMF_TYPEKIND_R8) then
         call mapl_assignFptr(field, ptr_r8, _RC)
         write(*,'(A,": ",A," min=",ES14.6," max=",ES14.6)') trim(label), trim(field_name), minval(ptr_r8), maxval(ptr_r8)
      else
         write(*,'(A,": ",A," unsupported typekind for min/max")') trim(label), trim(field_name)
      end if

      _RETURN(_SUCCESS)

   end subroutine print_field_min_max

   subroutine compare_state_to_expressions(state, internal_state, grid, support, threshold, rc)
      type(ESMF_State), intent(inout) :: state
      type(ESMF_State), intent(inout) :: internal_state
      type(ESMF_Grid), intent(in) :: grid
      type(Comp_Driver_Support), intent(inout) :: support
      real, intent(in) :: threshold
      integer, optional, intent(out) :: rc

      integer :: status, equal_pos
      character(len=:), allocatable :: lhs, rhs
      character(len=:), pointer :: equality
      type(StringVectorIterator) :: iter
      type(ESMF_Field) :: field_lhs, field_rhs
      real, pointer :: ptr_lhs(:), ptr_rhs(:)

      field_lhs = ESMF_FieldCreate(grid, ESMF_TYPEKIND_R4, _RC)
      field_rhs = ESMF_FieldCreate(grid, ESMF_TYPEKIND_R4, _RC)
      iter = support%import_testing_expressions%begin()
      do while (iter /= support%import_testing_expressions%end())
         equality => iter%of()
         equal_pos = index(equality, '=')
         _ASSERT(equal_pos /= 0, 'comparison expression is invalid')
         lhs = equality(:equal_pos - 1)
         rhs = equality(equal_pos + 1:)
         call MAPL_StateEval(state, lhs, field_lhs, _RC)
         call MAPL_StateEval(internal_state, rhs, field_rhs, _RC)
         call mapl_assignFptr(field_lhs, ptr_lhs, _RC)
         call mapl_assignFptr(field_rhs, ptr_rhs, _RC)
         if (any(abs(ptr_lhs - ptr_rhs) > threshold)) then
            _FAIL("state differs from reference state greater than allowed threshold")
         end if

         call iter%next()
      end do
      _RETURN(_SUCCESS)
   end subroutine compare_state_to_expressions

   subroutine fill_state_from_internal(state, internal_state, support, rc)
      type(ESMF_State), intent(inout) :: state
      type(ESMF_State), intent(inout) :: internal_state
      type(Comp_Driver_Support), intent(inout) :: support
      integer, optional, intent(out) :: rc

      integer :: status, item_count, i, j
      character(len=ESMF_MAXSTR), allocatable :: name_list(:)
      type(ESMF_StateItem_Flag), allocatable :: itemTypeList(:)
      type(ESMF_Field) :: field
      type(ESMF_FieldBundle) :: bundle
      type(ESMF_Field), allocatable :: field_list(:)
      character(len=:), pointer :: expression
      character(len=:), allocatable :: composite_name
      character(len=ESMF_MAXSTR) :: component_name
      character(*), parameter :: VECTOR_JOINTER = ";"
      character(len=1) :: jc

      call ESMF_StateGet(state, itemCount=item_count, _RC)
      allocate(name_list(item_count), _STAT)
      allocate(itemTypeList(item_count), _STAT)
      call ESMF_StateGet(state, itemTypeList=itemTypeList, itemNameList=name_list, _RC)
      do i = 1, item_count
         if (itemTypeList(i) == ESMF_STATEITEM_FIELD) then
            call ESMF_StateGet(state, trim(name_list(i)), field, _RC)
            expression => support%fillDefs%at(trim(name_list(i)))
            _ASSERT(associated(expression), "no expression for item " // trim(name_list(i)))
            call MAPL_StateEval(internal_state, expression, field, _RC)
         else if (itemTypeList(i) == ESMF_STATEITEM_FIELDBUNDLE) then
            call ESMF_StateGet(state, trim(name_list(i)), bundle, _RC)
            call MAPL_FieldBundleGet(bundle, fieldList=field_list, _RC)
            do j = 1, size(field_list)
               call ESMF_FieldGet(field_list(j), name=component_name, _RC)
               write(jc, '(I1)')j
               composite_name = trim(name_list(i)) // VECTOR_JOINTER // 'comp_' // jc
               expression => support%fillDefs%at(composite_name)
               _ASSERT(associated(expression), "no expression for item " // composite_name)
               call MAPL_StateEval(internal_state, expression, field_list(j), _RC)
            end do
         end if
      end do

      _RETURN(_SUCCESS)

   end subroutine fill_state_from_internal

   ! loop over destination state, find a matching name in source
   subroutine copy_state(dest_state, source_state, rc)
      type(ESMF_State), intent(inout) :: dest_state
      type(ESMF_State), intent(inout) :: source_state
      integer, optional, intent(out) :: rc

      integer :: itemCount, i, j, status
      type(ESMF_StateItem_Flag), allocatable :: itemTypeList(:)
      type(ESMF_StateItem_Flag) :: source_type
      character(len=ESMF_MAXSTR), allocatable :: itemNameList(:)
      type(ESMF_Field) :: dest_field, source_field
      type(ESMF_FieldBundle) :: dest_bundle, source_bundle
      type(ESMF_Field), allocatable :: dest_field_list(:), source_field_list(:)

      call ESMF_StateGet(dest_state, itemCount=itemCount, _RC)
      allocate(itemNameList(itemCount), _STAT)
      allocate(itemTypeList(itemCount), _STAT)
      call ESMF_StateGet(dest_state, itemTypeList=itemTypeList, itemNameList=itemNameList, _RC)
      do i = 1, itemCount
         if (itemTypeList(i) == ESMF_STATEITEM_FIELD) then
            call ESMF_StateGet(dest_state, trim(itemNameList(i)), dest_field, _RC)
            call ESMF_StateGet(source_state, trim(itemNameList(i)), source_type, _RC)
            _ASSERT(source_type == ESMF_STATEITEM_FIELD, 'source and destination are not both fields')
            call ESMF_StateGet(source_state, trim(itemNameList(i)), source_field, _RC)
            call MAPL_FieldCopy(source_field, dest_field, _RC)
         else if (itemTypeList(i) == ESMF_STATEITEM_FIELDBUNDLE) then
            call ESMF_StateGet(dest_state, trim(itemNameList(i)), dest_bundle, _RC)
            call ESMF_StateGet(source_state, trim(itemNameList(i)), source_type, _RC)
            _ASSERT(source_type == ESMF_STATEITEM_FIELDBUNDLE, 'source and destination are not both fieldbundles')
            call ESMF_StateGet(source_state, trim(itemNameList(i)), source_bundle, _RC)
            call MAPL_FieldBundleGet(source_bundle, fieldList=source_field_list, _RC)
            call MAPL_FieldBundleGet(dest_bundle, fieldList=dest_field_list, _RC)
            do j = 1, size(source_field_list)
               call MAPL_FieldCopy(source_field_list(j), dest_field_list(j), _RC)
            end do
         end if
      end do

      _RETURN(_SUCCESS)
   end subroutine copy_state

   subroutine compare_states(state, reference_state, threshold, rc)
      type(ESMF_State), intent(inout) :: state
      type(ESMF_State), intent(inout) :: reference_state
      real, intent(in) :: threshold
      integer, optional, intent(out) :: rc

      integer :: itemCount, i, j, status
      type(ESMF_StateItem_Flag), allocatable :: itemTypeList(:)
      type(ESMF_StateItem_Flag) :: source_type
      character(len=ESMF_MAXSTR), allocatable :: itemNameList(:)
      type(ESMF_Field) :: field, reference_field
      real(kind=ESMF_KIND_R4), pointer :: ptr(:), reference_ptr(:)
      real(kind=ESMF_KIND_R8), pointer :: ptr_r8(:), reference_ptr_r8(:)
      type(ESMF_TypeKind_Flag) :: typekind
      type(ESMF_FieldBundle) :: bundle, reference_bundle
      type(ESMF_Field), allocatable :: field_list(:), reference_field_list(:)

      call ESMF_StateGet(state, itemCount=itemCount, _RC)
      allocate(itemNameList(itemCount), _STAT)
      allocate(itemTypeList(itemCount), _STAT)
      call ESMF_StateGet(state, itemTypeList=itemTypeList, itemNameList=itemNameList, _RC)
      do i = 1, itemCount
         if (itemTypeList(i) == ESMF_STATEITEM_FIELD) then
            call ESMF_StateGet(state, trim(itemNameList(i)), field, _RC)
            call ESMF_StateGet(reference_state, trim(itemNameList(i)), source_type, _RC)
            _ASSERT(source_type == ESMF_STATEITEM_FIELD, 'source and destination are not both fields')
            call ESMF_StateGet(reference_state, trim(itemNameList(i)), reference_field, _RC)
            call ESMF_FieldGet(field, typekind=typekind, _RC)
            if (typekind == ESMF_TYPEKIND_R4) then
               call mapl_assignFptr(field, ptr, _RC)
               call mapl_assignFptr(reference_field, reference_ptr, _RC)
               if (any(abs(ptr - reference_ptr) > threshold)) then
                  _FAIL("state differs from reference state greater than allowed threshold")
               end if
            else if (typekind == ESMF_TYPEKIND_R8) then
               call mapl_assignFptr(field, ptr_r8, _RC)
               call mapl_assignFptr(reference_field, reference_ptr_r8, _RC)
               if (any(abs(ptr_r8 - reference_ptr_r8) > real(threshold, ESMF_KIND_R8))) then
                  _FAIL("state differs from reference state greater than allowed threshold")
               end if
            else
               _FAIL("unsupported typekind in compare_states")
            end if
         else if (itemTypeList(i) == ESMF_STATEITEM_FIELDBUNDLE) then
            call ESMF_StateGet(reference_state, trim(itemNameList(i)), reference_bundle, _RC)
            call ESMF_StateGet(state, trim(itemNameList(i)), source_type, _RC)
            _ASSERT(source_type == ESMF_STATEITEM_FIELDBUNDLE, 'source and destination are not both fieldbundles')
            call ESMF_StateGet(state, trim(itemNameList(i)), bundle, _RC)
            call MAPL_FieldBundleGet(bundle, fieldList=field_list, _RC)
            call MAPL_FieldBundleGet(reference_bundle, fieldList=reference_field_list, _RC)
            _ASSERT(size(field_list) == size(reference_field_list), 'fields from vector bundle not same size')
            do j = 1, size(field_list)
               call ESMF_FieldGet(field_list(j), typekind=typekind, _RC)
               if (typekind == ESMF_TYPEKIND_R4) then
                  call mapl_assignFptr(field_list(j), ptr, _RC)
                  call mapl_assignFptr(reference_field_list(j), reference_ptr, _RC)
                  _ASSERT(.not.any(abs(ptr - reference_ptr) > threshold) , "state differs from reference state greater than allowed threshold")
               else if (typekind == ESMF_TYPEKIND_R8) then
                  call mapl_assignFptr(field_list(j), ptr_r8, _RC)
                  call mapl_assignFptr(reference_field_list(j), reference_ptr_r8, _RC)
                  _ASSERT(.not.any(abs(ptr_r8 - reference_ptr_r8) > threshold) , "state differs from reference state greater than allowed threshold")
               else
                  _FAIL("unsupported typekind in compare_states")
               end if
            end do
         end if
      end do

      _RETURN(_SUCCESS)
   end subroutine compare_states

end module mapl_ComponentDriverDriverGridComp_mod

subroutine setServices(gridcomp, rc)
   use esmf
   use MAPL
   use mapl_ComponentDriverDriverGridComp_mod, only: Root_setServices => setServices
   type(ESMF_GridComp) :: gridcomp
   integer, intent(out) :: rc
   integer :: status
   call Root_setServices(gridcomp, _RC)
   _RETURN(_SUCCESS)
end subroutine setServices
