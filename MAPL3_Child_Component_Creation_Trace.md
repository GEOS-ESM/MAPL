# MAPL3 Child Component Creation Trace - Develop Branch

## Overview

This document provides a detailed step-by-step trace of how child components are created from parent components in MAPL3 (develop branch), starting from YAML loading through complete child gridcomp initialization.

---

## Step 1: Parent YAML Structure - Defining Children

### File Location: `/Users/wdboggs/src/MAPL/superstructure/generic/tests/scenarios/parent.yaml`

### YAML Structure with `children:` Section

```yaml
grid:
  class: LatLon
  im_world: 12
  jm_world: 6
  pole: pe
  dateline: de

children:
  A:
    dso:  libconfigurable_gridcomp
    config_file: scenarios/precision_extension/A.yaml
  B:
    dso:  libconfigurable_gridcomp
    config_file: scenarios/precision_extension/B.yaml

states: {}

connections:
  - src_name: E_A1
    dst_name: I_B1
    src_comp: A
    dst_comp: B
```

### Alternative YAML with setServices in children (History component):

File: `/Users/wdboggs/src/MAPL/superstructure/generic/tests/scenarios/statistics/history.yaml`

```yaml
mapl:
  children:
    collection_1:
      dso: libconfigurable_gridcomp
      config_file: scenarios/statistics/collection_1.yaml
    STAT:
      dso: libproto_stat_gc
      config_file: scenarios/statistics/stat.yaml

  states: {}

  import:
    A/T:
      vertical_dim_spec: MIRROR

  connections:
    - src_name: avg_T
      src_comp: STAT
      dst_name: avg_T
      dst_comp: collection_1

  setServices:
    sharedObj: libconfigurable_gridcomp
    userRoutine: setservices_
```

### Key YAML Sections:

1. **`children:` (top-level or under `mapl:`)** - Mapping of child component names to specifications
2. **Child specification keys:**
   - `dso` or `DSO` or `sharedObj` or `sharedobj` - Shared object library name
   - `SetServices` or `setServices` or `setservices` - Optional; default is `setservices_`
   - `config_file` - Path to child's config file (optional)
   - `timestep` - Optional time interval for child
   - `run_time_offset` - Optional offset (optional)

---

## Step 2: Parent Component Creation and YAML Loading

### Entry Point: GridCompCreate Function

**File:** `/Users/wdboggs/src/MAPL/superstructure/generic/GenericGridComp.F90`
**Function:** `create_grid_comp_primary()` (lines 87-142)

```fortran
recursive type(ESMF_GridComp) function create_grid_comp_primary( &
     name, set_services, config, unusable, petlist, rc) result(gridcomp)
  use :: mapl_UserSetServices_mod, only: AbstractUserSetServices

  character(*), intent(in) :: name
  class(AbstractUserSetServices), intent(in) :: set_services
  type(ESMF_HConfig), intent(in) :: config          ! <-- YAML loaded as HConfig
  class(KeywordEnforcer), optional, intent(in) :: unusable
  integer, optional, intent(in) :: petlist(:)
  integer, optional, intent(out) :: rc

  type(ESMF_GridComp) :: user_gridcomp
  type(OuterMetaComponent), pointer :: outer_meta
  type(GriddedComponentDriver) :: user_gc_driver
  
  ! ... create outer and inner gridcomps ...
  
  ! Store HConfig in OuterMetaComponent
  outer_meta = OuterMetaComponent(gridcomp, user_gc_driver, set_services, config)
  call outer_meta%init_meta(_RC)

end function create_grid_comp_primary
```

### OuterMetaComponent Initialization

**File:** `/Users/wdboggs/src/MAPL/superstructure/generic/OuterMetaComponent/new_outer_meta.F90`
**Function:** `new_outer_meta()` (lines 9-29)

```fortran
module function new_outer_meta(gridcomp, user_gc_driver, user_setServices, hconfig) &
    result(outer_meta)
  type(OuterMetaComponent) :: outer_meta
  type(ESMF_GridComp), intent(in) :: gridcomp
  type(GriddedComponentDriver), intent(in) :: user_gc_driver
  class(AbstractUserSetServices), intent(in) :: user_setservices
  type(ESMF_HConfig), intent(in) :: hconfig         ! <-- YAML config stored here
  
  outer_meta%self_gridcomp = gridcomp
  outer_meta%user_gc_driver = user_gc_driver
  allocate(outer_meta%user_setServices, source=user_setServices)
  outer_meta%hconfig = hconfig                      ! <-- KEY: HConfig stored in OuterMetaComponent
  
  ! ...
end function new_outer_meta
```

**Key Data Structure:**
```fortran
type :: OuterMetaComponent
  type(ESMF_HConfig) :: hconfig    ! <-- Stores parent YAML
  type(GriddedComponentDriverMap) :: children  ! <-- Stores child gridcomps
  type(ComponentSpec) :: component_spec  ! <-- Parsed spec including children
  ! ... other fields
end type OuterMetaComponent
```

---

## Step 3: Parent SetServices Execution and YAML Parsing

### Entry Point: GenericSetServices

**File:** `/Users/wdboggs/src/MAPL/superstructure/generic/GenericGridComp.F90`
**Subroutine:** `GenericSetServices()` (lines 34-82)

```fortran
recursive subroutine GenericSetServices(gridcomp, rc)
  type(ESMF_GridComp) :: gridcomp
  integer, intent(out) :: rc
  
  type(OuterMetaComponent), pointer :: outer_meta
  
  outer_meta => get_outer_meta(gridcomp, _RC)
  call outer_meta%setServices(_RC)      ! <-- Calls OuterMetaComponent%SetServices_
  ! ...
end subroutine GenericSetServices
```

### OuterMetaComponent%SetServices_ Execution

**File:** `/Users/wdboggs/src/MAPL/superstructure/generic/OuterMetaComponent/SetServices.F90`
**Subroutine:** `SetServices_()` (lines 29-100)

```fortran
recursive module subroutine SetServices_(this, rc)
  use mapl_ComponentSpecParser_mod
  class(OuterMetaComponent), target, intent(inout) :: this
  integer, intent(out) :: rc
  
  integer :: status
  type(ESMF_GridComp) :: user_gridcomp
  
  ! ===== STEP 1: Parse parent YAML into ComponentSpec =====
  this%component_spec = parse_component_spec(this%hconfig, this%registry, &
      this%user_gc_driver%get_name(), _RC)
  
  user_gridcomp = this%user_gc_driver%get_gridcomp()
  call attach_inner_meta(user_gridcomp, this%self_gridcomp, _RC)
  
  ! ===== STEP 2: Call user's SetServices =====
  call this%user_setservices%run(user_gridcomp, _RC)
  
  ! ===== STEP 3: Add children from parsed spec =====
  call add_children(this, _RC)
  
  ! ===== STEP 4: Call SetServices on all children =====
  call run_children_setservices(this, _RC)
  
  _RETURN(ESMF_SUCCESS)

contains

  recursive subroutine add_children(this, rc)
    ! Iterates through this%component_spec%children (parsed from YAML)
    ! and adds each child via this%add_child()
    type(ChildSpecMapIterator) :: iter
    type(ChildSpec), pointer :: child_spec
    character(:), allocatable :: child_name
    
    associate ( e => this%component_spec%children%ftn_end() )
      iter = this%component_spec%children%ftn_begin()
      do while (iter /= e)
        call iter%next()
        child_name = iter%first()
        child_spec => iter%second()
        call this%add_child(child_name, child_spec, _RC)
      end do
    end associate
  end subroutine add_children
  
end subroutine SetServices_
```

---

## Step 4: YAML Parsing - `parse_component_spec()`

### File: `/Users/wdboggs/src/MAPL/superstructure/generic/ComponentSpecParser/parse_component_spec.F90`
**Function:** `parse_component_spec()` (lines 8-33)

```fortran
module function parse_component_spec(hconfig, registry, component_name, rc) result(spec)
  type(ComponentSpec) :: spec
  type(ESMF_HConfig), target, intent(inout) :: hconfig
  type(StateRegistry), target, intent(in) :: registry
  character(*), intent(in) :: component_name
  integer, optional, intent(out) :: rc
  
  integer :: status
  logical :: has_mapl_section
  type(ESMF_HConfig) :: mapl_cfg
  
  ! Navigate to the 'mapl:' section in parent YAML
  has_mapl_section = ESMF_HConfigIsDefined(hconfig, keyString=MAPL_SECTION, _RC)
  _RETURN_UNLESS(has_mapl_section)
  mapl_cfg = ESMF_HConfigCreateAt(hconfig, keyString=MAPL_SECTION, _RC)
  
  ! Parse all sections including children
  spec%geometry_spec = parse_geometry_spec(mapl_cfg, registry, component_name, _RC)
  spec%var_specs = parse_var_specs(mapl_cfg, registry, component_name, _RC)
  spec%connections = parse_connections(mapl_cfg, _RC)
  spec%children = parse_children(mapl_cfg, _RC)    ! <-- KEY: Parse children section
  spec%misc = parse_misc(mapl_cfg, _RC)
  
  call ESMF_HConfigDestroy(mapl_cfg, _RC)
  
  _RETURN(_SUCCESS)
end function parse_component_spec
```

**Data Structure Created:**
```fortran
type :: ComponentSpec
  type(GeometrySpec) :: geometry_spec
  type(VariableSpecVector) :: var_specs
  type(ConnectionVector) :: connections
  type(ChildSpecMap) :: children    ! <-- Map: child_name => ChildSpec
  type(MiscellaneousComponentSpec) :: misc
end type ComponentSpec
```

---

## Step 5: Children Parsing - `parse_children()`

### File: `/Users/wdboggs/src/MAPL/superstructure/generic/ComponentSpecParser/parse_children.F90`
**Function:** `parse_children()` (lines 9-45)

```fortran
module function parse_children(hconfig, rc) result(children)
  type(ChildSpecMap) :: children
  type(ESMF_HConfig), intent(in) :: hconfig
  integer, optional, intent(out) :: rc
  
  integer :: status
  logical :: has_children
  logical :: is_map
  type(ESMF_HConfig) :: children_cfg, child_cfg
  type(ESMF_HConfigIter) :: iter, iter_begin, iter_end
  type(ChildSpec) :: child_spec
  character(:), allocatable :: child_name
  
  ! Check if 'children:' section exists
  has_children = ESMF_HConfigIsDefined(hconfig, keyString=COMPONENT_CHILDREN_SECTION, _RC)
  _RETURN_UNLESS(has_children)   ! Return empty if no children
  
  ! Navigate to 'children:' section
  children_cfg = ESMF_HConfigCreateAt(hconfig, keyString=COMPONENT_CHILDREN_SECTION, _RC)
  is_map = ESMF_HConfigIsMap(children_cfg, _RC)
  _ASSERT(is_map, 'children spec must be mapping')
  
  ! Iterate through each child (A, B, etc.)
  iter_begin = ESMF_HCOnfigIterBegin(children_cfg, _RC)
  iter_end = ESMF_HConfigIterEnd(children_cfg, _RC)
  iter = iter_begin
  
  do while (ESMF_HConfigIterLoop(iter, iter_begin, iter_end))
    ! Get child name (e.g., "A", "B", "collection_1")
    child_name = ESMF_HConfigAsStringMapKey(iter, _RC)
    
    ! Get child config section
    child_cfg = ESMF_HConfigCreateAtMapVal(iter, _RC)
    
    ! Parse individual child spec
    child_spec = parse_child(child_cfg, _RC)
    
    ! Store in map: children["A"] = child_spec
    call children%insert(child_name, child_spec)
    
    call ESMF_HConfigDestroy(child_cfg, _RC)
  end do
  
  call ESMF_HConfigDestroy(children_cfg, _RC)
  
  _RETURN(_SUCCESS)
end function parse_children
```

**Data Structure:**
```fortran
type :: ChildSpecMap
  ! Maps child name => ChildSpec
  ! For parent.yaml: {"A" => ChildSpec, "B" => ChildSpec}
end type ChildSpecMap
```

---

## Step 6: Individual Child Parsing - `parse_child()`

### File: `/Users/wdboggs/src/MAPL/superstructure/generic/ComponentSpecParser/parse_child.F90`
**Function:** `parse_child()` (lines 8-70)

### Key YAML Example Input (from parent.yaml):
```yaml
A:
  dso:  libconfigurable_gridcomp
  config_file: scenarios/precision_extension/A.yaml
```

### Parse Implementation:

```fortran
module function parse_child(hconfig, rc) result(child)
  type(ChildSpec) :: child
  type(ESMF_HConfig), intent(in) :: hconfig
  integer, optional, intent(out) :: rc
  
  integer :: status
  class(AbstractUserSetServices), allocatable :: setservices
  
  ! Define allowed key names for DSO
  character(*), parameter :: dso_keys(*) = [character(len=9) :: &
      'dso', 'DSO', 'sharedObj', 'sharedobj']
  
  ! Define allowed key names for SetServices routine name
  character(*), parameter :: userProcedure_keys(*) = [character(len=10) :: &
      'SetServices', 'setServices', 'setservices']
  
  integer :: i
  character(:), allocatable :: dso_key, userProcedure_key, try_key
  logical :: dso_found, userProcedure_found
  logical :: has_key
  logical :: has_config_file
  type(ESMF_HConfig), allocatable :: child_hconfig
  character(:), allocatable :: sharedObj, userProcedure, config_file
  type(ESMF_TimeInterval), allocatable :: offset
  type(ESMF_TimeInterval), allocatable :: timeStep
  
  ! ===== STEP 1: Read DSO name =====
  dso_found = .false.
  ! Ensure precisely one name is used for dso
  do i = 1, size(dso_keys)
    try_key = trim(dso_keys(i))
    has_key = ESMF_HconfigIsDefined(hconfig, keyString=try_key, _RC)
    if (has_key) then
      _ASSERT(.not. dso_found, 'multiple specifications for dso in hconfig for child')
      dso_found = .true.
      dso_key = try_key
    end if
  end do
  _ASSERT(dso_found, 'Must specify a dso for hconfig of child')
  sharedObj = ESMF_HconfigAsString(hconfig, keyString=dso_key, _RC)
  ! Result: sharedObj = "libconfigurable_gridcomp"
  
  ! ===== STEP 2: Read SetServices routine name =====
  userProcedure_found = .false.
  do i = 1, size(userProcedure_keys)
    try_key = userProcedure_keys(i)
    if (ESMF_HconfigIsDefined(hconfig, keyString=try_key)) then
      _ASSERT(.not. userProcedure_found, 'multiple specifications for dso in hconfig for child')
      userProcedure_found = .true.
      userProcedure_key = try_key
    end if
  end do
  ! Default to 'setservices_' if not specified
  userProcedure = 'setservices_'
  if (userProcedure_found) then
    userProcedure = ESMF_HconfigAsString(hconfig, keyString=userProcedure_key, _RC)
  end if
  ! Result: userProcedure = "setservices_"
  
  ! ===== STEP 3: Read config_file =====
  has_config_file = ESMF_HconfigIsDefined(hconfig, keyString='config_file', _RC)
  if (has_config_file) then
    config_file = ESMF_HconfigAsString(hconfig, keyString='config_file', _RC)
    ! Load child's config file into HConfig
    child_hconfig = ESMF_HConfigCreate(filename=config_file, _RC)
    ! Result: child_hconfig contains A.yaml contents
  end if
  
  ! ===== STEP 4: Create user_setservices object =====
  setservices = user_setservices(sharedObj, userProcedure)
  ! Result: DSOSetServices object holding ("libconfigurable_gridcomp", "setservices_")
  
  ! ===== STEP 5: Parse optional timestep and offset =====
  call parse_timespec(hconfig, timeStep, offset, _RC)
  
  ! ===== STEP 6: Create ChildSpec =====
  child = ChildSpec(setservices, hconfig=child_hconfig, &
      timeStep=timeStep, offset=offset)
  
  _RETURN(_SUCCESS)
end function parse_child
```

### Data Structure Created:

```fortran
type :: ChildSpec
  class(AbstractUserSetServices), allocatable :: user_setservices
  type(ESMF_HConfig) :: hconfig                ! Child's YAML config
  type(ESMF_TimeInterval), allocatable :: timeStep
  type(ESMF_TimeInterval) :: offset
end type ChildSpec

! Concrete implementation:
type, extends(AbstractUserSetServices) :: DSOSetServices
  character(:), allocatable :: sharedObj    ! "libconfigurable_gridcomp"
  character(:), allocatable :: userRoutine  ! "setservices_"
end type DSOSetServices
```

### Example Result for Child "A":

```
ChildSpec {
  user_setservices: DSOSetServices {
    sharedObj: "libconfigurable_gridcomp"
    userRoutine: "setservices_"
  }
  hconfig: <HConfig containing A.yaml content>
  timeStep: <not allocated>
  offset: 0 seconds
}
```

---

## Step 7: User SetServices Factory

### File: `/Users/wdboggs/src/MAPL/superstructure/generic/UserSetServices.F90`
**Function:** `new_DSOSetServices()` (lines 135-149)

```fortran
function new_DSOSetServices(sharedObj, userRoutine) result(dso_setservices)
  use mapl_DSO_Utilities_mod
  type(DSOSetServices) :: dso_setservices
  character(len=*), intent(in) :: sharedObj
  character(len=*), optional, intent(in) :: userRoutine
  
  character(:), allocatable :: userRoutine_
  
  userRoutine_ = 'setservices_'  ! Default value
  if (present(userRoutine)) userRoutine_ = userRoutine
  
  dso_setservices%sharedObj   = sharedObj
  dso_setservices%userRoutine = userRoutine_
  
end function new_DSOSetServices
```

---

## Step 8: Children Addition to OuterMetaComponent

### File: `/Users/wdboggs/src/MAPL/superstructure/generic/OuterMetaComponent/add_child_by_spec.F90`
**Subroutine:** `add_child_by_spec()` (lines 19-55)

Called by: `SetServices_()` line 69 in SetServices.F90

```fortran
module recursive subroutine add_child_by_spec(this, child_name, child_spec, rc)
  class(OuterMetaComponent), target, intent(inout) :: this
  character(*), intent(in) :: child_name
  type(ChildSpec), intent(inout) :: child_spec
  integer, optional, intent(out) :: rc
  
  integer :: status
  type(GriddedComponentDriver) :: child_driver
  type(ESMF_GridComp) :: child_outer_gc
  type(OuterMetaComponent), pointer :: child_meta
  type(ESMF_HConfig) :: total_hconfig
  class(Logger), pointer :: lgr
  character(:), allocatable :: this_name
  
  ! Validation
  _ASSERT(is_valid_name(child_name), 'Child name <' // child_name // &
      '> does not conform to GEOS standards.')
  _ASSERT(this%children%count(child_name) == 0, &
      'duplicate child name: <'//child_name//'>.')
  
  ! ===== STEP 1: Merge parent and child HConfigs =====
  total_hconfig = merge_hconfig(this%hconfig, child_spec%hconfig, _RC)
  ! Result: Combined config with parent's top-level settings + child's mapl section
  
  ! ===== STEP 2: Create child GridComp =====
  ! Call: MAPL_GridCompCreate(name, setservices, hconfig)
  ! Which internally calls: GridCompCreate() from GenericGridComp.F90
  child_outer_gc = MAPL_GridCompCreate(child_name, &
      child_spec%user_setservices, total_hconfig, _RC)
  ! Result: Child outer GridComp created with OuterMetaComponent attached
  
  ! ===== STEP 3: Extract child's OuterMetaComponent =====
  child_meta => get_outer_meta(child_outer_gc, _RC)
  
  ! ===== STEP 4: Register child's StateRegistry =====
  call this%registry%add_subregistry(child_meta%get_registry())
  
  ! ===== STEP 5: Set child timeStep if specified =====
  if (allocated(child_spec%timeStep)) then
    child_meta%user_timeStep = child_spec%timeStep
  end if
  
  ! ===== STEP 6: Set child offset =====
  child_meta%user_offset = this%user_offset + child_spec%offset
  
  ! ===== STEP 7: Store child in parent's children map =====
  child_driver = GriddedComponentDriver(child_outer_gc)
  call this%children%insert(child_name, child_driver)
  ! Result: this%children["A"] = GriddedComponentDriver(child_outer_gc)
  
  lgr => this%get_logger()
  this_name = this%get_name()
  call lgr%debug('%a added child <%a~>', this_name, child_name, _RC)
  
  _RETURN(_SUCCESS)
end subroutine add_child_by_spec
```

---

## Step 9: GridCompAddChild Call Chain

### Step 9a: Direct GridCompAddChild Overloads

**File:** `/Users/wdboggs/src/MAPL/superstructure/generic/MAPL_Generic.F90`
**Interface Definition:** lines 155-161

```fortran
interface GridCompAddChild
  procedure :: gridcomp_add_child_by_procedure_and_config
  procedure :: gridcomp_add_child_by_procedure_and_config_file
  procedure :: gridcomp_add_child_by_dso_and_config
  procedure :: gridcomp_add_child_by_dso_and_config_file
  procedure :: gridcomp_add_child_by_spec
end interface GridCompAddChild
```

### Step 9b: DSO + Config File Variant

**File:** `/Users/wdboggs/src/MAPL/superstructure/generic/MAPL_Generic.F90`
**Subroutine:** `gridcomp_add_child_by_dso_and_config_file()` (lines 506-535)

```fortran
subroutine gridcomp_add_child_by_dso_and_config_file(gridcomp, child_name, &
    shared_obj, user_routine, hconfig_file, unusable, timeStep, refTime_offset, rc)
  use mapl_UserSetServices_mod
  type(ESMF_GridComp), intent(inout) :: gridcomp
  character(len=*), intent(in) :: child_name
  character(len=*), intent(in) :: shared_obj
  character(len=*), intent(in) :: user_routine
  character(len=*), intent(in) :: hconfig_file
  class(KeywordEnforcer), optional, intent(out) :: unusable
  type(ESMF_TimeInterval), optional, intent(in) :: timeStep
  type(ESMF_TimeInterval), optional, intent(in) :: refTime_offset
  integer, optional, intent(out) :: rc
  
  type(ESMF_HConfig) :: hconfig
  integer :: status
  
  ! Load config file
  hconfig = ESMF_HConfigCreate(filename=hconfig_file, _RC)
  
  ! Delegate to dso+config variant
  call GridCompAddChild( &
      gridcomp, child_name, shared_obj, user_routine, hconfig, &
      timeStep=timeStep, refTime_offset=refTime_offset, _RC)
  
  call ESMF_HConfigDestroy(hconfig, _RC)
  
  _RETURN(_SUCCESS)
  _UNUSED_DUMMY(unusable)
end subroutine gridcomp_add_child_by_dso_and_config_file
```

### Step 9c: DSO + Config Variant

**File:** `/Users/wdboggs/src/MAPL/superstructure/generic/MAPL_Generic.F90`
**Subroutine:** `gridcomp_add_child_by_dso_and_config()` (lines 480-504)

```fortran
subroutine gridcomp_add_child_by_dso_and_config(gridcomp, child_name, shared_obj, &
    user_routine, hconfig, unusable, timeStep, refTime_offset, rc)
  use mapl_UserSetServices_mod
  type(ESMF_GridComp), intent(inout) :: gridcomp
  character(len=*), intent(in) :: child_name
  character(len=*), intent(in) :: shared_obj
  character(len=*), intent(in) :: user_routine
  type(ESMF_HConfig), intent(in) :: hconfig
  class(KeywordEnforcer), optional, intent(out) :: unusable
  type(ESMF_TimeInterval), optional, intent(in) :: timeStep
  type(ESMF_TimeInterval), optional, intent(in) :: refTime_offset
  integer, optional, intent(out) :: rc
  
  integer :: status
  class(AbstractUserSetServices), allocatable :: setservices
  type(ChildSpec) :: child_spec
  
  ! Validate name
  _ASSERT(is_valid_name(child_name), &
      'Child name <' // child_name //'> does not conform to GEOS standards.')
  
  ! ===== Create UserSetServices object =====
  setservices = user_setservices(shared_obj, user_routine)
  ! Result: DSOSetServices("libconfigurable_gridcomp", "setservices_")
  
  ! ===== Create ChildSpec =====
  child_spec = ChildSpec(setServices, hconfig=hconfig, timeStep=timeStep, &
      offset=refTime_offset)
  
  ! ===== Delegate to spec variant =====
  call GridCompAddChild(gridcomp, child_name, child_spec, _RC)
  
  _RETURN(_SUCCESS)
  _UNUSED_DUMMY(unusable)
end subroutine gridcomp_add_child_by_dso_and_config
```

### Step 9d: ChildSpec Variant (Final)

**File:** `/Users/wdboggs/src/MAPL/superstructure/generic/MAPL_Generic.F90`
**Subroutine:** `gridcomp_add_child_by_spec()` (lines 537-550)

```fortran
subroutine gridcomp_add_child_by_spec(gridcomp, child_name, child_spec, rc)
  type(ESMF_GridComp), intent(inout) :: gridcomp
  character(len=*), intent(in) :: child_name
  type(ChildSpec), intent(inout) :: child_spec
  integer, optional, intent(out) :: rc
  
  integer :: status
  type(OuterMetaComponent), pointer :: outer_meta
  
  ! Get parent's OuterMetaComponent
  call GridCompGetOuterMeta(gridcomp, outer_meta, _RC)
  
  ! ===== Delegate to OuterMetaComponent%add_child =====
  ! This is where the actual work happens
  call outer_meta%add_child(child_name, child_spec, _RC)
  
  _RETURN(_SUCCESS)
end subroutine gridcomp_add_child_by_spec
```

---

## Step 10: Child GridComp Creation - GridCompCreate

### File: `/Users/wdboggs/src/MAPL/superstructure/generic/GenericGridComp.F90`
**Function:** `create_grid_comp_primary()` (lines 87-142)

When called from `add_child_by_spec.F90` line 37 as:
```fortran
child_outer_gc = MAPL_GridCompCreate(child_name, child_spec%user_setservices, 
    total_hconfig, _RC)
```

This function:

1. **Creates outer GridComp**
   ```fortran
   gridcomp = ESMF_GridCompCreate(name=outer_name(name), ...)
   ! Creates "[A]" outer gridcomp
   ```

2. **Creates inner (user) GridComp**
   ```fortran
   user_gridcomp = ESMF_GridCompCreate(name=name, ...)
   ! Creates "A" inner gridcomp
   ```

3. **Attaches OuterMetaComponent to outer GridComp**
   ```fortran
   outer_meta = OuterMetaComponent(gridcomp, user_gc_driver, set_services, config)
   ! This stores:
   ! - gridcomp: the outer GridComp
   ! - user_gc_driver: wrapper for inner gridcomp
   ! - set_services: DSOSetServices object
   ! - config: merged HConfig (parent + child YAML)
   ```

4. **Initializes metadata**
   ```fortran
   call outer_meta%init_meta(_RC)
   ```

---

## Step 11: Child SetServices Execution

### File: `/Users/wdboggs/src/MAPL/superstructure/generic/OuterMetaComponent/SetServices.F90`
**Subroutine:** `run_children_setservices()` (lines 78-98)

Called from parent's `SetServices_()` after all children added.

```fortran
recursive subroutine run_children_setservices(this, rc)
  class(OuterMetaComponent), target, intent(inout) :: this
  integer, optional, intent(out) :: rc
  
  integer :: status, user_status
  type(GriddedComponentDriver), pointer :: child_comp
  type(ESMF_GridComp) :: child_outer_gc
  type(GriddedComponentDriverMapIterator) :: iter
  
  ! Iterate through all children
  associate ( e => this%children%ftn_end() )
    iter = this%children%ftn_begin()
    do while (iter /= e)
      call iter%next()
      child_comp => iter%second()
      child_outer_gc = child_comp%get_gridcomp()
      
      ! ===== Call ESMF_GridCompSetServices on child =====
      ! This triggers the child's GenericSetServices -> SetServices_
      call ESMF_GridCompSetServices(child_outer_gc, mapl_GenericSetServices, _USERRC)
    end do
  end associate
  
  _RETURN(ESMF_SUCCESS)
end subroutine run_children_setservices
```

### Cascading Effect:

1. Parent's `GenericSetServices()` called
2. Parent's `SetServices_()` executes
3. Parent parses YAML, creates children
4. Parent calls `run_children_setservices()`
5. For each child, calls `ESMF_GridCompSetServices(child, mapl_GenericSetServices)`
6. **This triggers child's GenericSetServices -> SetServices_**
7. Child's `SetServices_()` parses child's YAML
8. Child can create its own children (recursively)
9. Process continues depth-first

---

## Complete Call Chain Summary

```
GridCompCreate(parent_name, parent_setservices, parent_hconfig)
  ├─ Create ESMF_GridComp (outer)
  ├─ Create ESMF_GridComp (inner/user)
  ├─ Create OuterMetaComponent with hconfig=parent_hconfig
  └─ init_meta()

ESMF_GridCompSetServices(parent_outer_gc, GenericSetServices)
  └─ GenericSetServices(parent_outer_gc)
      └─ outer_meta%SetServices_()
          ├─ parse_component_spec(parent_hconfig)
          │   ├─ Navigate to 'mapl:' section
          │   └─ parse_children(mapl_cfg)
          │       ├─ Navigate to 'children:' section
          │       ├─ For each child name in YAML:
          │       │   ├─ parse_child(child_cfg)
          │       │   │   ├─ Read dso: "libconfigurable_gridcomp"
          │       │   │   ├─ Read setServices: "setservices_" (default)
          │       │   │   ├─ Load config_file: "A.yaml" → child_hconfig
          │       │   │   ├─ Create DSOSetServices("libconfigurable_gridcomp", "setservices_")
          │       │   │   ├─ parse_timespec(child_cfg) → timeStep, offset
          │       │   │   └─ Return ChildSpec {setservices, child_hconfig, timeStep, offset}
          │       │   └─ Insert into ChildSpecMap
          │       └─ Return ChildSpecMap
          ├─ Call user_setservices%run(user_gridcomp) [parent's SetServices]
          ├─ add_children(this)
          │   └─ For each child in component_spec%children:
          │       └─ this%add_child(child_name, child_spec)
          │           └─ outer_meta%add_child_by_spec(child_name, child_spec)
          │               ├─ merge_hconfig(parent_hconfig, child_hconfig)
          │               ├─ GridCompCreate(child_name, child_setservices, merged_hconfig)
          │               │   └─ Creates child OuterMetaComponent
          │               ├─ registry%add_subregistry(child_registry)
          │               └─ children%insert(child_name, child_driver)
          └─ run_children_setservices(this)
              └─ For each child in children map:
                  └─ ESMF_GridCompSetServices(child_outer_gc, GenericSetServices)
                      └─ GenericSetServices(child_outer_gc)
                          └─ [RECURSIVELY: child's SetServices_()]
                              ├─ parse_component_spec(child_hconfig)
                              ├─ Call child's user_setservices%run()
                              ├─ add_children(child) [if child has children]
                              └─ run_children_setservices(child) [if child has children]
```

---

## Step 12: DSO Loading and Actual SetServices Invocation

### File: `/Users/wdboggs/src/MAPL/superstructure/generic/UserSetServices.F90`
**Subroutine:** `run_DSOSetServices()` (lines 151-165)

When `child_spec%user_setservices%run(user_gridcomp, rc)` is called:

```fortran
subroutine run_DSOSetServices(this, gridcomp, rc)
  use mapl_DSO_Utilities_mod
  class(DSOSetservices), intent(in) :: this
  type(ESMF_GridComp) :: GridComp
  integer, intent(out) :: rc
  
  integer :: status, user_status
  logical :: found
  
  ! Verify DSO name is supported
  _ASSERT(is_supported_dso_name(this%sharedObj), &
      'unsupported dso name:: <'//this%sharedObj//'>')
  
  ! ===== ESMF loads DSO and calls user routine =====
  ! Calls: libconfigurable_gridcomp::setservices_()
  call ESMF_GridCompSetServices(gridcomp, &
      sharedObj=adjust_dso_name(this%sharedObj), &
      userRoutine=this%userRoutine,        ! "setservices_"
      userRoutinefound=found, _USERRC)
  
  _RETURN(_SUCCESS)
end subroutine run_DSOSetServices
```

This loads the DSO library and calls the SetServices routine defined in it.

---

## Data Flow Diagram

```
parent.yaml
    │
    ├─ Load as ESMF_HConfig
    │
    └─> OuterMetaComponent (parent)
        ├─ hconfig: <parent YAML>
        │
        └─ SetServices_()
            │
            ├─> parse_component_spec(hconfig)
            │   │
            │   └─> parse_children(mapl_cfg)
            │       │
            │       └─> For each child name:
            │           │
            │           └─> parse_child()
            │               │
            │               ├─ Read: dso
            │               ├─ Read: setServices (or default)
            │               ├─ Load: config_file
            │               │   │
            │               │   └─ A.yaml
            │               │       │
            │               │       └─> Load as child_hconfig
            │               │
            │               └─> Return ChildSpec
            │
            └─> add_children()
                │
                └─> For each child in ChildSpecMap:
                    │
                    ├─> merge_hconfig(parent, child)
                    │   │
                    │   └─> total_hconfig
                    │
                    └─> GridCompCreate(child_name, setservices, total_hconfig)
                        │
                        └─> OuterMetaComponent (child)
                            ├─ hconfig: <merged YAML>
                            │
                            └─ [Will recursively call SetServices_() later]
```

---

## Complete Execution Timeline

| Step | Component | Function | Location | Action |
|------|-----------|----------|----------|--------|
| 1 | Parent | GridCompCreate | GenericGridComp.F90:87 | Create outer/inner GridComps |
| 2 | Parent | new_outer_meta | OuterMetaComponent/new_outer_meta.F90:9 | Store HConfig in OuterMetaComponent |
| 3 | ESMF | GridCompSetServices | - | Call GenericSetServices on parent |
| 4 | Parent | GenericSetServices | GenericGridComp.F90:34 | Get OuterMetaComponent |
| 5 | Parent | SetServices_ | OuterMetaComponent/SetServices.F90:29 | Parse YAML and add children |
| 6 | Parser | parse_component_spec | ComponentSpecParser/parse_component_spec.F90:8 | Navigate to 'mapl:' section |
| 7 | Parser | parse_children | ComponentSpecParser/parse_children.F90:9 | Iterate 'children:' section |
| 8 | Parser | parse_child | ComponentSpecParser/parse_child.F90:8 | Read dso, config_file, etc. |
| 9 | Parser | user_setservices | UserSetServices.F90:135 | Create DSOSetServices object |
| 10 | Parent | add_children | OuterMetaComponent/SetServices.F90:54 | Loop through ChildSpecMap |
| 11 | Parent | add_child_by_spec | OuterMetaComponent/add_child_by_spec.F90:19 | Create child GridComp |
| 12 | Child | GridCompCreate | GenericGridComp.F90:87 | Create child outer/inner GridComps |
| 13 | Child | new_outer_meta | OuterMetaComponent/new_outer_meta.F90:9 | Store merged HConfig |
| 14 | Parent | run_children_setservices | OuterMetaComponent/SetServices.F90:78 | Call SetServices on children |
| 15 | Child | GenericSetServices | GenericGridComp.F90:34 | Repeat process for child |
| 16+ | Grandchild | ... | ... | Recursive if child has children |

---

## Key Data Structures

### ChildSpec

```fortran
type :: ChildSpec
  class(AbstractUserSetServices), allocatable :: user_setservices
  ! Either ProcSetServices or DSOSetServices
  
  type(ESMF_HConfig) :: hconfig
  ! Child's YAML config
  
  type(ESMF_TimeInterval), allocatable :: timeStep
  ! Optional: child's run timestep
  
  type(ESMF_TimeInterval) :: offset
  ! Optional: time offset relative to parent
end type ChildSpec
```

### DSOSetServices

```fortran
type, extends(AbstractUserSetServices) :: DSOSetServices
  character(:), allocatable :: sharedObj
  ! Library name: "libconfigurable_gridcomp", "libproto_stat_gc", etc.
  
  character(:), allocatable :: userRoutine
  ! Function name: "setservices_" (default) or custom name
end type DSOSetServices
```

### ChildSpecMap

```fortran
! GFortl 2 map data structure
type :: ChildSpecMap
  ! Key: child name (e.g., "A", "B", "collection_1")
  ! Value: ChildSpec
end type ChildSpecMap
```

### OuterMetaComponent

```fortran
type :: OuterMetaComponent
  type(ESMF_GridComp) :: self_gridcomp
  type(GriddedComponentDriver) :: user_gc_driver
  class(AbstractUserSetServices), allocatable :: user_setservices
  type(ESMF_HConfig) :: hconfig               ! <-- Parent/Child YAML
  type(GriddedComponentDriverMap) :: children  ! <-- Map of children
  type(ComponentSpec) :: component_spec       ! <-- Parsed spec
  ! ... other fields
end type OuterMetaComponent
```

---

## File Summary

| File | Location | Key Function/Subroutine | Lines | Purpose |
|------|----------|--------------------------|-------|---------|
| GenericGridComp.F90 | generic/ | create_grid_comp_primary, GenericSetServices | 87-142, 34-82 | Create GridComp and entry points |
| OuterMetaComponent.F90 | generic/OuterMetaComponent/ | Type definition | 34-133 | OuterMetaComponent type |
| new_outer_meta.F90 | generic/OuterMetaComponent/ | new_outer_meta | 9-29 | Constructor |
| SetServices.F90 | generic/OuterMetaComponent/ | SetServices_, add_children, run_children_setservices | 29-100 | Parse YAML and add children |
| add_child_by_spec.F90 | generic/OuterMetaComponent/ | add_child_by_spec | 19-55 | Create child GridComp |
| ComponentSpecParser.F90 | generic/ | Module interface | - | Parser declarations |
| parse_component_spec.F90 | generic/ComponentSpecParser/ | parse_component_spec | 8-33 | Parse 'mapl:' section |
| parse_children.F90 | generic/ComponentSpecParser/ | parse_children | 9-45 | Parse 'children:' section |
| parse_child.F90 | generic/ComponentSpecParser/ | parse_child | 8-70 | Parse individual child |
| parse_timespec.F90 | generic/ComponentSpecParser/ | parse_timespec | 9-22 | Parse timestep, offset |
| MAPL_Generic.F90 | generic/ | GridCompAddChild overloads | 155-550 | User-facing GridCompAddChild |
| UserSetServices.F90 | generic/ | new_DSOSetServices, run_DSOSetServices | 135-165 | DSO/user routine encapsulation |
| ChildSpec.F90 | generic/specs/ | ChildSpec type | 16-24 | Child specification type |

---

## References to YAML Sections

### Parent YAML Path Format

Option 1: Top-level children (simple)
```yaml
children:
  A:
    dso: ...
```

Option 2: Under 'mapl:' section (MAPL style)
```yaml
mapl:
  children:
    A:
      dso: ...
```

### Child YAML Path Format

```yaml
mapl:
  states:
    export: ...
    import: ...
  setServices:
    sharedObj: ...
    userRoutine: ...
```

---

## Example: Complete Trace for "parent.yaml" → Child "A"

### Input YAML Files

**parent.yaml:**
```yaml
grid:
  class: LatLon
  im_world: 12
  jm_world: 6

children:
  A:
    dso:  libconfigurable_gridcomp
    config_file: scenarios/precision_extension/A.yaml
  B:
    dso:  libconfigurable_gridcomp
    config_file: scenarios/precision_extension/B.yaml
```

**scenarios/precision_extension/A.yaml:**
```yaml
mapl:
  states:
    export:
      E_A1:
        standard_name: 'A1 standard name'
        units: 'barn'
        typekind: R4
```

### Execution Trace

1. **Load parent YAML**
   ```
   parent_hconfig = ESMF_HConfigCreate(filename="parent.yaml")
   ```

2. **Create parent GridComp**
   ```
   parent_gc = GridCompCreate("Parent", parent_setservices, parent_hconfig)
     → Creates OuterMetaComponent with hconfig=parent_hconfig
   ```

3. **Call parent SetServices**
   ```
   ESMF_GridCompSetServices(parent_gc, GenericSetServices)
     → Calls GenericSetServices(parent_gc)
       → Calls outer_meta%SetServices_()
   ```

4. **Parse parent YAML**
   ```
   component_spec = parse_component_spec(parent_hconfig)
     → Looks for "mapl:" section (not found in parent.yaml)
     → Returns empty component_spec
     → But: parse_children() looks for "children:" at top level
   ```

5. **Parse children section**
   ```
   children_cfg = ESMF_HConfigCreateAt(parent_hconfig, keyString="children")
   
   // Iterate: A, B
   child_name = "A"
   child_cfg = { dso: libconfigurable_gridcomp, config_file: ... }
   ```

6. **Parse child "A"**
   ```
   parse_child(child_cfg):
     dso_key = "dso"
     sharedObj = "libconfigurable_gridcomp"
     
     userProcedure_found = false
     userProcedure = "setservices_"  (default)
     
     has_config_file = true
     config_file = "scenarios/precision_extension/A.yaml"
     child_hconfig = ESMF_HConfigCreate(filename="scenarios/precision_extension/A.yaml")
     
     setservices = user_setservices("libconfigurable_gridcomp", "setservices_")
       → Creates DSOSetServices
     
     Return ChildSpec {
       user_setservices: DSOSetServices("libconfigurable_gridcomp", "setservices_"),
       hconfig: child_hconfig,
       timeStep: not allocated,
       offset: 0
     }
   ```

7. **Add child to ChildSpecMap**
   ```
   children%insert("A", child_spec)
   ```

8. **Add child "A" to parent**
   ```
   add_child_by_spec(parent, "A", child_spec):
     total_hconfig = merge_hconfig(parent_hconfig, child_hconfig)
     
     child_gc = GridCompCreate("A", setservices, total_hconfig)
       → Creates child OuterMetaComponent with:
           - hconfig = merged {grid: {...}, mapl: {states: {...}}}
           - user_setservices = DSOSetServices
       
       → Creates outer GridComp "[A]"
       → Creates inner GridComp "A"
     
     child_meta = get_outer_meta(child_gc)
     registry%add_subregistry(child_meta%get_registry())
     children%insert("A", GriddedComponentDriver(child_gc))
   ```

9. **Call SetServices on child**
   ```
   run_children_setservices(parent):
     For child in parent%children:
       child_gc = child_driver%get_gridcomp()
       ESMF_GridCompSetServices(child_gc, GenericSetServices)
         → Calls GenericSetServices(child_gc)
           → Calls child_outer_meta%SetServices_()
   ```

10. **Child SetServices execution**
    ```
    child%SetServices_():
      component_spec = parse_component_spec(child_hconfig)
        → Looks for "mapl:" section in child_hconfig
        → Found! Navigates to mapl section
        → parse_children(mapl_cfg) returns empty (no children in A.yaml)
        → parse_var_specs() returns export states: E_A1, E_A3, import: I_A2
      
      Call child%user_setservices%run(child_inner_gc)
        → Calls run_DSOSetServices()
          → ESMF_GridCompSetServices(child_inner_gc, 
                sharedObj="libconfigurable_gridcomp",
                userRoutine="setservices_")
            → Loads libconfigurable_gridcomp.so
            → Calls setservices_() function in DSO
            → This sets up child's advertise/realize methods, etc.
      
      add_children() returns early (no children)
      run_children_setservices() returns early (no children)
    ```

11. **Repeat for child "B"** (same process)

12. **Parent SetServices completes**
    ```
    add_children() iteration ends
    run_children_setservices() iteration ends
    SetServices_() returns _SUCCESS
    GenericSetServices() completes
    ```

13. **Result**
    ```
    parent OuterMetaComponent:
      - hconfig: parent_hconfig
      - children: {
          "A" -> child_A OuterMetaComponent,
          "B" -> child_B OuterMetaComponent
        }
      - component_spec with parsed connections
    
    child_A OuterMetaComponent:
      - hconfig: merged(parent, child_A)
      - children: {} (empty)
      - component_spec with exported states: E_A1, E_A3, import: I_A2
    
    child_B OuterMetaComponent:
      - Similar to child_A
    ```

---

## Key Points Summary

1. **YAML Loading**: ESMF_HConfig, not read by Python, but through ESMF Fortran API

2. **Two-Level Children Definition**:
   - Top-level `children:` (simple format)
   - Under `mapl:` section with `children:` (full format)

3. **DSO + SetServices Specification**:
   - `dso:` specifies the shared object library
   - `setServices:` (optional) specifies the routine name (default: `setservices_`)
   - DSO is loaded by ESMF when user's SetServices is called

4. **HConfig Merging**:
   - Parent's HConfig merged with child's HConfig
   - Allows parent's grid settings to apply to children

5. **Recursive Process**:
   - Each child's SetServices can create its own children
   - Depth-first tree traversal

6. **Data Flow**: YAML → HConfig → ComponentSpec → ChildSpecMap → GridComp

