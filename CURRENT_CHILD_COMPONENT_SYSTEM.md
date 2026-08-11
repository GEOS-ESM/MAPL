# CURRENT CHILD COMPONENT CREATION SYSTEM

## Overview

This document describes the current child component creation system based on GEOSgcm (old system using MAPL_AddChild) and MAPL3 (new system using YAML-based configuration).

---

## PART 1: GEOSgcm SYSTEM (Current - Using MAPL_AddChild)

### 1.1 Parent Component: GEOSphysics

**Location:** `/Users/wdboggs/src/GEOSgcm/src/Components/@GEOSgcm_GridComp/GEOSagcm_GridComp/GEOSphysics_GridComp/GEOS_PhysicsGridComp.F90`

**Child Creation Pattern:**
```fortran
! Lines 150-160: Direct MAPL_AddChild calls
GWD = MAPL_AddChild(GC, NAME='GWD', SS=GwdSetServices, RC=STATUS)
MOIST = MAPL_AddChild(GC, NAME='MOIST', SS=MoistSetServices, RC=STATUS)
TURBL = MAPL_AddChild(GC, NAME='TURBULENCE', SS=TurblSetServices, RC=STATUS)
CHEM = MAPL_AddChild(GC, NAME='CHEMISTRY', SS=AChemSetServices, RC=STATUS)
SURF = MAPL_AddChild(GC, NAME='SURFACE', SS=SurfSetServices, RC=STATUS)
RAD = MAPL_AddChild(GC, NAME='RADIATION', SS=RadiationSetServices, RC=STATUS)
```

**Key Features:**
- Hard-coded child creation in Fortran code
- SetServices subroutines imported as module procedures
- No YAML configuration files for parent specs
- Each child's SetServices routine name is: `SetServices`

**Module Imports (Lines 22-27):**
```fortran
use GEOS_SurfaceGridCompMod,    only : SurfSetServices      => SetServices
use GEOS_MoistGridCompMod,      only : MoistSetServices     => SetServices
use GEOS_TurbulenceGridCompMod, only : TurblSetServices     => SetServices
use GEOS_RadiationGridCompMod,  only : RadiationSetServices => SetServices
use GEOS_ChemGridCompMod,       only : AChemSetServices     => SetServices
use GEOS_GwdGridCompMod,        only : GwdSetServices       => SetServices
```

---

### 1.2 Child Example 1: Gravity Wave Drag (GWD)

**Location:** `/Users/wdboggs/src/GEOSgcm/src/Components/@GEOSgcm_GridComp/GEOSagcm_GridComp/GEOSphysics_GridComp/GEOSgwd_GridComp/GEOS_GwdGridComp.F90`

**SetServices Signature:**
```fortran
subroutine SetServices ( GC, RC )
    type(ESMF_GridComp), intent(INOUT) :: GC  ! gridded component
    integer, optional                  :: RC  ! return code
```

**Config File:** `/Users/wdboggs/src/GEOSgcm/src/Components/@GEOSgcm_GridComp/GEOSagcm_GridComp/GEOSphysics_GridComp/GEOSgwd_GridComp/GWD_GridComp.rc`
```
use_threads: .FALSE.
```

**Private State Structure (Lines 53-74):**
```fortran
type :: ThreadWorkspace
    type(GWBand)          :: beres_band
    type(BeresSourceDesc) :: beres_dc_desc
    type(GWBand)          :: oro_band
    type(GWBand)          :: rdg_band
end type ThreadWorkspace

type :: GEOS_GwdGridComp
    real :: GEOS_BGSTRESS
    real :: GEOS_EFFGWBKG
    real :: GEOS_EFFGWORO
    integer :: GEOS_PGWV
    real :: NCAR_EFFGWBKG
    real :: NCAR_EFFGWORO
    integer :: NCAR_NRDG
    real :: Z1
    real :: TAU1
    real :: H0
    real :: HH
    real, allocatable :: alpha(:) 
    type(ThreadWorkspace), allocatable :: workspaces(:)
end type GEOS_GwdGridComp
```

---

### 1.3 Child Example 2: GOCART (Dust Component System)

**Location:** `/Users/wdboggs/src/GEOSgcm/src/Components/@GEOSgcm_GridComp/GEOSagcm_GridComp/GEOSphysics_GridComp/@GEOSchem_GridComp/@GOCART/ESMF/GOCART2G_GridComp/GOCART2G_GridCompMod.F90`

**Parent-Child System:** GOCART contains dust (DU2G), sea salt (SS2G), sulfates (SU2G), and carbonaceous aerosols (CA2G)

**Child Creation Pattern (Lines 1310-1314):**
```fortran
call addChildren__ (gc, self%DU, setServices=DU2G_setServices, __RC__)
call addChildren__ (gc, self%SS, setServices=SS2G_setServices, __RC__)
call addChildren__ (gc, self%CA, setServices=CA2G_setServices, __RC__)
call addChildren__ (gc, self%SU, setServices=SU2G_setServices, __RC__)
call addChildren__ (gc, self%NI, setServices=NI2G_setServices, __RC__)
```

**Where SetServices are Imported (Lines 20-24):**
```fortran
use DU2G_GridCompMod,    only   : DU2G_setServices  => SetServices
use SS2G_GridCompMod,    only   : SS2G_setServices  => SetServices
use SU2G_GridCompMod,    only   : SU2G_setServices  => SetServices
use CA2G_GridCompMod,    only   : CA2G_setServices  => SetServices
use NI2G_GridCompMod,    only   : NI2G_setServices  => SetServices
```

**Embedded Child Creation (Line 1335):**
```fortran
do i = 1, n
    species%instances(i)%id = MAPL_AddChild(gc, name=species%instances(i)%name, SS=SetServices, __RC__)
end do
```

**Child State Structure - Dust (DU2G_GridCompMod.F90, Lines 54-75):**
```fortran
type, extends(GA_Environment) :: DU2G_GridComp
    real, allocatable      :: rlow(:)        ! particle effective radius lower bound [um]
    real, allocatable      :: rup(:)         ! particle effective radius upper bound [um]
    real, allocatable      :: sfrac(:)       ! fraction of total source
    real, allocatable      :: sdist(:)       ! FENGSHA aerosol fractional size distribution [1]
    real                   :: alpha          ! FENGSHA scaling factor
    real                   :: gamma          ! FENGSHA tuning exponent
    real                   :: kvhmax         ! FENGSHA max. vertical/horizontal mass flux ratio [1]
    real                   :: Ch_DU_res(NHRES) ! resolutions used for Ch_DU
    real                   :: Ch_DU          ! dust emission tuning coefficient [kg s2 m-5]
    logical                :: maringFlag=.false.  ! maring settling velocity correction
    integer                :: day_save = -1
    character(len=:), allocatable :: emission_scheme     ! emission scheme selector
    integer       :: clayFlag       ! clay and silt term in K14
    real          :: f_swc          ! soil mosture scaling factor
    real          :: f_scl          ! clay content scaling factor
    real          :: uts_gamma      ! threshold friction velocity parameter 'gamma'
    logical                :: doing_point_emissions = .false.
    character(len=255)     :: point_emissions_srcfilen   ! filename for pointwise emissions
    type(ThreadWorkspace), allocatable :: workspaces(:)
end type DU2G_GridComp
```

**Dust SetServices Signature (DU2G_GridCompMod.F90, Lines 89-93):**
```fortran
subroutine SetServices (GC, RC)
    type (ESMF_GridComp), intent(INOUT)   :: GC  ! gridded component
    integer,              intent(  OUT)   :: RC  ! return code
```

**Dust Config File:** `DU2G_instance_<COMP_NAME>.rc` or default `DU2G_instance_DU.rc`

---

## PART 2: MAPL3 SYSTEM (New - YAML-Based Configuration)

### 2.1 Parent YAML Configuration

**Simple Parent Example:**
**Location:** `/Users/wdboggs/src/MAPL/superstructure/generic/tests/scenarios/parent.yaml`

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

**Parent with Geometry and SetServices:**
**Location:** `/Users/wdboggs/src/MAPL/superstructure/generic/tests/scenarios/3d_specs/parent.yaml`

```yaml
mapl:
  geometry:
    esmf_geom:
      class: latlon
      im_world: 12
      jm_world: 13
      pole: PC
      dateline: DC
    vertical_grid:
      class: basic
      num_levels: 5

  children:
    A:
      dso: libconfigurable_gridcomp
      config_file: scenarios/3d_specs/A.yaml
    B:
      dso: libconfigurable_gridcomp
      config_file: scenarios/3d_specs/B.yaml

  states: {}

  connections:
    - src_name: E_A1
      dst_name: I_B1
      src_comp: A
      dst_comp: B
    - src_name: E_A3
      dst_name: I_B3
      src_comp: A
      dst_comp: B
```

**Parent with SetServices Override:**
**Location:** `/Users/wdboggs/src/MAPL/superstructure/generic/tests/scenarios/statistics/root.yaml`

```yaml
mapl:
  children:
    A:
      dso: libconfigurable_gridcomp
      config_file: scenarios/statistics/A.yaml

  states:
    import: {}
    export: {}

  setServices:
    sharedObj: libconfigurable_gridcomp
    userRoutine: setservices_
```

---

### 2.2 Child YAML Configuration

**Child with Geometry and State Variables:**
**Location:** `/Users/wdboggs/src/MAPL/superstructure/generic/tests/scenarios/vertical_alignment_with_grid/A.yaml`

```yaml
mapl:

  geometry:
    esmf_geom:
      class: latlon
      im_world: 12
      jm_world: 13
      pole: PC
      dateline: DC
    vertical_grid:
      class: fixed_levels
      levels: [30, 20, 10]
      units: hPa
      physical_dimension: pressure
      coordinate_direction: downward

  states:
    import: {}
    export:
      E_A:
        standard_name: E_A
        units: m
        fill_value: 15.
        vertical_dim_spec: center
        vertical_alignment: with_grid

  setServices:
    sharedObj: libconfigurable_gridcomp
    userRoutine: setservices_
```

**Simple Child Configuration:**
**Location:** `/Users/wdboggs/src/MAPL/superstructure/generic/tests/scenarios/statistics/A.yaml`

```yaml
mapl:
  states:
    import: {}
    export:
      T:
        standard_name: 'Temperature'
        units: 'K'
        vertical_dim_spec: NONE

  setServices:
    sharedObj: libconfigurable_gridcomp
    userRoutine: setservices_
```

**Child with 3D State Variables:**
**Location:** `/Users/wdboggs/src/MAPL/superstructure/generic/tests/scenarios/3d_specs/A.yaml`

```yaml
mapl:
  states:
    export:
      E_A1:
        standard_name: 'A1 standard name'
        units: 'barn'
        typekind: R4
        fill_value: 1.
        vertical_dim_spec: NONE
      E_A3:
        standard_name: 'A3 standard name'
        units: 'barn'
        typekind: R4
        fill_value: 7.
        vertical_dim_spec: NONE
    import:
      I_A2:
        standard_name: 'B2 standard name'
        units: 'barn'
        typekind: R4
        fill_value: 3.
        vertical_dim_spec: 'vertical_dim_center'

  setServices:
    sharedObj: libconfigurable_gridcomp
    userRoutine: setservices_
```

---

### 2.3 YAML Parsing Code (MAPL3)

**Main SetServices Entry Point:**
**Location:** `/Users/wdboggs/src/MAPL/superstructure/generic/OuterMetaComponent/SetServices.F90`

```fortran
recursive module subroutine SetServices_(this, rc)
   class(OuterMetaComponent), target, intent(inout) :: this
   integer, intent(out) :: rc

   ! Parse component spec from YAML
   this%component_spec = parse_component_spec(this%hconfig, this%registry, this%user_gc_driver%get_name(), _RC)

   user_gridcomp = this%user_gc_driver%get_gridcomp()
   call attach_inner_meta(user_gridcomp, this%self_gridcomp, _RC)
   logger => this%get_logger()
   call logger%info("SetServices:: starting...", _RC)
   call this%user_setservices%run(user_gridcomp, _RC)  ! Call user's SetServices
   call logger%info("SetServices:: ...completed", _RC)
   call add_children(this, _RC)  ! Add children from YAML spec
   call run_children_setservices(this, _RC)  ! Call SetServices on children
```

**Add Children Subroutine (Lines 54-74):**
```fortran
recursive subroutine add_children(this, rc)
   class(OuterMetaComponent), target, intent(inout) :: this
   integer, optional, intent(out) :: rc
   
   integer :: status
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

   _RETURN(_SUCCESS)
end subroutine add_children
```

**Run Children SetServices (Lines 78-98):**
```fortran
recursive subroutine run_children_setservices(this, rc)
   class(OuterMetaComponent), target, intent(inout) :: this
   integer, optional, intent(out) :: rc

   integer :: status, user_status
   type(GriddedComponentDriver), pointer :: child_comp
   type(ESMF_GridComp) :: child_outer_gc
   type(GriddedComponentDriverMapIterator) :: iter

   associate ( e => this%children%ftn_end() )
      iter = this%children%ftn_begin()
      do while (iter /= e)
         call iter%next()
         child_comp => iter%second()
         child_outer_gc = child_comp%get_gridcomp()
         call ESMF_GridCompSetServices(child_outer_gc, mapl_GenericSetServices, _USERRC)
      end do
   end associate

   _RETURN(ESMF_SUCCESS)
end subroutine run_children_setservices
```

---

### 2.4 Children Parsing (YAML -> ChildSpec)

**Parse Children Function:**
**Location:** `/Users/wdboggs/src/MAPL/superstructure/generic/ComponentSpecParser/parse_children.F90`

```fortran
module function parse_children(hconfig, rc) result(children)
   type(ChildSpecMap) :: children
   type(ESMF_HConfig), intent(in) :: hconfig
   integer, optional, intent(out) :: rc

   integer :: status
   logical :: has_children
   type(ESMF_HConfig) :: children_cfg, child_cfg
   type(ESMF_HConfigIter) :: iter, iter_begin, iter_end
   type(ChildSpec) :: child_spec
   character(:), allocatable :: child_name

   ! Check if "children" section exists
   has_children = ESMF_HConfigIsDefined(hconfig, keyString=COMPONENT_CHILDREN_SECTION, _RC)
   _RETURN_UNLESS(has_children)

   ! Get the children section
   children_cfg = ESMF_HConfigCreateAt(hconfig, keyString=COMPONENT_CHILDREN_SECTION, _RC)
   is_map = ESMF_HConfigIsMap(children_cfg, _RC)

   _ASSERT(is_map, 'children spec must be mapping')

   ! Iterate through each child definition
   iter_begin = ESMF_HConfigIterBegin(children_cfg, _RC)
   iter_end = ESMF_HConfigIterEnd(children_cfg, _RC)
   iter = iter_begin
   do while (ESMF_HConfigIterLoop(iter, iter_begin, iter_end))
      child_name = ESMF_HConfigAsStringMapKey(iter, _RC)
      child_cfg = ESMF_HConfigCreateAtMapVal(iter, _RC)
      child_spec = parse_child(child_cfg, _RC)
      call children%insert(child_name, child_spec)
      call ESMF_HConfigDestroy(child_cfg, _RC)
   end do

   call ESMF_HConfigDestroy(children_cfg, _RC)

   _RETURN(_SUCCESS)
end function parse_children
```

**Parse Single Child:**
**Location:** `/Users/wdboggs/src/MAPL/superstructure/generic/ComponentSpecParser/parse_child.F90`

```fortran
module function parse_child(hconfig, rc) result(child)
   type(ChildSpec) :: child
   type(ESMF_HConfig), intent(in) :: hconfig
   integer, optional, intent(out) :: rc

   integer :: status
   class(AbstractUserSetServices), allocatable :: setservices
   character(:), allocatable :: dso_key, userProcedure_key, try_key
   logical :: dso_found, userProcedure_found, has_key, has_config_file
   type(ESMF_HConfig), allocatable :: child_hconfig
   character(:), allocatable :: sharedObj, userProcedure, config_file
   type(ESMF_TimeInterval), allocatable :: offset, timeStep

   ! Look for DSO specification (tries multiple key names)
   dso_found = .false.
   character(*), parameter :: dso_keys(*) = [character(len=9) :: 'dso', 'DSO', 'sharedObj', 'sharedobj']
   do i = 1, size(dso_keys)
      has_key = ESMF_HConfigIsDefined(hconfig, keyString=dso_keys(i), _RC)
      if (has_key) then
         _ASSERT(.not. dso_found, 'multiple dso specifications in hconfig for child')
         dso_found = .true.
         dso_key = dso_keys(i)
      end if
   end do
   _ASSERT(dso_found, 'Must specify a dso for hconfig of child')
   sharedObj = ESMF_HConfigAsString(hconfig, keyString=dso_key, _RC)

   ! Look for SetServices routine name (tries multiple key names)
   userProcedure_found = .false.
   character(*), parameter :: userProcedure_keys(*) = [character(len=10) :: 'SetServices', 'setServices', 'setservices']
   do i = 1, size(userProcedure_keys)
      if (ESMF_HConfigIsDefined(hconfig, keyString=userProcedure_keys(i))) then
         _ASSERT(.not. userProcedure_found, 'multiple SetServices specifications')
         userProcedure_found = .true.
         userProcedure_key = userProcedure_keys(i)
      end if
   end do
   userProcedure = 'setservices_'         ! Default
   if (userProcedure_found) then
      userProcedure = ESMF_HConfigAsString(hconfig, keyString=userProcedure_key,_RC)
   end if

   ! Load config file if specified
   has_config_file = ESMF_HConfigIsDefined(hconfig, keyString='config_file', _RC)
   if (has_config_file) then
      config_file = ESMF_HConfigAsString(hconfig, keyString='config_file',_RC)
      child_hconfig = ESMF_HConfigCreate(filename=config_file,_RC)
   end if

   ! Create SetServices handler
   setservices = user_setservices(sharedObj, userProcedure)

   ! Parse timing specs
   call parse_timespec(hconfig, timeStep, offset, _RC)

   ! Create ChildSpec
   child = ChildSpec(setservices, hconfig=child_hconfig, timeStep=timeStep, offset=offset)

   _RETURN(_SUCCESS)
end function parse_child
```

---

### 2.5 ChildSpec Data Structure

**Location:** `/Users/wdboggs/src/MAPL/superstructure/generic/specs/ChildSpec.F90`

```fortran
type :: ChildSpec
   class(AbstractUserSetServices), allocatable :: user_setservices
   type(ESMF_HConfig) :: hconfig
   type(ESMF_TimeInterval), allocatable :: timeStep
   type(ESMF_TimeInterval) :: offset
end type ChildSpec
```

---

### 2.6 UserSetServices: SetServices Handler

**Location:** `/Users/wdboggs/src/MAPL/superstructure/generic/UserSetServices.F90`

**Abstract Base Class (Lines 30-35):**
```fortran
type, abstract :: AbstractUserSetServices
contains
   procedure(I_RunSetServices), deferred :: run
   procedure(I_write_formatted), deferred :: write_formatted
   generic :: write(formatted) => write_formatted
end type AbstractUserSetServices
```

**DSOSetServices (Lines 71-77):**
```fortran
type, extends(AbstractUserSetServices) :: DSOSetServices
   character(:), allocatable :: sharedObj    ! ESMF naming convention
   character(:), allocatable :: userRoutine  ! ESMF naming convention
contains
   procedure :: run => run_DSOSetServices
   procedure :: write_formatted => write_formatted_dso
end type DSOSetServices
```

**Factory Function (Lines 135-149):**
```fortran
function new_DSOSetServices(sharedObj, userRoutine) result(dso_setservices)
   type(DSOSetServices) :: dso_setservices
   character(len=*), intent(in) :: sharedObj
   character(len=*), optional, intent(in) :: userRoutine

   character(:), allocatable :: userRoutine_

   userRoutine_ = 'setservices_' ! default
   if (present(userRoutine)) userRoutine_ = userRoutine
      
   dso_setservices%sharedObj   = sharedObj
   dso_setservices%userRoutine = userRoutine_

end function new_DSOSetServices
```

**Parse SetServices Routine:**
**Location:** `/Users/wdboggs/src/MAPL/superstructure/generic/ComponentSpecParser/parse_setservices.F90`

```fortran
module function parse_setservices(config, rc) result(user_ss)
   type(DSOSetServices) :: user_ss
   type(ESMF_HConfig), target, intent(in) :: config
   integer, optional, intent(out) :: rc

   character(:), allocatable :: sharedObj, userRoutine
   integer :: status

   sharedObj = ESMF_HConfigAsString(config,keyString='sharedObj',rc=status)
   _ASSERT(status == 0, 'setServices spec does not specify sharedObj')

   if (ESMF_HConfigIsDefined(config,keyString='userRoutine')) then
      userRoutine = ESMF_HConfigAsString(config,keyString='userRoutine',_RC)
   else
      userRoutine = 'setservices_'  ! Default
   end if

   user_ss = user_setservices(sharedObj, userRoutine)
   
   _RETURN(_SUCCESS)
end function parse_setservices
```

---

## PART 3: Key Differences Summary

### GEOSgcm (Old System)
| Aspect | Implementation |
|--------|-----------------|
| **Child Specification** | Hard-coded Fortran (MAPL_AddChild calls) |
| **SetServices Location** | Imported as module procedures |
| **SetServices Signature** | Fixed: `subroutine SetServices(GC, RC)` |
| **Configuration** | Component-specific .rc files only |
| **Parent Config** | No parent YAML file |
| **Child Config** | Individual .rc files per component |
| **DSO Loading** | Implicit via Fortran modules |
| **Default Routine Name** | Hardcoded in child module |

### MAPL3 (New System)
| Aspect | Implementation |
|--------|-----------------|
| **Child Specification** | YAML-based hierarchical config |
| **SetServices Location** | DSO name specified in YAML |
| **SetServices Signature** | Dynamically loaded from shared object |
| **Configuration** | Single YAML file per component level |
| **Parent Config** | YAML with children section |
| **Child Config** | Separate YAML file specified in parent |
| **DSO Loading** | Dynamic via ESMF_HConfig mechanism |
| **Default Routine Name** | 'setservices_' (configurable) |

---

## PART 4: Child Component YAML Format Reference

### Parent YAML Structure (Top-Level)
```yaml
[# Optional grid specification]
grid:
  class: LatLon
  im_world: <integer>
  jm_world: <integer>
  [pole: <string>]
  [dateline: <string>]

[# Optional within mapl: section]
mapl:
  geometry:
    esmf_geom:
      class: latlon
      im_world: <integer>
      jm_world: <integer>
      [pole: <string>]
      [dateline: <string>]
    [vertical_grid:
      class: <string>
      ...]
  
  children:
    <child_name>:
      dso: <library_name>
      [setServices: <routine_name>]  # Optional, default: setservices_
      [config_file: <path/to/child.yaml>]
      [timestep: <time_string>]
      [run_time_offset: <time_string>]
    ...
  
  states:
    import: {}
    export: {}
    [internal: {}]
  
  [setServices:
    sharedObj: <library_name>
    [userRoutine: <routine_name>]]
  
  [connections:
    - src_name: <field_name>
      dst_name: <field_name>
      src_comp: <child_name>
      dst_comp: <child_name>
    ...]
```

### Child YAML Structure
```yaml
mapl:
  [geometry:
    esmf_geom:
      class: latlon
      im_world: <integer>
      jm_world: <integer>
      [pole: <string>]
      [dateline: <string>]
    [vertical_grid:
      class: <string>
      ...]]
  
  states:
    import:
      <field_name>:
        standard_name: <string>
        units: <string>
        [typekind: <string>]  # R4, R8, etc.
        [fill_value: <number>]
        [vertical_dim_spec: <string>]
    export:
      <field_name>:
        [same as import]
    [internal:
      [same as import]]
  
  setServices:
    sharedObj: <library_name>
    [userRoutine: <routine_name>]
  
  [connections: ...]
```

---

## PART 5: File Paths and Line References

### GEOSgcm Locations
- Parent SetServices: `/Users/wdboggs/src/GEOSgcm/src/Components/@GEOSgcm_GridComp/GEOSagcm_GridComp/GEOSphysics_GridComp/GEOS_PhysicsGridComp.F90` (lines 150-160)
- GWD Child: `/Users/wdboggs/src/GEOSgcm/src/Components/@GEOSgcm_GridComp/GEOSagcm_GridComp/GEOSphysics_GridComp/GEOSgwd_GridComp/GEOS_GwdGridComp.F90` (lines 88-100)
- GWD Config: `/Users/wdboggs/src/GEOSgcm/src/Components/@GEOSgcm_GridComp/GEOSagcm_GridComp/GEOSphysics_GridComp/GEOSgwd_GridComp/GWD_GridComp.rc`
- GOCART Parent: `/Users/wdboggs/src/GEOSgcm/src/Components/@GEOSgcm_GridComp/GEOSagcm_GridComp/GEOSphysics_GridComp/@GEOSchem_GridComp/@GOCART/ESMF/GOCART2G_GridComp/GOCART2G_GridCompMod.F90` (lines 1310-1314)
- Dust Child: `/Users/wdboggs/src/GEOSgcm/src/Components/@GEOSgcm_GridComp/GEOSagcm_GridComp/GEOSphysics_GridComp/@GEOSchem_GridComp/@GOCART/ESMF/GOCART2G_GridComp/DU2G_GridComp/DU2G_GridCompMod.F90` (lines 54-75)

### MAPL3 Locations
- SetServices Entry: `/Users/wdboggs/src/MAPL/superstructure/generic/OuterMetaComponent/SetServices.F90` (lines 29-100)
- Parse Children: `/Users/wdboggs/src/MAPL/superstructure/generic/ComponentSpecParser/parse_children.F90` (lines 9-45)
- Parse Child: `/Users/wdboggs/src/MAPL/superstructure/generic/ComponentSpecParser/parse_child.F90` (lines 8-70)
- ChildSpec Type: `/Users/wdboggs/src/MAPL/superstructure/generic/specs/ChildSpec.F90` (lines 16-24)
- UserSetServices: `/Users/wdboggs/src/MAPL/superstructure/generic/UserSetServices.F90` (lines 71-77, 135-149)
- Parse SetServices: `/Users/wdboggs/src/MAPL/superstructure/generic/ComponentSpecParser/parse_setservices.F90` (lines 10-30)

### Example YAML Files
- Simple Parent: `/Users/wdboggs/src/MAPL/superstructure/generic/tests/scenarios/parent.yaml`
- Simple Child: `/Users/wdboggs/src/MAPL/superstructure/generic/tests/scenarios/statistics/A.yaml`
- Parent with Geometry: `/Users/wdboggs/src/MAPL/superstructure/generic/tests/scenarios/3d_specs/parent.yaml`
- Child with Geometry: `/Users/wdboggs/src/MAPL/superstructure/generic/tests/scenarios/vertical_alignment_with_grid/A.yaml`
- Parent with SetServices: `/Users/wdboggs/src/MAPL/superstructure/generic/tests/scenarios/statistics/root.yaml`

