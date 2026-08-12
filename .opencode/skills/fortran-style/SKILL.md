---
name: fortran-style
description: MAPL Fortran coding standards and style conventions
compatibility: opencode
---

## What I Do

Provide comprehensive Fortran coding standards for MAPL development, covering:
- Naming conventions for all program entities
- Code structure and organization
- Indentation and formatting rules
- Modern Fortran practices
- Required and prohibited patterns

## When to Use Me

Use this skill when:
- Writing new Fortran code for MAPL
- Reviewing code for style compliance
- Refactoring existing code
- Learning MAPL conventions as a new contributor
- Resolving style debates in code reviews

## Naming Conventions

### Procedure Names

**Style:** snake_case, start with verb

**Examples:**
```fortran
subroutine get_next_token()
function compute_total_mass()
subroutine initialize_grid()
function check_bounds()
```

**Rationale:** Verbs indicate action, snake_case improves readability

### Variable Names

**Style:** snake_case, nouns (usually singular)

**Scope matters:**
- **Module variables:** Most important, use full descriptive names
- **Dummy arguments:** Intermediate importance, clear but can be shorter
- **Local variables:** Least crucial, but still descriptive

**Examples:**
```fortran
real :: density_of_air
integer :: number_of_bins  ! Or abbreviated: n_bins
real :: universal_gravitational_constant
type(ESMF_Field) :: humidity  ! Singular for fields
```

**Arrays/containers:** Use plural nouns
```fortran
integer, allocatable :: items(:)
real :: temperatures(:,:)
character(len=32) :: variable_names(:)
```

**Loop indices:** Single character
```fortran
do i = 1, n_rows
   do j = 1, n_cols
      matrix(i,j) = 0.0
   end do
end do
```

**Abbreviations:**
- Avoid unless exceptionally clear and consistent
- Exception: `n_` prefix for "number of" is acceptable
  - `n_bins` acceptable for `number_of_bins`
  - `n_levels` acceptable for `number_of_levels`

**Long names:** Use ASSOCIATE for aliases in formulae
```fortran
associate (G => universal_gravitational_constant)
   f = G * m1 * m2 / r**2
end associate
```

### Constants and Macros

**Style:** ALL_CAPS with underscores

**Examples:**
```fortran
integer, parameter :: MAX_STRING_LENGTH = 256
real, parameter :: PI = 3.141592653589793
integer, parameter :: UNDEFINED_VALUE = -999

! FPP/CPP macros
#define BUFFER_SIZE 1024
#define ERROR_CODE -1
```

### Derived Type Names

**Style:** CamelCase, singular nouns

**Examples:**
```fortran
type :: IdentityRegridder
type :: GridFactory
type :: ComponentDriver
type :: StateManager
```

**Rationale:** Distinguishes types from variables, follows OOP conventions

### Module Names

**Style:** CamelCase with package prefix

**Prefix:** Use package indicator (e.g., `MAPL_`, `pf_`, `FTL_`)

**Suffix:** Generally none (exception: top-level package module)

**Examples:**
```fortran
module MAPL_HistoryGridComp
module MAPL_AbstractRegridder
module pf_DirectoryService
module FTL_HashMap
```

**Module/Type correspondence:**
When module contains single public derived type, module name corresponds to type name:
```fortran
! Module: pf_DirectoryService (with package prefix)
! Type: DirectoryService (without package prefix)
module pf_DirectoryService
   type :: DirectoryService
      ! ...
   end type DirectoryService
end module pf_DirectoryService
```

**Top-level package module:**
Module name is just package name (no suffix), contains only USE statements:
```fortran
module MAPL
   use MAPL_BaseMod
   use MAPL_HistoryGridComp
   ! ...
end module MAPL
```

### File Names

**Style:** Match contained program unit name

**Pattern:** Module name WITHOUT package prefix + `.F90`

**Examples:**
```fortran
! Module: MAPL_HistoryGridComp
! File: HistoryGridComp.F90

! Module: pf_DirectoryService  
! File: DirectoryService.F90

! Module: MAPL_AbstractGridFactory
! File: AbstractGridFactory.F90
```

**File suffix:** `.F90` (capital F for FPP/CPP preprocessing, free-form)

**Ideal:** One program unit per file (subroutine, function, module, or program)

## Code Structure

### Module Organization

```fortran
module MAPL_MyModule
   use ESMF
   use MAPL_BaseMod
   use pFlogger, only: logging => Logger
   implicit none
   private
   
   ! Public declarations immediately after private
   public :: MyType
   public :: my_procedure
   public :: SPECIAL_CONSTANT
   
   ! Constants
   integer, parameter :: BUFFER_SIZE = 1024
   
   ! Types
   type :: MyType
      private  ! Data components private by default
      integer :: internal_counter
      real :: state_value
   contains
      procedure :: compute_result  ! Type-bound procedures public
      procedure :: initialize
   end type MyType
   
contains

   ! Procedure implementations
   subroutine my_procedure()
      ! ...
   end subroutine my_procedure
   
end module MAPL_MyModule
```

### Key Structure Rules

**1. implicit none - REQUIRED**
```fortran
module MyModule
   implicit none  ! MANDATORY
   ! ...
end module
```

**2. Default private**
```fortran
module MyModule
   implicit none
   private  ! Everything private by default
   
   public :: OnlyThisIsPublic
   public :: AndThis
```

**3. Derived type visibility**
```fortran
type :: MyType
   private  ! Data components private by default
   integer :: internal_data
   real :: private_state
contains
   procedure :: public_method  ! Type-bound procedures generally public
end type
```

Public overrides for data components allowed but rare:
```fortran
type :: SpecialType
   private
   integer, public :: public_counter  ! Explicit public override
   real :: private_value
end type
```

## Indentation

**Standard:** 3 spaces for block constructs and module/type contents

**Rationale:** Works well for Fortran where `if` and `do` are 2 characters + space

**Examples:**
```fortran
do i = 1, n_items
   x = elements(i)
   if (x < 0) then
      call handle_negative_value()
   end if
end do

type :: MyType
   private
   integer :: counter
   real :: value
contains
   procedure :: compute
end type MyType

subroutine process_data()
   integer :: i
   
   do i = 1, n_items
      if (valid(i)) then
         call process_item(i)
      else
         call skip_item(i)
      end if
   end do
end subroutine process_data
```

## Modern Fortran Practices

### Use Modern Operators

**Required:** Use symbolic operators, not old-style

**Comparison operators:**
```fortran
! CORRECT
if (x > y) then
if (a <= b) then
if (value == target) then
if (state /= INVALID) then

! WRONG - do not use
if (x .gt. y) then
if (a .le. b) then
if (value .eq. target) then
if (state .ne. INVALID) then
```

**Logical operators:**
```fortran
! For logical operands, use equivalence operators
logical :: flag1, flag2

if (flag1 .eqv. flag2) then   ! CORRECT for logical
if (flag1 .neqv. flag2) then  ! CORRECT for logical

! DO NOT use == or /= for logical operands (not standard conforming)
if (flag1 == flag2) then   ! WRONG for logical
if (flag1 /= flag2) then   ! WRONG for logical
```

### Array Constructors

**Use:** `[ ... ]` (modern syntax)
**Avoid:** `(/ ... /)` (old syntax)

```fortran
! CORRECT
integer :: values(3)
values = [1, 2, 3]
names = ['alpha', 'beta ', 'gamma']

! WRONG
values = (/ 1, 2, 3 /)
```

**Rationale:** Saves characters, easier to read, familiar from other languages

### Fortran Keywords

**Style:** lowercase

```fortran
! CORRECT
do i = 1, n
   if (condition) then
      integer :: local_var
   end if
end do
module MyModule
contains
   subroutine my_proc()
   
! WRONG
DO i = 1, n
   IF (condition) THEN
END DO
MODULE MyModule
```

## Error Handling

### Procedures with Error Conditions

**Rule:** Use subroutines (not functions) for procedures that can fail

```fortran
! CORRECT - subroutine with optional rc
subroutine read_configuration(filename, config, rc)
   character(len=*), intent(in) :: filename
   type(Config), intent(out) :: config
   integer, optional, intent(out) :: rc
   ! ... can return error codes
end subroutine

! Exceptions: Some procedures best expressed as functions
! Discuss with team if function requires error handling
```

**Structure constructors:** Must never have error conditions (implemented as functions)

**Rationale:** Polymorphism with implicit allocation can fail but can't return error code from function

**See `mapl-error-handling` skill** for comprehensive error handling patterns and macros.

## Prohibited Patterns

### Implicit Typing

**NEVER allow implicit typing**
```fortran
module MyModule
   implicit none  ! REQUIRED
```

### Public by Default

**AVOID public by default**
```fortran
! WRONG
module MyModule
   implicit none
   ! Everything public - dangerous!
   
! CORRECT  
module MyModule
   implicit none
   private  ! Safe default
   public :: IntendedPublicAPI
```

### Legacy Comparison Operators

**DO NOT USE:** `.gt.`, `.lt.`, `.ge.`, `.le.`, `.eq.`, `.ne.`

Use modern equivalents: `>`, `<`, `>=`, `<=`, `==`, `/=`

### Old Array Constructor Syntax

**DO NOT USE:** `(/ ... /)`

Use modern syntax: `[ ... ]`

## Recommended Patterns

### ASSOCIATE for Long Names

```fortran
associate (G => universal_gravitational_constant, &
           M => earth_mass, &
           R => earth_radius)
   potential = -G * M / R
end associate
```

### Explicit Intent

```fortran
subroutine process(input, output, status)
   integer, intent(in) :: input
   integer, intent(out) :: output
   integer, intent(inout) :: status  ! When appropriate
```

### Allocatable vs Pointer

Prefer allocatable over pointer when possible:
```fortran
! PREFERRED
integer, allocatable :: data(:)

! Only if pointer semantics truly needed
integer, pointer :: data(:)
```

## Complete Example

```fortran
module MAPL_ExampleModule
   use ESMF
   use MAPL_BaseMod
   implicit none
   private
   
   public :: ExampleType
   public :: create_example
   public :: MAX_ITERATIONS
   
   integer, parameter :: MAX_ITERATIONS = 100
   integer, parameter :: BUFFER_SIZE = 1024
   
   type :: ExampleType
      private
      integer :: iteration_count
      real :: convergence_tolerance
      integer, allocatable :: data_values(:)
   contains
      procedure :: initialize
      procedure :: compute_result
      procedure :: check_convergence
   end type ExampleType
   
contains

   function create_example(n_values, tolerance, rc) result(example)
      integer, intent(in) :: n_values
      real, intent(in) :: tolerance
      integer, optional, intent(out) :: rc
      type(ExampleType) :: example
      
      integer :: status
      
      allocate(example%data_values(n_values), stat=status)
      if (status /= 0) then
         if (present(rc)) rc = status
         return
      end if
      
      example%iteration_count = 0
      example%convergence_tolerance = tolerance
      
      if (present(rc)) rc = 0
   end function create_example
   
   subroutine initialize(this, initial_value)
      class(ExampleType), intent(inout) :: this
      real, intent(in) :: initial_value
      
      integer :: i
      
      do i = 1, size(this%data_values)
         this%data_values(i) = initial_value
      end do
      
      this%iteration_count = 0
   end subroutine initialize
   
end module MAPL_ExampleModule
```

## Code Review Checklist

When reviewing code for style compliance:

- [ ] All modules have `implicit none`
- [ ] Module entities default private with explicit public
- [ ] Procedures use snake_case with verb prefixes
- [ ] Variables use snake_case, meaningful names
- [ ] Types use CamelCase
- [ ] Constants/macros use ALL_CAPS
- [ ] Modern operators (`>`, `==`) not old style (`.gt.`, `.eq.`)
- [ ] Modern array constructors `[ ]` not `(/ /)`
- [ ] Fortran keywords in lowercase
- [ ] Indentation is 3 spaces
- [ ] Error-prone procedures are subroutines with optional rc
- [ ] File name matches module name (without package prefix)

## Summary

**Core Principles:**
1. Explicit over implicit (implicit none, default private)
2. Descriptive names over abbreviations
3. Modern syntax over legacy patterns
4. Consistency across codebase
5. Clear intent and error handling

**When in doubt:** Look at existing MAPL code for examples, ask in code review

## Related Skills

- **`mapl-error-handling`** - Error handling macro patterns
- **`github-workflow`** - Code review process
- **`Code Review Checklist`** - Available on MAPL wiki
