---
name: mapl-error-handling
description: MAPL error handling macros and best practices
compatibility: opencode
---

## What I Do

Provide comprehensive guidance on MAPL's error handling system, including:
- Error handling macros and their usage
- Return code conventions
- Best practices for robust error handling
- Complete code examples
- Legacy patterns to avoid

## When to Use Me

Use this skill when:
- Writing new Fortran procedures for MAPL
- Adding error handling to existing code
- Debugging error handling issues
- Code review (checking error handling compliance)
- Converting legacy error handling patterns

## Required Include

**All modules using MAPL error handling must include:**

```fortran
#include "MAPL_Generic.h"

module MyModule
   use MAPL
   implicit none
   private
   
   ! Your code here
   
end module MyModule
```

This provides access to all MAPL error handling macros.

## Return Code Conventions

**Standard convention:** 
- **0 = Success** (use `_SUCCESS` macro)
- **Non-zero = Failure** (various error codes)

MAPL uses ESMF return code constants:
- `ESMF_SUCCESS` = 0
- `ESMF_FAILURE` = non-zero

**MAPL macros:**
- `_SUCCESS` → `ESMF_SUCCESS` (0)
- `_FAILURE` → `ESMF_FAILURE` (non-zero)

## Core Error Handling Macros

### _RC (Most Common)

**Usage:** Compact call + verify pattern

```fortran
call some_procedure(arg1, arg2, _RC)
```

**Expands to:**
```fortran
rc=status); _VERIFY(status)
```

**What it does:**
1. Passes `status` variable as rc argument
2. Checks if status is success
3. If failure: prints file/line, returns with rc set to status

**Example:**
```fortran
subroutine process_data(input, output, rc)
   type(DataSet), intent(in) :: input
   type(DataSet), intent(out) :: output
   integer, optional, intent(out) :: rc
   
   integer :: status
   
   call read_configuration('config.yaml', _RC)
   call allocate_workspace(n_items, _RC)
   call transform_data(input, output, _RC)
   
   _RETURN(_SUCCESS)
end subroutine process_data
```

**Benefits:**
- Compact (one line for call + error check)
- Automatic file/line reporting
- Consistent error propagation

### _VERIFY

**Usage:** Check return codes explicitly

```fortran
call some_procedure(arg1, arg2, rc=status)
_VERIFY(status)
```

**What it does:**
- Checks if `status == _SUCCESS`
- If not: prints status, file, line number
- Returns from current procedure with rc=status

**Use when:**
- Need to separate call from verification
- Capturing return code for logging
- Custom logic between call and check

**Example:**
```fortran
call read_file(filename, data, rc=status)
if (status /= _SUCCESS) then
   call log_error('Failed to read: ' // trim(filename))
end if
_VERIFY(status)
```

Usually `_RC` is preferred over this pattern.

### _ASSERT

**Usage:** Check logical conditions

```fortran
_ASSERT(condition, "Error message")
```

**What it does:**
- If condition is FALSE: prints message, file/line, returns with failure
- If condition is TRUE: continues execution

**Examples:**
```fortran
_ASSERT(n_columns > 0, "Number of columns must be positive")
_ASSERT(allocated(workspace), "Workspace not allocated")
_ASSERT(size(input) == size(output), "Array size mismatch")
```

**Use for:**
- Precondition checking
- Invariant validation
- Argument validation
- State verification

**Don't use for:**
- Checking return codes (use `_VERIFY` or `_RC`)
- Conditions that should always use `_FAIL` instead

### _FAIL

**Usage:** Force procedure to fail with message

```fortran
_FAIL("Error message describing the problem")
```

**What it does:**
- Prints message, file, line
- Sets rc to `_FAILURE`
- Returns from procedure

**Common use:** SELECT CASE with invalid/unexpected values

**Example:**
```fortran
select case(operation_type)
case(ADD)
   result = a + b
case(SUBTRACT)
   result = a - b
case(MULTIPLY)
   result = a * b
case default
   _FAIL("Unknown operation type")
end select
```

**Also useful for:**
- Error conditions without a logical test
- Explicit failure paths
- "This should never happen" scenarios

**Note:** If you find yourself doing `_ASSERT(.FALSE., "message")`, use `_FAIL("message")` instead - it's clearer.

### _RETURN

**Usage:** Return from procedure with given status

```fortran
_RETURN(status_value)
```

**What it does:**
- Sets rc to provided value (if rc present)
- Returns from procedure

**Standard pattern at end of successful procedure:**
```fortran
subroutine my_procedure(arg, rc)
   integer, intent(in) :: arg
   integer, optional, intent(out) :: rc
   
   ! ... procedure implementation ...
   
   _RETURN(_SUCCESS)  ! Always end successful procedures this way
end subroutine my_procedure
```

**Can return with any value:**
```fortran
if (early_exit_condition) then
   _RETURN(_SUCCESS)  ! Early successful return
end if
```

### _SUCCESS

**Usage:** Represents successful return code (0)

```fortran
_RETURN(_SUCCESS)

if (status == _SUCCESS) then
   ! Success path
end if
```

**Expands to:** `ESMF_SUCCESS` (which is 0)

**Use instead of:** Literal `0` in code

**Benefits:** 
- Self-documenting
- Consistent with ESMF conventions
- Future-proof if convention ever changes

### _FAILURE

**Usage:** Represents failure return code (non-zero)

```fortran
if (status /= _SUCCESS) then
   ! This is a failure
end if
```

**Expands to:** `ESMF_FAILURE`

**Note:** Usually don't need this directly - macros handle it. Provided for completeness.

### _STAT

**Usage:** Check Fortran intrinsic STAT codes

```fortran
allocate(array(n), _STAT)
deallocate(array, _STAT)
```

**Expands to:**
```fortran
stat=status); _VERIFY(status)
```

**Examples:**
```fortran
subroutine allocate_workspace(n, rc)
   integer, intent(in) :: n
   integer, optional, intent(out) :: rc
   
   integer :: status
   real, allocatable :: workspace(:)
   
   allocate(workspace(n), _STAT)
   
   ! Use workspace
   
   deallocate(workspace, _STAT)
   
   _RETURN(_SUCCESS)
end subroutine
```

### _IOSTAT

**Usage:** Check Fortran I/O IOSTAT codes

```fortran
open(unit, file=filename, _IOSTAT)
read(unit, *, _IOSTAT) values
close(unit, _IOSTAT)
```

**Expands to:**
```fortran
iostat=status); _VERIFY(status)
```

**Example:**
```fortran
subroutine read_data(filename, values, rc)
   character(len=*), intent(in) :: filename
   real, intent(out) :: values(:)
   integer, optional, intent(out) :: rc
   
   integer :: status, unit
   
   open(newunit=unit, file=filename, status='old', _IOSTAT)
   read(unit, *, _IOSTAT) values
   close(unit, _IOSTAT)
   
   _RETURN(_SUCCESS)
end subroutine read_data
```

### _HERE

**Usage:** Debug print with file and line information

```fortran
_HERE, "debug message", variables
```

**Expands to:**
```fortran
print*, __FILE__, __LINE__, "debug message", variables
```

**Examples:**
```fortran
_HERE, "n_columns:", n_columns
_HERE, "Entering iteration:", iteration_count
_HERE, "Checkpoint A"
```

**Use for:**
- Debugging prints with location info
- Tracking execution flow
- Quick diagnostics

**Note:** Remember to remove or comment out after debugging!

## Complete Example

```fortran
#include "MAPL_Generic.h"

module MAPL_DataProcessor
   use ESMF
   use MAPL_BaseMod
   implicit none
   private
   
   public :: process_dataset
   
contains

   subroutine process_dataset(input_file, output_file, n_iterations, rc)
      character(len=*), intent(in) :: input_file
      character(len=*), intent(in) :: output_file
      integer, intent(in) :: n_iterations
      integer, optional, intent(out) :: rc
      
      integer :: status
      real, allocatable :: data(:)
      integer :: i, n_points
      
      ! Validate inputs
      _ASSERT(n_iterations > 0, "Iterations must be positive")
      _ASSERT(len_trim(input_file) > 0, "Input filename cannot be empty")
      
      ! Read number of points from file
      call read_file_size(input_file, n_points, _RC)
      
      ! Allocate workspace
      allocate(data(n_points), _STAT)
      
      ! Read data
      call read_data(input_file, data, _RC)
      
      ! Process
      do i = 1, n_iterations
         call apply_filter(data, _RC)
      end do
      
      ! Write results
      call write_data(output_file, data, _RC)
      
      ! Cleanup
      deallocate(data, _STAT)
      
      _RETURN(_SUCCESS)
   end subroutine process_dataset
   
   subroutine read_file_size(filename, n_points, rc)
      character(len=*), intent(in) :: filename
      integer, intent(out) :: n_points
      integer, optional, intent(out) :: rc
      
      integer :: status, unit
      
      open(newunit=unit, file=filename, status='old', _IOSTAT)
      read(unit, *, _IOSTAT) n_points
      close(unit, _IOSTAT)
      
      _ASSERT(n_points > 0, "File contains invalid point count")
      
      _RETURN(_SUCCESS)
   end subroutine read_file_size
   
   subroutine apply_filter(data, rc)
      real, intent(inout) :: data(:)
      integer, optional, intent(out) :: rc
      
      integer :: status, filter_type
      
      ! Get filter type from configuration
      call get_filter_type(filter_type, _RC)
      
      ! Apply appropriate filter
      select case(filter_type)
      case(1)
         call apply_gaussian_filter(data, _RC)
      case(2)
         call apply_median_filter(data, _RC)
      case(3)
         call apply_moving_average(data, _RC)
      case default
         _FAIL("Unknown filter type")
      end select
      
      _RETURN(_SUCCESS)
   end subroutine apply_filter

end module MAPL_DataProcessor
```

## Best Practices

### 1. Always Check Return Codes

**DO:**
```fortran
call some_procedure(args, _RC)
```

**DON'T:**
```fortran
call some_procedure(args)  ! Ignoring potential errors!
```

**Why:** If something fails, you want to know immediately, not debug mysterious crashes later.

### 2. End Procedures with _RETURN(_SUCCESS)

**DO:**
```fortran
subroutine my_proc(rc)
   integer, optional, intent(out) :: rc
   
   ! ... successful execution ...
   
   _RETURN(_SUCCESS)
end subroutine
```

**DON'T:**
```fortran
subroutine my_proc(rc)
   integer, optional, intent(out) :: rc
   
   ! ... successful execution ...
   
   ! Missing return status!
end subroutine
```

### 3. Validate Inputs Early

```fortran
subroutine process(n_items, tolerance, rc)
   integer, intent(in) :: n_items
   real, intent(in) :: tolerance
   integer, optional, intent(out) :: rc
   
   ! Validate first
   _ASSERT(n_items > 0, "n_items must be positive")
   _ASSERT(tolerance > 0.0, "tolerance must be positive")
   _ASSERT(tolerance < 1.0, "tolerance must be less than 1")
   
   ! Now proceed with validated inputs
   ! ...
   
   _RETURN(_SUCCESS)
end subroutine
```

### 4. Check All Allocations

```fortran
allocate(workspace(n), _STAT)
allocate(temporary(m), _STAT)
```

Never:
```fortran
allocate(workspace(n))  ! Unchecked!
```

### 5. Check All File Operations

```fortran
open(newunit=unit, file=filename, _IOSTAT)
read(unit, *, _IOSTAT) data
close(unit, _IOSTAT)
```

### 6. Provide Informative Error Messages

**Good:**
```fortran
_ASSERT(n_rows == n_cols, "Matrix must be square for this operation")
_FAIL("Configuration file missing required 'output_dir' parameter")
```

**Poor:**
```fortran
_ASSERT(n_rows == n_cols, "Error")
_FAIL("Bad input")
```

## Error Handling Patterns

### Pattern: Cleanup on Error

```fortran
subroutine process_with_cleanup(rc)
   integer, optional, intent(out) :: rc
   integer :: status
   real, allocatable :: temp(:)
   logical :: file_open
   
   file_open = .false.
   
   allocate(temp(100), _STAT)
   
   open(newunit=unit, file='data.txt', _IOSTAT)
   file_open = .true.
   
   call risky_operation(_RC)
   
   ! Cleanup
   if (file_open) close(unit)
   deallocate(temp)
   
   _RETURN(_SUCCESS)
end subroutine
```

Note: Error macros return immediately, so cleanup after error is tricky. Consider Fortran block scope or finalization for robust cleanup.

### Pattern: Error Message Context

```fortran
call read_configuration(config_file, config, rc=status)
if (status /= _SUCCESS) then
   call log_error("Failed reading config: " // trim(config_file))
end if
_VERIFY(status)
```

### Pattern: Continue on Error (Rare)

```fortran
! Unusual case: want to continue despite errors
call optional_operation(rc=status)
if (status /= _SUCCESS) then
   call log_warning("Optional operation failed, continuing")
   ! Don't call _VERIFY - continue execution
end if
```

## Legacy Macros (DO NOT USE)

**Old macros ending with underscore:** `VERIFY_`, `ASSERT_`, `__RC__`

**Problem:** Required `IAM` character variable with procedure name
```fortran
! OLD STYLE - Don't do this!
character(len=*), parameter :: IAM = 'my_procedure'
call something(args, __RC__)
```

**Issues:**
- Easily copied incorrectly (IAM doesn't match actual procedure)
- Manual maintenance burden
- Less informative than __FILE__ and __LINE__

**Migration:**
```fortran
! OLD
character(len=*), parameter :: IAM = 'my_procedure'
call foo(args, __RC__)

! NEW
call foo(args, _RC)
```

**If you see these in code review: Request update to new macros**

## Common Mistakes

### Forgetting Include

**Error:** Macros not defined
**Solution:**
```fortran
#include "MAPL_Generic.h"
```
Must come before module statement.

### No RC Parameter

**Error:** Macros try to set rc but procedure doesn't have it
**Solution:** Add optional rc parameter
```fortran
subroutine my_proc(arg, rc)  ! Add rc parameter
   integer, intent(in) :: arg
   integer, optional, intent(out) :: rc
```

### Using == for Logical

```fortran
! WRONG - not standard conforming
if (flag == .true.) then

! CORRECT - but redundant
if (flag .eqv. .true.) then

! BEST - direct use
if (flag) then
```

## Code Review Checklist

- [ ] `#include "MAPL_Generic.h"` present
- [ ] All procedures with potential errors have optional rc parameter
- [ ] All procedure calls with rc checked (use `_RC`)
- [ ] All allocations checked (use `_STAT`)
- [ ] All file I/O checked (use `_IOSTAT`)
- [ ] Procedures end with `_RETURN(_SUCCESS)`
- [ ] Assertions used for precondition validation
- [ ] Informative error messages provided
- [ ] No legacy macros (`VERIFY_`, `__RC__`, etc.)
- [ ] No unchecked operations

## Related Skills

- **`fortran-style`** - General Fortran coding standards
- **`github-workflow`** - Code review process
- **Error Handling Macros wiki** - Detailed macro documentation

## Summary

**Key Points:**
1. Include `MAPL_Generic.h` in all modules
2. Use `_RC` for compact call + check pattern
3. Use `_ASSERT` for precondition validation
4. Use `_STAT` and `_IOSTAT` for intrinsics
5. End successful procedures with `_RETURN(_SUCCESS)`
6. Always check return codes - no exceptions
7. Provide informative error messages
8. Avoid legacy macros (ending with _)

**Philosophy:** Error handling everywhere in calling chain. If something can fail, check it. Your future self (and teammates) will thank you during debugging.
