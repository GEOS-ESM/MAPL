# Migrating Away from BinIO: GETFILE, FREE_FILE, and Tile Files

`BinIO` (module `BinIOMod`) is being removed from MAPL. This guide shows
how to replace the two most commonly used routines — `GETFILE` and
`FREE_FILE` — with standard modern Fortran I/O.

---

## Tile files

Binary (`.til`) and ASCII tile files are no longer supported. If your
component reads a tile file, it must be converted to NetCDF4 format and
read via `MAPL_ReadTilingNC4`. The MAPL `LocStream` layer already handles
NetCDF4 tile files transparently — no code changes are needed there beyond
ensuring the tile file itself is in NetCDF4 format.

---

## What these routines did

### `GETFILE(filename, form=..., ALL_PES=..., rc=...)`

- On the root PE (or all PEs if `ALL_PES=.true.`), scans for a free
  Fortran logical unit number, opens the named file, and returns the unit.
- On non-root PEs (when `ALL_PES` is absent or `.false.`), returns a
  sentinel value (`UNDEF`) without opening anything.

### `FREE_FILE(unit, rc=...)`

- Closes the file attached to `unit` and marks the unit as available.

Both routines exist solely because legacy Fortran lacked a standard way to
obtain a free unit number. Fortran 2003 solved this with `NEWUNIT=`.

---

## The modern replacement

Use the `NEWUNIT=` specifier in an `OPEN` statement. The compiler assigns a
guaranteed-free unit number. Close with a plain `CLOSE` statement.

### Unformatted (binary) file — root PE only

**Before:**
```fortran
use BinIOMod, only: GETFILE, FREE_FILE
...
integer :: unit, status
unit = GETFILE(filename, form='UNFORMATTED', RC=status)
! ... read/write ...
call FREE_FILE(unit, RC=status)
```

**After:**
```fortran
integer :: unit, iostat
if (MAPL_AM_I_ROOT()) then
   open(newunit=unit, file=trim(filename), form='UNFORMATTED', &
        action='READ', iostat=iostat)
   ! check iostat ...
   ! ... read/write ...
   close(unit)
end if
```

---

### Formatted (ASCII) file — root PE only

**Before:**
```fortran
unit = GETFILE(filename, form='FORMATTED', RC=status)
! ... read/write ...
call FREE_FILE(unit, RC=status)
```

**After:**
```fortran
integer :: unit, iostat
if (MAPL_AM_I_ROOT()) then
   open(newunit=unit, file=trim(filename), form='FORMATTED', &
        action='READ', iostat=iostat)
   ! check iostat ...
   ! ... read/write ...
   close(unit)
end if
```

---

### All PEs open the same file (`ALL_PES=.true.`)

**Before:**
```fortran
unit = GETFILE(filename, form='FORMATTED', ALL_PES=.true., RC=status)
! ... read ...
call FREE_FILE(unit, RC=status)
```

**After:**
```fortran
integer :: unit, iostat
open(newunit=unit, file=trim(filename), form='FORMATTED', &
     action='READ', iostat=iostat)
! check iostat ...
! ... read ...
close(unit)
```

Simply omit the `if (MAPL_AM_I_ROOT())` guard when every PE needs its own
file handle.

---

## Error handling

`GETFILE` printed a message and returned an ESMF error code. With `NEWUNIT=`
use `iostat=` and handle it explicitly:

```fortran
integer :: unit, iostat
character(len=256) :: iomsg

open(newunit=unit, file=trim(filename), form='UNFORMATTED', &
     action='READ', iostat=iostat, iomsg=iomsg)
if (iostat /= 0) then
   write(0,*) 'ERROR opening "', trim(filename), '": ', trim(iomsg)
   ! set rc and return, or call MAPL_RETURN / _VERIFY as appropriate
end if
```

---

## Summary

| BinIO call | Modern equivalent |
|---|---|
| `unit = GETFILE(f, form='UNFORMATTED')` | `open(newunit=unit, file=f, form='UNFORMATTED', ...)` inside `if (MAPL_AM_I_ROOT())` |
| `unit = GETFILE(f, form='FORMATTED')` | `open(newunit=unit, file=f, form='FORMATTED', ...)` inside `if (MAPL_AM_I_ROOT())` |
| `unit = GETFILE(f, ..., ALL_PES=.true.)` | `open(newunit=unit, file=f, ...)` without PE guard |
| `call FREE_FILE(unit)` | `close(unit)` |

No module `use` statement is needed — `NEWUNIT=` and `CLOSE` are intrinsic
to Fortran 2003 and later.
