#include "MAPL.h"

!------------------------------------------------------------------------------
! NodeRevision: 64-bit monotonically-advancing version stamp on a
! StateItemNode's logical value (spec/03-graph-node-hierarchy.md
! REQ-NODE-003a, spec/11-revision-and-update.md REQ-REV-001..003).
!
! The internal integer is private (REQ-REV-001: "no client reads the raw
! integer"); the default-constructed value is a distinct INVALID_VALUE,
! not an ordinary-looking revision - is_valid() is how a caller
! distinguishes "never produced" from "produced at least once."
! advance() transitions invalid -> first valid, and valid -> next valid,
! detecting overflow *before* wrapping (REQ-REV-001) rather than letting
! the internal value silently wrap to an earlier-looking one.
!
! Changing the previous stub's default value (0, which behaved like an
! ordinary-looking revision) to this distinct invalid sentinel does not
! change the observable behavior of any existing Phase 1 caller: every
! Phase 1 use only ever compares two default-constructed instances for
! equality, and both sides are equally invalid under either scheme.
!
! to_string() is a diagnostic-only accessor (used by the graph-neutral
! exporter, spec/19-visualization-export.md REQ-VIZ-016/016a), mirroring
! the same precedent already set by NodeId%to_string() - an encapsulated
! identity type that still exposes a formatted, one-way string for
! display purposes without that constituting "reading the raw value" in
! the sense REQ-REV-001/NodeId's own encapsulation note is guarding
! against (arithmetic on it, reconstructing a revision from it, etc.).
!------------------------------------------------------------------------------
module mapl_NodeRevision_mod
   use, intrinsic :: iso_fortran_env, only: INT64
   use mapl_ErrorHandling_mod
   implicit none(type, external)
   private

   public :: NodeRevision
   public :: operator(==)
   public :: operator(/=)

   integer(INT64), parameter :: INVALID_VALUE = -1_INT64
   integer(INT64), parameter :: FIRST_VALID_VALUE = 0_INT64

   type :: NodeRevision
      private
      integer(INT64) :: value = INVALID_VALUE
   contains
      procedure :: is_valid => revision_is_valid
      procedure :: advance => revision_advance
      procedure :: to_string => revision_to_string
      ! Test-only backdoor: jumps straight to the maximum representable
      ! valid value so the overflow-detection path in advance() can be
      ! exercised without actually looping ~2^63 times. Matches the
      ! established test-backdoor pattern (see GraphStateItem's
      ! debug_force_double_allocate_for_test). Not part of the type's
      ! intended usage; production code MUST NOT call this.
      procedure :: debug_force_max_for_test => revision_debug_force_max
   end type NodeRevision

   interface operator(==)
      module procedure revision_equal
   end interface operator(==)

   interface operator(/=)
      module procedure revision_not_equal
   end interface operator(/=)

contains

   pure logical function revision_is_valid(this) result(valid)
      class(NodeRevision), intent(in) :: this

      valid = this%value /= INVALID_VALUE
   end function revision_is_valid

   ! REQ-REV-001: invalid -> first valid; valid -> next valid; overflow
   ! detected before the increment would wrap, leaving the observable
   ! value unchanged on failure.
   subroutine revision_advance(this, rc)
      class(NodeRevision), intent(inout) :: this
      integer, optional, intent(out) :: rc

      if (.not. this%is_valid()) then
         this%value = FIRST_VALID_VALUE
         _RETURN(_SUCCESS)
      end if

      _ASSERT(this%value < huge(this%value), 'NodeRevision: advance() would overflow')
      this%value = this%value + 1_INT64

      _RETURN(_SUCCESS)
   end subroutine revision_advance

   ! Diagnostic-only formatted value, for the graph-neutral exporter -
   ! see module header. Reports 'INVALID' rather than a numeric value
   ! when the revision has never been advanced.
   function revision_to_string(this) result(value)
      class(NodeRevision), intent(in) :: this
      character(:), allocatable :: value
      character(32) :: buffer

      if (.not. this%is_valid()) then
         value = 'INVALID'
         return
      end if

      write(buffer, '(I0)') this%value
      value = trim(adjustl(buffer))
   end function revision_to_string

   ! Test-only: see the procedure binding's comment above.
   subroutine revision_debug_force_max(this)
      class(NodeRevision), intent(inout) :: this

      this%value = huge(this%value)
   end subroutine revision_debug_force_max

   pure logical function revision_equal(left, right) result(equal)
      type(NodeRevision), intent(in) :: left, right

      equal = left%value == right%value
   end function revision_equal

   pure logical function revision_not_equal(left, right) result(not_equal)
      type(NodeRevision), intent(in) :: left, right

      not_equal = .not. (left == right)
   end function revision_not_equal

end module mapl_NodeRevision_mod
