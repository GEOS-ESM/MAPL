#include "MAPL.h"
module mapl_NodeRevision_mod
   use mapl_ErrorHandling_mod
   use, intrinsic :: iso_fortran_env, only: INT64
   implicit none(type, external)
   private

   public :: NodeRevision, INVALID_REVISION
   public :: operator(==), operator(/=)

   integer, parameter :: KIND_NODE_REVISION = INT64
   integer(kind=KIND_NODE_REVISION), parameter :: INVALID = -1

   type :: NodeRevision
      private
      integer(KIND_NODE_REVISION) :: value = INVALID
   contains
      procedure :: advance
      procedure :: is_valid
   end type NodeRevision

   type(NodeRevision), parameter :: INVALID_REVISION = NodeRevision(INVALID)

   interface operator(==)
      procedure :: equal_revision
   end interface operator(==)

   interface operator(/=)
      procedure :: not_equal_revision
   end interface operator(/=)

contains

   pure logical function equal_revision(this, other) result(is_equal)
      type(NodeRevision), intent(in) :: this
      type(NodeRevision), intent(in) :: other

      is_equal = this%value == other%value
   end function equal_revision

   pure logical function not_equal_revision(this, other) result(is_not_equal)
      type(NodeRevision), intent(in) :: this
      type(NodeRevision), intent(in) :: other

      is_not_equal = .not. (this == other)
   end function not_equal_revision

   subroutine advance(this, rc)
      class(NodeRevision), intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status

      _ASSERT(this%value < huge(1_KIND_NODE_REVISION), 'Revision count exceeded max')
      this%value = max(1_KIND_NODE_REVISION, this%value + 1)

      _RETURN(_SUCCESS)
   end subroutine advance

   pure logical function is_valid(this)
      class(NodeRevision), intent(in) :: this
      is_valid = (this%value > 0)
   end function is_valid

end module mapl_NodeRevision_mod
