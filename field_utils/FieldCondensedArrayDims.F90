module mapl3g_FieldCondensedArrayDims

   implicit none
   private
   public :: FieldCondensedArrayDims

   type :: FieldCondensedArrayDims
      integer :: horz_(2)
      integer :: vert_
      integer, allocatable :: ungridded_(:)
      integer :: dims_(3)
      integer :: horizontal
      integer :: vertical
      integer :: ungridded
   contains
      procedure :: arguments
      procedure :: initialize
      procedure :: reset
   end type FieldCondensedArrayDims

   interface FieldCondensedArrayDims
      module procedure :: construct
      module procedure :: construct_dimcount0
      module procedure :: construct_vert
      module procedure :: construct_surface
   end interface FieldCondensedArrayDims

contains

   function construct_dimcount0(w) result(cadims)
      type(FieldCondensedArrayDims) :: cadims
      integer, intent(in) :: w(:)

      cadims = FieldCondensedArrayDims(0, 0, 0, w)

   end function construct_dimcount0

   function construct_vert(k, w) result(cadims)
      type(FieldCondensedArrayDims) :: cadims
      integer, intent(in) :: k
      integer, optional, intent(in) :: w(:)

      cadims = FieldCondensedArrayDims(0, 0, k, w)

   end function construct_vert

   function construct_surface(x, y, w) result(cadims)
      type(FieldCondensedArrayDims) :: cadims
      integer, intent(in) :: x, y
      integer, optional, intent(in) :: w(:)

      cadims = FieldCondensedArrayDims(x, y, 0, w)

   end function construct_surface

   function construct(x, y, z, w) result(cadims)
      type(FieldCondensedArrayDims) :: cadims
      integer, intent(in) :: x, y
      integer, intent(in) :: z
      integer, optional, intent(in) :: w(:)
      integer :: dims_(3)
      integer :: i, j, k, n
      
      cadims%horz_ = [x, y]
      cadims%vert_ = z
      cadims%ungridded_ = [integer::]
      i = max(x, 1)
      j = max(y, 1)
      k = max(z, 1)

      n = 1
      if(present(w)) then
         cadims%ungridded_ = w
         n = product(max(w, 1))
      end if

      dims_ = [i*j, k, n]
      cadims%dims_ = dims_
      cadims%horizontal = dims_(1)
      cadims%horizontal = dims_(2)
      cadims%ungridded = dims_(3)

   end function construct

   function arguments(this) result(val)
      integer, allocatable :: val(:)
      class(FieldCondensedArrayDims), intent(in) :: this
      integer :: size_ungridded
      
      size_ungridded = size(this%ungridded_)
      allocate(val(3+size_ungridded))
      val(1:3) = [this%horz_(1), this%horz_(2), this%vert_]
      if(size_ungridded > 0) val(4:size(val)) =  this%ungridded_

   end function arguments

   subroutine initialize(this)
      class(FieldCondensedArrayDims) :: this

      this%horz_(2) = -1
      this%vert_ = -1
      this%dims_ = -1
      if(allocated(this%ungridded_)) deallocate(this%ungridded_)

   end subroutine initialize

   subroutine reset(this)
      class(FieldCondensedArrayDims) :: this

      call this%initialize()

   end subroutine reset
   
end module mapl3g_FieldCondensedArrayDims
