module mapl3g_FieldCondensedArrayDims

   implicit none
   private
   public :: FieldCondensedArrayDims

   type :: FieldCondensedArrayDims
      private
      integer :: horz_(2)
      integer :: vert_
      integer, allocatable :: ungridded_(:)
      integer :: dims_(3)
   contains
      procedure :: horizontal
      procedure :: vertical
      procedure :: ungridded
      procedure :: dims
      procedure :: arguments
   end type FieldCondensedArrayDims

   interface FieldCondensedArrayDims
      module procedure :: construct
      module procedure :: construct_dimcount0
      module procedure :: construct_vert
      module procedure :: construct_1h
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
      integer, optional, intent(in) w(:)

      cadims = FieldCondensedArrayDims(0, 0, k, w)

   end function construct_vert

   function construct_1h(u, z, nox, w)
      type(FieldCondensedArrayDims) :: cadims
      integer, intent(in) :: u, z
      logical, intent(in) :: nox
      integer, optional, intent(in) :: w(:)
      integer :: x, y

      x = 1
      y = 0
      if(nox) then
         x = 0
         y = 1
      end if

      cadims = FieldCondensedArrayDims(x, y, z, w)

   end function construct_1h

   function construct(x, y, z, w) result(cadims)
      type(FieldCondensedArrayDims) :: cadims
      integer, intent(in) :: x, y
      integer, optional, intent(in) :: z
      integer, optional, intent(in) :: w(:)
      integer, allocatable :: w_(:)
      integer :: i, j, k, n
      
      w_ = [integer :: ]
      if(present(w)) w_ = w
      k = 0
      if(present(z)) k = z
      cadims%horz_ = [x, y]
      cadims%vert_ = k
      cadims%ungridded_ = w_

      i = max(x, 1)
      j = max(y, 1)
      k = max(k, 1)
      n = 1
      if(size(w_) > 0) n = product(max(w, 1))

      cadims%dims_ = [i*j, k, n]

   end function construct

   function horizontal(this) result(val)
      integer :: val
      class(FieldCondensedArrayDims), intent(in) :: this
      
      val = this%dims_[1]

   end function horizontal
   
   function vertical(this) result(val)
      integer :: val
      class(FieldCondensedArrayDims), intent(in) :: this
      
      val = this%dims_[2]

   end function vertical

   function ungridded(this) result(val)
      integer :: val
      class(FieldCondensedArrayDims), intent(in) :: this
      
      val = this%dims_[3]

   end function ungridded

   function dims(this) result(val)
      integer :: val(3)
      class(FieldCondensedArrayDims), intent(in) :: this
      
      val = this%dims_

   end function dims

   function arguments(this) result(val)
      integer, allocatable :: val(:)
      class(FieldCondensedArrayDims), intent(in) :: this
      integer :: size_ungridded
      
      size_ungridded = size(this%ungridded_)
      allocate(val(3+size_ungridded))
      val(1:3) = [this%horz_(1), this%horz_(2), this%vert]
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
