#include "MAPL_Generic.h"

module MAPL_FieldPointerUtilities
   use ESMF
   use MAPL_ExceptionHandling
   use, intrinsic :: iso_c_binding, only: c_ptr, c_f_pointer, c_loc
   implicit none
   private

   public :: FieldsHaveUndef
   public :: GetFieldsUndef
   public :: assign_fptr
   public :: FieldGetLocalElementCount
   public :: FieldGetLocalSize
   public :: FieldGetCptr
   public :: FieldClone
   public :: FieldsAreConformable
   public :: FieldsAreBroadcastConformable
   public :: FieldsAreSameTypeKind
   public :: FieldCopy
   public :: MAPL_FieldDestroy
   public :: FieldCopyBroadcast

   interface GetFieldsUndef
      module procedure GetFieldsUndef_r4
      module procedure GetFieldsUndef_r8
   end interface

   interface assign_fptr
      module procedure assign_fptr_r4_rank1
      module procedure assign_fptr_r8_rank1
      module procedure assign_fptr_r4_rank2
      module procedure assign_fptr_r8_rank2
   end interface assign_fptr

   interface FieldGetCptr
      procedure get_cptr
   end interface

   interface FieldGetLocalSize
      procedure get_local_size
   end interface FieldGetLocalSize

   interface FieldGetLocalElementCount
      procedure get_local_element_count
   end interface FieldGetLocalElementCount

   interface FieldsAreConformable
      procedure are_conformable_scalar
      procedure are_conformable_array
   end interface

   interface FieldsAreBroadCastConformable
      procedure are_broadcast_conformable
   end interface

   interface FieldClone
      module procedure clone
   end interface FieldClone

   interface FieldsAreSameTypeKind
      module procedure are_same_type_kind
   end interface FieldsAreSameTypeKind

   interface verify_typekind
      module procedure verify_typekind_scalar
      module procedure verify_typekind_array
   end interface verify_typekind

   interface FieldCOPY
      procedure copy
   end interface FieldCOPY

   interface FieldCopyBroadcast
      procedure copy_broadcast
   end interface FieldCopyBroadcast

   interface MAPL_FieldDestroy
      procedure destroy
   end interface
contains


   subroutine assign_fptr_r4_rank1(x, fptr, rc)
      type(ESMF_Field), intent(inout) :: x
      real(kind=ESMF_KIND_R4), pointer, intent(out) :: fptr(:)
      integer, optional, intent(out) :: rc

      ! local declarations
      type(c_ptr) :: cptr
      integer(ESMF_KIND_I8), allocatable :: fp_shape(:)
      integer(ESMF_KIND_I8) :: local_size
      integer :: status

      local_size = FieldGetLocalSize(x, __RC)
      fp_shape = [ local_size ]
      call FieldGetCptr(x, cptr, __RC)
      call c_f_pointer(cptr, fptr, fp_shape)

      __RETURN(__SUCCESS)
   end subroutine assign_fptr_r4_rank1

   subroutine assign_fptr_r8_rank1(x, fptr, rc)
      type(ESMF_Field), intent(inout) :: x
      real(kind=ESMF_KIND_R8), pointer, intent(out) :: fptr(:)
      integer, optional, intent(out) :: rc

      ! local declarations
      type(c_ptr) :: cptr
      integer(ESMF_KIND_I8), allocatable :: fp_shape(:)
      integer(ESMF_KIND_I8) :: local_size
      integer :: status

      local_size = FieldGetLocalSize(x, __RC)
      fp_shape = [ local_size ]
      call FieldGetCptr(x, cptr, __RC)
      call c_f_pointer(cptr, fptr, fp_shape)

      __RETURN(__SUCCESS)
   end subroutine assign_fptr_r8_rank1

   subroutine assign_fptr_r4_rank2(x, fp_shape, fptr, rc)
      type(ESMF_Field), intent(inout) :: x
      integer(ESMF_KIND_I8), intent(in) :: fp_shape(:)
      real(kind=ESMF_KIND_R4), pointer, intent(out) :: fptr(:,:)
      integer, optional, intent(out) :: rc

      ! local declarations
      type(c_ptr) :: cptr
      integer :: status

      call FieldGetCptr(x, cptr, __RC)
      call c_f_pointer(cptr, fptr, fp_shape)

      __RETURN(__SUCCESS)
   end subroutine assign_fptr_r4_rank2

   subroutine assign_fptr_r8_rank2(x, fp_shape, fptr, rc)
      type(ESMF_Field), intent(inout) :: x
      integer(ESMF_KIND_I8), intent(in) :: fp_shape(:)
      real(kind=ESMF_KIND_R8), pointer, intent(out) :: fptr(:,:)
      integer, optional, intent(out) :: rc

      ! local declarations
      type(c_ptr) :: cptr
      integer :: status

      call FieldGetCptr(x, cptr, __RC)
      call c_f_pointer(cptr, fptr, fp_shape)

      __RETURN(__SUCCESS)
   end subroutine assign_fptr_r8_rank2

   subroutine get_cptr(x, cptr, rc)
      type(ESMF_Field), intent(inout) :: x
      type(c_ptr), intent(out) :: cptr
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_TypeKind_Flag) :: tk_x

      call ESMF_FieldGet(x, typekind=tk_x, __RC)

      if (tk_x == ESMF_TYPEKIND_R4) then
         call get_cptr_r4(x, cptr, __RC)
      elseif (tk_x == ESMF_TYPEKIND_R8) then
         call get_cptr_r8(x, cptr, __RC)
      elseif (tk_x == ESMF_TYPEKIND_I4) then
         call get_cptr_i4(x, cptr, __RC)
      elseif (tk_x == ESMF_TYPEKIND_I8) then
         call get_cptr_i8(x, cptr, __RC)
      else
         __FAIL('Unsupported typekind in FieldGetCptr().')
      end if

      __RETURN(__SUCCESS)
   end subroutine get_cptr

   subroutine get_cptr_r4(x, cptr, rc)
      type(ESMF_Field), intent(inout) :: x
      type(c_ptr), intent(out) :: cptr
      integer, optional, intent(out) :: rc

      integer :: status
      integer :: rank
      real(kind=ESMF_KIND_R4), pointer :: x_1d(:)
      real(kind=ESMF_KIND_R4), pointer :: x_2d(:,:)
      real(kind=ESMF_KIND_R4), pointer :: x_3d(:,:,:)
      real(kind=ESMF_KIND_R4), pointer :: x_4d(:,:,:,:)
      real(kind=ESMF_KIND_R4), pointer :: x_5d(:,:,:,:,:)

      call ESMF_FieldGet(x, rank=rank, __RC)

      select case (rank)
      case (1)
         call ESMF_FieldGet(x, farrayPtr = x_1d, __RC)
         cptr = c_loc(x_1d)
      case (2)
         call ESMF_FieldGet(x, farrayPtr = x_2d, __RC)
         cptr = c_loc(x_2d)
      case (3)
         call ESMF_FieldGet(x, farrayPtr = x_3d, __RC)
         cptr = c_loc(x_3d)
      case (4)
         call ESMF_FieldGet(x, farrayPtr = x_4d, __RC)
         cptr = c_loc(x_4d)
      case (5)
         call ESMF_FieldGet(x, farrayPtr = x_5d, __RC)
         cptr = c_loc(x_5d)
      case default
         __FAIL('Unsupported rank in FieldGetCptr().')
      end select

      __RETURN(__SUCCESS)
   end subroutine get_cptr_r4

   subroutine get_cptr_r8(x, cptr, rc)
      type(ESMF_Field), intent(inout) :: x
      type(c_ptr), intent(out) :: cptr
      integer, optional, intent(out) :: rc

      integer :: status
      integer :: rank
      real(kind=ESMF_KIND_R8), pointer :: x_1d(:)
      real(kind=ESMF_KIND_R8), pointer :: x_2d(:,:)
      real(kind=ESMF_KIND_R8), pointer :: x_3d(:,:,:)
      real(kind=ESMF_KIND_R8), pointer :: x_4d(:,:,:,:)
      real(kind=ESMF_KIND_R8), pointer :: x_5d(:,:,:,:,:)

      call ESMF_FieldGet(x, rank=rank, __RC)

      select case (rank)
      case (1)
         call ESMF_FieldGet(x, farrayPtr = x_1d, __RC)
         cptr = c_loc(x_1d)
      case (2)
         call ESMF_FieldGet(x, farrayPtr = x_2d, __RC)
         cptr = c_loc(x_2d)
      case (3)
         call ESMF_FieldGet(x, farrayPtr = x_3d, __RC)
         cptr = c_loc(x_3d)
      case (4)
         call ESMF_FieldGet(x, farrayPtr = x_4d, __RC)
         cptr = c_loc(x_4d)
      case (5)
         call ESMF_FieldGet(x, farrayPtr = x_5d, __RC)
         cptr = c_loc(x_5d)
      case default
         __FAIL('Unsupported rank in FieldGetCptr().')
      end select

      __RETURN(__SUCCESS)
   end subroutine get_cptr_r8

   subroutine get_cptr_i4(x, cptr, rc)
      type(ESMF_Field), intent(inout) :: x
      type(c_ptr), intent(out) :: cptr
      integer, optional, intent(out) :: rc

      integer :: status
      integer :: rank
      integer(kind=ESMF_KIND_I4), pointer :: x_1d(:)
      integer(kind=ESMF_KIND_I4), pointer :: x_2d(:,:)
      integer(kind=ESMF_KIND_I4), pointer :: x_3d(:,:,:)
      integer(kind=ESMF_KIND_I4), pointer :: x_4d(:,:,:,:)
      integer(kind=ESMF_KIND_I4), pointer :: x_5d(:,:,:,:,:)

      call ESMF_FieldGet(x, rank=rank, __RC)

      select case (rank)
      case (1)
         call ESMF_FieldGet(x, farrayPtr = x_1d, __RC)
         cptr = c_loc(x_1d)
      case (2)
         call ESMF_FieldGet(x, farrayPtr = x_2d, __RC)
         cptr = c_loc(x_2d)
      case (3)
         call ESMF_FieldGet(x, farrayPtr = x_3d, __RC)
         cptr = c_loc(x_3d)
      case (4)
         call ESMF_FieldGet(x, farrayPtr = x_4d, __RC)
         cptr = c_loc(x_4d)
      case (5)
         call ESMF_FieldGet(x, farrayPtr = x_5d, __RC)
         cptr = c_loc(x_5d)
      case default
         __FAIL('Unsupported rank in FieldGetCptr().')
      end select

      __RETURN(__SUCCESS)
   end subroutine get_cptr_i4

   subroutine get_cptr_i8(x, cptr, rc)
      type(ESMF_Field), intent(inout) :: x
      type(c_ptr), intent(out) :: cptr
      integer, optional, intent(out) :: rc

      integer :: status
      integer :: rank
      integer(kind=ESMF_KIND_I8), pointer :: x_1d(:)
      integer(kind=ESMF_KIND_I8), pointer :: x_2d(:,:)
      integer(kind=ESMF_KIND_I8), pointer :: x_3d(:,:,:)
      integer(kind=ESMF_KIND_I8), pointer :: x_4d(:,:,:,:)
      integer(kind=ESMF_KIND_I8), pointer :: x_5d(:,:,:,:,:)

      call ESMF_FieldGet(x, rank=rank, __RC)

      select case (rank)
      case (1)
         call ESMF_FieldGet(x, farrayPtr = x_1d, __RC)
         cptr = c_loc(x_1d)
      case (2)
         call ESMF_FieldGet(x, farrayPtr = x_2d, __RC)
         cptr = c_loc(x_2d)
      case (3)
         call ESMF_FieldGet(x, farrayPtr = x_3d, __RC)
         cptr = c_loc(x_3d)
      case (4)
         call ESMF_FieldGet(x, farrayPtr = x_4d, __RC)
         cptr = c_loc(x_4d)
      case (5)
         call ESMF_FieldGet(x, farrayPtr = x_5d, __RC)
         cptr = c_loc(x_5d)
      case default
         __FAIL('Unsupported rank in FieldGetCptr().')
      end select

      __RETURN(__SUCCESS)
   end subroutine get_cptr_i8

   function get_local_element_count(x, rc) result(element_count)
      type(ESMF_Field), intent(inout) :: x
      integer, optional, intent(out) :: rc
      integer, allocatable :: element_count(:)

      integer :: status
      integer :: rank

      call ESMF_FieldGet(x, rank=rank, __RC)
      allocate(element_count(rank))
      ! ESMF has a big fat bug with multi tile grids and loal element count
      !call ESMF_FieldGet(x, localElementCount=element_count, __RC)
      ! until it is fixed we must kluge :(
      call MAPL_FieldGetLocalElementCount(x, element_count, __RC)

      __RETURN(__SUCCESS)
   end function get_local_element_count

   function get_local_size(x, rc) result(sz)
      integer(kind=ESMF_KIND_I8) :: sz
      type(ESMF_Field), intent(inout) :: x
      integer, optional, intent(out) :: rc

      integer :: status
      integer, allocatable :: element_count(:)

      sz = 0
      element_count = FieldGetLocalElementCount(x, __RC)
      sz = int(product(element_count), kind=ESMF_KIND_I8)

      __RETURN(__SUCCESS)
   end function get_local_size

   subroutine clone(x, y, rc)
      type(ESMF_Field), intent(inout) :: x
      type(ESMF_Field), intent(inout) :: y
      integer, optional, intent(out) :: rc

      character(len=*), parameter :: CLONE_TAG = '_clone'
      !type(ESMF_ArraySpec) :: arrayspec
      type(ESMF_Grid) :: grid
      type(ESMF_StaggerLoc) :: staggerloc
      integer, allocatable :: gridToFieldMap(:)
      integer, allocatable :: ungriddedLBound(:)
      integer, allocatable :: ungriddedUBound(:)
      type(ESMF_TypeKind_Flag) :: tk
      character(len=ESMF_MAXSTR) :: name
      integer :: status
      integer :: field_rank, grid_rank,ungrid_size
      type(ESMF_Index_Flag) :: index_flag
      real(kind=ESMF_KIND_R4), pointer      :: VR4_1D(:), VR4_2D(:,:), VR4_3D(:,:,:), VR4_4D(:,:,:,:)
      real(kind=ESMF_KIND_R8), pointer      :: VR8_1D(:), VR8_2D(:,:), VR8_3D(:,:,:), VR8_4D(:,:,:,:)
      integer, allocatable :: lc(:)

      call ESMF_FieldGet(x,grid=grid,rank=field_rank,__RC)
      lc = get_local_element_count(x,__RC)
      call ESMF_GridGet(grid,dimCount=grid_rank,indexFlag=index_flag,__RC)
      ungrid_size = field_rank-grid_rank
      allocate(gridToFieldMap(grid_rank))
      allocate(ungriddedLBound(ungrid_size),ungriddedUBound(ungrid_size))
      call ESMF_FieldGet(x, typekind=tk, name = name, &
         staggerloc=staggerloc, gridToFieldMap=gridToFieldMap, &
         ungriddedLBound=ungriddedLBound, ungriddedUBound=ungriddedUBound,  __RC)

      name = trim(name) // CLONE_TAG

      if (index_flag == ESMF_INDEX_USER) then
         if (tk == ESMF_TYPEKIND_R4 .and. field_rank == 1) then
            allocate(VR4_1d(lc(1)),__STAT)
            y = ESMF_FieldCreate(grid,VR4_1d,gridToFieldMap=gridToFieldMap,name=name,__RC)
         else if (tk == ESMF_TYPEKIND_R8 .and. field_rank == 1) then
            allocate(VR8_1d(lc(1)),__STAT)
            y = ESMF_FieldCreate(grid,VR8_1d,gridToFieldMap=gridToFieldMap,name=name,__RC)
         else if (tk == ESMF_TYPEKIND_R4 .and. field_rank == 2) then
            allocate(VR4_2d(lc(1),lc(2)),__STAT)
            y = ESMF_FieldCreate(grid,VR4_2d,gridToFieldMap=gridToFieldMap,name=name,__RC)
         else if (tk == ESMF_TYPEKIND_R8 .and. field_rank == 2) then
            allocate(VR8_2d(lc(1),lc(2)),__STAT)
            y = ESMF_FieldCreate(grid,VR8_2d,gridToFieldMap=gridToFieldMap,name=name,__RC)
         else if (tk == ESMF_TYPEKIND_R4 .and. field_rank == 3) then
            allocate(VR4_3d(lc(1),lc(2),lc(3)),__STAT)
            y = ESMF_FieldCreate(grid,VR4_3d,gridToFieldMap=gridToFieldMap,name=name,__RC)
         else if (tk == ESMF_TYPEKIND_R8 .and. field_rank == 3) then
            allocate(VR8_3d(lc(1),lc(2),lc(3)),__STAT)
            y = ESMF_FieldCreate(grid,VR8_3d,gridToFieldMap=gridToFieldMap,name=name,__RC)
         else if (tk == ESMF_TYPEKIND_R4 .and. field_rank == 4) then
            allocate(VR4_4d(lc(1),lc(2),lc(3),lc(4)),__STAT)
            y = ESMF_FieldCreate(grid,VR4_4d,gridToFieldMap=gridToFieldMap,name=name,__RC)
         else if (tk == ESMF_TYPEKIND_R8 .and. field_rank == 4) then
            allocate(VR8_4d(lc(1),lc(2),lc(3),lc(4)),__STAT)
            y = ESMF_FieldCreate(grid,VR8_4d,gridToFieldMap=gridToFieldMap,name=name,__RC)
         else
            __FAIL( 'unsupported typekind+field_rank')
         end if
      else
         y = ESMF_FieldCreate(grid, tk, staggerloc=staggerloc, &
            gridToFieldMap=gridToFieldMap, ungriddedLBound=ungriddedLBound, &
            ungriddedUBound=ungriddedUBound, name=name, __RC)
      end if

      __RETURN(__SUCCESS)
   end subroutine clone

   logical function are_conformable_scalar(x, y, rc) result(conformable)
      type(ESMF_Field), intent(inout) :: x
      type(ESMF_Field), intent(inout) :: y
      integer, optional, intent(out) :: rc
      integer :: rank_x, rank_y
      integer, dimension(:), allocatable :: count_x, count_y
      integer :: status

      conformable = .false.

      call ESMF_FieldGet(x, rank=rank_x, __RC)
      call ESMF_FieldGet(y, rank=rank_y, __RC)

      if(rank_x == rank_y) then
         count_x = FieldGetLocalElementCount(x, __RC)
         count_y = FieldGetLocalElementCount(y, __RC)
         conformable = all(count_x == count_y)
      end if

      __RETURN(__SUCCESS)
   end function are_conformable_scalar

   logical function are_conformable_array(x, y, rc) result(conformable)
      type(ESMF_Field), intent(inout) :: x
      type(ESMF_Field), intent(inout) :: y(:)
      integer, optional, intent(out) :: rc

      integer :: status
      integer :: j
      logical :: element_not_conformable

      conformable = .false.
      element_not_conformable = .false.

      do j = 1, size(y)
         element_not_conformable = .not. FieldsAreConformable(x, y(j), __RC)
         if(element_not_conformable) return
      end do

      conformable = .true.

      __RETURN(__SUCCESS)
   end function are_conformable_array

   logical function are_broadcast_conformable(x, y, rc) result(conformable)
      type(ESMF_Field), intent(inout) :: x
      type(ESMF_Field), intent(inout) :: y
      integer, optional, intent(out) :: rc
      integer :: rank_x, rank_y
      integer, dimension(:), allocatable :: count_x, count_y
      integer :: status
      logical :: normal_conformable

      conformable = .false.
      ! this should really used the geom and ungridded dims
      ! for now we will do this until we have a geom agnostic stuff worked out...
      ! the ideal algorithm would be if geom == geom and input does not have ungridded
      ! and thing we are copying to does, then we are "conformable"
      normal_conformable = FIeldsAreConformable(x,y,__RC)

      if (normal_conformable) then
         conformable = .true.
         __RETURN(__SUCCESS)
      end if

      call ESMF_FieldGet(x, rank=rank_x, __RC)
      call ESMF_FieldGet(y, rank=rank_y, __RC)

      if( (rank_x+1) == rank_y) then
         count_x = FieldGetLocalElementCount(x, __RC)
         count_y = FieldGetLocalElementCount(y, __RC)
         conformable = all(count_x == count_y(:rank_y-1))
      end if

      __RETURN(__SUCCESS)
   end function are_broadcast_conformable

   logical function are_same_type_kind(x, y, rc) result(same_tk)
      type(ESMF_Field), intent(inout) :: x
      type(ESMF_Field), intent(inout) :: y
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_TypeKind_Flag) :: tk_x, tk_y

      same_tk = .false.
      call ESMF_FieldGet(x, typekind=tk_x, __RC)
      call ESMF_FieldGet(y, typekind=tk_y, __RC)

      same_tk = (tk_x == tk_y)

      __RETURN(__SUCCESS)
   end function are_same_type_kind

    subroutine verify_typekind_scalar(x, expected_tk, rc)
      type(ESMF_Field), intent(inout) :: x
      type(ESMF_TypeKind_Flag), intent(in) :: expected_tk
      integer, optional, intent(out) :: rc

      integer :: status

      type(ESMF_TypeKind_Flag) :: found_tk

      call ESMF_FieldGet(x, typekind=found_tk, __RC)

      __ASSERT((found_tk == expected_tk), 'Found incorrect typekind.')
      __RETURN(__SUCCESS)
   end subroutine verify_typekind_scalar

   subroutine verify_typekind_array(x, expected_tk, rc)
      type(ESMF_Field), intent(inout) :: x(:)
      type(ESMF_TypeKind_Flag), intent(in) :: expected_tk
      integer, optional, intent(out) :: rc

      integer :: status
      integer :: i

      do i = 1, size(x)
         call verify_typekind(x(i), expected_tk, __RC)
      end do
      __RETURN(__SUCCESS)
   end subroutine verify_typekind_array

   function is_valid_typekind(actual_tk, valid_tks) result(is_valid)
      type(ESMF_TypeKind_Flag), intent(in) :: actual_tk
      type(ESMF_TypeKind_Flag), intent(in) :: valid_tks(:)
      logical :: is_valid
      integer :: i

      is_valid = .FALSE.
      do i = 1, size(valid_tks)
         is_valid = (actual_tk == valid_tks(i))
         if(is_valid) return
      end do

   end function is_valid_typekind

   subroutine copy_broadcast(x, y, rc)
      type(ESMF_Field), intent(inout) :: x
      type(ESMF_Field), intent(inout) :: y
      integer, optional, intent(out) :: rc

      type(ESMF_TypeKind_Flag) :: tk_x, tk_y
      type(c_ptr) :: cptr_x, cptr_y
      integer(kind=ESMF_KIND_I8) :: n_input,n_extra
      integer :: status
      logical :: conformable, broadcast
      integer, allocatable :: x_shape(:), y_shape(:)
      logical :: x_is_double
      logical :: y_is_double
      character(len=*), parameter :: UNSUPPORTED_TK = &
         'Unsupported typekind in FieldCOPY() for '

      conformable = FieldsAreConformable(x, y)
      if (conformable) then
         call copy(x,y,__RC)
         __RETURN(__SUCCESS)
      end if
      broadcast = FieldsAreBroadcastConformable(x,y)
      __ASSERT(broadcast, 'FieldCopy() - field can not be broadcast.')

      call MAPL_FieldGetLocalElementCount(x,x_shape,__RC)
      call MAPL_FieldGetLocalElementCount(y,y_shape,__RC)
      call FieldGetCptr(x, cptr_x, __RC)
      call ESMF_FieldGet(x, typekind = tk_x, __RC)

      n_input = product(x_shape)
      n_extra = y_shape(size(y_shape))

      call FieldGetCptr(y, cptr_y, __RC)
      call ESMF_FieldGet(y, typekind = tk_y, __RC)

      y_is_double = (tk_y == ESMF_TYPEKIND_R8)
      __ASSERT(y_is_double .or. (tk_y == ESMF_TYPEKIND_R4), UNSUPPORTED_TK//'y.')

      x_is_double = (tk_x == ESMF_TYPEKIND_R8)
      __ASSERT(x_is_double .or. (tk_x == ESMF_TYPEKIND_R4), UNSUPPORTED_TK//'x.')

      if (y_is_double) then
         if (x_is_double) then
            call copy_bcast_r8_r8(cptr_x, cptr_y, n_input,n_extra)
         else
            call copy_bcast_r4_r8(cptr_x, cptr_y, n_input,n_extra)
         end if
      else
         if (x_is_double) then
            call copy_bcast_r8_r4(cptr_x, cptr_y, n_input,n_extra)
         else
            call copy_bcast_r4_r4(cptr_x, cptr_y, n_input,n_extra)
         end if
      end if

      __RETURN(__SUCCESS)
   end subroutine copy_broadcast

   subroutine copy_bcast_r4_r4(cptr_x, cptr_y, n1,n2)
      type(c_ptr), intent(in) :: cptr_x, cptr_y
      integer(ESMF_KIND_I8), intent(in) :: n1,n2

      integer :: i

      real(kind=ESMF_KIND_R4), pointer :: x_ptr(:)
      real(kind=ESMF_KIND_R4), pointer :: y_ptr(:,:)

      call c_f_pointer(cptr_x, x_ptr, [n1])
      call c_f_pointer(cptr_y, y_ptr, [n1,n2])

      do i=1,n2
         y_ptr(:,i) = x_ptr
      enddo
   end subroutine copy_bcast_r4_r4

   subroutine copy_bcast_r4_r8(cptr_x, cptr_y, n1,n2)
      type(c_ptr), intent(in) :: cptr_x, cptr_y
      integer(ESMF_KIND_I8), intent(in) :: n1,n2

      integer :: i

      real(kind=ESMF_KIND_R4), pointer :: x_ptr(:)
      real(kind=ESMF_KIND_R8), pointer :: y_ptr(:,:)

      call c_f_pointer(cptr_x, x_ptr, [n1])
      call c_f_pointer(cptr_y, y_ptr, [n1,n2])

      do i=1,n2
         y_ptr(:,i) = x_ptr
      enddo
   end subroutine copy_bcast_r4_r8

   subroutine copy_bcast_r8_r4(cptr_x, cptr_y, n1,n2)
      type(c_ptr), intent(in) :: cptr_x, cptr_y
      integer(ESMF_KIND_I8), intent(in) :: n1,n2

      integer :: i

      real(kind=ESMF_KIND_R8), pointer :: x_ptr(:)
      real(kind=ESMF_KIND_R4), pointer :: y_ptr(:,:)

      call c_f_pointer(cptr_x, x_ptr, [n1])
      call c_f_pointer(cptr_y, y_ptr, [n1,n2])

      do i=1,n2
         y_ptr(:,i) = x_ptr
      enddo
   end subroutine copy_bcast_r8_r4

   subroutine copy_bcast_r8_r8(cptr_x, cptr_y, n1,n2)
      type(c_ptr), intent(in) :: cptr_x, cptr_y
      integer(ESMF_KIND_I8), intent(in) :: n1,n2

      integer :: i

      real(kind=ESMF_KIND_R8), pointer :: x_ptr(:)
      real(kind=ESMF_KIND_R8), pointer :: y_ptr(:,:)

      call c_f_pointer(cptr_x, x_ptr, [n1])
      call c_f_pointer(cptr_y, y_ptr, [n1,n2])

      do i=1,n2
         y_ptr(:,i) = x_ptr
      enddo
   end subroutine copy_bcast_r8_r8

   subroutine copy(x, y, rc)
      type(ESMF_Field), intent(inout) :: x
      type(ESMF_Field), intent(inout) :: y
      integer, optional, intent(out) :: rc

      type(ESMF_TypeKind_Flag) :: tk_x, tk_y
      type(c_ptr) :: cptr_x, cptr_y
      integer(kind=ESMF_KIND_I8) :: n
      integer :: status
      logical :: conformable
      logical :: x_is_double
      logical :: y_is_double
      character(len=*), parameter :: UNSUPPORTED_TK = &
         'Unsupported typekind in FieldCOPY() for '

      conformable = FieldsAreConformable(x, y)
      !wdb fixme need to pass RC
      __ASSERT(conformable, 'FieldCopy() - fields not conformable.')
      call FieldGetCptr(x, cptr_x, __RC)
      call ESMF_FieldGet(x, typekind = tk_x, __RC)

      n  = FieldGetLocalSize(x, __RC)

      call FieldGetCptr(y, cptr_y, __RC)
      call ESMF_FieldGet(y, typekind = tk_y, __RC)

     !wdb  fixme convert between precisions ? get rid of extra cases
      y_is_double = (tk_y == ESMF_TYPEKIND_R8)
      __ASSERT(y_is_double .or. (tk_y == ESMF_TYPEKIND_R4), UNSUPPORTED_TK//'y.')

      x_is_double = (tk_x == ESMF_TYPEKIND_R8)
      __ASSERT(x_is_double .or. (tk_x == ESMF_TYPEKIND_R4), UNSUPPORTED_TK//'x.')

      if (y_is_double) then
         if (x_is_double) then
            call copy_r8_r8(cptr_x, cptr_y, n)
         else
            call copy_r4_r8(cptr_x, cptr_y, n)
         end if
      else
         if (x_is_double) then
            call copy_r8_r4(cptr_x, cptr_y, n)
         else
            call copy_r4_r4(cptr_x, cptr_y, n)
         end if
      end if

      __RETURN(__SUCCESS)
   end subroutine copy

   subroutine copy_r4_r4(cptr_x, cptr_y, n)
      type(c_ptr), intent(in) :: cptr_x, cptr_y
      integer(ESMF_KIND_I8), intent(in) :: n

      real(kind=ESMF_KIND_R4), pointer :: x_ptr(:)
      real(kind=ESMF_KIND_R4), pointer :: y_ptr(:)

      call c_f_pointer(cptr_x, x_ptr, [n])
      call c_f_pointer(cptr_y, y_ptr, [n])

      y_ptr=x_ptr
   end subroutine copy_r4_r4

   subroutine copy_r4_r8(cptr_x, cptr_y, n)
      type(c_ptr), intent(in) :: cptr_x, cptr_y
      integer(ESMF_KIND_I8), intent(in) :: n

      real(kind=ESMF_KIND_R4), pointer :: x_ptr(:)
      real(kind=ESMF_KIND_R8), pointer :: y_ptr(:)

      call c_f_pointer(cptr_x, x_ptr, [n])
      call c_f_pointer(cptr_y, y_ptr, [n])

      y_ptr=x_ptr
   end subroutine copy_r4_r8

   subroutine copy_r8_r4(cptr_x, cptr_y, n)
      type(c_ptr), intent(in) :: cptr_x, cptr_y
      integer(ESMF_KIND_I8), intent(in) :: n

      real(kind=ESMF_KIND_R8), pointer :: x_ptr(:)
      real(kind=ESMF_KIND_R4), pointer :: y_ptr(:)

      call c_f_pointer(cptr_x, x_ptr, [n])
      call c_f_pointer(cptr_y, y_ptr, [n])

      y_ptr=x_ptr
   end subroutine copy_r8_r4

   subroutine copy_r8_r8(cptr_x, cptr_y, n)
      type(c_ptr), intent(in) :: cptr_x, cptr_y
      integer(ESMF_KIND_I8), intent(in) :: n

      real(kind=ESMF_KIND_R8), pointer :: x_ptr(:)
      real(kind=ESMF_KIND_R8), pointer :: y_ptr(:)

      call c_f_pointer(cptr_x, x_ptr, [n])
      call c_f_pointer(cptr_y, y_ptr, [n])

      y_ptr=x_ptr
   end subroutine copy_r8_r8

! this procedure must go away as soon as ESMF Fixes their bug

  subroutine MAPL_FieldGetLocalElementCount(field,local_count,rc)
     type(ESMF_Field), intent(inout) :: field
     integer, allocatable, intent(out) :: local_count(:)
     integer, optional, intent(out) :: rc

     integer :: status, rank
     type(ESMF_TypeKind_Flag) :: tk

     real(kind=ESMF_KIND_R4), pointer :: r4_1d(:),r4_2d(:,:),r4_3d(:,:,:),r4_4d(:,:,:,:)
     real(kind=ESMF_KIND_R8), pointer :: r8_1d(:),r8_2d(:,:),r8_3d(:,:,:),r8_4d(:,:,:,:)

     call ESMF_FieldGet(field,rank=rank,typekind=tk,__RC)
     if (tk == ESMF_TypeKind_R4) then
        if (rank==1) then
           call ESMF_FieldGet(field,0,farrayptr=r4_1d,__RC)
           local_count = shape(r4_1d)
        else if (rank ==2) then
           call ESMF_FieldGet(field,0,farrayptr=r4_2d,__RC)
           local_count = shape(r4_2d)
        else if (rank ==3) then
           call ESMF_FieldGet(field,0,farrayptr=r4_3d,__RC)
           local_count = shape(r4_3d)
        else if (rank ==4) then
           call ESMF_FieldGet(field,0,farrayptr=r4_4d,__RC)
           local_count = shape(r4_4d)
        else
           __FAIL("Unsupported rank")
        end if
     else if (tk == ESMF_TypeKind_R8) then
        if (rank==1) then
           call ESMF_FieldGet(field,0,farrayptr=r8_1d,__RC)
           local_count = shape(r8_1d)
        else if (rank ==2) then
           call ESMF_FieldGet(field,0,farrayptr=r8_2d,__RC)
           local_count = shape(r8_2d)
        else if (rank ==3) then
           call ESMF_FieldGet(field,0,farrayptr=r8_3d,__RC)
           local_count = shape(r8_3d)
        else if (rank ==4) then
           call ESMF_FieldGet(field,0,farrayptr=r8_4d,__RC)
           local_count = shape(r8_4d)
        else
           __FAIL("Unsupported rank")
        end if
     else
        __FAIL("Unsupported type")
     end if
     __RETURN(__SUCCESS)
  end subroutine MAPL_FieldGetLocalElementCount

  function FieldsHaveUndef(fields,rc) result(all_have_undef)
     logical :: all_have_undef
     type(ESMF_Field), intent(inout) :: fields(:)
     integer, optional, intent(out) :: rc

     integer :: status, i
     logical :: isPresent

     all_have_undef = .true.
     do i =1,size(fields)
        call ESMF_AttributeGet(fields(i),name="missing_value",isPresent=isPresent,__RC)
        all_have_undef = (all_have_undef .and. isPresent)
     enddo
     __RETURN(__SUCCESS)
  end function

  subroutine GetFieldsUndef_r4(fields,undef_values,rc)
     type(ESMF_Field), intent(inout) :: fields(:)
     real(kind=ESMF_KIND_R4), allocatable,intent(inout) :: undef_values(:)
     integer, optional, intent(out) :: rc

     integer :: status, i
     logical :: isPresent

     allocate(undef_values(size(fields)))
     do i =1,size(fields)
        call ESMF_AttributeGet(fields(i),name="missing_value",isPresent=isPresent,__RC)
        __ASSERT(isPresent,"missing undef value")
        call ESMF_AttributeGet(fields(i),value=undef_values(i),name="missing_value",__RC)
     enddo
     __RETURN(__SUCCESS)
  end subroutine GetFieldsUndef_r4

  subroutine GetFieldsUndef_r8(fields,undef_values,rc)
     type(ESMF_Field), intent(inout) :: fields(:)
     real(kind=ESMF_KIND_R8), allocatable,intent(inout) :: undef_values(:)
     integer, optional, intent(out) :: rc

     integer :: status, i
     logical :: isPresent

     allocate(undef_values(size(fields)))
     do i =1,size(fields)
        call ESMF_AttributeGet(fields(i),name="missing_value",isPresent=isPresent,__RC)
        __ASSERT(isPresent,"missing undef value")
        call ESMF_AttributeGet(fields(i),value=undef_values(i),name="missing_value",__RC)
     enddo
     __RETURN(__SUCCESS)
  end subroutine GetFieldsUndef_r8

subroutine Destroy(Field,RC)
    type(ESMF_Field),          intent(INOUT) :: Field
    integer, optional,         intent(OUT  ) :: RC

    integer                               :: STATUS

    real(kind=ESMF_KIND_R4), pointer      :: VR4_1D(:), VR4_2D(:,:), VR4_3D(:,:,:), VR4_4D(:,:,:,:)
    real(kind=ESMF_KIND_R8), pointer      :: VR8_1D(:), VR8_2D(:,:), VR8_3D(:,:,:), VR8_4D(:,:,:,:)
    integer                      :: rank
    type(ESMF_TypeKind_Flag)     :: tk
    logical :: esmf_allocated

    call ESMF_FieldGet(Field,typekind=tk,dimCount=rank,isESMFAllocated=esmf_allocated,__RC)
    if (.not. esmf_allocated) then
       if (tk == ESMF_TYPEKIND_R4 .and. rank == 1) then
          call ESMF_FieldGet(Field,0,VR4_1d,__RC)
          deallocate(VR4_1d,__STAT)
       else if (tk == ESMF_TYPEKIND_R8 .and. rank == 1) then
          call ESMF_FieldGet(Field,0,VR8_1d,__RC)
          deallocate(VR8_1d,__STAT)
       else if (tk == ESMF_TYPEKIND_R4 .and. rank == 2) then
          call ESMF_FieldGet(Field,0,VR4_2d,__RC)
          deallocate(VR4_2d,__STAT)
       else if (tk == ESMF_TYPEKIND_R8 .and. rank == 2) then
          call ESMF_FieldGet(Field,0,VR8_2d,__RC)
          deallocate(VR8_2d,__STAT)
       else if (tk == ESMF_TYPEKIND_R4 .and. rank == 3) then
          call ESMF_FieldGet(Field,0,VR4_3D,__RC)
          deallocate(VR4_3d,__STAT)
       else if (tk == ESMF_TYPEKIND_R8 .and. rank == 3) then
          call ESMF_FieldGet(Field,0,VR8_3D,__RC)
          deallocate(VR8_3d,__STAT)
       else if (tk == ESMF_TYPEKIND_R4 .and. rank == 4) then
          call ESMF_FieldGet(Field,0,VR4_4D,__RC)
          deallocate(VR4_3d,__STAT)
       else if (tk == ESMF_TYPEKIND_R8 .and. rank == 4) then
          call ESMF_FieldGet(Field,0,VR8_4D,__RC)
          deallocate(VR8_3d,__STAT)
       else
          __FAIL( 'unsupported typekind+rank')
       end if
    end if
    call ESMF_FieldDestroy(Field,noGarbage = .true., rc=status)
    __VERIFY(STATUS)
    __RETURN(ESMF_SUCCESS)

  end subroutine Destroy
end module
