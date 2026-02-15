#include "MAPL.h"

module mapl3g_UngriddedDims
   use mapl3g_InfoUtilities
   use mapl3g_ESMF_Info_Keys
   use mapl3g_UngriddedDimVector
   use mapl3g_UngriddedDim
   use mapl3g_LU_Bound
   use mapl_ErrorHandling
   use esmf, only: ESMF_Info
   use esmf, only: ESMF_InfoCreate
   use esmf, only: ESMF_InfoSet
   use esmf, only: ESMF_InfoDestroy
   implicit none

   private

   public :: UngriddedDims
   public :: make_UngriddedDims
   public :: operator(==)
   public :: operator(/=)

   ! Note: GEOS convention is that the vertical dim spec should be
   ! before any other ungridded dim specs.
   type :: UngriddedDims
      private
      logical :: is_mirror = .false.
      type(UngriddedDimVector) :: dim_specs
   contains
      procedure :: add_dim
      procedure :: get_num_ungridded
      procedure :: get_ith_dim_spec
      procedure :: get_bounds
      procedure :: make_info
   end type UngriddedDims

   interface UngriddedDims
      module procedure new_UngriddedDims_empty
      module procedure new_UngriddedDims_vec
      module procedure new_UngriddedDims_arr
      module procedure new_UngriddedDims_extent_arr
   end interface UngriddedDims

   interface operator(==)
      module procedure equal_to
   end interface operator(==)

   interface operator(/=)
      module procedure not_equal_to
   end interface operator(/=)


contains

   function new_UngriddedDims_empty(is_mirror) result(spec)
      type(UngriddedDims) :: spec
      logical, optional, intent(in) :: is_mirror

      spec%dim_specs = UngriddedDimVector()
      if (present(is_mirror)) then
         spec%is_mirror = is_mirror
      end if

   end function new_UngriddedDims_empty

   pure function new_UngriddedDims_vec(dim_specs) result(spec)
      type(UngriddedDims) :: spec
      type(UngriddedDimVector), intent(in) :: dim_specs

      spec%dim_specs = dim_specs

   end function new_UngriddedDims_vec


   function new_UngriddedDims_arr(dim_specs) result(spec)
      type(UngriddedDims) :: spec
      type(UngriddedDim), intent(in) :: dim_specs(:)

      integer :: i

      do i = 1, size(dim_specs)
         call spec%dim_specs%push_back(dim_specs(i))
      end do

   end function new_UngriddedDims_arr

   function new_UngriddedDims_extent_arr(extent_arr) result(spec)
      type(UngriddedDims) :: spec
      integer, intent(in) :: extent_arr(:)

      integer :: i
      type(UngriddedDim) :: dim_spec

      do i = 1, size(extent_arr)
         dim_spec = UngriddedDim(extent_arr(i))
         call spec%dim_specs%push_back(dim_spec)
      end do
   end function new_UngriddedDims_extent_arr

   ! Note: Ensure that vertical is the first ungridded dimension.
   subroutine add_dim(this, dim_spec, rc)
      class(UngriddedDims), intent(inout) :: this
      type(UngriddedDim), intent(in) :: dim_spec
      integer, optional, intent(out) :: rc

      integer :: status
      if (dim_spec%get_name() == 'levels') then
         _ASSERT(this%get_num_ungridded() == 0, 'vertical levels must be 1st ungridded dimension.')
      end if
      call this%dim_specs%push_back(dim_spec)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(status)
   end subroutine add_dim

   pure integer function get_num_ungridded(this)
      class(UngriddedDims), intent(in) :: this

      get_num_ungridded = this%dim_specs%size()

   end function get_num_ungridded


   function get_ith_dim_spec(this, i, rc) result(dim_spec)
      type(UngriddedDim), pointer :: dim_spec
      class(UngriddedDims), target, intent(in) :: this
      integer, intent(in) :: i
      integer, optional, intent(out) :: rc

      integer :: status

      dim_spec => this%dim_specs%at(i, _RC)
      _RETURN(_SUCCESS)

   end function get_ith_dim_spec


   function get_bounds(this) result(bounds)
      type(LU_Bound), allocatable :: bounds(:)
      class(UngriddedDims), intent(in) :: this

      integer :: i
      class(UngriddedDim), pointer :: dim_spec

      allocate(bounds(this%get_num_ungridded()))
      do i = 1, this%get_num_ungridded()
         dim_spec => this%dim_specs%of(i)
         bounds(i) = dim_spec%get_bounds()
      end do

   end function get_bounds

   logical function equal_to(a, b)
      type(UngriddedDims), intent(in) :: a
      type(UngriddedDims), intent(in) :: b

      integer :: i

      equal_to = .false.

      if (a%is_mirror .neqv. b%is_mirror) return
      associate (n => a%dim_specs%size())

        if (b%dim_specs%size() /= n) return
        do i = 1, n
           if (a%dim_specs%of(i) /= b%dim_specs%of(i)) return
        end do

      end associate

      equal_to = .true.

   end function equal_to


   logical function not_equal_to(a, b)
      type(UngriddedDims), intent(in) :: a
      type(UngriddedDims), intent(in) :: b

      not_equal_to = .not. (a == b)

   end function not_equal_to

   function make_info(this, rc)  result(info)
      type(ESMF_Info) :: info
      class(UngriddedDims), target, intent(in) :: this
      integer, optional, intent(out) :: rc

      integer :: status
      integer :: i
      type(UngriddedDim), pointer :: dim_spec
      type(ESMF_Info) :: dim_info
      character(:), allocatable :: dim_key

      info = ESMF_InfoCreate(_RC)
      call MAPL_InfoSet(info, key='/num_ungridded_dimensions', value=this%get_num_ungridded(), _RC)

      do i = 1, this%get_num_ungridded()
         dim_spec => this%get_ith_dim_spec(i, _RC)
         dim_info = dim_spec%make_info(_RC)

         dim_key = make_dim_key(i)
         call ESMF_InfoSet(info, key=dim_key, value=dim_info, _RC)
         call ESMF_InfoDestroy(dim_info, _RC)
      end do


      _RETURN(_SUCCESS)
   end function make_info

   function make_ungriddedDims(info, key, rc) result(ungridded_dims)
      type(UngriddedDims) :: ungridded_dims
      type(ESMF_Info), intent(in) :: info
      character(*), optional, intent(in) :: key
      integer, optional, intent(out) :: rc

      integer :: status
      integer :: num_ungridded_dims
      integer :: i
      type(ESMF_Info) :: dim_info
      character(:), allocatable :: dim_key
      type(UngriddedDim), allocatable :: dim_specs(:)
      character(:), allocatable :: full_key

      ungridded_dims = UngriddedDims()
      full_key = KEY_NUM_UNGRIDDED_DIMS
      if (present(key)) then
         full_key = key // full_key
      end if

      call MAPL_InfoGet(info, key=full_key, value=num_ungridded_dims, _RC)
      allocate(dim_specs(num_ungridded_dims))

      do i = 1, num_ungridded_dims
         dim_key = make_dim_key(i, _RC)
         if (present(key)) then
            dim_key = key // dim_key
         end if
         dim_info = ESMF_InfoCreate(info, key=dim_key, _RC)
         dim_specs(i) = make_ungriddedDim(dim_info, _RC)
         call ESMF_InfoDestroy(dim_info, _RC)
      end do

      ungridded_dims = UngriddedDims(dim_specs)

      _RETURN(_SUCCESS)
   end function make_ungriddedDims

end module mapl3g_UngriddedDims

