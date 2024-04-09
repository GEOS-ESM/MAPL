#include "MAPL_Generic.h"

module mapl3g_UngriddedDimsSpec
   use mapl3g_DimSpecVector
   use mapl3g_UngriddedDimSpec
   use mapl3g_LU_Bound
   use mapl_ErrorHandling
   use esmf, only: ESMF_Info
   use esmf, only: ESMF_InfoCreate
   use esmf, only: ESMF_InfoSet
   use esmf, only: ESMF_InfoDestroy
   implicit none

   private

   public :: UngriddedDimsSpec
   public :: operator(==)
   public :: operator(/=)

   ! Note: GEOS convention is that the vertical dim spec should be
   ! before any other ungridded dim specs.
   type :: UngriddedDimsSpec
      private
      type(DimSpecVector) :: dim_specs
   contains
      procedure :: add_dim_spec
      procedure :: get_num_ungridded
      procedure :: get_ith_dim_spec
      procedure :: get_bounds
      procedure :: make_info
   end type UngriddedDimsSpec

   interface UngriddedDimsSpec
      module procedure new_UngriddedDimsSpec_empty
      module procedure new_UngriddedDimsSpec_vec
      module procedure new_UngriddedDimsSpec_arr
   end interface UngriddedDimsSpec

   interface operator(==)
      module procedure equal_to
   end interface operator(==)

   interface operator(/=)
      module procedure not_equal_to
   end interface operator(/=)


contains


   function new_UngriddedDimsSpec_empty() result(spec)
      type(UngriddedDimsSpec) :: spec

      spec%dim_specs = DimSpecVector()

   end function new_UngriddedDimsSpec_empty

   pure function new_UngriddedDimsSpec_vec(dim_specs) result(spec)
      type(UngriddedDimsSpec) :: spec
      type(DimSpecVector), intent(in) :: dim_specs

      spec%dim_specs = dim_specs

   end function new_UngriddedDimsSpec_vec


   function new_UngriddedDimsSpec_arr(dim_specs) result(spec)
      type(UngriddedDimsSpec) :: spec
      type(UngriddedDimSpec), intent(in) :: dim_specs(:)

      integer :: i

      do i = 1, size(dim_specs)
         call spec%dim_specs%push_back(dim_specs(i))
      end do

   end function new_UngriddedDimsSpec_arr


   ! Note: Ensure that vertical is the first ungridded dimension.
   subroutine add_dim_spec(this, dim_spec, rc)
      class(UngriddedDimsSpec), intent(inout) :: this
      type(UngriddedDimSpec), intent(in) :: dim_spec
      integer, optional, intent(out) :: rc

      integer :: status
      if (dim_spec%get_name() == 'levels') then
         _ASSERT(this%get_num_ungridded() == 0, 'vertical levels must be 1st ungridded dimension.')
      end if
      call this%dim_specs%push_back(dim_spec)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(status)
   end subroutine add_dim_spec

   pure integer function get_num_ungridded(this)
      class(UngriddedDimsSpec), intent(in) :: this

      get_num_ungridded = this%dim_specs%size()
      
   end function get_num_ungridded


   function get_ith_dim_spec(this, i, rc) result(dim_spec)
      type(UngriddedDimSpec), pointer :: dim_spec
      class(UngriddedDimsSpec), target, intent(in) :: this
      integer, intent(in) :: i
      integer, optional, intent(out) :: rc

      integer :: status
      
      dim_spec => this%dim_specs%at(i, _RC)
      _RETURN(_SUCCESS)

   end function get_ith_dim_spec


   function get_bounds(this) result(bounds)
      type(LU_Bound), allocatable :: bounds(:)
      class(UngriddedDimsSpec), intent(in) :: this

      integer :: i
      class(UngriddedDimSpec), pointer :: dim_spec

      allocate(bounds(this%get_num_ungridded()))
      do i = 1, this%get_num_ungridded()
         dim_spec => this%dim_specs%of(i)
         bounds(i) = dim_spec%get_bounds()
      end do

   end function get_bounds

   logical function equal_to(a, b)
      type(UngriddedDimsSpec), intent(in) :: a
      type(UngriddedDimsSpec), intent(in) :: b

      integer :: i

      equal_to = .false.
      associate (n => a%dim_specs%size())
      
        if (b%dim_specs%size() /= n) return
        do i = 1, n
           if (a%dim_specs%of(i) /= b%dim_specs%of(i)) return
        end do

      end associate

      equal_to = .true.
        
   end function equal_to


   logical function not_equal_to(a, b)
      type(UngriddedDimsSpec), intent(in) :: a
      type(UngriddedDimsSpec), intent(in) :: b

      not_equal_to = .not. (a == b)

   end function not_equal_to

   function make_info(this, rc)  result(info)
      type(ESMF_Info) :: info
      class(UngriddedDimsSpec), target, intent(in) :: this
      integer, optional, intent(out) :: rc

      integer :: status
      integer :: i
      type(UngriddedDimSpec), pointer :: dim_spec
      type(ESMF_Info) :: dim_info
      character(5) :: dim_key

      info = ESMF_InfoCreate(_RC)
      call ESMF_InfoSet(info, key='num_ungridded_dimensions', value=this%get_num_ungridded(), _RC)

      do i = 1, this%get_num_ungridded()
         dim_spec => this%get_ith_dim_spec(i, _RC)
         dim_info = dim_spec%make_info(_RC)

         write(dim_key, '("dim_", i0)') i
         call ESMF_InfoSet(info, key=dim_key, value=dim_info, _RC)
         call ESMF_InfoDestroy(dim_info, _RC)
      end do


      _RETURN(_SUCCESS)
   end function make_info

end module mapl3g_UngriddedDimsSpec

