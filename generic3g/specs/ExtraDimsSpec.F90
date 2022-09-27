#include "MAPL_Generic.h"

module mapl3g_ExtraDimsSpec
   use mapl3g_DimSpecVector
   use mapl3g_UngriddedDimSpec
   use mapl_ErrorHandling
   implicit none

   private

   public :: ExtraDimsSpec
   public :: operator(==)
   public :: operator(/=)

   ! Note: GEOS convention is that the vertical dim spec should be
   ! before any other ungridded dim specs.
   type :: ExtraDimsSpec
      private
      type(DimSpecVector) :: dim_specs
   contains
      procedure :: add_dim_spec
      procedure :: get_num_ungridded
      procedure :: get_ith_dim_spec
      procedure :: get_lbounds
      procedure :: get_ubounds
   end type ExtraDimsSpec

   interface ExtraDimsSpec
      module procedure new_ExtraDimsSpec_empty
      module procedure new_ExtraDimsSpec_vec
      module procedure new_ExtraDimsSpec_arr
   end interface ExtraDimsSpec

   interface operator(==)
      module procedure equal_to
   end interface operator(==)

   interface operator(/=)
      module procedure not_equal_to
   end interface operator(/=)


contains


   function new_ExtraDimsSpec_empty() result(spec)
      type(ExtraDimsSpec) :: spec

      spec%dim_specs = DimSpecVector()

   end function new_ExtraDimsSpec_empty

   pure function new_ExtraDimsSpec_vec(dim_specs) result(spec)
      type(ExtraDimsSpec) :: spec
      type(DimSpecVector), intent(in) :: dim_specs

      spec%dim_specs = dim_specs

   end function new_ExtraDimsSpec_vec


   function new_ExtraDimsSpec_arr(dim_specs) result(spec)
      type(ExtraDimsSpec) :: spec
      type(UngriddedDimSpec), intent(in) :: dim_specs(:)

      integer :: i

      do i = 1, size(dim_specs)
         call spec%dim_specs%push_back(dim_specs(i))
      end do

   end function new_ExtraDimsSpec_arr


   ! Note: Ensure that vertical is the first ungridded dimension.
   subroutine add_dim_spec(this, dim_spec, rc)
      class(ExtraDimsSpec), intent(inout) :: this
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
      class(ExtraDimsSpec), intent(in) :: this

      get_num_ungridded = this%dim_specs%size()
      
   end function get_num_ungridded


   function get_ith_dim_spec(this, i, rc) result(dim_spec)
      type(UngriddedDimSpec), pointer :: dim_spec
      class(ExtraDimsSpec), target, intent(in) :: this
      integer, intent(in) :: i
      integer, optional, intent(out) :: rc

      integer :: status
      
      dim_spec => this%dim_specs%at(i, _RC)
      _RETURN(_SUCCESS)

   end function get_ith_dim_spec


   function get_lbounds(this) result(lbounds)
      integer, allocatable :: lbounds(:)
      class(ExtraDimsSpec), intent(in) :: this

      integer :: i
      class(UngriddedDimSpec), pointer :: dim_spec

      allocate(lbounds(this%get_num_ungridded()))
      do i = 1, this%get_num_ungridded()
         dim_spec => this%dim_specs%of(i)
         lbounds(i) = dim_spec%get_lbound()
      end do

   end function get_lbounds


   function get_ubounds(this) result(ubounds)
      integer, allocatable :: ubounds(:)
      class(ExtraDimsSpec), intent(in) :: this

      integer :: i
      class(UngriddedDimSpec), pointer :: dim_spec
      
      allocate(ubounds(this%get_num_ungridded()))
      do i = 1, this%get_num_ungridded()
         dim_spec => this%dim_specs%of(i)
         ubounds(i) = dim_spec%get_ubound()
      end do

   end function get_ubounds


   logical function equal_to(a, b)
      type(ExtraDimsSpec), intent(in) :: a
      type(ExtraDimsSpec), intent(in) :: b

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
      type(ExtraDimsSpec), intent(in) :: a
      type(ExtraDimsSpec), intent(in) :: b

      not_equal_to = .not. (a == b)

   end function not_equal_to

end module mapl3g_ExtraDimsSpec

