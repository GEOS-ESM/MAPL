#include "MAPL_Generic.h"
module mapl3g_output_info

   use mapl3g_ungridded_dim_info
   use esmf, only: ESMF_Info, ESMF_InfoGet, ESMF_InfoGetCharAlloc
   use Mapl_ErrorHandling

   implicit none
   private

   public :: OutputInfo
   public :: operator(<)
   public :: operator(==)

   type :: OutputInfo
      integer :: num_levels
      character(len=:), allocatable :: vloc
      type(UngriddedDimInfo), allocatable :: ungridded_dims(:)
   contains
      procedure :: num_ungridded
   end type OutputInfo

   interface OutputInfo
      module procedure :: construct_object
   end interface OutputInfo

   interface operator(<)
      module procedure :: less
   end interface

   interface operator(==)
      module procedure :: equal
   end interface

   character(len=*), parameter :: PREFIX = 'MAPL/'

contains

   function construct_object(info, rc) result(obj)
      type(OutputInfo) :: obj
      type(ESMF_Info), intent(in) :: info
      integer, optional, intent(out) :: rc
      integer :: status
      integer :: num_levels, num_ungridded
      character(len=:), allocatable :: vloc

      call ESMF_InfoGet(info, PREFIX // 'num_levels', num_levels, _RC)
      _HERE, 'num_levels = ', num_levels
      call ESMF_InfoGetCharAlloc(info, PREFIX // 'vloc', vloc, _RC)
      _HERE, 'vloc = ', vloc
      call ESMF_InfoGet(info, PREFIX // 'num_ungridded', num_ungridded, _RC)
      _HERE, 'num_ungridded = ', num_ungridded

      obj%num_levels = num_levels
      obj%vloc = vloc
      obj%ungridded_dims = UngriddedDimsInfo(info, _RC)
      _ASSERT(size(obj%ungridded_dims) == num_ungridded, 'Size of ungridded_dims does not match num_ungridded info.')

      _HERE, 'Exiting construct_object'
      _RETURN(_SUCCESS)

   end function construct_object

   integer function num_ungridded(this)
      class(OutputInfo), intent(in) :: this

      num_ungridded = size(this%ungridded_dims)

   end function num_ungridded

   logical function less(a, b) result(t)
      class(OutputInfo), intent(in) :: a, b
      
      t = a%num_levels < b%num_levels
      if(t .or. a%num_levels > b%num_levels) return
      t = a%vloc < b%vloc
      if(t .or. a%vloc > b%vloc) return
      t = ungridded_dims_less(a, b)

   end function less

   logical function not_equal(a, b) result(t)
      class(OutputInfo), intent(in) :: a, b

      t = .not. (a == b)

   end function not_equal

   logical function equal(a, b) result(t)
      class(OutputInfo), intent(in) :: a, b

      t = a%num_levels == b%num_levels .and. a%vloc == b%vloc .and. ungridded_dims_equal(a, b)

   end function equal

   logical function ungridded_dims_less(a, b) result(t)
      class(OutputInfo), intent(in) :: a, b
      logical, allocatable :: lt(:), gt(:)
      integer :: i, n, nb

      n = a%num_ungridded()
      nb = b%num_ungridded()
      t = n < nb
      if(t .or. (nb < n)) return
      lt = a%ungridded_dims < b%ungridded_dims
      gt = b%ungridded_dims < a%ungridded_dims
      do i=1, n
         t = lt(i)
         if(t .or. gt(i)) return
      end do

   end function ungridded_dims_less

   logical function ungridded_dims_equal(a, b) result(t)
      class(OutputInfo), intent(in) :: a, b

      t = (a%num_ungridded() == b%num_ungridded()) .and. all(a%ungridded_dims == b%ungridded_dims)

   end function ungridded_dims_equal

end module mapl3g_output_info
