#include "MAPL_Generic.h"
module mapl3g_output_info

   use mapl3g_ungridded_dim_info
   use esmf, only: ESMF_Info, ESMF_InfoGet, ESMF_InfoGetCharAlloc, ESMF_InfoCreate, ESMF_InfoDestroy
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
      module procedure :: construct_output_info
   end interface OutputInfo

   interface operator(<)
      module procedure :: less
   end interface

   interface operator(==)
      module procedure :: equal
   end interface

   character(len=*), parameter :: PREFIX = 'MAPL/'
   character(len=*), parameter :: KEY_UNGRID_DIM = PREFIX // 'ungridded_dims'
   character(len=*), parameter :: KEY_VERT_DIM = PREFIX // 'vertical_dim'
   character(len=*), parameter :: KEY_VERT_GEOM = PREFIX // 'vertical_geom'
   character(len=*), parameter :: KEY_UNITS = PREFIX // 'units'
   character(len=*), parameter :: KEY_VLOC = 'vloc'
   character(len=*), parameter :: KEY_NUM_LEVELS = 'num_levels'

contains

   function construct_output_info(info, rc) result(obj)
      type(OutputInfo) :: obj
      type(ESMF_Info), intent(in) :: info
      integer, optional, intent(out) :: rc
      integer :: status
      integer :: num_levels
      character(len=:), allocatable :: vloc
      type(ESMF_Info) :: inner_info

      inner_info = ESMF_InfoCreate(info, key=KEY_UNGRID_DIM, _RC)
      obj%ungridded_dims = UngriddedDimsInfo(inner_info, _RC)
      call ESMF_InfoDestroy(inner_info, _RC)

      inner_info = ESMF_InfoCreate(info, key=KEY_VERT_DIM, _RC)
      call ESMF_InfoGetCharAlloc(inner_info, key=KEY_VLOC, value=vloc, _RC)
      obj%vloc = vloc
      call ESMF_InfoDestroy(inner_info, _RC)

      inner_info = ESMF_InfoCreate(info, key=KEY_VERT_GEOM, _RC)
      call ESMF_InfoGet(inner_info, key=KEY_NUM_LEVELS, value=num_levels, _RC)
      obj%num_levels = num_levels
      call ESMF_InfoDestroy(inner_info, _RC)

      _HERE, 'Exiting construct_output_info'
      _RETURN(_SUCCESS)

   end function construct_output_info

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
