#include "MAPL_Generic.h"
module mapl3g_ungridded_dim_info

   use esmf, only: ESMF_Info, ESMF_InfoGet, ESMF_InfoGetCharAlloc, ESMF_InfoCreate, ESMF_InfoDestroy
   use Mapl_ErrorHandling

   implicit none

   public :: UngriddedDimInfo
   public :: operator(<)
   public :: operator(==)
   
   type :: UngriddedDimInfo
      character(len=:), allocatable :: name
      character(len=:), allocatable :: units
      real, allocatable :: coordinates(:)
   contains
      procedure :: name_units
      procedure :: coordinate_dims
   end type UngriddedDimInfo

   interface UngriddedDimInfo
      module procedure :: construct_ungridded_dim_info
   end interface UngriddedDimInfo

   interface operator(<)
      module procedure :: less
   end interface

   interface operator(==)
      module procedure :: equal
   end interface

   character(len=*), parameter :: KEY_NUM_UNGRID = 'num_ungridded_dimensions'
   character(len=*), parameter :: KEYSTUB_DIM = 'dim_'
   character(len=*), parameter :: KEY_NAME = 'name'
   character(len=*), parameter :: KEY_UNITS = 'units'
   character(len=*), parameter :: KEY_COORS = 'coordinates'

contains

   function construct_ungridded_dim_info(info, rc) result(ud_info)
      type(UngriddedDimInfo) :: ud_info
      type(ESMF_Info), intent(in) :: info
      integer, optional, intent(out) :: rc
      integer :: status
      character(len=:), allocatable :: name
      character(len=:), allocatable :: units
      real, allocatable :: coordinates(:)
      integer :: sz

      allocate(coordinates(sz))
      call ESMF_InfoGet(info, key='coordinates', values=coordinates, _RC)
      ud_info%name = name
      ud_info%units = units
      ud_info%coordinates = coordinates

      _RETURN(_SUCCESS)
      
   end function construct_ungridded_dim_info
 
   pure function name_units(this) result(nu)
      character(len=:), allocatable :: nu
      class(UngriddedDimInfo), intent(in) :: this

      nu = this%name // this%units

   end function name_units

   pure integer function coordinate_dims(this)
      class(UngriddedDimInfo), intent(in) :: this
      real, allocatable :: coordinates(:)

      coordinates = this%coordinates
      coordinate_dims = size(coordinates)

   end function coordinate_dims

   elemental function equal(a, b) result(t)
      logical :: t
      class(UngriddedDimInfo), intent(in) :: a, b

      t = name_units_equal(a, b) .and. coordinates_equal(a, b)

   end function equal

   elemental function less(a, b) result(t)
      logical :: t
      class(UngriddedDimInfo), intent(in) :: a, b

      t = name_units_less(a, b)
      if(t .or. name_units_less(b, a)) return 
      t = coordinates_less(a, b)

   end function less

   elemental function name_units_equal(a, b) result(t)
      logical :: t
      class(UngriddedDimInfo), intent(in) :: a, b
      
      t = a%name_units() == b%name_units()

   end function name_units_equal

   elemental function name_units_less(a, b) result(t)
      logical :: t
      class(UngriddedDimInfo), intent(in) :: a, b

      t = a%name_units() < b%name_units()

   end function name_units_less

   elemental function coordinates_equal(a, b) result(t)
      logical :: t
      class(UngriddedDimInfo), intent(in) :: a, b

      t = a%coordinate_dims() == b%coordinate_dims()
      if(t) t = all(a%coordinates == b%coordinates)

   end function coordinates_equal

   elemental function coordinates_less(a, b) result(t)
      logical :: t
      class(UngriddedDimInfo), intent(in) :: a, b
      logical, allocatable :: lt(:), gt(:)
      integer :: i, n

      n = a%coordinate_dims()
      t = n < b%coordinate_dims()
      if(t .or. n > b%coordinate_dims()) return
      lt = a%coordinates < b%coordinates
      gt = a%coordinates > b%coordinates
      do i=1, n
         t = lt(i)
         if(t .or. gt(i)) return
      end do

   end function coordinates_less

end module mapl3g_ungridded_dim_info
