#include "MAPL_Generic.h"
module mapl3g_ungridded_dim_info

   use esmf, only: ESMF_Info, ESMF_InfoGet, ESMF_InfoGetCharAlloc, ESMF_InfoCreate, ESMF_InfoDestroy
   use Mapl_ErrorHandling

   implicit none
   private

   public :: UngriddedDimInfo
   public :: UngriddedDimsInfo
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

   interface UngriddedDimsInfo
      module procedure :: get_array
   end interface UngriddedDimsInfo

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

   function construct_ungridded_dim_info(info_in, rc) result(obj)
      type(UngriddedDimInfo) :: obj
      type(ESMF_Info), intent(in) :: info_in
      integer, optional, intent(out) :: rc
      integer :: status
      character(len=:), allocatable :: name
      character(len=:), allocatable :: units
      real, allocatable :: coordinates(:)
      integer :: sz

      call ESMF_InfoGetCharAlloc(info_in, key='name', value=name, _RC)
      call ESMF_InfoGetCharAlloc(info_in, key='units', value=units, _RC)
      call ESMF_InfoGet(info_in, key='coordinates', size=sz, _RC)
      allocate(coordinates(sz))
      call ESMF_InfoGet(info_in, key='coordinates', values=coordinates, _RC)
      obj%name = name
      obj%units = units
      obj%coordinates = coordinates

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

   function get_array(info, rc) result(array)
      type(UngriddedDimInfo), allocatable :: array(:)
      type(ESMF_Info), intent(in) :: info
      integer, optional, intent(out) :: rc
      integer :: status
      integer :: num_ungridded
      integer :: i, ios
      character(len=32) :: stri
      type(ESMF_Info) :: info_unit

      call ESMF_InfoGet(info, KEY_NUM_UNGRID, num_ungridded, _RC)
      _ASSERT(num_ungridded >= 0, 'num_ungridded must be nonnegative.')
      allocate(array(num_ungridded))
      if(num_ungridded == 0) then
         _RETURN(_SUCCESS)
      end if
      do i= 1, num_ungridded
         write(stri, fmt='(I0)', iostat=ios) i
         _ASSERT(ios == 0, 'failed to create ith ungridded dim index string')
         info_unit = ESMF_InfoCreate(info, key=KEYSTUB_DIM // trim(adjustl(stri)), _RC)
         array(i) = UngriddedDimInfo(info_unit, _RC)
         call ESMF_InfoDestroy(info_unit, _RC)
      end do

      _RETURN(_SUCCESS)

   end function get_array

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
