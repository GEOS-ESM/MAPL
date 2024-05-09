module mapl3g_ungridded_dim_info

   use esmf, only: ESMF_InfoGet

   implicit none
   private

   public :: UngriddedDimInfo
   public :: UngriddedDimsInfo
   public :: operator(<)
   public :: operator(==)
   
   type :: UngriddedDimInfo
      character(len=:), allocatable :: name
      character(len=:), allocatable :: units
      real :: coordinates(:)
   contains
      procedure, private :: name_units
      procedure, private :: size
   end type UngriddedDimInfo

   interface UngriddedDimInfo
      module procedure :: construct
   end interface UngriddedDimInfo

   interface UngriddedDimsInfo
      module procedure :: get_array
   end interface UngriddedDimsInfo

   interface operator(<)
      module procedure :: less
   end interface operator(<)

   interface operator(==)
      module procedure :: equal
   end interface operator(==)

   interface operator(.chlt.)
      module procedure :: name_units_less
   end interface operator(.chlt.)

   interface operator(.cheq.)
      module procedure :: name_units_equal
   end interface operator(.cheq.)

   interface operator(.rlt.)
      module procedure :: coordinates_less
   end interface operator(.rlt.)

   interface operator(.req.)
      module procedure :: coordinates_equal
   end interface operator(.req.)

contains

   function construct(info_in, unit_prefix, rc) result(obj)
      type(UngriddedDimInfo) :: obj
      type(ESMF_Info), intent(in) :: info_in
      character(len=*), intent(in) :: unit_prefix
      integer, optional, intent(out) :: rc
      integer :: status
      character(len=:), allocatable :: vloc
      character(len=:), allocatable :: name
      character(len=:), allocatable :: units
      real :: coordinates(:)

      call ESMF_InfoGet(info_in, key=unit_prefix//'name', name, _RC)
      call ESMF_InfoGet(info_in, key=unit_prefix//'units', units, _RC)
      call ESMF_InfoGet(info_in, key=unit_prefix//'coordinates', coordinates, _RC)
      obj%name = name
      obj%units = units
      obj%coordinates = coordinates

      _RETURN(_SUCCESS)
   end function construct
 
   function name_units(this) result(nu)
      character(len=:), allocatable :: nu
      class(UngriddedDimInfo), intent(in) :: this

      nu = this%name // this%units

   end function name_units

   integer function size(this)
      class(UngriddedDimInfo), intent(in) :: this

      size = size(a%coordinates)

   end function size

   function get_array(info_in, rc) result(array)
      type(UngriddedDimInfo), allocatable = array(:)
      type(ESMF_Info), intent(in) :: info_in
      integer, optional, intent(out) :: rc
      character(len=*), parameter :: PREFIX = 'MAPL/'
      integer :: status
      integer :: num_ungridded
      integer :: i, ios
      character(len=32) :: stri

      call ESMF_InfoGet(info_in, key=PREFIX // 'num_ungridded', num_ungridded, _RC)
      _ASSERT(num_ungridded >= 0, 'num_ungridded must be nonnegative.')
      allocate(array(num_ungridded))
      if(num_ungridded == 0) then
         _RETURN(_SUCCESS)
      end if
      do i= 1, num_ungridded
         write(stri, fmt='(I0)', iostat=ios) i
         _ASSERT(ios == 0, 'failed to create ith ungridded dim index string')
         array(i) = UngriddedDimInfo(info_in, PREFIX // 'dims_' // trim(adjustl(stri)) // '/')
      end do

      _RETURN(_SUCCESS)

   end function get_array

   logical function equal(a, b) result(t)
      class(UngriddedDimInfo), intent(in) :: a, b

      t = (a .cheq. b) .and. (a .req. b)

   end function equal

   logical function less(a, b) result(t)
      class(UngriddedDimInfo), intent(in) :: a, b

      t = a .chlt. b
      if(t .or. (b .chlt. a)) return 
      t = a .rlt. b

   end function less

   logical function name_units_equal(a, b) result(t)
      class(UngriddedDimInfo), intent(in) :: a, b
      
      t = a%name_units() == b%name_units()

   end function name_units_equal

   logical function name_units_less(a, b) result(t)
      class(UngriddedDimInfo), intent(in) :: a, b

      t = a%name_units() < b%name_units()

   end function name_units_less

   logical function coordinates_equal(a, b) result(t)
      class(UngriddedDimInfo), intent(in) :: a, b

      t = a%size() == b%size()
      if(t) t = all(a%coordinates == b%coordinates)

   end function coordinates_equal

   logical function coordinates_less(a, b) result(t)
      class(UngriddedDimInfo), intent(in) :: a, b
      logical, allocatable :: lt(:), gt(:)
      integer :: i, n

      n = a%size()
      t = n < b%size()
      if(t .or. n > b%size()) return
      lt = a%coordinates < b%coordinates
      gt = a%coordinates > b%coordinates
      do i=1, n
         t = lt(i)
         if(t .or. gt(i)) return
      end do

   end function coordinates_less

end module mapl3g_ungridded_dim_info
