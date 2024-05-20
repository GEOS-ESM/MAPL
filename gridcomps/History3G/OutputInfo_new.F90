module mapl3g_OutputInfo

   use esmf, only: ESMF_InfoGet

   implicit none
   private

   public :: OutputInfo
!   public :: operator(==)
!   public :: operator(/=)
   public :: operator(<)

   type :: OutputInfo
      integer :: num_levels
      character(len=:), allocatable :: vloc
      type(UngriddedDimInfo) :: ungridded_dims(:)
   end type OutputInfo

   interface OutputInfo
      module procedure :: construct_output_info
   end interface OutputInfo

!   interface operator(==)
!      module procedure :: equal_to_output_info
!      module procedure :: equal_to_ungridded_dim_info
!   end interface operator(==)
!
!   interface operator(/=)
!      module procedure :: not_equal_to_output_info
!      module procedure :: not_equal_to_ungridded_dim_info
!   end interface operator(/=)

   interface operator(<)
      module procedure :: less_than_output_info
      module procedure :: less_than_ungridded_dim_info
   end interface operator(<)

   type :: UngriddedDimInfo
      character(len=:), allocatable :: name
      character(len=:), allocatable :: units
      real :: coordinates(:)
   end type UngriddedDimInfo

!   type, abstract :: InfoKey
!      character(len=:), allocatable :: string_key
!   end type InfoKey
!
!   type, extends(InfoKey) :: OutputInfoKey
!      integer :: num_levels
!      type(UngriddedInfoKey), allocatable :: ungridded_dims_info(:)
!   end type OutputInfoKey
   
   character(len=*), parameter :: PREFIX = 'MAPL/'
   character(len=*), parameter :: NUM_LEVELS_KEY = PREFIX // 'num_levels'
   character(len=*), parameter :: VLOC_KEY = PREFIX // 'vloc'
   character(len=*), parameter :: UNGRIDDED_DIM_KEY = PREFIX // "dim_"
   character(len=*), parameter :: NAME_KEY = 'name'
   character(len=*), parameter :: UNITS_KEY = 'units'
   character(len=*), parameter :: COORDINATES_KEY = 'coordinates'

contains

!   function get_key_output_info(this) result(key)
!      type(OutputInfoKey) :: key
!      type(OutputInfo), intent(in) :: this
!
!      key%integer_key = [this%num_levels]
!      key%
   function construct_output_info(info_in, rc) result(output_info)
      type(OutputInfo) :: output_info
      type(ESMF_Info), intent(in) :: info_in
      integer, optional, intent(out) :: rc
      integer :: status
      character(len=:), allocatable :: num_levels
      character(len=:), allocatable :: vloc
      character(len=:), allocatable :: name
      character(len=:), allocatable :: units
      real :: coordinates(:)

      call ESMF_InfoGet(info_in, key=NUM_LEVELS_KEY, num_levels, _RC)
      call ESMF_InfoGet(info_in, key=VLOC_KEY, vloc, _RC)
      call ESMF_InfoGet(info_in, key=UNGRIDDED_KEY, ungridded, _RC)

      output_info%num_levels = num_levels
      output_info%vloc = vloc
      output_info%ungridded_dims = get_ungridded_dims_info(info_in, _RC)

      _RETURN(_SUCCESS)
   end function construct_output_info

   function construct_ungridded_dim_info(info_in, prefix, rc) result(info_out)
      type(UngriddedDimInfo) :: info_out
      type(ESMF_Info), intent(in) :: info_in
      character(len=*), intent(in) :: prefix
      integer, optional, intent(out) :: rc
      integer :: status
      character(len=:), allocatable :: vloc
      character(len=:), allocatable :: name
      character(len=:), allocatable :: units
      real :: coordinates(:)

      call ESMF_InfoGet(info_in, key=prefix//NAME_KEY, name, _RC)
      call ESMF_InfoGet(info_in, key=prefix//UNITS_KEY, units, _RC)
      call ESMF_InfoGet(info_in, key=prefix//COORDINATES_KEY, coordinates, _RC)
      info_out%name = name
      info_out%units = units
      info_out%coordinates = coordinates

      _RETURN(_SUCCESS)
   end function construct_ungridded_dim_info

   function get_ungridded_dims_info(info_in, rc) result(info_out)
      type(UngriddedDimInfo), allocatable = info_out(:)
      type(ESMF_Info), intent(in) :: info_in
      integer, optional, intent(out) :: rc
      integer :: status
      integer :: num_ungridded
      integer :: i, ios
      character(len=32) :: stri
      character(len=:), allocatable :: prefix

      call ESMF_InfoGet(info_in, key=NUM_UNGRIDDED_KEY, num_ungridded, _RC)
      _ASSERT(num_ungridded >= 0, 'num_ungridded must be nonnegative.')
      allocate(info_out(num_ungridded))
      if(num_ungridded == 0) then
         _RETURN(_SUCCESS)
      end if
      do i= 1, num_ungridded
         write(stri, fmt='(I0)', iostat=ios) i
         _ASSERT(ios == 0, 'failed to create ith ungridded dim index string')
         prefix = UNGRIDDED_DIM_KEY // trim(adjustl(stri)) // '/'
         info_out(i) = UngriddedDimInfo(info_in, prefix)
      end do

      _RETURN(_SUCCESS)

   end function get_ungridded_dims_info

!   logical function equal_to_output_info(a, b) result(equal)
!      class(OutputInfo), intent(in) :: a, b
!
!      integer :: num_levels
!      character(len=:), allocatable :: vloc
!      type(UngriddedDimInfo) :: ungridded_dims(:)
!      equal = a%num_levels == b%num_levels .and. a%vloc == b%vloc .and. &
!         all(a%ungridded_dims == b%ungridded_dims)
!
!   end function equal_to_output_info
!
!   logical function not_equal_to_output_info(a, b) result(not_equal)
!      class(OutputInfo), intent(in) :: a, b
!
!      not_equal = .not. (a == b)
!
!   end function not_equal_to_output_info
!
!   logical function equal_to_ungridded_dim_info(a, b) result(equal)
!      class(UngriddedDimInfo), intent(in) :: a, b
!
!         equal = a%name == b%name .and. a%units == b%units .and. &
!            all(a%coordinates == b%coordinates)
!
!   end function equal_to_ungridded_dim_info
!
!   logical function not_equal_to_ungridded_dim_info(a, b) result(not_equal)
!      class(UngriddedDimInfo), intent(in) :: a, b
!
!      not_equal = .not. (a == b)
!
!   end function not_equal_to_ungridded_dim_info

   logical function less_than_output_info(a, b) result(tval)
      type(OutputInfo), intent(in) :: a, b
      integer :: i
      
      tval = a%num_levels < b%num_levels
      if(tval .or. a%num_levels > b%num_levels) return
      tval = a%vloc < b%vloc
      if(tval .or. a%vloc > b%vloc) return
      tval = size(a%ungridded_dims) < size(b%ungridded_dims)
      if(tval .or. size(a%ungridded_dims) > size(b%ungridded_dims)) return
      do i= 1, size(a%ungridded_dims)
         tval = a%ungridded_dims(i) < b%ungridded_dims(i)
         if(tval .or. a%ungridded_dims(i) > b%ungridded_dims(i)) return
      end do

   end function less_than_output_info

   logical function less_than_ungridded_dim_info(a, b) result(eval)
      type(UngriddedDimInfo), intent(in) :: a, b
      integer :: i, asz, bsz
      real :: acoor, bcoor

      tval = a%name < b%name
      if(tval .or. a%name > b%name) return
      tval = a%units < b%units
      if(tval .or. a%units > b%units) return
      asz = size(a%coordinates)
      bsz = size(b%coordinates)
      tval = asz < bsz
      if(tval .or. asz > bsz) return
      do i=1, asz
         acoor = a%coordinates(i)
         bcoor = b%coordinates(i)
         tval = acoor < bcoor
         if(tval .or. acoor > bcoor) return
      end do

   end function less_than_ungridded_dim_info

end module mapl3g_OutputInfo
