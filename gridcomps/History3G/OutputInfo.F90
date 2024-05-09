module mapl3g_OutputInfo

   use mapl3g_ungridded_dim_info
   use esmf, only: ESMF_InfoGet

   implicit none
   private

   public :: OutputInfo
   public :: operator(<)
   public :: operator(==)

   type :: OutputInfo
      integer :: num_levels
      character(len=:), allocatable :: vloc
      type(UngriddedDimInfo) :: ungridded_dims(:)
   contains
      module procedure :: num_ungridded
   end type OutputInfo

   interface OutputInfo
      module procedure :: construct_object
   end interface OutputInfo

   interface operator(<)
      module procedure :: less
   end interface operator(<)

   interface operator(==)
      module procedure :: equal
   end interface operator(==)

   interface operator(/=)
      module procedure :: not_equal
   end interface operator(/=)

   character(len=*), parameter :: PREFIX = 'MAPL/'

contains

   function construct_object(info_in, rc) result(obj)
      type(OutputInfo) :: obj
      type(ESMF_Info), intent(in) :: info_in
      integer, optional, intent(out) :: rc
      integer :: status
      integer :: num_levels, num_ungridded
      character(len=:), allocatable :: vloc

      call ESMF_InfoGet(info_in, key=PREFIX // 'num_levels', num_levels, _RC)
      call ESMF_InfoGet(info_in, key=PREFIX // 'vloc', vloc, _RC)
      call ESMF_InfoGet(info_in, key=PREFIX // 'num_ungridded', num_ungridded, _RC)

      obj%num_levels = num_levels
      obj%vloc = vloc
      obj%ungridded_dims = UngriddedDimsInfo(info_in, _RC)
      _ASSERT(size(obj%ungridded_dims) == num_ungridded, 'Size of ungridded_dims does not match num_ungridded info.')

      _RETURN(_SUCCESS)

   end function construct_object

   integer function num_ungridded(this)
      class(OutputInfo), intent(in) :: this

      num_ungridded = size(this%ungridded_dims)

   end function num_ungridded

   logical function less(a, b) result(t)
      class(OutputInfo), intent(in) :: a, b
      integer :: i
      logical, allocatable :: lt(:), gt(:)
      
      t = a%num_levels < b%num_levels
      if(t .or. a%num_levels > b%num_levels) return
      t = a%vloc < b%vloc
      if(t .or. a%vloc > b%vloc) return
      t = a%num_ungridded() < b%num_ungridded()
      if(t .or. a%num_ungridded() > b%num_ungridded()) return
      lt = a%ungridded_dims < b%ungridded_dims
      gt = a%ungridded_dims > b%ungridded_dims
      do i= 1, a%num_ungridded
         t = lt(i)
         if(t .or. gt(i)) return
      end do

   end function less

   logical function not_equal(a, b) result(t)
      class(OutputInfo), intent(in) :: a, b

      t = .not (a == b)

   end function not_equal

   logical function equal(a, b) result(t)
      class(OutputInfo), intent(in) :: a, b

      t = .not. (a /= b)
      t = a%num_levels == b%num_levels .and. a%vloc == b%vloc .and. &
         a%num_ungridded() == b%num_ungridded() .and. all(a%ungridded_dims == b%UngriddedDimInfo)

   end function equal

end module mapl3g_OutputInfo
