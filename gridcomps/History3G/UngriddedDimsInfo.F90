#include "MAPL_Generic.h"
module mapl3g_ungridded_dims_info

   use mapl3g_ungridded_dim_info
   use mapl3g_ungridded_dim_set
   use esmf, only: ESMF_Info
   use Mapl_ErrorHandling

   implicit none

   public :: UngriddedDimsInfo
   public :: UngriddedDimInfo
   public :: UngriddedDimInfoSet
   private

   type :: UngriddedDimsInfo
      private
      type(UngriddedDimInfo), allocatable :: array(:)
   contains
      procedure :: as_set => ungridded_dims_info_as_set
      procedure :: as_array => ungridded_dims_info_as_array
   end type UngriddedDimsInfo

   interface UngriddedDimsInfo
      module procedure :: construct_ungridded_dims_info
   end interface UngriddedDimsInfo

   character(len=*), parameter :: KEY_NUM_UNGRID_DIMS = 'num_ungridded_dimensions'
   character(len=*), parameter :: KEYSTUB_DIM = 'dim_'

contains

   function construct_ungridded_dims_info(info, rc) result(self)
      type(UngriddedDimsInfo) :: self
      type(ESMF_Info), intent(in) :: info
      integer, optional, intent(out) :: rc
      integer :: status

      self%array = get_array(info, _RC)

   end function construct_ungridded_dims_info

   function ungridded_dims_info_as_set(this) result(as_set)
      type(UngriddedDimSet) :: as_set
      class(UngriddedDimsInfo), intent(in) :: this

      as_set = UngriddedDimSet(this%as_array())

   end function ungridded_dims_info_as_set

   function ungridded_dims_info_as_array(this) result(as_array)
      type(UngriddedDim) :: as_array(:)
      class(UngriddedDimsInfo), intent(in) :: this

      as_array = this%array

   end function ungridded_dims_info_as_array

   function get_array(info, rc) result(array)
      type(UngriddedDimInfo), allocatable :: array(:)
      type(ESMF_Info), intent(in) :: info
      integer, optional, intent(out) :: rc
      integer :: status
      
      integer :: num_ungridded
      integer :: i, ios
      character(len=32) :: stri
      type(UngriddedDimInfo), allocatable :: array(:)

      call ESMF_InfoGet(info, KEY_NUM_UNGRID_DIMS, num_ungridded, _RC)
      _ASSERT(num_ungridded >= 0, 'num_ungridded must be nonnegative.')
      allocate(array(num_ungridded))
      if(num_ungridded == 0) then
         _RETURN(_SUCCESS)
      end if
      do i= 1, num_ungridded
         write(stri, fmt='(I0)', iostat=ios) i
         _ASSERT(ios == 0, 'failed to create ith ungridded dim index string')
         array(i) = UngriddedDimInfo(info, KEYSTUB_DIM // trim(adjustl(stri)) // '/')
      end do

      _RETURN(_SUCCESS)

   end function get_array

end module mapl3g_ungridded_dims_info
