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

contains

   function construct_ungridded_dims_info(info) result(self)
      type(UngriddedDimsInfo) :: self
      type(ESMF_Info), intent(in) :: info
      type(UngriddedDimInfo) :: array(:)


      self%array = array

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

end module mapl3g_ungridded_dims_info
