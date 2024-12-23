#include "MAPL_ErrLog.h"

submodule (mapl3g_CoordinateAxis) get_coordinates_dim_smod
   use esmf, only: ESMF_UtilStringLowerCase
   use mapl_ErrorHandling
   use gftl2_StringVector
   use, intrinsic :: iso_fortran_env, only: REAL32, REAL64

contains
   
   module function get_coordinates_dim(file_metadata, dim_name, rc) result(coordinates)
      real(kind=R8), dimension(:), allocatable :: coordinates
      type(FileMetadata), intent(in) :: file_metadata
      character(len=*), intent(in) :: dim_name
      integer, optional, intent(out) :: rc

      integer :: status
      class (CoordinateVariable), pointer :: v
      class (*), pointer :: ptr(:)

      v => file_metadata%get_coordinate_variable(dim_name, _RC)
      ptr => v%get_coordinate_data()
      _ASSERT(associated(ptr),'coordinate data not allocated')

      select type (ptr)
      type is (real(kind=REAL64))
         coordinates = ptr
      type is (real(kind=REAL32))
         coordinates = ptr
      class default
         _FAIL('unsuppoted kind for coordinate data -- must be REAL32 or REAL64')
      end select

      _RETURN(_SUCCESS)
   end function get_coordinates_dim

end submodule get_coordinates_dim_smod
