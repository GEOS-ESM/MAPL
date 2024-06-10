#include "MAPL_Exceptions.h"
module mapl3g_esmf_info_keys

   use MAPL_ErrorHandling

   implicit none

   public :: KEY_UNGRIDDED_DIMS
   public :: KEY_VERT_DIM
   public :: KEY_VERT_GEOM
   public :: KEY_UNITS
   public :: KEY_LONG_NAME
   public :: KEY_STANDARD_NAME
   public :: KEY_NUM_LEVELS
   public :: KEY_VLOC
   public :: KEY_NUM_UNGRID_DIMS
   public :: KEYSTUB_DIM
   public :: KEY_UNGRIDDED_NAME
   public :: KEY_UNGRIDDED_UNITS
   public :: KEY_UNGRIDDED_COORD
   public :: KEY_DIM_STRINGS
   public :: make_dim_key
   private

   ! FieldSpec info keys
   character(len=*), parameter :: PREFIX = 'MAPL/'
   character(len=*), parameter :: KEY_UNGRIDDED_DIMS = PREFIX // 'ungridded_dims/'
   character(len=*), parameter :: KEY_VERT_DIM = PREFIX // 'vertical_dim/'
   character(len=*), parameter :: KEY_VERT_GEOM = PREFIX // 'vertical_geom/'
   character(len=*), parameter :: KEY_UNITS = PREFIX // 'units'
   character(len=*), parameter :: KEY_LONG_NAME = PREFIX // 'long_name'
   character(len=*), parameter :: KEY_STANDARD_NAME = PREFIX // 'standard_name'

   ! VerticalGeom info keys
   character(len=*), parameter :: KEY_NUM_LEVELS = KEY_VERT_GEOM // 'num_levels'

   ! VerticalDimSpec info keys
   character(len=*), parameter :: KEY_VLOC = KEY_VERT_DIM // 'vloc'

   ! UngriddedDims info keys
   character(len=*), parameter :: KEY_NUM_UNGRID_DIMS = KEY_UNGRIDDED_DIMS // 'num_ungridded_dimensions'
   character(len=*), parameter :: KEYSTUB_DIM = KEY_UNGRIDDED_DIMS // 'dim_'

   ! UngriddedDim info keys
   character(len=*), parameter :: KEY_UNGRIDDED_NAME = 'name'
   character(len=*), parameter :: KEY_UNGRIDDED_UNITS = 'units'
   character(len=*), parameter :: KEY_UNGRIDDED_COORD = 'coordinates'

   character(len=*), parameter :: KEY_DIM_STRINGS(9) = [ &
      KEYSTUB_DIM // '1', KEYSTUB_DIM // '2', KEYSTUB_DIM // '3', &
      KEYSTUB_DIM // '4', KEYSTUB_DIM // '5', KEYSTUB_DIM // '6', &
      KEYSTUB_DIM // '7', KEYSTUB_DIM // '8', KEYSTUB_DIM // '9']

contains

   function make_dim_key(n, rc) result(key)
      character(len=:), allocatable :: key
      integer, intent(in) :: n
      integer, optional, intent(out) :: rc
      integer :: status
      character(len=32) :: raw

      key = ''
      _ASSERT(n > 0, 'Index must be positive.')
      if(n <= size(KEY_DIM_STRINGS)) then
         key = KEY_DIM_STRINGS(n)
         _RETURN(_SUCCESS)
      end if
      write(raw, fmt='(I0)', iostat=status) n
      _ASSERT(status == 0, 'Write failed')
      key = KEYSTUB_DIM // trim(raw)
      _RETURN(_SUCCESS)

   end function make_dim_key

end module mapl3g_esmf_info_keys
