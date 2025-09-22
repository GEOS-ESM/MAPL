#include "MAPL_Exceptions.h"
module mapl3g_esmf_info_keys

   use MAPL_ErrorHandling

   implicit none

   public :: INFO_SHARED_NAMESPACE
   public :: INFO_PRIVATE_NAMESPACE
   public :: INFO_INTERNAL_NAMESPACE
   public :: KEY_UNGRIDDED_DIMS
   public :: KEY_VERT_DIM
   public :: KEY_VERT_GRID
   public :: KEY_INTERPOLATION_WEIGHTS
   public :: KEY_FIELD_PROTOTYPE
   public :: KEY_FIELDBUNDLETYPE
   public :: KEY_UNITS
   public :: KEY_LONG_NAME
   public :: KEY_STANDARD_NAME
   public :: KEY_TYPEKIND
   public :: KEY_NUM_LEVELS
   public :: KEY_VLOC
   public :: KEY_NUM_UNGRIDDED_DIMS
   public :: KEYSTUB_DIM
   public :: KEY_UNGRIDDED_NAME
   public :: KEY_UNGRIDDED_UNITS
   public :: KEY_UNGRIDDED_COORD
   public :: KEY_DIM_STRINGS
   public :: make_dim_key
   public :: KEY_VERT_STAGGERLOC
   public :: KEY_REGRID_TRANSFORM
   private

   ! FieldSpec info keys
   character(len=*), parameter :: PREFIX = '/MAPL'
   character(len=*), parameter :: INFO_SHARED_NAMESPACE   = PREFIX // '/shared'
   character(len=*), parameter :: INFO_PRIVATE_NAMESPACE  = PREFIX // '/private'
   character(len=*), parameter :: INFO_INTERNAL_NAMESPACE = PREFIX // '/internal'

   character(len=*), parameter :: KEY_UNGRIDDED_DIMS = '/ungridded_dims'
   character(len=*), parameter :: KEY_VERT_DIM = '/vertical_dim'
   character(len=*), parameter :: KEY_VERT_GRID = '/vertical_grid'
   character(len=*), parameter :: KEY_UNITS = '/units'
   character(len=*), parameter :: KEY_LONG_NAME = '/long_name'
   character(len=*), parameter :: KEY_STANDARD_NAME = '/standard_name'

   ! VerticalGeom info keys
   character(len=*), parameter :: KEY_NUM_LEVELS = KEY_VERT_GRID // '/num_levels'

   ! VerticalDimSpec info keys
   character(len=*), parameter :: KEY_VLOC = KEY_VERT_DIM // '/vloc'
   character(len=*), parameter :: KEY_VERT_STAGGERLOC = "/vert_staggerloc"


   ! UngriddedDims info keys
   character(len=*), parameter :: KEY_NUM_UNGRIDDED_DIMS = '/num_ungridded_dimensions'
   character(len=*), parameter :: KEYSTUB_DIM = '/dim_'

   ! UngriddedDim info keys
   character(len=*), parameter :: KEY_UNGRIDDED_NAME = 'name'
   character(len=*), parameter :: KEY_UNGRIDDED_UNITS = 'units'
   character(len=*), parameter :: KEY_UNGRIDDED_COORD = 'coordinates'

   ! Regridding info keys
   character(len=*), parameter :: KEY_REGRID_TRANSFORM = '/regrid_transform'

   character(len=*), parameter :: KEY_DIM_STRINGS(9) = [ &
      KEYSTUB_DIM // '1', KEYSTUB_DIM // '2', KEYSTUB_DIM // '3', &
      KEYSTUB_DIM // '4', KEYSTUB_DIM // '5', KEYSTUB_DIM // '6', &
      KEYSTUB_DIM // '7', KEYSTUB_DIM // '8', KEYSTUB_DIM // '9']

   character(len=*), parameter :: KEY_TYPEKIND = '/typekind'
   character(len=*), parameter :: KEY_FIELD_PROTOTYPE = '/field_prototype'
   character(len=*), parameter :: KEY_INTERPOLATION_WEIGHTS = '/interpolation_weights'
   character(len=*), parameter :: KEY_FIELDBUNDLETYPE = '/fieldBundleType'

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
