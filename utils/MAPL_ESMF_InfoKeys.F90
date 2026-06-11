#include "MAPL.h"
module mapl_esmf_info_keys_mod

   use mapl_ErrorHandling_mod

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
   public :: KEY_VERT_STAGGERLOC
   public :: KEY_BRACKET_UPDATED
   public :: KEY_VECTOR_BASIS_KIND
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
    character(len=*), parameter :: KEY_BRACKET_UPDATED = '/bracket_updated'
    character(len=*), parameter :: KEY_VECTOR_BASIS_KIND = '/vector_basis_kind'

    character(len=*), parameter :: KEY_DIM_STRINGS(9) = [ &
      KEYSTUB_DIM // '1', KEYSTUB_DIM // '2', KEYSTUB_DIM // '3', &
      KEYSTUB_DIM // '4', KEYSTUB_DIM // '5', KEYSTUB_DIM // '6', &
      KEYSTUB_DIM // '7', KEYSTUB_DIM // '8', KEYSTUB_DIM // '9']

   character(len=*), parameter :: KEY_TYPEKIND = '/typekind'
   character(len=*), parameter :: KEY_FIELD_PROTOTYPE = '/field_prototype'
   character(len=*), parameter :: KEY_INTERPOLATION_WEIGHTS = '/interpolation_weights'
   character(len=*), parameter :: KEY_FIELDBUNDLETYPE = '/fieldBundleType'

end module mapl_esmf_info_keys_mod
