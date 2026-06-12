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
   public :: KEY_UNGRIDDED_NAME
   public :: KEY_UNGRIDDED_UNITS
   public :: KEY_UNGRIDDED_COORD
   public :: KEY_DIM_STRINGS
   public :: KEY_VERT_STAGGERLOC
   public :: KEY_BRACKET_UPDATED
   public :: KEY_VECTOR_BASIS_KIND
   public :: KEY_ALLOCATION_STATUS
   public :: KEY_CONSERVATION_METADATA
   public :: KEY_FIELDBUNDLETYPE_FLAG
   public :: KEY_HAS_GEOM
   public :: KEY_NORMALIZATION_METADATA
   public :: KEY_QUANTITY_TYPE_METADATA
   public :: VAR_LIST_KEY
   public :: KEY_TIMESTEP
   public :: KEY_REF_DATETIME
   public :: KEY_ACCUMULATION_TYPE
   public :: KEY_TIME_SPEC
   public :: KEY_SOURCE
   public :: KEY_INSTANTANEOUS
   public :: KEY_REGRID
   public :: KEY_ATTRIBUTES
   public :: DELIMITER
   public :: KEY_FILL_VALUE
   public :: KEY_HAS_DEFERRED_ASPECTS
   public :: KEY_HORIZONTAL_DIMS_SPEC
   public :: KEY_MISSING_VALUE
   public :: KEY_REGRIDDER_PARAM
   public :: KEY_RESTART_MODE
   public :: KEY_UNDEF_VALUE
   public :: KEY_VERT_ALIGNMENT
   public :: KEY_VGRID_ID

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

   ! UngriddedDim info keys
   character(len=*), parameter :: KEY_UNGRIDDED_NAME = 'name'
   character(len=*), parameter :: KEY_UNGRIDDED_UNITS = 'units'
   character(len=*), parameter :: KEY_UNGRIDDED_COORD = 'coordinates'

    ! Regridding info keys
    character(len=*), parameter :: KEY_BRACKET_UPDATED = '/bracket_updated'
    character(len=*), parameter :: KEY_VECTOR_BASIS_KIND = '/vector_basis_kind'

   ! FieldBundleSpec info keys
   character(len=*), parameter :: KEY_ALLOCATION_STATUS = "/allocation_status"
   character(len=*), parameter :: KEY_CONSERVATION_METADATA = "/conservation_metadata"
   character(len=*), parameter :: KEY_FIELDBUNDLETYPE_FLAG = '/MAPL_FieldBundleType_Flag'
   character(len=*), parameter :: KEY_HAS_GEOM = "/has_geom"
   character(len=*), parameter :: KEY_NORMALIZATION_METADATA = "/normalization_metadata"
   character(len=*), parameter :: KEY_QUANTITY_TYPE_METADATA = "/quantity_type_metadata"

   ! History Constants keys
   character(len=*), parameter :: VAR_LIST_KEY = 'var_list'
   character(len=*), parameter :: KEY_TIMESTEP = 'frequency'
   character(len=*), parameter :: KEY_REF_DATETIME = 'ref_datetime'
   character(len=*), parameter :: KEY_ACCUMULATION_TYPE = 'mode'
   character(len=*), parameter :: KEY_TIME_SPEC = 'time_spec'
   character(len=*), parameter :: KEY_SOURCE = 'source'
   character(len=*), parameter :: KEY_INSTANTANEOUS = 'instantaneous'
   character(len=*), parameter :: KEY_REGRID = 'regrid'

   ! FieldInfo keys
   character(len=*), parameter :: KEY_ATTRIBUTES = '/attributes'
   character(len=*), parameter :: DELIMITER = '/'
   character(len=*), parameter :: KEY_FILL_VALUE = '/_FillValue'
   character(len=*), parameter :: KEY_HAS_DEFERRED_ASPECTS = '/has_deferred_aspects'
   character(len=*), parameter :: KEY_HORIZONTAL_DIMS_SPEC = '/horizontal_dims_spec'
   character(len=*), parameter :: KEY_MISSING_VALUE = '/missing_value'
   character(len=*), parameter :: KEY_REGRIDDER_PARAM = '/EsmfRegridderParam'
   character(len=*), parameter :: KEY_RESTART_MODE = '/restart_mode'
   character(len=*), parameter :: KEY_UNDEF_VALUE = '/undef_value'
   character(len=*), parameter :: KEY_VERT_ALIGNMENT = '/vert_alignment'
   character(len=*), parameter :: KEY_VGRID_ID = '/vgrid_id'

   character(len=*), parameter :: KEY_TYPEKIND = '/typekind'
   character(len=*), parameter :: KEY_FIELD_PROTOTYPE = '/field_prototype'
   character(len=*), parameter :: KEY_INTERPOLATION_WEIGHTS = '/interpolation_weights'
   character(len=*), parameter :: KEY_FIELDBUNDLETYPE = '/fieldBundleType'

end module mapl_esmf_info_keys_mod
