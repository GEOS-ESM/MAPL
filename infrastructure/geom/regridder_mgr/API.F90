module mapl_RegridderMgr_API
   use mapl_Regridder, only: Regridder
   use mapl_RegridderManager, only: RegridderManager, regridder_manager, get_regridder_manager
   use mapl_RegridderSpec, only: RegridderSpec
   use mapl_RegridderMethods, only: &
        REGRID_HINT_LOCAL, &
        REGRID_HINT_FILE_WEIGHTS, &
        REGRID_HINT_COMPUTE_TRANSPOSE, &
        REGRID_METHOD_BILINEAR, &
        REGRID_METHOD_BILINEAR_MONOTONIC, &
        REGRID_METHOD_BILINEAR_ROTATE, &
        REGRID_METHOD_CONSERVE, &
        REGRID_METHOD_CONSERVE_MONOTONIC, &
        REGRID_METHOD_VOTE, &
        REGRID_METHOD_FRACTION, &
        REGRID_METHOD_CONSERVE_2ND, &
        REGRID_METHOD_PATCH, &
        REGRID_METHOD_NEAREST_STOD, &
        REGRID_METHOD_CONSERVE_HFLUX, &
        UNSPECIFIED_REGRID_METHOD, &
        regrid_method_string_to_int, &
        regrid_method_int_to_string, &
        generate_esmf_regrid_param

   implicit none(type,external)
   private

   public :: Regridder
   public :: RegridderManager, regridder_manager, get_regridder_manager
   public :: RegridderSpec
   public :: REGRID_HINT_LOCAL
   public :: REGRID_HINT_FILE_WEIGHTS
   public :: REGRID_HINT_COMPUTE_TRANSPOSE
   public :: REGRID_METHOD_BILINEAR
   public :: REGRID_METHOD_BILINEAR_MONOTONIC
   public :: REGRID_METHOD_BILINEAR_ROTATE
   public :: REGRID_METHOD_CONSERVE
   public :: REGRID_METHOD_CONSERVE_MONOTONIC
   public :: REGRID_METHOD_VOTE
   public :: REGRID_METHOD_FRACTION
   public :: REGRID_METHOD_CONSERVE_2ND
   public :: REGRID_METHOD_PATCH
   public :: REGRID_METHOD_NEAREST_STOD
   public :: REGRID_METHOD_CONSERVE_HFLUX
   public :: UNSPECIFIED_REGRID_METHOD
   public :: regrid_method_string_to_int
   public :: regrid_method_int_to_string
   public :: generate_esmf_regrid_param

end module mapl_RegridderMgr_API
