! Export umbrella for the MAPL infrastructure/regridder_mgr layer.
! Public API exposed to external consumers.
module mapl_regridder_mgr_api

   use mapl_Regridder_mod, only: mapl_Regridder => Regridder
   use mapl_RegridderManager_mod, only: mapl_RegridderManager => RegridderManager
   use mapl_RegridderManager_mod, only: mapl_regridder_manager => regridder_manager
   use mapl_RegridderManager_mod, only: mapl_get_regridder_manager => get_regridder_manager
   use mapl_RegridderSpec_mod, only: mapl_RegridderSpec => RegridderSpec
   use mapl_RegridderMethods_mod, only: &
        MAPL_REGRID_HINT_LOCAL => REGRID_HINT_LOCAL, &
        MAPL_REGRID_HINT_FILE_WEIGHTS => REGRID_HINT_FILE_WEIGHTS, &
        MAPL_REGRID_HINT_COMPUTE_TRANSPOSE => REGRID_HINT_COMPUTE_TRANSPOSE, &
        MAPL_REGRID_METHOD_BILINEAR => REGRID_METHOD_BILINEAR, &
        MAPL_REGRID_METHOD_BILINEAR_MONOTONIC => REGRID_METHOD_BILINEAR_MONOTONIC, &
        MAPL_REGRID_METHOD_BILINEAR_ROTATE => REGRID_METHOD_BILINEAR_ROTATE, &
        MAPL_REGRID_METHOD_CONSERVE => REGRID_METHOD_CONSERVE, &
        MAPL_REGRID_METHOD_CONSERVE_MONOTONIC => REGRID_METHOD_CONSERVE_MONOTONIC, &
        MAPL_REGRID_METHOD_VOTE => REGRID_METHOD_VOTE, &
        MAPL_REGRID_METHOD_FRACTION => REGRID_METHOD_FRACTION, &
        MAPL_REGRID_METHOD_CONSERVE_2ND => REGRID_METHOD_CONSERVE_2ND, &
        MAPL_REGRID_METHOD_PATCH => REGRID_METHOD_PATCH, &
        MAPL_REGRID_METHOD_NEAREST_STOD => REGRID_METHOD_NEAREST_STOD, &
        MAPL_REGRID_METHOD_CONSERVE_HFLUX => REGRID_METHOD_CONSERVE_HFLUX, &
        MAPL_UNSPECIFIED_REGRID_METHOD => UNSPECIFIED_REGRID_METHOD, &
        mapl_regrid_method_string_to_int => regrid_method_string_to_int, &
        mapl_regrid_method_int_to_string => regrid_method_int_to_string, &
        mapl_generate_esmf_regrid_param => generate_esmf_regrid_param

   use mapl_EsmfRegridder_mod, only: mapl_EsmfRegridderParam => EsmfRegridderParam
   implicit none
   private

   ! Regridder types
   public :: mapl_Regridder
   public :: mapl_RegridderManager
   public :: mapl_regridder_manager
   public :: mapl_get_regridder_manager
   public :: mapl_RegridderSpec
   public :: mapl_esmfRegridderParam

   ! Regrid methods and hints
   public :: MAPL_REGRID_HINT_LOCAL
   public :: MAPL_REGRID_HINT_FILE_WEIGHTS
   public :: MAPL_REGRID_HINT_COMPUTE_TRANSPOSE
   public :: MAPL_REGRID_METHOD_BILINEAR
   public :: MAPL_REGRID_METHOD_BILINEAR_MONOTONIC
   public :: MAPL_REGRID_METHOD_BILINEAR_ROTATE
   public :: MAPL_REGRID_METHOD_CONSERVE
   public :: MAPL_REGRID_METHOD_CONSERVE_MONOTONIC
   public :: MAPL_REGRID_METHOD_VOTE
   public :: MAPL_REGRID_METHOD_FRACTION
   public :: MAPL_REGRID_METHOD_CONSERVE_2ND
   public :: MAPL_REGRID_METHOD_PATCH
   public :: MAPL_REGRID_METHOD_NEAREST_STOD
   public :: MAPL_REGRID_METHOD_CONSERVE_HFLUX
   public :: MAPL_UNSPECIFIED_REGRID_METHOD
   public :: mapl_regrid_method_string_to_int
   public :: mapl_regrid_method_int_to_string
   public :: mapl_generate_esmf_regrid_param


end module mapl_regridder_mgr_api
