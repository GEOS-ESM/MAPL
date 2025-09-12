#include "MAPL.h"
module mapl3g_RegridderMethods
   use ESMF
   use mapl3g_EsmfRegridder
   use mapl3g_DynamicMask
   use mapl_ErrorHandlingMod
   use MAPL_Constants, only: MAPL_UNDEFINED_REAL, MAPL_UNDEFINED_REAL64
   implicit none
   private

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

   enum, bind(c)
      enumerator :: REGRID_METHOD_BILINEAR
      enumerator :: REGRID_METHOD_BILINEAR_ROTATE
      enumerator :: REGRID_METHOD_CONSERVE
      enumerator :: REGRID_METHOD_VOTE
      enumerator :: REGRID_METHOD_FRACTION
      enumerator :: REGRID_METHOD_CONSERVE_2ND
      enumerator :: REGRID_METHOD_PATCH
      enumerator :: REGRID_METHOD_NEAREST_STOD
      enumerator :: REGRID_METHOD_CONSERVE_HFLUX
      enumerator :: REGRID_METHOD_BILINEAR_MONOTONIC
      enumerator :: REGRID_METHOD_CONSERVE_MONOTONIC
      enumerator :: UNSPECIFIED_REGRID_METHOD = -1
   end enum
   integer, parameter :: REGRID_HINT_LOCAL = 1
   integer, parameter :: REGRID_HINT_FILE_WEIGHTS = 2
   integer, parameter :: REGRID_HINT_COMPUTE_TRANSPOSE = 4

   contains

   function regrid_method_string_to_int(string_regrid_method) result(int_regrid_method)
      integer :: int_regrid_method
      character(len=*), intent(in) :: string_regrid_method

      character(len=:), allocatable :: temp_str
      temp_str = ESMF_UtilStringUpperCase(trim(string_regrid_method))

      select case (temp_str)
      case ("BILINEAR")
         int_regrid_method = REGRID_METHOD_BILINEAR
      case ("BILINEAR_ROTATE")
         int_regrid_method = REGRID_METHOD_BILINEAR_ROTATE
      case ("CONSERVE")
         int_regrid_method = REGRID_METHOD_CONSERVE
      case ("VOTE")
         int_regrid_method = REGRID_METHOD_VOTE
      case ("FRACTION")
         int_regrid_method = REGRID_METHOD_FRACTION
      case ("CONSERVE_2ND")
         int_regrid_method = REGRID_METHOD_CONSERVE_2ND
      case ("PATCH")
         int_regrid_method = REGRID_METHOD_PATCH
      case ("CONSERVE_HFLUX")
         int_regrid_method = REGRID_METHOD_CONSERVE_HFLUX
      case ("CONSERVE_MONOTONIC")
         int_regrid_method = REGRID_METHOD_CONSERVE_MONOTONIC
      case ("BILINEAR_MONOTONIC")
         int_regrid_method = REGRID_METHOD_BILINEAR_MONOTONIC
      case ("NEAREST_STOD")
         int_regrid_method = REGRID_METHOD_NEAREST_STOD
      case default
         int_regrid_method = UNSPECIFIED_REGRID_METHOD
      end select
   end function

   function regrid_method_int_to_string(int_regrid_method) result(string_regrid_method)
      integer, intent(in) :: int_regrid_method
      character(len=:), allocatable :: string_regrid_method

      select case (int_regrid_method)
      case (REGRID_METHOD_BILINEAR)
         string_regrid_method = "bilinear"
      case (REGRID_METHOD_BILINEAR_ROTATE)
         string_regrid_method = "bilinear_rotate"
      case (REGRID_METHOD_CONSERVE)
         string_regrid_method = "conserve"
      case (REGRID_METHOD_VOTE)
         string_regrid_method = "vote"
      case (REGRID_METHOD_FRACTION)
         string_regrid_method = "fraction"
      case (REGRID_METHOD_CONSERVE_2ND)
         string_regrid_method = "conserve_2nd"
      case (REGRID_METHOD_PATCH)
         string_regrid_method = "patch"
      case (REGRID_METHOD_CONSERVE_HFLUX)
         string_regrid_method = "conserve_hflux"
      case (REGRID_METHOD_CONSERVE_MONOTONIC)
         string_regrid_method = "conserve_monotonic"
      case (REGRID_METHOD_BILINEAR_MONOTONIC)
         string_regrid_method = "bilinear_monotonic"
      case (REGRID_METHOD_NEAREST_STOD)
         string_regrid_method = "nearest_stod"
      case default
         string_regrid_method = "unspecified_regrid_method"
      end select
   end function

   function generate_esmf_regrid_param(regrid_method, typekind, rc) result(regrid_param)
      type(EsmfRegridderParam) :: regrid_param
      integer, intent(in) :: regrid_method
      type(ESMF_TypeKind_Flag), intent(in) :: typekind
      integer, intent(out), optional :: rc

      type(DynamicMask) :: mapl_dyn_mask
      type(ESMF_REGRIDMETHOD_FLAG) :: esmf_regrid_method
      integer :: status

      select case (regrid_method)
      case (REGRID_METHOD_BILINEAR)
          if (typekind == ESMF_TYPEKIND_R4) then
             mapl_dyn_mask = DynamicMask('missing_value', MAPL_UNDEFINED_REAL, &
                             handleAllElements=.true., _RC)
          else if (typekind == ESMF_TYPEKIND_R8) then
             mapl_dyn_mask = DynamicMask('missing_value', MAPL_UNDEFINED_REAL64, &
                             handleAllElements=.true., _RC)
          end if
          regrid_param = EsmfRegridderParam(regridMethod=ESMF_REGRIDMETHOD_BILINEAR, dyn_mask=mapl_dyn_mask)
      case (REGRID_METHOD_CONSERVE)
          if (typekind == ESMF_TYPEKIND_R4) then
             mapl_dyn_mask = DynamicMask('missing_value', MAPL_UNDEFINED_REAL, &
                             handleAllElements=.true., _RC)
          else if (typekind == ESMF_TYPEKIND_R8) then
             mapl_dyn_mask = DynamicMask('missing_value', MAPL_UNDEFINED_REAL64, &
                             handleAllElements=.true., _RC)
          end if
          regrid_param = EsmfRegridderParam(regridMethod=ESMF_REGRIDMETHOD_CONSERVE, dyn_mask=mapl_dyn_mask)
      case (REGRID_METHOD_VOTE)
          if (typekind == ESMF_TYPEKIND_R4) then
             mapl_dyn_mask = DynamicMask('vote', MAPL_UNDEFINED_REAL, &
                             handleAllElements=.true., _RC)
          else if (typekind == ESMF_TYPEKIND_R8) then
             mapl_dyn_mask = DynamicMask('vote', MAPL_UNDEFINED_REAL64, &
                             handleAllElements=.true., _RC)
          end if
          regrid_param = EsmfRegridderParam(regridMethod=ESMF_REGRIDMETHOD_CONSERVE, dyn_mask=mapl_dyn_mask)
      case (REGRID_METHOD_FRACTION)
          if (typekind == ESMF_TYPEKIND_R4) then
             mapl_dyn_mask = DynamicMask('fraction', MAPL_UNDEFINED_REAL, &
                             handleAllElements=.true., _RC)
          else if (typekind == ESMF_TYPEKIND_R8) then
             mapl_dyn_mask = DynamicMask('fraction', MAPL_UNDEFINED_REAL64, &
                             handleAllElements=.true., _RC)
          end if
          regrid_param = EsmfRegridderParam(regridMethod=ESMF_REGRIDMETHOD_CONSERVE, dyn_mask=mapl_dyn_mask)
      case (REGRID_METHOD_CONSERVE_2ND)
          if (typekind == ESMF_TYPEKIND_R4) then
             mapl_dyn_mask = DynamicMask('missing_value', MAPL_UNDEFINED_REAL, &
                             handleAllElements=.true., _RC)
          else if (typekind == ESMF_TYPEKIND_R8) then
             mapl_dyn_mask = DynamicMask('missing_value', MAPL_UNDEFINED_REAL64, &
                             handleAllElements=.true., _RC)
          end if
          regrid_param = EsmfRegridderParam(regridMethod=ESMF_REGRIDMETHOD_CONSERVE, dyn_mask=mapl_dyn_mask)
      case (REGRID_METHOD_PATCH)
          if (typekind == ESMF_TYPEKIND_R4) then
             mapl_dyn_mask = DynamicMask('missing_value', MAPL_UNDEFINED_REAL, &
                             handleAllElements=.true., _RC)
          else if (typekind == ESMF_TYPEKIND_R8) then
             mapl_dyn_mask = DynamicMask('missing_value', MAPL_UNDEFINED_REAL64, &
                             handleAllElements=.true., _RC)
          end if
          regrid_param = EsmfRegridderParam(regridMethod=ESMF_REGRIDMETHOD_PATCH, dyn_mask=mapl_dyn_mask)
      case (REGRID_METHOD_CONSERVE_MONOTONIC)
          if (typekind == ESMF_TYPEKIND_R4) then
             mapl_dyn_mask = DynamicMask('monotonic', MAPL_UNDEFINED_REAL, &
                             handleAllElements=.true., _RC)
          else if (typekind == ESMF_TYPEKIND_R8) then
             mapl_dyn_mask = DynamicMask('monotonic', MAPL_UNDEFINED_REAL64, &
                             handleAllElements=.true., _RC)
          end if
          regrid_param = EsmfRegridderParam(regridMethod=ESMF_REGRIDMETHOD_CONSERVE, dyn_mask=mapl_dyn_mask)
      case (REGRID_METHOD_BILINEAR_MONOTONIC)
          if (typekind == ESMF_TYPEKIND_R4) then
             mapl_dyn_mask = DynamicMask('monotonic', MAPL_UNDEFINED_REAL, &
                             handleAllElements=.true., _RC)
          else if (typekind == ESMF_TYPEKIND_R8) then
             mapl_dyn_mask = DynamicMask('monotonic', MAPL_UNDEFINED_REAL64, &
                             handleAllElements=.true., _RC)
          end if
          regrid_param = EsmfRegridderParam(regridMethod=ESMF_REGRIDMETHOD_BILINEAR, dyn_mask=mapl_dyn_mask)
      case (REGRID_METHOD_NEAREST_STOD)
          if (typekind == ESMF_TYPEKIND_R4) then
             mapl_dyn_mask = DynamicMask('missing_value', MAPL_UNDEFINED_REAL, &
                             handleAllElements=.true., _RC)
          else if (typekind == ESMF_TYPEKIND_R8) then
             mapl_dyn_mask = DynamicMask('missing_value', MAPL_UNDEFINED_REAL64, &
                             handleAllElements=.true., _RC)
          end if
          regrid_param = EsmfRegridderParam(regridMethod=ESMF_REGRIDMETHOD_NEAREST_STOD, dyn_mask=mapl_dyn_mask)
      case default
         _FAIL("unknown regrid method")
      end select

   end function

end module mapl3g_RegridderMethods
