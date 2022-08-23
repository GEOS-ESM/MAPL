module mapl_RegridMethods
   use ESMF
   implicit none
   private

   public :: REGRID_HINT_LOCAL
   public :: REGRID_METHOD_IDENTITY
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
   public :: TILING_METHODS
   public :: get_regrid_method
   public :: translate_regrid_method

   enum, bind(c)
      enumerator :: REGRID_METHOD_IDENTITY
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
   integer, parameter :: TILING_METHODS(3) = [REGRID_METHOD_CONSERVE,REGRID_METHOD_VOTE,REGRID_METHOD_FRACTION]
   integer, parameter :: REGRID_HINT_LOCAL = 1

   contains

   function get_regrid_method(string_regrid_method) result(int_regrid_method)
      integer :: int_regrid_method
      character(len=*), intent(in) :: string_regrid_method

      character(len=:), allocatable :: temp_str
      temp_str = ESMF_UtilStringUpperCase(trim(string_regrid_method))

      select case (temp_str)
      case ("IDENTITY")
         int_regrid_method = REGRID_METHOD_IDENTITY
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

   function translate_regrid_method(int_regrid_method) result(string_regrid_method)
      integer, intent(in) :: int_regrid_method
      character(len=:), allocatable, intent(out) :: string_regrid_method

      select case (int_regrid_method)
      case (REGRID_METHOD_IDENTITY)
         string_regrid_method = "identity"
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
end module mapl_RegridMethods
