module mapl_RegridMethods
   implicit none
   private

   public :: REGRID_HINT_LOCAL
   public :: REGRID_METHOD_IDENTITY
   public :: REGRID_METHOD_BILINEAR
   public :: REGRID_METHOD_BILINEAR_ROTATE
   public :: REGRID_METHOD_CONSERVE
   public :: REGRID_METHOD_VOTE
   public :: REGRID_METHOD_FRACTION
   public :: REGRID_METHOD_CONSERVE_2ND
   public :: REGRID_METHOD_PATCH
   public :: REGRID_METHOD_NEAREST_STOD
   public :: REGRID_METHOD_CONSERVE_HFLUX
   public :: UNSPECIFIED_REGRID_METHOD
   public :: TILING_METHODS

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
      enumerator :: UNSPECIFIED_REGRID_METHOD = -1
   end enum
   integer, parameter :: TILING_METHODS(3) = [REGRID_METHOD_CONSERVE,REGRID_METHOD_VOTE,REGRID_METHOD_FRACTION]
   integer, parameter :: REGRID_HINT_LOCAL = 1


end module mapl_RegridMethods
