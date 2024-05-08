#include "MAPL_ErrLog.h"

module mapl3g_CubedSphereGeomSpec
   use mapl3g_GeomSpec
   use mapl3g_CubedSphereDecomposition
   use mapl3g_LonAxis
   use mapl3g_LatAxis
   use esmf, only: ESMF_KIND_R8
   implicit none
   private

   public :: CubedSphereGeomSpec
   public :: make_CubedSphereGeomSpec

   type, extends(GeomSpec) :: CubedSphereGeomSpec
      private
      integer :: im_world
      
   contains
      ! mandatory interface
      procedure :: equal_to

      ! CubedSphere specific
      procedure :: supports_hconfig => supports_hconfig_
      procedure :: supports_metadata => supports_metadata_
      generic :: supports => supports_hconfig, supports_metadata

      ! Accessors
   end type CubedSphereGeomSpec

   interface CubedSphereGeomSpec
      module procedure new_CubedSphereGeomSpec
   end interface CubedSphereGeomSpec

   interface make_CubedSphereGeomSpec
      procedure make_CubedSphereGeomSpec_from_hconfig
      procedure make_CubedSphereGeomSpec_from_metadata
   end interface make_CubedSphereGeomSpec

!#   interface get_coordinates
!#      procedure get_coordinates_try
!#   end interface get_coordinates
!#
   integer, parameter :: R8 = ESMF_KIND_R8

interface

      ! Basic constructor for CubedSphereGeomSpec
      module function new_CubedSphereGeomSpec(lon_axis, lat_axis, decomposition) result(spec)
         type(CubedSphereGeomSpec) :: spec
         type(LonAxis), intent(in) :: lon_axis
         type(LatAxis), intent(in) :: lat_axis
         type(CubedSpheredecomposition), intent(in) :: decomposition
      end function new_CubedSphereGeomSpec


      pure logical module function equal_to(a, b)
         class(CubedSphereGeomSpec), intent(in) :: a
         class(GeomSpec), intent(in) :: b
      end function equal_to


      ! HConfig section
      module function make_CubedSphereGeomSpec_from_hconfig(hconfig, rc) result(spec)
         use esmf, only: ESMF_HConfig
         type(CubedSphereGeomSpec) :: spec
         type(ESMF_HConfig), intent(in) :: hconfig
         integer, optional, intent(out) :: rc
      end function make_CubedSphereGeomSpec_from_hconfig

      ! File metadata section
      ! =====================
      ! Unfortunately, we cannot quite compute each axis (lat - lon) independently,
      ! as the optimal decomposition depends on the ratio of the extens along each
      ! dimension.
      module function make_CubedSphereGeomSpec_from_metadata(file_metadata, rc) result(spec)
         use pfio, only: FileMetadata
         type(CubedSphereGeomSpec) :: spec
         type(FileMetadata), intent(in) :: file_metadata
         integer, optional, intent(out) :: rc
      end function make_CubedSphereGeomSpec_from_metadata


      logical module function supports_hconfig_(this, hconfig, rc) result(supports)
         use esmf, only: ESMF_HConfig
         class(CubedSphereGeomSpec), intent(in) :: this
         type(ESMF_HConfig), intent(in) :: hconfig
         integer, optional, intent(out) :: rc
      end function supports_hconfig_

      logical module function supports_metadata_(this, file_metadata, rc) result(supports)
         use pfio, only: FileMetadata
         class(CubedSphereGeomSpec), intent(in) :: this
         type(FileMetadata), intent(in) :: file_metadata
         integer, optional, intent(out) :: rc
      end function supports_metadata_

   end interface

end module mapl3g_CubedSphereGeomSpec


