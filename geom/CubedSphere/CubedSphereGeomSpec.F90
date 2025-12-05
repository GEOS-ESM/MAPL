#include "MAPL_ErrLog.h"

module mapl3g_CubedSphereGeomSpec
   use mapl3g_GeomSpec
   use mapl3g_CubedSphereDecomposition
   use esmf, only: ESMF_KIND_R8, ESMF_CubedSphereTransform_Args
   implicit none
   real(kind=ESMF_Kind_R8) :: undef_schmidt = 1d15
   private

   public :: CubedSphereGeomSpec
   public :: make_CubedSphereGeomSpec

   type, extends(GeomSpec) :: CubedSphereGeomSpec
      private
      integer :: im_world
      type(ESMF_CubedSphereTransform_Args) :: schmidt_parameters
      type(CubedSphereDecomposition) :: decomposition  
      
   contains
      ! mandatory interface
      procedure :: equal_to

      ! CubedSphere specific
      procedure :: supports_hconfig => supports_hconfig_
      procedure :: supports_metadata => supports_metadata_
      generic :: supports => supports_hconfig, supports_metadata

      ! Accessors
      procedure :: get_decomposition
      procedure :: get_topology
      procedure :: get_im_world
      procedure :: get_schmidt_parameters
   end type CubedSphereGeomSpec

   interface CubedSphereGeomSpec
      module procedure new_CubedSphereGeomSpec
   end interface CubedSphereGeomSpec

   interface make_CubedSphereGeomSpec
      procedure make_CubedSphereGeomSpec_from_hconfig
      procedure make_CubedSphereGeomSpec_from_metadata
   end interface make_CubedSphereGeomSpec

   integer, parameter :: R8 = ESMF_KIND_R8

interface

      ! Basic constructor for CubedSphereGeomSpec
      module function new_CubedSphereGeomSpec(im_world, schmidt_parameters, decomposition) result(spec)
         type(CubedSphereGeomSpec) :: spec
         integer, intent(in) :: im_world
         type(ESMF_CubedSphereTransform_Args), intent(in) :: schmidt_parameters
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

      ! Accessors
      pure module function get_decomposition(spec) result(decomposition)
         type(CubedSphereDecomposition) :: decomposition
         class(CubedSphereGeomSpec), intent(in) :: spec
      end function get_decomposition

      pure module function get_topology(spec) result(topology)
         class(CubedSphereGeomSpec), intent(in) :: spec
         integer, allocatable :: topology(:)
      end function get_topology

      pure module function get_im_world(spec) result(im_world)
         integer :: im_world
         class(CubedSphereGeomSpec), intent(in) :: spec
      end function get_im_world

      pure module function get_schmidt_parameters(spec) result(schmidt_parameters)
         type(ESMF_CubedSphereTransform_Args) :: schmidt_parameters
         class(CubedSphereGeomSpec), intent(in) :: spec
      end function get_schmidt_parameters

   end interface

end module mapl3g_CubedSphereGeomSpec


