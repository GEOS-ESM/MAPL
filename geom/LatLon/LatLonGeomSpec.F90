#include "MAPL_ErrLog.h"

module mapl3g_LatLonGeomSpec
   use mapl3g_GeomSpec
   use mapl3g_LatLonDecomposition
   use mapl3g_LonAxis
   use mapl3g_LatAxis
   use esmf, only: ESMF_KIND_R8, ESMF_HCONFIG
   implicit none
   private

   public :: LatLonGeomSpec
   public :: make_LatLonGeomSpec

   type, extends(GeomSpec) :: LatLonGeomSpec
      private
      type(LonAxis) :: lon_axis
      type(LatAxis) :: lat_axis
      type(LatLonDecomposition) :: decomposition
   contains
      ! mandatory interface
      procedure :: equal_to

      ! LatLon specific
      procedure :: supports_hconfig => supports_hconfig_
      procedure :: supports_metadata => supports_metadata_
      generic :: supports => supports_hconfig, supports_metadata

      ! Accessors
      procedure :: get_lon_axis
      procedure :: get_lat_axis
      procedure :: get_decomposition
   end type LatLonGeomSpec

   interface LatLonGeomSpec
      module procedure new_LatLonGeomSpec
   end interface LatLonGeomSpec

   interface make_LatLonGeomSpec
      procedure make_LatLonGeomSpec_from_hconfig
      procedure make_LatLonGeomSpec_from_metadata
   end interface make_LatLonGeomSpec

!#   interface get_coordinates
!#      procedure get_coordinates_try
!#   end interface get_coordinates
!#
   integer, parameter :: R8 = ESMF_KIND_R8

interface

      pure logical module function equal_to(a, b)
         class(LatLonGeomSpec), intent(in) :: a
         class(GeomSpec), intent(in) :: b
      end function equal_to


      ! HConfig section
      module function make_LatLonGeomSpec_from_hconfig(hconfig, rc) result(spec)
         use esmf, only: ESMF_HConfig
         type(LatLonGeomSpec) :: spec
         type(ESMF_HConfig), intent(in) :: hconfig
         integer, optional, intent(out) :: rc
      end function make_LatLonGeomSpec_from_hconfig

      ! File metadata section
      ! =====================
      ! Unfortunately, we cannot quite compute each axis (lat - lon) independently,
      ! as the optimal decomposition depends on the ratio of the extens along each
      ! dimension.
      module function make_LatLonGeomSpec_from_metadata(file_metadata, rc) result(spec)
         use pfio, only: FileMetadata
         type(LatLonGeomSpec) :: spec
         type(FileMetadata), intent(in) :: file_metadata
         integer, optional, intent(out) :: rc
      end function make_LatLonGeomSpec_from_metadata

      ! ------------------------------------------------------------------------------------
      ! This module function attempts to find a layout with roughly square
      ! domains on each process.  Optimal value for
      !     nx = (im_world * petcount) / jm_world
      ! Except, it needs to be an integer
      ! --------------------------------------------------------------------
      module function make_de_layout_petcount(aspect_ratio, petCount) result(nx_ny)
         integer :: nx_ny(2)
         real, intent(in) :: aspect_ratio
         integer, intent(in) :: petCount
      end function make_de_layout_petcount

      module function make_de_layout_vm(aspect_ratio, vm, rc) result(nx_ny)
         use esmf, only: ESMF_VM
         integer :: nx_ny(2)
         real, optional, intent(in) :: aspect_ratio
         type(ESMF_VM), optional, intent(in) :: vm
         integer, optional, intent(out) :: rc
      end function make_de_layout_vm

      logical module function supports_hconfig_(this, hconfig, rc) result(supports)
         use esmf, only: ESMF_HConfig
         class(LatLonGeomSpec), intent(in) :: this
         type(ESMF_HConfig), intent(in) :: hconfig
         integer, optional, intent(out) :: rc
      end function supports_hconfig_

      logical module function supports_metadata_(this, file_metadata, rc) result(supports)
         use pfio, only: FileMetadata
         class(LatLonGeomSpec), intent(in) :: this
         type(FileMetadata), intent(in) :: file_metadata
         integer, optional, intent(out) :: rc
      end function supports_metadata_

      module function make_decomposition(hconfig, dims, rc) result(decomp)
         type(LatLonDecomposition) :: decomp
         type(ESMF_HConfig), intent(in) :: hconfig
         integer, intent(in) :: dims(2)
         integer, optional, intent(out) :: rc
      end function make_decomposition

   end interface

   CONTAINS

   ! Basic constructor for LatLonGeomSpec
   function new_LatLonGeomSpec(lon_axis, lat_axis, decomposition) result(spec)
      type(LatLonGeomSpec) :: spec
      type(LonAxis), intent(in) :: lon_axis
      type(LatAxis), intent(in) :: lat_axis
      type(LatLonDecomposition), intent(in) :: decomposition

      spec%lon_axis = lon_axis
      spec%lat_axis = lat_axis
      spec%decomposition = decomposition

   end function new_LatLonGeomSpec

   pure function get_decomposition(spec) result(decomposition)
      type(LatLonDecomposition) :: decomposition
      class(LatLonGeomSpec), intent(in) :: spec

      decomposition = spec%decomposition
   end function get_decomposition

   pure function get_lat_axis(spec) result(axis)
      class(LatLonGeomSpec), intent(in) :: spec
      type(LatAxis) :: axis
      axis = spec%lat_axis
   end function get_lat_axis

   ! Accessors
   pure function get_lon_axis(spec) result(axis)
      class(LatLonGeomSpec), intent(in) :: spec
      type(LonAxis) :: axis
      axis = spec%lon_axis
   end function get_lon_axis

end module mapl3g_LatLonGeomSpec


