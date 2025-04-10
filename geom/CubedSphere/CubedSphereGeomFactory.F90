#include "MAPL_ErrLog.h"

module mapl3g_CubedSphereGeomFactory
   use mapl3g_GeomSpec
   use mapl3g_GeomFactory
   use mapl3g_CubedSphereGeomSpec
   use mapl_KeywordEnforcerMod
   use gftl2_StringVector
   use pfio
   use esmf
   implicit none
   private

   public :: CubedSphereGeomFactory

   type, extends(GeomFactory) :: CubedSphereGeomFactory
      private
   contains
      ! Mandatory interfaces
      procedure :: make_geom_spec_from_hconfig
      procedure :: make_geom_spec_from_metadata
      procedure :: supports_spec
      procedure :: supports_hconfig
      procedure :: supports_metadata
      procedure :: make_geom
      procedure :: make_file_metadata
      procedure :: make_gridded_dims

      ! Helper methods
   end type CubedSphereGeomFactory


   interface

      module function make_geom_spec_from_hconfig(this, hconfig, rc) result(geom_spec)
         use mapl3g_GeomSpec, only: GeomSpec
         use esmf, only: ESMF_HConfig
         class(GeomSpec), allocatable :: geom_spec
         class(CubedSphereGeomFactory), intent(in) :: this
         type(ESMF_HConfig), intent(in) :: hconfig
         integer, optional, intent(out) :: rc
      end function make_geom_spec_from_hconfig


      module function make_geom_spec_from_metadata(this, file_metadata, rc) result(geom_spec)
         use mapl3g_GeomSpec, only: GeomSpec
         use pfio, only: FileMetadata
         class(GeomSpec), allocatable :: geom_spec
         class(CubedSphereGeomFactory), intent(in) :: this
         type(FileMetadata), intent(in) :: file_metadata
         integer, optional, intent(out) :: rc
      end function make_geom_spec_from_metadata


      logical module function supports_spec(this, geom_spec) result(supports)
         use mapl3g_GeomSpec, only: GeomSpec
         class(CubedSphereGeomFactory), intent(in) :: this
         class(GeomSpec), intent(in) :: geom_spec
      end function supports_spec

      logical module function supports_hconfig(this, hconfig, rc) result(supports)
         use esmf, only: ESMF_HConfig
         class(CubedSphereGeomFactory), intent(in) :: this
         type(ESMF_HConfig), intent(in) :: hconfig
         integer, optional, intent(out) :: rc

      end function supports_hconfig

      logical module function supports_metadata(this, file_metadata, rc) result(supports)
         use pfio, only: FileMetadata
         class(CubedSphereGeomFactory), intent(in) :: this
         type(FileMetadata), intent(in) :: file_metadata
         integer, optional, intent(out) :: rc
      end function supports_metadata


      module function make_geom(this, geom_spec, rc) result(geom)
         use mapl3g_GeomSpec, only: GeomSpec
         use esmf, only: ESMF_Geom
         type(ESMF_Geom) :: geom
         class(CubedSphereGeomFactory), intent(in) :: this
         class(GeomSpec), intent(in) :: geom_spec
         integer, optional, intent(out) :: rc
      end function make_geom


      module function create_basic_grid(spec, unusable, rc) result(grid)
         use mapl_KeywordEnforcer
         type(ESMF_Grid) :: grid
         type(CubedSphereGeomSpec), intent(in) :: spec
         class(KeywordEnforcer), optional, intent(in) :: unusable
         integer, optional, intent(out) :: rc
      end function create_basic_grid

      module function make_gridded_dims(this, geom_spec, rc) result(gridded_dims)
         type(StringVector) :: gridded_dims
         class(CubedSphereGeomFactory), intent(in) :: this
         class(GeomSpec), intent(in) :: geom_spec
         integer, optional, intent(out) :: rc
      end function make_gridded_dims


      module function make_file_metadata(this, geom_spec, unusable, chunksizes, rc) result(file_metadata)
         use mapl_KeywordEnforcerMod
         type(FileMetadata) :: file_metadata
         class(CubedSphereGeomFactory), intent(in) :: this
         class(GeomSpec), intent(in) :: geom_spec
         class(KeywordEnforcer), optional, intent(in) :: unusable
         integer, optional, intent(in) :: chunksizes(:)
         integer, optional, intent(out) :: rc
      end function make_file_metadata

   end interface
end module mapl3g_CubedSphereGeomFactory

