#include "MAPL_ErrLog.h"

module mapl3g_LatLonGeomFactory
   use mapl3g_GeomSpec
   use mapl3g_GeomFactory
   use mapl3g_LatLonGeomSpec
   use mapl_KeywordEnforcerMod
   use mapl_ErrorHandlingMod
   use gftl2_StringVector
   use gftl2_StringStringMap
   use pfio
   use esmf
   use mapl_KeywordEnforcer, only: KE => KeywordEnforcer
   implicit none
   private

   public :: LatLonGeomFactory

   type, extends(GeomFactory) :: LatLonGeomFactory
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
      procedure :: make_variable_attributes

      ! Helper methods
   end type LatLonGeomFactory


   interface

      module function make_geom(this, geom_spec, rc) result(geom)
         use mapl3g_GeomSpec, only: GeomSpec
         use esmf, only: ESMF_Geom
         type(ESMF_Geom) :: geom
         class(LatLonGeomFactory), intent(in) :: this
         class(GeomSpec), intent(in) :: geom_spec
         integer, optional, intent(out) :: rc
      end function make_geom


      module function create_basic_grid(spec, unusable, rc) result(grid)
         use mapl_KeywordEnforcer
         type(ESMF_Grid) :: grid
         type(LatLonGeomSpec), intent(in) :: spec
         class(KeywordEnforcer), optional, intent(in) :: unusable
         integer, optional, intent(out) :: rc
      end function create_basic_grid


      module subroutine fill_coordinates(spec, grid, unusable, rc)
         type(LatLonGeomSpec), intent(in) :: spec
         type(ESMF_Grid), intent(inout) :: grid
         class(KeywordEnforcer), optional, intent(in) :: unusable
         integer, optional, intent(out) :: rc
      end subroutine fill_coordinates

      module function make_gridded_dims(this, geom_spec, rc) result(gridded_dims)
         type(StringVector) :: gridded_dims
         class(LatLonGeomFactory), intent(in) :: this
         class(GeomSpec), intent(in) :: geom_spec
         integer, optional, intent(out) :: rc
      end function make_gridded_dims

      module function make_variable_attributes(this, geom_spec, rc) result(variable_attributes)
         type(StringStringMap) :: variable_attributes
         class(LatLonGeomFactory), intent(in) :: this
         class(GeomSpec), intent(in) :: geom_spec
         integer, optional, intent(out) :: rc
      end function make_variable_attributes

      module function make_file_metadata(this, geom_spec, unusable, chunksizes, rc) result(file_metadata)
         use mapl_KeywordEnforcerMod
         type(FileMetadata) :: file_metadata
         class(LatLonGeomFactory), intent(in) :: this
         class(GeomSpec), intent(in) :: geom_spec
         class(KeywordEnforcer), optional, intent(in) :: unusable
         integer, optional, intent(in) :: chunksizes(:)
         integer, optional, intent(out) :: rc
      end function make_file_metadata

      module function typesafe_make_file_metadata(geom_spec, unusable, chunksizes, rc) result(file_metadata)
         type(FileMetadata) :: file_metadata
         type(LatLonGeomSpec), intent(in) :: geom_spec
         class(KE), optional, intent(in) :: unusable
         integer, optional, intent(in) :: chunksizes(:)
         integer, optional, intent(out) :: rc
      end function typesafe_make_file_metadata

      module function typesafe_make_geom(spec, rc) result(geom)
         type(ESMF_Geom) :: geom
         class(LatLonGeomSpec), intent(in) :: spec
         integer, optional, intent(out) :: rc
      end function typesafe_make_geom

   end interface

   CONTAINS

   function make_geom_spec_from_hconfig(this, hconfig, rc) result(geom_spec)
      class(GeomSpec), allocatable :: geom_spec
      class(LatLonGeomFactory), intent(in) :: this
      type(ESMF_HConfig), intent(in) :: hconfig
      integer, optional, intent(out) :: rc

      integer :: status

      geom_spec = make_LatLonGeomSpec(hconfig, _RC)

      _RETURN(_SUCCESS)
   end function make_geom_spec_from_hconfig

   function make_geom_spec_from_metadata(this, file_metadata, rc) result(geom_spec)
      class(GeomSpec), allocatable :: geom_spec
      class(LatLonGeomFactory), intent(in) :: this
      type(FileMetadata), intent(in) :: file_metadata
      integer, optional, intent(out) :: rc

      integer :: status

      geom_spec = make_LatLonGeomSpec(file_metadata, _RC)

      _RETURN(_SUCCESS)
   end function make_geom_spec_from_metadata

   logical function supports_hconfig(this, hconfig, rc) result(supports)
      class(LatLonGeomFactory), intent(in) :: this
      type(ESMF_HConfig), intent(in) :: hconfig
      integer, optional, intent(out) :: rc

      integer :: status
      type(LatLonGeomSpec) :: spec

      supports = spec%supports(hconfig, _RC)

      _RETURN(_SUCCESS)
   end function supports_hconfig

   logical function supports_metadata(this, file_metadata, rc) result(supports)
      class(LatLonGeomFactory), intent(in) :: this
      type(FileMetadata), intent(in) :: file_metadata
      integer, optional, intent(out) :: rc

      integer :: status
      type(LatLonGeomSpec) :: spec

      supports = spec%supports(file_metadata, _RC)

      _RETURN(_SUCCESS)
   end function supports_metadata

   logical function supports_spec(this, geom_spec) result(supports)
      class(LatLonGeomFactory), intent(in) :: this
      class(GeomSpec), intent(in) :: geom_spec

      type(LatLonGeomSpec) :: reference

      supports = same_type_as(geom_spec, reference)

   end function supports_spec

end module mapl3g_LatLonGeomFactory

