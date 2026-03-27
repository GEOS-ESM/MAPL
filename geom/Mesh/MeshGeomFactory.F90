#include "MAPL.h"

module mapl3g_MeshGeomFactory
   use mapl3g_GeomSpec
   use mapl3g_GeomFactory
   use mapl3g_MeshGeomSpec
   use mapl_KeywordEnforcerMod, only: KeywordEnforcer
   use gftl2_StringVector, only: StringVector
   use mapl3g_StringDictionary, only: StringDictionary
   use pfio_FileMetadataMod, only: FileMetadata
   use esmf
   implicit none
   private

   public :: MeshGeomFactory

   type, extends(GeomFactory) :: MeshGeomFactory
      private
   contains
      ! Mandatory GeomFactory interfaces
      procedure :: make_geom_spec_from_hconfig
      procedure :: make_geom_spec_from_metadata
      procedure :: supports_spec
      procedure :: supports_hconfig
      procedure :: supports_metadata
      procedure :: make_geom
      procedure :: make_file_metadata
      procedure :: make_gridded_dims
      procedure :: make_variable_attributes
   end type MeshGeomFactory

   interface

      ! GeomSpec factory methods
      module function make_geom_spec_from_hconfig(this, hconfig, rc) result(geom_spec)
         class(GeomSpec), allocatable :: geom_spec
         class(MeshGeomFactory), intent(in) :: this
         type(ESMF_HConfig), intent(in) :: hconfig
         integer, optional, intent(out) :: rc
      end function make_geom_spec_from_hconfig

      module function make_geom_spec_from_metadata(this, file_metadata, rc) result(geom_spec)
         class(GeomSpec), allocatable :: geom_spec
         class(MeshGeomFactory), intent(in) :: this
         type(FileMetadata), intent(in) :: file_metadata
         integer, optional, intent(out) :: rc
      end function make_geom_spec_from_metadata

      ! Support checking methods
      module logical function supports_spec(this, geom_spec) result(supports)
         class(MeshGeomFactory), intent(in) :: this
         class(GeomSpec), intent(in) :: geom_spec
      end function supports_spec

      module logical function supports_hconfig(this, hconfig, rc) result(supports)
         class(MeshGeomFactory), intent(in) :: this
         type(ESMF_HConfig), intent(in) :: hconfig
         integer, optional, intent(out) :: rc
      end function supports_hconfig

      module logical function supports_metadata(this, file_metadata, rc) result(supports)
         class(MeshGeomFactory), intent(in) :: this
         type(FileMetadata), intent(in) :: file_metadata
         integer, optional, intent(out) :: rc
      end function supports_metadata

      ! Geometry creation
      module function make_geom(this, geom_spec, rc) result(geom)
         type(ESMF_Geom) :: geom
         class(MeshGeomFactory), intent(in) :: this
         class(GeomSpec), intent(in) :: geom_spec
         integer, optional, intent(out) :: rc
      end function make_geom

      ! File metadata generation
      module function make_file_metadata(this, geom_spec, unusable, chunksizes, rc) result(file_metadata)
         type(FileMetadata) :: file_metadata
         class(MeshGeomFactory), intent(in) :: this
         class(GeomSpec), intent(in) :: geom_spec
         class(KeywordEnforcer), optional, intent(in) :: unusable
         integer, optional, intent(in) :: chunksizes(:)
         integer, optional, intent(out) :: rc
      end function make_file_metadata

      ! Dimension and attribute helpers
      module function make_gridded_dims(this, geom_spec, rc) result(gridded_dims)
         type(StringVector) :: gridded_dims
         class(MeshGeomFactory), intent(in) :: this
         class(GeomSpec), intent(in) :: geom_spec
         integer, optional, intent(out) :: rc
      end function make_gridded_dims

      module function make_variable_attributes(this, geom_spec, rc) result(variable_attributes)
         type(StringDictionary) :: variable_attributes
         class(MeshGeomFactory), intent(in) :: this
         class(GeomSpec), intent(in) :: geom_spec
         integer, optional, intent(out) :: rc
      end function make_variable_attributes

   end interface

end module mapl3g_MeshGeomFactory
