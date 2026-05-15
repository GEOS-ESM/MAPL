#include "MAPL.h"

module mapl3g_GeomFactory
   implicit none
   private

   public :: GeomFactory

   type, abstract :: GeomFactory
      private
   contains
      procedure(I_make_geom_spec_from_hconfig),  deferred :: make_geom_spec_from_hconfig
      procedure(I_make_geom_spec_from_metadata), deferred :: make_geom_spec_from_metadata
      generic :: make_spec => make_geom_spec_from_hconfig
      generic :: make_spec => make_geom_spec_from_metadata
      procedure(I_supports_spec), deferred :: supports_spec
      procedure(I_supports_hconfig), deferred :: supports_hconfig
      procedure(I_supports_metadata), deferred :: supports_metadata
      generic :: supports => supports_spec
      generic :: supports => supports_hconfig
      generic :: supports => supports_metadata

      procedure(I_make_geom), deferred :: make_geom
      procedure(I_make_file_metadata), deferred :: make_file_metadata
      procedure(I_make_gridded_dims), deferred :: make_gridded_dims
      procedure(I_make_variable_attributes), deferred :: make_variable_attributes
   end type GeomFactory


   abstract interface

      function I_make_geom_spec_from_hconfig(this, hconfig, rc) result(spec)
         use esmf, only: ESMF_HConfig
         use mapl3g_GeomSpec
         import GeomFactory
         implicit none

         class(GeomSpec), allocatable :: spec
         class(GeomFactory), intent(in) :: this
         type(ESMF_HConfig), intent(in) :: hconfig
         integer, optional, intent(out) :: rc
      end function I_make_geom_spec_from_hconfig

      function I_make_geom_spec_from_metadata(this, file_metadata, rc) result(spec)
         use pfio_FileMetadataMod
         use mapl3g_GeomSpec
         import GeomFactory
         implicit none

         class(GeomSpec), allocatable :: spec
         class(GeomFactory), intent(in) :: this
         type(FileMetadata), intent(in) :: file_metadata
         integer, optional, intent(out) :: rc
      end function I_make_geom_spec_from_metadata

      function I_make_geom(this, geom_spec, rc) result(geom)
         use esmf, only: ESMF_Geom
         use mapl3g_GeomSpec
         import GeomFactory
         implicit none

         type(ESMF_Geom) :: geom
         class(GeomFactory), intent(in) :: this
         class(GeomSpec), intent(in) :: geom_spec
         integer, optional, intent(out) :: rc
      end function I_make_geom

      function I_make_file_metadata(this, geom_spec, unusable, chunksizes, rc) result(file_metadata)
         use mapl3g_GeomSpec
         use pfio_FileMetadataMod
         use mapl_KeywordEnforcerMod
         import GeomFactory
         implicit none

         type(FileMetadata) :: file_metadata
         class(GeomFactory), intent(in) :: this
         class(GeomSpec), intent(in) :: geom_spec
         class(KeywordEnforcer), optional, intent(in) :: unusable
         integer, optional, intent(in) :: chunksizes(:)
         integer, optional, intent(out) :: rc
      end function I_make_file_metadata

      function I_make_gridded_dims(this, geom_spec, rc) result(gridded_dims)
         use mapl3g_GeomSpec
         use gFTL2_StringVector
         import GeomFactory
         implicit none

         type(StringVector) :: gridded_dims
         class(GeomFactory), intent(in) :: this
         class(GeomSpec), intent(in) :: geom_spec
         integer, optional, intent(out) :: rc
      end function I_make_gridded_dims

      function I_make_variable_attributes(this, geom_spec, rc) result(variable_attributes)
         use mapl3g_GeomSpec
         use mapl3g_StringDictionary
         import GeomFactory
         implicit none

         type(StringDictionary) :: variable_attributes
         class(GeomFactory), intent(in) :: this
         class(GeomSpec), intent(in) :: geom_spec
         integer, optional, intent(out) :: rc
      end function I_make_variable_attributes

      logical function I_supports_spec(this, geom_spec) result(supports)
         use mapl3g_GeomSpec
         import GeomFactory
         class(GeomFactory), intent(in) :: this
         class(GeomSpec), intent(in) :: geom_spec
      end function I_supports_spec

      logical function I_supports_hconfig(this, hconfig, rc) result(supports)
         use esmf, only: ESMF_HConfig
         import GeomFactory
         class(GeomFactory), intent(in) :: this
         type(ESMF_HConfig), intent(in) :: hconfig
         integer, optional, intent(out) :: rc
      end function I_supports_hconfig

      logical function I_supports_metadata(this, file_metadata, rc) result(supports)
         use pfio_FileMetadataMod
         import GeomFactory
         class(GeomFactory), intent(in) :: this
         type(FileMetadata), intent(in) :: file_metadata
         integer, optional, intent(out) :: rc
      end function I_supports_metadata

   end interface

end module mapl3g_GeomFactory

