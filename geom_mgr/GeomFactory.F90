#include "MAPL_Generic.h"

module mapl_GeomFactory
   use mapl_MaplGeom
   implicit none
   private

   public :: GeomFactory

   type, abstract :: GeomFactory
      private
   contains
      procedure(I_make_geom_spec_from_config),   deferred :: make_geom_spec_from_config
      procedure(I_make_geom_spec_from_metadata), deferred :: make_geom_spec_from_metadata
      generic :: make_spec => make_geom_spec_from_config
      generic :: make_spec => make_geom_spec_from_metadata
      procedure(I_supports), deferred :: supports

      procedure(I_make_geom), deferred :: make_geom
      procedure(I_make_file_metadata), deferred :: make_file_metadata
      procedure(I_make_gridded_dims), deferred :: make_gridded_dims
   end type GeomFactory


   abstract interface

     function I_make_geom_spec_from_config(this, config, supports, rc) result(spec)
         use esmf, only: ESMF_Config
         use mapl_GeomSpec
         import GeomFactory
         implicit none

         class(GeomSpec), allocatable :: spec
         class(GeomFactory), intent(in) :: this
         type(ESMF_Config), intent(inout) :: config
         logical, optional, intent(out) :: supports
         integer, optional, intent(out) :: rc
      end function I_make_geom_spec_from_config

      function I_make_geom_spec_from_metadata(this, file_metadata, supports, rc) result(spec)
         use pfio_FileMetadataMod
         use mapl_GeomSpec
         import GeomFactory
         implicit none

         class(GeomSpec), allocatable :: spec
         class(GeomFactory), intent(in) :: this
         type(FileMetadata), intent(in) :: file_metadata
         logical, optional, intent(out) :: supports
         integer, optional, intent(out) :: rc
      end function I_make_geom_spec_from_metadata

      function I_make_geom(this, geom_spec, supports, rc) result(geom)
         use esmf, only: ESMF_Geom
         use mapl_GeomSpec
         import GeomFactory
         implicit none

         type(ESMF_Geom) :: geom
         class(GeomFactory), intent(in) :: this
         class(GeomSpec), intent(in) :: geom_spec
         logical, optional, intent(out) :: supports
         integer, optional, intent(out) :: rc
      end function I_make_geom

      function I_make_file_metadata(this, geom_spec, supports, rc) result(file_metadata)
         use mapl_GeomSpec
         use esmf, only: ESMF_Geom
         use pfio_FileMetadataMod
         import GeomFactory
         implicit none

         type(FileMetadata) :: file_metadata
         class(GeomFactory), intent(in) :: this
         class(GeomSpec), intent(in) :: geom_spec
         logical, optional, intent(out) :: supports
         integer, optional, intent(out) :: rc
      end function I_make_file_metadata

      function I_make_gridded_dims(this, geom_spec, supports, rc) result(gridded_dims)
         use mapl_GeomSpec
         use esmf, only: ESMF_Geom
         use gFTL2_StringVector
         import GeomFactory
         implicit none

         type(StringVector) :: gridded_dims
         class(GeomFactory), intent(in) :: this
         class(GeomSpec), intent(in) :: geom_spec
         logical, optional, intent(out) :: supports
         integer, optional, intent(out) :: rc
      end function I_make_gridded_dims

      logical function I_supports(this, geom_spec) result(supports)
         use mapl_GeomSpec
         import GeomFactory
         class(GeomFactory), intent(in) :: this
         class(GeomSpec), intent(in) :: geom_spec
      end function I_supports

   end interface

end module mapl_GeomFactory
