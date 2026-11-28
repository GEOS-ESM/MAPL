#include "MAPL.h"

module mapl_GeomFactory_mod
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
      procedure(I_supports_hconfig), deferred :: supports_hconfig
      procedure(I_supports_metadata), deferred :: supports_metadata
      generic :: supports => supports_hconfig
      generic :: supports => supports_metadata
   end type GeomFactory


   abstract interface

      function I_make_geom_spec_from_hconfig(this, hconfig, rc) result(spec)
         use esmf, only: ESMF_HConfig
         use mapl_GeomSpec_mod
         import GeomFactory
         implicit none

         class(GeomSpec), allocatable :: spec
         class(GeomFactory), intent(in) :: this
         type(ESMF_HConfig), intent(in) :: hconfig
         integer, optional, intent(out) :: rc
      end function I_make_geom_spec_from_hconfig

      function I_make_geom_spec_from_metadata(this, file_metadata, rc) result(spec)
         use pfio_FileMetadataMod
         use mapl_GeomSpec_mod
         import GeomFactory
         implicit none

         class(GeomSpec), allocatable :: spec
         class(GeomFactory), intent(in) :: this
         type(FileMetadata), intent(in) :: file_metadata
         integer, optional, intent(out) :: rc
      end function I_make_geom_spec_from_metadata

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

end module mapl_GeomFactory_mod

