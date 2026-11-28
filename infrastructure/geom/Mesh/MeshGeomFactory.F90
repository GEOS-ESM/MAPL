#include "MAPL.h"

module mapl_MeshGeomFactory_mod
   use mapl_GeomSpec_mod
   use mapl_GeomFactory_mod
   use mapl_MeshGeomSpec_mod
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
      procedure :: supports_hconfig
      procedure :: supports_metadata
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

   end interface

end module mapl_MeshGeomFactory_mod
