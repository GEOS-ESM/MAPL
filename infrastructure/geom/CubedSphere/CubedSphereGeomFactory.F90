#include "MAPL.h"

module mapl_CubedSphereGeomFactory_mod
   use mapl_GeomSpec_mod
   use mapl_GeomFactory_mod
   use mapl_CubedSphereGeomSpec_mod
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
      procedure :: supports_hconfig
      procedure :: supports_metadata

      ! Helper methods
   end type CubedSphereGeomFactory


   interface

      module function make_geom_spec_from_hconfig(this, hconfig, rc) result(geom_spec)
         use mapl_GeomSpec_mod, only: GeomSpec
         use esmf, only: ESMF_HConfig
         class(GeomSpec), allocatable :: geom_spec
         class(CubedSphereGeomFactory), intent(in) :: this
         type(ESMF_HConfig), intent(in) :: hconfig
         integer, optional, intent(out) :: rc
      end function make_geom_spec_from_hconfig


      module function make_geom_spec_from_metadata(this, file_metadata, rc) result(geom_spec)
         use mapl_GeomSpec_mod, only: GeomSpec
         use pfio, only: FileMetadata
         class(GeomSpec), allocatable :: geom_spec
         class(CubedSphereGeomFactory), intent(in) :: this
         type(FileMetadata), intent(in) :: file_metadata
         integer, optional, intent(out) :: rc
      end function make_geom_spec_from_metadata


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

   end interface
end module mapl_CubedSphereGeomFactory_mod

