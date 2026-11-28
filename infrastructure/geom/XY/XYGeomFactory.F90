#include "MAPL.h"

module mapl_XYGeomFactory_mod

   use mapl_GeomSpec_mod
   use mapl_GeomFactory_mod
   use mapl_XYGeomSpec_mod
   use mapl_ErrorHandling_mod
   use pfio
   use esmf

   implicit none
   private

   public :: XYGeomFactory

   type, extends(GeomFactory) :: XYGeomFactory
      private
   contains
      ! Mandatory GeomFactory interface
      procedure :: make_geom_spec_from_hconfig
      procedure :: make_geom_spec_from_metadata
      procedure :: supports_hconfig
      procedure :: supports_metadata
   end type XYGeomFactory

contains

   function make_geom_spec_from_hconfig(this, hconfig, rc) result(geom_spec)
      class(GeomSpec), allocatable :: geom_spec
      class(XYGeomFactory), intent(in) :: this
      type(ESMF_HConfig), intent(in) :: hconfig
      integer, optional, intent(out) :: rc

      integer :: status

      geom_spec = make_XYGeomSpec(hconfig, _RC)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(this)
   end function make_geom_spec_from_hconfig

   function make_geom_spec_from_metadata(this, file_metadata, rc) result(geom_spec)
      class(GeomSpec), allocatable :: geom_spec
      class(XYGeomFactory), intent(in) :: this
      type(FileMetadata), intent(in) :: file_metadata
      integer, optional, intent(out) :: rc

      integer :: status

      geom_spec = make_XYGeomSpec(file_metadata, _RC)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(this)
   end function make_geom_spec_from_metadata

   logical function supports_hconfig(this, hconfig, rc) result(supports)
      class(XYGeomFactory), intent(in) :: this
      type(ESMF_HConfig), intent(in) :: hconfig
      integer, optional, intent(out) :: rc

      integer :: status
      type(XYGeomSpec) :: spec

      supports = spec%supports(hconfig, _RC)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(this)
   end function supports_hconfig

   logical function supports_metadata(this, file_metadata, rc) result(supports)
      class(XYGeomFactory), intent(in) :: this
      type(FileMetadata), intent(in) :: file_metadata
      integer, optional, intent(out) :: rc

      integer :: status
      type(XYGeomSpec) :: spec

      supports = spec%supports(file_metadata, _RC)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(this)
   end function supports_metadata

end module mapl_XYGeomFactory_mod
