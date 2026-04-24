#include "MAPL.h"

module mapl3g_XYGeomFactory

   use mapl3g_GeomSpec
   use mapl3g_GeomFactory
   use mapl3g_XYGeomSpec
   use mapl_KeywordEnforcerMod
   use mapl_ErrorHandlingMod
   use gftl2_StringVector
   use mapl3g_StringDictionary
   use pfio
   use esmf
   use mapl_KeywordEnforcer, only: KE => KeywordEnforcer

   implicit none
   private

   public :: XYGeomFactory

   type, extends(GeomFactory) :: XYGeomFactory
      private
   contains
      ! Mandatory GeomFactory interface
      procedure :: make_geom_spec_from_hconfig
      procedure :: make_geom_spec_from_metadata
      procedure :: supports_spec
      procedure :: supports_hconfig
      procedure :: supports_metadata
      procedure :: make_geom
      procedure :: make_file_metadata
      procedure :: make_gridded_dims
      procedure :: make_variable_attributes
   end type XYGeomFactory

   interface

      module function make_geom(this, geom_spec, rc) result(geom)
         use mapl3g_GeomSpec, only: GeomSpec
         use esmf, only: ESMF_Geom
         type(ESMF_Geom) :: geom
         class(XYGeomFactory), intent(in) :: this
         class(GeomSpec), intent(in) :: geom_spec
         integer, optional, intent(out) :: rc
      end function make_geom

      module function typesafe_make_geom(spec, rc) result(geom)
         use esmf, only: ESMF_Geom
         type(ESMF_Geom) :: geom
         type(XYGeomSpec), intent(in) :: spec
         integer, optional, intent(out) :: rc
      end function typesafe_make_geom

      module function create_basic_grid(spec, unusable, rc) result(grid)
         use esmf, only: ESMF_Grid
         type(ESMF_Grid) :: grid
         type(XYGeomSpec), intent(in) :: spec
         class(KE), optional, intent(in) :: unusable
         integer, optional, intent(out) :: rc
      end function create_basic_grid

      module subroutine fill_coordinates(spec, grid, unusable, rc)
         use esmf, only: ESMF_Grid
         type(XYGeomSpec), intent(in) :: spec
         type(ESMF_Grid),  intent(inout) :: grid
         class(KE), optional, intent(in) :: unusable
         integer, optional, intent(out) :: rc
      end subroutine fill_coordinates

      module subroutine fill_coordinates_abi(spec, grid, unusable, rc)
         use esmf, only: ESMF_Grid
         type(XYGeomSpec), intent(in) :: spec
         type(ESMF_Grid),  intent(inout) :: grid
         class(KE), optional, intent(in) :: unusable
         integer, optional, intent(out) :: rc
      end subroutine fill_coordinates_abi

      module subroutine add_mask(spec, grid, rc)
         use esmf, only: ESMF_Grid
         type(XYGeomSpec), intent(in) :: spec
         type(ESMF_Grid),  intent(inout) :: grid
         integer, optional, intent(out) :: rc
      end subroutine add_mask

      module function make_gridded_dims(this, geom_spec, rc) result(gridded_dims)
         type(StringVector) :: gridded_dims
         class(XYGeomFactory), intent(in) :: this
         class(GeomSpec), intent(in) :: geom_spec
         integer, optional, intent(out) :: rc
      end function make_gridded_dims

      module function make_variable_attributes(this, geom_spec, rc) result(variable_attributes)
         type(StringDictionary) :: variable_attributes
         class(XYGeomFactory), intent(in) :: this
         class(GeomSpec), intent(in) :: geom_spec
         integer, optional, intent(out) :: rc
      end function make_variable_attributes

      module function make_file_metadata(this, geom_spec, unusable, chunksizes, rc) result(file_metadata)
         use mapl_KeywordEnforcerMod
         type(FileMetadata) :: file_metadata
         class(XYGeomFactory), intent(in) :: this
         class(GeomSpec), intent(in) :: geom_spec
         class(KeywordEnforcer), optional, intent(in) :: unusable
         integer, optional, intent(in) :: chunksizes(:)
         integer, optional, intent(out) :: rc
      end function make_file_metadata

      module function typesafe_make_file_metadata(spec, unusable, chunksizes, rc) result(file_metadata)
         type(FileMetadata) :: file_metadata
         type(XYGeomSpec), intent(in) :: spec
         class(KE), optional, intent(in) :: unusable
         integer, optional, intent(in) :: chunksizes(:)
         integer, optional, intent(out) :: rc
      end function typesafe_make_file_metadata

   end interface

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

   logical function supports_spec(this, geom_spec) result(supports)
      class(XYGeomFactory), intent(in) :: this
      class(GeomSpec), intent(in) :: geom_spec

      type(XYGeomSpec) :: reference

      supports = same_type_as(geom_spec, reference)

      _UNUSED_DUMMY(this)
   end function supports_spec

end module mapl3g_XYGeomFactory
