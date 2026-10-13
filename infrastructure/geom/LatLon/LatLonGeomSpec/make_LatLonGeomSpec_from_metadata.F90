#include "MAPL.h"

submodule (mapl_LatLonGeomSpec_mod) make_LatLonGeomSpec_from_metadata_smod
   use mapl_CoordinateAxis_mod
   use mapl_GeomSpec_mod
   use pfio
   use mapl_ErrorHandling_mod
   use esmf
   use, intrinsic :: iso_fortran_env, only: REAL32, REAL64
   implicit none (type, external)

   character(*), parameter :: COORDINATE_TOLERANCE_ATTR = 'coordinate_tolerance'

contains

   ! File metadata section

   ! Unfortunately, we cannot quite compute each axis (lat - lon) independently,
   ! as the optimal decomposition depends on the ratio of the extens along each
   ! dimension.
   module function make_LatLonGeomSpec_from_metadata(file_metadata, rc) result(spec)
      type(LatLonGeomSpec) :: spec
      type(FileMetadata), intent(in) :: file_metadata
      integer, optional, intent(out) :: rc

      integer :: status
      type(LonAxis) :: lon_axis
      type(LatAxis) :: lat_axis
      type(LatLonDecomposition) :: decomposition
      real(kind=R8) :: tolerance

      tolerance = get_coordinate_tolerance(file_metadata, _RC)

      lon_axis = make_LonAxis(file_metadata, tolerance=tolerance, _RC)
      lat_axis = make_LatAxis(file_metadata, tolerance=tolerance, _RC)

      associate (im_world => lon_axis%get_extent(), jm_world => lat_axis%get_extent())
        decomposition = make_LatLonDecomposition([im_world, jm_world], _RC)
      end associate
      spec = LatLonGeomSpec(lon_axis, lat_axis, decomposition)
      
      _RETURN(_SUCCESS)
   end function make_LatLonGeomSpec_from_metadata

   ! Reads an optional "coordinate_tolerance" global attribute directly
   ! off FileMetadata using its existing generic attribute API. This is
   ! deliberately NOT a method on FileMetadata/FileMetadataUtilities:
   ! coordinate_tolerance is a concern of the geom layer (and whichever
   ! client, e.g. ExtData, chooses to set it), not of pfio. Absent
   ! attribute (or unrecognized type) yields the default of 0.0 (strict,
   ! bitwise comparison - unchanged prior behavior).
   !
   ! The value is a FRACTION of this grid's own local spacing (DX), not
   ! an absolute coordinate difference - see CoordinateAxis::equal_to,
   ! which multiplies it by the minimum spacing between adjacent
   ! centers. It is also directional: it only ever affects comparisons
   ! where THIS grid is the new candidate being looked up against an
   ! already-registered grid, never the reverse.
   function get_coordinate_tolerance(file_metadata, rc) result(tolerance)
      real(kind=R8) :: tolerance
      type(FileMetadata), intent(in) :: file_metadata
      integer, optional, intent(out) :: rc

      integer :: status
      type(Attribute), pointer :: attr
      class(*), pointer :: val

      tolerance = 0.0_R8

      if (file_metadata%has_attribute(COORDINATE_TOLERANCE_ATTR)) then
         attr => file_metadata%get_attribute(COORDINATE_TOLERANCE_ATTR, _RC)
         val => attr%get_value()
         select type (val)
         type is (real(kind=REAL64))
            tolerance = real(val, kind=R8)
         type is (real(kind=REAL32))
            tolerance = real(val, kind=R8)
         class default
            _FAIL('"'//COORDINATE_TOLERANCE_ATTR//'" attribute must be a real scalar')
         end select
         _ASSERT(tolerance >= 0.0_R8, '"'//COORDINATE_TOLERANCE_ATTR//'" attribute must be non-negative')
      end if

      _RETURN(_SUCCESS)
   end function get_coordinate_tolerance

end submodule make_LatLonGeomSpec_from_metadata_smod
