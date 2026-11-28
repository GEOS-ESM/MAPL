#include "MAPL.h"

! NullGeomSpec is used to return a concrete object fore failing
! factory methods that return GeomSpec objects.
module mapl_NullGeomSpec_mod

   use mapl_GeomSpec_mod
   use esmf, only: ESMF_KIND_R4, ESMF_KIND_R8, ESMF_Geom
   use mapl_ErrorHandling_mod

   implicit none(type, external)
   private

   public :: NULL_GEOM_SPEC

   type, extends(GeomSpec) :: NullGeomSpec
   contains
      procedure :: equal_to
      procedure :: get_horz_ij_index_r4
      procedure :: get_horz_ij_index_r8
      procedure :: make_geom
      procedure :: make_file_metadata
      procedure :: make_gridded_dims
      procedure :: make_variable_attributes
   end type NullGeomSpec

   type(NullGeomSpec), protected :: NULL_GEOM_SPEC

contains

   logical function equal_to(a, b)
      class(NullGeomSpec), intent(in) :: a
      class(GeomSpec), intent(in) :: b
      equal_to = .false.
      _UNUSED_DUMMY(a)
      _UNUSED_DUMMY(b)
   end function equal_to

   subroutine get_horz_ij_index_r4(this, lon, lat, ii, jj, geom, rc)
      class(NullGeomSpec), intent(in) :: this
      real(kind=ESMF_KIND_R4), intent(in) :: lon(:)
      real(kind=ESMF_KIND_R4), intent(in) :: lat(:)
      integer, allocatable, intent(out) :: ii(:)
      integer, allocatable, intent(out) :: jj(:)
      type(ESMF_Geom), optional, intent(in) :: geom
      integer, optional, intent(out) :: rc

      integer :: status

      allocate(ii(1), jj(1), source=-1)

      _FAIL('get_horz_ij_index is not supported for NullGeomSpec')

      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(lon)
      _UNUSED_DUMMY(lat)
   end subroutine get_horz_ij_index_r4

   subroutine get_horz_ij_index_r8(this, lon, lat, ii, jj, geom, rc)
      class(NullGeomSpec), intent(in) :: this
      real(kind=ESMF_KIND_R8), intent(in) :: lon(:)
      real(kind=ESMF_KIND_R8), intent(in) :: lat(:)
      integer, allocatable, intent(out) :: ii(:)
      integer, allocatable, intent(out) :: jj(:)
      type(ESMF_Geom), optional, intent(in) :: geom
      integer, optional, intent(out) :: rc

      integer :: status

      allocate(ii(1), jj(1), source=-1)

      _FAIL('get_horz_ij_index is not supported for NullGeomSpec')

      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(lon)
      _UNUSED_DUMMY(lat)
   end subroutine get_horz_ij_index_r8

   function make_geom(this, rc) result(geom)
      class(NullGeomSpec), intent(in) :: this
      type(ESMF_Geom) :: geom
      integer, optional, intent(out) :: rc

      _FAIL('make_geom is not supported for NullGeomSpec')

      _UNUSED_DUMMY(this)
   end function make_geom

   function make_file_metadata(this, unusable, chunksizes, rc) &
        result(file_metadata)
      use pfio_FileMetadataMod
      use mapl_KeywordEnforcer_mod
      class(NullGeomSpec), intent(in) :: this
      type(FileMetadata) :: file_metadata
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(in) :: chunksizes(:)
      integer, optional, intent(out) :: rc

      _FAIL('make_file_metadata is not supported for NullGeomSpec')

      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(unusable)
      _UNUSED_DUMMY(chunksizes)
   end function make_file_metadata

   function make_gridded_dims(this, rc) result(gridded_dims)
      use gFTL2_StringVector
      class(NullGeomSpec), intent(in) :: this
      type(StringVector) :: gridded_dims
      integer, optional, intent(out) :: rc

      _FAIL('make_gridded_dims is not supported for NullGeomSpec')

      _UNUSED_DUMMY(this)
   end function make_gridded_dims

   function make_variable_attributes(this, rc) result(variable_attributes)
      use mapl_StringDictionary_mod
      class(NullGeomSpec), intent(in) :: this
      type(StringDictionary) :: variable_attributes
      integer, optional, intent(out) :: rc

      _FAIL('make_variable_attributes is not supported for NullGeomSpec')

      _UNUSED_DUMMY(this)
   end function make_variable_attributes

end module mapl_NullGeomSpec_mod
