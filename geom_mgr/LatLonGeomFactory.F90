#include "MAPL_Generic.h"

module mapl3g_LatLonGeomFactory
   use mapl3g_GeomFactory
   use mapl3g_GeomSpec
   use mapl3g_NullGeomSpec
   implicit none

   public :: LatLonGeomFactory
   public :: LatLonGeomSpec

   ! Note that LatLonGeomSpec (type and type constructor) are PRIVATE.
   ! This may be relaxed if we want for testing.
   type, extends(GeomSpec) :: LatLonGeomSpec
      private
      integer :: im_world ! cells per face x-edge
      integer :: jm_world ! cells per face y-edge
      integer :: lm       ! number of levels
      integer :: nx       ! decomposition in x direction
      integer :: ny       ! decomposition in y direction
      integer :: ims(:)   ! decomposition in x direction
      integer :: jms(:)   ! decomposition in y direction
      character(2) :: pole ! grid staggering relative to pole ("PC", "PE", "XY")
      character(2) :: dateline ! grid staggering relative to dateline ("DC", "DE", "GC", "GE")
   contains
      procedure :: equal_to
   end type LatLonGeomSpec

   type, extends(GeomFactory) :: LatLonGeomFactory
      private
   contains
      procedure :: make_geom_spec_from_config
      procedure :: make_geom_spec_from_metadata

      procedure :: make_geom
      procedure :: make_file_metadata
      procedure :: make_gridded_dims
   end type LatLonGeomFactory


   interface LatLonGeomSpec
      module procedure new_LatLonGeomSpec_from_config
      module procedure new_LatLonGeomSpec_from_metadata
   end interface LatLonGeomSpec

contains

  ! Process config to determine all necessary spec components. Some
   ! spec components (e.g. nx, ny) may be determined from default
   ! heuristics.
   function new_LatLonGeomSpec_from_config(config, supports, rc) result(spec)
      type(LatLonGeom_spec) :: spec
      type(ESMF_Config), intent(in) :: config
      integer, optional, intent(out) :: supports
      integer, optional, intent(out) :: rc

      integer :: status
      ...

      _RETURN(_SUCCESS)
   end function new_LatLonGeomSpec_from_config

   ! Process metadata to determine all necessary spec components.  Some
   ! spec components (e.g. nx, ny) may be determined from default
   ! heuristics.
   function new_LatLonGeomSpec_from_metadata(metadata, supports, rc) result(spec)
      type(LatLonGeom_spec) :: spec
      type(FileMetadata), intent(in) :: metadata
      integer, optional, intent(out) :: supports
      integer, optional, intent(out) :: rc

      integer :: status
      ...

      _RETURN(_SUCCESS)
   end function new_LatLonGeomSpec_from_metadata

 
   function make_geom_spec_from_config(config, supports, rc) result(geom_spec)
      class(GeomSpec), allocatable :: geom_spec
      class(LatLonGeomFactory), intent(in) :: this
      type(ESMF_Config), intent(in) :: config
      integer, optional, intent(out) :: supports
      integer, optional, intent(out) :: rc

      integer :: status
      
      geom_spec = LatLonGeomSpec(config, supports=supports, _RC)
      
      _RETURN(_SUCCESS)
   end function make_geom_spec_from_config

   function make_mapl_geom_from_metadata(metadata, rc) result(geom_spec)
      class(GeomSpec), allocatable :: geom_spec
      class(LatLonGeomFactory), intent(in) :: this
      type(FileMetadata), intent(in) :: metadata
      integer, optional, intent(out) :: rc

      integer :: status

      spec = LatLonGeomSpec(metadata, _RC)
      
      _RETURN(_SUCCESS)
   end function make_mapl_geom_from_metadata


   function make_mapl_geom_from_spec(this, geom_spec, supports, rc) result(mapl_geom)
      type(MaplGeom) :: mapl_geom
      class(LatLonGeomFactory), intent(in) :: this
      class(GeomSpec), intent(in) :: geom_spec
      integer, optional, intent(out) :: supports
      integer, optional, intent(out) :: rc

      select type(q => geom_spec)
      type is (LatLonGeomSpec)
         if (present(supports)) supports = .true.
         mapl_geom = type_safe_make_mapl_geom_from_spec(q, _RC)
      class default
         mapl_geom = NullGeomSpec()
         if (present(supports)) supports = .false.
      end select

      _RETURN(_SUCCESS)
   end function make_mapl_geom_from_spec


   function type_safe_make_mapl_geom_from_spec(spec, rc) result(mapl_geom)
      type(MaplGeom) :: mapl_geom
      type(LatLonGeomSpec), intent(in) :: spec
      integer, optional, intent(out) :: rc

      type(ESMF_Geom) :: geom

      geom = make_esmf_geom(spec, _RC)
      file_metadata = make_file_metadata(spec, _RC)
      gridded_dimensions = make_gridded_dimensions(spec, _RC)

      mapl_geom = MaplGeom(geom, file_metadata, gridded_dimensions)

   end function type_safe_make_mapl_geom_from_spec


   ! Helper procedures
   function make_esmf_geom(geom_spec, rc) result(geom)
      type(ESMF_Geom) :: geom
      type(LatLonGeomSpec), intent(in) :: geom_spec
      
      grid = ESMF_GridCreate(...)
      ...
      geom = ESMF_GeomCreate(geom)

   end function make_esmf_geom

   function make_file_metadata(geom_spec, rc) result(file_metadata)
      type(FileMetadata) :: file_metadata
      type(LatLonGeomSpec), intent(in) :: geom_spec
      integer, optional, intent(out) ::: rc

      metdata = FileMetadata()
      call add_dimensions(param, metadata, _RC)
      call add_coordinate_variables(param, metadata, _RC)

      _RETURN(_SUCCESS)
   end function make_file_metadata


   subroutine add_coordinates(this, metadata, rc)
      class(LatLonGeomSpec), intent(in) :: this
      type(FileMetadata), intent(inout) :: metadata
      integer, optional, intent(out) :: rc

      integer :: status
      type(Variable) :: v

      ! Coordinate variables
      v = coordinate('lon', 'longitude', 'degrees_east', this%get_longitudes_degrees())
      call metadata%add_variable(v)
      v = coordinate('lat', 'latitude', 'degrees_northt', this%get_latitude_degrees())
      call metadata%add_variable(v)

      if (this%has_vertical_dimension()) then
         v = VerticalCoordinate(...)
         call metadata%add_variable('lev', v)
      end if

      _RETURN(_SUCCESS)

   contains      

      function coordinate(dimensions, long_name, units, coords) result(v)
         type(Variable) :: v
         character(*), intent(in) :: dimensions
         character(*), intent(in) :: long_name
         character(*), intent(in) :: units
         real(kind=REAL64), intent(in) :: coords(:)

         v = Variable(type=PFIO_REAL64, dimensions=dimensions)
         call v%add_attribute('long_name', long_name)
         call v%add_attribute('units', units)
         call v%add_const_value(UnlimitedEntity(coords))

      end function coordinate

   end subroutine add_coordinates

   
   pure logical function equal_to(a, b)
      class(LatLonGeomSpec), intent(in) :: a
      class(GeomSpec), intent(in) :: b

      select type (b)
      type is (LatLonGeomSpec)
         equal_to = a%im_world == b%im_world .and. a%jm_world == b%jm_world &
              .and. a%lm == b%lm &
              .and. a%nx == b%nx .and. a%ny == b%ny &
              .and. a%ims == b%ims .and. a%jms == b%jms &
              .and. a%pole == b%pole .and. a%dateline == b%dateline
      class default
         equal_to = .false.
      end select

   end function equal_to

end module mapl3g_LatLonGeomFactory


