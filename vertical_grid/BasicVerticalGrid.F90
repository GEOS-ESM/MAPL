#include "MAPL.h"
module mapl3g_BasicVerticalGrid
   use mapl3g_VerticalGrid, only: VerticalGrid
   use mapl3g_VerticalGridSpec, only: VerticalGridSpec
   use mapl3g_VerticalGridFactory, only: VerticalGridFactory
   use mapl3g_ComponentDriver, only: ComponentDriver
   use pfio, only: FileMetadata
   use esmf
   use mapl3g_VerticalStaggerLoc, only: VerticalStaggerLoc
   use gftl2_StringVector, only: StringVector
   use mapl_ErrorHandling
   implicit none(type,external)
   private
   
   public :: BasicVerticalGrid
   public :: BasicVerticalGridSpec
   public :: BasicVerticalGridFactory
   
   ! Spec type
   type, extends(VerticalGridSpec) :: BasicVerticalGridSpec
      integer :: num_levels
   end type BasicVerticalGridSpec
   
   ! Grid type
   type, extends(VerticalGrid) :: BasicVerticalGrid
      private
      type(BasicVerticalGridSpec) :: spec
   contains
      procedure :: initialize
      procedure :: get_num_levels
      procedure :: get_coordinate_field
      procedure :: get_supported_physical_dimensions
      procedure :: get_units
      procedure :: matches
   end type BasicVerticalGrid
   
   ! Factory type
   type, extends(VerticalGridFactory) :: BasicVerticalGridFactory
   contains
      procedure :: get_name
      procedure :: supports_spec
      procedure :: supports_file_metadata
      procedure :: supports_config
      procedure :: create_spec_from_config
      procedure :: create_spec_from_file_metadata
      procedure :: create_grid_from_spec
   end type BasicVerticalGridFactory

contains

   subroutine initialize(this, spec)
      class(BasicVerticalGrid), intent(inout) :: this
      type(BasicVerticalGridSpec), intent(in) :: spec
      
      this%spec = spec
   end subroutine initialize

   function get_num_levels(this) result(num_levels)
      integer :: num_levels
      class(BasicVerticalGrid), intent(in) :: this
      
      num_levels = this%spec%num_levels
   end function get_num_levels

   function get_coordinate_field(this, geom, physical_dimension, units, typekind, coupler, rc) result(field)
      type(esmf_Field) :: field
      class(BasicVerticalGrid), intent(in) :: this
      type(esmf_Geom), intent(in) :: geom
      character(len=*), intent(in) :: physical_dimension
      character(len=*), intent(in) :: units
      type(esmf_TypeKind_Flag), intent(in) :: typekind
      class(ComponentDriver), pointer, intent(out) :: coupler
      integer, intent(out), optional :: rc

      integer :: status

      coupler => null()
      _FAIL('BasicVerticalGrid should have been connected to a different subclass before this is called.')

   end function get_coordinate_field

   ! New method: get supported physical dimensions
   function get_supported_physical_dimensions(this) result(dimensions)
      type(StringVector) :: dimensions
      class(BasicVerticalGrid), target, intent(in) :: this
      
      call dimensions%push_back("<unknown>")
   end function get_supported_physical_dimensions

   ! New method: get units for a physical dimension
   function get_units(this, physical_dimension, rc) result(units)
      character(len=:), allocatable :: units
      class(BasicVerticalGrid), intent(in) :: this
      character(len=*), intent(in) :: physical_dimension
      integer, optional, intent(out) :: rc
      
      units = "<unknown>"
      _RETURN(_SUCCESS)
   end function get_units

   logical function matches(this, other)
      class(BasicVerticalGrid), intent(in) :: this
      class(VerticalGrid), intent(in) :: other

      matches = this%get_num_levels() == other%get_num_levels()
   end function matches

   ! Factory methods
   function get_name(this) result(name)
      character(len=:), allocatable :: name
      class(BasicVerticalGridFactory), intent(in) :: this
      
      name = "BasicVerticalGrid"
   end function get_name

   function supports_spec(this, spec, rc) result(is_supported)
      logical :: is_supported
      class(BasicVerticalGridFactory), intent(in) :: this
      class(VerticalGridSpec), intent(in) :: spec
      integer, optional, intent(out) :: rc

      integer :: status
      type(BasicVerticalGridSpec) :: basic_spec

      is_supported = same_type_as(spec, basic_spec)

      _RETURN(_SUCCESS)
   end function supports_spec

   function supports_file_metadata(this, file_metadata, rc) result(is_supported)
      logical :: is_supported
      class(BasicVerticalGridFactory), intent(in) :: this
      type(FileMetadata), intent(in), target :: file_metadata
      integer, optional, intent(out) :: rc
      
      ! Basic grid can work with any file metadata as a fallback
      is_supported = .true.
      _RETURN(_SUCCESS)
   end function supports_file_metadata

   function supports_config(this, config, rc) result(is_supported)
      logical :: is_supported
      class(BasicVerticalGridFactory), intent(in) :: this
      type(esmf_HConfig), intent(in) :: config
      integer, optional, intent(out) :: rc

      integer :: status
      logical :: has_num_levels
      logical :: has_grid_type
      character(len=:), allocatable :: grid_type

      is_supported = .false.

      ! grid_type is optional here
      has_grid_type = esmf_HConfigIsDefined(config, keyString="grid_type", _RC)
      if (has_grid_type) then
         grid_type = esmf_HConfigAsString(config, keyString="grid_type", _RC)
         _RETURN_UNLESS(grid_type == 'basic')
      end if

      has_num_levels = esmf_HConfigIsDefined(config, keyString="num_levels", _RC)
      is_supported = has_num_levels

      _RETURN(_SUCCESS)
   end function supports_config

   function create_spec_from_config(this, config, rc) result(spec)
      class(VerticalGridSpec), allocatable :: spec
      class(BasicVerticalGridFactory), intent(in) :: this
      type(esmf_HConfig), intent(in), target :: config
      integer, intent(out), optional :: rc
      
      type(BasicVerticalGridSpec) :: local_spec
      integer :: status
      
      ! Get number of levels if specified, otherwise use default
      if (esmf_HConfigIsDefined(config, keyString="num_levels")) then
         local_spec%num_levels = esmf_HConfigAsI4(config, keyString="num_levels", _RC)
      else
         local_spec%num_levels = 1  ! Default for basic grid
      end if
      
      _ASSERT(local_spec%num_levels > 0, 'Number of levels must be positive')
      
      ! Use polymorphic allocation
      allocate(spec, source=local_spec)
      
      _RETURN(_SUCCESS)
   end function create_spec_from_config

   function create_spec_from_file_metadata(this, file_metadata, rc) result(spec)
      class(VerticalGridSpec), allocatable :: spec
      class(BasicVerticalGridFactory), intent(in) :: this
      type(FileMetadata), intent(in), target :: file_metadata
      integer, intent(out), optional :: rc
      
      type(BasicVerticalGridSpec) :: local_spec
      integer :: status
      
      ! For basic grid, just create a single-level spec as fallback
      local_spec%num_levels = 1
      
      allocate(spec, source=local_spec)
      
      _RETURN(_SUCCESS)
   end function create_spec_from_file_metadata

   function create_grid_from_spec(this, spec, rc) result(grid)
      class(VerticalGrid), allocatable :: grid
      class(BasicVerticalGridFactory), intent(in) :: this
      class(VerticalGridSpec), intent(in) :: spec
      integer, intent(out), optional :: rc
      
      type(BasicVerticalGrid) :: local_grid
      integer :: status
      
      select type (spec)
      type is (BasicVerticalGridSpec)
         call local_grid%initialize(spec)
         allocate(grid, source=local_grid)
      class default
         _RETURN(_FAILURE)
      end select
      
      _RETURN(_SUCCESS)
   end function create_grid_from_spec

end module mapl3g_BasicVerticalGrid

