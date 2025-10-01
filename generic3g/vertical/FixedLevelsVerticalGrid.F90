#include "MAPL.h"
module mapl3g_FixedLevelsVerticalGrid
   use mapl3g_Field_API
   use mapl3g_VerticalGrid, only: VerticalGrid
   use mapl3g_VerticalGridSpec, only: VerticalGridSpec
   use mapl3g_VerticalGridFactory, only: VerticalGridFactory
   use mapl3g_BasicVerticalGrid
   use mapl3g_ComponentDriver
   use mapl3g_FieldCondensedArray, only: assign_fptr_condensed_array
   use pfio
   use esmf, only: esmf_HConfig, esmf_Field, esmf_Geom, esmf_TypeKind_Flag
   use esmf
   use mapl3g_VerticalStaggerLoc
   use gftl2_StringVector, only: StringVector
   use mapl_ErrorHandling
   implicit none(type,external)
   private
   
   public :: FixedLevelsVerticalGrid
   public :: FixedLevelsVerticalGridSpec
   public :: FixedLevelsVerticalGridFactory
   public :: get_default_units
   
   ! Spec type
   type, extends(VerticalGridSpec) :: FixedLevelsVerticalGridSpec
      character(len=:), allocatable :: physical_dimension
      real, allocatable :: levels(:)
      character(len=:), allocatable :: units
   end type FixedLevelsVerticalGridSpec
   
   ! Grid type
   type, extends(VerticalGrid) :: FixedLevelsVerticalGrid
      private
      type(FixedLevelsVerticalGridSpec) :: spec
   contains
      procedure :: initialize
      procedure :: get_levels
      procedure :: get_physical_dimension
      procedure :: get_units
      procedure :: get_num_levels
      procedure :: get_coordinate_field
      procedure :: get_supported_physical_dimensions
      procedure :: matches
   end type FixedLevelsVerticalGrid
   
   ! Factory type
   type, extends(VerticalGridFactory) :: FixedLevelsVerticalGridFactory
   contains
      procedure :: get_name
      procedure :: supports_spec
      procedure :: supports_file_metadata
      procedure :: supports_config
      procedure :: create_spec_from_config
      procedure :: create_spec_from_file_metadata
      procedure :: create_grid_from_spec
   end type FixedLevelsVerticalGridFactory

   interface FixedLevelsVerticalGridSpec
      procedure :: new_FixedLevelsVerticalGridSpec
   end interface FixedLevelsVerticalGridSpec

contains

   function new_FixedLevelsVerticalGridSpec(physical_dimension, levels, units) result(spec)
      type(FixedLevelsVerticalGridSpec) :: spec
      character(*), intent(in) :: physical_dimension
      real, intent(in) :: levels(:)
      character(*), intent(in) :: units

      spec%physical_dimension = physical_dimension
      spec%levels = levels
      spec%units = units
   end function new_FixedLevelsVerticalGridSpec


   subroutine initialize(this, spec)
      class(FixedLevelsVerticalGrid), intent(inout) :: this
      type(FixedLevelsVerticalGridSpec), intent(in) :: spec

      this%spec = spec
   end subroutine initialize

   function get_levels(this) result(levels)
      real, allocatable :: levels(:)
      class(FixedLevelsVerticalGrid), intent(in) :: this
      
      levels = this%spec%levels
   end function get_levels

   function get_physical_dimension(this) result(physical_dimension)
      character(len=:), allocatable :: physical_dimension
      class(FixedLevelsVerticalGrid), intent(in) :: this
      
      physical_dimension = this%spec%physical_dimension
   end function get_physical_dimension

   function get_units(this, physical_dimension, rc) result(units)
      character(len=:), allocatable :: units
      class(FixedLevelsVerticalGrid), intent(in) :: this
      character(len=*), intent(in) :: physical_dimension
      integer, optional, intent(out) :: rc

      integer :: status
      _ASSERT(physical_dimension == this%get_physical_dimension(), 'Unsupported physical dimension: '//physical_dimension)
      units = this%spec%units

      _RETURN(_SUCCESS)
   end function get_units

   function get_num_levels(this) result(num_levels)
      integer :: num_levels
      class(FixedLevelsVerticalGrid), intent(in) :: this

      num_levels = size(this%spec%levels)
   end function get_num_levels

   function get_coordinate_field(this, geom, physical_dimension, units, typekind, coupler, rc) result(field)
      type(esmf_Field) :: field
      class(FixedLevelsVerticalGrid), intent(in) :: this
      type(esmf_Geom), intent(in) :: geom
      character(len=*), intent(in) :: physical_dimension
      character(len=*), intent(in) :: units
      type(esmf_TypeKind_Flag), intent(in) :: typekind
      class(ComponentDriver), pointer, intent(out) :: coupler
      integer, intent(out), optional :: rc
      
      integer :: status
      real(kind=ESMF_KIND_R4), pointer :: farray3d(:, :, :)
      integer :: shape_(3), horz, ungrd

      coupler => null()
      field = MAPL_FieldCreate( &
           geom=geom, &
           typekind=ESMF_TYPEKIND_R4, &
           num_levels=size(this%spec%levels), &
           vert_staggerloc=VERTICAL_STAGGER_CENTER, &
           _RC)


      ! Copy the 1D array, levels(:), to each point of the horz grid
      call assign_fptr_condensed_array(field, farray3d, _RC)
      shape_ = shape(farray3d)
      do concurrent (horz=1:shape_(1), ungrd=1:shape_(3))
         farray3d(horz, :, ungrd) = this%spec%levels(:)
      end do

      
      _RETURN(_SUCCESS)
   end function get_coordinate_field

   function get_supported_physical_dimensions(this) result(dimensions)
      type(StringVector) :: dimensions
      class(FixedLevelsVerticalGrid), target, intent(in) :: this
      
      call dimensions%push_back(this%get_physical_dimension())
   end function get_supported_physical_dimensions

   logical function matches(this, other)
      class(FixedLevelsVerticalGrid), intent(in) :: this
      class(VerticalGrid), intent(in) :: other

      type(StringVector) :: supported_dims

      matches = this%get_num_levels() == other%get_num_levels()
      if (.not. matches) return

      select type (other)
      type is (BasicVerticalGrid)
         matches = .true.
         return
      class default
         matches = .false.
      end select
      
   end function matches

   ! Factory methods
   function get_name(this) result(name)
      character(len=:), allocatable :: name
      class(FixedLevelsVerticalGridFactory), intent(in) :: this
      
      name = "FixedLevelsVerticalGrid"
   end function get_name

   function supports_spec(this, spec, rc) result(is_supported)
      logical :: is_supported
      class(FixedLevelsVerticalGridFactory), intent(in) :: this
      class(VerticalGridSpec), intent(in) :: spec
      integer, optional, intent(out) :: rc

      integer :: status
      type(FixedLevelsVerticalGridSpec) :: fixed_spec

      is_supported = same_type_as(spec, fixed_spec)

      _RETURN(_SUCCESS)
   end function supports_spec

   function supports_file_metadata(this, file_metadata, rc) result(is_supported)
      logical :: is_supported
      class(FixedLevelsVerticalGridFactory), intent(in) :: this
      type(FileMetadata), intent(in), target :: file_metadata
      integer, optional, intent(out) :: rc
      
      ! Implementation would check if file_metadata contains required information
      is_supported = .false.  ! Placeholder
      _RETURN(_SUCCESS)
   end function supports_file_metadata

   function supports_config(this, config, rc) result(is_supported)
      logical :: is_supported
      class(FixedLevelsVerticalGridFactory), intent(in) :: this
      type(esmf_HConfig), intent(in) :: config
      integer, optional, intent(out) :: rc

      logical :: has_levels
      logical :: has_physical_dimension
      logical :: has_grid_type
      character(:), allocatable :: grid_type
      integer :: status

      is_supported = .false.

      has_grid_type = esmf_HConfigIsDefined(config, keyString="grid_type", _RC)
      if (has_grid_type) then
         grid_type = esmf_HConfigAsString(config, keyString="grid_type", _RC)
         _RETURN_UNLESS(grid_type == 'fixed_levels')
      end if
      has_levels = esmf_HConfigIsDefined(config, keyString="levels", _RC)
      has_physical_dimension = esmf_HConfigIsDefined(config, keyString="physical_dimension", _RC)

      is_supported = has_levels .and. has_physical_dimension
           
      _RETURN(_SUCCESS)
   end function supports_config

   function create_spec_from_config(this, config, rc) result(spec)
      class(VerticalGridSpec), allocatable :: spec
      class(FixedLevelsVerticalGridFactory), intent(in) :: this
      type(esmf_HConfig), intent(in), target :: config
      integer, intent(out), optional :: rc
      
      type(FixedLevelsVerticalGridSpec) :: local_spec
      integer :: status
      
      _ASSERT(this%supports(config), 'FixedLevelsVerticalGridFactory does not support this configuration')
      
      ! Get physical dimension (required)
      local_spec%physical_dimension = esmf_HConfigAsString(config, keyString="physical_dimension", _RC)
      _ASSERT(len_trim(local_spec%physical_dimension) > 0, 'Physical dimension cannot be empty')
      
      ! Get levels (required)
      local_spec%levels = esmf_HConfigAsR4Seq(config, keyString="levels", _RC)
      _ASSERT(allocated(local_spec%levels), 'Levels array must be specified')
      _ASSERT(size(local_spec%levels) > 0, 'Levels array cannot be empty')
      
      ! Get units (optional - use default if not specified)
      if (esmf_HConfigIsDefined(config, keyString="units")) then
         local_spec%units = esmf_HConfigAsString(config, keyString="units", _RC)
      else
         local_spec%units = get_default_units(local_spec%physical_dimension)
      end if
      
      ! Use polymorphic allocation
      allocate(spec, source=local_spec)
      
      _RETURN(_SUCCESS)
   end function create_spec_from_config

   function create_spec_from_file_metadata(this, file_metadata, rc) result(spec)
      class(VerticalGridSpec), allocatable :: spec
      class(FixedLevelsVerticalGridFactory), intent(in) :: this
      type(FileMetadata), intent(in), target :: file_metadata
      integer, intent(out), optional :: rc
      
      ! Placeholder implementation
      integer :: status
      _RETURN(_FAILURE)
   end function create_spec_from_file_metadata

   function create_grid_from_spec(this, spec, rc) result(grid)
      class(VerticalGrid), allocatable :: grid
      class(FixedLevelsVerticalGridFactory), intent(in) :: this
      class(VerticalGridSpec), intent(in) :: spec
      integer, intent(out), optional :: rc
      
      type(FixedLevelsVerticalGrid) :: local_grid
      integer :: status
      
      select type (spec)
      type is (FixedLevelsVerticalGridSpec)
         call local_grid%initialize(spec)
         allocate(grid, source=local_grid)
      class default
         _RETURN(_FAILURE)
      end select
      
      _RETURN(_SUCCESS)
   end function create_grid_from_spec

   ! Helper function to get default units for a physical dimension
   function get_default_units(physical_dimension) result(units)
      character(len=:), allocatable :: units
      character(len=*), intent(in) :: physical_dimension
      
      select case (physical_dimension)
         case ('pressure')
            units = 'Pa'
         case ('height', 'altitude')
            units = 'm'
         case ('depth')
            units = 'm'
         case ('layer')
            units = '1'
         case default
            units = '<unknown>'
      end select
   end function get_default_units

end module mapl3g_FixedLevelsVerticalGrid

