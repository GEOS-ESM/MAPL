#include "MAPL.h"

module MAPL_ExternalGridFactoryMod
   use MAPL_AbstractGridFactoryMod
   use MAPL_MinMaxMod
   use MAPL_KeywordEnforcerMod
   use mapl_ErrorHandlingMod
   use ESMF
   use pFIO
   use MAPL_CommsMod
   use MAPL_Constants
   use, intrinsic :: iso_fortran_env, only: REAL32

   implicit none
   private

   public :: ExternalGridFactory

   character(len=*), parameter :: MOD_NAME = 'ExternalGridFactory::'

   type, extends(AbstractGridFactory) :: ExternalGridFactory
      character(len=:), allocatable :: grid_name
      type(ESMF_Grid),  allocatable :: external_grid
      integer,          allocatable :: lm
   contains
      procedure :: make_new_grid

      procedure :: equals

      procedure :: initialize_from_file_metadata
      procedure :: initialize_from_config_with_prefix
      procedure :: initialize_from_esmf_distGrid

      procedure :: halo

      procedure :: generate_grid_name

      procedure :: append_metadata
      procedure :: get_grid_vars
      procedure :: get_file_format_vars
      procedure :: append_variable_metadata
      procedure :: generate_file_bounds
      procedure :: generate_file_corner_bounds
      procedure :: generate_file_reference2D
      procedure :: generate_file_reference3D
      procedure :: decomps_are_equal
      procedure :: physical_params_are_equal
   end type ExternalGridFactory

   interface ExternalGridFactory
      module procedure ExternalGridFactory_from_parameters
   end interface ExternalGridFactory

contains

   function ExternalGridFactory_from_parameters(unusable, grid_name, grid, lm, rc) result(factory)
      type(ExternalGridFactory) :: factory
      class(KeywordEnforcer), optional, intent(in   ) :: unusable
      character(*),           optional, intent(in   ) :: grid_name
      type(ESMF_Grid),        optional, intent(in   ) :: grid
      integer,                optional, intent(in   ) :: lm
      integer,                optional, intent(  out) :: rc

      if (present(grid_name)) factory%grid_name = grid_name
      if (present(grid)) factory%external_grid = grid
      if (present(grid)) factory%lm = lm

      _RETURN(_SUCCESS)

      _UNUSED_DUMMY(unusable)
   end function ExternalGridFactory_from_parameters

   function make_new_grid(this, unusable, rc) result(grid)
      type(ESMF_Grid) :: grid
      class(ExternalGridFactory),       intent(in   ) :: this
      class(KeywordEnforcer), optional, intent(in   ) :: unusable
      integer,                optional, intent(  out) :: rc

      logical                     :: is_present
      integer                     :: status, lm
      type(ESMF_Info)             :: infoh

      if (allocated(this%external_grid)) then
         grid = this%external_grid
      else
         _FAIL('grid not allocated')
      end if

      if (allocated(this%lm)) then
         call ESMF_InfoGetFromHost(grid,infoh,rc=status)
         _VERIFY(status)
         is_present = ESMF_InfoIsPresent(infoh,'GRID_LM',rc=status)
         _VERIFY(status)

         if (is_present) then
            call ESMF_InfoGet(infoh,'GRID_LM',lm,rc=status)
            _VERIFY(status)

            _ASSERT(lm == this%lm,'inconsistent levels')
         else
            call ESMF_InfoSet(infoh,'GRID_LM',this%lm,rc=status)
            _VERIFY(status)
         end if
      end if

      _RETURN(_SUCCESS)

      _UNUSED_DUMMY(unusable)
   end function make_new_grid

   function decomps_are_equal(this,a) result(equal)
      class(ExternalGridFactory), intent(in) :: this
      class(AbstractGridFactory), intent(in) :: a
      logical :: equal

      select type(a)
      class default
         equal = .false.
         return
      class is (ExternalGridFactory)
         equal = .true.
      end select

      _UNUSED_DUMMY(this)
   end function decomps_are_equal

   function physical_params_are_equal(this,a) result(equal)
      class(ExternalGridFactory), intent(in) :: this
      class(AbstractGridFactory), intent(in) :: a
      logical :: equal

      select type(a)
      class default
         equal = .false.
         return
      class is (ExternalGridFactory)
         equal = .true.
      end select

      _UNUSED_DUMMY(this)
   end function physical_params_are_equal

   logical function equals(a, b)
      class(ExternalGridFactory), intent(in) :: a
      class(AbstractGridFactory), intent(in) :: b

      select type(b)
      class default
         equals = .false.
         return
      class is (ExternalGridFactory)
         equals = .true.
      end select

      _UNUSED_DUMMY(a)
   end function equals

   subroutine initialize_from_file_metadata(this, file_metadata, unusable, force_file_coordinates, rc)
      class(ExternalGridFactory),       intent(inout) :: this
      type(FileMetadata),     target,   intent(in   ) :: file_metadata
      class(KeywordEnforcer), optional, intent(in   ) :: unusable
      logical, optional, intent(in) :: force_file_coordinates
      integer,                optional, intent(  out) :: rc

      _RETURN(_FAILURE)

      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(unusable)
      _UNUSED_DUMMY(file_metadata)
      _UNUSED_DUMMY(force_file_coordinates)
   end subroutine initialize_from_file_metadata

   subroutine initialize_from_config_with_prefix(this, config, prefix, unusable, rc)
      class(ExternalGridFactory),       intent(inout) :: this
      type(ESMF_Config),                intent(inout) :: config
      character(*),                     intent(in   ) :: prefix
      class(KeywordEnforcer), optional, intent(in   ) :: unusable
      integer,                optional, intent(  out) :: rc

      _RETURN(_FAILURE)

      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(unusable)
      _UNUSED_DUMMY(config)
      _UNUSED_DUMMY(prefix)
   end subroutine initialize_from_config_with_prefix

   subroutine initialize_from_esmf_distGrid(this, dist_grid, lon_array, lat_array, unusable, rc)
      class(ExternalGridFactory),       intent(inout) :: this
      type(ESMF_DistGrid),              intent(in   ) :: dist_grid
      type(ESMF_LocalArray),            intent(in   ) :: lon_array
      type(ESMF_LocalArray),            intent(in   ) :: lat_array
      class(KeywordEnforcer), optional, intent(in   ) :: unusable
      integer,                optional, intent(  out) :: rc

      _RETURN(_FAILURE)

      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(unusable)
      _UNUSED_DUMMY(dist_grid)
      _UNUSED_DUMMY(lon_array)
      _UNUSED_DUMMY(lat_array)
   end subroutine initialize_from_esmf_distGrid

   subroutine halo(this, array, unusable, halo_width, rc)
      class(ExternalGridFactory),       intent(inout) :: this
      real(kind=REAL32),                intent(inout) :: array(:,:)
      class(KeywordEnforcer), optional, intent(in   ) :: unusable
      integer,                optional, intent(in   ) :: halo_width
      integer,                optional, intent(  out) :: rc

      _RETURN(_FAILURE)

      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(unusable)
      _UNUSED_DUMMY(array)
      _UNUSED_DUMMY(halo_width)
   end subroutine halo

   function generate_grid_name(this) result(name)
      character(:), allocatable :: name
      class(ExternalGridFactory), intent(in) :: this

      name = 'EXTERNAL'

      _UNUSED_DUMMY(this)
   end function generate_grid_name

   subroutine append_metadata(this, metadata)
      class(ExternalGridFactory), intent(inout) :: this
      type(FileMetadata),         intent(inout) :: metadata

      ! Unimplemented

      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(metadata)
   end subroutine append_metadata

   function get_grid_vars(this) result(vars)
      character(:), allocatable :: vars
      class(ExternalGridFactory), intent(inout) :: this

      vars = ''
      _UNUSED_DUMMY(this)
   end function get_grid_vars

   function get_file_format_vars(this) result(vars)
      character(:), allocatable :: vars
      class(ExternalGridFactory), intent(inout) :: this

      vars = ''
      _UNUSED_DUMMY(this)
   end function get_file_format_vars

   subroutine append_variable_metadata(this, var)
      class(ExternalGridFactory), intent(inout) :: this
      type(Variable),             intent(inout) :: var

      ! TODO: fill in the rest
      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(var)
   end subroutine append_variable_metadata

   subroutine generate_file_bounds(this, grid, local_start, global_start, global_count, metadata, rc)
      class(ExternalGridFactory), intent(inout) :: this
      type(ESMF_Grid),            intent(inout) :: grid
      integer,      allocatable,  intent(  out) :: local_start(:)
      integer,      allocatable,  intent(  out) :: global_start(:)
      integer,      allocatable,  intent(  out) :: global_count(:)
      type(FileMetaData), intent(in), optional :: metaData
      integer,      optional,     intent(  out) :: rc

      _RETURN(_FAILURE)

      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(grid)
      _UNUSED_DUMMY(local_start)
      _UNUSED_DUMMY(global_start)
      _UNUSED_DUMMY(global_count)
      _UNUSED_DUMMY(metaData)
   end subroutine generate_file_bounds

   subroutine generate_file_corner_bounds(this, grid, local_start, global_start, global_count, rc)
      class(ExternalGridFactory), intent(inout) :: this
      type(ESMF_Grid),            intent(inout) :: grid
      integer,      allocatable,  intent(  out) :: local_start(:)
      integer,      allocatable,  intent(  out) :: global_start(:)
      integer,      allocatable,  intent(  out) :: global_count(:)
      integer,      optional,     intent(  out) :: rc

      _RETURN(_FAILURE)

      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(grid)
      _UNUSED_DUMMY(local_start)
      _UNUSED_DUMMY(global_start)
      _UNUSED_DUMMY(global_count)
   end subroutine generate_file_corner_bounds

   function generate_file_reference2D(this, fpointer) result(ref)
      type(ArrayReference) :: ref
      class(ExternalGridFactory), intent(inout) :: this
      real, pointer,              intent(in   ) :: fpointer(:,:)

      ref = ArrayReference(fpointer)
      _UNUSED_DUMMY(this)
   end function generate_file_reference2D

   function generate_file_reference3D(this, fpointer, metadata) result(ref)
      type(ArrayReference) :: ref
      class(ExternalGridFactory), intent(inout) :: this
      real, pointer,              intent(in   ) :: fpointer(:,:,:)
      type(FileMetaData), intent(in), optional :: metaData

      ref = ArrayReference(fpointer)
 
      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(metaData)
   end function generate_file_reference3D

end module MAPL_ExternalGridFactoryMod
