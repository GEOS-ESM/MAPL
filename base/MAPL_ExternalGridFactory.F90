#include "MAPL_Generic.h"

module MAPL_ExternalGridFactoryMod
   use MAPL_AbstractGridFactoryMod
   use MAPL_MinMaxMod
   use MAPL_KeywordEnforcerMod
   use mapl_ErrorHandlingMod
   use ESMF
   use pFIO
   use MAPL_CommsMod
   use MAPL_ConstantsMod
   use MAPL_IOMod, only : GETFILE, FREE_FILE 
   use, intrinsic :: iso_fortran_env, only: REAL64,REAL32

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
      procedure :: append_variable_metadata
      procedure :: generate_file_bounds
      procedure :: generate_file_corner_bounds
      procedure :: generate_file_reference2D
      procedure :: generate_file_reference3D
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

      character(len=*), parameter :: Iam = MOD_NAME // 'ExternalGridFactory_from_parameters'

      _UNUSED_DUMMY(unusable)

      if (present(grid_name)) factory%grid_name = grid_name
      if (present(grid)) factory%external_grid = grid
      if (present(grid)) factory%lm = lm

      _RETURN(_SUCCESS)
   end function ExternalGridFactory_from_parameters

   function make_new_grid(this, unusable, rc) result(grid)
      type(ESMF_Grid) :: grid
      class(ExternalGridFactory),       intent(in   ) :: this
      class(KeywordEnforcer), optional, intent(in   ) :: unusable
      integer,                optional, intent(  out) :: rc

      character(len=*), parameter :: Iam = MOD_NAME // 'make_grid'
      logical                     :: is_present
      integer                     :: status, lm

      _UNUSED_DUMMY(unusable)

      if (allocated(this%external_grid)) then
         grid = this%external_grid
      else
         _FAIL('grid not allocated')
      end if

      if (allocated(this%lm)) then
         call ESMF_AttributeGet(grid, name='GRID_LM', isPresent=is_present, rc=status)
         _VERIFY(status)

         if (is_present) then
            call ESMF_AttributeGet(grid, name='GRID_LM', value=lm, rc=status)
            _VERIFY(status)

            _ASSERT(lm == this%lm,'inconsistent levels')
         else
            call ESMF_AttributeSet(grid, name='GRID_LM', value=this%lm, rc=status)
            _VERIFY(status)
         end if
      end if

      _RETURN(_SUCCESS)
   end function make_new_grid

   logical function equals(a, b)
      class(ExternalGridFactory), intent(in) :: a
      class(AbstractGridFactory), intent(in) :: b

      _UNUSED_DUMMY(a)
      select type(b)
      class default
         equals = .false.
         return
      class is (ExternalGridFactory)
         equals = .true.
      end select
   end function equals

   subroutine initialize_from_file_metadata(this, file_metadata, unusable, rc)
      class(ExternalGridFactory),       intent(inout) :: this
      type(FileMetadata),     target,   intent(in   ) :: file_metadata
      class(KeywordEnforcer), optional, intent(in   ) :: unusable
      integer,                optional, intent(  out) :: rc

      character(len=*), parameter :: Iam = MOD_NAME // 'initialize_from_file_metadata'

      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(unusable)
      _UNUSED_DUMMY(file_metadata)

      _FAIL('unimplemented')

      _RETURN(_SUCCESS)
   end subroutine initialize_from_file_metadata

   subroutine initialize_from_config_with_prefix(this, config, prefix, unusable, rc)
      class(ExternalGridFactory),       intent(inout) :: this
      type(ESMF_Config),                intent(inout) :: config
      character(*),                     intent(in   ) :: prefix
      class(KeywordEnforcer), optional, intent(in   ) :: unusable
      integer,                optional, intent(  out) :: rc

      character(len=*), parameter :: Iam = MOD_NAME // 'initialize_from_config_with_prefix'

      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(unusable)
      _UNUSED_DUMMY(config)
      _UNUSED_DUMMY(prefix)

      _FAIL('unimplemented')

      _RETURN(_SUCCESS)
   end subroutine initialize_from_config_with_prefix

   subroutine initialize_from_esmf_distGrid(this, dist_grid, lon_array, lat_array, unusable, rc)
      class(ExternalGridFactory),       intent(inout) :: this
      type(ESMF_DistGrid),              intent(in   ) :: dist_grid
      type(ESMF_LocalArray),            intent(in   ) :: lon_array
      type(ESMF_LocalArray),            intent(in   ) :: lat_array
      class(KeywordEnforcer), optional, intent(in   ) :: unusable
      integer,                optional, intent(  out) :: rc

      character(len=*), parameter :: Iam = MOD_NAME // 'initialize_from_esmf_distGrid'

      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(unusable)
      _UNUSED_DUMMY(dist_grid)
      _UNUSED_DUMMY(lon_array)
      _UNUSED_DUMMY(lat_array)

      _FAIL('unimplemented')

      _RETURN(_SUCCESS)
   end subroutine initialize_from_esmf_distGrid

   subroutine halo(this, array, unusable, halo_width, rc)
      class(ExternalGridFactory),       intent(inout) :: this
      real(kind=REAL32),                intent(inout) :: array(:,:)
      class(KeywordEnforcer), optional, intent(in   ) :: unusable
      integer,                optional, intent(in   ) :: halo_width
      integer,                optional, intent(  out) :: rc

      character(len=*), parameter :: Iam = MOD_NAME // 'halo'

      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(unusable)
      _UNUSED_DUMMY(array)
      _UNUSED_DUMMY(halo_width)

      _FAIL('unimplemented')

      _RETURN(_SUCCESS)
   end subroutine halo

   function generate_grid_name(this) result(name)
      character(:), allocatable :: name
      class(ExternalGridFactory), intent(in) :: this

      _UNUSED_DUMMY(this)
      name = 'EXTERNAL'
   end function generate_grid_name

   subroutine append_metadata(this, metadata)
      class(ExternalGridFactory), intent(inout) :: this
      type(FileMetadata),         intent(inout) :: metadata

      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(metadata)
      ! TODO: fill in the rest
   end subroutine append_metadata

   function get_grid_vars(this) result(vars)
      character(:), allocatable :: vars
      class(ExternalGridFactory), intent(inout) :: this

      _UNUSED_DUMMY(this)
      vars = ''
   end function get_grid_vars

   subroutine append_variable_metadata(this, var)
      class(ExternalGridFactory), intent(inout) :: this
      type(Variable),             intent(inout) :: var

      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(var)
      ! TODO: fill in the rest
   end subroutine append_variable_metadata

   subroutine generate_file_bounds(this, grid, local_start, global_start, global_count, rc)
      class(ExternalGridFactory), intent(inout) :: this
      type(ESMF_Grid),            intent(inout) :: grid
      integer,      allocatable,  intent(  out) :: local_start(:)
      integer,      allocatable,  intent(  out) :: global_start(:)
      integer,      allocatable,  intent(  out) :: global_count(:)
      integer,      optional,     intent(  out) :: rc

      character(len=*), parameter :: Iam = MOD_NAME // 'generate_file_bounds'

      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(grid)
      _UNUSED_DUMMY(local_start)
      _UNUSED_DUMMY(global_start)
      _UNUSED_DUMMY(global_count)

      _FAIL('unimplemented')

      _RETURN(_SUCCESS)
   end subroutine generate_file_bounds

   subroutine generate_file_corner_bounds(this, grid, local_start, global_start, global_count, rc)
      class(ExternalGridFactory), intent(inout) :: this
      type(ESMF_Grid),            intent(inout) :: grid
      integer,      allocatable,  intent(  out) :: local_start(:)
      integer,      allocatable,  intent(  out) :: global_start(:)
      integer,      allocatable,  intent(  out) :: global_count(:)
      integer,      optional,     intent(  out) :: rc

      character(len=*), parameter :: Iam = MOD_NAME // 'generate_file_bounds'

      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(grid)
      _UNUSED_DUMMY(local_start)
      _UNUSED_DUMMY(global_start)
      _UNUSED_DUMMY(global_count)

      _RETURN(_SUCCESS)
   end subroutine generate_file_corner_bounds

   function generate_file_reference2D(this, fpointer) result(ref)
      type(ArrayReference) :: ref
      class(ExternalGridFactory), intent(inout) :: this
      real, pointer,              intent(in   ) :: fpointer(:,:)

      _UNUSED_DUMMY(this)
      ref = ArrayReference(fpointer)
   end function generate_file_reference2D

   function generate_file_reference3D(this, fpointer) result(ref)
      type(ArrayReference) :: ref
      class(ExternalGridFactory), intent(inout) :: this
      real, pointer,              intent(in   ) :: fpointer(:,:,:)

      _UNUSED_DUMMY(this)
      ref = ArrayReference(fpointer)
   end function generate_file_reference3D

end module MAPL_ExternalGridFactoryMod
