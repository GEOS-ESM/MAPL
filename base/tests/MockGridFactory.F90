#define _success      0
#define _failure     1
#define _verify(A)   if(  A/=0) then; if(present(rc)) rc=A; PRINT *, Iam, __LINE__; return; endif
#define _assert(A)   if(.not.A) then; if(present(rc)) rc=_failure; PRINT *, Iam, __LINE__; return; endif
#define _return(A)   if(present(rc)) rc=A; return
#include "unused_dummy.H"

module MockGridFactoryMod
   use MAPL_AbstractGridFactoryMod
   use MAPL_KeywordEnforcerMod
   use pFIO
   implicit none
   private

   public :: MockGridFactory

   type, extends(AbstractGridFactory) :: MockGridFactory
      private
      character(len=:), allocatable :: name
   contains
      procedure :: make_new_grid
      procedure :: initialize_from_config_with_prefix
      procedure :: initialize_from_esmf_distgrid
      procedure :: halo
      procedure :: generate_grid_name
      procedure :: equals
      procedure :: to_string
      procedure :: initialize_from_file_metadata
      procedure :: get_grid_vars

      procedure :: decomps_are_equal
      procedure :: physical_params_are_equal
      procedure :: append_metadata
      procedure :: append_variable_metadata
      procedure :: generate_file_bounds
      procedure :: generate_file_corner_bounds
      procedure :: generate_file_reference2D
      procedure :: generate_file_reference3D
      procedure :: get_file_format_vars
   end type MockGridFactory

   interface MockGridFactory
      module procedure newMockGridFactory
   end interface MockGridFactory

   character(len=*), parameter :: MOD_NAME = 'MockGridFactory::'

contains


   function newMockGridFactory(name) result(factory)
      type (MockGridFactory) :: factory
      character(len=*), intent(in) :: name

      factory%name = name

   end function newMockGridFactory

   subroutine initialize_from_config_with_prefix(this, config, prefix, unusable, rc)
      use ESMF
      class (MockGridFactory), intent(inout) :: this
      type (ESMF_Config), intent(inout) :: config
      character(len=*), intent(in) :: prefix
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc
      _unused_dummy(this)
      _unused_dummy(config)
      _unused_dummy(prefix)
      _unused_dummy(unusable)
      _unused_dummy(rc)
   end subroutine initialize_from_config_with_prefix


   function make_new_grid(this, unusable, rc) result(grid)
      use ESMF
      type (ESMF_Grid) :: grid
      class (MockGridFactory), intent(in) :: this
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      _unused_dummy(this)
      _unused_dummy(unusable)
      _unused_dummy(rc)

      grid = ESMF_GridEmptyCreate()
      call ESMF_AttributeSet(grid, 'GRID_NAME', this%name)
      call ESMF_AttributeSet(grid, 'GridType', this%name)

      _return(_success)

   end function make_new_grid

   function physical_params_are_equal(this,a) result(equal)
      class (MockGridFactory), intent(in) :: this
      class (AbstractGridFactory), intent(in) :: a
      logical :: equal

      select type (a)
      class default
         equal = .false.
         return
      class is (MockGridFactory)
         equal = .true.
      end select
   end function physical_params_are_equal

   function decomps_are_equal(this,a) result(equal)
      class (MockGridFactory), intent(in) :: this
      class (AbstractGridFactory), intent(in) :: a
      logical :: equal

      select type (a)
      class default
         equal = .false.
         return
      class is (MockGridFactory)
         equal = .true.
      end select
   end function decomps_are_equal

   logical function equals(a, b)
      class (MockGridFactory), intent(in) :: a
      class (AbstractGridFactory), intent(in) :: b

      select type (b)
      class default
         equals = .false.
         return
      class is (MockGridFactory)
         equals = (a%name == b%name)
      end select
         
   end function equals


   function to_string(this) result(string)
      class (MockGridFactory), intent(in) :: this
      character(len=:), allocatable :: string

      string = this%name

   end function to_string

   subroutine initialize_from_esmf_distGrid(this, dist_grid, lon_array, lat_array, unusable, rc) 
      use ESMF
      use MAPL_KeywordEnforcerMod
      class (MockGridFactory), intent(inout)  :: this
      type (ESMF_DistGrid), intent(in) :: dist_grid
      type (ESMF_LocalArray), intent(in) :: lon_array
      type (ESMF_LocalArray), intent(in) :: lat_array
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc
      _unused_dummy(this)
      _unused_dummy(dist_grid)
      _unused_dummy(lon_array)
      _unused_dummy(lat_array)
      _unused_dummy(unusable)
      _unused_dummy(rc)
   end subroutine initialize_from_esmf_distGrid


   subroutine halo(this, array, unusable, halo_width, rc)
      use, intrinsic :: iso_fortran_env, only: REAL32
      use MAPL_KeywordEnforcerMod
      class (MockGridFactory), intent(inout) :: this
      real(kind=REAL32), intent(inout) :: array(:,:)
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(in) :: halo_width
      integer, optional, intent(out) :: rc
      _unused_dummy(this)
      _unused_dummy(array)
      _unused_dummy(unusable)
      _unused_dummy(halo_width)
      _unused_dummy(rc)
   end subroutine halo

   
   function generate_grid_name(this) result(name)
      character(len=:), allocatable :: name
      class (MockGridFactory), intent(in) :: this
      name = 'MockGridFactory'
      _unused_dummy(this)
   end function generate_grid_name

   subroutine initialize_from_file_metadata(this, file_metadata, unusable, force_file_coordinates, rc)
      use MAPL_KeywordEnforcerMod
      class (MockGridFactory), intent(inout)  :: this
      type (FileMetadata), target, intent(in) :: file_metadata
      class (KeywordEnforcer), optional, intent(in) :: unusable
      logical, optional, intent(in) :: force_file_coordinates
      integer, optional, intent(out) :: rc
      _unused_dummy(this)
      _unused_dummy(file_metadata)
      _unused_dummy(unusable)
      _unused_dummy(rc)
   end subroutine initialize_from_file_metadata


   subroutine append_metadata(this, metadata)
      class (MockGridFactory), intent(inout) :: this
      type (FileMetadata), intent(inout) :: metadata

      _unused_dummy(this)
      _unused_dummy(metadata)
      
!!$      ! Horizontal grid dimensions
!!$      call metadata%add_dimension('lon', this%im_world)
!!$      call metadata%add_dimension('lat', this%jm_world)
   end subroutine append_metadata

   function get_grid_vars(this) result(vars)
      class (MockGridFactory), intent(inout) :: this

      character(len=:), allocatable :: vars
      _unused_dummy(this)

      vars = 'lon,lat, mock'

   end function get_grid_vars

   subroutine append_variable_metadata(this,var)
      class (MockGridFactory), intent(inout) :: this
      type(Variable), intent(inout) :: var
      _unused_dummy(this)
      _unused_dummy(var)
   end subroutine append_variable_metadata

   subroutine generate_file_bounds(this,grid,local_start,global_start,global_count,metadata,rc)
      use MAPL_BaseMod
      use ESMF
      class(MockGridFactory), intent(inout) :: this
      type(ESMF_Grid),      intent(inout) :: grid
      integer, allocatable, intent(out) :: local_start(:)
      integer, allocatable, intent(out) :: global_start(:)
      integer, allocatable, intent(out) :: global_count(:)
      type(FileMetaData), intent(in), optional :: metaData
      integer, optional, intent(out) :: rc

      _unused_dummy(this)
      _unused_dummy(grid)
      _unused_dummy(local_start)
      _unused_dummy(global_start)
      _unused_dummy(global_count)
      _unused_dummy(rc)

   end subroutine generate_file_bounds

   subroutine generate_file_corner_bounds(this,grid,local_start,global_start,global_count,rc)
      use MAPL_BaseMod
      use ESMF
      class(MockGridFactory), intent(inout) :: this
      type(ESMF_Grid),      intent(inout) :: grid
      integer, allocatable, intent(out) :: local_start(:)
      integer, allocatable, intent(out) :: global_start(:)
      integer, allocatable, intent(out) :: global_count(:)
      integer, optional, intent(out) :: rc

      _unused_dummy(this)
      _unused_dummy(grid)
      _unused_dummy(local_start)
      _unused_dummy(global_start)
      _unused_dummy(global_count)
      _unused_dummy(rc)

   end subroutine generate_file_corner_bounds

   function generate_file_reference2D(this,fpointer) result(ref)
      use pFIO
      type(ArrayReference) :: ref
      class(MockGridFactory), intent(inout) :: this
      real, pointer, intent(in) :: fpointer(:,:)
      _unused_dummy(this)
      ref = ArrayReference(fpointer)
   end function generate_file_reference2D

   function generate_file_reference3D(this,fpointer,metadata) result(ref)
      use pFIO
      type(ArrayReference) :: ref
      class(MockGridFactory), intent(inout) :: this
      type(FileMetaData), intent(in), optional :: metaData
      real, pointer, intent(in) :: fpointer(:,:,:)
      _unused_dummy(this)
      ref = ArrayReference(fpointer)
   end function generate_file_reference3D

   function get_file_format_vars(this) result(vars)
      class (MockGridFactory), intent(inout) :: this
      character(len=:), allocatable :: vars
      vars=""
   end function get_file_format_vars

end module MockGridFactoryMod
