#define _SUCCESS      0
#define _FAILURE     1
#define _VERIFY(A)   if(  A/=0) then; if(present(rc)) rc=A; PRINT *, Iam, __LINE__; return; endif
#define _ASSERT(A)   if(.not.A) then; if(present(rc)) rc=_FAILURE; PRINT *, Iam, __LINE__; return; endif
#define _RETURN(A)   if(present(rc)) rc=A; return
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

      procedure :: append_metadata
      procedure :: append_variable_metadata
      procedure :: generate_file_bounds
      procedure :: generate_file_corner_bounds
      procedure :: generate_file_reference2D
      procedure :: generate_file_reference3D
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
      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(config)
      _UNUSED_DUMMY(prefix)
      _UNUSED_DUMMY(unusable)
      _UNUSED_DUMMY(rc)
   end subroutine initialize_from_config_with_prefix


   function make_new_grid(this, unusable, rc) result(grid)
      use ESMF
      type (ESMF_Grid) :: grid
      class (MockGridFactory), intent(in) :: this
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(unusable)
      _UNUSED_DUMMY(rc)

      grid = ESMF_GridEmptyCreate()
      call ESMF_AttributeSet(grid, 'GRID_NAME', this%name)
      call ESMF_AttributeSet(grid, 'GridType', this%name)

      _RETURN(_SUCCESS)

   end function make_new_grid



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
      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(dist_grid)
      _UNUSED_DUMMY(lon_array)
      _UNUSED_DUMMY(lat_array)
      _UNUSED_DUMMY(unusable)
      _UNUSED_DUMMY(rc)
   end subroutine initialize_from_esmf_distGrid


   subroutine halo(this, array, unusable, halo_width, rc)
      use, intrinsic :: iso_fortran_env, only: REAL32
      use MAPL_KeywordEnforcerMod
      class (MockGridFactory), intent(inout) :: this
      real(kind=REAL32), intent(inout) :: array(:,:)
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(in) :: halo_width
      integer, optional, intent(out) :: rc
      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(array)
      _UNUSED_DUMMY(unusable)
      _UNUSED_DUMMY(halo_width)
      _UNUSED_DUMMY(rc)
   end subroutine halo

   
   function generate_grid_name(this) result(name)
      character(len=:), allocatable :: name
      class (MockGridFactory), intent(in) :: this
      name = 'MockGridFactory'
      _UNUSED_DUMMY(this)
   end function generate_grid_name

   subroutine initialize_from_file_metadata(this, file_metadata, unusable, rc)
      use MAPL_KeywordEnforcerMod
      class (MockGridFactory), intent(inout)  :: this
      type (FileMetadata), target, intent(in) :: file_metadata
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc
      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(file_metadata)
      _UNUSED_DUMMY(unusable)
      _UNUSED_DUMMY(rc)
   end subroutine initialize_from_file_metadata


   subroutine append_metadata(this, metadata)
      class (MockGridFactory), intent(inout) :: this
      type (FileMetadata), intent(inout) :: metadata

      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(metadata)
      
!!$      ! Horizontal grid dimensions
!!$      call metadata%add_dimension('lon', this%im_world)
!!$      call metadata%add_dimension('lat', this%jm_world)
   end subroutine append_metadata

   function get_grid_vars(this) result(vars)
      class (MockGridFactory), intent(inout) :: this

      character(len=:), allocatable :: vars
      _UNUSED_DUMMY(this)

      vars = 'lon,lat, mock'

   end function get_grid_vars

   subroutine append_variable_metadata(this,var)
      class (MockGridFactory), intent(inout) :: this
      type(Variable), intent(inout) :: var
      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(var)
   end subroutine append_variable_metadata

   subroutine generate_file_bounds(this,grid,local_start,global_start,global_count,rc)
      use MAPL_BaseMod
      use ESMF
      class(MockGridFactory), intent(inout) :: this
      type(ESMF_Grid),      intent(inout) :: grid
      integer, allocatable, intent(out) :: local_start(:)
      integer, allocatable, intent(out) :: global_start(:)
      integer, allocatable, intent(out) :: global_count(:)
      integer, optional, intent(out) :: rc

      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(grid)
      _UNUSED_DUMMY(local_start)
      _UNUSED_DUMMY(global_start)
      _UNUSED_DUMMY(global_count)
      _UNUSED_DUMMY(rc)

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

      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(grid)
      _UNUSED_DUMMY(local_start)
      _UNUSED_DUMMY(global_start)
      _UNUSED_DUMMY(global_count)
      _UNUSED_DUMMY(rc)

   end subroutine generate_file_corner_bounds

   function generate_file_reference2D(this,fpointer) result(ref)
      use pFIO
      type(ArrayReference) :: ref
      class(MockGridFactory), intent(inout) :: this
      real, pointer, intent(in) :: fpointer(:,:)
      _UNUSED_DUMMY(this)
      ref = ArrayReference(fpointer)
   end function generate_file_reference2D

   function generate_file_reference3D(this,fpointer) result(ref)
      use pFIO
      type(ArrayReference) :: ref
      class(MockGridFactory), intent(inout) :: this
      real, pointer, intent(in) :: fpointer(:,:,:)
      _UNUSED_DUMMY(this)
      ref = ArrayReference(fpointer)
   end function generate_file_reference3D

end module MockGridFactoryMod
