#define _SUCCESS      0
#define _FAILURE     1
#define _VERIFY(A)   if(  A/=0) then; if(present(rc)) rc=A; PRINT *, Iam, __LINE__; return; endif
#define _ASSERT(A)   if(.not.(A)) then; if(present(rc)) rc=_FAILURE; PRINT *, Iam, __LINE__; return; endif
#define _RETURN(A)   if(present(rc)) rc=A; return
#include "unused_dummy.H"

module MAPL_ExternalGridFactory
   use MAPL_AbstractGridFactoryMod
   use MAPL_MinMaxMod
   use MAPL_KeywordEnforcerMod
   use ESMF
   use pFIO
   use MAPL_CommsMod
   use MAPL_ConstantsMod
   use MAPL_IOMod, only : GETFILE, FREE_FILE 
   use, intrinsic :: iso_fortran_env, only: REAL64,REAL32

   implicit none
   private

   public :: ExternalGridFactory

   type, extends(AbstractGridFactory) :: ExternalGridFactory
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
contains
   function make_new_grid(this, unusable, rc) result(grid)
      type(ESMF_Grid) :: grid
      class(ExternalGridFactory),       intent(in   ) :: this
      class(KeywordEnforcer), optional, intent(in   ) :: unusable
      integer,                optional, intent(  out) :: rc

      _UNUSED_DUMMY(unusable)

      ! TODO: fill in the rest

      _RETURN(_SUCCESS)
   end function make_new_grid

   logical function equals(a, b)
      class(ExternalGridFactory), intent(in) :: a
      class(AbstractGridFactory), intent(in) :: b

      select type(b)
      class default
         equals = .false.
         return
      class is (ExternalGridFactory)
         equals = .true.
         ! TODO: fill in the rest
      end select
   end function equals

   subroutine initialize_from_file_metadata(this, file_metadata, unusable, rc)
      class(ExternalGridFactory),       intent(inout) :: this
      type(FileMetadata),     target,   intent(in   ) :: file_metadata
      class(KeywordEnforcer), optional, intent(in   ) :: unusable
      integer,                optional, intent(  out) :: rc

      _UNUSED_DUMMY(unusable)

      ! TODO: fill in the rest

      _RETURN(_SUCCESS)
   end subroutine initialize_from_file_metadata

   subroutine initialize_from_config_with_prefix(this, config, prefix, unusable, rc)
      class(ExternalGridFactory),       intent(inout) :: this
      type(ESMF_Config),                intent(inout) :: config
      character(*),                     intent(in   ) :: prefix
      class(KeywordEnforcer), optional, intent(in   ) :: unusable
      integer,                optional, intent(  out) :: rc

      _UNUSED_DUMMY(unusable)

      ! TODO: fill in the rest

      _RETURN(_SUCCESS)
   end subroutine initialize_from_config_with_prefix

   subroutine initialize_from_esmf_distGrid(this, dist_grid, lon_array, lat_array, unusable, rc)
      class(ExternalGridFactory),       intent(inout) :: this
      type(ESMF_DistGrid),              intent(in   ) :: dist_grid
      type(ESMF_LocalArray),            intent(in   ) :: lon_array
      type(ESMF_LocalArray),            intent(in   ) :: lat_array
      class(KeywordEnforcer), optional, intent(in   ) :: unusable
      integer,                optional, intent(  out) :: rc

      _UNUSED_DUMMY(unusable)

      ! TODO: fill in the rest

      _RETURN(_SUCCESS)
   end subroutine initialize_from_esmf_distGrid

   subroutine halo(this, array, unusable, halo_width, rc)
      class(ExternalGridFactory),       intent(inout) :: this
      real(kind=REAL32),                intent(inout) :: array(:,:)
      class(KeywordEnforcer), optional, intent(in   ) :: unusable
      integer,                optional, intent(in   ):: halo_width
      integer,                optional, intent(  out) :: rc

      _UNUSED_DUMMY(unusable)

      ! TODO: fill in the rest

      _RETURN(_SUCCESS)
   end subroutine halo

   function generate_grid_name(this) result(name)
      character(:), allocatable :: name
      class(ExternalGridFactory), intent(in) :: this

      ! TODO: fill in the rest
   end function generate_grid_name

   subroutine append_metadata(this, metadata)
      class(ExternalGridFactory), intent(inout) :: this
      type(FileMetadata),         intent(inout) :: metadata

      ! TODO: fill in the rest
   end subroutine append_metadata

   function get_grid_vars(this) result(vars)
      character(:), allocatable :: vars
      class(ExternalGridFactory), intent(inout) :: this

      ! TODO: fill in the rest
   end function get_grid_vars

   subroutine append_variable_metadata(this, var)
      class(ExternalGridFactory), intent(inout) :: this
      type(Variable),             intent(inout) :: var

      ! TODO: fill in the rest
   end subroutine append_variable_metadata

   subroutine generate_file_bounds(this, grid, local_start, global_start, global_count, rc)
      class(ExternalGridFactory), intent(inout) :: this
      type(ESMF_Grid),            intent(inout) :: grid
      integer,      allocatable,  intent(  out) :: local_start(:)
      integer,      allocatable,  intent(  out) :: global_start(:)
      integer,      allocatable,  intent(  out) :: global_count(:)
      integer,      optional,     intent(  out) :: rc

      ! TODO: fill in the rest

      _RETURN(_SUCCESS)
   end subroutine generate_file_bounds

   subroutine generate_file_corner_bounds(this, grid, local_start, global_start, global_count, rc)
      class(ExternalGridFactory), intent(inout) :: this
      type(ESMF_Grid),            intent(inout) :: grid
      integer,      allocatable,  intent(  out) :: local_start(:)
      integer,      allocatable,  intent(  out) :: global_start(:)
      integer,      allocatable,  intent(  out) :: global_count(:)
      integer,      optional,     intent(  out) :: rc

      ! TODO: fill in the rest

      _RETURN(_SUCCESS)
   end subroutine generate_file_corner_bounds

   function generate_file_reference2D(this, fpointer) result(ref)
      type(ArrayReference) :: ref
      class(ExternalGridFactory), intent(inout) :: this
      real, pointer,              intent(in   ) :: fpointer(:,:)

      ! TODO: fill in the rest
   end function generate_file_reference2D

   function generate_file_reference3D(this, fpointer) result(ref)
      type(ArrayReference) :: ref
      class(ExternalGridFactory), intent(inout) :: this
      real, pointer,              intent(in   ) :: fpointer(:,:,:)

      ! TODO: fill in the rest
   end function generate_file_reference3D
end module MAPL_ExternalGridFactory
