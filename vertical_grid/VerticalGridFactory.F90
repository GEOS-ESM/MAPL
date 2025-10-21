#include "MAPL.h"
module mapl3g_VerticalGridFactory
   use pfio, only: FileMetadata
   use esmf, only: esmf_HConfig
   use mapl3g_VerticalGrid, only: VerticalGrid
   use mapl3g_VerticalGridSpec, only: VerticalGridSpec
   use mapl_ErrorHandling
   implicit none(type,external)
   private
   
   public :: VerticalGridFactory
   
   type, abstract :: VerticalGridFactory
      private
   contains
      procedure(I_supports_spec), deferred :: supports_spec
      procedure(I_supports_file_metadata), deferred :: supports_file_metadata
      procedure(I_supports_config), deferred :: supports_config
      generic :: supports => supports_spec, supports_file_metadata, supports_config
      procedure(I_get_name), deferred :: get_name
      procedure(I_create_spec_from_config), deferred :: create_spec_from_config
      procedure(I_create_spec_from_file_metadata), deferred :: create_spec_from_file_metadata
      generic :: create_spec => create_spec_from_config, create_spec_from_file_metadata
      procedure(I_create_grid_from_spec), deferred :: create_grid_from_spec
      ! Non-deferred concrete methods that use the deferred methods
      procedure :: create_grid_from_config
      procedure :: create_grid_from_file_metadata
      generic :: create_grid => create_grid_from_config, create_grid_from_file_metadata
   end type VerticalGridFactory
   
   abstract interface
      function I_supports_spec(this, spec, rc) result(is_supported)
         use pfio, only: FileMetadata
         import VerticalGridFactory
         import VerticalGridSpec
         implicit none
         logical :: is_supported
         class(VerticalGridFactory), intent(in) :: this
         class(VerticalGridSpec), intent(in) :: spec
         integer, optional, intent(out) :: rc
      end function I_supports_spec

      function I_supports_file_metadata(this, file_metadata, rc) result(is_supported)
         use pfio, only: FileMetadata
         import VerticalGridFactory
         implicit none
         logical :: is_supported
         class(VerticalGridFactory), intent(in) :: this
         type(FileMetadata), intent(in), target :: file_metadata
         integer, optional, intent(out) :: rc
      end function I_supports_file_metadata
      
      function I_supports_config(this, config, rc) result(is_supported)
         use esmf, only: esmf_HConfig
         import VerticalGridFactory
         implicit none
         logical :: is_supported
         class(VerticalGridFactory), intent(in) :: this
         type(esmf_HConfig), intent(in) :: config
         integer, optional, intent(out) :: rc
      end function I_supports_config
      
      function I_get_name(this) result(name)
         import VerticalGridFactory
         implicit none
         character(len=:), allocatable :: name
         class(VerticalGridFactory), intent(in) :: this
      end function I_get_name
      
      function I_create_spec_from_config(this, config, rc) result(spec)
         use esmf, only: esmf_HConfig
         use mapl3g_VerticalGridSpec, only: VerticalGridSpec
         import VerticalGridFactory
         implicit none
         class(VerticalGridSpec), allocatable :: spec
         class(VerticalGridFactory), intent(in) :: this
         type(esmf_HConfig), intent(in), target :: config
         integer, intent(out), optional :: rc
      end function I_create_spec_from_config
      
      function I_create_spec_from_file_metadata(this, file_metadata, rc) result(spec)
         use pfio, only: FileMetadata
         use mapl3g_VerticalGridSpec, only: VerticalGridSpec
         import VerticalGridFactory
         implicit none
         class(VerticalGridSpec), allocatable :: spec
         class(VerticalGridFactory), intent(in) :: this
         type(FileMetadata), intent(in), target :: file_metadata
         integer, intent(out), optional :: rc
      end function I_create_spec_from_file_metadata
      
      function I_create_grid_from_spec(this, spec, rc) result(grid)
         use mapl3g_VerticalGrid, only: VerticalGrid
         use mapl3g_VerticalGridSpec, only: VerticalGridSpec
         import VerticalGridFactory
         implicit none
         class(VerticalGrid), allocatable :: grid
         class(VerticalGridFactory), intent(in) :: this
         class(VerticalGridSpec), intent(in) :: spec
         integer, intent(out), optional :: rc
      end function I_create_grid_from_spec
   end interface

contains

   ! Common concrete methods that use the deferred methods
   
   function create_grid_from_config(this, config, rc) result(grid)
      class(VerticalGrid), allocatable :: grid
      class(VerticalGridFactory), intent(in) :: this
      type(esmf_HConfig), intent(in), target :: config
      integer, intent(out), optional :: rc
      
      class(VerticalGridSpec), allocatable :: spec
      integer :: status
      
      ! Create spec and then grid from spec
      spec = this%create_spec_from_config(config, _RC)
      allocate(grid, source=this%create_grid_from_spec(spec, rc=status))
      _VERIFY(status)
      
      _RETURN(_SUCCESS)
   end function create_grid_from_config
   
   function create_grid_from_file_metadata(this, file_metadata, rc) result(grid)
      class(VerticalGrid), allocatable :: grid
      class(VerticalGridFactory), intent(in) :: this
      type(FileMetadata), intent(in), target :: file_metadata
      integer, intent(out), optional :: rc
      
      class(VerticalGridSpec), allocatable :: spec
      integer :: status
      
      ! Create spec and then grid from spec
      spec = this%create_spec_from_file_metadata(file_metadata, _RC)
      grid = this%create_grid_from_spec(spec, _RC)
      
      _RETURN(_SUCCESS)
   end function create_grid_from_file_metadata

end module mapl3g_VerticalGridFactory
