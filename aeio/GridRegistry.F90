#include "MAPL_Generic.h"
#include "NUOPC_ErrLog.h"

module GridRegistryMod
   use ESMF
   use yaFyaml
   use MAPL_ExceptionHandling
   use MAPL_KeywordEnforcerMod
   use MAPL_LatLonGridFactoryMod

   implicit none
   private

   public GridRegistry

   type :: GridRegistry
      private
      character(len=:), allocatable :: config_file
   contains
      procedure :: import_grids
      procedure :: make_grid
   end type GridRegistry
contains

   subroutine import_grids(this, config_file, unusable, rc)
      class(GridRegistry),             intent(inout) :: this
      character(len=*),                intent(inout) :: config_file
      class(KeywordEnforcer), optional, intent(  out) :: unusable
      integer,                optional, intent(  out) :: rc

      _UNUSED_DUMMY(unusable)

      this%config_file=config_file

      _RETURN(_SUCCESS)
   end subroutine import_grids

   function make_grid(this, grid_name,pet_list_1d,rc) result(grid)
      class(GridRegistry),             intent(inout) :: this
      character(len=*), intent(in) :: grid_name
      integer, intent(in) :: pet_list_1d(:) 
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_Grid) :: grid
      integer :: im,jm,lm,nx,ny,pet_count
      character(len=:), allocatable :: dateline,pole,grid_type
      integer, allocatable :: pet_list(:,:)
      type(LatLonGridFactory) :: ll_factory

      type(Configuration) :: config,grids_def,grid_def
      
      type(parser) :: p
      type(filestream) :: file_stream
      p=parser('core')
      file_stream=FileStream(this%config_file)
      config = p%load(file_stream)
      
      call config%get(grids_def,"Grids",_RC)
       call grids_def%get(grid_def,trim(grid_name),_RC)
       call grid_def%get(grid_type,'grid_type',_RC)
       _ASSERT(grid_type == 'LatLon','only lat-lon supported')
       call grid_def%get(im,'im_world',_RC)
       call grid_def%get(jm,'jm_world',_RC)
       call grid_def%get(lm,'lm',_RC)
       call grid_def%get(dateline,'dateline',_RC)
       call grid_def%get(pole,'pole',_RC)

       pet_count=size(pet_list_1d)
       do nx = floor(sqrt(real(2*pet_count))), 1, -1
          if (mod(pet_count, nx) == 0) then ! found a decomposition
             ny = pet_count / nx
             exit
          end if
       end do
       allocate(pet_list(nx,ny))
       pet_list=reshape(pet_list_1d,[nx,ny])
       ll_factory = LatLonGridFactory(im_world=im,jm_world=jm,lm=lm,nx=nx,ny=ny, &
              pole=pole,dateline=dateline,pet_list=pet_list,_RC)
       grid = ll_factory%make_grid(_RC)

      _RETURN(_SUCCESS)
   end function make_grid

end module GridRegistryMod
