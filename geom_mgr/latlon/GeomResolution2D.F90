#include "MAPL_ErrLog.h"

module mapl3g_GeomResolution2D
   use mapl3_HConfigUtils
   use pfio_FileMetadata
   implicit none
   private

   public :: GeomResolution2D

   type :: GeomResolution2D
      integer :: im_world = MAPL_UNDEFINED_INTEGER
      integer :: jm_world = MAPL_UNDEFINED_INTEGER
   end type GeomResolution2D

   interface GeomResolution2D
      procedure new_GeomResolution2D
   end interface GeomResolution2D

   interface make_GeomResolution2D
      procedure make_GeomResolution2D_from_hconfig
      procedure make_GeomResolution2D_from_metadata
   end interface make_GeomResolution2D

contains

   function new_GeomResolution2D(im_world, jm_world) result(resolution)
      type(GeomResolution2D) :: resolution
      integer, intent(in) :: im_world, jm_world

      resolution%im_world = im_world
      resolution%jm_world = jm_world
   end function new_GeomResolution2D

   function make_GeomResolution2D_from_hconfig(hconfig, rc) result(resolution)
      type(GeomResolution2D) :: resolution
      type(MAPL_Config) :: hconfig
      itneger, optional ,intent(out) :: rc

      integer :: im_world, jm_world
      integer :: status

      call MAPL_GetResource(im_world, hconfig, 'im_world', _RC)
      call MAPL_GetResource(jm_world, hconfig, 'jm_world', _RC)

      resolution = GeomResolution2D(im_world, jm_world)

      _RETURN(_SUCCESS)
   end function make_GeomResolution2D_from_hconfig
   
   function make_GeomResolution2D_from_metadata(file_metadata, lon_name, lat_name, rc) result(resolution)
      type(GeomResolution2D) :: resolution
      type(FileMetadata), intent(in) :: file_metadata
      character(*), intent(in) :: lon_name
      character(*), intent(in) :: lat_name
      integer, optional, intent(out) :: rc

      integer :: im_world, jm_world

      im_world = file_metadata%get_dimension(lon_name, _RC)
      jm_world = file_metadata%get_dimension(lat_name, _RC)

      resolution = GeomResolution2D(im_world, jm_world)

      _RETURN(_SUCCESS)
   end function make_GeomResolution2D_from_hconfig
   

end module mapl3g_GeomResolution2D
