#include "MAPL.h"

submodule (mapl_GeomManager_mod) make_geom_spec_from_metadata_smod
   use mapl_NullGeomSpec_mod, only: NULL_GEOM_SPEC
   implicit none(type,external)

contains
   
   module function make_geom_spec_from_metadata(this, file_metadata, rc) result(geom_spec)
      class(GeomSpec), allocatable :: geom_spec
      class(GeomManager), target, intent(inout) :: this
      type(FileMetadata), intent(in) :: file_metadata
      integer, optional, intent(out) :: rc

      class(GeomFactory), pointer :: factory
      integer :: status
      integer :: i
      logical :: found

      geom_spec = NULL_GEOM_SPEC
      found = .false.
      do i = 1, this%factories%size()
         factory => this%factories%of(i)
         if (factory%supports(file_metadata)) then
            found = .true.
            exit
         end if
      end do
      _ASSERT(found, 'No factory supports file metadata.')
      geom_spec = factory%make_spec(file_metadata, _RC)

      _RETURN(_SUCCESS)
   end function make_geom_spec_from_metadata

end submodule make_geom_spec_from_metadata_smod
