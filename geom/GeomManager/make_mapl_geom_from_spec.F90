#include "MAPL.h"

submodule (mapl3g_GeomManager) make_mapl_geom_from_spec_smod

   implicit none

contains
   
   module function make_mapl_geom_from_spec(this, spec, rc) result(mapl_geom)
      use gftl2_StringVector
      use mapl3g_StringDictionary
      type(MaplGeom) :: mapl_geom
      class(GeomManager), target, intent(inout) :: this
      class(GeomSpec), intent(in) :: spec
      integer, optional, intent(out) :: rc

      class(GeomFactory), pointer :: factory
      integer :: status
      integer :: i
      type(ESMF_Geom) :: geom
      type(FileMetadata) :: file_metadata
      type(StringVector) :: gridded_dims
      type(StringDictionary) :: variable_attributes
      logical :: found

      found = .false.
      do i = 1, this%factories%size()
         factory => this%factories%of(i)
         if (factory%supports(spec)) then
            found = .true.
            exit
         end if
      end do
      _ASSERT(found, 'No factory supports spec.')

      geom = factory%make_geom(spec, _RC)
      file_metadata = factory%make_file_metadata(spec, _RC)
      gridded_dims = factory%make_gridded_dims(spec, _RC)
      variable_attributes = factory%make_variable_attributes(spec, _RC)
      mapl_geom = MaplGeom(spec=spec, geom=geom, factory=factory, file_metadata=file_metadata, gridded_dims=gridded_dims, variable_attributes=variable_attributes)

      _RETURN(_SUCCESS)
   end function make_mapl_geom_from_spec

end submodule make_mapl_geom_from_spec_smod
