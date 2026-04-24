#include "MAPL.h"

submodule (mapl3g_XYGeomSpec) supports_metadata_smod
   use mapl_ErrorHandlingMod
   use pfio, only: FileMetadata, Attribute
   implicit none

contains

   ! A file metadata object represents an XY grid if it has a
   ! 'grid_type' global attribute equal to 'XY' (set by the MAPL2
   ! XYGridFactory via append_metadata).
   logical module function supports_metadata_(this, file_metadata, rc) result(supports)
      class(XYGeomSpec), intent(in) :: this
      type(FileMetadata), intent(in) :: file_metadata
      integer, optional, intent(out) :: rc

      integer :: status
      character(len=:), allocatable :: grid_type
      type(Attribute), pointer :: attr

      supports = file_metadata%has_attribute('grid_type')
      if (.not. supports) then
         _RETURN(_SUCCESS)
      end if

      attr => file_metadata%get_attribute('grid_type', _RC)
      grid_type = attr%get_string(_RC)
      supports = (grid_type == 'XY')

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(this)
   end function supports_metadata_

end submodule supports_metadata_smod
