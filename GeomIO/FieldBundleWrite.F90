#include "MAPL.h"

module mapl3g_FieldBundleWrite

   use ESMF
   use mapl_ErrorHandling
   use mapl3g_GeomPFIO
   use mapl3g_GeomCatagorizer
   use mapl3g_SharedIO, only: bundle_to_metadata
   use pFIO, only: FileMetaData, o_Clients

   implicit none(type, external)
   private

   public :: FieldBundleWrite

contains

   !---------------------------------------------------------------------------
   ! FieldBundleWrite
   !
   ! Write all fields in a bundle to a NetCDF file using the GeomIO layer.
   ! The geometry is inferred from the first field in the bundle.
   !
   ! Arguments:
   !   bundle   - ESMF_FieldBundle to write
   !   filename - Output NetCDF filename
   !   time     - Current time (written as the single time step in the file)
   !   rc       - Return code (optional)
   !---------------------------------------------------------------------------
   subroutine FieldBundleWrite(bundle, filename, time, rc)
      type(ESMF_FieldBundle), intent(in)    :: bundle
      character(*),           intent(in)    :: filename
      type(ESMF_Time),        intent(in)    :: time
      integer, optional,      intent(out)   :: rc

      integer :: status
      integer :: field_count
      type(ESMF_Field), allocatable :: field_list(:)
      type(ESMF_Geom)  :: geom
      type(FileMetaData) :: metadata
      class(GeomPFIO), allocatable :: writer

      ! Need at least one field to determine the geometry
      call ESMF_FieldBundleGet(bundle, fieldCount=field_count, _RC)
      _ASSERT(field_count > 0, 'FieldBundleWrite: bundle must contain at least one field')

      allocate(field_list(field_count), _STAT)
      call ESMF_FieldBundleGet(bundle, fieldList=field_list, &
           itemorderflag=ESMF_ITEMORDER_ADDORDER, _RC)

      ! Get the geometry from the first field
      call ESMF_FieldGet(field_list(1), geom=geom, _RC)

      ! Build file metadata from bundle + geom
      metadata = bundle_to_metadata(bundle, geom, _RC)

      ! Select the appropriate GeomPFIO subtype (GridPFIO for ESMF_Grid, etc.)
      allocate(writer, source=make_geom_pfio(metadata), _STAT)

      ! Register this output collection with o_Clients
      call writer%initialize(metadata, geom, _RC)

      ! Stamp the time on the server
      call writer%update_time_on_server(time, _RC)

      ! Stage all fields (single time step, index 1)
      call writer%stage_data_to_file(bundle, filename, 1, _RC)

      ! Flush to disk
      call o_Clients%done_collective_stage()
      call o_Clients%post_wait()

      _RETURN(_SUCCESS)
   end subroutine FieldBundleWrite

end module mapl3g_FieldBundleWrite
