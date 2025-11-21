#include "MAPL.h"

module mapl3g_GeomPFIO
   use mapl_ErrorHandling
   use ESMF
   use pfio, only: i_Clients, o_Clients, StringVariableMap, ArrayReference
   use mapl3g_Geom_API
   use mapl3g_SharedIO
   implicit none
   private

   public :: GeomPFIO

   type, abstract :: GeomPFIO
      private
      integer :: collection_id
      type(ESMF_Geom) :: esmfgeom
      type(FileMetadata) :: file_metadata
   contains
      procedure(I_stage_data_to_file), deferred :: stage_data_to_file
      procedure(I_stage_coordinates_to_file), deferred :: stage_coordinates_to_file
      procedure(I_request_data_from_file), deferred :: request_data_from_file
      procedure, private :: init_with_metadata
      procedure, private :: init_with_filename
      generic :: initialize => init_with_metadata, init_with_filename
      procedure :: update_time_on_server
      procedure :: stage_time_to_file
      procedure, non_overridable :: get_collection_id
      procedure, non_overridable :: get_file_metadata
      procedure, non_overridable :: get_esmf_geom
   end type GeomPFIO

   abstract interface

     subroutine I_stage_data_to_file(this, bundle, filename, time_index, rc)
        use esmf
        import GeomPFIO
        class(GeomPFIO), intent(inout) :: this
        type(ESMF_FieldBundle), intent(in) :: bundle
        character(len=*), intent(in) :: filename
        integer, intent(in) :: time_index
        integer, intent(out), optional :: rc
     end subroutine I_stage_data_to_file

     subroutine I_stage_coordinates_to_file(this, filename, rc)
        use esmf
        import GeomPFIO
        class(GeomPFIO), intent(inout) :: this
        character(len=*), intent(in) :: filename
        integer, intent(out), optional :: rc
     end subroutine I_stage_coordinates_to_file

     subroutine I_request_data_from_file(this, filename, bundle, rc)
        use esmf
        import GeomPFIO
        class(GeomPFIO), intent(inout) :: this
        character(len=*), intent(in) :: filename
        type(ESMF_FieldBundle), intent(inout) :: bundle
        integer, intent(out), optional :: rc
     end subroutine I_request_data_from_file

   end interface

contains

   subroutine update_time_on_server(this, time, rc)
      class(GeomPFIO), intent(inout) :: this
      type(ESMF_Time), intent(in) :: time
      integer, intent(out), optional :: rc

      integer :: status
      type(StringVariableMap) :: var_map
      type(Variable) :: time_var

      time_var = create_time_variable(time, _RC)
      call var_map%insert('time',time_var)
      call o_Clients%modify_metadata(this%collection_id, var_map=var_map, _RC)

      _RETURN(_SUCCESS)

   end subroutine update_time_on_server

   subroutine stage_time_to_file(this,filename, times, rc)
      class(GeomPFIO), intent(inout) :: this
      character(len=*), intent(in) :: filename
      real, target, intent(in) :: times(:)
      integer, optional, intent(out) :: rc

      integer :: status
      type(ArrayReference) :: ref

      ref = ArrayReference(times)
      call o_Clients%stage_nondistributed_data(this%collection_id, filename, 'time', ref, _RC)

   end subroutine

   subroutine init_with_metadata(this, metadata, esmfgeom,  rc)
      class(GeomPFIO), intent(inout) :: this
      type(FileMetadata), intent(in) :: metadata
      type(ESMF_Geom), intent(in) :: esmfgeom
      integer, optional, intent(out) :: rc

      integer :: status

      this%esmfgeom = esmfgeom
      this%collection_id = o_Clients%add_data_collection(metadata, _RC)
      this%file_metadata = metadata

      _RETURN(_SUCCESS)
   end subroutine init_with_metadata

   subroutine init_with_filename(this, file_name, esmfgeom,  rc)
      class(GeomPFIO), intent(inout) :: this
      character(len=*), intent(in) :: file_name
      type(ESMF_Geom), intent(in) :: esmfgeom
      integer, optional, intent(out) :: rc

      integer :: status

      this%esmfgeom = esmfgeom
      this%collection_id = i_Clients%add_data_collection(file_name, _RC)

      _RETURN(_SUCCESS)
   end subroutine init_with_filename

   pure integer function get_collection_id(this)
      class(GeomPFIO), intent(in) :: this
 
      get_collection_id = this%collection_id
   end function get_collection_id

   function get_file_metadata(this) result(file_metadata)
      type(FileMetadata) :: file_metadata
      class(GeomPFIO), intent(in) :: this
 
      file_metadata = this%file_metadata
   end function get_file_metadata

   function get_esmf_geom(this) result(esmfgeom)
      type(ESMF_Geom) :: esmfgeom
      class(GeomPFIO), intent(in) :: this

      esmfgeom=this%esmfgeom
   end function get_esmf_geom

end module mapl3g_GeomPFIO
