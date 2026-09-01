#include "MAPL.h"

module mapl_GeomPFIO_mod
    use mapl_ErrorHandling_mod
    use ESMF
    use pfio, only: get_client, ClientThread, StringVariableMap, ArrayReference, FileMetadata, Variable
    use mapl_geom_api
    use mapl_SharedIO_mod
    use mapl_DefaultServerNames_mod, only: MAPL_DEFAULT_INPUT_SERVER, MAPL_DEFAULT_OUTPUT_SERVER
    implicit none
   private

   public :: GeomPFIO

   type, abstract :: GeomPFIO
      private
      integer :: collection_id
      type(ESMF_Geom) :: esmfgeom
      type(FileMetadata) :: file_metadata
      character(:), allocatable :: output_server_name
      character(:), allocatable :: input_server_name
   contains
      procedure(I_stage_data_to_file), deferred :: stage_data_to_file
      procedure(I_stage_coordinates_to_file), deferred :: stage_coordinates_to_file
      procedure(I_request_data_from_file), deferred :: request_data_from_file
      procedure :: init_with_metadata
      procedure :: init_with_filename
      generic :: initialize => init_with_metadata, init_with_filename
      procedure :: update_time_on_server
      procedure :: stage_time_to_file
      procedure, non_overridable :: get_collection_id
      procedure, non_overridable :: get_file_metadata
      procedure, non_overridable :: get_esmf_geom
      procedure, non_overridable :: get_output_server_name
      procedure, non_overridable :: get_input_server_name
   end type GeomPFIO

   abstract interface

     subroutine I_stage_data_to_file(this, bundle, filename, time_index, rc)
        import GeomPFIO
        import ESMF_FieldBundle
        class(GeomPFIO), intent(inout) :: this
        type(ESMF_FieldBundle), intent(in) :: bundle
        character(len=*), intent(in) :: filename
        integer, intent(in) :: time_index
        integer, intent(out), optional :: rc
     end subroutine I_stage_data_to_file

     subroutine I_stage_coordinates_to_file(this, filename, rc)
        import GeomPFIO
        class(GeomPFIO), intent(inout) :: this
        character(len=*), intent(in) :: filename
        integer, intent(out), optional :: rc
     end subroutine I_stage_coordinates_to_file

     subroutine I_request_data_from_file(this, filename, bundle, rc)
        import GeomPFIO
        import ESMF_FieldBundle
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
       class(ClientThread), pointer :: client

       time_var = create_time_variable(time, _RC)
       call var_map%insert('time',time_var)
       client => get_client(this%output_server_name, _RC)
       call client%modify_metadata(this%collection_id, var_map=var_map, _RC)

      _RETURN(_SUCCESS)

   end subroutine update_time_on_server

    subroutine stage_time_to_file(this,filename, times, rc)
       class(GeomPFIO), intent(inout) :: this
       character(len=*), intent(in) :: filename
       real, target, intent(in) :: times(:)
       integer, optional, intent(out) :: rc

      integer :: status
       type(ArrayReference) :: ref
       integer :: request_id
       class(ClientThread), pointer :: client

        ref = ArrayReference(times)
        client => get_client(this%output_server_name, _RC)
        request_id = client%stage_nondistributed_data(this%collection_id, filename, 'time', ref, _RC)
       _RETURN(_SUCCESS)

   end subroutine

   subroutine init_with_metadata(this, metadata, esmfgeom, output_server_name, rc)
       class(GeomPFIO), intent(inout) :: this
       type(FileMetadata), intent(in) :: metadata
       type(ESMF_Geom), intent(in) :: esmfgeom
       character(len=*), intent(in), optional :: output_server_name
       integer, optional, intent(out) :: rc

       integer :: status
       class(ClientThread), pointer :: client
       character(len=:), allocatable :: server_name

       server_name = MAPL_DEFAULT_OUTPUT_SERVER
       if (present(output_server_name)) server_name = output_server_name
       this%esmfgeom = esmfgeom
       this%output_server_name = server_name
       client => get_client(this%output_server_name, _RC)
       this%collection_id = client%add_data_collection(metadata, _RC)
       this%file_metadata = metadata

      _RETURN(_SUCCESS)
   end subroutine init_with_metadata

   subroutine init_with_filename(this, file_name, esmfgeom, input_server_name, rc)
       class(GeomPFIO), intent(inout) :: this
       character(len=*), intent(in) :: file_name
       type(ESMF_Geom), intent(in) :: esmfgeom
       character(len=*), intent(in), optional :: input_server_name
       integer, optional, intent(out) :: rc

       integer :: status
       class(ClientThread), pointer :: client
       character(len=:), allocatable :: server_name

       server_name = MAPL_DEFAULT_INPUT_SERVER
       if (present(input_server_name)) server_name = input_server_name
       this%esmfgeom = esmfgeom
       this%input_server_name = server_name
       client => get_client(this%input_server_name, _RC)
       this%collection_id = client%add_data_collection(file_name, _RC)

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

   function get_output_server_name(this) result(server_name)
      class(GeomPFIO), intent(in) :: this
      character(:), allocatable :: server_name

      server_name = this%output_server_name

   end function get_output_server_name

   function get_input_server_name(this) result(server_name)
      class(GeomPFIO), intent(in) :: this
      character(:), allocatable :: server_name

      server_name = this%input_server_name

   end function get_input_server_name

end module mapl_GeomPFIO_mod
