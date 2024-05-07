#include "MAPL_Generic.h"

module mapl3g_GeomPFIO
   use mapl_ErrorHandling
   use ESMF
   use PFIO
   use mapl3g_geom_mgr
   use mapl3g_SharedIO
   implicit none
   private

   public :: GeomPFIO
   type, abstract :: GeomPFIO
      private
      integer :: collection_id
      type(MaplGeom), pointer :: mapl_geom
   contains
      procedure(I_stage_data_to_file), deferred :: stage_data_to_file
      procedure :: initialize
      procedure :: update_time_on_server
      procedure :: stage_time_to_file
      procedure, non_overridable :: get_collection_id

   end type GeomPFIO

   abstract interface

     subroutine I_stage_data_to_file(this, bundle, filename, time_index, rc)
        use esmf
        import GeomPFIO
        class(GeomPFIO), intent(inout) :: this
        type(ESMF_FieldBundle), intent(in) :: bundle
        character(len=*), intent(in) :: filename
        integer, intent(in), optional :: time_index
        integer, intent(out), optional :: rc
     end subroutine I_stage_data_to_file

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
      real, intent(in) :: times
      integer, optional, intent(out) :: rc

      integer :: status
      type(ArrayReference) :: ref

      ref = ArrayReference(times)
      call o_Clients%stage_nondistributed_data(this%collection_id, filename, 'time', ref)

   end subroutine

   subroutine initialize(this, metadata, mapl_geom,  rc)
      class(GeomPFIO), intent(inout) :: this
      type(FileMetadata), intent(in) :: metadata
      type(MaplGeom), intent(in), pointer :: mapl_geom
      integer, optional, intent(out) :: rc

      integer :: status

      this%mapl_geom => mapl_geom
      this%collection_id = o_Clients%add_hist_collection(metadata)
      _RETURN(_SUCCESS)
   end subroutine initialize

   pure integer function get_collection_id(this)
      class(GeomPFIO), intent(in) :: this
 
      get_collection_id = this%collection_id
   end function get_collection_id

end module mapl3g_GeomPFIO
