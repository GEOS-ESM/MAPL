#include "MAPL_Generic.h"

module mapl3g_BundleWriter
   use mapl_ErrorHandlingMod
   use esmf
   use pfio
   use mapl3g_geom_mgr
   use gFTL2_StringVector
   use MAPL_BaseMod
   implicit none
   private

   public BundleWriter

   type BundleWriter
      integer :: collection_id
      contains
         procedure initialize
         procedure update_time_on_server
         procedure send_field_data
   end type

   contains

   ! have to pass in geom, because comes from outer metacomp
   ! bundle, state, gridcomp can not query it
   ! otherwise would have to pick a random field in bundle or state
   subroutine initialize(this, bundle, geom, rc)
      class(BundleWriter), intent(inout) :: this
      type(ESMF_FieldBundle), intent(in) :: bundle
      type(ESMF_Geom), intent(in) :: geom
      integer, optional, intent(out) :: rc

      integer:: status, id
      type(FileMetadata) :: metadata, variables
      type(GeomManager), pointer :: geom_mgr
      type(StringVector) :: grid_variables
      type(MaplGeom), pointer :: mapl_geom
      type(Variable) :: time_var
      type(ESMF_Time) :: fake_time

      geom_mgr => get_geom_manager() 
      id = MAPL_GeomGetId(geom,_RC)
      mapl_geom => geom_mgr%get_mapl_geom_from_id(id,_RC)
      metadata = mapl_geom%get_file_metadata()
      ! Add metadata for vertical geom, note could be both center and edge

      ! Add metadata for all unique ungridded dimensions the set of fields has

      ! Add time metadata
      call ESMF_TimeSet(fake_time, timeString="1900-04-03T21:00:00", _RC)
      call metadata%add_dimension('time', pFIO_UNLIMITED)
      time_var = create_time_variable(fake_time, _RC)
      call metadata%add_variable('time', time_var, _RC)

      ! Variables
      grid_variables = mapl_geom%get_gridded_dims()
      call add_variables(metadata, bundle, grid_variables, _RC)
      print*,metadata
      this%collection_id = o_Clients%add_hist_collection(metadata)

      contains
 
         subroutine add_variables(metadata, bundle, grid_variables, rc) 
            type(ESMF_FieldBundle), intent(in) :: bundle
            type(StringVector), intent(in) :: grid_variables
            type(FileMetaData), intent(inout) :: metadata
            integer, intent(out), optional :: rc
             
            integer :: status, num_fields, i
            character(len=ESMF_MAXSTR), allocatable :: field_names(:)            
            type(ESMF_Field) :: field
            
            call ESMF_FieldBundleGet(bundle, fieldCount=num_fields, _RC)
            allocate(field_names(num_fields))
            call ESMF_FieldBundleGet(bundle, fieldNameList=field_names, _RC)
            do i=1,num_fields
               call ESMF_FieldBundleGet(bundle, field_names(i), field=field, _RC)
               call add_variable(metadata, field, grid_variables, _RC)
            enddo
            _RETURN(_SUCCESS) 
  
         end subroutine 

         subroutine add_variable(metadata, field, grid_variables, rc) 
            type(ESMF_Field), intent(in) :: field
            type(StringVector), intent(in) :: grid_variables
            type(FileMetaData), intent(inout) :: metadata
            integer, intent(out), optional :: rc
             
            type(Variable) :: v 
            integer :: status
            character(len=:), allocatable :: dims
            type(ESMF_TYPEKIND_FLAG) :: typekind
            integer :: pfio_type
            type(ESMF_Info) :: info
            character(len=:), allocatable :: char
            character(len=ESMF_MAXSTR) :: fname
            
            dims = string_vec_to_comma_sep(grid_variables)
            call ESMF_FieldGet(field, name=fname, typekind = typekind, _RC)
            ! add vertical dimension

            ! add any ungridded dimensions

            ! add time dimension
            dims = dims//",time"
            pfio_type = esmf_to_pfio_type(typekind ,_RC)
            v = Variable(type=pfio_type, dimensions=dims)
            call ESMF_InfoGetFromHost(field, info, _RC)
            call ESMF_InfoGetCharAlloc(info, 'MAPL/units', char, _RC)
            call v%add_attribute('units',char) 
            call ESMF_InfoGetCharAlloc(info, 'MAPL/standard_name', char, _RC)
            call v%add_attribute('long_name',char) 
            call metadata%add_variable(trim(fname), v, _RC)
            
            _RETURN(_SUCCESS) 
  
         end subroutine 


         function esmf_to_pfio_type(esmf_type, rc) result(pfio_type)
            integer :: pfio_type
            type(ESMF_TYPEKIND_FLAG), intent(in) :: esmf_type
            integer, intent(out), optional :: rc
            if (esmf_type == ESMF_TYPEKIND_R4) then
               pfio_type = pFIO_REAL32
            else if (esmf_type == ESMF_TYPEKIND_R8) then
               pfio_type = pFIO_REAL64
            else
               _FAIL("Unsupported ESMF field typekind for output")
            end if
            _RETURN(_SUCCESS)
         end function

         function string_vec_to_comma_sep(string_vec) result(comma_sep)
            character(len=:), allocatable :: comma_sep
            type(StringVector), intent(in) :: string_vec
            type(stringVectorIterator) :: iter
            character(len=:), pointer :: var

            iter = string_vec%begin()
            var => iter%of()
            comma_sep = var
            call iter%next()
            do while (iter /= string_vec%end())
               var => iter%of()
               comma_sep = comma_sep//","//var
               call iter%next()
            enddo
         end function 
              

   end subroutine initialize

   subroutine update_time_on_server(this, current_time, rc)
      class(BundleWriter), intent(inout) :: this
      type(ESMF_Time), intent(in) :: current_time
      integer, intent(out), optional :: rc

      integer :: status
      type(Variable) :: time_var
      type(StringVariableMap) :: var_map

      time_var = create_time_variable(current_time, _RC)
      call var_map%insert('time',time_var)
      call o_Clients%modify_metadata(this%collection_id, var_map=var_map, _RC)

      _RETURN(_SUCCESS)

   end subroutine update_time_on_server

   subroutine send_field_data(this, bundle, filename, time_index, rc)
      class(BundleWriter), intent(inout) :: this
      type(ESMF_FieldBundle), intent(in) :: bundle
      character(len=*), intent(in) :: filename
      integer, intent(in) :: time_index
      integer, intent(out), optional :: rc

      integer :: status, num_fields, i
      character(len=ESMF_MAXSTR), allocatable :: field_names(:)            
      type(ESMF_Field) :: field
      type(ArrayReference) :: ref
      real, pointer :: ptr2d(:,:)
      integer, allocatable :: local_start(:), global_start(:), global_count(:)

      type(ESMF_Grid) :: grid ! NEEDS TO BE GEOM
      integer :: global_dim(3), i1, j1, in, jn
      
      call ESMF_FieldBundleGet(bundle, fieldCount=num_fields, _RC)
      allocate(field_names(num_fields))
      call ESMF_FieldBundleGet(bundle, fieldNameList=field_names, _RC)
      do i=1,num_fields
         call ESMF_FieldBundleGet(bundle, field_names(i), field=field, _RC)
         ! all this logic needs to be generalized
         call ESMF_FieldGet(field, farrayPtr=ptr2d, _RC)
         allocate(global_start, source=[1,1])
         call ESMF_FieldGet(field, grid=grid, _RC)
         call MAPL_GridGet(grid, globalCellCountPerDim=global_dim, _RC)
         allocate(global_count, source=[global_dim(1),global_dim(2)])
         call MAPL_GridGetInterior(grid, i1, in, j1, jn)
         allocate(local_start, source=[i1, j1])
         ref = ArrayReference(ptr2d)
         ! end generalization
         call o_clients%collective_stage_data(this%collection_id,filename, trim(field_names(i)), &
              ref, start=local_start, global_start=global_start, global_count=global_count)
      enddo      

      _RETURN(_SUCCESS)

   end subroutine send_field_data 

   function create_time_variable(current_time, rc) result(time_var)
      type(Variable) :: time_var
      type(ESMF_Time), intent(in) :: current_time
      integer, optional, intent(out) :: rc

      integer :: status
      character(len=ESMF_MAXSTR) :: iso_time_string

      call ESMF_TimeGet(current_time, timeString=iso_time_string, _RC)
      iso_time_string = "minutes since "//trim(iso_time_string)
      time_var = Variable(type=PFIO_REAL32, dimensions='time')
      call time_var%add_attribute('long_name', 'time')
      call time_var%add_attribute('units', iso_time_string, _RC)
 
      _RETURN(_SUCCESS)
   end function create_time_variable 

end module

  
