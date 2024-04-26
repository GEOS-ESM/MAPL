#include "MAPL_Generic.h"

module mapl3g_BundleWriter
   use mapl_ErrorHandlingMod
   use esmf
   use pfio
   use mapl3g_geom_mgr
   use gftl_StringVector
   implicit none
   private

   public BundleWriter

   type BundleWriter
      integer :: collection_id
      contains
         procedure initialize
         !procedure send_field_data
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

      geom_mgr => get_geom_manager() 
      id = MAPL_GeomGetId(geom,_RC)
      mapl_geom => geom_mgr%get_mapl_geom_from_id(id,_RC)
      ! now we only have the geom associated metadata 
      metadata = mapl_geom%get_file_metadata()
      ! we need vertical spec/geom metadata, in theory property of outermeta that could be queried

      ! we need ungridded dim spec metadata but that function of individual fields

      ! time metdata?

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
            logical :: first
         
            first = .true. 
            iter = string_vec%begin()
            do while (iter /= string_Vec%end())
               var => iter%get()
               if (first) then
                  comma_sep = var
                  first = .false.
               else
                  comma_sep = comma_sep//","//var
               endif
               call iter%next()
            enddo
         end function 
              

   end subroutine initialize

end module

  
