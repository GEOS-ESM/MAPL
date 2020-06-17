#include "MAPL_Generic.h"
module MAPL_ESMFFieldBundleRead
   use ESMF
   use pFIO
   use MAPL_BaseMod
   use MAPL_newCFIOMod
   use MAPL_TimeDataMod
   use MAPL_newCFIOitemVectorMod
   use MAPL_newCFIOitemMod
   use MAPL_ioClientsMod, only: io_client, o_Clients
   use MAPL_ExceptionHandling
   use MAPL_AbstractGridFactoryMod
   use MAPL_AbstractRegridderMod
   use MAPL_GridManagerMod 
   use MAPL_FileMetadataUtilsMod
   use, intrinsic :: iso_fortran_env, only: REAL32
   implicit none
   private
  
   public MAPL_create_bundle_from_file
   public MAPL_read_bundle 
   contains

      subroutine MAPL_create_bundle_from_file(bundle,file_name,only_vars,rc)
         type(ESMF_FieldBundle), intent(inout) :: bundle
         character(len=*), intent(in) :: file_name
         character(len=*), optional, intent(in) :: only_vars
         integer, optional, intent(out) :: rc
         
         integer :: status
         type(FileMetadata) :: basic_metadata
         type(FileMetadataUtils) :: metadata
         type (NetCDF4_FileFormatter) :: formatter
         type(ESMF_Grid) :: grid
         integer :: num_fields,var_present,dims,location
         logical :: create_variable, has_vertical_level
         class (AbstractGridFactory), pointer :: factory
         character(len=:), allocatable :: grid_vars,exclude_vars
         type(Variable), pointer :: var
         type(StringVariableMap), pointer :: variables
         type(StringVariableMapIterator) :: var_iter
         character(len=:), pointer :: var_name
         type(ESMF_Field) :: field

         call formatter%open(file_name, pFIO_READ,rc=status)
         _VERIFY(status)
         basic_metadata = formatter%read(rc=status)
         _VERIFY(status)
         call metadata%create(basic_metadata,file_name)
         has_vertical_level = (metadata%get_level_name(rc=status)/='')
         _VERIFY(status)
         call ESMF_FieldBundleGet(bundle,grid=grid,FieldCount=num_fields,rc=status)
         _VERIFY(status)
         _ASSERT(num_fields == 0,"Trying to fill non-empty bundle")
         factory => get_factory(grid,rc=status)
         _VERIFY(status) 
         grid_vars = factory%generate_grid_specific_vars()
         exclude_vars = grid_vars//",lev,time,lons,lats"

         variables => metadata%get_variables()
         var_iter = variables%begin()
         do while (var_iter /= variables%end())
            var_name => var_iter%key()
            if (index(exclude_vars,trim(var_name)) > 0) then
               call var_iter%next()
               cycle
            end if
            create_variable = .true.
            if (present(only_vars)) then
               if (index(only_vars,trim(var_name)) < 1) create_variable = .false.
            end if
            if (create_variable) then
               if( has_vertical_level) then 
                   location=MAPL_VLocationCenter
                   dims = MAPL_DimsHorzVert
               else
                   location=MAPL_VLocationNone
                   dims = MAPL_DimsHorzOnly
               end if
               field=MAPL_FieldCreateEmpty(trim(var_name),grid,rc=status)
               _VERIFY(status)
               call ESMF_AttributeSet(field,name='DIMS',value=dims,rc=status)
               _VERIFY(status)
               call ESMF_AttributeSet(field,name='VLOCATION',value=location,rc=status)
               _VERIFY(status)
               call ESMF_AttributeSet(field,name='UNITS',value='NA',rc=status)
               _VERIFY(status)
               call ESMF_AttributeSet(field,name='LONG_NAME',value='NA',rc=status)
               _VERIFY(status)
               call MAPL_FieldAllocCommit(field,dims=dims,location=location, &
                       typekind=REAL32,hw=0,rc=status)
               _VERIFY(status)
               call MAPL_FieldBundleAdd(bundle,field,rc=status)
               _VERIFY(status)                  
            end if
            call var_iter%next()
         end do

         _RETURN(_SUCCESS)
 
      end subroutine MAPL_create_bundle_from_file

      subroutine MAPL_read_bundle(bundle,file_name,time,only_vars,rc)
         type(ESMF_FieldBundle), intent(inout) :: bundle
         character(len=*), intent(in) :: file_name
         type(ESMF_Time), intent(in) :: time
         character(len=*), optional, intent(in) :: only_vars
         integer, optional, intent(out) :: rc

         integer :: status
         integer :: num_fields, metadata_id, collection_id
         
         call ESMF_FieldBundleGet(bundle,fieldCount=num_fields,rc=status)
         _VERIFY(status)
 
         if (num_fields ==0) then
            call MAPL_create_bundle_from_file(bundle,file_name,only_vars=only_vars,rc=status)
            _VERIFY(status)
         end if
         

      end subroutine MAPL_read_bundle 

end module MAPL_ESMFFieldBundleRead
