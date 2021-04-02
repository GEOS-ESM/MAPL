#include "MAPL_Generic.h"
module MAPL_ESMFFieldBundleRead
   use ESMF
   use pFIO
   use MAPL_BaseMod
   use MAPL_newCFIOMod
   use MAPL_TimeDataMod
   use MAPL_newCFIOitemVectorMod
   use MAPL_newCFIOitemMod
   use MAPL_ExceptionHandling
   use MAPL_AbstractGridFactoryMod
   use MAPL_AbstractRegridderMod
   use MAPL_GridManagerMod 
   use MAPL_ExtDataCollectionMod
   use MAPL_CollectionVectorMod
   use MAPL_ExtDataCollectionManagerMod
   use MAPL_FileMetadataUtilsMod
   use pFIO_ClientManagerMod, only : i_Clients
   use MAPL_newCFIOItemMod
   use MAPL_newCFIOItemVectorMod
   use MAPL_SimpleAlarm
   use MAPL_StringTemplate
   use gFTL_StringVector
   use, intrinsic :: iso_fortran_env, only: REAL32
   implicit none
   private
  
   public MAPL_create_bundle_from_metdata_id
   public MAPL_read_bundle 
   contains

      subroutine MAPL_create_bundle_from_metdata_id(bundle,metadata_id,file_name,only_vars,rc)
         type(ESMF_FieldBundle), intent(inout) :: bundle
         integer, intent(in) :: metadata_id
         character(len=*), intent(in) :: file_name
         character(len=*), optional, intent(in) :: only_vars
         integer, optional, intent(out) :: rc
         
         integer :: status
         type(MAPLExtDataCollection), pointer :: collection => null()
         type(fileMetaDataUtils), pointer :: metadata
         type(ESMF_Grid) :: grid,file_grid
         integer :: num_fields,dims,location
         logical :: create_variable, has_vertical_level, var_has_levels
         class (AbstractGridFactory), pointer :: factory
         character(len=:), allocatable :: grid_vars,exclude_vars
         type(StringVariableMap), pointer :: variables
         type(Variable), pointer :: this_variable
         type(StringVariableMapIterator) :: var_iter
         character(len=:), pointer :: var_name,dim_name
         character(len=:), allocatable :: lev_name
         type(ESMF_Field) :: field
         type (StringVector), pointer :: dimensions
         type (StringVectorIterator) :: dim_iter
         integer :: lev_size, grid_size(3)

         collection => ExtDataCollections%at(metadata_id)
         metadata => collection%find(trim(file_name))
         file_grid=collection%src_grid
         lev_name = metadata%get_level_name(rc=status)
         _VERIFY(status)
         has_vertical_level = (metadata%get_level_name(rc=status)/='')
         call ESMF_FieldBundleGet(bundle,grid=grid,FieldCount=num_fields,rc=status)
         _VERIFY(status)
         call MAPL_GridGet(grid,localCellCountPerDim=grid_size,rc=status)
         _VERIFY(status)

         _ASSERT(num_fields == 0,"Trying to fill non-empty bundle")
         factory => get_factory(file_grid,rc=status)
         _VERIFY(status) 
         grid_vars = factory%get_file_format_vars()
         exclude_vars = grid_vars//",lev,time,lons,lats"
         if (has_vertical_level) lev_size = metadata%get_dimension(trim(lev_name))

         variables => metadata%get_variables()
         var_iter = variables%begin()
         do while (var_iter /= variables%end())
            var_has_levels = .false.
            var_name => var_iter%key()
            this_variable => var_iter%value()
            if (has_vertical_level) then
               dimensions => this_variable%get_dimensions()
               dim_iter = dimensions%begin()
               do while (dim_iter /= dimensions%end())
                  dim_name => dim_iter%get()
                  if (trim(dim_name) == lev_name) var_has_levels=.true.
                  call dim_iter%next()
               enddo
            end if

            if (index(','//trim(exclude_vars)//',',','//trim(var_name)//',') > 0) then
               call var_iter%next()
                  cycle
               end if
            create_variable = .true.
            if (present(only_vars)) then
               if (index(','//trim(only_vars)//',',','//trim(var_name)//',') < 1) create_variable = .false.
            end if            
            if (create_variable) then
               if(var_has_levels) then 
                   if (grid_size(3) == lev_size) then
                      location=MAPL_VLocationCenter
                      dims = MAPL_DimsHorzVert
                      field= ESMF_FieldCreate(grid,name=trim(var_name),typekind=ESMF_TYPEKIND_R4, &
                        ungriddedUbound=[grid_size(3)],ungriddedLBound=[1], rc=status)
                   else if (grid_size(3)+1 == lev_size) then
                      location=MAPL_VLocationEdge
                      dims = MAPL_DimsHorzVert
                      field= ESMF_FieldCreate(grid,name=trim(var_name),typekind=ESMF_TYPEKIND_R4, &
                        ungriddedUbound=[grid_size(3)],ungriddedLBound=[0], rc=status)
                  end if
               else
                   location=MAPL_VLocationNone
                   dims = MAPL_DimsHorzOnly
                   field= ESMF_FieldCreate(grid,name=trim(var_name),typekind=ESMF_TYPEKIND_R4, &
                      rc=status)
               end if
               call ESMF_AttributeSet(field,name='DIMS',value=dims,rc=status)
               _VERIFY(status)
               call ESMF_AttributeSet(field,name='VLOCATION',value=location,rc=status)
               _VERIFY(status)
               call ESMF_AttributeSet(field,name='UNITS',value='NA',rc=status)
               _VERIFY(status)
               call ESMF_AttributeSet(field,name='LONG_NAME',value='NA',rc=status)
               _VERIFY(status)
               call MAPL_FieldBundleAdd(bundle,field,rc=status)
               _VERIFY(status)                  
            end if
            call var_iter%next()
         end do

         _RETURN(_SUCCESS)
 
      end subroutine MAPL_create_bundle_from_metdata_id

      subroutine MAPL_read_bundle(bundle,file_tmpl,time,only_vars,regrid_method,noread,rc)
         type(ESMF_FieldBundle), intent(inout) :: bundle
         character(len=*), intent(in) :: file_tmpl
         type(ESMF_Time), intent(in) :: time
         character(len=*), optional, intent(in) :: only_vars
         integer, optional, intent(in) :: regrid_method
         logical, optional, intent(in) :: noread
         integer, optional, intent(out) :: rc

         integer :: status
         integer :: num_fields, metadata_id, collection_id, time_index, i
         type(MAPL_newCFIO) :: cfio
         character(len=ESMF_MAXPATHLEN) :: file_name
         type(MAPLExtDataCollection), pointer :: collection => null()
         type(fileMetaDataUtils), pointer :: metadata
         type(ESMF_Time), allocatable :: time_series(:)
         type(newCFIOItemVector)            :: items
         character(len=ESMF_MAXSTR), allocatable :: field_names(:)
         type(newCFIOitem) :: item
        
         call fill_grads_template(file_name,file_tmpl,time=time,rc=status)
         _VERIFY(status)

         collection_id=i_clients%add_ext_collection(trim(file_tmpl))

         metadata_id = MAPL_ExtDataAddCollection(trim(file_tmpl))
         collection => ExtDataCollections%at(metadata_id)
         metadata => collection%find(trim(file_name))
         call metadata%get_time_info(timeVector=time_series,rc=status)
         _VERIFY(status)
         time_index=-1
         do i=1,size(time_series)
            if (time==time_series(i)) then
               time_index=i
               exit
            end if
         end do
         _ASSERT(time_index/=-1,"Time not found on file"//trim(file_name))
         deallocate(time_series)

         call ESMF_FieldBundleGet(bundle,fieldCount=num_fields,rc=status)
         _VERIFY(status)
         if (num_fields ==0) then
            call MAPL_create_bundle_from_metdata_id(bundle,metadata_id,file_name,only_vars=only_vars,rc=status)
            _VERIFY(status)
         end if
         if (present(noread)) then
            if (noread) then
               _RETURN(_SUCCESS)
            end if
         end if
         
         call ESMF_FieldBundleGet(bundle,fieldCount=num_fields,rc=status)
         _VERIFY(status)
         allocate(field_names(num_fields))
         call ESMF_FieldBundleGet(bundle,fieldnamelist=field_names,rc=status)
         _VERIFY(status)
         do i=1,num_fields
            item%itemTYpe=ItemTYpeScalar
            item%xname=trim(field_names(i))
            call items%push_back(item)
         enddo
        

         cfio=MAPL_NewCFIO(output_bundle=bundle,metadata_collection_id=metadata_id,read_collection_id=collection_id,items=items)
         call cfio%set_param(regrid_method=regrid_method)
         call cfio%request_data_from_file(trim(file_name),timeindex=time_index,rc=status)
         _VERIFY(status)
         call i_clients%done_collective_prefetch()
         call i_clients%wait()
         call cfio%process_data_from_file(rc=status)
         _VERIFY(status)

      end subroutine MAPL_read_bundle 

end module MAPL_ESMFFieldBundleRead
