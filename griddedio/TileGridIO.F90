#include "MAPL_Generic.h"

module MAPL_TileGridIOMod
  use ESMF
  use ESMFL_Mod
  use MAPL_AbstractGridFactoryMod
  use MAPL_AbstractRegridderMod
  use MAPL_GridManagerMod
  use MAPL_BaseMod
  use MAPL_NewRegridderManager
  use MAPL_RegridMethods
  use MAPL_TimeDataMod
  use MAPL_VerticalDataMod
  use MAPL_Constants
  use MAPL_GriddedIOMod
  use MAPL_LocStreamMod
  use MAPL_CommsMod
  use MAPL_BaseMod
  use MAPL_FieldPointerUtilities
  use MAPL_SortMod
  use pFIO
  use MAPL_GriddedIOItemVectorMod
  use MAPL_GriddedIOItemMod
  use MAPL_ExceptionHandling
  use pFIO_ClientManagerMod
  use MAPL_DataCollectionMod
  use MAPL_DataCollectionManagerMod
  use gFTL_StringVector
  use gFTL_StringStringMap
  use MAPL_FileMetadataUtilsMod
  use MAPL_DownbitMod
  use, intrinsic :: ISO_C_BINDING
  use, intrinsic :: iso_fortran_env, only: REAL64
  use ieee_arithmetic, only: isnan => ieee_is_nan
  use netcdf, only: nf90_inq_libvers
  use FIleIOSharedMod, only: MAPL_TileMaskGet
  implicit none

  private

  public :: MAPL_TileGridIO

  type tile_buffer
    real, allocatable :: ptr(:)
  end type

  type, extends(MAPL_GriddedIO) :: MAPL_TileGridIO
     type(ESMF_FieldBundle)         :: bundle
     type(tile_buffer), allocatable :: tile_buffer(:)
     type(ESMF_Field)               :: field_in, field_out
     type(ESMF_RouteHandle)         :: routeHandle
     real, allocatable              :: tilelons(:), tilelats(:)
     integer, allocatable           :: i_index(:), j_index(:)
     contains
        procedure :: CreateFileMetaData
        procedure :: CreateVariable
        procedure :: CreateQuantizationInfo
        procedure :: bundlePost
        procedure :: stageData
        procedure :: stage2DLatLon
        procedure :: regridScalar
        procedure :: request_data_from_file
        procedure :: process_data_from_file
        procedure :: swap_undef_value
        procedure :: destroy
        procedure :: InitRedistHandle
  end type MAPL_TileGridIO

  interface MAPL_TileGridIO
     module procedure new_MAPL_TileGridIO
     module procedure new_MAPL_TileIO
  end interface MAPL_TileGridIO

  contains

     function new_MAPL_TileGridIO(metadata,input_bundle,output_bundle,write_collection_id,read_collection_id, &
             metadata_collection_id,regrid_method,fraction,items,rc) result(TileGridIO)
        type(MAPL_TileGridIO) :: TileGridIO
        type(Filemetadata), intent(in), optional :: metadata
        type(ESMF_FieldBundle), intent(in), optional :: input_bundle
        type(ESMF_FieldBundle), intent(in), optional :: output_bundle
        integer, intent(in), optional :: write_collection_id
        integer, intent(in), optional :: read_collection_id
        integer, intent(in), optional :: metadata_collection_id
        integer, intent(in), optional :: regrid_method
        integer, intent(in), optional :: fraction
        type(GriddedIOitemVector), intent(in), optional :: items
        integer, intent(out), optional :: rc
        integer :: status

        TileGridIO%MAPL_GriddedIO = MAPL_GriddedIO(metadata,input_bundle,output_bundle,write_collection_id,read_collection_id, &
             metadata_collection_id,regrid_method,fraction,items,_RC)

        _RETURN(ESMF_SUCCESS)
     end function new_MAPL_TileGridIO

     function new_MAPL_TileIO(bundle,read_collection_id) result(TileIO)
        type(MAPL_TileGridIO) :: TileIO
        type(ESMF_FieldBundle),intent(in) :: bundle
        integer, intent(in) :: read_collection_id

        TileIO%bundle = bundle
        TileIO%read_collection_id = read_collection_id
     end function

     subroutine CreateFileMetaData(this,items,bundle,timeInfo,vdata,ogrid,global_attributes,rc)
        class (MAPL_TileGridIO), target, intent(inout) :: this
        type(GriddedIOitemVector), target, intent(inout) :: items
        type(ESMF_FieldBundle), intent(inout) :: bundle
        type(TimeData), intent(inout) :: timeInfo
        type(VerticalData), intent(inout), optional :: vdata
        type (ESMF_Grid), intent(inout), pointer, optional :: ogrid
        type(StringStringMap), target, intent(in), optional :: global_attributes
        integer, intent(out), optional :: rc

        type(ESMF_Grid) :: input_grid, output_grid

        type(GriddedIOitemVectorIterator) :: iter
        type(GriddedIOitem), pointer :: item
        type(stringVector) :: order
        integer :: metadataVarsSize
        type(StringStringMapIterator) :: s_iter
        character(len=:), pointer :: attr_name, attr_val
        class(Variable), pointer :: coord_var
        integer :: status
        type(Variable) :: v

        if ( allocated (this%metadata) ) deallocate(this%metadata)
        allocate(this%metadata)

        call MAPL_FieldBundleDestroy(this%output_bundle, _RC)

        this%items = items
        this%input_bundle = bundle
        this%output_bundle = ESMF_FieldBundleCreate(rc=status)
        _VERIFY(status)
        this%timeInfo = timeInfo

        call this%InitRedistHandle(_RC)

        call ESMF_FieldBundleSet(this%output_bundle,grid=this%output_grid,rc=status)
        _VERIFY(status)

        call this%timeInfo%add_time_to_metadata(this%metadata,rc=status)
        _VERIFY(status)

        order = this%metadata%get_order(rc=status)
        _VERIFY(status)
        metadataVarsSize = order%size()

        iter = this%items%begin()
        do while (iter /= this%items%end())
           item => iter%get()
           if (item%itemType == ItemTypeScalar) then
              call this%CreateVariable(item%xname,rc=status)
              _VERIFY(status)
           else if (item%itemType == ItemTypeVector) then
              call this%CreateVariable(item%xname,rc=status)
              _VERIFY(status)
              call this%CreateVariable(item%yname,rc=status)
              _VERIFY(status)
           end if
           call iter%next()
        enddo

        if (allocated(this%tilelons)) then
           v = Variable(type=PFIO_REAL32,dimensions='tile')
           call v%add_attribute('units','degrees_east')
           call v%add_attribute('long_name','longitude')
           call this%metadata%add_variable('lon',v,rc=status)
           _VERIFY(status)
           v = Variable(type=PFIO_REAL32,dimensions='tile')
           call v%add_attribute('units','degrees_north')
           call v%add_attribute('long_name','latitude')
           call this%metadata%add_variable('lat',v,rc=status)
           _VERIFY(status)
           v = Variable(type=PFIO_INT32,dimensions='tile')
           call v%add_attribute('units','1')
           call v%add_attribute('long_name','i_index')
           call this%metadata%add_variable('IG',v,rc=status)
           _VERIFY(status)
           v = Variable(type=PFIO_INT32,dimensions='tile')
           call v%add_attribute('units','1')
           call v%add_attribute('long_name','j_index')
           call this%metadata%add_variable('JG',v,rc=status)
           _VERIFY(status)
        endif


        if (this%itemOrderAlphabetical) then
           call this%alphabatize_variables(metadataVarsSize,rc=status)
           _VERIFY(status)
        end if

        if (present(global_attributes)) then
           s_iter = global_attributes%begin()
           do while(s_iter /= global_attributes%end())
              attr_name => s_iter%key()
              attr_val => s_iter%value()
              call this%metadata%add_attribute(attr_name,attr_val,_RC)
              call s_iter%next()
           enddo
        end if

        _RETURN(_SUCCESS)

     end subroutine CreateFileMetaData

     subroutine CreateVariable(this,itemName,rc)
        class (MAPL_TileGridIO), intent(inout) :: this
        character(len=*), intent(in) :: itemName
        integer, optional, intent(out) :: rc

        integer :: status

        type(ESMF_Field) :: field,newField
        class (AbstractGridFactory), pointer :: factory
        integer :: fieldRank, global_dim(3)
        logical :: isPresent
        real, pointer :: ptr1d(:), ptr2d(:,:), ptr3d(:,:,:)
        character(len=ESMF_MAXSTR) :: varName,longName,units
        character(len=:), allocatable :: grid_dims
        character(len=:), allocatable :: vdims
        type(Variable) :: v

        call ESMF_FieldBundleGet(this%input_bundle,itemName,field=field,rc=status)
        _VERIFY(status)

        call ESMF_FieldGet(field,rank=fieldRank,rc=status)
        _VERIFY(status)
        call ESMF_FieldGet(field,name=varName,rc=status)
        _VERIFY(status)
        call ESMF_AttributeGet(field,name="LONG_NAME",isPresent=isPresent,rc=status)
        _VERIFY(status)
        if ( isPresent ) then
           call ESMF_AttributeGet  (FIELD, NAME="LONG_NAME",VALUE=LongName, RC=STATUS)
           _VERIFY(STATUS)
        else
           LongName = varName
        endif
        call ESMF_AttributeGet(field,name="UNITS",isPresent=isPresent,rc=status)
        _VERIFY(status)
        if ( isPresent ) then
           call ESMF_AttributeGet  (FIELD, NAME="UNITS",VALUE=units, RC=STATUS)
           _VERIFY(STATUS)
        else
           units = 'unknown'
        endif
         call MAPL_GridGet(this%output_grid,globalCellCountPerDim=global_dim,rc=status)
        _VERIFY(status)
        grid_dims = 'tile'

        if (.not. this%metadata%has_dimension('tile')) then
           call this%metadata%add_dimension('tile', global_dim(1))
        endif

        if (this%timeInfo%is_initialized) then
           
           if (fieldRank==1) then
              vdims = grid_dims//",time"
              call ESMF_FieldGet(field,farrayPtr=ptr1d, _RC)
           else if (fieldRank==2) then
              vdims=grid_dims//",unknown_dim1,time"
           else if (fieldRank==3) then
              vdims=grid_dims//",unknown_dim2,unknown_dim1,time"
           else if (fieldRank==4) then
              vdims=grid_dims//",unknown_dim3,unknown_dim2,unknown_dim1,time"
           else 
              _FAIL( 'Unsupported field rank')
           end if
        else
           if (fieldRank==1) then
              vdims = grid_dims
           elseif (fieldRank==2) then
              vdims=grid_dims//",unknown_dim1"
           else if (fieldRank==3) then
              vdims=grid_dims//",unknown_dim2,unkown_dim1"
           else
              _FAIL( 'Unsupported field rank')
           end if
        end if
        v = Variable(type=PFIO_REAL32,dimensions=vdims,chunksizes=this%chunking,deflation=this%deflateLevel,quantize_algorithm=this%quantizeAlgorithm,quantize_level=this%quantizeLevel,zstandard_level=this%zstandardLevel)
        call v%add_attribute('units',trim(units))
        call v%add_attribute('long_name',trim(longName))
        call v%add_attribute('standard_name',trim(longName))
        call v%add_attribute('missing_value',MAPL_UNDEF)
        call v%add_attribute('fmissing_value',MAPL_UNDEF)
        call v%add_attribute('_FillValue',MAPL_UNDEF)

        call this%metadata%add_variable(trim(varName),v,rc=status)
        _VERIFY(status)
        ! finally make a new field if neccessary
        newField = ESMF_FieldCreate(this%output_grid, name=trim(varName), typekind=ESMF_TYPEKIND_R4, _RC)
        call MAPL_FieldCopyAttributes(FIELD_IN=field, FIELD_OUT= newField, _RC)
        !MAPL_FieldCreate(field,this%output_grid,rc=status)
        !_VERIFY(status)
        call MAPL_FieldBundleAdd(this%output_bundle,newField,rc=status)
        _VERIFY(status)
        _RETURN(_SUCCESS)

     end subroutine CreateVariable

     subroutine CreateQuantizationInfo(this,rc)
        class (MAPL_TileGridIO), intent(inout) :: this
        integer, optional, intent(out) :: rc

        integer :: status

        _RETURN(_SUCCESS)

     end subroutine CreateQuantizationInfo

     subroutine bundlepost(this,filename,oClients,rc)
        class (MAPL_TileGridIO), target, intent(inout) :: this
        character(len=*), intent(in) :: filename
        type (ClientManager), optional, intent(inout) :: oClients
        integer, optional, intent(out) :: rc

        integer :: status
        type(ESMF_Field) :: outField
        integer :: tindex
        type(ArrayReference) :: ref

        type(GriddedIOitemVectorIterator) :: iter
        type(GriddedIOitem), pointer :: item
        logical :: have_time

        have_time = this%timeInfo%am_i_initialized()

        if (have_time) then
           this%times = this%timeInfo%compute_time_vector(this%metadata,rc=status)
           _VERIFY(status)
           ref = ArrayReference(this%times)
           call oClients%stage_nondistributed_data(this%write_collection_id,trim(filename),'time',ref)
           tindex = size(this%times)
           if (tindex==1) then
              call this%stage2DLatLon(filename,oClients=oClients,_RC)
           end if
        else
           tindex = -1
           call this%stage2DLatLon(filename,oClients=oClients,_RC)
        end if

        iter = this%items%begin()
        do while (iter /= this%items%end())
           item => iter%get()
           if (item%itemType == ItemTypeScalar) then
              call this%RegridScalar(item%xname,rc=status)
              _VERIFY(status)
              call ESMF_FieldBundleGet(this%output_bundle,item%xname,field=outField,rc=status)
              _VERIFY(status)
              call this%stageData(outField,filename,tIndex, oClients=oClients,rc=status)
              _VERIFY(status)
           else if (item%itemType == ItemTypeVector) then
              _FAIL('NO Vector for redistribution')
              !call this%RegridVector(item%xname,item%yname,rc=status)
              !_VERIFY(status)
              !call ESMF_FieldBundleGet(this%output_bundle,item%xname,field=outField,rc=status)
              !_VERIFY(status)
              !if (this%vdata%regrid_type==VERTICAL_METHOD_ETA2LEV .and. (this%vdata%extrap_below_surf .eqv. .false.)) then
              !   call this%vdata%correct_topo(outField,rc=status)
              !   _VERIFY(status)
              !end if
              !call this%stageData(outField,filename,tIndex,oClients=oClients,rc=status)
              !_VERIFY(status)
              !call ESMF_FieldBundleGet(this%output_bundle,item%yname,field=outField,rc=status)
              !_VERIFY(status)
              !if (this%vdata%regrid_type==VERTICAL_METHOD_ETA2LEV .and. (this%vdata%extrap_below_surf .eqv. .false.)) then
              !   call this%vdata%correct_topo(outField,rc=status)
              !   _VERIFY(status)
              !end if
              !call this%stageData(outField,filename,tIndex,oClients=oClients,rc=status)
              !_VERIFY(status)
              _FAIL('not yet implmented for tile vector')
           end if
           call iter%next()
        enddo

        _RETURN(ESMF_SUCCESS)

     end subroutine bundlepost

     subroutine RegridScalar(this,itemName,rc)
        class (MAPL_TileGridIO), intent(inout) :: this
        character(len=*), intent(in) :: itemName
        integer, optional, intent(out) :: rc

        integer :: status

        type(ESMF_Field) :: field,outField
        integer :: fieldRank, i
        real, pointer :: ptr3d(:,:,:),outptr3d(:,:,:)
        real, pointer :: ptr2d(:,:), outptr2d(:,:), ptr1d(:), outptr1d(:)
        real, allocatable, target :: ptr3d_inter(:,:,:)
        type(ESMF_Grid) :: gridIn,gridOut
        logical :: hasDE_in, hasDE_out, isPresent
        character(len=ESMF_MAXSTR) :: long_name

        ptr3d => null()

        call ESMF_FieldBundleGet(this%output_bundle,itemName,field=outField,rc=status)
        _VERIFY(status)
        call ESMF_FieldBundleGet(this%input_bundle,itemName,field=field,rc=status)
        _VERIFY(status)
        call ESMF_FieldGet(field,rank=fieldRank,rc=status)
        _VERIFY(status)

        
        if (fieldRank==1) then
           call ESMF_FieldRedist(field, outField, this%routeHandle, rc=status)
           _VERIFY(status)
        else if (fieldRank==2) then
           _FAIL('NO 2d for tile redistribution')
          !call MAPL_FieldGetPointer(Field,    ptr2d,rc=status)
          ! _VERIFY(status)
          ! call MAPL_FieldGetPointer(outField, outptr2d,rc=status)
          ! _VERIFY(status)
          ! call MAPL_FieldGetPointer(this%field_in,ptr1d,rc=status)
          ! _VERIFY(status)
          ! call MAPL_FieldGetPointer(this%field_out,outptr1d,rc=status)
          ! _VERIFY(status)
          ! do i =1, size(ptr2d, 2)
          !    ptr1d(:) = ptr2d(:,i)
          !    call ESMF_FieldRedist(this%field_in, this%field_out, this%routeHandle, rc=status)
          !    _VERIFY(status)
          !    outptr2d(:,i) = outptr1d(:)
          ! enddo

        else if (fieldRank==3) then

           _FAIL('rank not supported')
        end if

        if (allocated(ptr3d_inter)) deallocate(ptr3d_inter)
        _RETURN(_SUCCESS)

     end subroutine RegridScalar

     subroutine RegridVector(this,xName,yName,rc)
        class (MAPL_TileGridIO), intent(inout) :: this
        character(len=*), intent(in) :: xName
        character(len=*), intent(in) :: yName
        integer, optional, intent(out) :: rc
        _FAIL("No Vector for tile")
   end subroutine RegridVector

   ! this subroutine inherit form GriddedIO
   ! Now it is used to stage 1d latlon and II, JJ
   subroutine stage2DLatLon(this, fileName, oClients, rc)
     class (MAPL_TileGridIO), target, intent(inout) :: this
     character(len=*), intent(in) :: fileName
     type (ClientManager), optional, target, intent(inout) :: oClients
     integer, optional, intent(out) :: rc

     integer :: status
     integer :: i1, j1, in, jn, global_dim(3)
     type(ArrayReference), target :: ref
     integer, allocatable :: localStart(:),globalStart(:),globalCount(:)

     if (allocated(this%tilelons)) then
        call MAPL_GridGet(this%output_grid,globalCellCountPerDim=global_dim,_RC)
        call MAPL_GridGetInterior(this%output_grid,i1,in,j1,jn)
        allocate(localstart,source=[i1])
        allocate(globalstart,source=[1])
        allocate(globalcount,source=[global_dim(1)])
        ref = ArrayReference(this%tilelons)
        call oClients%collective_stage_data(this%write_collection_id,trim(filename),'lon', &
             ref,start=localStart, global_start=GlobalStart, global_count=GlobalCount)

        ref = ArrayReference(this%tilelats)
        call oClients%collective_stage_data(this%write_collection_id,trim(filename),'lat', &
             ref,start=localStart, global_start=GlobalStart, global_count=GlobalCount)

        ref = ArrayReference(this%i_index)
        call oClients%collective_stage_data(this%write_collection_id,trim(filename),'IG', &
             ref,start=localStart, global_start=GlobalStart, global_count=GlobalCount)
        
        ref = ArrayReference(this%j_index)
        call oClients%collective_stage_data(this%write_collection_id,trim(filename),'JG', &
             ref,start=localStart, global_start=GlobalStart, global_count=GlobalCount)

     endif

     _RETURN(_SUCCESS)

   end subroutine stage2DLatLon

  subroutine stageData(this, field, fileName, tIndex, oClients, rc)
     class (MAPL_TileGridIO), intent(inout) :: this
     type(ESMF_Field), intent(inout) :: field
     character(len=*), intent(in) :: fileName
     integer, intent(in) :: tIndex
     type (ClientManager), optional, intent(inout) :: oClients
     integer, optional, intent(out) :: rc

     integer :: status
     integer :: fieldRank
     character(len=ESMF_MAXSTR) :: fieldName
     real, pointer :: ptr3d(:,:,:) => null()
     real, pointer :: ptr2d(:,:) => null()
     real, pointer :: ptr1d(:) => null()
     type(ArrayReference) :: ref
     integer :: lm
     logical :: hasDE
     integer, allocatable :: localStart(:),globalStart(:),globalCount(:)
     integer, allocatable :: gridLocalStart(:),gridGlobalStart(:),gridGlobalCount(:)
     class (AbstractGridFactory), pointer :: factory
     real, allocatable :: temp_2d(:,:), temp_3d(:,:,:)
     type(ESMF_VM) :: vm
     integer :: mpi_comm, i1, in, j1, jn, global_dim(3)

     call ESMF_VMGetCurrent(vm,_RC)
     call ESMF_VMGet(vm,mpiCommunicator=mpi_comm,_RC)
     call MAPL_GridGet(this%output_grid,globalCellCountPerDim=global_dim,_RC)
     call MAPL_GridGetInterior(this%output_grid,i1,in,j1,jn)
     call ESMF_FieldGet(field,rank=fieldRank,name=fieldName,rc=status)
     _VERIFY(status)
     if (fieldRank == 1) then
       allocate(localstart,source=[i1])
       allocate(globalstart,source=[1])
       allocate(globalcount,source=[global_dim(1)])
       call ESMF_FieldGet(Field,farrayPtr=ptr1d,rc=status)
       ref = ArrayReference(ptr1d)
     else
       _FAIL('only 1d so far')
     endif
     if (tindex > -1) then
        localstart  = [localstart,1]
        globalstart = [globalstart, tindex]
        globalcount = [globalcount, 1]
     endif
     call oClients%collective_stage_data(this%write_collection_id,trim(filename),trim(fieldName), &
           ref,start=localStart, global_start=GlobalStart, global_count=GlobalCount)
      _RETURN(_SUCCESS)

  end subroutine stageData

  subroutine request_data_from_file(this,filename,timeindex,rc)
     class(MAPL_TileGridIO), intent(inout) :: this
     character(len=*), intent(in) :: filename
     integer, intent(in) :: timeindex
     integer, intent(out), optional :: rc

     integer :: status
     integer :: num_vars,i,rank
     type(ArrayReference) :: ref
     character(len=ESMF_MAXSTR), allocatable :: names(:)
     type(ESMF_Field) :: field
     type(ESMF_Grid) :: grid
     integer :: counts(3)
     integer, allocatable :: local_start(:), global_start(:), global_count(:)


     call ESMF_FieldBundleGet(this%bundle, fieldCount=num_vars, _RC)
     allocate(this%tile_buffer(num_vars))
     allocate(names(num_vars))
     call ESMF_FieldBundleGet(this%bundle, fieldNameList=names, _RC)
     do i=1,num_vars
        call ESMF_FieldBundleGet(this%bundle,names(i),field=field,_RC)
        call ESMF_FieldGet(field,rank=rank,grid=grid,_RC)
        call MAPL_GridGet(grid,globalCellCountPerDim=counts,_RC)
        if (rank==1) then
           allocate(local_start(2),global_start(2),global_count(2))
           local_start = [1,timeindex]
           global_start = [1,timeindex]
           global_count = [counts(1),1]
           if (mapl_am_I_root()) then
              allocate(this%tile_buffer(i)%ptr(counts(1)),_STAT)
           else
              allocate(this%tile_buffer(i)%ptr((0)),_STAT)
           end if
           ref = ArrayReference(this%tile_buffer(i)%ptr)
           call i_clients%collective_prefetch_data(this%read_collection_id, filename, trim(names(i)), ref,  &
              start=local_start, global_start=global_start, global_count = global_count, _RC)
           deallocate(local_start,global_start,global_count)
        else
           _FAIL("rank >1 tile fields not supported")
        end if
     end do

     _RETURN(_SUCCESS)   

  end subroutine request_data_from_file

  subroutine process_data_from_file(this,rc)
     class(MAPL_TileGridIO), intent(inout) :: this
     integer, intent(out), optional :: rc

     integer :: status
     integer :: i,num_vars,rank
     type(ESMF_Field) :: field
     character(len=ESMF_MAXSTR), allocatable :: names(:)
     type(ESMF_Grid) :: grid
     integer, pointer :: mask(:)
     real, pointer :: ptr1d(:)

     call ESMF_FieldBundleGet(this%bundle, fieldCount=num_vars, _RC)
     allocate(names(num_vars))
     call ESMF_FieldBundleGet(this%bundle, fieldNameList=names, _RC)
     do i=1,num_vars
        call ESMF_FieldBundleGet(this%bundle,names(i),field=field,_RC)
        call ESMF_FieldGet(field,rank=rank,grid=grid,_RC)
        call MAPL_TileMaskGet(grid,mask,_RC)
        if (rank==1) then
           call ESMF_FieldGet(field,localDE=0,farrayPtr=ptr1d,_RC)
           call ArrayScatter(ptr1d,this%tile_buffer(i)%ptr,grid,mask=mask,_RC)
           deallocate(this%tile_buffer(i)%ptr)
        else
           _FAIL("rank not supported for tile io")
        end if
     enddo
     deallocate(this%tile_buffer)
     _RETURN(_SUCCESS)

  end subroutine process_data_from_file

  subroutine swap_undef_value(this,fname,rc)
     class (MAPL_TileGridIO), intent(inout) :: this
     character(len=*), intent(in) :: fname
     integer, optional, intent(out) :: rc

     integer :: status

     type(ESMF_Field) :: field
     integer :: fieldRank
     real, pointer :: ptr3d(:,:,:)
     real, pointer :: ptr2d(:,:)
     real, pointer :: ptr1d(:)
     type(ESMF_Grid) :: gridIn
     logical :: hasDE_in
     real(REAL32) :: fill_value

     if ( .not. this%current_file_metadata%var_has_missing_value(fname) ) then
        _RETURN(_SUCCESS)
     endif

     fill_value = this%current_file_metadata%var_get_missing_value(fname,_RC)

     call ESMF_FieldBundleGet(this%input_bundle,fname,field=field,_RC)
     call ESMF_FieldBundleGet(this%input_bundle,grid=gridIn,_RC)
     call ESMF_FieldGet(field,rank=fieldRank,_RC)
     hasDE_in = MAPL_GridHasDE(gridIn,_RC)

     if (fieldRank==1) then
        if (hasDE_in) then
           call MAPL_FieldGetPointer(field,ptr1d,_RC)
        else
           allocate(ptr1d(0))
        end if

        if (isnan(fill_value)) then
           where(isnan(ptr1d)) ptr1d=MAPL_UNDEF
        else
           where(ptr1d==fill_value) ptr1d=MAPL_UNDEF
        endif
     else if (fieldRank==2) then
        if (hasDE_in) then
           call MAPL_FieldGetPointer(field,ptr2d,_RC)
        else
           allocate(ptr2d(0,0))
        end if

        if (isnan(fill_value)) then
           where(isnan(ptr2d)) ptr2d=MAPL_UNDEF
        else
           where(ptr2d==fill_value) ptr2d=MAPL_UNDEF
        endif

     else if (fieldRank==3) then
        if (hasDE_in) then
           call ESMF_FieldGet(field,farrayPtr=ptr3d,_RC)
        else
           allocate(ptr3d(0,0,0))
        end if

        if (isnan(fill_value)) then
           where(isnan(ptr3d)) ptr3d=MAPL_UNDEF
        else
           where(ptr3d==fill_value) ptr3d=MAPL_UNDEF
        endif

     else
        _FAIL('rank not supported')
     end if

     _RETURN(_SUCCESS)

  end subroutine swap_undef_value

  subroutine destroy(this, rc)
     class (MAPL_TileGridIO), intent(inout) :: this
     integer, intent(out), optional :: rc
     integer :: status
     if(allocated(this%chunking)) deallocate(this%chunking)
     call ESMF_FieldRedistRelease(this%routeHandle, _RC)
     call MAPL_FieldBundleDestroy(this%output_bundle, _RC)
     call ESMF_FieldDestroy(this%field_in, noGarbage=.true., _RC)
     call ESMF_FieldDestroy(this%field_out,noGarbage=.true., _RC)
     _RETURN(_SUCCESS)
  end subroutine destroy

  subroutine InitRedistHandle(this, rc)
     class (MAPL_TileGridIO), intent(inout) :: this
     integer, intent(out), optional :: rc


     type(ESMF_Grid) :: tilegrid, ordered_tilegrid
     integer, pointer :: local_id(:), local_i(:), local_j(:)
     real, pointer    :: tilelons(:), tilelats(:), ptr1d(:), outptr1d(:)
     integer, allocatable :: global_id(:)
     integer              :: nt_global, i1, i2, j1, j2, status
     type (ESMF_DistGrid)              :: distgrid
     integer                           :: arbIndexCount
     integer, allocatable              :: arbIndex(:,:)
     integer(kind=INT64)               :: ADDR
     type (MAPL_LocStream)             :: locstream
     character(len=ESMF_MAXSTR)        :: gname
     type(ESMF_GRID)                   :: attachedgrid  

     call ESMF_FieldBundleGet(this%input_bundle,grid=tilegrid,rc=status)
     _VERIFY(status)
     call ESMF_AttributeGet(tilegrid, name='TILEGRID_LOCSTREAM_ADDR', &
              value=ADDR, _RC)
     call c_MAPL_LocStreamRestorePtr(locstream, ADDR)
     call MAPL_LocStreamGet(locstream, nt_global = nt_global, local_id = local_id, &
                            local_i  = local_i, local_j  = local_j,                &
                            tilelons = tilelons,    tilelats = tilelats, _RC)

     allocate(global_id(nt_global))
     call ESMFL_FCollect(tilegrid, global_id, local_id, _RC)
     call MAPL_grid_interior(tilegrid, i1, i2, j1, j2)
     call MAPL_Sort(global_id)  
     call ESMF_GridGet(tilegrid, name=gname, _RC)

     distgrid = ESMF_DistGridCreate( & 
         arbSeqIndexList=global_id(i1:i2), rc=status)
     _VERIFY(STATUS)

     ordered_TILEGRID = ESMF_GridEmptyCreate(rc=status)
     _VERIFY(STATUS)
     arbIndexCount = i2-i1 +1
     allocate(arbIndex(arbIndexCount,1))
     arbIndex(:,1) = global_id(i1:i2)
     gname = 'ordered_'//trim(gname)
     call ESMF_GridSet(ordered_tilegrid,  &
         name=trim(gname),        &
         distgrid=distgrid, &
         indexFlag=ESMF_INDEX_DELOCAL, &
         distDim = (/1/), &
         localArbIndexCount=arbIndexCount, &
         localArbIndex=arbIndex, &
         minIndex=(/1/), &
         maxIndex=(/NT_GLOBAL/), &
         rc=status)
     _VERIFY(STATUS)
      
     call ESMF_GridCommit(ordered_tilegrid, rc=status)
    _VERIFY(STATUS)   
     this%field_in  = ESMF_FieldCreate(grid=tilegrid,  typekind=ESMF_TYPEKIND_R4, _RC)
     this%field_out = ESMF_FieldCreate(grid=ordered_tilegrid, typekind=ESMF_TYPEKIND_R4, _RC)
     this%output_grid = ordered_tilegrid
   
     call ESMF_FieldRedistStore(srcField= this%field_in, dstField=this%field_out, &
                routehandle=this%routehandle, _RC)

     ! reordered lat-lon, II, and JJ
     if (associated(tilelons) .and. associated(tilelats) .and. associated(local_i) .and. associated(local_j)) then 
        allocate(this%tilelons(arbIndexCount), this%tilelats(arbIndexCount))
        allocate(this%i_index(arbIndexCount),  this%j_index(arbIndexCount))
        call MAPL_FieldGetPointer(this%field_in, ptr1d,rc=status)
        call MAPL_FieldGetPointer(this%field_out,outptr1d,rc=status)

        call MAPL_LocStreamGet(locstream, attachedgrid=attachedgrid, _RC)
        call MAPL_grid_interior(attachedgrid, i1, i2, j1, j2)
        call ESMF_GridGet(attachedgrid, name=gname, _RC)

        ptr1d(:) = tilelons(:)
        call ESMF_FieldRedist(this%field_in, this%field_out, this%routeHandle, rc=status)
        this%tilelons = outptr1d*MAPL_RADIANS_TO_DEGREES

        ptr1d(:) = tilelats(:)
        call ESMF_FieldRedist(this%field_in, this%field_out, this%routeHandle, rc=status)
        this%tilelats = outptr1d*MAPL_RADIANS_TO_DEGREES

        ptr1d(:) = local_i(:) + i1 -1
        if (index(gname, 'EASE') /=0) ptr1d = ptr1d - 1
        call ESMF_FieldRedist(this%field_in, this%field_out, this%routeHandle, rc=status)
        this%i_index = nint(outptr1d)

        ptr1d(:) = local_j(:) + j1 -1
        if (index(gname, 'EASE') /=0) ptr1d = ptr1d - 1
        call ESMF_FieldRedist(this%field_in, this%field_out, this%routeHandle, rc=status)
        this%j_index = nint(outptr1d) 
     endif
     _RETURN(_SUCCESS)
     
  end subroutine  

end module MAPL_TileGridIOMod
