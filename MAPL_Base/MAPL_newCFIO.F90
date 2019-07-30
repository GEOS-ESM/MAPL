#include "MAPL_Generic.h"

module MAPL_newCFIOMod
  use ESMF
  use MAPL_AbstractGridFactoryMod
  use MAPL_AbstractRegridderMod
  use MAPL_GridManagerMod
  use MAPL_GenericMod
  use MAPL_BaseMod
  use MAPL_IntegerVectorMod
  use MAPL_RegridderManagerMod
  use MAPL_RegridderSpecMod
  use MAPL_TimeDataMod
  use MAPL_VerticalDataMod
  use MAPL_ConstantsMod
  use pFIO
  use MAPL_newCFIOItemVectorMod
  use MAPL_newCFIOItemMod
  use MAPL_ErrorHandlingMod
  use MAPL_ServerManagerMod
  use, intrinsic :: ISO_C_BINDING
  use, intrinsic :: iso_fortran_env, only: REAL64
  implicit none
  
  private

  type, public :: MAPL_newCFIO
     type(FileMetaData) :: metadata
     integer :: collection_id
     type(IntegerVector) :: data_req
     class (AbstractRegridder), pointer :: regrid_handle => null()
     type(ESMF_Grid) :: output_grid
     logical :: doVertRegrid
     type(ESMF_FieldBundle) :: output_bundle
     type(ESMF_FieldBundle) :: bundle
     type(ESMF_Time) :: startTime
     integer :: regrid_method = REGRID_METHOD_BILINEAR
     integer :: nbits = 1000
     real, allocatable :: lons(:,:),lats(:,:)
     real, allocatable :: times(:)
     type(TimeData) :: timeInfo
     type(VerticalData) :: vdata
     type(newCFIOitemVector) :: items
     integer :: deflateLevel = 0
     integer, allocatable :: chunking(:)
     contains
        procedure :: CreateFileMetaData
        procedure :: CreateVariable
        procedure :: modifyTime
        procedure :: bundlePost
        procedure :: bundleWait
        procedure :: stageData
        procedure :: stage2DLatLon
        procedure :: regridScalar
        procedure :: regridVector
        procedure :: set_param
        procedure :: set_default_chunking
  end type MAPL_newCFIO

  contains

     subroutine CreateFileMetaData(this,items,bundle,timeInfo,vdata,ogrid,rc)
        class (MAPL_newCFIO), intent(inout) :: this
        type(newCFIOitemVector), target, intent(inout) :: items
        type(ESMF_FieldBundle), intent(inout) :: bundle
        type(TimeData), intent(inout) :: timeInfo
        type(VerticalData), intent(inout), optional :: vdata
        type (ESMF_Grid), intent(inout), pointer, optional :: ogrid
        integer, intent(out), optional :: rc

        type(ESMF_Grid) :: input_grid
        class (AbstractGridFactory), pointer :: factory

        type(newCFIOitemVectorIterator) :: iter
        type(newCFIOitem), pointer :: item

        integer :: status

        this%items = items
        this%bundle = bundle
        this%output_bundle = ESMF_FieldBundleCreate(rc=status)
        _VERIFY(status)
        this%timeInfo = timeInfo
        call ESMF_FieldBundleGet(this%bundle,grid=input_grid,rc=status)
        _VERIFY(status)
        if (present(ogrid)) then
           this%output_grid=ogrid
        else
           call ESMF_FieldBundleGet(this%bundle,grid=this%output_grid,rc=status)
           _VERIFY(status)
        end if
        this%regrid_handle => new_regridder_manager%make_regridder(input_grid,this%output_grid,this%regrid_method,rc=status)
        _VERIFY(status)
        call ESMF_FieldBundleSet(this%output_bundle,grid=this%output_grid,rc=status)
        _VERIFY(status)
        factory => get_factory(this%output_grid,rc=status)
        _VERIFY(status)
        call factory%append_metadata(this%metadata)

        if (present(vdata)) then
           this%vdata=vdata
        else
           this%vdata=VerticalData(rc=status)
           _VERIFY(status)
        end if
        call this%vdata%append_vertical_metadata(this%metadata,this%bundle,rc=status)
        _VERIFY(status)
        this%doVertRegrid = (this%vdata%regrid_type /= VERTICAL_METHOD_NONE)
        if (this%vdata%regrid_type == VERTICAL_METHOD_ETA2LEV) call this%vdata%get_interpolating_variable(this%bundle,rc=status)
        _VERIFY(status)

        call this%timeInfo%add_time_to_metadata(this%metadata,rc=status)
        _VERIFY(status)

        iter = this%items%begin()
        if (.not.allocated(this%chunking)) then
           call this%set_default_chunking(rc=status)
           _VERIFY(status)
        end if
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
       
     end subroutine CreateFileMetaData

     subroutine set_param(this,deflation,chunking,nbits,regrid_method,rc)
        class (MAPL_newCFIO), intent(inout) :: this
        integer, optional, intent(in) :: deflation
        integer, optional, intent(in) :: chunking(:)
        integer, optional, intent(in) :: nbits
        integer, optional, intent(in) :: regrid_method
        integer, optional, intent(out) :: rc

        integer :: status

        if (present(regrid_method)) this%regrid_method=regrid_method
        if (present(nbits)) this%nbits=nbits
        if (present(deflation)) this%deflateLevel = deflation
        if (present(chunking)) then
           allocate(this%chunking,source=chunking,stat=status)
           _VERIFY(status)
        end if
        _RETURN(ESMF_SUCCESS)

     end subroutine set_param

     subroutine set_default_chunking(this,rc)
        class (MAPL_newCFIO), intent(inout) :: this
        integer, optional, intent(out) :: rc

        integer ::  global_dim(3)
        integer :: status

        call MAPL_GridGet(this%output_grid,globalCellCountPerDim=global_dim,rc=status)
        _VERIFY(status)
        if (global_dim(1)*6 == global_dim(2)) then
           allocate(this%chunking(5))
           this%chunking(1) = global_dim(1)
           this%chunking(2) = global_dim(1)
           this%chunking(3) = 1
           this%chunking(4) = 1
           this%chunking(5) = 1
        else
           allocate(this%chunking(4))
           this%chunking(1) = global_dim(1)
           this%chunking(2) = global_dim(2)
           this%chunking(3) = 1
           this%chunking(4) = 1
        endif
        _RETURN(ESMF_SUCCESS)

     end subroutine set_default_chunking


     subroutine CreateVariable(this,itemName,rc)
        class (MAPL_newCFIO), intent(inout) :: this
        character(len=*), intent(in) :: itemName
        integer, optional, intent(out) :: rc
 
        integer :: status

        type(ESMF_Field) :: field,newField
        class (AbstractGridFactory), pointer :: factory
        integer :: fieldRank
        logical :: isPresent
        character(len=ESMF_MAXSTR) :: varName,longName,units
        character(len=:), allocatable :: grid_dims
        character(len=:), allocatable :: vdims
        type(Variable) :: v

        call ESMF_FieldBundleGet(this%bundle,itemName,field=field,rc=status)
        _VERIFY(status)
        factory => get_factory(this%output_grid,rc=status)
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
        grid_dims = factory%get_grid_vars()
        if (fieldRank==2) then
           vdims=grid_dims//",time"
        else if (fieldRank==3) then
           vdims=grid_dims//",lev,time"
        end if
        v = Variable(type=PFIO_REAL32,dimensions=vdims,chunksizes=this%chunking,deflation=this%deflateLevel)
        call v%add_attribute('units',trim(units))
        call v%add_attribute('long_name',trim(longName))
        call v%add_attribute('missing_value',MAPL_UNDEF)
        call v%add_attribute('_FillValue',MAPL_UNDEF)
        call v%add_attribute('valid_range',(/-MAPL_UNDEF,MAPL_UNDEF/))
        call factory%append_variable_metadata(v)
        call this%metadata%add_variable(trim(varName),v)
        ! finally make a new field if neccessary
        if (this%doVertRegrid .and. (fieldRank ==3) ) then
           newField = MAPL_FieldCreate(field,this%output_grid,lm=this%vData%lm,rc=status)
           _VERIFY(status)
           call MAPL_FieldBundleAdd(this%output_bundle,newField,rc=status)
        else
           newField = MAPL_FieldCreate(field,this%output_grid,rc=status)
           _VERIFY(status)
           call MAPL_FieldBundleAdd(this%output_bundle,newField,rc=status)
        end if
        

     end subroutine CreateVariable

     subroutine modifyTime(this,rc) 
        class(MAPL_newCFIO), intent(inout) :: this
        integer, optional, intent(out) :: rc
 
        type(Variable) :: v
        type(StringVariableMap) :: var_map
        integer :: status

        v = this%timeInfo%define_time_variable(rc=status)
        _VERIFY(status)
        call this%metadata%modify_variable('time',v,rc=status)
        _VERIFY(status)
        call var_map%insert('time',v)
        call  o_ClientPtr%modify_metadata(this%collection_id, var_map = var_map)
        _RETURN(ESMF_SUCCESS)

     end subroutine modifyTime

     subroutine bundlepost(this,filename,rc)
        class (MAPL_newCFIO), intent(inout) :: this
        character(len=*), intent(in) :: filename
        integer, optional, intent(out) :: rc

        integer :: status
        type(ESMF_Field) :: outField
        integer :: tindex,request_id
        type(ArrayReference) :: ref

        type(newCFIOitemVectorIterator) :: iter
        type(newCFIOitem), pointer :: item

        this%times = this%timeInfo%compute_time_vector(this%metadata,rc=status)
        _VERIFY(status)
        ref = ArrayReference(this%times)
        request_id = o_ClientPtr%stage_nondistributed_data(this%collection_id,trim(filename),'time',ref) 
        call this%data_req%push_back(request_id)

        tindex = size(this%times)
        if (tindex==1) then
           call this%stage2DLatLon(filename,rc=status)
        end if

        iter = this%items%begin()
        do while (iter /= this%items%end())
           item => iter%get()
           if (item%itemType == ItemTypeScalar) then
              call this%RegridScalar(item%xname,rc=status)
              _VERIFY(status)
              call ESMF_FieldBundleGet(this%output_bundle,item%xname,field=outField,rc=status)
              _VERIFY(status)
              call this%stageData(outField,filename,tIndex,rc=status)
              _VERIFY(status)
           else if (item%itemType == ItemTypeVector) then
              call this%RegridVector(item%xname,item%yname,rc=status)
              _VERIFY(status)
              call ESMF_FieldBundleGet(this%output_bundle,item%xname,field=outField,rc=status)
              _VERIFY(status)
              call this%stageData(outField,filename,tIndex,rc=status)
              _VERIFY(status)
              call ESMF_FieldBundleGet(this%output_bundle,item%yname,field=outField,rc=status)
              _VERIFY(status)
              call this%stageData(outField,filename,tIndex,rc=status)
              _VERIFY(status)
           end if
           call iter%next()
        enddo

        _RETURN(ESMF_SUCCESS)

     end subroutine bundlepost

     subroutine RegridScalar(this,itemName,rc)
        class (MAPL_newCFIO), intent(inout) :: this
        character(len=*), intent(in) :: itemName
        integer, optional, intent(out) :: rc

        integer :: status

        type(ESMF_Field) :: field,outField
        integer :: fieldRank
        real, pointer :: ptr3d(:,:,:),outptr3d(:,:,:)
        real, pointer :: ptr2d(:,:), outptr2d(:,:)
        real, allocatable, target :: ptr3d_inter(:,:,:)

        call ESMF_FieldBundleGet(this%output_bundle,itemName,field=outField,rc=status)
        _VERIFY(status)

        if (this%doVertRegrid) then
           call ESMF_FieldBundleGet(this%bundle,itemName,field=field,rc=status)
           _VERIFY(status)
           call ESMF_FieldGet(Field,rank=fieldRank,rc=status)
           _VERIFY(status)
           if (fieldRank==3) then
              call ESMF_FieldGet(field,farrayPtr=ptr3d,rc=status)
              _VERIFY(status)
              allocate(ptr3d_inter(size(ptr3d,1),size(ptr3d,2),this%vdata%lm),stat=status)
              _VERIFY(status)
              if (this%vdata%regrid_type==VERTICAL_METHOD_SELECT) then
                 call this%vdata%regrid_select_level(ptr3d,ptr3d_inter,rc=status)
                 _VERIFY(status)
              else if (this%vdata%regrid_type==VERTICAL_METHOD_ETA2LEV) then
                 call this%vdata%regrid_eta_to_pressure(ptr3d,ptr3d_inter,rc=status)
                 _VERIFY(status)
              end if
              ptr3d => ptr3d_inter
           end if
        else
           if (associated(ptr3d)) nullify(ptr3d)
        end if

        call ESMF_FieldBundleGet(this%bundle,itemName,field=field,rc=status)
        _VERIFY(status)
        call ESMF_FieldGet(field,rank=fieldRank,rc=status)
        _VERIFY(status)
        if (fieldRank==2) then
           call ESMF_FieldGet(field,farrayPtr=ptr2d,rc=status)
           _VERIFY(status)
           call ESMF_FieldGet(OutField,farrayPtr=outptr2d,rc=status)
           _VERIFY(status)
           call this%regrid_handle%regrid(ptr2d,outPtr2d,rc=status)
           _VERIFY(status)
        else if (fieldRank==3) then
           if (.not.associated(ptr3d)) then
              call ESMF_FieldGet(field,farrayPtr=ptr3d,rc=status)
              _VERIFY(status)
           end if
           call ESMF_FieldGet(OutField,farrayPtr=outptr3d,rc=status)
           _VERIFY(status)
           call this%regrid_handle%regrid(ptr3d,outPtr3d,rc=status)
           _VERIFY(status)
        end if

        if (allocated(ptr3d_inter)) deallocate(ptr3d_inter)

     end subroutine RegridScalar

     subroutine RegridVector(this,xName,yName,rc)
        class (MAPL_newCFIO), intent(inout) :: this
        character(len=*), intent(in) :: xName
        character(len=*), intent(in) :: yName
        integer, optional, intent(out) :: rc

        integer :: status

        type(ESMF_Field) :: xfield,xoutField
        type(ESMF_Field) :: yfield,youtField
        integer :: fieldRank
        real, pointer :: xptr3d(:,:,:),xoutptr3d(:,:,:)
        real, pointer :: xptr2d(:,:), xoutptr2d(:,:)
        real, allocatable, target :: xptr3d_inter(:,:,:)
        real, pointer :: yptr3d(:,:,:),youtptr3d(:,:,:)
        real, pointer :: yptr2d(:,:), youtptr2d(:,:)
        real, allocatable, target :: yptr3d_inter(:,:,:)

        call ESMF_FieldBundleGet(this%output_bundle,xName,field=xoutField,rc=status)
        _VERIFY(status)
        call ESMF_FieldBundleGet(this%output_bundle,yName,field=youtField,rc=status)
        _VERIFY(status)

        if (this%doVertRegrid) then
           call ESMF_FieldBundleGet(this%bundle,xName,field=xfield,rc=status)
           _VERIFY(status)
           call ESMF_FieldGet(xField,rank=fieldRank,rc=status)
           _VERIFY(status)
           if (fieldRank==3) then
              call ESMF_FieldGet(xfield,farrayPtr=xptr3d,rc=status)
              _VERIFY(status)
              allocate(xptr3d_inter(size(xptr3d,1),size(xptr3d,2),this%vdata%lm),stat=status)
              _VERIFY(status)
              if (this%vdata%regrid_type==VERTICAL_METHOD_SELECT) then
                 call this%vdata%regrid_select_level(xptr3d,xptr3d_inter,rc=status)
                 _VERIFY(status)
              else if (this%vdata%regrid_type==VERTICAL_METHOD_ETA2LEV) then
                 call this%vdata%regrid_eta_to_pressure(xptr3d,xptr3d_inter,rc=status)
                 _VERIFY(status)
              end if
              xptr3d => xptr3d_inter
           end if
           call ESMF_FieldBundleGet(this%bundle,yName,field=yfield,rc=status)
           _VERIFY(status)
           call ESMF_FieldGet(yField,rank=fieldRank,rc=status)
           _VERIFY(status)
           if (fieldRank==3) then
              call ESMF_FieldGet(yfield,farrayPtr=yptr3d,rc=status)
              _VERIFY(status)
              allocate(yptr3d_inter(size(yptr3d,1),size(yptr3d,2),this%vdata%lm),stat=status)
              _VERIFY(status)
              if (this%vdata%regrid_type==VERTICAL_METHOD_SELECT) then
                 call this%vdata%regrid_select_level(yptr3d,yptr3d_inter,rc=status)
                 _VERIFY(status)
              else if (this%vdata%regrid_type==VERTICAL_METHOD_ETA2LEV) then
                 call this%vdata%regrid_eta_to_pressure(yptr3d,yptr3d_inter,rc=status)
                 _VERIFY(status)
              end if
              yptr3d => yptr3d_inter
           end if
        else
           if (associated(xptr3d)) nullify(xptr3d)
           if (associated(yptr3d)) nullify(yptr3d)
        end if

        call ESMF_FieldBundleGet(this%bundle,xname,field=xfield,rc=status)
        _VERIFY(status)
        call ESMF_FieldBundleGet(this%bundle,yname,field=yfield,rc=status)
        _VERIFY(status)
        call ESMF_FieldGet(xfield,rank=fieldRank,rc=status)
        _VERIFY(status)
        if (fieldRank==2) then
           call ESMF_FieldGet(xfield,farrayPtr=xptr2d,rc=status)
           _VERIFY(status)
           call ESMF_FieldGet(xOutField,farrayPtr=xoutptr2d,rc=status)
           _VERIFY(status)

           call ESMF_FieldGet(yfield,farrayPtr=yptr2d,rc=status)
           _VERIFY(status)
           call ESMF_FieldGet(yOutField,farrayPtr=youtptr2d,rc=status)
           _VERIFY(status)

           call this%regrid_handle%regrid(xptr2d,yptr2d,xoutPtr2d,youtPtr2d,rc=status)
           _VERIFY(status)
        else if (fieldRank==3) then
           if (.not.associated(xptr3d)) then
              call ESMF_FieldGet(xfield,farrayPtr=xptr3d,rc=status)
              _VERIFY(status)
           end if
           call ESMF_FieldGet(xOutField,farrayPtr=xoutptr3d,rc=status)
           _VERIFY(status)

           if (.not.associated(yptr3d)) then
              call ESMF_FieldGet(yfield,farrayPtr=yptr3d,rc=status)
              _VERIFY(status)
           end if
           call ESMF_FieldGet(yOutField,farrayPtr=youtptr3d,rc=status)
           _VERIFY(status)

           call this%regrid_handle%regrid(xptr3d,yptr3d,xoutPtr3d,youtptr3d,rc=status)
           _VERIFY(status)
        end if

        if (allocated(xptr3d_inter)) deallocate(xptr3d_inter)
        if (allocated(yptr3d_inter)) deallocate(yptr3d_inter)

     end subroutine RegridVector

     subroutine bundlewait(this,rc)
        class (MAPL_newCFIO), intent(inout) :: this
        integer, optional, intent(out) :: rc

        integer :: status
        type(IntegerVectorIterator) :: req_iter
        integer :: request_id

        req_iter = this%data_req%begin()
        do while (req_iter /= this%data_req%end())
           request_id = req_iter%get()
           call o_ClientPtr%wait(request_id)
           call req_iter%next()
        end do
        call this%data_req%erase(this%data_req%begin(),this%data_req%end())
        deallocate(this%times)
        _RETURN(ESMF_SUCCESS)

     end subroutine bundlewait

  subroutine stage2DLatLon(this, fileName, rc)
     class (MAPL_newCFIO), intent(inout) :: this
     character(len=*), intent(in) :: fileName
     integer, optional, intent(out) :: rc

     integer :: request_id
     integer :: status
     logical :: isCubed
     real(REAL64), pointer :: ptr2d(:,:)
     type(ArrayReference) :: ref
     integer :: i1,in,j1,jn,tile
     integer :: global_dim(3)

     call MAPL_GridGet(this%output_grid,globalCellCountPerDim=global_dim,rc=status)
     _VERIFY(status)
     isCubed=.false.
     if (global_dim(1)*6 == global_dim(2)) isCubed=.true.

     if (isCubed) then

        call MAPL_Grid_interior(this%output_grid,i1,in,j1,jn)
        tile = j1/global_dim(1)
        call ESMF_GridGetCoord(this%output_grid, localDE=0, coordDim=1, &
        staggerloc=ESMF_STAGGERLOC_CENTER, &
        farrayPtr=ptr2d, rc=status)
        _VERIFY(STATUS)
        if (.not.allocated(this%lons)) allocate(this%lons(size(ptr2d,1),size(ptr2d,2)))
        !ref = ArrayReference(ptr2d)
        this%lons=ptr2d*MAPL_RADIANS_TO_DEGREES
        ref = ArrayReference(this%lons)
        request_id = o_ClientPtr%collective_stage_data(this%collection_id,trim(filename),'lons', &
                     ref,start=[i1,j1-tile*global_dim(1),tile+1], &
                     global_start=[1,1,1], global_count=[global_dim(1),global_dim(1),6])
        call this%data_req%push_back(request_id)
        call ESMF_GridGetCoord(this%output_grid, localDE=0, coordDim=2, &
        staggerloc=ESMF_STAGGERLOC_CENTER, &
        farrayPtr=ptr2d, rc=status)
        _VERIFY(STATUS)
        if (.not.allocated(this%lats)) allocate(this%lats(size(ptr2d,1),size(ptr2d,2)))
        !ref = ArrayReference(ptr2d)
        this%lats=ptr2d*MAPL_RADIANS_TO_DEGREES
        ref = ArrayReference(this%lats)
        request_id = o_ClientPtr%collective_stage_data(this%collection_id,trim(filename),'lats', &
                     ref,start=[i1,j1-tile*global_dim(1),tile+1], &
                     global_start=[1,1,1], global_count=[global_dim(1),global_dim(1),6])
        call this%data_req%push_back(request_id)
     end if

  end subroutine stage2DLatLon
  
  subroutine stageData(this, field, fileName, tIndex, rc) 
     class (MAPL_newCFIO), intent(inout) :: this
     type(ESMF_Field), intent(inout) :: field
     character(len=*), intent(in) :: fileName
     integer, intent(in) :: tIndex
     integer, optional, intent(out) :: rc

     integer :: request_id
     integer :: status
     integer :: fieldRank
     character(len=ESMF_MAXSTR) :: fieldName
     logical :: isCubed
     real, pointer :: ptr3d(:,:,:)
     real, pointer :: ptr2d(:,:)
     type(ArrayReference) :: ref
     integer :: i1,in,j1,jn,tile
     integer :: global_dim(3)
     type(c_ptr) :: cptr
     real, pointer :: ptr_ref_3d(:,:,:,:,:)

     call MAPL_GridGet(this%output_grid,globalCellCountPerDim=global_dim,rc=status)
     _VERIFY(status)
     isCubed=.false.
     if (global_dim(1)*6 == global_dim(2)) isCubed=.true.
     call MAPL_Grid_interior(this%output_grid,i1,in,j1,jn)
     call ESMF_FieldGet(field,rank=fieldRank,name=fieldName,rc=status)
     _VERIFY(status)

     if (isCubed) then
        tile = j1/global_dim(1)
        call ESMF_FieldGet(field,rank=fieldRank,name=fieldName,rc=status)
        _VERIFY(status)
        if (fieldRank==2) then
           call ESMF_FieldGet(field,farrayPtr=ptr2d,rc=status)
           _VERIFY(status)
           if (this%nbits < 24) then
              call pFIO_DownBit(ptr2d,ptr2d,this%nbits,undef=MAPL_undef,rc=status)
              _VERIFY(status)
           end if
           ref = ArrayReference(ptr2d)
           request_id = o_ClientPtr%collective_stage_data(this%collection_id,trim(filename),trim(fieldName), &
                        ref,start=[i1,j1-tile*global_dim(1),tile+1,1], &
                        global_start=[1,1,1,tindex], global_count=[global_dim(1),global_dim(1),6,1])
        else if (fieldRank==3) then
           call ESMF_FieldGet(field,farrayPtr=ptr3d,rc=status)
           _VERIFY(status)
           if (this%nbits < 24) then
              call pFIO_DownBit(ptr3d,ptr3d,this%nbits,undef=MAPL_undef,rc=status)
              _VERIFY(status)
           end if
           cptr = c_loc(ptr3d)
           call C_F_pointer(cptr,ptr_ref_3d,[size(ptr3d,1),size(ptr3d,2),1,size(ptr3d,3),1])
           ref = ArrayReference(ptr_ref_3d)
           request_id = o_ClientPtr%collective_stage_data(this%collection_id,trim(filename),trim(fieldName), &
                        ref,start=[i1,j1-tile*global_dim(1),tile+1,1,1], &
                        global_start=[1,1,1,1,tindex], global_count=[global_dim(1),global_dim(1),6,size(ptr3d,3),1])
        end if
     else
        if (fieldRank==2) then
           call ESMF_FieldGet(Field,farrayPtr=ptr2d,rc=status)
           _VERIFY(status)
           if (this%nbits < 24) then
              call pFIO_DownBit(ptr2d,ptr2d,this%nbits,undef=MAPL_undef,rc=status)
              _VERIFY(status)
           end if
           ref = ArrayReference(Ptr2D)
           request_id = o_ClientPtr%collective_stage_data(this%collection_id,trim(filename),trim(fieldName), &
                        ref,start=[i1,j1,1], &
                        global_start=[1,1,tindex], global_count=[global_dim(1),global_dim(2),1])
         else if (fieldRank==3) then
            call ESMF_FieldGet(field,farrayPtr=ptr3d,rc=status)
            _VERIFY(status)
           if (this%nbits < 24) then
              call pFIO_DownBit(ptr3d,ptr3d,this%nbits,undef=MAPL_undef,rc=status)
              _VERIFY(status)
           end if
            ref = ArrayReference(Ptr3D)
            request_id = o_ClientPtr%collective_stage_data(this%collection_id,trim(filename),trim(fieldName), &
                      ref,start=[i1,j1,1,1], &
                      global_start=[1,1,1,tindex], global_count=[global_dim(1),global_dim(2),size(ptr3d,3),1])
         end if
     end if
     call this%data_req%push_back(request_id)

  end subroutine stageData

end module MAPL_newCFIOMod
