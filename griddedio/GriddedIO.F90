#include "MAPL_Generic.h"

module MAPL_GriddedIOMod
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
  implicit none

  private

  type, public :: MAPL_GriddedIO
     type(FileMetaData), allocatable :: metadata
     type(fileMetadataUtils), pointer :: current_file_metadata
     integer :: write_collection_id
     integer :: read_collection_id
     integer :: metadata_collection_id
     class (AbstractRegridder), pointer :: regrid_handle => null()
     type(ESMF_Grid) :: output_grid
     logical :: doVertRegrid = .false.
     type(ESMF_FieldBundle) :: output_bundle
     type(ESMF_FieldBundle) :: input_bundle
     type(ESMF_Time) :: startTime
     integer :: regrid_method = REGRID_METHOD_BILINEAR
     integer :: nbits_to_keep = MAPL_NBITS_NOT_SET
     real, allocatable :: lons(:,:),lats(:,:)
     real, allocatable :: corner_lons(:,:),corner_lats(:,:)
     real, allocatable :: times(:)
     type(TimeData) :: timeInfo
     type(VerticalData) :: vdata
     type(GriddedIOitemVector) :: items
     integer :: deflateLevel = 0
     integer :: quantizeAlgorithm = MAPL_NOQUANTIZE
     integer :: quantizeLevel = 0
     integer :: zstandardLevel = 0
     integer, allocatable :: chunking(:)
     logical :: itemOrderAlphabetical = .true.
     integer :: fraction
     integer :: regrid_hints = 0
     contains
        procedure :: CreateFileMetaData
        procedure :: CreateVariable
        procedure :: CreateQuantizationInfo
        procedure :: modifyTime
        procedure :: modifyTimeIncrement
        procedure :: bundlePost
        procedure :: stageData
        procedure :: stage2DLatLon
        procedure :: regridScalar
        procedure :: regridVector
        procedure :: set_param
        procedure :: set_default_chunking
        procedure :: check_chunking
        procedure :: alphabatize_variables
        procedure :: request_data_from_file
        procedure :: process_data_from_file
        procedure :: swap_undef_value
        procedure :: destroy
  end type MAPL_GriddedIO

  interface MAPL_GriddedIO
     module procedure new_MAPL_GriddedIO
  end interface MAPL_GriddedIO

  contains

     function new_MAPL_GriddedIO(metadata,input_bundle,output_bundle,write_collection_id,read_collection_id, &
             metadata_collection_id,regrid_method,fraction,items,rc) result(GriddedIO)
        type(MAPL_GriddedIO) :: GriddedIO
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

        if (present(metadata)) GriddedIO%metadata=metadata
        if (present(input_bundle)) GriddedIO%input_bundle=input_bundle
        if (present(output_bundle)) GriddedIO%output_bundle=output_bundle
        if (present(regrid_method)) GriddedIO%regrid_method=regrid_method
        if (present(write_collection_id)) GriddedIO%write_collection_id=write_collection_id
        if (present(read_collection_id)) GriddedIO%read_collection_id=read_collection_id
        if (present(metadata_collection_id)) GriddedIO%metadata_collection_id=metadata_collection_id
        if (present(items)) GriddedIO%items=items
        if (present(fraction)) GriddedIO%fraction=fraction
        _RETURN(ESMF_SUCCESS)
     end function new_MAPL_GriddedIO

     subroutine CreateFileMetaData(this,items,bundle,timeInfo,vdata,ogrid,global_attributes,rc)
        class (MAPL_GriddedIO), target, intent(inout) :: this
        type(GriddedIOitemVector), target, intent(inout) :: items
        type(ESMF_FieldBundle), intent(inout) :: bundle
        type(TimeData), intent(inout) :: timeInfo
        type(VerticalData), intent(inout), optional :: vdata
        type (ESMF_Grid), intent(inout), pointer, optional :: ogrid
        type(StringStringMap), target, intent(in), optional :: global_attributes
        integer, intent(out), optional :: rc

        type(ESMF_Grid) :: input_grid
        class (AbstractGridFactory), pointer :: factory

        type(GriddedIOitemVectorIterator) :: iter
        type(GriddedIOitem), pointer :: item
        type(stringVector) :: order
        integer :: metadataVarsSize
        type(StringStringMapIterator) :: s_iter
        character(len=:), pointer :: attr_name, attr_val
        class(Variable), pointer :: coord_var
        integer :: status

        if ( allocated (this%metadata) ) deallocate(this%metadata)
        allocate(this%metadata)

        call MAPL_FieldBundleDestroy(this%output_bundle, _RC)

        this%items = items
        this%input_bundle = bundle
        this%output_bundle = ESMF_FieldBundleCreate(rc=status)
        _VERIFY(status)
        this%timeInfo = timeInfo
        call ESMF_FieldBundleGet(this%input_bundle,grid=input_grid,rc=status)
        _VERIFY(status)
        if (present(ogrid)) then
           this%output_grid=ogrid
        else
           call ESMF_FieldBundleGet(this%input_bundle,grid=this%output_grid,rc=status)
           _VERIFY(status)
        end if

        this%regrid_handle => new_regridder_manager%make_regridder(input_grid,this%output_grid,this%regrid_method,hints=this%regrid_hints,rc=status)
        _VERIFY(status)


        ! We get the regrid_method here because in the case of Identity, we set it to
        ! REGRID_METHOD_IDENTITY in the regridder constructor if identity. Now we need
        ! to change the regrid_method in the GriddedIO object to be the same as the
        ! the regridder object.
        this%regrid_method = this%regrid_handle%get_regrid_method()

        call ESMF_FieldBundleSet(this%output_bundle,grid=this%output_grid,rc=status)
        _VERIFY(status)
        factory => get_factory(this%output_grid,rc=status)
        _VERIFY(status)
        call factory%append_metadata(this%metadata)
        coord_var => this%metadata%get_variable('lons')
        if (associated(coord_var)) call coord_var%set_deflation(this%deflateLevel)
        coord_var => this%metadata%get_variable('lats')
        if (associated(coord_var)) call coord_var%set_deflation(this%deflateLevel)
        coord_var => this%metadata%get_variable('corner_lons')
        if (associated(coord_var)) call coord_var%set_deflation(this%deflateLevel)
        coord_var => this%metadata%get_variable('corner_lats')
        if (associated(coord_var)) call coord_var%set_deflation(this%deflateLevel)


        if (present(vdata)) then
           this%vdata=vdata
        else
           this%vdata=VerticalData(rc=status)
           _VERIFY(status)
        end if
        call this%vdata%append_vertical_metadata(this%metadata,this%input_bundle,rc=status)
        _VERIFY(status)
        this%doVertRegrid = (this%vdata%regrid_type /= VERTICAL_METHOD_NONE)
        if (this%vdata%regrid_type == VERTICAL_METHOD_ETA2LEV) call this%vdata%get_interpolating_variable(this%input_bundle,rc=status)
        _VERIFY(status)

        call this%timeInfo%add_time_to_metadata(this%metadata,rc=status)
        _VERIFY(status)

        if (.not.allocated(this%chunking)) then
           call this%set_default_chunking(rc=status)
           _VERIFY(status)
        else
           call this%check_chunking(this%vdata%lm,_RC)
        end if

        order = this%metadata%get_order(rc=status)
        _VERIFY(status)
        metadataVarsSize = order%size()

        ! If quantize algorithm is set, create a quantization_info variable
        if (this%quantizeAlgorithm /= MAPL_NOQUANTIZE) then
           call this%CreateQuantizationInfo(_RC)
        end if

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


      subroutine destroy(this, rc)
        class (MAPL_GriddedIO), intent(inout) :: this
        integer, intent(out), optional :: rc
        if(allocated(this%chunking)) deallocate(this%chunking)
        _RETURN(_SUCCESS)
      end subroutine destroy


     subroutine set_param(this,deflation,quantize_algorithm,quantize_level,zstandard_level,chunking,nbits_to_keep,regrid_method,itemOrder,write_collection_id,regrid_hints,rc)
        class (MAPL_GriddedIO), intent(inout) :: this
        integer, optional, intent(in) :: deflation
        integer, optional, intent(in) :: quantize_algorithm
        integer, optional, intent(in) :: quantize_level
        integer, optional, intent(in) :: zstandard_level
        integer, optional, intent(in) :: chunking(:)
        integer, optional, intent(in) :: nbits_to_keep
        integer, optional, intent(in) :: regrid_method
        logical, optional, intent(in) :: itemOrder
        integer, optional, intent(in) :: write_collection_id
        integer, optional, intent(in) :: regrid_hints
        integer, optional, intent(out) :: rc

        integer :: status

        if (present(regrid_method)) this%regrid_method=regrid_method
        if (present(nbits_to_keep)) this%nbits_to_keep=nbits_to_keep
        if (present(deflation)) this%deflateLevel = deflation
        if (present(quantize_algorithm)) this%quantizeAlgorithm = quantize_algorithm
        if (present(quantize_level)) this%quantizeLevel = quantize_level
        if (present(zstandard_level)) this%zstandardLevel = zstandard_level
        if (present(chunking)) then
           allocate(this%chunking,source=chunking,stat=status)
           _VERIFY(status)
        end if
        if (present(itemOrder)) this%itemOrderAlphabetical = itemOrder
        if (present(write_collection_id)) this%write_collection_id=write_collection_id
        if (present(regrid_hints)) this%regrid_hints = regrid_hints
        _RETURN(ESMF_SUCCESS)

     end subroutine set_param

     subroutine set_default_chunking(this,rc)
        class (MAPL_GriddedIO), intent(inout) :: this
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

     subroutine check_chunking(this,lev_size,rc)
        class (MAPL_GriddedIO), intent(inout) :: this
        integer, intent(in) :: lev_size
        integer, optional, intent(out) :: rc

        integer ::  global_dim(3)
        integer :: status
        character(len=5) :: c1,c2

        call MAPL_GridGet(this%output_grid,globalCellCountPerDim=global_dim,rc=status)
        _VERIFY(status)
        if (global_dim(1)*6 == global_dim(2)) then
           write(c2,'(I5)')global_dim(1)
           write(c1,'(I5)')this%chunking(1)
           _ASSERT(this%chunking(1) <= global_dim(1), "Chunk for Xdim "//c1//" must be less than or equal to "//c2)
           write(c1,'(I5)')this%chunking(2)
           _ASSERT(this%chunking(2) <= global_dim(1), "Chunk for Ydim "//c1//" must be less than or equal to "//c2)
           _ASSERT(this%chunking(3) <= 6, "Chunksize for face dimension must be 6 or less")
           if (lev_size > 0) then
              write(c2,'(I5)')lev_size
              write(c1,'(I5)')this%chunking(4)
              _ASSERT(this%chunking(4) <= lev_size, "Chunk for level size "//c1//" must be less than or equal to "//c2)
           end if
           _ASSERT(this%chunking(5) == 1, "Time must have chunk size of 1")
        else
           write(c2,'(I5)')global_dim(1)
           write(c1,'(I5)')this%chunking(1)
           _ASSERT(this%chunking(1) <= global_dim(1), "Chunk for lon "//c1//" must be less than or equal to "//c2)
           write(c2,'(I5)')global_dim(2)
           write(c1,'(I5)')this%chunking(2)
           _ASSERT(this%chunking(2) <= global_dim(2), "Chunk for lat "//c1//" must be less than or equal to "//c2)
           if (lev_size > 0) then
              write(c2,'(I5)')lev_size
              write(c1,'(I5)')this%chunking(3)
              _ASSERT(this%chunking(3) <= lev_size, "Chunk for level size "//c1//" must be less than or equal to "//c2)
           end if
           _ASSERT(this%chunking(4) == 1, "Time must have chunk size of 1")
        endif
        _RETURN(ESMF_SUCCESS)

     end subroutine check_chunking

     subroutine CreateVariable(this,itemName,rc)
        class (MAPL_GriddedIO), intent(inout) :: this
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

        call ESMF_FieldBundleGet(this%input_bundle,itemName,field=field,rc=status)
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
        if (this%timeInfo%is_initialized) then
           if (fieldRank==2) then
              vdims=grid_dims//",time"
           else if (fieldRank==3) then
              vdims=grid_dims//",lev,time"
           else
              _FAIL( 'Unsupported field rank')
           end if
        else
           if (fieldRank==2) then
              vdims=grid_dims
           else if (fieldRank==3) then
              vdims=grid_dims//",lev"
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
        call v%add_attribute('vmax',MAPL_UNDEF)
        call v%add_attribute('vmin',-MAPL_UNDEF)
        call v%add_attribute('scale_factor',1.0)
        call v%add_attribute('add_offset',0.0)
        call v%add_attribute('_FillValue',MAPL_UNDEF)
        call v%add_attribute('valid_range',(/-MAPL_UNDEF,MAPL_UNDEF/))
        ! Weird workaround for NAG 7.1.113
#ifdef __NAG_COMPILER_RELEASE
        associate (s => regrid_method_int_to_string(this%regrid_method))
          call v%add_attribute('regrid_method', s)
        end associate
#else
        call v%add_attribute('regrid_method', regrid_method_int_to_string(this%regrid_method))
#endif
        ! The CF Convention will soon support quantization. This requires three new attributes
        ! if enabled:
        ! 1. quantization --> Will point to a quantization_info container with the quantization algorithm
        !                     (NOTE: this will need to be programmatic when per-variable quantization is enabled)
        ! 2a. quantization_nsb --> Number of significant bits (only for bitround)
        ! 2b. quantization_nsd --> Number of significant digits (only for bitgroom and granular_bitround)
        ! 3. quantization_maximum_relative_error --> Maximum relative error (defined as 2^(-nsb) for bitround, and UNDEFINED? for bitgroom and granular_bitround)

        ! Bitround
        if (this%quantizeAlgorithm == MAPL_QUANTIZE_BITROUND) then
           call v%add_attribute('quantization', 'quantization_info')
           call v%add_attribute('quantization_nsb', this%quantizeLevel)
           call v%add_attribute('quantization_maximum_relative_error', 0.5 * 2.0**(-this%quantizeLevel))
        end if
        ! granular_bitround and bitgroom
        if (this%quantizeAlgorithm == MAPL_QUANTIZE_BITGROOM .or. this%quantizeAlgorithm == MAPL_QUANTIZE_GRANULAR_BITROUND) then
           call v%add_attribute('quantization', 'quantization_info')
           call v%add_attribute('quantization_nsd', this%quantizeLevel)
           ! Per czender, these have maximum_absolute_error. We use the calculate_mae function below
           ! which replicates a table in doi 10.5194/gmd-12-4099-2019
           ! NOTE: This might not be the right formula. As the CF Convention draft is updated,
           ! we will update this code.
           call v%add_attribute('quantization_maximum_absolute_error', calculate_mae(this%quantizeLevel))
        end if

        call factory%append_variable_metadata(v)
        call this%metadata%add_variable(trim(varName),v,rc=status)
        _VERIFY(status)
        ! finally make a new field if neccessary
        if (this%doVertRegrid .and. (fieldRank ==3) ) then
           newField = MAPL_FieldCreate(field,this%output_grid,lm=this%vData%lm,rc=status)
           _VERIFY(status)
           call MAPL_FieldBundleAdd(this%output_bundle,newField,rc=status)
           _VERIFY(status)
        else
           newField = MAPL_FieldCreate(field,this%output_grid,rc=status)
           _VERIFY(status)
           call MAPL_FieldBundleAdd(this%output_bundle,newField,rc=status)
           _VERIFY(status)
        end if
        _RETURN(_SUCCESS)

     end subroutine CreateVariable

     function calculate_mae(nsd) result(mae)

        ! This function is based on Table 3 of doi 10.5194/gmd-12-4099-2019
        ! The algorithm is weird, but it does duplicate the table

        integer, intent(in) :: nsd
        real(kind=REAL32) :: mae
        real(kind=REAL32) :: mae_base
        integer :: correction

        mae_base = 4.0 * (1.0/16.0)**floor(real(nsd)/2.0) * (1.0/8.0)**ceiling(real(nsd)/2.0)

        correction = 1
        if ( (nsd > 2 .and. mod(nsd, 2) == 0) .or. nsd == 7 ) then
           correction = 2
        end if

        mae = mae_base * correction
     end function calculate_mae

     subroutine CreateQuantizationInfo(this,rc)
        class (MAPL_GriddedIO), intent(inout) :: this
        integer, optional, intent(out) :: rc

        integer :: status

        class (AbstractGridFactory), pointer :: factory
        character(len=:), allocatable :: varName, netcdf_version
        type(Variable) :: v

        factory => get_factory(this%output_grid,_RC)

        v = Variable(type=PFIO_CHAR)

        ! In the future when we can do per variable quantization, we will need
        ! to do things like quantization_info1, quantization_info2, etc.
        ! For now, we will just use quantization_info as it is per collection
        varName = "quantization_info"

        ! We need to convert the quantization algorithm to a string
        select case (this%quantizeAlgorithm)
        case (MAPL_QUANTIZE_BITGROOM)
           call v%add_attribute('algorithm', 'bitgroom')
        case (MAPL_QUANTIZE_BITROUND)
           call v%add_attribute('algorithm', 'bitround')
        case (MAPL_QUANTIZE_GRANULAR_BITROUND)
           call v%add_attribute('algorithm', 'granular_bitround')
        case default
           _FAIL('Unknown quantization algorithm')
        end select

        ! Next add the implementation details
        ! 3. implementation: This property contains free-form text
        ! that concisely conveys the algorithm provenance, including the
        ! name of the library or client that performed the quantization,
        ! the software version, and the name of the author(s) if deemed
        ! relevant.
        !
        ! In the current case, all algorithms are from libnetcdf
        ! we make a string using nf90_inq_libvers()

        netcdf_version = 'libnetcdf ' // nf90_inq_libvers()
        call v%add_attribute('implementation', netcdf_version)

        ! NOTE: In the future if we add the MAPL bit-shaving
        ! to use the quantization parts of the code, it will
        ! need a different implementation string

        call factory%append_variable_metadata(v)
        call this%metadata%add_variable(trim(varName),v,_RC)

        _RETURN(_SUCCESS)

     end subroutine CreateQuantizationInfo

     subroutine modifyTime(this, oClients, rc)
        class(MAPL_GriddedIO), intent(inout) :: this
        type (ClientManager), optional, intent(inout) :: oClients
        integer, optional, intent(out) :: rc

        type(Variable) :: v
        type(StringVariableMap) :: var_map
        integer :: status

        if (this%timeInfo%is_initialized) then
           v = this%timeInfo%define_time_variable(_RC)
           call this%metadata%modify_variable('time',v,rc=status)
           _VERIFY(status)
           if (present(oClients)) then
              call var_map%insert('time',v)
              call oClients%modify_metadata(this%write_collection_id, var_map=var_map, rc=status)
              _VERIFY(status)
           end if
        else
           _FAIL("Time was not initialized for the GriddedIO class instance")
        end if
        _RETURN(ESMF_SUCCESS)

     end subroutine modifyTime

     subroutine modifyTimeIncrement(this, frequency, rc)
        class(MAPL_GriddedIO), intent(inout) :: this
        integer, intent(in) :: frequency
        integer, optional, intent(out) :: rc

        integer :: status

        if (this%timeInfo%is_initialized) then
           call this%timeInfo%setFrequency(frequency, rc=status)
           _VERIFY(status)
        else
           _FAIL("Time was not initialized for the GriddedIO class instance")
        end if

        _RETURN(ESMF_SUCCESS)

      end subroutine modifyTimeIncrement

     subroutine bundlepost(this,filename,oClients,rc)
        class (MAPL_GriddedIO), target, intent(inout) :: this
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

        if (this%vdata%regrid_type==VERTICAL_METHOD_ETA2LEV) then
           call this%vdata%setup_eta_to_pressure(regrid_handle=this%regrid_handle,output_grid=this%output_grid,rc=status)
           _VERIFY(status)
        end if

        iter = this%items%begin()
        do while (iter /= this%items%end())
           item => iter%get()
           if (item%itemType == ItemTypeScalar) then
              call this%RegridScalar(item%xname,rc=status)
              _VERIFY(status)
              call ESMF_FieldBundleGet(this%output_bundle,item%xname,field=outField,rc=status)
              _VERIFY(status)
              if (this%vdata%regrid_type==VERTICAL_METHOD_ETA2LEV .and. (this%vdata%extrap_below_surf .eqv. .false.)) then
                 call this%vdata%correct_topo(outField,rc=status)
                 _VERIFY(status)
              end if
              call this%stageData(outField,filename,tIndex, oClients=oClients,rc=status)
              _VERIFY(status)
           else if (item%itemType == ItemTypeVector) then
              call this%RegridVector(item%xname,item%yname,rc=status)
              _VERIFY(status)
              call ESMF_FieldBundleGet(this%output_bundle,item%xname,field=outField,rc=status)
              _VERIFY(status)
              if (this%vdata%regrid_type==VERTICAL_METHOD_ETA2LEV) then
                 call this%vdata%correct_topo(outField,rc=status)
                 _VERIFY(status)
              end if
              call this%stageData(outField,filename,tIndex,oClients=oClients,rc=status)
              _VERIFY(status)
              call ESMF_FieldBundleGet(this%output_bundle,item%yname,field=outField,rc=status)
              _VERIFY(status)
              if (this%vdata%regrid_type==VERTICAL_METHOD_ETA2LEV) then
                 call this%vdata%correct_topo(outField,rc=status)
                 _VERIFY(status)
              end if
              call this%stageData(outField,filename,tIndex,oClients=oClients,rc=status)
              _VERIFY(status)
           end if
           call iter%next()
        enddo

        _RETURN(ESMF_SUCCESS)

     end subroutine bundlepost

     subroutine RegridScalar(this,itemName,rc)
        class (MAPL_GriddedIO), intent(inout) :: this
        character(len=*), intent(in) :: itemName
        integer, optional, intent(out) :: rc

        integer :: status

        type(ESMF_Field) :: field,outField
        integer :: fieldRank
        real, pointer :: ptr3d(:,:,:),outptr3d(:,:,:)
        real, pointer :: ptr2d(:,:), outptr2d(:,:)
        real, allocatable, target :: ptr3d_inter(:,:,:)
        type(ESMF_Grid) :: gridIn,gridOut
        logical :: hasDE_in, hasDE_out, isPresent
        character(len=ESMF_MAXSTR) :: long_name

        ptr3d => null()

        call ESMF_FieldBundleGet(this%output_bundle,itemName,field=outField,rc=status)
        _VERIFY(status)
        long_name = 'unknown'
        call ESMF_AttributeGet(outField, name="LONG_NAME", isPresent=isPresent, _RC)
        if ( isPresent ) then
           call ESMF_AttributeGet(outField, name="LONG_NAME",value=long_name, _RC)
        endif
        call ESMF_FieldBundleGet(this%input_bundle,grid=gridIn,rc=status)
        _VERIFY(status)
        call ESMF_FieldBundleGet(this%output_bundle,grid=gridOut,rc=status)
        _VERIFY(status)
        hasDE_in = MAPL_GridHasDE(gridIn,rc=status)
        _VERIFY(status)
        hasDE_out = MAPL_GridHasDE(gridOut,rc=status)
        _VERIFY(status)

        if (this%doVertRegrid) then
           call ESMF_FieldBundleGet(this%input_bundle,itemName,field=field,rc=status)
           _VERIFY(status)
           call ESMF_FieldGet(Field,rank=fieldRank,rc=status)
           _VERIFY(status)
           if (fieldRank==3) then
              if (hasDE_in) then
                 call ESMF_FieldGet(field,farrayPtr=ptr3d,rc=status)
                 _VERIFY(status)
              else
                 allocate(ptr3d(0,0,0))
              end if
              allocate(ptr3d_inter(size(ptr3d,1),size(ptr3d,2),this%vdata%lm),stat=status)
              _VERIFY(status)
              if (this%vdata%regrid_type==VERTICAL_METHOD_SELECT) then
                 call this%vdata%regrid_select_level(ptr3d,ptr3d_inter,rc=status)
                 _VERIFY(status)
              else if (this%vdata%regrid_type==VERTICAL_METHOD_ETA2LEV) then
                 call this%vdata%regrid_eta_to_pressure(ptr3d,ptr3d_inter,var_name=long_name,rc=status)
                 _VERIFY(status)
              else if (this%vdata%regrid_type==VERTICAL_METHOD_FLIP) then
                 call this%vdata%flip_levels(ptr3d,ptr3d_inter,rc=status)
                 _VERIFY(status)
              end if
              ptr3d => ptr3d_inter
           end if
        else
           if (associated(ptr3d)) nullify(ptr3d)
        end if

        call ESMF_FieldBundleGet(this%input_bundle,itemName,field=field,rc=status)
        _VERIFY(status)
        call ESMF_FieldGet(field,rank=fieldRank,rc=status)
        _VERIFY(status)
        if (fieldRank==2) then
           if (hasDE_in) then
              call MAPL_FieldGetPointer(field,ptr2d,rc=status)
              _VERIFY(status)
           else
              allocate(ptr2d(0,0))
           end if
           if (hasDE_out) then
              call MAPL_FieldGetPointer(OutField,outptr2d,rc=status)
              _VERIFY(status)
           else
              allocate(outptr2d(0,0))
           end if
           if (gridIn==gridOut) then
              outPtr2d=ptr2d
           else
              if (this%regrid_method==REGRID_METHOD_FRACTION) ptr2d=ptr2d-this%fraction
              call this%regrid_handle%regrid(ptr2d,outPtr2d,rc=status)
              _VERIFY(status)
           end if
        else if (fieldRank==3) then
           if (.not.associated(ptr3d)) then
              if (hasDE_in) then
                 call ESMF_FieldGet(field,farrayPtr=ptr3d,rc=status)
                 _VERIFY(status)
              else
                 allocate(ptr3d(0,0,0))
              end if
           end if
           if (hasDE_out) then
              call MAPL_FieldGetPointer(OutField,outptr3d,rc=status)
              _VERIFY(status)
           else
              allocate(outptr3d(0,0,0))
           end if
           if (gridIn==gridOut) then
              outPtr3d=Ptr3d
           else
              if (this%regrid_method==REGRID_METHOD_FRACTION) ptr3d=ptr3d-this%fraction
              call this%regrid_handle%regrid(ptr3d,outPtr3d,rc=status)
              _VERIFY(status)
           end if
        else
           _FAIL('rank not supported')
        end if

        if (allocated(ptr3d_inter)) deallocate(ptr3d_inter)
        _RETURN(_SUCCESS)

     end subroutine RegridScalar

     subroutine RegridVector(this,xName,yName,rc)
        class (MAPL_GriddedIO), intent(inout) :: this
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
        type(ESMF_Grid) :: gridIn, gridOut
        logical :: hasDE_in, hasDE_out, isPresent
        character(len=ESMF_MAXSTR) :: long_name

        call ESMF_FieldBundleGet(this%output_bundle,xName,field=xoutField,rc=status)
        _VERIFY(status)
        long_name = 'unknown'
        call ESMF_AttributeGet(xoutField, name="LONG_NAME", isPresent=isPresent, _RC)
        if ( isPresent ) then
           call ESMF_AttributeGet(xoutField, name="LONG_NAME",value=long_name, _RC)
        endif
        call ESMF_FieldBundleGet(this%output_bundle,yName,field=youtField,rc=status)
        _VERIFY(status)
        call ESMF_FieldBundleGet(this%input_bundle,grid=gridIn,rc=status)
        _VERIFY(status)
        call ESMF_FieldBundleGet(this%output_bundle,grid=gridOut,rc=status)
        _VERIFY(status)
        hasDE_in = MAPL_GridHasDE(gridIn,rc=status)
        _VERIFY(status)
        hasDE_out = MAPL_GridHasDE(gridOut,rc=status)
        _VERIFY(status)

        if (this%doVertRegrid) then
           call ESMF_FieldBundleGet(this%input_bundle,xName,field=xfield,rc=status)
           _VERIFY(status)
           call ESMF_FieldGet(xField,rank=fieldRank,rc=status)
           _VERIFY(status)
           if (fieldRank==3) then
              if (hasDE_in) then
                 call ESMF_FieldGet(xfield,farrayPtr=xptr3d,rc=status)
                 _VERIFY(status)
              else
                 allocate(xptr3d(0,0,0))
              end if
              allocate(xptr3d_inter(size(xptr3d,1),size(xptr3d,2),this%vdata%lm),stat=status)
              _VERIFY(status)
              if (this%vdata%regrid_type==VERTICAL_METHOD_SELECT) then
                 call this%vdata%regrid_select_level(xptr3d,xptr3d_inter,rc=status)
                 _VERIFY(status)
              else if (this%vdata%regrid_type==VERTICAL_METHOD_ETA2LEV) then
                 call this%vdata%regrid_eta_to_pressure(xptr3d,xptr3d_inter,var_name=long_name,rc=status)
                 _VERIFY(status)
              else if (this%vdata%regrid_type==VERTICAL_METHOD_FLIP) then
                 call this%vdata%flip_levels(xptr3d,xptr3d_inter,rc=status)
                 _VERIFY(status)
              end if
              xptr3d => xptr3d_inter
           end if
           call ESMF_FieldBundleGet(this%input_bundle,yName,field=yfield,rc=status)
           _VERIFY(status)
           call ESMF_FieldGet(yField,rank=fieldRank,rc=status)
           _VERIFY(status)
           if (fieldRank==3) then
              if (hasDE_in) then
                 call ESMF_FieldGet(yfield,farrayPtr=yptr3d,rc=status)
                 _VERIFY(status)
              else
                 allocate(yptr3d(0,0,0))
              end if
              allocate(yptr3d_inter(size(yptr3d,1),size(yptr3d,2),this%vdata%lm),stat=status)
              _VERIFY(status)
              if (this%vdata%regrid_type==VERTICAL_METHOD_SELECT) then
                 call this%vdata%regrid_select_level(yptr3d,yptr3d_inter,rc=status)
                 _VERIFY(status)
              else if (this%vdata%regrid_type==VERTICAL_METHOD_ETA2LEV) then
                 call this%vdata%regrid_eta_to_pressure(yptr3d,yptr3d_inter,var_name=long_name,rc=status)
                 _VERIFY(status)
              else if (this%vdata%regrid_type==VERTICAL_METHOD_FLIP) then
                 call this%vdata%flip_levels(yptr3d,yptr3d_inter,rc=status)
                 _VERIFY(status)
              end if
              yptr3d => yptr3d_inter
           end if
        else
           nullify(xptr3d)
           nullify(yptr3d)
        end if

        call ESMF_FieldBundleGet(this%input_bundle,xname,field=xfield,rc=status)
        _VERIFY(status)
        call ESMF_FieldBundleGet(this%input_bundle,yname,field=yfield,rc=status)
        _VERIFY(status)
        call ESMF_FieldGet(xfield,rank=fieldRank,rc=status)
        _VERIFY(status)
        if (fieldRank==2) then
           if (hasDE_in) then
              call MAPL_FieldGetPointer(xfield,xptr2d,rc=status)
              _VERIFY(status)
              call MAPL_FieldGetPointer(yfield,yptr2d,rc=status)
              _VERIFY(status)
           else
              allocate(xptr2d(0,0))
              allocate(yptr2d(0,0))
           end if

           if (hasDE_in) then
              call MAPL_FieldGetPointer(xOutField,xoutptr2d,rc=status)
              _VERIFY(status)
              call MAPL_FieldGetPointer(yOutField,youtptr2d,rc=status)
              _VERIFY(status)
           else
              allocate(xoutptr2d(0,0))
              allocate(youtptr2d(0,0))
           end if


           if (gridIn==gridOut) then
              xoutPtr2d=xptr2d
              youtPtr2d=yptr2d
           else
              call this%regrid_handle%regrid(xptr2d,yptr2d,xoutPtr2d,youtPtr2d,rc=status)
              _VERIFY(status)
           end if
        else if (fieldRank==3) then
           if (.not.associated(xptr3d)) then
              if (hasDE_in) then
                 call MAPL_FieldGetPointer(xfield,xptr3d,rc=status)
                 _VERIFY(status)
              else
                 allocate(xptr3d(0,0,0))
              end if
           end if
           if (.not.associated(yptr3d)) then
              if (hasDE_in) then
                 call MAPL_FieldGetPointer(yfield,yptr3d,rc=status)
                 _VERIFY(status)
              else
                 allocate(yptr3d(0,0,0))
              end if
           end if

           if (hasDE_out) then
              call MAPL_FieldGetPointer(xOutField,xoutptr3d,rc=status)
              _VERIFY(status)
              call MAPL_FieldGetPointer(yOutField,youtptr3d,rc=status)
              _VERIFY(status)
           else
              allocate(xoutptr3d(0,0,0))
              allocate(youtptr3d(0,0,0))
           end if

           if (gridIn==gridOut) then
              xoutPtr3d=xptr3d
              youtPtr3d=yptr3d
           else
              call this%regrid_handle%regrid(xptr3d,yptr3d,xoutPtr3d,youtPtr3d,rc=status)
              _VERIFY(status)
           end if
        end if

        if (allocated(xptr3d_inter)) deallocate(xptr3d_inter)
        if (allocated(yptr3d_inter)) deallocate(yptr3d_inter)
        _RETURN(_SUCCESS)

     end subroutine RegridVector

  subroutine stage2DLatLon(this, fileName, oClients, rc)
     class (MAPL_GriddedIO), target, intent(inout) :: this
     character(len=*), intent(in) :: fileName
     type (ClientManager), optional, target, intent(inout) :: oClients
     integer, optional, intent(out) :: rc

     integer :: status
     real(REAL64), pointer :: ptr2d(:,:)
     type(ArrayReference), target :: ref
     class (AbstractGridFactory), pointer :: factory
     integer, allocatable :: localStart(:),globalStart(:),globalCount(:)
     logical :: hasll
     class(Variable), pointer :: var_lat,var_lon

     var_lon => this%metadata%get_variable('lons')
     var_lat => this%metadata%get_variable('lats')

     hasll = associated(var_lon) .and. associated(var_lat)
     if (hasll) then
        factory => get_factory(this%output_grid,rc=status)
        _VERIFY(status)

        call factory%generate_file_bounds(this%output_grid,LocalStart,GlobalStart,GlobalCount,rc=status)
        _VERIFY(status)
        call ESMF_GridGetCoord(this%output_grid, localDE=0, coordDim=1, &
        staggerloc=ESMF_STAGGERLOC_CENTER, &
        farrayPtr=ptr2d, rc=status)
        _VERIFY(STATUS)
        this%lons=ptr2d*MAPL_RADIANS_TO_DEGREES

        ref = ArrayReference(this%lons)
        call oClients%collective_stage_data(this%write_collection_id,trim(filename),'lons', &
             ref,start=localStart, global_start=GlobalStart, global_count=GlobalCount)

        call ESMF_GridGetCoord(this%output_grid, localDE=0, coordDim=2, &
        staggerloc=ESMF_STAGGERLOC_CENTER, &
        farrayPtr=ptr2d, rc=status)
        _VERIFY(STATUS)
        if (.not.allocated(this%lats)) allocate(this%lats(size(ptr2d,1),size(ptr2d,2)))
        this%lats=ptr2d*MAPL_RADIANS_TO_DEGREES
        ref = ArrayReference(this%lats)
         call oClients%collective_stage_data(this%write_collection_id,trim(filename),'lats', &
              ref,start=localStart, global_start=GlobalStart, global_count=GlobalCount)
        deallocate(LocalStart,GlobalStart,GlobalCount)
     end if

     var_lon => this%metadata%get_variable('corner_lons')
     var_lat => this%metadata%get_variable('corner_lats')

     hasll = associated(var_lon) .and. associated(var_lat)
     if (hasll) then
        factory => get_factory(this%output_grid,rc=status)
        _VERIFY(status)

        call factory%generate_file_corner_bounds(this%output_grid,LocalStart,GlobalStart,GlobalCount,rc=status)
        _VERIFY(status)
        call ESMF_GridGetCoord(this%output_grid, localDE=0, coordDim=1, &
        staggerloc=ESMF_STAGGERLOC_CORNER, &
        farrayPtr=ptr2d, rc=status)
        _VERIFY(STATUS)
        this%corner_lons=ptr2d*MAPL_RADIANS_TO_DEGREES
        ref = ArrayReference(this%corner_lons)
         call oClients%collective_stage_data(this%write_collection_id,trim(filename),'corner_lons', &
              ref,start=localStart, global_start=GlobalStart, global_count=GlobalCount)
        call ESMF_GridGetCoord(this%output_grid, localDE=0, coordDim=2, &
        staggerloc=ESMF_STAGGERLOC_CORNER, &
        farrayPtr=ptr2d, rc=status)
        _VERIFY(STATUS)
        if (.not.allocated(this%corner_lats)) allocate(this%corner_lats(size(ptr2d,1),size(ptr2d,2)))
        this%corner_lats=ptr2d*MAPL_RADIANS_TO_DEGREES
        ref = ArrayReference(this%corner_lats)
         call oClients%collective_stage_data(this%write_collection_id,trim(filename),'corner_lats', &
              ref,start=localStart, global_start=GlobalStart, global_count=GlobalCount)
     end if

     _RETURN(_SUCCESS)

  end subroutine stage2DLatLon

  subroutine stageData(this, field, fileName, tIndex, oClients, rc)
     class (MAPL_GriddedIO), intent(inout) :: this
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
     type(ArrayReference) :: ref
     integer :: lm
     logical :: hasDE
     integer, allocatable :: localStart(:),globalStart(:),globalCount(:)
     integer, allocatable :: gridLocalStart(:),gridGlobalStart(:),gridGlobalCount(:)
     class (AbstractGridFactory), pointer :: factory
     real, allocatable :: temp_2d(:,:), temp_3d(:,:,:)
     type(ESMF_VM) :: vm
     integer :: mpi_comm

     call ESMF_VMGetCurrent(vm,_RC)
     call ESMF_VMGet(vm,mpiCommunicator=mpi_comm,_RC)

     factory => get_factory(this%output_grid,rc=status)
     _VERIFY(status)
     hasDE = MAPL_GridHasDE(this%output_grid,rc=status)
     _VERIFY(status)
     lm = this%vdata%lm
     call ESMF_FieldGet(field,rank=fieldRank,name=fieldName,rc=status)
     _VERIFY(status)

     call factory%generate_file_bounds(this%output_grid,gridLocalStart,gridGlobalStart,gridGlobalCount,rc=status)
     _VERIFY(status)
     if (fieldRank==2) then
        if (hasDE) then
           call ESMF_FieldGet(Field,farrayPtr=ptr2d,rc=status)
           _VERIFY(status)
           if (this%nbits_to_keep < MAPL_NBITS_UPPER_LIMIT) then
              allocate(temp_2d,source=ptr2d)
              call DownBit(temp_2d,ptr2d,this%nbits_to_keep,undef=MAPL_undef,mpi_comm=mpi_comm,rc=status)
              _VERIFY(status)
           end if
        else
           allocate(ptr2d(0,0))
        end if
        ref = factory%generate_file_reference2D(Ptr2D)
        if (tindex > -1) then
           allocate(localStart,source=[gridLocalStart,1])
           allocate(globalStart,source=[gridGlobalStart,tindex])
           allocate(globalCount,source=[gridGlobalCount,1])
        else
           allocate(localStart,source=[gridLocalStart])
           allocate(globalStart,source=gridGlobalStart)
           allocate(globalCount,source=gridGlobalCount)
        end if
      else if (fieldRank==3) then
         if (HasDE) then
            call ESMF_FieldGet(field,farrayPtr=ptr3d,rc=status)
            _VERIFY(status)
            if (this%nbits_to_keep < MAPL_NBITS_UPPER_LIMIT) then
               allocate(temp_3d,source=ptr3d)
               call DownBit(temp_3d,ptr3d,this%nbits_to_keep,undef=MAPL_undef,mpi_comm=mpi_comm,rc=status)
               _VERIFY(status)
            end if
         else
            allocate(ptr3d(0,0,0))
         end if
         ref = factory%generate_file_reference3D(Ptr3D)
         if (tindex > -1) then
            allocate(localStart,source=[gridLocalStart,1,1])
            allocate(globalStart,source=[gridGlobalStart,1,tindex])
            allocate(globalCount,source=[gridGlobalCount,lm,1])
         else
            allocate(localStart,source=[gridLocalStart,1])
            allocate(globalStart,source=[gridGlobalStart,1])
            allocate(globalCount,source=[gridGlobalCount,lm])
         end if
      else
         _FAIL( "Rank not supported")
      end if

      call oClients%collective_stage_data(this%write_collection_id,trim(filename),trim(fieldName), &
           ref,start=localStart, global_start=GlobalStart, global_count=GlobalCount)
      _RETURN(_SUCCESS)

  end subroutine stageData

  subroutine alphabatize_variables(this,nfixedVars,rc)
     class (MAPL_GriddedIO), intent(inout) :: this
     integer, intent(in) :: nFixedVars
     integer, optional, intent(out) :: rc

     type(StringVector) :: order
     type(StringVector) :: newOrder
     character(len=:), pointer :: v1
     character(len=ESMF_MAXSTR) :: c1,c2
     character(len=ESMF_MAXSTR), allocatable :: temp(:)
     logical :: swapped
     integer :: n,i
     integer :: status

     order = this%metadata%get_order(rc=status)
     _VERIFY(status)
     n = Order%size()
     allocate(temp(nFixedVars+1:n))
     do i=1,n
        v1 => order%at(i)
        if ( i > nFixedVars) temp(i)=trim(v1)
     enddo

     swapped = .true.
     do while(swapped)
        swapped = .false.
        do i=nFixedVars+1,n-1
           c1 = temp(i)
           c2 = temp(i+1)
           if (c1 > c2) then
              temp(i+1)=c1
              temp(i)=c2
              swapped =.true.
           end if
        enddo
     enddo

     do i=1,nFixedVars
        v1 => Order%at(i)
        call newOrder%push_back(v1)
     enddo
     do i=nFixedVars+1,n
        call newOrder%push_back(trim(temp(i)))
     enddo
     call this%metadata%set_order(newOrder,rc=status)
     _VERIFY(status)
     deallocate(temp)

     _RETURN(_SUCCESS)

  end subroutine alphabatize_variables

  subroutine request_data_from_file(this,filename,timeindex,rc)
     class(mapl_GriddedIO), intent(inout) :: this
     character(len=*), intent(in) :: filename
     integer, intent(in) :: timeindex
     integer, intent(out), optional :: rc

     integer :: status
     type(esmf_grid) :: filegrid
     type(maplDatacollection), pointer :: collection
     integer :: i,numVars
     character(len=ESMF_MAXSTR), allocatable :: names(:)
     type(ESMF_Field) :: output_field
     type(ESMF_Field), allocatable :: input_fields(:)
     integer :: ub(1),lb(1),dims(3),lm,rank
     type(ArrayReference) :: ref
     real, pointer :: ptr2d(:,:) => null()
     real, pointer :: ptr3d(:,:,:) => null()
     integer, allocatable :: localStart(:), globalStart(:), globalCount(:)
     integer, allocatable :: gridLocalStart(:), gridGlobalStart(:), gridGlobalCount(:)
     type(ESMF_Grid) :: output_grid
     logical :: hasDE
     class(AbstractGridFactory), pointer :: factory
     integer :: regrid_hints

     collection => Datacollections%at(this%metadata_collection_id)
     this%current_file_metadata => collection%find(filename, _RC)
     filegrid = collection%src_grid
     factory => get_factory(filegrid)
     hasDE=MAPL_GridHasDE(filegrid,rc=status)
     _VERIFY(status)
     call ESMF_FieldBundleGet(this%output_bundle,grid=output_grid,rc=status)
     _VERIFY(status)
     if (filegrid/=output_grid) then
        this%regrid_handle => new_regridder_manager%make_regridder(filegrid,output_grid,this%regrid_method,hints=this%regrid_hints,rc=status)
        _VERIFY(status)
     end if
     call MAPL_GridGet(filegrid,globalCellCountPerdim=dims,rc=status)
     _VERIFY(status)
     call factory%generate_file_bounds(fileGrid,gridLocalStart,gridGlobalStart,gridGlobalCount,metadata=this%current_file_metadata%fileMetadata,rc=status)
     _VERIFY(status)
     ! create input bundle
     call ESMF_FieldBundleGet(this%output_bundle,fieldCount=numVars,rc=status)
     _VERIFY(status)
     allocate(names(numVars),stat=status)
     _VERIFY(status)
     allocate(input_fields(numVars),stat=status)
     _VERIFY(status)
     call ESMF_FieldBundleGet(this%output_bundle,fieldNameList=names,rc=status)
     _VERIFY(status)
     do i=1,numVars
        call ESMF_FieldBundleGet(this%output_bundle,names(i),field=output_field,rc=status)
        _VERIFY(status)
        call ESMF_FieldGet(output_field,rank=rank,rc=status)
        _VERIFY(status)
        if (rank==2) then
           input_fields(i) = ESMF_FieldCreate(filegrid,typekind=ESMF_TYPEKIND_R4,gridToFieldMap=[1,2],name=trim(names(i)),rc=status)
           _VERIFY(status)
           if (hasDE) then
              call ESMF_FieldGet(input_fields(I),0,farrayPtr=ptr2d,rc=status)
              _VERIFY(status)
           else
              allocate(ptr2d(0,0),stat=status)
              _VERIFY(status)
           end if
           ref=factory%generate_file_reference2D(ptr2d)
           allocate(localStart,source=[gridLocalStart,timeIndex])
           allocate(globalStart,source=[gridGlobalStart,timeIndex])
           allocate(globalCount,source=[gridGlobalCount,1])
        else if (rank==3) then
           call ESMF_FieldGet(output_field,ungriddedLBound=lb,ungriddedUBound=ub,rc=status)
           _VERIFY(status)
           lm=ub(1)-lb(1)+1
           input_fields(i) = ESMF_FieldCreate(filegrid,typekind=ESMF_TYPEKIND_R4,gridToFieldMap=[1,2], &
              ungriddedLBound=lb,ungriddedUBound=ub,name=trim(names(i)),rc=status)
           _VERIFY(status)
           if (hasDE) then
              call ESMF_FieldGet(input_fields(I),0,farrayPtr=ptr3d,rc=status)
              _VERIFY(status)
           else
              allocate(ptr3d(0,0,0),stat=status)
              _VERIFY(status)
           end if
           ref=factory%generate_file_reference3D(ptr3d,metadata=this%current_file_metadata%filemetadata)
           allocate(localStart,source=[gridLocalStart,1,timeIndex])
           allocate(globalStart,source=[gridGlobalStart,1,timeIndex])
           allocate(globalCount,source=[gridGlobalCount,lm,1])
        end if
        call i_Clients%collective_prefetch_data( &
             this%read_collection_id, fileName, trim(names(i)), &
             & ref, start=localStart, global_start=globalStart, global_count=globalCount, _RC)
        deallocate(localStart,globalStart,globalCount)
     enddo
     deallocate(gridLocalStart,gridGlobalStart,gridGlobalCount)
     this%input_bundle = ESMF_FieldBundleCreate(fieldList=input_fields,rc=status)
     _VERIFY(status)
     _RETURN(_SUCCESS)

  end subroutine request_data_from_file

  subroutine process_data_from_file(this,rc)
     class(mapl_GriddedIO), intent(inout) :: this
     integer, intent(out), optional :: rc

     integer :: status
     integer :: i,numVars
     character(len=ESMF_MAXSTR), allocatable :: names(:)
     type(ESMF_Field) :: field
     type(GriddedIOitem), pointer :: item
     type(GriddedIOitemVectorIterator) :: iter

     call ESMF_FieldBundleGet(this%output_bundle,fieldCount=numVars,rc=status)
     _VERIFY(status)
     allocate(names(numVars),stat=status)
     _VERIFY(status)
     call ESMF_FieldBundleGet(this%output_bundle,fieldNameList=names,rc=status)
     _VERIFY(status)
     iter = this%items%begin()
     do while(iter /= this%items%end())
        item => iter%get()
        if (item%itemType == ItemTypeScalar) then
           call this%swap_undef_value(trim(item%xname),_RC)
           call this%regridScalar(trim(item%xname),rc=status)
           _VERIFY(status)
        else if (item%itemType == ItemTypeVector) then
           call this%swap_undef_value(trim(item%xname),_RC)
           call this%swap_undef_value(trim(item%yname),_RC)
           call this%regridVector(trim(item%xname),trim(item%yname),rc=status)
           _VERIFY(status)
        end if
        call iter%next()
     enddo

     do i=1,numVars
        call ESMF_FieldBundleGet(this%input_bundle,trim(names(i)),field=field,rc=status)
        _VERIFY(status)
        call ESMF_FieldDestroy(field,noGarbage=.true., rc=status)
        _VERIFY(status)
     enddo
     call ESMF_FieldBundleDestroy(this%input_bundle,noGarbage=.true.,rc=status)
     _VERIFY(status)
     _RETURN(_SUCCESS)

  end subroutine process_data_from_file

  subroutine swap_undef_value(this,fname,rc)
     class (MAPL_GriddedIO), intent(inout) :: this
     character(len=*), intent(in) :: fname
     integer, optional, intent(out) :: rc

     integer :: status

     type(ESMF_Field) :: field
     integer :: fieldRank
     real, pointer :: ptr3d(:,:,:)
     real, pointer :: ptr2d(:,:)
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

     if (fieldRank==2) then
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

end module MAPL_GriddedIOMod
