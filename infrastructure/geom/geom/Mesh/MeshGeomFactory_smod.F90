#include "MAPL.h"

submodule (mapl3g_MeshGeomFactory) MeshGeomFactory_smod

   use mapl3g_GeomSpec
   use mapl3g_MeshGeomSpec
   use mapl3g_MeshDecomposition
   use mapl_ErrorHandlingMod
   use mapl_Constants, only: MAPL_PI_R8
   use mapl_StringUtilities, only: to_lower
   use mapl3g_get_hconfig, only: get_hconfig
   use mapl3g_hconfig_params, only: HConfigParams
   use pfio
   use gftl2_StringVector
   use mapl3g_StringDictionary
   use esmf
   use mapl_KeywordEnforcer, only: KE => KeywordEnforcer

   implicit none

contains

   ! Make GeomSpec from HConfig
   module function make_geom_spec_from_hconfig(this, hconfig, rc) result(geom_spec)
      class(GeomSpec), allocatable :: geom_spec
      class(MeshGeomFactory), intent(in) :: this
      type(ESMF_HConfig), intent(in) :: hconfig
      integer, optional, intent(out) :: rc

      integer :: status

      geom_spec = make_MeshGeomSpec(hconfig, _RC)

      _UNUSED_DUMMY(this)
      _RETURN(_SUCCESS)
   end function make_geom_spec_from_hconfig

   ! Make GeomSpec from FileMetadata
   module function make_geom_spec_from_metadata(this, file_metadata, rc) result(geom_spec)
      class(GeomSpec), allocatable :: geom_spec
      class(MeshGeomFactory), intent(in) :: this
      type(FileMetadata), intent(in) :: file_metadata
      integer, optional, intent(out) :: rc

      integer :: status

      geom_spec = make_MeshGeomSpec(file_metadata, _RC)

      _UNUSED_DUMMY(this)
      _RETURN(_SUCCESS)
   end function make_geom_spec_from_metadata

   ! Check if factory supports this GeomSpec type
   module logical function supports_spec(this, geom_spec) result(supports)
      class(MeshGeomFactory), intent(in) :: this
      class(GeomSpec), intent(in) :: geom_spec

      type(MeshGeomSpec) :: reference

      supports = same_type_as(geom_spec, reference)
      _UNUSED_DUMMY(this)
   end function supports_spec

   ! Check if factory supports this HConfig
   module logical function supports_hconfig(this, hconfig, rc) result(supports)
      class(MeshGeomFactory), intent(in) :: this
      type(ESMF_HConfig), intent(in) :: hconfig
      integer, optional, intent(out) :: rc

      integer :: status
      character(len=:), allocatable :: class_name
      type(HConfigParams) :: params
      logical :: has_file

      ! Check for class: mesh (case-insensitive)
      params = HConfigParams(hconfig, "class")
      call get_hconfig(class_name, params, _RC)
      
      if (.not. allocated(class_name)) then
         supports = .false.
         _RETURN(_SUCCESS)
      end if

      class_name = to_lower(class_name)
      supports = trim(class_name) == "mesh"
      
      if (.not. supports) then
         _RETURN(_SUCCESS)
      end if

      ! Verify required fields
      has_file = ESMF_HConfigIsDefined(hconfig, keyString='file', _RC)
      supports = has_file  ! For now, require file

      _UNUSED_DUMMY(this)
      _RETURN(_SUCCESS)
   end function supports_hconfig

    ! Check if factory supports this FileMetadata
    module logical function supports_metadata(this, file_metadata, rc) result(supports)
       class(MeshGeomFactory), intent(in) :: this
       type(FileMetadata), intent(in) :: file_metadata
       integer, optional, intent(out) :: rc

       integer :: status
       character(len=:), allocatable :: source_file
       type(NetCDF4_FileFormatter) :: formatter
       type(FileMetadata) :: temp_metadata

       ! Check if file_metadata has a source_file set
       ! If so, try to read it and check if it's a valid mesh file
       supports = .false.
       
       ! Try to get source_file - if it fails, this metadata doesn't have one
       source_file = file_metadata%get_source_file(rc=status)
       if (status /= 0) then
          _RETURN(_SUCCESS)
       end if
       
       ! Try to open the file and check if it has mesh structure
       ! Mesh must have nodeCount, elementCount, and connectivity
       ! This distinguishes it from LocStream (which lacks elementCount/connectivity)
       call formatter%open(source_file, pFIO_READ, _RC)
       temp_metadata = formatter%read(_RC)
       call formatter%close(_RC)
       
       supports = temp_metadata%has_dimension('nodeCount') .and. &
                  temp_metadata%has_dimension('elementCount') .and. &
                  temp_metadata%has_variable('elementConn')

       _UNUSED_DUMMY(this)
       _RETURN(_SUCCESS)
    end function supports_metadata

   ! Create ESMF_Geom from GeomSpec
   module function make_geom(this, geom_spec, rc) result(geom)
      type(ESMF_Geom) :: geom
      class(MeshGeomFactory), intent(in) :: this
      class(GeomSpec), intent(in) :: geom_spec
      integer, optional, intent(out) :: rc

      integer :: status


      select type (geom_spec)
      type is (MeshGeomSpec)
         geom = typesafe_make_geom(geom_spec, _RC)
      class default
         _FAIL("geom_spec type not supported")
      end select

      _UNUSED_DUMMY(this)
      _RETURN(_SUCCESS)
   end function make_geom

   ! Type-safe helper for creating ESMF_Geom from MeshGeomSpec
    function typesafe_make_geom(spec, rc) result(geom)
       type(ESMF_Geom) :: geom
       type(MeshGeomSpec), intent(in) :: spec
       integer, optional, intent(out) :: rc

       integer :: status
       type(ESMF_Mesh) :: mesh
       type(ESMF_VM) :: vm

       mesh = create_esmf_mesh(spec, _RC)
       geom = ESMF_GeomCreate(mesh=mesh, _RC)
       
       ! Synchronize after ESMF_GeomCreate (collective operation)
       call ESMF_VMGetCurrent(vm, rc=status)
       call ESMF_VMBarrier(vm, _RC)

       _RETURN(_SUCCESS)
    end function typesafe_make_geom

   ! Helper function: Check if decomposition is uniform (matches ESMF automatic distribution)
   function is_uniform_decomposition(decomp, nelements, petCount) result(is_uniform)
      logical :: is_uniform
      type(MeshDecomposition), intent(in) :: decomp
      integer, intent(in) :: nelements
      integer, intent(in) :: petCount
      
      integer, allocatable :: point_dist(:)
      integer, allocatable :: expected_dist(:)
      integer :: i, im, remainder
      
      ! Get actual distribution
      point_dist = decomp%get_point_distribution()
      
      ! Check size matches petCount
      if (size(point_dist) /= petCount) then
         is_uniform = .false.
         return
      end if
      
      ! Compute expected uniform distribution (same as ESMF algorithm)
      allocate(expected_dist(petCount))
      im = nelements / petCount
      expected_dist = im
      remainder = nelements - petCount * im
      expected_dist(:remainder) = expected_dist(:remainder) + 1
      
      ! Compare
      is_uniform = all(point_dist == expected_dist)
      
   end function is_uniform_decomposition

   ! Helper function: Convert MeshDecomposition to ESMF_DistGrid for element distribution
   function decomp_to_distgrid(decomp, nelements, rc) result(distgrid)
      type(ESMF_DistGrid) :: distgrid
      type(MeshDecomposition), intent(in) :: decomp
      integer, intent(in) :: nelements
      integer, optional, intent(out) :: rc
      
      integer :: status
      integer, allocatable :: point_dist(:)
      integer :: i, j, petCount, idx
      integer, allocatable :: indexList(:)
      type(ESMF_VM) :: vm
      integer :: localPet
      
      ! Get element distribution from decomposition
      point_dist = decomp%get_point_distribution()
      petCount = size(point_dist)
      
      ! Get current VM and local PET
      call ESMF_VMGetCurrent(vm, _RC)
      call ESMF_VMGet(vm, localPet=localPet, _RC)
      
      ! Build local index list for this PET
      allocate(indexList(point_dist(localPet+1)))
      
      ! Calculate starting global index for this PET
      idx = 1
      do i = 1, localPet
         idx = idx + point_dist(i)
      end do
      
      ! Fill local index list with global indices
      do j = 1, point_dist(localPet+1)
         indexList(j) = idx
         idx = idx + 1
      end do
      
      ! Create DistGrid with arbitrary sequence index list
      distgrid = ESMF_DistGridCreate(arbSeqIndexList=indexList, _RC)
      
      deallocate(indexList)
      
      _RETURN(_SUCCESS)
   end function decomp_to_distgrid

   ! Create ESMF_Mesh from MeshGeomSpec
   function create_esmf_mesh(spec, rc) result(mesh)
      type(ESMF_Mesh) :: mesh
      type(MeshGeomSpec), intent(in) :: spec
      integer, optional, intent(out) :: rc

      integer :: status
      integer :: nelements
      character(len=:), allocatable :: filename
      type(ESMF_DistGrid) :: elementDistgrid  ! For custom distribution
      
      type(MeshDecomposition) :: decomp
      type(ESMF_VM) :: vm
      integer :: localPet, petCount

      ! Get mesh data from spec
      nelements = spec%get_nelements()
      
      ! Get filename - mesh will be created directly from file
      filename = spec%get_filename()
      _ASSERT(allocated(filename), 'Filename must be provided in MeshGeomSpec')
      
      ! Get decomposition and VM info
      decomp = spec%get_decomposition()
      call ESMF_VMGetCurrent(vm, _RC)
      call ESMF_VMGet(vm, localPet=localPet, petCount=petCount, _RC)
      
      ! Check if decomposition is uniform (matches ESMF automatic distribution)
      if (is_uniform_decomposition(decomp, nelements, petCount)) then
         ! Path 1: Use ESMF_MeshCreate with automatic distribution
         ! This is the simplest and most efficient path
         mesh = ESMF_MeshCreate(filename, fileFormat=ESMF_FILEFORMAT_ESMFMESH, _RC)
      else
         ! Path 2: Use ESMF_MeshCreate with custom elementDistgrid
         ! This allows custom load balancing
         elementDistgrid = decomp_to_distgrid(decomp, nelements, _RC)
         mesh = ESMF_MeshCreate(filename, fileFormat=ESMF_FILEFORMAT_ESMFMESH, &
                                elementDistgrid=elementDistgrid, _RC)
         call ESMF_DistGridDestroy(elementDistgrid, _RC)
      end if

      _RETURN(_SUCCESS)
   end function create_esmf_mesh

   ! Generate file metadata for mesh
   module function make_file_metadata(this, geom_spec, unusable, chunksizes, rc) result(file_metadata)
      type(FileMetadata) :: file_metadata
      class(MeshGeomFactory), intent(in) :: this
      class(GeomSpec), intent(in) :: geom_spec
      class(KE), optional, intent(in) :: unusable
      integer, optional, intent(in) :: chunksizes(:)
      integer, optional, intent(out) :: rc

      integer :: status

      select type (geom_spec)
      type is (MeshGeomSpec)
         file_metadata = typesafe_make_file_metadata(geom_spec, _RC)
      class default
         _FAIL('geom_spec is not of dynamic type MeshGeomSpec.')
      end select

      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(unusable)
      _UNUSED_DUMMY(chunksizes)
      _RETURN(_SUCCESS)
   end function make_file_metadata

    ! Type-safe helper for creating FileMetadata from MeshGeomSpec
    ! The file_metadata only needs to store the filename, since the file contains
    ! all mesh information (dimensions, coordinates, connectivity, etc.)
    function typesafe_make_file_metadata(geom_spec, rc) result(file_metadata)
       type(FileMetadata) :: file_metadata
       type(MeshGeomSpec), intent(in) :: geom_spec
       integer, optional, intent(out) :: rc

       integer :: status
       character(len=:), allocatable :: filename

       file_metadata = FileMetadata()

       ! Get filename from spec
       filename = geom_spec%get_filename()
       _ASSERT(allocated(filename), 'Filename must be provided in MeshGeomSpec')

       ! Store the filename using set_source_file
       ! This allows the file_metadata to reference the mesh file
       call file_metadata%set_source_file(filename, _RC)

       _RETURN(_SUCCESS)
    end function typesafe_make_file_metadata

   ! Return gridded dimensions for mesh
   module function make_gridded_dims(this, geom_spec, rc) result(gridded_dims)
      type(StringVector) :: gridded_dims
      class(MeshGeomFactory), intent(in) :: this
      class(GeomSpec), intent(in) :: geom_spec
      integer, optional, intent(out) :: rc

      integer :: status

      select type (geom_spec)
      type is (MeshGeomSpec)
         ! For unstructured mesh, use element count as the gridded dimension
         call gridded_dims%push_back("ncells")
      class default
         _FAIL('geom_spec is not of dynamic type MeshGeomSpec.')
      end select

      _UNUSED_DUMMY(this)
      _RETURN(_SUCCESS)
   end function make_gridded_dims

   ! Return variable attributes for mesh
   module function make_variable_attributes(this, geom_spec, rc) result(variable_attributes)
      type(StringDictionary) :: variable_attributes
      class(MeshGeomFactory), intent(in) :: this
      class(GeomSpec), intent(in) :: geom_spec
      integer, optional, intent(out) :: rc

      integer :: status

      variable_attributes = StringDictionary()

      select type (geom_spec)
      type is (MeshGeomSpec)
         ! CF-compliant attributes for unstructured mesh
         call variable_attributes%put('location', 'face')
         call variable_attributes%put('mesh', 'mesh_topology')
      class default
         _FAIL('geom_spec is not of dynamic type MeshGeomSpec.')
      end select

      _UNUSED_DUMMY(this)
      _RETURN(_SUCCESS)
   end function make_variable_attributes

end submodule MeshGeomFactory_smod
