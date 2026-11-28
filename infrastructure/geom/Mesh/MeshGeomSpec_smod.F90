#include "MAPL.h"

submodule (mapl_MeshGeomSpec_mod) MeshGeomSpec_smod

   use mapl_GeomSpec_mod
   use mapl_CoordinateAxis_mod, only: get_dim_name, get_coordinates
   use mapl_MeshDecomposition_mod
   use mapl_ErrorHandling_mod
   use mapl_StringUtilities_mod, only: to_lower
   use mapl_get_hconfig_mod, only: get_hconfig
   use mapl_hconfig_params_mod, only: HConfigParams
   use gftl2_StringVector
   use mapl_StringDictionary_mod
   use pfio
   use esmf
   use, intrinsic :: iso_fortran_env, only: REAL64, INT32

   implicit none

contains

   ! New file-based constructor that reads nnodes/nelements from file
   module function new_MeshGeomSpec_from_file(filename, unusable, decomposition, rc) result(spec)
      use mapl_KeywordEnforcer_mod
      type(MeshGeomSpec) :: spec
      character(len=*), intent(in) :: filename
      class(KeywordEnforcer), optional, intent(in) :: unusable
      type(MeshDecomposition), optional, intent(in) :: decomposition
      integer, optional, intent(out) :: rc

      integer :: status
      integer :: nnodes, nelements
      type(NetCDF4_FileFormatter) :: file_formatter
      type(FileMetadata) :: metadata
      type(MeshDecomposition) :: decomp
      type(ESMF_VM) :: vm
      integer :: petCount

      _UNUSED_DUMMY(unusable)

      ! Read file to get dimensions
      call file_formatter%open(filename, pFIO_READ, _RC)
      metadata = file_formatter%read(_RC)
      call file_formatter%close(_RC)

      ! Extract dimensions from metadata
      nnodes = metadata%get_dimension('nodeCount', _RC)
      nelements = metadata%get_dimension('elementCount', _RC)

      ! Create or use provided decomposition
      if (present(decomposition)) then
         decomp = decomposition
      else
         ! Auto-create decomposition based on current VM
         call ESMF_VMGetCurrent(vm, _RC)
         call ESMF_VMGet(vm, petCount=petCount, _RC)
         decomp = MeshDecomposition(nelements, nnodes, petCount=petCount)
      end if

      ! Set spec fields directly (no old constructor)
      spec%decomposition = decomp
      spec%filename = filename

      _RETURN(_SUCCESS)
   end function new_MeshGeomSpec_from_file

   ! Factory method from HConfig
   module function make_MeshGeomSpec_from_hconfig(hconfig, rc) result(spec)
      type(MeshGeomSpec) :: spec
      type(ESMF_HConfig), intent(in) :: hconfig
      integer, optional, intent(out) :: rc

      integer :: status
      logical :: has_file
      character(len=:), allocatable :: filename

      ! Check if file-based configuration
      has_file = ESMF_HConfigIsDefined(hconfig, keyString='file', _RC)

      if (has_file) then
         filename = ESMF_HConfigAsString(hconfig, keyString='file', _RC)
         
         ! Use new file-based constructor
         spec = MeshGeomSpec(filename, rc=status)
         _VERIFY(status)
      else
         ! For now, require file-based configuration
         ! TODO: Add support for inline mesh specification
         _FAIL('Inline mesh specification not yet supported in HConfig')
      end if

      _RETURN(_SUCCESS)
   end function make_MeshGeomSpec_from_hconfig

   ! Factory method from FileMetadata
   module function make_MeshGeomSpec_from_metadata(file_metadata, filename, rc) result(spec)
      type(MeshGeomSpec) :: spec
      type(FileMetadata), intent(in) :: file_metadata
      character(len=*), optional, intent(in) :: filename
      integer, optional, intent(out) :: rc

      integer :: status

      ! If filename is provided, use the file-based constructor
      if (present(filename)) then
         spec = MeshGeomSpec(filename, rc=status)
         _VERIFY(status)
      else
         _FAIL('Filename is required for MeshGeomSpec creation')
      end if

      _RETURN(_SUCCESS)
   end function make_MeshGeomSpec_from_metadata

   ! Check if HConfig is supported
   module logical function supports_hconfig_(this, hconfig, rc) result(supports)
      class(MeshGeomSpec), intent(in) :: this
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
      
      _RETURN_UNLESS(supports)


      ! Verify required fields
      has_file = ESMF_HConfigIsDefined(hconfig, keyString='file', _RC)
      supports = has_file  ! For now, require file

      _UNUSED_DUMMY(this)
      _RETURN(_SUCCESS)
   end function supports_hconfig_

   ! Check if FileMetadata is supported
   module logical function supports_metadata_(this, file_metadata, rc) result(supports)
      class(MeshGeomSpec), intent(in) :: this
      type(FileMetadata), intent(in) :: file_metadata
      integer, optional, intent(out) :: rc

      integer :: status

      ! Mesh must have nodeCount, elementCount, and connectivity
      ! This distinguishes it from LocStream (which lacks elementCount/connectivity)
      supports = file_metadata%has_dimension('nodeCount') .and. &
                 file_metadata%has_dimension('elementCount') .and. &
                 file_metadata%has_variable('elementConn')

      _UNUSED_DUMMY(this)
      _RETURN(_SUCCESS)
   end function supports_metadata_

   ! Equality comparison
   module pure logical function equal_to(a, b) result(equal)
      class(MeshGeomSpec), intent(in) :: a
      class(GeomSpec), intent(in) :: b

      equal = .false.
      
      select type (b)
      type is (MeshGeomSpec)
         ! Compare filenames (if both allocated)
         if (allocated(a%filename) .and. allocated(b%filename)) then
            if (a%filename /= b%filename) return
         else if (allocated(a%filename) .neqv. allocated(b%filename)) then
            return
         end if
         
         ! Compare decompositions
         if (a%decomposition /= b%decomposition) return
         
         equal = .true.
      class default
         equal = .false.
      end select

   end function equal_to

   ! Create ESMF_Geom from GeomSpec
   module function make_geom(this, rc) result(geom)
      type(ESMF_Geom) :: geom
      class(MeshGeomSpec), intent(in) :: this
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_Mesh) :: mesh
      type(ESMF_VM) :: vm

      mesh = create_esmf_mesh(this, _RC)
      geom = ESMF_GeomCreate(mesh=mesh, _RC)

      ! Synchronize after ESMF_GeomCreate (collective operation)
      call ESMF_VMGetCurrent(vm, rc=status)
      call ESMF_VMBarrier(vm, _RC)

      _RETURN(_SUCCESS)
   end function make_geom

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
   ! The file_metadata only needs to store the filename, since the file contains
   ! all mesh information (dimensions, coordinates, connectivity, etc.)
   module function make_file_metadata(this, unusable, chunksizes, rc) &
        result(file_metadata)
      use mapl_KeywordEnforcer_mod
      type(FileMetadata) :: file_metadata
      class(MeshGeomSpec), intent(in) :: this
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(in) :: chunksizes(:)
      integer, optional, intent(out) :: rc

      integer :: status
      character(len=:), allocatable :: filename

      file_metadata = FileMetadata()

      ! Get filename from spec
      filename = this%get_filename()
      _ASSERT(allocated(filename), 'Filename must be provided in MeshGeomSpec')

      ! Store the filename using set_source_file
      ! This allows the file_metadata to reference the mesh file
      call file_metadata%set_source_file(filename, _RC)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
      _UNUSED_DUMMY(chunksizes)
   end function make_file_metadata

   ! Return gridded dimensions for mesh
   module function make_gridded_dims(this, rc) result(gridded_dims)
      type(StringVector) :: gridded_dims
      class(MeshGeomSpec), intent(in) :: this
      integer, optional, intent(out) :: rc

      ! For unstructured mesh, use element count as the gridded dimension
      call gridded_dims%push_back("ncells")

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(this)
   end function make_gridded_dims

   ! Return variable attributes for mesh
   module function make_variable_attributes(this, rc) &
        result(variable_attributes)
      type(StringDictionary) :: variable_attributes
      class(MeshGeomSpec), intent(in) :: this
      integer, optional, intent(out) :: rc

      variable_attributes = StringDictionary()

      ! CF-compliant attributes for unstructured mesh
      call variable_attributes%put('location', 'face')
      call variable_attributes%put('mesh', 'mesh_topology')

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(this)
   end function make_variable_attributes

end submodule MeshGeomSpec_smod
