#include "MAPL.h"

submodule (mapl3g_MeshGeomSpec) MeshGeomSpec_smod

   use mapl3g_GeomSpec
   use mapl3g_CoordinateAxis, only: get_dim_name, get_coordinates
   use mapl3g_MeshDecomposition
   use mapl_ErrorHandlingMod
   use mapl_StringUtilities, only: to_lower
   use mapl3g_get_hconfig, only: get_hconfig
   use mapl3g_hconfig_params, only: HConfigParams
   use pfio
   use esmf
   use, intrinsic :: iso_fortran_env, only: REAL64, INT32

   implicit none

contains

   ! Factory method from HConfig
   module function make_MeshGeomSpec_from_hconfig(hconfig, rc) result(spec)
      type(MeshGeomSpec) :: spec
      type(ESMF_HConfig), intent(in) :: hconfig
      integer, optional, intent(out) :: rc

      integer :: status
      logical :: has_file
      character(len=:), allocatable :: filename
      type(NetCDF4_FileFormatter) :: file_formatter
      type(FileMetadata) :: metadata

      ! Check if file-based configuration
      has_file = ESMF_HConfigIsDefined(hconfig, keyString='file', _RC)

      if (has_file) then
         filename = ESMF_HConfigAsString(hconfig, keyString='file', _RC)
         
         ! Read mesh from file
         call file_formatter%open(filename, pFIO_READ, _RC)
         metadata = file_formatter%read(_RC)
         call file_formatter%close(_RC)
         
         ! Pass filename to metadata factory method
         spec = make_MeshGeomSpec(metadata, filename=filename, rc=status)
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
      integer :: nnodes, nelements
      type(MeshDecomposition) :: decomp
      type(ESMF_VM) :: vm
      integer :: petCount

      ! Get dimensions from metadata
      nnodes = file_metadata%get_dimension('nodeCount', _RC)
      nelements = file_metadata%get_dimension('elementCount', _RC)
      
      ! Get petCount from current VM
      call ESMF_VMGetCurrent(vm, _RC)
      call ESMF_VMGet(vm, petCount=petCount, _RC)
      
      ! Create decomposition with both element and node distribution (Phase 3)
      decomp = MeshDecomposition(nelements, nnodes, petCount=petCount)
      
      ! Create spec with basic info
      spec = MeshGeomSpec(nnodes, nelements, decomp)
      
      ! Store filename if provided
      if (present(filename)) then
         spec%filename = filename
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
      
      if (.not. supports) then
         _RETURN(_SUCCESS)
      end if

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
         ! Compare basic properties
         if (a%nnodes /= b%nnodes) return
         if (a%nelements /= b%nelements) return
         
         ! Compare decompositions
         if (a%decomposition /= b%decomposition) return
         
         equal = .true.
      class default
         equal = .false.
      end select

   end function equal_to

end submodule MeshGeomSpec_smod
