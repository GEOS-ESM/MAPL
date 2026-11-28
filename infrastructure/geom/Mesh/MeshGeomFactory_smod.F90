#include "MAPL.h"

submodule (mapl_MeshGeomFactory_mod) MeshGeomFactory_smod

   use mapl_GeomSpec_mod
   use mapl_MeshGeomSpec_mod
   use mapl_ErrorHandling_mod
   use mapl_StringUtilities_mod, only: to_lower
   use mapl_get_hconfig_mod, only: get_hconfig
   use mapl_hconfig_params_mod, only: HConfigParams
   use pfio
   use esmf

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

end submodule MeshGeomFactory_smod
