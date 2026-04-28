#include "MAPL.h"

module mapl3g_MeshGeomSpec
   use mapl3g_GeomSpec
   use mapl3g_MeshDecomposition
   use mapl_ErrorHandlingMod
   use mapl_KeywordEnforcer
   use esmf, only: ESMF_KIND_R8, ESMF_HConfig, ESMF_KIND_R4
   use pfio, only: FileMetadata
   implicit none(type,external)
   private

   public :: MeshGeomSpec
   public :: make_MeshGeomSpec

   integer, parameter :: R8 = ESMF_KIND_R8

   type, extends(GeomSpec) :: MeshGeomSpec
      private
      character(len=:), allocatable :: filename  ! Mesh data file path
      
      type(MeshDecomposition) :: decomposition
   contains
      ! Mandatory interface
      procedure :: equal_to
      procedure :: get_horz_ij_index_r4
      procedure :: get_horz_ij_index_r8
      
      ! Mesh-specific support methods
      procedure :: supports_hconfig => supports_hconfig_
      procedure :: supports_metadata => supports_metadata_
      generic :: supports => supports_hconfig, supports_metadata
      
      ! Accessors
      procedure, public :: get_nnodes
      procedure, public :: get_nelements
      procedure, public :: get_filename
      procedure, public :: set_filename
      procedure, public :: get_decomposition
   end type MeshGeomSpec

   interface MeshGeomSpec
      module procedure new_MeshGeomSpec_from_file
   end interface MeshGeomSpec

   interface make_MeshGeomSpec
      procedure make_MeshGeomSpec_from_hconfig
      procedure make_MeshGeomSpec_from_metadata
   end interface make_MeshGeomSpec

   interface
      ! Factory methods (in submodule)
      module function make_MeshGeomSpec_from_hconfig(hconfig, rc) result(spec)
         type(MeshGeomSpec) :: spec
         type(ESMF_HConfig), intent(in) :: hconfig
         integer, optional, intent(out) :: rc
      end function make_MeshGeomSpec_from_hconfig

      module function make_MeshGeomSpec_from_metadata(file_metadata, filename, rc) result(spec)
         type(MeshGeomSpec) :: spec
         type(FileMetadata), intent(in) :: file_metadata
         character(len=*), optional, intent(in) :: filename
         integer, optional, intent(out) :: rc
      end function make_MeshGeomSpec_from_metadata

      ! New file-based constructor (in submodule)
      module function new_MeshGeomSpec_from_file(filename, unusable, decomposition, rc) result(spec)
         use mapl_KeywordEnforcer
         type(MeshGeomSpec) :: spec
         character(len=*), intent(in) :: filename
         class(KeywordEnforcer), optional, intent(in) :: unusable
         type(MeshDecomposition), optional, intent(in) :: decomposition
         integer, optional, intent(out) :: rc
      end function new_MeshGeomSpec_from_file

      ! Support methods (in submodule)
      module logical function supports_hconfig_(this, hconfig, rc) result(supports)
         class(MeshGeomSpec), intent(in) :: this
         type(ESMF_HConfig), intent(in) :: hconfig
         integer, optional, intent(out) :: rc
      end function supports_hconfig_

      module logical function supports_metadata_(this, file_metadata, rc) result(supports)
         class(MeshGeomSpec), intent(in) :: this
         type(FileMetadata), intent(in) :: file_metadata
         integer, optional, intent(out) :: rc
      end function supports_metadata_

      ! Equality (in submodule)
      module pure logical function equal_to(a, b) result(equal)
         class(MeshGeomSpec), intent(in) :: a
         class(GeomSpec), intent(in) :: b
      end function equal_to
   end interface

contains

   ! Accessor methods - read from file on demand
   integer function get_nnodes(this) result(n)
      use pfio
      class(MeshGeomSpec), intent(in) :: this
      type(NetCDF4_FileFormatter) :: file_formatter
      type(FileMetadata) :: metadata
      integer :: status
      
      if (.not. allocated(this%filename)) then
         n = 0
         return
      end if
      
      call file_formatter%open(this%filename, pFIO_READ, rc=status)
      if (status /= 0) then
         n = 0
         return
      end if
      metadata = file_formatter%read(rc=status)
      call file_formatter%close(rc=status)
      
      n = metadata%get_dimension('nodeCount', rc=status)
   end function get_nnodes

    integer function get_nelements(this) result(n)
       use pfio
       class(MeshGeomSpec), intent(in) :: this
       type(NetCDF4_FileFormatter) :: file_formatter
       type(FileMetadata) :: metadata
       integer :: status
       
       if (.not. allocated(this%filename)) then
          n = 0
          return
       end if
       
       call file_formatter%open(this%filename, pFIO_READ, rc=status)
       if (status /= 0) then
          n = 0
          return
       end if
       metadata = file_formatter%read(rc=status)
       call file_formatter%close(rc=status)
       
       n = metadata%get_dimension('elementCount', rc=status)
    end function get_nelements

     function get_filename(this) result(filename)
        character(len=:), allocatable :: filename
        class(MeshGeomSpec), intent(in) :: this
        if (allocated(this%filename)) then
           filename = this%filename
        end if
     end function get_filename

     subroutine set_filename(this, filename)
        class(MeshGeomSpec), intent(inout) :: this
        character(*), intent(in) :: filename
        this%filename = filename
     end subroutine set_filename

     pure function get_decomposition(this) result(decomposition)
      type(MeshDecomposition) :: decomposition
      class(MeshGeomSpec), intent(in) :: this
      decomposition = this%decomposition
   end function get_decomposition

   subroutine get_horz_ij_index_r4(this, lon, lat, ii, jj, rc)
      class(MeshGeomSpec), intent(in) :: this
      real(kind=ESMF_KIND_R4), intent(in) :: lon(:)
      real(kind=ESMF_KIND_R4), intent(in) :: lat(:)
      integer, allocatable, intent(out) :: ii(:)
      integer, allocatable, intent(out) :: jj(:)
      integer, optional, intent(out) :: rc

      integer :: status

      allocate(ii(1), jj(1), source=-1)
      _FAIL('get_horz_ij_index is not supported for MeshGeomSpec')

      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(lon)
      _UNUSED_DUMMY(lat)
   end subroutine get_horz_ij_index_r4

   subroutine get_horz_ij_index_r8(this, lon, lat, ii, jj, rc)
      class(MeshGeomSpec), intent(in) :: this
      real(kind=R8), intent(in) :: lon(:)
      real(kind=R8), intent(in) :: lat(:)
      integer, allocatable, intent(out) :: ii(:)
      integer, allocatable, intent(out) :: jj(:)
      integer, optional, intent(out) :: rc

      integer :: status

      allocate(ii(1), jj(1), source=-1)
      _FAIL('get_horz_ij_index is not supported for MeshGeomSpec')

      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(lon)
      _UNUSED_DUMMY(lat)
   end subroutine get_horz_ij_index_r8

end module mapl3g_MeshGeomSpec
