#include "MAPL_ErrLog.h"

module mapl3g_MaplGeom
   use mapl3g_GeomSpec
   use mapl3g_VectorBasis
   use mapl_ErrorHandlingMod
   use pfio_FileMetadataMod, only: FileMetadata
   use ESMF, only: ESMF_Geom
   use ESMF, only: ESMF_Info
   use ESMF, only: ESMF_InfoGetFromHost
   use ESMF, only: ESMF_InfoSet
   use gftl2_StringVector
   implicit none
   private

   public :: MaplGeom

   ! The bases are expensive, and not always needed.  So we use lazy
   ! initialization to fill upon request.
   type VectorBases
      type(VectorBasis), allocatable :: NS_basis ! inverse is transpose
      type(VectorBasis), allocatable :: NS_basis_inverse
      type(VectorBasis), allocatable :: grid_basis
      type(VectorBasis), allocatable :: grid_basis_inverse
   end type VectorBases

   ! MaplGeom encapsulates an ESMF Geom object and various items associated
   ! with that object.
   type :: MaplGeom
      private
      class(GeomSpec), allocatable :: spec
      type(ESMF_Geom) :: geom
      type(FileMetadata) :: file_metadata
      type(StringVector) :: gridded_dims ! center staggered

      ! Derived - lazy initialization
      type(VectorBases) :: bases
   contains
      procedure :: set_id
      procedure :: get_spec
      procedure :: get_geom
!!$      procedure :: get_grid
      procedure :: get_file_metadata
!!$      procedure :: get_gridded_dims

      ! Only used by regridder
      procedure :: get_basis
   end type MaplGeom

   interface MaplGeom
      procedure :: new_MaplGeom
   end interface MaplGeom

contains

   function new_MaplGeom(spec, geom, file_metadata, gridded_dims) result(mapl_geom)
      class(GeomSpec), intent(in) :: spec
      type(MaplGeom) :: mapl_geom
      type(ESMF_Geom), intent(in) :: geom
      type(FileMetadata), optional, intent(in) :: file_metadata
      type(StringVector), optional, intent(in) :: gridded_dims

      mapl_geom%spec = spec
      mapl_geom%geom = geom
      if (present(file_metadata)) mapl_geom%file_metadata = file_metadata
      if (present(gridded_dims)) mapl_geom%gridded_dims = gridded_dims

   end function new_MaplGeom

   subroutine set_id(this, id, rc)
      class(MaplGeom), intent(inout) :: this
      integer, intent(in) :: id
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_Info) :: infoh

      call ESMF_InfoGetFromHost(this%geom, infoh, _RC)
      call ESMF_InfoSet(infoh, 'MAPL::id', id, _RC)
      
      _RETURN(_SUCCESS)
   end subroutine set_id

   function get_spec(this) result(spec)
      class(GeomSpec), allocatable :: spec
      class(MaplGeom), intent(in) :: this
      spec = this%spec
   end function get_spec

   function get_geom(this) result(geom)
      type(ESMF_Geom) :: geom
      class(MaplGeom), intent(in) :: this
      geom = this%geom
   end function get_geom

   function get_file_metadata(this) result(file_metadata)
     type(FileMetadata) :: file_metadata
      class(MaplGeom), intent(in) :: this
      file_metadata = this%file_metadata
   end function get_file_metadata

   recursive function get_basis(this, mode, rc) result(basis)
      type(VectorBasis), pointer :: basis
      class(MaplGeom), target, intent(inout) :: this
      character(len=*), optional, intent(in) :: mode
      integer, optional, intent(out) :: rc

      integer :: status

      select case (mode)

      case ('NS') ! Inverse is transpose, so no neeed for separate case
         if (.not. allocated(this%bases%ns_basis)) then
            this%bases%ns_basis = NS_VectorBasis(this%geom, _RC)
         end if
         basis => this%bases%ns_basis

      case ('NS_inverse') ! Inverse is transpose, so no neeed for separate case
         if (.not. allocated(this%bases%ns_basis_inverse)) then
            ! shallow copy of ESMF_Field components
            this%bases%ns_basis_inverse = this%get_basis('NS', _RC) 
         end if
         basis => this%bases%ns_basis_inverse

      case ('grid')
          if (.not. allocated(this%bases%grid_basis)) then
              this%bases%grid_basis = GridVectorBasis(this%geom, _RC)
          end if
          basis => this%bases%grid_basis

      case ('grid_inverse')
          if (.not. allocated(this%bases%grid_basis_inverse)) then
              this%bases%grid_basis_inverse = GridVectorBasis(this%geom, inverse=.true., _RC)
          end if
          basis => this%bases%grid_basis_inverse

      case default
         basis => null()
         _FAIL('Unsupported mode for get_bases().')
      end select

      _RETURN(_SUCCESS)
   end function get_basis

end module mapl3g_MaplGeom

