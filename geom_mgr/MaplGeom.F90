#include "MAPL_ErrLog.h"

module mapl3g_MaplGeom
   use mapl3g_GeomSpec
   use mapl3g_VectorBasis
   use pfio_FileMetadataMod, only: FileMetadata
   use ESMF, only: ESMF_Geom
   use gftl_StringVector
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

   interface 
      module function new_MaplGeom(spec, geom, file_metadata, gridded_dims) result(mapl_geom)
         class(GeomSpec), intent(in) :: spec
         type(MaplGeom) :: mapl_geom
         type(ESMF_Geom), intent(in) :: geom
         type(FileMetadata), optional, intent(in) :: file_metadata
         type(StringVector), optional, intent(in) :: gridded_dims
      end function new_MaplGeom

      module subroutine set_id(this, id, rc)
         class(MaplGeom), intent(inout) :: this
         integer, intent(in) :: id
         integer, optional, intent(out) :: rc
      end subroutine set_id

      module function get_spec(this) result(spec)
         class(GeomSpec), allocatable :: spec
         class(MaplGeom), intent(in) :: this
      end function get_spec

      module function get_geom(this) result(geom)
         type(ESMF_Geom) :: geom
         class(MaplGeom), intent(in) :: this
      end function get_geom

      module function get_file_metadata(this) result(file_metadata)
         type(FileMetadata) :: file_metadata
         class(MaplGeom), intent(in) :: this
      end function get_file_metadata

      recursive module function get_basis(this, mode, rc) result(basis)
         type(VectorBasis), pointer :: basis
         class(MaplGeom), target, intent(inout) :: this
         character(len=*), optional, intent(in) :: mode
         integer, optional, intent(out) :: rc
      end function get_basis

   end interface

end module mapl3g_MaplGeom


