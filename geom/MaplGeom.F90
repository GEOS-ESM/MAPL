#include "MAPL_ErrLog.h"

module mapl3g_MaplGeom
   use mapl3g_GeomSpec
   use mapl3g_VectorBasis
   use mapl3g_GeomFactory
   use pfio_FileMetadataMod, only: FileMetadata
   use ESMF, only: ESMF_Geom
   use gftl2_StringVector
   use gftl2_StringStringMap
   implicit none
   private

   public :: MaplGeom

   ! The bases are expensive, and not always needed.  So we use lazy
   ! initialization to fill upon request.
   type VectorBases
      type(VectorBasis), allocatable :: NS_basis ! inverse is transpose
      type(VectorBasis), allocatable :: grid_basis
   end type VectorBases

   ! MaplGeom encapsulates an ESMF Geom object and various items associated
   ! with that object.
   type :: MaplGeom
      private
      class(GeomSpec), allocatable :: spec
      type(ESMF_Geom) :: geom
      class(GeomFactory), allocatable :: factory
      type(FileMetadata) :: file_metadata
      type(StringVector) :: gridded_dims ! center staggered
      type(StringStringMap) :: variable_attributes

      ! Derived - lazy initialization
      type(VectorBases) :: bases
   contains
      procedure :: set_id
      procedure :: get_spec
      procedure :: get_geom
      procedure :: get_factory
!!$      procedure :: get_grid
      procedure :: get_file_metadata
      procedure :: get_gridded_dims
      procedure :: get_variable_attributes

      ! Only used by regridder
      procedure :: get_basis
   end type MaplGeom

   interface MaplGeom
      procedure :: new_MaplGeom
   end interface MaplGeom

   interface 
      module function new_MaplGeom(spec, geom, factory, file_metadata, gridded_dims, variable_attributes) result(mapl_geom)
         class(GeomSpec), intent(in) :: spec
         type(MaplGeom) :: mapl_geom
         type(ESMF_Geom), intent(in) :: geom
         class(GeomFactory), intent(in) :: factory
         type(FileMetadata), optional, intent(in) :: file_metadata
         type(StringVector), optional, intent(in) :: gridded_dims
         type(StringStringMap), optional, intent(in) :: variable_attributes
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

      module function get_factory(this) result(factory)
         class(GeomFactory), allocatable :: factory
         class(MaplGeom), intent(in) :: this
      end function get_factory

      module function get_file_metadata(this) result(file_metadata)
         type(FileMetadata) :: file_metadata
         class(MaplGeom), intent(in) :: this
      end function get_file_metadata

      module function get_gridded_dims(this) result(gridded_dims)
         type(StringVector) :: gridded_dims
         class(MaplGeom), intent(in) :: this
      end function get_gridded_dims

      module function get_variable_attributes(this) result(variable_attributes)
         type(StringStringMap) :: variable_attributes
         class(MaplGeom), intent(in) :: this
      end function get_variable_attributes

      recursive module function get_basis(this, mode, rc) result(basis)
         type(VectorBasis), pointer :: basis
         class(MaplGeom), target, intent(inout) :: this
         character(len=*), optional, intent(in) :: mode
         integer, optional, intent(out) :: rc
      end function get_basis

   end interface

end module mapl3g_MaplGeom


