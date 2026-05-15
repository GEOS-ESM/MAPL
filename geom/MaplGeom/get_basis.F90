#include "MAPL.h"

submodule (mapl3g_MaplGeom) get_basis_smod

use mapl3g_GeomSpec
   use mapl3g_VectorBasis
   use mapl3g_VectorBasisKind
   use mapl3g_GeomUtilities
   use mapl_ErrorHandlingMod
   use pfio_FileMetadataMod, only: FileMetadata
   use ESMF, only: ESMF_Info
   use ESMF, only: ESMF_InfoGetFromHost
   use ESMF, only: ESMF_InfoSet

   implicit none(type,external)

contains

   ! Supports lazy initialization as vector regridding is relatively
   ! rare.
   recursive module function get_basis(this, basis_kind, rc) result(basis)
      type(VectorBasis), pointer :: basis
      class(MaplGeom), target, intent(inout) :: this
      type(VectorBasisKind), optional, intent(in) :: basis_kind
      integer, optional, intent(out) :: rc

      integer :: status

      if (basis_kind == VECTOR_BASIS_KIND_NS) then
         if (allocated(this%bases%ns_basis)) then
            basis => this%bases%ns_basis
            _RETURN(_SUCCESS)
         end if
         allocate(this%bases%ns_basis)
         this%bases%ns_basis = NS_VectorBasis(this%geom, _RC)
         basis => this%bases%ns_basis
         _RETURN(_SUCCESS)
      end if

      if (basis_kind == VECTOR_BASIS_KIND_GRID) then
         if (allocated(this%bases%grid_basis)) then
            basis => this%bases%grid_basis
            _RETURN(_SUCCESS)
         end if
         allocate(this%bases%grid_basis)
         this%bases%grid_basis = GridVectorBasis(this%geom, _RC)
         basis => this%bases%grid_basis
         _RETURN(_SUCCESS)
      end if

      basis => null()
      _FAIL('Unsupported basis kind: ' // basis_kind%to_string())
   end function get_basis

end submodule get_basis_smod
