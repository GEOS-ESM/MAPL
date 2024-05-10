#include "MAPL_ErrLog.h"

submodule (mapl3g_MaplGeom) get_basis_smod
   use mapl3g_GeomSpec
   use mapl3g_VectorBasis
   use mapl3g_GeomUtilities
   use mapl_ErrorHandlingMod
   use pfio_FileMetadataMod, only: FileMetadata
   use ESMF, only: ESMF_Info
   use ESMF, only: ESMF_InfoGetFromHost
   use ESMF, only: ESMF_InfoSet

contains
   
   recursive module function get_basis(this, mode, rc) result(basis)
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


end submodule get_basis_smod
