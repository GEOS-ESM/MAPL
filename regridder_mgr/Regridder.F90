#include "MAPL_Generic.h"

module mapl3g_Regridder
   use esmf
   use mapl_ErrorHandlingMod
   use mapl3g_geom_mgr
   use mapl3g_RegridderSpec
   use mapl3g_VectorBasis
   implicit none
   private

   public :: Regridder

   type, abstract :: Regridder
      private
      class(RegridderSpec), allocatable :: spec
   contains
      procedure(I_regrid_scalar), deferred :: regrid_scalar
      procedure, non_overridable :: regrid_vector
      generic :: regrid => regrid_scalar
      generic :: regrid => regrid_vector

!!$      procedure :: set_spec
!!$      procedure :: get_spec
   end type Regridder

   abstract interface
      subroutine I_regrid_scalar(this, f_in, f_out, rc)
         use esmf, only: ESMF_Field
         import Regridder
         class(Regridder), intent(inout) :: this
         type(ESMF_Field), intent(inout) :: f_in
         type(ESMF_Field), intent(inout) :: f_out
         integer, optional, intent(out) :: rc
      end subroutine I_regrid_scalar
   end interface

contains

   subroutine regrid_vector(this, fv_in, fv_out, rc)
      class(Regridder), intent(inout) :: this
      type(ESMF_Field), intent(inout) :: fv_in(2), fv_out(2)
      integer, optional, intent(out) :: rc

      type(ESMF_Field) :: xyz_in(3), xyz_out(3)
      integer :: status
      integer :: i
      type(MaplGeom), pointer :: mapl_geom
      type(VectorBasis), pointer :: basis

!!$      _ASSERT(FieldsAreConformable(fv_in, fv_out), 'Incompatible vectors for regrid.')
!!$      call create_field_vector(xyz_in, template=fv_in(1), _RC)
!!$      call create_field_vector(xyz_out, template=fv_out(1), _RC)

!!$      mapl_geom => geom_manager%get_mapl_geom(this%spec%geom_id_out)
      basis => mapl_geom%get_basis('NS')
!!$      call FieldGEMV('N', basis, fv_in, xyz_in, _RC)

      ! Regrid component-by-component
      do i = 1, 3 
         call this%regrid(xyz_in(i), xyz_out(i), _RC)
      end do

!!$      mapl_geom => geom_manager%get_mapl_geom(this%spec%id_grid_out)
      basis => mapl_geom%get_basis('NS_inverse')
!!$      call FieldGEMV('T', basis, xyz_out, fv_out, _RC)

      call destroy_field_vector(xyz_in, _RC)
      call destroy_field_vector(xyz_out, _RC)

      _RETURN(_SUCCESS)
   end subroutine regrid_vector

   subroutine create_field_vector(fv, f, rc)
      type(ESMF_Field), intent(out) :: fv(:)
      type(ESMF_Field), intent(in) :: f
      integer, optional, intent(out) :: rc

      integer :: i
      integer :: status

      do i = 1, size(fv)
!!$         call MAPL_CloneField(f, fv(i), _RC)
      end do

      _RETURN(_SUCCESS)
   end subroutine create_field_vector

   subroutine destroy_field_vector(fv, rc)
      type(ESMF_Field), intent(out) :: fv(:)
      integer, optional, intent(out) :: rc

      integer :: i
      integer :: status

      do i = 1, size(fv)
         call ESMF_FieldDestroy(fv(i), noGarbage=.true., _RC)
      end do

      _RETURN(_SUCCESS)
   end subroutine destroy_field_vector

end module mapl3g_Regridder
      
