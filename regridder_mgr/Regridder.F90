#include "MAPL_Generic.h"

module mapl3g_Regridder
   use esmf
   use mapl_FieldUtils
   use mapl3g_FieldBundleGet
   use mapl_ErrorHandlingMod
   use mapl3g_geom_mgr
   use mapl3g_RegridderSpec
   use mapl3g_VectorBasis
   implicit none(type,external)
   private

   public :: Regridder

   type, abstract :: Regridder
      private
      type(GeomManager), pointer :: geom_manager => null()
   contains
      procedure(I_regrid_scalar), deferred :: regrid_scalar
      procedure, non_overridable :: regrid_vector
      generic :: regrid => regrid_scalar
      generic :: regrid => regrid_vector

      procedure :: get_geom_manager => get_geom_mgr
      procedure :: set_geom_manager
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

   subroutine regrid_vector(this, fb_in, fb_out, rc)
      class(Regridder), intent(inout) :: this
      type(ESMF_FieldBundle), intent(inout) :: fb_in, fb_out
      integer, optional, intent(out) :: rc

      type(ESMF_Field), allocatable :: uv_in(:), uv_out(:)
      type(ESMF_Field) :: xyz_in(3), xyz_out(3)
      integer :: status
      integer :: i
      integer :: id_in, id_out
      type(MaplGeom), pointer :: mapl_geom
      type(VectorBasis), pointer :: basis
      type(GeomManager), pointer :: geom_mgr
      type(ESMF_Field), allocatable :: transpose_basis(:,:)
      type(ESMF_Geom) :: geom_in, geom_out

      call MAPL_FieldBundleGet(fb_in, fieldList=uv_in, _RC)
      call MAPL_FieldBundleGet(fb_out, fieldList=uv_out, _RC)

      _ASSERT(size(uv_in) == 2, 'TangentVector must consiste of exactly 2 fields.')
      _ASSERT(size(uv_out) == 2, 'TangentVector must consiste of exactly 2 fields.')

      call create_field_vector(archetype=uv_in(1), fv=xyz_in, _RC)
      call create_field_vector(archetype=uv_out(1), fv=xyz_out, _RC)

      geom_mgr => this%get_geom_manager()

      call ESMF_FieldGet(uv_in(1), geom=geom_in, _RC)
      id_in = MAPL_GeomGetId(geom_in, _RC)
      mapl_geom => geom_mgr%get_mapl_geom(id_in, _RC)
      basis => mapl_geom%get_basis('NS', _RC)

      call FieldGEMV('N', 1., basis%elements, uv_in, 0., xyz_in, _RC)

      ! Regrid component-by-component
      do i = 1, 3 
         call this%regrid(xyz_in(i), xyz_out(i), _RC)
      end do

      call ESMF_FieldGet(uv_out(1), geom=geom_out, _RC)
      id_out = MAPL_GeomGetId(geom_out, _RC)
      mapl_geom => geom_mgr%get_mapl_geom(id_out, _RC)
      basis => mapl_geom%get_basis('NS', _RC)
      call FieldGEMV('T', 1., basis%elements, xyz_out, 0., uv_out, _RC)

      call destroy_field_vector(xyz_in, _RC)
      call destroy_field_vector(xyz_out, _RC)

      _RETURN(_SUCCESS)
   end subroutine regrid_vector

   subroutine create_field_vector(archetype, fv, rc)
      type(ESMF_Field), intent(inout) :: archetype
      type(ESMF_Field), intent(out) :: fv(:)
      integer, optional, intent(out) :: rc

      integer :: i
      integer :: status

      do i = 1, size(fv)
         call FieldClone(archetype, fv(i), _RC)
      end do

      _RETURN(_SUCCESS)
   end subroutine create_field_vector

   subroutine destroy_field_vector(fv, rc)
      type(ESMF_Field), intent(inout) :: fv(:)
      integer, optional, intent(out) :: rc

      integer :: i
      integer :: status

      do i = 1, size(fv)
         call ESMF_FieldDestroy(fv(i), noGarbage=.true., _RC)
      end do

      _RETURN(_SUCCESS)
   end subroutine destroy_field_vector

   subroutine set_geom_manager(this, geom_manager)
      class(Regridder), intent(inout) :: this
      type(GeomManager), pointer, intent(in) :: geom_manager
      this%geom_manager => geom_manager
   end subroutine set_geom_manager

   function get_geom_mgr(this) result(geom_manager)
      type(GeomManager), pointer :: geom_manager
      class(Regridder), intent(in) :: this
      geom_manager => this%geom_manager
   end function get_geom_mgr

end module mapl3g_Regridder
      
