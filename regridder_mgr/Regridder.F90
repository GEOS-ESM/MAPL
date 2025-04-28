#include "MAPL_Generic.h"

module mapl3g_Regridder
   use esmf
   use mapl_FieldUtils
   use mapl3g_FieldBundle_API
   use mapl_ErrorHandlingMod
   use mapl3g_Geom_API
   use mapl3g_RegridderSpec
   use mapl3g_VectorBasis
   implicit none(type,external)
   private

   public :: Regridder

   type, abstract :: Regridder
      private
      type(GeomManager), pointer :: geom_manager => null()
   contains
      procedure(I_regrid_field), deferred :: regrid_field
      procedure, non_overridable :: regrid_fieldbundle
      generic :: regrid => regrid_field
      generic :: regrid => regrid_fieldbundle

      procedure, non_overridable :: regrid_basic_bundle
      procedure, non_overridable :: regrid_vector

      procedure :: get_geom_manager => get_geom_mgr
      procedure :: set_geom_manager
   end type Regridder

   abstract interface
      subroutine I_regrid_field(this, f_in, f_out, rc)
         use esmf, only: ESMF_Field
         import Regridder
         class(Regridder), intent(inout) :: this
         type(ESMF_Field), intent(inout) :: f_in
         type(ESMF_Field), intent(inout) :: f_out
         integer, optional, intent(out) :: rc
      end subroutine I_regrid_field

   end interface

contains

   subroutine regrid_fieldbundle(this, fb_in, fb_out, rc)
      class(Regridder), intent(inout) :: this
      type(ESMF_FieldBundle), intent(inout) :: fb_in, fb_out
      integer, optional, intent(out) :: rc

      integer :: status
      type(FieldBundleType_Flag) :: bundleType_in, bundleType_out

      call MAPL_FieldBundleGet(fb_in, fieldBundleType=bundleType_in, _RC)
      call MAPL_FieldBundleGet(fb_out, fieldBundleType=bundleType_out, _RC)
      _ASSERT(bundleType_out == bundleType_in, 'Bundle types must match.')

      if (bundleType_in == FIELDBUNDLETYPE_VECTOR) then
         call this%regrid_vector(fb_in, fb_out, _RC)
         _RETURN(_SUCCESS)
      end if

      call this%regrid_basic_bundle(fb_in, fb_out, _RC)

      _RETURN(_SUCCESS)
   end subroutine regrid_fieldbundle

   subroutine regrid_basic_bundle(this, fb_in, fb_out, rc)
      class(Regridder), intent(inout) :: this
      type(ESMF_FieldBundle), intent(inout) :: fb_in, fb_out
      integer, optional, intent(out) :: rc

      type(ESMF_Field), allocatable :: fieldList_in(:), fieldList_out(:)
      integer :: status
      integer :: i

      call MAPL_FieldBundleGet(fb_in, fieldList=fieldList_in, _RC)
      call MAPL_FieldBundleGet(fb_out, fieldList=fieldList_out, _RC)

      _ASSERT(size(fieldList_out) == size(fieldList_in), 'Brackets must have same size.')

      do i = 1, size(fieldList_in)
         call this%regrid(fieldList_in(i), fieldList_out(i), _RC)
      end do

      _RETURN(_SUCCESS)
   end subroutine regrid_basic_bundle

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
      type(ESMF_Geom) :: geom_in, geom_out

      _HERE
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
      
