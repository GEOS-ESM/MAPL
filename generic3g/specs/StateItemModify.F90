#include "MAPL.h"

module mapl3g_StateItemModify
   use mapl3g_StateItemSpec
   use mapl3g_StateItemAspect
   use mapl3g_AspectId
   use mapl3g_GeomAspect
   use mapl3g_VerticalGridAspect
   use mapl3g_VerticalStaggerLoc
   use mapl3g_UnitsAspect
   use mapl3g_TypeKindAspect
   use mapl3g_VerticalGrid
   use mapl3g_FieldInfo, only: FieldInfoGetInternal
   use mapl3g_FieldBundleInfo, only: FieldBundleInfoGetInternal
   use mapl_KeywordEnforcer
   use mapl_ErrorHandling
   use esmf
   use, intrinsic :: iso_c_binding, only: c_ptr, c_f_pointer
   implicit none(type,external)
   private

   public :: MAPL_FieldModify
   public :: MAPL_FieldBundleModify

   interface MAPL_FieldModify
      procedure :: field_modify
   end interface MAPL_FieldModify

   interface MAPL_FieldBundleModify
      procedure :: bundle_modify
   end interface MAPL_FieldBundleModify


contains


   subroutine field_modify(field, unusable, geom, vertical_grid, vertical_stagger, units, typekind, rc)
      type(ESMF_FieldBundle), intent(inout) :: field
      class(KeywordEnforcer), optional, intent(in) :: unusable
      type(ESMF_Geom), optional, intent(in) :: geom
      class(VerticalGrid), optional, intent(in) :: vertical_grid
      type(VerticalStaggerLoc), optional, intent(in) :: vertical_stagger
      character(*), optional, intent(in) :: units
      type(ESMF_TypeKind_Flag), optional, intent(in) :: typekind
      integer, optional, intent(out) :: rc

      integer :: status
      integer, allocatable :: spec_handle(:)
      type(ESMF_Info) :: info

      call ESMF_InfoGetFromHost(field, info, _RC)
      call FieldInfoGetInternal(info, spec_handle=spec_handle, _RC)

      call stateitem_modify(spec_handle, geom=geom, vertical_grid=vertical_grid, vertical_stagger=vertical_stagger, units=units, typekind=typekind, _RC)

   end subroutine field_modify


   subroutine bundle_modify(fieldbundle, unusable, geom, vertical_grid, vertical_stagger, units, typekind, rc)
      type(ESMF_FieldBundle), intent(inout) :: fieldbundle
      class(KeywordEnforcer), optional, intent(in) :: unusable
      type(ESMF_Geom), optional, intent(in) :: geom
      class(VerticalGrid), optional, intent(in) :: vertical_grid
      type(VerticalStaggerLoc), optional, intent(in) :: vertical_stagger
      character(*), optional, intent(in) :: units
      type(ESMF_TypeKind_Flag), optional, intent(in) :: typekind
      integer, optional, intent(out) :: rc

      integer :: status
      integer, allocatable :: spec_handle(:)
      type(ESMF_Info) :: info

      call ESMF_InfoGetFromHost(fieldbundle, info, _RC)
      call FieldBundleInfoGetInternal(info, spec_handle=spec_handle, _RC)

      call stateitem_modify(spec_handle, geom=geom, vertical_grid=vertical_grid, vertical_stagger=vertical_stagger, units=units, typekind=typekind, _RC)

   end subroutine bundle_modify

   subroutine stateitem_modify(spec_handle, unusable, geom, vertical_grid, vertical_stagger, units, typekind, rc)
      integer, intent(in) :: spec_handle(:)
      class(KeywordEnforcer), optional, intent(in) :: unusable
      type(ESMF_Geom), optional, intent(in) :: geom
      class(VerticalGrid), optional, intent(in) :: vertical_grid
      type(VerticalStaggerLoc), optional, intent(in) :: vertical_stagger
      character(*), optional, intent(in) :: units
      type(ESMF_TypeKind_Flag), optional, intent(in) :: typekind
      integer, optional, intent(out) :: rc

      integer :: status
      type(c_ptr) :: spec_cptr
      type(StateItemSpec), pointer :: spec
      class(StateItemAspect), pointer :: aspect

      spec_cptr = transfer(spec_handle, spec_cptr)
      call c_f_pointer(spec_cptr, spec)

      if (present(geom)) then
         aspect => spec%get_aspect(GEOM_ASPECT_ID)
         select type(aspect)
         type is (GeomAspect)
            call aspect%set_geom(geom)
         class default
            _FAIL('incorrect aspect')
         end select
      end if

      if (present(vertical_grid)) then
         aspect => spec%get_aspect(VERTICAL_GRID_ASPECT_ID)
         if (.not. associated(aspect)) then
            _FAIL('null aspect pointer for VERTICAL_GRID_ASPECT_ID')
         end if
         select type(aspect)
         type is (VerticalGridAspect)
            call aspect%set_vertical_grid(vertical_grid)
         class default
            _FAIL('Expected VerticalGridAspect but got different type')
         end select
      end if

      if (present(vertical_stagger)) then
         aspect => spec%get_aspect(VERTICAL_GRID_ASPECT_ID)
         _ASSERT(associated(aspect), 'null aspect pointer for VERTICAL_GRID_ASPECT_ID')
         select type(aspect)
         type is (VerticalGridAspect)
            call aspect%set_vertical_stagger(vertical_stagger)
         class default
            _FAIL('Expected VerticalGridAspect but got different type')
         end select
      end if

      if (present(units)) then
         aspect => spec%get_aspect(UNITS_ASPECT_ID)
         select type(aspect)
         type is (UnitsAspect)
            call aspect%set_units(units)
         class default
            _FAIL('incorrect aspect')
         end select
      end if

      if (present(typekind)) then
         aspect => spec%get_aspect(TYPEKIND_ASPECT_ID)
         select type (aspect)
         type is (TypeKindAspect)
            call aspect%set_typekind(typekind)
         class default
            _FAIL('incorrect aspect')
         end select
      end if

   end subroutine stateitem_modify

end module mapl3g_StateItemModify
