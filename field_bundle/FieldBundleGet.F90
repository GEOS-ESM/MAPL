#include "MAPL_Generic.h"

module mapl3g_FieldBundleGet
   use mapl_KeywordEnforcer
   use mapl_ErrorHandling
   use mapl3g_Field_private_API
   use mapl3g_Field_api
   use mapl3g_InfoUtilities
   use mapl3g_LU_Bound
   use esmf
   implicit none
   private

   public :: MAPL_FieldBundleGet
   public :: MAPL_FieldBundleSet


   interface MAPL_FieldBundleGet
      procedure bundle_get
   end interface MAPL_FieldBundleGet

   interface MAPL_FieldBundleSet
      procedure bundle_set
   end interface MAPL_FieldBundleSet

contains

   ! Supplement ESMF
   subroutine bundle_get(fieldBundle, unusable, fieldList, geom, typekind, ungriddedUbound, rc)
      type(ESMF_FieldBundle), intent(in) :: fieldBundle
      class(KeywordEnforcer), optional, intent(in) :: unusable
      type(ESMF_Field), optional, allocatable, intent(out) :: fieldList(:)
      type(ESMF_Geom), optional, intent(out) :: geom
      type(ESMF_TypeKind_Flag), optional, intent(out) :: typekind
      integer, allocatable, optional, intent(out) :: ungriddedUbound(:)
      integer, optional, intent(out) :: rc

      integer :: status
      integer :: fieldCount
      type(ESMF_GeomType_Flag) :: geomtype
      character(:), allocatable :: typekind_str
      type(ESMF_Info) :: ungridded_info
      type(UngriddedDims) :: ungridded_dims
      type(LU_Bound), allocatable :: bounds(:)
      integer :: num_levels
      character(:), allocatable :: vert_staggerloc

      if (present(fieldList)) then
         call ESMF_FieldBundleGet(fieldBundle, fieldCount=fieldCount, _RC)
         allocate(fieldList(fieldCount))
         call ESMF_FieldBundleGet(fieldBundle, fieldList=fieldList, itemOrderflag=ESMF_ITEMORDER_ADDORDER, _RC)
      end if

      if (present(geom)) then
         call get_geom(fieldBundle, geom, rc)
      end if

      if (present(typekind)) then
         call MAPL_InfoGetInternal(fieldBundle, key=KEY_TYPEKIND, value=typekind_str, _RC)
         select case (typekind_str)
         case ('R4')
            typekind = ESMF_TYPEKIND_R4
         case ('R8')
            typekind = ESMF_TYPEKIND_R8
         case ('I4')
            typekind = ESMF_TYPEKIND_I4
         case ('I8')
            typekind = ESMF_TYPEKIND_I8
         case ('LOGICAL')
            typekind = ESMF_TYPEKIND_LOGICAL
         case default
            _FAIL('unsupported typekind')
         end select
      end if

       if (present(ungriddedUbound)) then
         ungridded_info = MAPL_InfoCreateFromInternal(fieldBundle, _RC)
         ungridded_dims =  make_ungriddedDims(ungridded_info, KEY_UNGRIDDED_DIMS, _RC)
         bounds = ungridded_dims%get_bounds()

         call MAPL_InfoGetInternal(fieldBundle, key=KEY_VERT_STAGGERLOC, value=vert_staggerloc, _RC)
         if (vert_staggerloc /= 'VERTICAL_STAGGER_NONE') then
            call MAPL_InfoGetInternal(fieldBundle, key=KEY_NUM_LEVELS, value=num_levels, _RC)
            bounds = [LU_Bound(1, num_levels), bounds]
         end if
         ungriddedUbound = bounds%upper
      end if

      _RETURN(_SUCCESS)

   contains

      subroutine get_geom(fieldBundle, geom, rc)
         type(ESMF_FieldBundle), intent(in) :: fieldBundle
         type(ESMF_Geom), intent(inout) :: geom
         integer, optional, intent(out) :: rc

         integer :: status
         type(ESMF_GeomType_Flag) :: geomtype
         type(ESMF_Grid) :: grid

         call ESMF_FieldBundleGet(fieldBundle, geomtype=geomtype, _RC)
         if (geomtype == ESMF_GEOMTYPE_GRID) then
            call ESMF_FieldBundleGet(fieldBundle, grid=grid, _RC)
            ! memory leak
            geom = ESMF_GeomCreate(grid=grid, _RC)
            _RETURN(_SUCCESS)
         end if

         _FAIL('unsupported geomtype; needs simple extension')

         _RETURN(_SUCCESS)
      end subroutine get_geom

   end subroutine bundle_get

   subroutine bundle_set(fieldBundle, unusable, geom, rc)
      type(ESMF_FieldBundle), intent(inout) :: fieldBundle
      class(KeywordEnforcer), optional, intent(in) :: unusable
      type(ESMF_Geom), optional, intent(in) :: geom
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_GeomType_Flag) :: geomtype
      type(ESMF_Grid) :: grid

      if (present(geom)) then
         call ESMF_GeomGet(geom, geomtype=geomtype, _RC)
         if (geomtype == ESMF_GEOMTYPE_GRID) then
            call ESMF_GeomGet(geom, grid=grid, _RC)
            call ESMF_FieldBundleSet(fieldBundle, grid=grid, _RC)
            _RETURN(_SUCCESS)
         end if
         _FAIL('unsupported geomtype')
      end if

      _RETURN(_SUCCESS)
   end subroutine Bundle_Set


end module mapl3g_FieldBundleGet
