#include "MAPL_Generic.h"

module mapl3g_FieldUtilities
   use MAPL_FieldPointerUtilities, only: FieldGetLocalElementCount
   use mapl_ErrorHandling

   use esmf
   implicit none
   private

   public :: MAPL_FieldReallocate

   interface MAPL_FieldReallocate
      procedure :: reallocate
   end interface MAPL_FieldReallocate

   interface operator(==)
      procedure :: ESMF_GeomEqual
   end interface operator(==)

   interface operator(/=)
      procedure :: ESMF_GeomNotEqual
   end interface operator(/=)

contains


   subroutine reallocate(field, geom, typekind, ungriddedUBound, rc)
      type(ESMF_Field), intent(inout) :: field
      type(ESMF_Geom), optional, intent(in) :: geom
      type(ESMF_TypeKind_Flag), optional, intent(in) :: typekind
      integer, optional, intent(in) :: ungriddedUBound(:)
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_TypeKind_Flag) :: old_typekind, typekind_
      type(ESMF_Geom) :: old_geom, geom_
      logical :: skip_reallocate
      integer :: ungriddedDimCount, rank
      integer, allocatable :: localElementCount(:)
      integer, allocatable :: old_ungriddedUBound(:)
      integer, allocatable :: ungriddedUBound_(:), ungriddedLBound_(:)
      integer :: i

      skip_reallocate = .true.

      call ESMF_FieldGet(field, typekind=old_typekind, geom=old_geom, ungriddedDimCount=ungriddedDimCount, rank=rank, _RC)
      localElementCount = FieldGetLocalElementCount(field, _RC)
      old_ungriddedUBound = localElementCount(rank-ungriddedDimCount+1:)

      typekind_ = old_typekind
      if (present(typekind)) typekind_ = typekind

      geom_ = old_geom
      if (present(geom)) geom_ = geom

      ungriddedUBound_ = old_ungriddedUBound
      if (present(ungriddedUBound)) ungriddedUBound_ = ungriddedUBound
      _ASSERT(size(ungriddedUBound_) == size(old_ungriddedUBound), 'MAPL does not allow the rank of a field to change after creation.')
      
      if (typekind_ /= old_typekind) skip_reallocate = .false.
      if (geom_ /= old_geom) skip_reallocate = .false.
      if (any(ungriddedUBound_ /= old_ungriddedUBound)) skip_reallocate = .false.
      _RETURN_IF(skip_reallocate)

      field%ftypep%status = ESMF_FIELDSTATUS_GRIDSET

      call ESMF_ArrayDestroy(field%ftypep%array, _RC)

      call ESMF_FieldEmptySet(field, geom=geom_, _RC)
      ungriddedLBound_ = [(1, i=1, size(ungriddedUBound_))]
      call ESMF_FieldEmptyComplete(field, typekind=typekind_, ungriddedLBound=ungriddedLBound_, ungriddedUbound=ungriddedUBound_, _RC)


      _RETURN(_SUCCESS)
   end subroutine reallocate


   impure elemental logical function ESMF_GeomEqual(geom1, geom2)
      type(ESMF_Geom), intent(in) :: geom1, geom2

      type(ESMF_GeomType_Flag) :: geomtype1, geomtype2
      type(ESMF_Grid) :: grid1, grid2
      type(ESMF_LocStream) :: locstream1, locstream2
      type(ESMF_Mesh) :: mesh1, mesh2
      type(ESMF_XGrid) :: xgrid1, xgrid2
      
      ESMF_GeomEqual = .false.

      call ESMF_GeomGet(geom1, geomtype=geomtype1)
      call ESMF_GeomGet(geom2, geomtype=geomtype2)

      if (geomtype1 /= geomtype2) return
      
      if (geomtype1 == ESMF_GEOMTYPE_GRID) then
         call ESMF_GeomGet(geom1, grid=grid1)
         call ESMF_GeomGet(geom2, grid=grid2)
         ESMF_GeomEqual = (grid1 == grid2)
         return
      end if

      if (geomtype1 == ESMF_GEOMTYPE_LOCSTREAM) then
         call ESMF_GeomGet(geom1, locstream=locstream1)
         call ESMF_GeomGet(geom2, locstream=locstream2)
         ESMF_GeomEqual = (locstream1 == locstream2)
         return
      end if

      if (geomtype1 == ESMF_GEOMTYPE_MESH) then
         call ESMF_GeomGet(geom1, mesh=mesh1)
         call ESMF_GeomGet(geom2, mesh=mesh2)
         ESMF_GeomEqual = (mesh1 == mesh2)
         return
      end if

      if (geomtype1 == ESMF_GEOMTYPE_XGRID) then
         call ESMF_GeomGet(geom1, xgrid=xgrid1)
         call ESMF_GeomGet(geom2, xgrid=xgrid2)
         ESMF_GeomEqual = (xgrid1 == xgrid2)
         return
      end if
      
   end function ESMF_GeomEqual


   impure elemental logical function ESMF_GeomNotEqual(geom1, geom2)
      type(ESMF_Geom), intent(in) :: geom1, geom2
      ESMF_GeomNotEqual = .not. (geom1 == geom2)
   end function ESMF_GeomNotEqual

end module mapl3g_FieldUtilities
