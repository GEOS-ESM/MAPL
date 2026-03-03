#include "MAPL_ErrLog.h"

submodule (mapl3g_VectorBasis) create_fields_smod
contains


   module subroutine create_fields(elements, geom, rc)
      type(ESMF_Field), intent(inout) :: elements(NI,NJ)
      type(ESMF_Geom), intent(in) :: geom
      integer, optional, intent(out) :: rc

      integer :: status
      integer :: i, j
      type(ESMF_GeomType_Flag) :: geomtype
      type(ESMF_Grid) :: grid
      type(ESMF_LocStream) :: locstream
      type(ESMF_Mesh) :: mesh

      
      
      call ESMF_GeomGet(geom, geomtype=geomtype, _RC)

      if (geomtype == ESMF_GEOMTYPE_GRID) then
         call ESMF_GeomGet(geom, grid=grid, _RC)
         do j = 1, nj
            do i = 1, ni
               elements(i,j) = ESMF_FieldCreate(grid, typekind=ESMF_TYPEKIND_R8, &
                    staggerloc=ESMF_STAGGERLOC_CENTER, _RC)
            end do
         end do
      elseif (geomtype == ESMF_GEOMTYPE_LOCSTREAM) then
         call ESMF_GeomGet(geom, locstream=locstream, _RC)
         do j = 1, nj
            do i = 1, ni
               elements(i,j) = ESMF_FieldCreate(locstream, typekind=ESMF_TYPEKIND_R8, _RC)
            end do
         end do
      elseif (geomtype == ESMF_GEOMTYPE_MESH) then
         call ESMF_GeomGet(geom, mesh=mesh, _RC)
         do j = 1, nj
            do i = 1, ni
               elements(i,j) = ESMF_FieldCreate(mesh, typekind=ESMF_TYPEKIND_R8, _RC)
            end do
         end do
      elseif (geomtype == ESMF_GEOMTYPE_XGRID) then
         _FAIL('Unsupported geomtype XGRID')
      else
         _FAIL('Unknown geomtype.')
      end if

      _RETURN(ESMF_SUCCESS)
   end subroutine create_fields

end submodule create_fields_smod
