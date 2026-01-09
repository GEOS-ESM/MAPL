#include "MAPL_ErrLog.h"

submodule (mapl3g_VectorBasis) new_GridVectorBasis_smod
   implicit none(type,external)
contains

   ! Valid only for grids.
   module function new_GridVectorBasis(geom, inverse, rc) result(basis)
      type(VectorBasis) :: basis
      type(ESMF_Geom), intent(inout) :: geom
      logical, optional, intent(in) :: inverse
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_Grid) :: grid
      type(ESMF_GeomType_Flag) :: geomtype
      logical :: inverse_
      integer :: i, j
      real(kind=ESMF_KIND_R8), allocatable :: centers(:,:,:)
      real(kind=ESMF_KIND_R8), allocatable :: corners(:,:,:)

      inverse_ = .false.
      if (present(inverse)) inverse_ = inverse

      call ESMF_GeomGet(geom, geomtype=geomtype, _RC)
      _ASSERT(geomtype == ESMF_GEOMTYPE_GRID, 'GridVectorBasis is only valid for ESMF_Grid geoms.')
      call ESMF_GeomGet(geom, grid=grid, _RC)

      call create_fields(basis%elements, geom, _RC)

      call GridGetCoords(grid, centers, _RC)
      call GridGetCorners(grid, corners, _RC)

      call fill_fields(basis, centers, corners, inverse_, _RC)

      _RETURN(ESMF_SUCCESS)
   contains

      subroutine fill_fields(basis, centers, corners, inverse, rc)
         type(VectorBasis), intent(inout) :: basis
         real(kind=ESMF_KIND_R8), intent(in) :: centers(:,:,:)
         real(kind=ESMF_KIND_R8), intent(in) :: corners(:,:,:)
         logical, intent(in) :: inverse
         integer, optional, intent(out) :: rc

         integer :: status
         integer :: k1, k2
         integer :: im, jm
         type(Ptr_2d) :: x(NI,NJ)

         im = size(centers,1)
         jm = size(centers,2)

         do k2 = 1, NJ
            do k1 = 1, NI
               call assign_fptr(basis%elements(k1,k2), int([im,jm],kind=ESMF_KIND_I8), x(k1,k2)%ptr, _RC)
            end do
         end do

         do concurrent (i=1:im, j=1:jm)
            associate (local_basis => fill_element(centers(i,j,:), corners(i:i+1,j+j+1,:), inverse) )
              
              do k2 = 1, NJ
                 do k1 = 1, NI
                    x(k1,k2)%ptr(i,j) = local_basis(k1,k2)
                 end do
              end do
            end associate
         end do
       
         _RETURN(ESMF_SUCCESS)
      end subroutine fill_fields
      !--------------------------------------
      !
      !   ^ lat
      !   !
      !   !         x c    p4     x  d
      !   !
      !   !
      !   !         p1     C       p3
      !   !
      !   !
      !   !         x a    p2     x  b
      !   !
      !   !
      !   !------------------------------> lon
      !
      !--------------------------------------

      pure function fill_element(center, corners, inverse) result(basis)
         real(kind=ESMF_KIND_R8), intent(in) :: center(2)
         real(kind=ESMF_KIND_R8), intent(in) :: corners(2,2,2) ! last dim is lat/lon
         logical, intent(in) :: inverse
         real(kind=ESMF_KIND_R8) :: basis(NI,2)

         associate ( &
              p1 => mid_pt_sphere(corners(1,1,:),corners(1,2,:)), &
              p2 => mid_pt_sphere(corners(1,1,:),corners(2,1,:)), &
              p3 => mid_pt_sphere(corners(2,1,:),corners(2,2,:)), &
              p4 => mid_pt_sphere(corners(1,2,:),corners(2,2,:)) )

           associate ( &
                e1 => get_unit_vector(p3, center, p1), &
                e2 => get_unit_vector(p4, center, p2) )

             if (.not. inverse) then
                basis(:,1) = e1
                basis(:,2) = e2
                return
             end if

             associate (dot => dot_product(e1, e2))
               basis(:,1) = (e1 - dot*e2) / (1-dot**2)
               basis(:,2) = (e2 - dot*e1) / (1-dot**2)
             end associate

           end associate
         end associate

      end function fill_element

   end function new_GridVectorBasis

end submodule new_GridVectorBasis_smod
