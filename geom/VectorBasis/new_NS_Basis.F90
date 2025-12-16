#include "MAPL_ErrLog.h"

submodule (mapl3g_VectorBasis) new_NS_Basis_smod
contains


   module function new_NS_Basis(geom, rc) result(basis)
      type(VectorBasis) :: basis
      type(ESMF_Geom), intent(inout) :: geom
      integer, optional, intent(out) :: rc

      integer :: status
      real(kind=ESMF_KIND_R8), pointer :: longitudes(:)
      real(kind=ESMF_KIND_R8), pointer :: latitudes(:)

      allocate(basis%elements(NI,NJ))
      call create_fields(basis%elements, geom, _RC)
      call MAPL_GeomGetCoords(geom, longitudes, latitudes, _RC)
      call fill_fields(basis, longitudes, latitudes, _RC)

      _RETURN(ESMF_SUCCESS)

   contains

      subroutine fill_fields(basis, longitudes, latitudes, rc)
         type(VectorBasis), intent(inout) :: basis
         real(kind=ESMF_KIND_R8), intent(in) :: longitudes(:)
         real(kind=ESMF_KIND_R8), intent(in) :: latitudes(:)
         integer, optional, intent(out) :: rc

         integer :: status
         type(Ptr_1d) :: x(NI,NJ)
         integer :: i, j, n
         real(kind=ESMF_KIND_R8) :: local_basis(NI,NJ)

         real(kind=ESMF_KIND_R8) :: global_min(NI,NJ)
         real(kind=ESMF_KIND_R8) :: global_max(NI,NJ)

         global_min = huge(1.)
         global_max = -huge(1.)
         
         do j = 1, NJ
            do i = 1, NI
               call assign_fptr(basis%elements(i,j), x(i,j)%ptr, _RC)
            end do
         end do

         do n = 1, size(x(1,1)%ptr)
            local_basis = fill_element(longitudes(n), latitudes(n))

            do j = 1, NJ
               do i = 1, NI
                  x(i,j)%ptr(n) = local_basis(i,j)
               end do
            end do

            global_min = min(global_min, local_basis)
            global_max = max(global_max, local_basis)

         end do

         _RETURN(ESMF_SUCCESS)
      end subroutine fill_fields

      pure function fill_element(longitude, latitude) result(x)
         real(kind=ESMF_KIND_R8) :: x(NI,NJ)
         real(kind=ESMF_KIND_R8), intent(in) :: longitude
         real(kind=ESMF_KIND_R8), intent(in) :: latitude

         x(:,1) = [ -sin(longitude), cos(longitude), 0._ESMF_KIND_R8 ]
         x(:,2) = [ -sin(latitude)*cos(longitude), -sin(latitude)*sin(longitude), cos(latitude) ]

      end function fill_element

   end function new_NS_Basis

end submodule new_NS_Basis_smod
