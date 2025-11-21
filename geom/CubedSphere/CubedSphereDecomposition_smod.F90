#include "MAPL_ErrLog.h"

submodule (mapl3g_CubedSphereDecomposition) CubedSphereDecomposition_smod
   use mapl_ErrorHandlingMod
   use MAPL_Base
   implicit none

contains

   pure module function new_CubedSphereDecomposition_basic(x_distribution, y_distribution) result(decomp)
      type(CubedSphereDecomposition) :: decomp
      integer, intent(in) :: x_distribution(:)
      integer, intent(in) :: y_distribution(:)

      decomp%x_distribution = x_distribution
      decomp%y_distribution = y_distribution

   end function new_CubedSphereDecomposition_basic

   pure module function new_CubedSphereDecomposition_petcount(dims, unusable, petCount) result(decomp)
      use mapl_KeywordEnforcer
      type(CubedSphereDecomposition) :: decomp
      integer, intent(in) :: dims(2)
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, intent(in) :: petCount

      integer :: nx, nx_start

      associate (aspect_ratio => real(dims(1))/dims(2))
        nx_start = max(1, floor(sqrt(petCount * aspect_ratio)))
        do nx = nx_start, 1, -1
           if (mod(petcount, nx) == 0) then ! found a decomposition
              exit
           end if
        end do
      end associate

      decomp = CubedSphereDecomposition(dims, topology=[nx, petCount/nx])
   end function new_CubedSphereDecomposition_petcount

   pure module function new_CubedSphereDecomposition_topo(dims, unusable, topology) result(decomp)
      use mapl_KeywordEnforcer
      type(CubedSphereDecomposition) :: decomp
      integer, intent(in) :: dims(2)
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, intent(in) :: topology(2)

      allocate(decomp%x_distribution(topology(1)))
      allocate(decomp%y_distribution(topology(2)))

      call MAPL_DecomposeDim(dims(1), decomp%x_distribution, topology(1), min_DE_extent=2)
      call MAPL_DecomposeDim(dims(2), decomp%y_distribution, topology(2), min_DE_extent=2)

   end function new_CubedSphereDecomposition_topo


   ! accessors
   pure module function get_x_distribution(decomp) result(x_distribution)
      integer, allocatable :: x_distribution(:)
      class(CubedSphereDecomposition), intent(in) :: decomp
      x_distribution = decomp%x_distribution
   end function get_x_distribution
   
   pure module function get_y_distribution(decomp) result(y_distribution)
      integer, allocatable :: y_distribution(:)
      class(CubedSphereDecomposition), intent(in) :: decomp
      y_distribution = decomp%y_distribution
   end function get_y_distribution

   ! Static factory methods
   module function make_CubedSphereDecomposition_current_vm(dims, rc) result(decomp)
      type(CubedSphereDecomposition) :: decomp
      integer, intent(in) :: dims(2)
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_VM) :: vm

      call ESMF_VMGetCurrent(vm, _RC)
      decomp = make_CubedSphereDecomposition(dims, vm, _RC)

      _RETURN(_SUCCESS)
   end function make_CubedSphereDecomposition_current_vm

   module function make_CubedSphereDecomposition_vm(dims, vm, rc) result(decomp)
      type(CubedSphereDecomposition) :: decomp
      integer, intent(in) :: dims(2)
      type(ESMF_VM), intent(in) :: vm
      integer, optional, intent(out) :: rc

      integer :: status
      integer :: petCount

      call ESMF_VMGet(vm, petCount=petCount, _RC)
      _ASSERT(mod(petCount,6)==0, "For cubed-sphere grid PET count must be multiple of 6")
      petCount=petCount/6
      decomp = CubedSphereDecomposition(dims, petCount=petCount)

      _RETURN(_SUCCESS)
   end function make_CubedSphereDecomposition_vm


   elemental module function equal_to(decomp1, decomp2)
      logical :: equal_to
      type(CubedSphereDecomposition), intent(in) :: decomp1
      type(CubedSphereDecomposition), intent(in) :: decomp2

      equal_to = size(decomp1%x_distribution) == size(decomp2%x_distribution)
      if (.not. equal_to) return

      equal_to = size(decomp1%y_distribution) == size(decomp2%y_distribution)
      if (.not. equal_to) return

      equal_to = all(decomp1%x_distribution == decomp2%x_distribution)
      if (.not. equal_to) return

      equal_to = all(decomp1%y_distribution == decomp2%y_distribution)

   end function equal_to

   elemental module function not_equal_to(decomp1, decomp2)
      logical :: not_equal_to
      type(CubedSphereDecomposition), intent(in) :: decomp1
      type(CubedSphereDecomposition), intent(in) :: decomp2

      not_equal_to = .not. (decomp1 == decomp2)

   end function not_equal_to

end submodule CubedSphereDecomposition_smod

