#include "MAPL_ErrLog.h"

submodule (mapl3g_LatLonDecomposition) LatLonDecomposition_smod
   use mapl_ErrorHandlingMod
   use MAPL_Base
   implicit none

contains

   pure module function new_LatLonDecomposition_basic(lon_distribution, lat_distribution) result(decomp)
      type(LatLonDecomposition) :: decomp
      integer, intent(in) :: lon_distribution(:)
      integer, intent(in) :: lat_distribution(:)

      decomp%lon_distribution = lon_distribution
      decomp%lat_distribution = lat_distribution

   end function new_LatLonDecomposition_basic

   pure module function new_LatLonDecomposition_petcount(dims, unusable, petCount) result(decomp)
      type(LatLonDecomposition) :: decomp
      integer, intent(in) :: dims(2)
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, intent(in) :: petCount

      integer :: status
      integer :: nx, nx_start

      associate (aspect_ratio => real(dims(1))/dims(2))
        nx_start = floor(sqrt(petCount * aspect_ratio))
        do nx = nx_start, 1, -1
           if (mod(petcount, nx) == 0) then ! found a decomposition
              exit
           end if
        end do

      end associate
      decomp = LatLonDecomposition(dims, topology=[nx, petCount/nx])

   end function new_LatLonDecomposition_petcount

   pure module function new_LatLonDecomposition_topo(dims, unusable, topology) result(decomp)
      type(LatLonDecomposition) :: decomp
      integer, intent(in) :: dims(2)
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, intent(in) :: topology(2)

      allocate(decomp%lon_distribution(topology(1)))
      allocate(decomp%lat_distribution(topology(2)))

      call MAPL_DecomposeDim(dims(1), decomp%lon_distribution, topology(1), min_DE_extent=2)
      call MAPL_DecomposeDim(dims(2), decomp%lat_distribution, topology(2), min_DE_extent=2)

   end function new_LatLonDecomposition_topo


   ! accessors
   pure module function get_lon_distribution(decomp) result(lon_distribution)
      integer, allocatable :: lon_distribution(:)
      class(LatLonDecomposition), intent(in) :: decomp
      lon_distribution = decomp%lon_distribution
   end function get_lon_distribution
   
   pure module function get_lat_distribution(decomp) result(lat_distribution)
      integer, allocatable :: lat_distribution(:)
      class(LatLonDecomposition), intent(in) :: decomp
      lat_distribution = decomp%lat_distribution
   end function get_lat_distribution


   pure module function get_lon_subset(this, coordinates, rank) result(subset)
      real(kind=R8), allocatable :: subset(:)
      class(LatLonDecomposition), intent(in) :: this
      real(kind=R8), intent(in) :: coordinates(:)
      integer, intent(in) :: rank

      subset = get_subset(this%lon_distribution, coordinates, rank)

   end function get_lon_subset

   pure module function get_lat_subset(this, coordinates, rank) result(subset)
      real(kind=R8), allocatable :: subset(:)
      class(LatLonDecomposition), intent(in) :: this
      real(kind=R8), intent(in) :: coordinates(:)
      integer, intent(in) :: rank

      subset = get_subset(this%lat_distribution, coordinates, rank)
      associate (d => this%lon_distribution)
        subset = coordinates(1+sum(d(:rank-1)):sum(d(:rank)))
      end associate

   end function get_lat_subset

   pure function get_subset(distribution, coordinates, rank) result(subset)
      real(kind=R8), allocatable :: subset(:)
      integer, intent(in) :: distribution(:)
      real(kind=R8), intent(in) :: coordinates(:)
      integer, intent(in) :: rank
      
      associate (d => distribution)
        subset = coordinates(1+sum(d(:rank-1)):sum(d(:rank)))
      end associate

   end function get_subset

      
   ! Static factory methods
   module function make_LatLonDecomposition_current_vm(dims, rc) result(decomp)
      type(LatLonDecomposition) :: decomp
      integer, intent(in) :: dims(2)
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_VM) :: vm

      call ESMF_VMGetCurrent(vm, _RC)

      decomp = make_LatLonDecomposition(dims, vm, _RC)

      _RETURN(_SUCCESS)
   end function make_LatLonDecomposition_current_vm

   module function make_LatLonDecomposition_vm(dims, vm, rc) result(decomp)
      type(LatLonDecomposition) :: decomp
      integer, intent(in) :: dims(2)
      type(ESMF_VM), intent(in) :: vm
      integer, optional, intent(out) :: rc

      integer :: status
      integer :: petCount

      call ESMF_VMGet(vm, petCount=petCount, _RC)
      decomp = make_LatLonDecomposition(dims, petCount)

      _RETURN(_SUCCESS)
   end function make_LatLonDecomposition_vm


   elemental module function equal_to(decomp1, decomp2)
      logical :: equal_to
      type(LatLonDecomposition), intent(in) :: decomp1
      type(LatLonDecomposition), intent(in) :: decomp2

      equal_to = size(decomp1%lon_distribution) == size(decomp2%lon_distribution)
      if (.not. equal_to) return

      equal_to = size(decomp1%lat_distribution) == size(decomp2%lat_distribution)
      if (.not. equal_to) return

      equal_to = all(decomp1%lon_distribution == decomp2%lon_distribution)
      if (.not. equal_to) return

      equal_to = all(decomp1%lat_distribution == decomp2%lat_distribution)

   end function equal_to

   elemental module function not_equal_to(decomp1, decomp2)
      logical :: not_equal_to
      type(LatLonDecomposition), intent(in) :: decomp1
      type(LatLonDecomposition), intent(in) :: decomp2

      not_equal_to = .not. (decomp1 == decomp2)

   end function not_equal_to

end submodule LatLonDecomposition_smod

