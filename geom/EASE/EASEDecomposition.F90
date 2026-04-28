#include "MAPL.h"

module mapl3g_EASEDecomposition

   use mapl_Partition
   use mapl_KeywordEnforcer, only: KE => KeywordEnforcer
   use mapl_ErrorHandling
   use esmf

   implicit none
   private

   public :: EASEDecomposition
   public :: make_EASEDecomposition
   public :: operator(==)
   public :: operator(/=)

   type :: EASEDecomposition
      private
      integer, allocatable :: lon_distribution(:)
      integer, allocatable :: lat_distribution(:)
   contains
      procedure :: get_lon_distribution
      procedure :: get_lat_distribution
   end type EASEDecomposition

   interface EASEDecomposition
      procedure :: new_EASEDecomposition_basic
      procedure :: new_EASEDecomposition_topology
      procedure :: new_EASEDecomposition_petcount
   end interface EASEDecomposition

   interface make_EASEDecomposition
      procedure :: make_EASEDecomposition_current_vm
      procedure :: make_EASEDecomposition_vm
   end interface make_EASEDecomposition

   interface operator(==)
      procedure :: equal_to
   end interface operator(==)

   interface operator(/=)
      procedure :: not_equal_to
   end interface operator(/=)

contains

   ! Basic constructor from explicit distribution arrays.
   function new_EASEDecomposition_basic(lon_distribution, lat_distribution) result(decomp)
      type(EASEDecomposition) :: decomp
      integer, intent(in) :: lon_distribution(:)
      integer, intent(in) :: lat_distribution(:)
      decomp%lon_distribution = lon_distribution
      decomp%lat_distribution = lat_distribution
   end function new_EASEDecomposition_basic

   ! Constructor from (im_world, jm_world) and explicit nx x ny topology.
   function new_EASEDecomposition_topology(dims, unusable, topology) result(decomp)
      type(EASEDecomposition) :: decomp
      integer, intent(in) :: dims(2)
      class(KE), optional, intent(in) :: unusable
      integer, intent(in) :: topology(2)
      decomp%lon_distribution = mapl_GetPartition(dims(1), k=topology(1), min_extent=2)
      decomp%lat_distribution = mapl_GetPartition(dims(2), k=topology(2), min_extent=2)
      _UNUSED_DUMMY(unusable)
   end function new_EASEDecomposition_topology

   ! Constructor from (im_world, jm_world) and a total petcount.
   ! Chooses nx to minimise aspect ratio distortion.
   function new_EASEDecomposition_petcount(dims, unusable, petCount) result(decomp)
      type(EASEDecomposition) :: decomp
      integer, intent(in) :: dims(2)
      class(KE), optional, intent(in) :: unusable
      integer, intent(in) :: petCount

      integer :: nx, nx_start

      associate (aspect_ratio => real(dims(1))/dims(2))
         nx_start = max(1, floor(sqrt(petCount * aspect_ratio)))
         do nx = nx_start, 1, -1
            if (mod(petCount, nx) == 0) exit
         end do
      end associate

      decomp = EASEDecomposition(dims, topology=[nx, petCount/nx])
      _UNUSED_DUMMY(unusable)
   end function new_EASEDecomposition_petcount

   ! Factory: build from the current ESMF VM.
   function make_EASEDecomposition_current_vm(dims, rc) result(decomp)
      type(EASEDecomposition) :: decomp
      integer, intent(in) :: dims(2)
      integer, optional, intent(out) :: rc

      type(ESMF_VM) :: vm
      integer :: petCount, status

      call ESMF_VMGetCurrent(vm, _RC)
      call ESMF_VMGet(vm, petCount=petCount, _RC)
      decomp = EASEDecomposition(dims, petCount=petCount)
      _RETURN(_SUCCESS)
   end function make_EASEDecomposition_current_vm

   ! Factory: build from an explicit ESMF VM.
   function make_EASEDecomposition_vm(dims, vm, rc) result(decomp)
      type(EASEDecomposition) :: decomp
      integer, intent(in) :: dims(2)
      type(ESMF_VM), intent(in) :: vm
      integer, optional, intent(out) :: rc

      integer :: petCount, status

      call ESMF_VMGet(vm, petCount=petCount, _RC)
      decomp = EASEDecomposition(dims, petCount=petCount)
      _RETURN(_SUCCESS)
   end function make_EASEDecomposition_vm

   ! Accessors
   pure function get_lon_distribution(this) result(dist)
      integer, allocatable :: dist(:)
      class(EASEDecomposition), intent(in) :: this
      dist = this%lon_distribution
   end function get_lon_distribution

   pure function get_lat_distribution(this) result(dist)
      integer, allocatable :: dist(:)
      class(EASEDecomposition), intent(in) :: this
      dist = this%lat_distribution
   end function get_lat_distribution

   ! Equality
   elemental function equal_to(a, b) result(eq)
      logical :: eq
      type(EASEDecomposition), intent(in) :: a, b
      eq = size(a%lon_distribution) == size(b%lon_distribution) .and. &
           size(a%lat_distribution) == size(b%lat_distribution)
      if (.not. eq) return
      eq = all(a%lon_distribution == b%lon_distribution) .and. &
           all(a%lat_distribution == b%lat_distribution)
   end function equal_to

   elemental function not_equal_to(a, b) result(neq)
      logical :: neq
      type(EASEDecomposition), intent(in) :: a, b
      neq = .not. (a == b)
   end function not_equal_to

end module mapl3g_EASEDecomposition
