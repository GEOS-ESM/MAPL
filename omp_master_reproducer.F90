! Standalone reproducer for NAG compiler bug:
!   !$omp master used outside an explicit parallel region crashes
!   with optimization flags but works with debug flags.
!
! Compile (debug - works):
!   nagfor -O0 -g -gline -C=all -openmp omp_master_reproducer.F90 -o reproducer_debug
!
! Compile (optimized - crashes):
!   nagfor -O2 -openmp omp_master_reproducer.F90 -o reproducer_opt
!
! Run:
!   ./reproducer_debug    ! succeeds
!   ./reproducer_opt      ! segfaults in __NAGf90_OpenMP_filter2

program omp_master_reproducer
   implicit none

   ! Dummy parallel region to force NAG's OpenMP runtime to initialize
   ! before the !$omp master is encountered outside a parallel region.
   !$omp parallel
   !$omp end parallel

   call initialize_something()
   print *, 'Done.'

contains

   subroutine initialize_something()
      integer :: x

      ! Called during serial initialization - no enclosing !$omp parallel.
      ! Per the OpenMP standard the implicit initial parallel region
      ! (single-thread team) provides the binding region, so this is
      ! conforming.  NAG's optimized runtime crashes here anyway.
      x = 0
      !$omp master
      x = 42
      !$omp end master

      print *, 'x =', x

   end subroutine initialize_something

end program omp_master_reproducer
