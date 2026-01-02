#include "MAPL.h"

module mapl3g_AreaMean

   use mpi
   use, intrinsic :: iso_fortran_env, only: real32, real64
   use MAPL_Constants, only: MAPL_UNDEFINED_REAL, MAPL_UNDEFINED_REAL64
   use MAPL_ErrorHandling, only: MAPL_Verify, MAPL_Return

   implicit none
   private

   public :: AreaMean

   interface AreaMean
      ! module procedure AreaMean_2d_r8_bitrep
      module procedure AreaMean_2d_r8
   end interface AreaMean

contains

   ! subroutine AreaMean_2d_r8_bitrep( qave, q, area, grid, bitreproducible, rc )
   !    real(kind=real64), intent(  OUT) :: qave
   !    real,              intent(IN   ) :: q(:,:)
   !    real,              intent(IN   ) :: area(:,:)
   !    type(ESMF_Grid),   intent(INout) :: grid
   !    logical,           intent(IN   ) :: bitreproducible
   !    integer, optional, intent(  OUT) :: rc

   !    ! Log err vars
   !    integer :: status
   !    character(len=ESMF_MAXSTR), parameter :: Iam='AreaMeanBR'

   !    ! Local vars
   !    real(kind=real64)  :: qdum(2)
   !    integer :: im,jm
   !    integer :: DIMS(3)

   !    integer :: i,j
   !    logical :: amIRoot

   !    real, allocatable :: qglobal(:,:)
   !    real, allocatable :: aglobal(:,:)

   !    type(ESMF_VM) :: vm

   !    if (.not. bitreproducible) then
   !       call AreaMean(qave, q, area, grid, rc=status )
   !       _RETURN(STATUS)
   !    end if

   !    ! get VM (should get from the grid, but this is quicker)
   !    call ESMF_VmGetCurrent(vm, rc=status)
   !    _VERIFY(STATUS)

   !    amIRoot = MAPL_AM_I_Root(vm)

   !    if (amIRoot) then
   !       call MAPL_GridGet(GRID, globalCellCountPerDim=DIMS, RC=STATUS)
   !       im = DIMS(1) ! global grid dim
   !       jm = DIMS(2) ! global grid dim
   !    else
   !       im = 0 ! dummy sizes
   !       jm = 0 ! dummy sizes
   !    end if

   !    allocate(qglobal(im,jm), stat=status)
   !    _VERIFY(STATUS)
   !    allocate(aglobal(im,jm), stat=status)
   !    _VERIFY(STATUS)

   !    call ArrayGather(local_array=area, global_array=aglobal, grid=grid, rc=status)
   !    _VERIFY(STATUS)
   !    call ArrayGather(local_array=q,    global_array=qglobal, grid=grid, rc=status)
   !    _VERIFY(STATUS)
   !    qdum = 0.0d+0
   !    ! do calculation on root
   !    if (amIRoot) then
   !       do j=1,jm
   !          do i=1,im
   !             if (qglobal(i,j) == MAPL_Undef) cycle ! exclude any undefs
   !             qdum(1) = qdum(1) + qglobal(i,j)*aglobal(i,j)
   !             qdum(2) = qdum(2) + aglobal(i,j)
   !          enddo
   !       end do

   !       if (qdum(2) /= 0.0d+0) then

   !          qave = qdum(1) / qdum(2)

   !          !ALT: convert the the result to single precision
   !          !     This technically is not needed here, but is is done to be in sync
   !          !     with the parallel method below
   !          !     qave = real(qave, kind=4)
   !       else
   !          qave = MAPL_Undef
   !       end if
   !    end if
   !    deallocate(aglobal)
   !    deallocate(qglobal)

   !    call MAPL_CommsBcast(vm, DATA=qave, N=1, Root=MAPL_Root, RC=status)
   !    _VERIFY(STATUS)

   !    _RETURN(ESMF_SUCCESS)
   ! end subroutine AreaMean_2d_r8_bitrep

   function AreaMean_2d_r8(q, area, comm, rc) result(qmean)
      real, intent(in) :: q(:,:)
      real, intent(in) :: area(:,:)
      integer, intent(in) :: comm
      integer, optional, intent(out) :: rc
      real(real64) :: qmean ! result

      real(kind=real64) :: sum_local(2), sum_global(2)
      integer, parameter :: TWO = 2
      integer :: im, jm, i, j, status

      im = size(area, 1) ! local grid dim
      jm = size(area, 2) ! local grid dim

      ! compute local sum
      sum_local = 0.0d+0
      do j = 1, jm
         do i = 1, im
            if (q(i, j) == MAPL_UNDEFINED_REAL) cycle ! exclude any undefs
            sum_local(1) = sum_local(1) + q(i, j) * area(i, j)
            sum_local(2) = sum_local(2) + area(i,j)
         enddo
      end do

      call MPI_AllReduce(sum_local, sum_global, TWO, MPI_DOUBLE, MPI_SUM, comm, status)
      _VERIFY(status)

      if (sum_global(2) /= 0.0d+0) then
         qmean = sum_global(1) / sum_global(2)
         !ALT: convert the the result to single precision to get rid of
         !     numerical non-associativity in floating point numbers
         !     qmean = real(qmean, kind=4)
      else
         qmean = MAPL_UNDEFINED_REAL64
      end if

      _RETURN(_SUCCESS)
   end function AreaMean_2d_r8

end module mapl3g_AreaMean
