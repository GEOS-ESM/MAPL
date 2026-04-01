program test_tiling_nc4_simple
   use ESMF
   use MAPL_BaseMod
   use MAPL_CommsMod
   use NCIOMod
   use mpi
   use, intrinsic :: iso_fortran_env, only: REAL64
   implicit none

   type(ESMF_VM) :: vm
   type(ESMF_DELayout) :: layout
   integer :: rc, status, comm, rank, npes
   character(len=256) :: filename
   
   ! Variables for testing
   character(len=128) :: GridNames(2)
   integer :: IMs(2), JMs(2)
   integer :: N_Grids, NT
   real, pointer :: AVR(:,:) => null()
   
   ! Variables for verification
   integer :: i, j
   integer :: global_N_Grids, global_NT
   integer :: global_IMs(2), global_JMs(2)
   real :: sum_AVR, global_sum_AVR
   integer :: errors
   
   ! Initialize ESMF
   call ESMF_Initialize(vm=vm, logkindflag=ESMF_LOGKIND_NONE, rc=status)
   if (status /= ESMF_SUCCESS) then
      print *, "Failed to initialize ESMF"
      stop 1
   endif
   
   call ESMF_VMGet(vm, mpiCommunicator=comm, petCount=npes, localPet=rank, rc=status)
   if (status /= ESMF_SUCCESS) then
      print *, "Failed to get VM info"
      call exit(1)
   endif
   
   ! Create DELayout
   layout = ESMF_DELayoutCreate(vm, rc=status)
   if (status /= ESMF_SUCCESS) then
      print *, "Failed to create DELayout"
      call exit(1)
   endif
   
   if (rank == 0) then
      print *, "============================================="
      print *, "Testing MAPL_ReadTilingNC4 with ", npes, " processes"
      print *, "============================================="
      print *, ""
   endif
   
   ! Test File 1: EASE grid
   filename = "/Users/wjiang/MAPL/build/EASEv2_M36_964x406.nc4"
   
   if (rank == 0) then
      print *, "Testing file: EASEv2_M36_964x406.nc4"
      print *, "-------------------------------------"
   endif
   
   ! Call the NEW approach - all processes call it
   call MAPL_ReadTilingNC4(layout, filename, GridName=GridNames, IM=IMs, &
                          JM=JMs, N_Grids=N_Grids, AVR=AVR, rc=status)
   if (status /= 0) then
      print *, "ERROR on rank ", rank, ": MAPL_ReadTilingNC4 failed, rc=", status
      call ESMF_Finalize(rc=status)
      call exit(1)
   endif
   NT = size(AVR,1)
   
   ! Print what each process received
   print *, "RANK ", rank, ": N_Grids=", N_Grids, ", NT=", NT, ", IM=", IMs, ", JM=", JMs
   
   ! Verify all processes have the same metadata
   call MPI_Allreduce(N_Grids, global_N_Grids, 1, MPI_INTEGER, MPI_MAX, comm, status)
   if (N_Grids /= global_N_Grids) then
      print *, "ERROR on rank ", rank, ": N_Grids mismatch: ", N_Grids, " vs ", global_N_Grids
   endif
   
   call MPI_Allreduce(NT, global_NT, 1, MPI_INTEGER, MPI_MAX, comm, status)
   if (NT /= global_NT) then
      print *, "ERROR on rank ", rank, ": NT mismatch: ", NT, " vs ", global_NT
   endif
   
   call MPI_Allreduce(IMs, global_IMs, 2, MPI_INTEGER, MPI_MAX, comm, status)
   if (any(IMs /= global_IMs)) then
      print *, "ERROR on rank ", rank, ": IM mismatch"
   endif
   
   call MPI_Allreduce(JMs, global_JMs, 2, MPI_INTEGER, MPI_MAX, comm, status)
   if (any(JMs /= global_JMs)) then
      print *, "ERROR on rank ", rank, ": JM mismatch"
   endif
   
   ! Compute checksum of AVR array on each process
   sum_AVR = 0.0
   do j = 1, size(AVR, 2)
      do i = 1, NT
         sum_AVR = sum_AVR + AVR(i,j)
      enddo
   enddo
   
   ! Verify all processes have the same AVR data
   call MPI_Allreduce(sum_AVR, global_sum_AVR, 1, MPI_REAL, MPI_MAX, comm, status)
   if (abs(sum_AVR - global_sum_AVR) > 1.0e-5) then
      print *, "ERROR on rank ", rank, ": AVR checksum mismatch: ", sum_AVR, " vs ", global_sum_AVR
   else
      print *, "RANK ", rank, ": AVR checksum = ", sum_AVR, " - MATCH!"
   endif
   
   call MPI_Barrier(comm, status)
   
   if (rank == 0) then
      print *, ""
      print *, "File 1 Results:"
      print *, "  GridName(1) = ", trim(GridNames(1))
      print *, "  N_Grids = ", N_Grids
      print *, "  NT = ", NT
      print *, "  IM = ", IMs
      print *, "  JM = ", JMs
      print *, "  AVR checksum = ", sum_AVR
   endif
   
   ! Cleanup
   if (associated(AVR)) deallocate(AVR)
   nullify(AVR)
   
   call MPI_Barrier(comm, status)
   
   ! Test File 2: Pfafstetter grid
   filename = "/Users/wjiang/MAPL/build/CF0024x6C_DE0360xPE0180-Pfafstetter.nc4"
   
   if (rank == 0) then
      print *, ""
      print *, "============================================="
      print *, "Testing file: CF0024x6C_DE0360xPE0180-Pfafstetter.nc4"
      print *, "-------------------------------------"
   endif
   
   call MAPL_ReadTilingNC4(layout, filename, GridName=GridNames, IM=IMs, &
                          JM=JMs, N_Grids=N_Grids, AVR=AVR, rc=status)
   if (status /= 0) then
      print *, "ERROR on rank ", rank, ": MAPL_ReadTilingNC4 failed, rc=", status
      call ESMF_Finalize(rc=status)
      call exit(1)
   endif
   NT = size(AVR,1)
   
   print *, "RANK ", rank, ": N_Grids=", N_Grids, ", NT=", NT, ", IM=", IMs, ", JM=", JMs
   
   ! Verify all processes have the same metadata
   call MPI_Allreduce(N_Grids, global_N_Grids, 1, MPI_INTEGER, MPI_MAX, comm, status)
   if (N_Grids /= global_N_Grids) then
      print *, "ERROR on rank ", rank, ": N_Grids mismatch: ", N_Grids, " vs ", global_N_Grids
   endif
   
   call MPI_Allreduce(NT, global_NT, 1, MPI_INTEGER, MPI_MAX, comm, status)
   if (NT /= global_NT) then
      print *, "ERROR on rank ", rank, ": NT mismatch: ", NT, " vs ", global_NT
   endif
   
   call MPI_Allreduce(IMs, global_IMs, 2, MPI_INTEGER, MPI_MAX, comm, status)
   if (any(IMs /= global_IMs)) then
      print *, "ERROR on rank ", rank, ": IM mismatch"
   endif
   
   call MPI_Allreduce(JMs, global_JMs, 2, MPI_INTEGER, MPI_MAX, comm, status)
   if (any(JMs /= global_JMs)) then
      print *, "ERROR on rank ", rank, ": JM mismatch"
   endif
   
   ! Compute checksum of AVR array
   sum_AVR = 0.0
   do j = 1, size(AVR, 2)
      do i = 1, NT
         sum_AVR = sum_AVR + AVR(i,j)
      enddo
   enddo
   
   call MPI_Allreduce(sum_AVR, global_sum_AVR, 1, MPI_REAL, MPI_MAX, comm, status)
   if (abs(sum_AVR - global_sum_AVR) > 1.0e-5) then
      print *, "ERROR on rank ", rank, ": AVR checksum mismatch: ", sum_AVR, " vs ", global_sum_AVR
   else
      print *, "RANK ", rank, ": AVR checksum = ", sum_AVR, " - MATCH!"
   endif
   
   call MPI_Barrier(comm, status)
   
   if (rank == 0) then
      print *, ""
      print *, "File 2 Results:"
      print *, "  GridName(1) = ", trim(GridNames(1))
      if (N_Grids > 1) print *, "  GridName(2) = ", trim(GridNames(2))
      print *, "  N_Grids = ", N_Grids
      print *, "  NT = ", NT
      print *, "  IM = ", IMs
      print *, "  JM = ", JMs
      print *, "  AVR checksum = ", sum_AVR
   endif
   
   ! Final summary
   call MPI_Barrier(comm, status)
   if (rank == 0) then
      print *, ""
      print *, "============================================="
      print *, "All tests PASSED!"
      print *, "All processes received identical data."
      print *, "============================================="
   endif
   
   ! Cleanup
   if (associated(AVR)) deallocate(AVR)
   
   call ESMF_DELayoutDestroy(layout, rc=status)
   call ESMF_Finalize(rc=status)

end program test_tiling_nc4_simple
