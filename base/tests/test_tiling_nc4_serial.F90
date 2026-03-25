program test_tiling_nc4_serial
   use NCIOMod
   use, intrinsic :: iso_fortran_env, only: REAL64
   implicit none

   character(len=256) :: filename
   
   ! Variables for testing
   character(len=128) :: GridNames(2)
   integer :: IMs(2), JMs(2)
   integer :: N_Grids, NT, status
   real, pointer :: AVR(:,:) => null()
   
   print *, "============================================="
   print *, "Testing MAPL_ReadTilingNC4 (serial version)"
   print *, "============================================="
   print *, ""
   
   ! Test File 1: EASE grid
   filename = "/Users/wjiang/MAPL/build/EASEv2_M36_964x406.nc4"
   
   print *, "Testing file: EASEv2_M36_964x406.nc4"
   print *, "-------------------------------------"
   
   ! Call serial version (no layout parameter)
   call MAPL_ReadTilingNC4(filename, GridName=GridNames, IM=IMs, &
                          JM=JMs, N_Grids=N_Grids, AVR=AVR, rc=status)
   if (status /= 0) then
      print *, "ERROR: MAPL_ReadTilingNC4 failed, rc=", status
      call exit(1)
   endif
   NT = size(AVR,1)
   
   print *, "File 1 Results:"
   print *, "  GridName(1) = ", trim(GridNames(1))
   print *, "  N_Grids = ", N_Grids
   print *, "  NT = ", NT
   print *, "  IM = ", IMs
   print *, "  JM = ", JMs
   print *, ""
   
   ! Cleanup
   if (associated(AVR)) deallocate(AVR)
   nullify(AVR)
   
   ! Test File 2: Pfafstetter grid
   filename = "/Users/wjiang/MAPL/build/CF0024x6C_DE0360xPE0180-Pfafstetter.nc4"
   
   print *, "Testing file: CF0024x6C_DE0360xPE0180-Pfafstetter.nc4"
   print *, "-------------------------------------"
   
   call MAPL_ReadTilingNC4(filename, GridName=GridNames, IM=IMs, &
                          JM=JMs, N_Grids=N_Grids, AVR=AVR, rc=status)
   if (status /= 0) then
      print *, "ERROR: MAPL_ReadTilingNC4 failed, rc=", status
      call exit(1)
   endif
   NT = size(AVR,1)
   
   print *, "File 2 Results:"
   print *, "  GridName(1) = ", trim(GridNames(1))
   if (N_Grids > 1) print *, "  GridName(2) = ", trim(GridNames(2))
   print *, "  N_Grids = ", N_Grids
   print *, "  NT = ", NT
   print *, "  IM = ", IMs
   print *, "  JM = ", JMs
   print *, ""
   
   ! Cleanup
   if (associated(AVR)) deallocate(AVR)
   
   print *, "============================================="
   print *, "All serial tests PASSED!"
   print *, "============================================="

end program test_tiling_nc4_serial
