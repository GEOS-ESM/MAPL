module mapl3g_PrecisionConverter
   implicit none

contains

   subroutine run(this, f_in, f_out)
      ! Use low-level utility 
      call MAPL_ConvertPrecision(f_in, f_out)
   end subroutine run

end module mapl3g_PrecisionConverter
