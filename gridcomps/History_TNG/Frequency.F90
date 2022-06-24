#include "MAPL_Generic.h"
#include "NUOPC_ErrLog.h"

module FrequencyMod
   use, intrinsic :: iso_fortran_env, only: INT64
   use ESMF
   use NUOPC
   use MAPL_ExceptionHandling
   use MAPL_KeywordEnforcerMod

   implicit none
   private

   public :: Frequency

   type :: Frequency
      private
      character(:), allocatable :: frequency
   contains
      procedure :: initialize
      procedure :: get_frequency
   end type Frequency
contains
   subroutine initialize(this, freq)
      class(Frequency), intent(inout) :: this
      character(*),     intent(in   ) :: freq

      this%frequency = freq
   end subroutine initialize

   function get_frequency(this) result(freq)
      character(:), allocatable :: freq
      class(Frequency), intent(inout) :: this

      freq = this%frequency
   end function get_frequency
end module FrequencyMod
