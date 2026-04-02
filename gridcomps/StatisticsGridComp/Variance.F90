#include "MAPL.h"
module mapl3g_Variance
   use mapl3g_AbstractVariance, AbstractVariance => Variance
   use mapl3g_WelfordVariance, only: WelfordVariance
   use mapl3g_ShiftedVariance, only: ShiftedVariance
   use mapl_KeywordEnforcer
   implicit none(type, external)
   private

   public :: Variance => newVariance
   public :: WELFORD, SHIFTED

   enum, bind(c)
      enumerator :: VARIANCE_ALGORITHM = 0
      enumerator :: WELFORD
      enumerator :: SHIFTED
   end enum

   integer(kind=VARIANCE_ALGORITHM), parameter :: DEFAULT_ALGORITHM = WELFORD

contains

   function newVariance(unusable, algorithm, f, variance_field, alarm, biased) result(var)
      class(AbstractVariance), allocatable :: var
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer(kind=kind(VARIANCE_ALGORITHM)), optional, intent(in) :: algorithm
      type(esmf_Field), intent(in) :: f
      type(esmf_Field), intent(in) :: variance_field
      type(esmf_Alarm), intent(in) :: alarm
      logical, optional, intent(in) :: biased
      integer(kind=kind(VARIANCE_ALGORITHM)) :: algorithm_
      
      algorithm_ = DEFAULT_ALGORITHM
      if(present(algorithm)) algorithm_ = algorithm

      select case(algo)
      case (WELFORD)
         var = WelfordVariance()
      case (SHIFTED)
         var = ShiftedVariance()
      case default
         _FAIL("Unrecognized Variance algorithm")
      end select

      call var%set_common(f=f, variance_field=variance_field, alarm=alarm, biased=biased)

   end function newVariance

end module mapl3g_Variance
