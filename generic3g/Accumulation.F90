#include "MAPL_ErrLog.h"
module mapl3g_Accumulation
   use mapl3g_GenericCouplerComponent
   use esmf
   use mapl_ErrorHandling
   use mapl3g_MeanAction
   use mapl3g_MaxAction
   use mapl3g_MinAction

   implicit none
   private

   character(len=*), parameter :: KEY_ACCUMULATION_ACTION = 'accumulation_action'
   character(len=*), parameter :: KEY_MEAN_ACTION = 'mean'
   character(len=*), parameter :: KEY_MAX_ACTION = 'max'
   character(len=*), parameter :: KEY_MIN_ACTION = 'min'

contains

   subroutine make_accumulator(gcc, destination_name, source_name, accumulation_type, rc)
      type(GenericCouplerComponent), intent(inout) :: gcc
      character(len=*), intent(in) :: destination_name
      character(len=*), intent(in) :: source_name
      character(len=*), intent(in) :: accumulation_type
      integer, optional, intent(out) :: gc
      integer :: status
      class(AccumulatorAction), allocatable :: acc_action

      call make_accumulation_action(acc_action, accumulation_type, _RC)

      _RETURN(_SUCCESS)

   end subroutine make_accumulator

   subroutine make_destination_field(destination, source, rc)
      type(ESMF_Field), intent(inout) :: destination
      type(ESMF_Field), intent(inout) :: source
      integer, optional, intent(out) :: rc
      integer :: status

      _RETURN(_SUCCESS)

   end subroutine make_destination_field
       
   subroutine make_accumulation_action(acc_action, accumulation_type, rc)
      class(AccumulatorAction), allocatable, intent(out) :: acc_action
      character(len=*), intent(in) :: accumulation_type
      integer, optional, intent(out) :: rc
      integer :: status

      select case(accumulation_type)
      case(KEY_MEAN_ACTION)
         acc_action = MeanAction()
      case(KEY_MAX_ACTION)
         acc_action = MaxAction()
      case(KEY_MIN_ACTION)
         acc_action = MinAction()
      case default
         acc_action = AccumulatorAction()
         _FAIL('Unrecognized AccumulationAction')
      end select
      _RETURN(_SUCCESS)

   end subroutine make_accumulation_action

end module mapl3g_Accumulation
