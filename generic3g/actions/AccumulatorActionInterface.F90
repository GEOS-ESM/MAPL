#include "MAPL_Generic.h"
module mapl3g_AccumulatorActionInterface
   use mapl3g_AccumulatorAction
   use mapl3g_MeanAction
   use mapl3g_MaxAction
   use mapl3g_MinAction
   use mapl3g_ExtensionAction
   use mapl_ErrorHandling
   use mapl_KeywordEnforcer
   use esmf, only: ESMF_TypeKind_Flag, ESMF_TYPEKIND_R4, operator(/=)
   implicit none

   public :: AccumulatorAction
   public :: MeanAction
   public :: MaxAction
   public :: MinAction
   public :: MAX_ACCUMULATION
   public :: MEAN_ACCUMULATION
   public :: MIN_ACCUMULATION
   public :: SIMPLE_ACCUMULATION
   public :: NO_ACCUMULATION
   public :: accumulation_type_is_valid
   public :: get_accumulator_action

   character(len=*), parameter :: MAX_ACCUMULATION = 'max'
   character(len=*), parameter :: MEAN_ACCUMULATION = 'mean'
   character(len=*), parameter :: MIN_ACCUMULATION = 'min'
   character(len=*), parameter :: SIMPLE_ACCUMULATION = 'simple'
   character(len=*), parameter :: NO_ACCUMULATION =''
   character(len=8), parameter :: ACCUMULATION_TYPES(4) = [character(len=8) :: &
      MAX_ACCUMULATION, MEAN_ACCUMULATION, MIN_ACCUMULATION, SIMPLE_ACCUMULATION]

contains

   logical function accumulation_type_is_valid(acctype) result(lval)
      character(len=*), optional, intent(in) :: acctype

      lval = present(acctype)
      if(lval) lval = any(ACCUMULATION_TYPES == acctype)

   end function accumulation_type_is_valid

   subroutine get_accumulator_action(accumulation_type, typekind, action, rc)
      character(len=*), intent(in) :: accumulation_type
      type(ESMF_TypeKind_Flag), intent(in) :: typekind
      class(ExtensionAction), allocatable, intent(out) :: action
      integer, optional, intent(out) :: rc 

      integer :: status

      if(typekind /= ESMF_TYPEKIND_R4) then
         _FAIL('Unsupported typekind')
      end if
      _ASSERT(accumulation_type_is_valid(accumulation_type), 'Unsupported AccumulatorAction')

      select case(accumulation_type)
      case (SIMPLE_ACCUMULATION)
         action = AccumulatorAction(typekind)
      case (MEAN_ACCUMULATION)
         action = MeanAction(typekind)
      case (MAX_ACCUMULATION)
         action = MaxAction(typekind)
      case (MIN_ACCUMULATION)
         action = MinAction(typekind)
      case default
         _FAIL('Unsupported AccumulatorAction')
      end select

      _RETURN(_SUCCESS)

   end subroutine get_accumulator_action

end module mapl3g_AccumulatorActionInterface
