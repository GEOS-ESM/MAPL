#include "MAPL_Generic.h"
module mapl3g_AccumulatorTransformInterface
   use mapl3g_AccumulatorTransform
   use mapl3g_MeanTransform
   use mapl3g_MaxTransform
   use mapl3g_MinTransform
   use mapl3g_ExtensionTransform
   use mapl3g_NullTransform
   use mapl_ErrorHandling
   use mapl_KeywordEnforcer
   use esmf, only: ESMF_TypeKind_Flag, ESMF_TYPEKIND_R4, operator(/=)
   implicit none

   public :: AccumulatorTransform
   public :: MeanTransform
   public :: MaxTransform
   public :: MinTransform
   public :: MAX_ACCUMULATION
   public :: MEAN_ACCUMULATION
   public :: MIN_ACCUMULATION
   public :: SIMPLE_ACCUMULATION
   public :: INSTANTANEOUS
   public :: accumulation_type_is_valid
   public :: get_accumulator_transform

   ! This is the default case where accumulation_type is not set.
   character(len=*), parameter :: INSTANTANEOUS =''
   
   ! These are explicit accumulation_type values.
   character(len=*), parameter :: MAX_ACCUMULATION = 'max'
   character(len=*), parameter :: MEAN_ACCUMULATION = 'mean'
   character(len=*), parameter :: MIN_ACCUMULATION = 'min'
   character(len=*), parameter :: SIMPLE_ACCUMULATION = 'simple'
   character(len=8), parameter :: ACCUMULATION_TYPES(4) = [character(len=8) :: &
      MAX_ACCUMULATION, MEAN_ACCUMULATION, MIN_ACCUMULATION, SIMPLE_ACCUMULATION]

contains

   logical function accumulation_type_is_valid(acctype) result(lval)
      character(len=*), optional, intent(in) :: acctype

      lval = .FALSE.
      if(.not. present(acctype)) return
      lval = any(ACCUMULATION_TYPES == acctype)

   end function accumulation_type_is_valid

   subroutine get_accumulator_transform(accumulation_type, typekind, transform, rc)
      character(len=*), intent(in) :: accumulation_type
      type(ESMF_TypeKind_Flag), intent(in) :: typekind
      class(ExtensionTransform), allocatable, intent(out) :: transform
      integer, optional, intent(out) :: rc 

      integer :: status

      allocate(transform, source=NullTransform())

      if(typekind /= ESMF_TYPEKIND_R4) then
         _FAIL('Unsupported typekind')
      end if

      select case(accumulation_type)
      case (SIMPLE_ACCUMULATION)
         allocate(transform, source=AccumulatorTransform(typekind))
      case (MEAN_ACCUMULATION)
         allocate(transform, source=MeanTransform(typekind))
      case (MAX_ACCUMULATION)
         allocate(transform, source=MaxTransform(typekind))
      case (MIN_ACCUMULATION)
         allocate(transform, source=MinTransform(typekind))
      case (INSTANTANEOUS)
         _FAIL('No AccumulatorTransform for instantaneous.')
      case default
         _FAIL('Unsupported AccumulatorTransform')
      end select

      _RETURN(_SUCCESS)

   end subroutine get_accumulator_transform

end module mapl3g_AccumulatorTransformInterface
