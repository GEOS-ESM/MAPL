#include "MAPL_Generic.h"
module mapl3g_AccumulationAspect
   use mapl3g_StateItemAspect
   use mapl3g_AccumulatorActionInterface
   implicit none
   private

   public :: AccumulationAspect

   type, extends(StateItemAspect) :: AccumulationAspect
      character(len=:), allocatable :: accumulation_type
      type(ESMF_TypeKind_Flag) :: typekind = ESMF_TYPEKIND_R4
      logical :: type_is_valid = .FALSE.
   contains
      procedure :: matches
      procedure :: supports_conversion_general
      procedure :: supports_conversion_specific
      procedure :: make_action
   end type AccumulationAspect

   interface AccumulationAspect
      module procedure :: construct_accumulation_aspect
   end interface AccumulationAspect

contains

   function construct_accumulation_aspect(accumulation_type, typekind) result(aspect)
      type(AccumulationAspect) :: aspect
      character(len=*), optional, intent(in) :: accumulation_type
      type(ESMF_TypeKind_Flag), optional, intent(in) :: typekind

      aspect%accumulation_type = 'NO_ACCUMULATION'
      if(present(accumulation_type)) aspect%accumulation_type = accumulation_type
      if(present(typekind)) aspect%typekind = typekind
      aspect%type_is_valid = accumulation_is_valid(accumulation_type)
      
   end function construct_accumulation_aspect

   logical function matches(this, dst) result(matches)
      import :: StateItemAspect
      class(AccumulationAspect), intent(in) :: this
      class(StateItemAspect), intent(in) :: dst
   end function matches

   function make_action(this, dst, rc) result(action)
      use mapl3g_ExtensionAction
      class(ExtensionAction), allocatable :: action
      class(AccumulationAspect), intent(in) :: this
      class(StateItemAspect), intent(in) :: dst
      integer, optional, intent(out) :: rc
      integer :: state

      select type(dst)
      class is (AccumulationAspect)
      class default
         allocate(action, source=NullAction())
         _FAIL('AccumulationAspect cannot convert from other class.')
      end select
      _RETURN(_SUCCESS)

   end function make_action

   logical function supports_conversion_general(this) result(supports_conversion)
      class(StateItemAspect), intent(in) :: this

      supports_conversion = .TRUE.

   end function supports_conversion_general

   logical function supports_conversion_specific(this, dst) result(supports_conversion)
      class(AccumulationAspect), intent(in) :: this
      class(StateItemAspect), intent(in) :: dst
   end function supports_conversion_specific

end module mapl3g_AccumulationAspect
