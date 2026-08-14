#include "MAPL.h"

module mapl_ChildSpec_mod
   use mapl_UserSetServices_mod
   use mapl_KeywordEnforcer_mod
   use esmf
   implicit none
   private

   public :: ChildSpec
   public :: operator(==)
   public :: operator(/=)

   type :: ChildSpec
      class(UserSetServices), allocatable :: user_setservices
      type(ESMF_HConfig) :: hconfig
      type(ESMF_TimeInterval), allocatable :: timeStep
      type(ESMF_TimeInterval) :: offset
   end type ChildSpec

   interface ChildSpec
      module procedure new_ChildSpec
   end interface ChildSpec

   interface operator(==)
      module procedure equal
   end interface operator(==)
      
   interface operator(/=)
      module procedure not_equal
   end interface operator(/=)


contains

   function new_ChildSpec(unusable, user_setservices, hconfig, timeStep, offset) result(spec)
      type(ChildSpec) :: spec
      class(KeywordEnforcer), optional, intent(in) :: unusable
      class(UserSetServices), optional, intent(in) :: user_setservices
      type(ESMF_HConfig), optional, intent(in) :: hconfig
      type(ESMF_TimeInterval), optional, intent(in) :: timeStep
      type(ESMF_TimeInterval), optional, intent(in) :: offset

      if(present(hconfig)) then
         spec%user_setservices = user_setservices
      end if

      if (present(hconfig)) then
         spec%hconfig = hconfig
      else
         spec%hconfig = ESMF_HConfigCreate(content='{}')
      end if

      call ESMF_TimeIntervalSet(spec%offset, s=0)
      if (present(timeStep)) spec%timeStep = timeStep
      if (present(offset)) spec%offset = offset

      _UNUSED_DUMMY(unusable)
   end function new_ChildSpec
      

   logical function equal(a, b)
      type(ChildSpec), intent(in) :: a
      type(ChildSpec), intent(in) :: b
      logical :: uss_is_allocated

      uss_is_allocated = allocated(a%user_setservices) 
      equal = uss_is_allocated .eqv. allocated(b%user_setservices)
      if (.not. equal) return

      if(uss_is_allocated) then
         equal = (a%user_setservices == b%user_setservices)
         if (.not. equal) return
      end if

      equal = equal_hconfig(a%hconfig, b%hconfig)
      if (.not. equal) return

      equal = equal_timestep(a%timeStep, b%timestep)
      if (.not. equal) return
      
      equal = equal_offset(a%offset, b%offset)
      if (.not. equal) return

   contains

      logical function equal_hconfig(a, b) result(equal)
         type(ESMF_HConfig), intent(in) :: a
         type(ESMF_HConfig), intent(in) :: b

         type(ESMF_HConfigMatch_Flag) :: match_flag
         
         match_flag = ESMF_HConfigMatch(a, b)
         equal = (match_flag == ESMF_HCONFIGMATCH_EXACT)

      end function equal_hconfig

      logical function equal_timestep(a, b) result(equal)
         type(ESMF_TimeInterval), allocatable, intent(in) :: a
         type(ESMF_TimeInterval), allocatable, intent(in) :: b

         equal = (allocated(a) .eqv. allocated(b))
         if (.not. equal) return

         if (allocated(a)) equal = (a == b)

      end function equal_timestep

      logical function equal_offset(a, b) result(equal)
         type(ESMF_TimeInterval), intent(in) :: a
         type(ESMF_TimeInterval), intent(in) :: b

         equal = (a == b)

      end function equal_offset

   end function equal

   logical function not_equal(a, b)
      type(ChildSpec), intent(in) :: a
      type(ChildSpec), intent(in) :: b

      not_equal = .not. (a == b)
   end function not_equal

end module mapl_ChildSpec_mod
