#include "MAPL_ErrLog.h"

module mapl3g_ChildSpec
   use mapl3g_UserSetServices
   use mapl_KeywordEnforcer
   use esmf
   implicit none
   private

   public :: ChildSpec
   public :: operator(==)
   public :: operator(/=)

   public :: dump
   
   type :: ChildSpec
      class(AbstractUserSetServices), allocatable :: user_setservices
      type(ESMF_HConfig), allocatable :: hconfig
      type(ESMF_TimeInterval), allocatable :: timeStep
      type(ESMF_Time), allocatable :: refTime
   contains
      procedure :: write_formatted
      generic :: write(formatted) => write_formatted
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

   function new_ChildSpec(user_setservices, unusable, hconfig, timeStep, refTime) result(spec)
      type(ChildSpec) :: spec
      class(AbstractUserSetServices), intent(in) :: user_setservices
      class(KeywordEnforcer), optional, intent(in) :: unusable
      type(ESMF_HConfig), optional, intent(in) :: hconfig
      type(ESMF_TimeInterval), optional, intent(in) :: timeStep
      type(ESMF_Time), optional, intent(in) :: refTime

      spec%user_setservices = user_setservices
      if (present(hconfig)) spec%hconfig = hconfig

      if (present(timeStep)) spec%timeStep = timeStep
      if (present(refTime)) spec%refTime = refTime

      _UNUSED_DUMMY(unusable)
   end function new_ChildSpec
      

   logical function equal(a, b)
      type(ChildSpec), intent(in) :: a
      type(ChildSpec), intent(in) :: b

      equal = (a%user_setservices == b%user_setservices)
      if (.not. equal) return
      
      equal = equal_alloc_hconfig(a%hconfig, b%hconfig)
      if (.not. equal) return

      equal = equal_timestep(a%timeStep, b%timestep)
      if (.not. equal) return
      
      equal = equal_refTime(a%refTime, b%refTime)
      if (.not. equal) return

   contains

      logical function equal_alloc_hconfig(a, b) result(equal)
         type(ESMF_HConfig), allocatable, intent(in) :: a
         type(ESMF_HConfig), allocatable, intent(in) :: b


         type(ESMF_HConfigMatch_Flag) :: match_flag
         
         equal = (allocated(a) .eqv. allocated(b))
         if (.not. equal) return

         if (allocated(a)) then
            match_flag = ESMF_HConfigMatch(a, b)
            equal = (match_flag == ESMF_HCONFIGMATCH_EXACT)
         end if

      end function equal_alloc_hconfig

      logical function equal_timestep(a, b) result(equal)
         type(ESMF_TimeInterval), allocatable, intent(in) :: a
         type(ESMF_TimeInterval), allocatable, intent(in) :: b

         equal = (allocated(a) .eqv. allocated(b))
         if (.not. equal) return

         if (allocated(a)) equal = (a == b)

      end function equal_timestep

      logical function equal_refTime(a, b) result(equal)
         type(ESMF_Time), allocatable, intent(in) :: a
         type(ESMF_Time), allocatable, intent(in) :: b

         equal = (allocated(a) .eqv. allocated(b))
         if (.not. equal) return

         if (allocated(a)) equal = (a == b)

      end function equal_refTime

   end function equal

   logical function not_equal(a, b)
      type(ChildSpec), intent(in) :: a
      type(ChildSpec), intent(in) :: b

      not_equal = .not. (a == b)
   end function not_equal

   subroutine dump(x)
      type(ChildSpec) :: x

      select type (q => x%user_setservices)
      type is (Dsosetservices)
         print*,__FILE__,__LINE__, q%sharedObj, '::', q%userRoutine
      end select
   end subroutine dump

   subroutine write_formatted(this, unit, iotype, v_list, iostat, iomsg)
      class(ChildSpec), intent(in) :: this
      integer, intent(in) :: unit
      character(*), intent(in) :: iotype
      integer, intent(in) :: v_list(:)
      integer, intent(out) :: iostat
      character(*), intent(inout) :: iomsg

      write(unit,'(a, DT)', iostat=iostat, iomsg=iomsg) 'UserSetServices: ', this%user_setservices

      _UNUSED_DUMMY(iotype)
      _UNUSED_DUMMY(v_list)
      
   end subroutine write_formatted

end module mapl3g_ChildSpec
