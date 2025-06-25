#define _success      0
#define _failure     1
#define _verify(A)   if(  A/=0) then; if(present(rc)) rc=A; PRINT *, Iam, __LINE__; return; endif
#define _assert(A)   if(.not.A) then; if(present(rc)) rc=_failure; PRINT *, Iam, __LINE__; return; endif
#define _return(A)   if(present(rc)) rc=A; return
#include "unused_dummy.H"

module MockRegridderMod
   use MAPL_AbstractRegridderMod
   use, intrinsic :: iso_fortran_env, only: REAL64
   implicit none
   private

   public :: MockRegridder

   type, extends(AbstractRegridder) :: MockRegridder
      private
      character(len=:), allocatable :: name
   contains
      procedure :: regrid_scalar_2d_real64
      procedure :: get_name
      procedure :: initialize_subclass
   end type MockRegridder

   interface MockRegridder
      module procedure newMockRegridder
   end interface MockRegridder

   character(len=*), parameter :: MOD_NAME = 'MockRegridder::'

contains


   function newMockRegridder(name) result(regridder)
      type (MockRegridder) :: regridder
      character(len=*), intent(in) :: name

      regridder%name = name

   end function newMockRegridder


   subroutine regrid_scalar_2d_real64(this, q_in, q_out, rc)
      class (MockRegridder), intent(in) :: this
      real(kind=REAL64), intent(in) :: q_in(:,:)
      real(kind=REAL64), intent(out) :: q_out(:,:)
      integer, optional, intent(out) :: rc
      character(len=*), parameter :: Iam = MOD_NAME//'regrid_scalar_2d_real64'

      _unused_dummy(this)
      _unused_dummy(q_in)

      q_out = 0
      _return(_success)

   end subroutine regrid_scalar_2d_real64

   function get_name(this) result(name)
      class (MockRegridder), intent(in) :: this
      character(len=:), allocatable :: name

      name = this%name

   end function get_name


   subroutine initialize_subclass(this, unusable, rc)
      use MAPL_KeywordEnforcerMod
      class (MockRegridder), intent(inout) :: this
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      _unused_dummy(this)
      _unused_dummy(unusable)
      _unused_dummy(rc)

   end subroutine initialize_subclass


end module MockRegridderMod
