#include "MAPL_ErrLog.h"

module mapl3g_ChildSpec
   use mapl3g_UserSetServices
   use mapl_KeywordEnforcer
   implicit none
   private

   public :: ChildSpec
   public :: operator(==)
   public :: operator(/=)

   public :: dump
   
   type :: ChildSpec
      class(AbstractUserSetServices), allocatable :: user_setservices
      character(:), allocatable :: config_file
      ! Prevent default structure constructor
      integer, private ::  hack
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

   function new_ChildSpec(user_setservices, unusable, config_file) result(spec)
      type(ChildSpec) :: spec
      class(AbstractUserSetServices), intent(in) :: user_setservices
      class(KeywordEnforcer), optional, intent(in) :: unusable
      character(*), optional, intent(in) :: config_file

      spec%user_setservices = user_setservices
      if (present(config_file)) spec%config_file = config_file

      _UNUSED_DUMMY(unusable)
   end function new_ChildSpec
      

   logical function equal(a, b)
      type(ChildSpec), intent(in) :: a
      type(ChildSpec), intent(in) :: b

      equal = (a%user_setservices == b%user_setservices)
      if (.not. equal) return
      
      equal = equal_alloc_str(a%config_file, b%config_file)
      if (.not. equal) return

   contains

      logical function equal_alloc_str(a, b) result(equal)
         character(:), allocatable, intent(in) :: a
         character(:), allocatable, intent(in) :: b

         equal = (allocated(a) .eqv. allocated(b))
         if (.not. equal) return

         if (allocated(a)) equal = (a == b)

      end function equal_alloc_str

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

      character(:), allocatable :: file

      if (allocated(this%config_file)) then
         file = this%config_file
      else
         file = '<none>'
      end if

      write(unit,'(a,a)',iostat=iostat, iomsg=iomsg) 'Config file: ', file
      if (iostat /= 0) return

      write(unit,'(a, DT)', iostat=iostat, iomsg=iomsg) 'UserSetServices: ', this%user_setservices

      _UNUSED_DUMMY(iotype)
      _UNUSED_DUMMY(v_list)
      
   end subroutine write_formatted



end module mapl3g_ChildSpec
