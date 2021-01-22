#include "MAPL_Generic.h"

! A RegridderSpec is used to indicate which subclass of regridder will be used.
module mapl_RegridderSpec
   use MAPL_BaseMod, only: MAPL_UNDEF
   use MAPL_KeywordEnforcerMod
   use MAPL_ErrorHandlingMod
   use mapl_RegridMethods
   use ESMF
   use, intrinsic :: iso_fortran_env, only: INT64
   implicit none
   private

      
   public :: RegridderSpec

   type :: RegridderSpec
      type (ESMF_Grid) :: grid_in
      type (ESMF_Grid) :: grid_out
      integer :: regrid_method = UNSPECIFIED_REGRID_METHOD
      integer :: hints = 0
   contains
      procedure :: equals
      procedure :: get_grid_type
      generic :: operator (==) => equals
      procedure :: less_than
      generic :: operator (<) => less_than
   end type RegridderSpec
   

   interface RegridderSpec
      module procedure newRegridderSpec
   end interface RegridderSpec

   character(len=*), parameter :: MOD_NAME = 'MAPL_RegridderSpec_private::'

contains


   function newRegridderSpec(grid_in, grid_out, regrid_method, unusable, hints, rc) result(spec)
      type (RegridderSpec) :: spec
      type (ESMF_Grid), intent(in) :: grid_in
      type (ESMF_Grid), intent(in) :: grid_out
      integer :: regrid_method
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(in) :: hints
      integer, optional, intent(out) :: rc

      _UNUSED_DUMMY(unusable)
      _UNUSED_DUMMY(rc)

      spec%grid_in = grid_in
      spec%grid_out = grid_out
      spec%regrid_method = regrid_method

      if (present(hints)) spec%hints = hints

   end function newRegridderSpec


   !---------------
   ! Two regridders are equivalent if their specs are identical.  This
   ! can be used to check if a given regridder is already contained in
   ! the regridder_manager without instantiating the regridder.
   ! Currently the implementation assumes that two regridders are identical
   ! if the corresponding grids are the same and the same method is in use.
   ! This will need to be changed to incorporate LocStreams.
   !---------------
   logical function equals(a, b)
      use MAPL_GridManagerMod, only: get_factory_id
      class (RegridderSpec), intent(in) :: a
      type (RegridderSpec), intent(in) :: b

      equals = &
           & (get_factory_id(a%grid_in) == get_factory_id(b%grid_in)) .and. &
           & (get_factory_id(a%grid_out) == get_factory_id(b%grid_out)) .and. &
           & (a%regrid_method == b%regrid_method) .and. &
           & (a%hints == b%hints)

   end function equals

   subroutine get_grid_type(this,unusable,grid_type_in, grid_type_out, rc)
      class (RegridderSpec), intent(in) :: this
      class (KeywordEnforcer), optional, intent(in) :: unusable
      character(len=*), optional, intent(out) :: grid_type_in
      character(len=*), optional, intent(out) :: grid_type_out
      integer, optional, intent(out) :: rc

      integer :: status

      _UNUSED_DUMMY(unusable)

      if (present(grid_type_in)) then
         call ESMF_AttributeGet(this%grid_in,'GridType',grid_type_in,rc=status)
         _VERIFY(status)
      end if
      if (present(grid_type_out)) then
         call ESMF_AttributeGet(this%grid_out,'GridType',grid_type_out,rc=status)
         _VERIFY(status)
      end if
      _RETURN(_SUCCESS)

   end subroutine get_grid_type

   logical function less_than(a, b)
      use MAPL_GridManagerMod, only: get_factory_id
      class (RegridderSpec), intent(in) :: a
      type (RegridderSpec), intent(in) :: b
      integer (kind=INT64) :: a_in_id, b_in_id
      integer (kind=INT64) :: a_out_id, b_out_id

      integer :: a_esmf_method, b_esmf_method
      
      select case (a%regrid_method)
      case (REGRID_METHOD_CONSERVE, REGRID_METHOD_VOTE, REGRID_METHOD_FRACTION)
         a_esmf_method = REGRID_METHOD_CONSERVE
      case default
         a_esmf_method = a%regrid_method
      end select

      select case (b%regrid_method)
      case (REGRID_METHOD_CONSERVE, REGRID_METHOD_VOTE, REGRID_METHOD_FRACTION)
         b_esmf_method = REGRID_METHOD_CONSERVE
      case default
         b_esmf_method = b%regrid_method
      end select

      if (a_esmf_method > b_esmf_method) then
         less_than = .false.
         return
      elseif (a_esmf_method < b_esmf_method) then
         less_than = .true.
         return
      end if

      a_in_id = get_factory_id(a%grid_in)
      b_in_id = get_factory_id(b%grid_in)
      if (a_in_id > b_in_id) then
         less_than = .false.
         return
      elseif (a_in_id < b_in_id) then
         less_than = .true.
         return
      end if
         
      a_out_id = get_factory_id(a%grid_out)
      b_out_id = get_factory_id(b%grid_out)
      if (a_out_id > b_out_id) then
         less_than = .false.
         return
      elseif (a_out_id < b_out_id) then
         less_than = .true.
         return
      end if

      less_than = .false.
      return
         
   end function less_than

end module MAPL_RegridderSpec
#undef _UNUSED_DUMMY

