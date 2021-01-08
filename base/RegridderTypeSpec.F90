#include "MAPL_Generic.h"
! A RegridderSpec is used to indicate which subclass of regridder will be used.
module mapl_RegridderTypeSpec
   use MAPL_BaseMod, only: MAPL_UNDEF
   use MAPL_KeywordEnforcerMod
   use mapl_RegridMethods
   use ESMF
   use, intrinsic :: iso_fortran_env, only: INT64
   implicit none
   private

   public :: RegridderTypeSpec 

   type :: RegridderTypeSpec
      character(len=:), allocatable :: grid_type_in
      character(len=:), allocatable :: grid_type_out
      integer :: regrid_method = UNSPECIFIED_REGRID_METHOD
   contains
      procedure :: less_than
      generic :: operator (<) => less_Than
   end type RegridderTypeSpec

   interface RegridderTypeSpec
      module procedure new_RegridderTypeSpec
   end interface RegridderTypeSpec


contains


   function new_RegridderTypeSpec(grid_type_in, grid_type_out, regrid_method, unusable, rc) result(spec)
      type (RegridderTypeSpec) :: spec
      character(len=*), intent(in) :: grid_type_in
      character(len=*), intent(in) :: grid_type_out
      integer, intent(in) :: regrid_method
      class (KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      _UNUSED_DUMMY(unusable)
      _UNUSED_DUMMY(rc)

      spec%grid_type_in = grid_type_in
      spec%grid_type_out = grid_type_out
      spec%regrid_method = regrid_method

   end function new_RegridderTypeSpec

   logical function less_than(a, b)
      class (RegridderTypeSpec), intent(in) :: a
      type (RegridderTypeSpec), intent(in) :: b

      ! Compare methods

      if (a%regrid_method /= b%regrid_method) then
         less_than = (a%regrid_method < b%regrid_method) ! strictly less than
         return
      end if ! else tie
         
      ! To get here, methods are the same for a and b.
      if (any(a%regrid_method == TILING_METHODS)) then
         less_than = .false.
         ! do not care about grid types in this case
         return
      end if

      ! Compare grid types
      if (a%grid_type_in /= b%grid_type_in) then
         less_than = (a%grid_type_in < b%grid_type_in) ! strictly less than
         return
      end if ! else tie

      ! Compare out grid types
      less_than = (a%grid_type_out < b%grid_type_out)
      return
      
   end function less_than


end module mapl_RegridderTypeSpec
#undef _UNUSED_DUMMY

