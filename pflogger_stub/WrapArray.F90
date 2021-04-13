#include "unused_dummy.H"
module PFL_WrapArray
   implicit none
   private

   public :: wrapArray
   public :: WrapArray1d
   public :: WrapArray2d
   public :: WrapArray3d
   public :: WrapArray4d
   public :: WrapArray5d
   
   type :: WrapArray1d
      integer ::placeholder
   end type WrapArray1d

   type :: WrapArray2d
      integer ::placeholder
   end type WrapArray2d

   type :: WrapArray3d
      integer ::placeholder
   end type WrapArray3d

   type :: WrapArray4d
      integer ::placeholder
   end type WrapArray4d

   type :: WrapArray5d
      integer ::placeholder
   end type WrapArray5d

   interface wrapArray
      module procedure wrap1d
      module procedure wrap2d
      module procedure wrap3d
      module procedure wrap4d
      module procedure wrap5d
   end interface wrapArray

contains


   function wrap1d(array) result(wrapper)
      type (WrapArray1d) :: wrapper
      class (*), intent(in) :: array(:)
      wrapper%placeholder = 0
      _UNUSED_DUMMY(array)
   end function wrap1d

   function wrap2d(array) result(wrapper)
      type (WrapArray2d) :: wrapper
      class (*), intent(in) :: array(:,:)
      wrapper%placeholder = 0
      _UNUSED_DUMMY(array)
   end function wrap2d

   function wrap3d(array) result(wrapper)
      type (WrapArray3d) :: wrapper
      class (*), intent(in) :: array(:,:,:)
      wrapper%placeholder = 0
      _UNUSED_DUMMY(array)
   end function wrap3d


   function wrap4d(array) result(wrapper)
      type (WrapArray4d) :: wrapper
      class (*), intent(in) :: array(:,:,:,:)
      wrapper%placeholder = 0
      _UNUSED_DUMMY(array)
   end function wrap4d

   function wrap5d(array) result(wrapper)
      type (WrapArray5d) :: wrapper
      class (*), intent(in) :: array(:,:,:,:,:)
      wrapper%placeholder = 0
      _UNUSED_DUMMY(array)
   end function wrap5d
   
end module PFL_WrapArray
