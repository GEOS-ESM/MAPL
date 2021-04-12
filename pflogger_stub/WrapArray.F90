!---------------------------------------------------------------------
!*MODULE: PFL_WrapArray
!
!> @brief Encapsulation of arrays as scalars.
!> @details
!! This module enables the encapsulation of arrays as scalars for
!! processing with Formatters.
!!
!! NOTE: that with Gfortran 5.1, incorrect results will be obtained
!! if a literal (as opposed to a variable) is passed to wrapArray()
!!
!! Also note: the 2003 standard permits a much cleaner implementation
!! here, by reshaping all arrays to 1D.  This would eliminate a
!! significant amount of code in FormatString.  Unfortunately,
!! GFortran 5.1 has compiletime and runtime issues with all variants of
!! this concept.  Thus, we are left with a separate type for each array
!! rank
!
!> @author ASTG staff
!> @date 01 Jan 2015 - Initial Version
! ---------------------------------------------------------------------

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
      class(*), allocatable :: array(:)
   end type WrapArray1d

   type :: WrapArray2d
      class(*), allocatable :: array(:,:)
   end type WrapArray2d

   type :: WrapArray3d
      class(*), allocatable :: array(:,:,:)
   end type WrapArray3d

   type :: WrapArray4d
      class(*), allocatable :: array(:,:,:,:)
   end type WrapArray4d

   type :: WrapArray5d
      class(*), allocatable :: array(:,:,:,:,:)
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
#ifndef __GFORTRAN__
      allocate(wrapper%array, source=array)
#else
      block
        integer :: n
        n = size(array,1)
        allocate(wrapper%array(n), source=array(1:n))
      end block
#endif
   end function wrap1d

   function wrap2d(array) result(wrapper)
      type (WrapArray2d) :: wrapper
      class (*), intent(in) :: array(:,:)
#ifndef __GFORTRAN__
      allocate(wrapper%array, source=array)
#else
      allocate(wrapper%array(size(array,1),size(array,2)), source=array)
#endif
   end function wrap2d


   function wrap3d(array) result(wrapper)
      type (WrapArray3d) :: wrapper
      class (*), intent(in) :: array(:,:,:)
#ifndef __GFORTRAN__
      allocate(wrapper%array, source=array)
#else
      allocate(wrapper%array(size(array,1),size(array,2),size(array,3)), source=array)
#endif
   end function wrap3d


   function wrap4d(array) result(wrapper)
      type (WrapArray4d) :: wrapper
      class (*), intent(in) :: array(:,:,:,:)
#ifndef __GFORTRAN__
      allocate(wrapper%array, source=array)
#else
      allocate(wrapper%array(size(array,1),size(array,2),size(array,3),size(array,4)), &
           &source=array)
#endif
   end function wrap4d

   function wrap5d(array) result(wrapper)
      type (WrapArray5d) :: wrapper
      class (*), intent(in) :: array(:,:,:,:,:)
#ifndef __GFORTRAN__
      allocate(wrapper%array, source=array)
#else
      allocate(wrapper%array( &
           & size(array,1),size(array,2),size(array,3),size(array,4),size(array,5) &
           & ), source=array)
#endif
   end function wrap5d
   
end module PFL_WrapArray
