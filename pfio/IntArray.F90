!------------------------------------------------------------------------------
!               Global Modeling and Assimilation Office (GMAO)                !
!                    Goddard Earth Observing System (GEOS)                    !
!                                 MAPL Component                              !
!------------------------------------------------------------------------------
#include "MAPL_ErrLog.h"
#include "unused_dummy.H"

!>
!### MODULE: `pFIO_IntArrayMod`
!
! Author: GMAO SI-Team
!
! The module `pFIO_IntArrayMod` is a simple integer 1d attribute to avoid ifort bug ( until 2024.0.0)
!
module pFIO_IntArrayMod

   use pFIO_ConstantsMod
   use pFIO_UtilitiesMod
   use MAPL_ExceptionHandling
   use, intrinsic :: iso_fortran_env, only: INT32, INT64
   use, intrinsic :: iso_fortran_env, only: REAL32, REAL64

   implicit none
   private

   public :: IntArray
   type :: IntArray
      private
      integer, pointer :: values(:)
   contains
      procedure :: get_values
      procedure :: destroy
   end type IntArray

   interface IntArray
      module procedure new_IntArray_1d    !! vector constructor
      module procedure new_IntArray_1d_size  !! just size
   end interface IntArray

contains

   function new_IntArray_1d(values, rc) result(attr)
      type (IntArray) :: attr
      integer, intent(in) :: values(:)
      integer, optional, intent(out) :: rc

      allocate(attr%values, source=values)

      __RETURN(__SUCCESS)
   end function new_IntArray_1d

   function new_IntArray_1d_size(size, rc) result(attr)
      type (IntArray) :: attr
      integer(kind=INT64), intent(in) :: size
      integer, optional,  intent(out) :: rc

      allocate(attr%values(size))

      __RETURN(__SUCCESS)
   end function new_IntArray_1d_size

   subroutine destroy(this, rc)
      class (IntArray), intent(inout) :: this
      integer, optional, intent(out) :: rc
      if(associated(this%values)) deallocate(this%values)
      __RETURN(__SUCCESS)
   end subroutine destroy

   function get_values(this, rc) result(values)
      class (IntArray), target, intent(in) :: this
      integer, optional, intent(out) :: rc
      integer, pointer :: values(:)

      if (associated(this%values)) then
        values => this%values
      else
        values => null()
      end if
      __RETURN(__SUCCESS)
   end function get_values

end module pFIO_IntArrayMod


! The following module defines an FTL map (associative array) with keys that are deferred
! length strings and values that are IntArrays.

module pFIO_StringIntArrayMapMod
   use pFIO_IntArrayMod
   
#include "types/key_deferredLengthString.inc"   
#define _value type (IntArray)
#define _value_equal_defined

#define _map StringIntArrayMap
#define _iterator StringIntArrayMapIterator

#define _alt
#include "templates/map.inc"
   
end module pFIO_StringIntArrayMapMod
