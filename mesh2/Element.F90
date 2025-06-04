#include "MAPL_ErrLog.h"
module sf_Element
   use sf_Direction
   use mapl_Errorhandling
   use, intrinsic :: iso_fortran_env, only: INT32, INT64
   implicit none(type,external)
   private

   public :: Element
   public :: PIXEL_KIND

   integer, parameter :: PIXEL_KIND = INT32


   ! [1] ----------- [2]
   
   ! [1] --- [3] --- [2]
   
   type :: Element
      integer(kind=INT64) :: iv_0 ! idx of vertex at SW corner
      integer :: dir  ! dir=1 for east, and clockwise from there; Will
                      ! be east for all except the element covering
                      ! the south pole.

      integer(kind=PIXEL_KIND), pointer :: pixels(:,:) => null()
      logical :: fully_refined = .false.
   contains
      procedure :: do_refine
      procedure :: is_fully_refined
      procedure :: set_fully_refined
   end type Element

   interface Element
      procedure :: new_Element
   end interface Element

contains

   function new_Element(pixels, iv_0, dir) result(e)
      type(Element) :: e
      integer(kind=PIXEL_KIND), target, intent(in) :: pixels(:,:)
      integer(kind=INT64), intent(in) :: iv_0 ! id of vertex in mesh
      integer, intent(in) :: dir ! EAST, SOUTH, WEST, NORTH

      e%pixels => pixels
      e%iv_0 = iv_0
      e%dir = dir
      e%fully_refined = .false.
      
   end function new_Element


   !    [4] ------------------- [3]
   !     |                       |
   !     |                       |
   !     |                       |
   !     |         1             |
   !     |                       |
   !     |                       |
   !     |                       |   
   !     |                       |
   !    [1] ------------------- [2]

   !
   ! When refine by factor of 3 we get:
   !

   !    [4] ------------------- [3]
   !     |                       |
   !     |          3            |
   !    [7] ................... [6|
   !     |                       |
   !     |          2            |
   !    [8] ................... [5]
   !     |                       |
   !     |          1            |
   !    [1] ------------------- [2]

 


   ! An element must be refined if it contains more than one value in
   ! its pixels.
   logical function do_refine(e)
      class(Element), intent(in) :: e

      integer(kind=PIXEL_KIND) :: archetype
      integer :: i, j
      integer :: ni, nj

      if (e%fully_refined) then
         do_refine = .false.
         return
      end if

      nj = size(e%pixels,2)
      ni = size(e%pixels,1)
      if (ni*nj == 0) then
         do_refine = .false.
         return
      end if

      ! Use "far corner" as the archetype.   Maximizes chance of
      ! early detection that element needs to be refined while still
      ! allowing streaming forward in memory.
      archetype = e%pixels(ni,nj)
      
      do j = 1, nj
         do i = 1, ni
            if (e%pixels(i,j) /= archetype) then
               do_refine = .true.
               return
            end if
         end do
      end do

      do_refine = .false.
      
   end function do_refine

   subroutine set_fully_refined(this)
      class(Element), intent(inout) :: this
      this%fully_refined = .true.
   end subroutine set_fully_refined

   logical function is_fully_refined(this)
      class(Element), intent(in) :: this
      is_fully_refined = this%fully_refined
   end function is_fully_refined
end module sf_Element
