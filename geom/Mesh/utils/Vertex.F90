#include "MAPL_ErrLog.h"
module sf_Vertex
   use sf_Direction
   use mapl_ErrorHandling
   use gFTL2_IntegerVector
   use, intrinsic :: iso_fortran_env, only: INT64, REAL64
   implicit none(type,external)
   private

   public :: Vertex

   type :: Vertex
      integer :: loc(2) ! coordinates in original pixel array
      integer :: connections(N_DIR) = -1 
   contains
      procedure :: degree
      procedure :: get_connection
      procedure :: has_connection

!#      procedure :: insert_connection_vertex
      procedure :: insert_connection_dir
!#      generic :: insert_connection => insert_connection_vertex
      generic :: insert_connection => insert_connection_dir
  
      procedure :: replace_connection
      procedure :: get_direction_to
   end type Vertex

   interface Vertex
      procedure :: new_Vertex_simple
      procedure :: new_Vertex_array
   end interface Vertex

contains

   function new_Vertex_simple(loc) result(v)
      type(Vertex) :: v
      integer, intent(in) :: loc(2)

      v%loc = loc
      
   end function new_Vertex_simple

   function new_Vertex_array(loc, connections) result(v)
      type(Vertex) :: v
      integer, intent(in) :: loc(2)
      integer, intent(in) :: connections(N_DIR)

      v%loc = loc
      v%connections = connections
      
   end function new_Vertex_array

   integer function degree(this)
      class(Vertex), intent(in) :: this
      degree = count(this%connections /= -1)
   end function degree

   integer function get_connection(this, dir) result(iv)
      class(Vertex), intent(in) :: this
      integer, intent(in) :: dir
      iv = this%connections(dir)
   end function get_connection

   logical function has_connection(this, dir)
      class(Vertex), intent(in) :: this
      integer, intent(in) :: dir

      has_connection = this%connections(dir) /= -1
   end function has_connection


   integer function get_direction_to(this, v, shp, rc) result(dir)
      class(Vertex), target, intent(inout) :: this
      type(Vertex), intent(in) :: v
      integer, intent(in) :: shp(2)
      integer, optional, intent(out) :: rc

      integer :: status

!#      if (product(v%loc - this%loc) /= 0) then
!#         _HERE, v%loc
!#         _HERE, this%loc
!#      end if
      _ASSERT(product(v%loc - this%loc) == 0, 'Illegal direction.')
      _ASSERT(.not. all(v%loc == this%loc), 'Cannot connect to self - indeterminate direction')

      select case (v%loc(2) - this%loc(2))
      case (1:)
         dir = NORTH
      case (:-1)
         dir = SOUTH
      case (0) ! East-west is periodic ...
         associate (ni => shp(1))
           if (modulo(v%loc(1) - this%loc(1), ni) > ni/2) then
              dir = WEST
           else
              dir = EAST
           end if
         end associate
      end select

      _RETURN(_SUCCESS)

   end function get_direction_to

   subroutine insert_connection_dir(v, iv, dir, rc)
      class(Vertex), target, intent(inout) :: v
      integer(kind=INT64), intent(in) :: iv
      integer, intent(in) :: dir
      integer, optional, intent(out) :: rc

      integer :: status
      
      _ASSERT(v%connections(dir) == -1, 'connection in direction already exists')
      v%connections(dir) = iv

      _RETURN(_SUCCESS)

   end subroutine insert_connection_dir



   ! [1] ----------- [2]
   !
   ! [1] --- [3] --- [2]
   
   subroutine replace_connection(this, iv, dir, rc)
      class(Vertex), target, intent(inout) :: this
      integer(kind=INT64), intent(in) :: iv
      integer, intent(in) :: dir
      integer, optional, intent(out) :: rc

      integer :: status 
      _ASSERT(this%connections(dir) > 0, 'no connection in specified direction')
      this%connections(dir) = iv

      _RETURN(_SUCCESS)
   end subroutine replace_connection

end module sf_Vertex
