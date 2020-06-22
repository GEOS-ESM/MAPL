module MAPL_AbstractMeterNode
   use MAPL_AbstractMeter
   implicit none
   private

   public :: AbstractMeterNode
   public :: AbstractMeterNodeIterator

   ! A node consists of a meter and a name.  We need an abstract base
   ! class so that we can use gFTL in a relatively painless manner.
   type, abstract :: AbstractMeterNode
      private
   contains
      procedure(i_get_meter), deferred :: get_meter
      procedure(i_get_name), deferred :: get_name
      procedure(i_get_depth), deferred :: get_depth
      procedure(i_get_inclusive), deferred :: get_inclusive
      procedure(i_get_inclusive), deferred :: get_exclusive
      procedure(i_add_child), deferred :: add_child
      procedure(i_get_child), deferred :: get_child
      procedure(i_has_child), deferred :: has_child
      procedure(i_get_num_nodes), deferred :: get_num_children
      procedure(i_get_num_nodes), deferred :: get_num_nodes
      procedure(i_reset), deferred :: reset
      procedure(i_accumulate), deferred :: accumulate

      ! Iterator factory methods
      procedure(i_make_iterator), deferred :: begin
      procedure(i_make_iterator), deferred :: end
   end type AbstractMeterNode
   
   type, abstract :: AbstractMeterNodeIterator
      private
   contains
      procedure(i_get), deferred :: get
      procedure(i_iter_get_meter), deferred :: get_meter
      procedure(i_iter_get_name), deferred :: get_name
      procedure(i_compare), deferred :: equals
      procedure(i_compare), deferred :: not_equals
      generic :: operator(==) => equals
      generic :: operator(/=) => not_equals
      procedure(i_next), deferred :: next
   end type AbstractMeterNodeIterator


   abstract interface

      function i_get_meter(this) result(meter)
         import AbstractMeter
         import AbstractMeterNode
         class(AbstractMeter), pointer :: meter
         class(AbstractMeterNode), target, intent(in) :: this
      end function i_get_meter

      function i_get_depth(this) result(depth)
         import AbstractMeterNode
         integer :: depth
         class(AbstractMeterNode), intent(in) :: this
      end function i_get_depth


      subroutine i_add_child(this, name, meter)
         import AbstractMeterNode
         import AbstractMeter
         class(AbstractMeterNode), target, intent(inout) :: this
         character(*), intent(in) :: name
         class (AbstractMeter), intent(in) :: meter
      end subroutine i_add_child


      function i_get_child(this, name) result(children)
         import AbstractMeterNode
         class(AbstractMeterNode), pointer :: children
         class(AbstractMeterNode), target, intent(inout) :: this
         character(*), intent(in) :: name
      end function i_get_child


      logical function i_has_child(this, name)
         import AbstractMeterNode
         class(AbstractMeterNode), pointer :: children
         class(AbstractMeterNode), target, intent(in) :: this
         character(*), intent(in) :: name
      end function i_has_child


      integer function i_get_num_nodes(this) result(num_nodes)
         import AbstractMeterNode
         class(AbstractMeterNode), target, intent(in) :: this
      end function i_get_num_nodes


      subroutine i_accumulate(this, other)
         import AbstractMeterNode
         class(AbstractMeterNode), intent(inout) :: this
         class(AbstractMeterNode), target, intent(in) :: other
      end subroutine i_accumulate


      function i_get(this) result(node)
         import AbstractMeterNode
         import AbstractMeterNodeIterator
         class(AbstractMeterNode), pointer :: node
         class(AbstractMeterNodeIterator), target, intent(in) :: this
      end function i_get


      function i_iter_get_meter(this) result(t)
         import AbstractMeterNode
         import AbstractMeterNodeIterator
         import AbstractMeter
         class(AbstractMeter), pointer :: t
         class(AbstractMeterNodeIterator), intent(in) :: this
      end function i_iter_get_meter


      function i_iter_get_name(this) result(name)
         import AbstractMeterNode
         import AbstractMeterNodeIterator
         character(:), pointer :: name
         class(AbstractMeterNodeIterator), intent(in) :: this
      end function i_iter_get_name


      function i_make_iterator(this) result(iterator)
         import AbstractMeterNode
         import AbstractMeterNodeIterator
         class(AbstractMeterNodeIterator), allocatable :: iterator
         class(AbstractMeterNode), target, intent(in) :: this
      end function i_make_iterator


      logical function i_compare(a, b)
         import AbstractMeterNodeIterator
         class(AbstractMeterNodeIterator), intent(in) :: a
         class(AbstractMeterNodeIterator), intent(in) :: b
      end function i_compare


      subroutine i_next(this)
         import AbstractMeterNodeIterator
         class(AbstractMeterNodeIterator), intent(inout) :: this
      end subroutine i_next


      function i_get_name(this) result(name)
         import AbstractMeterNode
         character(:), pointer :: name
         class(AbstractMeterNode), target, intent(in) :: this
      end function i_get_name


      function i_get_inclusive(this) result(inclusive)
         use, intrinsic :: iso_fortran_env, only: REAL64
         import AbstractMeterNode
         real(kind=REAL64) :: inclusive
         class(AbstractMeterNode), intent(in) :: this
      end function i_get_inclusive
      
      subroutine i_reset(this)
         import AbstractMeterNode
         class(AbstractMeterNode), target, intent(inout) :: this
      end subroutine i_reset

   end interface

end module MAPL_AbstractMeterNode
