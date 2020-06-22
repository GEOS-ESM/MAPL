#include"MAPL_ErrLog.h"

module MAPL_BaseProfiler
   use MAPL_AdvancedMeter
   use MAPL_AbstractMeter
   use MAPL_AbstractMeterNode
   use MAPL_MeterNode
   use MAPL_MeterNodeStack
   use MAPL_ExceptionHandling
   implicit none
   private

   public :: BaseProfiler
   public :: BaseProfilerIterator

   public :: INCORRECTLY_NESTED_METERS

   enum, bind(c)
      enumerator :: INCORRECTLY_NESTED_METERS=1
   end enum
   
   type, abstract :: BaseProfiler
      private
      type(MeterNode) :: node
      type(MeterNodeStack) :: stack
      integer :: status = 0
      integer :: comm_world
   contains
      procedure :: start_name
      procedure :: stop_name
      procedure :: start_self
      procedure :: stop_self
      generic :: start => start_name
      generic :: start => start_self
      generic :: stop => stop_name
      generic :: stop => stop_self
      generic :: zeit_ci => start_name
      generic :: zeit_co => stop_name
      procedure :: get_num_meters
      procedure :: finalize

      ! Override make_meter() to measure other things.
      procedure(i_make_meter), deferred :: make_meter

      procedure :: set_node
      procedure :: get_root_node
      procedure :: get_status
      procedure :: copy_profiler
      procedure(copy_profiler), deferred :: copy
      generic :: assignment(=) => copy

      procedure :: reset
      procedure :: accumulate

      procedure :: begin
      procedure :: end
      procedure :: get_depth
      procedure :: set_comm_world
      
   end type BaseProfiler

   type :: BaseProfilerIterator
      private
      class (AbstractMeterNodeIterator), allocatable :: node_iterator
   contains
      procedure :: get_node
      procedure :: get_meter
      procedure :: get_name
      procedure :: next
      procedure :: equals
      procedure :: not_equals
      generic :: operator(==) => equals
      generic :: operator(/=) => not_equals
   end type BaseProfilerIterator

   abstract interface

      function i_make_meter(this) result(meter)
         import AbstractMeter
         import BaseProfiler
         class(AbstractMeter), allocatable :: meter
         class(BaseProfiler), intent(in) :: this
      end function i_make_meter
      
   end interface


contains


   subroutine start_self(this, rc)
      class(BaseProfiler), target, intent(inout) :: this
      integer, optional, intent(out) :: rc
      class(AbstractMeter), pointer :: t

      call this%stack%push_back(this%node)

      if( .not. this%stack%size() == 1) then
         block
           use MPI
           integer :: rank, ierror
           call MPI_Comm_rank(this%comm_world, rank, ierror)
           if (rank == 0) then
             _ASSERT(.false., "Timer "//this%node%get_name()// ' is not a fresh self start')
           end if
         end block
      end if

      t => this%node%get_meter()
      call t%start()
      _RETURN(_SUCCESS)
   end subroutine start_self

   subroutine start_name(this, name, rc)
      class(BaseProfiler), target, intent(inout) :: this
      character(*), intent(in) :: name
      integer, optional, intent(out) :: rc

      class(AbstractMeter), pointer :: t
      class(AbstractMeterNode), pointer :: node
      class(AbstractMeter), allocatable :: m

      if (this%stack%empty()) then
         block
           use MPI
           integer :: rank, ierror
           call MPI_Comm_rank(MPI_COMM_WORLD, rank, ierror)
           if (rank == 0) then
             _ASSERT(.false., "Timer "//name//' should not start when empty: ')
           end if
         end block
         return
      else
         node => this%stack%back()
         if (.not. node%has_child(name)) then
           m = this%make_meter()
           call node%add_child(name, m) !this%make_meter())
         end if
      endif

      node => node%get_child(name)
      call this%stack%push_back(node)
      
      t => node%get_meter()
      call t%start()

      _RETURN(_SUCCESS)
   end subroutine start_name


   subroutine stop_name(this, name, rc)
      class(BaseProfiler), intent(inout) :: this
      character(*), intent(in) :: name
      integer, optional, intent(out) :: rc

      class(AbstractMeter), pointer :: t
      class(AbstractMeterNode), pointer :: node

      node => this%stack%back()
      t => node%get_meter()
      if (name /= node%get_name()) then
         this%status = INCORRECTLY_NESTED_METERS
         block
           use MPI
           integer :: rank, ierror
           call MPI_Comm_rank(this%comm_world, rank, ierror)
           if (rank == 0) then
             _ASSERT(.false., "Timer "//name// " likely does not find its pair")
           end if
         end block
         return
      end if
      call t%stop()
      call this%stack%pop_back()

      _RETURN(_SUCCESS)
   end subroutine stop_name

   subroutine stop_self(this, rc)
      class(BaseProfiler), intent(inout) :: this
      integer, optional, intent(out) :: rc

      class(AbstractMeter), pointer :: t
      class(AbstractMeterNode), pointer :: node

      if( .not. this%stack%size() == 1) then
         block
           use MPI
           integer :: rank, ierror
           call MPI_Comm_rank(this%comm_world, rank, ierror)
           if (rank == 0) then
             _ASSERT(.false., "There are still something on the stack when stop timer")
           end if
         end block
      end if

      node => this%stack%back()
      t => node%get_meter()
      call t%stop()
      call this%stack%pop_back()

      _RETURN(_SUCCESS)
   end subroutine stop_self


   integer function get_num_meters(this) result(num_meters)
      class(BaseProfiler), intent(in) :: this

      num_meters = this%node%get_num_nodes()

   end function get_num_meters


   subroutine finalize(this)
      class(BaseProfiler), target, intent(inout) :: this

      class(AbstractMeter), pointer :: t

      call this%stack%pop_back()
      t => this%node%get_meter()
      call t%stop()

   end subroutine finalize

   subroutine copy_profiler(new, old)
      class(BaseProfiler), target, intent(inout) :: new
      class(BaseProfiler), target, intent(in) :: old

      class(AbstractMeterNode), pointer :: subnode
      class(AbstractMeterNode), pointer :: next_item
      type(MeterNodeStackIterator) :: iter
      character(:), pointer :: name

      new%node = old%node
      new%comm_world = old%comm_world
      subnode => new%node

      ! Stack always starts with root node of node

      if (old%stack%empty()) return
 
      iter = old%stack%begin()
      call new%stack%push_back(subnode)
      call iter%next()

      do while (iter /= old%stack%end())
         next_item => iter%get()
         name => next_item%get_name()
         subnode => subnode%get_child(name)
         call new%stack%push_back(subnode)
         call iter%next()
      end do
      
   end subroutine copy_profiler


   integer function get_status(this) result(status)
      class(BaseProfiler), intent(in) :: this
      status = this%status
   end function get_status



   function get_root_node(this) result(root_node)
      class(AbstractMeterNode), pointer :: root_node
      class(BaseProfiler), target, intent(in) :: this

      root_node => this%node

   end function get_root_node


   ! TODO: move most logic to MeterNode
   recursive subroutine reset(this)
      class(BaseProfiler), target, intent(inout) :: this
      class(AbstractMeterNodeIterator), allocatable :: iter
      class(AbstractMeterNode), pointer :: node
      class(AbstractMeter), pointer :: t

      node => this%get_root_node()
      iter = node%begin()
      do while (iter /= node%end())
         t => iter%get_meter()
         call t%reset()
         call iter%next()
      end do

      call this%start()
      
   end subroutine reset


   recursive subroutine accumulate(a, b)
      class(BaseProfiler), target, intent(inout) :: a
      class(BaseProfiler), target, intent(in) :: b

      class(AbstractMeterNode), pointer :: node_a, node_b

      node_a => a%stack%back()
      node_b => b%get_root_node()

      call node_a%accumulate(node_b)

   end subroutine accumulate

   
   function begin(this) result(iterator)
      type (BaseProfilerIterator) :: iterator
      class (BaseProfiler), target, intent(in) :: this

      iterator%node_iterator = this%node%begin()
   end function begin

   function end(this) result(iterator)
      type (BaseProfilerIterator) :: iterator
      class (BaseProfiler), target, intent(in) :: this

      iterator%node_iterator = this%node%end()
   end function end


   subroutine next(this)
      class (BaseProfilerIterator), intent(inout) :: this
      call this%node_iterator%next()
   end subroutine next

   ! Type cast to concrete class for convenience of client code.
   function get_node(this) result(node)
      class (MeterNode), pointer :: node
      class (BaseProfilerIterator), target, intent(in) :: this

      class (AbstractMeterNode), pointer :: abstract_node
      
      abstract_node => this%node_iterator%get()
      select type (q => abstract_node)
      class is (MeterNode)
         node => q
      class default
         print*,'put error handling here'
      end select

   end function get_node


   subroutine set_node(this, node)
      class (BaseProfiler), intent(inout) :: this
      type (MeterNode), intent(in) :: node
      this%node = node
   end subroutine set_node

   function get_name(this) result(name)
      character(:), pointer :: name
      class (BaseProfilerIterator), target, intent(in) :: this
      name => this%node_iterator%get_name()
   end function get_name

   function get_meter(this) result(meter)
      class (AdvancedMeter), pointer :: meter
      class (BaseProfilerIterator), target, intent(in) :: this

      class (AbstractMeter), pointer :: abstract_meter

      abstract_meter => this%node_iterator%get_meter()
      select type (q => abstract_meter)
      class is (AdvancedMeter)
         meter => q
      class default
         print*,'put error handling here'
      end select
   end function get_meter

   logical function equals(this, other)
      class (BaseProfilerIterator), intent(in) :: this
      class (BaseProfilerIterator), intent(in) :: other
      equals = (this%node_iterator == other%node_iterator)
   end function equals

   logical function not_equals(this, other)
      class (BaseProfilerIterator), intent(in) :: this
      class (BaseProfilerIterator), intent(in) :: other
      not_equals = .not. (this == other)
   end function not_equals

   integer function get_depth(this) result(depth)
      class(BaseProfiler), intent(in) :: this
      depth = this%stack%size()
   end function get_depth

   subroutine set_comm_world(this, comm_world)
      use MPI
      class(BaseProfiler), intent(inout) :: this
      integer, optional, intent(in) :: comm_world
      integer :: status

      if(present(comm_world)) then
        call MPI_Comm_dup(comm_world, this%comm_world, status)
      else
        this%comm_world =  MPI_COMM_WORLD
      endif
   end subroutine set_comm_world

end module MAPL_BaseProfiler



