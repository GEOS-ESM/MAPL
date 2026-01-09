#include "MAPL_ErrLog.h"
#include "unused_dummy.H"

module mapl_BaseProfiler
   use mapl_AdvancedMeter
   use mapl_AbstractMeter
   use mapl_AbstractMeterNode
   use mapl_MeterNode
   use mapl_MeterNodePtr
   use mapl_MeterNodeStack
   use mapl_ErrorHandlingMod
   use mapl_KeywordEnforcerMod
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
      type(MeterNode) :: root_node
      type(MeterNodeStack) :: stack
      integer :: status
      integer :: comm_world
   contains
      procedure :: start_name
      procedure :: stop_name
      procedure :: start_node
      procedure :: stop_node
      procedure :: start_self
      procedure :: stop_self
      generic :: start => start_name
      generic :: start => start_node
      generic :: start => start_self
      generic :: stop => stop_name
      generic :: stop => stop_node
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

      procedure :: reset
      procedure :: accumulate

      procedure :: begin => begin_profiler
      procedure :: end => end_profiler
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
      procedure :: next => next_profiler
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


   subroutine start_self(this, unusable, rc)
      class(BaseProfiler), target, intent(inout) :: this
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      logical :: empty_stack

      this%status = 0
      empty_stack = .true.
      !$omp master
      if (this%stack%size()/= 0) this%status = INCORRECTLY_NESTED_METERS
      empty_stack = this%stack%size()== 0

      if(empty_stack) call this%start(this%root_node)
      !$omp end master

      _ASSERT_RC(empty_stack,"Timer "//this%root_node%get_name()// " is not a fresh self start",INCORRECTLY_NESTED_METERS)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine start_self


   subroutine start_node(this, node)
      class(BaseProfiler), intent(inout) :: this
      class(AbstractMeterNode), target, intent(inout) :: node

      class(AbstractMeter), pointer :: t
      type(MeterNodePtr), pointer :: node_ptr

      !$omp master
      allocate(node_ptr)
      node_ptr%ptr => node
      call this%stack%push_back(node_ptr)
      deallocate(node_ptr)

      t => node%get_meter()
      call t%start()
      !$omp end master

   end subroutine start_node

   subroutine start_name(this, name, rc)
      class(BaseProfiler), target, intent(inout) :: this
      character(*), intent(in) :: name
      integer, optional, intent(out) :: rc

      class(AbstractMeter), allocatable :: m
      type(MeterNodePtr), pointer :: node_ptr
      class(AbstractMeterNode), pointer :: node => null()

      logical :: stack_is_not_empty

      stack_is_not_empty = .true.
      !$omp master
      if (this%stack%empty()) this%status = INCORRECTLY_NESTED_METERS
      stack_is_not_empty = .not. this%stack%empty()

      if(stack_is_not_empty) then
         node_ptr => this%stack%back()
         node => node_ptr%ptr
         if (.not. node%has_child(name)) then
            m = this%make_meter()
            call node%add_child(name, m) !this%make_meter())
         end if

         node => node%get_child(name)
      end if
      !$omp end master
      _ASSERT_RC(stack_is_not_empty, "Timer <"//name// "> should not start when empty.",INCORRECTLY_NESTED_METERS)
      !$omp master
      call this%start(node)
      !$omp end master

      _RETURN(_SUCCESS)
   end subroutine start_name


   subroutine stop_name(this, name, rc)
      class(BaseProfiler), intent(inout) :: this
      character(*), intent(in) :: name
      integer, optional, intent(out) :: rc

      type(MeterNodePtr), pointer :: node_ptr
      class(AbstractMeterNode), pointer :: node => null()

      logical :: name_is_node_name

      name_is_node_name = .true.
      !$omp master
      node_ptr => this%stack%back()
      node => node_ptr%ptr
      if (name /= node%get_name()) this%status = INCORRECTLY_NESTED_METERS
      name_is_node_name = name == node%get_name()

      if(name_is_node_name) call this%stop(node)
      !$omp end master

      _ASSERT_RC(name_is_node_name,"Timer <"//name// "> does not match start timer <"//node%get_name()//">",INCORRECTLY_NESTED_METERS)

      _RETURN(_SUCCESS)
   end subroutine stop_name

   subroutine stop_self(this, rc)
      class(BaseProfiler), intent(inout) :: this
      integer, optional, intent(out) :: rc

      class(MeterNodePtr), pointer :: node_ptr
      class(AbstractMeterNode), pointer :: node

      logical :: stack_size_is_one

      stack_size_is_one = .true.
      !$omp master
      if (this%stack%size()/= 1) this%status = INCORRECTLY_NESTED_METERS
      stack_size_is_one = this%stack%size()== 1

      if(stack_size_is_one) then
        node_ptr => this%stack%back()
        node => node_ptr%ptr
        call this%stop(node)
      end if
      !$omp end master
      _ASSERT_RC(stack_size_is_one,"Stack not empty when timer stopped.  Active timer: " // node%get_name(),INCORRECTLY_NESTED_METERS)
      _RETURN(_SUCCESS)
   end subroutine stop_self

   subroutine stop_node(this, node)
      class(BaseProfiler), intent(inout) :: this
      class(AbstractMeterNode), target, intent(inout) :: node
      class(AbstractMeter), pointer :: t

      !$omp master
      t => node%get_meter()
      call t%stop()
      call this%stack%pop_back()
      !$omp end master

   end subroutine stop_node


   integer function get_num_meters(this) result(num_meters)
      class(BaseProfiler), intent(in) :: this

      !$omp master
      num_meters = this%root_node%get_num_nodes()
      !$omp end master

   end function get_num_meters


   subroutine finalize(this)
      class(BaseProfiler), target, intent(inout) :: this

      class(AbstractMeter), pointer :: t

      !$omp master
      call this%stack%pop_back()
      t => this%root_node%get_meter()
      call t%stop()
      call t%finalize()
      !$omp end master

   end subroutine finalize

   subroutine copy_profiler(new, old)
      class(BaseProfiler), target, intent(inout) :: new
      class(BaseProfiler), target, intent(in) :: old

      type(MeterNodePtr), pointer :: node_ptr
      class(AbstractMeterNode), pointer :: subnode
      type(MeterNodePtr), pointer :: next_item
      type(MeterNodeStackIterator) :: iter
      character(:), pointer :: name

      !$omp master
      new%root_node = old%root_node
      new%comm_world = old%comm_world
      subnode => new%root_node

      ! Stack always starts with root node of node

      if (.not. old%stack%empty()) then

         iter = old%stack%begin()
         node_ptr%ptr => subnode
         call new%stack%push_back(node_ptr)
         call iter%next()

         do while (iter /= old%stack%end())
            next_item => iter%of()
            name => next_item%ptr%get_name()
            subnode => subnode%get_child(name)
            node_ptr%ptr => subnode
            call new%stack%push_back(node_ptr)
            call iter%next()
         end do
      end if
      !$omp end master

   end subroutine copy_profiler


   integer function get_status(this) result(status)
      class(BaseProfiler), intent(in) :: this
      !$omp master
      status = this%status
      !$omp end master
   end function get_status



   function get_root_node(this) result(root_node)
      class(AbstractMeterNode), pointer :: root_node
      class(BaseProfiler), target, intent(in) :: this

      !$omp master
      root_node => this%root_node
      !$omp end master

   end function get_root_node


   ! TODO: move most logic to MeterNode
   recursive subroutine reset(this)
      class(BaseProfiler), target, intent(inout) :: this
      class(AbstractMeterNodeIterator), allocatable :: iter
      class(AbstractMeterNode), pointer :: node
      class(AbstractMeter), pointer :: t

      !$omp master
      node => this%get_root_node()
      iter = node%begin()
      do while (iter /= node%end())
         t => iter%get_meter()
         call t%reset()
         call iter%next()
      end do

      call this%start()
      !$omp end master

   end subroutine reset


   recursive subroutine accumulate(a, b)
      class(BaseProfiler), target, intent(inout) :: a
      class(BaseProfiler), target, intent(in) :: b

      type(MeterNodePtr), pointer :: node_ptr
      class(AbstractMeterNode), pointer :: node_a, node_b

      !$omp master
      node_ptr => a%stack%back()
      node_a => node_ptr%ptr

      node_b => b%get_root_node()

      call node_a%accumulate(node_b)
      !$omp end master

   end subroutine accumulate


   function begin_profiler(this) result(iterator)
      type (BaseProfilerIterator) :: iterator
      class (BaseProfiler), target, intent(in) :: this

      !$omp master
      iterator%node_iterator = this%root_node%begin()
      !$omp end master
   end function begin_profiler

   function end_profiler(this) result(iterator)
      type (BaseProfilerIterator) :: iterator
      class (BaseProfiler), target, intent(in) :: this

      !$omp master
      iterator%node_iterator = this%root_node%end()
      !$omp end master
   end function end_profiler


   subroutine next_profiler(this)
      class (BaseProfilerIterator), intent(inout) :: this
      !$omp master
      call this%node_iterator%next()
      !$omp end master
   end subroutine next_profiler

   ! Type cast to concrete class for convenience of client code.
   function get_node(this) result(node)
      class (MeterNode), pointer :: node
      class (BaseProfilerIterator), target, intent(in) :: this

      class (AbstractMeterNode), pointer :: abstract_node

      !$omp master
      abstract_node => this%node_iterator%get()
      select type (q => abstract_node)
      class is (MeterNode)
         node => q
      class default
         error stop "missing error handling in " // __FILE__
      end select
      !$omp end master

   end function get_node


   subroutine set_node(this, node)
      class(BaseProfiler), intent(inout) :: this
      class(MeterNode), intent(in) :: node
      !$omp master
      this%root_node = node
      !$omp end master
   end subroutine set_node

   function get_name(this) result(name)
      character(:), pointer :: name
      class (BaseProfilerIterator), target, intent(in) :: this
      !$omp master
      name => this%node_iterator%get_name()
      !$omp end master
   end function get_name

   function get_meter(this) result(meter)
      class (AdvancedMeter), pointer :: meter
      class (BaseProfilerIterator), target, intent(in) :: this

      class (AbstractMeter), pointer :: abstract_meter

      !$omp master
      abstract_meter => this%node_iterator%get_meter()
      select type (q => abstract_meter)
      class is (AdvancedMeter)
         meter => q
      class default
         print*,'put error handling here'
      end select
      !$omp end master
   end function get_meter

   logical function equals(this, other)
      class (BaseProfilerIterator), intent(in) :: this
      class (BaseProfilerIterator), intent(in) :: other
      !$omp master
      equals = (this%node_iterator == other%node_iterator)
      !$omp end master
   end function equals

   logical function not_equals(this, other)
      class (BaseProfilerIterator), intent(in) :: this
      class (BaseProfilerIterator), intent(in) :: other
      !$omp master
      not_equals = .not. (this == other)
      !$omp end master
   end function not_equals

   integer function get_depth(this) result(depth)
      class(BaseProfiler), intent(in) :: this
      !$omp master
      depth = this%stack%size()
      !$omp end master
   end function get_depth

   subroutine set_comm_world(this, comm_world)
      use MPI
      class(BaseProfiler), intent(inout) :: this
      integer, optional, intent(in) :: comm_world

      !$omp master
      if(present(comm_world)) then
        this%comm_world = comm_world
      else
        this%comm_world =  MPI_COMM_WORLD
      endif
      !$omp end master
   end subroutine set_comm_world

   ! For debugging
   subroutine print_stack(s)
      type(MeterNodeStack), intent(in) :: s
      type(MeterNodeStackIterator) :: iter
      type(MeterNodePtr), pointer :: node_ptr

      !$omp master
      print*
      print*,'Stack Size: ', s%size()
      print*,'---------------'
      associate(b => s%begin(), e => s%end())
        iter = b
        do while (iter /= e)
           node_ptr => iter%of()
           print*,node_ptr%ptr%get_name()
           call iter%next()
        end do
      end associate
      print*,'---------------'
      print*
      !$omp end master

   end subroutine print_stack

end module mapl_BaseProfiler



