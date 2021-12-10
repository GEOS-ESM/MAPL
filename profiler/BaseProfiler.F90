#include "MAPL_ErrLog.h"
#include "unused_dummy.H"

module mapl_BaseProfiler
   use mapl_AdvancedMeter
   use mapl_AbstractMeter
   use mapl_AbstractMeterNode
   use mapl_MeterNode
   use mapl_MeterNodePtr
   use mapl_MeterNodeStack
   use mapl_ExceptionHandling
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
      integer :: status = 0
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
      procedure(copy_profiler), deferred :: copy
      generic :: assignment(=) => copy

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

      if (this%stack%size()/= 0) this%status = INCORRECTLY_NESTED_METERS
      _ASSERT_RC(this%stack%size()== 0,"Timer "//this%root_node%get_name()// " is not a fresh self start",INCORRECTLY_NESTED_METERS)

      call this%start(this%root_node)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine start_self


   subroutine start_node(this, node)
      class(BaseProfiler), intent(inout) :: this
      class(AbstractMeterNode), target, intent(inout) :: node

      class(AbstractMeter), pointer :: t
      type(MeterNodePtr), pointer :: node_ptr

      allocate(node_ptr)
      node_ptr%ptr => node
      call this%stack%push_back(node_ptr)
      deallocate(node_ptr)
      
      t => node%get_meter()
      call t%start()

   end subroutine start_node

   subroutine start_name(this, name, rc)
      class(BaseProfiler), target, intent(inout) :: this
      character(*), intent(in) :: name
      integer, optional, intent(out) :: rc

      class(AbstractMeter), allocatable :: m
      type(MeterNodePtr), pointer :: node_ptr
      class(AbstractMeterNode), pointer :: node

      if (this%stack%empty()) this%status = INCORRECTLY_NESTED_METERS
      _ASSERT_RC(.not. this%stack%empty(),"Timer <"//name// "> should not start when empty.",INCORRECTLY_NESTED_METERS)

      node_ptr => this%stack%back()
      node => node_ptr%ptr
      if (.not. node%has_child(name)) then
         m = this%make_meter()
         call node%add_child(name, m) !this%make_meter())
      end if

      node => node%get_child(name)
      call this%start(node)

      _RETURN(_SUCCESS)
   end subroutine start_name


   subroutine stop_name(this, name, rc)
      class(BaseProfiler), intent(inout) :: this
      character(*), intent(in) :: name
      integer, optional, intent(out) :: rc

      type(MeterNodePtr), pointer :: node_ptr
      class(AbstractMeterNode), pointer :: node

      node_ptr => this%stack%back()
      node => node_ptr%ptr
      if (name /= node%get_name()) this%status = INCORRECTLY_NESTED_METERS
      _ASSERT_RC(name == node%get_name(),"Timer <"//name// "> does not match start timer <"//node%get_name()//">",INCORRECTLY_NESTED_METERS)

      call this%stop(node)

      _RETURN(_SUCCESS)
   end subroutine stop_name

   subroutine stop_self(this, rc)
      class(BaseProfiler), intent(inout) :: this
      integer, optional, intent(out) :: rc

      class(MeterNodePtr), pointer :: node_ptr
      class(AbstractMeterNode), pointer :: node

      if (this%stack%size()/= 1) this%status = INCORRECTLY_NESTED_METERS
      _ASSERT_RC(this%stack%size()== 1,"Stack not empty when timer stopped.",INCORRECTLY_NESTED_METERS)

      node_ptr => this%stack%back()
      node => node_ptr%ptr
      call this%stop(node)
      _RETURN(_SUCCESS)
   end subroutine stop_self

   subroutine stop_node(this, node)
      class(BaseProfiler), intent(inout) :: this
      class(AbstractMeterNode), target, intent(inout) :: node
      class(AbstractMeter), pointer :: t

      t => node%get_meter()
      call t%stop()
      call this%stack%pop_back()

   end subroutine stop_node


   integer function get_num_meters(this) result(num_meters)
      class(BaseProfiler), intent(in) :: this

      num_meters = this%root_node%get_num_nodes()

   end function get_num_meters


   subroutine finalize(this)
      class(BaseProfiler), target, intent(inout) :: this

      class(AbstractMeter), pointer :: t

      call this%stack%pop_back()
      t => this%root_node%get_meter()
      call t%stop()

   end subroutine finalize

   subroutine copy_profiler(new, old)
      class(BaseProfiler), target, intent(inout) :: new
      class(BaseProfiler), target, intent(in) :: old

      type(MeterNodePtr), pointer :: node_ptr
      class(AbstractMeterNode), pointer :: subnode
      type(MeterNodePtr), pointer :: next_item
      type(MeterNodeStackIterator) :: iter
      character(:), pointer :: name

      new%root_node = old%root_node
      new%comm_world = old%comm_world
      subnode => new%root_node

      ! Stack always starts with root node of node

      if (old%stack%empty()) return
 
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
      
   end subroutine copy_profiler


   integer function get_status(this) result(status)
      class(BaseProfiler), intent(in) :: this
      status = this%status
   end function get_status



   function get_root_node(this) result(root_node)
      class(AbstractMeterNode), pointer :: root_node
      class(BaseProfiler), target, intent(in) :: this

      root_node => this%root_node

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

      type(MeterNodePtr), pointer :: node_ptr
      class(AbstractMeterNode), pointer :: node_a, node_b

      node_ptr => a%stack%back()
      node_a => node_ptr%ptr
      
      node_b => b%get_root_node()

      call node_a%accumulate(node_b)

   end subroutine accumulate

   
   function begin_profiler(this) result(iterator)
      type (BaseProfilerIterator) :: iterator
      class (BaseProfiler), target, intent(in) :: this

      iterator%node_iterator = this%root_node%begin()
   end function begin_profiler

   function end_profiler(this) result(iterator)
      type (BaseProfilerIterator) :: iterator
      class (BaseProfiler), target, intent(in) :: this

      iterator%node_iterator = this%root_node%end()
   end function end_profiler


   subroutine next_profiler(this)
      class (BaseProfilerIterator), intent(inout) :: this
      call this%node_iterator%next()
   end subroutine next_profiler

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
         error stop "missing error handling in " // __FILE__
      end select

   end function get_node


   subroutine set_node(this, node)
      class (BaseProfiler), intent(inout) :: this
      type (MeterNode), intent(in) :: node
      this%root_node = node
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

      if(present(comm_world)) then
        this%comm_world = comm_world
      else
        this%comm_world =  MPI_COMM_WORLD
      endif
   end subroutine set_comm_world

   ! For debugging
   subroutine print_stack(s)
      type(MeterNodeStack), intent(in) :: s
      type(MeterNodeStackIterator) :: iter
      type(MeterNodePtr), pointer :: node_ptr

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
        
   end subroutine print_stack
end module mapl_BaseProfiler



