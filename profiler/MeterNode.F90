module MAPL_MeterNode
   use, intrinsic :: iso_fortran_env, only: REAL64, REAL128
   use MAPL_AbstractMeter
   use MAPL_AbstractMeterNode
   use MAPL_MeterNodeVector
   implicit none
   private

   public :: MeterNode
   public :: MeterNodeIterator

   type, extends(AbstractMeterNode) :: MeterNode
      private

      ! Node data
      class(AbstractMeter), allocatable :: meter
      character(:), allocatable :: name

      ! Tree structure
      integer :: depth
      type (MeterNodeVector) :: children
      integer :: last_child_accessed = 0

   contains
      procedure :: get_meter
      procedure :: get_name
      procedure :: get_depth
      procedure :: get_inclusive
      procedure :: get_exclusive
      procedure :: add_child
      procedure :: find_child
      procedure :: get_child
      procedure :: has_child
      procedure :: get_num_nodes
      procedure :: get_num_children

      procedure :: accumulate
      procedure :: reset

      procedure :: begin
      procedure :: end
   end type MeterNode


   type, extends(AbstractMeterNodeIterator) :: MeterNodeIterator
      private
      class (MeterNode), pointer :: reference => null()
      class (AbstractMeterNode), pointer :: current => null()

      ! Subiterators are allocated after iterator goes beyond the root node
      type (MeterNodeVectorIterator), allocatable :: iterator_over_children
      class (AbstractMeterNodeIterator), allocatable :: iterator_of_current_child
   contains
      procedure :: get
      procedure :: get_name => get_name_iter
      procedure :: get_meter => get_meter_iter
      procedure :: equals
      procedure :: not_equals
      procedure :: next
   end type MeterNodeIterator


   interface MeterNode
      module procedure new_MeterNode
   end interface MeterNode

   interface MeterNodeIterator
      module procedure new_MeterNodeIterator
   end interface MeterNodeIterator
      

   integer, parameter :: NOT_FOUND = -1
   
contains


   function new_MeterNode(name, meter, depth) result(tree)
      type (MeterNode) :: tree
      character(*), intent(in) :: name
      class(AbstractMeter), intent(in) :: meter
      integer, optional, intent(in) :: depth
      
      tree%name = name
      tree%meter = meter

      if (present(depth)) then
         tree%depth = depth
      else
         tree%depth = 0
      end if

      tree%last_child_accessed = 0

   end function new_MeterNode


   function get_meter(this) result(meter)
      class (AbstractMeter), pointer :: meter
      class (MeterNode), target, intent(in) :: this
      meter => this%meter
   end function get_meter
   

   function get_name(this) result(name)
      character(:), pointer :: name
      class (MeterNode), target, intent(in) :: this
      name => this%name
   end function get_name
   

   function get_inclusive(this) result(inclusive)
      real(kind=REAL64) :: inclusive
      class (MeterNode), intent(in) :: this
      inclusive = this%meter%get_total()
   end function get_inclusive


   function get_exclusive(this) result(exclusive)
      real(kind=REAL64) :: exclusive
      class (MeterNode), intent(in) :: this

      type (MeterNodevectorIterator) :: iter
      class (AbstractMeterNode), pointer :: child
      real(kind=REAL128) :: tmp

      ! Subtract time of submeters from time of node meter.  Note the
      ! use of 128-bit precision to avoid negative exclusive times due
      ! to roundoff.
      tmp = this%get_inclusive()

      iter = this%children%begin()
      do while (iter /= this%children%end())
         child => iter%get()
         tmp = tmp - child%get_inclusive()
         call iter%next()
      end do

      exclusive = tmp
   end function get_exclusive


   subroutine add_child(this, name, meter)
      class(MeterNode), target, intent(inout) :: this
      character(*), intent(in) :: name
      class(AbstractMeter), intent(in) :: meter

      class(AbstractMeterNode), pointer :: child
      type (MeterNode) :: tmp
      integer :: idx

      idx = this%find_child(name)

      if (idx == NOT_FOUND) then ! really add child
         tmp = MeterNode(name, meter, this%get_depth()+1)
         call this%children%push_back(tmp)
         ! Note: last still references the previous child because we are likely
         ! to follow this call with a get_child(), which should then be the 1st child
         ! tested.
         this%last_child_accessed = this%children%size() - 1
      else
         ! node exists - makes it easier on client code to not throw
         ! an exception here.
      end if

   end subroutine add_child


   function get_depth(this) result(depth)
      integer :: depth
      class (MeterNode), intent(in) :: this
      depth = this%depth
   end function get_depth


   ! TODO:  needs return code for failure
   function get_child(this, name) result(child)
      class (AbstractMeterNode), pointer :: child
      class (MeterNode), target, intent(inout) :: this
      character(*), intent(in) :: name

      integer :: idx
      
      idx = this%find_child(name)
      if (idx /= NOT_FOUND) then
         child => this%children%at(idx)
         this%last_child_accessed = idx
      else
         child => null()
         this%last_child_accessed = 0
      end if

   end function get_child

   ! We search by starting just after the last child accessed.  The
   ! theory is that meters are usually accessed cyclically in the same
   ! order as they are first created.  This is why the children
   ! are stored as a vector rather than a map with the names as keys.
   integer function find_child(this, name) result(idx)
      class (MeterNode), intent(in) :: this
      character(*), intent(in) :: name

      integer :: i, ii, n
      class (AbstractMeterNode), pointer :: t

      n = this%children%size()
      do i = 1, n
         ii = 1 + mod(i + this%last_child_accessed - 1, n)
         t => this%children%at(ii)
         select type (t)
         class is (MeterNode)
            if (name == t%name) then
               idx = ii
               return
            end if
         class default
            print*,'insert error handler'
         end select
      end do

      idx = NOT_FOUND

   end function find_child

   logical function has_child(this, name)
      class (AbstractMeterNode), pointer :: child
      class (MeterNode), target, intent(in) :: this
      character(*), intent(in) :: name
      has_child = (this%find_child(name) /= NOT_FOUND)
   end function has_child


   recursive integer function get_num_nodes(this) result(num_nodes)
      class (MeterNode), target, intent(in) :: this
      type (MeterNodeVectorIterator) :: iter

      class (AbstractMeterNode), pointer :: child
      
      num_nodes = 1
      iter = this%children%begin()
      do while (iter /= this%children%end())
         child => iter%get()
         num_nodes = num_nodes + child%get_num_nodes()
         call iter%next()
      end do

   end function get_num_nodes


   integer function get_num_children(this) result(num_children)
      class (MeterNode), target, intent(in) :: this

      num_children = this%children%size()

   end function get_num_children



   function new_MeterNodeIterator(meter_node) result(iterator)
      type (MeterNode), target, intent(in) :: meter_node
      type (MeterNodeIterator) :: iterator

      iterator%reference => meter_node
      iterator%current => meter_node

   end function new_MeterNodeIterator


   function begin(this) result(iterator)
      class (AbstractMeterNodeIterator), allocatable :: iterator
      class (MeterNode), target, intent(in) :: this

!!$      iterator = MeterNodeIterator(this)
      allocate(iterator, source=MeterNodeIterator(this))

   end function begin
      


   function end(this) result(iterator)
      class (AbstractMeterNodeIterator), allocatable :: iterator
      class (MeterNode), target, intent(in) :: this

      type (MeterNodeIterator) :: tmp

      tmp = MeterNodeIterator(this)
!!$      iterator = MeterNodeIterator(this)
      iterator = tmp

      select type (q => iterator)
      class is (MeterNodeIterator)
         q%current => null()
      class default
         print*,'uh oh'
      end select

   end function end

   
   recursive subroutine next(this)
      class (MeterNodeIterator), intent(inout) :: this
      class (AbstractMeterNode), pointer :: current_child


      if (.not. associated(this%current)) return ! done

      if (.not. allocated(this%iterator_over_children)) then
         this%iterator_over_children = this%reference%children%begin()
         if (this%iterator_over_children /= this%reference%children%end()) then
            current_child => this%iterator_over_children%get()
            this%iterator_of_current_child = current_child%begin()
            this%current => this%iterator_of_current_child%get()
         else
            this%current => null()
         end if
      else
         call this%iterator_of_current_child%next()
         this%current => this%iterator_of_current_child%get()

         if (.not. associated(this%current)) then ! go to next child
            deallocate(this%iterator_of_current_child)
            call this%iterator_over_children%next()
            if (this%iterator_over_children == this%reference%children%end()) then ! done
               deallocate(this%iterator_over_children) 
            else
               current_child => this%iterator_over_children%get()
               this%iterator_of_current_child = current_child%begin() ! always at least one node
               this%current => this%iterator_of_current_child%get()
            end if
         end if
      end if
      
   end subroutine next


   function get(this) result(tree)
      class (AbstractMeterNode), pointer :: tree
      class (MeterNodeIterator), target, intent(in) :: this
      tree => this%current
   end function get


   function get_meter_iter(this) result(t)
      class (AbstractMeter), pointer :: t
      class (MeterNodeIterator), intent(in) :: this
      t => this%current%get_meter()
   end function get_meter_iter


   function get_name_iter(this) result(name)
      character(:), pointer :: name
      class (MeterNodeIterator), intent(in) :: this
      name => this%current%get_name()
   end function get_name_iter


   logical function equals(a, b)
      class (MeterNodeIterator), intent(in) :: a
      class (AbstractMeterNodeIterator), intent(in) :: b


      select type (b)
      type is (MeterNodeIterator)
         equals = associated(a%reference, b%reference)
         if (.not. equals) return
         
         equals = associated(a%current) .eqv. associated(b%current)
         if (.not. equals) return

         if (associated(a%current)) then
            equals = associated(a%current, b%current)
            if (.not. equals) return
         end if
      class default
         equals = .false.
      end select

   end function equals


   logical function not_equals(a, b)
      class (MeterNodeIterator), intent(in) :: a
      class (AbstractMeterNodeIterator), intent(in) :: b
      not_equals = .not. (a == b)
   end function not_equals


   ! Set all meters back to 0
   recursive subroutine reset(this)
      class (MeterNode), target, intent(inout) :: this
      type (MeterNodeVectorIterator) :: iter
      class (AbstractMeterNode), pointer :: child

      call this%meter%reset

      iter = this%children%begin()
      do while (iter /= this%children%end())
         child => iter%get()
         call child%reset()
         call iter%next()
      end do

   end subroutine reset

   recursive subroutine accumulate(this, other)
      class (MeterNode), intent(inout) :: this
      class (AbstractMeterNode), target, intent(in) :: other

      class (AbstractMeterNode), pointer :: child
      class (AbstractMeterNodeIterator), allocatable :: iter
      class (AbstractMeter), pointer :: t
      character(:), pointer :: name

      ! GFortran 8.2 complains about recursive call of nonrecursive
      ! procedure (nested copy of data structure)


      name => other%get_name()
      child => this%get_child(name)
      if (associated(child)) then
         t => child%get_meter()
      else
         call this%add_child(name, this%get_meter())
         child => this%get_child(name)
         t => child%get_meter()
         call t%reset()
      end if
      call t%accumulate(other%get_meter())
         
      ! recurse over children of other
      iter = other%begin()
      call iter%next() ! skip top node (already handled)
      do while (iter /= other%end())
         call child%accumulate(iter%get())
         call iter%next()
      end do

   end subroutine accumulate

   
end module MAPL_MeterNode
