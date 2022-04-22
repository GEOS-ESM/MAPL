
module r_mapl3g_UserSetServices
   implicit none
   private

   public :: user_setservices        ! overloaded factory method
   public :: AbstractUserSetServices  ! Base class for variant SS functors
   public :: DSOSetServices
   public :: operator(==)
   public :: operator(/=)
   
   type, abstract :: AbstractUserSetServices
   contains
      procedure(I_RunSetServices), deferred :: run
   end type AbstractUserSetServices

   abstract interface

      subroutine I_RunSetServices(this, rc)
         import AbstractUserSetServices
         class(AbstractUserSetServices), intent(in) :: this
         integer, intent(out) :: rc
      end subroutine I_RunSetServices

   end interface

   type, extends(AbstractUserSetServices) :: DSOSetServices
      character(:), allocatable :: sharedObj
      character(:), allocatable :: userRoutine
   contains
      procedure :: run => run_dso_setservices
   end type DSOSetServices

   interface user_setservices
      module procedure new_dso_setservices
   end interface user_setservices

   interface operator(==)
      module procedure equal_setServices
   end interface operator(==)

   interface operator(/=)
      module procedure not_equal_setServices
   end interface operator(/=)

contains

   !----------------------------------
   ! Direct procedure support


   !----------------------------------
   ! DSO support
   
   ! Argument names correspond to ESMF arguments.
   function new_dso_setservices(sharedObj, userRoutine) result(dso_setservices)
      type(DSOSetServices) :: dso_setservices
      character(len=*), intent(in) :: sharedObj
      character(len=*), intent(in) :: userRoutine

      dso_setservices%sharedObj   = sharedObj
      dso_setservices%userRoutine = userRoutine

   end function new_dso_setservices

   subroutine run_dso_setservices(this, rc)
      class(DSOSetservices), intent(in) :: this
      integer, intent(out) :: rc

      integer :: status, userRC
      logical :: found

   end subroutine run_dso_setservices


   logical function equal_setServices(a, b) result(equal)
      class(AbstractUserSetServices), intent(in) :: a, b

      select type (a)
      type is (DSOSetservices)
         select type(b)
         type is (DSOSetservices)
            equal = equal_DSOSetServices(a,b)
         class default
            equal = .false.
         end select
      class default
         equal = .false.
      end select

   end function equal_setServices

   logical function not_equal_setServices(a, b) result(not_equal)
      class(AbstractUserSetServices), intent(in) :: a, b
      not_equal = .not. (a == b)
   end function not_equal_setServices

   logical function equal_DSOSetServices(a, b) result(equal)
      type(DSOSetServices), intent(in) :: a, b
      
      equal = (a%sharedObj == b%sharedObj) .and. (a%userRoutine == b%userRoutine)
   end function equal_DSOSetServices

   logical function not_equal_DSOSetServices(a, b) result(not_equal)
      type(DSOSetServices), intent(in) :: a, b
      not_equal = .not. (a == b)
   end function not_equal_DSOSetServices
   
end module r_mapl3g_UserSetServices


module r_mapl3g_ChildSpec
   use r_mapl3g_UserSetServices
   implicit none
   private

   public :: ChildSpec
   public :: operator(==)
   public :: operator(/=)

   public :: dump
   
   type :: ChildSpec
      character(:), allocatable :: yaml_config_file
      character(:), allocatable :: esmf_config_file
      class(AbstractUserSetServices), allocatable :: user_setservices
      ! Prevent default structure constructor
      integer, private ::  hack
   end type ChildSpec

   interface ChildSpec
      module procedure new_ChildSpec
   end interface ChildSpec

   interface operator(==)
      module procedure equal
   end interface operator(==)
      
   interface operator(/=)
      module procedure not_equal
   end interface operator(/=)


contains

   pure function new_ChildSpec(user_setservices, yaml_config, esmf_config) result(spec)
      type(ChildSpec) :: spec
      class(AbstractUserSetServices), intent(in) :: user_setservices
      character(*), optional, intent(in) :: yaml_config
      character(*), optional, intent(in) :: esmf_config

      spec%user_setservices = user_setservices

      if (present(yaml_config)) spec%yaml_config_file = yaml_config
      if (present(esmf_config)) spec%esmf_config_file = esmf_config

   end function new_ChildSpec
      

   logical function equal(a, b)
      type(ChildSpec), intent(in) :: a
      type(ChildSpec), intent(in) :: b

      equal = (a%user_setservices == b%user_setservices)
      if (.not. equal) return
      
      equal = equal_config(a%yaml_config_file, b%yaml_config_file)
      if (.not. equal) return

      equal = equal_config(a%esmf_config_file, b%esmf_config_file)
      if (.not. equal) return

   contains

      pure logical function equal_config(a, b) result(equal)
         character(:), allocatable, intent(in) :: a
         character(:), allocatable, intent(in) :: b

         equal = (allocated(a) .eqv. allocated(b))
         if (.not. equal) return

         if (allocated(a)) equal = (a == b)

      end function equal_config

   end function equal

   logical function not_equal(a, b)
      type(ChildSpec), intent(in) :: a
      type(ChildSpec), intent(in) :: b

      not_equal = .not. (a == b)
   end function not_equal

   subroutine dump(x)
      type(ChildSpec) :: x

      select type (q => x%user_setservices)
      type is (Dsosetservices)
         print*,__FILE__,__LINE__, q%sharedObj, '::', q%userRoutine
      end select
   end subroutine dump
end module r_mapl3g_ChildSpec

module r_mapl3g_ChildSpecMap
   use r_mapl3g_ChildSpec

   implicit none
   type KeywordEnforcer
   end type KeywordEnforcer

   integer, parameter :: SUCCESS = 0
   integer, parameter :: OUT_OF_RANGE = 1
   integer, parameter :: BAD_ALLOC = 2
   integer, parameter :: ILLEGAL_INPUT = 3
   integer, parameter :: LENGTH_ERROR = 4
   integer, parameter :: TYPE_HAS_NO_DEFAULT_VALUE = 5

   integer, parameter :: GFTL_SIZE_KIND = selected_int_kind(18)
   type :: NO_TYPE_
   end type NO_TYPE_
   type(NO_TYPE_), parameter :: NO_TYPE__ = NO_TYPE_()

   private ! except for
   public :: ChildSpecMap
   public :: ChildSpecMapIterator
   public :: ChildSpecPair

   public :: swap

   public :: advance
   public :: begin
   public :: end
   public :: next
   public :: prev

   public :: operator(==)
   public :: operator(/=)

   public :: find
   public :: find_if
   public :: find_if_not

   type :: ChildSpecPair
      character(len=:), allocatable :: first
      type(ChildSpec) :: second
   contains
   end type ChildSpecPair

   interface ChildSpecPair
      module procedure map_p_new_pair
   end interface ChildSpecPair

   interface swap
      module procedure map_p_swap
   end interface swap

   interface map_Set
      module procedure map_s_new_set_empty
      module procedure map_s_new_set_copy

      module procedure map_s_new_set_initializer_list

   end interface map_Set

   type, abstract :: map_s_BaseNode
   contains
      procedure(I_to_node), deferred :: to_node
      procedure(I_get_parent), deferred :: get_parent
      procedure(I_set_parent), deferred :: set_parent
      procedure(I_has_child), deferred :: has_child
      procedure(I_get_child), deferred :: get_child
      procedure(I_set_child), deferred :: set_child
      procedure(I_deallocate_child), deferred :: deallocate_child
      procedure(I_get_value), deferred :: get_value
      procedure(I_set_value), deferred :: set_value

      procedure(I_which_side_am_i), deferred :: which_side_am_i
      procedure(I_which_child), deferred :: which_child
      procedure(I_get_height), deferred :: get_height
      procedure(I_update_height), deferred :: update_height
   end type map_s_BaseNode

   type, extends(map_s_BaseNode) ::  map_s_Node
      type(map_s_Node), pointer :: parent => null()
      class(map_s_BaseNode), allocatable :: left
      class(map_s_BaseNode), allocatable :: right
      integer :: height=1
      type(ChildSpecPair) :: value
   contains
      procedure :: to_node => map_s_to_node
      procedure :: get_parent =>  map_s_get_parent
      procedure :: set_parent =>  map_s_set_parent
      procedure :: has_child => map_s_has_child
      procedure :: get_child => map_s_get_child
      procedure :: set_child => map_s_set_child
      procedure :: deallocate_child => map_s_deallocate_child
      procedure :: get_value => map_s_get_value
      procedure :: set_value => map_s_set_value

      procedure :: which_child => map_s_which_child
      procedure :: which_side_am_i => map_s_which_side_am_i

      procedure :: get_height => map_s_get_height
      procedure :: update_height => map_s_update_height
   end type map_s_Node

   abstract interface

      function I_to_node(this) result(node)
         import map_s_BaseNode
         import map_s_Node
         type(map_s_Node), pointer :: node
         class(map_s_BaseNode), target, intent(in) :: this
      end function I_to_node

      function I_get_parent(this) result(parent)
         import map_s_BaseNode
         import map_s_Node
         type(map_s_Node), pointer :: parent
         class(map_s_BaseNode), intent(in) :: this
      end function I_get_parent

      subroutine I_set_parent(this, parent)
         import map_s_BaseNode
         import map_s_Node
         class(map_s_BaseNode), intent(inout) :: this
         type(map_s_Node), pointer, intent(in) :: parent
      end subroutine I_set_parent

      logical function I_has_child(this, side) result(has_child)
         import map_s_BaseNode
         class(map_s_BaseNode), intent(in) :: this
         integer, intent(in) :: side
      end function I_has_child

      function I_get_child(this, side) result(child)
         import map_s_BaseNode
         import map_s_Node
         type(map_s_Node), pointer :: child
         class(map_s_BaseNode), target, intent(in) :: this
         integer, intent(in) :: side
      end function I_get_child

      subroutine I_set_child(this, side, node)
         import map_s_BaseNode
         import map_s_Node
         class(map_s_BaseNode), intent(inout) :: this
         integer, intent(in) :: side
         type(map_s_Node), allocatable, intent(inout) :: node
      end subroutine I_set_child

      subroutine I_deallocate_child(this, side)
         import map_s_BaseNode
         class(map_s_BaseNode), intent(inout) :: this
         integer, intent(in) :: side
      end subroutine I_deallocate_child

      function I_get_value(this) result(value)
         import ! have to import all to get ChildSpecPair as we don't know if it is intrinsic
         type(ChildSpecPair), pointer :: value
         class(map_s_BaseNode), target, intent(in) :: this
      end function I_get_value

      subroutine I_set_value(this, value)
         import ! have to import all to get ChildSpecPair as we don't know if it is intrinsic
         class(map_s_BaseNode), intent(inout) :: this
         type(ChildSpecPair), intent(in) :: value
      end subroutine I_set_value

      integer function I_which_side_am_i(this) result(side)
         import map_s_BaseNode
         class(map_s_BaseNode), target, intent(in) :: this
      end function I_which_side_am_i

      integer function I_which_child(this, child) result(side)
         import map_s_BaseNode
         import map_s_Node
         class(map_s_BaseNode), intent(in) :: this
         type(map_s_Node), target, intent(in) :: child
      end function I_which_child

      integer function I_get_height(this) result(height)
         import map_s_BaseNode
         class(map_s_BaseNode), intent(in) :: this
      end function I_get_height

      subroutine I_update_height(this)
         import map_s_BaseNode
         class(map_s_BaseNode), intent(inout) :: this
      end subroutine I_update_height

   end interface

   type :: map_Set
      private
      class(map_s_BaseNode), allocatable :: root
      integer(kind=GFTL_SIZE_KIND) :: tsize = 0
   contains
      procedure :: empty => map_s_empty
      procedure :: size => map_s_size
      procedure, nopass :: max_size => map_s_max_size
      procedure :: count => map_s_count
      procedure :: find => map_s_find
      procedure :: clear => map_s_clear

      procedure :: insert_single => map_s_insert_single
      procedure :: insert_single_with_hint => map_s_insert_single_with_hint
      procedure :: insert_range => map_s_insert_range

      procedure :: insert_initializer_list => map_s_insert_initializer_list

      generic :: insert => insert_single
      generic :: insert => insert_single_with_hint
      generic :: insert => insert_range

      generic :: insert => insert_initializer_list

      procedure :: erase_iter => map_s_erase_iter
      procedure :: erase_value => map_s_erase_value
      procedure :: erase_range => map_s_erase_range
      generic :: erase => erase_iter, erase_value, erase_range
      procedure :: begin => map_s_begin
      procedure :: end => map_s_end
      procedure :: lower_bound => map_s_lower_bound
      procedure :: upper_bound => map_s_upper_bound

      procedure :: merge  => map_s_merge

      procedure :: deep_copy => map_s_deep_copy

      procedure :: copy_list => map_s_copy_list
      generic :: assignment(=) => copy_list

      procedure :: swap => map_s_swap

      procedure, private :: find_node => map_s_find_node
      procedure, private :: rebalance=> map_s_rebalance
      procedure, private :: erase_nonleaf => map_s_erase_nonleaf
      procedure, private :: advpos => map_s_advpos
      procedure, private :: rot => map_s_rot

      procedure :: write_formatted => map_s_write_formatted
      generic :: write(formatted) => write_formatted

      procedure :: key_compare => map_s_value_compare
      procedure :: value_compare => map_s_value_compare

   end type map_Set

   interface swap
      module procedure map_s_swap
   end interface swap

   interface operator(==)
      module procedure map_s_equal
   end interface operator(==)
   interface operator(/=)
      module procedure map_s_not_equal
   end interface operator(/=)
   interface operator(<)
      module procedure map_s_less_than
   end interface operator(<)
   interface operator(<=)
      module procedure map_s_less_than_or_equal
   end interface operator(<=)
   interface operator(>)
      module procedure map_s_greater_than
   end interface operator(>)
   interface operator(>=)
      module procedure map_s_greater_than_or_equal
   end interface operator(>=)

   type :: map_SetIterator
      private
      type(map_Set), pointer :: tree => null()
      type(map_s_Node), pointer :: node => null()
   contains
      procedure :: of => map_s_iter_of
      procedure :: next => map_s_iter_next
      procedure :: prev => map_s_iter_prev
   end type map_SetIterator

   interface operator(==)
      module procedure map_s_iter_equal
   end interface operator(==)

   interface operator(/=)
      module procedure map_s_iter_not_equal
   end interface operator(/=)

   interface advance

      module procedure map_s_iter_advance_size_kind

      module procedure map_s_iter_advance_default
   end interface advance

   interface begin
      module procedure map_s_iter_begin
   end interface begin

   interface end
      module procedure map_s_iter_end
   end interface end

   interface next
      module procedure map_s_iter_next_1

      module procedure map_s_iter_next_n_size_kind

      module procedure map_s_iter_next_n_default
   end interface next

   interface prev
      module procedure map_s_iter_prev_1

      module procedure map_s_iter_prev_n_size_kind

      module procedure map_s_iter_prev_n_default
   end interface prev

  interface find
      module procedure map_s_find_basic
   end interface find

   interface find_if
      module procedure map_s_find_if
   end interface find_if

   interface find_if_not
      module procedure map_s_find_if_not
   end interface find_if_not

   interface ChildSpecMap
      module procedure map_new_map_empty
      module procedure map_new_map_copy
      module procedure map_new_map_initializer_list
   end interface ChildSpecMap

   type :: ChildSpecMap
      private
      type(map_Set) :: tree
   contains
      procedure :: empty => map_empty
      procedure :: size => map_size
      procedure, nopass :: max_size => map_max_size

      procedure :: insert_key_value => map_insert_key_value
      procedure :: insert_pair => map_insert_pair
      generic :: insert => insert_key_value
      generic :: insert => insert_pair

      procedure :: of => map_of ! [] operator
      procedure :: at_rc => map_at_rc
      generic :: at => of
      generic :: at => at_rc  ! [] operator

      procedure :: erase_iter => map_erase_iter
      procedure :: erase_key => map_erase_key
      procedure :: erase_range => map_erase_range
      generic :: erase => erase_iter
      generic :: erase => erase_key
      generic :: erase => erase_range
      procedure :: clear => map_clear
      procedure :: set => map_set_

      procedure :: begin => map_begin
      procedure :: end => map_end
      procedure :: find => map_find

      procedure :: count => map_count
      procedure :: deep_copy => map_deep_copy

   end type ChildSpecMap

   interface operator(==)
      module procedure map_equal
   end interface operator(==)
   interface operator(/=)
      module procedure map_not_equal
   end interface operator(/=)

   type :: ChildSpecMapIterator
      private
      type(map_SetIterator) :: set_iter
      class(ChildSpecMap), pointer :: reference
   contains
      procedure :: of => map_iter_of
      procedure :: first => map_iter_first
      procedure :: second => map_iter_second
      procedure :: next => map_iter_next
      procedure :: prev => map_iter_prev
   end type ChildSpecMapIterator

   interface operator(==)
      module procedure :: map_iter_equal
   end interface operator(==)

   interface operator(/=)
      module procedure :: map_iter_not_equal
   end interface operator(/=)

   interface advance

      module procedure map_iter_advance_size_kind

      module procedure map_iter_advance_default
   end interface advance

   interface begin
      module procedure map_iter_begin
   end interface begin

   interface end
      module procedure map_iter_end
   end interface end

   interface next
      module procedure map_iter_next_1

      module procedure map_iter_next_n_size_kind

      module procedure map_iter_next_n_default
   end interface next

   interface prev
      module procedure map_iter_prev_1

      module procedure map_iter_prev_n_size_kind

      module procedure map_iter_prev_n_default
   end interface prev

  interface find
      module procedure map_find_basic
   end interface find

   interface find_if
      module procedure map_find_if
   end interface find_if

   interface find_if_not
      module procedure map_find_if_not
   end interface find_if_not

   contains

   function map_p_new_pair(first,second) result(p)
      type (ChildSpecPair) :: p
      character(len=*), intent(in) :: first
      type(ChildSpec), intent(in) :: second
      p%first = first
      p%second = second
   end function map_p_new_pair

   subroutine map_p_swap(a, b)
      type(ChildSpecPair), intent(inout) :: a
      type(ChildSpecPair), intent(inout) :: b

      character(len=:), allocatable :: tmp_first
      type(ChildSpec) :: tmp_second

      call move_alloc(from=a%first,to=tmp_first)
      call move_alloc(from=b%first,to=a%first)
      call move_alloc(from=tmp_first,to=b%first)

      tmp_second=a%second
      a%second=b%second
      b%second=tmp_second

   end subroutine map_p_swap

   function map_s_to_node(this) result(node)
      class(map_s_Node), target, intent(in) :: this
      type(map_s_Node), pointer :: node

      select type(this)
      type is (map_s_Node)
         node => this
      end select

   end function map_s_to_node

   function map_s_get_parent(this) result(parent)
      class(map_s_Node), intent(in) :: this
      type(map_s_Node), pointer :: parent

      parent => this%parent

   end function map_s_get_parent

   subroutine map_s_set_parent(this, parent)
      class(map_s_Node), intent(inout) :: this
      type(map_s_Node), pointer, intent(in) :: parent

      this%parent => parent

   end subroutine map_s_set_parent

   logical function map_s_has_child(this, side) result(has_child)
      class(map_s_Node), intent(in) :: this
      integer, intent(in) :: side

      if (side ==0) has_child = allocated(this%left)
      if (side == 1) has_child = allocated(this%right)

   end function map_s_has_child

   function map_s_get_child(this, side) result(child)
      type(map_s_Node), pointer :: child
      class(map_s_Node), target, intent(in) :: this
      integer, intent(in) :: side

      if (side == 0) then
         if (allocated(this%left)) then
            select type (q => this%left)
            type is (map_s_Node)
               child => q
            end select
            return
         end if
      end if

      if (side == 1) then
         if (allocated(this%right)) then
            select type (q => this%right)
            type is (map_s_Node)
               child => q
            end select
            return
         end if
      end if
      child => null()

   end function map_s_get_child

   subroutine map_s_set_child(this, side, node)
      class(map_s_Node), intent(inout) :: this
      integer, intent(in) :: side
      type(map_s_Node), allocatable, intent(inout) :: node

      select case (side)
      case (0)
         call move_alloc(from=node, to=this%left)
      case (1)
         call move_alloc(from=node, to=this%right)
      end select

      return

   end subroutine map_s_set_child

   subroutine map_s_deallocate_child(this, side)
      class(map_s_Node), intent(inout) :: this
      integer, intent(in) :: side

      select case (side)
      case (0)
         deallocate(this%left)
      case (1)
         deallocate(this%right)
      end select

      return

   end subroutine map_s_deallocate_child

   subroutine map_s_set_value(this, value)
      class(map_s_Node), intent(inout) :: this
      type(ChildSpecPair), intent(in) :: value

      this%value=value
      return
   end subroutine map_s_set_value

   function map_s_get_value(this) result(value)
      type(ChildSpecPair), pointer :: value
      class(map_s_Node), target, intent(in) :: this

      value => this%value

   end function map_s_get_value

   integer function map_s_which_side_am_i(this) result(side)
      class(map_s_Node), target, intent(in) :: this

      type(map_s_Node), pointer :: parent

      parent => this%get_parent()
      if (.not. associated(parent)) error stop 'root node is neither left nor right'

      side = parent%which_child(this)

   end function map_s_which_side_am_i

   function map_s_which_child(this, child) result(side)
      integer :: side
      class(map_s_Node), intent(in) :: this
      type(map_s_Node), target, intent(in) :: child

      type(map_s_Node), pointer :: left

      left => this%get_child(0)
      if (associated(left)) then
         if (associated(left, target=child)) then
            side = 0
            return
         else
            side = 1
            return
         end if
      else ! must be at least one child when this procedure is called
         side = 1
      end if
      return

   end function map_s_which_child

   integer function map_s_get_height(this) result(height)
      class(map_s_Node), intent(in) :: this
      height = this%height
   end function map_s_get_height

   subroutine map_s_update_height(this)
      class(map_s_Node), intent(inout) :: this
      integer :: h0, h1

      h0 = 0
      h1 = 0
      if (allocated(this%left)) h0 = this%left%get_height()
      if (allocated(this%right)) h1 = this%right%get_height()
      this%height = max(h0, h1) + 1

      return
   end subroutine map_s_update_height

   function map_s_new_set_empty() result(s)
      type(map_Set) :: s

      s%tsize = 0
   end function map_s_new_set_empty

   function map_s_new_set_copy(x) result(s)
      type(map_Set) :: s
      type(map_Set), intent(in) :: x

      s = x
   end function map_s_new_set_copy

   function map_s_new_set_initializer_list(il) result(s)
      type (map_Set) :: s
      type(ChildSpecPair), dimension(:), intent(in) :: il ! initializer list

      integer :: i

      do i = 1, size(il)
         call s%insert(il(i))
      end do

      return
   end function map_s_new_set_initializer_list

   logical function map_s_empty(this) result(empty)
      class(map_Set), intent(in) :: this

      empty = .not. allocated(this%root)

   end function map_s_empty

   function map_s_size(this) result(size)
      integer(kind=GFTL_SIZE_KIND) :: size
      class(map_Set), intent(in) :: this

      size = this%tsize

   end function map_s_size

   pure function map_s_max_size() result(res)
      integer(kind=GFTL_SIZE_KIND) :: res

      integer(kind=GFTL_SIZE_KIND) :: index

      res = huge(index)

      return
   end function map_s_max_size

   function map_s_find(this, value) result(find)
      type(map_SetIterator) :: find
      class(map_Set), target, intent(in) :: this
      type(ChildSpecPair), intent(in) :: value

      find%tree => this
      find%node => this%find_node(value, .false.)

      if (associated(find%node)) then
         if (.not. map_s_order_eq(find%node%get_value(),value)) then
            find%node => null()
         end if
      end if

      return
   end function map_s_find

   logical function map_s_order_eq(x, y) result(equal)
      type(ChildSpecPair), intent(in) :: x
      type(ChildSpecPair), intent(in) :: y

      equal = .not. map_s_lessThan(x,y) .and. .not. map_s_lessThan(y,x)
   end function map_s_order_eq

   function map_s_count(this, value) result(count)
      integer(kind=GFTL_SIZE_KIND) :: count
      class(map_Set), target, intent(in) :: this
      type(ChildSpecPair), intent(in) :: value

      type (map_SetIterator) :: i

      i = this%find(value)

      if (associated(i%node)) then
         count = 1
      else 
         count = 0
      end if

   end function map_s_count

   recursive subroutine map_s_clear(this)
      class(map_Set), intent(inout) :: this

      if (allocated(this%root)) deallocate(this%root)
      this%tsize = 0
      return
   end subroutine map_s_clear

   subroutine map_s_insert_single(this, value, unused, is_new, iter)
      class(map_Set), target, intent(inout) :: this
      type(ChildSpecPair), intent(in) :: value
      type (KeywordEnforcer), optional :: unused
      logical, optional, intent(out) :: is_new
      type(map_SetIterator), optional, intent(out) :: iter
      type(map_s_Node), target, allocatable :: new
      type(map_s_Node), pointer :: parent

      class(map_s_Node), pointer :: r

      if (present(iter)) iter%tree => this

      if (allocated(this%root)) then

         parent => this%find_node(value, .false.)
         if (map_s_order_eq(parent%get_value(), value)) then
            if (present(iter)) then
               iter%node => parent
            else

               call parent%set_value(value)
            endif
            if (present(is_new)) then
               is_new = .false.
            end if
            return
         endif

         if (present(is_new)) then
            is_new = .true.
         end if

         allocate(new)
         if (present(iter)) iter%node => new
         call new%set_parent(parent)
         new%value=value
         call parent%set_child(merge(0, 1, map_key_less_than(value,parent%get_value())),new)
         call this%rebalance(parent, .true.)
      else
         allocate(map_s_Node :: this%root)
         if (present(iter)) iter%node => this%root%to_node()
         select type (q => this%root)
         type is (map_s_Node)
            r => q
         end select
         call r%set_value(value)
         if (present(is_new)) then
            is_new = .true.
         end if
      endif
      this%tsize = this%tsize + 1
      return
      if (present(unused)) print*,shape(unused)

   end subroutine map_s_insert_single

   subroutine map_s_insert_initializer_list(this, values)
      class(map_Set), intent(inout) :: this
      type(ChildSpecPair), intent(in) :: values(:)
      integer :: i

      do i = 1, size(values)
         call this%insert(values(i))
      end do

   end subroutine map_s_insert_initializer_list

   subroutine map_s_insert_range(this, first, last)
      class(map_Set), intent(inout) :: this
      type(map_SetIterator), intent(in) :: first
      type(map_SetIterator), intent(in) :: last

      type(map_SetIterator) :: iter

      iter = first
      do while (iter /= last)
         call this%insert(iter%of())
         call iter%next()
      end do

   end subroutine map_s_insert_range

   subroutine map_s_insert_single_with_hint(this, hint, value, unused, iter)
      class(map_Set), intent(inout) :: this
      type(map_SetIterator), intent(in) :: hint
      type(ChildSpecPair), intent(in) :: value
      type (KeywordEnforcer), optional :: unused
      type(map_SetIterator), optional, intent(out) :: iter

      call this%insert(value, iter=iter)

   end subroutine map_s_insert_single_with_hint

   logical function map_s_lessThan(x, y) result(less)
      type(ChildSpecPair), intent(in) :: x
      type(ChildSpecPair), intent(in) :: y

      less = map_key_less_than(x,y)

   contains

      logical function dictionaryLessThan1d(x, y) result(less)
         integer, intent(in) :: x(:)
         integer, intent(in) :: y(:)

         integer(kind=GFTL_SIZE_KIND) :: i, n

         n = min(size(x),size(y))

         do i = 1, n
            less = (x(i) < y(i))
            if (.not. x(i) == y(i)) return
         end do

         less = (size(x) < size(y))

      end function dictionaryLessThan1d

   end function map_s_lessThan

   function map_s_erase_iter(this, position) result(iter)
      type(map_SetIterator) :: iter
      class(map_Set), target, intent(inout) :: this
      type(map_SetIterator), intent(in) :: position

      type (map_SetIterator) :: last

      last = position
      call last%next()
      iter = this%erase(position, last)

   end function map_s_erase_iter

   function map_s_erase_value(this, value) result(n)
      integer(kind=GFTL_SIZE_KIND) :: n
      class(map_Set), target, intent(inout) :: this
      type(ChildSpecPair), intent(in) :: value

      type(map_SetIterator) :: iter

      iter = this%find(value)
      if (iter /= this%end()) then
         iter = this%erase(iter)

         n = 1

      else
         n = 0
      end if
   end function map_s_erase_value

   function map_s_erase_range(this, first, last) result(next_iter)
      type(map_SetIterator) :: next_iter
      class(map_Set), intent(inout) :: this
      type(map_SetIterator), intent(in) :: first
      type(map_SetIterator), intent(in) :: last
      type(map_s_Node), pointer :: parent
      type(map_s_Node), pointer :: pos

      type (map_SetIterator) :: iter

      next_iter = last

      iter = first
      do while (iter /= last)
         pos => iter%node
         call iter%next()
         if (pos%has_child(1)) then
            call this%erase_nonleaf(pos, 1)
         else if (pos%has_child(0)) then
            call this%erase_nonleaf(pos, 0)
         else
            parent => pos%get_parent()
            if (associated(parent)) then
               call parent%deallocate_child(parent%which_child(pos))
               call this%rebalance(parent, .false.)
            else
               deallocate(this%root)
            endif
         endif
         this%tsize=this%tsize-1
      end do

      return
   end function map_s_erase_range

   function map_s_begin(this) result(begin)
      class(map_Set), target, intent(in) :: this
      type(map_SetIterator) :: begin

      begin%tree => this
      call begin%next()
      return
   end function map_s_begin

   function map_s_end(this) result(end_)
      class(map_Set), target, intent(in) :: this
      type(map_SetIterator) :: end_

      end_%tree => this

      return
   end function map_s_end

   function map_s_lower_bound(this, value) result(lb)
      type(map_SetIterator) :: lb
      class(map_Set), target, intent(in) :: this
      type(ChildSpecPair), intent(in) :: value

      type(map_s_Node), pointer :: node

      lb%tree => this
      node => this%find_node(value, .false.)
      lb%node => node

      if (map_key_less_than(node%value,value)) then
         if (lb /= this%end()) call lb%next()
      end if

      return
   end function map_s_lower_bound

   function map_s_upper_bound(this, value) result(ub)
      type(map_SetIterator) :: ub
      class(map_Set), target, intent(in) :: this
      type(ChildSpecPair), intent(in) :: value

      type(map_s_Node), pointer :: node

      ub%tree => this
      node => this%find_node(value, .false.)
      ub%node => node

      if (.not. (map_key_less_than(value,node%value))) then
         if (ub /= this%end()) call ub%next()
      end if

      return
   end function map_s_upper_bound

   function map_s_find_node(this, value, last) result(find_node)
      type(map_s_Node), pointer :: find_node
      class(map_Set), target, intent(in) :: this
      type(ChildSpecPair), intent(in) :: value
      logical, intent(in) :: last
      integer :: side

      if (.not. allocated(this%root)) then
         find_node => null()
         return
      end if

      find_node => this%root%to_node()
      if (associated(find_node)) then
         do
            if (.not. last .and. (                                           &
                 &     (map_s_order_eq(find_node%get_value(),value)))) return
            side=merge(0, 1, map_s_lessThan(value, find_node%get_value()))
            if (.not.associated(find_node%get_child(side))) return
            find_node => find_node%get_child(side)
         end do
      end if

      return
   end function map_s_find_node

   subroutine map_s_rebalance(this, pos, once)
      class(map_Set), intent(inout) :: this
      type(map_s_Node), pointer, intent(in) :: pos
      logical, intent(in) :: once
      type(map_s_Node), pointer :: curr, child
      integer :: hl, hr, chl, chr, side, child_side
      logical :: unbalanced

      curr => pos
      do while (associated(curr))
         hl=0
         hr=0
         if (curr%has_child(0)) hl=curr%left%get_height()
         if (curr%has_child(1)) hr=curr%right%get_height()
         unbalanced=abs(hl-hr)>1
         if (unbalanced) then
            side = merge(0, 1, hl > hr)
            child => curr%get_child(side)
            chl = 0
            chr = 0
            if (child%has_child(0)) chl = child%left%get_height()
            if (child%has_child(1)) chr = child%right%get_height()
            if (chr /= chl) then
               child_side=merge(0, 1, chl > chr)
               if (side /= child_side) call this%rot(child, 1-child_side)
               call this%rot(curr, 1-side)
            endif
         endif
         call curr%update_height()
         if (unbalanced.and.once) return
         curr => curr%parent
      end do
      return
   end subroutine map_s_rebalance

subroutine map_s_erase_nonleaf(this, pos, side)
      class(map_Set), intent(inout) :: this
      type(map_s_Node), pointer, intent(inout) :: pos
      integer, intent(in) :: side
      type(map_s_Node), pointer :: parent, other, child0, child1
      type(map_s_Node), pointer :: otherchild, otherparent
      class(map_s_BaseNode), allocatable :: tmp_other, tmp_pos

      parent => pos%parent
      other => pos
      call this%advpos(other, side)
      child0 => pos%get_child(side)
      child1 => pos%get_child(1-side)
      otherchild => other%get_child(side)
      otherparent => other%parent

      select case (other%which_side_am_i())
      case (0)
         call move_alloc(from=otherparent%left, to=tmp_other)
      case (1)
         call move_alloc(from=otherparent%right, to=tmp_other)
      end select

      call tmp_other%set_parent(parent) 
      if (associated(parent)) then
         select case (pos%which_side_am_i())
         case (0)
            call move_alloc(from=parent%left, to=tmp_pos)
            call move_alloc(from=tmp_other, to=parent%left)
         case (1)
            call move_alloc(from=parent%right, to=tmp_pos)
            call move_alloc(from=tmp_other, to=parent%right)
         end select
      else
         call move_alloc(from=this%root, to=tmp_pos)
         call move_alloc(from=tmp_other, to=this%root)
      endif

      if (associated(child1)) then
         select type (q => tmp_pos)
         type is (map_s_Node)
            select case(side)
            case (0)
               call move_alloc(from=q%right, to=other%right)
               call other%right%set_parent(other)
            case (1)
               call move_alloc(from=q%left, to=other%left)
               call other%left%set_parent(other)
            end select
         end select
      end if

      if (associated(other, target=child0)) then ! degenerate
         call this%rebalance(other, .false.)
      else
         select type (q => tmp_pos)
         type is (map_s_Node)
            select case (side)
            case (0)
               if (associated(otherchild)) call move_alloc(from=other%left, to=otherparent%right)
               call move_alloc(from=q%left, to=other%left)
               call other%left%set_parent(other)
            case (1)
               if (associated(otherchild)) call move_alloc(from=other%right, to=otherparent%left)
               call move_alloc(from=q%right, to=other%right)
               call other%right%set_parent(other)
            end select
         end select
         if (associated(otherchild)) then
            call otherchild%set_parent(otherparent)
         end if
         call this%rebalance(otherparent, .false.)
      end if

      deallocate(tmp_pos)
      return
   end subroutine map_s_erase_nonleaf

   subroutine map_s_advpos(this, pos, dir)
      class(map_Set), target, intent(in) :: this
      type(map_s_Node), pointer, intent(inout) :: pos
      integer, intent(in) :: dir   ! dir=1 forward, dir=0 backward
      type(map_s_Node), pointer :: prev

      if (.not.associated(pos)) then
         if (.not. allocated(this%root)) return
         pos => this%root%to_node()
         do while (associated(pos%get_child(1-dir)))
            pos => pos%get_child(1-dir)
         end do
      else if (associated(pos%get_child(dir))) then
         pos => pos%get_child(dir)
         do while (associated(pos%get_child(1-dir)))
            pos => pos%get_child(1-dir)
         end do
      else
         prev => pos
         pos => pos%parent
         do while (associated(pos))
            if (.not.associated(pos%get_child(dir), prev)) exit
            prev => pos
            pos => pos%parent
         end do
      endif
      return
   end subroutine map_s_advpos

   subroutine map_s_rot(this, pos, dir)
      class(map_Set), intent(inout) :: this
      type(map_s_Node), pointer, intent(inout) :: pos
      integer, intent(in) :: dir
      type(map_s_Node), pointer :: parent, child, grandchild => null()

      class(map_s_BaseNode), allocatable :: A, B, C
      integer :: pos_side

      parent => pos%parent

      if (associated(parent)) then
         pos_side = pos%which_side_am_i()
         select case (pos_side)
         case (0)
            call move_alloc(from=parent%left, to=A)
         case (1)
            call move_alloc(from=parent%right, to=A)
         end select
      else
         call move_alloc(from=this%root, to=A)
      endif

      child => pos%get_child(1-dir)
      if (associated(child)) then
         select case (1-dir)
         case (0)
            call move_alloc(from=pos%left, to=B)
         case (1)
            call move_alloc(from=pos%right, to=B)
         end select
      else
         error stop "isn't there always a child for rot?"
      end if

      grandchild => child%get_child(dir)
      if (associated(grandchild)) then
         select case (dir)
         case (0)
            call move_alloc(from=child%left, to=C)
         case (1)
            call move_alloc(from=child%right, to=C)
         end select
      end if

      if (associated(grandchild)) then
         select type (A)
         type is (map_s_Node)
            select case (1-dir)
            case (0)
               call move_alloc(from=C, to=A%left)
            case (1)
               call move_alloc(from=C, to=A%right)
            end select
         end select
         call grandchild%set_parent(pos)
      end if

      if (associated(child)) then
         select type (B)
         type is (map_s_Node)
            select case (dir)
            case (0)
               call move_alloc(from=A, to=B%left)
            case (1)
               call move_alloc(from=A, to=B%right)
            end select
         end select
         call pos%set_parent(child)
      end if

      if (associated(parent)) then
         select case (pos_side)
         case (0)
            call move_alloc(from=B, to=parent%left)
         case (1)
            call move_alloc(from=B, to=parent%right)
         end select
      else
         call move_alloc(from=B, to=this%root)
      endif
      call child%set_parent(parent)

      call pos%update_height()
      if (associated(child)) call child%update_height()
      return
   contains

      subroutine cheat(a,b)
         type(map_s_Node), allocatable :: a, b
         call move_alloc(from=a, to=b)
      end subroutine cheat
   end subroutine map_s_rot

   logical function map_s_value_compare(this, x, y) result(value_compare)
      class(map_Set), intent(in) :: this
      type(ChildSpecPair), intent(in) :: x
      type(ChildSpecPair), intent(in) :: y

      if (.false.) print*,shape(this)
      value_compare = map_key_less_than(x,y)

      return
   end function map_s_value_compare

   logical function map_s_equal(a, b) result(equal)
      type(map_Set), target, intent(in) :: a
      type(map_Set), target, intent(in) :: b

      type (map_SetIterator) :: iter_a
      type (map_SetIterator) :: iter_b
      type(ChildSpecPair), pointer :: ptr_a
      type(ChildSpecPair), pointer :: ptr_b

      equal = .false. ! unless
      if (a%size() /= b%size()) return

      iter_a = a%begin()
      iter_b = b%begin()
      do while (iter_a /= a%end())
         ptr_a => iter_a%of()
         ptr_b => iter_b%of()

         if (.not. map_s_order_eq(ptr_a,ptr_b)) return

         call iter_a%next()
         call iter_b%next()
      end do

      equal = .true.

   end function map_s_equal

   logical function map_s_not_equal(a, b) result(not_equal)
      type(map_Set), intent(in) :: a
      type(map_Set), intent(in) :: b

      not_equal = .not. (a == b)

   end function map_s_not_equal

   logical function map_s_less_than(a,b) result(lt)
      type(map_Set), intent(in) :: a
      type(map_Set), intent(in) :: b

      type (map_SetIterator) :: iter_a
      type (map_SetIterator) :: iter_b
      type(ChildSpecPair), pointer :: ptr_a
      type(ChildSpecPair), pointer :: ptr_b

      iter_a = a%begin()
      iter_b = b%begin()
      do while (iter_a /= a%end() .and. iter_b /= b%end())
         ptr_a => iter_a%of()
         ptr_b => iter_b%of()

         lt = map_key_less_than(ptr_a,ptr_b)
         if (lt) return

         lt = map_key_less_than(ptr_b,ptr_a)
         if (lt) return

         call iter_a%next()
         call iter_b%next()
      end do

      lt = (a%size() < b%size())

      return
   end function map_s_less_than

   logical function map_s_less_than_or_equal(a,b) result(le)
      type(map_Set), intent(in) :: a
      type(map_Set), intent(in) :: b

      le = .not. (b < a)
      return
   end function map_s_less_than_or_equal

   logical function map_s_greater_than(a,b) result(gt)
      type(map_Set), intent(in) :: a
      type(map_Set), intent(in) :: b

      gt = (b < a)
      return
   end function map_s_greater_than

   logical function map_s_greater_than_or_equal(a,b) result(ge)
      type(map_Set), intent(in) :: a
      type(map_Set), intent(in) :: b

      ge = .not. (a < b)
      return
   end function map_s_greater_than_or_equal

   recursive subroutine map_s_deep_copy(this, other)
      class(map_Set), target, intent(out) :: this
      class(map_Set), target, intent(in) :: other

      type(map_SetIterator) :: iter
      type(ChildSpecPair), pointer :: ptr

      iter = other%begin()
      do while (iter /= other%end())
         ptr => iter%of()
         call this%insert(ptr)
         call iter%next()
      end do

      this%tsize = other%tsize

   end subroutine map_s_deep_copy

   subroutine map_s_copy_list(this, il)
      class(map_Set), intent(out) :: this
      type(ChildSpecPair), intent(in) :: il(:)

      call this%insert(il)

   end subroutine map_s_copy_list

   subroutine map_s_merge(this, source)
      class(map_Set), intent(inout) :: this
      type(map_Set), target, intent(inout) :: source

      type(map_SetIterator) :: iter

      iter = source%begin()
      do while (iter /= source%end())
         if (this%count(iter%of()) == 0) then

            call this%insert(iter%of())
            iter = source%erase(iter)
         else
            call iter%next()
         end if
      end do
   end subroutine map_s_merge

   subroutine map_s_write_formatted(this, unit, iotype, v_list, iostat, iomsg)
    class(map_Set), intent(in) :: this
    integer, intent(in) :: unit
    character(*), intent(in) :: iotype
    integer, intent(in) :: v_list(:)
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    iostat = 0

    write(unit,'(a)') 'Set<' // 'unknown' // '>'

    write(unit,'(a)') new_line('a')
    write(unit,'(4x,a10,1x,i0)') 'size: ',this%size()
   end subroutine map_s_write_formatted

   subroutine map_s_swap(this, x)
      class(map_Set), target, intent(inout) :: this
      type(map_Set), target, intent(inout) :: x

      class(map_s_BaseNode), allocatable :: tmp
      integer(kind=GFTL_SIZE_KIND) :: tsize

      call move_alloc(from=this%root, to=tmp)
      call move_alloc(from=x%root, to=this%root)
      call move_alloc(from=tmp, to=x%root)

      tsize = this%tsize
      this%tsize = x%tsize
      x%tsize = tsize

      return
   end subroutine map_s_swap

   function map_s_iter_of(this) result(value)
      class(map_SetIterator), intent(in) :: this
      type(ChildSpecPair), pointer :: value

      if (associated(this%node)) then
         value  => this%node%get_value()
      else
         value => null()
      end if

   end function map_s_iter_of

   subroutine map_s_iter_next(this)
      class(map_SetIterator), intent(inout) :: this

      call this%tree%advpos(this%node, 1)

   end subroutine map_s_iter_next

   subroutine map_s_iter_prev(this)
      class(map_SetIterator), intent(inout) :: this

      call this%tree%advpos(this%node, 0)

   end subroutine map_s_iter_prev

   logical function map_s_iter_equal(a, b) result(eq)
      type(map_SetIterator), intent(in) :: a
      type(map_SetIterator), intent(in) :: b

      eq = &
           &    associated(a%tree, target=b%tree) .and.                         &
           &   ((.not.associated(a%node) .and. .not.associated(b%node))         &
           &   .or.associated(a%node, target=b%node))

   end function map_s_iter_equal

   logical function map_s_iter_not_equal(a, b) result(ne)
      implicit none
      class(map_SetIterator), intent(in) :: a, b

      ne = .not. (a == b)

   end function map_s_iter_not_equal

   subroutine map_s_iter_advance_size_kind(it, n)
      type(map_SetIterator), intent(inout) :: it
      integer(kind=selected_int_kind(18)), intent(in) :: n

      integer :: i

      do i = 1, n
         call it%next()
      end do

      return
   end subroutine map_s_iter_advance_size_kind

   subroutine map_s_iter_advance_default(it, n)
      type(map_SetIterator), intent(inout) :: it
      integer, intent(in) :: n

      integer :: i

      do i = 1, n
         call it%next()
      end do

      return
   end subroutine map_s_iter_advance_default

   function map_s_iter_begin(cont) result(begin)
      type(map_SetIterator) :: begin
      type(map_Set), target, intent(in) :: cont

      begin = cont%begin()

      return
   end function map_s_iter_begin

   function map_s_iter_end(cont) result(end)
      type(map_SetIterator) :: end
      type(map_Set), target, intent(in) :: cont

      end = cont%end()

   end function map_s_iter_end

   function map_s_iter_next_1(it) result(new_it)
      type(map_SetIterator) :: new_it
      type(map_SetIterator), intent(in) :: it

      new_it = next(it,1)

      return
   end function map_s_iter_next_1

   function map_s_iter_next_n_size_kind(it, n) result(new_it)
      type(map_SetIterator) :: new_it
      type(map_SetIterator), intent(in) :: it
      integer(kind=selected_int_kind(18)), intent(in) :: n

      integer :: i

      new_it = it
      do i = 1, n
         call new_it%next()
      end do

      return
   end function map_s_iter_next_n_size_kind

   function map_s_iter_next_n_default(it, n) result(new_it)
      type(map_SetIterator) :: new_it
      type(map_SetIterator), intent(in) :: it
      integer, intent(in) :: n

      integer :: i

      new_it = it
      do i = 1, n
         call new_it%next()
      end do

      return
   end function map_s_iter_next_n_default

   function map_s_iter_prev_1(it) result(new_it)
      type(map_SetIterator) :: new_it
      type(map_SetIterator), intent(in) :: it

      new_it = prev(it,1)

      return
   end function map_s_iter_prev_1

   function map_s_iter_prev_n_size_kind(it, n) result(new_it)
      type(map_SetIterator) :: new_it
      type(map_SetIterator), intent(in) :: it
      integer(kind=selected_int_kind(18)), intent(in) :: n

      integer :: i

      new_it = it
      do i = 1, n
         call new_it%prev()
      end do

      return
   end function map_s_iter_prev_n_size_kind

   function map_s_iter_prev_n_default(it, n) result(new_it)
      type(map_SetIterator) :: new_it
      type(map_SetIterator), intent(in) :: it
      integer, intent(in) :: n

      integer :: i

      new_it = it
      do i = 1, n
         call new_it%prev()
      end do

      return
   end function map_s_iter_prev_n_default

       function map_s_find_basic(do_not_use,unused) result(j)
          type :: map_s_keywordenforcer
             integer :: placeholder
         end type map_s_Keywordenforcer
         type(map_s_keywordenforcer) :: j
         type(map_SetIterator), intent(in) :: do_not_use
         type(keywordenforcer), intent(in) :: unused

         j%placeholder = -1
       end function map_s_find_basic

      function map_s_find_if(first, last, p) result(it)
         type(map_SetIterator) :: it
         type(map_SetIterator), intent(in) :: first
         type(map_SetIterator), intent(in) :: last
         interface
            logical function p(item)
               import
               implicit none
               type(ChildSpecPair), intent(in) :: item
            end function p
         end interface

         it = first
         do while (it /= last)
            if (p(it%of())) return

            call it%next()
         end do

         it = last
      end function map_s_find_if

      function map_s_find_if_not(first, last, q) result(it)
         type(map_SetIterator) :: it
         type(map_SetIterator), intent(in) :: first
         type(map_SetIterator), intent(in) :: last
         interface
            logical function q(item)
               import
               implicit none
               type(ChildSpecPair), intent(in) :: item
            end function q
         end interface

         it = first
         do while (it /= last)
            if (.not. q(it%of())) return
            call it%next()
         end do

         it = last
      end function map_s_find_if_not

      function map_new_map_empty() result(m)
         type (ChildSpecMap) :: m

         m%tree = map_Set()
      end function map_new_map_empty

      function map_new_map_copy(x) result(m)
         type (ChildSpecMap) :: m
         type (ChildSpecMap), intent(in) :: x

         m%tree = x%tree
      end function map_new_map_copy

      function map_new_map_initializer_list(il) result(m)
         type (ChildSpecMap) :: m
         type (ChildSpecPair), intent(in) :: il(:)

         integer :: i

         m = ChildSpecMap()
         do i = 1, size(il)
            call m%insert(il(i))
         end do

      end function map_new_map_initializer_list

      logical function map_empty(this) result(isEmpty)
         class (ChildSpecMap), intent(in) :: this

         isEmpty = this%tree%empty()

      end function map_empty

      function map_size(this) result(size)
         integer(kind=GFTL_SIZE_KIND) :: size
         class (ChildSpecMap), intent(in) :: this

         size = this%tree%size()

      end function map_size

      function map_max_size() result(max_size)
         integer(kind=GFTL_SIZE_KIND) :: max_size

         max_size = huge(1_GFTL_SIZE_KIND)

      end function map_max_size

      subroutine map_insert_key_value(this, key, value)
         class (ChildSpecMap), intent(inout) :: this
         character(len=*), intent(in) :: key
         type(ChildSpec), intent(in) :: value

         type (ChildSpecPair) :: p

         p%first=key
         p%second=value

         call this%tree%insert(p)

      end subroutine map_insert_key_value

      subroutine map_insert_pair(this, p)
         class (ChildSpecMap), intent(inout) :: this
         type (ChildSpecPair), intent(in) :: p

         call this%tree%insert(p)

      end subroutine map_insert_pair

      subroutine map_set_(this, key, value)
      class(ChildSpecMap), intent(inout) :: this
      character(len=*), intent(in) :: key
      type(ChildSpec), intent(in) :: value
      type(ChildSpecPair) :: p

      p%first=key
      p%second=value

      call this%tree%insert(p)
      return

      end subroutine map_set_

      function map_of(this, key) result(res)
      class(ChildSpecMap), target, intent(inout) :: this
      character(len=*), intent(in) :: key
      type(ChildSpec), pointer :: res
      type(ChildSpecPair) :: p

      logical :: is_new
      type(map_SetIterator) :: iter
      type(ChildSpecPair), pointer :: pair_ptr

      p%first=key

      call this%tree%insert(p, iter=iter, is_new=is_new)
      if (.not. is_new) then
         pair_ptr => iter%of()
         res => pair_ptr%second
      else
         res => null()
      end if

      return
      end function map_of

      function map_at_rc(this, key, rc) result(res)
      type(ChildSpec), pointer :: res
      class(ChildSpecMap), target, intent(in) :: this
      character(len=*), intent(in) :: key
      integer, intent(out) :: rc

      type (ChildSpecMapIterator) :: iter

      iter = this%find(key)
      if (iter == this%end()) then
         res => null()
         rc = OUT_OF_RANGE
      else
         res => iter%second()
         rc = SUCCESS
      end if

      return
      end function map_at_rc

      function map_erase_iter(this, iter) result(new_iter)
         type(ChildSpecMapIterator) :: new_iter
         class(ChildSpecMap), intent(inout) :: this
         type(ChildSpecMapIterator), intent(in) :: iter

         new_iter%reference => iter%reference
         new_iter%set_iter = this%tree%erase(iter%set_iter)

      end function map_erase_iter

      function map_erase_key(this, k) result(n)
         integer(kind=GFTL_SIZE_KIND) :: n
         class(ChildSpecMap), intent(inout) :: this
         character(len=*), intent(in) :: k

         type(ChildSpecMapIterator) :: iter

         iter = this%find(k)
         if (iter /= this%end()) then
            iter = this%erase(iter)
            n = 1
         else
            n = 0
         end if

      end function map_erase_key

      function map_erase_range(this, first, last) result(new_iter)
         type(ChildSpecMapIterator) :: new_iter
         class(ChildSpecMap), target, intent(inout) :: this
         type(ChildSpecMapIterator), intent(in) :: first
         type(ChildSpecMapIterator), intent(in) :: last

         new_iter%reference => first%reference
         new_iter%set_iter = this%tree%erase(first%set_iter, last%set_iter)

      end function map_erase_range

      recursive subroutine map_clear(this)
      class(ChildSpecMap), intent(inout) :: this

      call this%tree%clear()

      end subroutine map_clear

   logical function map_equal(a, b) result(equal)
      type(ChildSpecMap), intent(in) :: a
      type(ChildSpecMap), intent(in) :: b

      equal = a%tree == b%tree

   end function map_equal

   logical function map_not_equal(a, b) result(not_equal)
      type(ChildSpecMap), intent(in) :: a
      type(ChildSpecMap), intent(in) :: b

      not_equal = .not. (a == b)

   end function map_not_equal

      function map_begin(this) result(iter)
         class(ChildSpecMap), target, intent(in) :: this
         type (ChildSpecMapIterator) :: iter

         iter%reference => this
         iter%set_iter = this%tree%begin()

      end function map_begin

      function map_end(this) result(iter)
         class(ChildSpecMap), target, intent(in) :: this
         type (ChildSpecMapIterator) :: iter

         iter%reference => this
         iter%set_iter = this%tree%end()

      end function map_end

      function map_find(this, key) result(iter)
         type (ChildSpecMapIterator) :: iter
         class(ChildSpecMap), target, intent(in) :: this
         character(len=*), intent(in) :: key

         type (ChildSpecPair) :: p

         p%first=key

         iter%reference => this
         iter%set_iter = this%tree%find(p)

      end function map_find

      function map_count(this, key) result(count)
         integer(kind=GFTL_SIZE_KIND) :: count
         class(ChildSpecMap), intent(in) :: this
         character(len=*), intent(in) :: key

         type (ChildSpecPair) :: p

         p%first=key

         count = this%tree%count(p)

      end function map_count

      recursive subroutine map_deep_copy(this, x)
         class(ChildSpecMap), intent(out) :: this
         type(ChildSpecMap), intent(in) :: x

         this%tree = x%tree

      end subroutine map_deep_copy

      logical function map_key_less_than(a,b) result(less_than)
         type(ChildSpecPair), intent(in) :: a
         type(ChildSpecPair), intent(in) :: b

         less_than = a%first < b%first

         return
      end function map_key_less_than

     function map_iter_of(this) result(p)
        type(ChildSpecPair), pointer :: p
         class(ChildSpecMapIterator), target, intent(in) :: this

         p => this%set_iter%of()

      end function map_iter_of

      function map_iter_first(this) result(first)
         character(len=:), pointer :: first
         class(ChildSpecMapIterator), target, intent(in) :: this

         type(ChildSpecPair), pointer :: p

         p => this%of()
         if (associated(p)) then
            first => p%first
         else
            first => null()
         end if

      end function map_iter_first

      function map_iter_second(this) result(second)
         type(ChildSpec), pointer :: second
         class(ChildSpecMapIterator), target, intent(in) :: this

         type(ChildSpecPair), pointer :: p

         p => this%of()
         if (associated(p)) then
            second => p%second
         else
            second => null()
         end if

      end function map_iter_second

      logical function map_iter_equal(a, b) result(equal)
         type(ChildSpecMapIterator), intent(in) :: a
         type(ChildSpecMapIterator), intent(in) :: b

         equal = (a%set_iter == b%set_iter)

      end function map_iter_equal

      logical function map_iter_not_equal(a, b) result(not_equal)
         type(ChildSpecMapIterator), intent(in) :: a
         type(ChildSpecMapIterator), intent(in) :: b

         not_equal = .not. (a == b)
      end function map_iter_not_equal

      subroutine map_iter_next(this)
         class(ChildSpecMapIterator), intent(inout) :: this

         call this%set_iter%next()
      end subroutine map_iter_next

      subroutine map_iter_prev(this)
         class(ChildSpecMapIterator), intent(inout) :: this

         call this%set_iter%prev()
      end subroutine map_iter_prev

      subroutine map_iter_advance_size_kind(it, n)
         type(ChildSpecMapIterator), intent(inout) :: it
         integer(kind=selected_int_kind(18)), intent(in) :: n

         integer :: i

         do i = 1, n
            call it%next()
         end do
         return
      end subroutine map_iter_advance_size_kind

      subroutine map_iter_advance_default(it, n)
         type(ChildSpecMapIterator), intent(inout) :: it
         integer, intent(in) :: n
         integer :: i

         do i = 1, n
            call it%next()
         end do

         return
      end subroutine map_iter_advance_default

      function map_iter_begin(cont) result(begin)
         type(ChildSpecMapIterator) :: begin
         type(ChildSpecMap), target, intent(in) :: cont

         begin = cont%begin()

         return
      end function map_iter_begin

      function map_iter_end(cont) result(end)
         type(ChildSpecMapIterator) :: end
         type(ChildSpecMap), target, intent(in) :: cont

         end = cont%end()

         return
      end function map_iter_end

      function map_iter_next_1(it) result(new_it)
         type(ChildSpecMapIterator) :: new_it
         type(ChildSpecMapIterator), intent(in) :: it

         new_it = next(it,1)

         return
      end function map_iter_next_1

      function map_iter_next_n_size_kind(it, n) result(new_it)
         type(ChildSpecMapIterator) :: new_it
         type(ChildSpecMapIterator), intent(in) :: it
         integer(kind=selected_int_kind(18)), intent(in) :: n
         integer :: i

         new_it = it

         do i = 1, n
            call new_it%next()
         end do

         return
      end function map_iter_next_n_size_kind

      function map_iter_next_n_default(it, n) result(new_it)
         type(ChildSpecMapIterator) :: new_it
         type(ChildSpecMapIterator), intent(in) :: it
         integer, intent(in) :: n
         integer :: i

         new_it = it

         do i = 1, n
            call new_it%next()
         end do

         return
      end function map_iter_next_n_default

     function map_iter_prev_1(it) result(new_it)
         type(ChildSpecMapIterator) :: new_it
         type(ChildSpecMapIterator), intent(in) :: it

         new_it = prev(it,1)

         return
      end function map_iter_prev_1

      function map_iter_prev_n_size_kind(it, n) result(new_it)
         type(ChildSpecMapIterator) :: new_it
         type(ChildSpecMapIterator), intent(in) :: it
         integer(kind=selected_int_kind(18)), intent(in) :: n
         integer :: i

         new_it = it

         do i = 1, n
            call new_it%prev()
         enddo

         return
      end function map_iter_prev_n_size_kind

      function map_iter_prev_n_default(it, n) result(new_it)
         type(ChildSpecMapIterator) :: new_it
         type(ChildSpecMapIterator), intent(in) :: it
         integer, intent(in) :: n
         integer :: i

         new_it = it

         do i = 1, n
            call new_it%prev()
         enddo

         return
      end function map_iter_prev_n_default

       function map_find_basic(do_not_use,unused) result(j)
          type :: map_keywordenforcer
             integer :: placeholder
         end type map_Keywordenforcer
         type(map_keywordenforcer) :: j
         type(ChildSpecMapIterator), intent(in) :: do_not_use
         type(keywordenforcer), intent(in) :: unused

         j%placeholder = -1
       end function map_find_basic

      function map_find_if(first, last, p) result(it)
         type(ChildSpecMapIterator) :: it
         type(ChildSpecMapIterator), intent(in) :: first
         type(ChildSpecMapIterator), intent(in) :: last
         interface
            logical function p(item)
               import
               implicit none
               type(ChildSpecPair), intent(in) :: item
            end function p
         end interface

         it = first
         do while (it /= last)
            if (p(it%of())) return

            call it%next()
         end do

         it = last
      end function map_find_if

      function map_find_if_not(first, last, q) result(it)
         type(ChildSpecMapIterator) :: it
         type(ChildSpecMapIterator), intent(in) :: first
         type(ChildSpecMapIterator), intent(in) :: last
         interface
            logical function q(item)
               import
               implicit none
               type(ChildSpecPair), intent(in) :: item
            end function q
         end interface

         it = first
         do while (it /= last)
            if (.not. q(it%of())) return
            call it%next()
         end do

         it = last
      end function map_find_if_not

end module r_mapl3g_ChildSpecMap

module r_mapl3g_ComponentSpecBuilder
   use r_mapl3g_ChildSpecMap
   use r_mapl3g_ChildSpec
   use r_mapl3g_UserSetServices
   implicit none
   private

   public :: build_ChildSpecMap
   
contains


   type(ChildSpecMap) function build_ChildSpecMap(rc) result(specs)
      integer, optional, intent(out) :: rc

      integer :: status

      character(:), allocatable :: child_name
      type(ChildSpec) :: child_spec

      integer :: counter
      
      do counter = 1, 2
           select case(counter)
           case (1)
              child_name = 'A'
              child_spec = ChildSpec(user_setservices('libA','setservices_'))
              call specs%insert('A', ChildSpec(user_setservices('libA','setservices_')))
           case (2)
              child_name = 'B'
              child_spec = ChildSpec(user_setservices('libB','setservices_'))
              call specs%insert('B', ChildSpec(user_setservices('libB','setservices_')))
           end select
        end do

      print*,__FILE__,__LINE__, specs%size()
      print*,__FILE__,__LINE__, specs == specs
      rc = 0 
   end function build_ChildSpecMap


end module r_mapl3g_ComponentSpecBuilder

program main
   use r_mapl3g_ChildSpec
   use r_mapl3g_ChildSpecMap
   use r_mapl3g_UserSetServices
   use r_mapl3g_ComponentSpecBuilder
   implicit none

      type(ChildSpecMap) :: expected, found
      integer :: status

   call expected%insert('A', ChildSpec(user_setservices('libA','setservices_')))
   call expected%insert('B', ChildSpec(user_setservices('libB','setservices_')))

   found = build_ChildSpecMap(rc=status)
   print*,__FILE__,__LINE__, found == expected

end program main

