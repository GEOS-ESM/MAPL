module mapl3g_InternalConnectionPt
   use gftl2_StringVector
   implicit none
   private
  
   public :: InternalConnectionPt
   public :: operator(<)
   public :: operator(==)
  
   type :: InternalConnectionPt
      character(:), allocatable :: state_intent_
      type(StringVector) :: nested_name
   contains
      procedure :: state_intent
      procedure :: short_name
      procedure :: is_import
      procedure :: is_export
      procedure :: is_internal
      procedure :: set_short_name
      procedure :: to_string
   end type InternalConnectionPt
  
   interface operator(<)
      module procedure less
   end interface operator(<)

   interface operator(==)
      module procedure equal_to
   end interface operator(==)

   interface InternalConnectionPt
      module procedure new_cp_nested_name
      module procedure new_cp_short_name
      module procedure new_cp_split
   end interface InternalConnectionPt
   
contains

   function new_cp_nested_name(state_intent, nested_name) result(internal_pt)
      type(InternalConnectionPt) :: internal_pt
      character(*), intent(in) :: state_intent
      type(StringVector), intent(in) :: nested_name

      internal_pt%state_intent_ = state_intent
      internal_pt%nested_name = nested_name

   end function new_cp_nested_name


   function new_cp_short_name(state_intent, short_name) result(internal_pt)
      type(InternalConnectionPt) :: internal_pt
      character(*), intent(in) :: state_intent
      character(*), intent(in) :: short_name

      internal_pt = InternalConnectionPt(state_intent, StringVector(1, short_name))

   end function new_cp_short_name

   ! This constructor uses a "/" separated string to define a nesting
   ! for a relative point.  Not that there must be at least one "/",
   ! but there is currently not a check for that.
   function new_cp_split(long_name) result(internal_pt)
      type(InternalConnectionPt) :: internal_pt
      character(*), intent(in) :: long_name

      character(:), allocatable :: buf
      type(StringVector) :: nested_name

      buf = long_name
      associate (state_intent => get_next_item(buf))
        internal_pt%state_intent_ = state_intent
        do
           if (len(buf) == 0) exit
           call nested_name%push_back(get_next_item(buf))
        end do
        internal_pt = InternalConnectionPt(state_intent, nested_name)
      end associate

   contains

      function get_next_item(buf) result(item)
         character(:), allocatable :: item
         character(:), allocatable, intent(inout) :: buf

         associate (idx => index(buf, '/'))
           if (idx == 0) then
              item = buf
              buf = ''
           else
              item = buf(:idx-1)
              buf = buf(idx+1:)
           end if
         end associate
      end function get_next_item

   end function new_cp_split


   ! Short name is always the last item in the nesting.
   function short_name(this)
      character(:), pointer :: short_name
      class(InternalConnectionPt), target, intent(in) :: this
      short_name => this%nested_name%back()
   end function short_name

   ! state intent is always the top item in nestingn
   function state_intent(this)
      character(:), pointer :: state_intent
      class(InternalConnectionPt), target, intent(in) :: this
      state_intent => this%state_intent_
   end function state_intent

   logical function less(lhs, rhs)
      type(InternalConnectionPt), intent(in) :: lhs
      type(InternalConnectionPt), intent(in) :: rhs

      logical :: greater

      less = lhs%state_intent_ < rhs%state_intent_
      if (less) return

      ! Not less, but maybe equal ...
      greater = rhs%state_intent_ < lhs%state_intent_
      if (greater) return
      
      ! same intent, then ...
      less = lhs%nested_name < rhs%nested_name
   end function less

   logical function equal_to(lhs, rhs)
      type(InternalConnectionPt), intent(in) :: lhs
      type(InternalConnectionPt), intent(in) :: rhs
      equal_to = (lhs%state_intent_ == rhs%state_intent_) .and. (lhs%nested_name == rhs%nested_name)
   end function equal_to

   logical function is_import(this)
      class(InternalConnectionPt), intent(in) :: this
      is_import = (this%state_intent() == 'import')
   end function is_import

   logical function is_export(this)
      class(InternalConnectionPt), intent(in) :: this
      is_export = (this%state_intent() == 'export')
   end function is_export

   logical function is_internal(this)
      class(InternalConnectionPt), intent(in) :: this
      is_internal = (this%state_intent() == 'internal')
   end function is_internal


   subroutine set_short_name(this, new_name)
      class(InternalConnectionPt), intent(inout) :: this
      character(*), intent(in) :: new_name

      call this%nested_name%pop_back()
      call this%nested_name%push_back(new_name)
   end subroutine set_short_name

   function to_string(this) result(s)
      class(InternalConnectionPt), intent(in) :: this
      character(:), allocatable :: s

      type(StringVectorIterator) :: iter
      s = this%state_intent_
      associate (e => this%nested_name%end())
        iter = this%nested_name%begin()
        do while (iter /= e)
           s = s // '/' // iter%of()
           call iter%next()
        end do
      end associate
   end function to_string

end module mapl3g_InternalConnectionPt
