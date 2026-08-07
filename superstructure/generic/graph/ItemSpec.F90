module mapl_ItemSpec_mod
   implicit none(type, external)
   private

   public :: ItemSpec, StringAttribute
   public :: operator(==), operator(/=)

   type :: StringAttribute
      character(:), allocatable :: name
      character(:), allocatable :: value
   end type StringAttribute

   type :: ItemSpec
      private
      character(:), allocatable :: category_
      integer :: precision_ = 0
      integer :: gridded_rank_ = 0
      integer :: ungridded_rank_ = 0
      character(:), allocatable :: units_
      character(:), allocatable :: grid_name_
      type(StringAttribute), allocatable :: attributes_(:)
   contains
      procedure :: category
      procedure :: precision
      procedure :: gridded_rank
      procedure :: ungridded_rank
      procedure :: units
      procedure :: grid_name
      procedure :: attributes
      procedure :: structurally_compatible
      procedure :: with_precision
      procedure :: with_units
   end type ItemSpec

   interface ItemSpec
      procedure new_item_spec
   end interface

   interface operator(==)
      procedure equal
   end interface

   interface operator(/=)
      procedure not_equal
   end interface

contains

   function new_item_spec(category, precision, gridded_rank, ungridded_rank, units, grid_name, attributes) result(spec)
      character(*), intent(in) :: category, units, grid_name
      integer, intent(in) :: precision, gridded_rank, ungridded_rank
      type(StringAttribute), optional, intent(in) :: attributes(:)
      type(ItemSpec) :: spec

      spec%category_ = category
      spec%precision_ = precision
      spec%gridded_rank_ = gridded_rank
      spec%ungridded_rank_ = ungridded_rank
      spec%units_ = units
      spec%grid_name_ = grid_name
      if (present(attributes)) then
         spec%attributes_ = attributes
      else
         allocate(spec%attributes_(0))
      end if
   end function new_item_spec

   function category(this) result(value)
      class(ItemSpec), intent(in) :: this
      character(:), allocatable :: value
      value = this%category_
   end function category

   pure integer function precision(this)
      class(ItemSpec), intent(in) :: this
      precision = this%precision_
   end function precision

   pure integer function gridded_rank(this)
      class(ItemSpec), intent(in) :: this
      gridded_rank = this%gridded_rank_
   end function gridded_rank

   pure integer function ungridded_rank(this)
      class(ItemSpec), intent(in) :: this
      ungridded_rank = this%ungridded_rank_
   end function ungridded_rank

   function units(this) result(value)
      class(ItemSpec), intent(in) :: this
      character(:), allocatable :: value
      value = this%units_
   end function units

   function grid_name(this) result(value)
      class(ItemSpec), intent(in) :: this
      character(:), allocatable :: value
      value = this%grid_name_
   end function grid_name

   function attributes(this) result(values)
      class(ItemSpec), intent(in) :: this
      type(StringAttribute), allocatable :: values(:)
      values = this%attributes_
   end function attributes

   logical function structurally_compatible(this, other)
      class(ItemSpec), intent(in) :: this
      type(ItemSpec), intent(in) :: other

      structurally_compatible = this%category_ == other%category_ .and. &
         this%gridded_rank_ == other%gridded_rank_ .and. &
         this%ungridded_rank_ == other%ungridded_rank_ .and. &
         this%grid_name_ == other%grid_name_ .and. same_attributes(this%attributes_, other%attributes_)
   end function structurally_compatible

   function with_precision(this, value) result(spec)
      class(ItemSpec), intent(in) :: this
      integer, intent(in) :: value
      type(ItemSpec) :: spec
      spec = this
      spec%precision_ = value
   end function with_precision

   function with_units(this, value) result(spec)
      class(ItemSpec), intent(in) :: this
      character(*), intent(in) :: value
      type(ItemSpec) :: spec
      spec = this
      spec%units_ = value
   end function with_units

   logical function equal(lhs, rhs)
      type(ItemSpec), intent(in) :: lhs, rhs
      equal = lhs%structurally_compatible(rhs) .and. lhs%precision_ == rhs%precision_ .and. lhs%units_ == rhs%units_
   end function equal

   logical function not_equal(lhs, rhs)
      type(ItemSpec), intent(in) :: lhs, rhs
      not_equal = .not. (lhs == rhs)
   end function not_equal

   logical function same_attributes(lhs, rhs)
      type(StringAttribute), intent(in) :: lhs(:), rhs(:)
      integer :: i, j

      same_attributes = .false.
      if (size(lhs) /= size(rhs)) return
      do i = 1, size(lhs)
         do j = 1, size(rhs)
            if (lhs(i)%name == rhs(j)%name .and. lhs(i)%value == rhs(j)%value) exit
         end do
         if (j > size(rhs)) return
      end do
      same_attributes = .true.
   end function same_attributes

end module mapl_ItemSpec_mod
