module mapl3g_FieldDictionaryItem
   use gftl2_StringVector
   implicit none
   private

   public :: FieldDictionaryItem

   type :: FieldDictionaryItem
      private
      character(:), allocatable :: long_name
      character(:), allocatable :: canonical_units
      type(StringVector) :: aliases
!!$      character(:), allocatable :: physical_dimensions

   contains

      procedure :: get_long_name
      procedure :: get_units
      procedure :: get_aliases
      
   end type FieldDictionaryItem

   !************************
   ! Caution:  Multiple constructor arguments are strings, and
   ! as such incorrect order is a potential source of error
   ! in client code.
   !************************
   
   interface FieldDictionaryItem
      module procedure new_FieldDictionaryItem_
      module procedure new_FieldDictionaryItem_one_alias
      module procedure new_FieldDictionaryItem_multi_aliases
      module procedure new_FieldDictionaryItem_vector
   end interface


contains


   function new_FieldDictionaryItem_(long_name, canonical_units) result(item)
      type(FieldDictionaryItem) :: item
      character(*), intent(in) :: long_name
      character(*), intent(in) :: canonical_units

      item = FieldDictionaryItem(long_name, canonical_units, [character(1) ::])

   end function new_FieldDictionaryItem_

   function new_FieldDictionaryItem_one_alias(long_name, canonical_units, alias) result(item)
      type(FieldDictionaryItem) :: item
      character(*), intent(in) :: long_name
      character(*), intent(in) :: canonical_units
      character(*), intent(in) :: alias

      item = FieldDictionaryItem(long_name, canonical_units, [alias])

   end function new_FieldDictionaryItem_one_alias

   function new_FieldDictionaryItem_multi_aliases(long_name, canonical_units, aliases) result(item)
      type(FieldDictionaryItem) :: item
      character(*), intent(in) :: long_name
      character(*), intent(in) :: canonical_units
      character(*), intent(in) :: aliases(:)

      integer :: i
      type(StringVector) :: aliases_vector

      do i = 1, size(aliases)
         call aliases_vector%push_back(trim(aliases(i)))
      end do

      item = FieldDictionaryItem(long_name, canonical_units, aliases_vector)
      
   end function new_FieldDictionaryItem_multi_aliases

   function new_FieldDictionaryItem_vector(long_name, canonical_units, aliases) result(item)
      type(FieldDictionaryItem) :: item
      character(*), intent(in) :: long_name
      character(*), intent(in) :: canonical_units
      type(StringVector), intent(in) :: aliases

      item%long_name = long_name
      item%canonical_units = canonical_units
      item%aliases = aliases
      
   end function new_FieldDictionaryItem_vector


   ! accessors


   pure function get_long_name(this) result(long_name)
      character(len=:), allocatable :: long_name
      class(FieldDictionaryItem), intent(in) :: this
      long_name = this%long_name
   end function get_long_name

   pure function get_units(this) result(units)
      character(len=:), allocatable :: units
      class(FieldDictionaryItem), intent(in) :: this
      units = this%canonical_units
   end function get_units

   pure function get_aliases(this) result(aliases)
      type(StringVector) :: aliases
      class(FieldDictionaryItem), intent(in) :: this
      aliases = this%aliases
   end function get_aliases

end module mapl3g_FieldDictionaryItem
