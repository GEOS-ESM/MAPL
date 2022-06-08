module mapl3g_FieldDictionaryItem
   use gftl2_StringVector
   implicit none
   private

   public :: FieldDictionaryItem

   type :: FieldDictionaryItem
      character(:), allocatable :: long_name
      character(:), allocatable :: units
      type(StringVector) :: short_names ! aliases
   end type FieldDictionaryItem

   interface FieldDictionaryItem
      module procedure new_FieldDictionaryItem_
      module procedure new_FieldDictionaryItem_one_short
      module procedure new_FieldDictionaryItem_multi_short
      module procedure new_FieldDictionaryItem_vector
   end interface

contains
   
   function new_FieldDictionaryItem_(long_name, units) result(item)
      type(FieldDictionaryItem) :: item
      character(*), intent(in) :: long_name
      character(*), intent(in) :: units

      item = FieldDictionaryItem(long_name, units, [character(1) ::])

   end function new_FieldDictionaryItem_

   function new_FieldDictionaryItem_one_short(long_name, units, short_name) result(item)
      type(FieldDictionaryItem) :: item
      character(*), intent(in) :: long_name
      character(*), intent(in) :: units
      character(*), intent(in) :: short_name


      item = FieldDictionaryItem(long_name, units, [short_name])

   end function new_FieldDictionaryItem_one_short

   function new_FieldDictionaryItem_multi_short(long_name, units, short_names) result(item)
      type(FieldDictionaryItem) :: item
      character(*), intent(in) :: long_name
      character(*), intent(in) :: units
      character(*), intent(in) :: short_names(:)

      integer :: i
      type(StringVector) :: short_names_vector

      do i = 1, size(short_names)
         call short_names_vector%push_back(trim(short_names(i)))
      end do

      item = FieldDictionaryItem(long_name, units, short_names_vector)
      
   end function new_FieldDictionaryItem_multi_short

   function new_FieldDictionaryItem_vector(long_name, units, short_names) result(item)
      type(FieldDictionaryItem) :: item
      character(*), intent(in) :: long_name
      character(*), intent(in) :: units
      type(StringVector), intent(in) :: short_names

      item%long_name = long_name
      item%units = units
      item%short_names = short_names
      
   end function new_FieldDictionaryItem_vector


end module mapl3g_FieldDictionaryItem
