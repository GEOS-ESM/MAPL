#include "MAPL_ErrLog.h"

! The FieldDictionary serves as a central structure for both ensuring
! consistent standard names and units across GEOS as well as a convenient
! mechanism to avoid duplicating such information in the FieldSpec's in
! various components.

! The dictionary keys are CF standard names, and each entry must include a
! long name and units.   It may optionally include additional short names that
! are convenient as alternative keys into the dictionary.

! Note that each short name must be unique such that it is unambiguous
! as to which entry a short name is referring.

module mapl3g_FieldDictionary
   use yaFyaml
   use mapl_ErrorHandling
   use gftl2_StringVector
   use gftl2_StringStringMap
   use mapl3g_FieldDictionaryItem
   use mapl3g_FieldDictionaryItemMap
   use yaFyaml, only: AbstractTextStream, FileStream
   use yaFyaml, only: Parser
   use yaFyaml, only: YAML_Node
   implicit none
   private

   public :: FieldDictionary
   public :: GEOS_Field_Dictionary

   type :: FieldDictionary
      private
      type(FieldDictionaryItemMap) :: entries
      type(StringStringMap) :: alias_map  ! For efficiency
   contains

      procedure :: add_item => add_item

      ! accessors
      procedure :: get_item
      procedure :: get_units
      procedure :: get_long_name
      procedure :: get_standard_name
      procedure :: size

   end type FieldDictionary

   interface FieldDictionary
      module procedure new_empty
      module procedure new_from_filename
      module procedure new_from_textstream
   end interface FieldDictionary

   type(FieldDictionary), protected :: GEOS_Field_Dictionary
   
contains

   function new_empty() result(fd)
      type(FieldDictionary) :: fd

      fd = FieldDictionary(TextStream('{}'))

   end function new_empty
   
   function new_from_filename(filename, rc) result(fd)
      type(FieldDictionary) :: fd
      character(len=*), intent(in) :: filename
      integer, optional, intent(out) :: rc

      integer :: status

      fd = FieldDictionary(FileStream(filename), rc=status)

      _RETURN(_SUCCESS)
   end function new_from_filename

   ! This interface is to support unit testing
   function new_from_textstream(stream, rc) result(fd)
      type(FieldDictionary) :: fd
      class(AbstractTextStream), intent(in) :: stream
      integer, optional, intent(out) :: rc

      type(Parser) :: p
      class(YAML_Node), target, allocatable :: node
      integer :: status
      class(NodeIterator), allocatable :: iter
      character(:), pointer :: standard_name
      type(FieldDictionaryItem) :: item

      p = Parser()
      node = p%load(stream)

      _ASSERT(node%is_mapping(), 'FieldDictionary requires a YAML mapping node')

      associate (b => node%begin(), e => node%end())

        iter = b
        do while (iter /= e)

           standard_name => to_string(iter%first(), _RC)
           item = to_item(iter%second(), _RC)
           call fd%add_item(standard_name, item)

           call iter%next()

        end do
      end associate
           
      
      _RETURN(_SUCCESS)

   contains


      function to_item(item_node, rc) result(item)
         type(FieldDictionaryItem) :: item
         class(YAML_Node), intent(in) :: item_node
         integer, optional, intent(out) :: rc

         integer :: status
         class(NodeIterator), allocatable :: iter
         class(YAML_Node), pointer :: short_names_node, short_name_node
         character(:), allocatable :: long_name, units
         type(StringVector) :: short_names

         _ASSERT(item_node%is_mapping(), 'Each node in FieldDictionary yaml must be a mapping node')


         call item_node%get(long_name, "long name", _RC)
         call item_node%get(units, "units", _RC)

         if (item_node%has('short names')) then
            short_names_node => item_node%of('short names')
            _ASSERT(short_names_node%is_sequence(), 'short names must be a sequence')

            associate (b => short_names_node%begin(), e => short_names_node%end())
              iter = b
              do while (iter /= e)
                 short_name_node => iter%at(_RC)
                 _ASSERT(short_name_node%is_string(), 'short name must be a string')
                 call short_names%push_back(to_string(short_name_node))
                 call iter%next()
              end do
            end associate

         end if

         item = FieldDictionaryItem(long_name, units, short_names)
         
         _RETURN(_SUCCESS)
      end function to_item

   end function new_from_textstream



   subroutine add_item(this, standard_name, field_item)
      class(FieldDictionary), intent(inout) :: this
      character(*), intent(in) :: standard_name
      type(FieldDictionaryItem), intent(in) :: field_item

      call this%entries%insert(standard_name, field_item)
   end subroutine add_item


   ! This accessor returns a copy for safety reasons.  Returning a
   ! pointer would be more efficient, but it would allow client code
   ! to modify the dictionary.
   function get_item(this, standard_name, rc) result(item)
      type(FieldDictionaryItem) :: item
      class(FieldDictionary), intent(in) :: this
      character(*), intent(in) :: standard_name
      integer, optional, intent(out) :: rc

      integer :: status

      item = this%entries%at(standard_name, _RC)

      _RETURN(_SUCCESS)
   end function get_item


   function get_units(this, standard_name, rc) result(units)
      character(:), allocatable :: units
      class(FieldDictionary), target, intent(in) :: this
      character(*), intent(in) :: standard_name
      integer, optional, intent(out) :: rc

      type(FieldDictionaryItem), pointer :: item
      integer :: status

      item => this%entries%at(standard_name, _RC)
      units = item%units

      _RETURN(_SUCCESS)
   end function get_units


   function get_long_name(this, standard_name, rc) result(long_name)
      character(:), allocatable :: long_name
      class(FieldDictionary), target, intent(in) :: this
      character(*), intent(in) :: standard_name
      integer, optional, intent(out) :: rc

      type(FieldDictionaryItem), pointer :: item
      integer :: status

      item => this%entries%at(standard_name, _RC)
      long_name = item%long_name

      _RETURN(_SUCCESS)
   end function get_long_name

   function get_standard_name(this, alias, rc) result(standard_name)
      character(:), allocatable :: standard_name
      class(FieldDictionary), target, intent(in) :: this
      character(*), intent(in) :: alias
      integer, optional, intent(out) :: rc

      type(FieldDictionaryItem), pointer :: item
      type(FieldDictionaryItemMapIterator) :: iter
      type(StringVectorIterator) :: alias_iter
      integer :: status

      associate (b => this%entries%begin(), e => this%entries%end())
        iter = b
        do while (iter /= e)
           item => iter%second()

           associate (b_aliases => item%short_names%begin(), e_aliases => item%short_names%end())
             alias_iter = find(first=b_aliases, last=e_aliases, value=alias)
             if (alias_iter /=  e_aliases) then
                standard_name = iter%first()
                _RETURN(_SUCCESS)
             end if
           end associate
           call iter%next()
        end do
      end associate
      _FAIL('alias <'//alias//'> not found in field dictionary.')
      
      _RETURN(_SUCCESS)
   end function get_standard_name

   integer function size(this)
      class(FieldDictionary), intent(in) :: this

      size = this%entries%size()
   end function size

   
end module mapl3g_FieldDictionary
