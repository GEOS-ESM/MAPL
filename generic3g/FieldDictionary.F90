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

   type :: FieldDictionary
      private
      type(FieldDictionaryItemMap) :: entries
      type(StringStringMap) :: alias_map  ! For efficiency
   contains

      procedure :: add_item
      procedure :: add_aliases

      ! accessors
      procedure :: get_item   ! returns a pointer
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
           _ASSERT(len_trim(standard_name) /= 0, 'Standard name is all blanks.')
           _ASSERT(fd%entries%count(standard_name) == 0, 'Duplicate standard name: <'//trim(standard_name)//'>')

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
         class(YAML_Node), pointer :: aliases_node, alias_node
         character(:), allocatable :: long_name, units
         type(StringVector) :: aliases

         _ASSERT(item_node%is_mapping(), 'Each node in FieldDictionary yaml must be a mapping node')

         call item_node%get(long_name, 'long_name', _RC)
         call item_node%get(units, 'canonical_units', _RC)

         if (item_node%has('aliases')) then
            aliases_node => item_node%of('aliases')
            _ASSERT(aliases_node%is_sequence(), "'aliases' must be a sequence")

            associate (b => aliases_node%begin(), e => aliases_node%end())
              iter = b
              do while (iter /= e)
                 alias_node => iter%at(_RC)
                 _ASSERT(alias_node%is_string(), 'short name must be a string')
                 call aliases%push_back(to_string(alias_node))
                 
                 call iter%next()
              end do
            end associate

         end if

         item = FieldDictionaryItem(long_name, units, aliases)
         
         _RETURN(_SUCCESS)
      end function to_item

   end function new_from_textstream



   subroutine add_item(this, standard_name, field_item, rc)
      class(FieldDictionary), intent(inout) :: this
      character(*), intent(in) :: standard_name
      type(FieldDictionaryItem), intent(in) :: field_item
      integer, intent(out), optional :: rc

      integer :: status

      call this%entries%insert(standard_name, field_item)
      call this%add_aliases(standard_name, field_item%get_aliases(), _RC)

      _RETURN(_SUCCESS)
   end subroutine add_item

   subroutine add_aliases(this, standard_name, aliases, rc)
      class(FieldDictionary), intent(inout) :: this
      character(*), intent(in) :: standard_name
      type(StringVector), intent(in) :: aliases
      integer, optional, intent(out) :: rc

      integer :: status
      type(StringVectorIterator) :: iter
      character(:), pointer :: alias

      associate (b => aliases%begin(), e => aliases%end())
        iter = b
        do while (iter /= e)
           alias => iter%of()
           _ASSERT(this%alias_map%count(alias) == 0, 'ambiguous short name references more than one item in dictionary')
           call this%alias_map%insert(alias, standard_name)
           call iter%next()
        end do
      end associate

      _RETURN(_SUCCESS)
   end subroutine add_aliases
      

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


   function get_units(this, standard_name, rc) result(canonical_units)
      character(:), allocatable :: canonical_units
      class(FieldDictionary), target, intent(in) :: this
      character(*), intent(in) :: standard_name
      integer, optional, intent(out) :: rc

      type(FieldDictionaryItem), pointer :: item
      integer :: status

      item => this%entries%at(standard_name, _RC)
      canonical_units = item%get_units()

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
      long_name = item%get_long_name()

      _RETURN(_SUCCESS)
   end function get_long_name


   function get_standard_name(this, alias, rc) result(standard_name)
      character(:), allocatable :: standard_name
      class(FieldDictionary), target, intent(in) :: this
      character(*), intent(in) :: alias
      integer, optional, intent(out) :: rc

      integer :: status

      standard_name = this%alias_map%at(alias, _RC)
      
      _RETURN(_SUCCESS)
   end function get_standard_name


   integer function size(this)
      class(FieldDictionary), intent(in) :: this
      size = this%entries%size()
   end function size

   
end module mapl3g_FieldDictionary
