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
   use esmf
   use mapl_ErrorHandling
   use gftl2_StringVector
   use gftl2_StringStringMap
   use mapl3g_FieldDictionaryItem
   use mapl3g_FieldDictionaryItemMap
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
      !module procedure new_empty
      module procedure new_from_yaml
   end interface FieldDictionary

contains

   !function new_empty() result(fd)
      !type(FieldDictionary) :: fd
!
      !fd = FieldDictionary(stream='{}')
!
   !end function new_empty

   function new_from_yaml(filename, stream, rc) result(fd)
      type(FieldDictionary) :: fd
      character(len=*), optional, intent(in) :: filename
      character(len=*), optional, intent(in) :: stream
      integer, optional, intent(out) :: rc

      type(ESMF_HConfig), target :: node
      type(ESMF_HConfigIter) :: hconfigIter,hconfigIterBegin,hconfigIterEnd
      integer :: status
      character(:), allocatable :: standard_name
      type(FieldDictionaryItem) :: item
      type(ESMF_HConfig) :: val


      _ASSERT( (.not.present(filename)) .and. (.not.present(stream)), "cannot specify both")
      if (present(filename)) then
         node = ESMF_HConfigCreate(filename=filename,_RC)
      else if (present(stream)) then
         node = ESMF_HConfigCreate(content=stream,_RC)
      else
         _FAIL("must provide one or the other")
      end if

      _ASSERT(ESMF_HConfigIsMap(node), 'FieldDictionary requires a YAML mapping node')

      hconfigIter = ESMF_HConfigIterBegin(node)
      hconfigIterBegin = ESMF_HConfigIterBegin(node)
      hconfigIterEnd = ESMF_HConfigIterEnd(node)
      do while (ESMF_HConfigIterLoop(hconfigIter,hconfigIterBegin,hconfigIterEnd))
         standard_name = ESMF_HConfigAsStringMapKey(hconfigIter,_RC)
         _ASSERT(len_trim(standard_name) /= 0, 'Standard name is all blanks.')
         _ASSERT(fd%entries%count(standard_name) == 0, 'Duplicate standard name: <'//trim(standard_name)//'>')
         val = ESMF_HConfigCreateAtMapVal(hconfigIter,_RC)
         item = to_item(val,_RC)
         call fd%add_item(standard_name, item)
      enddo

      _RETURN(_SUCCESS)

   contains


      function to_item(item_node, rc) result(item)
         type(FieldDictionaryItem) :: item
         type(ESMF_HConfig), intent(in) :: item_node
         integer, optional, intent(out) :: rc

         integer :: status
         type(ESMF_HConfig) :: aliases_node, alias_node
         character(:), allocatable :: long_name, units, temp_string
         type(StringVector) :: aliases
         type(ESMF_HConfigIter) :: hconfigIter,hconfigIterBegin,hconfigIterEnd

         _ASSERT(ESMF_HConfigIsMap(item_node), 'Each node in FieldDictionary yaml must be a mapping node')

         long_name = ESMF_HconfigAsString(item_node,keyString='long_name',_RC)
         units = ESMF_HConfigAsString(item_node,keyString='canonical_units',_RC)

         if (ESMF_HConfigIsDefined(item_node,keyString='aliases')) then
          
            aliases_node = ESMF_HConfigCreateAt(item_node,keyString='aliases',_RC) 
            _ASSERT(ESMF_HConfigIsSequence(aliases_node), "'aliases' must be a sequence")

            hconfigIter = ESMF_HConfigIterBegin(aliases_node)
            hconfigIterBegin = ESMF_HConfigIterBegin(aliases_node)
            hconfigIterEnd = ESMF_HConfigIterEnd(aliases_node)

            do while (ESMF_HConfigIterLoop(hconfigIter,hconfigIterBegin,hconfigIterEnd))
               temp_string = ESMF_HConfigAsString(hconfigIter,_RC)
               call aliases%push_back(temp_string)
            enddo

         end if

         item = FieldDictionaryItem(long_name, units, aliases)
         
         _RETURN(_SUCCESS)
      end function to_item

   end function new_from_yaml

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
