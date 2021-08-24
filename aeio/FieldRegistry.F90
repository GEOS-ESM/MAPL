#include "MAPL_Generic.h"
#include "NUOPC_ErrLog.h"

module FieldRegistryMod
   use, intrinsic :: iso_fortran_env, only: INT64
   use ESMF
   use NUOPC
   use gFTL_StringVector
   use MAPL_ExceptionHandling
   use MAPL_KeywordEnforcerMod

   use FieldEntryMod
   use FieldEntryMapMod

   implicit none
   private

   public FieldRegistry

   type :: FieldRegistry
      private
      type(FieldEntryMap) :: map
   contains
      procedure :: size
      procedure :: count
      procedure :: at
      procedure :: insert

      procedure :: get_map

      procedure :: contains_short_name
   end type FieldRegistry
contains
   integer(kind=INT64) function size(this)
      class(FieldRegistry), intent(in) :: this

      size = this%map%size()
   end function size

   integer(kind=INT64) function count(this, key)
      class(FieldRegistry), intent(in) :: this
      character(*),         intent(in) :: key

      count = this%map%count(key)
   end function count

   function at(this, key) result(field_entry)
      class(FieldEntry), pointer :: field_entry
      class(FieldRegistry), intent(in) :: this
      character(*),         intent(in) :: key

      field_entry => this%map%at(key)
   end function at

   subroutine insert(this, field_entry)
      class(FieldRegistry), intent(inout) :: this
      class(FieldEntry),    intent(inout) :: field_entry

      call this%map%insert(field_entry%standard_name(), field_entry)
   end subroutine insert

   function get_map(this) result(map)
      type(FieldEntryMap) :: map
      class(FieldRegistry), intent(inout) :: this

      map = this%map
   end function get_map

   logical function contains_short_name(this, short_name)
      class(FieldRegistry), intent(in) :: this
      character(*),         intent(in) :: short_name

      logical                     :: found
      class(FieldEntry), pointer  :: field_entry
      type(FieldEntryMapIterator) :: iter

      found = .false.

      iter = this%map%begin()
      do while(iter /= this%map%end())
         field_entry => iter%value()

         if (short_name == field_entry%get_short_name()) then
            found = .true.
            exit
         end if
         call iter%next()
      end do

      contains_short_name = found
   end function contains_short_name
end module FieldRegistryMod
