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

      procedure :: advertise
      procedure :: realize
      procedure :: register

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

   subroutine advertise(this, state, unusable, TransferOfferGeomObject, rc)
      class(FieldRegistry),             intent(inout) :: this
      type(ESMF_State),                 intent(inout) :: state
      class(KeywordEnforcer), optional, intent(in   ) :: unusable
      character(len=*),       optional, intent(in   ) :: TransferOfferGeomObject
      integer,                optional, intent(  out) :: rc

      class(FieldEntry), pointer  :: field_entry
      type(FieldEntryMapIterator) :: iter

      integer :: status

      _UNUSED_DUMMY(unusable)

      iter = this%map%begin()
      do while(iter /= this%map%end())
         field_entry => iter%value()
         call field_entry%advertise(state, TransferOfferGeomObject=TransferOfferGeomObject,  __RC__)

         call iter%next()
      end do

      _RETURN(_SUCCESS)
   end subroutine advertise

   subroutine register(this, unusable, rc)
      class(FieldRegistry),             intent(inout) :: this
      class(KeywordEnforcer), optional, intent(in   ) :: unusable
      integer,                optional, intent(  out) :: rc

      class(FieldEntry), pointer  :: field_entry
      type(FieldEntryMapIterator) :: iter

      integer :: status

      _UNUSED_DUMMY(unusable)

      iter = this%map%begin()
      do while(iter /= this%map%end())
         field_entry => iter%value()
         call field_entry%register( __RC__)

         call iter%next()
      end do

      _RETURN(_SUCCESS)
   end subroutine register

   ! TODO: unit tests for realize

   subroutine realize(this, state, export_state, unusable, rc)
      class(FieldRegistry),             intent(inout) :: this
      type(ESMF_State),                 intent(inout) :: state
      type(ESMF_State),                 intent(inout) :: export_state
      class(KeywordEnforcer), optional, intent(in   ) :: unusable
      integer,                optional, intent(  out) :: rc

      class(FieldEntry), pointer  :: field_entry
      type(FieldEntryMapIterator) :: iter

      integer :: status

      _UNUSED_DUMMY(unusable)

      iter = this%map%begin()
      do while(iter /= this%map%end())
         field_entry => iter%value()
         call field_entry%realize(state, export_state,__RC__)

         call iter%next()
      end do

      _RETURN(_SUCCESS)
   end subroutine realize

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
