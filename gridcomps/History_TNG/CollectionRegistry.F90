#include "MAPL_Generic.h"
#include "NUOPC_ErrLog.h"

module CollectionRegistryMod
   use, intrinsic :: iso_fortran_env, only: INT64
   use ESMF
   use NUOPC
   use yaFyaml
   use MAPL_ExceptionHandling
   use MAPL_KeywordEnforcerMod

   use CollectionMod
   use CollectionMapMod

   implicit none
   private

   public CollectionRegistry

   type :: CollectionRegistry
      private
      type(CollectionMap) :: map
   contains
      procedure :: size
      procedure :: count
      procedure :: at
      procedure :: insert

      procedure :: import_collections
      procedure :: import_collection
   end type CollectionRegistry
contains
   integer(kind=INT64) function size(this)
      class(CollectionRegistry), intent(in) :: this

      size = this%map%size()
   end function size

   integer(kind=INT64) function count(this, key)
      class(CollectionRegistry), intent(in) :: this
      character(*),              intent(in) :: key

      count = this%map%count(key)
   end function count

   function at(this, key) result(collection_entry)
      class(Collection), pointer :: collection_entry
      class(CollectionRegistry), intent(in) :: this
      character(*),              intent(in) :: key

      collection_entry => this%map%at(key)
   end function at

   subroutine insert(this, collection_entry, unusable, rc)
      class(CollectionRegistry),        intent(inout) :: this
      class(Collection),                intent(inout) :: collection_entry
      class(KeywordEnforcer), optional, intent(  out) :: unusable
      integer,                optional, intent(  out) :: rc

      character(:), allocatable :: name
      integer :: status

      _UNUSED_DUMMY(unusable)

      name = collection_entry%get_name()

      status = 0
      if (this%count(name) > 0) then
         status = 1
      else
         call this%map%insert(name, collection_entry)
      end if

      if (present(rc)) rc = status
   end subroutine insert

   subroutine import_collections(this, config, unusable, rc)
      class(CollectionRegistry),        intent(inout) :: this
      type(Configuration),              intent(inout) :: config
      class(KeywordEnforcer), optional, intent(  out) :: unusable
      integer,                optional, intent(  out) :: rc

      character(:), pointer       :: key
      type(ConfigurationIterator) :: iter
      type(Configuration)         :: sub_config

      integer :: status

      _UNUSED_DUMMY(unusable)

      iter = config%begin()
      do while(iter /= config%end())
         key        => iter%key()
         sub_config =  iter%value()

         call this%import_collection(key, sub_config, __RC__)
         call iter%next()
      end do

      _RETURN(_SUCCESS)
   end subroutine import_collections

   subroutine import_collection(this, name, config, unusable, rc)
      class(CollectionRegistry),        intent(inout) :: this
      character(*),                     intent(in   ) :: name
      type(Configuration),              intent(inout) :: config
      class(KeywordEnforcer), optional, intent(  out) :: unusable
      integer,                optional, intent(  out) :: rc

      type(Collection) :: collection_entry

      integer :: status

      _UNUSED_DUMMY(unusable)

      call collection_entry%import_collection(name, config, __RC__)
      call this%insert(collection_entry, __RC__)

      _RETURN(_SUCCESS)
   end subroutine import_collection
end module CollectionRegistryMod
