#include "MAPL_Generic.h"
#include "NUOPC_ErrLog.h"

module GroupRegistryMod
   use, intrinsic :: iso_fortran_env, only: INT64
   use ESMF
   use NUOPC
   use yaFyaml
   use MAPL_ExceptionHandling
   use MAPL_KeywordEnforcerMod

   use GroupMod
   use GroupMapMod

   implicit none
   private

   public GroupRegistry

   type :: GroupRegistry
      private
      type(GroupMap) :: map
   contains
      procedure :: size
      procedure :: count
      procedure :: at
      procedure :: insert

      procedure :: import_groups
      procedure :: import_group
   end type GroupRegistry
contains
   integer(kind=INT64) function size(this)
      class(GroupRegistry), intent(in) :: this

      size = this%map%size()
   end function size

   integer(kind=INT64) function count(this, key)
      class(GroupRegistry), intent(in) :: this
      character(*),         intent(in) :: key

      count = this%map%count(key)
   end function count

   function at(this, key) result(group_entry)
      class(Group), pointer :: group_entry
      class(GroupRegistry), intent(in) :: this
      character(*),         intent(in) :: key

      group_entry => this%map%at(key)
   end function at

   subroutine insert(this, key, group_entry, unusable, rc)
      class(GroupRegistry),             intent(inout) :: this
      character(*),                     intent(in   ) :: key
      class(Group),                     intent(inout) :: group_entry
      class(KeywordEnforcer), optional, intent(  out) :: unusable
      integer,                optional, intent(  out) :: rc

      integer :: status

      _UNUSED_DUMMY(unusable)

      status = 0
      if (this%count(key) > 0) then
         status = 1
      else
         call this%map%insert(key, group_entry)
      end if

      if (present(rc)) rc = status
   end subroutine insert

   subroutine import_groups(this, config, unusable, rc)
      class(GroupRegistry),             intent(inout) :: this
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

         call this%import_group(key, sub_config, __RC__)
         call iter%next()
      end do

      _RETURN(_SUCCESS)
   end subroutine import_groups

   subroutine import_group(this, key, config, unusable, rc)
      class(GroupRegistry),             intent(inout) :: this
      character(*),                     intent(in   ) :: key
      type(Configuration),              intent(inout) :: config
      class(KeywordEnforcer), optional, intent(  out) :: unusable
      integer,                optional, intent(  out) :: rc

      type(Group) :: group_entry
      integer     :: status

      _UNUSED_DUMMY(unusable)

      call group_entry%import_group(config, __RC__)
      call this%insert(key, group_entry, __RC__)

      _RETURN(_SUCCESS)
   end subroutine import_group
end module GroupRegistryMod
