#include "MAPL_Generic.h"
#include "NUOPC_ErrLog.h"

module HistoryConfigMod
   use, intrinsic :: iso_fortran_env, only: INT64
   use ESMF
   use NUOPC
   use yaFyaml
   use gFTL_StringVector
   use MAPL_ExceptionHandling
   use MAPL_KeywordEnforcerMod

   Use FieldRegistryMod
   use GroupRegistryMod
   use CollectionMod
   use CollectionRegistryMod

   implicit none
   private

   public HistoryConfig

   character(*), parameter :: enabled_key     = 'Enabled'
   character(*), parameter :: groups_key      = 'Groups'
   character(*), parameter :: collections_key = 'Collections'

   type :: HistoryConfig
      private
      type(StringVector) :: enabled

      class(FieldRegistry),      allocatable :: fields
      class(GroupRegistry),      allocatable :: groups
      class(CollectionRegistry), allocatable :: collections
   contains
      procedure :: get_enabled
      procedure :: set_enabled
      procedure :: get_fields
      procedure :: get_groups
      procedure :: get_collections
      procedure :: set_collections

      procedure :: import_yaml
      procedure :: import_enabled
      procedure :: finish_import

      procedure :: fill_collection_groups
      procedure :: fill_field_registry
   end type HistoryConfig
contains
   function get_enabled(this) result(enabled)
      type(StringVector) :: enabled
      class(HistoryConfig), intent(in) :: this

      enabled = this%enabled
   end function get_enabled

   subroutine set_enabled(this, enabled)
      class(HistoryConfig), intent(inout) :: this
      type(StringVector),   intent(in   ) :: enabled

      character(:), allocatable  :: group_name
      type(StringVectorIterator) :: iter

      iter = enabled%begin()
      do while(iter /= enabled%end())
         group_name = iter%get()

         call this%enabled%push_back(group_name)
         call iter%next()
      end do
   end subroutine set_enabled

   function get_fields(this) result(fields)
      class(FieldRegistry), allocatable :: fields
      class(HistoryConfig), intent(in) :: this

      fields = this%fields
   end function get_fields

   function get_groups(this) result(groups)
      class(GroupRegistry), allocatable :: groups
      class(HistoryConfig), intent(in) :: this

      groups = this%groups
   end function get_groups

   function get_collections(this) result(collections)
      class(CollectionRegistry), allocatable :: collections
      class(HistoryConfig), intent(in) :: this

      collections = this%collections
   end function get_collections

   subroutine set_collections(this, collections, unusable, rc)
      class(HistoryConfig),             intent(inout) :: this
      class(CollectionRegistry),        intent(in   ) :: collections
      class(KeywordEnforcer), optional, intent(  out) :: unusable
      integer,                optional, intent(  out) :: rc

      integer :: status

      _UNUSED_DUMMY(unusable)

      status = 0
      if (allocated(this%collections)) then
         status = 1
      else
         this%collections = collections
      end if

      if (present(rc)) rc = status
   end subroutine set_collections

   subroutine import_yaml(this, config, unusable, rc)
      class(HistoryConfig),             intent(inout) :: this
      type(Configuration),              intent(inout) :: config
      class(KeywordEnforcer), optional, intent(  out) :: unusable
      integer,                optional, intent(  out) :: rc

      character(:), pointer       :: key
      type(ConfigurationIterator) :: iter
      type(Configuration)         :: sub_config

      integer :: status

      _UNUSED_DUMMY(unusable)

      if (.not. allocated(this%groups))      allocate(this%groups)
      if (.not. allocated(this%collections)) allocate(this%collections)

      iter = config%begin()
      do while(iter /= config%end())
         key => iter%key()

         select case (key)
         case (enabled_key)
            sub_config = iter%value()
            call this%import_enabled(sub_config, __RC__)
         case (groups_key)
            sub_config = iter%value()
            call this%groups%import_groups(sub_config, __RC__)
         case (collections_key)
            sub_config = iter%value()
            call this%collections%import_collections(sub_config, __RC__)
         end select

         call iter%next()
      end do

      _RETURN(_SUCCESS)
   end subroutine import_yaml

   subroutine import_enabled(this, config, unusable, rc)
      class(HistoryConfig),             intent(inout) :: this
      type(Configuration),              intent(inout) :: config
      class(KeywordEnforcer), optional, intent(  out) :: unusable
      integer,                optional, intent(  out) :: rc

      character(:), allocatable   :: collection_name
      type(ConfigurationIterator) :: iter

      integer :: status

      _UNUSED_DUMMY(unusable)

      if (config%is_sequence()) then
         iter = config%begin()
         do while(iter /= config%end())
            collection_name = iter%get()
            call this%enabled%push_back(collection_name)

            call iter%next()
         end do

         status = 0
      else
         status = 1
      end if

      _RETURN(status)
   end subroutine import_enabled

   subroutine finish_import(this, unusable, rc)
      class(HistoryConfig),             intent(inout) :: this
      class(KeywordEnforcer), optional, intent(  out) :: unusable
      integer,                optional, intent(  out) :: rc

      integer :: status

      _UNUSED_DUMMY(unusable)

      call this%fill_collection_groups(__RC__)
      call this%fill_field_registry(__RC__)

      _RETURN(_SUCCESS)
   end subroutine finish_import

   subroutine fill_collection_groups(this, unusable, rc)
      class(HistoryConfig),             intent(inout) :: this
      class(KeywordEnforcer), optional, intent(  out) :: unusable
      integer,                optional, intent(  out) :: rc

      character(:), pointer      :: collection_name
      type(StringVectorIterator) :: iter
      class(Collection), pointer :: collection_entry

      integer :: status

      _UNUSED_DUMMY(unusable)

      if (.not. allocated(this%groups)) allocate(this%groups)

      status = 0
      iter = this%enabled%begin()
      do while(iter /= this%enabled%end())
         collection_name => iter%get()

         if (this%collections%count(collection_name) > 0) then
            collection_entry => this%collections%at(collection_name)

            call collection_entry%fill_groups(this%groups, __RC__)
         else
            status = 1
            exit
         end if

         call iter%next()
      end do

      if (present(rc)) rc = status
   end subroutine fill_collection_groups

   subroutine fill_field_registry(this, unusable, rc)
      class(HistoryConfig),             intent(inout) :: this
      class(KeywordEnforcer), optional, intent(  out) :: unusable
      integer,                optional, intent(  out) :: rc

      character(:), pointer      :: collection_name
      type(StringVectorIterator) :: iter
      class(Collection), pointer :: collection_entry

      integer :: status

      _UNUSED_DUMMY(unusable)

      if (.not. allocated(this%fields)) allocate(this%fields)

      status = 0
      iter = this%enabled%begin()
      do while(iter /= this%enabled%end())
         collection_name => iter%get()

         if (this%collections%count(collection_name) > 0) then
            collection_entry => this%collections%at(collection_name)

            call collection_entry%register(this%fields)
         else
            status = 1
            exit
         end if

         call iter%next()
      end do

      if (present(rc)) rc = status
   end subroutine fill_field_registry
end module HistoryConfigMod
