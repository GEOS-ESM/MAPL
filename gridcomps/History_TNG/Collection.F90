#include "MAPL_Generic.h"
#include "NUOPC_ErrLog.h"

module CollectionMod
   use, intrinsic :: iso_fortran_env, only: INT64
   use ESMF
   use NUOPC
   use yaFyaml
   use gFTL_StringVector
   use MAPL_ExceptionHandling
   use MAPL_KeywordEnforcerMod

   use FieldRegistryMod
   use GroupMod
   use GroupRegistryMod
   use TemplateMod
   use FrequencyMod

   implicit none
   private

   public Collection

   character(*), parameter :: template_key  = 'template'
   character(*), parameter :: frequency_key = 'frequency'
   character(*), parameter :: groups_key    = 'groups'

   type :: Collection
      private
      character(:), allocatable :: name

      type(Template)  :: template
      type(Frequency) :: frequency

      type(StringVector) :: groups

      class(Group), allocatable :: fields
   contains
      procedure :: get_name
      procedure :: set_name
      procedure :: get_template
      procedure :: get_frequency
      procedure :: get_groups
      procedure :: set_groups
      procedure :: get_fields
      procedure :: set_fields

      procedure :: advertise
      procedure :: realize
      procedure :: register

      procedure :: import_collection
      procedure :: import_groups

      procedure :: fill_groups
   end type Collection
contains
   function get_name(this) result(collection_name)
      character(:), allocatable :: collection_name
      class(Collection), intent(in) :: this

      collection_name = this%name
   end function get_name

   subroutine set_name(this, name, unusable, rc)
      class(Collection),                intent(inout) :: this
      character(*),                     intent(in   ) :: name
      class(KeywordEnforcer), optional, intent(in   ) :: unusable
      integer,                optional, intent(  out) :: rc

      integer :: status

      _UNUSED_DUMMY(unusable)

      status = 0
      if (allocated(this%name)) then
         status = 1
      else
         this%name = name
      end if

      if (present(rc)) rc = status
   end subroutine set_name

   function get_template(this) result(tmplt)
      type(Template) :: tmplt
      class(Collection), intent(in) :: this

      tmplt = this%template
   end function get_template

   function get_frequency(this) result(freq)
      type(Frequency) :: freq
      class(Collection), intent(in) :: this

      freq = this%frequency
   end function get_frequency

   function get_groups(this) result(gps)
      type(StringVector) :: gps
      class(Collection), intent(in) :: this

      gps = this%groups
   end function get_groups

   subroutine set_groups(this, groups)
      class(Collection),  intent(inout) :: this
      type(StringVector), intent(in   ) :: groups

      character(:), allocatable  :: group_name
      type(StringVectorIterator) :: iter

      iter = groups%begin()
      do while(iter /= groups%end())
         group_name = iter%get()

         call this%groups%push_back(group_name)
         call iter%next()
      end do
   end subroutine set_groups

   function get_fields(this) result(fields)
      class(Group), allocatable :: fields
      class(Collection), intent(in) :: this

      fields = this%fields
   end function get_fields

   subroutine set_fields(this, fields, unusable, rc)
      class(Collection),                intent(inout) :: this
      class(Group),                     intent(in   ) :: fields
      class(KeywordEnforcer), optional, intent(in   ) :: unusable
      integer,                optional, intent(  out) :: rc

      integer :: status

      _UNUSED_DUMMY(unusable)

      status = 0
      if (allocated(this%fields)) then
         status = 1
      else
         this%fields = fields
      end if

      if(present(rc)) rc = status
   end subroutine set_fields

   subroutine advertise(this, state, unusable, TransferOfferGeomObject, rc)
      class(Collection),                intent(inout) :: this
      type(ESMF_State),                 intent(inout) :: state
      class(KeywordEnforcer), optional, intent(in   ) :: unusable
      character(len=*),       optional, intent(in   ) :: TransferOfferGeomObject
      integer,                optional, intent(  out) :: rc

      integer :: status

      _UNUSED_DUMMY(unusable)

      call this%fields%advertise(state,TransferOfferGeomObject=TransferOfferGeomObject, __RC__)

      _RETURN(_SUCCESS)
   end subroutine advertise

   subroutine realize(this, state, unusable, rc)
      class(Collection),                intent(inout) :: this
      type(ESMF_State),                 intent(inout) :: state
      class(KeywordEnforcer), optional, intent(in   ) :: unusable
      integer,                optional, intent(  out) :: rc

      integer :: status

      _UNUSED_DUMMY(unusable)

      call this%fields%realize(state, __RC__)

      _RETURN(_SUCCESS)
   end subroutine realize

   subroutine register(this, field_registry)
      class(Collection),   intent(inout) :: this
      type(FieldRegistry), intent(inout) :: field_registry

      call this%fields%register(field_registry)
   end subroutine register

   subroutine import_collection(this, name, config, unusable, rc)
      class(Collection),                intent(inout) :: this
      character(*),                     intent(in   ) :: name
      type(Configuration),              intent(inout) :: config
      class(KeywordEnforcer), optional, intent(in   ) :: unusable
      integer,                optional, intent(  out) :: rc

      character(:), allocatable   :: config_value
      character(:), pointer       :: key
      type(ConfigurationIterator) :: iter
      type(Configuration)         :: sub_config

      integer :: status

      _UNUSED_DUMMY(unusable)

      this%name = name

      if (.not. allocated(this%fields)) allocate(this%fields)

      ! Import the fields
      call this%fields%import_group(config, __RC__)

      ! Import everything else
      iter = config%begin()
      do while(iter /= config%end())
         key => iter%key()

         select case (key)
         case (template_key)
            config_value = iter%value()
            call this%template%initialize(config_value)
         case (frequency_key)
            config_value = iter%value()
            call this%frequency%initialize(config_value)
         case (groups_key)
            sub_config = iter%value()
            call this%import_groups(sub_config)
         end select

         call iter%next()
      end do

      _RETURN(_SUCCESS)
   end subroutine import_collection

   subroutine import_groups(this, config, unusable, rc)
      class(Collection),                intent(inout) :: this
      type(Configuration),              intent(inout) :: config
      class(KeywordEnforcer), optional, intent(in   ) :: unusable
      integer,                optional, intent(  out) :: rc

      character(:), allocatable   :: group_name
      type(ConfigurationIterator) :: iter

      integer :: status

      _UNUSED_DUMMY(unusable)

      if (config%is_sequence()) then
         iter = config%begin()
         do while(iter /= config%end())
            group_name = iter%get()
            call this%groups%push_back(group_name)

            call iter%next()
         end do

         status = 0
      else
         status = 1
      end if

      if (present(rc)) rc = status
   end subroutine import_groups

   subroutine fill_groups(this, group_registry, unusable, rc)
      class(Collection),                intent(inout) :: this
      type(GroupRegistry),              intent(in   ) :: group_registry
      class(KeywordEnforcer), optional, intent(in   ) :: unusable
      integer,                optional, intent(  out) :: rc

      character(:), pointer      :: group_name
      class(Group), pointer      :: group_entry
      type(StringVectorIterator) :: iter

      integer :: status

      _UNUSED_DUMMY(unusable)

      status = 0
      iter = this%groups%begin()
      do while(iter /= this%groups%end())
         group_name => iter%get()

         if (group_registry%count(group_name) > 0) then
            group_entry => group_registry%at(group_name)

            call this%fields%union(group_entry, __RC__)
         else
            status = 1
            exit
         end if
         call iter%next()
      end do

      if (present(rc)) rc = status
   end subroutine fill_groups
end module CollectionMod
