#include "MAPL_Generic.h"
#include "NUOPC_ErrLog.h"

module FieldGroupMod
   use, intrinsic :: iso_fortran_env, only: INT64
   use ESMF
   use NUOPC
   use yaFyaml
   use MAPL_ExceptionHandling
   use MAPL_KeywordEnforcerMod

   use FieldGroupEntryMod
   use FieldGroupEntryMapMod

   use FieldEntryMod
   use FieldRegistryMod

   implicit none
   private

   public FieldGroup

   character(*), parameter :: alias_key = 'alias'
   character(*), parameter :: units_key = 'units'

   type :: FieldGroup
      private
      type(FieldGroupEntryMap) :: map
   contains
      procedure :: size
      procedure :: count
      procedure :: at
      procedure :: insert
      procedure :: erase

      procedure :: union
      procedure :: set_difference

      procedure :: advertise
      procedure :: realize
      procedure :: register

      procedure :: import_group
      procedure :: import_component
      procedure :: import_field
   end type FieldGroup
contains
   integer(kind=INT64) function size(this)
      class(FieldGroup), intent(in) :: this

      size = this%map%size()
   end function size

   integer(kind=INT64) function count(this, key)
      class(FieldGroup), intent(in) :: this
      character(*),      intent(in) :: key

      count = this%map%count(key)
   end function count

   function at(this, key) result(field_entry)
      class(FieldGroupEntry), pointer :: field_entry
      class(FieldGroup), intent(in) :: this
      character(*),      intent(in) :: key

      field_entry => this%map%at(key)
   end function at

   subroutine insert(this, field_entry, unusable, rc)
      class(FieldGroup),                intent(inout) :: this
      class(FieldGroupEntry),           intent(inout) :: field_entry
      class(KeywordEnforcer), optional, intent(  out) :: unusable
      integer,                optional, intent(  out) :: rc

      character(:), allocatable       :: standard_name
      class(FieldGroupEntry), pointer :: current_entry
      integer                         :: status

      _UNUSED_DUMMY(unusable)

      standard_name = field_entry%standard_name()

      status = 0
      if (this%count(standard_name) > 0) then
         current_entry => this%at(standard_name)
         if (current_entry /= field_entry) status = 1
      else
         call this%map%insert(standard_name, field_entry)
      end if

      if (present(rc)) rc = status
   end subroutine insert

   subroutine erase(this, field_entry, unusable, rc)
      class(FieldGroup),                intent(inout) :: this
      class(FieldGroupEntry),           intent(inout) :: field_entry
      class(KeywordEnforcer), optional, intent(  out) :: unusable
      integer,                optional, intent(  out) :: rc

      character(:), allocatable        :: standard_name
      class(FieldGroupEntry), pointer  :: current_entry
      type(FieldGroupEntryMapIterator) :: iter
      integer                          :: status

      _UNUSED_DUMMY(unusable)

      standard_name = field_entry%standard_name()

      status = 0
      if (this%count(standard_name) > 0) then
         current_entry => this%at(standard_name)
         if (current_entry /= field_entry) then
            status = 1
         else
            iter = this%map%find(standard_name)
            call this%map%erase(iter)
         end if
      else
         status = 2
      end if

      if (present(rc)) rc = status
   end subroutine erase

   subroutine advertise(this, state, unusable, TransferOfferGeomObject, rc)
      class(FieldGroup),                intent(inout) :: this
      type(ESMF_State),                 intent(inout) :: state
      class(KeywordEnforcer), optional, intent(in   ) :: unusable
      character(len=*),       optional, intent(in   ) :: TransferOfferGeomObject
      integer,                optional, intent(  out) :: rc

      class(FieldGroupEntry), pointer  :: field_entry
      type(FieldGroupEntryMapIterator) :: iter

      integer :: status

      _UNUSED_DUMMY(unusable)

      iter = this%map%begin()
      do while(iter /= this%map%end())
         field_entry => iter%value()
         call field_entry%advertise(state, TransferOfferGeomObject=TransferOfferGeomObject, __RC__)

         call iter%next()
      end do

      _RETURN(_SUCCESS)
   end subroutine advertise

   subroutine realize(this, state, unusable, rc)
      class(FieldGroup),                intent(inout) :: this
      type(ESMF_State),                 intent(inout) :: state
      class(KeywordEnforcer), optional, intent(in   ) :: unusable
      integer,                optional, intent(  out) :: rc

      class(FieldGroupEntry), pointer  :: field_entry
      type(FieldGroupEntryMapIterator) :: iter

      integer :: status

      _UNUSED_DUMMY(unusable)

      iter = this%map%begin()
      do while(iter /= this%map%end())
         field_entry => iter%value()
         call field_entry%realize(state, __RC__)

         call iter%next()
      end do

      _RETURN(_SUCCESS)
   end subroutine realize

   subroutine register(this, field_registry)
      class(FieldGroup),   intent(inout) :: this
      type(FieldRegistry), intent(inout) :: field_registry

      class(FieldGroupEntry), pointer  :: field_entry
      class(FieldEntry), allocatable   :: registry_entry
      type(FieldGroupEntryMapIterator) :: iter

      iter = this%map%begin()
      do while(iter /= this%map%end())
         field_entry    => iter%value()
         registry_entry =  field_entry%get_field_entry()
         call field_registry%insert(registry_entry)

         call iter%next()
      end do
   end subroutine register

   subroutine union(this, field_group, unusable, rc)
      class(FieldGroup),                intent(inout) :: this
      class(FieldGroup),                intent(inout) :: field_group
      class(KeywordEnforcer), optional, intent(  out) :: unusable
      integer,                optional, intent(  out) :: rc

      type(FieldGroupEntry), pointer   :: field_entry
      type(FieldGroupEntryMapIterator) :: iter

      integer :: status

      _UNUSED_DUMMY(unusable)

      iter = field_group%map%begin()
      do while(iter /= field_group%map%end())
         field_entry => iter%value()
         call this%insert(field_entry, __RC__)

         call iter%next()
      end do

      _RETURN(_SUCCESS)
   end subroutine union

   subroutine set_difference(this, field_group, unusable, rc)
      class(FieldGroup),                intent(inout) :: this
      class(FieldGroup),                intent(inout) :: field_group
      class(KeywordEnforcer), optional, intent(  out) :: unusable
      integer,                optional, intent(  out) :: rc

      character(:),          pointer   :: standard_name
      type(FieldGroupEntry), pointer   :: field_entry
      type(FieldGroupEntryMapIterator) :: iter

      integer :: status

      _UNUSED_DUMMY(unusable)

      iter = field_group%map%begin()
      do while(iter /= field_group%map%end())
         standard_name => iter%key()
         field_entry   => iter%value()

         if (this%count(standard_name) > 0) then
            call this%erase(field_entry, __RC__)
         end if

         call iter%next()
      end do

      _RETURN(_SUCCESS)
   end subroutine set_difference

   subroutine import_group(this, config, unusable, rc)
      class(FieldGroup),                intent(inout) :: this
      type(Configuration),              intent(inout) :: config
      class(KeywordEnforcer), optional, intent(  out) :: unusable
      integer,                optional, intent(  out) :: rc

      character(:), pointer       :: component_name
      type(ConfigurationIterator) :: iter
      type(Configuration)         :: sub_config

      integer :: status

      _UNUSED_DUMMY(unusable)

      iter = config%begin()
      do while(iter /= config%end())
         component_name => iter%key()
         sub_config     =  iter%value()

         call this%import_component(component_name, sub_config, __RC__)
         call iter%next()
      end do

      _RETURN(_SUCCESS)
   end subroutine import_group

   subroutine import_component(this, component_name, config, unusable, rc)
      class(FieldGroup),                intent(inout) :: this
      character(*),                     intent(in   ) :: component_name
      type(Configuration),              intent(inout) :: config
      class(KeywordEnforcer), optional, intent(  out) :: unusable
      integer,                optional, intent(  out) :: rc

      character(:), pointer       :: short_name
      type(ConfigurationIterator) :: iter
      type(Configuration)         :: sub_config

      integer :: status

      _UNUSED_DUMMY(unusable)

      iter = config%begin()
      do while(iter /= config%end())
         short_name => iter%key()
         sub_config =  iter%value()

         call this%import_field(short_name, component_name, sub_config, __RC__)
         call iter%next()
      end do

      _RETURN(_SUCCESS)
   end subroutine import_component

   subroutine import_field(this, short_name, component_name, config, unusable, rc)
      class(FieldGroup),                intent(inout) :: this
      character(*),                     intent(in   ) :: short_name
      character(*),                     intent(in   ) :: component_name
      type(Configuration),              intent(inout) :: config
      class(KeywordEnforcer), optional, intent(  out) :: unusable
      integer,                optional, intent(  out) :: rc

      character(:), allocatable  :: alias
      character(:), allocatable  :: units
      type(FieldGroupEntry) :: field_entry

      character(:), pointer       :: key
      type(ConfigurationIterator) :: iter
      type(Configuration)         :: sub_config

      integer :: status

      _UNUSED_DUMMY(unusable)

      call field_entry%initialize(short_name, component_name)

      iter =  config%begin()
      do while(iter /= config%end())
         key => iter%key()

         select case (key)
         case (alias_key)
            alias = iter%value()
            call field_entry%set_alias_name(alias, __RC__)
         case (units_key)
            units = iter%value()
            call field_entry%set_units(units, __RC__)
         end select

         call iter%next()
      end do

      call this%insert(field_entry)
      _RETURN(_SUCCESS)
   end subroutine import_field
end module FieldGroupMod
