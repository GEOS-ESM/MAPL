#include "MAPL_Generic.h"
#include "NUOPC_ErrLog.h"

module GroupMod
   use, intrinsic :: iso_fortran_env, only: INT64
   use ESMF
   use NUOPC
   use yaFyaml
   use MAPL_ExceptionHandling
   use MAPL_KeywordEnforcerMod

   use FieldRegistryMod
   use FieldGroupMod

   implicit none
   private

   public Group

   character(*), parameter :: fields_key     = 'fields'
   character(*), parameter :: aux_fields_key = 'aux'

   type :: Group
      private
      class(FieldGroup), allocatable :: fields
      class(FieldGroup), allocatable :: aux_fields

      ! TODO: later will add regex and exper to this
   contains
      procedure :: initialize
      procedure :: get_fields
      procedure :: get_aux_fields

      procedure :: union

      procedure :: advertise
      procedure :: realize
      procedure :: register

      procedure :: import_group
   end type Group
contains
   subroutine initialize(this, fields, aux_fields)
      class(Group),      intent(inout) :: this
      class(FieldGroup), intent(in   ) :: fields
      class(FieldGroup), intent(in   ) :: aux_fields

      this%fields = fields
      this%aux_fields    = aux_fields
   end subroutine initialize

   function get_fields(this) result(fields)
      class(FieldGroup), allocatable :: fields
      class(Group), intent(in) :: this

      fields = this%fields
   end function get_fields

   function get_aux_fields(this) result(aux_fields)
      class(FieldGroup), allocatable :: aux_fields
      class(Group), intent(in) :: this

      aux_fields = this%aux_fields
   end function get_aux_fields

   subroutine union(this, other, unusable, rc)
      class(Group),                     intent(inout) :: this
      class(Group),                     intent(inout) :: other
      class(KeywordEnforcer), optional, intent(  out) :: unusable
      integer,                optional, intent(  out) :: rc

      class(FieldGroup), allocatable :: fields
      class(FieldGroup), allocatable :: aux_fields

      integer :: status

      _UNUSED_DUMMY(unusable)

      fields     = other%get_fields()
      aux_fields = other%get_aux_fields()

      call this%fields%union(    fields,     __RC__)
      call this%aux_fields%union(aux_fields, __RC__)

      call this%aux_fields%set_difference(this%fields, __RC__)

      _RETURN(_SUCCESS)
   end subroutine union

   subroutine advertise(this, state, unusable, TransferOfferGeomObject, rc)
      class(Group),                      intent(inout) :: this
      type(ESMF_State),                 intent(inout) :: state
      class(KeywordEnforcer), optional, intent(in   ) :: unusable
      character(len=*),       optional, intent(in  ) :: TransferOfferGeomObject
      integer,                optional, intent(  out) :: rc

      integer :: status

      _UNUSED_DUMMY(unusable)

      call this%    fields%advertise(state, TransferOfferGeomObject=TransferOfferGeomObject,  __RC__)
      call this%aux_fields%advertise(state, TransferOfferGeomObject=TransferOfferGeomObject, __RC__)

      _RETURN(_SUCCESS)
   end subroutine advertise

   subroutine realize(this, state, unusable, rc)
      class(Group),                      intent(inout) :: this
      type(ESMF_State),                 intent(inout) :: state
      class(KeywordEnforcer), optional, intent(in   ) :: unusable
      integer,                optional, intent(  out) :: rc

      integer :: status

      _UNUSED_DUMMY(unusable)

      call this%    fields%realize(state, __RC__)
      call this%aux_fields%realize(state, __RC__)

      _RETURN(_SUCCESS)
   end subroutine realize

   subroutine register(this, field_registry)
      class(Group),        intent(inout) :: this
      type(FieldRegistry), intent(inout) :: field_registry

      call this%fields%    register(field_registry)
      call this%aux_fields%register(field_registry)
   end subroutine register

   subroutine import_group(this, config, unusable, rc)
      class(Group),                     intent(inout) :: this
      type(Configuration),              intent(inout) :: config
      class(KeywordEnforcer), optional, intent(  out) :: unusable
      integer,                optional, intent(  out) :: rc

      character(:), pointer       :: key
      type(ConfigurationIterator) :: iter
      type(Configuration)         :: sub_config

      integer :: status

      _UNUSED_DUMMY(unusable)

      if (.not. allocated(this%fields))     allocate(this%fields)
      if (.not. allocated(this%aux_fields)) allocate(this%aux_fields)

      iter = config%begin()
      do while(iter /= config%end())
         key        => iter%key()
         sub_config =  iter%value()

         select case (key)
         case (fields_key)
            call this%fields%    import_group(sub_config, __RC__)
         case (aux_fields_key)
            call this%aux_fields%import_group(sub_config, __RC__)
         end select

         call iter%next()
      end do

      call this%aux_fields%set_difference(this%fields, __RC__)

      _RETURN(_SUCCESS)
   end subroutine import_group
end module GroupMod

