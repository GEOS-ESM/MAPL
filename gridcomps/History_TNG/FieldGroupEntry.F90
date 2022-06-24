#include "MAPL_Generic.h"
#include "NUOPC_ErrLog.h"

module FieldGroupEntryMod
   use ESMF
   use NUOPC
   use MAPL_ExceptionHandling
   use MAPL_KeywordEnforcerMod

   use FieldEntryMod

   implicit none
   private

   public FieldGroupEntry
   public default_alias

   character(*), parameter :: default_alias = "no_alias"

   type :: FieldGroupEntry
      private
      class(FieldEntry), allocatable :: field_entry

      character(:), allocatable :: alias_name
   contains
      procedure :: initialize
      procedure :: get_field_entry
      procedure :: get_alias_name
      procedure :: set_alias_name
      procedure :: set_units

      procedure :: equal_to_entry
      generic   :: operator(==) => equal_to_entry
      procedure :: not_equal_to_entry
      generic   :: operator(/=) => not_equal_to_entry

      procedure :: standard_name
      procedure :: advertise
      procedure :: realize
   end type FieldGroupEntry
contains
   subroutine initialize(this, short_name, component_name, unusable, units, alias_name)
      class(FieldGroupEntry),           intent(  out) :: this
      character(*),                     intent(in   ) :: short_name
      character(*),                     intent(in   ) :: component_name
      class(KeywordEnforcer), optional, intent(in   ) :: unusable
      character(*),           optional, intent(in   ) :: units
      character(*),           optional, intent(in   ) :: alias_name

      _UNUSED_DUMMY(unusable)

      if (.not. allocated(this%field_entry)) allocate(this%field_entry)
      call this%field_entry%initialize(short_name, component_name, units=units)

      if (present(alias_name)) then
         this%alias_name = alias_name
      else
         this%alias_name = default_alias
      end if
   end subroutine initialize

   function get_field_entry(this) result(field_entry)
      class(FieldEntry), allocatable :: field_entry
      class(FieldGroupEntry), intent(in) :: this

      field_entry = this%field_entry
   end function get_field_entry

   function get_alias_name(this) result(alias)
      character(:), allocatable :: alias
      class(FieldGroupEntry), intent(in) :: this

      alias = this%alias_name
   end function get_alias_name

   subroutine set_alias_name(this, alias_name, unusable, rc)
      class(FieldGroupEntry),           intent(inout) :: this
      character(*),                     intent(in   ) :: alias_name
      class(KeywordEnforcer), optional, intent(  out) :: unusable
      integer,                optional, intent(  out) :: rc

      integer :: status

      _UNUSED_DUMMY(unusable)

      status = 0

      if (this%alias_name == default_alias) then
         this%alias_name = alias_name
      else
         status = 1
      end if

      if (present(rc)) rc = status
   end subroutine set_alias_name

   subroutine set_units(this, units, unusable, rc)
      class(FieldGroupEntry),           intent(inout) :: this
      character(*),                     intent(in   ) :: units
      class(KeywordEnforcer), optional, intent(  out) :: unusable
      integer,                optional, intent(  out) :: rc

      integer :: status

      _UNUSED_DUMMY(unusable)

      call this%field_entry%set_units(units, __RC__)

      _RETURN(_SUCCESS)
   end subroutine set_units

   logical function equal_to_entry(a, b)
      class(FieldGroupEntry), intent(in) :: a
      class(FieldGroupEntry), intent(in) :: b

      logical :: equiv

      equiv = same_type_as(a, b)

      if (a%field_entry /= b%field_entry)     equiv = .false.
      if (a%alias_name /= b%get_alias_name()) equiv = .false.

      equal_to_entry = equiv
   end function equal_to_entry

   logical function not_equal_to_entry(a, b)
      class(FieldGroupEntry), intent(in) :: a
      class(FieldGroupEntry), intent(in) :: b

      not_equal_to_entry = .not. (a == b)
   end function not_equal_to_entry

   function standard_name(this) result(std_name)
      character(:), allocatable :: std_name
      class(FieldGroupEntry), intent(inout) :: this

      std_name = this%field_entry%standard_name()
   end function standard_name

   subroutine advertise(this, state, unusable,  TransferOfferGeomObject, rc)
      class(FieldGroupEntry),           intent(inout) :: this
      type(ESMF_State),                 intent(inout) :: state
      class(KeywordEnforcer), optional, intent(in   ) :: unusable
      character(len=*),       optional, intent(in   ) :: TransferOfferGeomObject
      integer,                optional, intent(  out) :: rc

      integer :: status

      _UNUSED_DUMMY(unusable)

      call this%field_entry%advertise(state,TransferOfferGeomObject=TransferOfferGeomObject, __RC__)

      _RETURN(_SUCCESS)
   end subroutine advertise

   subroutine realize(this, state, unusable, rc)
      class(FieldGroupEntry),           intent(inout) :: this
      type(ESMF_State),                 intent(inout) :: state
      class(KeywordEnforcer), optional, intent(in   ) :: unusable
      integer,                optional, intent(  out) :: rc

      integer :: status

      _UNUSED_DUMMY(unusable)

      call this%field_entry%realize(state, state, __RC__)

      _RETURN(_SUCCESS)
   end subroutine realize
end module FieldGroupEntryMod
