#include "MAPL_Generic.h"
#include "NUOPC_ErrLog.h"

module NUOPC_MAPLfields_Mod
   use ESMF
   use NUOPC
   use MAPL_Mod

   implicit none
   private

   public FieldAttributes

   type :: FieldAttributes
      type(ESMF_Field)          :: field
      character(:), allocatable :: name
      character(:), allocatable :: long_name
      character(:), allocatable :: units
      character(:), allocatable :: NUOPCname
      character(:), allocatable :: TransferOfferGeomObject
      character(:), allocatable :: SharePolicyField
      character(:), allocatable :: SharePolicyGeomObject
   contains
      procedure :: has_field_dictionary_entry
      procedure :: add_field_dictionary_entry
      procedure :: add_to_field_dictionary
      procedure :: advertise
      procedure :: advertise_to_state
      procedure :: realize
      procedure :: validate
      procedure :: realize_to_import_state
      procedure :: allocate_coupling
      procedure :: realize_to_export_state

      procedure :: get_field_from_state
      procedure :: get_name_from_field
      procedure :: get_long_name_from_field
      procedure :: get_units_from_field
      procedure :: get_NUOPCname_from_field
      procedure :: get_TransferOfferGeomObject_from_field
      procedure :: get_SharePolicyField_from_field
      procedure :: get_SharePolicyGeomObject_from_field
      procedure :: fill_from_state
   end type FieldAttributes
contains
   logical function has_field_dictionary_entry(this, rc)
      class(FieldAttributes), intent(inout) :: this
      integer, optional,      intent(  out) :: rc

      integer :: status

      has_field_dictionary_entry = &
            NUOPC_FieldDictionaryHasEntry(trim(this%name), rc=status)
      VERIFY_NUOPC_(status)

      _RETURN(_SUCCESS)
   end function has_field_dictionary_entry

   subroutine add_field_dictionary_entry(this, rc)
      class(FieldAttributes), intent(inout) :: this
      integer, optional,      intent(  out) :: rc

      integer :: status

      call NUOPC_FieldDictionaryAddEntry(standardName=trim(this%name), &
            canonicalUnits=trim(this%units), rc=status)
      VERIFY_NUOPC_(status)

      _RETURN(_SUCCESS)
   end subroutine add_field_dictionary_entry

   subroutine add_to_field_dictionary(this, rc)
      class(FieldAttributes), intent(inout) :: this
      integer, optional,      intent(  out) :: rc

      integer :: status
      logical :: check_entry

      check_entry = this%has_field_dictionary_entry(__RC__)

      if (.not. check_entry) then
         call this%add_field_dictionary_entry(__RC__)
      end if

      _RETURN(_SUCCESS)
   end subroutine add_to_field_dictionary

   subroutine advertise(this, state, rc)
      class(FieldAttributes), intent(inout) :: this
      type(ESMF_State),       intent(inout) :: state
      integer, optional,      intent(  out) :: rc

      integer :: status

      !! NOTE: Should this be NUOPCname or the MAPL name? !!

      call NUOPC_Advertise(state, standardName=trim(this%name), &
            TransferOfferGeomObject=this%TransferOfferGeomObject, &
            SharePolicyField=this%SharePolicyField, &
            SharePolicyGeomObject=this%SharePolicyGeomObject, &
            rc=status)
      VERIFY_NUOPC_(status)

      _RETURN(_SUCCESS)
   end subroutine advertise

   subroutine advertise_to_state(this, state, rc)
      class(FieldAttributes), intent(inout) :: this
      type(ESMF_State),       intent(inout) :: state
      integer, optional,      intent(  out) :: rc

      integer :: status

      call this%add_to_field_dictionary(__RC__)

      call this%advertise(state, __RC__)

      _RETURN(_SUCCESS)
   end subroutine advertise_to_state

   subroutine realize(this, state, rc)
      class(FieldAttributes), intent(inout) :: this
      type(ESMF_State),       intent(inout) :: state
      integer, optional,      intent(  out) :: rc

      integer :: status

      call NUOPC_Realize(state, field=this%field, rc=status)
      VERIFY_NUOPC_(status)

      _RETURN(_SUCCESS)
   end subroutine realize

   subroutine validate(this, rc)
      class(FieldAttributes), intent(inout) :: this
      integer, optional,      intent(  out) :: rc

      integer :: status

      call ESMF_FieldValidate(this%field, __RC__)

      _RETURN(_SUCCESS)
   end subroutine validate

   subroutine realize_to_import_state(this, state, rc)
      class(FieldAttributes), intent(inout) :: this
      type(ESMF_State),       intent(inout) :: state
      integer, optional,      intent(  out) :: rc

      integer :: status

      call this%validate(__RC__)
      call this%realize(state, __RC__)

      _RETURN(_SUCCESS)
   end subroutine realize_to_import_state

   subroutine allocate_coupling(this, rc)
      class(FieldAttributes), intent(inout) :: this
      integer, optional,      intent(  out) :: rc

      integer :: status

      call MAPL_AllocateCoupling(this%field, __RC__)

      _RETURN(_SUCCESS)
   end subroutine allocate_coupling

   subroutine realize_to_export_state(this, state, rc)
      class(FieldAttributes), intent(inout) :: this
      type(ESMF_State),       intent(inout) :: state
      integer,optional,       intent(  out) :: rc

      integer :: status

      call this%allocate_coupling(__RC__)

      call this%realize(state, __RC__)

      _RETURN(_SUCCESS)
   end subroutine realize_to_export_state

   subroutine get_field_from_state(this, state, name, rc)
      class(FieldAttributes), intent(inout) :: this
      type(ESMF_State),       intent(inout) :: state
      character(*),           intent(in   ) :: name
      integer,optional,       intent(  out) :: rc

      integer :: status

      call ESMF_StateGet(state, name, this%field, __RC__)

      call this%validate(__RC__)

      _RETURN(_SUCCESS)
   end subroutine get_field_from_state

   subroutine get_name_from_field(this, rc)
      class(FieldAttributes), intent(inout) :: this
      integer,optional,       intent(  out) :: rc

      character(len=ESMF_MAXSTR) :: name
      integer                    :: status

      call ESMF_FieldGet(this%field, name=name, __RC__)
      this%name = trim(name)

      _RETURN(_SUCCESS)
   end subroutine get_name_from_field

   subroutine get_long_name_from_field(this, rc)
      class(FieldAttributes), intent(inout) :: this
      integer,optional,       intent(  out) :: rc

      character(len=ESMF_MAXSTR) :: long_name
      integer                    :: status

      call ESMF_AttributeGet(this%field, name="LONG_NAME", value=long_name, __RC__)
      this%long_name = trim(long_name)

      _RETURN(_SUCCESS)
   end subroutine get_long_name_from_field

   subroutine get_units_from_field(this, rc)
      class(FieldAttributes), intent(inout) :: this
      integer,optional,       intent(  out) :: rc

      character(len=ESMF_MAXSTR) :: units
      integer                    :: status

      call ESMF_AttributeGet(this%field, name="UNITS", value=units, __RC__)

      if (units == "" .or. units == " ") units = "1"
      this%units = trim(units)

      _RETURN(_SUCCESS)
   end subroutine get_units_from_field

   subroutine get_NUOPCname_from_field(this, rc)
      class(FieldAttributes), intent(inout) :: this
      integer,optional,       intent(  out) :: rc

      character(len=ESMF_MAXSTR) :: NUOPCname
      integer                    :: status

      call ESMF_AttributeGet(this%field, name="NUOPCname", value=NUOPCname, __RC__)
      this%NUOPCname = trim(NUOPCname)

      _RETURN(_SUCCESS)
   end subroutine get_NUOPCname_from_field

   subroutine get_TransferOfferGeomObject_from_field(this, rc)
      class(FieldAttributes), intent(inout) :: this
      integer,optional,       intent(  out) :: rc

      character(len=ESMF_MAXSTR) :: TransferOfferGeomObject
      integer                    :: status

      call ESMF_AttributeGet(this%field, name="TransferOfferGeomObject", value=TransferOfferGeomObject, __RC__)
      this%TransferOfferGeomObject = trim(TransferOfferGeomObject)

      _RETURN(_SUCCESS)
   end subroutine get_TransferOfferGeomObject_from_field

   subroutine get_SharePolicyField_from_field(this, rc)
      class(FieldAttributes), intent(inout) :: this
      integer,optional,       intent(  out) :: rc

      character(len=ESMF_MAXSTR) :: SharePolicyField
      integer                    :: status

      call ESMF_AttributeGet(this%field, name="SharePolicyField", value=SharePolicyField, __RC__)
      this%SharePolicyField = trim(SharePolicyField)

      _RETURN(_SUCCESS)
   end subroutine get_SharePolicyField_from_field

   subroutine get_SharePolicyGeomObject_from_field(this, rc)
      class(FieldAttributes), intent(inout) :: this
      integer,optional,       intent(  out) :: rc

      character(len=ESMF_MAXSTR) :: SharePolicyGeomObject
      integer                    :: status

      call ESMF_AttributeGet(this%field, name="SharePolicyGeomObject", value=SharePolicyGeomObject, __RC__)
      this%SharePolicyGeomObject = trim(SharePolicyGeomObject)

      _RETURN(_SUCCESS)
   end subroutine get_SharePolicyGeomObject_from_field

   subroutine fill_from_state(this, state, name, rc)
      class(FieldAttributes), intent(inout) :: this
      type(ESMF_State),       intent(inout) :: state
      character(*),           intent(in   ) :: name
      integer,optional,       intent(  out) :: rc

      integer :: status

      call this%get_field_from_state(state, name, __RC__)

      call this%get_name_from_field(__RC__)

      call this%get_long_name_from_field(__RC__)

      call this%get_units_from_field(__RC__)

      call this%get_NUOPCname_from_field(__RC__)

      call this%get_TransferOfferGeomObject_from_field(__RC__)

      call this%get_SharePolicyField_from_field(__RC__)

      call this%get_SharePolicyGeomObject_from_field(__RC__)
   end subroutine fill_from_state
end module NUOPC_MAPLfields_Mod
