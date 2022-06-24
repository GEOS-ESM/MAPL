#include "MAPL_Generic.h"
#include "NUOPC_ErrLog.h"

module FieldEntryMod
   use ESMF
   use NUOPC
   use MAPL_BaseMod
   use MAPL_ExceptionHandling
   use MAPL_KeywordEnforcerMod
   use ESMFL_Mod

   implicit none
   private

   public :: FieldEntry
   public :: default_units
   public :: default_TransferOfferGeomObject
   public :: default_SharePolicyField

   character(*), parameter :: default_units = '1'

   character(*), parameter :: default_TransferOfferGeomObject = 'will provide'
   character(*), parameter :: default_SharePolicyField        = 'not share'
   character(*), parameter :: default_SharePolicyGeomObject   = 'not share'

   type :: FieldEntry
      private
      character(:), allocatable :: short_name
      character(:), allocatable :: component_name
      character(:), allocatable :: units

      character(:), allocatable :: TransferOfferGeomObject
      character(:), allocatable :: SharePolicyField
      character(:), allocatable :: SharePolicyGeomObject
   contains
      procedure :: initialize

      procedure :: get_short_name
      procedure :: get_component_name
      procedure :: get_units

      procedure :: set_units

      procedure :: equal_to_entry
      generic   :: operator(==) => equal_to_entry
      procedure :: not_equal_to_entry
      generic   :: operator(/=) => not_equal_to_entry

      procedure :: standard_name

      procedure :: NUOPC_has_entry
      procedure :: NUOPC_add_entry
      procedure :: register

      procedure :: NUOPC_advert
      procedure :: advertise

      procedure :: NUOPC_real_provided
      procedure :: NUOPC_real_accepted
      procedure :: get_field_from_state
      procedure :: realize
   end type FieldEntry
contains
   subroutine initialize(this, short_name, component_name, unusable, &
         units, TransferOfferGeomObject, SharePolicyField, SharePolicyGeomObject)
      class(FieldEntry),                intent(  out) :: this
      character(*),                     intent(in   ) :: short_name
      character(*),                     intent(in   ) :: component_name
      class(KeywordEnforcer), optional, intent(in   ) :: unusable
      character(*),           optional, intent(in   ) :: units
      character(*),           optional, intent(in   ) :: TransferOfferGeomObject
      character(*),           optional, intent(in   ) :: SharePolicyField
      character(*),           optional, intent(in   ) :: SharePolicyGeomObject

      _UNUSED_DUMMY(unusable)

      this%short_name     = short_name
      this%component_name = component_name

      if (present(units)) then
         this%units = units
      else
         this%units = default_units
      end if

      if (present(TransferOfferGeomObject)) then
         this%TransferOfferGeomObject = TransferOfferGeomObject
      else
         this%TransferOfferGeomObject = default_TransferOfferGeomObject
      end if

      if (present(SharePolicyField)) then
         this%SharePolicyField = SharePolicyField
      else
         this%SharePolicyField = default_SharePolicyField
      end if

      if (present(SharePolicyGeomObject)) then
         this%SharePolicyGeomObject = SharePolicyGeomObject
      else
         this%SharePolicyGeomObject = this%SharePolicyField
      end if
   end subroutine initialize

   function get_short_name(this) result(short_name)
      character(:), allocatable :: short_name
      class(FieldEntry), intent(in) :: this

      short_name = this%short_name
   end function get_short_name

   function get_component_name(this) result(component_name)
      character(:), allocatable :: component_name
      class(FieldEntry), intent(in) :: this

      component_name = this%component_name
   end function get_component_name

   function get_units(this) result(units)
      character(:), allocatable :: units
      class(FieldEntry), intent(in) :: this

      units = this%units
   end function get_units

   subroutine set_units(this, units, unusable, rc)
      class(FieldEntry),                intent(inout) :: this
      character(*),                     intent(in   ) :: units
      class(KeywordEnforcer), optional, intent(  out) :: unusable
      integer,                optional, intent(  out) :: rc

      integer :: status

      _UNUSED_DUMMY(unusable)

      status = 0

      if (this%units /= default_units) then
         status = 1
      else
         this%units = units
      end if

      if(present(rc)) rc = status
   end subroutine set_units

   logical function equal_to_entry(this, field_entry)
      class(FieldEntry), intent(in) :: this
      class(FieldEntry), intent(in) :: field_entry

      logical :: equiv

      equiv = same_type_as(this, field_entry)

      if (this%short_name     /= field_entry%get_short_name())     equiv = .false.
      if (this%component_name /= field_entry%get_component_name()) equiv = .false.
      if (this%units          /= field_entry%get_units())          equiv = .false.

      equal_to_entry = equiv
   end function equal_to_entry

   logical function not_equal_to_entry(a, b)
      class(FieldEntry), intent(in) :: a
      class(FieldEntry), intent(in) :: b

      not_equal_to_entry = .not. (a == b)
   end function not_equal_to_entry

   function standard_name(this) result(std_name)
      character(:), allocatable :: std_name
      class(FieldEntry), intent(inout) :: this

      std_name = this%short_name // '.' // this%component_name
   end function standard_name

   subroutine NUOPC_has_entry(this, name, has_entry, unusable, rc)
      class(FieldEntry),        intent(inout) :: this
      character(*),                     intent(in   ) :: name
      logical,                          intent(  out) :: has_entry
      class(KeywordEnforcer), optional, intent(  out) :: unusable
      integer,                optional, intent(  out) :: rc

      integer :: status

      _UNUSED_DUMMY(unusable)

      has_entry = NUOPC_FieldDictionaryHasEntry(name, rc=status)
      !VERIFY_NUOPC_(status)

      _RETURN(_SUCCESS)
   end subroutine NUOPC_has_entry

   subroutine NUOPC_add_entry(this, name, units, unusable, rc)
      class(FieldEntry),                intent(inout) :: this
      character(*),                     intent(in   ) :: name
      character(*),                     intent(in   ) :: units
      class(KeywordEnforcer), optional, intent(  out) :: unusable
      integer,                optional, intent(  out) :: rc

      integer :: status

      _UNUSED_DUMMY(unusable)

      call NUOPC_FieldDictionaryAddEntry(name, units, rc=status)
      VERIFY_NUOPC_(status)

      _RETURN(_SUCCESS)
   end subroutine NUOPC_add_entry

   subroutine register(this, unusable, rc)
      class(FieldEntry),                intent(inout) :: this
      class(KeywordEnforcer), optional, intent(  out) :: unusable
      integer,                optional, intent(  out) :: rc

      logical                   :: has_entry
      character(:), allocatable :: standard_name

      integer :: status

      _UNUSED_DUMMY(unusable)

      standard_name = this%standard_name()

      call this%NUOPC_has_entry(standard_name, has_entry, __RC__)

      if (.not. has_entry) then
         call this%NUOPC_add_entry(standard_name, this%units, __RC__)
      end if

      _RETURN(_SUCCESS)
   end subroutine register

   subroutine NUOPC_advert(this, state, standard_name, unusable,&
         TransferOfferGeomObject, SharePolicyField, SharePolicyGeomObject, rc)
      class(FieldEntry),                intent(inout) :: this
      type(ESMF_State),                 intent(inout) :: state
      character(*),                     intent(in   ) :: standard_name
      class(KeywordEnforcer), optional, intent(in   ) :: unusable
      character(*),           optional, intent(in   ) :: TransferOfferGeomObject
      character(*),           optional, intent(in   ) :: SharePolicyField
      character(*),           optional, intent(in   ) :: SharePolicyGeomObject
      integer,                optional, intent(  out) :: rc

      integer :: status

      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(unusable)

      call NUOPC_Advertise(state, standard_name, &
         TransferOfferGeomObject=TransferOfferGeomObject, &
         SharePolicyField=SharePolicyField, &
         SharePolicyGeomObject=SharePolicyGeomObject, &
         __RC__)
      VERIFY_NUOPC_(status)

      _RETURN(_SUCCESS)
   end subroutine NUOPC_advert

   subroutine advertise(this, state, unusable, TransferOfferGeomObject, rc)
      class(FieldEntry),                intent(inout) :: this
      type(ESMF_State),                 intent(inout) :: state
      class(KeywordEnforcer), optional, intent(in   ) :: unusable
      character(len=*),       optional, intent(in   ) :: TransferOfferGeomObject
      integer,                optional, intent(  out) :: rc

      integer :: status

      _UNUSED_DUMMY(unusable)

      call this%register(__RC__)
   
      if (present(TransferOfferGeomObject)) then
         call this%NUOPC_advert(state, this%standard_name(),&
            TransferOfferGeomObject=TransferOfferGeomObject, &
            SharePolicyField=this%SharePolicyField, &
            SharePolicyGeomObject=this%SharePolicyGeomObject, &
            __RC__)
      else
         call this%NUOPC_advert(state, this%standard_name(),&
            TransferOfferGeomObject=this%TransferOfferGeomObject, &
            SharePolicyField=this%SharePolicyField, &
            SharePolicyGeomObject=this%SharePolicyGeomObject, &
            __RC__)
      end if
      _RETURN(_SUCCESS)
   end subroutine advertise

   ! TODO: unit tests for realize

   subroutine NUOPC_real_accepted(this, state, fname, unusable, rc)
      class(FieldEntry),                intent(inout) :: this
      type(ESMF_State),                 intent(inout) :: state
      character(len=*),                 intent(in   ) :: fname
      class(KeywordEnforcer), optional, intent(  out) :: unusable
      integer,                optional, intent(  out) :: rc

      integer :: status

      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(unusable)

      call NUOPC_Realize(state, fname, rc=status)
      VERIFY_NUOPC_(status)

      _RETURN(_SUCCESS)
   end subroutine NUOPC_real_accepted

   subroutine NUOPC_real_provided(this, state, field, unusable, rc)
      class(FieldEntry),                intent(inout) :: this
      type(ESMF_State),                 intent(inout) :: state
      type(ESMF_Field),                 intent(in   ) :: field
      class(KeywordEnforcer), optional, intent(  out) :: unusable
      integer,                optional, intent(  out) :: rc

      integer :: status

      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(unusable)

      call NUOPC_Realize(state, field, rc=status)
      VERIFY_NUOPC_(status)

      _RETURN(_SUCCESS)
   end subroutine NUOPC_real_provided

   subroutine get_field_from_state(this, state, field, unusable, rc)
      class(FieldEntry),                intent(inout) :: this
      type(ESMF_State),                 intent(inout) :: state
      type(ESMF_Field),                 intent(  out) :: field
      class(KeywordEnforcer), optional, intent(  out) :: unusable
      integer,                optional, intent(  out) :: rc

      type(ESMF_StateItem_Flag) :: item_type
      integer :: item_count
      type(ESMF_Field) :: field_short
      type(ESMF_FieldStatus_Flag)     :: fieldStatus
      
      integer :: status

      _UNUSED_DUMMY(unusable)

      !call ESMF_StateGet(state, this%short_name, itemType=item_type, rc=status) 
      !call ESMF_StateGet(state, this%short_name//"."//this%component_name, itemType=item_type, rc=status) 
      call ESMF_StateGet(state, this%short_name, itemType=item_type, rc=status) 

      if (item_type == ESMF_STATEITEM_FIELD) then
         !call ESMF_StateGet(state, this%short_name//"."//this%component_name, field, __RC__)
         call ESMF_StateGet(state, this%short_name, field_short, __RC__)
         call ESMF_FieldGet(field_short, status=fieldStatus,__RC__)
         if (fieldStatus /= ESMF_FIELDSTATUS_COMPLETE) then 
            call MAPL_AllocateCOupling(field_short,__RC__)
         end if
         
         field=MAPL_FieldCreate(field_short,this%short_name//"."//this%component_name,__RC__)
         call ESMF_FieldValidate(field, __RC__)
      else
         ! This field entry is not found
         _VERIFY(1)
      end if

      _RETURN(_SUCCESS)
   end subroutine get_field_from_state

   subroutine realize(this, state, export_state, unusable, rc)
      class(FieldEntry),                intent(inout) :: this
      type(ESMF_State),                 intent(inout) :: state
      type(ESMF_State),                 intent(inout) :: export_state
      class(KeywordEnforcer), optional, intent(  out) :: unusable
      integer,                optional, intent(  out) :: rc

      type(ESMF_StateIntent_Flag) :: state_intent
      type(ESMF_Field)            :: field

      integer :: status

      _UNUSED_DUMMY(unusable)

      call ESMF_StateGet(state, stateIntent=state_intent, __RC__)

      if (state_intent == ESMF_STATEINTENT_EXPORT) then
         call this%get_field_from_state(export_state, field, __RC__)
         call MAPL_AllocateCoupling(field, status)
         _VERIFY(status)
         call this%NUOPC_real_provided(state, field, __RC__)
      else if (state_intent == ESMF_STATEINTENT_IMPORT) then
         call this%NUOPC_real_accepted(state, this%short_name//"."//this%component_name, __RC__)
      else
         ! This should only be run on an import or export state
         _VERIFY(1)
      end if

      _RETURN(_SUCCESS)
   end subroutine realize
end module FieldEntryMod
