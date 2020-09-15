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
      character(:), allocatable :: trans_geom
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

      check_entry = this%has_field_dictionary_entry(rc=status)
      VERIFY_NUOPC_(status)

      if (.not. check_entry) then
         call this%add_field_dictionary_entry(rc=status)
         VERIFY_NUOPC_(status)
      end if

      _RETURN(_SUCCESS)
   end subroutine add_to_field_dictionary

   subroutine advertise(this, state, rc)
      class(FieldAttributes), intent(inout) :: this
      type(ESMF_State),       intent(inout) :: state
      integer, optional,      intent(  out) :: rc

      integer :: status

      call NUOPC_Advertise(state, standardName=trim(this%name), &
            TransferOfferGeomObject=this%trans_geom, rc=status)
      VERIFY_NUOPC_(status)

      _RETURN(_SUCCESS)
   end subroutine advertise

   subroutine advertise_to_state(this, state, rc)
      class(FieldAttributes), intent(inout) :: this
      type(ESMF_State),       intent(inout) :: state
      integer, optional,      intent(  out) :: rc

      integer :: status

      call this%add_to_field_dictionary(rc=status)
      VERIFY_NUOPC_(status)

      call this%advertise(state, rc=status)
      VERIFY_NUOPC_(status)

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

      call ESMF_FieldValidate(this%field, rc=status)
      VERIFY_NUOPC_(status)

      _RETURN(_SUCCESS)
   end subroutine validate

   subroutine realize_to_import_state(this, state, rc)
      class(FieldAttributes), intent(inout) :: this
      type(ESMF_State),       intent(inout) :: state
      integer, optional,      intent(  out) :: rc

      integer :: status

      call this%validate(rc=status)
      VERIFY_NUOPC_(status)

      call this%realize(state, rc=status)
      VERIFY_NUOPC_(status)

      _RETURN(_SUCCESS)
   end subroutine realize_to_import_state

   subroutine allocate_coupling(this, rc)
      class(FieldAttributes), intent(inout) :: this
      integer, optional,      intent(  out) :: rc

      integer :: status

      call MAPL_AllocateCoupling(this%field, rc=status)
      VERIFY_NUOPC_(status)

      _RETURN(_SUCCESS)
   end subroutine allocate_coupling

   subroutine realize_to_export_state(this, state, rc)
      class(FieldAttributes), intent(inout) :: this
      type(ESMF_State),       intent(inout) :: state
      integer,optional,       intent(  out) :: rc

      integer :: status

      call this%allocate_coupling(rc=status)
      VERIFY_NUOPC_(status)

      call this%realize(state, rc=status)
      VERIFY_NUOPC_(status)

      _RETURN(_SUCCESS)
   end subroutine realize_to_export_state
end module NUOPC_MAPLfields_Mod
