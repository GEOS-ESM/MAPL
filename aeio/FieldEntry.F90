#include "MAPL_Generic.h"
#include "NUOPC_ErrLog.h"

module FieldEntryMod
   use ESMF
   use NUOPC
   use MAPL_BaseMod
   use MAPL_ExceptionHandling
   use MAPL_KeywordEnforcerMod
   use ESMFL_Mod
   use MAPL_GenericMod

   implicit none
   private

   public :: FieldEntry

   character(*), parameter :: default_units = '1'

   type :: FieldEntry
      private
      character(:), allocatable :: short_name
      character(:), allocatable :: component_name
      character(:), allocatable :: units

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
      procedure :: add_to_bundle

   end type FieldEntry
contains
   subroutine initialize(this, short_name, component_name, unusable, units)
      class(FieldEntry),                intent(  out) :: this
      character(*),                     intent(in   ) :: short_name
      character(*),                     intent(in   ) :: component_name
      class(KeywordEnforcer), optional, intent(in   ) :: unusable
      character(*),           optional, intent(in   ) :: units

      _UNUSED_DUMMY(unusable)

      write(*,*)"bmaa adding ",trim(short_name)," ",trim(component_name)
      this%short_name     = short_name
      this%component_name = component_name

      if (present(units)) then
         this%units = units
      else
         this%units = default_units
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

   subroutine add_to_bundle(this, state, bundle, unusable, grid, rc)
      class(FieldEntry),                intent(inout) :: this
      type(ESMF_State),                 intent(inout) :: state
      type(ESMF_FieldBundle),           intent(inout) :: bundle
      class(KeywordEnforcer), optional, intent(in   ) :: unusable
      type(ESMF_Grid),        optional, intent(in   ) :: grid
      integer,                optional, intent(  out) :: rc

      integer :: status
      type(ESMF_State) :: export_state
      type(ESMF_Field) :: field,new_field
      integer :: lb(1),ub(1),rank, local_des
      real(ESMF_KIND_R4), pointer :: ptr2d(:,:),ptr3d(:,:,:)

      _UNUSED_DUMMY(unusable)

      call MAPL_ExportStateGet([state],trim(this%component_name),export_state,rc=status)
      _VERIFY(status)
      call ESMF_StateGet(export_state,trim(this%short_name),field,rc=status)
      _VERIFY(status)
      if (present(grid)) then
         call ESMF_FieldGet(field,rank=rank,rc=status)
         if (rank==2) then
            new_field = ESMF_FieldCreate(grid,ESMF_TYPEKIND_R4,name=trim(this%short_name),rc=status)
            _VERIFY(status)
            call ESMF_FieldGet(new_field,localDECount=local_des,rc=status)
            _VERIFY(status)
            if (local_des >0) then
               call ESMF_FieldGet(new_field,localDE=0,farrayPtr=ptr2d,rc=status)
               _VERIFY(status)
               ptr2d=0.0
            end if
         else if (rank==3) then
            call ESMF_FieldGet(field,ungriddedLBound=lb,ungriddedUBound=ub,rc=status)
            new_field = ESMF_FieldCreate(grid,ESMF_TYPEKIND_R4,name=trim(this%short_name), &
                        ungriddedLBound=lb,ungriddedUBound=ub,rc=status)
            _VERIFY(status)
            call ESMF_FieldGet(new_field,localDECount=local_des,rc=status)
            _VERIFY(status)
            if (local_des >0) then
               call ESMF_FieldGet(new_field,localDE=0,farrayPtr=ptr3d,rc=status)
               _VERIFY(status)
               ptr3d=0.0
            end if
         end if
         call ESMF_FieldBundleAdd(bundle,[new_field],rc=status)
         _VERIFY(status)
      else
         call ESMF_FieldBundleAdd(bundle,[field],rc=status)
         _VERIFY(status)
      end if

      _RETURN(_SUCCESS)
   end subroutine add_to_bundle

end module FieldEntryMod
