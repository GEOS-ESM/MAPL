#include "MAPL_Generic.h"

module mapl3g_FieldSpec
   use mapl3g_AbstractStateItemSpec
   use mapl3g_ExtraDimsSpec
   use mapl_ErrorHandling
   use esmf

   implicit none
   private

   public :: FieldSpec

   type, extends(AbstractStateItemSpec) :: FieldSpec
      private

      character(:), allocatable :: units
      type(ESMF_typekind_flag) :: typekind
      type(ESMF_Grid) :: grid
      type(ExtraDimsSpec) :: extra_dims
!!$      type(FrequencySpec) :: freq_spec
!!$      class(AbstractFrequencySpec), allocatable :: freq_spec
      integer :: halo_width = 0

      type(ESMF_Field) :: payload

   contains
      procedure :: create
      procedure :: destroy
      procedure :: allocate

      procedure :: connect_to
      procedure :: can_connect_to
      procedure :: requires_extension
      procedure :: add_to_state
   end type FieldSpec

   interface FieldSpec
      module procedure new_FieldSpec_full
      module procedure new_FieldSpec_defaults
   end interface FieldSpec

contains


   function new_FieldSpec_full(extra_dims, typekind, grid) result(field_spec)
      type(FieldSpec) :: field_spec
      type(ExtraDimsSpec), intent(in) :: extra_dims
      type(ESMF_Typekind_Flag), intent(in) :: typekind
      type(ESMF_Grid), intent(in) :: grid
   end function new_FieldSpec_full


   function new_FieldSpec_defaults(extra_dims, grid) result(field_spec)
      type(FieldSpec) :: field_spec
      type(ExtraDimsSpec), intent(in) :: extra_dims
      type(ESMF_Grid), intent(in) :: grid
      
      field_spec = new_FieldSpec_full(extra_dims, ESMF_TYPEKIND_R4, grid)
      
   end function new_FieldSpec_defaults


   subroutine create(this, rc)
      class(FieldSpec), intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status
      
      this%payload = ESMF_FieldEmptyCreate(_RC)
      call ESMF_FieldEmptySet(this%payload, grid=this%grid, _RC)

      call this%set_created()

      _RETURN(ESMF_SUCCESS)
   end subroutine create


   subroutine destroy(this, rc)
      class(FieldSpec), intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status

      call ESMF_FieldDestroy(this%payload, _RC)
      call this%set_created(.false.)

      _RETURN(ESMF_SUCCESS)
   end subroutine destroy


   ! Tile / Grid   X  or X, Y
   subroutine allocate(this, rc)
      class(FieldSpec), intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_FieldStatus_Flag) :: fstatus
      
      call ESMF_FieldGet(this%payload, status=fstatus, _RC)
      if (fstatus == ESMF_FIELDSTATUS_EMPTY) then

         call ESMF_FieldEmptyComplete(this%payload, this%typekind, &
              ungriddedLBound= this%extra_dims%get_lbounds(),  &
              ungriddedUBound= this%extra_dims%get_ubounds(),  &
              _RC)

         call this%set_allocated()
      end if

      _RETURN(ESMF_SUCCESS)
   end subroutine allocate


   subroutine connect_to(this, src_spec, rc)
      class(FieldSpec), intent(inout) :: this
      class(AbstractStateItemSpec), intent(in) :: src_spec
      integer, optional, intent(out) :: rc

      integer :: status

      _ASSERT(this%can_connect_to(src_spec), 'illegal connection')

      select type (src_spec)
      class is (FieldSpec)
         ! ok
      class default
         _FAIL('Cannot connect field spec to non field spec.')
      end select

      _RETURN(ESMF_SUCCESS)

   end subroutine connect_to


   logical function can_connect_to(this, src_spec)
      class(FieldSpec), intent(in) :: this
      class(AbstractStateItemSpec), intent(in) :: src_spec

      select type(src_spec)
      class is (FieldSpec)
         can_connect_to = all ([ &
              this%typekind == src_spec%typekind,   &
              this%extra_dims == src_spec%extra_dims, &
!!$              this%freq_spec == src_spec%freq_spec,   &
!!$              this%halo_width == src_spec%halo_width,  &
!!$              this%vm == sourc%vm, &
              can_convert_units(this, src_spec) &
              ])
      class default
         can_connect_to = .false.
      end select

   end function can_connect_to


   logical function requires_extension(this, src_spec)
      class(FieldSpec), intent(in) :: this
      class(AbstractStateItemSpec), intent(in) :: src_spec

      requires_extension = .true.

      select type(src_spec)
      class is (FieldSpec)
         requires_extension = any([ &
              this%extra_dims /= src_spec%extra_dims, &
              this%typekind /= src_spec%typekind,   &
!!$              this%freq_spec /= src_spec%freq_spec,   &
!!$              this%units /= src_spec%units,           &
!!$              this%halo_width /= src_spec%halo_width, &
!!$              this%vm /= sourc%vm,               &
              this%grid /= src_spec%grid             &
              ])
         requires_extension = .false.
      end select
   end function requires_extension

   logical function same_typekind(a, b)
      class(FieldSpec), intent(in) :: a
      class(FieldSpec), intent(in) :: b
      same_typekind = (a%typekind == b%typekind)
   end function same_typekind

   ! Eventually we will integrate UDunits, but for now
   ! we require units to exactly match when connecting
   ! fields.
   logical function can_convert_units(a,b)
      class(FieldSpec), intent(in) :: a
      class(FieldSpec), intent(in) :: b

      can_convert_units = a%units == b%units

   end function can_convert_units

   subroutine add_to_state(this, state, short_name, rc)
      class(FieldSpec), intent(in) :: this
      type(ESMF_State), intent(inout) :: state
      character(*), intent(in) :: short_name
      integer, optional, intent(out) :: rc

      type(ESMF_Field) :: alias
      integer :: status

      _FAIL('unimplemented')

!!$      alias = ESMF_NamedAlias(this%payload, name=short_name, _RC)
!!$      call ESMF_StateAdd(state, this%payload, short_name, _RC)
!!$

   end subroutine add_to_state

end module mapl3g_FieldSpec
