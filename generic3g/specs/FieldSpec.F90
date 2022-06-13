module mapl3g_FieldSpec
   use mapl3g_AbstractStateItemSpec
   use mapl3g_GridSpec
   use mapl3g_DimsSpec
   use esmf, only: ESMF_TypeKind_Flag
   use esmf, only: ESMF_TYPEKIND_R4

   implicit none
   private

   public :: FieldSpec

   type, extends(AbstractStateItemSpec) :: FieldSpec
      type(DimsSpec) :: dims_spec
      type(ESMF_typekind_flag) :: typekind
      class(GridSpec), allocatable :: grid_spec
!!$   contains
!!$      procedure, deferred :: can_share_pointer
   end type FieldSpec

   interface FieldSpec
      module procedure new_FieldSpec_full
      module procedure new_FieldSpec_defaults
   end interface FieldSpec

contains

      

   function new_FieldSpec_full(dims_spec, typekind, grid_spec) result(field_spec)
      type(FieldSpec) :: field_spec
      type(DimsSpec), intent(in) :: dims_spec
      type(ESMF_Typekind_Flag), intent(in) :: typekind
      type(GridSpec), intent(in) :: grid_spec
   end function new_FieldSpec_full


   function new_FieldSpec_defaults(dims_spec) result(field_spec)
      type(FieldSpec) :: field_spec
      type(DimsSpec), intent(in) :: dims_spec
      
      field_spec = new_FieldSpec_full(dims_spec, ESMF_TYPEKIND_R4, GridSpec(GRID_ORIGIN_FROM_PARENT))
      
   end function new_FieldSpec_defaults
   

!!$   logical function can_share_pointer(this, other)
!!$      class(FieldSpec), intent(in) :: this
!!$      type(FieldSpec), intent(in) :: other
!!$
!!$      can_share_pointer = same_type_kind(this, other) &
!!$           .and. same_grid(this, other) &
!!$           .and. same_units(this, other) 
!!$
!!$   contains
!!$
!!$      logical function same_type_kind(a, b)
!!$      end function same_type_kind
!!$
!!$      logical function same_grid(a,b)
!!$      end function same_grid
!!$
!!$      logical function same_units(a,b)
!!$         call field_dictionary%get(units_a, a%name, 'units', _RC)
!!$         call field_dictionary%get(units_b, b%name, 'units', _RC)
!!$
!!$         same_units = (units_a == units_b)
!!$      end function same_units
!!$
!!$   end function can_share_pointer
!!$
end module mapl3g_FieldSpec
