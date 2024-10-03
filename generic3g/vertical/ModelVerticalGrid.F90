#include "MAPL_Generic.h"

module mapl3g_ModelVerticalGrid
   use mapl3g_VerticalGrid
   use mapl3g_StateRegistry
   use mapl3g_MultiState
   use mapl3g_VirtualConnectionPt
   use mapl3g_ActualConnectionPt
   use mapl3g_StateItemSpec
   use mapl3g_FieldSpec
   use mapl3g_UngriddedDims
   use mapl3g_StateItemExtension
   use mapl3g_ExtensionFamily
   use mapl3g_ExtensionAction
   use mapl3g_VerticalDimSpec
   use mapl3g_StateItemExtensionPtrVector
   use mapl_ErrorHandling
   use mapl3g_GriddedComponentDriver
   use gftl2_StringVector
   use esmf
   implicit none
   private

   public :: ModelVerticalGrid

   type, extends(VerticalGrid) :: ModelVerticalGrid
      private
      integer :: num_levels = -1
      type(StringVector) :: variants

!#      character(:), allocatable :: short_name
!#      character(:), allocatable :: standard_name
!#      type(ESMF_Field) :: reference_field
      type(StateRegistry), pointer :: registry => null()
   contains
      procedure :: get_num_levels
      procedure :: get_coordinate_field
      procedure :: can_connect_to

      ! subclass-specific methods
      procedure :: add_variant
      procedure :: get_num_variants
      procedure :: set_registry
      procedure :: get_registry
   end type ModelVerticalGrid

   interface ModelVerticalGrid
      procedure new_ModelVerticalGrid_basic
   end interface ModelVerticalGrid

   interface
      module function can_connect_to(this, src, rc)
         logical :: can_connect_to
         class(ModelVerticalGrid), intent(in) :: this
         class(VerticalGrid), intent(in) :: src
         integer, optional, intent(out) :: rc
      end function
   end interface

   ! TODO:
   ! - Ensure that there really is a vertical dimension

contains

   function new_ModelVerticalGrid_basic(num_levels) result(vgrid)
      type(ModelVerticalGrid) :: vgrid
      integer, intent(in) :: num_levels
!#      character(*), intent(in) :: short_name
!#      character(*), intent(in) :: standard_name
!#      type(StateRegistry), pointer, intent(in) :: registry

      call vgrid%set_id()
      vgrid%num_levels = num_levels
!#      vgrid%short_name = short_name
!#      vgrid%standard_name = standard_name
!#      vgrid%registry => registry

   end function new_ModelVerticalGrid_basic


   integer function get_num_levels(this) result(num_levels)
      class(ModelVerticalGrid), intent(in) :: this
      num_levels = this%num_levels
   end function get_num_levels

   subroutine add_variant(this, short_name)
      class(ModelVerticalGrid), intent(inout) :: this
      character(*), intent(in) :: short_name

      call this%variants%push_back(short_name)
   end subroutine add_variant

   integer function get_num_variants(this) result(num_variants)
      class(ModelVerticalGrid), intent(in) :: this
      num_variants = this%variants%size()
   end function get_num_variants

    subroutine set_registry(this, registry)
       class(ModelVerticalGrid), intent(inout) :: this
       type(StateRegistry), target, intent(in) :: registry
  
       this%registry => registry
    end subroutine set_registry

    function get_registry(this) result(registry)
       class(ModelVerticalGrid), intent(in) :: this
       type(StateRegistry), pointer :: registry
       registry => this%registry
    end function get_registry

    subroutine get_coordinate_field(this, field, coupler, standard_name, geom, typekind, units, vertical_dim_spec, rc)
       class(ModelVerticalGrid), intent(in) :: this
       type(ESMF_Field), intent(out) :: field
       type(GriddedComponentDriver), pointer, intent(out) :: coupler
       character(*), intent(in) :: standard_name
       type(ESMF_Geom), intent(in) :: geom
       type(ESMF_TypeKind_Flag), intent(in) :: typekind
       character(*), intent(in) :: units
       type(VerticalDimSpec), intent(in) :: vertical_dim_spec
       integer, optional, intent(out) :: rc

       integer :: status
       type(VirtualConnectionPt) :: v_pt
       type(StateItemExtension), pointer :: new_extension
       class(StateItemSpec), pointer :: new_spec
       type(FieldSpec) :: goal_spec
       integer :: i

       v_pt = VirtualConnectionPt(state_intent='export', short_name=this%variants%of(1))
       goal_spec = FieldSpec( &
            geom=geom, vertical_grid=this, vertical_dim_spec=vertical_dim_spec, &
            typekind=typekind, &
            standard_name=standard_name, &
            units=units, &
            ungridded_dims=UngriddedDims())
       new_extension => this%registry%extend(v_pt, goal_spec, _RC)
       coupler => new_extension%get_producer()
       new_spec => new_extension%get_spec()
       select type (new_spec)
       type is (FieldSpec)
          field = new_spec%get_payload()
       class default
          _FAIL('unsupported spec type; must be FieldSpec')
       end select

       _RETURN(_SUCCESS)
    end subroutine get_coordinate_field

end module mapl3g_ModelVerticalGrid
