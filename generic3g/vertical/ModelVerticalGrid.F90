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

      ! subclass-specific methods
      procedure :: add_variant
      procedure :: get_num_variants
      procedure :: set_registry
      procedure :: get_registry
   end type ModelVerticalGrid

   interface ModelVerticalGrid
      procedure new_ModelVerticalGrid_basic
   end interface ModelVerticalGrid


   ! TODO:
   ! - Ensure that there really is a vertical dimension

contains

   function new_ModelVerticalGrid_basic(num_levels) result(vgrid)
      type(ModelVerticalGrid) :: vgrid
      integer, intent(in) :: num_levels
!#      character(*), intent(in) :: short_name
!#      character(*), intent(in) :: standard_name
!#      type(StateRegistry), pointer, intent(in) :: registry

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


    subroutine get_coordinate_field(this, field, coupler, standard_name, geom, typekind, units, rc)
       class(ModelVerticalGrid), intent(inout) :: this
       type(ESMF_Field), intent(out) :: field
       type(GriddedComponentDriver), pointer, intent(out) :: coupler
       character(*), intent(in) :: standard_name
       type(ESMF_Geom), intent(in) :: geom
       type(ESMF_TypeKind_Flag), intent(in) :: typekind
       character(*), intent(in) :: units
       integer, optional, intent(out) :: rc

       integer :: status
       type(VirtualConnectionPt) :: v_pt
       type(ActualConnectionPt) :: a_pt
       integer :: cost, lowest_cost
       type(StateItemExtensionPtr), pointer :: extensionPtr
       type(StateItemExtension) :: tmp_extension
       type(StateItemExtension), pointer :: best_extension
       type(StateItemExtension), pointer :: new_extension
       type(StateItemExtensionPtrVector), pointer :: extensions
       class(StateItemSpec), pointer :: spec, new_spec
       type(ExtensionFamily), pointer :: family
       type(MultiState) :: multi_state
       type(FieldSpec) :: goal_spec
       type(MultiState) :: coupler_states
       integer :: i

       v_pt = VirtualConnectionPt(state_intent='export', short_name=this%variants%of(1))

       family => this%registry%get_extension_family(v_pt, _RC)
       extensions => family%get_extensions()

       goal_spec = FieldSpec(geom=geom, vertical_grid=this, vertical_dim_spec=VERTICAL_DIM_EDGE, &
            typekind=typekind, standard_name=standard_name, units=units, &
            ungridded_dims=UngriddedDims())

       lowest_cost = huge(1)
       best_extension => null()
       do i = 1, extensions%size()
          extensionPtr => extensions%of(i)
          spec => extensionPtr%ptr%get_spec()
          cost = goal_spec%extension_cost(spec, _RC)
          if (cost < lowest_cost) then
             lowest_cost = cost
             best_extension => extensionPtr%ptr
          end if
       end do
          

       do
          spec => best_extension%get_spec()
          call spec%set_active()
          cost = goal_spec%extension_cost(spec, _RC)
          if (cost == 0) exit

          tmp_extension = best_extension%make_extension(goal_spec, _RC)
          new_extension => this%registry%add_extension(v_pt, tmp_extension, _RC)
          coupler => new_extension%get_producer()
          
          coupler_states = coupler%get_states()
          a_pt = ActualConnectionPt(VirtualConnectionPt(state_intent='import', short_name='import[1]'))
          call spec%add_to_state(coupler_states, a_pt, _RC)
          a_pt = ActualConnectionPt(VirtualConnectionPt(state_intent='export', short_name='export[1]'))
          new_spec => new_extension%get_spec()
          call new_spec%add_to_state(coupler_states, a_pt, _RC)

          call best_extension%add_consumer(coupler)
          best_extension => new_extension

       end do

       coupler => best_extension%get_producer()
       spec => best_extension%get_spec()
       call spec%set_active()
       multi_state = MultiState()
       a_pt = ActualConnectionPt(VirtualConnectionPt(state_intent='export', short_name='vcoord'))
       call spec%add_to_state(multi_state, a_pt, _RC)
       call ESMF_StateGet(multi_state%exportState, itemName='vcoord', field=field, _RC)
       _RETURN(_SUCCESS)

    end subroutine get_coordinate_field

end module mapl3g_ModelVerticalGrid
