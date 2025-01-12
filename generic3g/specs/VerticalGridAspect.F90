#include "MAPL_Generic.h"

module mapl3g_VerticalGridAspect
   use mapl3g_StateItemAspect
   use mapl3g_ExtensionAction
   use mapl3g_VerticalGrid
   use mapl3g_NullAction
   use mapl3g_VerticalRegridAction
   use mapl3g_VerticalRegridMethod
   use mapl3g_VerticalDimSpec
   use mapl3g_VerticalRegridMethod
   use mapl3g_ComponentDriver
   use mapl_ErrorHandling
   use ESMF
   implicit none
   private

   public :: VerticalGridAspect


   type, extends(StateItemAspect) :: VerticalGridAspect
!#      private
      class(VerticalGrid), allocatable :: vertical_grid
      type(VerticalRegridMethod) :: regrid_method = VERTICAL_REGRID_LINEAR
!#      type(VerticalStaggerLoc), allocatable :: vertical_staggerloc
      type(VerticalDimSpec), allocatable :: vertical_dim_spec

      ! These might be updated due to intervening couplers
      type(ESMF_Geom), allocatable :: geom
      type(ESMF_Typekind_Flag) :: typekind
   contains
      procedure :: matches
      procedure :: supports_conversion_general
      procedure :: supports_conversion_specific
      procedure :: make_action
      procedure :: typesafe_make_action

      procedure :: set_vertical_grid
      procedure :: set_geom
      procedure :: set_typekind

      procedure :: get_description
   end type VerticalGridAspect

   interface VerticalGridAspect
      procedure new_VerticalGridAspect_specific
   end interface

contains

   function new_VerticalGridAspect_specific(vertical_grid, regrid_method, vertical_dim_spec, geom, typekind, time_dependent) result(aspect)
      type(VerticalGridAspect) :: aspect
      class(VerticalGrid), optional, intent(in) :: vertical_grid
      type(VerticalRegridMethod), optional, intent(in) :: regrid_method
      type(VerticalDimSpec), optional, intent(in) :: vertical_dim_spec
      type(ESMF_Geom), optional, intent(in) :: geom
      type(ESMF_Typekind_Flag), optional, intent(in) :: typekind
      logical, optional, intent(in) :: time_dependent

      call aspect%set_mirror(.true.)
      if (present(vertical_grid)) then
         aspect%vertical_grid = vertical_grid
         call aspect%set_mirror(.false.)
      end if

      if (present(regrid_method)) then
         aspect%regrid_method = regrid_method
      end if

      if (present(vertical_dim_spec)) then
         aspect%vertical_dim_spec = vertical_dim_spec
      end if
    
      if (present(geom)) then
         aspect%geom = geom
      end if

      if (present(typekind)) then
         aspect%typekind = typekind
      end if

      call aspect%set_time_dependent(time_dependent)

   end function new_VerticalGridAspect_specific

   function new_VerticalGridAspect_mirror() result(aspect)
      type(VerticalGridAspect) :: aspect

      call aspect%set_mirror(.true.)

   end function new_VerticalGridAspect_mirror

   logical function supports_conversion_general(src)
      class(VerticalGridAspect), intent(in) :: src
      supports_conversion_general = .true.
   end function supports_conversion_general

   logical function supports_conversion_specific(src, dst)
      class(VerticalGridAspect), intent(in) :: src
      class(StateItemAspect), intent(in) :: dst

      integer :: status
      
      supports_conversion_specific = .false.

      select type (dst)
      class is (VerticalGridAspect)
         ! Note: "grid%can_connect_to()" reverses dst and src.   Something that should be fixed.
         supports_conversion_specific = src%vertical_grid%can_connect_to(dst%vertical_grid)
      end select

   end function supports_conversion_specific

   logical function matches(src, dst)
      class(VerticalGridAspect), intent(in) :: src
      class(StateItemAspect), intent(in) :: dst

      select type(dst)
      class is (VerticalGridAspect)
         matches = dst%vertical_grid%is_identical_to(src%vertical_grid)
      class default
         matches = .false.
      end select

   end function matches

   function make_action(src, dst, rc) result(action)
      class(ExtensionAction), allocatable :: action
      class(VerticalGridAspect), intent(in) :: src
      class(StateItemAspect), intent(in)  :: dst
      integer, optional, intent(out) :: rc

      select type (dst)
      class is (VerticalGridAspect)
         action = src%typesafe_make_action(dst, rc)
      class default
         action = NullAction()
         _FAIL('dst is not a VerticalGridAspect')
      end select

      _RETURN(_SUCCESS)
   end function make_action

   function typesafe_make_action(src, dst, rc) result(action)
      class(ExtensionAction), allocatable :: action
      class(VerticalGridAspect), intent(in) :: src
      class(VerticalGridAspect), intent(in)  :: dst
      integer, optional, intent(out) :: rc

      class(ComponentDriver), pointer :: v_in_coupler
      class(ComponentDriver), pointer :: v_out_coupler
      type(ESMF_Field) :: v_in_field, v_out_field

      type(ESMF_Geom) :: geom
      type(ESMF_TypeKind_Flag) :: typekind
      character(:), allocatable :: units
      integer :: status

      geom = src%geom
      typekind = src%typekind
      units = src%vertical_grid%get_units()

!#      call src%vertical_grid%get_coordinate_field(v_in_field, v_in_coupler, geom, typekind, src%vertical_staggerloc, _RC)
!#      call dst%vertical_grid%get_coordinate_field(v_out_field, v_out_coupler, geom, typekind, dst%vertical_staggerloc, _RC)

      call src%vertical_grid%get_coordinate_field(v_in_field, v_in_coupler, 'ignore', geom, typekind, units, src%vertical_dim_spec, _RC)
      call dst%vertical_grid%get_coordinate_field(v_out_field, v_out_coupler, 'ignore', geom, typekind, units, dst%vertical_dim_spec, _RC)

      action = VerticalRegridAction(v_in_field, v_in_coupler, v_out_field, v_out_coupler, dst%regrid_method)

      _RETURN(_SUCCESS)
   end function typesafe_make_action

   subroutine set_vertical_grid(self, vertical_grid)
      class(VerticalGridAspect), intent(inout) :: self
      class(VerticalGrid), intent(in) :: vertical_grid

      self%vertical_grid = vertical_grid
      call self%set_mirror(.false.)
   end subroutine set_vertical_grid

   subroutine set_geom(self, geom)
      class(VerticalGridAspect), intent(inout) :: self
      type(ESMF_Geom), intent(in) :: geom

      self%geom = geom
   end subroutine set_geom

   subroutine set_typekind(self, typekind)
      class(VerticalGridAspect), intent(inout) :: self
      type(ESMF_Typekind_Flag), intent(in) :: typekind

      self%typekind = typekind
   end subroutine set_typekind

   function get_description(this) result(s)
      character(:), allocatable :: s
      class(VerticalGridAspect), intent(in) :: this

      ! Should not get here in mirror'd case, but ...
      s = 'description not implemented'

   end function get_description

end module mapl3g_VerticalGridAspect
