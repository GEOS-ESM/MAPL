#include "MAPL_Generic.h"

module mapl3g_GeomAspect
   use mapl3g_HorizontalDimsSpec
   use mapl3g_StateItemAspect
   use mapl3g_geom_mgr, only: MAPL_SameGeom
   use mapl3g_regridder_mgr, only: EsmfRegridderParam
   use mapl3g_ExtensionAction
   use mapl3g_RegridAction
   use mapl3g_NullAction
   use mapl_ErrorHandling
   use ESMF, only: ESMF_Geom
   implicit none
   private

   public :: GeomAspect


   type, extends(StateItemAspect) :: GeomAspect
      type(ESMF_Geom), allocatable :: geom
      type(EsmfRegridderParam) :: regridder_param
      type(HorizontalDimsSpec) :: horizontal_dims_spec = HORIZONTAL_DIMS_GEOM ! none, geom
   contains
      procedure :: matches
      procedure :: make_action
      procedure :: supports_conversion_general
      procedure :: supports_conversion_specific
      procedure :: set_geom
   end type GeomAspect

   interface GeomAspect
      procedure new_GeomAspect
   end interface

contains

   function new_GeomAspect(geom, regridder_param, horizontal_dims_spec, is_time_dependent) result(aspect)
      type(GeomAspect) :: aspect
      type(ESMF_Geom), optional, intent(in) :: geom
      type(EsmfRegridderParam), optional, intent(in) :: regridder_param
      type(HorizontalDimsSpec), optional, intent(in) :: horizontal_dims_spec
      logical, optional, intent(in) :: is_time_dependent

      call aspect%set_mirror(.true.)

      if (present(geom)) then
         aspect%geom = geom
         call aspect%set_mirror(.false.)
      end if

      aspect%regridder_param = EsmfRegridderParam() ! default
      if (present(regridder_param)) then
         aspect%regridder_param = regridder_param
      end if

      aspect%horizontal_dims_spec = HORIZONTAL_DIMS_GEOM ! default
      if (present(horizontal_dims_spec)) then
         aspect%horizontal_dims_spec = horizontal_dims_spec
      end if

      call aspect%set_time_dependent(is_time_dependent)

   end function new_GeomAspect

   ! Generally, geoms can be converted via RouteHandle, but there
   ! are definitely many exceptions.   A better implementation here could attempt to create
   ! the relevant regridder.
   logical function supports_conversion_general(src)
      class(GeomAspect), intent(in) :: src
      supports_conversion_general = .true.
   end function supports_conversion_general

   logical function supports_conversion_specific(src, dst)
      class(GeomAspect), intent(in) :: src
      class(StateItemAspect), intent(in) :: dst

      supports_conversion_specific = .false.
      select type(dst)
      class is (GeomAspect)
         supports_conversion_specific = (src%horizontal_dims_spec == dst%horizontal_dims_spec)
      end select

   end function supports_conversion_specific

   logical function matches(src, dst)
      class(GeomAspect), intent(in) :: src
      class(StateItemAspect), intent(in) :: dst

      select type(dst)
      class is (GeomAspect)
         matches = MAPL_SameGeom(src%geom, dst%geom) .and. (src%horizontal_dims_spec == dst%horizontal_dims_spec)
      class default
         matches = .false.
      end select

   end function matches

   function make_action(src, dst, rc) result(action)
      class(ExtensionAction), allocatable :: action
      class(GeomAspect), intent(in) :: src
      class(StateItemAspect), intent(in)  :: dst
      integer, optional, intent(out) :: rc

      select type(dst)
      class is (GeomAspect)
         allocate(action, source=RegridAction(src%geom, dst%geom, dst%regridder_param))
      class default
         allocate(action,source=NullAction())
         _FAIL('src is GeomAspect but dst is different subclass')
      end select

      _RETURN(_SUCCESS)
   end function make_action

   subroutine set_geom(this, geom)
      class(GeomAspect), intent(inout) :: this
      type(ESMF_Geom) :: geom

      this%geom = geom
      call this%set_mirror(.false.)
      
   end subroutine set_geom

end module mapl3g_GeomAspect
