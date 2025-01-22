#include "MAPL_Generic.h"

module mapl3g_GeomAspect
   use mapl3g_AspectId
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
   public :: to_GeomAspect ! cast from poly

   interface to_GeomAspect
      procedure :: to_geom_from_poly
      procedure :: to_geom_from_map
   end interface to_GeomAspect

   type, extends(StateItemAspect) :: GeomAspect
      private
      type(ESMF_Geom), allocatable :: geom
      type(EsmfRegridderParam) :: regridder_param
      type(HorizontalDimsSpec) :: horizontal_dims_spec = HORIZONTAL_DIMS_GEOM ! none, geom
   contains
      procedure :: matches
      procedure :: make_action
      procedure :: make_action2
      procedure :: connect_to_export
      procedure :: supports_conversion_general
      procedure :: supports_conversion_specific
      procedure :: set_geom
      procedure :: get_geom
      procedure, nopass :: get_aspect_id
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

   function make_action2(src, dst, other_aspects, rc) result(action)
      class(ExtensionAction), allocatable :: action
      class(GeomAspect), intent(in) :: src
      class(StateItemAspect), intent(in)  :: dst
      type(AspectMap), target, intent(in)  :: other_aspects
      integer, optional, intent(out) :: rc

      integer :: status
      type(GeomAspect) :: dst_

      allocate(action,source=NullAction()) ! just in case
      dst_ = to_GeomAspect(dst, _RC)

      deallocate(action)
      allocate(action, source=RegridAction(src%geom, dst_%geom, dst_%regridder_param))

      _RETURN(_SUCCESS)
   end function make_action2

   subroutine set_geom(this, geom)
      class(GeomAspect), intent(inout) :: this
      type(ESMF_Geom) :: geom

      this%geom = geom
      call this%set_mirror(.false.)
      
   end subroutine set_geom

   function get_geom(this, rc) result(geom)
      class(GeomAspect), intent(in) :: this
      type(ESMF_Geom) :: geom
      integer, optional, intent(out) :: rc

      _ASSERT(allocated(this%geom), 'geom not allocated')
      geom = this%geom

      _RETURN(_SUCCESS)
   end function get_geom

   subroutine connect_to_export(this, export, rc)
      class(GeomAspect), intent(inout) :: this
      class(StateItemAspect), intent(in) :: export
      integer, optional, intent(out) :: rc

      type(GeomAspect) :: export_
      integer :: status

      export_ = to_GeomAspect(export, _RC)
      this%geom = export_%geom

      _RETURN(_SUCCESS)
   end subroutine connect_to_export

   function to_geom_from_poly(aspect, rc) result(geom_aspect)
      type(GeomAspect) :: geom_aspect
      class(StateItemAspect), intent(in) :: aspect
      integer, optional, intent(out) :: rc

      integer :: status

      select type(aspect)
      class is (GeomAspect)
         geom_aspect = aspect
      class default
         _FAIL('aspect is not GeomAspect')
      end select

      _RETURN(_SUCCESS)
   end function to_geom_from_poly

   function to_geom_from_map(map, rc) result(geom_aspect)
      type(GeomAspect) :: geom_aspect
      type(AspectMap), target, intent(in) :: map
      integer, optional, intent(out) :: rc

      integer :: status
      class(StateItemAspect), pointer :: poly

      poly => map%at(GEOM_ASPECT_ID, _RC)
      geom_aspect = to_GeomAspect(poly, _RC)

      _RETURN(_SUCCESS)
   end function to_geom_from_map
   

   function get_aspect_id() result(aspect_id)
      type(AspectId) :: aspect_id
      aspect_id = GEOM_ASPECT_ID
   end function get_aspect_id

end module mapl3g_GeomAspect
