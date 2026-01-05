#include "MAPL.h"
module mapl3g_GeomAspect
   use mapl3g_ActualConnectionPt
   use mapl3g_AspectId
   use mapl3g_HorizontalDimsSpec
   use mapl3g_StateItemAspect
   use mapl3g_Geom_API, only: MAPL_SameGeom
   use mapl3g_regridder_mgr, only: EsmfRegridderParam
   use mapl3g_ExtensionTransform
   use mapl3g_ExtendTransform
   use mapl3g_RegridTransform
   use mapl3g_NullTransform
   use mapl3g_Field_API
   use mapl3g_FieldBundle_API
   use mapl_ErrorHandling
   use ESMF, only: esmf_Geom
   use ESMF, only: esmf_Field, esmf_FieldBundle, esmf_State
   implicit none
   private

   public :: GeomAspect
   public :: to_GeomAspect ! cast from poly

   interface to_GeomAspect
      procedure :: to_geom_from_poly
      procedure :: to_geom_from_map
   end interface to_GeomAspect

   type, extends(StateItemAspect) :: GeomAspect
!#      private
      type(ESMF_Geom), allocatable :: geom
      type(EsmfRegridderParam), allocatable :: regridder_param
      type(HorizontalDimsSpec) :: horizontal_dims_spec = HORIZONTAL_DIMS_GEOM ! none, geom
   contains
      procedure :: matches
      procedure :: make_transform
      procedure :: connect_to_export
      procedure :: supports_conversion_general
      procedure :: supports_conversion_specific
      procedure :: set_geom
      procedure :: get_geom
      procedure :: set_regridder_param
      procedure :: get_horizontal_dims_spec
      procedure, nopass :: get_aspect_id

      procedure :: update_from_payload
      procedure :: update_payload
      procedure :: print_aspect
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

      if (present(regridder_param)) then
         allocate(aspect%regridder_param, source=regridder_param)
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
         if (src%is_mirror()) then
            matches = .false. ! need geom extension
         else
            matches = MAPL_SameGeom(src%geom, dst%geom) .and. (src%horizontal_dims_spec == dst%horizontal_dims_spec)
         end if
      class default
         matches = .false.
      end select

   end function matches

   function make_transform(src, dst, other_aspects, rc) result(transform)
      class(ExtensionTransform), allocatable :: transform
      class(GeomAspect), intent(in) :: src
      class(StateItemAspect), intent(in)  :: dst
      type(AspectMap), target, intent(in)  :: other_aspects
      integer, optional, intent(out) :: rc

      integer :: status
      type(GeomAspect) :: dst_
      type(EsmfRegridderParam) :: regridder_param

      allocate(transform,source=NullTransform()) ! just in case
      dst_ = to_GeomAspect(dst, _RC)

      deallocate(transform)

      if (src%is_mirror()) then
         allocate(transform, source=ExtendTransform())
      else
         regridder_param = get_regridder_param(src, dst_, _RC)
         allocate(transform, source=RegridTransform(src%geom, dst_%geom, regridder_param))
      end if

      _RETURN(_SUCCESS)
   end function make_transform

   function get_regridder_param(src_aspect, dst_aspect, rc) result(regridder_param)
      type(EsmfRegridderParam) :: regridder_param
      class(GeomAspect), intent(in) :: src_aspect
      class(GeomAspect), intent(in) :: dst_aspect
      integer, optional, intent(out) :: rc

      logical :: allocated_dst_rgdr_param
      logical :: allocated_src_rgdr_param

      allocated_dst_rgdr_param = allocated(dst_aspect%regridder_param)
      allocated_src_rgdr_param = allocated(src_aspect%regridder_param)

      if (allocated_dst_rgdr_param .and. allocated_src_rgdr_param) then
         _FAIL('both src and dst specified regridder params only one can')
      else if (allocated_dst_rgdr_param .and. (.not. allocated_src_rgdr_param)) then
         regridder_param = dst_aspect%regridder_param
      else if (allocated_src_rgdr_param .and. (.not. allocated_dst_rgdr_param)) then
         regridder_param = src_aspect%regridder_param
      else
         regridder_param = EsmfRegridderParam() ! default
      end if 
      _RETURN(_SUCCESS)
   end function get_regridder_param

   subroutine set_geom(this, geom)
      class(GeomAspect), intent(inout) :: this
      type(ESMF_Geom) :: geom

      this%geom = geom
      call this%set_mirror(.false.)
      
   end subroutine set_geom

   subroutine set_regridder_param(this, regridder_param)
      class(GeomAspect), intent(inout) :: this
      type(EsmfRegridderParam) :: regridder_param

      this%regridder_param = regridder_param
      
   end subroutine set_regridder_param

   function get_geom(this, rc) result(geom)
      class(GeomAspect), intent(in) :: this
      type(ESMF_Geom) :: geom
      integer, optional, intent(out) :: rc

      _ASSERT(allocated(this%geom), 'geom not allocated')
      geom = this%geom

      _RETURN(_SUCCESS)
   end function get_geom

   function get_horizontal_dims_spec(this, rc) result(horizontal_dims_spec)
      class(GeomAspect), intent(in) :: this
      integer, optional, intent(out) :: rc
      type(HorizontalDimsSpec) :: horizontal_dims_spec

      horizontal_dims_spec = this%horizontal_dims_spec

      _RETURN(_SUCCESS)
   end function get_horizontal_dims_spec

   subroutine connect_to_export(this, export, actual_pt, rc)
      class(GeomAspect), intent(inout) :: this
      class(StateItemAspect), intent(in) :: export
      type(ActualConnectionPt), intent(in) :: actual_pt
      integer, optional, intent(out) :: rc

      type(GeomAspect) :: export_
      integer :: status

      export_ = to_GeomAspect(export, _RC)
      this%geom = export_%geom

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(actual_pt)
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

   subroutine update_from_payload(this, field, bundle, state, rc)
      class(GeomAspect), intent(inout) :: this
      type(esmf_Field), optional, intent(in) :: field
      type(esmf_FieldBundle), optional, intent(in) :: bundle
      type(esmf_State), optional, intent(in) :: state
      integer, optional, intent(out) :: rc

      integer :: status

      _RETURN_UNLESS(present(field) .or. present(bundle))

      if (present(field)) then
         call mapl_FieldGet(field, geom=this%geom, _RC)
      else if (present(bundle)) then
         call mapl_FieldBundleGet(bundle, geom=this%geom, _RC)
      end if

      call this%set_mirror(.not. allocated(this%geom))

      _RETURN(_SUCCESS)
   end subroutine update_from_payload

   subroutine update_payload(this, field, bundle, state, rc)
      class(GeomAspect), intent(in) :: this
      type(esmf_Field), optional, intent(inout) :: field
      type(esmf_FieldBundle), optional, intent(inout) :: bundle
      type(esmf_State), optional, intent(inout) :: state
      integer, optional, intent(out) :: rc

      integer :: status

      _RETURN_UNLESS(present(field) .or. present(bundle))

      if (present(field)) then
         call mapl_FieldSet(field, geom=this%geom, _RC)
      else if (present(bundle)) then
         call mapl_FieldBundleSet(bundle, geom=this%geom, _RC)
      end if

      _RETURN(_SUCCESS)
   end subroutine update_payload

   subroutine print_aspect(this, file, line, rc)
      class(GeomAspect), intent(in) :: this
      character(*), intent(in) :: file
      integer, intent(in) :: line
      integer, optional, intent(out) :: rc

      _HERE, file, line, this%is_mirror(), allocated(this%geom)
         
      
      _RETURN(_SUCCESS)
   end subroutine print_aspect
   
end module mapl3g_GeomAspect
