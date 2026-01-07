#include "MAPL.h"

module mapl3g_FieldBundleInfo
   use mapl3g_esmf_info_keys
   use mapl3g_InfoUtilities
   use mapl3g_ESMF_Info_Keys
   use mapl3g_Field_API
   use mapl3g_FieldInfo
   use mapl3g_UngriddedDims
   use mapl3g_FieldBundleType_Flag
   use mapl3g_VerticalGrid_API
   use mapl_KeywordEnforcer
   use mapl_ErrorHandling
   use esmf
   implicit none(type,external)
   private

   public :: FieldBundleInfoGetInternal
   public :: FieldBundleInfoSetInternal

   interface FieldBundleInfoGetInternal
      procedure fieldbundle_get_internal
   end interface

   interface FieldBundleInfoSetInternal
      procedure fieldbundle_set_internal
   end interface

   character(*), parameter :: KEY_FIELDBUNDLETYPE_FLAG = '/FieldBundleType_Flag'
   character(*), parameter :: KEY_ALLOCATION_STATUS = "/allocation_status"
   character(*), parameter :: KEY_HAS_GEOM = "/has_geom"

contains

   subroutine fieldbundle_get_internal(info, unusable, &
        namespace, &
        vgrid_id, &
        fieldBundleType, &
        typekind, interpolation_weights, &
        ungridded_dims, num_levels, vert_staggerloc, num_vgrid_levels, &
        units, long_name, standard_name, &
        allocation_status, &
        spec_handle, &
        bracket_updated, &
        has_geom, &
        has_deferred_aspects, &
        regridder_param_info, &
        rc)

      type(ESMF_Info), intent(in) :: info
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: vgrid_id
      character(*), optional, intent(in) :: namespace
      type(FieldBundleType_Flag), optional, intent(out) :: fieldBundleType
      type(ESMF_TypeKind_Flag), optional, intent(out) :: typekind
      real(kind=ESMF_KIND_R4), optional, allocatable, intent(out) :: interpolation_weights(:)
      type(UngriddedDims), optional, intent(out) :: ungridded_dims
      integer, optional, intent(out) :: num_levels
      type(VerticalStaggerLoc), optional, intent(out) :: vert_staggerloc
      integer, optional, intent(out) :: num_vgrid_levels
      character(:), optional, allocatable, intent(out) :: units
      character(:), optional, allocatable, intent(out) :: long_name
      character(:), optional, allocatable, intent(out) :: standard_name
      type(StateItemAllocation), optional, intent(out) :: allocation_status
      integer, optional, allocatable, intent(out) :: spec_handle(:)
      logical, optional, intent(out) :: bracket_updated
      logical, optional, intent(out) :: has_geom
      logical, optional, intent(out) :: has_deferred_aspects
      type(esmf_Info), optional, intent(out) :: regridder_param_info
      integer, optional, intent(out) :: rc

      integer :: status
      character(:), allocatable :: typekind_str
      character(:), allocatable :: fieldBundleType_str, allocation_status_str
      character(:), allocatable :: namespace_

      namespace_ = INFO_INTERNAL_NAMESPACE
      if (present(namespace)) then
         namespace_ = namespace
      end if

     if (present(fieldBundleType)) then
         call ESMF_InfoGetCharAlloc(info, key=namespace_//KEY_FIELDBUNDLETYPE_FLAG, value=fieldBundleType_str, _RC)
         fieldBundleType = FieldBundleType_Flag(fieldBundleType_str)
      end if

      if (present(interpolation_weights)) then
         call ESMF_InfoGetAlloc(info, key=namespace_//KEY_INTERPOLATION_WEIGHTS, values=interpolation_weights, _RC)
      end if

      ! Fields have a type-kind, but FieldBundle's do not, so we need to store typekind here
      if (present(typekind)) then
         call MAPL_InfoGet(info, key=namespace_//KEY_TYPEKIND, value=typekind_str, _RC)
         typekind = to_TypeKind(typekind_str)
      end if

      if (present(allocation_status)) then
         call MAPL_InfoGet(info, key=namespace_//KEY_ALLOCATION_STATUS, value=allocation_status_str, _RC)
         allocation_status = StateItemAllocation(allocation_status_str)
      end if

      if (present(bracket_updated)) then
         call ESMF_InfoGet(info, key=namespace_//KEY_BRACKET_UPDATED, value=bracket_updated, _RC)
      end if

      if (present(has_geom)) then
         call ESMF_InfoGet(info, key=namespace_//KEY_HAS_GEOM, value=has_geom, default=.false., _RC)
      end if

      ! Field-prototype items that come from field-info
      call FieldInfoGetInternal(info, namespace = namespace_//KEY_FIELD_PROTOTYPE, &
           ungridded_dims=ungridded_dims, &
           num_levels=num_levels, vert_staggerloc=vert_staggerloc, num_vgrid_levels=num_vgrid_levels, &
           units=units, long_name=long_name, standard_name=standard_name, spec_handle=spec_handle, &
           vgrid_id=vgrid_id, &
           has_deferred_aspects=has_deferred_aspects, &
           regridder_param_info=regridder_param_info, &
           _RC)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)

   contains

      function to_TypeKind(typekind_str) result(typekind)
          type(ESMF_TypeKind_Flag) :: typekind
          character(*), intent(in) :: typekind_str
          
          select case (typekind_str)
          case ('R8')
             typekind = ESMF_TYPEKIND_R8
          case ('R4')
             typekind = ESMF_TYPEKIND_R4
          case default
             typekind = ESMF_NOKIND
          end select

       end function to_TypeKind

   end subroutine fieldbundle_get_internal


   subroutine fieldbundle_set_internal(info, unusable, &
        namespace, &
        fieldBundleType, typekind, interpolation_weights, &
        ungridded_dims, &
        num_levels, vert_staggerloc, &
        units, standard_name, long_name, &
        allocation_status, &
        vgrid_id, &
        spec_handle, &
        bracket_updated, &
        has_geom, &
        has_deferred_aspects, &
        regridder_param_info, &
        rc)

      type(ESMF_Info), intent(inout) :: info
      class(KeywordEnforcer), optional, intent(in) :: unusable
      character(*), optional, intent(in) :: namespace
      type(FieldBundleType_Flag), optional, intent(in) :: fieldBundleType
      type(ESMF_TypeKind_Flag), optional, intent(in) :: typekind
      real(ESMF_KIND_R4), optional, intent(in) :: interpolation_weights(:)
      type(UngriddedDims), optional, intent(in) :: ungridded_dims
      integer, optional, intent(in) :: num_levels
      type(VerticalStaggerLoc), optional, intent(in) :: vert_staggerloc
      character(*), optional, intent(in) :: units
      character(*), optional, intent(in) :: standard_name
      character(*), optional, intent(in) :: long_name
      type(StateItemAllocation), optional, intent(in) :: allocation_status
      integer, optional, intent(in) :: vgrid_id
      integer, optional, intent(in) :: spec_handle(:)
      logical, optional, intent(in) :: bracket_updated
      logical, optional, intent(in) :: has_geom
      logical, optional, intent(in) :: has_deferred_aspects
      type(esmf_info), optional, intent(in) :: regridder_param_info
      integer, optional, intent(out) :: rc
      
      integer :: status
      character(:), allocatable :: typekind_str
      character(:), allocatable :: fieldBundleType_str
      character(:), allocatable :: namespace_

      namespace_ = INFO_INTERNAL_NAMESPACE
      if (present(namespace)) then
         namespace_ = namespace
      end if

      if (present(typekind)) then
         typekind_str = to_string(typekind)
         call ESMF_InfoSet(info, key=namespace_ // KEY_TYPEKIND, value=typekind_str, _RC)
      end if

      if (present(allocation_status)) then
         call ESMF_InfoSet(info, key=namespace_ // KEY_ALLOCATION_STATUS, value=allocation_status%to_string(), _RC)
      end if

      if (present(fieldBundleType)) then
         fieldBundleType_str = fieldBundleType%to_string()
         call ESMF_InfoSet(info, key=namespace_ // KEY_FIELDBUNDLETYPE_FLAG, value=fieldBundleType_str, _RC)
      end if

      if (present(interpolation_weights)) then
         call ESMF_InfoSet(info, key=namespace_ // KEY_INTERPOLATION_WEIGHTS, values=interpolation_weights, _RC)
      end if

      if (present(bracket_updated)) then
         call ESMF_InfoSet(info, key=namespace_ // KEY_BRACKET_UPDATED, value=bracket_updated, _RC)
      end if

      if (present(has_geom)) then
         call ESMF_InfoSet(info, key=namespace_ // KEY_HAS_GEOM, value=has_geom, _RC)
      end if

       call FieldInfoSetInternal(info, namespace=namespace_ // KEY_FIELD_PROTOTYPE, &
           ungridded_dims=ungridded_dims, &
           num_levels=num_levels, vert_staggerloc=vert_staggerloc, &
           units=units, long_name=long_name, standard_name=standard_name, &
           vgrid_id=vgrid_id, &
           spec_handle=spec_handle, &
           has_deferred_aspects=has_deferred_aspects, &
           regridder_param_info=regridder_param_info, &
           _RC)

       _RETURN(_SUCCESS)
       _UNUSED_DUMMY(unusable)

   contains

      function to_string(typekind)
         type(ESMF_TypeKind_Flag), intent(in) :: typekind
         character(:), allocatable :: to_string

         if (typekind == ESMF_TYPEKIND_R8) then
            to_string = 'R8'
         elseif (typekind == ESMF_TYPEKIND_R4) then
            to_string = 'R4'
         elseif (typekind == ESMF_TYPEKIND_I8) then
            to_string = 'I8'
         elseif (typekind == ESMF_TYPEKIND_I4) then
            to_string = 'I4'
         elseif (typekind == ESMF_TYPEKIND_LOGICAL) then
            to_string = 'LOGICAL'
         elseif (typekind == ESMF_TYPEKIND_CHARACTER) then
            to_string = 'CHARACTER'
         else
            to_string = 'NOKIND'
         end if
      end function to_string

             
   end subroutine fieldbundle_set_internal

end module mapl3g_FieldBundleInfo
