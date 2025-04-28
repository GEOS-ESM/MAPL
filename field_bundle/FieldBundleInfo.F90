#include "MAPL_Generic.h"

module mapl3g_FieldBundleInfo
   use mapl3g_esmf_info_keys
   use mapl3g_InfoUtilities
   use mapl3g_ESMF_Info_Keys
   use mapl3g_FieldInfo
   use mapl3g_UngriddedDims
   use mapl3g_FieldBundleType_Flag
   use mapl3g_VerticalStaggerLoc
   use mapl_KeywordEnforcer
   use mapl_ErrorHandling
   use esmf
   implicit none(type,external)
   private

   public :: MAPL_FieldBundleInfoGetInternal
   public :: MAPL_FieldBundleInfoSetInternal

   interface MAPL_FieldBundleInfoGetInternal
      procedure fieldbundle_get_internal
   end interface

   interface MAPL_FieldBundleInfoSetInternal
      procedure fieldbundle_set_internal
   end interface

   character(*), parameter :: KEY_FIELDBUNDLETYPE_FLAG = '/FieldBundleType_Flag'


contains

   subroutine fieldbundle_get_internal(info, unusable, &
        namespace, &
        fieldBundleType, &
        typekind, interpolation_weights, &
        ungridded_dims, num_levels, vert_staggerloc, num_vgrid_levels, &
        units, long_name, standard_name, &
        is_active, &
        rc)

      type(ESMF_Info), intent(in) :: info
      class(KeywordEnforcer), optional, intent(in) :: unusable
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
      logical, optional, intent(out) :: is_active
      integer, optional, intent(out) :: rc

      integer :: status
      character(:), allocatable :: typekind_str
      character(:), allocatable :: fieldBundleType_str
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

      ! Field-prototype items that come from field-info
      call MAPL_FieldInfoGetInternal(info, namespace = namespace_//KEY_FIELD_PROTOTYPE, &
           ungridded_dims=ungridded_dims, &
           num_levels=num_levels, vert_staggerloc=vert_staggerloc, num_vgrid_levels=num_vgrid_levels, &
           units=units, long_name=long_name, standard_name=standard_name, is_active=is_active, _RC)


      _RETURN(_SUCCESS)
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
         geom, &
        fieldBundleType, typekind, interpolation_weights, &
        ungridded_dims, &
        num_levels, vert_staggerloc, &
        units, standard_name, long_name, &
        is_active, &
        rc)

      type(ESMF_Info), intent(inout) :: info
      class(KeywordEnforcer), optional, intent(in) :: unusable
      character(*), optional, intent(in) :: namespace
      type(ESMF_Geom), optional, intent(in) :: geom
      type(FieldBundleType_Flag), optional, intent(in) :: fieldBundleType
      type(ESMF_TypeKind_Flag), optional, intent(in) :: typekind
      real(ESMF_KIND_R4), optional, intent(in) :: interpolation_weights(:)
      type(UngriddedDims), optional, intent(in) :: ungridded_dims
      integer, optional, intent(in) :: num_levels
      type(VerticalStaggerLoc), optional, intent(in) :: vert_staggerloc
      character(*), optional, intent(in) :: units
      character(*), optional, intent(in) :: standard_name
      character(*), optional, intent(in) :: long_name
      logical, optional, intent(in) :: is_active
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

      if (present(fieldBundleType)) then
         fieldBundleType_str = fieldBundleType%to_string()
         call ESMF_InfoSet(info, key=namespace_ // KEY_FIELDBUNDLETYPE_FLAG, value=fieldBundleType_str, _RC)
      end if

      if (present(interpolation_weights)) then
         call ESMF_InfoSet(info, key=namespace_ // KEY_INTERPOLATION_WEIGHTS, values=interpolation_weights, _RC)
      end if

       call MAPL_FieldInfoSetInternal(info, namespace=namespace_ // KEY_FIELD_PROTOTYPE, &
           ungridded_dims=ungridded_dims, &
           num_levels=num_levels, vert_staggerloc=vert_staggerloc, &
           units=units, long_name=long_name, standard_name=standard_name, &
           is_active=is_active, _RC)

      _RETURN(_SUCCESS)

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
