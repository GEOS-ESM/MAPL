#include "MAPL_Generic.h"

module mapl3g_FieldBundleInfo
   use mapl3g_InfoUtilities
   use mapl3g_FieldInfo
   use mapl3g_UngriddedDims
   use mapl3g_VerticalStaggerLoc
   use mapl_KeywordEnforcer
   use mapl_ErrorHandling
   use esmf
   implicit none(type,external)
   private

   public :: MAPL_FieldBundleInfoGetInternal

   interface MAPL_FieldBundleInfoGetInternal
      procedure fieldbundle_get_internal
   end interface

   character(*), parameter :: KEY_FIELD_PROTOTYPE = '/field_prototype'

contains

   subroutine fieldbundle_get_internal(bundle_info, unusable, &
        num_levels, vert_staggerloc, num_vgrid_levels, &
        units, long_name, standard_name, &
        ungridded_dims, &
        typekind, &
        rc)

      type(ESMF_Info), intent(in) :: bundle_info
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: num_levels
      type(VerticalStaggerLoc), optional, intent(out) :: vert_staggerloc
      integer, optional, intent(out) :: num_vgrid_levels
      character(:), optional, allocatable, intent(out) :: units
      character(:), optional, allocatable, intent(out) :: long_name
      character(:), optional, allocatable, intent(out) :: standard_name
      type(UngriddedDims), optional, intent(out) :: ungridded_dims
      type(ESMF_TypeKind_Flag), optional, intent(out) :: typekind
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_Info) :: field_prototype_info
      character(:), allocatable :: typekind_str

      field_prototype_info = ESMF_InfoCreate(bundle_info, key=KEY_FIELD_PROTOTYPE, _RC)
      call MAPL_FieldInfoGetInternal(field_prototype_info, &
           num_levels=num_levels, vert_staggerloc=vert_staggerloc, num_vgrid_levels=num_vgrid_levels, &
           units=units, long_name=long_name, standard_name=standard_name, ungridded_dims=ungridded_dims, _RC)

      if (present(typekind)) then
         call ESMF_InfoGet(field_prototype_info, key=KEY_TYPEKIND, value=typekind_str, _RC)
         typekind = to_TypeKind(typekind_str)
      end if

      call ESMF_InfoDestroy(field_prototype_info, _RC)

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



end module mapl3g_FieldBundleInfo
