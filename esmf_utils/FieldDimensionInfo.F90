#include "MAPL_Generic.h"
module mapl3g_FieldDimensionInfo
   use mapl3g_InfoUtilities
   use mapl3g_UngriddedDim
   use mapl3g_UngriddedDimVector
   use mapl3g_UngriddedDims
   use mapl3g_esmf_info_keys
   use gFTL2_StringVector
   use esmf, only: ESMF_Field, ESMF_FieldBundle, ESMF_FieldBundleGet
   use esmf, only: ESMF_Info, ESMF_InfoIsPresent, ESMF_InfoGet
   use esmf, only: ESMF_InfoDestroy, ESMF_InfoCreate
   use esmf, only: ESMF_InfoPrint
   use Mapl_ErrorHandling

   implicit none (type, external)

   private

   public :: get_num_levels
   public :: get_vertical_dim_spec_names
   public :: get_vertical_dim_spec_name
   public :: get_ungridded_dims
   public :: get_num_levels_bundle_info
   public :: get_vertical_dim_spec_names_bundle_info
   public :: get_ungridded_dims_bundle_info

   interface get_num_levels
      module procedure :: get_num_levels_bundle
      module procedure :: get_num_levels_field
   end interface get_num_levels

   interface get_vertical_dim_spec_names
      module procedure :: get_vertical_dim_spec_names_bundle
   end interface get_vertical_dim_spec_names

   interface get_vertical_dim_spec_name
      module procedure :: get_vertical_dim_spec_name_field
   end interface get_vertical_dim_spec_name

   interface get_ungridded_dims
      module procedure :: get_ungridded_dims_bundle
      module procedure :: get_ungridded_dims_field
   end interface get_ungridded_dims

   character(len=*), parameter :: VERT_DIM_NONE = 'VERTICAL_DIM_NONE'

contains

   integer function get_num_levels_bundle(bundle, rc) result(num)
      type(ESMF_FieldBundle), intent(in) :: bundle
      integer, optional, intent(out) :: rc
      integer :: status
      type(ESMF_Info), allocatable :: info(:)

      info = create_bundle_info(bundle, _RC)
      num = get_num_levels_bundle_info(info, _RC)
      _RETURN(_SUCCESS)

   end function get_num_levels_bundle

   integer function get_num_levels_bundle_info(infos, rc) result(num)
      type(ESMF_Info), intent(in) :: infos(:)
      integer, optional, intent(out) :: rc
      integer :: status
      integer :: i, n

      num = 0
      do i=1, size(infos)
         n = get_num_levels_info(infos(i), _RC)
         num = max(num, n)
         if(n == 0) cycle
         _ASSERT(n == num, 'Fields with vertical levels must have the same number of levels.')
      end do
      _RETURN(_SUCCESS)

   end function get_num_levels_bundle_info

   integer function get_num_levels_field(field, rc) result(num)
      type(ESMF_Field), intent(in) :: field
      integer, optional, intent(out) :: rc
      integer :: status
      type(ESMF_Info) :: info

      info = MAPL_InfoCreateFromInternal(field, _RC)
      num = get_num_levels_info(info, _RC)
      call ESMF_InfoDestroy(info, _RC)

      _RETURN(_SUCCESS)
   end function get_num_levels_field

   integer function get_num_levels_info(info, rc) result(num)
      type(ESMF_Info), intent(in) :: info
      integer, optional, intent(out) :: rc
      integer :: status
      character(len=:), allocatable :: spec_name

      num = 0
      spec_name = get_vertical_dim_spec_info(info, _RC)
      _RETURN_IF(spec_name == VERT_DIM_NONE)
      call MAPL_InfoGet(info, key=KEY_NUM_LEVELS, value=num, _RC)

      _RETURN(_SUCCESS)
   end function get_num_levels_info

   function get_vertical_dim_spec_names_bundle(bundle, rc) result(names)
      type(StringVector) :: names
      type(ESMF_FieldBundle), intent(in) :: bundle
      integer, optional, intent(out) :: rc
      integer :: status
      type(ESMF_Info), allocatable :: info(:)

      info = create_bundle_info(bundle, _RC)
      names = get_vertical_dim_spec_names_bundle_info(info, _RC)
      _RETURN(_SUCCESS)

   end function get_vertical_dim_spec_names_bundle

   function get_vertical_dim_spec_names_bundle_info(info, rc) result(names)
      type(StringVector) :: names
      type(ESMF_Info), intent(in) :: info(:)
      integer, optional, intent(out) :: rc
      integer :: status
      integer :: i
      character(len=:), allocatable :: spec_name

      names = StringVector()
      do i=1, size(info)
         spec_name = get_vertical_dim_spec_info(info(i), _RC)
         if(find_index(names, spec_name) == 0) call names%push_back(spec_name)
      end do
      _RETURN(_SUCCESS)

   end function get_vertical_dim_spec_names_bundle_info

   function get_vertical_dim_spec_name_field(field, rc) result(spec_name)
      character(len=:), allocatable :: spec_name
      type(ESMF_Field), intent(in) :: field
      integer, optional, intent(out) :: rc
      integer :: status
      type(ESMF_Info) :: info

      info = MAPL_InfoCreateFromInternal(field, _RC)
      spec_name = get_vertical_dim_spec_info(info, _RC)
      call ESMF_InfoDestroy(info, _RC)

      _RETURN(_SUCCESS)
   end function get_vertical_dim_spec_name_field

   function get_vertical_dim_spec_info(info, rc) result(spec_name)
      character(len=:), allocatable :: spec_name
      type(ESMF_Info), intent(in) :: info
      integer, optional, intent(out) :: rc
      integer :: status
      logical :: isPresent

      call MAPL_InfoGet(info, key=KEY_VLOC, value=spec_name, _RC)
      isPresent = ESMF_InfoIsPresent(info, key=KEY_VLOC, _RC)

      _RETURN(_SUCCESS)
   end function get_vertical_dim_spec_info

   function get_ungridded_dims_bundle(bundle, rc) result(dims)
      type(UngriddedDims) :: dims
      type(ESMF_FieldBundle), intent(in) :: bundle
      integer, optional, intent(out) :: rc
      integer :: status
      type(ESMF_Info), allocatable :: info(:)
      type(UngriddedDimVector) :: vec

      info = create_bundle_info(bundle, _RC)
      vec = get_ungridded_dims_bundle_info(info, _RC)
      dims = UngriddedDims(vec)
      _RETURN(_SUCCESS)

   end function get_ungridded_dims_bundle

   function get_ungridded_dims_bundle_info(info, rc) result(vec)
      type(UngriddedDimVector) :: vec
      type(ESMF_Info), intent(in) :: info(:)
      integer, optional, intent(out) :: rc
      integer :: status
      integer :: i
      type(UngriddedDims) :: dims

      do i=1, size(info)
         dims = make_ungriddedDims(info(i), key=KEY_UNGRIDDED_DIMS, _RC)
         call merge_ungridded_dims(vec, dims, rc)
      end do
      _RETURN(_SUCCESS)

   end function get_ungridded_dims_bundle_info

   function get_ungridded_dims_field(field, rc) result(ungridded)
      type(UngriddedDims) :: ungridded
      type(ESMF_Field), intent(in) :: field
      integer, optional, intent(out) :: rc
      integer :: status
      type(ESMF_Info) :: info

      info = MAPL_InfoCreateFromInternal(field, _RC)
      ungridded = make_UngriddedDims(info, key=KEY_UNGRIDDED_DIMS, _RC)
      call ESMF_InfoDestroy(info, _RC)

      _RETURN(_SUCCESS)
   end function get_ungridded_dims_field


   subroutine merge_ungridded_dims(vec, dims, rc)
      class(UngriddedDimVector), intent(inout) :: vec
      class(UngriddedDims), intent(in) :: dims
      integer, optional, intent(out) :: rc
      integer :: status
      integer :: i

      do i = 1, dims%get_num_ungridded()
         call check_duplicate(vec, dims%get_ith_dim_spec(i), _RC)
         call vec%push_back(dims%get_ith_dim_spec(i), _RC)
      end do
      _RETURN(_SUCCESS)

   end subroutine merge_ungridded_dims

   integer function find_index(v, name) result(i)
      class(StringVector), intent(in) :: v
      character(len=*), intent(in) :: name
      type(StringVectorIterator) :: iter

      i = 0
      iter = v%begin()
      do while (iter /= v%end())
         i = i+1
         if(iter%of() == name) return
         call iter%next()
      end do
      i = 0

   end function find_index

   subroutine check_duplicate(vec, udim, rc)
      class(UngriddedDimVector), intent(in) :: vec
      class(UngriddedDim), intent(in) :: udim
      integer, optional, intent(out) :: rc
      type(UngriddedDimVectorIterator) :: iter
      type(UngriddedDim) :: vdim

      iter = vec%ftn_begin()
      do while(iter < vec%ftn_end())
         call iter%next()
         vdim = iter%of()
         if(udim%get_name() /= vdim%get_name()) cycle
         _ASSERT(udim == vdim, 'UngriddedDim mismatch.')
      end do

      _RETURN(_SUCCESS)

   end subroutine check_duplicate

   function create_bundle_info(bundle, rc) result(bundle_info)
      type(ESMF_Info), allocatable :: bundle_info(:)
      type(ESMF_FieldBundle), intent(in) :: bundle
      integer, optional, intent(out) :: rc

      integer :: status
      integer :: field_count, i
      type(ESMF_Field), allocatable :: fields(:)
      type(ESMF_Info) :: info

      status = 0
      call ESMF_FieldBundleGet(bundle, fieldCount=field_count, _RC)
      _ASSERT(field_count > 0, 'Empty bundle')
      allocate(fields(field_count))
      call ESMF_FieldBundleGet(bundle, fieldList=fields, _RC)
      allocate(bundle_info(field_count))
      do i=1, field_count
         bundle_info(i) = MAPL_InfoCreateFromInternal(fields(i), _RC)
      end do
      _RETURN(_SUCCESS)

   end function create_bundle_info

   subroutine destroy_bundle_info(bundle_info, rc)
      type(ESMF_Info), intent(inout) :: bundle_info(:)
      integer, optional, intent(out) :: rc
      integer :: status, i

      do i=1, size(bundle_info)
         call ESMF_InfoDestroy(bundle_info(i), _RC)
      end do
      _RETURN(_SUCCESS)

   end subroutine destroy_bundle_info

end module mapl3g_FieldDimensionInfo
