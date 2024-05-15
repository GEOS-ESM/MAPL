#include "MAPL_Generic.h"
module mapl3g_output_info

   use mapl3g_ungridded_dims_info
   use esmf, only: ESMF_Field, ESMF_FieldBundle, ESMF_Info, ESMF_InfoGet, ESMF_InfoGetCharAlloc, ESMF_InfoCreate, ESMF_InfoDestroy
   use Mapl_ErrorHandling

   implicit none
   private

   public :: get_num_levels
   public :: get_vertical_dim_spec_names
   public :: get_ungridded_dims_info
   public :: UngriddedDimInfoSet

   interface get_num_levels
      module procedure :: get_num_levels_bundle
   end interface get_num_levels

   interface get_vertical_dim_spec_names
      module procedure :: get_vertical_dim_spec_names_bundle
   end interface get_vertical_dim_spec_names

   interface get_ungridded_dims_info
      module procedure ::get_ungridded_dims_info_bundle 
   end interface get_ungridded_dims_info

   character(len=*), parameter :: PREFIX = 'MAPL/'
   character(len=*), parameter :: KEY_UNGRID_DIM = PREFIX // 'ungridded_dims'
   character(len=*), parameter :: KEY_VERT_DIM = PREFIX // 'vertical_dim'
   character(len=*), parameter :: KEY_VERT_GEOM = PREFIX // 'vertical_geom'
   character(len=*), parameter :: KEY_UNITS = PREFIX // 'units'
   character(len=*), parameter :: KEY_VLOC = 'vloc'
   character(len=*), parameter :: KEY_NUM_LEVELS = 'num_levels'

contains

   integer function get_num_levels_bundle(bundle, rc) result(num)
      integer :: num
      type(ESMF_FieldBundle), intent(in) :: bundle
      integer, optional, intent(out) :: rc
      integer :: status
      type(ESMF_Field), allocatable :: fields(:)
      integer :: nums(:)
      integer :: sz

      fields = get_bundle_fields(bundle, _RC)
      sz = size(fields)
      _ASSERT(sz > 0, 'Empty ESMF_FieldBundle')
      num = get_num_levels_field(fields(1), _RC)
      _RETURN_IF(sz == 1)
      nums = get_num_levels_field(fields(2:sz), _RC)
      _ASSERT(all(nums == num), 'All fields must have the same number of vertical levels.')

   end function get_num_levels_bundle

   elemental integer function get_num_levels_field(field, rc) result(n)
      type(ESMF_Field), intent(in) :: field
      integer, optional, intent(out) :: rc
      integer :: status
      type(ESMF_Info) :: info

      call ESMF_InfoGetFromHost(field, info, _RC)
      n = get_num_levels_info(info, _RC)
      call ESMF_InfoDestroy(info, _RC)
      _RETURN(_SUCCESS)

   end function get_num_levels_field

   elemental integer function get_num_levels_info(info, rc) result(n)
      type(ESMF_Info), intent(in) :: info
      integer, optional, intent(out) :: rc
      integer :: status
      type(ESMF_Info) :: inner_info

      inner_info = ESMF_InfoCreate(info, key=KEY_VERT_GEOM, _RC)
      call ESMF_InfoGet(inner_info, key=KEY_NUM_LEVELS, value=n, _RC)
      call ESMF_InfoDestroy(inner_info, _RC)
      _RETURN(_SUCCESS)
      
   end function get_num_levels_info

   function get_vertical_dim_spec_names_bundle(bundle, rc) result(names)
      type(StringSet) :: names
      type(ESMF_FieldBundle), intent(in) :: bundle
      integer, optional, intent(out) :: rc
      integer :: status
      type(ESMF_Field), allocatable :: fields(:)
      integer :: sz, i
      character(len=:), allocatable :: name

      fields = get_bundle_fields(bundle, _RC)
      sz = size(fields)
      _ASSERT(sz > 0, 'Empty ESMF_FieldBundle')

      names = StringSet()
      do i=1, sz
         name = get_vertical_dim_spec_name_field(field, _RC)
         call names%insert(name)
      end do

   end function get_vertical_dim_spec_names_bundle

   elemental function get_vertical_dim_spec_name_field(field, rc) result(spec_name)
      character(len=:), allocatable :: spec_name
      type(ESMF_Field), intent(inout) :: field
      integer, optional, intent(out) :: rc
      integer :: status
      type(ESMF_Info) :: info

      call ESMF_InfoGetFromHost(field, info, _RC)
      spec_name = get_vertical_dim_spec_name_info(info, _RC)
      call ESMF_InfoDestroy(info, _RC)
      _RETURN(_SUCCESS)

   end function get_vertical_dim_spec_name_field

   elemental function get_vertical_dim_spec_name_info(info, rc) result(spec_name)
      character(len=:), allocatable :: spec_name
      type(ESMF_Info), intent(in) :: info
      integer, optional, intent(out) :: rc
      integer :: status
      type(ESMF_Info) :: inner_info

      inner_info = ESMF_InfoCreate(info, key=KEY_VERT_DIM, _RC)
      call ESMF_InfoGetCharAlloc(inner_info, key=KEY_VLOC, value=spec_name, _RC)
      call ESMF_InfoDestroy(inner_info, _RC)
      _RETURN(_SUCCESS)

   end function get_vertical_dim_spec_name_info

   function get_ungridded_dims_info_bundle(bundle, rc) result(dim_info_set)
      type(UngriddedDimInfoSet) :: dim_info_set
      type(ESMF_FieldBundle), intent(in) :: bundle
      integer, optional, intent(out) :: rc
      integer :: status
      type(ESMF_Field), allocatable :: fields(:)
      type(UngriddedDimsInfo), allocatable :: dims_info(:)
      integer :: i

      fields = get_bundle_fields(bundle, _RC)
      _ASSERT(size(fields) > 0, 'Empty ESMF_FieldBundle')

      dims_info = get_ungridded_dims_info_field(fields, _RC)
      do i=1, size(fields)
         call dim_info_set%merge(dims_info(i)%as_set())
      end do
      _RETURN(_SUCCESS)

   end function get_ungridded_dims_info_bundle

   elemental function get_ungridded_dims_info_field(field, rc) result(ungridded)
      type(UngriddedDimsInfo) :: ungridded
      type(ESMF_Field), intent(inout) :: field
      integer, optional, intent(out) :: rc
      integer :: status
      type(ESMF_Info) :: info

      call ESMF_InfoGetFromHost(field, info, _RC)
      ungridded = get_ungridded_dims_info_info(info, _RC)
      call ESMF_InfoDestroy(info, _RC)
      _RETURN(_SUCCESS)

   end function get_ungridded_dims_info_field

   elemental function get_ungridded_dims_info_info(info, rc) result(ungridded)
      type(UngriddedDimsInfo) :: ungridded
      type(ESMF_Info), intent(in) :: info
      integer, optional, intent(out) :: rc
      integer :: status
      type(ESMF_Info) :: inner_info

      inner_info = ESMF_InfoCreate(info, key=KEY_UNGRID_DIM, _rc)
      ungridded = get_ungridded_dims_info(inner_info, _rc)
      call ESMF_InfoDestroy(inner_info, _rc)
      _RETURN(_SUCCESS)

   end function get_ungridded_dims_info_info

   function get_bundle_fields(bundle, rc) result(fields)
      type(ESMF_Field), allocatable :: fields(:)
      type(ESMF_FieldBundle), intent(in) :: bundle
      integer, optional, intent(out) :: rc
      integer :: status
      integer :: field_count

      call ESMF_FieldBundleGet(bundle, fieldCount=field_count, _RC)
      allocate(fields(field_count))
      call ESMF_FieldBundleGet(bundle, fieldList=fields, _RC)

      _RETURN(_SUCCESS)

   end function get_bundle_fields

end module mapl3g_output_info
