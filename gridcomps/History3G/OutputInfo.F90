#include "MAPL_Generic.h"
module mapl3g_output_info

   use mapl3g_ESMF_Info_Keys
   use mapl3g_UngriddedDims
   use mapl3g_UngriddedDim
   use gFTL2_StringVector
   use esmf, only: ESMF_Field, ESMF_FieldBundle
   use esmf, only: ESMF_Info, ESMF_InfoCreate, ESMF_InfoDestroy
   use esmf, only: ESMF_InfoGet, ESMF_InfoGetCharAlloc
   use Mapl_ErrorHandling

   implicit none

   private

   public :: get_num_levels
   public :: get_vertical_dim_spec_names
   public :: get_vertical_dim_spec_name
   public :: get_ungridded_dims

   interface get_num_levels
      module procedure :: get_num_levels_bundle
      module procedure :: get_num_levels_field
   end interface get_num_levels

   interface get_vertical_dim_spec_names
      module procedure :: get_vertical_dim_spec_names_bundle
   end interface get_vertical_dim_spec_names

   interface get_ungridded_dims
      module procedure :: get_ungridded_dim_bundle 
      module procedure :: get_ungridded_dims_field
   end interface get_ungridded_dims

   interface get_vertical_dim_spec_name
      module procedure :: get_vertical_dim_spec_name_field
   end interface get_vertical_dim_spec_name

contains

   integer function get_num_levels_bundle(bundle, rc) result(num)
      integer :: num
      type(ESMF_FieldBundle), intent(in) :: bundle
      integer, optional, intent(out) :: rc
      integer :: status
      integer :: i, n
      type(ESMF_Info), allocatable :: info(:)

      info = get_bundle_info(bundle, _RC)
      num = get_num_levels_info(info(1), _RC)
      do i=2, size(info)
         n = get_num_levels_info(info(i), _RC)
         _ASSERT(n == num, 'All fields must have the same number of vertical levels.')
      end do
      call destroy_info(info, _RC)
      _RETURN(_SUCCESS)

   end function get_num_levels_bundle

   integer function get_num_levels_field(field, rc) result(num)
      type(ESMF_Field), intent(in) :: field
      integer, optional, intent(out) :: rc
      integer :: status
      type(ESMF_Info) :: info

      call ESMF_InfoGetFromHost(field, info, _RC)
      num = get_num_levels_info(info, _RC)
      call ESMF_InfoDestroy(info, _RC)
      _RETURN(_SUCCESS)

   end function get_num_levels_field

   integer function get_num_levels_info(info, rc) result(num)
      type(ESMF_Info), intent(in) :: info
      integer, optional, intent(out) :: rc
      integer :: status
      logical :: key_present

      num = 0
      key_present = ESMF_InfoIsPresent(info, key=KEY_NUM_LEVELS, _RC)
      if(key_present) then
         call ESMF_InfoGet(info, key=KEY_NUM_LEVELS, value=num, _RC)
      end if
      _RETURN(_SUCCESS)
      
   end function get_num_levels_info

   function get_vertical_dim_spec_names_bundle(bundle, rc) result(names)
      type(StringVector) :: names
      type(ESMF_FieldBundle), intent(in) :: bundle
      integer, optional, intent(out) :: rc
      integer :: status
      integer :: i
      character(len=:), allocatable :: name
      type(ESMF_Info), allocatable :: info(:)

      info = get_bundle_info(bundle, _RC)
      names = StringVector()
      do i=1, size(info)
         name = get_vertical_dim_spec_info(info(i), _RC)
         if(names%get_index(name)==0) names%push_back(name)
      end do
      call destroy_bundle_info(info, _RC)
      _RETURN(_SUCCESS)

   end function get_vertical_dim_spec_names_bundle

   function get_vertical_dim_spec_name_field(field, rc) result(spec_name)
      character(len=:), allocatable :: spec_name
      type(ESMF_Field), intent(inout) :: field
      integer, optional, intent(out) :: rc
      integer :: status
      type(ESMF_Info) :: info

      call ESMF_InfoGetFromHost(field, info, _RC)
      spec_name = get_vertical_dim_spec_info(info, _RC)
      call ESMF_InfoDestroy(info, _RC)
      _RETURN(_SUCCESS)

   end function get_vertical_dim_spec_name_field

   function get_vertical_dim_spec_info(info, rc) result(spec_name)
      character(len=:), allocatable :: spec_name
      type(ESMF_Info), intent(in) :: info
      integer, optional, intent(out) :: rc
      integer :: status
      integer :: n

      spec_name = ''
      n = get_num_levels_info(info, _RC) 
      _RETURN_UNLESS(n > 0)
      call ESMF_InfoGetCharAlloc(info, key=KEY_VLOC, value=spec_name, _RC)
      _RETURN(_SUCCESS)

   end function get_vertical_dim_spec_info

   function get_ungridded_dim_bundle(bundle, rc) result(dims)
      type(UngriddedDims) :: dims
      type(ESMF_FieldBundle), intent(in) :: bundle
      integer, optional, intent(out) :: rc
      integer :: status
      integer :: i
      type(ESMF_Info), allocatable :: info(:)
      type(UngriddedDimVector) :: vec

      info = get_bundle_info(bundle, _RC)
      vec = UngriddedDimVector()
      do i=1, size(info)
         call push_ungridded_dim_info(vec, info(i), _RC)
      end do
      dims = UngriddedDims(vec)
      call destroy_bundle_info(info, _RC)
      _RETURN(_SUCCESS)

   end function get_ungridded_dim_bundle

   function get_ungridded_dims_field(field, rc) result(ungridded)
      type(UngriddedDims) :: ungridded
      type(ESMF_Field), intent(inout) :: field
      integer, optional, intent(out) :: rc
      integer :: status
      type(ESMF_Info) :: info
      type(UngriddedDimVector) :: vec

      call ESMF_InfoGetFromHost(field, info, _RC)
      call push_ungridded_info(vec, info, _RC)
      ungridded = UngriddedDims(vec)
      call ESMF_InfoDestroy(info, _RC)
      _RETURN(_SUCCESS)

   end function get_ungridded_dims_field

   subroutine push_ungridded_dim_info(vec, info, rc)
      type(UngriddedDimVector), intent(inout) :: vec
      type(ESMF_Info), intent(in) :: info
      integer, optional, intent(out) :: rc 
      integer :: status
      type(UngriddedDim) :: next
      integer :: num_dims, i, vi
      logical :: has_dims
      integer :: num_coord
      character(len=:), allocatable :: name
      character(len=:), allocatable :: units
      character(len=:), allocatable :: dim_key
      real, allocatable :: coordinates(:)

      num_dims = 0
      has_dims = ESMF_InfoIsPresent(info, key=KEY_NUM_UNGRID_DIMS, _RC)
      if(has_dims) then
         num_dims = ESMF_InfoGet(info, key=KEY_NUM_UNGRID_DIMS, _RC)
      end if
      do i=1, num_dims
         dim_key = make_dim_key(i, _RC)
         call ESMF_InfoGetCharAlloc(info, key=dim_key // KEY_NAME, value=name, _RC)
         call ESMF_InfoGetCharAlloc(info, key=dim_key // KEY_UNITS, value=units, _RC)
         call ESMF_InfoGet(info, key=dim_key // KEY_COORD, size=num_coord, _RC)
         allocate(coordinates(num_coord))
         call ESMF_InfoGet(info, key=dim_key // KEY_COORD, values=coordinates, _RC)
         next = UngriddedDim(name, units, coordinates)
         vi = get_index_by_name(vec, name)
         if(vi > 0) then
            _ASSERT(UngriddedDim(name, units, coordinates) == vec%at(vi), 'UngriddedDim mismatch.')
         end if
         call vec%push_back(UngriddedDim(name, units, coordinates))
      end do
      _RETURN(_SUCCESS)

   end subroutine push_ungridded_dim_info
      
   integer function get_index_by_name(vec, name) result(n)
      integer :: n
      type(UngriddedDimVector), intent(in) :: vec
      character(len=*), intent(in) :: name
      type(UngriddedDimVectorIterator) :: iter

      n = 1
      iter = vec%begin()
      do while(iter <= vec%end())
         if(iter%of()%get_name() == name) return
         n = n + 1
         call iter%next()
      end do
      if(n > vec%size()) n = 0 

   end function get_index_by_name

   function get_bundle_info(bundle, rc) result(bundle_info)
      type(ESMF_Info), allocatable :: bundle_info(:)
      type(ESMF_FieldBundle), intent(in) :: bundle
      integer, optional, intent(out) :: rc
      integer :: status
      integer :: field_count
      type(ESMF_Field), allocatable :: fields(:)
      type(ESMF_Info) :: info

      call ESMF_FieldBundleGet(bundle, fieldCount=field_count, _RC)
      _ASSERT(field_count > 0, 'Empty bundle')
      allocate(fields(field_count))
      call ESMF_FieldBundleGet(bundle, fieldList=fields, _RC)
      allocate(bundle_info(field_count))
      do i=1, field_count
         call ESMF_InfoGetFromHost(field, info, _RC)
         bundle_info(i) = info
      end do
      _RETURN(_SUCCESS)

   end function get_bundle_info

   subroutine destroy_bundle_info(bundle_info, rc)
      type(ESMF_Info), intent(inout) :: bundle_info(:)
      integer, optional, intent(out) :: rc 
      integer :: status, i

      do i=1, size(bundle_info)
         call ESMF_InfoDestroy(bundle_info(i), _RC)
      end do
      _RETURN(_SUCCESS)

   end subroutine destroy_bundle_info
   
end module mapl3g_output_info
