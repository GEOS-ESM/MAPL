#include "MAPL_Generic.h"
module mapl3g_output_info

   use mapl3g_UngriddedDim
   use mapl3g_UngriddedDimVector
   use mapl3g_UngriddedDims
   use mapl3g_ESMF_Info_Keys
   use gFTL2_StringVector
   use esmf, only: ESMF_Field, ESMF_FieldBundle, ESMF_FieldBundleGet
   use esmf, only: ESMF_Info, ESMF_InfoDestroy, ESMF_InfoIsPresent
   use esmf, only: ESMF_InfoGet, ESMF_InfoGetCharAlloc, ESMF_InfoGetFromHost
   use esmf, only: ESMF_InfoGetAlloc
   use Mapl_ErrorHandling

   implicit none

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

contains

   integer function get_num_levels_bundle(bundle, rc) result(num)
      type(ESMF_FieldBundle), intent(in) :: bundle
      integer, optional, intent(out) :: rc
      integer :: status
      type(ESMF_Info), allocatable :: info(:)

      info = create_bundle_info(bundle, _RC)
      num = get_num_levels_bundle_info(info, _RC)
      call destroy_bundle_info(info, _RC)
      _RETURN(_SUCCESS)

   end function get_num_levels_bundle

   integer function get_num_levels_bundle_info(info, rc) result(num)
      type(ESMF_Info), intent(in) :: info(:)
      integer, optional, intent(out) :: rc
      integer :: status
      integer :: i, n

      num = get_num_levels_info(info(1), _RC)
      do i=2, size(info)
         n = get_num_levels_info(info(i), _RC)
         _ASSERT(n == num, 'All fields must have the same number of vertical levels.')
      end do
      _RETURN(_SUCCESS)

   end function get_num_levels_bundle_info

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

      info = create_bundle_info(bundle, _RC)
      names = get_vertical_dim_spec_names_bundle_info(info, _RC)
      call destroy_bundle_info(info, _RC)
      _RETURN(_SUCCESS)

   end function get_vertical_dim_spec_names_bundle

   function get_vertical_dim_spec_names_bundle_info(info, rc) result(names)
      type(StringVector) :: names
      type(ESMF_Info), intent(in) :: info(:)
      integer, optional, intent(out) :: rc
      integer :: status
      integer :: i
      character(len=:), allocatable :: name

      names = StringVector()
      do i=1, size(info)
         name = get_vertical_dim_spec_info(info(i), _RC)
         if(find_index(names, name) == 0) call names%push_back(name)
      end do
      _RETURN(_SUCCESS)

   end function get_vertical_dim_spec_names_bundle_info

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
      call destroy_bundle_info(info, _RC)
      _RETURN(_SUCCESS)

   end function get_ungridded_dims_bundle

   function get_ungridded_dims_bundle_info(info, rc) result(vec)
      type(UngriddedDimVector) :: vec
      type(ESMF_Info), intent(in) :: info(:)
      integer, optional, intent(out) :: rc
      integer :: status
      integer :: i

      vec = UngriddedDimVector()
      do i=1, size(info)
         call push_ungridded_dim(vec, info(i), _RC)
      end do
      _RETURN(_SUCCESS)

   end function get_ungridded_dims_bundle_info

   function get_ungridded_dims_field(field, rc) result(ungridded)
      type(UngriddedDims) :: ungridded
      type(ESMF_Field), intent(inout) :: field
      integer, optional, intent(out) :: rc
      integer :: status
      type(ESMF_Info) :: info
      type(UngriddedDimVector) :: vec

      call ESMF_InfoGetFromHost(field, info, _RC)
      call push_ungridded_dim(vec, info, _RC)
      ungridded = UngriddedDims(vec)
      call ESMF_InfoDestroy(info, _RC)
      _RETURN(_SUCCESS)

   end function get_ungridded_dims_field

   subroutine push_ungridded_dim(vec, info, rc)
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
         call ESMF_InfoGet(info, key=KEY_NUM_UNGRID_DIMS, value=num_dims, _RC)
      end if
      do i=1, num_dims
         dim_key = make_dim_key(i, _RC)
         call ESMF_InfoGetCharAlloc(info, key=dim_key // KEY_UNGRIDDED_NAME, value=name, _RC)
         call ESMF_InfoGetCharAlloc(info, key=dim_key // KEY_UNGRIDDED_UNITS, value=units, _RC)
         call ESMF_InfoGetAlloc(info, key=dim_key // KEY_UNGRIDDED_COORD, values=coordinates, _RC)
         call push_next(name, units, coordinates, vec, _RC)
      end do
      _RETURN(_SUCCESS)

   end subroutine push_ungridded_dim
      
   subroutine push_next(name, units, coordinates, vec, tol, rc) result(next)
      type(UngriddedDim) :: next
      character(len=*), intent(in) :: name
      character(len=*), intent(in) :: units
      real, intent(in) :: coordinates(:)
      type(UngriddedDimVector), intent(inout) :: vec
      real, optional, intent(in) :: tol
      integer, optional, intent(out) :: rc 
      integer :: status
      type(UngriddedDimVectorIterator) :: iter
      real :: tol_ = 1.0E-8
      logical :: below
      
      if(present(tol)) tol_ = tol
      _ASSERT(tol_ >= 0, 'A negative relative tolerance is not valid.')
      iter = vec%ftn_begin()
      do while(iter < vec%ftn_end())
         call iter%next()
         ud = iter%of()
         if(ud%get_name() /= name) cycle
         _ASSERT(ud%get_units() == units, 'units does not match.')
         _ASSERT(size(ud%get_coordinates()) == size(coordinates), 'coordinates has a different size.')
         below = check_difference(ud%get_coordinates(), coordinates, tol_, _RC)
         _ASSERT(below, 'coordinates differ by more than the relative tolerance.')
      end do
      call vec%push_back(UngriddedDim(name, units, coordinates))
      _RETURN(_SUCCESS)

   end subroutine push_next

   logical function check_difference(a, b, tol, rc) result(below)
      real, intent(in) :: a(:)
      real, intent(in) :: b(:)
      real, intent(in) :: tol
      integer, optional, intent(out) :: rc
      integer :: status
      real :: distance, mean

      _ASSERT(size(a) == size(b), 'arrays have different length.')
      _ASSERT(tol >= 0, 'tol must not be negative.')
      mean = 0.5 * (norm2(a) + norm2(b)) 
      distance = norm2(a - b)
      below = (distance <= tol * mean)

   end function check_difference

   function create_bundle_info(bundle, rc) result(bundle_info)
      type(ESMF_Info), allocatable :: bundle_info(:)
      type(ESMF_FieldBundle), intent(in) :: bundle
      integer, optional, intent(out) :: rc
      integer :: status
      integer :: field_count, i
      type(ESMF_Field) :: field
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

end module mapl3g_output_info
