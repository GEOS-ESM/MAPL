module mapl3g_esmf_info_keys

   implicit none

   ! FieldSpec info keys
   character(len=*), parameter :: PREFIX = 'MAPL/'
   character(len=*), parameter :: KEY_UNGRIDDED_DIM = PREFIX // 'ungridded_dims/'
   character(len=*), parameter :: KEY_VERT_DIM = PREFIX // 'vertical_dim/'
   character(len=*), parameter :: KEY_VERT_GEOM = PREFIX // 'vertical_geom/'
   character(len=*), parameter :: KEY_UNITS = PREFIX // 'units'
   character(len=*), parameter :: KEY_LONG_NAME = PREFIX // 'long_name'
   character(len=*), parameter :: KEY_STANDARD_NAME = PREFIX // 'standard_name'

   ! VerticalGeom info keys
   character(len=*), parameter :: KEY_NUM_LEVELS = KEY_VERT_GEOM // 'num_levels'
   
   ! VerticalDimSpec info keys
   character(len=*), parameter :: KEY_VLOC = KEY_VERT_DIM // 'vloc'

   ! UngriddedDims info keys
   character(len=*), parameter :: KEY_NUM_UNGRID_DIMS = KEY_UNGRIDDED_DIM // 'num_ungridded_dimensions'
   character(len=*), parameter :: KEYSTUB_DIM = KEY_UNGRIDDED_DIM // 'dim_'

   ! UngriddedDim info keys
   character(len=*), parameter :: KEY_UNGRIDDED_NAME = 'name'
   character(len=*), parameter :: KEY_UNGRIDDED_UNITS = 'units'
   character(len=*), parameter :: KEY_UNGRIDDED_COORD = 'coordinates'

   private :: SUCCESS, FAILURE, EMPTY_STRING

   integer, parameter :: SUCCESS = 0
   integer, parameter :: FAILURE = SUCCESS - 1
   character(len=*), parameter :: EMPTY_STRING = ''

contains

   function make_dim_key(n, rc) result(key)
      character(len=:), allocatable :: key
      integer, intent(in) :: n
      integer, optional, intent(out) :: rc
      integer :: status
      character(len=*), parameter :: FMT_ = '(I0)'
      character(len=20) :: raw

      key = EMPTY_STRING
      _ASSERT(n >=0, "n must be positive")

      write(raw, fmt='(I0)', iostat=status) n
      key = KEYSTUB_DIM // trim(adjustl(raw)) // '/'
      if(present(rc)) rc = status
      
   end function make_dim_key

end module mapl3g_esmf_info_keys
