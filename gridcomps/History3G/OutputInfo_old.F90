module mapl3g_OutputInfo

   use mapl3g_VerticalGeom, only: VerticalGeom
   use mapl3g_VerticalDimSpec, only: VerticalDimSpec
   use mapl3g_UngriddedDims, only: UngriddedDims
   use esmf, only: ESMF_InfoGet

   implicit none
   private

   public :: OutputInfo
   public :: operator(==)
   public :: operator(/=)

   type :: OutputInfo
      type(VerticalGeomInfo) :: vertical_geom_info
      type(VerticalDimSpec) :: vertical_dim_spec_info
      type(UngriddedDimsInfo) :: ungridded_dims_info
   end type OutputInfo

   interface OutputInfo
      module procedure :: construct_output_info
   end interface OutputInfo

   interface operator(==)
      module procedure :: equal_to_output_info
      module procedure :: equal_to_vertical_geom_info
      module procedure :: equal_to_vertical_dims_spec_info
      module procedure :: equal_to_ungridded_dim_info
      module procedure :: equal_to_ungridded_dims_info
   end interface operator(==)

   interface operator(/=)
      module procedure :: not_equal_to_output_info
   end interface operator(/=)

   type :: VerticalGeomInfo
      integer :: num_levels
   end type VerticalGeomInfo

   type :: VerticalDimSpecInfo
      character(len=:), allocatable :: vloc
   end type VerticalDimSpecInfo

   type :: UngriddedDimInfo
      character(len=:), allocatable :: name
      character(len=:), allocatable :: units
      real :: coordinates(:)
   end type UngriddedDimInfo

   type :: UngriddedDimsInfo
      type(UngriddedDimInfo) :: dim_specs(:)
   end type UngriddedDimsInfo

contains

   function construct_output_info(esmfinfo) result(output_info)
      type(OutputInfo) :: output_info
      type(ESMF_Info), intent(in) :: esmfinfo

      call ESMF_InfoGet(esmfinfo, key=VERT_GEOM_KEY, vert_geom, _RC)
      output_info%vert_geom => vert_geom
      call ESMF_InfoGet(esmfinfo, key=VERT_SPEC_KEY, vert_spec, _RC)
      output_info%vert_spec => vert_spec
      call ESMF_InfoGet(esmfinfo, key=UNGRIDDED_KEY, ungridded, _RC)
      output_info%ungridded => ungridded

   end function construct_output_info

   logical function equal_to_output_info(a, b) result(equal)
      class(OutputInfo), intent(in) :: a, b

      equal = a%vertical_geom_info == b%vertical_geom_info .and. &
         a%vertical_dim_spec_info == b%vertical_dim_spec_info .and. &
         a%vertical_ungridded_dims_info == b%vertical_ungridded_dims_info

   end function equal_to_output_info

   logical function not_equal_to_output_info(a, b) result(not_equal)
      class(OutputInfo), intent(in) :: a, b

      not_equal = .not. (a == b)

   end function not_equal_to_output_info

   logical function equal_to_vertical_geom_info(a, b) result(equal)
      class(VerticalGeomInfo), intent(in) :: a, b

         equal = a%num_levels == b%num_levels

   end function equal_to_vertical_geom_info

   logical function not_equal_to_vertical_geom_info(a, b) result(not_equal)
      class(VerticalGeomInfo), intent(in) :: a, b

      not_equal = .not. (a == b)

   end function not_equal_to_vertical_geom_info

   logical function equal_to_vertical_dim_spec_info(a, b) result(equal)
      class(VerticalDimSpecInfo), intent(in) :: a, b

         equal = a%vloc == b%vloc

   end function equal_to_vertical_dim_spec_info

   logical function not_equal_to_vertical_dim_spec_info(a, b) result(not_equal)
      class(VerticalDimSpecInfo), intent(in) :: a, b

      not_equal = .not. (a == b)

   end function not_equal_to_vertical_dim_spec_info

   logical function equal_to_ungridded_dim_info(a, b) result(equal)
      class(UngriddedDimInfo), intent(in) :: a, b

         equal = a%name == b%name .and. a%units == b%units .and. &
            all(a%coordinates == b%coordinates)

   end function equal_to_ungridded_dim_info

   logical function not_equal_to_ungridded_dim_info(a, b) result(not_equal)
      class(UngriddedDimInfo), intent(in) :: a, b

      not_equal = .not. (a == b)

   end function not_equal_to_ungridded_dim_info

   logical function equal_to_ungridded_dims_info(a, b) result(equal)
      class(UngriddedDimsInfo), intent(in) :: a, b

      equal = all(a == b)

   end function equal_to_ungridded_dims_info

   logical function not_equal_to_ungridded_dims_info(a, b) result(not_equal)
      class(UngriddedDimsInfo), intent(in) :: a, b

      not_equal = .not. (a == b)

   end function not_equal_to_ungridded_dims_info

end module mapl3g_OutputInfo
