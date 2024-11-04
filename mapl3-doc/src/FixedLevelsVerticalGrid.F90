#include "MAPL_Generic.h"

module mapl3g_FixedLevelsVerticalGrid

   use mapl_ErrorHandling
   use mapl3g_VerticalGrid
   use mapl3g_VerticalStaggerLoc
   use mapl3g_FieldCreate
   use mapl3g_GriddedComponentDriver
   use mapl3g_VerticalDimSpec
   use mapl3g_InfoUtilities, only: MAPL_InfoSetInternal
   use mapl3g_esmf_info_keys, only: KEY_VLOC, KEY_NUM_LEVELS
   use esmf

   use, intrinsic :: iso_fortran_env, only: REAL32

   implicit none
   private

   public :: FixedLevelsVerticalGrid
   public :: operator(==)
   public :: operator(/=)

   type, extends(VerticalGrid) :: FixedLevelsVerticalGrid
      private
      real(kind=REAL32), allocatable :: levels(:)
      character(:), allocatable :: standard_name ! air_pressure, height, etc.
      character(:), allocatable :: units
   contains
      procedure :: get_num_levels
      procedure :: get_coordinate_field
      procedure :: can_connect_to
      procedure :: write_formatted
   end type FixedLevelsVerticalGrid

   interface FixedLevelsVerticalGrid
      procedure new_FixedLevelsVerticalGrid_r32
   end interface FixedLevelsVerticalGrid

   interface operator(==)
      module procedure equal_FixedLevelsVerticalGrid
   end interface operator(==)

   interface operator(/=)
      module procedure not_equal_FixedLevelsVerticalGrid
   end interface operator(/=)

contains

   function new_FixedLevelsVerticalGrid_r32(standard_name, levels, units) result(grid)
      type(FixedLevelsVerticalGrid) :: grid
      character(*), intent(in) :: standard_name
      real(REAL32), intent(in) :: levels(:)
      character(*), intent(in) :: units

      call grid%set_id()
      grid%standard_name = standard_name
      grid%levels = levels
      grid%units = units
   end function new_FixedLevelsVerticalGrid_r32

   integer function get_num_levels(this) result(num_levels)
      class(FixedLevelsVerticalGrid), intent(in) :: this
      num_levels = size(this%levels)
   end function get_num_levels

   subroutine get_coordinate_field(this, field, coupler, standard_name, geom, typekind, units, vertical_dim_spec, rc)
      class(FixedLevelsVerticalGrid), intent(in) :: this
      type(ESMF_Field), intent(out) :: field
      type(GriddedComponentDriver), pointer, intent(out) :: coupler
      character(*), intent(in) :: standard_name
      type(ESMF_Geom), intent(in) :: geom
      type(ESMF_TypeKind_Flag), intent(in) :: typekind
      character(*), intent(in) :: units
      type(VerticalDimSpec), intent(in) :: vertical_dim_spec
      integer, optional, intent(out) :: rc

      real(kind=REAL32), allocatable :: adjusted_levels(:)
      character(:), allocatable :: vloc
      integer :: status

      ! KLUDGE - for VERTICAL_DIM_EDGE, we simply extend the the size
      ! [40, 30, 20, 10] -> [40, 30, 20, 10, 10]
      ! Also, vloc assignment gets simpler once we have co-located description in VerticalDimSpec
      if (vertical_dim_spec == VERTICAL_DIM_CENTER) then
         adjusted_levels = this%levels
         vloc = "VERTICAL_DIM_CENTER"
      else if (vertical_dim_spec == VERTICAL_DIM_EDGE) then
         adjusted_levels = [this%levels, this%levels(size(this%levels))]
         vloc = "VERTICAL_DIM_EDGE"
      else
         _FAIL("invalid vertical_dim_spec")
      end if

      field = esmf_field_create_(geom, adjusted_levels, _RC)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(coupler)
      _UNUSED_DUMMY(standard_name)
      _UNUSED_DUMMY(typekind)
      _UNUSED_DUMMY(units)
   end subroutine get_coordinate_field

   logical function can_connect_to(this, src, rc)
      class(FixedLevelsVerticalGrid), intent(in) :: this
      class(VerticalGrid), intent(in) :: src
      integer, optional, intent(out) :: rc

      can_connect_to = .false.
      _FAIL("not implemented")
      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(src)
   end function can_connect_to

   subroutine write_formatted(this, unit, iotype, v_list, iostat, iomsg)
      class(FixedLevelsVerticalGrid), intent(in) :: this
      integer, intent(in) :: unit
      character(*), intent(in) :: iotype
      integer, intent(in) :: v_list(:)
      integer, intent(out) :: iostat
      character(*), intent(inout) :: iomsg

      write(unit, "(a, a, 3x, a, a, a, 3x, a, a, a, 3x, a, *(g0, 1x))", iostat=iostat, iomsg=iomsg) &
           "FixedLevelsVerticalGrid(", new_line("a"), &
           "standard name: ", this%standard_name, new_line("a"), &
           "units: ", this%units, new_line("a"), &
           "levels: ", this %levels
      write(unit, "(a, a)", iostat=iostat, iomsg=iomsg) new_line("a"), ")"

      _UNUSED_DUMMY(iotype)
      _UNUSED_DUMMY(v_list)
   end subroutine write_formatted

   impure elemental logical function equal_FixedLevelsVerticalGrid(a, b) result(equal)
      type(FixedLevelsVerticalGrid), intent(in) :: a, b

      equal = a%standard_name == b%standard_name
      if (.not. equal) return
      equal = a%units == b%units
      if (.not. equal) return
      equal = size(a%levels) == size(b%levels)
      if (.not. equal) return
      equal = all(a%levels == b%levels)
   end function equal_FixedLevelsVerticalGrid

   impure elemental logical function not_equal_FixedLevelsVerticalGrid(a, b) result(not_equal)
      type(FixedLevelsVerticalGrid), intent(in) :: a, b

      not_equal = .not. (a==b)
   end function not_equal_FixedLevelsVerticalGrid

   ! Create an ESMF_Field containing a 3D array that is replicated from
   ! a 1D array at each point of the horizontal grid
   function esmf_field_create_(geom, farray1d, rc) result(field)
      type(ESMF_Field) :: field ! result
      type(ESMF_Geom), intent(in) :: geom
      real(kind=REAL32), intent(in) :: farray1d(:)
!#      character(len=*), intent(in) :: vloc
      integer, optional, intent(out) :: rc

      integer, allocatable :: local_cell_count(:)
      real(kind=REAL32), pointer :: farray3d(:, :, :)
      integer :: i, j, IM, JM, status

!#      ! First, copy the 1D array, farray1d, to each point on the horz grid
!#      allocate(farray3d(IM, JM, size(farray1d)))
!#      do concurrent (i=1:IM, j=1:JM)
!#         farray3d(i, j, :) = farray1d(:)
!#      end do

      ! Create an ESMF_Field containing farray3d
      field = MAPL_FieldCreate( &
           geom=geom, typekind=ESMF_TYPEKIND_R4, &
           num_levels=size(farray1d), &
           vert_staggerloc=VERTICAL_STAGGER_CENTER, &
           _RC)

!#      ! First, copy the 1D array, farray1d, to each point on the horz grid
      call ESMF_FieldGet(field, fArrayPtr=farray3d, _RC)
      call MAPL_GeomGet_(geom, localCellCount=local_cell_count, _RC)
      IM = local_cell_count(1); JM = local_cell_count(2)
      do concurrent (i=1:IM, j=1:JM)
         farray3d(i, j, :) = farray1d(:)
      end do

!#      field = ESMF_FieldCreate( &
!#           geom=geom, &
!#           farray=farray3d, &
!#           indexflag=ESMF_INDEX_DELOCAL, &
!#           datacopyFlag=ESMF_DATACOPY_VALUE, &
!#           ungriddedLBound=[1], &
!#           ungriddedUBound=[size(farray1d)], &
!#           _RC)
!#      
!#      call MAPL_InfoSetInternal(field, key=KEY_NUM_LEVELS, value=size(farray1d), _RC)
!#      call MAPL_InfoSetInternal(field, key=KEY_VEVLOC, value=vloc, _RC)

      _RETURN(_SUCCESS)
   end function esmf_field_create_

   ! Temporary version here while the detailed MAPL_GeomGet utility gets developed
   subroutine MAPL_GeomGet_(geom, localCellCount, rc)
      use MAPLBase_Mod
      type(ESMF_Geom), intent(in) :: geom
      integer, allocatable, intent(out), optional :: localCellCount(:)
      integer, intent(out), optional :: rc

      type(ESMF_Grid) :: grid
      integer :: status

      if (present(localCellCount)) then
         call ESMF_GeomGet(geom, grid=grid)
         allocate(localCellCount(3), source=-1)
         call MAPL_GridGet(grid, localCellCountPerDim=localCellCount, _RC)
      end if

      _RETURN(_SUCCESS)
   end subroutine MAPL_GeomGet_

end module mapl3g_FixedLevelsVerticalGrid
