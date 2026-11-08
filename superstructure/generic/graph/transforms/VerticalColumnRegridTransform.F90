#include "MAPL.h"

!------------------------------------------------------------------------------
! VerticalColumnRegridTransform: regrids a field from the input field's
! vertical grid to the output field's, column by column. Field only, R4
! only, linear only.
!
! Each field carries its vertical grid, stagger, and alignment (read with
! FieldGet at compute). The weights are built at the first compute and again
! whenever one of those changes. One weight matrix serves every column,
! since only grids whose levels are the same in every column are supported.
!
! Each side's level values are put in that field's own storage order
! (reversed when its alignment is opposite to its grid's direction), so the
! weights map stored data to stored data and no data is flipped.
!
! Cases:
! - same grid, or grids that match (a basic grid matches any grid with the
!   same level count): copy, reversed if the two orientations differ. The
!   staggers must agree.
! - otherwise both grids must supply level values
!   (VerticalGrid%get_column_coordinate) in a shared physical dimension;
!   the export levels are converted to the import's units if needed. Both
!   fields must be at level centers. Import levels outside the export range
!   take the nearest end value, and a warning is logged.
!
! Independent of the legacy layer: must not use the legacy Aspects (specs/),
! registry/, or the legacy transforms/.
!------------------------------------------------------------------------------
module mapl_VerticalColumnRegridTransform_mod
   use mapl_Transform_mod, only: Transform
   use mapl_PortNameGraphStateItemMap_mod, only: PortNameGraphStateItemMap
   use mapl_VerticalFlipTransform_mod, only: copy_vertical_column
   use mapl_VerticalFlipTransform_mod, only: get_vertical_port_field
   use mapl_field_api, only: MAPL_FieldGet
   use mapl_FieldCondensedArray_mod, only: assign_fptr_condensed_array
   use mapl_VerticalGrid_mod, only: VerticalGrid
   use mapl_VerticalStaggerLoc_mod, only: VerticalStaggerLoc
   use mapl_VerticalStaggerLoc_mod, only: VERTICAL_STAGGER_CENTER
   use mapl_VerticalStaggerLoc_mod, only: operator(==), operator(/=)
   use mapl_VerticalAlignment_mod, only: VerticalAlignment
   use mapl_VerticalAlignment_mod, only: operator(==), operator(/=)
   use mapl_VerticalCoordinateDirection_mod, only: VerticalCoordinateDirection
   use mapl_VerticalCoordinateDirection_mod, only: VCOORD_DIRECTION_UP
   use mapl_VerticalCoordinateDirection_mod, only: VCOORD_DIRECTION_DOWN
   use mapl_VerticalCoordinateDirection_mod, only: VCOORD_DIRECTION_INVALID
   use mapl_VerticalCoordinateDirection_mod, only: operator(==)
   use mapl_VerticalRegridMethod_mod, only: VerticalRegridMethod
   use mapl_VerticalRegridMethod_mod, only: VERTICAL_REGRID_LINEAR
   use mapl_VerticalRegridMethod_mod, only: operator(==), operator(/=)
   use mapl_VerticalLinearMap_mod, only: compute_linear_map
   use mapl_CSR_SparseMatrix_mod, only: SparseMatrix_sp => CSR_SparseMatrix_sp
   use mapl_CSR_SparseMatrix_mod, only: matmul, shape
   use udunits2f, only: UDUNITS_Converter => Converter
   use udunits2f, only: UDUNITS_GetConverter => get_converter
   use gftl2_StringVector, only: StringVector
   use pflogger, only: logging, Logger
   use ESMF, only: ESMF_Field, ESMF_FieldGet, ESMF_TypeKind_Flag
   use ESMF, only: ESMF_TYPEKIND_R4, ESMF_KIND_R4, ESMF_MAXSTR
   use ESMF, only: operator(==), operator(/=)
   use mapl_ErrorHandling_mod
   implicit none(type, external)
   private

   public :: VerticalColumnRegridTransform
   public :: vertical_regrid_r8_message

   type, extends(Transform) :: VerticalColumnRegridTransform
      private
      type(VerticalRegridMethod) :: method = VERTICAL_REGRID_LINEAR
      logical :: ready = .false.
      ! What the current weights were built for.
      integer :: vgrid_id_in = -1
      integer :: vgrid_id_out = -1
      type(VerticalStaggerLoc) :: stagger_in
      type(VerticalStaggerLoc) :: stagger_out
      type(VerticalAlignment) :: alignment_in
      type(VerticalAlignment) :: alignment_out
      ! Copy (flip or not) instead of weights.
      logical :: copy_only = .false.
      logical :: flip = .false.
      type(SparseMatrix_sp) :: matrix
      integer :: num_out_of_range = 0
      integer :: num_builds = 0
   contains
      procedure :: compute => vregrid_compute
      procedure :: get_num_out_of_range
      procedure :: get_num_builds
      procedure, private :: update_weights_for
   end type VerticalColumnRegridTransform

   interface VerticalColumnRegridTransform
      module procedure new_VerticalColumnRegridTransform
   end interface VerticalColumnRegridTransform

   character(*), parameter :: INPUT_PORT_NAME = 'source_field'
   character(*), parameter :: OUTPUT_PORT_NAME = 'destination_field'
   character(*), parameter :: WHO = 'VerticalColumnRegridTransform'
   real, parameter :: RANGE_TOL = 1.0e-5

contains

   function new_VerticalColumnRegridTransform(method) result(transform)
      type(VerticalRegridMethod), optional, intent(in) :: method
      type(VerticalColumnRegridTransform) :: transform

      integer :: status

      if (present(method)) transform%method = method
      call transform%declare_input_port(INPUT_PORT_NAME, rc=status)
      call transform%declare_output_port(OUTPUT_PORT_NAME, rc=status)
   end function new_VerticalColumnRegridTransform

   ! Shared with the connect-time check, so both report R8 the same way.
   function vertical_regrid_r8_message(name) result(m)
      character(*), intent(in) :: name
      character(:), allocatable :: m

      m = 'vertical regrid of ' // name // ': R8 not supported'
   end function vertical_regrid_r8_message

   function msg(name, text) result(m)
      character(*), intent(in) :: name, text
      character(:), allocatable :: m

      m = 'vertical regrid of ' // name // ': ' // text
   end function msg

   subroutine vregrid_compute(this, inputs, outputs, rc)
      class(VerticalColumnRegridTransform), intent(inout) :: this
      type(PortNameGraphStateItemMap), intent(in) :: inputs
      type(PortNameGraphStateItemMap), intent(in) :: outputs
      integer, intent(out) :: rc

      integer :: status
      type(ESMF_Field) :: f_in, f_out
      character(:), allocatable :: name

      call get_vertical_port_field(inputs, INPUT_PORT_NAME, WHO, f_in, _RC)
      call get_vertical_port_field(outputs, OUTPUT_PORT_NAME, WHO, f_out, _RC)
      call get_field_name(f_out, name, _RC)

      call check_r4(f_in, name, _RC)
      call check_r4(f_out, name, _RC)

      call this%update_weights_for(f_in, f_out, name, _RC)
      if (this%copy_only) then
         call copy_vertical_column(f_in, f_out, this%flip, _RC)
      else
         call apply_weights(this%matrix, f_in, f_out, name, _RC)
      end if

      _RETURN(_SUCCESS)
   end subroutine vregrid_compute

   subroutine update_weights_for(this, f_in, f_out, name, rc)
      class(VerticalColumnRegridTransform), intent(inout) :: this
      type(ESMF_Field), intent(inout) :: f_in, f_out
      character(*), intent(in) :: name
      integer, optional, intent(out) :: rc

      integer :: status
      class(VerticalGrid), pointer :: vgrid_in, vgrid_out
      type(VerticalStaggerLoc) :: stagger_in, stagger_out
      type(VerticalAlignment) :: alignment_in, alignment_out
      type(VerticalCoordinateDirection) :: dir_in, dir_out
      logical :: changed, same_grid

      call MAPL_FieldGet(f_in, vgrid=vgrid_in, vert_staggerloc=stagger_in, &
           vert_alignment=alignment_in, _RC)
      call MAPL_FieldGet(f_out, vgrid=vgrid_out, vert_staggerloc=stagger_out, &
           vert_alignment=alignment_out, _RC)
      _ASSERT(associated(vgrid_in), msg(name, 'source has no vgrid'))
      _ASSERT(associated(vgrid_out), msg(name, 'destination has no vgrid'))

      changed = .not. this%ready
      if (.not. changed) then
         changed = vgrid_in%get_id() /= this%vgrid_id_in &
              .or. vgrid_out%get_id() /= this%vgrid_id_out &
              .or. stagger_in /= this%stagger_in &
              .or. stagger_out /= this%stagger_out &
              .or. alignment_in /= this%alignment_in &
              .or. alignment_out /= this%alignment_out
      end if
      _RETURN_UNLESS(changed)

      ! Cache nothing until the build succeeds, so a failure cannot leave
      ! old weights paired with the new grids.
      this%ready = .false.

      dir_in = alignment_in%resolve(vgrid_in%get_coordinate_direction())
      dir_out = alignment_out%resolve(vgrid_out%get_coordinate_direction())
      if (dir_in == VCOORD_DIRECTION_INVALID) then
         _FAIL(msg(name, 'source has an invalid vertical alignment'))
      end if
      if (dir_out == VCOORD_DIRECTION_INVALID) then
         _FAIL(msg(name, 'destination has an invalid vertical alignment'))
      end if

      same_grid = vgrid_in%get_id() == vgrid_out%get_id()
      if (same_grid .or. vgrid_in%matches(vgrid_out)) then
         if (stagger_in /= stagger_out) then
            _FAIL(msg(name, 'stagger differs on the same vertical grid'))
         end if
         this%copy_only = .true.
         this%flip = opposite(dir_in, dir_out)
         this%num_out_of_range = 0
      else
         ! A copy or flip needs no method, so only a real regrid checks it.
         if (this%method /= VERTICAL_REGRID_LINEAR) then
            _FAIL(msg(name, 'only linear is supported'))
         end if
         call build_weights(vgrid_in, stagger_in, dir_in, vgrid_out, &
              stagger_out, dir_out, name, this%matrix, &
              this%num_out_of_range, _RC)
         this%copy_only = .false.
         this%flip = .false.
      end if

      this%vgrid_id_in = vgrid_in%get_id()
      this%vgrid_id_out = vgrid_out%get_id()
      this%stagger_in = stagger_in
      this%stagger_out = stagger_out
      this%alignment_in = alignment_in
      this%alignment_out = alignment_out
      this%num_builds = this%num_builds + 1
      this%ready = .true.

      _RETURN(_SUCCESS)
   end subroutine update_weights_for

   ! A single level has no direction, so it never needs reversing.
   logical function opposite(a, b)
      type(VerticalCoordinateDirection), intent(in) :: a, b

      opposite = (a == VCOORD_DIRECTION_UP .and. b == VCOORD_DIRECTION_DOWN) &
           .or. (a == VCOORD_DIRECTION_DOWN .and. b == VCOORD_DIRECTION_UP)
   end function opposite

   subroutine build_weights(vgrid_in, stagger_in, dir_in, vgrid_out, &
        stagger_out, dir_out, name, matrix, num_out_of_range, rc)
      class(VerticalGrid), intent(in) :: vgrid_in, vgrid_out
      type(VerticalStaggerLoc), intent(in) :: stagger_in, stagger_out
      type(VerticalCoordinateDirection), intent(in) :: dir_in, dir_out
      character(*), intent(in) :: name
      type(SparseMatrix_sp), intent(out) :: matrix
      integer, intent(out) :: num_out_of_range
      integer, optional, intent(out) :: rc

      integer :: status
      character(:), allocatable :: dimension, units_in, units_out, grids
      character(:), allocatable :: text
      real, allocatable :: levels_in(:), levels_out(:)
      type(UDUNITS_Converter) :: converter
      class(Logger), pointer :: lgr
      character(16) :: buf
      real :: tol

      num_out_of_range = 0
      write(buf, '(i0," and ",i0)') vgrid_in%get_id(), vgrid_out%get_id()
      grids = 'vertical grids ' // trim(buf)

      if (stagger_in /= VERTICAL_STAGGER_CENTER .or. &
           stagger_out /= VERTICAL_STAGGER_CENTER) then
         _FAIL(msg(name, 'only level centers regrid between different grids'))
      end if

      call find_common_dimension(vgrid_in, vgrid_out, dimension)
      if (.not. allocated(dimension)) then
         _FAIL(msg(name, grids // ' share no physical dimension'))
      end if

      call get_levels(vgrid_in, vgrid_out, 'source', dimension, name, &
           levels_in, _RC)
      call get_levels(vgrid_out, vgrid_in, 'destination', dimension, name, &
           levels_out, _RC)

      units_in = vgrid_in%get_units(dimension, _RC)
      units_out = vgrid_out%get_units(dimension, _RC)
      if (units_in /= units_out) then
         call UDUNITS_GetConverter(converter, from=units_in, to=units_out, &
              rc=status)
         if (status /= 0) then
            text = 'cannot convert levels from ' // units_in // ' to ' // &
                 units_out
            _FAIL(msg(name, text))
         end if
         levels_in = converter%convert(levels_in)
         call converter%free()
      end if

      ! Put each side's levels in its field's storage order.
      if (opposite(vgrid_in%get_coordinate_direction(), dir_in)) then
         levels_in = levels_in(size(levels_in):1:-1)
      end if
      if (opposite(vgrid_out%get_coordinate_direction(), dir_out)) then
         levels_out = levels_out(size(levels_out):1:-1)
      end if

      ! Tolerance so levels equal up to rounding (e.g. after a units
      ! conversion) are not reported.
      tol = RANGE_TOL * maxval(abs(levels_in))
      num_out_of_range = count(levels_out < minval(levels_in) - tol &
           .or. levels_out > maxval(levels_in) + tol)
      if (num_out_of_range > 0) then
         write(buf, '(i0)') num_out_of_range
         lgr => logging%get_logger('mapl.generic')
         text = grids // ': ' // trim(buf) // ' destination levels' // &
              ' outside the source range take the end value'
         call lgr%warning(msg(name, text))
      end if

      call compute_linear_map(levels_in, levels_out, matrix, &
           allow_out_of_range=.true., _RC)

      _RETURN(_SUCCESS)
   end subroutine build_weights

   ! Grids that do not match need level values on both sides.
   subroutine get_levels(vgrid, other, side, dimension, name, levels, rc)
      class(VerticalGrid), intent(in) :: vgrid, other
      character(*), intent(in) :: side, dimension, name
      real, allocatable, intent(out) :: levels(:)
      integer, optional, intent(out) :: rc

      integer :: status
      character(64) :: buf
      character(:), allocatable :: text

      call vgrid%get_column_coordinate(dimension, levels, rc=status)
      if (status /= 0) then
         write(buf, '(i0," and ",i0)') vgrid%get_num_layers(), &
              other%get_num_layers()
         text = side // ' grid has no level values, and the grids do' // &
              ' not match (' // trim(buf) // ' levels)'
         _FAIL(msg(name, text))
      end if

      _RETURN(_SUCCESS)
   end subroutine get_levels

   ! Leaves dimension unallocated if the grids share none.
   subroutine find_common_dimension(vgrid_in, vgrid_out, dimension)
      class(VerticalGrid), intent(in) :: vgrid_in, vgrid_out
      character(:), allocatable, intent(out) :: dimension

      type(StringVector) :: dims_in, dims_out
      integer :: i, j

      dims_in = vgrid_in%get_supported_physical_dimensions()
      dims_out = vgrid_out%get_supported_physical_dimensions()
      do i = 1, dims_in%size()
         do j = 1, dims_out%size()
            if (dims_in%of(i) == dims_out%of(j)) then
               dimension = dims_in%of(i)
               return
            end if
         end do
      end do
   end subroutine find_common_dimension

   subroutine apply_weights(matrix, f_in, f_out, name, rc)
      type(SparseMatrix_sp), intent(in) :: matrix
      type(ESMF_Field), intent(inout) :: f_in, f_out
      character(*), intent(in) :: name
      integer, optional, intent(out) :: rc

      integer :: status, horz, ungrd
      real(kind=ESMF_KIND_R4), pointer :: x_in(:,:,:), x_out(:,:,:)
      integer :: s_in(3), s_out(3), m(2)

      call assign_fptr_condensed_array(f_in, x_in, _RC)
      call assign_fptr_condensed_array(f_out, x_out, _RC)
      s_in = shape(x_in)
      s_out = shape(x_out)
      m = shape(matrix)
      _ASSERT(s_in(1) == s_out(1), msg(name, 'horizontal sizes differ'))
      _ASSERT(s_in(3) == s_out(3), msg(name, 'ungridded sizes differ'))
      _ASSERT(s_in(2) == m(2), msg(name, 'source levels do not fit grid'))
      _ASSERT(s_out(2) == m(1), msg(name, 'destination levels do not fit grid'))

      do concurrent (horz=1:s_in(1), ungrd=1:s_in(3))
         x_out(horz, :, ungrd) = matmul(matrix, x_in(horz, :, ungrd))
      end do

      _RETURN(_SUCCESS)
   end subroutine apply_weights

   subroutine check_r4(field, name, rc)
      type(ESMF_Field), intent(in) :: field
      character(*), intent(in) :: name
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_TypeKind_Flag) :: typekind

      call ESMF_FieldGet(field, typekind=typekind, _RC)
      if (typekind /= ESMF_TYPEKIND_R4) then
         _FAIL(vertical_regrid_r8_message(name))
      end if

      _RETURN(_SUCCESS)
   end subroutine check_r4

   subroutine get_field_name(field, name, rc)
      type(ESMF_Field), intent(in) :: field
      character(:), allocatable, intent(out) :: name
      integer, optional, intent(out) :: rc

      integer :: status
      character(ESMF_MAXSTR) :: buf

      call ESMF_FieldGet(field, name=buf, _RC)
      name = trim(buf)

      _RETURN(_SUCCESS)
   end subroutine get_field_name

   integer function get_num_out_of_range(this) result(n)
      class(VerticalColumnRegridTransform), intent(in) :: this

      n = this%num_out_of_range
   end function get_num_out_of_range

   integer function get_num_builds(this) result(n)
      class(VerticalColumnRegridTransform), intent(in) :: this

      n = this%num_builds
   end function get_num_builds

end module mapl_VerticalColumnRegridTransform_mod
