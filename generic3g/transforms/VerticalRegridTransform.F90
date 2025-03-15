#include "MAPL_Generic.h"

module mapl3g_VerticalRegridTransform

   use mapl_ErrorHandling
   use mapl3g_ExtensionTransform
   use mapl3g_ComponentDriver
   use mapl3g_CouplerPhases, only: GENERIC_COUPLER_UPDATE
   use mapl3g_VerticalRegridMethod
   use mapl3g_VerticalLinearMap, only: compute_linear_map
   use mapl3g_CSR_SparseMatrix, only: SparseMatrix_sp => CSR_SparseMatrix_sp, matmul
   use mapl3g_FieldCondensedArray, only: assign_fptr_condensed_array
   use esmf

   implicit none
   private

   public :: VerticalRegridTransform
   public :: VERTICAL_REGRID_UNKNOWN
   public :: VERTICAL_REGRID_LINEAR
   public :: VERTICAL_REGRID_CONSERVATIVE
   public :: operator(==), operator(/=)

   type, extends(ExtensionTransform) :: VerticalRegridTransform
      type(ESMF_Field) :: v_in_coord, v_out_coord
      type(SparseMatrix_sp), allocatable :: matrix(:)
      class(ComponentDriver), pointer :: v_in_coupler => null()
      class(ComponentDriver), pointer :: v_out_coupler => null()
      type(VerticalRegridMethod) :: method = VERTICAL_REGRID_UNKNOWN
   contains
      procedure :: initialize
      procedure :: update
      procedure :: write_formatted
      generic :: write(formatted) => write_formatted
   end type VerticalRegridTransform

   interface VerticalRegridTransform
      procedure :: new_VerticalRegridTransform
   end interface VerticalRegridTransform

contains

   function new_VerticalRegridTransform(v_in_coord, v_in_coupler, v_out_coord, v_out_coupler, method) result(transform)
      type(VerticalRegridTransform) :: transform
      type(ESMF_Field), intent(in) :: v_in_coord
      class(ComponentDriver), pointer, intent(in) :: v_in_coupler
      type(ESMF_Field), intent(in) :: v_out_coord
      class(ComponentDriver), pointer, intent(in) :: v_out_coupler
      type(VerticalRegridMethod), optional, intent(in) :: method

      transform%v_in_coord = v_in_coord
      transform%v_out_coord = v_out_coord

      transform%v_in_coupler => v_in_coupler
      transform%v_out_coupler => v_out_coupler

      if (present(method)) then
         transform%method = method
      end if
   end function new_VerticalRegridTransform

   subroutine initialize(this, importState, exportState, clock, rc)
      class(VerticalRegridTransform), intent(inout) :: this
      type(ESMF_State) :: importState
      type(ESMF_State) :: exportState
      type(ESMF_Clock) :: clock
      integer, optional, intent(out) :: rc

      _ASSERT(this%method == VERTICAL_REGRID_LINEAR, "regrid method can only be linear")

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(importState)
      _UNUSED_DUMMY(exportState)
      _UNUSED_DUMMY(clock)
   end subroutine initialize

   subroutine update(this, importState, exportState, clock, rc)
      class(VerticalRegridTransform), intent(inout) :: this
      type(ESMF_State) :: importState
      type(ESMF_State) :: exportState
      type(ESMF_Clock) :: clock
      integer, optional, intent(out) :: rc

      type(ESMF_Field) :: f_in, f_out
      real(ESMF_KIND_R4), pointer :: x_in(:,:,:), x_out(:,:,:)
      integer :: shape_in(3), shape_out(3), n_horz, n_ungridded
      integer :: horz, ungrd, status

      ! if (associated(this%v_in_coupler)) then
      !    call this%v_in_coupler%run(phase_idx=GENERIC_COUPLER_UPDATE, _RC)
      ! end if

      ! if (associated(this%v_out_coupler)) then
      !    call this%v_out_coupler%run(phase_idx=GENERIC_COUPLER_UPDATE, _RC)
      ! end if

      call compute_interpolation_matrix_(this%v_in_coord, this%v_out_coord, this%matrix, _RC)

      call ESMF_StateGet(importState, itemName='import[1]', field=f_in, _RC)
      call assign_fptr_condensed_array(f_in, x_in, _RC)
      shape_in = shape(x_in)
      n_horz = shape_in(1)
      n_ungridded = shape_in(3)

      call ESMF_StateGet(exportState, itemName='export[1]', field=f_out, _RC)
      call assign_fptr_condensed_array(f_out, x_out, _RC)
      shape_out = shape(x_out)

      _ASSERT((shape_in(1) == shape_out(1)), "horz dims are expected to be equal")
      _ASSERT((shape_in(3) == shape_out(3)), "ungridded dims are expected to be equal")

      do concurrent (horz=1:n_horz, ungrd=1:n_ungridded)
         x_out(horz, :, ungrd) = matmul(this%matrix(horz), x_in(horz, :, ungrd))
      end do

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(clock)
   end subroutine update

   subroutine write_formatted(this, unit, iotype, v_list, iostat, iomsg)
      class(VerticalRegridTransform), intent(in) :: this
      integer, intent(in) :: unit
      character(*), intent(in) :: iotype
      integer, intent(in) :: v_list(:)
      integer, intent(out) :: iostat
      character(*), intent(inout) :: iomsg

      real(ESMF_KIND_R4), pointer :: v_in(:), v_out(:)
      integer :: rc, status

      call ESMF_FieldGet(this%v_in_coord, fArrayPtr=v_in, _RC)
      call ESMF_FieldGet(this%v_out_coord, fArrayPtr=v_out, _RC)

      write(unit, "(a, a)", iostat=iostat, iomsg=iomsg) "VerticalRegridTransform(", new_line("a")
      if (iostat /= 0) return
      write(unit, "(4x, a, l1, a, 4x, a, l1, a)", iostat=iostat, iomsg=iomsg) &
           "v_in_coupler: ", associated(this%v_in_coupler), new_line("a"), &
           "v_out_coupler: ", associated(this%v_out_coupler), new_line("a")
      if (iostat /= 0) return
      write(unit, "(4x, a, *(g0, 1x))", iostat=iostat, iomsg=iomsg) "v_in_coord: ", v_in
      if (iostat /= 0) return
      write(unit, "(a)", iostat=iostat, iomsg=iomsg) new_line("a")
      if (iostat /= 0) return
      write(unit, "(4x, a, *(g0, 1x))", iostat=iostat, iomsg=iomsg) "v_out_coord: ", v_out
      if (iostat /= 0) return
      write(unit, "(a, 1x, a)", iostat=iostat, iomsg=iomsg) new_line("a"), ")"

      _UNUSED_DUMMY(iotype)
      _UNUSED_DUMMY(v_list)
   end subroutine write_formatted

   subroutine compute_interpolation_matrix_(v_in_coord, v_out_coord, matrix, rc)
      type(ESMF_Field), intent(inout) :: v_in_coord
      type(ESMF_Field), intent(inout) :: v_out_coord
      type(SparseMatrix_sp), allocatable, intent(out) :: matrix(:)
      integer, optional, intent(out) :: rc

      real(ESMF_KIND_R4), pointer :: v_in(:, :, :), v_out(:, :, :)
      integer :: shape_in(3), shape_out(3), n_horz, n_ungridded
      integer :: horz, ungrd, status

      call assign_fptr_condensed_array(v_in_coord, v_in, _RC)
      shape_in = shape(v_in)
      n_horz = shape_in(1)
      n_ungridded = shape_in(3)

      call assign_fptr_condensed_array(v_out_coord, v_out, _RC)
      shape_out = shape(v_out)
      _ASSERT((shape_in(1) == shape_out(1)), "horz dims are expected to be equal")
      _ASSERT((shape_in(3) == shape_out(3)), "ungridded dims are expected to be equal")

      allocate(matrix(n_horz))

      ! TODO: Convert to a `do concurrent` loop
      do horz = 1, n_horz
         do ungrd = 1, n_ungridded
            associate(src => v_in(horz, :, ungrd), dst => v_out(horz, :, ungrd))
              call compute_linear_map(src, dst, matrix(horz), _RC)
            end associate
         end do
      end do

      _RETURN(_SUCCESS)
   end subroutine compute_interpolation_matrix_

end module mapl3g_VerticalRegridTransform
