#include "MAPL_Generic.h"

module mapl3g_VerticalRegridAction

   use mapl_ErrorHandling
   use mapl3g_ExtensionAction
   use mapl3g_GriddedComponentDriver
   use mapl3g_CouplerPhases, only: GENERIC_COUPLER_UPDATE
   use mapl3g_VerticalRegridMethod
   use mapl3g_VerticalLinearMap, only: compute_linear_map
   use mapl3g_CSR_SparseMatrix, only: SparseMatrix_sp => CSR_SparseMatrix_sp, matmul
   use esmf

   implicit none
   private

   public :: VerticalRegridAction
   public :: VERTICAL_REGRID_UNKNOWN
   public :: VERTICAL_REGRID_LINEAR
   public :: VERTICAL_REGRID_CONSERVATIVE
   public :: operator(==), operator(/=)

   type, extends(ExtensionAction) :: VerticalRegridAction
      type(ESMF_Field) :: v_in_coord, v_out_coord
      type(SparseMatrix_sp) :: matrix
      type(GriddedComponentDriver), pointer :: v_in_coupler => null()
      type(GriddedComponentDriver), pointer :: v_out_coupler => null()
      type(VerticalRegridMethod) :: method = VERTICAL_REGRID_UNKNOWN
   contains
      procedure :: initialize
      procedure :: update
      procedure :: write_formatted
      generic :: write(formatted) => write_formatted
   end type VerticalRegridAction

   interface VerticalRegridAction
      procedure :: new_VerticalRegridAction
   end interface VerticalRegridAction

contains

   function new_VerticalRegridAction(v_in_coord, v_in_coupler, v_out_coord, v_out_coupler, method) result(action)
      type(VerticalRegridAction) :: action
      type(ESMF_Field), intent(in) :: v_in_coord
      type(GriddedComponentDriver), pointer, intent(in) :: v_in_coupler
      type(ESMF_Field), intent(in) :: v_out_coord
      type(GriddedComponentDriver), pointer, intent(in) :: v_out_coupler
      type(VerticalRegridMethod), intent(in) :: method

      action%v_in_coord = v_in_coord
      action%v_out_coord = v_out_coord

      action%v_in_coupler => v_in_coupler
      action%v_out_coupler => v_out_coupler

      action%method = method
   end function new_VerticalRegridAction

   subroutine initialize(this, importState, exportState, clock, rc)
      use esmf
      class(VerticalRegridAction), intent(inout) :: this
      type(ESMF_State) :: importState
      type(ESMF_State) :: exportState
      type(ESMF_Clock) :: clock
      integer, optional, intent(out) :: rc

      real(ESMF_KIND_R4), pointer :: vcoord_in(:)
      real(ESMF_KIND_R4), pointer :: vcoord_out(:)
      integer :: status

      _ASSERT(this%method == VERTICAL_REGRID_LINEAR, "regrid method can only be linear")

      ! if (associated(this%v_in_coupler)) then
      !    call this%v_in_coupler%initialize(_RC)
      ! end if

      ! if (associated(this%v_out_coupler)) then
      !    call this%v_out_coupler%initialize(_RC)
      ! end if

      call ESMF_FieldGet(this%v_in_coord, fArrayPtr=vcoord_in, _RC)
      call ESMF_FieldGet(this%v_out_coord, fArrayPtr=vcoord_out, _RC)

      call compute_linear_map(vcoord_in, vcoord_out, this%matrix, RC)

      _RETURN(_SUCCESS)
   end subroutine initialize

   subroutine update(this, importState, exportState, clock, rc)
      use esmf
      class(VerticalRegridAction), intent(inout) :: this
      type(ESMF_State) :: importState
      type(ESMF_State) :: exportState
      type(ESMF_Clock) :: clock
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_Field) :: f_in, f_out


      real(ESMF_KIND_R4), pointer :: x_in(:,:,:)
      real(ESMF_KIND_R4), pointer :: x_out(:,:,:)

      real(ESMF_KIND_R4), pointer :: v_in(:)
      real(ESMF_KIND_R4), pointer :: v_out(:)

      integer :: istart, iend, jstart, jend, i, j

      ! if (associated(this%v_in_coupler)) then
      !    call this%v_in_coupler%run(phase_idx=GENERIC_COUPLER_UPDATE, _RC)
      ! end if

      ! if (associated(this%v_out_coupler)) then
      !    call this%v_out_coupler%run(phase_idx=GENERIC_COUPLER_UPDATE, _RC)
      ! end if

      call ESMF_StateGet(importState, itemName='import[1]', field=f_in, _RC)
      call ESMF_FieldGet(f_in, fArrayPtr=x_in, _RC)

      call ESMF_StateGet(exportState, itemName='export[1]', field=f_out, _RC)
      call ESMF_FieldGet(f_out, fArrayPtr=x_out, _RC)

      istart = lbound(x_out, 1); iend = ubound(x_out, 1)
      jstart = lbound(x_out, 2); jend = ubound(x_out, 2)

      do concurrent (i=istart:iend, j=jstart:jend)
         x_out(i, j, :) = matmul(this%matrix, x_in(i, j, :))
      end do

      _RETURN(_SUCCESS)
   end subroutine update

   subroutine write_formatted(this, unit, iotype, v_list, iostat, iomsg)
      class(VerticalRegridAction), intent(in) :: this
      integer, intent(in) :: unit
      character(*), intent(in) :: iotype
      integer, intent(in) :: v_list(:)
      integer, intent(out) :: iostat
      character(*), intent(inout) :: iomsg

      real(ESMF_KIND_R4), pointer :: v_in(:), v_out(:)
      integer :: rc, status

      call ESMF_FieldGet(this%v_in_coord, fArrayPtr=v_in, _RC)
      call ESMF_FieldGet(this%v_out_coord, fArrayPtr=v_out, _RC)

      write(unit, "(a, a)", iostat=iostat, iomsg=iomsg) "VerticalRegridAction(", new_line("a")
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

end module mapl3g_VerticalRegridAction
