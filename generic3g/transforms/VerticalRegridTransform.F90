#include "MAPL.h"

module mapl3g_VerticalRegridTransform
   use mapl3g_TransformId
   use mapl_ErrorHandling
   use mapl3g_FieldBundle_API
   use mapl3g_StateItem
   use mapl3g_ExtensionTransform
   use mapl3g_ComponentDriver
   use mapl3g_CouplerPhases, only: GENERIC_COUPLER_UPDATE
   use mapl3g_VerticalRegridMethod
   use mapl3g_VerticalLinearMap, only: compute_linear_map
   use mapl3g_CSR_SparseMatrix, only: SparseMatrix_sp => CSR_SparseMatrix_sp, matmul
   use mapl3g_FieldCondensedArray, only: assign_fptr_condensed_array
   use esmf

   implicit none(type,external)
   private

   public :: VerticalRegridTransform
   public :: VERTICAL_REGRID_UNKNOWN
   public :: VERTICAL_REGRID_LINEAR
   public :: VERTICAL_REGRID_CONSERVATIVE
   public :: operator(==), operator(/=)
!   public :: COUPLER_IMPORT_NAME
!   public :: COUPLER_EXPORT_NAME
!  import[1]
!  export[1]

   type, extends(ExtensionTransform) :: VerticalRegridTransform
      type(ESMF_Field) :: v_in_coord, v_out_coord
      type(SparseMatrix_sp), allocatable :: matrix(:)
      class(ComponentDriver), pointer :: v_in_coupler => null()
      class(ComponentDriver), pointer :: v_out_coupler => null()
      type(VerticalRegridMethod) :: method = VERTICAL_REGRID_UNKNOWN
   contains
      procedure :: initialize
      procedure :: update
      procedure :: get_transformId
      procedure :: write_formatted
      generic :: write(formatted) => write_formatted
   end type VerticalRegridTransform

   interface VerticalRegridTransform
      procedure :: new_VerticalRegridTransform
   end interface VerticalRegridTransform

!   character(len=*), parameter :: COUPLER_IMPORT_NAME = 'coupler_import'
!   character(len=*), parameter :: COUPLER_EXPORT_NAME = 'coupler_export'

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

      type(ESMF_StateItem_Flag) :: itemtype_in, itemtype_out
      type(ESMF_Field) :: f_in, f_out
      type(ESMF_FieldBundle) :: fb_in, fb_out
      type(ESMF_Field), allocatable :: fieldlist_in(:), fieldlist_out(:)
      integer :: status
      integer :: i

      ! if (associated(this%v_in_coupler)) then
      !    call this%v_in_coupler%run(phase_idx=GENERIC_COUPLER_UPDATE, _RC)
      ! end if

      ! if (associated(this%v_out_coupler)) then
      !    call this%v_out_coupler%run(phase_idx=GENERIC_COUPLER_UPDATE, _RC)
      ! end if

      call compute_interpolation_matrix_(this%v_in_coord, this%v_out_coord, this%matrix, _RC)

      call ESMF_StateGet(importState, itemName=COUPLER_IMPORT_NAME, itemtype=itemtype_in, _RC)
      call ESMF_StateGet(exportState, itemName=COUPLER_EXPORT_NAME, itemtype=itemtype_out, _RC)
      _ASSERT(itemtype_out == itemtype_in, "Mismathed item types.")

      if (itemtype_in == MAPL_STATEITEM_FIELD) then
         call ESMF_StateGet(importState, itemName=COUPLER_IMPORT_NAME, field=f_in, _RC)
         call ESMF_StateGet(exportState, itemName=COUPLER_EXPORT_NAME, field=f_out, _RC)
         call regrid_field_(this%matrix, f_in, f_out, _RC)
      elseif (itemtype_in == MAPL_STATEITEM_FIELDBUNDLE) then
         call ESMF_StateGet(importState, itemName=COUPLER_IMPORT_NAME, fieldBundle=fb_in, _RC)
         call ESMF_StateGet(exportState, itemName=COUPLER_EXPORT_NAME, fieldBundle=fb_out, _RC)
         call MAPL_FieldBundleGet(fb_in, fieldlist=fieldlist_in, _RC)
         call MAPL_FieldBundleGet(fb_out, fieldlist=fieldlist_out, _RC)
         do i = 1, size(fieldlist_in)
            call regrid_field_(this%matrix, fieldlist_in(i), fieldlist_out(i), _RC)
         end do
      else
         _FAIL("Unsupported state item type.")
      end if

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

   subroutine regrid_field_(matrix, f_in, f_out, rc)
      type(SparseMatrix_sp), allocatable, intent(in) :: matrix(:)
      type(ESMF_Field), intent(inout) :: f_in, f_out
      integer, optional, intent(out) :: rc

      real(ESMF_KIND_R4), pointer :: x_in(:,:,:), x_out(:,:,:)
      integer :: shape_in(3), shape_out(3), n_horz, n_ungridded
      integer :: horz, ungrd, status

      call assign_fptr_condensed_array(f_in, x_in, _RC)
      shape_in = shape(x_in)
      call assign_fptr_condensed_array(f_out, x_out, _RC)
      shape_out = shape(x_out)
      _ASSERT((shape_in(1) == shape_out(1)), "horz dims are expected to be equal")
      _ASSERT((shape_in(3) == shape_out(3)), "ungridded dims are expected to be equal")

      n_horz = shape_in(1)
      n_ungridded = shape_in(3)
      do concurrent (horz=1:n_horz, ungrd=1:n_ungridded)
         x_out(horz, :, ungrd) = matmul(matrix(horz), x_in(horz, :, ungrd))
      end do

      _RETURN(_SUCCESS)
   end subroutine regrid_field_

   function get_transformId(this) result(id)
      type(TransformId) :: id
      class(VerticalRegridTransform), intent(in) :: this

      id = VERTICAL_GRID_TRANSFORM_ID
   end function get_transformId

end module mapl3g_VerticalRegridTransform
