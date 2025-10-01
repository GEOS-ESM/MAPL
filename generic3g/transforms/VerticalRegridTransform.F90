#include "MAPL.h"

module mapl3g_VerticalRegridTransform
   use mapl3g_TransformId
   use mapl3g_Field_API
   use mapl_ErrorHandling
   use mapl3g_FieldBundle_API
   use mapl3g_StateItem
   use mapl3g_ExtensionTransform
   use mapl3g_ComponentDriver
   use mapl3g_CouplerPhases, only: GENERIC_COUPLER_UPDATE
   use mapl3g_VerticalRegridMethod
   use mapl3g_VerticalStaggerLoc
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
      type(VerticalStaggerLoc) :: stagger_in
      type(VerticalStaggerLoc) :: stagger_out
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

   function new_VerticalRegridTransform(v_in_coord, v_in_coupler, stagger_in, v_out_coord, v_out_coupler, stagger_out, method) result(transform)
      type(VerticalRegridTransform) :: transform
      type(ESMF_Field), intent(in) :: v_in_coord
      class(ComponentDriver), pointer, intent(in) :: v_in_coupler
      type(VerticalStaggerLoc), intent(in) :: stagger_in
      type(ESMF_Field), intent(in) :: v_out_coord
      class(ComponentDriver), pointer, intent(in) :: v_out_coupler
      type(VerticalStaggerLoc), intent(in) :: stagger_out
      type(VerticalRegridMethod), optional, intent(in) :: method

      transform%v_in_coord = v_in_coord
      transform%v_out_coord = v_out_coord

      transform%v_in_coupler => v_in_coupler
      transform%v_out_coupler => v_out_coupler

      transform%stagger_in = stagger_in
      transform%stagger_out = stagger_out

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

      _ASSERT(this%method == VERTICAL_REGRID_LINEAR, "conservative not supported (yet)")
      call compute_interpolation_matrix_(this%v_in_coord, this%stagger_in, this%v_out_coord, this%stagger_out, this%matrix, _RC)

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

   subroutine compute_interpolation_matrix_(v_in_coord, stagger_in, v_out_coord, stagger_out, matrix, rc)
      type(ESMF_Field), intent(inout) :: v_in_coord
      type(VerticalStaggerLoc), intent(in) :: stagger_in
      type(ESMF_Field), intent(inout) :: v_out_coord
      type(VerticalStaggerLoc), intent(in) :: stagger_out
      type(SparseMatrix_sp), allocatable, intent(out) :: matrix(:)
      integer, optional, intent(out) :: rc

      real(ESMF_KIND_R4), pointer :: v_in(:, :, :), v_out(:, :, :)
      integer :: shape_in(3), shape_out(3), n_horz, n_ungridded
      integer :: horz, ungrd, status
      type(VerticalStaggerLoc) :: grid_stagger
      real(ESMF_KIND_R4), allocatable :: vv_in(:, :, :), vv_out(:, :, :)

      call assign_fptr_condensed_array(v_in_coord, v_in, _RC)
      shape_in = shape(v_in)
      n_horz = shape_in(1)
      n_ungridded = shape_in(3)

      call assign_fptr_condensed_array(v_out_coord, v_out, _RC)
      shape_out = shape(v_out)
      _ASSERT((shape_in(1) == shape_out(1)), "horz dims are expected to be equal")
      _ASSERT((shape_in(3) == shape_out(3)), "ungridded dims are expected to be equal")

      call mapl_FieldGet(v_in_coord, vert_staggerloc=grid_stagger, _RC)
      vv_in = adjust_coords(v_in, grid_stagger, stagger_in, _RC)
      call mapl_FieldGet(v_out_coord, vert_staggerloc=grid_stagger, _RC)
      vv_out = adjust_coords(v_out, grid_stagger, stagger_out, _RC)

      allocate(matrix(n_horz))

      ! TODO: Convert to a `do concurrent` loop
      do horz = 1, n_horz
         do ungrd = 1, n_ungridded
            associate(src => vv_in(horz, :, ungrd), dst => vv_out(horz, :, ungrd))
              call compute_linear_map(src, dst, matrix(horz), _RC)
            end associate
         end do
      end do

      _RETURN(_SUCCESS)
   end subroutine compute_interpolation_matrix_

   function adjust_coords(v, grid_stagger, field_stagger, rc) result(vv)
      real(kind=ESMF_KIND_R4), allocatable :: vv(:,:,:)
      real(kind=ESMF_KIND_R4), intent(in) :: v(:,:,:)
      type(VerticalStaggerLoc), intent(in) :: grid_stagger
      type(VerticalStaggerLoc), intent(in) :: field_stagger
      integer, optional, intent(out)  :: rc

      integer :: status
      integer :: n

      if (grid_stagger == field_stagger) then
         vv = v
         _RETURN(_SUCCESS)
      end if

      if (grid_stagger == VERTICAL_STAGGER_EDGE) then
         n = size(v,2)
         vv = (v(:,1:n-1,:) + v(:,2:n,:)) / 2
         _RETURN(_SUCCESS)
      end if

      allocate(vv(0,0,0))
      _FAIL("Cannot have edge variable on centered vertical grid.")
   end function adjust_coords

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
