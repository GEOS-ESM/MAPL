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
   use mapl3g_VerticalConservativeMap, only: compute_conservative_map
   use mapl3g_CSR_SparseMatrix, only: SparseMatrix_sp => CSR_SparseMatrix_sp, matmul
   use mapl3g_FieldCondensedArray, only: assign_fptr_condensed_array
   use mapl3g_VerticalCoordinateDirection
   use esmf

   implicit none(type,external)
   private

   public :: VerticalRegridTransform
   public :: VerticalRegridParam
   public :: VERTICAL_REGRID_UNKNOWN
   public :: VERTICAL_REGRID_LINEAR
   public :: VERTICAL_REGRID_CONSERVATIVE
   public :: operator(==), operator(/=)

   !> @brief Parameters for vertical regridding between components with different vertical grids or alignments
   !!
   !! Vertical regridding supports two key scenarios:
   !! 1. **Different vertical grids**: Full interpolation required (e.g., 72 levels -> 48 levels)
   !! 2. **Same grid, different alignments (degenerate case)**: Only array reversal needed
   !!
   !! The degenerate case is detected by VerticalGridAspect by comparing vertical coordinate values.
   !! When detected, no interpolation matrix is computed; only alignment transformation is performed.
   type :: VerticalRegridParam
      type(VerticalStaggerLoc) :: stagger_in                               !< Source vertical stagger location
      type(VerticalStaggerLoc) :: stagger_out                              !< Destination vertical stagger location
      type(VerticalRegridMethod) :: method = VERTICAL_REGRID_UNKNOWN       !< Regridding method (currently only LINEAR)
      type(VerticalCoordinateDirection) :: src_alignment = VCOORD_DIRECTION_DOWN  !< Source data storage convention
      type(VerticalCoordinateDirection) :: dst_alignment = VCOORD_DIRECTION_DOWN  !< Destination data storage convention  
      logical :: is_degenerate_case = .false.                              !< True if grids match, only alignment differs
   end type VerticalRegridParam

   !> @brief Vertical regridding transform with support for data alignment
   !!
   !! This transform handles vertical data exchange between components with:
   !! - Different vertical grids (requires interpolation)
   !! - Different vertical alignments (may require array reversal)
   !! - Combination of both
   !!
   !! **Alignment Resolution Algorithm:**
   !!
   !! The transform determines whether to flip (reverse) source or destination arrays
   !! based on their vertical_alignment relative to the grid's coordinate_direction:
   !!
   !! ```
   !! src_needs_flip = (src_alignment != grid_coordinate_direction)
   !! dst_needs_flip = (dst_alignment != grid_coordinate_direction)
   !! ```
   !!
   !! **Degenerate Case (Same Grid, Different Alignments):**
   !!
   !! When source and destination have identical vertical coordinates but different
   !! alignments, the transform:
   !! 1. Skips interpolation matrix computation (is_degenerate_case = true)
   !! 2. Performs only array reversal if `src_needs_flip XOR dst_needs_flip`
   !! 3. Performs identity copy if both or neither need flipping
   !!
   !! **Full Regridding Case:**
   !!
   !! When grids differ (is_degenerate_case = false):
   !! 1. Canonicalizes coordinates to monotonically decreasing order for interpolation
   !! 2. Computes sparse interpolation matrix
   !! 3. Applies alignment transformations before/after interpolation as needed
   !!
   !! The interpolation matrix is real32. This type may need to be extended
   !! with a subtype for ESMF_KIND_R4 Fields and a subtype for ESMF_KIND_R8 Fields
   !! with real32 and real64 matrices, respectively.
   type, extends(ExtensionTransform) :: VerticalRegridTransform
      type(ESMF_Field) :: v_in_coord, v_out_coord
      type(SparseMatrix_sp), allocatable :: matrix(:)
      class(ComponentDriver), pointer :: v_in_coupler => null()
      class(ComponentDriver), pointer :: v_out_coupler => null()
      type(VerticalRegridMethod) :: method = VERTICAL_REGRID_UNKNOWN
      type(VerticalStaggerLoc) :: stagger_in
      type(VerticalStaggerLoc) :: stagger_out
      type(VerticalCoordinateDirection) :: src_alignment
      type(VerticalCoordinateDirection) :: dst_alignment
      logical :: is_degenerate_case = .false.
   contains
      procedure :: initialize
      procedure :: update
      procedure :: get_transformId
      procedure :: write_formatted
      generic :: write(formatted) => write_formatted
      procedure, private :: process_field
      procedure, private :: process_fieldbundle
   end type VerticalRegridTransform

   interface VerticalRegridTransform
      procedure :: new_VerticalRegridTransform
   end interface VerticalRegridTransform

contains

   function new_VerticalRegridTransform(v_in_coord, v_in_coupler, v_out_coord, v_out_coupler, regrid_param) result(transform)
      type(VerticalRegridTransform) :: transform
      type(ESMF_Field), intent(in) :: v_in_coord
      class(ComponentDriver), pointer, intent(in) :: v_in_coupler
      type(ESMF_Field), intent(in) :: v_out_coord
      class(ComponentDriver), pointer, intent(in) :: v_out_coupler
      type(VerticalRegridParam), intent(in) :: regrid_param

      transform%v_in_coord = v_in_coord
      transform%v_out_coord = v_out_coord

      transform%v_in_coupler => v_in_coupler
      transform%v_out_coupler => v_out_coupler

      transform%stagger_in = regrid_param%stagger_in
      transform%stagger_out = regrid_param%stagger_out
      transform%method = regrid_param%method
      transform%src_alignment = regrid_param%src_alignment
      transform%dst_alignment = regrid_param%dst_alignment
      transform%is_degenerate_case = regrid_param%is_degenerate_case
   end function new_VerticalRegridTransform

   !> Initialize the vertical regrid transform.
   !!
   !! This method validates the regridding method. The degenerate case detection
   !! (same grid with different alignments) is performed upstream by VerticalGridAspect
   !! during field matching and passed to the constructor via VerticalRegridParam.
   !!
   !! @param[inout] this            The transform object
   !! @param[in]    importState     ESMF import state (unused)
   !! @param[in]    exportState     ESMF export state (unused)
   !! @param[in]    clock           ESMF clock (unused)
   !! @param[out]   rc              Return code
   subroutine initialize(this, importState, exportState, clock, rc)
      class(VerticalRegridTransform), intent(inout) :: this
      type(ESMF_State) :: importState
      type(ESMF_State) :: exportState
      type(ESMF_Clock) :: clock
      integer, optional, intent(out) :: rc

      _ASSERT(this%method == VERTICAL_REGRID_LINEAR .or. this%method == VERTICAL_REGRID_CONSERVATIVE, "method must be LINEAR or CONSERVATIVE")

      ! Degenerate case is determined by VerticalGridAspect and passed to constructor
      ! No need to re-check here

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(importState)
      _UNUSED_DUMMY(exportState)
      _UNUSED_DUMMY(clock)
   end subroutine initialize

   !> Update routine: performs vertical regridding or flipping.
   !!
   !! This is the main execution method called during the MAPL run phase.
   !! It handles two distinct cases:
   !!
   !! 1. Degenerate case (same grid, different alignments):
   !!    - No regridding needed
   !!    - Simply flips the vertical dimension via copy_field_flipped_
   !!
   !! 2. Normal regridding case (different grids):
   !!    - Computes interpolation matrix with alignment-aware coordinate canonicalization
   !!    - Applies regridding via regrid_field_ (which handles output flipping if needed)
   !!
   !! The interpolation matrix is computed fresh on each update call. For time-varying
   !! vertical coordinates, this ensures the matrix stays consistent with current coordinates.
   !!
   !! @param[inout] this         The transform object
   !! @param[in]    importState  ESMF import state containing source field/bundle
   !! @param[in]    exportState  ESMF export state containing destination field/bundle
   !! @param[in]    clock        ESMF clock (unused)
   !! @param[out]   rc           Return code
   subroutine update(this, importState, exportState, clock, rc)
      class(VerticalRegridTransform), intent(inout) :: this
      type(ESMF_State) :: importState
      type(ESMF_State) :: exportState
      type(ESMF_Clock) :: clock
      integer, optional, intent(out) :: rc

       type(ESMF_StateItem_Flag) :: itemtype_in, itemtype_out
       type(ESMF_Field) :: f_in, f_out
       type(ESMF_FieldBundle) :: fb_in, fb_out
       integer :: status

      ! if (associated(this%v_in_coupler)) then
      !    call this%v_in_coupler%run(phase_idx=GENERIC_COUPLER_UPDATE, _RC)
      ! end if

      ! if (associated(this%v_out_coupler)) then
      !    call this%v_out_coupler%run(phase_idx=GENERIC_COUPLER_UPDATE, _RC)
      ! end if

      call ESMF_StateGet(importState, itemName=COUPLER_IMPORT_NAME, itemtype=itemtype_in, _RC)
      call ESMF_StateGet(exportState, itemName=COUPLER_EXPORT_NAME, itemtype=itemtype_out, _RC)
      _ASSERT(itemtype_out == itemtype_in, "Mismatched item types.")

      ! Compute interpolation matrix once (if needed for regridding)
      if (.not. this%is_degenerate_case) then
         call compute_interpolation_matrix_(this%method, this%v_in_coord, this%stagger_in, this%src_alignment, &
              this%v_out_coord, this%stagger_out, this%dst_alignment, this%matrix, _RC)
      end if

      ! Process fields
      if (itemtype_in == MAPL_STATEITEM_FIELD) then
         call ESMF_StateGet(importState, itemName=COUPLER_IMPORT_NAME, field=f_in, _RC)
         call ESMF_StateGet(exportState, itemName=COUPLER_EXPORT_NAME, field=f_out, _RC)
         call this%process_field(f_in, f_out, _RC)
      else if (itemtype_in == MAPL_STATEITEM_FIELDBUNDLE) then
         call ESMF_StateGet(importState, itemName=COUPLER_IMPORT_NAME, fieldBundle=fb_in, _RC)
         call ESMF_StateGet(exportState, itemName=COUPLER_EXPORT_NAME, fieldBundle=fb_out, _RC)
         call this%process_fieldbundle(fb_in, fb_out, _RC)
      else
         _FAIL("Unsupported state item type.")
      end if

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(clock)
   end subroutine update

   !> Process all fields in a FieldBundle through vertical regridding/flipping.
   !!
   !! Extracts field lists from input and output bundles and processes each
   !! field pair using process_field. Assumes matching field order between
   !! input and output bundles.
   !!
   !! @param[in]    this     The transform object
   !! @param[in]    fb_in    Input field bundle
   !! @param[inout] fb_out   Output field bundle (modified in place)
   !! @param[out]   rc       Return code
   subroutine process_fieldbundle(this, fb_in, fb_out, rc)
      class(VerticalRegridTransform), intent(in) :: this
      type(ESMF_FieldBundle), intent(in) :: fb_in
      type(ESMF_FieldBundle), intent(inout) :: fb_out
      integer, optional, intent(out) :: rc

      type(ESMF_Field), allocatable :: fieldlist_in(:), fieldlist_out(:)
      integer :: i, status

      call MAPL_FieldBundleGet(fb_in, fieldlist=fieldlist_in, _RC)
      call MAPL_FieldBundleGet(fb_out, fieldlist=fieldlist_out, _RC)

      do i = 1, size(fieldlist_in)
         call this%process_field(fieldlist_in(i), fieldlist_out(i), _RC)
      end do

      _RETURN(_SUCCESS)
   end subroutine process_fieldbundle

   !> Process a single field through vertical regridding or flipping.
   !!
   !! This method dispatches to the appropriate operation based on whether
   !! this is a degenerate case (same grid, different alignments):
   !!
   !! Degenerate case:
   !!   - Same vertical grid, different alignments
   !!   - No regridding needed, only vertical flip
   !!   - Uses copy_field_flipped_ to reverse vertical dimension
   !!
   !! Normal case:
   !!   - Different vertical grids (may have same or different alignments)
   !!   - Uses regrid_field_ with pre-computed interpolation matrix
   !!   - Regridding handles both interpolation and alignment adjustment
   !!
    !! @param[in]    this   The transform object
    !! @param[inout] f_in   Input field (ESMF requires inout for pointer access)
    !! @param[inout] f_out  Output field (modified in place)
   !! @param[out]   rc     Return code
    subroutine process_field(this, f_in, f_out, rc)
       class(VerticalRegridTransform), intent(in) :: this
       type(ESMF_Field), intent(inout) :: f_in
       type(ESMF_Field), intent(inout) :: f_out
       integer, optional, intent(out) :: rc

      integer :: status

      if (this%is_degenerate_case) then
         ! Same grid, different alignments → must flip
         _ASSERT(this%src_alignment /= this%dst_alignment, "same grid + same alignment should use NullTransform")
         call copy_field_flipped_(f_in, f_out, _RC)
      else
         ! Different grids → regrid with alignment support
         call regrid_field_(this%matrix, f_in, this%src_alignment, f_out, this%dst_alignment, _RC)
      end if

      _RETURN(_SUCCESS)
   end subroutine process_field

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

    !> Compute interpolation matrix for vertical regridding with alignment support.
    !!
    !! This subroutine performs alignment-aware coordinate canonicalization before
    !! computing the interpolation matrix. The key insight is that MAPL's interpolation
    !! algorithms assume coordinates are in a canonical DOWN orientation (traditional ESM
    !! convention where vertical index increases downward from top-of-atmosphere).
    !!
    !! Alignment Canonicalization Algorithm:
    !! --------------------------------------
    !! 1. Adjust coordinates for stagger location (center vs. edge)
    !! 2. Canonicalize to DOWN orientation for interpolation:
    !!    - DOWN alignment: coordinates already in canonical form (no flip)
    !!    - UP alignment: flip coordinates to DOWN orientation
    !! 3. Compute interpolation matrix using canonicalized coordinates
    !! 4. The matrix maps from canonical source to canonical destination
    !!
    !! The output flipping (if needed) is handled separately by regrid_field_, which
    !! applies the matrix and then flips the output if dst_alignment is UP.
    !!
    !! Example: Ocean (UP) → Atmosphere (DOWN)
    !! - Source coords (ocean): [0, 10, 20, 30] meters (depth increases down = UP)
    !! - After flip:            [30, 20, 10, 0] meters (canonical DOWN for interpolation)
    !! - Dest coords (atm):     [1000, 850, 500] hPa (pressure increases down = DOWN, no flip)
    !! - Matrix computed using flipped ocean coords and atm coords
    !! - regrid_field_ applies matrix (no output flip needed since dst is DOWN)
    !!
    !! @param[in]    method        Regridding method (LINEAR or CONSERVATIVE)
    !! @param[inout] v_in_coord    Source vertical coordinate field
    !! @param[in]    stagger_in    Source stagger location
    !! @param[in]    src_alignment Source coordinate direction (UP/DOWN)
    !! @param[inout] v_out_coord   Destination vertical coordinate field
    !! @param[in]    stagger_out   Destination stagger location
    !! @param[in]    dst_alignment Destination coordinate direction (UP/DOWN)
    !! @param[out]   matrix        Computed interpolation matrix array
    !! @param[out]   rc            Return code
    subroutine compute_interpolation_matrix_(method, v_in_coord, stagger_in, src_alignment, &
         v_out_coord, stagger_out, dst_alignment, matrix, rc)
       type(VerticalRegridMethod), intent(in) :: method
       type(ESMF_Field), intent(inout) :: v_in_coord
       type(VerticalStaggerLoc), intent(in) :: stagger_in
       type(VerticalCoordinateDirection), intent(in) :: src_alignment
       type(ESMF_Field), intent(inout) :: v_out_coord
       type(VerticalStaggerLoc), intent(in) :: stagger_out
       type(VerticalCoordinateDirection), intent(in) :: dst_alignment
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

        ! Adjust coordinates for stagger location
        call mapl_FieldGet(v_in_coord, vert_staggerloc=grid_stagger, _RC)
        vv_in = adjust_coords(v_in, grid_stagger, stagger_in, _RC)
        
        call mapl_FieldGet(v_out_coord, vert_staggerloc=grid_stagger, _RC)
        vv_out = adjust_coords(v_out, grid_stagger, stagger_out, _RC)
        
        ! Canonicalize coordinates for interpolation
        ! DOWN alignment = default ESM orientation (no flip needed)
        ! UP alignment = reversed orientation (flip to DOWN for interpolation)
        if (src_alignment == VCOORD_DIRECTION_UP) then
           vv_in = flip_vertical_coords(vv_in)
        end if
        
        if (dst_alignment == VCOORD_DIRECTION_UP) then
           vv_out = flip_vertical_coords(vv_out)
        end if

       allocate(matrix(n_horz))

       ! TODO: Convert to a `do concurrent` loop
       do horz = 1, n_horz
          do ungrd = 1, n_ungridded
             associate(src => vv_in(horz, :, ungrd), dst => vv_out(horz, :, ungrd))
               if (method == VERTICAL_REGRID_LINEAR) then
                  call compute_linear_map(src, dst, matrix(horz), _RC)
               else if (method == VERTICAL_REGRID_CONSERVATIVE) then
                  call compute_conservative_map(src, dst, matrix(horz), _RC)
               else
                  _FAIL("Unknown vertical regridding method")
               end if
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

    !> Apply vertical regridding with alignment-aware data transformation.
    !!
    !! This subroutine applies the pre-computed interpolation matrix to field data,
    !! handling alignment transformations on both input and output.
    !!
    !! Data Flow:
    !! ----------
    !! 1. Canonicalize input data to DOWN orientation (if needed)
    !!    - DOWN alignment: use data as-is
    !!    - UP alignment: flip vertical dimension
    !! 2. Apply interpolation matrix (operates on canonicalized data)
    !! 3. Transform output to destination alignment
    !!    - DOWN alignment: use result as-is
    !!    - UP alignment: flip vertical dimension
    !!
    !! The matrix was computed using canonicalized coordinates (see compute_interpolation_matrix_),
    !! so we must ensure the data is also canonicalized before applying the matrix.
    !!
    !! Example: Ocean (UP) → Atmosphere (DOWN)
    !! - Input data (ocean): [T1, T2, T3, T4] aligned with UP coords
    !! - After flip:         [T4, T3, T2, T1] (canonicalized to DOWN)
    !! - Apply matrix:       [Ta, Tb, Tc] (interpolated result in DOWN)
    !! - No output flip needed (dst is DOWN)
    !! - Final output:       [Ta, Tb, Tc] aligned with DOWN coords
    !!
    !! @param[in]    matrix        Pre-computed interpolation matrix
    !! @param[inout] f_in          Source field
    !! @param[in]    src_alignment Source coordinate direction
    !! @param[inout] f_out         Destination field (modified in place)
    !! @param[in]    dst_alignment Destination coordinate direction
    !! @param[out]   rc            Return code
    subroutine regrid_field_(matrix, f_in, src_alignment, f_out, dst_alignment, rc)
       type(SparseMatrix_sp), allocatable, intent(in) :: matrix(:)
       type(ESMF_Field), intent(inout) :: f_in, f_out
       type(VerticalCoordinateDirection), intent(in) :: src_alignment
       type(VerticalCoordinateDirection), intent(in) :: dst_alignment
       integer, optional, intent(out) :: rc

       real(ESMF_KIND_R4), pointer :: x_in(:,:,:), x_out(:,:,:)
       real(ESMF_KIND_R4), allocatable :: x_in_working(:,:,:), x_out_working(:,:,:)
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
       
       ! Canonicalize input data to match coordinate transformation
       ! DOWN alignment = default (no flip)
       ! UP alignment = reversed (flip to DOWN for interpolation)
       if (src_alignment == VCOORD_DIRECTION_UP) then
          x_in_working = flip_vertical_data(x_in)
       else
          x_in_working = x_in
       end if
       
       ! Apply interpolation matrix
       allocate(x_out_working(shape_out(1), shape_out(2), shape_out(3)))
       do concurrent (horz=1:n_horz, ungrd=1:n_ungridded)
          x_out_working(horz, :, ungrd) = matmul(matrix(horz), x_in_working(horz, :, ungrd))
       end do
       
       ! Transform output to destination alignment
       ! Matrix output is in DOWN alignment
       ! If destination is UP, flip the result
       if (dst_alignment == VCOORD_DIRECTION_UP) then
          x_out = flip_vertical_data(x_out_working)
       else
          x_out = x_out_working
       end if

       _RETURN(_SUCCESS)
    end subroutine regrid_field_

    function get_transformId(this) result(id)
       type(TransformId) :: id
       class(VerticalRegridTransform), intent(in) :: this

       id = VERTICAL_GRID_TRANSFORM_ID

       _UNUSED_DUMMY(this)
    end function get_transformId

    ! Helper function to flip vertical coordinates (3D array)
    function flip_vertical_coords(coords) result(flipped)
       real(ESMF_KIND_R4), intent(in) :: coords(:,:,:)
       real(ESMF_KIND_R4), allocatable :: flipped(:,:,:)
       
       allocate(flipped, mold=coords)
       flipped(:,:,:) = coords(:, size(coords,2):1:-1, :)
    end function flip_vertical_coords

    ! Helper function to flip vertical data (3D array)
    function flip_vertical_data(data) result(flipped)
       real(ESMF_KIND_R4), intent(in) :: data(:,:,:)
       real(ESMF_KIND_R4), allocatable :: flipped(:,:,:)
       
       allocate(flipped, mold=data)
       flipped(:,:,:) = data(:, size(data,2):1:-1, :)
    end function flip_vertical_data

   subroutine copy_field_(f_in, f_out, rc)
      type(ESMF_Field), intent(inout) :: f_in, f_out
      integer, optional, intent(out) :: rc

      real(ESMF_KIND_R4), pointer :: x_in(:,:,:), x_out(:,:,:)
      integer :: status

      call assign_fptr_condensed_array(f_in, x_in, _RC)
      call assign_fptr_condensed_array(f_out, x_out, _RC)

      x_out = x_in

      _RETURN(_SUCCESS)
   end subroutine copy_field_

   !> Copy field data with vertical dimension flipped (for degenerate case).
   !!
   !! This is used when source and destination have the same vertical grid but
   !! different alignments. No interpolation is needed - we simply reverse the
   !! vertical dimension to convert between UP and DOWN orientations.
   !!
   !! Example: 72-level grid, DOWN → UP
   !! - Input (DOWN):  [T1, T2, ..., T72] (surface at index 72)
   !! - Output (UP):   [T72, ..., T2, T1] (surface at index 1)
   !!
   !! The flip operation is symmetric (DOWN→UP and UP→DOWN use same code).
   !!
   !! @param[in]    f_in   Source field
   !! @param[inout] f_out  Destination field (modified in place)
   !! @param[out]   rc     Return code
   subroutine copy_field_flipped_(f_in, f_out, rc)
      type(ESMF_Field), intent(inout) :: f_in, f_out
      integer, optional, intent(out) :: rc

      real(ESMF_KIND_R4), pointer :: x_in(:,:,:), x_out(:,:,:)
      integer :: nlev, status

      call assign_fptr_condensed_array(f_in, x_in, _RC)
      call assign_fptr_condensed_array(f_out, x_out, _RC)

      nlev = size(x_in, 2)
      ! Flip the vertical dimension: x_out(:,k,:) = x_in(:,nlev-k+1,:)
      x_out(:, :, :) = x_in(:, nlev:1:-1, :)

      _RETURN(_SUCCESS)
   end subroutine copy_field_flipped_

end module mapl3g_VerticalRegridTransform
