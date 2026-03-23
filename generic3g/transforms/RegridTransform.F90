#include "MAPL.h"

module mapl3g_RegridTransform
   use mapl3g_TransformId
   use mapl3g_Field_API, only: MAPL_FieldClone, MAPL_FieldGet
   use mapl3g_FieldBundle_API
   use mapl3g_ExtensionTransform
   use mapl3g_TransformId
   use mapl3g_regridder_mgr
   use mapl3g_StateItem
   use mapl3g_ExtensionTransformUtils, only: bundle_types_valid
   use mapl3g_NormalizationMetadata
   use mapl3g_NormalizationType
   use mapl3g_ComponentDriver, only: ComponentDriver
   use mapl3g_CouplerPhases, only: GENERIC_COUPLER_UPDATE
   use mapl_ErrorHandling
   use mapl3g_FieldCondensedArray, only: assign_fptr_condensed_array
   use esmf

   implicit none(type,external)
   private

   public :: RegridTransform

   type, extends(ExtensionTransform) :: ScalarRegridTransform
      type(ESMF_Geom) :: src_geom
      type(ESMF_Geom) :: dst_geom
      type(EsmfRegridderParam) :: dst_param

      class(Regridder), pointer :: regrdr

      ! Integrated normalization members
      logical :: has_normalization = .false.
      logical :: field_normalized_created = .false.
      type(NormalizationMetadata) :: norm_metadata
      type(ESMF_Field) :: vcoord_field
      type(ESMF_Field) :: field_normalized
      class(ComponentDriver), pointer :: vcoord_coupler => null()
   contains
      procedure :: initialize
      procedure :: update
      procedure :: change_geoms
      procedure :: get_transformId
      procedure :: update_transform
      procedure :: regrid_with_normalization
      procedure, private :: compute_layer_thickness
   end type ScalarRegridTransform

   interface RegridTransform
      module procedure :: new_ScalarRegridTransform
   end interface RegridTransform

contains

   function new_ScalarRegridTransform(src_geom, dst_geom, dst_param, &
        vcoord_field, vcoord_coupler, norm_metadata) result(transform)
      type(ScalarRegridTransform) :: transform
      type(ESMF_Geom), intent(in) :: src_geom
      type(ESMF_Geom), intent(in) :: dst_geom
      type(EsmfRegridderParam), intent(in) :: dst_param
      type(ESMF_Field), optional, intent(in) :: vcoord_field
      class(ComponentDriver), optional, pointer, intent(in) :: vcoord_coupler
      type(NormalizationMetadata), optional, intent(in) :: norm_metadata

      transform%src_geom = src_geom
      transform%dst_geom = dst_geom
      transform%dst_param = dst_param

      ! Store normalization info if all three are provided
      if (present(vcoord_field) .and. present(vcoord_coupler) .and. present(norm_metadata)) then
         transform%has_normalization = .true.
         transform%vcoord_field = vcoord_field
         transform%vcoord_coupler => vcoord_coupler
         transform%norm_metadata = norm_metadata
      end if

   end function new_ScalarRegridTransform

   subroutine change_geoms(this, src_geom, dst_geom)
      class(ScalarRegridTransform), intent(inout) :: this
      type(ESMF_Geom), optional, intent(in) :: src_geom
      type(ESMF_Geom), optional, intent(in) :: dst_geom
      if (present(src_geom)) this%src_geom = src_geom
      if (present(dst_geom)) this%dst_geom = dst_geom

   end subroutine change_geoms

   subroutine initialize(this, importState, exportState, clock, rc)
      class(ScalarRegridTransform), intent(inout) :: this
      type(ESMF_State)      :: importState
      type(ESMF_State)      :: exportState
      type(ESMF_Clock)      :: clock      
      integer, optional, intent(out) :: rc

      integer :: status
      type(RegridderSpec) :: spec
      type(RegridderManager), pointer :: regridder_manager

      regridder_manager => get_regridder_manager()

      this%src_geom = get_geom(importState, COUPLER_IMPORT_NAME)
      this%dst_geom = get_geom(exportState, COUPLER_EXPORT_NAME)
      spec = RegridderSpec(this%dst_param, this%src_geom, this%dst_geom)
      this%regrdr => regridder_manager%get_regridder(spec, _RC)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(clock)

   contains

      function get_geom(state, itemName, rc) result(geom)
         type(ESMF_State), intent(inout) :: state
         character(*), intent(in) :: itemName
         integer, optional, intent(out) :: rc

         integer :: status
         type(ESMF_StateItem_Flag) :: itemType
         type(ESMF_Field) :: f
         type(ESMF_FieldBundle) :: fb
         type(ESMF_Geom) :: geom

         type(ESMF_Geom), allocatable :: geom_

         call ESMF_StateGet(state, itemName, itemType=itemType, _RC)
         if (itemType == ESMF_STATEITEM_FIELD) then
            call ESMF_StateGet(state, itemName, field=f, _RC)
            call MAPL_FieldGet(f, geom=geom_, _RC)
         elseif (itemType == ESMF_STATEITEM_FIELDBUNDLE) then
            call ESMF_StateGet(state, itemName, fieldBundle=fb, _RC)
            call MAPL_FieldBundleGet(fb, geom=geom_, _RC)
         else
            _FAIL('unsupported itemType')
         end if

         _ASSERT(allocated(geom_), 'Guard that geom is allocated before we return.')

         geom = geom_

         _RETURN(_SUCCESS)
      end function get_geom
   end subroutine initialize


   subroutine update(this, importState, exportState, clock, rc)
      class(ScalarRegridTransform), intent(inout) :: this
      type(ESMF_State)      :: importState
      type(ESMF_State)      :: exportState
      type(ESMF_Clock)      :: clock      
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_Field) :: f_in, f_out
      type(ESMF_FieldBundle) :: fb_in, fb_out
      type(ESMF_StateItem_Flag) :: itemType_in, itemType_out
      type(ESMF_Geom), allocatable :: geom_in, geom_out
      logical :: do_transform
      type(FieldBundleType_Flag) :: field_bundle_type

      call ESMF_StateGet(importState, itemName=COUPLER_IMPORT_NAME, itemType=itemType_in, _RC)
      call ESMF_StateGet(exportState, itemName=COUPLER_EXPORT_NAME, itemType=itemType_out, _RC)

      _ASSERT(itemType_in == itemType_out, 'Regridder requires same itemType for input and output.')

      if (itemType_in == MAPL_STATEITEM_FIELD) then
         call ESMF_StateGet(importState, itemName=COUPLER_IMPORT_NAME, field=f_in, _RC)
         call ESMF_StateGet(exportState, itemName=COUPLER_EXPORT_NAME, field=f_out, _RC)
         allocate(geom_in, geom_out)
         call ESMF_FieldGet(f_in, geom=geom_in, _RC)
         call ESMF_FieldGet(f_out, geom=geom_out, _RC)
         call this%update_transform(geom_in, geom_out)

         ! Perform regrid with integrated normalization if needed. The
         ! presence of layers is determined upstream via VerticalStaggerLoc
         ! when the transform is constructed; there is no need to gate on
         ! the runtime rank here.
         if (this%has_normalization .and. associated(this%vcoord_coupler)) then
            call this%regrid_with_normalization(f_in, f_out, _RC)
         else
            call this%regrdr%regrid(f_in, f_out, _RC)
         end if
      else ! bundle case
         call ESMF_StateGet(importState, itemName=COUPLER_IMPORT_NAME, fieldBundle=fb_in, _RC)
         call ESMF_StateGet(exportState, itemName=COUPLER_EXPORT_NAME, fieldBundle=fb_out, _RC)
         call bundle_types_valid(fb_in, fb_out, _RC)
         call MAPL_FieldBundleGet(fb_in, geom=geom_in, _RC)
         call MAPL_FieldBundleGet(fb_out, geom=geom_out, _RC)
         _ASSERT(allocated(geom_in), 'should be allocated by here')
         _ASSERT(allocated(geom_out), 'should be allocated by here')

         call this%update_transform(geom_in, geom_out)
         do_transform = .true.
         call MAPL_FieldBundleGet(fb_in, fieldBundleType= field_bundle_type, _RC)
         if (field_bundle_type == FIELDBUNDLETYPE_BRACKET .or. field_bundle_type == FIELDBUNDLETYPE_VECTORBRACKET) then 
            call MAPL_FieldBundleGet(fb_in, bracket_updated=do_transform, _RC)
         end if
         if (do_transform) then
            call this%regrdr%regrid(fb_in, fb_out, _RC)
         end if
      end if

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(clock)
   end subroutine update

   subroutine regrid_with_normalization(this, field_in, field_out, rc)
      class(ScalarRegridTransform), intent(inout) :: this
      type(ESMF_Field), intent(inout) :: field_in, field_out
      integer, optional, intent(out) :: rc

      type(ESMF_TypeKind_Flag) :: tk
      integer :: status

      call ESMF_FieldGet(field_in, typekind=tk, _RC)

      if (tk == ESMF_TYPEKIND_R4) then
         call regrid_with_normalization_r4(this, field_in, field_out, _RC)
      elseif (tk == ESMF_TYPEKIND_R8) then
         call regrid_with_normalization_r8(this, field_in, field_out, _RC)
      else
         _FAIL('Only R4 and R8 supported for normalization')
      end if

      _RETURN(_SUCCESS)
   end subroutine regrid_with_normalization

   subroutine regrid_with_normalization_r4(this, field_in, field_out, rc)
      class(ScalarRegridTransform), intent(inout) :: this
      type(ESMF_Field), intent(inout) :: field_in, field_out
      integer, optional, intent(out) :: rc

      real(ESMF_KIND_R4), pointer :: x_in(:,:,:), x_out(:,:,:), x_norm(:,:,:)
      real(ESMF_KIND_R4), allocatable :: dp(:,:,:)
      type(ESMF_TypeKind_Flag) :: tk_field, tk_coord
      integer :: status

      ! Create intermediate field on first call
      if (.not. this%field_normalized_created) then
         call MAPL_FieldClone(field_in, this%field_normalized, _RC)
         this%field_normalized_created = .true.
      end if

      ! Run vertical coordinate coupler to update values if needed
      if (associated(this%vcoord_coupler)) then
         call this%vcoord_coupler%run(phase_idx=GENERIC_COUPLER_UPDATE, _RC)
      end if

      ! Sanity check: main field and coord field must have same typekind
      call ESMF_FieldGet(field_in,       typekind=tk_field, _RC)
      call ESMF_FieldGet(this%vcoord_field, typekind=tk_coord, _RC)
      _ASSERT(tk_field == ESMF_TYPEKIND_R4,  'regrid_with_normalization_r4 requires R4 field')
      _ASSERT(tk_coord == ESMF_TYPEKIND_R4,  'vcoord_field must be R4 for R4 normalization')

      ! Get condensed-array views: (fused horizontal, vertical, fused non-geometric)
      call assign_fptr_condensed_array(field_in,            x_in,   _RC)
      call assign_fptr_condensed_array(this%field_normalized, x_norm, _RC)
      call assign_fptr_condensed_array(field_out,           x_out,  _RC)

      ! Compute layer thickness from vertical coordinate field (condensed layout)
      dp = this%compute_layer_thickness(_RC)

      ! Denormalize: [per-layer] -> layer-integrated quantity
      x_norm = x_in * dp

      ! Horizontal conservative regrid of normalized field
      call this%regrdr%regrid(this%field_normalized, field_out, _RC)

      ! Renormalize: layer-integrated -> per-layer quantity
      x_out = x_out / dp

      _RETURN(_SUCCESS)
   end subroutine regrid_with_normalization_r4

   subroutine regrid_with_normalization_r8(this, field_in, field_out, rc)
      class(ScalarRegridTransform), intent(inout) :: this
      type(ESMF_Field), intent(inout) :: field_in, field_out
      integer, optional, intent(out) :: rc

      real(ESMF_KIND_R8), pointer :: x_in(:,:,:), x_out(:,:,:), x_norm(:,:,:)
      real(ESMF_KIND_R8), allocatable :: dp(:,:,:)
      type(ESMF_TypeKind_Flag) :: tk_field, tk_coord
      integer :: status

      ! Create intermediate field on first call
      if (.not. this%field_normalized_created) then
         call MAPL_FieldClone(field_in, this%field_normalized, _RC)
         this%field_normalized_created = .true.
      end if

      ! Run vertical coordinate coupler to update values if needed
      if (associated(this%vcoord_coupler)) then
         call this%vcoord_coupler%run(phase_idx=GENERIC_COUPLER_UPDATE, _RC)
      end if

      ! Sanity check: main field and coord field must have same typekind
      call ESMF_FieldGet(field_in,       typekind=tk_field, _RC)
      call ESMF_FieldGet(this%vcoord_field, typekind=tk_coord, _RC)
      _ASSERT(tk_field == ESMF_TYPEKIND_R8,  'regrid_with_normalization_r8 requires R8 field')
      _ASSERT(tk_coord == ESMF_TYPEKIND_R8,  'vcoord_field must be R8 for R8 normalization')

      ! Get condensed-array views: (fused horizontal, vertical, fused non-geometric)
      call assign_fptr_condensed_array(field_in,            x_in,   _RC)
      call assign_fptr_condensed_array(this%field_normalized, x_norm, _RC)
      call assign_fptr_condensed_array(field_out,           x_out,  _RC)

      ! Compute layer thickness from vertical coordinate field (condensed layout)
      dp = this%compute_layer_thickness(_RC)

      ! Denormalize: [per-layer] -> layer-integrated quantity
      x_norm = x_in * dp

      ! Horizontal conservative regrid of normalized field
      call this%regrdr%regrid(this%field_normalized, field_out, _RC)

      ! Renormalize: layer-integrated -> per-layer quantity
      x_out = x_out / dp

      _RETURN(_SUCCESS)
   end subroutine regrid_with_normalization_r8

   function compute_layer_thickness(this, rc) result(dp)
      class(ScalarRegridTransform), intent(inout) :: this
      integer, optional, intent(out) :: rc
      real, allocatable :: dp(:,:,:)

      type(ESMF_TypeKind_Flag) :: tk
      integer :: status

      call ESMF_FieldGet(this%vcoord_field, typekind=tk, _RC)

      if (tk == ESMF_TYPEKIND_R4) then
         dp = compute_layer_thickness_r4(this%vcoord_field, _RC)
      elseif (tk == ESMF_TYPEKIND_R8) then
         dp = compute_layer_thickness_r8(this%vcoord_field, _RC)
      else
         _FAIL('Only R4 and R8 coord fields supported for dp')
      end if

      _RETURN(_SUCCESS)
   end function compute_layer_thickness

   function compute_layer_thickness_r4(vcoord_field, rc) result(dp)
      type(ESMF_Field), intent(inout) :: vcoord_field
        integer, optional, intent(out) :: rc
        real(ESMF_KIND_R4), allocatable :: dp(:,:,:)

        real(ESMF_KIND_R4), pointer :: vcoord(:,:,:)
        integer :: n_horz, n_levels, n_layers, n_ungridded, k
        integer :: status

       ! Get condensed vertical coordinate field (edges) in fused layout
       call assign_fptr_condensed_array(vcoord_field, vcoord, _RC)

      n_horz      = size(vcoord, 1)
      n_levels    = size(vcoord, 2)
      n_layers    = n_levels - 1
      n_ungridded = size(vcoord, 3)

      allocate(dp(n_horz, n_layers, n_ungridded), _STAT)

      do k = 1, n_layers
         dp(:,k,:) = vcoord(:,k+1,:) - vcoord(:,k,:)
      end do

      _RETURN(_SUCCESS)
   end function compute_layer_thickness_r4

   function compute_layer_thickness_r8(vcoord_field, rc) result(dp)
      type(ESMF_Field), intent(inout) :: vcoord_field
        integer, optional, intent(out) :: rc
        real(ESMF_KIND_R8), allocatable :: dp(:,:,:)

        real(ESMF_KIND_R8), pointer :: vcoord(:,:,:)
        integer :: n_horz, n_levels, n_layers, n_ungridded, k
        integer :: status

       ! Get condensed vertical coordinate field (edges) in fused layout
       call assign_fptr_condensed_array(vcoord_field, vcoord, _RC)

      n_horz      = size(vcoord, 1)
      n_levels    = size(vcoord, 2)
      n_layers    = n_levels - 1
      n_ungridded = size(vcoord, 3)

      allocate(dp(n_horz, n_layers, n_ungridded), _STAT)

      do k = 1, n_layers
         dp(:,k,:) = vcoord(:,k+1,:) - vcoord(:,k,:)
      end do

      _RETURN(_SUCCESS)
   end function compute_layer_thickness_r8

   subroutine update_transform(this, src_geom, dst_geom, rc)
      class(ScalarRegridTransform), intent(inout) :: this
      type(ESMF_Geom), intent(in) :: src_geom
      type(ESMF_Geom), intent(in) :: dst_geom
      integer, optional, intent(out) :: rc

      logical :: scr_geom_changed, dst_geom_changed
      type(RegridderSpec) :: spec
      type(RegridderManager), pointer :: regridder_manager
      integer :: status

      scr_geom_changed = ESMF_GEOMMATCH_GEOMALIAS /= ESMF_GeomMatch(src_geom, this%src_geom)
      dst_geom_changed = ESMF_GEOMMATCH_GEOMALIAS /= ESMF_GeomMatch(dst_geom, this%dst_geom)
      if (scr_geom_changed) call this%change_geoms(src_geom=src_geom)      
      if (dst_geom_changed) call this%change_geoms(dst_geom=dst_geom)
      if (scr_geom_changed .or. dst_geom_changed) then
         regridder_manager => get_regridder_manager()
         spec = RegridderSpec(this%dst_param, this%src_geom, this%dst_geom)
         this%regrdr => regridder_manager%get_regridder(spec, _RC)
      end if
      _RETURN(_SUCCESS)
   end subroutine update_transform

   function get_transformId(this) result(id)
      type(TransformId) :: id
      class(ScalarRegridTransform), intent(in) :: this

      id = GEOM_TRANSFORM_ID

      _UNUSED_DUMMY(this)
   end function get_transformId

end module mapl3g_RegridTransform
