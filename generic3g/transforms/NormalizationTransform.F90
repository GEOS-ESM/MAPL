#include "MAPL.h"
#include "unused_dummy.H"

!> @brief Transform that multiplies fields by auxiliary normalization fields
!!
!! This transform normalizes fields for conservative regridding by multiplying
!! with an auxiliary field (e.g., DELP, DZ) and a scale factor (e.g., 1/g).
!! For example, mixing ratios [kg/kg] are converted to column masses [kg/m²]
!! by multiplying with DELP/g.
!!
!! @par Limitation (Phase 2, Task 2.2):
!! This is a simplified implementation for testing the core normalization logic.
!! The auxiliary field must be provided at construction time and kept valid
!! externally. The framework CANNOT use this in production form.
!!
!! @par Future Work (Task 2.2c or later):
!! Full coupler infrastructure needed:
!! - Auxiliary field obtained via coupler (similar to VerticalRegridTransform)
!! - Coupler run during update() to ensure latest values
!! - Handles case where auxiliary field is itself an extension (e.g., regridded)
!! - See VerticalGridAspect::make_transform and VerticalRegridTransform pattern
!!
module mapl3g_NormalizationTransform
   use mapl3g_TransformId
   use mapl3g_StateItem
   use mapl3g_ExtensionTransform
   use mapl3g_ExtensionTransformUtils, only: bundle_types_valid
   use MAPL_FieldUtils
   use mapl_ErrorHandling
   use esmf

   implicit none(type,external)
   private

   public :: NormalizationTransform

    !> Transform that multiplies a field by an auxiliary normalization field
    !!
    !! This transform normalizes fields by multiplying with an auxiliary field
    !! (e.g., DELP, DZ) scaled by a constant factor (e.g., 1/g).
    !!
    !! LIMITATION (Phase 2, Task 2.2):
    !! - Auxiliary field is stored directly in the transform
    !! - No coupler support yet - auxiliary field must be passed at construction
    !! - Framework cannot use this in production form
    !! - Future work: Add coupler infrastructure to update auxiliary field values
    !!   (see VerticalRegridTransform pattern with v_in_coupler/v_out_coupler)
     type, extends(ExtensionTransform) :: NormalizationTransform
        private
        character(:), allocatable :: aux_field_name     ! Name of auxiliary field (for debugging/logging)
        real :: scale_factor = 0.0                      ! Scaling factor (e.g., 1/g for DELP)
        type(ESMF_Field) :: aux_field                   ! Auxiliary field (DELP or DZ)
        ! TODO (future): Add coupler support
        ! class(ComponentDriver), pointer :: aux_coupler => null()
     contains
        procedure :: initialize
        procedure :: update
        procedure :: get_transformId
     end type NormalizationTransform


   interface NormalizationTransform
      procedure new_NormalizationTransform
   end interface NormalizationTransform

contains

    !> Constructor for NormalizationTransform
    !!
    !! @param aux_field_name Name of auxiliary field (for debugging/logging)
    !! @param scale_factor   Scaling factor to apply (e.g., 1/g for DELP)
    !! @param aux_field      Auxiliary field to multiply with
    !!
    !! LIMITATION: aux_field must be provided and kept valid externally.
    !! Future work will add coupler support to update aux_field automatically.
    function new_NormalizationTransform(aux_field_name, scale_factor, aux_field) result(transform)
       type(NormalizationTransform) :: transform
       character(*), intent(in) :: aux_field_name
       real, intent(in) :: scale_factor
       type(ESMF_Field), intent(in) :: aux_field

       transform%aux_field_name = aux_field_name
       transform%scale_factor = scale_factor
       transform%aux_field = aux_field

    end function new_NormalizationTransform

    subroutine initialize(this, importState, exportState, clock, rc)
       use esmf
       class(NormalizationTransform), intent(inout) :: this
       type(ESMF_State)      :: importState
       type(ESMF_State)      :: exportState
       type(ESMF_Clock)      :: clock      
       integer, optional, intent(out) :: rc

       integer :: status

        ! Validate that auxiliary field is initialized
        ! TODO (future): When coupler support is added, run coupler here
        _ASSERT(ESMF_FieldIsCreated(this%aux_field), "Auxiliary field not initialized")

        _UNUSED_DUMMY(exportState)
        _UNUSED_DUMMY(importState)
        _UNUSED_DUMMY(clock)
        _RETURN(_SUCCESS)
    end subroutine initialize

   subroutine update_field(f_in, f_out, f_aux, scale_factor, rc)
      type(ESMF_Field), intent(inout) :: f_in, f_out, f_aux
      real, intent(in) :: scale_factor
      integer, optional, intent(out) :: rc
      integer :: status
      real(kind=ESMF_KIND_R4), pointer :: x4_in(:), x4_out(:), x4_aux(:)
      real(kind=ESMF_KIND_R8), pointer :: x8_in(:), x8_out(:), x8_aux(:)
      type(ESMF_TypeKind_Flag) :: typekind

      call ESMF_FieldGet(f_in, typekind=typekind, _RC)
      
      if (typekind == ESMF_TYPEKIND_R4) then
         call assign_fptr(f_in, x4_in, _RC)
         call assign_fptr(f_out, x4_out, _RC)
         call assign_fptr(f_aux, x4_aux, _RC)
         ! Normalize: out = in × (aux × scale)
         x4_out = x4_in * x4_aux * scale_factor
         _RETURN(_SUCCESS)
      end if

      if (typekind == ESMF_TYPEKIND_R8) then
         call assign_fptr(f_in, x8_in, _RC)
         call assign_fptr(f_out, x8_out, _RC)
         call assign_fptr(f_aux, x8_aux, _RC)
         ! Normalize: out = in × (aux × scale)
         x8_out = x8_in * x8_aux * scale_factor
         _RETURN(_SUCCESS)
      end if

      _FAIL('unsupported typekind')

   end subroutine update_field
      
   subroutine update_field_bundle(fb_in, fb_out, f_aux, scale_factor, rc)
      type(ESMF_FieldBundle), intent(inout) :: fb_in, fb_out
      type(ESMF_Field), intent(inout) :: f_aux
      real, intent(in) :: scale_factor
      integer, optional, intent(out) :: rc
      integer :: status
      integer :: i, fieldCount
      type(ESMF_Field), allocatable :: flist_in(:), flist_out(:)

      call ESMF_FieldBundleGet(fb_out, fieldCount=fieldCount, _RC)
      call ESMF_FieldBundleGet(fb_in, fieldCount=i, _RC)
      _ASSERT(i==fieldCount, 'The number of ESMF_Field''s in the ESMF_Bundles'' do not match.')
      allocate(flist_in(fieldCount))
      allocate(flist_out(fieldCount))
      call ESMF_FieldBundleGet(fb_in, fieldList=flist_in, _RC)
      call ESMF_FieldBundleGet(fb_out, fieldList=flist_out, _RC)
      _ASSERT(size(flist_in) == size(flist_out), 'The FieldBundles have different sizes.')
      do i=1, size(flist_in)
         call update_field(flist_in(i), flist_out(i), f_aux, scale_factor, _RC)
      end do
      _RETURN(_SUCCESS)

   end subroutine update_field_bundle

    subroutine update(this, importState, exportState, clock, rc)
       use esmf
       class(NormalizationTransform), intent(inout) :: this
       type(ESMF_State)      :: importState
       type(ESMF_State)      :: exportState
       type(ESMF_Clock)      :: clock      
       integer, optional, intent(out) :: rc

       integer :: status
       type(ESMF_Field) :: f_in, f_out
       type(ESMF_FieldBundle) :: fb_in, fb_out
       type(ESMF_StateItem_Flag) :: itemtype_in, itemtype_out

       ! TODO (future): When coupler support is added, run coupler to update aux field
       ! if (associated(this%aux_coupler)) then
       !    call this%aux_coupler%run(phase_idx=GENERIC_COUPLER_UPDATE, _RC)
       ! end if

       ! Get the field or bundle to normalize
       call ESMF_StateGet(importState, itemName=COUPLER_IMPORT_NAME, itemtype=itemtype_in, _RC)
       call ESMF_StateGet(exportState, itemName=COUPLER_EXPORT_NAME, itemtype=itemtype_out, _RC)
       _ASSERT(itemtype_in == itemtype_out, "Mismatched item types.")

       if(itemtype_in == MAPL_STATEITEM_FIELD) then
          call ESMF_StateGet(importState, itemName=COUPLER_IMPORT_NAME, field=f_in, _RC)
          call ESMF_StateGet(exportState, itemName=COUPLER_EXPORT_NAME, field=f_out, _RC)
          call update_field(f_in, f_out, this%aux_field, this%scale_factor, _RC)
       elseif(itemType_in == MAPL_STATEITEM_FIELDBUNDLE) then
          call ESMF_StateGet(importState, itemName=COUPLER_IMPORT_NAME, fieldBundle=fb_in, _RC)
          call ESMF_StateGet(exportState, itemName=COUPLER_EXPORT_NAME, fieldBundle=fb_out, _RC)
          call bundle_types_valid(fb_in, fb_out, _RC)
          call update_field_bundle(fb_in, fb_out, this%aux_field, this%scale_factor, _RC)
        else
           _FAIL("Unsupported state item type")
        end if

        _UNUSED_DUMMY(clock)
        _RETURN(_SUCCESS)

     end subroutine update

    function get_transformId(this) result(id)
       type(TransformId) :: id
       class(NormalizationTransform), intent(in) :: this

       _UNUSED_DUMMY(this)
       id = NORMALIZATION_TRANSFORM_ID

    end function get_transformId

end module mapl3g_NormalizationTransform
