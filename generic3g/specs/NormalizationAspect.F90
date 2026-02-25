#include "MAPL.h"

module mapl3g_NormalizationAspect

   use mapl3g_ActualConnectionPt
   use mapl3g_AspectId
   use mapl3g_StateItemAspect
   use mapl3g_ExtensionTransform
   use mapl3g_NullTransform
   use mapl3g_QuantityTypeAspect
   use mapl3g_NormalizationType
   use mapl3g_NormalizationMetadata
   use mapl3g_Field_API
   use mapl3g_FieldBundle_API
   use mapl_KeywordEnforcer
   use mapl_ErrorHandling
   use esmf

   implicit none
   private

   public :: NormalizationAspect
   public :: to_NormalizationAspect

   interface to_NormalizationAspect
      procedure :: to_normalization_from_poly
      procedure :: to_normalization_from_map
   end interface to_NormalizationAspect

   type, extends(StateItemAspect) :: NormalizationAspect
      private
      
      ! Normalization parameters
      character(:), allocatable :: aux_field_name     ! "DELP" or "DZ"
      real :: scale_factor = 1.0                      ! e.g., 1/g for delp
      character(:), allocatable :: source_units       ! e.g., "kg/kg" or "kg/m2"
      character(:), allocatable :: target_units       ! e.g., "kg/m2" or "kg/kg"
      
      ! Mode flag: false = normalize, true = denormalize
      logical :: is_inverse = .false.
      
      ! Aspect ID (set based on is_inverse in constructor)
      type(AspectId) :: aspect_id_value = NORMALIZATION_ASPECT_ID
      
   contains
      ! StateItemAspect interface
      procedure :: matches
      procedure :: make_transform
      procedure :: connect_to_export
       procedure :: supports_conversion_general
       procedure :: supports_conversion_specific
       procedure, nopass :: get_aspect_id

      ! Getters/setters
      procedure :: get_aux_field_name
      procedure :: set_aux_field_name
      procedure :: get_scale_factor
      procedure :: set_scale_factor
      procedure :: get_source_units
      procedure :: set_source_units
       procedure :: get_target_units
       procedure :: set_target_units
       
       ! is_inverse getter/setter
       procedure :: get_is_inverse
       procedure :: set_is_inverse
       
       ! Instance aspect_id getter (returns the actual ID for this instance)
       procedure :: get_instance_aspect_id

       procedure :: update_from_payload
      procedure :: update_payload
      procedure :: print_aspect
   end type NormalizationAspect

   interface NormalizationAspect
      procedure new_NormalizationAspect
   end interface

contains

   function new_NormalizationAspect(aux_field_name, scale_factor, source_units, target_units, is_inverse, is_time_dependent) result(aspect)
       type(NormalizationAspect) :: aspect
       character(*), optional, intent(in) :: aux_field_name
       real, optional, intent(in) :: scale_factor
       character(*), optional, intent(in) :: source_units
       character(*), optional, intent(in) :: target_units
       logical, optional, intent(in) :: is_inverse
       logical, optional, intent(in) :: is_time_dependent

       call aspect%set_mirror(.true.)
       
       if (present(aux_field_name)) then
          aspect%aux_field_name = aux_field_name
          call aspect%set_mirror(.false.)
       end if
       
       if (present(scale_factor)) then
          aspect%scale_factor = scale_factor
       end if
       
       if (present(source_units)) then
          aspect%source_units = source_units
       end if
       
       if (present(target_units)) then
          aspect%target_units = target_units
       end if
       
       if (present(is_inverse)) then
          aspect%is_inverse = is_inverse
       end if
       
       ! Set aspect_id based on is_inverse flag
       if (aspect%is_inverse) then
          aspect%aspect_id_value = INVERSE_NORMALIZATION_ASPECT_ID
       else
          aspect%aspect_id_value = NORMALIZATION_ASPECT_ID
       end if

       call aspect%set_time_dependent(is_time_dependent)

    end function new_NormalizationAspect

   logical function supports_conversion_general(src)
      class(NormalizationAspect), intent(in) :: src

      ! NormalizationAspect supports conversion (normalization is a transformation)
      supports_conversion_general = .true.

      _UNUSED_DUMMY(src)
   end function supports_conversion_general

   logical function supports_conversion_specific(src, dst)
      class(NormalizationAspect), intent(in) :: src
      class(StateItemAspect), intent(in) :: dst

      select type (dst)
      class is (NormalizationAspect)
         ! For now, only support exact match
         if (allocated(src%aux_field_name) .and. allocated(dst%aux_field_name)) then
            supports_conversion_specific = (src%aux_field_name == dst%aux_field_name) .and. &
                                          (abs(src%scale_factor - dst%scale_factor) < 1e-10)
         else
            supports_conversion_specific = .false.
         end if
      class default
         supports_conversion_specific = .false.
      end select

   end function supports_conversion_specific

   logical function matches(src, dst)
      class(NormalizationAspect), intent(in) :: src
      class(StateItemAspect), intent(in) :: dst

      select type(dst)
      class is (NormalizationAspect)
         ! Match if normalization parameters match or if either is a mirror
         if (src%is_mirror() .or. dst%is_mirror()) then
            matches = .true.
         else if (allocated(src%aux_field_name) .and. allocated(dst%aux_field_name)) then
            matches = (src%aux_field_name == dst%aux_field_name) .and. &
                     (abs(src%scale_factor - dst%scale_factor) < 1e-10)
         else
            matches = .false.
         end if
      class default
         matches = .false.
      end select

   end function matches

   function make_transform(src, dst, other_aspects, rc) result(transform)
      class(ExtensionTransform), allocatable :: transform
      class(NormalizationAspect), intent(in) :: src
      class(StateItemAspect), intent(in)  :: dst
      type(AspectMap), target, intent(in)  :: other_aspects
      integer, optional, intent(out) :: rc

      integer :: status

      ! For Phase 2, Task 2.1, we return NullTransform
      ! Task 2.2 will create the actual NormalizationTransform
      ! For now, normalization is handled by other mechanisms
      allocate(transform, source=NullTransform())

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(src)
      _UNUSED_DUMMY(dst)
      _UNUSED_DUMMY(other_aspects)
   end function make_transform

   subroutine connect_to_export(this, export, actual_pt, rc)
      class(NormalizationAspect), intent(inout) :: this
      class(StateItemAspect), intent(in) :: export
      type(ActualConnectionPt), intent(in) :: actual_pt
      integer, optional, intent(out) :: rc

      type(NormalizationAspect) :: export_
      integer :: status

      export_ = to_NormalizationAspect(export, _RC)
      
      if (allocated(export_%aux_field_name)) this%aux_field_name = export_%aux_field_name
      this%scale_factor = export_%scale_factor
      if (allocated(export_%source_units)) this%source_units = export_%source_units
      if (allocated(export_%target_units)) this%target_units = export_%target_units

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(actual_pt)
   end subroutine connect_to_export

   function to_normalization_from_poly(aspect, rc) result(normalization_aspect)
      type(NormalizationAspect) :: normalization_aspect
      class(StateItemAspect), intent(in) :: aspect
      integer, optional, intent(out) :: rc

      select type(aspect)
      class is (NormalizationAspect)
         normalization_aspect = aspect
      class default
         _FAIL('aspect is not NormalizationAspect')
      end select

      _RETURN(_SUCCESS)
   end function to_normalization_from_poly

   function to_normalization_from_map(map, aspect_id, rc) result(normalization_aspect)
      type(NormalizationAspect) :: normalization_aspect
      type(AspectMap), target, intent(in) :: map
      type(AspectId), optional, intent(in) :: aspect_id
      integer, optional, intent(out) :: rc

      integer :: status
      class(StateItemAspect), pointer :: poly
      type(AspectId) :: id_to_use

      ! Use provided aspect_id or default to NORMALIZATION_ASPECT_ID
      if (present(aspect_id)) then
         id_to_use = aspect_id
      else
         id_to_use = NORMALIZATION_ASPECT_ID
      end if

      poly => map%at(id_to_use, _RC)
      normalization_aspect = to_NormalizationAspect(poly, _RC)

      _RETURN(_SUCCESS)
   end function to_normalization_from_map

   function get_aspect_id() result(aspect_id)
      type(AspectId) :: aspect_id
      ! Return the default aspect ID for NormalizationAspect class
      ! For instance-specific ID (which may be INVERSE_NORMALIZATION_ASPECT_ID),
      ! use get_instance_aspect_id() instead
      aspect_id = NORMALIZATION_ASPECT_ID
   end function get_aspect_id
   
   function get_instance_aspect_id(this, rc) result(aspect_id)
      type(AspectId) :: aspect_id
      class(NormalizationAspect), intent(in) :: this
      integer, optional, intent(out) :: rc
      
      ! Return the actual aspect ID for this instance
      aspect_id = this%aspect_id_value
      
      _RETURN(_SUCCESS)
   end function get_instance_aspect_id

   ! Getters/Setters
   
   function get_aux_field_name(this, rc) result(aux_field_name)
      character(:), allocatable :: aux_field_name
      class(NormalizationAspect), intent(in) :: this
      integer, optional, intent(out) :: rc

      if (allocated(this%aux_field_name)) then
         aux_field_name = this%aux_field_name
      else
         aux_field_name = ''
      end if

      _RETURN(_SUCCESS)
   end function get_aux_field_name

   subroutine set_aux_field_name(this, aux_field_name, rc)
      class(NormalizationAspect), intent(inout) :: this
      character(*), intent(in) :: aux_field_name
      integer, optional, intent(out) :: rc

      this%aux_field_name = aux_field_name
      call this%set_mirror(.false.)

      _RETURN(_SUCCESS)
   end subroutine set_aux_field_name

   function get_scale_factor(this, rc) result(scale_factor)
      real :: scale_factor
      class(NormalizationAspect), intent(in) :: this
      integer, optional, intent(out) :: rc

      scale_factor = this%scale_factor

      _RETURN(_SUCCESS)
   end function get_scale_factor

   subroutine set_scale_factor(this, scale_factor, rc)
      class(NormalizationAspect), intent(inout) :: this
      real, intent(in) :: scale_factor
      integer, optional, intent(out) :: rc

      this%scale_factor = scale_factor

      _RETURN(_SUCCESS)
   end subroutine set_scale_factor

   function get_source_units(this, rc) result(source_units)
      character(:), allocatable :: source_units
      class(NormalizationAspect), intent(in) :: this
      integer, optional, intent(out) :: rc

      if (allocated(this%source_units)) then
         source_units = this%source_units
      else
         source_units = ''
      end if

      _RETURN(_SUCCESS)
   end function get_source_units

   subroutine set_source_units(this, source_units, rc)
      class(NormalizationAspect), intent(inout) :: this
      character(*), intent(in) :: source_units
      integer, optional, intent(out) :: rc

      this%source_units = source_units

      _RETURN(_SUCCESS)
   end subroutine set_source_units

   function get_target_units(this, rc) result(target_units)
      character(:), allocatable :: target_units
      class(NormalizationAspect), intent(in) :: this
      integer, optional, intent(out) :: rc

      if (allocated(this%target_units)) then
         target_units = this%target_units
      else
         target_units = ''
      end if

      _RETURN(_SUCCESS)
   end function get_target_units

   subroutine set_target_units(this, target_units, rc)
      class(NormalizationAspect), intent(inout) :: this
      character(*), intent(in) :: target_units
      integer, optional, intent(out) :: rc

      this%target_units = target_units

      _RETURN(_SUCCESS)
   end subroutine set_target_units

   function get_is_inverse(this, rc) result(is_inverse)
      logical :: is_inverse
      class(NormalizationAspect), intent(in) :: this
      integer, optional, intent(out) :: rc

      is_inverse = this%is_inverse

      _RETURN(_SUCCESS)
   end function get_is_inverse

   subroutine set_is_inverse(this, is_inverse, rc)
      class(NormalizationAspect), intent(inout) :: this
      logical, intent(in) :: is_inverse
      integer, optional, intent(out) :: rc

      this%is_inverse = is_inverse

      _RETURN(_SUCCESS)
   end subroutine set_is_inverse

   subroutine update_from_payload(this, field, bundle, state, rc)
      class(NormalizationAspect), intent(inout) :: this
      type(esmf_Field), optional, intent(in) :: field
      type(esmf_FieldBundle), optional, intent(in) :: bundle
      type(esmf_State), optional, intent(in) :: state
      integer, optional, intent(out) :: rc

      integer :: status
      type(NormalizationMetadata) :: norm_metadata
      type(NormalizationType) :: norm_type
      character(:), allocatable :: units

      _RETURN_UNLESS(present(field) .or. present(bundle))

      ! Get NormalizationMetadata from field/bundle
      if (present(field)) then
         call MAPL_FieldGet(field, normalization_metadata=norm_metadata, _RC)
      else if (present(bundle)) then
         call MAPL_FieldBundleGet(bundle, normalization_metadata=norm_metadata, _RC)
      end if

      ! Check if metadata indicates normalization is needed
      if (norm_metadata%is_mirror()) then
         call this%set_mirror(.true.)
         _RETURN(_SUCCESS)
      end if

      ! Get normalization parameters from metadata
      norm_type = norm_metadata%get_normalization_type()

      if (norm_type /= NORMALIZE_NONE) then
         ! Field needs normalization/denormalization - extract parameters
         this%aux_field_name = norm_metadata%get_aux_field_name()
         this%scale_factor = norm_metadata%get_normalization_scale()
         
         ! Get source units
         if (present(field)) then
            call MAPL_FieldGet(field, units=units, _RC)
         else if (present(bundle)) then
            call MAPL_FieldBundleGet(bundle, units=units, _RC)
         end if
         this%source_units = units
         
         ! Compute target units (depends on is_inverse flag)
         if (this%is_inverse) then
            ! Denormalization: compute original units from normalized units
            call compute_denormalized_units(this%source_units, this%aux_field_name, &
                                          this%scale_factor, this%target_units, _RC)
         else
            ! Normalization: compute normalized units from original units
            call compute_normalized_units(this%source_units, this%aux_field_name, &
                                        this%scale_factor, this%target_units, _RC)
         end if
         
         call this%set_mirror(.false.)
      else
         ! Field doesn't need normalization/denormalization
         call this%set_mirror(.true.)
      end if

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(state)
   end subroutine update_from_payload

   subroutine update_payload(this, field, bundle, state, rc)
      class(NormalizationAspect), intent(in) :: this
      type(esmf_Field), optional, intent(inout) :: field
      type(esmf_FieldBundle), optional, intent(inout) :: bundle
      type(esmf_State), optional, intent(inout) :: state
      integer, optional, intent(out) :: rc

      integer :: status

      _RETURN_UNLESS(present(field) .or. present(bundle))
      _RETURN_IF(this%is_mirror())

      ! For now, NormalizationAspect doesn't directly modify the payload
      ! The normalization/denormalization will be handled by transforms:
      !   - NormalizationTransform (Task 2.2) for normalization
      !   - InverseNormalizationTransform (Task 2.4) for denormalization
      ! This aspect serves as metadata storage only in Phase 2

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(state)
      _UNUSED_DUMMY(field)
      _UNUSED_DUMMY(bundle)
   end subroutine update_payload

   subroutine print_aspect(this, file, line, rc)
      class(NormalizationAspect), intent(in) :: this
      character(*), intent(in) :: file
      integer, intent(in) :: line
      integer, optional, intent(out) :: rc

      _HERE, file, line, this%is_mirror()
      _HERE, file, line, 'is_inverse:', this%is_inverse
      if (allocated(this%aux_field_name)) then
         _HERE, file, line, 'aux_field_name:', this%aux_field_name
      end if
      _HERE, file, line, 'scale_factor:', this%scale_factor
      if (allocated(this%source_units)) then
         _HERE, file, line, 'source_units:', this%source_units
      end if
      if (allocated(this%target_units)) then
         _HERE, file, line, 'target_units:', this%target_units
      end if

      _RETURN(_SUCCESS)
   end subroutine print_aspect

   ! Helper subroutine to compute normalized units
   subroutine compute_normalized_units(source_units, aux_field, scale, target_units, rc)
      character(*), intent(in) :: source_units
      character(*), intent(in) :: aux_field
      real, intent(in) :: scale
      character(:), allocatable, intent(out) :: target_units
      integer, optional, intent(out) :: rc

      integer :: status

      ! For now, simple unit computation
      ! In full implementation, this would use UDUNITS
      ! source_units × (aux_field_units × scale) = target_units
      
      select case (trim(aux_field))
      case ('DELP')
         ! DELP is in Pa, with scale 1/g, result is kg/m^2
         ! e.g., "kg/kg" × ("Pa" × 1/g) = "kg/m2"
         if (trim(source_units) == 'kg/kg') then
            target_units = 'kg/m2'
         else
            target_units = source_units  ! Fallback
         end if
      case ('DZ')
         ! DZ is in m, with scale 1.0
         ! e.g., "kg/m3" × "m" = "kg/m2"
         if (trim(source_units) == 'kg/m3') then
            target_units = 'kg/m2'
         else
            target_units = source_units  ! Fallback
         end if
      case default
         target_units = source_units  ! Fallback - no conversion
      end select

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(scale)
   end subroutine compute_normalized_units

   ! Helper subroutine to compute denormalized (original) units
   subroutine compute_denormalized_units(source_units, aux_field, scale, target_units, rc)
      character(*), intent(in) :: source_units
      character(*), intent(in) :: aux_field
      real, intent(in) :: scale
      character(:), allocatable, intent(out) :: target_units
      integer, optional, intent(out) :: rc

      integer :: status

      ! For now, simple unit computation (inverse of normalization)
      ! In full implementation, this would use UDUNITS
      ! source_units ÷ (aux_field_units × scale) = target_units
      
      select case (trim(aux_field))
      case ('DELP')
         ! DELP is in Pa, with scale 1/g, inverse converts back
         ! e.g., "kg/m2" ÷ ("Pa" × 1/g) = "kg/kg"
         if (trim(source_units) == 'kg/m2') then
            target_units = 'kg/kg'
         else
            target_units = source_units  ! Fallback
         end if
      case ('DZ')
         ! DZ is in m, with scale 1.0, inverse converts back
         ! e.g., "kg/m2" ÷ "m" = "kg/m3"
         if (trim(source_units) == 'kg/m2') then
            target_units = 'kg/m3'
         else
            target_units = source_units  ! Fallback
         end if
      case default
         target_units = source_units  ! Fallback - no conversion
      end select

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(scale)
   end subroutine compute_denormalized_units

end module mapl3g_NormalizationAspect
