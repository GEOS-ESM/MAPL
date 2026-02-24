#include "MAPL.h"

module mapl3g_QuantityTypeAspect

   use mapl3g_ActualConnectionPt
   use mapl3g_AspectId
   use mapl3g_StateItemAspect
   use mapl3g_ExtensionTransform
   use mapl3g_NullTransform
   use mapl3g_QuantityType
   use mapl3g_QuantityTypeMetadata
   use mapl3g_NormalizationType
   use mapl3g_NormalizationMetadata
   use mapl3g_Field_API
   use mapl3g_FieldBundle_API
   use mapl_KeywordEnforcer
   use mapl_ErrorHandling
   use esmf

   implicit none
   private

   public :: QuantityTypeAspect
   public :: to_QuantityTypeAspect

   interface to_QuantityTypeAspect
      procedure :: to_quantity_type_from_poly
      procedure :: to_quantity_type_from_map
   end interface to_QuantityTypeAspect

   type, extends(StateItemAspect) :: QuantityTypeAspect
      private
      
      ! Semantic properties (user-specified)
      type(QuantityType) :: quantity_type = QUANTITY_UNKNOWN
      character(:), allocatable :: dimensions          ! e.g., "kg/kg", "kg/m3"
      type(MixingRatioBasis) :: basis = BASIS_NONE
      real, allocatable :: molecular_weight            ! For volume mixing ratios
      
   contains
      procedure :: validate
      
      ! StateItemAspect interface
      procedure :: matches
      procedure :: make_transform
      procedure :: connect_to_export
      procedure :: supports_conversion_general
      procedure :: supports_conversion_specific
      procedure, nopass :: get_aspect_id

      ! Getters/setters
      procedure :: get_quantity_type
      procedure :: set_quantity_type
      procedure :: get_dimensions
      procedure :: set_dimensions
      procedure :: get_basis
      procedure :: set_basis
      procedure :: get_molecular_weight
      procedure :: set_molecular_weight
      procedure :: set_from_metadata

      procedure :: update_from_payload
      procedure :: update_payload
      procedure :: print_aspect
   end type QuantityTypeAspect

   interface QuantityTypeAspect
      procedure new_QuantityTypeAspect
   end interface

   ! Physical constants
   real, parameter :: GRAV = 9.80665  ! m/s^2, standard gravity

contains

   function new_QuantityTypeAspect(quantity_type, dimensions, basis, molecular_weight, is_time_dependent) result(aspect)
       type(QuantityTypeAspect) :: aspect
       type(QuantityType), optional, intent(in) :: quantity_type
       character(*), optional, intent(in) :: dimensions
       type(MixingRatioBasis), optional, intent(in) :: basis
       real, optional, intent(in) :: molecular_weight
       logical, optional, intent(in) :: is_time_dependent

       integer :: rc
       integer :: status

       call aspect%set_mirror(.true.)
       
       if (present(quantity_type)) then
          aspect%quantity_type = quantity_type
          call aspect%set_mirror(.false.)
       end if
       
       if (present(dimensions)) then
          aspect%dimensions = dimensions
       end if
       
       if (present(basis)) then
          aspect%basis = basis
       end if
       
       if (present(molecular_weight)) then
          allocate(aspect%molecular_weight, source=molecular_weight)
       end if

       call aspect%set_time_dependent(is_time_dependent)

    end function new_QuantityTypeAspect

   subroutine validate(this, rc)
      class(QuantityTypeAspect), intent(in) :: this
      integer, optional, intent(out) :: rc

      integer :: status

      ! Validate quantity type
      _ASSERT(this%quantity_type%is_valid(), 'Invalid quantity type')

      ! Validate basis consistency
      if (this%quantity_type == QUANTITY_MIXING_RATIO) then
         _ASSERT(this%basis /= BASIS_NONE, 'Mixing ratio requires basis specification')
         if (this%basis == BASIS_VOLUME) then
            _ASSERT(allocated(this%molecular_weight), 'Volume mixing ratio requires molecular weight')
            _ASSERT(this%molecular_weight > 0.0, 'Molecular weight must be positive')
         end if
      else
         if (this%basis /= BASIS_NONE) then
            _FAIL('Only mixing ratios can have basis specification')
         end if
      end if

      _RETURN(_SUCCESS)
   end subroutine validate

   logical function supports_conversion_general(src)
      class(QuantityTypeAspect), intent(in) :: src

      ! QuantityType is metadata-only, always supports conversion (mirroring)
      supports_conversion_general = .true.

      _UNUSED_DUMMY(src)
   end function supports_conversion_general

   logical function supports_conversion_specific(src, dst)
      class(QuantityTypeAspect), intent(in) :: src
      class(StateItemAspect), intent(in) :: dst

      select type (dst)
      class is (QuantityTypeAspect)
         ! For now, only support exact match
         ! Future: could support conversion between mixing ratio types, etc.
         supports_conversion_specific = (src%quantity_type == dst%quantity_type) .and. &
                                       (src%basis == dst%basis)
      class default
         supports_conversion_specific = .false.
      end select

   end function supports_conversion_specific

   logical function matches(src, dst)
      class(QuantityTypeAspect), intent(in) :: src
      class(StateItemAspect), intent(in) :: dst

      select type(dst)
      class is (QuantityTypeAspect)
         ! Match if quantity types and basis match, or if either is unknown/none
         matches = (src%quantity_type == dst%quantity_type .or. &
                   src%quantity_type == QUANTITY_UNKNOWN .or. &
                   dst%quantity_type == QUANTITY_UNKNOWN) .and. &
                  (src%basis == dst%basis .or. &
                   src%basis == BASIS_NONE .or. &
                   dst%basis == BASIS_NONE)
      class default
         matches = .false.
      end select

   end function matches

   function make_transform(src, dst, other_aspects, rc) result(transform)
      class(ExtensionTransform), allocatable :: transform
      class(QuantityTypeAspect), intent(in) :: src
      class(StateItemAspect), intent(in)  :: dst
      type(AspectMap), target, intent(in)  :: other_aspects
      integer, optional, intent(out) :: rc

      ! QuantityTypeAspect is metadata-only, returns NullTransform
      ! The actual normalization/denormalization is handled by separate aspects
      allocate(transform, source=NullTransform())

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(src)
      _UNUSED_DUMMY(dst)
      _UNUSED_DUMMY(other_aspects)
   end function make_transform

   subroutine connect_to_export(this, export, actual_pt, rc)
      class(QuantityTypeAspect), intent(inout) :: this
      class(StateItemAspect), intent(in) :: export
      type(ActualConnectionPt), intent(in) :: actual_pt
      integer, optional, intent(out) :: rc

      type(QuantityTypeAspect) :: export_
      integer :: status

      export_ = to_QuantityTypeAspect(export, _RC)
      this%quantity_type = export_%quantity_type
      if (allocated(export_%dimensions)) this%dimensions = export_%dimensions
      this%basis = export_%basis
      if (allocated(export_%molecular_weight)) then
         if (.not. allocated(this%molecular_weight)) allocate(this%molecular_weight)
         this%molecular_weight = export_%molecular_weight
      end if

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(actual_pt)
   end subroutine connect_to_export

   function to_quantity_type_from_poly(aspect, rc) result(quantity_type_aspect)
      type(QuantityTypeAspect) :: quantity_type_aspect
      class(StateItemAspect), intent(in) :: aspect
      integer, optional, intent(out) :: rc

      select type(aspect)
      class is (QuantityTypeAspect)
         quantity_type_aspect = aspect
      class default
         _FAIL('aspect is not QuantityTypeAspect')
      end select

      _RETURN(_SUCCESS)
   end function to_quantity_type_from_poly

   function to_quantity_type_from_map(map, rc) result(quantity_type_aspect)
      type(QuantityTypeAspect) :: quantity_type_aspect
      type(AspectMap), target, intent(in) :: map
      integer, optional, intent(out) :: rc

      integer :: status
      class(StateItemAspect), pointer :: poly

      poly => map%at(QUANTITY_TYPE_ASPECT_ID, _RC)
      quantity_type_aspect = to_QuantityTypeAspect(poly, _RC)

      _RETURN(_SUCCESS)
   end function to_quantity_type_from_map

   function get_aspect_id() result(aspect_id)
      type(AspectId) :: aspect_id
      aspect_id = QUANTITY_TYPE_ASPECT_ID
   end function get_aspect_id

   ! Getters/Setters
   
   function get_quantity_type(this, rc) result(quantity_type)
      type(QuantityType) :: quantity_type
      class(QuantityTypeAspect), intent(in) :: this
      integer, optional, intent(out) :: rc

      quantity_type = this%quantity_type

      _RETURN(_SUCCESS)
   end function get_quantity_type

   subroutine set_quantity_type(this, quantity_type, rc)
      class(QuantityTypeAspect), intent(inout) :: this
      type(QuantityType), intent(in) :: quantity_type
      integer, optional, intent(out) :: rc

      integer :: status

      this%quantity_type = quantity_type
      call this%set_mirror(.false.)

      _RETURN(_SUCCESS)
   end subroutine set_quantity_type

   function get_dimensions(this, rc) result(dimensions)
      character(:), allocatable :: dimensions
      class(QuantityTypeAspect), intent(in) :: this
      integer, optional, intent(out) :: rc

      if (allocated(this%dimensions)) then
         dimensions = this%dimensions
      else
         dimensions = ''
      end if

      _RETURN(_SUCCESS)
   end function get_dimensions

   subroutine set_dimensions(this, dimensions, rc)
      class(QuantityTypeAspect), intent(inout) :: this
      character(*), intent(in) :: dimensions
      integer, optional, intent(out) :: rc

      this%dimensions = dimensions

      _RETURN(_SUCCESS)
   end subroutine set_dimensions

   function get_basis(this, rc) result(basis)
      type(MixingRatioBasis) :: basis
      class(QuantityTypeAspect), intent(in) :: this
      integer, optional, intent(out) :: rc

      basis = this%basis

      _RETURN(_SUCCESS)
   end function get_basis

   subroutine set_basis(this, basis, rc)
      class(QuantityTypeAspect), intent(inout) :: this
      type(MixingRatioBasis), intent(in) :: basis
      integer, optional, intent(out) :: rc

      integer :: status

      this%basis = basis

      _RETURN(_SUCCESS)
   end subroutine set_basis

   function get_molecular_weight(this, rc) result(molecular_weight)
      real :: molecular_weight
      class(QuantityTypeAspect), intent(in) :: this
      integer, optional, intent(out) :: rc

      _ASSERT(allocated(this%molecular_weight), 'molecular_weight not set')
      molecular_weight = this%molecular_weight

      _RETURN(_SUCCESS)
   end function get_molecular_weight

   subroutine set_molecular_weight(this, molecular_weight, rc)
      class(QuantityTypeAspect), intent(inout) :: this
      real, intent(in) :: molecular_weight
      integer, optional, intent(out) :: rc

      if (.not. allocated(this%molecular_weight)) allocate(this%molecular_weight)
      this%molecular_weight = molecular_weight

      _RETURN(_SUCCESS)
   end subroutine set_molecular_weight

   subroutine set_from_metadata(this, metadata, rc)
      class(QuantityTypeAspect), intent(inout) :: this
      type(QuantityTypeMetadata), intent(in) :: metadata
      integer, optional, intent(out) :: rc

      integer :: status

      ! Extract and set all properties from metadata
      this%quantity_type = metadata%get_quantity_type()
      this%dimensions = metadata%get_dimensions()
      this%basis = metadata%get_basis()
      if (metadata%has_molecular_weight()) then
         if (.not. allocated(this%molecular_weight)) allocate(this%molecular_weight)
         this%molecular_weight = metadata%get_molecular_weight()
      end if
      
      call this%set_mirror(.false.)

      _RETURN(_SUCCESS)
   end subroutine set_from_metadata

   subroutine update_from_payload(this, field, bundle, state, rc)
      class(QuantityTypeAspect), intent(inout) :: this
      type(esmf_Field), optional, intent(in) :: field
      type(esmf_FieldBundle), optional, intent(in) :: bundle
      type(esmf_State), optional, intent(in) :: state
      integer, optional, intent(out) :: rc

      integer :: status
      type(QuantityTypeMetadata) :: metadata
      logical :: has_metadata

      _RETURN_UNLESS(present(field) .or. present(bundle))

      has_metadata = .false.

      if (present(field)) then
         call MAPL_FieldGet(field, quantity_type_metadata=metadata, _RC)
         has_metadata = .not. metadata%is_mirror()
      else if (present(bundle)) then
         call MAPL_FieldBundleGet(bundle, quantity_type_metadata=metadata, _RC)
         has_metadata = .not. metadata%is_mirror()
      end if

      if (has_metadata) then
         call this%set_from_metadata(metadata, _RC)
      else
         call this%set_mirror(.true.)
      end if

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(state)
   end subroutine update_from_payload

   subroutine update_payload(this, field, bundle, state, rc)
      class(QuantityTypeAspect), intent(in) :: this
      type(esmf_Field), optional, intent(inout) :: field
      type(esmf_FieldBundle), optional, intent(inout) :: bundle
      type(esmf_State), optional, intent(inout) :: state
      integer, optional, intent(out) :: rc

      integer :: status
      type(QuantityTypeMetadata) :: qty_metadata
      type(NormalizationMetadata) :: norm_metadata

      _RETURN_UNLESS(present(field) .or. present(bundle))
      _RETURN_IF(this%is_mirror())

      ! Create QuantityTypeMetadata from aspect properties
      qty_metadata = QuantityTypeMetadata( &
         quantity_type = this%quantity_type, &
         dimensions = this%dimensions, &
         basis = this%basis, &
         molecular_weight = this%molecular_weight &
      )

      ! Derive NormalizationMetadata from quantity_type
      norm_metadata = derive_normalization_metadata(this%quantity_type)

      if (present(field)) then
         call MAPL_FieldSet(field, quantity_type_metadata=qty_metadata, _RC)
         call MAPL_FieldSet(field, normalization_metadata=norm_metadata, _RC)
      else if (present(bundle)) then
         call MAPL_FieldBundleSet(bundle, quantity_type_metadata=qty_metadata, _RC)
         call MAPL_FieldBundleSet(bundle, normalization_metadata=norm_metadata, _RC)
      end if

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(state)
   end subroutine update_payload

   function derive_normalization_metadata(quantity_type) result(metadata)
      type(QuantityType), intent(in) :: quantity_type
      type(NormalizationMetadata) :: metadata

      ! Derive normalization parameters from quantity_type
      select case (quantity_type%to_string())
      case ('QUANTITY_MIXING_RATIO')
         metadata = NormalizationMetadata( &
            conservative_regridable = .true., &
            normalization_type = NORMALIZE_DELP, &
            normalization_scale = 1.0 / GRAV, &
            aux_field_name = 'DELP' &
         )
         
      case ('QUANTITY_CONCENTRATION')
         metadata = NormalizationMetadata( &
            conservative_regridable = .true., &
            normalization_type = NORMALIZE_DZ, &
            normalization_scale = 1.0, &
            aux_field_name = 'DZ' &
         )
         
      case ('QUANTITY_DRY_MIXING_RATIO')
         metadata = NormalizationMetadata( &
            conservative_regridable = .true., &
            normalization_type = NORMALIZE_DELP, &
            normalization_scale = 1.0 / GRAV, &
            aux_field_name = 'DELP' &
         )
         
      case default
         ! Not a conservative regridable quantity
         metadata = NormalizationMetadata()  ! mirror (no normalization)
      end select

   end function derive_normalization_metadata

   subroutine print_aspect(this, file, line, rc)
      class(QuantityTypeAspect), intent(in) :: this
      character(*), intent(in) :: file
      integer, intent(in) :: line
      integer, optional, intent(out) :: rc

      if (allocated(this%dimensions)) then
      end if
      _HERE, file, line, 'basis:', this%basis%to_string()

      _RETURN(_SUCCESS)
   end subroutine print_aspect

end module mapl3g_QuantityTypeAspect
