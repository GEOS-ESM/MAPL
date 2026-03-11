#include "MAPL.h"

module mapl3g_ConservationAspect

   use mapl3g_ActualConnectionPt
   use mapl3g_AspectId
   use mapl3g_StateItemAspect
   use mapl3g_ExtensionTransform
   use mapl3g_NullTransform
   use mapl3g_ConservationType
   use mapl3g_QuantityType
   use mapl3g_QuantityTypeMetadata
   use mapl3g_Field_API
   use mapl3g_FieldBundle_API
   use mapl_KeywordEnforcer
   use mapl_ErrorHandling
   use esmf

   implicit none
   private

   public :: ConservationAspect
   public :: to_ConservationAspect

   interface to_ConservationAspect
      procedure :: to_conservation_from_poly
      procedure :: to_conservation_from_map
   end interface to_ConservationAspect

   type, extends(StateItemAspect) :: ConservationAspect
      private
      
      ! Conservation properties
      type(ConservationType) :: conservation_type = CONSERVE_NONE
      logical :: is_conservable = .false.
      
   contains
      ! StateItemAspect interface
      procedure :: matches
      procedure :: make_transform
      procedure :: connect_to_export
      procedure :: supports_conversion_general
      procedure :: supports_conversion_specific
      procedure, nopass :: get_aspect_id

      ! Getters/setters
      procedure :: get_conservation_type
      procedure :: set_conservation_type
      procedure :: get_is_conservable
      procedure :: set_is_conservable

      procedure :: update_from_payload
      procedure :: update_payload
      procedure :: print_aspect
   end type ConservationAspect

   interface ConservationAspect
      procedure new_ConservationAspect
   end interface

contains

   function new_ConservationAspect(conservation_type, is_conservable, is_time_dependent) result(aspect)
       type(ConservationAspect) :: aspect
       type(ConservationType), optional, intent(in) :: conservation_type
       logical, optional, intent(in) :: is_conservable
       logical, optional, intent(in) :: is_time_dependent

       call aspect%set_mirror(.true.)
       
       if (present(conservation_type)) then
          aspect%conservation_type = conservation_type
          call aspect%set_mirror(.false.)
       end if
       
       if (present(is_conservable)) then
          aspect%is_conservable = is_conservable
       end if

       call aspect%set_time_dependent(is_time_dependent)

    end function new_ConservationAspect

   logical function supports_conversion_general(src)
      class(ConservationAspect), intent(in) :: src

      ! Conservation aspect supports conversion (mirroring)
      supports_conversion_general = .true.

      _UNUSED_DUMMY(src)
   end function supports_conversion_general

   logical function supports_conversion_specific(src, dst)
      class(ConservationAspect), intent(in) :: src
      class(StateItemAspect), intent(in) :: dst

      select type (dst)
      class is (ConservationAspect)
         ! Only support exact match for now
         ! Future: could support transforms between conservation types
         supports_conversion_specific = (src%conservation_type == dst%conservation_type)
      class default
         supports_conversion_specific = .false.
      end select

   end function supports_conversion_specific

   logical function matches(src, dst)
      class(ConservationAspect), intent(in) :: src
      class(StateItemAspect), intent(in) :: dst

      select type(dst)
      class is (ConservationAspect)
         ! Match if conservation types match, or if either is a mirror
         if (src%is_mirror() .or. dst%is_mirror()) then
            matches = .true.
         else
            ! Must conserve the same quantity to match
            matches = (src%conservation_type == dst%conservation_type)
         end if
      class default
         matches = .false.
      end select

   end function matches

   function make_transform(src, dst, other_aspects, rc) result(transform)
      class(ExtensionTransform), allocatable :: transform
      class(ConservationAspect), intent(in) :: src
      class(StateItemAspect), intent(in)  :: dst
      type(AspectMap), target, intent(in)  :: other_aspects
      integer, optional, intent(out) :: rc

      ! ConservationAspect is metadata-only, returns NullTransform
      ! Actual conservation handling is done via normalization aspects
      allocate(transform, source=NullTransform())

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(src)
      _UNUSED_DUMMY(dst)
      _UNUSED_DUMMY(other_aspects)
   end function make_transform

   subroutine connect_to_export(this, export, actual_pt, rc)
      class(ConservationAspect), intent(inout) :: this
      class(StateItemAspect), intent(in) :: export
      type(ActualConnectionPt), intent(in) :: actual_pt
      integer, optional, intent(out) :: rc

      type(ConservationAspect) :: export_
      integer :: status

      export_ = to_ConservationAspect(export, _RC)
      
      this%conservation_type = export_%conservation_type
      this%is_conservable = export_%is_conservable

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(actual_pt)
   end subroutine connect_to_export

   function to_conservation_from_poly(aspect, rc) result(conservation_aspect)
      type(ConservationAspect) :: conservation_aspect
      class(StateItemAspect), intent(in) :: aspect
      integer, optional, intent(out) :: rc

      select type(aspect)
      class is (ConservationAspect)
         conservation_aspect = aspect
      class default
         _FAIL('aspect is not ConservationAspect')
      end select

      _RETURN(_SUCCESS)
   end function to_conservation_from_poly

   function to_conservation_from_map(map, aspect_id, rc) result(conservation_aspect)
      type(ConservationAspect) :: conservation_aspect
      type(AspectMap), target, intent(in) :: map
      type(AspectId), optional, intent(in) :: aspect_id
      integer, optional, intent(out) :: rc

      integer :: status
      class(StateItemAspect), pointer :: poly
      type(AspectId) :: id_to_use

      ! Use provided aspect_id or default to CONSERVATION_ASPECT_ID
      if (present(aspect_id)) then
         id_to_use = aspect_id
      else
         id_to_use = CONSERVATION_ASPECT_ID
      end if

      poly => map%at(id_to_use, _RC)
      conservation_aspect = to_ConservationAspect(poly, _RC)

      _RETURN(_SUCCESS)
   end function to_conservation_from_map

   function get_aspect_id() result(aspect_id)
      type(AspectId) :: aspect_id
      aspect_id = CONSERVATION_ASPECT_ID
   end function get_aspect_id

   ! Getters/Setters
   
   function get_conservation_type(this, rc) result(conservation_type)
      type(ConservationType) :: conservation_type
      class(ConservationAspect), intent(in) :: this
      integer, optional, intent(out) :: rc

      conservation_type = this%conservation_type

      _RETURN(_SUCCESS)
   end function get_conservation_type

   subroutine set_conservation_type(this, conservation_type, rc)
      class(ConservationAspect), intent(inout) :: this
      type(ConservationType), intent(in) :: conservation_type
      integer, optional, intent(out) :: rc

      this%conservation_type = conservation_type
      call this%set_mirror(.false.)

      _RETURN(_SUCCESS)
   end subroutine set_conservation_type

   function get_is_conservable(this, rc) result(is_conservable)
      logical :: is_conservable
      class(ConservationAspect), intent(in) :: this
      integer, optional, intent(out) :: rc

      is_conservable = this%is_conservable

      _RETURN(_SUCCESS)
   end function get_is_conservable

   subroutine set_is_conservable(this, is_conservable, rc)
      class(ConservationAspect), intent(inout) :: this
      logical, intent(in) :: is_conservable
      integer, optional, intent(out) :: rc

      this%is_conservable = is_conservable

      _RETURN(_SUCCESS)
   end subroutine set_is_conservable

    subroutine update_from_payload(this, field, bundle, state, rc)
       class(ConservationAspect), intent(inout) :: this
       type(esmf_Field), optional, intent(in) :: field
       type(esmf_FieldBundle), optional, intent(in) :: bundle
       type(esmf_State), optional, intent(in) :: state
       integer, optional, intent(out) :: rc

       integer :: status
       type(QuantityTypeMetadata) :: qty_metadata
       type(QuantityType) :: qty_type
       type(ESMF_Info) :: info
       character(:), allocatable :: conservation_str
       logical :: is_present

       _RETURN_UNLESS(present(field) .or. present(bundle))

       ! Get Info object from field or bundle
       if (present(field)) then
          call ESMF_InfoGetFromHost(field, info, _RC)
       else if (present(bundle)) then
          call ESMF_InfoGetFromHost(bundle, info, _RC)
       end if

       is_present = ESMF_InfoIsPresent(info, key="/conservation_type", _RC)
       
       if (is_present) then
          ! Conservation type explicitly set - use it
          call ESMF_InfoGet(info, key="/conservation_type", value=conservation_str, _RC)
          this%conservation_type = ConservationType(conservation_str)
          call ESMF_InfoGet(info, key="/is_conservable", value=this%is_conservable, _RC)
          call this%set_mirror(.false.)
       else
          ! Conservation type not set - infer from QuantityTypeMetadata if available
          ! This provides backward compatibility
          if (present(field)) then
             call MAPL_FieldGet(field, quantity_type_metadata=qty_metadata, _RC)
          else if (present(bundle)) then
             call MAPL_FieldBundleGet(bundle, quantity_type_metadata=qty_metadata, _RC)
          end if

          if (.not. qty_metadata%is_mirror()) then
             qty_type = qty_metadata%get_quantity_type()
             
             ! Infer conservation type from quantity type
             select case (qty_type%to_string())
             case ("QUANTITY_MIXING_RATIO")
                this%conservation_type = CONSERVE_MASS
                this%is_conservable = .true.
             case ("QUANTITY_CONCENTRATION")
                this%conservation_type = CONSERVE_MASS
                this%is_conservable = .true.
             case ("QUANTITY_PRESSURE")
                ! Surface pressure conserves mass (relates to column mass)
                this%conservation_type = CONSERVE_MASS
                this%is_conservable = .true.
             case ("QUANTITY_EXTENSIVE")
                ! Extensive quantities are conservable (mass per unit area)
                this%conservation_type = CONSERVE_MASS
                this%is_conservable = .true.
             case ("QUANTITY_TEMPERATURE", "QUANTITY_UNKNOWN")
                ! Not conservable
                this%conservation_type = CONSERVE_NONE
                this%is_conservable = .false.
             case default
                ! Unknown - default to non-conservable
                this%conservation_type = CONSERVE_NONE
                this%is_conservable = .false.
             end select
             
             call this%set_mirror(.false.)
          else
             ! No quantity type metadata - remain as mirror
             call this%set_mirror(.true.)
          end if
       end if

       _RETURN(_SUCCESS)
       _UNUSED_DUMMY(state)
    end subroutine update_from_payload

    subroutine update_payload(this, field, bundle, state, rc)
       class(ConservationAspect), intent(in) :: this
       type(esmf_Field), optional, intent(inout) :: field
       type(esmf_FieldBundle), optional, intent(inout) :: bundle
       type(esmf_State), optional, intent(inout) :: state
       integer, optional, intent(out) :: rc

       integer :: status
       type(ESMF_Info) :: info

       _RETURN_UNLESS(present(field) .or. present(bundle))

       ! Get Info object from field or bundle
       if (present(field)) then
          call ESMF_InfoGetFromHost(field, info, _RC)
       else if (present(bundle)) then
          call ESMF_InfoGetFromHost(bundle, info, _RC)
       end if

       call ESMF_InfoSet(info, key="/conservation_type", value=this%conservation_type%to_string(), _RC)
       call ESMF_InfoSet(info, key="/is_conservable", value=this%is_conservable, _RC)

       _RETURN(_SUCCESS)
       _UNUSED_DUMMY(state)
    end subroutine update_payload

   subroutine print_aspect(this, file, line, rc)
      class(ConservationAspect), intent(in) :: this
      character(*), intent(in) :: file
      integer, intent(in) :: line
      integer, optional, intent(out) :: rc

      _HERE, file, line, this%is_mirror()
      _HERE, file, line, 'conservation_type:', this%conservation_type%to_string()
      _HERE, file, line, 'is_conservable:', this%is_conservable

      _RETURN(_SUCCESS)
   end subroutine print_aspect

end module mapl3g_ConservationAspect
