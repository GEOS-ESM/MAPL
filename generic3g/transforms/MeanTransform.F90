#include "MAPL_Generic.h"
module mapl3g_MeanTransform
   use mapl3g_AccumulatorTransform
   use MAPL_InternalConstantsMod, only: MAPL_UNDEFINED_REAL, MAPL_UNDEFINED_REAL64
   use MAPL_ExceptionHandling
   use MAPL_FieldPointerUtilities, only: assign_fptr
   use mapl3g_FieldCreate, only: MAPL_FieldCreate
   use mapl3g_Field_API, only: MAPL_FieldGet
   use MAPL_FieldUtilities, only: FieldSet
   use ESMF
   implicit none
   private
   public :: MeanTransform
   public :: construct_MeanTransform

   type, extends(AccumulatorTransform) :: MeanTransform
      type(ESMF_Field), allocatable :: counter_field
   contains
      procedure :: clear => clear_mean
      procedure :: create_fields => create_fields_mean
      procedure :: update_result => update_result_mean
      procedure :: calculate_mean
      procedure :: calculate_mean_R4
      procedure :: accumulate_R4
   end type MeanTransform

   type(ESMF_TypeKind_Flag), parameter :: COUNTER_TYPEKIND = ESMF_TYPEKIND_I4
   integer, parameter :: COUNTER_KIND = ESMF_KIND_I4

contains

   function construct_MeanTransform(typekind) result(acc)
      type(MeanTransform) :: acc
      type(ESMF_TypeKind_Flag), intent(in) :: typekind

      acc%typekind = typekind

   end function construct_MeanTransform

   subroutine create_fields_mean(this, import_field, export_field, rc)
      class(MeanTransform), intent(inout) :: this
      type(ESMF_Field), intent(inout) :: import_field
      type(ESMF_Field), intent(inout) :: export_field
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_Geom) :: geom
      integer, allocatable :: gmap(:)
      integer :: ndims

      _RETURN_IF(this%initialized)
      call this%AccumulatorTransform%create_fields(import_field, export_field, _RC)
      associate(f => this%accumulation_field)
         call ESMF_FieldGet(f, dimCount=ndims, _RC)
         allocate(gmap(ndims))
         call ESMF_FieldGet(f, geom=geom, gridToFieldMap=gmap, _RC)
         this%counter_field =  MAPL_FieldCreate(geom, typekind=ESMF_TYPEKIND_I4, gridToFieldMap=gmap, _RC)
      end associate
      _RETURN(_SUCCESS)

   end subroutine create_fields_mean

   subroutine clear_mean(this, rc)
      class(MeanTransform), intent(inout) :: this
      integer, optional, intent(out) :: rc
      
      integer :: status
      integer(COUNTER_KIND), pointer :: counter(:)

      call this%AccumulatorTransform%clear(_RC)
      counter => null()
      call assign_fptr(this%counter_field, counter, _RC)
      counter = 0_COUNTER_KIND
      _RETURN(_SUCCESS)

   end subroutine clear_mean

   subroutine calculate_mean(this, rc)
      class(MeanTransform), intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status

      if(this%typekind == ESMF_TYPEKIND_R4) then
         call this%calculate_mean_R4(_RC)
      else
         _FAIL('Unsupported typekind')
      end if
      _RETURN(_SUCCESS)

   end subroutine calculate_mean

   subroutine update_result_mean(this, rc)
      class(MeanTransform), intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status

      call this%calculate_mean(_RC)
      call this%AccumulatorTransform%update_result(_RC)
      _RETURN(_SUCCESS)

   end subroutine update_result_mean

   subroutine calculate_mean_R4(this, rc)
      class(MeanTransform), intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status
      real(kind=ESMF_KIND_R4), pointer :: current_ptr(:)
      integer(kind=COUNTER_KIND), pointer :: counter(:)
      real(kind=ESMF_KIND_R4), parameter :: UNDEF = MAPL_UNDEFINED_REAL

      current_ptr => null()
      counter => null()
      call assign_fptr(this%accumulation_field, current_ptr, _RC)
      call assign_fptr(this%counter_field, counter, _RC)
      where(counter /= 0)
         current_ptr = current_ptr / counter
      elsewhere
         current_ptr = UNDEF
      end where
      _RETURN(_SUCCESS)

   end subroutine calculate_mean_R4

   subroutine accumulate_R4(this, update_field, rc)
      class(MeanTransform), intent(inout) :: this
      type(ESMF_Field), intent(inout) :: update_field
      integer, optional, intent(out) :: rc

      integer :: status
      real(kind=ESMF_KIND_R4), pointer :: current(:)
      real(kind=ESMF_KIND_R4), pointer :: latest(:)
      integer(kind=COUNTER_KIND), pointer :: counter(:)
      real(kind=ESMF_KIND_R4), parameter :: UNDEF = MAPL_UNDEFINED_REAL

      current => null()
      latest => null()
      counter => null()
      call assign_fptr(this%accumulation_field, current, _RC)
      call assign_fptr(update_field, latest, _RC)
      call assign_fptr(this%counter_field, counter, _RC)
      where(latest /= UNDEF)
        current = current + latest
        counter = counter + 1_COUNTER_KIND
      end where
      _RETURN(_SUCCESS)

   end subroutine accumulate_R4

end module mapl3g_MeanTransform
