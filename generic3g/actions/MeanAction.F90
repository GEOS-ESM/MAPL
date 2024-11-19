#include "MAPL_Generic.h"
module mapl3g_MeanAction
   use mapl3g_AccumulatorAction
   use MAPL_InternalConstantsMod, only: MAPL_UNDEFINED_REAL, MAPL_UNDEFINED_REAL64
   use MAPL_ExceptionHandling
   use MAPL_FieldPointerUtilities
   use ESMF
   implicit none
   private
   public :: MeanAction

   type, extends(AccumulatorAction) :: MeanAction
      type(ESMF_Field) :: counter_field
   contains
      procedure :: clear_accumulator => clear_mean_accumulator
      procedure :: post_initialize => mean_post_initialize
      procedure :: pre_initialize => mean_pre_initialize
      procedure :: accumulate_R4 => accumulate_mean_R4
      procedure :: pre_update => mean_pre_update
      procedure :: calculate_mean
      procedure :: calculate_mean_R4
      procedure :: increment_counter
   end type MeanAction

contains

   subroutine mean_pre_initialize(this, rc)
      class(MeanAction), intent(inout) :: this
      integer, optional, intent(out) :: rc
      
      integer :: status

      if(this%initialized()) then
         call ESMF_FieldDestroy(this%counter_field, _RC)
      end if
      call Accumulator%pre_initialize(_RC)
      _RETURN(_SUCCESS)

   end subroutine mean_pre_initialize

   subroutine mean_post_initialize(this, rc)
      class(MeanAction), intent(inout) :: this
      integer, optional, intent(out) :: rc
      
      integer :: status
      type(ESMF_Geom) :: geom
      type(ESMF_Grid) :: grid
      type(ESMF_TypeKind_Flag) :: typekind
      type(ESMF_StaggerLoc) :: stagger_loc
      integer :: gridToFieldMap(:)
      type(UngriddedDims), optional, intent(in) :: ungridded_dims
      integer, optional, intent(in) :: num_levels
      type(VerticalStaggerLoc), optional, intent(in) :: vert_staggerloc
      ! Get from accumulation field

      this%counter_field =  MAPL_FieldCreate(geom, typekind, gridToFieldMap=gridToFieldMap,&
         ungridded_dims=ungridded_dims, num_levels, vert_staggerloc=vert_staggerloc, _RC)
      call AccumulatorAction%post_initialize(_RC)
      _RETURN(_SUCCESS)

   end subroutine mean_post_initialize

   subroutine clear_mean_accumulator(this, rc)
      class(MeanAction), intent(inout) :: this
      integer, optional, intent(out) :: rc
      
      integer :: status

      call this%AccumulatorAction%clear_accumulator(_RC)
      call FieldSet(this%counter_field, 0_ESMF_KIND_R8, _RC)
      _RETURN(_SUCCESS)

   end subroutine clear_mean_accumulator

   subroutine calculate_mean(this, rc)
      class(MeanAction), intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_TypeKind_Flag) :: tk

      call ESMF_FieldGet(this%accumulation_field, typekind=tk, _RC)
      if(tk == ESMF_TypeKind_R4) then
         call this%calculate_mean_R4(_RC)
      else
         _FAIL('Unsupported typekind')
      end if
      _RETURN(_SUCCESS)

   end subroutine calculate_mean

   subroutine mean_pre_update(this, rc)
      class(MeanAction), intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status

      call this%calculate_mean(_RC)
      call Accumulator%pre_update(_RC)
      _RETURN(_SUCCESS)

   end mean_pre_update

   subroutine calculate_mean_R4(this, rc)
      class(MeanAction), intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status
      real(kind=ESMF_KIND_R4), pointer :: current_ptr(:) => null()
      real(kind=ESMF_KIND_R8), pointer :: counter(:) => null()
      real(kind=ESMF_KIND_R4), parameter :: UNDEF = MAPL_UNDEFINED_REAL

      call assign_fptr(this%accumulation_field, current_ptr, _RC)
      call assign_fptr(this%counter_field, counter, _RC)
      where(current_ptr /= UNDEF .and. counter /= 0)
         current_ptr = current_ptr / counter
      elsewhere
         current_ptr = UNDEF
      end where
      _RETURN(_SUCCESS)

   end subroutine calculate_mean_R4

   subroutine accumulate_mean_R4(this, update_field, rc)
      class(MeanAction), intent(inout) :: this
      type(ESMF_Field), intent(inout) :: update_field
      integer, optional, intent(out) :: rc

      integer :: status
      real(kind=ESMF_KIND_R4), pointer :: current(:)
      real(kind=ESMF_KIND_R4), pointer :: latest(:)
      real(kind=ESMF_KIND_R8), pointer :: counter(:) => null()
      real(kind=ESMF_KIND_R4) :: undef

      undef = MAPL_UNDEFINED_REAL
      call assign_fptr(this%accumulation_field, current, _RC)
      call assign_fptr(update_field, latest, _RC)
      call assign_fptr(this%counter_field, _RC)
      where(current /= undef .and. latest /= undef)
        current = current + latest
        counter = count+1
      end where
      _RETURN(_SUCCESS)

   end subroutine accumulate_mean_R4

end module mapl3g_MeanAction
