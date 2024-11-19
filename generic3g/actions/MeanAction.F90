#include "MAPL_Generic.h"
module mapl3g_MeanAction
   use mapl3g_AccumulatorAction
   use MAPL_InternalConstantsMod, only: MAPL_UNDEFINED_REAL, MAPL_UNDEFINED_REAL64
   use MAPL_ExceptionHandling
   use MAPL_FieldPointerUtilities, only: assign_fptr
   use mapl3g_FieldCreate, only: MAPL_FieldCreate
   use mapl3g_FieldGet, only: MAPL_FieldGet
   use MAPL_FieldUtilities, only: FieldSet
   use ESMF
   implicit none
   private
   public :: MeanAction

   type, extends(AccumulatorAction) :: MeanAction
      type(ESMF_Field) :: counter_field
   contains
!      procedure :: clear => clear_mean_accumulator
      procedure :: clear_post => clear_mean_post
      procedure :: initialize_post => mean_initialize_post
      procedure :: initialize_pre => mean_initialize_pre
      procedure :: accumulate_R4 => accumulate_mean_R4
      procedure :: update_pre => mean_update_pre
      procedure :: calculate_mean
      procedure :: calculate_mean_R4
   end type MeanAction

contains

   subroutine mean_initialize_pre(this, rc)
      class(MeanAction), intent(inout) :: this
      integer, optional, intent(out) :: rc
      
      integer :: status

      if(this%initialized()) then
         call ESMF_FieldDestroy(this%counter_field, _RC)
      end if
      _RETURN(_SUCCESS)

   end subroutine mean_initialize_pre

   subroutine mean_initialize_post(this, rc)
      class(MeanAction), intent(inout) :: this
      integer, optional, intent(out) :: rc
      
      integer :: status
      type(ESMF_Geom) :: geom
      integer, allocatable :: gmap(:)
      integer :: ndims

      associate(f => this%accumulation_field)
         call ESMF_FieldGet(f, dimCount=ndims, _RC)
         allocate(gmap(ndims))
         call ESMF_FieldGet(f, geom=geom, gridToFieldMap=gmap, _RC)
         this%counter_field =  MAPL_FieldCreate(geom, typekind=this%typekind, gridToFieldMap=gmap, _RC) !, &
      end associate
      call this%clear(_RC)
      _RETURN(_SUCCESS)

   end subroutine mean_initialize_post

!   subroutine clear_mean_accumulator(this, rc)
!      class(MeanAction), intent(inout) :: this
!      integer, optional, intent(out) :: rc
!      
!      integer :: status
!
!      call this%AccumulatorAction%clear(_RC)
!      call FieldSet(this%counter_field, this%CLEAR_VALUE_R4, _RC)
!      _RETURN(_SUCCESS)
!
!   end subroutine clear_mean_accumulator

   subroutine clear_mean_post(this, rc)
      class(MeanAction), intent(inout) :: this
      integer, optional, intent(out) :: rc
      
      integer :: status

      call FieldSet(this%counter_field, this%CLEAR_VALUE_R4, _RC)
      _RETURN(_SUCCESS)

   end subroutine clear_mean_post

   subroutine calculate_mean(this, rc)
      class(MeanAction), intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status

      if(this%typekind == ESMF_TypeKind_R4) then
         call this%calculate_mean_R4(_RC)
      else
         _FAIL('Unsupported typekind')
      end if
      _RETURN(_SUCCESS)

   end subroutine calculate_mean

   subroutine mean_update_pre(this, rc)
      class(MeanAction), intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status

      call this%calculate_mean(_RC)
      _RETURN(_SUCCESS)

   end subroutine mean_update_pre

   subroutine calculate_mean_R4(this, rc)
      class(MeanAction), intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status
      real(kind=ESMF_KIND_R4), pointer :: current_ptr(:) => null()
      real(kind=ESMF_KIND_R4), pointer :: counter(:) => null()
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
      real(kind=ESMF_KIND_R4), pointer :: current(:) => null()
      real(kind=ESMF_KIND_R4), pointer :: latest(:) => null()
      real(kind=ESMF_KIND_R4), pointer :: counter(:) => null()
      real(kind=ESMF_KIND_R4) :: undef

      undef = MAPL_UNDEFINED_REAL
      call assign_fptr(this%accumulation_field, current, _RC)
      call assign_fptr(update_field, latest, _RC)
      call assign_fptr(this%counter_field, counter, _RC)
      where(current /= undef .and. latest /= undef)
        current = current + latest
        counter = counter + 1_ESMF_KIND_R4
      end where
      _RETURN(_SUCCESS)

   end subroutine accumulate_mean_R4

end module mapl3g_MeanAction
