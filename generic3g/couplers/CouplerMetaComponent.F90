#include "MAPL.h"

module mapl3g_CouplerMetaComponent
   use mapl3g_TransformId
   use mapl3g_esmf_info_keys, only: INFO_SHARED_NAMESPACE
   use mapl3g_CouplerPhases
   use mapl3g_ComponentDriver, only: ComponentDriver, ComponentDriverPtr
   use mapl3g_GriddedComponentDriver, only: GriddedComponentDriver
   use mapl3g_ComponentDriverVector, only: ComponentDriverVector
   use mapl3g_ComponentDriverPtrVector, only: ComponentDriverPtrVector
   use mapl3g_ExtensionTransform
   use mapl3g_VerticalRegridTransform
   use mapl_ErrorHandlingMod
   use mapl3g_ESMF_Interfaces
   use mapl3g_Field_API
   use mapl3g_FieldBundle_API
   use esmf

   implicit none(type,external)
   private

   ! Class
   public :: CouplerMetaComponent

   ! non TBF procedures
   public :: get_coupler_meta
   public :: attach_coupler_meta
   public :: free_coupler_meta

   type :: TimeVaryingAspects
      real, allocatable :: interpolation_weights(:)
      type(ESMF_Geom), allocatable :: geom

      type(ESMF_Geom), allocatable :: geom_in
      type(ESMF_Geom), allocatable :: geom_out
      
   end type TimeVaryingAspects

   type :: CouplerMetaComponent
      private
      class(ExtensionTransform), allocatable :: transform
      type(ComponentDriverPtrVector) :: sources
      type(ComponentDriverVector) :: consumers
      logical :: stale = .true.

      type(TimeVaryingAspects) :: time_varying
   contains
      ! ESMF methods
      procedure :: initialize
      procedure :: update
      procedure :: update_time_varying
      procedure :: invalidate
      procedure :: clock_advance

      ! Helper procedures
      procedure :: initialize_sources
      procedure :: update_sources
      procedure :: invalidate_consumers
      procedure :: add_source
      procedure :: add_consumer

      ! Accessors
      procedure, non_overridable :: is_up_to_date
      procedure, non_overridable :: is_stale
      procedure, non_overridable :: set_up_to_date
      procedure, non_overridable :: set_stale
   end type CouplerMetaComponent

   character(len=*), parameter :: COUPLER_META_PRIVATE_STATE = "CouplerMetaComponent Private State"

   type CouplerMetaWrapper
      type(CouplerMetaComponent), pointer :: coupler_meta
   end type CouplerMetaWrapper

   interface CouplerMetaComponent
      procedure :: new_CouplerMetaComponent
   end interface CouplerMetaComponent

   character(*), parameter :: IMPORT_NAME = 'import[1]'
   character(*), parameter :: EXPORT_NAME = 'export[1]'

contains

   function new_CouplerMetaComponent(transform, source) result (this)
      type(CouplerMetaComponent) :: this
      class(ExtensionTransform), intent(in) :: transform
      class(ComponentDriver), target, optional, intent(in) :: source

      type(ComponentDriverPtr) :: source_wrapper

      this%transform = transform
      if (present(source)) then
         source_wrapper%ptr => source
         call this%sources%push_back(source_wrapper)
      end if

   end function new_CouplerMetaComponent

   recursive subroutine initialize(this, importState, exportState, clock, rc)
      class(CouplerMetaComponent), intent(inout) :: this
      type(ESMF_State), intent(inout) :: importState
      type(ESMF_State), intent(inout) :: exportState
      type(ESMF_Clock), intent(inout) :: clock
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_Geom) :: geom_in, geom_out

      call this%initialize_sources(_RC)

      if (all(this%transform%get_transformId() /= [EXTEND_TRANSFORM_ID, EVAL_TRANSFORM_ID])) then
         call copy_shared_attributes()
            
         geom_in = get_geom(importState, IMPORT_NAME, _RC)
         geom_out = get_geom(exportState, EXPORT_NAME, _RC)
         if (this%transform%get_transformId() /= GEOM_TRANSFORM_ID) then
!#         _ASSERT(geom_in == geom_out, 'mismatched geom in non regrid coupler')
            this%time_varying%geom = geom_in
         else
            this%time_varying%geom_in = geom_in
            this%time_varying%geom_out = geom_out
         end if
      end if

      call this%transform%initialize(importState, exportState, clock, _RC)

      _RETURN(_SUCCESS)

   contains

      subroutine copy_shared_attributes(rc)
         integer, optional, intent(out) :: rc

         integer :: status
         type(ESMF_Info) :: info_in, info_out
         type(ESMF_Info) :: shared_attrs
         logical :: has_shared

         call get_info(importState, itemName=IMPORT_NAME, info=info_in, _RC)
         has_shared = ESMF_InfoIsPresent(info_in, INFO_SHARED_NAMESPACE, _RC)
         _RETURN_UNLESS(has_shared)

         ! Shared attributes - can only alter from import side
         shared_attrs = ESMF_InfoCreate(info_in, INFO_SHARED_NAMESPACE, _RC)

         call get_info(exportState, itemName=EXPORT_NAME, info=info_out, _RC)
         call ESMF_InfoSet(info_out, INFO_SHARED_NAMESPACE, shared_attrs, _RC)
         call ESMF_InfoDestroy(shared_attrs)
      
         _RETURN(_SUCCESS)
      end subroutine copy_shared_attributes

   end subroutine initialize

   recursive subroutine initialize_sources(this, rc)
      class(CouplerMetaComponent) :: this
      integer, intent(out) :: rc

      integer :: status
      integer :: i
      type(ComponentDriverPtr), pointer :: source_wrapper

      do i = 1, this%sources%size()
         source_wrapper => this%sources%of(i)
         call source_wrapper%ptr%initialize(phase_idx=GENERIC_COUPLER_INITIALIZE, _RC)
      end do

      _RETURN(_SUCCESS)
   end subroutine initialize_sources


   recursive subroutine update(this, importState, exportState, clock, rc)
      class(CouplerMetaComponent), intent(inout) :: this
      type(ESMF_State), intent(inout) :: importState
      type(ESMF_State), intent(inout) :: exportState
      type(ESMF_Clock), intent(inout) :: clock
      integer, optional, intent(out) :: rc

      integer :: status

      _RETURN_IF(this%is_up_to_date())

      call this%update_sources(_RC)
      if (all(this%transform%get_transformId() /= [EXTEND_TRANSFORM_ID, EVAL_TRANSFORM_ID])) then
         call this%update_time_varying(importState, exportState, clock, _RC)
      end if

      call this%transform%update(importState, exportState, clock, _RC)
      call this%set_up_to_date()

      _RETURN(_SUCCESS)
   end subroutine update


   ! Check if export item has been updated and update import item
   ! accordingly.
   recursive subroutine update_time_varying(this, importState, exportState, clock, rc)
      class(CouplerMetaComponent), intent(inout) :: this
      type(ESMF_State), intent(inout) :: importState
      type(ESMF_State), intent(inout) :: exportState
      type(ESMF_Clock), intent(inout) :: clock
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_Field) :: f_in, f_out
      type(ESMF_StateItem_Flag) :: itemType_in, itemType_out

      call ESMF_StateGet(importState, itemName=IMPORT_NAME, itemType=itemType_in, _RC)
      call ESMF_StateGet(exportState, itemName=EXPORT_NAME, itemType=itemType_out, _RC)

      call dispatch(itemType_in, itemType_out, _RC)
      
      _RETURN(_SUCCESS)

   contains

      subroutine dispatch (itemType_in, itemType_out, rc)
         type(ESMF_StateItem_Flag), intent(in) :: itemType_in, itemType_out
         integer, optional, intent(out) :: rc

         integer :: status

         if (itemtype_in == ESMF_STATEITEM_FIELD) then
            if (itemtype_out == ESMF_STATEITEM_FIELD) then
               call update_time_varying_field_field(_RC)
            elseif (itemtype_out == ESMF_STATEITEM_FIELDBUNDLE) then
               _FAIL('No support for field --> field bundle')
            end if
         elseif (itemType_in == ESMF_STATEITEM_FIELDBUNDLE) then
            if (itemType_out == ESMF_STATEITEM_FIELD) then
               call update_time_varying_fieldbundle_field(_RC)
            elseif (itemType_out == ESMF_STATEITEM_FIELDBUNDLE) then
               call update_time_varying_fieldbundle_fieldbundle(_RC)
            else
               _FAIL('export item must be a field or a field bundle')
            end if
         else
            _FAIL('import item must be a field or a field bundle')
         end if
         _RETURN(_SUCCESS)
      end subroutine dispatch

      ! Things that are allowed to be time-varying for bundle-bundle:
      !   - interpolation weights
      !   - geom (sampler, extdata)
      subroutine update_time_varying_fieldbundle_fieldbundle(rc)
         integer, optional, intent(out) :: rc

         integer :: status
         type(ESMF_FieldBundle) :: fb_in, fb_out
         real, allocatable :: interpolation_weights(:)
         type(FieldBundleType_Flag) :: fieldBundleType

         call ESMF_StateGet(importState, itemName=IMPORT_NAME, fieldBundle=fb_in, _RC)
         call ESMF_StateGet(exportState, itemName=EXPORT_NAME, fieldBundle=fb_out, _RC)

         ! (1) Interpolation weights can only change on import side
         call MAPL_FieldBundleGet(fb_in, fieldBundleType=fieldBundleType, _RC)
         if (fieldBundleType == FIELDBUNDLETYPE_BRACKET) then
            call MAPL_FieldBundleGet(fb_in, interpolation_weights=interpolation_weights, _RC)
            if (.not. same_weights(interpolation_weights, this%time_varying%interpolation_weights)) then
               call MAPL_FieldBundleSet(fb_out, interpolation_weights=interpolation_weights, _RC)
               this%time_varying%interpolation_weights = interpolation_weights
            end if
         end if

         _RETURN(_SUCCESS)
      end subroutine update_time_varying_fieldbundle_fieldbundle


      logical function same_weights(w1, w2) result(same)
         real, allocatable, intent(in) :: w1(:), w2(:)


         same = allocated(w1) .eqv. allocated(w2)
         if (.not. same) return
         if (.not. allocated(w1)) return

         same = size(w1) == size(w2)
         if (.not. same) return

         same = all(w1 == w2)
      end function same_weights

      ! Things that are allowed to be time-varying for field-field
      !   - geom?
      subroutine update_time_varying_fieldbundle_field(rc)
         integer, optional, intent(out) :: rc

         integer :: status
         type(ESMF_FieldBundle) :: fb_in
         type(ESMF_Field) :: f_out
         type(ESMF_Geom) :: geom_in, geom_out

         call ESMF_StateGet(importState, itemName=IMPORT_NAME, fieldBundle=fb_in, _RC)
         call ESMF_StateGet(exportState, itemName=EXPORT_NAME, field=f_out, _RC)

         geom_in = get_geom(importState, IMPORT_NAME, _RC)
         geom_out = get_geom(exportState, EXPORT_NAME, _RC)

         if (this%transform%get_transformId() /= GEOM_TRANSFORM_ID) then ! only one side can vary
            if (geom_in /= this%time_varying%geom) then
               call MAPL_FieldSet(f_out, geom=geom_in, _RC)
               this%time_varying%geom = geom_in
            else if (geom_out /= this%time_varying%geom) then
               call MAPL_FieldBundleSet(fb_in, geom=geom_out)
               this%time_varying%geom = geom_out
            end if
         else
            if (geom_in /= this%time_varying%geom_in .or. geom_out /= this%time_varying%geom_out) then
               call this%transform%initialize(importState, exportState, clock, _RC)
               this%time_varying%geom_in = geom_in
               this%time_varying%geom_out = geom_out
            end if
         end if

         _RETURN(_SUCCESS)
      end subroutine update_time_varying_fieldbundle_field
      
      ! Things that are allowed to be time-varying for field-field
      !   - geom (sampler, extdata)
      subroutine update_time_varying_field_field(rc)
         integer, optional, intent(out) :: rc

         integer :: status
         type(ESMF_Field) :: f_in, f_out
         type(ESMF_Geom) :: geom_in, geom_out

         call ESMF_StateGet(importState, itemName=IMPORT_NAME, field=f_in, _RC)
         call ESMF_StateGet(exportState, itemName=EXPORT_NAME, field=f_out, _RC)

         geom_in = get_geom(importState, IMPORT_NAME, _RC)
         geom_out = get_geom(exportState, EXPORT_NAME, _RC)

         if (this%transform%get_transformId() /= GEOM_TRANSFORM_ID) then ! only one side can vary
            if (geom_in /= this%time_varying%geom) then
               call MAPL_FieldSet(f_out, geom=geom_in, _RC)
               this%time_varying%geom = geom_in
            else if (geom_out /= this%time_varying%geom) then
               call MAPL_FieldSet(f_in, geom=geom_out)
               this%time_varying%geom = geom_out
            end if
         else
            if (geom_in /= this%time_varying%geom_in .or. geom_out /= this%time_varying%geom_out) then
               call this%transform%initialize(importState, exportState, clock, _RC)
               this%time_varying%geom_in = geom_in
               this%time_varying%geom_out = geom_out
            end if
         end if

         _RETURN(_SUCCESS)
      end subroutine update_time_varying_field_field
      
   end subroutine update_time_varying

   recursive subroutine update_sources(this, rc)
      class(CouplerMetaComponent) :: this
      integer, intent(out) :: rc

      integer :: status
      integer :: i
      type(ComponentDriverPtr), pointer :: source_wrapper

      do i = 1, this%sources%size()
         source_wrapper => this%sources%of(i)
         call source_wrapper%ptr%run(phase_idx=GENERIC_COUPLER_UPDATE, _RC)
      end do

      _RETURN(_SUCCESS)
   end subroutine update_sources

   ! Check if export item has been updated and update import item
   ! accordingly.
   recursive subroutine invalidate_time_varying(this, importState, exportState, clock, rc)
      class(CouplerMetaComponent), intent(inout) :: this
      type(ESMF_State), intent(inout) :: importState
      type(ESMF_State), intent(inout) :: exportState
      type(ESMF_Clock), intent(inout) :: clock
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_Field) :: f_in, f_out

!#      _RETURN_UNLESS(this%import_is_time_varying())
      call ESMF_StateGet(importState, itemName=IMPORT_NAME, field=f_in, _RC)
      call ESMF_StateGet(exportState, itemName=EXPORT_NAME, field=f_out, _RC)

!#      call FieldUpdate(f_out, from=f_in, ignore=this%transform%get_ignore(), _RC)
      
      _RETURN(_SUCCESS)
   end subroutine invalidate_time_varying

   recursive subroutine invalidate(this, importState, exportState, clock, rc)
      class(CouplerMetaComponent) :: this
      type(ESMF_State) :: importState
      type(ESMF_State) :: exportState
      type(ESMF_Clock) :: clock
      integer, intent(out) :: rc

      integer :: status

      if(this%transform%runs_invalidate()) then
         call this%update_sources(_RC)
         call this%transform%invalidate(importState, exportState, clock, _RC)
      end if
      _RETURN_IF(this%is_stale())

      call this%invalidate_consumers(_RC)
      call this%set_stale()
  
      _RETURN(_SUCCESS)

   end subroutine invalidate
      
   recursive subroutine invalidate_consumers(this, rc)
      class(CouplerMetaComponent), target :: this
      integer, intent(out) :: rc

      integer :: status
      class(ComponentDriver), pointer :: consumer
      integer :: i

      do i = 1, this%consumers%size()
         consumer => this%consumers%of(i)
         call consumer%run(phase_idx=GENERIC_COUPLER_INVALIDATE, _RC)
      end do

      _RETURN(_SUCCESS)
   end subroutine invalidate_consumers

   recursive subroutine clock_advance(this, importState, exportState, clock, rc)
      class(CouplerMetaComponent), intent(inout) :: this
      type(ESMF_State), intent(inout) :: importState
      type(ESMF_State), intent(inout) :: exportState
      type(ESMF_Clock), intent(inout) :: clock
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_Alarm) :: alarm
      logical :: is_ringing

      call ESMF_ClockGetAlarm(clock, "MAPL::RUN_ALARM", alarm, _RC)
      is_ringing = ESMF_AlarmIsRinging(alarm, _RC)
      _RETURN_UNLESS(is_ringing)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(this)
      _UNUSED_DUMMY(exportState)
      _UNUSED_DUMMY(importState)
   end subroutine clock_advance
      
   subroutine add_consumer(this, consumer)
      class(CouplerMetaComponent), target, intent(inout) :: this
      class(ComponentDriver) :: consumer

      call this%consumers%push_back(consumer)
   end subroutine add_consumer

   subroutine add_source(this, source)
      class(CouplerMetaComponent), target, intent(inout) :: this
      type(GriddedComponentDriver), pointer, intent(in) :: source

      type(ComponentDriverPtr) :: source_wrapper
      source_wrapper%ptr => source

      call this%sources%push_back(source_wrapper)
   end subroutine add_source

   function get_coupler_meta(gridcomp, rc) result(meta)
      type(CouplerMetaComponent), pointer :: meta
      type(ESMF_GridComp), intent(inout) :: gridcomp
      integer, optional, intent(out) :: rc

      integer :: status

      !TODO: This block is a workaround for weird link error with NAG
      !      7.2 Appears to be a collision in numbering of local
      !      scopes.
      block
      end block
      _GET_NAMED_PRIVATE_STATE(gridcomp, CouplerMetaComponent, COUPLER_META_PRIVATE_STATE, meta)

      _RETURN(_SUCCESS)
   end function get_coupler_meta

   subroutine attach_coupler_meta(gridcomp, rc)
      type(ESMF_GridComp), intent(inout) :: gridcomp
      integer, optional, intent(out) :: rc

      integer :: status

      _SET_NAMED_PRIVATE_STATE(gridcomp, CouplerMetaComponent, COUPLER_META_PRIVATE_STATE)

      _RETURN(_SUCCESS)
   end subroutine attach_coupler_meta

   subroutine free_coupler_meta(gridcomp, rc)
      type(ESMF_GridComp), intent(inout) :: gridcomp
      integer, optional, intent(out) :: rc

      integer :: status
      type(CouplerMetaWrapper) :: wrapper

      call MAPL_UserCompGetInternalState(gridcomp, COUPLER_META_PRIVATE_STATE, wrapper, status)
      _ASSERT(status==ESMF_SUCCESS, "CouplerMetaComponent not created for this gridcomp")

      deallocate(wrapper%coupler_meta)

      _RETURN(_SUCCESS)
   end subroutine free_coupler_meta

   pure subroutine set_up_to_date(this)
      class(CouplerMetaComponent), intent(inout) :: this
      this%stale = .false.
   end subroutine set_up_to_date

   pure subroutine set_stale(this)
      class(CouplerMetaComponent), intent(inout) :: this
      this%stale = .true.
    end subroutine set_stale

    pure logical function is_up_to_date(this)
      class(CouplerMetaComponent), intent(in) :: this
      is_up_to_date = .not. this%stale
   end function is_up_to_date

   pure logical function is_stale(this)
      class(CouplerMetaComponent), intent(in) :: this
      is_stale = this%stale
   end function is_stale

   function get_geom(state, itemName, rc) result(geom)
      type(ESMF_Geom) :: geom
      type(ESMF_State), intent(inout) :: state
      character(*), intent(in) :: itemName
      integer, optional, intent(out) :: rc

      integer :: status
      type(ESMF_StateItem_Flag) :: itemType
      type(ESMF_Field) :: f
      type(ESMF_FieldBundle) :: fb

      call ESMF_StateGet(state, itemName=itemName, itemType=itemType, _RC)
      if (itemType == ESMF_STATEITEM_FIELD) then
         call ESMF_StateGet(state, itemName=itemName, field=f, _RC)
         call MAPL_FieldGet(f, geom=geom, _RC)
      elseif (itemType == ESMF_STATEITEM_FIELDBundle) then
         call ESMF_StateGet(state, itemName=itemName, fieldBundle=fb, _RC)
         call MAPL_FieldBundleGet(fb, geom=geom, _RC)
      else
         _FAIL('unsupported itemType')
      end if

      _RETURN(_SUCCESS)
   end function get_geom

      subroutine get_info(state, itemName, info, rc)
         type(ESMF_State), intent(inout) :: state
         character(*), intent(in) :: itemName
         type(ESMF_Info), intent(out) :: info
         integer, optional, intent(out) :: rc

         integer :: status
         type(ESMF_Field) :: f
         type(ESMF_FieldBundle) :: fb
         type(ESMF_StateItem_Flag) :: itemType

         call ESMF_StateGet(state, itemName, itemType=itemType, _RC)

         if (itemType == ESMF_STATEITEM_FIELD) then
            call ESMF_StateGet(state, itemName, field=f, _RC)
            call ESMF_InfoGetFromHost(f, info, _RC)
         elseif (itemType == ESMF_STATEITEM_FIELDBUNDLE) then
            call ESMF_StateGet(state, itemName, fieldBundle=fb, _RC)
            call ESMF_InfoGetFromHost(fb, info, _RC)
         else
            _FAIL(itemName // ':: unsupported itemType; must be Field or FieldBundle')
         end if

         _RETURN(_SUCCESS)
      end subroutine get_info

end module mapl3g_CouplerMetaComponent
