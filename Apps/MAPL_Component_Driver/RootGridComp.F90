#include "MAPL_Generic.h"

module mapl3g_RootgGridComp

   use mapl_ErrorHandling
   use mapl3
   use mapl, only: MAPL_GetPointer
   use esmf
   use gFTL2_StringStringMap
   use MAPL_StateUtils
   use timeSupport

   implicit none
   private

   public :: setServices

   type :: Comp_Driver_Support
      type(StringStringMap) :: fillDefs
      character(len=:), allocatable :: runMode
      type(timeVar) :: tFunc
      real :: delay ! in seconds
   end type Comp_Driver_Support 

   character(*), parameter :: PRIVATE_STATE = "Comp_Driver_Support"
   character(*), parameter :: MAPL_SECTION = "mapl"
   character(*), parameter :: COMPONENT_STATES_SECTION = "states"
   character(*), parameter :: COMPONENT_EXPORT_STATE_SECTION = "export"
   character(*), parameter :: KEY_DEFAULT_VERT_PROFILE = "default_vertical_profile"
   character(len=*), parameter :: runModeGenerateExports = "GenerateExports"

contains

   subroutine setServices(gridcomp, rc)
      type(ESMF_GridComp) :: gridcomp
      integer, intent(out) :: rc

      integer :: status

       
      call MAPL_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_INITIALIZE, init, _RC)
      call MAPL_GridCompSetEntryPoint(gridcomp, ESMF_METHOD_RUN, run, phase_name="run", _RC)
            ! Attach private state
      _SET_NAMED_PRIVATE_STATE(gridcomp, Comp_Driver_Support, PRIVATE_STATE)
      call add_internal_specs(gridcomp, _RC)

      _RETURN(_SUCCESS)
      contains

         subroutine add_internal_specs(gridcomp, rc)
            type(ESMF_GridComp), intent(inout) :: gridcomp
            integer, intent(out), optional :: rc
            type(VariableSpec) :: varspec
            integer :: status
            varspec = make_VariableSpec(state_intent=ESMF_STATEINTENT_INTERNAL, &
                                        short_name='time_interval' , & 
                                        standard_name='unknown', &
                                        units='unknown', &
                                        vertical_stagger=VERTICAL_STAGGER_NONE, &
                                        default_value=0.0, _RC)
            call MAPL_GridCompAddVarSpec(gridcomp, varspec, _RC)
            varspec = make_VariableSpec(state_intent=ESMF_STATEINTENT_INTERNAL, &
                                        short_name='rand' , & 
                                        standard_name='randomnumber', &
                                        units='unknown', &
                                        vertical_stagger=VERTICAL_STAGGER_NONE, &
                                        default_value=0.0, _RC)
            call MAPL_GridCompAddVarSpec(gridcomp, varspec, _RC)
            varspec = make_VariableSpec(state_intent=ESMF_STATEINTENT_INTERNAL, &
                                        short_name='grid_lons' , & 
                                        standard_name='longitude', &
                                        units='degrees_east', &
                                        vertical_stagger=VERTICAL_STAGGER_NONE, &
                                        default_value=0.0, _RC)
            call MAPL_GridCompAddVarSpec(gridcomp, varspec, _RC)
            varspec = make_VariableSpec(state_intent=ESMF_STATEINTENT_INTERNAL, &
                                        short_name='grid_lats' , & 
                                        standard_name='latitude', &
                                        units='degrees_north', &
                                        vertical_stagger=VERTICAL_STAGGER_NONE, &
                                        default_value=0.0, _RC)
            call MAPL_GridCompAddVarSpec(gridcomp, varspec, _RC)
            _RETURN(_SUCCESS)

         end subroutine
   end subroutine setServices

   subroutine init(gridcomp, importState, exportState, clock, rc)
      type(ESMF_GridComp) :: gridcomp
      type(ESMF_State) :: importState
      type(ESMF_State) :: exportState
      type(ESMF_Clock) :: clock
      integer, intent(out) :: rc

      character(:), allocatable :: field_name
      type(ESMF_HConfig) :: hconfig, mapl_cfg, states_cfg, export_cfg, field_cfg, fill_def
      logical :: has_export_section, has_default_vert_profile
      real(kind=ESMF_KIND_R4), allocatable :: default_vert_profile(:)
      real(kind=ESMF_KIND_R4), pointer :: ptr3d(:, :, :)
      integer :: ii, jj, shape_(3), status
      type(ESMF_State) :: internal_state
      type(Comp_Driver_Support), pointer :: support
      type(ESMF_HConfigIter) :: iter, e, b
      logical :: is_present
      character(len=:), allocatable :: key, keyVal
      type(ESMF_Time) :: current_time

      _GET_NAMED_PRIVATE_STATE(gridcomp, Comp_Driver_Support, PRIVATE_STATE, support)
      call MAPL_GridCompGet(gridcomp, hconfig=hconfig, _RC)
      ! ASSUME: mapl and states sections always exist
      mapl_cfg = ESMF_HConfigCreateAt(hconfig, keyString=MAPL_SECTION, _RC)
      states_cfg = ESMF_HConfigCreateAt(mapl_cfg, keyString=COMPONENT_STATES_SECTION, _RC)
      has_export_section = ESMF_HConfigIsDefined(states_cfg, keyString=COMPONENT_EXPORT_STATE_SECTION, _RC)
      _RETURN_UNLESS(has_export_section)

      ! For each field getting 'export'ed, check hconfig and use default_vert_profile if specified
      export_cfg = ESMF_HConfigCreateAt(states_cfg, keyString=COMPONENT_EXPORT_STATE_SECTION, _RC)
      b = ESMF_HConfigIterBegin(export_cfg, _RC)
      e = ESMF_HConfigIterEnd(export_cfg, _RC)
      iter = b
      do while (ESMF_HConfigIterLoop(iter, b, e))
         field_name = ESMF_HConfigAsStringMapKey(iter, _RC)
         ! print *, "FIELD: ", field_name
         field_cfg = ESMF_HConfigCreateAtMapVal(iter, _RC)
         has_default_vert_profile = ESMF_HConfigIsDefined(field_cfg, keyString=KEY_DEFAULT_VERT_PROFILE, _RC)
         if (has_default_vert_profile) then
            default_vert_profile = ESMF_HConfigAsR4Seq(field_cfg, keyString=KEY_DEFAULT_VERT_PROFILE, _RC)
            call MAPL_GetPointer(exportState, ptr3d, trim(field_name), _RC)
            shape_ = shape(ptr3d)
            _ASSERT(shape_(3) == size(default_vert_profile), "incorrect size of vertical profile")
            do concurrent(ii = 1:shape_(1), jj=1:shape_(2))
               ptr3d(ii, jj, :) = default_vert_profile
            end do
         end if
      end do

      support%runMode = ESMF_HConfigAsString(hconfig, keyString='RUN_MODE', _RC)
      support%delay = -1.0
      is_present = ESMF_HConfigIsDefined(hconfig, keyString='delay', _RC)
      if (is_present) then
         support%delay = ESMF_HConfigAsR4(hconfig, keyString='delay', _RC)
      end if
      fill_def = ESMF_HConfigCreateAt(hconfig, keyString='FILL_DEF', _RC)
      b = ESMF_HConfigIterBegin(fill_def, _RC)
      e = ESMF_HConfigIterEnd(fill_def, _RC)
      iter = b
      do while (ESMF_HConfigIterLoop(iter, b, e))
         key = ESMF_HConfigAsStringMapKey(iter, _RC)
         keyVal = ESMF_HConfigAsStringMapVal(iter, _RC)
         call support%fillDefs%insert(key, keyVal) 
      enddo 
      call ESMF_ClockGet(clock, currTime=current_time, _RC)
      call support%tFunc%init_time(hconfig, current_time, _RC)
      
      call MAPL_GridCompGetInternalState(gridcomp, internal_state, _RC) 
      call initialize_internal_state(internal_state, support, _RC)
      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(importState)
      _UNUSED_DUMMY(clock)
   end subroutine init

   recursive subroutine run(gridcomp, importState, exportState, clock, rc)
      type(ESMF_GridComp) :: gridcomp
      type(ESMF_State) :: importState
      type(ESMF_State) :: exportState
      type(ESMF_Clock) :: clock
      integer, intent(out) :: rc

      integer :: status
      type(ESMF_State) :: internal_state
      type(Comp_Driver_Support), pointer :: support
      type(ESMF_Time) :: current_time

      _GET_NAMED_PRIVATE_STATE(gridcomp, Comp_Driver_Support, PRIVATE_STATE, support)
      call ESMF_ClockGet(clock, currTime=current_time, _RC)
      call MAPL_GridCompGetInternalState(gridcomp, internal_state, _RC) 
      call update_internal_state(internal_state, current_time, support, _RC)

      if (support%runMode == "GenerateExports") then
         call fill_state_from_internal(exportState, internal_state, support, _RC)
      else
         _FAIL("no run mode selected")
      end if
      _UNUSED_DUMMY(importState)
      _UNUSED_DUMMY(exportState)
      _UNUSED_DUMMY(clock)
      _RETURN(_SUCCESS)

   end subroutine run

   subroutine initialize_internal_state(internal_state, support, rc)
      type(ESMF_State), intent(inout) :: internal_state
      type(Comp_Driver_Support), intent(inout) :: support
      integer, optional, intent(out) :: rc

      real, pointer :: ptr_2d(:,:)
      real(ESMF_KIND_R8), pointer :: coords(:,:)
      integer :: status, seed_size, mypet
      integer, allocatable :: seeds(:)
      type(ESMF_Field) :: field
      type(ESMF_Grid) :: grid
      type(ESMF_VM) :: vm

      ! rand
      call MAPL_StateGetPointer(internal_state, ptr_2d, 'rand', _RC)
      call random_seed(size=seed_size)
      allocate(seeds(seed_size))
      call ESMF_VMGetCurrent(vm, _RC)
      call ESMF_VMGet(vm, localPet=mypet, _RC)
      seeds = mypet
      call random_seed(put=seeds)
      call random_number(ptr_2d)
      ! lons and lats
      call MAPL_StateGetPointer(internal_state, ptr_2d, 'grid_lons', _RC)
      call ESMF_StateGet(internal_state, 'grid_lons', field, _RC)
      call ESMF_FieldGet(field, grid=grid, _RC)
      call ESMF_GridGetCoord(grid, coordDim=1, localDE=0, &
                             staggerloc=ESMF_STAGGERLOC_CENTER, &
                             farrayPtr=coords, _RC)
      ptr_2d = coords
      call MAPL_StateGetPointer(internal_state, ptr_2d, 'grid_lats', _RC)
      call ESMF_GridGetCoord(grid, coordDim=2, localDE=0, &
                             staggerloc=ESMF_STAGGERLOC_CENTER, &
                             farrayPtr=coords, _RC)
      ptr_2d = coords

      _RETURN(_SUCCESS)

   end subroutine initialize_internal_state

   subroutine update_internal_state(internal_state, current_time, support, rc)
      type(ESMF_State), intent(inout) :: internal_state
      type(ESMF_Time), intent(inout) :: current_time
      type(Comp_Driver_Support), intent(inout) :: support
      integer, optional, intent(out) :: rc

      integer :: status
      real, pointer :: ptr_2d(:,:)

      call MAPL_StateGetPointer(internal_state, ptr_2d, 'time_interval', _RC)
      ptr_2d = support%tFunc%evaluate_time(current_time, _RC)

      _RETURN(_SUCCESS)

   end subroutine update_internal_state

   subroutine fill_state_from_internal(state, internal_state, support, rc)
      type(ESMF_State), intent(inout) :: state
      type(ESMF_State), intent(inout) :: internal_state
      type(Comp_Driver_Support), intent(inout) :: support
      integer, optional, intent(out) :: rc
     
      integer :: status, item_count, i
      character(len=ESMF_MAXSTR), allocatable :: name_list(:)
      type(ESMF_Field) :: field
      character(len=:), pointer :: expression
    
      call ESMF_StateGet(state, itemCount=item_count, _RC) 
      allocate(name_list(item_count), _STAT)
      call ESMF_StateGet(state, itemNameList=name_list, _RC)
      do i=1,item_count
         call ESMF_StateGet(state, trim(name_list(i)), field, _RC)
         expression => support%fillDefs%at(trim(name_list(i)))
         _ASSERT(associated(expression), "no expression for item "//trim(name_list(i)))
         call MAPL_StateEval(internal_state, expression, field, _RC)
      enddo
      
      _RETURN(_SUCCESS)

   end subroutine fill_state_from_internal 

end module Mapl3g_RootgGridComp

subroutine setServices(gridcomp, rc)
   use ESMF
   use MAPL_ErrorHandlingMod
   use mapl3g_RootgGridComp, only: Root_setServices => SetServices
   type(ESMF_GridComp)  :: gridcomp
   integer, intent(out) :: rc

   integer :: status

   call Root_setServices(gridcomp, _RC)

   _RETURN(_SUCCESS)
end subroutine setServices
