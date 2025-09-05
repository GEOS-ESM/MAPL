#include "MAPL.h"

module mapl3g_OuterMetaComponent
   use mapl3g_UserSetServices, only: AbstractUserSetServices
   use mapl3g_ComponentSpec
   use mapl3g_VariableSpec
   use mapl3g_ChildSpec
   use mapl3g_InnerMetaComponent
   use mapl3g_MethodPhasesMap
   use mapl3g_StateRegistry
   use mapl3g_ESMF_Interfaces, only: I_Run
   use mapl3g_GriddedComponentDriver
   use mapl3g_ComponentDriverVector
   use mapl3g_GriddedComponentDriverMap, only: GriddedComponentDriverMap
   use mapl3g_GriddedComponentDriverMap, only: operator(/=)
   use mapl3g_VerticalGrid
   use mapl3g_SimpleAlarm
   use gFTL2_StringVector
   use mapl_keywordEnforcer, only: KE => KeywordEnforcer
   use esmf
   use pflogger, only: Logger

   implicit none
   private

   public :: OuterMetaComponent
   public :: get_outer_meta
   public :: attach_outer_meta
   public :: free_outer_meta

   type :: OuterMetaComponent
      private

      type(ESMF_GridComp)                         :: self_gridcomp
      type(GriddedComponentDriver)                :: user_gc_driver
      class(AbstractUserSetServices), allocatable :: user_setservices
      type(ESMF_TimeInterval), allocatable        :: user_timeStep
      ! These are only allocated when parent overrides default timestepping.
      type(ESMF_TimeInterval)                     :: user_offset
      type(MethodPhasesMap)                       :: user_phases_map
      type(ESMF_HConfig)                          :: hconfig

      type(ESMF_Geom), allocatable                :: geom
      class(VerticalGrid), allocatable            :: vertical_grid

      type(InnerMetaComponent), allocatable       :: inner_meta

      ! Hierarchy
      type(GriddedComponentDriverMap)             :: children
      type(StateRegistry) :: registry

      class(Logger), pointer :: lgr  => null() ! "MAPL.Generic" // name

      type(ComponentSpec)                         :: component_spec

      integer :: counter

      type(SimpleAlarm) :: user_run_alarm

   contains

      procedure :: get_user_gc_driver
      procedure :: set_hconfig
      procedure :: get_hconfig
      procedure :: has_geom
      procedure :: get_geom
      procedure :: get_registry
      procedure :: get_logger
      procedure :: set_misc

      procedure :: get_phases

      ! Generic methods
      procedure :: setServices => setservices_

      procedure :: init_meta  ! object

      procedure :: run_custom
      procedure :: initialize_user
      procedure :: initialize_set_clock
      procedure :: initialize_geom_a
      procedure :: initialize_geom_b
      procedure :: initialize_advertise
      procedure :: advertise_variable
      procedure :: initialize_modify_advertised
      procedure :: initialize_realize
      procedure :: initialize_read_restart

      procedure :: run_user
      procedure :: run_clock_advance
      procedure :: finalize
      procedure :: write_restart

      ! Hierarchy
      procedure, private :: add_child_by_spec
      procedure, private :: get_child_by_name
      procedure, private :: run_child_by_name
      procedure, private :: run_children_

      generic :: add_child => add_child_by_spec
      generic :: get_child => get_child_by_name
      generic :: run_child => run_child_by_name
      generic :: run_children => run_children_

      procedure :: set_entry_point
      procedure :: set_geom
      procedure :: get_name
      procedure :: get_gridcomp

      procedure :: get_component_spec
      procedure :: get_internal_state

      procedure :: set_vertical_grid
      procedure :: get_vertical_grid

      procedure :: connect_all

   end type OuterMetaComponent

   type OuterMetaWrapper
      type(OuterMetaComponent), pointer :: outer_meta
   end type OuterMetaWrapper


   interface get_outer_meta
      module procedure :: get_outer_meta_from_outer_gc
   end interface get_outer_meta

   character(len=*), parameter :: OUTER_META_PRIVATE_STATE = "MAPL::OuterMetaComponent::private"

   abstract interface
      subroutine I_child_op(this, child_meta, rc)
         import OuterMetaComponent
         class(OuterMetaComponent), target, intent(inout) :: this
         type(OuterMetaComponent), target, intent(inout) :: child_meta
         integer, optional, intent(out) :: rc
      end subroutine I_child_Op
   end interface

   ! Submodule interfaces
   interface

      recursive module subroutine SetServices_(this, rc)
         class(OuterMetaComponent), target, intent(inout) :: this
         integer, intent(out) :: rc
      end subroutine

      module recursive subroutine add_child_by_spec(this, child_name, child_spec, rc)
         class(OuterMetaComponent), target, intent(inout) :: this
         character(*), intent(in) :: child_name
#if defined(ESMF_HCONFIGSET_HAS_INTENT_INOUT)
         type(ChildSpec), intent(inout) :: child_spec
#else
         type(ChildSpec), intent(in) :: child_spec
#endif
         integer, optional, intent(out) :: rc
      end subroutine add_child_by_spec

      module function new_outer_meta(gridcomp, user_gc_driver, user_setServices, hconfig) result(outer_meta)
         type(OuterMetaComponent) :: outer_meta
         type(ESMF_GridComp), intent(in) :: gridcomp
         type(GriddedComponentDriver), intent(in) :: user_gc_driver
         class(AbstractUserSetServices), intent(in) :: user_setservices
         type(ESMF_HConfig), intent(in) :: hconfig
      end function new_outer_meta

      module subroutine init_meta(this, rc)
         class(OuterMetaComponent), intent(inout) :: this
         integer, optional, intent(out) :: rc
      end subroutine init_meta

      module function get_child_by_name(this, child_name, rc) result(child_component)
         type(GriddedComponentDriver) :: child_component
         class(OuterMetaComponent), intent(in) :: this
         character(len=*), intent(in) :: child_name
         integer, optional, intent(out) :: rc
      end function get_child_by_name

      module recursive subroutine run_child_by_name(this, child_name, unusable, phase_name, rc)
         class(OuterMetaComponent), intent(inout) :: this
         character(len=*), intent(in) :: child_name
         class(KE), optional, intent(in) :: unusable
         character(len=*), optional, intent(in) :: phase_name
         integer, optional, intent(out) :: rc
      end subroutine run_child_by_name

      module recursive subroutine run_children_(this, unusable, phase_name, rc)
         class(OuterMetaComponent), target, intent(inout) :: this
         class(KE), optional, intent(in) :: unusable
         character(len=*), optional, intent(in) :: phase_name
         integer, optional, intent(out) :: rc
      end subroutine run_children_

      module function get_outer_meta_from_outer_gc(gridcomp, rc) result(outer_meta)
         type(OuterMetaComponent), pointer :: outer_meta
         type(ESMF_GridComp), intent(inout) :: gridcomp
         integer, optional, intent(out) :: rc
      end function get_outer_meta_from_outer_gc

      module subroutine attach_outer_meta(gridcomp, rc)
         type(ESMF_GridComp), intent(inout) :: gridcomp
         integer, optional, intent(out) :: rc
      end subroutine attach_outer_meta

      module subroutine free_outer_meta(gridcomp, rc)
         type(ESMF_GridComp), intent(inout) :: gridcomp
         integer, optional, intent(out) :: rc
      end subroutine free_outer_meta

      module function get_phases(this, method_flag) result(phases)
         type(StringVector), pointer :: phases
         class(OuterMetaComponent), target, intent(inout):: this
         type(ESMF_Method_Flag), intent(in) :: method_flag
      end function get_phases

      module subroutine set_hconfig(this, hconfig)
         class(OuterMetaComponent), intent(inout) :: this
         type(ESMF_HConfig), intent(in) :: hconfig
      end subroutine set_hconfig

      module function get_hconfig(this) result(hconfig)
         type(ESMF_Hconfig) :: hconfig
         class(OuterMetaComponent), intent(inout) :: this
      end function get_hconfig

      module function has_geom(this)
         logical :: has_geom
         class(OuterMetaComponent), intent(in) :: this
      end function has_geom

      module function get_geom(this, rc) result(geom)
         type(ESMF_Geom) :: geom
         class(OuterMetaComponent), intent(inout) :: this
         integer, intent(out), optional :: rc
      end function get_geom

      module recursive subroutine initialize_set_clock(this, outer_clock, unusable, rc)
         class(OuterMetaComponent), target, intent(inout) :: this
         type(ESMF_Clock), intent(in) :: outer_clock
         ! optional arguments
         class(KE), optional, intent(in) :: unusable
         integer, optional, intent(out) :: rc
      end subroutine initialize_set_clock

      module recursive subroutine initialize_geom_a(this, unusable, rc)
         class(OuterMetaComponent), target, intent(inout) :: this
         ! optional arguments
         class(KE), optional, intent(in) :: unusable
         integer, optional, intent(out) :: rc
      end subroutine initialize_geom_a

      module recursive subroutine initialize_geom_b(this, unusable, rc)
         class(OuterMetaComponent), target, intent(inout) :: this
         ! optional arguments
         class(KE), optional, intent(in) :: unusable
         integer, optional, intent(out) :: rc
      end subroutine initialize_geom_b

      module subroutine advertise_variable(this, var_spec, rc)
         class(OuterMetaComponent), target, intent(inout) :: this
         type(VariableSpec), intent(in) :: var_spec
         integer, optional, intent(out) :: rc
      end subroutine advertise_variable

      module recursive subroutine initialize_advertise(this, unusable, rc)
         class(OuterMetaComponent), target, intent(inout) :: this
         ! optional arguments
         class(KE), optional, intent(in) :: unusable
         integer, optional, intent(out) :: rc
      end subroutine initialize_advertise

     module recursive subroutine initialize_modify_advertised(this, importState, exportState, clock, unusable, rc)
         class(OuterMetaComponent), target, intent(inout) :: this
         ! optional arguments
         type(ESMF_State) :: importState
         type(ESMF_State) :: exportState
         type(ESMF_Clock) :: clock
         class(KE), optional, intent(in) :: unusable
         integer, optional, intent(out) :: rc
      end subroutine initialize_modify_advertised

      module recursive subroutine initialize_realize(this, importState, exportState, clock, unusable, rc)
         class(OuterMetaComponent), intent(inout) :: this
         type(ESMF_State) :: importState
         type(ESMF_State) :: exportState
         type(ESMF_Clock) :: clock
        ! optional arguments
         class(KE), optional, intent(in) :: unusable
         integer, optional, intent(out) :: rc
      end subroutine initialize_realize

      module recursive subroutine initialize_read_restart(this, unusable, rc)
         class(OuterMetaComponent), target, intent(inout) :: this
         ! optional arguments
         class(KE), optional, intent(in) :: unusable
         integer, optional, intent(out) :: rc
      end subroutine initialize_read_restart

      module recursive subroutine recurse_(this, phase_idx, rc)
         class(OuterMetaComponent), target, intent(inout) :: this
         integer :: phase_idx
         integer, optional, intent(out) :: rc
      end subroutine recurse_

      module recursive subroutine recurse_write_restart_(this, rc)
         class(OuterMetaComponent), target, intent(inout) :: this
         integer, optional, intent(out) :: rc
      end subroutine recurse_write_restart_

      module subroutine apply_to_children_custom(this, oper, rc)
         class(OuterMetaComponent), intent(inout) :: this
         procedure(I_child_op) :: oper
         integer, optional, intent(out) :: rc
      end subroutine apply_to_children_custom

      module recursive subroutine initialize_user(this, unusable, rc)
         class(OuterMetaComponent), intent(inout) :: this
         ! optional arguments
         class(KE), optional, intent(in) :: unusable
         integer, optional, intent(out) :: rc
      end subroutine initialize_user

      module subroutine run_custom(this, method_flag, phase_name, rc)
         class(OuterMetaComponent), intent(inout) :: this
         type(ESMF_METHOD_FLAG), intent(in) :: method_flag
         character(*), intent(in) :: phase_name
         integer, optional, intent(out) :: rc
      end subroutine run_custom

      module recursive subroutine run_user(this, clock, phase_name, unusable, rc)
         class(OuterMetaComponent), target, intent(inout) :: this
         type(ESMF_Clock), intent(inout) :: clock
         ! optional arguments
         character(len=*), optional, intent(in) :: phase_name
         class(KE), optional, intent(in) :: unusable
         integer, optional, intent(out) :: rc
      end subroutine run_user

      module recursive subroutine run_clock_advance(this, clock, unusable, rc)
         class(OuterMetaComponent), target, intent(inout) :: this
         type(ESMF_Clock), intent(inout) :: clock
         ! optional arguments
         class(KE), optional, intent(in) :: unusable
         integer, optional, intent(out) :: rc
      end subroutine run_clock_advance

      module recursive subroutine finalize(this, importState, exportState, clock, unusable, rc)
         class(OuterMetaComponent), intent(inout) :: this
         type(ESMF_State) :: importState
         type(ESMF_State) :: exportState
         type(ESMF_Clock) :: clock
         ! optional arguments
         class(KE), optional, intent(in) :: unusable
         integer, optional, intent(out) :: rc
      end subroutine finalize

      module recursive subroutine write_restart(this, importState, exportState, clock, unusable, rc)
         class(OuterMetaComponent), target, intent(inout) :: this
         type(ESMF_State) :: importState
         type(ESMF_State) :: exportState
         type(ESMF_Clock) :: clock
         ! optional arguments
         class(KE), optional, intent(in) :: unusable
         integer, optional, intent(out) :: rc
      end subroutine write_restart

      module function get_name(this, rc) result(name)
         character(:), allocatable :: name
         class(OuterMetaComponent), intent(in) :: this
         integer, optional, intent(out) :: rc
      end function get_name

      module function get_gridcomp(this) result(gridcomp)
         type(ESMF_GridComp) :: gridcomp
         class(OuterMetaComponent), intent(in) :: this
      end function get_gridcomp

      module subroutine set_geom(this, geom)
         class(OuterMetaComponent), intent(inout) :: this
         type(ESMF_Geom), intent(in) :: geom
      end subroutine set_geom

      module subroutine set_vertical_grid(this, vertical_grid)
         class(OuterMetaComponent), intent(inout) :: this
         class(VerticalGrid), intent(in) :: verticaL_grid
      end subroutine set_vertical_grid

      module function get_vertical_grid(this) result(vertical_grid)
         class(VerticalGrid), allocatable :: verticaL_grid
         class(OuterMetaComponent), intent(inout) :: this
      end function get_vertical_grid

      module function get_registry(this) result(registry)
         type(StateRegistry), pointer :: registry
         class(OuterMetaComponent), target, intent(in) :: this
      end function get_registry

      module function get_component_spec(this) result(component_spec)
         type(ComponentSpec), pointer :: component_spec
         class(OuterMetaComponent), target, intent(in) :: this
      end function get_component_spec

      module function get_internal_state(this) result(internal_state)
         type(ESMF_State) :: internal_state
         class(OuterMetaComponent), intent(in) :: this
      end function get_internal_state

      module function get_logger(this) result(lgr)
         class(Logger), pointer :: lgr
         class(OuterMetaComponent), target, intent(in) :: this
      end function get_logger

      module function get_user_gc_driver(this) result(user_gc_driver)
         type(GriddedComponentDriver), pointer :: user_gc_driver
         class(OuterMetaComponent), target, intent(in) :: this
      end function get_user_gc_driver

      module subroutine connect_all(this, src_comp, dst_comp, rc)
         class(OuterMetaComponent), intent(inout) :: this
         character(*), intent(in) :: src_comp
         character(*), intent(in) :: dst_comp
         integer, optional, intent(out) :: rc
      end subroutine connect_all

      module subroutine set_entry_point(this, method_flag, userProcedure, unusable, phase_name, rc)
         class(OuterMetaComponent), intent(inout) :: this
         type(ESMF_Method_Flag), intent(in) :: method_flag
         procedure(I_Run) :: userProcedure
         class(KE), optional, intent(in) :: unusable
         character(len=*), optional, intent(in) :: phase_name
         integer, optional, intent(out) ::rc
      end subroutine set_entry_point

      ! Currently resides in write_restart_smod
      module function get_checkpoint_subdir(hconfig, currTime, rc) result(subdir)
         character(:), allocatable :: subdir
         type(esmf_HConfig), intent(in) :: hconfig
         type(esmf_Time), intent(in) :: currTime
         integer, optional, intent(out) :: rc
      end function get_checkpoint_subdir

   end interface

   interface OuterMetaComponent
      module procedure new_outer_meta
   end interface OuterMetaComponent


   interface recurse
      module procedure recurse_
   end interface recurse

   interface recurse_write_restart
      module procedure recurse_write_restart_
   end interface recurse_write_restart

   interface apply_to_children
      module procedure apply_to_children_custom
   end interface apply_to_children

   integer, save :: counter = 0

   character(*), parameter :: RUN_USER_ALARM = 'run_user'

contains

   subroutine set_misc(this, unusable, activate_all_exports, activate_all_imports, checkpoint_controls, restart_controls)
      class(OuterMetaComponent), intent(inout) :: this
      class(KE), optional, intent(in) :: unusable
      logical, optional, intent(in) :: activate_all_exports
      logical, optional, intent(in) :: activate_all_imports
      type(CheckpointControls), optional, intent(in) :: checkpoint_controls
      type(CheckpointControls), optional, intent(in) :: restart_controls

      if (present(activate_all_exports)) then
         this%component_spec%misc%activate_all_exports = activate_all_exports
      end if
      if (present(activate_all_imports)) then
         this%component_spec%misc%activate_all_imports = activate_all_imports
      end if
      if (present(checkpoint_controls)) then
         this%component_spec%misc%checkpoint_controls = checkpoint_controls
      end if
      if (present(restart_controls)) then
         this%component_spec%misc%restart_controls = restart_controls
      end if

   end subroutine set_misc

end module mapl3g_OuterMetaComponent
