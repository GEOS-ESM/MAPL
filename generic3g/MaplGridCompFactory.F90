#include "MAPL_ErrLog.h"

module mapl3g_GridCompFactory
   use esmf
   use mapl3g_UserSetServices, only: AbstractUserSetServices
   use mapl3g_UserSetServices, only: UserSetServices
   implicit none
   private

   public :: make_MAPL_GridComp
   public :: free_MAPL_GridComp

   ! The following are implementend in Fortran submodules.
   interface
      
      module recursive subroutine setServices(gridcomp, rc)
         type(ESMF_GridComp) :: gridcomp
         integer, intent(out) :: rc
      end subroutine setServices

      module recursive subroutine initialize(gridcomp, importState, exportState, clock, rc)
         type(ESMF_GridComp)         :: gridcomp   
         type(ESMF_State)            :: importState
         type(ESMF_State)            :: exportState
         type(ESMF_Clock)            :: clock      
         integer, intent(out)        :: rc

         outer_meta => ...
         call outer_meta%initialize()
      end subroutine initialize

      module recursive subroutine run(gridcomp, importState, exportState, clock, rc)
         type(ESMF_GridComp)         :: gridcomp   
         type(ESMF_State)            :: importState
         type(ESMF_State)            :: exportState
         type(ESMF_Clock)            :: clock      
         integer, intent(out)        :: rc         
      end subroutine run
      
      module recursive subroutine finalize(gridcomp, importState, exportState, clock, rc)
         type(ESMF_GridComp)         :: gridcomp   
         type(ESMF_State)            :: importState
         type(ESMF_State)            :: exportState
         type(ESMF_Clock)            :: clock      
         integer, intent(out)        :: rc         
      end subroutine finalize

   end interface

   ! Factory method
   interface make_MAPL_GridComp
      module procedure make_gc_traditional
      module procedure make_gc_advanced
!!$      module procedure make_gc_hybrid  ! might not be needed
   end interface make_MAPL_GridComp


   !-----------
   ! Allow use of two distinct types of config
   ! TODO:  Do we even need to have esmf_config at this level?
   !        Probably not, but need to send it to internal meta.
   !        Maybe just through GC?
   !-----------
   ! Maybe MAPL_Resource?
   type :: MAPL_Configuration
      type(ESMF_Config), allocatable :: esmf_cfg
      type(Configuration), allocatable :: yaml_config
   end type MAPL_Configuration
      

   type :: ChildGridComp
      type(ESMF_GridComp) :: gc
      type(ESMF_State) :: import_state
      type(ESMF_State) :: export_state
      type(ESMF_State) :: internal_state
   end type ChildGridComp


!!$   type :: OuterMetaPrivateState  ! outer_meta
   type :: PrivateState
      private
      type(ESMF_GridComp)                         :: self_gc
      type(ESMF_GridComp)                         :: user_gc
      type(MAPL_Configuration)                    :: config
      class(AbstractUserSetServices), allocatable :: user_setservices
      type(ComponentSpec)                         :: component_spec
      type(PrivateState), pointer                 :: parent_private_state
      type(MAPL_MetaComp), allocatable            :: inner_meta

      type(ChildComponentMap) :: children

   contains
      procedure :: set_esmf_config
      procedure :: set_yaml_config
      generic   :: set_config => set_esmf_config, set_yaml_config
!!$      procedure :: initialize
!!$      procedure :: run
!!$      procedure :: finalize
!!$      procedure :: setservices

      procedure :: add_child
      procedure :: get_child_by_name
      procedure :: get_child_by_index
   end type PrivateState

   type PrivateStateWrapper
      type(PrivateState), pointer :: wrapper
   end type PrivateStateWrapper

   character(len=*), parameter :: MAPL_GRIDCOMP_PRIVATE_STATE = 'MAPL outer gridcomp private state'

contains


   ! Traditional gridcomp - user specified setservices procedure and an ESMF Config.
   recursive function make_gc_traditional(name, user_setservices, unusable, config, petlist, rc) result(gc)
      type(ESMF_GridComp) :: gc
      character(len=*), intent(in) :: name
      procedure(I_SetServices) :: user_setservices
      type(ESMF_config), intent(inout) :: config
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status
      
      gc = make_basic_gridcomp(name=name, _RC)

      outer_meta => get_private_state(gc, _RC)
      outer_meta%config%esmf_cfg
!!$      call outer_meta%set_config(config, _RC)
      outer_meta%user_setservices = UserSetServices(user_setservices)

      _RETURN(ESMF_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end function make_gc_traditional
   

   ! Advanced - all metadata specified through a YAML config file.
   ! SetServices is found from a DSO described in the config file.
   recursive function make_gc_advanced(name, config, unusable, rc) result(gc)
      use yaFyaml, only: Configuration
      type(ESMF_GridComp) :: gc
      character(len=*), intent(in) :: name
      type(Configuration), intent(inout) :: config
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status

      gc = make_basic_gridcomp(name=name, _RC)

      outer_meta => get_private_state(gc, _RC)
      outer_meta%yaml_config = config

      _RETURN(ESMF_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end function make_gc_advanced


   ! Create ESMF GridComp, attach an internal state for meta, and a config.
   function make_basic_gridcomp(name, unusable, rc) relult(gc)
      character(len=*), intent(in) :: name
      class(KeywordEnforcer), optional, intent(in) :: unusable
      type(ESMF_Config), optional, intent(in) :: config
      integer, optional, intent(out) :: rc

      integer :: status

      gc = ESMF_GridCompCreate(name=name, _RC)
      call attach_private_state(gc, _RC)
      
      _RETURN(ESMF_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end function make_basic_gridcomp

   subroutine attach_private_state(gc, unusable, _RC)
      type(ESMF_GridComp), intent(inout) :: gc
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      integer :: status
      type(PrivateStateWrapper) :: wrapper
      type(PrivateState), pointer :: this
!!$      character(len=ESMF_MAXSTR) :: comp_name

      allocate(wrapper%private_state) 
      call ESMF_UserCompSetInternalState(gc, MAPL_GRIDCOMP_PRIVATE_STATE, wrapper, status); _VERIFY(status)

      this => wrapper%private_state
      this%self_gridcomp = gc
!!$      allocate(this%meta)
!!$      call ESMF_GridCompGet(gc, name=comp_name, _RC)
!!$      call meta%initialize(comp_name, _RC)

      _RETURN(ESMF_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine attach_private_state


!!$   ! Create a new MetaComp object and initialize it.
!!$   subroutine set_esmf_config(this, config, rc)
!!$      class(PrivateState), intent(inout) :: this
!!$      type(ESMF_Config), intent(in) :: config
!!$      integer, optional, intent(out) :: rc
!!$
!!$      integer :: status
!!$      type(MetaComp), pointer :: meta
!!$      
!!$      this%config%esmf_config = config
!!$      call ESMF_GridCompSet(this%self_gc, config=config, _RC)
!!$
!!$      _RETURN(ESMF_SUCCESS)
!!$   end subroutine set_config_esmf
   
!!$   subroutine set_config_yaml(this, config, rc)
!!$      class(PrivateState), intent(inout) :: this
!!$      type(Configuration), intent(in) :: config
!!$      integer, optional, intent(out) :: rc
!!$
!!$      integer :: status
!!$      
!!$      call this%config%yaml_config=config
!!$
!!$      _RETURN(ESMF_SUCCESS)
!!$   end subroutine set_config_yaml

   
   function get_private_state(gc, rc) result(outer_meta)
      type(PrivateState), pointer :: outer_meta
      type(ESMF_GridComp), intent(inout) :: gc
      integer, optional, intent(out) :: rc

      type(PrivateStateWrapper) :: wrapper

      call ESMF_UserCompGetInternalState(gc, MAPL_GRIDCOMP_PRIVATE_STATE, wrapper, status); _VERIFY(status)
      outer_meta => wrapper%private_state

      _RETURN(ESMF_SUCCESS)
   end function get_private_state


   ! Restore memory from the internal state.
   subroutine free_MAPL_gridcomp(gc, rc)
      type(ESMF_GridComp), intent(inout) :: gc
      integer, optional, intent(out) :: rc

      integer :: status
      type(PrivateState), pointer :: outer_meta

      outer_meta => get_private_state(gc, _RC)
      deallocate(outer_meta)
      call ESMF_GridCompDestroy(gc, _RC)
      
      _RETURN(ESMF_SUCCESS)
   end subroutine free_MAPL_gridcomp


   subroutine add_child(this, name, child, rc)
      class(PrivateState), intent(inout) :: this
      character(len=*), intent(in) :: name
      type(ESMF_GridComp), intent(in) :: child
      integer, optional, intent(ut) :: rc

      type(GridComp) :: child

      child = make_MAPL_GridComp(...)
      call this%children%insert(name, child)

      child_outer_meta => get_outer_meta(child, _RC)
      call child_outer_meta%set_parent(this)
      
   end subroutine add_child

end module mapl3g_GridCompFactory
