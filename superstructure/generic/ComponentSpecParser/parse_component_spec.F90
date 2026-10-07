#include "MAPL.h"

submodule (mapl_ComponentSpecParser_mod) parse_component_spec_smod
   implicit none(type,external)

contains

    module function parse_component_spec(hconfig, registry, component_name, rc) result(spec)
       type(ComponentSpec) :: spec
       type(ESMF_HConfig), target, intent(inout) :: hconfig
       type(StateRegistry), target, intent(in) :: registry
       character(*), intent(in) :: component_name
       integer, optional, intent(out) :: rc

       integer :: status
       logical :: has_mapl_section
       logical :: has_setservices_section
       type(ESMF_HConfig) :: mapl_cfg
       type(ESMF_HConfig) :: setservices_cfg

       write(*,*) '========================================='
       write(*,*) 'parse_component_spec for: ', trim(component_name)
       write(*,*) '========================================='
       write(*,*) 'Dumping input hconfig:'
       call ESMF_HConfigLog(hconfig)
       
       has_mapl_section = ESMF_HConfigIsDefined(hconfig, keyString=MAPL_SECTION, _RC)
       write(*,*) 'has_mapl_section = ', has_mapl_section
       if (.not. has_mapl_section) then
          write(*,*) 'WARNING: No mapl section found in hconfig'
          write(*,*) 'This may be OK for non-MAPL components'
       endif
       _RETURN_UNLESS(has_mapl_section)
       mapl_cfg = ESMF_HConfigCreateAt(hconfig, keyString=MAPL_SECTION, _RC)
       write(*,*) 'Successfully created mapl_cfg'
       write(*,*) 'Dumping mapl_cfg:'
       call ESMF_HConfigLog(mapl_cfg)
       write(*,*) 'Is mapl_cfg a map? ', ESMF_HConfigIsMap(mapl_cfg)
       if (ESMF_HConfigIsMap(mapl_cfg)) then
          write(*,*) 'mapl_cfg keys:'
          ! Try to iterate through mapl_cfg to see what keys it has
       endif
       
       spec%geometry_spec = parse_geometry_spec(mapl_cfg, registry, component_name, _RC)
       spec%var_specs = parse_var_specs(mapl_cfg, registry, component_name, _RC)
       spec%connections = parse_connections(mapl_cfg, _RC)
       spec%children = parse_children(mapl_cfg, _RC)

       has_setservices_section = ESMF_HConfigIsDefined(mapl_cfg, keyString=COMPONENT_SETSERVICES_SECTION, _RC)
       if (has_setservices_section) then
          setservices_cfg = ESMF_HConfigCreateAt(mapl_cfg, keyString=COMPONENT_SETSERVICES_SECTION, _RC)
          spec%setservices = parse_setservices(setservices_cfg, _RC)
          call ESMF_HConfigDestroy(setservices_cfg, _RC)
       end if

       write(*,*) 'About to call parse_misc for component: ', trim(component_name)
       spec%misc = parse_misc(mapl_cfg, _RC)
       write(*,*) 'Returned from parse_misc'

      call ESMF_HConfigDestroy(mapl_cfg, _RC)

      _RETURN(_SUCCESS)
   end function parse_component_spec

   ! TODO - we may want a `misc` section in the mapl section, but
   ! should wait to see what else goes there.  Or maybe a `test`
   ! section?
   
      function parse_misc(hconfig, rc) result(misc)
         use mapl_OpenMP_Support_mod, only: get_num_threads
         type(MiscellaneousComponentSpec) :: misc
        type(ESMF_HConfig), intent(in) :: hconfig
         integer, optional, intent(out) :: rc

         integer :: status
         logical :: has_misc_section
         logical :: has_num_threads
         type(ESMF_HConfig) :: misc_cfg
         type(ESMF_HConfigIter) :: iter_misc_begin, iter_misc_end, iter_misc
         integer :: iter_status
         character(:), allocatable :: misc_key

         write(*,*) 'DEBUG parse_misc: Dumping hconfig content:'
         call ESMF_HConfigLog(hconfig)
         
         has_misc_section = ESMF_HConfigIsDefined(hconfig, keyString=COMPONENT_MISC_SECTION, _RC)
         if (.not. has_misc_section) then
            write(*,*) 'WARNING: parse_misc - misc section NOT found. Returning with defaults.'
            write(*,*) '  Expected to find key: ', trim(COMPONENT_MISC_SECTION)
         endif
        _RETURN_UNLESS(has_misc_section)
        misc_cfg = ESMF_HConfigCreateAt(hconfig, keyString=COMPONENT_MISC_SECTION, _RC)
        write(*,*) 'DEBUG parse_misc: Extracted misc section, dumping misc_cfg:'
        call ESMF_HConfigLog(misc_cfg)
        write(*,*) 'Is misc_cfg a map? ', ESMF_HConfigIsMap(misc_cfg)
        if (ESMF_HConfigIsMap(misc_cfg)) then
           write(*,*) 'Iterating through misc_cfg keys to debug...'
           iter_misc_begin = ESMF_HConfigIterBegin(misc_cfg, _RC)
           iter_misc_end = ESMF_HConfigIterEnd(misc_cfg, _RC)
           iter_misc = iter_misc_begin
           do while (ESMF_HConfigIterLoop(iter_misc, iter_misc_begin, iter_misc_end, rc=iter_status))
              if (iter_status /= ESMF_SUCCESS) exit
              misc_key = ESMF_HConfigAsStringMapKey(iter_misc, _RC)
              write(*,*) '  Found key in misc_cfg: ', trim(misc_key)
           end do
        endif

        call parse_item(misc_cfg, key=COMPONENT_ACTIVATE_ALL_EXPORTS, value=misc%activate_all_exports, _RC)
       call parse_item(misc_cfg, key=COMPONENT_ACTIVATE_ALL_IMPORTS, value=misc%activate_all_imports, _RC)
       call parse_item(misc_cfg, key=COMPONENT_USE_THREADS, value=misc%use_threads, _RC)

      ! An explicit number of threads always wins.  Otherwise a component
      ! that requests threading uses all threads available to the process.
      has_num_threads = ESMF_HConfigIsDefined(misc_cfg, keyString=COMPONENT_NUM_THREADS, _RC)
      if (has_num_threads) then
         misc%num_threads = ESMF_HConfigAsI4(misc_cfg, keyString=COMPONENT_NUM_THREADS, _RC)
         _ASSERT(misc%num_threads >= 1, 'num_threads must be at least 1')
      else if (misc%use_threads) then
         misc%num_threads = get_num_threads()
      end if

      misc%checkpoint_controls = parse_checkpoint_controls(misc_cfg, key=COMPONENT_CHECKPOINT, _RC)
      misc%restart_controls = parse_checkpoint_controls(misc_cfg, key=COMPONENT_RESTART, _RC)

      _RETURN(_SUCCESS)
   end function parse_misc


   function parse_checkpoint_controls(hconfig, key, rc) result(controls)
      type(CheckpointControls) :: controls
      type(ESMF_HConfig), intent(in) :: hconfig
      character(*), intent(in) :: key
      integer, optional, intent(out) :: rc
      
      integer :: status
      logical :: has_controls_section
      type(ESMF_HConfig) :: controls_cfg
      logical :: temp_value

      has_controls_section = ESMF_HConfigIsDefined(hconfig, keyString=key, _RC)
      _RETURN_UNLESS(has_controls_section)
      controls_cfg = ESMF_HConfigCreateAt(hconfig, keyString=key, _RC)

      temp_value = .false.
      call parse_item(controls_cfg, key=KEY_IMPORT, value=temp_value, _RC)
      call controls%set_import(temp_value)
      
      temp_value = .false.
      call parse_item(controls_cfg, key=KEY_INTERNAL, value=temp_value, _RC)
      call controls%set_internal(temp_value)
      
      temp_value = .false.
      call parse_item(controls_cfg, key=KEY_BOOTSTRAP, value=temp_value, _RC)
      call controls%set_bootstrap(temp_value)

      ! We allow checkpointing of exports for testing, but restarting
      ! from exports is nonsensical.
      _RETURN_IF (key == COMPONENT_RESTART)
      temp_value = .false.
      call parse_item(controls_cfg, key=KEY_EXpORT, value=temp_value, _RC)
      call controls%set_export(temp_value)

      _RETURN(_SUCCESS)
   end function parse_checkpoint_controls

    subroutine parse_item(hconfig, key, value, rc)
       type(ESMF_HConfig), intent(in) :: hconfig
       character(*), intent(in) :: key
       logical, intent(inout) :: value
       integer, optional, intent(out) :: rc

       integer :: status
       logical :: has_key

       has_key = ESMF_HConfigIsDefined(hconfig,keyString=key, _RC)
       if (.not. has_key) then
          write(*,*) 'WARNING: parse_item - key NOT found: ', trim(key)
       endif
       _RETURN_UNLESS(has_key)
       value = ESMF_HConfigAsLogical(hconfig, keyString=key, _RC)
       write(*,*) 'parse_item: key=', trim(key), ', value=', value
       
       _RETURN(_SUCCESS)
    end subroutine parse_item

end submodule parse_component_spec_smod
