#include "MAPL.h"
module mapl_pFUnit_Initialize_mod
   implicit none(type,external)

   public :: Initialize
   public :: Initialize_strict

contains

   subroutine Initialize()
      call do_initialize()
   end subroutine Initialize

   ! Identical to Initialize(), but additionally forces the field-dictionary
   ! ValidationMode to STRICT for the remainder of this process. Intended for
   ! use as a pFUnit EXTRA_INITIALIZE hook (add_pfunit_ctest's
   ! EXTRA_USE=mapl_pFUnit_Initialize_mod, EXTRA_INITIALIZE=Initialize_strict)
   ! on a dedicated test executable whose scenarios have been verified clean
   ! under strict enforcement - see openspec change
   ! use-field-dictionary-in-scenario-tests. Every other pFUnit executable in
   ! the tree keeps using plain Initialize() and is unaffected.
   subroutine Initialize_strict()
      use esmf, only: ESMF_HConfig, ESMF_HConfigCreate
      use mapl_FieldDictionaryConfig_mod, only: FieldDictionaryConfig, set_field_dictionary_config

      type(ESMF_HConfig) :: node
      integer :: status

      call do_initialize()

      ! Same default dictionary path every pFUnit binary in
      ! superstructure/generic/tests/ already resolves to (see that
      ! directory's CMakeLists.txt) - only the mode differs here.
      node = ESMF_HConfigCreate( &
           content='{path: geos_field_dictionary.yaml, validation_mode: strict}', rc=status)
      call set_field_dictionary_config(FieldDictionaryConfig(node))
   end subroutine Initialize_strict

   subroutine do_initialize()
      use MAPL
      use esmf, only: ESMF_GridComp
      use fArgParse
      use mapl_Throw_mod, only: MAPL_set_throw_method
      use MAPL_pFUnit_ThrowMod
      use pflogger, only: pfl_initialize => initialize, WARNING, DEBUG
      use gFTL2_StringUnlimitedMap

      type(ArgParser), target :: parser
      type (StringUnlimitedMap), target :: options
      character(:), allocatable :: level_name
      class(*), pointer :: option
      type(ESMF_GridComp), allocatable :: servers(:)

      call set_command_line_options()

      option => options%at('level')
      if (associated(option)) then
         call cast(option, level_name)
      end if

      call MAPL_initialize(level_name=level_name)
      call MAPL_CreateServers(servers)

      call MAPL_set_throw_method(throw)
   contains

      subroutine set_command_line_options()
         
         parser = ArgParser()
         call parser%add_argument('--level', '-l', action='store', default='WARNING', help='set logging level')
         
#ifndef _GNU
         options = parser%parse_args()
#else
         call parser%parse_args_kludge(option_values=options)
#endif
      end subroutine set_command_line_options
 
   end subroutine do_initialize
end module mapl_pFUnit_Initialize_mod
