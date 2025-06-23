#include "MAPL_Generic.h"
module MAPL_pFUnit_Initialize
   implicit none(type,external)

contains
   subroutine Initialize()
      use mapl3
      use fArgParse
      use MAPL_ThrowMod, only: MAPL_set_throw_method
      use MAPL_pFUnit_ThrowMod
      use pflogger, only: pfl_initialize => initialize, WARNING, DEBUG
      use gFTL2_StringUnlimitedMap

      type(ArgParser), target :: parser
      type (StringUnlimitedMap), target :: options
      character(:), allocatable :: level_name
      class(*), pointer :: option

      call MAPL_set_throw_method(throw)

      call set_command_line_options()

      option => options%at('level')
      if (associated(option)) then
         call cast(option, level_name)
      end if

      call MAPL_initialize(level_name=level_name)

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
 
   end subroutine Initialize
end module MAPL_pFUnit_Initialize
