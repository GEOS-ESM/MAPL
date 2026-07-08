#include "MAPL.h"
module mapl_pFUnit_Initialize_NoLeap_mod
   implicit none(type,external)

contains
   subroutine Initialize()
      use MAPL
      use esmf, only: ESMF_GridComp, ESMF_CalendarSetDefault, ESMF_CALKIND_NOLEAP
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
      integer :: status
      integer :: rc

      call set_command_line_options()

      option => options%at('level')
      if (associated(option)) then
         call cast(option, level_name)
      end if

      call MAPL_initialize(level_name=level_name)
      call ESMF_CalendarSetDefault(ESMF_CALKIND_NOLEAP, _RC)
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
 
   end subroutine Initialize
end module mapl_pFUnit_Initialize_NoLeap_mod
