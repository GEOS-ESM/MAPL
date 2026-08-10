#include "MAPL.h"

submodule (mapl_ComponentSpecParser_mod) parse_child_smod

   implicit none(type,external)
contains

   module function parse_child(hconfig, rc) result(child)
      type(ChildSpec) :: child
      type(ESMF_HConfig), intent(in) :: hconfig
      integer, optional, intent(out) :: rc

      integer :: status
      logical :: has_config_file
      type(ESMF_HConfig), allocatable :: child_hconfig
      character(:), allocatable :: config_file
      type(ESMF_TimeInterval), allocatable :: offset
      type(ESMF_TimeInterval), allocatable :: timeStep
      character(len=*), parameter :: CONFIG_FILE_KEY = 'config_file'

      has_config_file = ESMF_HconfigIsDefined(hconfig, keyString=CONFIG_FILE_KEY, _RC)
      _ASSERT(has_config_file, 'No "config_file" key found for child.')

      if (has_config_file) then
         config_file = ESMF_HconfigAsString(hconfig, keyString=CONFIG_FILE_KEY,_RC)
         child_hconfig = ESMF_HConfigCreate(filename=config_file,_RC)
      end if

      call parse_timespec(hconfig, timeStep, offset, _RC)

      child = ChildSpec(hconfig=child_hconfig, timeStep=timeStep, offset=offset)

      _RETURN(_SUCCESS)
   end function parse_child

end submodule parse_child_smod
