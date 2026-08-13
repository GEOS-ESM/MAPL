#include "MAPL.h"

submodule (mapl_ComponentSpecParser_mod) parse_child_smod

   implicit none(type,external)
contains

   module function parse_child(hconfig, rc) result(child)
      type(ChildSpec) :: child
      type(ESMF_HConfig), intent(in) :: hconfig
      integer, optional, intent(out) :: rc

      integer :: status
      class(AbstractUserSetServices), allocatable :: setservices

      logical :: has_key
      type(ESMF_HConfig), allocatable :: child_hconfig, setservices_hconfig
      character(:), allocatable :: sharedObj, userProcedure, config_file
      type(ESMF_TimeInterval), allocatable :: offset
      type(ESMF_TimeInterval), allocatable :: timeStep
      character(len=*), parameter :: CONFIG_FILE_KEY = 'config_file'

      has_key = ESMF_HconfigIsDefined(hconfig, keyString=CONFIG_FILE_KEY, _RC)
      _ASSERT(has_key, CONFIG_FILE_KEY // ' was not found.')
      config_file = ESMF_HconfigAsString(hconfig, keyString=CONFIG_FILE_KEY, _RC)
      child_hconfig = ESMF_HConfigCreate(config_file, _RC)
      has_key = ESMF_HconfigIsDefined(child_hconfig, keyString=COMPONENT_SETSERVICES_SECTION, _RC)
      _ASSERT(has_key, COMPONENT_SETSERVICES_SECTION // ' was not found.')
      setservices_hconfig = ESMF_CreateAt(child_hconfig, keyString=COMPONENT_SETSERVICES_SECTION, _RC)
      setservices = parse_setservices(setservices_hconfig, _RC)
      call parse_timespec(hconfig, timeStep, offset, _RC)

      child = ChildSpec(setservices, hconfig=child_hconfig, timeStep=timeStep, offset=offset)

      call ESMF_HConfigDestroy(setservices_hconfig, _RC)
      call ESMF_HConfigDestroy(child_hconfig, _RC)
      
      _RETURN(_SUCCESS)

   end function parse_child

end submodule parse_child_smod
