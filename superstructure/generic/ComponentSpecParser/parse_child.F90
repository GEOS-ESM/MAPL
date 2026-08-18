#include "MAPL.h"

submodule (mapl_ComponentSpecParser_mod) parse_child_smod

   implicit none(type,external)
contains

   module function parse_child(hconfig, rc) result(child)
      type(ChildSpec) :: child
      type(ESMF_HConfig), intent(in) :: hconfig
      integer, optional, intent(out) :: rc

      integer :: status

      character(*), parameter :: dso_keys(*) = [character(len=9) :: 'dso', 'DSO', 'sharedObj', 'sharedobj']
      character(*), parameter :: userProcedure_keys(*) = [character(len=11) :: 'SetServices', 'setServices', 'setservices']
      integer :: i
      character(:), allocatable :: try_key
      logical :: has_deprecated_dso_key, has_deprecated_routine_key
      logical :: has_key
      logical :: has_config_file
      type(ESMF_HConfig) :: child_hconfig
      character(:), allocatable :: config_file
      type(ESMF_TimeInterval), allocatable :: offset
      type(ESMF_TimeInterval), allocatable :: timeStep

      has_deprecated_dso_key = .false.
      do i = 1, size(dso_keys)
         try_key = trim(dso_keys(i))
         has_key = ESMF_HconfigIsDefined(hconfig, keyString=try_key, _RC)
          if (has_key) then
             has_deprecated_dso_key = .true.
             exit
          end if
       end do

      has_deprecated_routine_key = .false.
      do i = 1, size(userProcedure_keys)
         try_key = userProcedure_keys(i)
          if (ESMF_HconfigIsDefined(hconfig, keyString=try_key)) then
             has_deprecated_routine_key = .true.
             exit
          end if
       end do

      _ASSERT(.not. has_deprecated_dso_key .and. .not. has_deprecated_routine_key, 'Child DSO metadata must be declared in child config mapl.setServices, not parent mapl.children entry')

      has_config_file = ESMF_HconfigIsDefined(hconfig, keyString='config_file', _RC)
      _ASSERT(has_config_file, 'Child config entry must specify config_file')
      config_file = ESMF_HconfigAsString(hconfig, keyString='config_file',_RC)
      child_hconfig = ESMF_HConfigCreate(filename=config_file,_RC)

      call parse_timespec(hconfig, timeStep, offset, _RC)

      child = ChildSpec(hconfig=child_hconfig, timeStep=timeStep, offset=offset)


      _RETURN(_SUCCESS)
   end function parse_child

end submodule parse_child_smod
