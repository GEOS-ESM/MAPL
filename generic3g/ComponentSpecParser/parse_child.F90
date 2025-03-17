#include "MAPL_ErrLog.h"

submodule (mapl3g_ComponentSpecParser) parse_child_smod

   implicit none(type,external)
contains

   module function parse_child(hconfig, rc) result(child)
      type(ChildSpec) :: child
      type(ESMF_HConfig), intent(in) :: hconfig
      integer, optional, intent(out) :: rc

      integer :: status
      class(AbstractUserSetServices), allocatable :: setservices

      character(*), parameter :: dso_keys(*) = [character(len=9) :: 'dso', 'DSO', 'sharedObj', 'sharedobj']
      character(*), parameter :: userProcedure_keys(*) = [character(len=10) :: 'SetServices', 'setServices', 'setservices']
      integer :: i
      character(:), allocatable :: dso_key, userProcedure_key, try_key
      logical :: dso_found, userProcedure_found
      logical :: has_key
      logical :: has_config_file
      type(ESMF_HConfig), allocatable :: child_hconfig
      character(:), allocatable :: sharedObj, userProcedure, config_file
      type(ESMF_TimeInterval), allocatable :: offset
      type(ESMF_TimeInterval), allocatable :: timeStep

      dso_found = .false.
      ! Ensure precisely one name is used for dso
      do i = 1, size(dso_keys)
         try_key = trim(dso_keys(i))
         has_key = ESMF_HconfigIsDefined(hconfig, keyString=try_key, _RC)
         if (has_key) then
            _ASSERT(.not. dso_found, 'multiple specifications for dso in hconfig for child')
            dso_found = .true.
            dso_key = try_key
         end if
      end do
      _ASSERT(dso_found, 'Must specify a dso for hconfig of child')
      sharedObj = ESMF_HconfigAsString(hconfig, keyString=dso_key, _RC)

      userProcedure_found = .false.
      do i = 1, size(userProcedure_keys)
         try_key = userProcedure_keys(i)
         if (ESMF_HconfigIsDefined(hconfig, keyString=try_key)) then
            _ASSERT(.not. userProcedure_found, 'multiple specifications for dso in hconfig for child')
            userProcedure_found = .true.
            userProcedure_key = try_key
         end if
      end do
      userProcedure = 'setservices_'         
      if (userProcedure_found) then
         userProcedure = ESMF_HconfigAsString(hconfig, keyString=userProcedure_key,_RC)
      end if

      has_config_file = ESMF_HconfigIsDefined(hconfig, keyString='config_file', _RC)
      if (has_config_file) then
         config_file = ESMF_HconfigAsString(hconfig, keyString='config_file',_RC)
         child_hconfig = ESMF_HConfigCreate(filename=config_file,_RC)
      end if

      setservices = user_setservices(sharedObj, userProcedure)

      call parse_timespec(hconfig, timeStep, offset, _RC)

      child = ChildSpec(setservices, hconfig=child_hconfig, timeStep=timeStep, offset=offset)


      _RETURN(_SUCCESS)
   end function parse_child

end submodule parse_child_smod
