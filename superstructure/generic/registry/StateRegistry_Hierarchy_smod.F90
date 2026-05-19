#include "MAPL.h"

! Hierarchy procedures for StateRegistry:
! - add_subregistry: Add a child registry
! - has_subregistry: Check if subregistry exists
! - get_subregistry_by_name: Retrieve subregistry by name
! - get_subregistry_by_conn_pt: Retrieve subregistry by connection point

submodule (mapl3g_StateRegistry) StateRegistry_Hierarchy_smod
   implicit none(type,external)

contains

   module subroutine add_subregistry(this, subregistry, rc)
      class(StateRegistry), target, intent(inout) :: this
      class(StateRegistry), target, intent(in) :: subregistry
      integer, optional, intent(out) :: rc

      character(:), allocatable :: name
      type(RegistryPtr) :: wrap

      name = subregistry%get_name()
      _ASSERT(.not. this%has_subregistry(name), 'Duplicate subregistry entry.')
      wrap%registry => subregistry
      call this%subregistries%insert(name, wrap)

      _RETURN(_SUCCESS)
   end subroutine add_subregistry

   module function has_subregistry(this, name) result(has_sub)
      logical :: has_sub
      class(StateRegistry), intent(in) :: this
      character(len=*), intent(in) :: name
      has_sub = (this%subregistries%count(name) > 0)
   end function has_subregistry

   module function get_subregistry_by_name(this, name, rc) result(subregistry)
      type(StateRegistry), pointer :: subregistry
      class(StateRegistry), target, intent(in) :: this
      character(len=*), intent(in) :: name
      integer, optional, intent(out) :: rc

      type(RegistryPtr), pointer :: wrap
      integer :: status

      subregistry => null()
      if (name == this%get_name() .or. name == SELF) then
         subregistry => this
         _RETURN(_SUCCESS)
      end if

      wrap => this%subregistries%at(name, _RC)
      _ASSERT(associated(wrap%registry), 'null pointer encountered for subregistry.')

      select type (q => wrap%registry)
      type is (StateRegistry)
         subregistry => q
         _RETURN(_SUCCESS)
      class default
         _FAIL('Illegal subtype of AbstractRegistry encountered.')
      end select

      _RETURN(_SUCCESS)
   end function get_subregistry_by_name

   module function get_subregistry_by_conn_pt(this, conn_pt, rc) result(subregistry)
      type(StateRegistry), pointer :: subregistry
      class(StateRegistry), target, intent(in) :: this
      type(ConnectionPt), intent(in) :: conn_pt
      integer, optional, intent(out) :: rc

      integer :: status

      subregistry => this%get_subregistry(conn_pt%component_name, _RC)

      _RETURN(_SUCCESS)
   end function get_subregistry_by_conn_pt

end submodule StateRegistry_Hierarchy_smod
