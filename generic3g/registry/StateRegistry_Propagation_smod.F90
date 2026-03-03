#include "MAPL.h"

! Propagation procedures for StateRegistry:
! - propagate_unsatisfied_imports_all: Propagate imports from all subregistries
! - propagate_unsatisfied_imports_subregistry: Propagate imports from one subregistry
! - propagate_unsatisfied_imports_virtual_pt: Propagate imports for one virtual point
! - propagate_exports_all: Propagate exports from all subregistries
! - propagate_exports_subregistry: Propagate exports from one subregistry
! - propagate_exports_virtual_pt: Propagate exports for one virtual point

submodule (mapl3g_StateRegistry) StateRegistry_Propagation_smod
   use mapl3g_RegistryPtrMap, only: RegistryPtrMapIterator
   use mapl3g_VirtualPtFamilyMap, only: VirtualPtFamilyMapIterator
   implicit none(type,external)

contains

   module subroutine propagate_unsatisfied_imports_all(this, rc)
      class(StateRegistry), target, intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status
      class(StateRegistry), pointer :: subregistry
      type(RegistryPtrMapIterator) :: iter

      associate (e => this%subregistries%ftn_end())
        iter = this%subregistries%ftn_begin()
        do while (iter /= e)
           call iter%next()
           subregistry => this%get_subregistry(iter%first(), _RC)
           call this%propagate_unsatisfied_imports(subregistry, _RC)
        end do
      end associate
   
      _RETURN(_SUCCESS)
   end subroutine propagate_unsatisfied_imports_all

   module subroutine propagate_unsatisfied_imports_subregistry(this, subregistry, rc)
      class(StateRegistry), target, intent(inout) :: this
      class(StateRegistry), target, intent(in) :: subregistry
      integer, optional, intent(out) :: rc

      integer :: status
      type(VirtualPtFamilyMapIterator) :: iter
      type(VirtualConnectionPt), pointer :: virtual_pt
      type(ExtensionFamily), pointer :: family

      associate (e => subregistry%family_map%ftn_end())
        iter = subregistry%family_map%ftn_begin()
        do while (iter /= e)
           call iter%next()
           virtual_pt => iter%first()
           if (.not. virtual_pt%is_import()) cycle
           family => iter%second()
           call this%propagate_unsatisfied_imports(virtual_pt, family, _RC)
        end do
      end associate

      _RETURN(_SUCCESS)
   end subroutine propagate_unsatisfied_imports_subregistry

   module subroutine propagate_unsatisfied_imports_virtual_pt(this, virtual_pt, family, rc)
      class(StateRegistry), target, intent(inout) :: this
      type(VirtualConnectionPt), intent(in) :: virtual_pt
      type(ExtensionFamily), intent(in) :: family
      integer, optional, intent(out) :: rc

      integer :: status
      type(StateItemSpecPtrVector) :: extensions
      type(StateItemSpecPtr), pointer :: extension
      integer :: i

      extensions = family%get_specs()
      do i = 1, extensions%size()
         extension => extensions%of(i)
         call link(extension%ptr, _RC)
      end do

      _RETURN(_SUCCESS)
   contains

      subroutine link(extension, rc)
         class(StateItemSpec), pointer :: extension
         integer, optional, intent(out) :: rc

         integer :: status
         
         _RETURN_IF(extension%is_active())

         if (.not. this%has_virtual_pt(virtual_pt)) then
            call this%add_virtual_pt(virtual_pt, _RC)
         end if
         call this%link_spec(virtual_pt, extension, _RC)

         _RETURN(_SUCCESS)
      end subroutine link
      
      
   end subroutine propagate_unsatisfied_imports_virtual_pt

   ! Loop over subregistry and propagate exports of each
   module subroutine propagate_exports_all(this, rc)
      class(StateRegistry), target, intent(inout) :: this
      integer, optional, intent(out) :: rc

      integer :: status
      class(StateRegistry), pointer :: subregistry
      type(RegistryPtrMapIterator) :: iter

      associate (e => this%subregistries%ftn_end())
        iter = this%subregistries%ftn_begin()
        do while (iter /= e)
           call iter%next()
           subregistry => this%get_subregistry(iter%first(), _RC)
           call this%propagate_exports(subregistry, _RC)
        end do
      end associate

      _RETURN(_SUCCESS)
   end subroutine propagate_exports_all

   module subroutine propagate_exports_subregistry(this, subregistry, rc)
      class(StateRegistry), target, intent(inout) :: this
      type(StateRegistry), target, intent(in) :: subregistry
      integer, optional, intent(out) :: rc

      integer :: status
      type(VirtualPtFamilyMapIterator) :: iter

     associate (e => subregistry%family_map%ftn_end())
        iter = subregistry%family_map%ftn_begin()
        do while (iter /= e)
           call iter%next()
           call this%propagate_exports(subregistry%get_name(), iter, _RC)
        end do
      end associate

      _RETURN(_SUCCESS)
   end subroutine propagate_exports_subregistry

   module subroutine propagate_exports_virtual_pt(this, subregistry_name, iter, rc)
      class(StateRegistry), target, intent(inout) :: this
      character(*), intent(in) :: subregistry_name
      type(VirtualPtFamilyMapIterator), intent(in) :: iter
      integer, optional, intent(out) :: rc

      type(VirtualConnectionPt), pointer :: virtual_pt
      type(VirtualConnectionPt) :: new_virtual_pt
      type(ExtensionFamily), pointer :: family
      type(ExtensionFamily), pointer :: parent_family

      virtual_pt => iter%first()
      _RETURN_UNLESS(virtual_pt%is_export())

      new_virtual_pt = virtual_pt
      if (virtual_pt%get_comp_name() == '') then
         new_virtual_pt = VirtualConnectionPt(virtual_pt, comp_name=subregistry_name)
      end if

      if (.not. this%has_virtual_pt(new_virtual_pt)) then
         call this%add_virtual_pt(new_virtual_pt)
      end if

      family => iter%second()
      parent_family => this%get_extension_family(new_virtual_pt)
      call parent_family%merge(family)

      _RETURN(_SUCCESS)
   end subroutine propagate_exports_virtual_pt

end submodule StateRegistry_Propagation_smod
