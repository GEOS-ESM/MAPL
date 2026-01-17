#include "MAPL.h"

! Lifecycle procedures for StateRegistry:
! - new_StateRegistry: Constructor
! - get_name: Name accessor
! - has_virtual_pt: Check if virtual connection point exists
! - num_owned_items: Count owned items
! - write_formatted: Formatted output for debugging

submodule (mapl3g_StateRegistry) StateRegistry_Lifecycle_smod
   use mapl3g_VirtualPtFamilyMap, only: VirtualPtFamilyMapIterator
   implicit none(type,external)

contains

   module function new_StateRegistry(name) result(r)
      type(StateRegistry) :: r
      character(*), intent(in) :: name

      r%name = name
   end function new_StateRegistry

   module function get_name(this) result(name)
      character(:), allocatable :: name
      class(StateRegistry), intent(in) :: this
      name = this%name
   end function get_name

   module function has_virtual_pt(this, virtual_pt) result(has_pt)
      logical :: has_pt
      class(StateRegistry), intent(in) :: this
      type(VirtualConnectionPt), intent(in) :: virtual_pt
      has_pt = (this%family_map%count(virtual_pt) > 0)
   end function has_virtual_pt

   module function num_owned_items(this) result(num_items)
      integer :: num_items
      class(StateRegistry), intent(in) :: this
      num_items = this%owned_items%size()
   end function num_owned_items

   module subroutine write_formatted(this, unit, iotype, v_list, iostat, iomsg)
      class(StateRegistry), intent(in) :: this
      integer, intent(in) :: unit
      character(*), intent(in) :: iotype
      integer, intent(in) :: v_list(:)
      integer, intent(out) :: iostat
      character(*), intent(inout) :: iomsg

      write(unit,*,iostat=iostat,iomsg=iomsg) new_line('a')
      if (iostat /= 0) return

      call write_header(this, iostat=iostat, iomsg=iomsg)
      if (iostat /= 0) return

      call write_virtual_pts(this, iostat=iostat, iomsg=iomsg)
      if (iostat /= 0) return
      _UNUSED_DUMMY(v_list)
      _UNUSED_DUMMY(iotype)
   contains
      
      subroutine write_header(this, iostat, iomsg)
         class(StateRegistry), target, intent(in) :: this
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         integer :: total
         type(VirtualPtFamilyMapIterator) :: iter
         type(ExtensionFamily), pointer :: family

         total = 0
         associate (e => this%family_map%ftn_end())
           iter = this%family_map%ftn_begin()
           do while (iter /= e)
              call iter%next()
              family => iter%second()
              total = total + family%num_variants()
           end do
         end associate

         write(unit,'(a,a, a,i0, a,i0, a,i0,a)',iostat=iostat,iomsg=iomsg) &
              'Registry(name=', this%name, &
              ', n_owned=', this%num_owned_items(), &
              ', n_virtual=', this%family_map%size(), &
              ', n_extensions=', total, ')' // new_line('a')
         if (iostat /= 0) return
         write(unit,*,iostat=iostat,iomsg=iomsg) '   extensions: '// new_line('a')
      end subroutine write_header

      subroutine write_virtual_pts(this, iostat, iomsg)
         class(StateRegistry), target, intent(in) :: this
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         type(VirtualPtFamilyMapIterator) :: virtual_iter
         type(ExtensionFamily), pointer :: family
         type(StateItemExtension), pointer :: extension
         type(StateItemSpec), pointer :: spec
         logical :: is_active

         write(unit,*,iostat=iostat,iomsg=iomsg) '   virtuals: '// new_line('a')
         if (iostat /= 0) return
         associate (e => this%family_map%ftn_end())
           virtual_iter = this%family_map%ftn_begin()
           do while (virtual_iter /= e)
              call virtual_iter%next()
              associate (virtual_pt => virtual_iter%first())
                family => virtual_iter%second()
                is_active = .false.
                if (family%has_primary()) then
                   extension => family%get_primary()
                   spec => extension%get_spec()
                   is_active = spec%is_active()
                end if
                write(unit,*,iostat=iostat,iomsg=iomsg)'        ',virtual_pt,  &
                     ': ',family%num_variants(), ' variants ', &
                     ' is primary? ', family%has_primary(),  ' is active? ', is_active, new_line('a')
                if (iostat /= 0) return
              end associate
           end do
         end associate
      end subroutine write_virtual_pts

      
   end subroutine write_formatted

end submodule StateRegistry_Lifecycle_smod
