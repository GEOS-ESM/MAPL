#include "MAPL.h"

submodule (mapl3g_ESMF_HConfigUtilities) write_hconfig_smod
   implicit none

contains

   module subroutine write_hconfig(hconfig, unit, iotype, v_list, iostat, iomsg)
      type(ESMF_Hconfig), intent(in) :: hconfig
      integer, intent(in) :: unit
      character(*), intent(in) :: iotype
      integer, intent(in) :: v_list(:)
      integer, intent(out) :: iostat
      character(*), intent(inout) :: iomsg

      ! Workaround for GFortran recursion bug
      integer, parameter :: MAX_DEPTH = 10
      type(ESMF_HConfig) :: val_hconfigs(MAX_DEPTH)
      integer :: depth = 0

      call write_hconfig_recursive(hconfig, unit, iotype, v_list, iostat, iomsg)

   contains

      recursive subroutine write_hconfig_recursive(hconfig, unit, iotype, v_list, iostat, iomsg)
         type(ESMF_Hconfig), intent(in) :: hconfig
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in) :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         logical :: is_mapping, is_sequence, is_scalar

         iostat = 0 ! unless
         depth = depth + 1
         if (depth > MAX_DEPTH) then
            iostat = 9999
            return
         end if

         is_mapping = ESMF_HConfigIsMap(hconfig, rc=iostat)
         if (iostat /= 0) return

         if (is_mapping) then
            call write_mapping(hconfig, unit, iotype, v_list, iostat, iomsg)
            depth = depth - 1
            return
         end if

         is_sequence = ESMF_HConfigIsSequence(hconfig, rc=iostat)
         if (iostat /= 0) return

         if (is_sequence) then
            call write_sequence(hconfig, unit, iotype, v_list, iostat, iomsg)
            depth = depth - 1
            return
         end if

         is_scalar = ESMF_HConfigIsScalar(hconfig, rc=iostat)
         if (iostat /= 0) return

         if (is_scalar) then
            call write_scalar(hconfig, unit, iotype, v_list, iostat, iomsg)
            depth = depth - 1
            return
         end if

         iostat = 0 ! Illegal node type
      end subroutine write_hconfig_recursive

      recursive subroutine write_mapping(hconfig, unit, iotype, v_list, iostat, iomsg)
         type(ESMF_Hconfig), intent(in) :: hconfig
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in) :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         type(ESMF_HConfigIter) :: iter, iter_begin, iter_end
         character(:), allocatable :: key
         logical :: first

         iostat = 0 ! unless

         write(unit, '("{")', iostat=iostat, iomsg=iomsg)
         if (iostat /= 0) return
         iter_begin = ESMF_HConfigIterBegin(hconfig, rc=iostat)
         if (iostat /= 0) return
         iter_end = ESMF_HConfigIterEnd(hconfig, rc=iostat)
         if (iostat /= 0) return
         iter = iter_begin

         first = .true.
         do while (ESMF_HConfigIterLoop(iter, iter_begin, iter_end, rc=iostat))
            if (iostat /= 0) return

            key = ESMF_HConfigAsStringMapKey(iter, rc=iostat)
            if (iostat /= 0) return

            if (.not. first) then
               write(unit, '(", ")', iostat=iostat, iomsg=iomsg)
               if (iostat /= 0) return
            end if
            first =.false.
            write(unit, '(a,a)', iostat=iostat, iomsg=iomsg) key, ': '
            if (iostat /= 0) return

            val_hconfigs(depth) = ESMF_HConfigCreateAtMapVal(iter, rc=iostat)
            if (iostat /= 0) return

            call write_hconfig_recursive(val_hconfigs(depth), unit, iotype, v_list, iostat, iomsg)
            if (iostat /= 0) return

            call ESMF_HConfigDestroy(val_hconfigs(depth), rc=iostat)
            if (iostat /= 0) return

         end do
         write(unit, '("}")', iostat=iostat, iomsg=iomsg)
         if (iostat /= 0) return

      end subroutine write_mapping

      recursive subroutine write_sequence(hconfig, unit, iotype, v_list, iostat, iomsg)
         type(ESMF_Hconfig), intent(in) :: hconfig
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in) :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         type(ESMF_HConfigIter) :: iter, iter_begin, iter_end
         type(ESMF_HConfig) :: val_hconfig
         logical :: first

         iostat = 0 ! unless
         write(unit, '("[")', iostat=iostat, iomsg=iomsg)

         iter_begin = ESMF_HConfigIterBegin(hconfig, rc=iostat)
         if (iostat /= 0) return
         iter_end = ESMF_HConfigIterEnd(hconfig, rc=iostat)
         if (iostat /= 0) return
         iter = iter_begin
         first = .true.
         do while (ESMF_HConfigIterLoop(iter, iter_begin, iter_end, rc=iostat))
            if (iostat /= 0) return

            if (.not. first) then
               write(unit, '(", ")', iostat=iostat, iomsg=iomsg)
               if (iostat /= 0) return
            end if
            first =.false.

            val_hconfigs(depth) = ESMF_HConfigCreateAt(iter, rc=iostat)
            if (iostat /= 0) return
            call write_hconfig_recursive(val_hconfigs(depth), unit, iotype, v_list, iostat, iomsg)
            if (iostat /= 0) return
            call ESMF_HConfigDestroy(val_hconfigs(depth), rc=iostat)
            if (iostat /= 0) return

         end do

         write(unit, '("]")', iostat=iostat, iomsg=iomsg)
         if (iostat /= 0) return

      end subroutine write_sequence

      recursive subroutine write_scalar(hconfig, unit, iotype, v_list, iostat, iomsg)
         type(ESMF_Hconfig), intent(in) :: hconfig
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in) :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         character(:), allocatable :: str

         iostat = 0 ! unless

         str = ESMF_HConfigAsString(hconfig, rc=iostat)
         if (iostat /= 0) return
         write(unit, '(a)', iostat=iostat, iomsg=iomsg)  str
         if (iostat /= 0) return

      end subroutine write_scalar

   end subroutine write_hconfig

end submodule write_hconfig_smod
