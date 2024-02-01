#include "MAPL_Generic.h"

module mapl3g_ESMF_HConfigUtilities
   use esmf
   use mapl_ErrorHandling
   implicit none
   private

   public :: write(formatted)
   public :: MAPL_HConfigMatch

   interface write(formatted)
      procedure write_hconfig
   end interface write(formatted)

contains

   subroutine write_hconfig(hconfig, unit, iotype, v_list, iostat, iomsg)
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

   logical function MAPL_HConfigMatch(a, b, rc) result(match)
      type(ESMF_HConfig), intent(in) :: a, b
      integer, optional, intent(out) :: rc

      integer :: status

      ! Workaround for GFortran recursion bug
      integer, parameter :: MAX_DEPTH = 10
      type(ESMF_HConfig) :: a_hconfigs(MAX_DEPTH)
      type(ESMF_HConfig) :: b_hconfigs(MAX_DEPTH)
      integer :: depth = 0

      match = recursive_HConfigMatch(a, b, _RC)
      _RETURN(_SUCCESS)
   contains

      recursive logical function recursive_HConfigMatch(a, b, rc) result(match)
         type(ESMF_HConfig), intent(in) :: a, b
         integer, optional, intent(out) :: rc

         integer :: status
         character(:), allocatable :: a_type, b_type

         match = .false. ! unless
         depth = depth + 1
         _ASSERT(depth <= MAX_DEPTH, "Recursion limit execeeded in MAPL_HConfigMatch()")

         a_type = get_hconfig_type(a, _RC)
         b_type = get_hconfig_type(b, _RC)

         if (a_type /= b_type) then
            _RETURN(_SUCCESS)
         end if

         if (a_type == 'MAPPING') then
            match = MAPL_HConfigMatchMapping(a, b, _RC)
         else if (a_type == 'SEQUENCE') then
            match = MAPL_HConfigMatchSequence(a, b, _RC)
         else if (a_type == 'SCALAR') then
            match = MAPL_HConfigMatchScalar(a, b, _RC)
         else
            _FAIL('unsupported HConfig type.')
         end if
         depth = depth - 1

         _RETURN(_SUCCESS)
      end function recursive_HConfigMatch

      function get_hconfig_type(hconfig, rc) result(hconfig_type)
         type(ESMF_HConfig), intent(in) :: hconfig
         integer, optional, intent(out) :: rc

         integer :: status
         character(:), allocatable :: hconfig_type
         logical :: is_scalar
         logical :: is_sequence
         logical :: is_mapping

         is_scalar = ESMF_HConfigIsScalar(hconfig, _RC)
         if (is_scalar) then
            hconfig_type = 'SCALAR'
            _RETURN(_SUCCESS)
         end if

         is_sequence = ESMF_HConfigIsSequence(hconfig, _RC)
         if (is_sequence) then
            hconfig_type = 'SEQUENCE'
            _RETURN(_SUCCESS)
         end if

         is_mapping = ESMF_HConfigIsMap(hconfig, _RC)
         if (is_mapping) then
            hconfig_type = 'MAPPING'
            _RETURN(_SUCCESS)
         end if

         hconfig_type = 'UNKNOWN'
         _FAIL('unsupported HConfig type.')

         _RETURN(_SUCCESS)
      end function get_hconfig_type

      recursive logical function MAPL_HConfigMatchScalar(a, b, rc) result(match)
         type(ESMF_HConfig), intent(in) :: a, b
         integer, optional, intent(out) :: rc

         integer :: status
         character(:), allocatable :: a_str, b_str

         match = .false. ! unless

         a_str = ESMF_HConfigAsString(a, _RC)
         b_str = ESMF_HConfigAsString(b, _RC)
         match = (a_str == b_str)

         _RETURN(_SUCCESS)
      end function MAPL_HConfigMatchScalar


      recursive logical function MAPL_HConfigMatchSequence(a, b, rc) result(match)
         type(ESMF_HConfig), intent(in) :: a, b
         integer, optional, intent(out) :: rc

         integer :: status
         type(ESMF_HConfig) :: a_val_hconfig, b_val_hconfig
         integer :: i
         integer :: a_size, b_size

         match = .false. ! unless

         a_size = ESMF_HConfigGetSize(a, _RC)
         b_size = ESMF_HConfigGetSize(b, _RC)

         _RETURN_UNLESS(a_size == b_size)

         do i = 1, a_size

            a_hconfigs(depth) = ESMF_HConfigCreateAt(a, index=i, _RC)
            b_hconfigs(depth) = ESMF_HConfigCreateAt(b, index=i, _RC)

            match = recursive_HConfigMatch(a_hconfigs(depth), b_hconfigs(depth), _RC)

            call ESMF_HConfigDestroy(a_hconfigs(depth), _RC)
            call ESMF_HConfigDestroy(b_hconfigs(depth), _RC)

            _RETURN_UNLESS(match)
         end do

         match = .true.

         _RETURN(_SUCCESS)
      end function MAPL_HConfigMatchSequence

      recursive logical function MAPL_HConfigMatchMapping(a, b, rc) result(match)
         type(ESMF_HConfig), intent(in) :: a, b
         integer, optional, intent(out) :: rc

         integer :: status
         type(ESMF_HConfig) :: a_val_hconfig, b_val_hconfig
         character(:), allocatable :: key
         type(ESMF_HConfigIter) :: iter, iter_begin, iter_end
         integer :: a_size, b_size

         match = .false. ! unless

         a_size = ESMF_HConfigGetSize(a, _RC)
         b_size = ESMF_HConfigGetSize(b, _RC)

         _RETURN_UNLESS(a_size == b_size)

         iter_begin = ESMF_HConfigIterBegin(a, _RC)
         iter_end = ESMF_HConfigIterEnd(a, _RC)
         iter = iter_begin

         do while (ESMF_HConfigIterLoop(iter, iter_begin, iter_end, rc=status))
            _VERIFY(status)

            key = ESMF_HConfigAsStringMapKey(iter, _RC)
            match = ESMF_HConfigIsDefined(b, keystring=key, _RC)
            _RETURN_UNLESS(match)

            a_hconfigs(depth) = ESMF_HConfigCreateAt(a, keyString=key, _RC)
            b_hconfigs(depth) = ESMF_HConfigCreateAt(b, keyString=key, _RC)

            match = recursive_HConfigMatch(a_hconfigs(depth), b_hconfigs(depth), _RC)

            call ESMF_HConfigDestroy(a_hconfigs(depth), _RC)
            call ESMF_HConfigDestroy(b_hconfigs(depth), _RC)

            _RETURN_UNLESS(match)
         end do

         match = .true.

         _RETURN(_SUCCESS)
      end function MAPL_HConfigMatchMapping

   end function MAPL_HConfigMatch
end module mapl3g_ESMF_HConfigUtilities
