#include "MAPL.h"

submodule (mapl3g_ESMF_HConfigUtilities) MAPL_HConfigMatch_smod
   implicit none


contains

   module function MAPL_HConfigMatch(a, b, rc) result(match)
      logical :: match
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
         character(:), allocatable :: a_tag, b_tag
         logical :: a_as_bool, b_as_bool
         integer(kind=ESMF_KIND_I8) :: a_as_int, b_as_int
         real(kind=ESMF_KIND_R8) :: a_as_float, b_as_float
         
         match = .false. ! nless

         a_tag = ESMF_HConfigGetTag(a, _RC)
         b_tag = ESMF_HConfigGetTag(b, _RC)
         _RETURN_UNLESS(a_tag == b_tag)
 
         select case(a_tag)
         case (CORE_SCHEMA_BOOL_TAG)
            a_as_bool  = ESMF_HConfigAsLogical(a, _RC)
            b_as_bool  = ESMF_HConfigAsLogical(b, _RC)
            match = a_as_bool .eqv. b_as_bool

         case (CORE_SCHEMA_INT_TAG)
            a_as_int  = ESMF_HConfigAsI8(a, _RC)
            b_as_int  = ESMF_HConfigAsI8(b, _RC)
            match = (a_as_int == b_as_int)

         case (CORE_SCHEMA_FLOAT_TAG)
            a_as_float  = ESMF_HConfigAsR8(a, _RC)
            b_as_float  = ESMF_HConfigAsR8(b, _RC)
            match = (a_as_float == b_as_float)

         case (CORE_SCHEMA_STR_TAG)
            ! Otherwise they are strings ...
            a_str = ESMF_HConfigAsString(a, _RC)
            b_str = ESMF_HConfigAsString(b, _RC)
            match = (a_str == b_str)

         case default
            _FAIL('unsupported yaml tag: <'//a_tag//'>')
         end select

         _RETURN(_SUCCESS)
      end function MAPL_HConfigMatchScalar


      recursive logical function MAPL_HConfigMatchSequence(a, b, rc) result(match)
         type(ESMF_HConfig), intent(in) :: a, b
         integer, optional, intent(out) :: rc

         integer :: status
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
   
end submodule MAPL_HConfigMatch_smod
