#include "MAPL_Generic.h"
module mapl3g_VariableSpec_private

   use esmf, only: ESMF_KIND_R4, ESMF_RegridMethod_Flag
   use esmf, only: ESMF_StateItem_Flag, ESMF_StateIntent_Flag
   use esmf, only: ESMF_STATEITEM_FIELD, ESMF_STATEITEM_FIELDBUNDLE
   use esmf, only: ESMF_STATEINTENT_EXPORT, ESMF_STATEINTENT_IMPORT, ESMF_STATEINTENT_INTERNAL
   use esmf, only: operator(==)
   use mapl3g_EsmfRegridder, only: EsmfRegridderParam
   use gFTL2_StringVector
   use mapl_ErrorHandling

   implicit none(type, external)
   private
   public :: validate_short_name
   public :: validate_state_intent
   public :: validate_state_item
   public :: validate_regrid

contains

   function to_string(array) result(string)
      character, intent(in) :: array(:)
      character(len=size(array)) :: string
      integer :: i

      do i = 1, size(array)
         string(i:i) = array(i)
      end do

   end function to_string

   function get_ascii_range(bounds) result(range)
      character, allocatable :: range(:)
      character(len=2), intent(in) :: bounds
      integer :: ibounds(2)
      integer :: i

      ibounds = iachar([bounds(1:1), bounds(2:2)])
      range = [(achar(i), i=minval(ibounds), maxval(ibounds))]

   end function get_ascii_range

   function get_alpha() result(range)
      character(len=:), allocatable :: range

      range = to_string(get_ascii_range('AZ'))//to_string(get_ascii_range('az'))

   end function get_alpha

   function get_alpha_numeric_() result(range)
       character(len=:), allocatable :: range

       range = get_alpha() // to_string(get_ascii_range('09')) // '_'

   end function get_alpha_numeric_
   
   logical function is_all_alpha(s)
      character(len=*), intent(in) :: s
      
      is_all_alpha = verify(s, get_alpha()) == 0

   end function is_all_alpha

   logical function is_all_alphanumeric_(s)
      character(len=*), intent(in) :: s
      
      is_all_alphanumeric_ = verify(s, get_alpha_numeric_()) == 0

   end function is_all_alphanumeric_

   logical function is_valid_identifier(s)
      character(len=*), intent(in) :: s

      is_valid_identifier = .FALSE.
      if(len_trim(s) == 0) return
      if(verify(s, ' ') > 1) return
      
      is_valid_identifier = is_all_alpha(trim(s(1:1))) .and. is_all_alphanumeric_(trim(s(2:)))

   end function is_valid_identifier

   logical function is_in_integer(bounds, n) result(lval)
      integer, intent(in) :: bounds(:)
      integer, intent(in) :: n
      integer :: i

      lval = .FALSE.
      if(size(bounds) < 2) return

      do i = 2, mod(size(bounds), 2), 2
         lval = n >= bounds(i-1) .and. n <= bounds(i)
         if(lval) exit
      end do

   end function is_in_integer

   logical function is_in_realR4(bounds, t) result(lval)
      real(kind=ESMF_KIND_R4), intent(in) :: bounds(:)
      real(kind=ESMF_KIND_R4), intent(in) :: t
      integer :: i

      lval = .FALSE.
      if(size(bounds) < 2) return

      do i = 2, mod(size(bounds), 2), 2
         lval = t >= bounds(i-1) .and. t <= bounds(i)
         if(lval) exit
      end do

   end function is_in_realR4

   logical function is_not_empty(string)
      character(len=*), intent(in) :: string

      is_not_empty = len_trim(string) > 0

   end function is_not_empty

   logical function no_test(v)
      class(*), intent(in) :: v

      no_test = .TRUE.

   end function no_test

   logical function string_in_vector(string, vector) result(in_vector)
      character(len=*), intent(in) :: string
      class(StringVector), intent(in) :: vector
      type(StringVectorIterator) :: e, iter

      in_vector = .TRUE.
      e = vector%end()
      iter = vector%begin()
      do while(iter /= e)
         if(string == iter%of()) return
         call iter%next()
      end do
      in_vector = .FALSE.

   end function string_in_vector

   logical function is_vector_in_string_vector(V0, V) result(lval)
      class(StringVector), intent(in) :: V0
      class(StringVector), intent(in) :: V
      type(StringVectorIterator) :: iter, e

      lval = .FALSE.
      iter = V%begin()
      e = V%end()
      do while(iter /= e)
         if(.not. string_in_vector(iter%of(), V0)) return
         call iter%next()
      end do
      lval = .TRUE.

   end function is_vector_in_string_vector

   subroutine validate_short_name(v, rc)
      character(len=*), intent(in) :: v
      integer, optional, intent(out) :: rc
      integer :: status

      _ASSERT(is_valid_identifier(v), 'Invalid value')
      _RETURN(_SUCCESS)

   end subroutine validate_short_name

   subroutine validate_regrid(param, method, rc)
      type(EsmfRegridderParam), optional, intent(in) :: param
      type(ESMF_RegridMethod_Flag), optional, intent(in) :: method
      integer, optional, intent(out) :: rc
      integer :: status

      if(present(param)) then
         _ASSERT(.not. present(method), 'regrid_param and regrid_method are mutually exclusive.')
      end if
      _RETURN(_SUCCESS)

   end subroutine validate_regrid

   subroutine validate_state_intent(v, rc)
      type(ESMF_StateIntent_Flag), intent(in) :: v
      integer, optional, intent(out) :: rc
      integer :: status

      _ASSERT(valid_state_intent(v), 'Invalid value')
      _RETURN(_SUCCESS)

   end subroutine validate_state_intent

   subroutine validate_state_item(v, rc)
      type(ESMF_StateItem_Flag), intent(in) :: v
      integer, optional, intent(out) :: rc
      integer :: status

      _ASSERT(valid_state_item(v), 'Invalid value')
      _RETURN(_SUCCESS)

   end subroutine validate_state_item

   logical function valid_state_intent(val)
      type(ESMF_StateIntent_Flag), intent(in) :: val

      valid_state_intent = (val == ESMF_STATEINTENT_EXPORT .or. val == ESMF_STATEINTENT_IMPORT .or. val == ESMF_STATEINTENT_INTERNAL)

   end function valid_state_intent

   logical function valid_state_item(val)
      type(ESMF_StateItem_Flag), intent(in) :: val
       
      valid_state_item = (val == ESMF_STATEITEM_FIELD .or. val == ESMF_STATEITEM_FIELDBUNDLE)

   end function valid_state_item

end module mapl3g_VariableSpec_private
