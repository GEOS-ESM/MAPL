#include "MAPL_Generic.h"
module mapl3g_VariableSpec_private

   use esmf, only: ESMF_KIND_R4, ESMF_RegridMethod_Flag
   use esmf, only: ESMF_StateItem_Flag, ESMF_StateIntent_Flag
   use esmf, only: ESMF_STATEITEM_FIELD, ESMF_STATEITEM_FIELDBUNDLE
   use esmf, only: ESMF_STATEINTENT_UNSPECIFIED
   use esmf, only: operator(==), operator(/=)
   use mapl3g_EsmfRegridder, only: EsmfRegridderParam
   use gFTL2_StringVector
   use mapl_ErrorHandling

   implicit none(type, external)
   private
   public :: validate_short_name
   public :: validate_state_intent
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

   function get_alphanumeric_() result(range)
       character(len=:), allocatable :: range

       range = get_alpha() // to_string(get_ascii_range('09')) // '_'

   end function get_alphanumeric_
   
   logical function is_all_alpha(s)
      character(len=*), intent(in) :: s
      
      is_all_alpha = verify(s, get_alpha()) == 0

   end function is_all_alpha

   logical function is_all_alphanumeric_(s)
      character(len=*), intent(in) :: s
      
      is_all_alphanumeric_ = verify(s, get_alphanumeric_()) == 0

   end function is_all_alphanumeric_

   logical function valid_identifier(s) result(res)
      character(len=*), intent(in) :: s

      res = .FALSE.
      if(len_trim(s) == 0) return
      if(verify(s, ' ') > 1) return
      
      res = is_all_alpha(trim(s(1:1))) .and. is_all_alphanumeric_(trim(s(2:)))

   end function valid_identifier

   logical function valid_regrid_member(param, method) result(res)
      type(EsmfRegridderParam), optional, intent(in) :: param
      type(ESMF_RegridMethod_Flag), optional, intent(in) :: method

      res = .TRUE.
      if(present(param)) res = .not. present(method)

   end function valid_regrid_member

   logical function valid_state_intent(val) result(res)
      type(ESMF_StateIntent_Flag), intent(in) :: val

      res = val /= ESMF_STATEINTENT_UNSPECIFIED

   end function valid_state_intent

   subroutine validate_short_name(v, rc)
      character(len=*), intent(in) :: v
      integer, optional, intent(out) :: rc
      integer :: status
      character(len=*), parameter :: M='short_name must begin with a letter and include alphanumeric characters or _ only.'

      _ASSERT(present(v), 'short_name not allocated')
      _ASSERT(valid_identifier(v), M)
      _RETURN(_SUCCESS)

   end subroutine validate_short_name

   subroutine validate_state_intent(v, rc)
      type(ESMF_StateIntent_Flag), intent(in) :: v
      integer, optional, intent(out) :: rc
      integer :: status
      character(len=*), parameter :: M='The state intent is not an allowed flag value.'

      _ASSERT(valid_state_intent(v), M)
      _RETURN(_SUCCESS)

   end subroutine validate_state_intent

   subroutine validate_regrid(p, f, rc)
      type(EsmfRegridderParam), optional, intent(in) :: p
      type(ESMF_RegridMethod_Flag), optional, intent(in) :: f
      integer, optional, intent(out) :: rc
      integer :: status
      character(len=*), parameter :: M='regrid_param and regrid_method are mutually exclusive.'
      
      _ASSERT(valid_regrid_member(p, f), M)
      _RETURN(_SUCCESS)

   end subroutine validate_regrid

end module mapl3g_VariableSpec_private
