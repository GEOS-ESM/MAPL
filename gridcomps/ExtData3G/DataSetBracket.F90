#include "MAPL_Exceptions.h"
#include "MAPL_ErrLog.h"
module mapl3g_DataSetBracket
   use ESMF
   use MAPL_KeywordEnforcerMod
   use MAPL_ExceptionHandling
   use mapl3g_DataSetNode
   implicit none
   private

   public :: DataSetBracket

   type DataSetBracket
      type(DataSetNode) :: left_node
      type(DataSetNode) :: right_node
      logical          :: disable_interpolation = .false.
      contains
         procedure :: compute_bracket_weights 
         procedure :: time_in_bracket
         procedure :: set_parameters
         procedure :: get_left_node
         procedure :: get_right_node
         procedure :: set_node
   end type DataSetBracket

contains

   subroutine set_parameters(this, disable_interpolation, left_node, right_node)
      class(DataSetBracket), intent(inout) :: this
      logical, intent(in), optional :: disable_interpolation
      type(DataSetNode), intent(inout), optional :: left_node
      type(DataSetNode), intent(inout), optional :: right_node

      if (present(disable_interpolation)) this%disable_interpolation = disable_interpolation
      if (present(left_node)) this%left_node = left_node
      if (present(right_node)) this%right_node = right_node
   end subroutine

   function time_in_bracket(this,time) result(in_bracket)
      logical :: in_bracket
      class(DataSetBracket), intent(inout) :: this
      type(ESMF_Time), intent(in) :: time
      type(ESMF_Time) :: left_time, right_time
      
      left_time = this%left_node%get_interp_time()
      right_time = this%right_node%get_interp_time()

      in_bracket = (left_time <=time) .and. (time < right_time)

   end function time_in_bracket

   subroutine set_node(this, bracketside, node, rc)
      class(DataSetBracket), intent(inout) :: this
      integer, intent(in) :: bracketside
      type(DataSetNode), intent(in) :: node
      integer, optional, intent(out) :: rc

      if (bracketside==NODE_LEFT) then
         this%left_node = node
      else if (bracketside==NODE_RIGHT) then
         this%right_node = node
      else
         _FAIL('wrong bracket side')
      end if
      _RETURN(_SUCCESS)

   end subroutine set_node

   function get_right_node(this, rc) result(node)
      type(DataSetNode) :: node
      class(DataSetBracket), intent(inout) :: this
      integer, optional, intent(out) :: rc

      node = this%right_node
      _RETURN(_SUCCESS)

   end function get_right_node

   function get_left_node(this, rc) result(node)
      type(DataSetNode) :: node
      class(DataSetBracket), intent(inout) :: this
      integer, optional, intent(out) :: rc

      node = this%left_node
      _RETURN(_SUCCESS)

   end function get_left_node

   function compute_bracket_weights(this,time,rc) result(weights)
      real :: weights(2)
      class(DataSetBracket), intent(inout) :: this
      type(ESMF_Time), intent(in) :: time
      integer, optional, intent(out) :: rc

      type(ESMF_TimeInterval)    :: tinv1, tinv2
      type(ESMF_Time) :: time1, time2
      real                       :: alpha
      integer :: status

      alpha = 0.0
      if ( this%disable_interpolation) then
         weights(1) = 1.0
         weights(2) = 0.0
      else
         time1 = this%left_node%get_interp_time()
         time2 = this%right_node%get_interp_time()
         tinv1 = time - time1
         tinv2 = time2 - time1
         alpha = tinv1/tinv2
         weights(1) = alpha
         weights(2) = 1.0 - alpha
      end if

   end function compute_bracket_weights 

end module mapl3g_DataSetBracket
