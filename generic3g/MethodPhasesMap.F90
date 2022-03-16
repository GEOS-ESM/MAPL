! Maybe change this to be a map of ESMF_MethodFlag to a
! PhaseMethodMap?
#include "MAPL_ErrLog.h"

module mapl3g_MethodPhasesMap_private
   use :: gFTL2_StringVector, only: StringVector
   use :: esmf, only: ESMF_Method_Flag

#define Key ESMF_Method_Flag
#define Key_LT(a,b) method_less(a,b)
#define T StringVector
#define Map MethodPhasesMap
#define MapIterator MethodPhasesMapIterator
#define Pair MethodPhasesPair

#include "map/template.inc"

#undef MethodPhasesPair
#undef MapIterator
#undef Map
#undef T
#undef Key

   ! This function imposes an ordering on objects of type
   ! ESMF_Method_Flag.  Unfortunately, the internal integer used by
   ! ESMF is PRIVATE.
   logical function method_less(a,b) result(less)
      type(ESMF_Method_Flag), intent(in) :: a, b

      associate (idx_a => find(a), idx_b => find(b))
        less = (idx_a < idx_b)
      end associate

   contains

      integer function find(a) result(idx)
         use :: esmf, only: ESMF_METHOD_INITIALIZE, ESMF_METHOD_RUN, ESMF_METHOD_FINALIZE
         use :: esmf, only: ESMF_METHOD_READRESTART, ESMF_METHOD_WRITERESTART
         use :: esmf, only: operator(==)
         type(ESMF_Method_Flag), intent(in) :: a

         type(ESMF_Method_Flag), parameter :: METHODS(*) = [ &
              ESMF_METHOD_INITIALIZE,  &
              ESMF_METHOD_RUN,         &
              ESMF_METHOD_FINALIZE,    &
              ESMF_METHOD_READRESTART, &
              ESMF_METHOD_WRITERESTART]
         
         integer :: i

         do i = 1, size(METHODS)
            if (a == METHODS(i)) return
         end do

         idx = -1 ! should not be reachable
      end function find
      
   end function method_less

end module mapl3g_MethodPhasesMap_private

module mapl3g_MethodPhasesMapUtils
   use mapl3g_MethodPhasesMap_private
   use mapl_ErrorHandling
   use :: mapl_KeywordEnforcer
   use :: esmf, only: ESMF_Method_Flag
   use :: gftl2_StringVector
   implicit none
   private

   public :: add_phase
   public :: get_phase_index

   interface add_phase
      module procedure add_phase_
   end interface

   interface get_phase_index
      module procedure get_phase_index_
   end interface

   character(len=*), parameter :: DEFAULT_PHASE_NAME = "default"

contains

   subroutine add_phase_(phases_map, method_flag, phase_name, unusable, rc)
      use :: esmf, only: ESMF_METHOD_INITIALIZE, ESMF_METHOD_RUN, ESMF_METHOD_FINALIZE
      use :: esmf, only: ESMF_METHOD_READRESTART, ESMF_METHOD_WRITERESTART
      use :: esmf, only: operator(==)
      type(MethodPhasesMap), intent(inout) :: phases_map
      type(ESMF_Method_Flag), intent(in) :: method_flag
      character(len=*), optional, intent(in) :: phase_name
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) ::rc

      character(len=:), allocatable :: phase_name_
      type(StringVector), pointer :: phase_names
      integer :: status
      integer :: i

      _ASSERT(phases_map%count(method_flag) > 0, "Unsupported value for 'method_flag'.")

      phase_name_ = DEFAULT_PHASE_NAME
      if (present(phase_name)) phase_name_ = phase_name
      
      if (phases_map%count(method_flag) == 0) then
         call phases_map%insert(method_flag, StringVector())
      end if
      
      phase_names => phases_map%of(method_flag)
      _ASSERT(find(phase_names%begin(), phase_names%end(), phase_name_) == phase_names%end(), "duplicate phase name")
      call phase_names%push_back(phase_name_)

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)
   end subroutine add_phase_

   integer function get_phase_index_(phases, phase_name, unusable, rc) result(phase_index)
      type(StringVector), intent(in) :: phases
      character(len=*), intent(in) :: phase_name
      class(KeywordEnforcer), optional, intent(in) :: unusable
      integer, optional, intent(out) :: rc

      associate (b => phases%begin(), e => phases%end())
        associate (iter => find(b, e, phase_name))
          _ASSERT(iter /= phases%end(), "phase <"//trim(phase_name)//"> not found")
          phase_index = 1 + distance(b, iter)
        end associate
      end associate

   end function get_phase_index_

end module mapl3g_MethodPhasesMapUtils

module mapl3g_MethodPhasesMap
   use mapl3g_MethodPhasesMap_private
   use mapl3g_MethodPhasesMapUtils
   implicit none

contains

   subroutine initialize_phases_map(phases_map)
      use :: gFTL2_StringVector, only: StringVector
      use :: esmf, only: ESMF_METHOD_INITIALIZE, ESMF_METHOD_RUN, ESMF_METHOD_FINALIZE
      use :: esmf, only: ESMF_METHOD_READRESTART, ESMF_METHOD_WRITERESTART
      type(MethodPhasesMap), intent(out) :: phases_map

      call phases_map%insert(ESMF_METHOD_INITIALIZE,   StringVector())
      call phases_map%insert(ESMF_METHOD_RUN,          StringVector())
      call phases_map%insert(ESMF_METHOD_FINALIZE,     StringVector())
      call phases_map%insert(ESMF_METHOD_READRESTART,  StringVector())
      call phases_map%insert(ESMF_METHOD_WRITERESTART, StringVector())

   end subroutine initialize_phases_map

end module mapl3g_MethodPhasesMap
