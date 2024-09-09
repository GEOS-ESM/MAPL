module mapl3g_TimeAnalysisCouplerComponent
   use mapl3g_GenericCouplerComponent
   use, intrinsic :: iso_c_binding, only: c_int
!_   use, intrinsic :: iso_fortran_env, only: int32, int64, real32, real64
   implicit none
   private
!_   public ::

   enum, bind(c)
      enumerator :: ANALYSIS_ACCUMULATE = 1
      enumerator :: ANALYSIS_MEAN
      enumerator :: ANALYSIS_MAX
      enumerator :: ANALYSIS_MIN
      enumerator :: ANALYSIS_UNKNOWN
   end enum

   integer(c_int), parameter :: ANALYSIS_KIND = kind(ANALYSIS_UNKNOWN)

   abstract interface
      elemental function AccumulateFunction(current, update) result(updated)
         real, intent(in) :: updated
         real, intent(in) :: current
         real, intent(in) :: update
      end function AccumulateFunction
      elemental function CoupleFunction(current, counter) result(coupled)
         real :: coupled
         real, intent(in) :: current
         integer, intent(in) :: counter
      end function CoupleFunction
   end interface

   type :: TimeAnalysisField(k)
      integer, kind :: k
      type(VarSpec), pointer :: source
      type(VarSpec), pointer :: destination
      type(ESMF_Alarm), pointer   :: time_to_clear
      type(ESMF_Alarm), pointer   :: time_to_couple
      type(ESMF_LocalArray), pointer :: field_data
      type(ESMF_LocalArray), pointer :: array_count
      integer(ESMF_I8) :: scalar_count = -1
      integer(ESMF_I8) :: clear_interval
      integer(ESMF_I8) :: couple_interval
      integer, allocatable :: shape(:)
      procedure(AccumulateFunction), pointer :: accumulate_ptr
      procedure(CoupleFunction), pointer :: couple_ptr
      procedure, pointer :: clear_ptr
      integer(ANALYSIS_KIND) :: analysis_type
   end type TimeAnalysisField

   type :: TimeAnalysisInternal
      type(ESMF_Field), allocatable :: analysis_fields(:)
      logical :: active
      type (ESMF_Alarm), pointer   :: time2cpl_alarm => null()
      character(LEN=ESMF_MAXSTR)   :: name
   end type TimeAnalysisInternal

   abstract interface
      function Accumulate(
      subroutine UpdateArrays(acc, array_count, scalar_count, update_func, source_state, rc)
         type(ESMF_LocalArray), intent(inout) :: acc
         type(ESMF_LocalArray), intent(inout) :: array_count
         integer(ESMF_I8), intent(inout) :: scalar_count
         procedure(), pointer, intent(in) :: update_func
         type(ESMF_State), intent(in) :: source_state
         integer, optional, intent(out) :: rc
      end subroutine UpdateArrays
   end abstract interface

   integer(kind=c_int), parameter :: ANALYSIS_KIND = kind(ANALYSIS_UNKNOWN)

contains

   subroutine accumulate(analysis_field, source_state, rc)
         type(TimeAnalysisField), intent(inout) :: analysis_field
         type(ESMF_State), intent(in) :: source_state
         integer, optional, intent(out) :: rc
         integer :: status
   end subroutine accumulate

   function accumulate_function(acc, next) result(update)
      type(ESMF_Array) :: update
      type(ESMF_Array), intent(in) :: acc
      type(ESMF_Array), intent(in) :: next
   end function accumulate_function

   function mean_function(acc, next) result(update)
      type(ESMF_Array) :: update
      type(ESMF_Array), intent(in) :: acc
      type(ESMF_Array), intent(in) :: next
   end function mean_function

   function max_function(acc, next) result(update)
      type(ESMF_Array) :: update
      type(ESMF_Array), intent(in) :: acc
      type(ESMF_Array), intent(in) :: next
   end function max_function

   function min_function(acc, next) result(update)
      type(ESMF_Array) :: update
      type(ESMF_Array), intent(in) :: acc
      type(ESMF_Array), intent(in) :: next
   end function min_function

   function get_fortran_ptr_R64(array) result(f_ptr)
      real(ESMF_R8), pointer :: f_ptr
      type(ESMF_Array), pointer, intent(in) :: array
   end function get_fortran_ptr_R64
      
   function get_fortran_ptr_R32(array) result(f_ptr)
      real(ESMF_R4), pointer :: f_ptr
      type(ESMF_Array), pointer, intent(in) :: array
   end function get_fortran_ptr_R32
      
   subroutine get_couple_function(analysis_type, funct, rc)
   end subroutine get_couple_function

   subroutine get_analysis_function(analysis_type, func, rc)
      integer(ANALYSIS_KIND), intent(in) :: analysis_type
      procedure, pointer, intent(out) :: func
      integer, optional, intent(out) :: rc
      integer :: status
      
      select case(analysis_type)
         case (ANALYSIS_ACCUMULATE)
            func => accumulate_function
         case (ANALYSIS_MEAN)
            func => mean_function
         case (ANALYSIS_MAX)
            func => max_function
         case (ANALYSIS_MIN)
            func => min_function
         case default
            _FAIL('Unknown analysis function')
      end select
   end subroutine get_analysis_function


end module mapl3g_TimeAnalysisCouplerComponent
