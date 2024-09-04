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


   type :: TimeAnalysisField
      type(VarSpec), pointer :: source
      type(VarSpec), pointer :: destination
      type(ESMF_Alarm), pointer   :: time_to_clear
      type(ESMF_Alarm), pointer   :: time_to_couple
      type(ESMF_Array), pointer   :: accumulator
      type(ESMF_Array), allocatable :: count
      integer(ESMF_I8) :: scalar_count
      integer :: clear_interval
      integer :: couple_interval
      integer, allocatable :: shape(:)
      integer(ANALYSIS_KIND) :: coupler_type
      procedure(AnalysisFunction), pointer :: func => null()
   end type TimeAnalysisField

   type :: TimeAnalysisInternal
      type(ESMF_Field), allocatable :: analysis_fields(:)
      logical :: active
      type (ESMF_Alarm), pointer   :: time2cpl_alarm => null()
      character(LEN=ESMF_MAXSTR)   :: name
   end type TimeAnalysisInternal

   abstract interface
      function AnalysisFunction(acc, next) result(update)
         type(ESMF_Array) :: update
         type(ESMF_Array), intent(in) :: acc
         type(ESMF_Array), intent(in) :: next
      end function AnalysisFunction
   end interface

   integer(kind=c_int), parameter :: ANALYSIS_KIND = kind(ANALYSIS_UNKNOWN)

contains

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
      
   logical function is_scalar(this)
      class(MAPL_CplCnt), intent(in) :: this

      is_scalar = this%is_scalar_

   end function is_scalar

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
