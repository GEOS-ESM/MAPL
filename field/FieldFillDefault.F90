module mapl3g_FieldFillDefault
   use iso_fortran_env, only: REAL32, REAL64
   implicit none(type, external)
   private

   public :: set_field_fill_defaults
   public :: get_field_fill_default_r4
   public :: get_field_fill_default_r8
   public :: initialize_field_fill_defaults
   public :: reset_field_fill_defaults

   ! Module-level singleton fill values.  A separate flag tracks whether the
   ! module has been explicitly initialized, so that get_field_fill_default_*
   ! can lazily self-initialize to sNaN on first use.  This allows FieldFill
   ! to work correctly even in test contexts that do not go through MaplFramework.
   logical :: is_initialized = .false.
   real(REAL32) :: fill_default_r4
   real(REAL64) :: fill_default_r8

contains

   ! Sets both defaults to sNaN.  Called explicitly by MaplFramework during
   ! startup, and implicitly (lazily) on first use of the getters.
   ! Individual values may subsequently be overridden via set_field_fill_defaults().
   subroutine initialize_field_fill_defaults()
      use, intrinsic :: ieee_arithmetic, only: ieee_value, ieee_signaling_nan
      fill_default_r4 = ieee_value(fill_default_r4, ieee_signaling_nan)
      fill_default_r8 = ieee_value(fill_default_r8, ieee_signaling_nan)
      is_initialized = .true.
   end subroutine initialize_field_fill_defaults

   ! Sets one or both singleton fill values.  Arguments are allocatable so
   ! callers can pass unallocated variables to leave a typekind unchanged.
   subroutine set_field_fill_defaults(r4, r8)
      real(REAL32), allocatable, optional, intent(in) :: r4
      real(REAL64), allocatable, optional, intent(in) :: r8
      if (.not. is_initialized) call initialize_field_fill_defaults()
      if (present(r4)) then
         if (allocated(r4)) fill_default_r4 = r4
      end if
      if (present(r8)) then
         if (allocated(r8)) fill_default_r8 = r8
      end if
   end subroutine set_field_fill_defaults

   function get_field_fill_default_r4() result(value)
      real(REAL32) :: value
      if (.not. is_initialized) call initialize_field_fill_defaults()
      value = fill_default_r4
   end function get_field_fill_default_r4

   function get_field_fill_default_r8() result(value)
      real(REAL64) :: value
      if (.not. is_initialized) call initialize_field_fill_defaults()
      value = fill_default_r8
   end function get_field_fill_default_r8

   ! Restores both defaults to sNaN.  Primarily intended for use in unit tests.
   subroutine reset_field_fill_defaults()
      call initialize_field_fill_defaults()
   end subroutine reset_field_fill_defaults

end module mapl3g_FieldFillDefault
