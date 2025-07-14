#include "MAPL_ErrLog.h"
module mapl3g_hconfig_get_private
   use mapl3g_hconfig_params
   use mapl3g_get_hconfig
   use mapl3g_generalized_equality
   use :: esmf, only: ESMF_MAXSTR
   use :: esmf, only: ESMF_KIND_R4, ESMF_KIND_R8, ESMF_KIND_I4, ESMF_KIND_I8
   use :: esmf, only: ESMF_HConfig, ESMF_HConfigIsDefined
   use mapl_ErrorHandling

   implicit none
   private
   public :: get_value, HConfigParams, DEFAULT_TAG, ELLIPSIS

   interface get_value
      procedure :: get_value_i4
      module procedure :: get_value_i8
      module procedure :: get_value_r4
      module procedure :: get_value_r8
      module procedure :: get_value_string
      module procedure :: get_value_logical
      module procedure :: get_value_i4seq
      module procedure :: get_value_i8seq
      module procedure :: get_value_r4seq
      module procedure :: get_value_r8seq
      module procedure :: get_value_logical_seq
   end interface get_value

   character(len=*), parameter :: DEFAULT_TAG = ' (default)'
   character(len=*), parameter :: ELLIPSIS = ', ...'
   integer, parameter :: MAX_NUM_ITEMS_OUTPUT = 3

contains

#define EDIT_DESC_I4 'G0'
#define EDIT_DESC_I8 'G0'
#define EDIT_DESC_R4 'G0.7'
#define EDIT_DESC_R8 'G0.16'
#define EDIT_DESC_L 'L1'
#define EDIT_DESC_CH 'A'

!============================= SCALAR VALUE TYPES ==============================
#if defined ISARRAY
#  undef ISARRAY
#endif

   subroutine get_value_i4(params, value, default, valuestring, rc )
      integer(kind=ESMF_KIND_I4), intent(inout) :: value
      integer(kind=ESMF_KIND_I4), optional, intent(in) :: default
      character(len=*), parameter :: typestring = 'I4'
      character(len=*), parameter :: edit_descriptor = EDIT_DESC_I4
#include "hconfig_get_value_template.h"
   end subroutine get_value_i4

   subroutine get_value_i8(params, value, default, valuestring, rc)
      integer(kind=ESMF_KIND_I8), intent(inout) :: value
      integer(kind=ESMF_KIND_I8), optional, intent(in) :: default
      character(len=*), parameter :: typestring = 'I8'
      character(len=*), parameter :: edit_descriptor = EDIT_DESC_I8
#include "hconfig_get_value_template.h"
   end subroutine get_value_i8

   subroutine get_value_r4(params, value, default, valuestring, rc)
      real(kind=ESMF_KIND_R4), intent(inout) :: value
      real(kind=ESMF_KIND_R4), optional, intent(in) :: default
      character(len=*), parameter :: typestring = 'R4'
      character(len=*), parameter :: edit_descriptor = EDIT_DESC_R4
#include "hconfig_get_value_template.h"
   end subroutine get_value_r4

   subroutine get_value_r8(params, value, default, valuestring, rc)
      real(kind=ESMF_KIND_R8), intent(inout) :: value
      real(kind=ESMF_KIND_R8), optional, intent(in) :: default
      character(len=*), parameter :: typestring = 'R8'
      character(len=*), parameter :: edit_descriptor = EDIT_DESC_R8
#include "hconfig_get_value_template.h"
   end subroutine get_value_r8

   subroutine get_value_string(params, value, default, valuestring, rc)
      character(len=:), allocatable, intent(inout) :: value
      character(len=*), optional, intent(in) :: default
      character(len=*), parameter :: typestring = 'CH'
      character(len=*), parameter :: edit_descriptor = EDIT_DESC_CH
#include "hconfig_get_value_template.h"
   end subroutine get_value_string

   subroutine get_value_logical(params, value, default, valuestring, rc)
      logical, intent(inout) :: value
      logical, optional, intent(in) :: default
      character(len=*), parameter :: typestring = 'L'
      character(len=*), parameter :: edit_descriptor = EDIT_DESC_L
#include "hconfig_get_value_template.h"
   end subroutine get_value_logical

!============== Scalar subroutines must appear above this line. ================

!============================= ARRAY VALUE TYPES ===============================
#define ISARRAY 1
!=============== Array subroutines must appear below this line. ================

   subroutine get_value_i4seq(params, value, default, valuestring, rc)
      integer(kind=ESMF_KIND_I4), dimension(:), allocatable, intent(inout) :: value
      integer(kind=ESMF_KIND_I4), dimension(:), optional, intent(in) :: default
      character(len=*), parameter :: typestring = 'I4'
      character(len=*), parameter :: edit_descriptor = EDIT_DESC_I4
#include "hconfig_get_value_template.h"
   end subroutine get_value_i4seq

   subroutine get_value_i8seq(params, value, default, valuestring, rc)
      integer(kind=ESMF_KIND_I8), dimension(:), allocatable, intent(inout) :: value
      integer(kind=ESMF_KIND_I8), dimension(:), optional, intent(in) :: default
      character(len=*), parameter :: typestring = 'I8'
      character(len=*), parameter :: edit_descriptor = EDIT_DESC_I8
#include "hconfig_get_value_template.h"
   end subroutine get_value_i8seq

   subroutine get_value_r4seq(params, value, default, valuestring, rc)
      real(kind=ESMF_KIND_R4), dimension(:), allocatable, intent(inout) :: value
      real(kind=ESMF_KIND_R4), dimension(:), optional, intent(in) :: default
      character(len=*), parameter :: typestring = 'R4'
      character(len=*), parameter :: edit_descriptor = EDIT_DESC_R4
#include "hconfig_get_value_template.h"
   end subroutine get_value_r4seq

   subroutine get_value_r8seq(params, value, default, valuestring, rc)
      real(kind=ESMF_KIND_R8), dimension(:), allocatable, intent(inout) :: value
      real(kind=ESMF_KIND_R8), dimension(:), optional, intent(in) :: default
      character(len=*), parameter :: typestring = 'R8'
      character(len=*), parameter :: edit_descriptor = EDIT_DESC_R8
#include "hconfig_get_value_template.h"
   end subroutine get_value_r8seq

   subroutine get_value_logical_seq(params, value, default, valuestring, rc)
      logical, dimension(:), allocatable, intent(inout) :: value
      logical, dimension(:), optional, intent(in) :: default
      character(len=*), parameter :: typestring = 'L'
      character(len=*), parameter :: edit_descriptor = EDIT_DESC_L
#include "hconfig_get_value_template.h"
   end subroutine get_value_logical_seq

   function make_fmt(descriptor) result(fmt)
      character(len=:), allocatable :: fmt
      character(len=*), intent(in) :: descriptor

      fmt =  '(*(' // descriptor // ':", "))'

   end function make_fmt

   subroutine get_value_with_args_i4(hconfig, label, value, default, logger, value_set, rc)
      type(ESMF_HConfig), intent(in) :: hconfig
      character(len=*), intent(in) :: label
      integer(kind=ESMF_KIND_I4), intent(out) :: value
      integer(kind=ESMF_KIND_I4), optional, intent(in) :: default
      class(Logger), pointer, optional, intent(in) :: logger
      logical, optional, intent(out) :: value_set
      integer, optional, intent(out) :: rc
   end subroutine get_value_with_args_i4

end module mapl3g_hconfig_get_private
