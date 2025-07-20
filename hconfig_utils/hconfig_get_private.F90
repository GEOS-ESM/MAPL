#include "MAPL_ErrLog.h"
module mapl3g_hconfig_get_private
   use mapl3g_hconfig_params
   use mapl3g_get_hconfig
   use mapl3g_generalized_equality
   use :: esmf, only: ESMF_MAXSTR
   use :: esmf, only: ESMF_KIND_R4, ESMF_KIND_R8, ESMF_KIND_I4, ESMF_KIND_I8
   use :: esmf, only: ESMF_HConfig, ESMF_HConfigIsDefined
   use mapl_ErrorHandling
   use :: pflogger, only: Logger_t => Logger

   implicit none
   private
   public :: HConfigParams, DEFAULT_TAG, ELLIPSIS
   public :: mapl_get_value_i4
   public :: mapl_get_value_i8
   public :: mapl_get_value_r4
   public :: mapl_get_value_r8
   public :: mapl_get_value_string
   public :: mapl_get_value_logical
   public :: mapl_get_value_i4seq
   public :: mapl_get_value_i8seq
   public :: mapl_get_value_r4seq
   public :: mapl_get_value_r8seq
   public :: mapl_get_value_logicalseq
 
   interface get_value
      procedure :: get_value_i4
      procedure :: get_value_i8
      procedure :: get_value_r4
      procedure :: get_value_r8
      procedure :: get_value_string
      procedure :: get_value_logical
      procedure :: get_value_i4seq
      procedure :: get_value_i8seq
      procedure :: get_value_r4seq
      procedure :: get_value_r8seq
      procedure :: get_value_logical_seq
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

   subroutine get_value_i4(params, value, default, valuestring, rc)
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

#include "mapl_hconfig_macros.h"

#define _SUB_ mapl_get_value_i4
#define _TYPE_ _INT_
#define _KIND_ 4
#include "mapl_hconfig_get_value_template.h"

#define _SUB mapl_get_value_i8
#define _TYPE_ _INT_
#define _KIND_ 8
#include "mapl_hconfig_get_value_template.h"

#define _SUB mapl_get_value_r4
#define _TYPE_ _REAL_
#define _KIND_ 4
#include "mapl_hconfig_get_value_template.h"

#define _SUB mapl_get_value_r8
#define _TYPE_ _REAL_
#define _KIND_ 8
#include "mapl_hconfig_get_value_template.h"

#define _SUB mapl_get_value_logical
#define _TYPE_ _LOGICAL_
#include "mapl_hconfig_get_value_template.h"

#define _SUB mapl_get_value_string
#define _TYPE_ _STRING_
#include "mapl_hconfig_get_value_template.h"

#define _SUB mapl_get_value_i4seq
#define _TYPE_ _INT_
#define _KIND_ 4
#define _ARRAY_ 1
#include "mapl_hconfig_get_value_template.h"

#define _SUB_ mapl_get_value_i8seq
#define _TYPE_ _INT_
#define _KIND_ 8
#define _ARRAY_ 1
#include "mapl_hconfig_get_value_template.h"

#define _SUB_ mapl_get_value_r4seq
#define _TYPE_ _REAL_
#define _KIND_ 4
#define _ARRAY_ 1
#include "mapl_hconfig_get_value_template.h"

#define _SUB_ mapl_get_value_r8seq
#define _TYPE_ _REAL_
#define _KIND_ 8
#define _ARRAY_ 1
#include "mapl_hconfig_get_value_template.h"

#define _SUB_ mapl_get_value_logicalseq
#define _TYPE_ _LOGICAL_
#define _ARRAY_ 1
#include "mapl_hconfig_get_value_template.h"

#include "mapl_hconfig_macros_undef.h"
end module mapl3g_hconfig_get_private
