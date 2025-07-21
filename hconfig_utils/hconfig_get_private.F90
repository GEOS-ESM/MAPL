#include "MAPL_ErrLog.h"
module mapl3g_hconfig_get_private
   use mapl3g_hconfig_params
   use mapl3g_get_hconfig
   use mapl3g_generalized_equality
   use :: esmf, only: ESMF_MAXSTR
   use :: esmf, only: ESMF_KIND_R4, ESMF_KIND_R8, ESMF_KIND_I4, ESMF_KIND_I8
   use :: esmf, only: ESMF_HConfig, ESMF_HConfigIsDefined
   use :: esmf, only: ESMF_HConfigAsI4, ESMF_HConfigAsI8
   use :: esmf, only: ESMF_HConfigAsR4, ESMF_HConfigAsR8
   use :: esmf, only: ESMF_HConfigAsLogical, ESMF_HConfigAsString
   use :: esmf, only: ESMF_HConfigAsI4Seq, ESMF_HConfigAsI8Seq
   use :: esmf, only: ESMF_HConfigAsR4Seq, ESMF_HConfigAsR8Seq
   use :: esmf, only: ESMF_HConfigAsLogicalSeq
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

#define _SUB_ mapl_get_value_I4
#define _FTYPE_ integer(kind=ESMF_KIND_I4)
#define _ESMF_FUNC_ ESMF_HConfigAsI4
#define _TYPESTRING_ "I4"
#define _EDIT_DESCRIPTOR_ "G0"
#include "mapl_hconfig_get_value_template.h"

#define _SUB_ mapl_get_value_I8
#define _FTYPE_ integer(kind=ESMF_KIND_I8)
#define _ESMF_FUNC_ ESMF_HConfigAsI8
#define _TYPESTRING_ "I8"
#define _EDIT_DESCRIPTOR_ "G0"
#include "mapl_hconfig_get_value_template.h"

#define _SUB_ mapl_get_value_R4
#define _FTYPE_ real(kind=ESMF_KIND_R4)
#define _ESMF_FUNC_ ESMF_HConfigAsR4
#define _TYPESTRING_ "R4"
#define _EDIT_DESCRIPTOR_ "G0.7"
#include "mapl_hconfig_get_value_template.h"

#define _SUB_ mapl_get_value_R8
#define _FTYPE_ real(kind=ESMF_KIND_R8)
#define _ESMF_FUNC_ ESMF_HConfigAsR8
#define _TYPESTRING_ "R8"
#define _EDIT_DESCRIPTOR_ "G0.16"
#include "mapl_hconfig_get_value_template.h"

#define _SUB_ mapl_get_value_Logical
#define _FTYPE_ logical
#define _ESMF_FUNC_ ESMF_HConfigAsLogical
#define _TYPESTRING_ "L"
#define _EDIT_DESCRIPTOR_ "L1"
#define _RELATION_ .eqv.
#include "mapl_hconfig_get_value_template.h"

#define _SUB_ mapl_get_value_String
#define _FTYPE_ character(len=*)
#define _ESMF_FUNC_ ESMF_HConfigAsString
#define _TYPESTRING_ "CH"
#define _EDIT_DESCRIPTOR_ "A"
#include "mapl_hconfig_get_value_template.h"

#define _SUB_ mapl_get_value_I4Seq
#define _FTYPE_ integer(kind=ESMF_KIND_I4)
#define _ESMF_FUNC_ ESMF_HConfigAsI4Seq
#define _TYPESTRING_ "I4"
#define _EDIT_DESCRIPTOR_ "G0"
#define _ARRAY_ 1
#include "mapl_hconfig_get_value_template.h"

#define _SUB_ mapl_get_value_I8Seq
#define _FTYPE_ integer(kind=ESMF_KIND_I8)
#define _ESMF_FUNC_ ESMF_HConfigAsI8Seq
#define _TYPESTRING_ "I8"
#define _EDIT_DESCRIPTOR_ "G0"
#define _ARRAY_ 1
#include "mapl_hconfig_get_value_template.h"

#define _SUB_ mapl_get_value_R4Seq
#define _FTYPE_ real(kind=ESMF_KIND_R4)
#define _ESMF_FUNC_ ESMF_HConfigAsR4Seq
#define _TYPESTRING_ "R4"
#define _EDIT_DESCRIPTOR_ "G0.7"
#define _ARRAY_ 1
#include "mapl_hconfig_get_value_template.h"

#define _SUB_ mapl_get_value_R8Seq
#define _FTYPE_ real(kind=ESMF_KIND_R8)
#define _ESMF_FUNC_ ESMF_HConfigAsR8Seq
#define _TYPESTRING_ "R8"
#define _EDIT_DESCRIPTOR_ "G0.16"
#define _ARRAY_ 1
#include "mapl_hconfig_get_value_template.h"

#define _SUB_ mapl_get_value_LogicalSeq
#define _FTYPE_ logical
#define _ESMF_FUNC_ ESMF_HConfigAsLogicalSeq
#define _TYPESTRING_ "L"
#define _EDIT_DESCRIPTOR_ "L1"
#define _ARRAY_ 1
#define _RELATION_ .eqv.
#include "mapl_hconfig_get_value_template.h"

end module mapl3g_hconfig_get_private
