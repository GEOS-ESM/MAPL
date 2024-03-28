#include "MAPL_ErrLog.h"
module mapl3g_hconfig_get_private
   use mapl3g_hconfig_params
   use :: esmf, only: ESMF_MAXSTR
   use :: esmf, only: ESMF_KIND_R4, ESMF_KIND_R8, ESMF_KIND_I4, ESMF_KIND_I8
   use :: esmf, only: ESMF_HConfig, ESMF_HConfigIsDefined, ESMF_HConfigAsString
   use :: esmf, only: ESMF_HConfigAsLogical, ESMF_HConfigAsLogicalSeq
   use :: esmf, only: ESMF_HConfigAsI4, ESMF_HConfigAsI4Seq 
   use :: esmf, only: ESMF_HConfigAsR4, ESMF_HConfigAsR4Seq
   use :: esmf, only: ESMF_HConfigAsI8, ESMF_HConfigAsI8Seq 
   use :: esmf, only: ESMF_HConfigAsR8, ESMF_HConfigAsR8Seq
   use mapl_ErrorHandling

   implicit none
   private
   public :: get_value, HConfigParams

   interface get_value
      module procedure :: get_value_i4
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

contains

!============================= INITIALIZE MACROS ===============================
#if defined FMT_
#  undef FMT_
#endif
#define FMT_ 'G0:", "'

#if defined ESMF_HCONFIG_AS
#   undef ESMF_HCONFIG_AS
#endif

#if defined RELATION
#   undef RELATION
#endif
!===============================================================================


!======================= SCALAR VALUES (except logical) ========================
#define RELATION(A, B) A==B
#define ESMF_HCONFIG_AS ESMF_HConfigAsI4
   subroutine get_value_i4(params, value, default, valuestring_out, rc )
      integer(kind=ESMF_KIND_I4), intent(inout) :: value
      integer(kind=ESMF_KIND_I4), optional, intent(in) :: default
      character(len=*), parameter :: typestring = 'I4'
#include "mapl3g_hconfig_get_value_template.h"
   end subroutine get_value_i4
#undef ESMF_HCONFIG_AS

#define ESMF_HCONFIG_AS ESMF_HConfigAsI8
   subroutine get_value_i8(params, value, default, valuestring_out, rc)
      integer(kind=ESMF_KIND_I8), intent(inout) :: value
      integer(kind=ESMF_KIND_I8), optional, intent(in) :: default
      character(len=*), parameter :: typestring = 'I8'
#include "mapl3g_hconfig_get_value_template.h"
   end subroutine get_value_i8
#undef ESMF_HCONFIG_AS

#define ESMF_HCONFIG_AS ESMF_HConfigAsR4
   subroutine get_value_r4(params, value, default, valuestring_out, rc)
      real(kind=ESMF_KIND_R4), intent(inout) :: value
      real(kind=ESMF_KIND_R4), optional, intent(in) :: default
      character(len=*), parameter :: typestring = 'R4'
#include "mapl3g_hconfig_get_value_template.h"
   end subroutine get_value_r4
#undef ESMF_HCONFIG_AS

#define ESMF_HCONFIG_AS ESMF_HConfigAsR8
   subroutine get_value_r8(params, value, default, valuestring_out, rc)
      real(kind=ESMF_KIND_R8), intent(inout) :: value
      real(kind=ESMF_KIND_R8), optional, intent(in) :: default
      character(len=*), parameter :: typestring = 'R8'
#include "mapl3g_hconfig_get_value_template.h"
   end subroutine get_value_r8
#undef ESMF_HCONFIG_AS

#define ESMF_HCONFIG_AS ESMF_HConfigAsString
   subroutine get_value_string(params, value, default, valuestring_out, rc)
      character(len=:), allocatable, intent(inout) :: value
      character(len=*), optional, intent(in) :: default
      character(len=*), parameter :: typestring = 'CH'
#include "mapl3g_hconfig_get_value_template.h"
   end subroutine get_value_string
#undef ESMF_HCONFIG_AS
!===============================================================================


!========================== SCALAR VALUES (logical) ============================
#define RELATION(A, B) A.eqv.B
#define ESMF_HCONFIG_AS ESMF_HConfigAsLogical
   subroutine get_value_logical(params, value, default, valuestring_out, rc)
      logical, intent(inout) :: value
      logical, optional, intent(in) :: default
      character(len=*), parameter :: typestring = 'L'
#include "mapl3g_hconfig_get_value_template.h"
   end subroutine get_value_logical
#undef ESMF_HCONFIG_AS
#undef RELATION
!===============================================================================


!==================== ARRAY VALUES (except logical array) ======================
#define RELATION(A, B) all(A==B)
#define ESMF_HCONFIG_AS ESMF_HConfigAsI4Seq
   subroutine get_value_i4seq(params, value, default, valuestring_out, rc)
      integer(kind=ESMF_KIND_I4), dimension(:), allocatable, intent(inout) :: value
      integer(kind=ESMF_KIND_I4), dimension(:), optional, intent(in) :: default
      character(len=*), parameter :: typestring = 'I4'
#include "mapl3g_hconfig_get_value_template.h"
   end subroutine get_value_i4seq
#undef ESMF_HCONFIG_AS

#define ESMF_HCONFIG_AS ESMF_HConfigAsI8Seq
   subroutine get_value_i8seq(params, value, default, valuestring_out, rc)
      integer(kind=ESMF_KIND_I8), dimension(:), allocatable, intent(inout) :: value
      integer(kind=ESMF_KIND_I8), dimension(:), optional, intent(in) :: default
      character(len=*), parameter :: typestring = 'I8'
#include "mapl3g_hconfig_get_value_template.h"
   end subroutine get_value_i8seq
#undef ESMF_HCONFIG_AS

#define ESMF_HCONFIG_AS ESMF_HConfigAsR4Seq
   subroutine get_value_r4seq(params, value, default, valuestring_out, rc)
      real(kind=ESMF_KIND_R4), dimension(:), allocatable, intent(inout) :: value
      real(kind=ESMF_KIND_R4), dimension(:), optional, intent(in) :: default
      character(len=*), parameter :: typestring = 'R4'
#include "mapl3g_hconfig_get_value_template.h"
   end subroutine get_value_r4seq
#undef ESMF_HCONFIG_AS

#define ESMF_HCONFIG_AS ESMF_HConfigAsR8Seq
   subroutine get_value_r8seq(params, value, default, valuestring_out, rc)
      real(kind=ESMF_KIND_R8), dimension(:), allocatable, intent(inout) :: value
      real(kind=ESMF_KIND_R8), dimension(:), optional, intent(in) :: default
      character(len=*), parameter :: typestring = 'R8'
#include "mapl3g_hconfig_get_value_template.h"
   end subroutine get_value_r8seq
#undef ESMF_HCONFIG_AS
!===============================================================================


!======================== ARRAY VALUES (logical array) =========================
#define RELATION(A, B) all(A.eqv.B)
#define ESMF_HCONFIG_AS ESMF_HConfigAsLogicalSeq
   subroutine get_value_logical_seq(params, value, default, valuestring_out, rc)
      logical, dimension(:), allocatable, intent(inout) :: value
      logical, optional, intent(in) :: default
      character(len=*), parameter :: typestring = 'L'
#include "mapl3g_hconfig_get_value_template.h"
   end subroutine get_value_logical_seq
#undef ESMF_HCONFIG_AS
#undef RELATION
!===============================================================================

end module mapl3g_hconfig_get_private
