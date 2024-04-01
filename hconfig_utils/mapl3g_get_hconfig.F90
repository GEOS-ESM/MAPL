#include "MAPL_ErrLog.h"
module mapl3g_get_hconfig

   use mapl3g_hconfig_params
   use :: esmf, only: ESMF_KIND_R4, ESMF_KIND_R8, ESMF_KIND_I4, ESMF_KIND_I8
   use :: esmf, only: ESMF_HConfig, ESMF_HConfigAsString
   use :: esmf, only: ESMF_HConfigAsLogical, ESMF_HConfigAsLogicalSeq
   use :: esmf, only: ESMF_HConfigAsI4, ESMF_HConfigAsI4Seq 
   use :: esmf, only: ESMF_HConfigAsR4, ESMF_HConfigAsR4Seq
   use :: esmf, only: ESMF_HConfigAsI8, ESMF_HConfigAsI8Seq 
   use :: esmf, only: ESMF_HConfigAsR8, ESMF_HConfigAsR8Seq
   use mapl_ErrorHandling

   implicit none
   private

   public :: get_hconfig

   interface get_hconfig
      module procedure :: get_hconfig_as_i4
      module procedure :: get_hconfig_as_i8
      module procedure :: get_hconfig_as_r4
      module procedure :: get_hconfig_as_r8
      module procedure :: get_hconfig_as_logical
      module procedure :: get_hconfig_as_i4seq
      module procedure :: get_hconfig_as_i8seq
      module procedure :: get_hconfig_as_r4seq
      module procedure :: get_hconfig_as_r8seq
      module procedure :: get_hconfig_as_logical_seq
      module procedure :: get_hconfig_as_string
   end interface get_hconfig

contains

   subroutine get_hconfig_as_i4(value, params, rc)
      integer(kind=ESMF_KIND_I4), intent(out) :: value
      class(HConfigParams), intent(in) :: params
      integer, optional, intent(out) :: rc
      integer :: status

      value = ESMF_HConfigAsI4(params%hconfig, keyString=params%label, _RC)
      _RETURN(_SUCCESS)

   end subroutine get_hconfig_as_i4

   subroutine get_hconfig_as_i8(value, params, rc)
      integer(kind=ESMF_KIND_I8), intent(out) :: value
      class(HConfigParams), intent(in) :: params
      integer, optional, intent(out) :: rc
      integer :: status

      value = ESMF_HConfigAsI8(params%hconfig, keyString=params%label, _RC)
      _RETURN(_SUCCESS)

   end subroutine get_hconfig_as_i8

   subroutine get_hconfig_as_r4(value, params, rc)
      real(kind=ESMF_KIND_R4), intent(out) :: value
      class(HConfigParams), intent(in) :: params
      integer, optional, intent(out) :: rc
      integer :: status

      value = ESMF_HConfigAsR4(params%hconfig, keyString=params%label, _RC)
      _RETURN(_SUCCESS)

   end subroutine get_hconfig_as_r4
   
   subroutine get_hconfig_as_r8(value, params, rc)
      real(kind=ESMF_KIND_R8), intent(out) :: value
      class(HConfigParams), intent(in) :: params
      integer, optional, intent(out) :: rc
      integer :: status

      value = ESMF_HConfigAsR8(params%hconfig, keyString=params%label, _RC)
      _RETURN(_SUCCESS)

   end subroutine get_hconfig_as_r8

   subroutine get_hconfig_as_logical(value, params, rc)
      logical, intent(out) :: value
      class(HConfigParams), intent(in) :: params
      integer, optional, intent(out) :: rc
      integer :: status

      value = ESMF_HConfigAsLogical(params%hconfig, keyString=params%label, _RC)
      _RETURN(_SUCCESS)

   end subroutine get_hconfig_as_logical

   subroutine get_hconfig_as_string(value, params, rc)
      character(len=:), allocatable, intent(out) :: value
      class(HConfigParams), intent(in) :: params
      integer, optional, intent(out) :: rc
      integer :: status

      value = ESMF_HConfigAsString(params%hconfig, keyString=params%label, _RC)
      _RETURN(_SUCCESS)

   end subroutine get_hconfig_as_string

   subroutine get_hconfig_as_i4seq(value, params, rc)
      integer(kind=ESMF_KIND_I4), dimension(:), allocatable, intent(out) :: value
      class(HConfigParams), intent(in) :: params
      integer, optional, intent(out) :: rc
      integer :: status

      value = ESMF_HConfigAsI4Seq(params%hconfig, keyString=params%label, _RC)
      _RETURN(_SUCCESS)

   end subroutine get_hconfig_as_i4seq

   subroutine get_hconfig_as_i8seq(value, params, rc)
      integer(kind=ESMF_KIND_I8), dimension(:), allocatable, intent(out) :: value
      class(HConfigParams), intent(in) :: params
      integer, optional, intent(out) :: rc
      integer :: status

      value = ESMF_HConfigAsI8Seq(params%hconfig, keyString=params%label, _RC)
      _RETURN(_SUCCESS)

   end subroutine get_hconfig_as_i8seq

   subroutine get_hconfig_as_r4seq(value, params, rc)
      real(kind=ESMF_KIND_R4), dimension(:), allocatable, intent(out) :: value
      class(HConfigParams), intent(in) :: params
      integer, optional, intent(out) :: rc
      integer :: status

      value = ESMF_HConfigAsR4Seq(params%hconfig, keyString=params%label, _RC)
      _RETURN(_SUCCESS)

   end subroutine get_hconfig_as_r4seq
   
   subroutine get_hconfig_as_r8seq(value, params, rc)
      real(kind=ESMF_KIND_R8), dimension(:), allocatable, intent(out) :: value
      class(HConfigParams), intent(in) :: params
      integer, optional, intent(out) :: rc
      integer :: status

      value = ESMF_HConfigAsR8Seq(params%hconfig, keyString=params%label, _RC)
      _RETURN(_SUCCESS)

   end subroutine get_hconfig_as_r8seq

   subroutine get_hconfig_as_logical_seq(value, params, rc)
      logical, dimension(:), allocatable, intent(out) :: value
      class(HConfigParams), intent(in) :: params
      integer, optional, intent(out) :: rc
      integer :: status

      value = ESMF_HConfigAsLogicalSeq(params%hconfig, keyString=params%label, _RC)
      _RETURN(_SUCCESS)

   end subroutine get_hconfig_as_logical_seq

end module mapl3g_get_hconfig
