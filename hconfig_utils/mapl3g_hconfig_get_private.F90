#include "MAPL_ErrLog.h"
#include "mapl3g_hconfig_valuetype_macros.h"
module mapl3g_hconfig_get_private
   !wdb Could this be submodule(d)? Yes. todo
   !wdb todo For submodule, define interfaces with arguments below via template.
   !wdb todo Then, implement the subroutines in a submodule via another template.
   !wdb todo Macros are in declarations except RELATION, ESMF_HCONFIG_AS and possibly TYPESTRING_
   use :: esmf, only: ESMF_HConfig, ESMF_HConfigIsDefined, MAXSTRLEN => ESMF_MAXSTR
   use :: esmf, only: ESMF_KIND_R4, ESMF_KIND_R8, ESMF_KIND_I4, ESMF_KIND_I8
   use :: esmf, only: ESMF_HConfigAsI4, ESMF_HConfigAsI4Seq, ESMF_HConfigAsString

   use :: pflogger, only: logger_t => logger
   use mapl_KeywordEnforcer
   use mapl_ErrorHandling

   implicit none
   private
   public :: get_value

   interface get_value
      module procedure :: get_value_scalar
      module procedure :: get_value_array
   end interface get_value

contains

   subroutine get_value_scalar (hconfig, value, label, unusable, default, valueset, logger, rc)
      type(ESMF_HConfig), intent(in) :: hconfig
      class(*), intent(inout) :: value
      character(len=*), intent(in) :: label
      class(KeywordEnforcer), optional, intent(in) :: unusable
      class(*), optional, intent(in) :: default
      logical, optional, intent(out) :: valueset
      class(Logger_t), optional, intent(inout) :: logger
      integer, optional, intent(out) :: rc
      integer :: status
      logical :: found = .FALSE.
      logical :: value_equals_default = .FALSE.
      character(len=:), allocatable :: typestring
      character(len=:), allocatable :: valuestring

      if(present(default)) then
         _ASSERT(same_type_as(value, default), 'value and default are not the same type.')
      end if
      if(present(valueset)) valueset = .FALSE.
      found = ESMF_HConfigIsDefined(hconfig, keyString=label, _RC)
      if(.not. present(valueset)) status = _FAILURE
      if(present(rc)) rc = status
      if(.not. (found .or. present(default))) return
      ! At this point, either the label was found or default is present.
      
      select type(value)
      type is (integer(kind=ESMF_KIND_I4))
         typestring = 'I4'
         call get_i4(hconfig, found, label, value, valuestring, value_equals_default, default=default, _RC)
      type is (character(len=*))
         typestring = 'CH'
         call get_string(hconfig, found, label, value, valuestring, value_equals_default, default=default, _RC)
      class default
         _FAIL('unrecognized type') !wdb todo better message
      end select
      if(present(valueset)) valueset = .TRUE.
      ! If there is no logger, can return now.
      _RETURN_UNLESS(present(logger))

      call logger%info(typestring //' '// label //' = '// valuestring)
      
      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)

   end subroutine get_value_scalar

   subroutine get_value_array (hconfig, value, label, unusable, default, valueset, logger, rc)
      type(ESMF_HConfig), intent(in) :: hconfig
      class(*), allocatable, intent(inout) :: value(:)
      character(len=*), intent(in) :: label
      class(KeywordEnforcer), optional, intent(in) :: unusable
      class(*), optional, intent(in) :: default(:)
      logical, optional, intent(out) :: valueset
      class(Logger_t), optional, intent(inout) :: logger
      integer, optional, intent(out) :: rc
      integer :: status
      logical :: found = .FALSE.
      logical :: value_equals_default = .FALSE.
      character(len=:), allocatable :: typestring
      character(len=:), allocatable :: valuestring

      if(present(default)) then
         _ASSERT(same_type_as(value, default), 'value and default are not the same type.')
      end if
      if(present(valueset)) valueset = .FALSE.
      found = ESMF_HConfigIsDefined(hconfig, keyString=label, _RC)
      if(.not. present(valueset)) status = _FAILURE
      if(present(rc)) rc = status
      if(.not. (found .or. present(default))) return
      ! At this point, either the label was found or default is present.
      
      select type(value)
      type is (integer(kind=ESMF_KIND_I4))
         typestring = 'I4'
         call get_i4seq(hconfig, found, label, value, valuestring, value_equals_default, default=default, _RC)
      class default
         _FAIL('unrecognized type') !wdb todo better message
      end select
      if(present(valueset)) valueset = .TRUE.
      ! If there is no logger, can return now.
      _RETURN_UNLESS(present(logger))

      call logger%info(typestring //' '// label //' = '// valuestring)
      
      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)

   end subroutine get_value_array

#define TYPENUM TYPEI4
#define SUBROUTINE_NAME get_i4
#include "mapl3g_hconfig_get_value_template.h"
#undef TYPENUM
#undef SUBROUTINE_NAME

#define TYPENUM TYPEI4SEQ
#define SUBROUTINE_NAME get_i4seq
#include "mapl3g_hconfig_get_value_template.h"
#undef TYPENUM
#undef SUBROUTINE_NAME

#define TYPENUM TYPECH
#define SUBROUTINE_NAME get_string
#include "mapl3g_hconfig_get_value_template.h"
#undef TYPENUM
#undef SUBROUTINE_NAME

end module mapl3g_hconfig_get_private
