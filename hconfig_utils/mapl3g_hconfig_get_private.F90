#include "MAPL_ErrLog.h"
#include "mapl3g_hconfig_valuetype_macros.h"
module mapl3g_hconfig_get_private
   !wdb Could this be submodule(d)? Yes. todo
   !wdb todo For submodule, define interfaces with arguments below via template.
   !wdb todo Then, implement the subroutines in a submodule via another template.
   !wdb todo Macros are in declarations except RELATION, ESMF_HCONFIG_AS and possibly TYPESTRING_
   use :: esmf, only: ESMF_HConfig, ESMF_HConfigIsDefined, MAXSTRLEN => ESMF_MAXSTR
   use :: esmf, only: ESMF_KIND_R4, ESMF_KIND_R8, ESMF_KIND_I4, ESMF_KIND_I8
   use :: esmf, only: ESMF_HConfigAsI4, ESMF_HConfigAsI4Seq
   use :: pflogger, only: logger_t => logger
   use mapl_KeywordEnforcer
   use mapl_ErrorHandling

   implicit none
   private
   public :: get_value

   interface get_value
      module procedure :: get_value_scalar
      module procedure :: get_value_array
      module procedure :: get_value_string
   end interface get_value

   interface get_by_type
      module procedure :: get_i4 
      module procedure :: get_i4seq
   end interface get_by_type

contains

#define PRIVATE_GET_VALUE_ get_value_scalar
#define VALTYPEDIMS ,
#define DEFTYPEDIMS VALTYPEDIMS
#include "mapl3g_hconfig_get_private_template.h"
#undef PRIVATE_GET_VALUE_
#undef VALTYPEDIMS
#undef DEFTYPEDIMS

#define PRIVATE_GET_VALUE_ get_value_array
#define DEFTYPEDIMS , dimension(:),
#define VALTYPEDIMS DEFTYPEDIMS, allocatable,
#include "mapl3g_hconfig_get_private_template.h"
#undef PRIVATE_GET_VALUE_
#undef VALTYPEDIMS
#undef DEFTYPEDIMS

#define PRIVATE_GET_VALUE_ get_value_string
#define DEFTYPEDIMS , dimension(*),
#define VALTYPEDIMS , dimension(:), allocatable,
#include "mapl3g_hconfig_get_private_template.h"
#undef PRIVATE_GET_VALUE_
#undef VALTYPEDIMS
#undef DEFTYPEDIMS

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

end module mapl3g_hconfig_get_private
