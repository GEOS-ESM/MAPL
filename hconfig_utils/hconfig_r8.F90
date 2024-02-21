#define TYPE_ real(kind=ESMF_KIND_R8)
#define UT_ R8
#define LT_ r8
#define FMT_ '(G24.16)'
#define TYPESTRING_ 'UT_'

module hconfig_r8

   use esmf, only: ESMF_HConfigAsUT_, ESMF_KIND_UT_
#include "hconfig_value_declarations.h"

contains

#include "hconfig_value_procedures.h"

end module hconfig_r8
