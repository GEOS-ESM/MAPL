#define TYPE_ real(kind=ESMF_KIND_R4)
#define UT_ R4
#define LT_ r4
#define FMT_ '(G17.8)'
#define TYPESTRING_ 'UT_'

module hconfig_r4

   use esmf, only: ESMF_HConfigAsUT_, ESMF_KIND_UT_
#include "hconfig_value_declarations.h"

contains

#include "hconfig_value_procedures.h"

end module hconfig_r4
