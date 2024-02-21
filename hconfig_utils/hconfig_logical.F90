#define TYPE_ logical
#define UT_ Logical
#define LT_ logical
#define FMT_ '(L1)'
#define TYPESTRING_ 'L'

module hconfig_logical

   use esmf, only: ESMF_HConfigAsLogical
#include "hconfig_value_declarations.h"

contains

#include "hconfig_value_procedures.h"

end module hconfig_logical
