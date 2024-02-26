#define TYPE_ integer(kind=ESMF_KIND_I4)
#define UT_ I4
#define LT_ i4
#define FMT_ '(I12)'
#define TYPESTRING_ 'UT_'

module hconfig_i4

   use esmf, only: ESMF_HConfigAsI4, ESMF_KIND_I4
#include "hconfig_value_declarations.h"

contains

#include "hconfig_value_procedures.h"

end module hconfig_i4
