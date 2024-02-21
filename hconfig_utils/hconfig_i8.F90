#define TYPE_ integer(kind=ESMF_KIND_I8)
#define UT_ I4
#define LT_ i4
#define FMT_ '(I22)'
#define TYPESTRING_ 'UT_'

module hconfig_i8

   use esmf, only: ESMF_HConfigAsUT_, ESMF_KIND_UT_
#include "hconfig_value_declarations.h"

contains

#include "hconfig_value_procedures.h"

end module hconfig_i8
