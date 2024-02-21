#define TYPE_ character(len=*)
#define UT_ String
#define LT_ string
#define FMT_ '(A)'
#define TYPESTRING_ 'CH'

module hconfig_string

   use esmf, only: ESMF_HConfigAsString
#include "hconfig_value_declarations.h"

contains

#include "hconfig_value_procedures.h"

end module hconfig_string
