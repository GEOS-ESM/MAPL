module hconfig_string
#include "hconfig_preamble.h"
#define VTYPE character(len=*)
#define MTYPE character(len=:)
#define TYPESTR 'CH'
#define DTYPE HConfigValueString
#define ESMF_HCONFIG_AS ESMF_HConfigAsString
#define WRITE_STATEMENT(C, F, S, V) C = '"' // trim(V) // '"'; S = 0
#include "hconfig_template.h"

end module hconfig_string
