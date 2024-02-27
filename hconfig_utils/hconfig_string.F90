module hconfig_string
#include "hconfig_preamble.h"
#define VTYPE character(len=*)
#define MTYPE character(len=:)
#define TFMT '(A)'
#define TYPESTR 'CH'
#define DTYPE HConfigValueString
#define ESMF_HCONFIG_AS ESMF_HConfigAsString
#include "hconfig_template.h"

end module hconfig_string
