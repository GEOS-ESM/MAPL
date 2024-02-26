module hconfig_string

#define VTYPE character(len=*)
#define TFMT '(A)'
#define TYPESTR 'CH'
#define DTYPE HConfigValueString
#define ESMF_HCONFIG_AS_ ESMF_HConfigAsString
#include "hconfig_template.h"

end module hconfig_string
