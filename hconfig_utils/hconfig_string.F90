module hconfig_string
#include "hconfig_preamble.h"
#define IS_STRING
#define TYPESTR 'CH'
#define ESMF_HCONFIG_AS ESMF_HConfigAsString
#define GETFCT get_hconfig_string
#include "hconfig_template.h"

end module hconfig_string
