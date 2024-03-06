module hconfig_logical
#include "hconfig_preamble.h"
#define VTYPE logical
#define TYPESTR 'L'
#define RELOPR .eqv.
#define ESMF_HCONFIG_AS ESMF_HConfigAsLogical
#define HCONFIG_GET get_hconfig_logical
#include "hconfig_template.h"

end module hconfig_logical
