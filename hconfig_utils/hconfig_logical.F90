module hconfig_logical
#include "hconfig_preamble.h"
#define VTYPE logical
#define TFMT '(L1)'
#define TYPESTR 'L'
#define DTYPE HConfigValueLogical
#define RELOPR .eqv.
#define ESMF_HCONFIG_AS ESMF_HConfigAsLogical
#include "hconfig_template.h"

end module hconfig_logical
