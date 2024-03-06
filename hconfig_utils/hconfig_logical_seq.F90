module hconfig_logical_seq
#include "hconfig_preamble.h"
#define VTYPE logical
#define TYPESTR 'L'
#define RELOPR .eqv.
#define ESMF_HCONFIG_AS ESMF_HConfigAsLogicalSeq
#define GETFCT get_hconfig_logical_seq
#define IS_ARRAY
#include "hconfig_template.h"

end module hconfig_logical_seq
