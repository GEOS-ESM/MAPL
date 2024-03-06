module hconfig_i4seq
#include "hconfig_preamble.h"
#define VTYPE integer(kind=ESMF_KIND_I4)
#define TYPESTR 'I4'
#define ESMF_HCONFIG_AS ESMF_HConfigAsI4Seq
#define GETFCT get_hconfig_i4_seq
#define IS_ARRAY
#include "hconfig_template.h"

end module hconfig_i4seq
