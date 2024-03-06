module hconfig_i8seq
#include "hconfig_preamble.h"
#define VTYPE integer(kind=ESMF_KIND_I8)
#define TYPESTR 'I8'
#define ESMF_HCONFIG_AS ESMF_HConfigAsI8Seq
#define GETFCT get_hconfig_i8_seq
#define IS_ARRAY
#include "hconfig_template.h"

end module hconfig_i8seq
