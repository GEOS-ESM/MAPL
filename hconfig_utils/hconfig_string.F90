module hconfig_string
#include "hconfig_preamble.h"
#define VTYPE character(len=*)
#define MTYPE character(len=:)
#define TFMT '(A)'
#define TYPESTR 'CH'
#define DTYPE HConfigValueString
#define ESMF_HCONFIG_AS ESMF_HConfigAsString
#define WRITE_STATEMENT(RW, FT, ST, V) raw = this%value_ptr; ST = 0
#include "hconfig_template.h"

end module hconfig_string
