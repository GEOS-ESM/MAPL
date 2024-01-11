#include <stdlib.h>
#include <stdio.h>
#include "udunits2.h"

ut_error_message_handler ut_set_ignore_error_message_handler()
{
    return ut_set_error_message_handler(ut_ignore);
}
