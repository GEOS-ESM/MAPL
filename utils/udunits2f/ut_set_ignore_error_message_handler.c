#include <stdlib.h>
#include <stdio.h>
#include "udunits2.h"

/* Helper function to augment udunits2 error handling
 * Sets the udunits2 error handler to ut_ignore
 * which disables error messages from udunits2
 * udunits2 requires a ut_error_message_handler be passed
 * into ut_set_error_message_handler to change the error handler,
 * and ut_error_message_handler is a function with a variadic list
 * of arguments, which is not possible in Fortran.
*/
ut_error_message_handler ut_set_ignore_error_message_handler()
{
    return ut_set_error_message_handler(ut_ignore);
}
