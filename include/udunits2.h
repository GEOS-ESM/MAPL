#ifndef UT_UNITS2_H_INCLUDED
#define UT_UNITS2_H_INCLUDED
#endif

#include <stdarg.h>
#include <stddef.h>

#define _USE_MATH_DEFINES

#ifndef EXTERNL
#   define EXTERNL extern
#endif

typedef int (*ut_error_message_handler)(const char* fmt, va_list args);

/*
 * Returns the previously-installed error-message handler and optionally
 * installs a new handler.  The initial handler is "ut_write_to_stderr()".
 *
 * Arguments:
 *      handler		NULL or pointer to the error-message handler.  If NULL,
 *			then the handler is not changed.  The
 *			currently-installed handler can be obtained this way.
 * Returns:
 *	Pointer to the previously-installed error-message handler.
 */
EXTERNL ut_error_message_handler
ut_set_error_message_handler(
    ut_error_message_handler	handler);

/*
 * Does nothing with an error-message.
 *
 * Arguments:
 *	fmt	The format for the error-message.
 *	args	The arguments of "fmt".
 * Returns:
 *	0	Always.
 */
EXTERNL int
ut_ignore(
    const char* const	fmt,
    va_list		args);

/*
 * Sets error message handler ot ut_ignore
 */
EXTERNL ut_error_message_handler ut_set_ignore_error_message_handler();
