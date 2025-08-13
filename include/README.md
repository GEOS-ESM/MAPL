# Introduction

MAPL provides some error handling macros (defined via the preprocessor) that can make your Fortran code cleaner. The these are used in cases when your procedure (a subroutine or function), either a procedure you are creating or a procedure you are calling provides an optional return code like so:

```fortran
subroutine foo(...,rc)
integer, intent(out), optional :: rc

! if something goes wrong
if (present(rc)) then
   rc = bad_return_value
   return
end if

! and at the end
if (present(rc)) then
   rc = good_return_value
end if
```

and when calling foo you would want to do something like this:

```fortran
integer :: status

call foo(...,rc=status)
if (status /= good_return_value) then
! do something here, maybe report where I am to standard out
! and return rather than continue execution of this procedure
end if
```

Note that most MAPL and all ESMF procedures have optional return codes. In addition, other libraries also provide these such as MPI to name one. If you had to write code that in every place that would get tedious fast. As a service to uses MAPL defines a slew of macros to automate this boilerplate code and provide different behaviours and more informative error trapping. 

## A Note About "good" vs "bad" Return Values

Generally most libraries that use integer return codes have the convention that 0 is "success" and anything not 0 is a "failure". In some libraries the non-zero return codes have specific meanings that give a clue to error condition e.g. ESMF and NetCDF. ESMF even goes so far as to provide a constant `ESMF_SUCCESS` (which is just an integer set to 0) See the `_SUCCESS` and `_FAILURE` macros for more information.

## Advice

Error handling is most useful if it is used everywhere in the calling chain. If something throws a fault, but then execution continues because some procedure in the calling tree did not check the return code from one of the procedures it calls, well this is bad. If something threw an error, there was probably a good reason but if not trapped it will probably just lead to some other problem later and will result in more confusion during debugging. The bottom line, if the procedure you are calling has an optional rc code check it! If you are creating a new procedure include a return code if there any any failure conditions that can be trapped, including calling other procedures that themselves have error codes!

# Provided Error Macros

To use the MAPL error handling macros your module/subroutine/function should obviously needs to use the MAPL library and include a specific header file `MAPL_Generic.h` As an example:

```fortran
#include "MAPL_Generic.h"
module foo
use MAPL

...

end module
```

## `_HERE`

`_HERE` is a useful macro to add a debugging print to your output, for example:
```fortran
_HERE, "ncols: ", ncols
```
This would expand to:
```fortran
print*,__FILE__,__LINE__, "ncols: ", ncols
```
where `__FILE__` and `__LINE__` are built-in CPP macros that output the file and line of the call. This provides an easy way to get debugging prints with the location of the print.

## `_VERIFY`
`_VERIFY` handles the case when you have called a subroutine/function from some program unit (another subroutine or function) and included the optional return code like so:
```fortran
integer :: status

call foo(...,rc=status)
```
Now you want to check that the status is a good or bad value, otherwise what's the point of adding the rc=status argument. Generally if the value is "bad" you would want to return rather than continue execution of the program unit. It would be even better if you got message with the status and maybe where you failed. That's what `_VERIFY` is for. You can just add this after the call to foo:
```fortran
call foo(...,rc=status)
_VERIFY(status)
```
the `_VERIFY` macro will check that status is the "good" value, if it is execution of the program unit will continue. If the status is anything but the good value the `_VERIFY` will trigger a premature return from the program unit. Before it exits it will **print the status, filename, and line number** to standard out. Finally if the program unit itself has an optional rc argument, rc will be set to status.

## `_RC`

We can take the `_VERIFY` a step further. This is valid Fortran:
```fortran
call foo(...,rc=status); _VERIFY(status)
```
note that everything is nicely on one line, but wouldn't it be nice if there was a macro so you didn't have to type the `rc=status); _VERIFY(status)` there is, it is `_RC`. The code above can be replaced with:
```fortran
call foo(...,_RC)
```
for even more compact and readable code. We suggest using this.

## `_USERRC`

Some ESMF commands return not only an `rc`, but also `userrc`. For example
```fortran
 call ESMF_GridCompRun (gcs(i), importState=gim(i), exportState=gex(i), phase=1, &
 clock=clock, userRC=user_status, rc=status)
```
A user could add:
```fortran
_VERIFY(status)
_VERIFY(user_status)
```
after this, but the `_USERRC` macro does this for you much like `_RC` above:

```fortran
 call ESMF_GridCompRun (gcs(i), importState=gim(i), exportState=gex(i), phase=1, &
 clock=clock, _USERRC)
```

## `_ASSERT`

Sometimes you may have some logical condition you want to check in your procedure, if met just continue but if not met you would want to return, report the filename, line number, and a message to standard out, and set the return status to the failure value if it was passed. The `_ASSERT` macro provides this ability. It can be used like so:
```fortran
logical :: my_conditional

! code does stuff
! code sets my_conditional
_ASSERT(my_conditional,"Provide informative message here")
```

Note that if you want to do `_ASSERT(.FALSE.,"Informative message")`, just use `_FAIL("Informative message")` as described below as they are equivalent and `_FAIL()` is more readable.

## `_FAIL`
_FAIL simply forces the procedure to return with a bad value and a message. It reports the filename, line number, and the message to standard out the _FAIL is call on, sets the rc code to the failure value if the rc was passed, and returns.   This construct is most often useful within `SELECT CASE` and `SELECT TYPE` constructs where an additional conditional is unwarranted.   Otherwise, `_ASSERT` can usually be used more effectively.
For example:
```fortran
select case(number)
case(2)
 
case(1)

case(0)
   _FAIL("Informative Message")
end if
```

## `_RETURN`
When you return from a procedure, if the procedure has an optional rc value you want to set to the success value so that if the calling is checking this they get the "success" value. If someone is checking it, i.e.:
```fortran
if (present(rc)) then
   rc = success_value
end if
RETURN
```
The _RC macro just implements the above can be used like so:
```fortran
_RETURN(success_value)
```
Note you can technically `_RETURN` with any value, not just the success_value. But you want to return and pass a failure, that's what`_FAIL` is for and what we recommend.

## `_SUCCESS`
`_SUCCESS` is just a macro that represents the integer constant `ESMF_SUCCESS` (which itself is just 0). Still good practice to use `_SUCCESS` rather than 0 in your code. Every procedure that has an optional integer rc return code should end with:
```fortran
_RETURN(_SUCCESS)
```
We merely include this so that if someday everyone decides 0 should not be the "good" value we don't have to change this in a gazillion places.

## `_FAILURE`
Similarly `_FAILURE` is just a macro that evaluates to `ESMF_FAILURE` (which itself is just a non-zero integer). However the user should not have need for this in their code. If you want to return with a failure we recommend using `_FAIL` or `_ASSERT`.

## `_RETURN_IF`

`_RETURN_IF` is a conditional variant of `_RETURN` so a user can, say, return early from a routine if a condition is *true* a la:
```fortran

! Get number of columns to do work on
ncols = get_number_of_columns_to_do_work(foo)

! return if no columns
_RETURN_IF(ncol == 0)

! do more work if not
...
```

## `_RETURN_UNLESS`

`_RETURN_UNLESS` is the "opposite" of `_RETURN_IF` which returns if a condition is *false*. For example:

```fortran

! Get number of columns to do work on
ncols = get_number_of_columns_to_do_work(foo)

! return when ncol is more than zero
_RETURN_UNLESS(ncol > 0)

! do more work if not
...
```

In many cases, a `_RETURN_IF` and a `_RETURN_UNLESS` are interchangable, but how the code reads can be better with one or the other.

## `_STAT`

This can be used with Fortran intrinsics that return a `stat`. For example, `allocate()`:
```fortran
allocate(foo, stat=status); _VERIFY(status)
```
we provide a `_STAT` macro that allow you to write the above as:
```fortran
allocate(foo, _STAT)
```

## `_IOSTAT`

`_IOSTAT` is the IO-equivalent of `_STAT` for Fortran IO-based intrinsics that return `iostat`, like `open()` and `close()`:

```fortran
open(...,iostat=status); _VERIFY(status)
```
becomes:
```fortran
open(...,_IOSTAT)
```

# Error Handling at the "program" scope
What happens though if the scope you are in is the "main" program rather than a subroutine or function. When you check the return code from a call there is nowhere to return to; rather you want to stop execution of your program at this point. MAPL does have something to handle this.

# Legacy Macros
In GEOS code outside of MAPL you will probably see macros that start, or start and end with an underscore (`VERIFY_, ASSERT_, __RC__` etc) and see a variable named `IAM` defined. These are legacy macros. They generally provide similar functionality to what has been described above (i.e. `VERIFY_` and `_VERIFY` handle the same case, checking the return value after a procedure call) to handle procedures with optional return parameters. There is one crucial difference that makes the use of these not recommended which will be explained below.

**NEW CODE SHOULD NOT USE THESE AND WE HIGHLY ENCOURAGE THAT IF YOU HAVE EXISTING CODE YOU MAINTAIN TO UPDATE TO THE NEW ONES AS THE OLD ONES ARE ERROR PRONE AND LESS INFORMATIVE.** 

For historical purposes they will be explained here.

Before the macros that begin with an underscore were created MAPL had existing error handling macros that ended in an underscore. The behaviour of these macros did not report the line number and filename. Instead they reported the line number and whatever a local character variable named `IAM` was set to. The idea was that `IAM` would be the name of the program unit you are in such as the subroutine name. However, this can not be obtained from the preprocessor so the user would need to explicitly define this via the `IAM` variable. There is nothing that forces the user to set `IAM` to the name of the subroutine or function. Indeed many times error have occurred because they copied and pasted from another subroutine or if the subroutine has a generic name like "run" that's of little good. We have depreciated these in MAPL but still support them for legacy code. However, we do not endorse the use of these in any new code.
