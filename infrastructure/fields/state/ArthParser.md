The MAPL library includes an arithmetic expression parser. The main use for this is to allow `ExtData` and `History` to derive fields using some arithmetic expression involving other fields. As such the parser's main subroutine used by MAPL, `MAPL_StateEval` takes an ESMF_State, a character representing the expression, where the variables in the expression are fields in the state, and finally fills the field passed into the subroutine.  The value in the resultant field is simple the value by applying the expression to each array index in the original fields in the expression. So you can think of this as a shortcut to avoid having to loop over each element of the fields.


Here are some rules about expressions

1. Fields in expression can only be model fields.
2. If the model field has an alias you must use the alias in the expression.
3. You can not mix center and edge fields in an expression. You can mix 2D and 3D fields if the 3D fields are all center or edge. In this case each level of the 3D field operated with the 2D field. Another way to think of this is that in an expression involving a 2D and 3D field the 2D field gets promoted to a 3D field with the same data in each level.
4. When parsing an expression the parser first checks if the fields in an expression are part of the collection. Any model field in a collection can be used in an expression in the same collection. However, there might be cases where you wish to output an expression but not the model fields used in the expression. In this case if the parser does not find the field in the collection it checks the gridded component name after the expression for the model field. If the field is found in the gridded component it can use it in the expression. Note that if you have an expression with two model fields from different gridded components you can not use this mechanism to output the expression without outputting either field. One of them must be in the collection.
5. The alias of an expression can not be used in a subsequent expression.

Here are the rules for the expressions:

1. The function string can contain the following mathematical operators `+`,  `-`,  `*`, `/`, `^` and `()`.
2. Variable names - Parsing of variable names is case sensitive.
3. The following single argument Fortran intrinsic functions and user defined functions are implmented: `abs`, `exp`, `log10`, `log`, `sqrt`, `sinh`, `cosh`, `tanh`, `sin`, `cos`, `tan`, `asin`, `acos`, `atan`, `heav` (the Heaviside step function). Parsing of functions is case insensitive.
4. Integers or real constants. To be recognized as explicit constants these must conform to the format `[+|-][nnn][.nnn][e|E|d|D][+|-][nnn]` where `nnn` means any number of digits. The mantissa must contain at least one digit before or following an optional decimal point. Valid exponent identifiers are `e`, `E`, `d` or `D`. If they appear they must be followed by a valid exponent!
5. Operations are evaluated in the order
   1. expressions in parentheses
   2. unary minux (`-X`)
   3. exponentiation (`X^Y`)
   4. multiplicaiton (`X*Y`) and division (`X/Y`)
   5. addition (`X+Y`) and subtraction (`X-Y`)
