module mapl3g_State_API
   use mapl3g_StateGet, only: MAPL_StateGet => StateGet
   use mapl3g_StateGetPointer, only: MAPL_StateGetPointer => StateGetPointer
   use MAPL_StateArithmeticParserMod, only: MAPL_ParserVariablesInExpression => parser_variables_in_expression
   implicit none
   private

   ! Available to users
   public :: MAPL_StateGet
   public :: MAPL_StateGetPointer
   public :: MAPL_ParserVariablesInExpression

end module mapl3g_State_API
