module mapl_State_API
   use mapl_StateGet, only: MAPL_StateGet => StateGet
   use mapl_StateGetPointer, only: MAPL_StateGetPointer => StateGetPointer
   use MAPL_StateArithmeticParserMod, only: MAPL_ParserVariablesInExpression => parser_variables_in_expression
   use mapl_StateAddMethod, only: mapl_StateAddMethod
   use mapl_StateAddMethod, only: CallbackMap
   use mapl_StateAddMethod, only: CallbackMapIterator
   use mapl_StateAddMethod, only: CallbackMethodWrapper
   use mapl_StateAddMethod, only: get_callbacks
   use mapl_StateGetGeom, only: MAPL_StateGetGeom => StateGetGeom
   implicit none
   private

   ! Available to users
   public :: MAPL_StateGet
   public :: MAPL_StateGetPointer
   public :: MAPL_ParserVariablesInExpression
   public :: mapl_StateAddMethod
   public :: CallbackMap
   public :: CallbackMapIterator
   public :: CallbackMethodWrapper
   public :: get_callbacks
   public :: MAPL_StateGetGeom

end module mapl_State_API
