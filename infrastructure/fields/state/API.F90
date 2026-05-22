module mapl_State_API
   use mapl_StateGetImpl, only: MAPL_StateGet => StateGet
   use mapl_StateGetPointerImpl, only: MAPL_StateGetPointer => StateGetPointer
   use MAPL_StateArithmeticParserMod, only: MAPL_ParserVariablesInExpression => parser_variables_in_expression
   use mapl_StateAddMethodImpl, only: mapl_StateAddMethod
   use mapl_StateAddMethodImpl, only: CallbackMap
   use mapl_StateAddMethodImpl, only: CallbackMapIterator
   use mapl_StateAddMethodImpl, only: CallbackMethodWrapper
   use mapl_StateAddMethodImpl, only: get_callbacks
   use mapl_StateGetGeomImpl, only: MAPL_StateGetGeom => StateGetGeom
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
