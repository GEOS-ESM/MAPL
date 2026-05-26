module mapl_State_API_mod
   use mapl_StateGetImpl_mod, only: MAPL_StateGet => StateGet
   use mapl_StateGetPointerImpl_mod, only: MAPL_StateGetPointer => StateGetPointer
   use mapl_StateArithmeticParser_mod, only: MAPL_ParserVariablesInExpression => parser_variables_in_expression
   use mapl_StateAddMethodImpl_mod, only: mapl_StateAddMethod
   use mapl_StateAddMethodImpl_mod, only: CallbackMap
   use mapl_StateAddMethodImpl_mod, only: CallbackMapIterator
   use mapl_StateAddMethodImpl_mod, only: CallbackMethodWrapper
   use mapl_StateAddMethodImpl_mod, only: get_callbacks
   use mapl_StateGetGeomImpl_mod, only: MAPL_StateGetGeom => StateGetGeom
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

end module mapl_State_API_mod
