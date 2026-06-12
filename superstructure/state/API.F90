! Export umbrella for the MAPL.state library.
module mapl_state_api

   use mapl_StateArithmeticParser_mod, only: MAPL_ParserVariablesInExpression => parser_variables_in_expression
   use mapl_StateArithmeticParser_mod, only: mapl_StateEval => StateEval
   use mapl_StateAddMethod_mod, only: mapl_StateAddMethod => StateAddMethod
   use mapl_StateDestroy_mod, only: MAPL_StateDestroy => StateDestroy
   use mapl_StateGet_mod, only: MAPL_StateGet => StateGet
   use mapl_StateSet_mod, only: MAPL_StateSet => StateSet
   use mapl_StateGetGeom_mod, only: MAPL_StateGetGeom => StateGetGeom
   use mapl_StateGetPointer_mod, only: MAPL_StateGetPointer => StateGetPointer
   use mapl_StateMask_mod, only: MAPL_StateMask => StateMask
   

   implicit none
   private

   public :: MAPL_ParserVariablesInExpression
   public :: mapl_StateAddMethod
   public :: MAPL_StateDestroy
   public :: MAPL_StateGet
   public :: MAPL_StateSet
   public :: MAPL_StateGetGeom
   public :: MAPL_StateGetPointer
   public :: mapl_StateEval

   public :: MAPL_StateMask
end module mapl_state_api
