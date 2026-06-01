! Export umbrella for the MAPL.state library.
module mapl_state_export

   use mapl_StateArithmeticParser_mod, only: MAPL_ParserVariablesInExpression => parser_variables_in_expression
   use mapl_StateAddMethodImpl_mod, only: mapl_StateAddMethod
   use mapl_StateDestroyImpl_mod, only: MAPL_StateDestroy
   use mapl_StateGetImpl_mod, only: MAPL_StateGet => StateGet
   use mapl_StateGetGeomImpl_mod, only: MAPL_StateGetGeom => StateGetGeom
   use mapl_StateGetPointerImpl_mod, only: MAPL_StateGetPointer => StateGetPointer

   implicit none
   private

   public :: MAPL_ParserVariablesInExpression
   public :: mapl_StateAddMethod
   public :: MAPL_StateDestroy
   public :: MAPL_StateGet
   public :: MAPL_StateGetGeom
   public :: MAPL_StateGetPointer

end module mapl_state_export
