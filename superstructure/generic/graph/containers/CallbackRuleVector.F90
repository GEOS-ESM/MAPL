!------------------------------------------------------------------------------
! CallbackRuleVector: gFTL vector of CallbackRule, used by CallbackInterface
! for its declared rules in declaration order. Kept in its own module
! because the gFTL vector/template.inc emits its own implicit none/private
! directives, which conflict with a preceding one in the including module.
!------------------------------------------------------------------------------
module mapl_CallbackRuleVector_mod
   use mapl_CallbackRule_mod, only: CallbackRule

#define T CallbackRule
#define Vector CallbackRuleVector
#define VectorIterator CallbackRuleVectorIterator

#include "vector/template.inc"

#undef VectorIterator
#undef Vector
#undef T

end module mapl_CallbackRuleVector_mod
