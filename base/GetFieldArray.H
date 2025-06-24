#ifdef NAME_
#undef NAME_
#endif

#ifdef NAMESTR_
#undef NAMESTR_
#endif

#define NAME_ ESMFL_FieldGetPtrToData
#define NAMESTR_ 'ESMFL_FieldGetPtrToData'

#include "overload.macro"

 subroutine SUB_(Field, PTR, RC)
   type(ESMF_Field),  intent(INOUT) :: Field
   real(KIND=EKIND_), pointer :: PTR DIMENSIONS_
   integer, optional, intent(  OUT) :: RC
   
   integer                         :: STATUS

   logical :: hasDE
   type(ESMF_Grid) :: grid

   call ESMF_FieldGet(field,grid=grid,rc=status)
   _verify(status)
   HasDE = MAPL_GridHasDE(grid,rc=status)
   _verify(STATUS)
   if (HasDE) then
      call ESMF_FieldGet(field, 0, Ptr, rc=status)
      _verify(STATUS)
   end if

   _return(ESMF_SUCCESS)
   
 end subroutine SUB_

#undef DIMENSIONS_
#undef RANK_
#undef RANKSTR_
   
