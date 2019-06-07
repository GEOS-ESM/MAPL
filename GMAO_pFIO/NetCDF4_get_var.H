#define _NAME get_var

#if _RANK==0
#  define _OPTIONS rc
#else
#  define _OPTIONS start, count, rc
#endif

#include "new_overload.macro"

   subroutine _SUB(this, var_name, values, unusable, _OPTIONS)
      class (NetCDF4_FileFormatter), intent(in) :: this
      character(len=*), intent(in) :: var_name
      _TYPEDECLARE, intent(out) :: values _DIMENSIONS
      class (KeywordEnforcer), optional, intent(in) :: unusable
#if _RANK != 0      						 
      integer, optional, intent(in) :: start(:)
      integer, optional, intent(in) :: count(:)
#endif
      integer, optional, intent(out) :: rc

      integer :: status
      integer :: varid

      _UNUSED_DUMMY(unusable)

     !$omp critical
      status = nf90_inq_varid(this%ncid, name=var_name, varid=varid)
     !$omp end critical

      _VERIFY(status)
     !$omp critical
#if (_RANK == 0)
      status = nf90_get_var(this%ncid, varid, values)
#else
      status = nf90_get_var(this%ncid, varid, values, start, count)
#endif
     !$omp end critical
      _VERIFY(status)

      _RETURN(_SUCCESS)

   end subroutine _SUB


#include "undo_overload.macro"
#undef _OPTIONS
#undef _NAME
