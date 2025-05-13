#define _NAME get_var

#if _RANK==0
#  define _OPTIONS rc
#else
#  define _OPTIONS start, count, stride, rc
#endif

#include "new_overload.macro"

   subroutine _SUB(this, var_name, values, unusable, group_name, _OPTIONS)
      class (NetCDF4_FileFormatter), intent(in) :: this
      character(len=*), intent(in) :: var_name
      _TYPEDECLARE, intent(out) :: values _DIMENSIONS
      class (KeywordEnforcer), optional, intent(in) :: unusable
      character(len=*), optional, intent(in) :: group_name
#if _RANK != 0
      integer, optional, intent(in) :: start(:)
      integer, optional, intent(in) :: count(:)
      integer, optional, intent(in) :: stride(:)
#endif
      integer, optional, intent(out) :: rc

      integer :: status
      integer :: varid
      integer :: ncid

      ncid = this%ncid
      if (present(group_name)) then
         status = nf90_inq_ncid(this%ncid, group_name, ncid)
         _ASSERT(status == 0,'Subgroup not found.')
      end if

     !$omp critical
      status = nf90_inq_varid(ncid, name=var_name, varid=varid)
     !$omp end critical

     _ASSERT(status==0,"Variable not found: "//trim(var_name)//" in file: "//trim(this%origin_file))
     !$omp critical
#if (_VARTYPE == 0)

#if (_RANK == 1)
    ! only support rank 1 of string
    status = pfio_nf90_get_var_string(ncid, varid, values, start, count)
#endif

#else

#if (_RANK == 0)
      status = nf90_get_var(ncid, varid, values)
#else
      status = nf90_get_var(ncid, varid, values, start, count, stride)
#endif
#endif
     !$omp end critical
      _ASSERT(status==0,"Unable to get variable: "//trim(var_name)//" from file: "//trim(this%origin_file))

      _RETURN(_SUCCESS)
      _UNUSED_DUMMY(unusable)

   end subroutine _SUB


#include "undo_overload.macro"
#undef _OPTIONS
#undef _NAME
