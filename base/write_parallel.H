
#ifdef NAME_
#undef NAME_
#endif

#ifdef NAMESTR_
#undef NAMESTR_
#endif

#define NAME_ WRITE_PARALLEL_
#define NAMESTR_ WRITE_PARALLEL_

#include "overload.macro"


subroutine SUB_ ( data, UNIT, ARRDES, format, RC)

#if (VARTYPE_ > 0)
  TYPE_(kind=EKIND_ ), intent(in   )            :: data DIMENSIONS_
#else
  character(LEN=*),    intent(in   )            :: data DIMENSIONS_
#endif
  integer,             intent(in   ),  optional :: UNIT
  type(ArrDescr),      intent(INOUT),  optional :: ARRDES
  character(len=*),    intent(in   ),  optional :: format
  integer         ,    intent(  out),  optional :: RC

  character(len=ESMF_MAXSTR) :: FORMATTED
  integer :: recl, status

 if(present(arrdes)) then
  if (MAPL_AM_I_ROOT()) then
   if(arrdes%offset>=0) then
      call MPI_FILE_SEEK(UNIT, arrdes%offset, MPI_SEEK_SET, STATUS)
      _VERIFY(STATUS)
   endif

#if (RANK_ == 0)
#if (VARTYPE_ == 0)
   recl = len(data)*4
   call MPI_FILE_WRITE(UNIT, recl, 1,         MPI_INTEGER, MPI_STATUS_IGNORE, STATUS)
   call MPI_FILE_WRITE(UNIT, data, len(data), MPITYPE_   , MPI_STATUS_IGNORE, STATUS)
   call MPI_FILE_WRITE(UNIT, recl, 1,         MPI_INTEGER, MPI_STATUS_IGNORE, STATUS)
#ifdef DEBUG_MPIIO
   print*, arrdes%offset, recl, arrdes%offset + len(data)*4 + 8
#endif
#else
   recl = 1*EKIND_
   call MPI_FILE_WRITE(UNIT, recl, 1,         MPI_INTEGER, MPI_STATUS_IGNORE, STATUS)
   call MPI_FILE_WRITE(UNIT, data, 1, MPITYPE_, MPI_STATUS_IGNORE, STATUS)
   call MPI_FILE_WRITE(UNIT, recl, 1,         MPI_INTEGER, MPI_STATUS_IGNORE, STATUS)
#ifdef DEBUG_MPIIO
   print*, arrdes%offset, recl, arrdes%offset + 1*EKIND_ + 8
#endif
#endif
#else
   recl = size(data)*EKIND_
   call MPI_FILE_WRITE(UNIT, recl, 1,         MPI_INTEGER, MPI_STATUS_IGNORE, STATUS)
   call MPI_FILE_WRITE(UNIT, data, size(data), MPITYPE_, MPI_STATUS_IGNORE, STATUS)
   call MPI_FILE_WRITE(UNIT, recl, 1,         MPI_INTEGER, MPI_STATUS_IGNORE, STATUS)
#ifdef DEBUG_MPIIO
   print*, arrdes%offset, recl, arrdes%offset + size(data)*EKIND_ + 8
#endif
#endif
   _VERIFY(STATUS)

  endif

#if (RANK_ == 0)
#if (VARTYPE_ == 0)
   arrdes%offset = arrdes%offset + len(data)*4 + 8
#else
   arrdes%offset = arrdes%offset + 1*EKIND_ + 8
#endif
#else
   arrdes%offset = arrdes%offset + size(data)*EKIND_ + 8
#endif

   _RETURN(ESMF_SUCCESS)
 endif

  if (present(unit)) then
     if (unit == UNDEF) then
        _RETURN(ESMF_SUCCESS)
     endif
     if(unit < 0) then

#if (RANK_ > 2 || VARTYPE_ <= 0)

        _FAIL('needs informative message')

#else

        _ASSERT(-UNIT<=LAST_UNIT,'needs informative message')
        munit => MEM_units(-unit)
        munit%prevrec = munit%prevrec + 1

        if(.not.associated(munit%Records)) then
           allocate(Rec(16),stat=status)
           munit%Records => REC
           _VERIFY(STATUS)
        elseif(size(munit%Records)< munit%prevrec) then
           allocate(REC(munit%prevrec*2),stat=status)
           _VERIFY(STATUS)
           REC(:munit%prevrec-1) = munit%Records
           deallocate(munit%Records)
           munit%Records => REC
        endif

#if (RANK_ == 0)
        call dealloc_(munit%Records(munit%prevrec),rc=status)
        _VERIFY(STATUS)
#endif

#if (RANK_ == 1)
        call alloc_(munit%Records(munit%prevrec),TKR_,size(data,1),rc=status)
        _VERIFY(STATUS)
#endif

#if (RANK_ == 2)
        call alloc_(munit%Records(munit%prevrec),TKR_,size(data,1),size(data,2),rc=status)
        _VERIFY(STATUS)
#endif

        munit%Records(munit%prevrec)%TKR_  = data

#endif
     else ! unit is > 0
        if (MAPL_AM_I_ROOT()) then
           inquire(unit=UNIT, formatted=FORMATTED)
           if   (FORMATTED == "YES") then
              if (present(format)) then;    write(UNIT, format) data
              else;    write(UNIT, *     ) data
              end if
           elseif(FORMATTED == "NO") then; write(UNIT        ) data
           end if
        endif
     end if
  else

     if (MAPL_AM_I_ROOT()) then
        if (present(format)) then; write(*, format) data
        else; write(*,      *) data
        end if
     end if
  end if

  _RETURN(ESMF_SUCCESS)

end subroutine SUB_

!---------------------------
#undef NAME_
#undef NAMESTR_

#undef DIMENSIONS_
#undef RANK_
#undef RANKSTR_
#undef VARTYPE_
