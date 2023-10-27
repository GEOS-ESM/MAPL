program main
   use mpi
   use iso_fortran_env, only: INT64
   use iso_c_binding, only: C_PTR

   integer(kind=INT64) :: sz
   type (c_ptr) :: ptr
   
   call MPI_Alloc_mem(sz, MPI_INFO_NULL, ptr, ierror)
   
end program main

