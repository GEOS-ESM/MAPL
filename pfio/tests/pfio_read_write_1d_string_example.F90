program main
  use MAPL_ExceptionHandling
  use pFIO
  use gFTL_StringVector
  use gFTL_StringIntegerMap
  use, intrinsic :: iso_c_binding, only: c_f_pointer, c_loc
  use, intrinsic :: iso_fortran_env, only: REAL32

  use pFIO_NetCDF4_FileFormatterMod
  implicit none (type, external)

  character(len=:), allocatable, target :: cvar1(:)
  character(len=:), allocatable, target :: cvar2(:)
  type (NetCDF4_FileFormatter) :: test_formatter
  type (FileMetadata) :: metadata
  type (Variable) :: v
  integer :: Ydim, status, length, my_dim, k

  Ydim = 5
  call metadata%add_dimension('Ydim', Ydim)
  v = Variable(type=pFIO_STRING, dimensions='Ydim')
  call metadata%add_variable('char1',v)
  cvar1 = ["1","22","333", "4    ", "5    "]

  call test_formatter%create('test_in.nc4', rc=status)
  call test_formatter%write(metadata, rc=status)
  call test_formatter%put_var('char1', cvar1, start=[1], count=[Ydim])
  !call test_formatter%put_var('char1', cvar1)
  call test_formatter%close(rc=status)

! read back

  call test_formatter%open('test_in.nc4', PFIO_READ, rc=status)
  call test_formatter%inq_var_string_length('char1',length)
  print*, "length :", length
  metadata = test_formatter%read()
  my_dim = metadata%get_dimension('Ydim')
  if (my_dim /= Ydim ) print *, "dim is wrong"
 
  allocate(character(len=length):: cvar2(my_dim))
  call test_formatter%get_var('char1', cvar2, start=[1], count=[Ydim])
 ! call test_formatter%get_var('char1', cvar2)
  call test_formatter%close()
  do k = 1, Ydim
    print*, cvar2(k) 
  enddo
  

end program

