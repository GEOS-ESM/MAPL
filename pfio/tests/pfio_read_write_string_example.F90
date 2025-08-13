program main
  use MAPL_ExceptionHandling
  use pFIO
  use gFTL_StringVector
  use gFTL_StringIntegerMap
  use, intrinsic :: iso_c_binding, only: c_f_pointer, c_loc
  use, intrinsic :: iso_fortran_env, only: REAL32

  use pFIO_NetCDF4_FileFormatterMod
  implicit none

  character(len=:), allocatable, target :: cvar1(:)
  character(len=:), allocatable, target :: cvar2(:)
  character(len=:), allocatable, target :: cvar3
  character(len=:), allocatable         :: cvar4
  type (NetCDF4_FileFormatter) :: test_formatter
  type (FileMetadata) :: metadata
  type (Variable) :: v
  integer :: Ydim, status, length, my_dim, k

  Ydim = 5
  call metadata%add_dimension('Ydim', Ydim)
  v = Variable(type=pFIO_STRING, dimensions='Ydim')
  call metadata%add_variable('char1',v)
  v = Variable(type=pFIO_STRING)
  call metadata%add_variable('char3',v)


  cvar1 = ["1","22","333", "4    ", "5    "]
  cvar3 = "scalar added"

  call test_formatter%create('test_in.nc4', rc=status)
  call test_formatter%write(metadata, rc=status)
  call test_formatter%put_var('char1', cvar1, start=[1], count=[Ydim])
  call test_formatter%put_var('char3', cvar3)
  !call test_formatter%put_var('char1', cvar1)
  call test_formatter%close(rc=status)

! read back
  print*, ''
  length = 0
  call test_formatter%open('test_in.nc4', PFIO_READ, rc=status)
  call test_formatter%inq_var_string_length('char1',length)
  print*, "char1 length :", length
  metadata = test_formatter%read()
  my_dim = metadata%get_dimension('Ydim')
  if (my_dim /= Ydim ) print *, "dim is wrong"
 
  allocate(character(len=length):: cvar2(my_dim))
  call test_formatter%get_var('char1', cvar2, start=[1], count=[Ydim])
 ! call test_formatter%get_var('char1', cvar2)
  call test_formatter%inq_var_string_length('char3',length)
  print*, "char3 length :", length
  allocate(character(len=length):: cvar4)
  call test_formatter%get_var('char3', cvar4)
  call test_formatter%close()
  print*,''
  print*, "cvar1 in the file and read back: "
  do k = 1, Ydim
    print*, cvar1(k), " : ", cvar2(k)
  enddo
  
  print*,''
  print*, "cvar3 in the file: "//cvar3
  print*, "cvar3 read back  : "//cvar4

end program

