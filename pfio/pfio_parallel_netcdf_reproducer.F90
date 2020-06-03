program main
   use MPI
   use FLAP
   use pFIO
   implicit none

   integer :: ierror
   type (command_line_interface) :: cli
   integer :: im
   integer :: lm
   integer :: n_fields
   character(:), allocatable :: output_filename

   call MPI_Init(ierror)

   call cli%init(description='potential reproducer of parallel netcdf problem on SCU12')
   call add_cli_options(cli)
   call parse_cli_arguments(cli, im, lm, n_fields, output_filename)

   call run(im, lm, n_fields, output_filename)
   
   call MPI_Finalize(ierror)

contains


   subroutine add_cli_options(cli)
      type (command_line_interface), intent(inout) :: cli

      call cli%add(switch='--im',  &
                  help='IM World', &
                  required=.true., &
                  act='store')

      call cli%add(switch='--lm',  &
                  help='# levels per field', &
                  required=.true., &
                  act='store')

      call cli%add(switch='--n_fields',  &
                  help='# of fields', &
                  required=.true., &
                  act='store')
      
      call cli%add(switch='-o',  &
                  help='output file name', &
                  required=.true., &
                  act='store')
   end subroutine add_cli_options

   subroutine parse_cli_arguments(cli, im, lm, n_fields, output_filename)
      type (command_line_interface), intent(inout) :: cli
      integer, intent(out) :: im
      integer, intent(out) :: lm
      integer, intent(out) :: n_fields
      character(:), allocatable, intent(out) :: output_filename


      character(1000) :: buffer
      call cli%get(switch='--im', val=im)
      call cli%get(switch='--lm', val=lm)
      call cli%get(switch='--n_fields', val=n_fields)
      call cli%get(switch='-o', val=buffer)
      output_filename = trim(buffer)

   end subroutine parse_cli_arguments


   subroutine run(im, lm, n_fields, output_filename)
      integer, intent(in) :: im
      integer, intent(in) :: lm
      integer, intent(in) :: n_fields
      character(*), intent(in) :: output_filename

      type (Netcdf4_Fileformatter) :: formatter
      type (FileMetadata) :: metadata
      real, allocatable :: field(:,:,:)

      integer :: jm
      integer :: j0, j1
      integer :: nj_local
      integer :: rank, npes, ierror
      integer :: j, n
      character(:), allocatable :: field_name
      character(3) :: field_idx_str

      call mpi_comm_size(MPI_COMM_WORLD, npes, ierror)
      call mpi_comm_rank(MPI_COMM_WORLD, rank, ierror)

      jm = im*6 ! pseudo cubed sphere
      call metadata%add_dimension('IM_WORLD', im)
      call metadata%add_dimension('JM_WORLD', jm)
      call metadata%add_dimension('LM', lm)

      do n = 1, n_fields
         write(field_idx_str,'(i3.3)') n
         field_name = 'field_' // field_idx_str
         call metadata%add_variable(field_name, Variable(pFIO_REAL32, dimensions='IM_WORLD,JM_WORLD,LM'))
      end do

      call formatter%create_par(output_filename, comm=MPI_COMM_WORLD)
      call formatter%write(metadata)

      j0 = 1 + rank*jm/npes
      j1 = (rank+1)*jm/npes
      nj_local = (j1 - j0) + 1
      allocate(field(im, nj_local, lm))

      do j = j0, j1
         field(:,j-j0+1,:) = j
      end do

      do n = 1, n_fields
         write(field_idx_str,'(i3.3)') n
         field_name = 'field_' // field_idx_str
         call formatter%put_var(field_name, field, start=[1,j0,1], count=[im,nj_local,lm])
      end do

      call formatter%close()
   end subroutine run

end program main
