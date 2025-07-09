#undef I_AM_MAIN
#include "MAPL_ErrLog.h"
module mapl_checkpoint_support_mod

   use ESMF
   use MPI
   use NetCDF
   use MAPL_ErrorHandlingMod
   use fargparse
   use, intrinsic :: iso_fortran_env, only: INT64, REAL64, REAL32
   implicit none

   real(kind=REAL64), parameter :: byte_to_mega = (1.0d0/1024.0d0)*(1.0d0/1024.0d0)
   type array_wrapper
      character(len=:), allocatable :: field_name
      real, allocatable :: field(:,:,:)
   end type

   type test_support
      integer :: nx,ny,im_world,lm,num_arrays,num_writers
      integer :: gather_comm
      integer :: writers_comm
      integer :: xcomm
      integer :: ycomm
      integer :: ncid
      integer, allocatable :: i1(:),in(:),j1(:),jn(:)
      type(array_wrapper), allocatable :: bundle(:)
      integer :: face_index
      integer(kind=INT64) :: write_counter
      logical :: do_chunking
      logical :: gather_3D
      logical :: split_file
      logical :: extra_info
      logical :: write_barrier
      logical :: do_writes
      real(kind=REAL64) :: data_volume
      real(kind=REAL64) :: time_writing
      real(kind=REAL64) :: time_mpi
      logical :: netcdf_writes
      integer :: n_trials
      logical :: random

      integer(kind=INT64) :: mpi_time
      integer(kind=INT64) :: write_3d_time
      integer(kind=INT64) :: write_2d_time
      integer(kind=INT64) :: create_file_time
      integer(kind=INT64) :: close_file_time
      contains
         procedure :: set_parameters_by_config
         procedure :: set_parameters_by_cli
         procedure :: compute_decomposition
         procedure :: allocate_n_arrays
         procedure :: create_arrays
         procedure :: create_communicators
         procedure :: create_file
         procedure :: close_file
         procedure :: write_file
         procedure :: write_level
         procedure :: write_variable
         procedure :: reset
   end type

   type cli_options
      integer :: nx
      integer :: ny
      integer :: im_world
      integer :: lm
      integer :: num_writers
      integer :: num_arrays
      integer :: n_trials
      logical :: split_file = .false.
      logical :: gather_3d = .false.
      logical :: write_barrier = .false.
      logical :: random_data = .true.
      logical :: do_writes = .true.
      logical :: netcdf_writes = .true.
      logical :: do_chunking = .true.
      character(len=:), allocatable :: config_file
   end type cli_options

contains

   function parse_arguments() result(options)

      type(StringUnlimitedMap) :: options
      type(ArgParser), target :: parser

      call parser%initialize('checkpoint_simulator.x')
      parser = ArgParser()

      call parser%add_argument("--config_file", &
         help="The configuration file to use", &
         action="store", &
         type="string")

      call parser%add_argument("--nx", &
         help="The number of cells in the x direction (default: 4)", &
         action="store", &
         type="integer", &
         default=4)

      call parser%add_argument("--ny", &
         help="The number of cells in the y direction (default: 4)", &
         action="store", &
         type="integer", &
         default=4)

      call parser%add_argument("--im_world", &
         help="The resolution of the cubed sphere (default: 90)", &
         action="store", &
         type="integer", &
         default=90)

      call parser%add_argument("--lm", &
         help="The number of levels in each 3D variable (default: 137)", &
         action="store", &
         type="integer", &
         default=137)

      call parser%add_argument("--num_writers", &
         help="The number of processes that will write (default: 1)", &
         action="store", &
         type="integer", &
         default=1)

      call parser%add_argument("--num_arrays", &
         help="The number of 3D arrays to write (default: 5)", &
         action="store", &
         type="integer", &
         default=5)

      call parser%add_argument("--ntrials", &
         help="The number of trials to run (default: 3)", &
         action="store", &
         type="integer", &
         default=3)

      call parser%add_argument("--split_file", &
         help="Split the file into multiple files (default: do not split)", &
         action="store_true", &
         default=.false.)

      call parser%add_argument("--gather_3d", &
         help="Gather all levels at once instead of one at a time (default: gather one at a time)", &
         action="store_true", &
         default=.false.)

      call parser%add_argument("--write_barrier", &
         help="Add a barrier after every write (default: no barrier)", &
         action="store_true", &
         default=.false.)

      call parser%add_argument("--static_data", &
         help="Use static data (rank of process) instead of random data (default: random data)", &
         action="store_true", &
         default=.False.)

      call parser%add_argument("--suppress_writes", &
         help="Do not write data (default: write data)", &
         action="store_true", &
         default=.False.)

      call parser%add_argument("--write_binary", &
         help="Write binary data instead of NetCDF (default: write NetCDF)", &
         action="store_true", &
         default=.false.)

      call parser%add_argument("--no_chunking", &
         help="Do not chunk output (default: chunk the output)", &
         action="store_true", &
         default=.false.)

      options = parser%parse_args()

   end function parse_arguments

   subroutine get_cli_options(options, cli)
      type(StringUnlimitedMap), intent(in) :: options
      type(cli_options), intent(out) :: cli
      class(*), pointer :: option
      logical :: tmp

      option => options%at("config_file")
      if (associated(option)) call cast(option, cli%config_file)

      option => options%at("nx")
      if (associated(option)) call cast(option, cli%nx)

      option => options%at("ny")
      if (associated(option)) call cast(option, cli%ny)

      option => options%at("im_world")
      if (associated(option)) call cast(option, cli%im_world)

      option => options%at("lm")
      if (associated(option)) call cast(option, cli%lm)

      option => options%at("num_writers")
      if (associated(option)) call cast(option, cli%num_writers)

      option => options%at("num_arrays")
      if (associated(option)) call cast(option, cli%num_arrays)

      option => options%at("ntrials")
      if (associated(option)) call cast(option, cli%n_trials)

      option => options%at("split_file")
      if (associated(option)) call cast(option, cli%split_file)

      option => options%at("gather_3d")
      if (associated(option)) call cast(option, cli%gather_3d)

      option => options%at("write_barrier")
      if (associated(option)) call cast(option, cli%write_barrier)

      option => options%at("static_data")
      if (associated(option)) call cast(option, tmp)
      cli%random_data = .not. tmp

      option => options%at("suppress_writes")
      if (associated(option)) call cast(option, tmp)
      cli%do_writes = .not. tmp

      option => options%at("write_binary")
      if (associated(option)) call cast(option, tmp)
      cli%netcdf_writes = .not. tmp

      option => options%at("no_chunking")
      if (associated(option)) call cast(option, tmp)
      cli%do_chunking = .not. tmp

   end subroutine get_cli_options

   subroutine set_parameters_by_config(this,config_file)
      class(test_support), intent(inout) :: this
      character(len=*), intent(in) :: config_file
      type(ESMF_Config) :: config

      logical :: is_present
      integer :: comm_size, status,error_code,rc

      config = ESMF_ConfigCreate()
      this%extra_info = .false.
      this%write_barrier = .false.
      this%do_writes = .true.
      call ESMF_ConfigLoadFile(config,config_file)
      call ESMF_ConfigGetAttribute(config,this%nx,label="NX:")
      call ESMF_ConfigGetAttribute(config,this%ny,label="NY:")
      call ESMF_ConfigGetAttribute(config,this%im_world,label="IM_WORLD:")
      call ESMF_ConfigGetAttribute(config,this%lm,label="LM:")
      call ESMF_ConfigGetAttribute(config,this%num_writers,label="NUM_WRITERS:")
      call ESMF_ConfigGetAttribute(config,this%num_arrays,label="NUM_ARRAYS:")
      this%do_chunking = get_logical_key(config,"CHUNK:",.true.)
      this%gather_3d = get_logical_key(config,"GATHER_3D:",.false.)
      this%split_file = get_logical_key(config,"SPLIT_FILE:",.false.)
      this%extra_info = get_logical_key(config,"EXTRA_INFO:",.false.)
      this%write_barrier = get_logical_key(config,"WRITE_BARRIER:",.false.)
      this%do_writes = get_logical_key(config,"DO_WRITES:",.true.)
      this%netcdf_writes = get_logical_key(config,"NETCDF_WRITES:",.true.)
      this%n_trials = get_integer_key(config,"NTRIALS:",3)
      this%random = get_logical_key(config,"RANDOM_DATA:",.true.)

      this%write_counter = 0
      this%write_3d_time = 0
      this%write_2d_time = 0
      this%create_file_time = 0
      this%close_file_time = 0
      this%data_volume = 0.d0
      this%time_writing = 0.d0
      this%mpi_time = 0.0
      call MPI_COMM_SIZE(MPI_COMM_WORLD,comm_size,status)
      _verify(status)
      if (comm_size /= (this%nx*this%ny*6)) then
         call MPI_Abort(mpi_comm_world,error_code,status)
         _verify(status)
      endif

      contains

      function get_logical_key(config,label,default_val) result(val)
         logical :: val
         type(ESMF_Config), intent(Inout)  :: config
         character(len=*), intent(in) :: label
         logical, intent(in) :: default_val

         logical :: is_present
         call ESMF_ConfigFindlabel(config,label,isPresent=is_present)
         if (is_present) then
            call ESMF_ConfigGetAttribute(config,val,label=label)
         else
            val = default_val
         end if
      end function

      function get_integer_key(config,label,default_val) result(val)
         integer :: val
         type(ESMF_Config), intent(Inout)  :: config
         character(len=*), intent(in) :: label
         integer, intent(in) :: default_val

         logical :: is_present
         call ESMF_ConfigFindlabel(config,label,isPresent=is_present)
         if (is_present) then
            call ESMF_ConfigGetAttribute(config,val,label=label)
         else
            val = default_val
         end if
      end function

   end subroutine set_parameters_by_config

   subroutine set_parameters_by_cli(this,cli)
      class(test_support), intent(inout) :: this
      type(cli_options), intent(in) :: cli

      logical :: is_present
      integer :: comm_size, status,error_code,rc

      this%extra_info = .false.
      this%write_barrier = cli%write_barrier
      this%do_writes = cli%do_writes
      this%netcdf_writes = cli%netcdf_writes
      this%do_chunking = cli%do_chunking
      this%gather_3d = cli%gather_3d
      this%split_file = cli%split_file
      this%nx = cli%nx
      this%ny = cli%ny
      this%im_world = cli%im_world
      this%lm = cli%lm
      this%num_writers = cli%num_writers
      this%num_arrays = cli%num_arrays
      this%n_trials = cli%n_trials
      this%random = cli%random_data

      this%write_counter = 0
      this%write_3d_time = 0
      this%write_2d_time = 0
      this%create_file_time = 0
      this%close_file_time = 0
      this%data_volume = 0.d0
      this%time_writing = 0.d0
      this%mpi_time = 0.0
      call MPI_COMM_SIZE(MPI_COMM_WORLD,comm_size,status)
      _verify(status)
      if (comm_size /= (this%nx*this%ny*6)) then
         call MPI_Abort(mpi_comm_world,error_code,status)
         _verify(status)
      endif

   end subroutine set_parameters_by_cli

   subroutine reset(this)
      class(test_support), intent(inout) :: this
      this%write_counter = 0
      this%write_3d_time = 0
      this%write_2d_time = 0
      this%create_file_time = 0
      this%close_file_time = 0
      this%data_volume = 0.d0
      this%time_writing = 0.d0
      this%mpi_time = 0.0
   end subroutine

   function compute_decomposition(this,axis) result(decomp)
      integer, allocatable :: decomp(:)
      class(test_support), intent(inout) :: this
      integer, intent(in) :: axis

      integer :: n_loc, rm, im, n
      integer :: seed_size

      if (axis == 1) then
         n_loc = this%nx
      else if (axis ==2) then
         n_loc = this%ny
      end if
      allocate(decomp(n_loc))
      im = this%im_world/n_loc
      rm = this%im_world-n_loc*im
      do n = 1,n_loc
         decomp(n) = im
         if (n.le.rm) decomp(n) = im+1
      enddo

   end function

   subroutine allocate_n_arrays(this,im,jm)
      class(test_support), intent(inout) :: this
      integer, intent(in) :: im
      integer, intent(in) :: jm

      integer :: n,rank,status,rc
      character(len=3) :: formatted_int
      integer :: seed_size
      integer, allocatable :: seeds(:)

      call MPI_COMM_RANK(MPI_COMM_WORLD,rank,status)
      _verify(status)
      call random_seed(size=seed_size)
      allocate(seeds(seed_size))
      seeds = rank
      call random_seed(put=seeds)
      do n=1,size(this%bundle)
         write(formatted_int,'(i0.3)')n
         this%bundle(n)%field_name = "VAR"//formatted_int
         allocate(this%bundle(n)%field(im,jm,this%lm))
         if (this%random) then
            call random_number(this%bundle(n)%field)
         else
            this%bundle(n)%field = rank
         end if
      enddo
   end subroutine

   subroutine create_arrays(this)
      class(test_support), intent(inout) :: this

      integer, allocatable :: ims(:),jms(:)
      integer :: rank, status,comm_size,n,i,j,rank_counter,offset,index_offset,rc

      call MPI_Comm_Rank(MPI_COMM_WORLD,rank,status)
      _verify(status)
      call MPI_Comm_Size(MPI_COMM_WORLD,comm_size,status)
      _verify(status)
      allocate(this%bundle(this%num_arrays))
      ims = this%compute_decomposition(axis=1)
      jms = this%compute_decomposition(axis=2)
      allocate(this%i1(this%nx))
      allocate(this%in(this%nx))
      allocate(this%j1(this%ny*6))
      allocate(this%jn(this%ny*6))
      rank_counter = 0
      this%i1(1)=1
      this%j1(1)=1
      this%in(1)=ims(1)
      this%jn(1)=jms(1)

      do i=2,this%nx
         this%i1(i) = this%in(i-1)+1
         this%in(i) = this%in(i-1)+ims(i)
      enddo

      do j=2,this%ny
         this%j1(j) = this%jn(j-1)+1
         this%jn(j) = this%jn(j-1)+jms(j)
      enddo

      do n=2,6
         index_offset = (n-1)*this%ny
         offset = (n-1)*this%im_world
         do j=1,this%ny
            this%j1(j+index_offset)=this%j1(j) + offset
            this%jn(j+index_offset)=this%jn(j) + offset
         enddo
      enddo

      do n=1,6
         do j=1,this%ny
            do i=1,this%nx
               if (rank == rank_counter) then
                  call this%allocate_n_arrays(ims(i),jms(j))
               end if
               rank_counter = rank_counter + 1
           enddo
        enddo
     enddo

  end subroutine

  subroutine create_communicators(this)
     class(test_support), intent(inout) :: this

     integer :: myid,status,nx0,ny0,color,j,ny_by_writers,local_ny,key,rc

     local_ny = this%ny*6
     call MPI_Comm_Rank(mpi_comm_world,myid,status)
     _verify(status)
     nx0 = mod(myid,this%nx) + 1
     ny0 = myid/this%nx + 1
     color = nx0
     call MPI_Comm_Split(MPI_COMM_WORLD,color,myid,this%ycomm,status)
     _verify(status)
     color = ny0
     call MPI_Comm_Split(MPI_COMM_WORLD,color,myid,this%xcomm,status)
     _verify(status)


     ny_by_writers = local_ny/this%num_writers
     if (mod(myid,(this%nx*local_ny)/this%num_writers) == 0) then
        color = 0
     else
        color = MPI_UNDEFINED
     end if
     call MPI_COMM_SPLIT(MPI_COMM_WORLD,color,myid,this%writers_comm,status)
     _verify(status)

     if (this%num_writers == local_ny) then
        this%gather_comm = this%xcomm
     else
        j = ny0 - mod(ny0-1,ny_by_writers)
        call MPI_COMM_SPLIT(MPI_COMM_WORLD,j,myid,this%gather_comm, status)
        _verify(status)
     end if

     call MPI_BARRIER(mpi_comm_world, status)
     _verify(status)


  end subroutine

  subroutine close_file(this)
     class(test_support), intent(inout) :: this

     integer :: status, rc

     integer(kind=INT64) :: sub_start,sub_end

     call system_clock(count=sub_start)

     if (this%writers_comm /= MPI_COMM_NULL) then
        if (this%netcdf_writes) then
           status = nf90_close(this%ncid)
        else
           close(this%ncid)
        end if
     end if
     call MPI_BARRIER(MPI_COMM_WORLD, status)
     _verify(status)
     call system_clock(count=sub_end)
     this%close_file_time =  sub_end-sub_start
  end subroutine

  subroutine create_file(this)
     class(test_support), intent(inout) :: this

     integer :: status, rc
     integer :: info
     integer :: xdim,ydim,zdim,i,varid,create_mode
     character(len=:), allocatable :: fname
     character(len=3) :: fc
     integer(kind=INT64) :: sub_start,sub_end
     integer :: y_size,writer_rank,z_chunk,chunk_factor

     call system_clock(count=sub_start)
     if (this%netcdf_writes) then

        create_mode = NF90_CLOBBER
        create_mode = IOR(create_mode,NF90_NETCDF4)
        create_mode = IOR(create_mode,NF90_SHARE)
        create_mode = IOR(create_mode,NF90_MPIIO)
        call MPI_INFO_CREATE(info, status)
        _verify(status)
        call MPI_INFO_SET(info,"cb_buffer_size","16777216", status)
        _verify(status)
        call MPI_INFO_SET(info,"romio_cb_write","enable", status)
        _verify(status)
        if (this%extra_info) then
           call MPI_INFO_SET(info,"IBM_largeblock_io","true", status)
           _verify(status)
           call MPI_INFO_SET(info,"striping_unit","4194304", status)
           _verify(status)
        end if
        if (this%writers_comm /= MPI_COMM_NULL) then
           if (this%split_file) then
              call MPI_COMM_RANK(this%writers_comm,writer_rank, status)
              _verify(status)
              write(fc,'(I0.3)')writer_rank
              fname = "checkpoint_"//fc//".nc4"
              status = nf90_create(fname,ior(NF90_NETCDF4,NF90_CLOBBER), this%ncid)
              _verify(status)
              chunk_factor = 1
           else
              fname = "checkpoint.nc4"
              status = nf90_create(fname,create_mode, this%ncid, comm=this%writers_comm, info=info)
              _verify(status)
              chunk_factor = this%num_writers
           end if
           status = nf90_def_dim(this%ncid,"lon",this%im_world,xdim)
           _verify(status)
           if (this%split_file) then
              y_size = this%im_world*6/this%num_writers
           else
              y_size = this%im_world*6
           end if
           status = nf90_def_dim(this%ncid,"lat",y_size,ydim)
           _verify(status)
           status = nf90_def_dim(this%ncid,"lev",this%lm,zdim)
           _verify(status)
           if (this%gather_3d) then
              z_chunk = this%lm
           else
              z_chunk = 1
           end if
           do i=1,this%num_arrays
              if (this%do_chunking) then
                 status = nf90_def_var(this%ncid,this%bundle(i)%field_name,NF90_FLOAT,[xdim,ydim,zdim],varid,chunksizes=[this%im_world,y_size/chunk_factor,z_chunk])
                 _verify(status)
              else
                 status = nf90_def_var(this%ncid,this%bundle(i)%field_name,NF90_FLOAT,[xdim,ydim,zdim],varid)
                 _verify(status)
              end if
              status = nf90_def_var_fill(this%ncid,varid,NF90_NOFILL,0)
              _verify(status)
              !status = nf90_var_par_access(this%ncid,varid,NF90_COLLECTIVE) ! you can turn this on if you really want to hork up performance
              !_verify(status)
           enddo
           status = nf90_enddef(this%ncid)
        end if
     else
        if (this%writers_comm /= MPI_COMM_NULL) then
           if (this%split_file) then
              call MPI_COMM_RANK(this%writers_comm,writer_rank, status)
              _verify(status)
              write(fc,'(I0.3)')writer_rank
              fname = "checkpoint_"//fc//".bin"
              open(file=fname,newunit=this%ncid,status='replace',form='unformatted',access='sequential')
           else
              fname = "checkpoint.bin"
              open(file=fname,newunit=this%ncid,status='replace',form='unformatted',access='sequential')
           end if
        end if
     end if
     call MPI_BARRIER(MPI_COMM_WORLD, status)
     _verify(status)
     call system_clock(count=sub_end)
     this%create_file_time = sub_end-sub_start
  end subroutine


  subroutine write_file(this)
     class(test_support), intent(inout) :: this
     integer :: status,i,l,rc

     integer(kind=INT64) :: sub_start,sub_end

     call MPI_BARRIER(MPI_COMM_WORLD, status)
     _verify(status)
     call system_clock(count=sub_start)
     call MPI_BARRIER(MPI_COMM_WORLD, status)
     _verify(status)
     do i=1,this%num_arrays
        if (this%gather_3d) then
           call this%write_variable(this%bundle(i)%field_name,this%bundle(i)%field)
        else
           do l = 1,this%lm
              call this%write_level(this%bundle(i)%field_name,this%bundle(i)%field(:,:,l),l)
           enddo
        end if
     enddo
     call MPI_BARRIER(MPI_COMM_WORLD, status)
     _verify(status)
     call system_clock(count=sub_end)
     call MPI_BARRIER(MPI_COMM_WORLD, status)
     _verify(status)
     this%write_3d_time = sub_end-sub_start
     call MPI_BARRIER(MPI_COMM_WORLD, status)
     _verify(status)
  end subroutine

  subroutine write_variable(this,var_name,local_var)
     class(test_support), intent(inout) :: this
     character(len=*), intent(in) :: var_name
     real, intent(in) :: local_var(:,:,:)
     integer :: status,rc
     real,  allocatable :: recvbuf(:)
     integer                               :: I,J,N,K,L,myrow,myiorank,ndes_x
     integer                               :: start(3), cnt(3)
     integer                               :: jsize, jprev, num_io_rows
     integer, allocatable                  :: recvcounts(:), displs(:)
     integer :: im_world,jm_world,varid
     real, allocatable :: var(:,:,:)
     integer(kind=INT64) :: start_time,end_time,count_rate,lev,start_mpi,end_mpi
     real(kind=REAL64) :: io_time

     call system_clock(count_rate=count_rate)
     call system_clock(count=start_mpi)
     im_world = this%im_world
     jm_world = this%im_world*6
     ndes_x = size(this%in)

       call mpi_comm_rank(this%ycomm,myrow, status)
       _verify(status)
       call mpi_comm_rank(this%gather_comm,myiorank, status)
       _verify(status)
       call mpi_comm_size(this%gather_comm,num_io_rows, status)
       _verify(status)
       num_io_rows=num_io_rows/ndes_x

       allocate (recvcounts(ndes_x*num_io_rows), displs(ndes_x*num_io_rows), stat=status)

       if(myiorank==0) then
          do j=1,num_io_rows
             jsize = this%jn(myrow+j) - this%j1(myrow+j) + 1
             recvcounts((j-1)*ndes_x+1:(j-1)*ndes_x+ndes_x) = ( this%IN -  this%I1 + 1) * jsize * this%lm
          enddo

          displs(1) = 0
          do i=2,ndes_x*num_io_rows
             displs(i) = displs(i-1) + recvcounts(i-1)
          enddo

          jsize = 0
          do j=1,num_io_rows
             jsize=jsize + (this%jn(myrow+j) - this%j1(myrow+j) + 1)
          enddo
          allocate(VAR(IM_WORLD,jsize,this%lm), stat=status)
          _verify(status)
          allocate(recvbuf(IM_WORLD*jsize*this%lm), stat=status)
          _verify(status)
       end if

       if(myiorank/=0) then
          allocate(recvbuf(0), stat=status)
          _verify(status)
       endif

       call mpi_gatherv( local_var, size(local_var), MPI_REAL, recvbuf, recvcounts, displs, MPI_REAL, &
                      0, this%gather_comm, status )
       _verify(status)
       call system_clock(count=end_mpi)
       this%time_mpi = this%mpi_time  + (end_mpi - start_mpi)
       if (this%write_barrier) then
          call MPI_Barrier(MPI_COMM_WORLD, status)
          _verify(status)
       endif

       if(myiorank==0) then

          jprev = 0
          k=1
          do l=1,num_io_rows
            jsize = this%jn(myrow+l) - this%j1(myrow+l) + 1
            do n=1,ndes_x
              do lev =1,this%lm
                 do j=1,jsize
                   do i=this%i1(n),this%in(n)
                     VAR(i,jprev+j,lev) = recvbuf(k)
                     k=k+1
                   end do
                 end do
               enddo
            end do
            jprev = jprev + jsize
          end do
          jsize=jprev

          start(1) = 1
          if (this%split_file) then
             start(2) = 1
          else
             start(2) = this%j1(myrow+1)
          end if
          start(3)= 1
          cnt(1) = IM_WORLD
          cnt(2) = jsize
          cnt(3) = this%lm

          call system_clock(count=start_time)
          if (this%do_writes) then
             if (this%netcdf_writes) then
                status = nf90_inq_varid(this%ncid,name=var_name ,varid=varid)
                _verify(status)
                status = nf90_put_var(this%ncid,varid,var,start,cnt)
                _verify(status)
             else
                write(this%ncid)var
             end if
          end if
          call system_clock(count=end_time)
          this%write_counter = this%write_counter + 1
          io_time = end_time-start_time
          this%data_volume = this%data_volume+byte_to_mega*4.d0*size(var,kind=INT64)
          this%time_writing = this%time_writing + real(io_time,kind=REAL64)/real(count_rate,kind=REAL64)

          deallocate(VAR, stat=status)

       endif ! myiorank

       deallocate(recvbuf, stat=status)
       deallocate (recvcounts, displs, stat=status)

  end subroutine

  subroutine write_level(this,var_name,local_var,z_index)
     class(test_support), intent(inout) :: this
     character(len=*), intent(in) :: var_name
     real, intent(in) :: local_var(:,:)
     integer, intent(in) :: z_index
     integer :: status, rc
     real,  allocatable :: recvbuf(:)
     integer                               :: I,J,N,K,L,myrow,myiorank,ndes_x
     integer                               :: start(3), cnt(3)
     integer                               :: jsize, jprev, num_io_rows
     integer, allocatable                  :: recvcounts(:), displs(:)
     integer :: im_world,jm_world,varid
     real, allocatable :: var(:,:)
     integer(kind=INT64) :: start_time,end_time,count_rate,start_mpi,end_mpi
     real(kind=REAL64) :: io_time

     call system_clock(count_rate=count_rate)
     call system_clock(count=start_mpi)
     im_world = this%im_world
     jm_world = this%im_world*6
     ndes_x = size(this%in)

       call mpi_comm_rank(this%ycomm,myrow, status)
       _verify(status)
       call mpi_comm_rank(this%gather_comm,myiorank, status)
       _verify(status)
       call mpi_comm_size(this%gather_comm,num_io_rows, status)
       _verify(status)
       num_io_rows=num_io_rows/ndes_x

       allocate (recvcounts(ndes_x*num_io_rows), displs(ndes_x*num_io_rows), stat=status)

       if(myiorank==0) then
          do j=1,num_io_rows
             jsize = this%jn(myrow+j) - this%j1(myrow+j) + 1
             recvcounts((j-1)*ndes_x+1:(j-1)*ndes_x+ndes_x) = ( this%IN -  this%I1 + 1) * jsize
          enddo

          displs(1) = 0
          do i=2,ndes_x*num_io_rows
             displs(i) = displs(i-1) + recvcounts(i-1)
          enddo

          jsize = 0
          do j=1,num_io_rows
             jsize=jsize + (this%jn(myrow+j) - this%j1(myrow+j) + 1)
          enddo
          allocate(VAR(IM_WORLD,jsize), stat=status)
          allocate(recvbuf(IM_WORLD*jsize), stat=status)
       end if

       if(myiorank/=0) then
          allocate(recvbuf(0), stat=status)
       endif

       call mpi_gatherv( local_var, size(local_var), MPI_REAL, recvbuf, recvcounts, displs, MPI_REAL, &
                      0, this%gather_comm, status )
       call system_clock(count=end_mpi)
       this%mpi_time = this%mpi_time + (end_mpi - start_mpi)
       if (this%write_barrier) then
          call MPI_Barrier(MPI_COMM_WORLD, status)
          _verify(status)
       endif

       if(myiorank==0) then

          jprev = 0
          k=1
          do l=1,num_io_rows
            jsize = this%jn(myrow+l) - this%j1(myrow+l) + 1
            do n=1,ndes_x
              do j=1,jsize
                do i=this%i1(n),this%in(n)
                  VAR(i,jprev+j) = recvbuf(k)
                  k=k+1
                end do
              end do
            end do
            jprev = jprev + jsize
          end do
          jsize=jprev

          start(1) = 1
          if (this%split_file) then
             start(2) = 1
          else
             start(2) = this%j1(myrow+1)
          end if
          start(3)=z_index
          cnt(1) = IM_WORLD
          cnt(2) = jsize
          cnt(3) = 1

          call system_clock(count=start_time)
          if (this%do_writes) then
             if (this%netcdf_writes) then
                status = nf90_inq_varid(this%ncid,name=var_name ,varid=varid)
                _verify(status)
                status = nf90_put_var(this%ncid,varid,var,start,cnt)
                _verify(status)
             else
                write(this%ncid)var
             end if
          end if
          call system_clock(count=end_time)
          this%write_counter = this%write_counter + 1
          io_time = end_time-start_time
          this%data_volume = this%data_volume+byte_to_mega*4.d0*size(var,kind=INT64)
          this%time_writing = this%time_writing + real(io_time,kind=REAL64)/real(count_rate,kind=REAL64)

          deallocate(VAR, stat=status)

       endif ! myiorank

       deallocate(recvbuf, stat=status)
       deallocate (recvcounts, displs, stat=status)

  end subroutine

end module

#define I_AM_MAIN
#include "MAPL_ErrLog.h"
program checkpoint_tester
   use ESMF
   use MAPL_ErrorHandlingMod
   use mapl_checkpoint_support_mod
   use MPI
   use NetCDF
   use fargparse
   use, intrinsic :: iso_fortran_env, only: REAL64, INT64
   implicit NONE

   integer :: status,rank,writer_size,writer_rank,comm_size,i,rc
   type(test_support) :: support
   integer(kind=INT64) :: start_write,end_time,count_rate,start_app,end_app
   real(kind=REAL64) :: time_sum,write_time,create_time,close_time,write_3d_time,write_2d_time
   real(kind=REAL64) :: application_time,data_volume
   real(kind=REAL64) :: average_volume,average_time
   real(kind=REAL64), allocatable :: total_throughput(:), all_proc_throughput(:)
   real(kind=REAL64) :: mean_throughput, mean_fs_throughput
   real(kind=REAL64) :: std_throughput, std_fs_throughput

   type(StringUnlimitedMap) :: options
   type(cli_options) :: cli

   call system_clock(count=start_app,count_rate=count_rate)
   call MPI_Init(status)
   _verify(status)
   call MPI_Barrier(MPI_COMM_WORLD,status)
   _verify(status)

   call MPI_Comm_Rank(MPI_COMM_WORLD,rank,status)
   _verify(status)
   call MPI_Comm_Size(MPI_COMM_WORLD,comm_size,status)
   _verify(status)
   call ESMF_Initialize(logKindFlag=ESMF_LOGKIND_NONE,mpiCommunicator=MPI_COMM_WORLD)
   call MPI_Barrier(MPI_COMM_WORLD,status)
   _verify(status)

   options = parse_arguments()

   call get_cli_options(options,cli)

   ! if we have it, we load the configuration file
   if (allocated(cli%config_file)) then
      if (rank == 0) write(*,*) "Using configuration file ",cli%config_file
      if (rank == 0) write(*,*) "NOTE: This overrides any other command line options"
      call support%set_parameters_by_config(cli%config_file)
   else
      call support%set_parameters_by_cli(cli)
   end if

   call MPI_Barrier(MPI_COMM_WORLD,status)
   _verify(status)

   call support%create_arrays()
   call MPI_Barrier(MPI_COMM_WORLD,status)
   _verify(status)

   call support%create_communicators()
   call MPI_Barrier(MPI_COMM_WORLD,status)
   _verify(status)

   allocate(total_throughput(support%n_trials))
   allocate(all_proc_throughput(support%n_trials))
   do i=1,support%n_trials
      if (rank == 0) write(*,*)"Trial ",i
      call support%reset()

      call system_clock(count=start_write)
      call MPI_Barrier(MPI_COMM_WORLD, status)
      _verify(status)
      if (support%do_writes) call support%create_file()
      call MPI_Barrier(MPI_COMM_WORLD, status)
      _verify(status)

      call support%write_file()
      call MPI_Barrier(MPI_COMM_WORLD, status)
      _verify(status)

      if (support%do_writes) call support%close_file()
      call MPI_Barrier(MPI_COMM_WORLD, status)
      _verify(status)

      call system_clock(count=end_time)
      write_time = real(end_time-start_write,kind=REAL64)/real(count_rate,kind=REAL64)
      create_time = real(support%create_file_time,kind=REAL64)/real(count_rate,kind=REAL64)
      write_3d_time = real(support%write_3d_time,kind=REAL64)/real(count_rate,kind=REAL64)
      close_time = real(support%close_file_time,kind=REAL64)/real(count_rate,kind=REAL64)
      time_sum = create_time + write_3d_time + close_time
      application_time = real(end_time - start_app,kind=REAL64)/real(count_rate,kind=REAL64)

      if (support%write_counter > 0) then
         call MPI_COMM_SIZE(support%writers_comm,writer_size, status)
         _verify(status)
         call MPI_COMM_RANK(support%writers_comm,writer_rank, status)
         _verify(status)
         call MPI_AllReduce(support%data_volume,average_volume,1,MPI_DOUBLE_PRECISION,MPI_SUM,support%writers_comm, status)
         _verify(status)
         average_volume = average_volume/real(writer_size,kind=REAL64)
         call MPI_AllReduce(support%time_writing,average_time,1,MPI_DOUBLE_PRECISION,MPI_SUM,support%writers_comm, status)
         _verify(status)
         average_time = average_time/real(writer_size,kind=REAL64)
      end if
      if (rank == 0) then
         total_throughput(i) = byte_to_mega*real(support%num_arrays,kind=REAL64)*real(support%im_world,kind=REAL64) &
               *real(support%im_world,kind=REAL64)*6.d0*real(support%lm,kind=REAL64)*4.d0/write_3d_time
         all_proc_throughput(i) = real(support%num_writers,kind=REAL32)*average_volume/average_time
      end if
   enddo

   call system_clock(count=end_app)
   application_time = real(end_app - start_app,kind=REAL64)/real(count_rate,kind=REAL64)
   if (rank == 0) then
      data_volume = byte_to_mega*real(support%num_arrays,kind=REAL64)*real(support%im_world,kind=REAL64) &
            *real(support%im_world,kind=REAL64)*6.d0*real(support%lm,kind=REAL64)*4.d0
      write(*,*)"***************************************************"
      write(*,*)"Summary of run: "
      write(*,'(A,G16.8)')"Total data volume in megabytes: ",data_volume
      write(*,'(A,I3)')"Num writers:  ",support%num_writers
      write(*,'(A,I6)')"Total cores:  ",comm_size
      write(*,'(A,I6,I6)')"Cube size:  ",support%im_world,support%lm
      write(*,'(A,7(L1))')"Split file, 3D_gather, chunk, extra, netcdf output, write barrier, do writes:  ",&
         support%split_file, support%gather_3d, &
         support%do_chunking,support%extra_info, &
         support%netcdf_writes,support%write_barrier, support%do_writes
      write(*,'(A,I6)')"Number of trial:  ",support%n_trials
      write(*,'(A,G16.8)')"Application  time: ",application_time
   end if

   if (rank == 0) then
      write(*,'(A)')"Real throughput MB/s, Std Real throughput MB/s, file system MB/S, std file system MB/s"
      mean_throughput = sum(total_throughput)/real(support%n_trials,kind=REAL64)
      mean_fs_throughput = sum(all_proc_throughput)/real(support%n_trials,kind=REAL64)
      std_throughput = 0.d0
      std_fs_throughput = 0.d0
      do i=1,support%n_trials
         std_throughput = std_throughput + (total_throughput(i)-mean_throughput)**2
         std_fs_throughput = std_fs_throughput + (all_proc_throughput(i)-mean_fs_throughput)**2
      enddo
      std_throughput = sqrt(std_throughput/real(support%n_trials,kind=REAL64))
      std_fs_throughput = sqrt(std_fs_throughput/real(support%n_trials,kind=REAL64))
      write(*,'(G16.8,G16.8,G16.8,G16.8)')mean_throughput,std_throughput,mean_fs_throughput,std_fs_throughput
   end if


   call MPI_Finalize(status)
end program
