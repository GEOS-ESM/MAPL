#include "MAPL_ErrLog.h"
module mapl_restart_support_mod

   use ESMF
   use MPI
   use NetCDF
   use MAPL_ErrorHandlingMod
   use MAPL_MemUtilsMod
   use, intrinsic :: iso_fortran_env, only: INT64, REAL64, REAL32
   implicit none (type)

   real(kind=REAL64), parameter :: byte_to_mega = (1.0d0/1024.0d0)*(1.0d0/1024.0d0)
   type array_wrapper
      character(len=:), allocatable :: field_name
      real, allocatable :: field(:,:,:)
   end type

   type test_support
      integer :: nx,ny,im_world,lm,num_arrays,num_readers,my_rank
      integer :: scatter_comm
      integer :: readers_comm
      integer :: xcomm
      integer :: ycomm
      integer :: ncid
      integer, allocatable :: i1(:),in(:),j1(:),jn(:)
      type(array_wrapper), allocatable :: bundle(:)
      integer :: face_index
      integer(kind=INT64) :: read_counter
      logical :: scatter_3d
      logical :: split_file
      logical :: extra_info
      logical :: read_barrier
      logical :: do_reads
      real(kind=REAL64) :: data_volume
      real(kind=REAL64) :: time_reading
      real(kind=REAL64) :: time_mpi
      logical :: netcdf_reads
      integer :: n_trials
      logical :: random

      integer(kind=INT64) :: mpi_time
      integer(kind=INT64) :: read_3d_time
      integer(kind=INT64) :: read_2d_time
      integer(kind=INT64) :: open_file_time
      integer(kind=INT64) :: close_file_time
      contains
         procedure :: set_parameters
         procedure :: compute_decomposition
         procedure :: allocate_n_arrays
         procedure :: create_arrays
         procedure :: create_communicators
         procedure :: open_file
         procedure :: close_file
         procedure :: read_file
         procedure :: read_level
         procedure :: read_variable
         procedure :: reset
   end type

contains

   subroutine set_parameters(this,config_file)
      class(test_support), intent(inout) :: this
      character(len=*), intent(in) :: config_file
      type(ESMF_Config) :: config

      integer :: comm_size, status,error_code

      config = ESMF_ConfigCreate()
      this%extra_info = .false.
      this%read_barrier = .false.
      this%do_reads = .true.
      call ESMF_ConfigLoadFile(config,config_file)
      call ESMF_ConfigGetAttribute(config,this%nx,label="NX:")
      call ESMF_ConfigGetAttribute(config,this%ny,label="NY:")
      call ESMF_ConfigGetAttribute(config,this%im_world,label="IM_WORLD:")
      call ESMF_ConfigGetAttribute(config,this%lm,label="LM:")
      call ESMF_ConfigGetAttribute(config,this%num_readers,label="NUM_READERS")
      call ESMF_ConfigGetAttribute(config,this%num_arrays,label="NUM_ARRAYS:")
      this%scatter_3d = get_logical_key(config,"SCATTER_3D:",.false.)
      this%split_file = get_logical_key(config,"SPLIT_FILE:",.false.)
      this%extra_info = get_logical_key(config,"EXTRA_INFO:",.false.)
      this%read_barrier = get_logical_key(config,"read_BARRIER:",.false.)
      this%do_reads = get_logical_key(config,"DO_READS:",.true.)
      this%netcdf_reads = get_logical_key(config,"netcdf_reads:",.true.)
      this%n_trials = get_integer_key(config,"NTRIALS:",1)
      this%random = get_logical_key(config,"RANDOM_DATA:",.true.)

      this%read_counter = 0
      this%read_3d_time = 0
      this%read_2d_time = 0
      this%open_file_time = 0
      this%close_file_time = 0
      this%data_volume = 0.d0
      this%time_reading = 0.d0
      this%mpi_time = 0.0
      call MPI_COMM_SIZE(MPI_COMM_WORLD,comm_size,status)
      if (comm_size /= (this%nx*this%ny*6)) call MPI_Abort(mpi_comm_world,error_code,status)

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

   end subroutine

   subroutine reset(this)
      class(test_support), intent(inout) :: this
      this%read_counter = 0
      this%read_3d_time = 0
      this%read_2d_time = 0
      this%open_file_time = 0
      this%close_file_time = 0
      this%data_volume = 0.d0
      this%time_reading = 0.d0
      this%mpi_time = 0.0
   end subroutine

   function compute_decomposition(this,axis) result(decomp)
      integer, allocatable :: decomp(:)
      class(test_support), intent(inout) :: this
      integer, intent(in) :: axis

      integer :: n_loc, rm, im, n

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

      integer :: n,rank,status
      character(len=3) :: formatted_int
      integer :: seed_size
      integer, allocatable :: seeds(:)

      call MPI_COMM_RANK(MPI_COMM_WORLD,rank,status)
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
      integer :: rank, status,comm_size,n,i,j,rank_counter,offset,index_offset

      call MPI_Comm_Rank(MPI_COMM_WORLD,rank,status)
      call MPI_Comm_Size(MPI_COMM_WORLD,comm_size,status)
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

     integer :: myid,status,nx0,ny0,color,j,ny_by_readers,local_ny

     local_ny = this%ny*6
     call MPI_Comm_Rank(mpi_comm_world,myid,status)
     nx0 = mod(myid,this%nx) + 1
     ny0 = myid/this%nx + 1
     color = nx0
     call MPI_Comm_Split(MPI_COMM_WORLD,color,myid,this%ycomm,status)
     color = ny0
     call MPI_Comm_Split(MPI_COMM_WORLD,color,myid,this%xcomm,status)


     ny_by_readers = local_ny/this%num_readers
     if (mod(myid,(this%nx*local_ny)/this%num_readers) == 0) then
        color = 0
     else
        color = MPI_UNDEFINED
     end if
     call MPI_COMM_SPLIT(MPI_COMM_WORLD,color,myid,this%readers_comm,status)

     if (this%num_readers == local_ny) then
        this%scatter_comm = this%xcomm
     else
        j = ny0 - mod(ny0-1,ny_by_readers)
        call MPI_COMM_SPLIT(MPI_COMM_WORLD,j,myid,this%scatter_comm,status)
     end if

     call MPI_BARRIER(mpi_comm_world,status)


  end subroutine

  subroutine close_file(this)
     class(test_support), intent(inout) :: this

     integer :: status

     integer(kind=INT64) :: sub_start,sub_end

     call system_clock(count=sub_start)

     if (this%readers_comm /= MPI_COMM_NULL) then
        if (this%netcdf_reads) then
           status = nf90_close(this%ncid)
        else
           close(this%ncid)
        end if
     end if
     call MPI_BARRIER(MPI_COMM_WORLD,status)
     call system_clock(count=sub_end)
     this%close_file_time =  sub_end-sub_start
  end subroutine

  subroutine open_file(this)
     class(test_support), intent(inout) :: this

     integer :: status, rc
     integer :: info
     integer :: xdim,ydim,zdim,i,varid,create_mode
     character(len=:), allocatable :: fname
     character(len=3) :: fc
     integer(kind=INT64) :: sub_start,sub_end
     integer :: writer_rank

     call system_clock(count=sub_start)
     if (this%netcdf_reads) then

        create_mode = NF90_NOWRITE
        create_mode = IOR(create_mode,NF90_SHARE)
        create_mode = IOR(create_mode,NF90_MPIIO)
        call MPI_INFO_CREATE(info,status)
        call MPI_INFO_SET(info,"cb_buffer_size","16777216",status)
        call MPI_INFO_SET(info,"romio_cb_write","enable",status)
        if (this%extra_info) then
           call MPI_INFO_SET(info,"IBM_largeblock_io","true",status)
           call MPI_INFO_SET(info,"striping_unit","4194304",status)
        end if
        if (this%readers_comm /= MPI_COMM_NULL) then
           if (this%split_file) then
              call MPI_COMM_RANK(this%readers_comm,writer_rank,status)
              write(fc,'(I0.3)')writer_rank
              fname = "checkpoint_"//fc//".nc4"
              status = nf90_open(fname,ior(NF90_NETCDF4,NF90_CLOBBER), this%ncid)
           else
              fname = "checkpoint.nc4"
              status = nf90_open(fname,create_mode, this%ncid, comm=this%readers_comm, info=info)
           end if
        end if
     else
        if (this%readers_comm /= MPI_COMM_NULL) then
           if (this%split_file) then
              call MPI_COMM_RANK(this%readers_comm,writer_rank,status)
              write(fc,'(I0.3)')writer_rank
              fname = "checkpoint_"//fc//".bin"
              open(file=fname,newunit=this%ncid,status='old',form='unformatted',access='sequential')
           end if
        end if
     end if
     call MPI_BARRIER(MPI_COMM_WORLD,status)
     call system_clock(count=sub_end)
     this%open_file_time = sub_end-sub_start
  end subroutine


  subroutine read_file(this)
     class(test_support), intent(inout) :: this
     integer :: status,i,l

     integer(kind=INT64) :: sub_start,sub_end

     call MPI_BARRIER(MPI_COMM_WORLD,status)
     call system_clock(count=sub_start)
     call MPI_BARRIER(MPI_COMM_WORLD,status)
     do i=1,this%num_arrays
        if (this%scatter_3d) then
           call this%read_variable(this%bundle(i)%field_name,this%bundle(i)%field)
        else
           do l = 1,this%lm
              call this%read_level(this%bundle(i)%field_name,this%bundle(i)%field(:,:,l),l)
           enddo
        end if
     enddo
     call MPI_BARRIER(MPI_COMM_WORLD,status)
     call system_clock(count=sub_end)
     call MPI_BARRIER(MPI_COMM_WORLD,status)
     this%read_3d_time = sub_end-sub_start
     call MPI_BARRIER(MPI_COMM_WORLD,status)
  end subroutine

  subroutine read_variable(this,var_name,local_var)
     class(test_support), intent(inout) :: this
     character(len=*), intent(in) :: var_name
     real, intent(in) :: local_var(:,:,:)
     integer :: status
     real,  allocatable :: buf(:)
     integer                               :: I,J,N,K,L,myrow,myiorank,ndes_x
     integer                               :: start(3), cnt(3)
     integer                               :: jsize, jprev, num_io_rows
     integer, allocatable                  :: sendcounts(:), displs(:)
     integer :: im_world,jm_world,varid
     real, allocatable :: var(:,:,:)
     integer(kind=INT64) :: start_time,end_time,count_rate,lev,start_mpi,end_mpi
     real(kind=REAL64) :: io_time

     call system_clock(count_rate=count_rate)
     im_world = this%im_world
     jm_world = this%im_world*6
     ndes_x = size(this%in)

       call mpi_comm_rank(this%ycomm,myrow,status)
       call mpi_comm_rank(this%scatter_comm,myiorank,status)
       call mpi_comm_size(this%scatter_comm,num_io_rows,status)
       num_io_rows=num_io_rows/ndes_x

       allocate (sendcounts(ndes_x*num_io_rows), displs(ndes_x*num_io_rows), stat=status)

       if(myiorank==0) then
          do j=1,num_io_rows
             jsize = this%jn(myrow+j) - this%j1(myrow+j) + 1
             sendcounts((j-1)*ndes_x+1:(j-1)*ndes_x+ndes_x) = ( this%IN -  this%I1 + 1) * jsize * this%lm
          enddo

          displs(1) = 0
          do i=2,ndes_x*num_io_rows
             displs(i) = displs(i-1) + sendcounts(i-1)
          enddo

          jsize = 0
          do j=1,num_io_rows
             jsize=jsize + (this%jn(myrow+j) - this%j1(myrow+j) + 1)
          enddo
          allocate(VAR(IM_WORLD,jsize,this%lm), stat=status)
          allocate(buf(IM_WORLD*jsize*this%lm), stat=status)

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
          if (this%do_reads) then
             if (this%netcdf_reads) then
                status = nf90_inq_varid(this%ncid,name=var_name ,varid=varid)
                status = nf90_get_var(this%ncid,varid,var,start,cnt)
             else
                write(this%ncid)var
             end if
          else
             var=this%my_rank
          end if
          call system_clock(count=end_time)
          this%read_counter = this%read_counter + 1
          io_time = end_time-start_time
          this%data_volume = this%data_volume+byte_to_mega*4.d0*size(var,kind=INT64)
          this%time_reading = this%time_reading + real(io_time,kind=REAL64)/real(count_rate,kind=REAL64)

          jprev = 0
          k=1
          do l=1,num_io_rows
            jsize = this%jn(myrow+l) - this%j1(myrow+l) + 1
            do n=1,ndes_x
              do lev =1,this%lm
                 do j=1,jsize
                   do i=this%i1(n),this%in(n)
                     buf(k) = VAR(i,jprev+j, lev)
                     k=k+1
                   end do
                 end do
               enddo
            end do
            jprev = jprev + jsize
          end do
          jsize=jprev

          deallocate(VAR, stat=status)
       end if

       if(myiorank/=0) then
          allocate(buf(0), stat=status)
       endif

       call system_clock(count=start_mpi)
       call mpi_scatterv( buf, sendcounts, displs, MPI_REAL, local_var, size(local_var), MPI_REAL, &
                      0, this%scatter_comm, status )
       call system_clock(count=end_mpi)
       this%time_mpi = this%mpi_time  + (end_mpi - start_mpi)
       if (this%read_barrier) call MPI_Barrier(MPI_COMM_WORLD,status)

       deallocate(buf, stat=status)
       deallocate (sendcounts, displs, stat=status)

  end subroutine

  subroutine read_level(this,var_name,local_var,z_index)
     class(test_support), intent(inout) :: this
     character(len=*), intent(in) :: var_name
     real, intent(in) :: local_var(:,:)
     integer, intent(in) :: z_index
     integer :: status
     real,  allocatable :: buf(:)
     integer                               :: I,J,N,K,L,myrow,myiorank,ndes_x
     integer                               :: start(3), cnt(3)
     integer                               :: jsize, jprev, num_io_rows
     integer, allocatable                  :: sendcounts(:), displs(:)
     integer :: im_world,jm_world,varid
     real, allocatable :: var(:,:)
     integer(kind=INT64) :: start_time,end_time,count_rate,start_mpi,end_mpi
     real(kind=REAL64) :: io_time

     call system_clock(count_rate=count_rate)
     im_world = this%im_world
     jm_world = this%im_world*6
     ndes_x = size(this%in)

       call mpi_comm_rank(this%ycomm,myrow,status)
       call mpi_comm_rank(this%scatter_comm,myiorank,status)
       call mpi_comm_size(this%scatter_comm,num_io_rows,status)
       num_io_rows=num_io_rows/ndes_x

       allocate (sendcounts(ndes_x*num_io_rows), displs(ndes_x*num_io_rows), stat=status)

       if(myiorank==0) then
          do j=1,num_io_rows
             jsize = this%jn(myrow+j) - this%j1(myrow+j) + 1
             sendcounts((j-1)*ndes_x+1:(j-1)*ndes_x+ndes_x) = ( this%IN -  this%I1 + 1) * jsize
          enddo

          displs(1) = 0
          do i=2,ndes_x*num_io_rows
             displs(i) = displs(i-1) + sendcounts(i-1)
          enddo

          jsize = 0
          do j=1,num_io_rows
             jsize=jsize + (this%jn(myrow+j) - this%j1(myrow+j) + 1)
          enddo
          allocate(VAR(IM_WORLD,jsize), stat=status)
          allocate(buf(IM_WORLD*jsize), stat=status)

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
          if (this%do_reads) then
             if (this%netcdf_reads) then
                status = nf90_inq_varid(this%ncid,name=var_name ,varid=varid)
                status = nf90_get_var(this%ncid,varid,var,start,cnt)
             else
                read(this%ncid)var
             end if
          else
             var=this%my_rank
          end if
          call system_clock(count=end_time)
          this%read_counter = this%read_counter + 1
          io_time = end_time-start_time
          this%data_volume = this%data_volume+byte_to_mega*4.d0*size(var,kind=INT64)
          this%time_reading = this%time_reading + real(io_time,kind=REAL64)/real(count_rate,kind=REAL64)

          jprev = 0
          k=1
          do l=1,num_io_rows
            jsize = this%jn(myrow+l) - this%j1(myrow+l) + 1
            do n=1,ndes_x
              do j=1,jsize
                do i=this%i1(n),this%in(n)
                  buf(k) = var(i,jprev+j)
                  k=k+1
                end do
              end do
            end do
            jprev = jprev + jsize
          end do
          jsize=jprev


          deallocate(VAR, stat=status)
       end if

       if(myiorank/=0) then
          allocate(buf(0), stat=status)
       endif

      call system_clock(count=start_mpi)
       call mpi_scatterv( buf, sendcounts, displs, MPI_REAL, local_var, size(local_var),  MPI_REAL, &
                      0, this%scatter_comm, status )
       call system_clock(count=end_mpi)
       this%mpi_time = this%mpi_time + (end_mpi - start_mpi)
       if (this%read_barrier) call MPI_Barrier(MPI_COMM_WORLD,status)

       deallocate(buf, stat=status)
       deallocate (sendcounts, displs, stat=status)

  end subroutine

end module

#include "MAPL_ErrLog.h"
program checkpoint_tester
   use ESMF
   use MPI
   use NetCDF
   use mapl_restart_support_mod
   use, intrinsic :: iso_fortran_env, only: REAL64, INT64
   implicit none (type)

   integer :: status,rank,reader_size,reader_rank,comm_size,i
   type(test_support) :: support
   integer(kind=INT64) :: start_read,end_time,count_rate,start_app,end_app
   real(kind=REAL64) :: time_sum,read_time,create_time,close_time,read_3d_time,read_2d_time
   real(kind=REAL64) :: application_time,data_volume
   real(kind=REAL64) :: average_volume,average_time
   real(kind=REAL64), allocatable :: total_throughput(:), all_proc_throughput(:)
   real(kind=REAL64) :: mean_throughput, mean_fs_throughput
   real(kind=REAL64) :: std_throughput, std_fs_throughput

   call system_clock(count=start_app,count_rate=count_rate)
   call MPI_Init(status)
   call MPI_Barrier(MPI_COMM_WORLD,status)

   call MPI_Comm_Rank(MPI_COMM_WORLD,rank,status)
   support%my_rank = rank
   call MPI_Comm_Size(MPI_COMM_WORLD,comm_size,status)
   call ESMF_Initialize(logKindFlag=ESMF_LOGKIND_NONE,mpiCommunicator=MPI_COMM_WORLD)
   call MPI_Barrier(MPI_COMM_WORLD,status)

   call support%set_parameters("restart_benchmark.rc")
   call MPI_Barrier(MPI_COMM_WORLD,status)

   call support%create_arrays()
   call MPI_Barrier(MPI_COMM_WORLD,status)

   call support%create_communicators()
   call MPI_Barrier(MPI_COMM_WORLD,status)

   allocate(total_throughput(support%n_trials))
   allocate(all_proc_throughput(support%n_trials))
   do i=1,support%n_trials
      if (rank == 0) write(*,*)"Trial ",i
      call support%reset()

      call system_clock(count=start_read)
      call MPI_Barrier(MPI_COMM_WORLD,status)
      call support%open_file()
      call MPI_Barrier(MPI_COMM_WORLD,status)

      call support%read_file()
      call MPI_Barrier(MPI_COMM_WORLD,status)

      call support%close_file()
      call MPI_Barrier(MPI_COMM_WORLD,status)

      call system_clock(count=end_time)
      read_time = real(end_time-start_read,kind=REAL64)/real(count_rate,kind=REAL64)
      create_time = real(support%open_file_time,kind=REAL64)/real(count_rate,kind=REAL64)
      read_3d_time = real(support%read_3d_time,kind=REAL64)/real(count_rate,kind=REAL64)
      close_time = real(support%close_file_time,kind=REAL64)/real(count_rate,kind=REAL64)
      time_sum = create_time + read_3d_time + close_time
      application_time = real(end_time - start_app,kind=REAL64)/real(count_rate,kind=REAL64)

      if (support%readers_comm /= MPI_COMM_NULL) then
         call MPI_COMM_SIZE(support%readers_comm,reader_size,status)
         call MPI_COMM_RANK(support%readers_comm,reader_rank,status)
         call MPI_AllReduce(support%data_volume,average_volume,1,MPI_DOUBLE_PRECISION,MPI_SUM,support%readers_comm,status)
         average_volume = average_volume/real(reader_size,kind=REAL64)
         call MPI_AllReduce(support%time_reading,average_time,1,MPI_DOUBLE_PRECISION,MPI_SUM,support%readers_comm,status)
         average_time = average_time/real(reader_size,kind=REAL64)
      end if
      if (rank == 0) then
         total_throughput(i) = byte_to_mega*real(support%num_arrays,kind=REAL64)*real(support%im_world,kind=REAL64) &
               *real(support%im_world,kind=REAL64)*6.d0*real(support%lm,kind=REAL64)*4.d0/read_3d_time
         all_proc_throughput(i) = real(support%num_readers,kind=REAL32)*average_volume/average_time
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
      write(*,'(A,I3)')"Num readers:  ",support%num_readers
      write(*,'(A,I6)')"Total cores:  ",comm_size
      write(*,'(A,I6,I6)')"Cube size/LM:  ",support%im_world,support%lm
      write(*,'(A,6(L1))')"Split file, 3D_scatter, extra, netcdf output, write barrier, do writes:  ",&
         support%split_file, support%scatter_3d, &
         support%extra_info, &
         support%netcdf_reads,support%read_barrier, support%do_reads
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
