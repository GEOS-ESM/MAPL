! Many-small-files benchmark through the ADIOS2 "Plugin" engine with the external
! NetCDF-4 plugin (adios2_nc4_plugin.cpp).
!
! Same ADIOS2 calls as adios2_multifile_cs_writer.x, and the plugin writes the same
! files as nc4_multifile_cs_writer.x. The plugin writes synchronously, so each file
! is closed at the end of its own I/O phase, as in the NetCDF-4 program.
!
! Usage (all arguments optional, key=value):
!   mpirun -np N adios2_nc4plugin_multifile_cs_writer.x dir=. prefix=cs nfiles=100 \
!          im=1440 target_mb=500 [nvar=K] compute_s=5 [nx=..] [ny=..] \
!          [plugin=/path/libadios2_nc4_plugin.so] \
!          [layout=contiguous|chunked] [access=collective|independent] \
!          [put_mode=sync|deferred] [params=Key1=V1,Key2=V2] [verbose=1]
!
!   plugin    PluginLibrary; a bare name is searched in ADIOS2_PLUGIN_PATH
!   layout, access   as in nc4_multifile_cs_writer.x
!   put_mode  sync (default) writes each variable inside Put; deferred writes all
!             of them in EndStep.
!   params    extra plugin parameters
program adios2_nc4plugin_multifile_cs_writer

   use, intrinsic :: iso_fortran_env, only: REAL32, REAL64, INT64, OUTPUT_UNIT
   use mpi
   use adios2

   implicit none

   character(len=512)  :: dir = '.'
   character(len=128)  :: prefix = 'cs'
   character(len=512)  :: plugin_lib = 'adios2_nc4_plugin'
   character(len=32)   :: layout = 'contiguous'
   character(len=32)   :: access = 'collective'
   character(len=32)   :: put_mode = 'sync'
   character(len=1024) :: params = ''
   integer :: im = 1440, nfiles = 100, nvar = -1
   integer :: nx = -1, ny = -1
   real(REAL64) :: target_mb = 500.0d0, compute_s = 5.0d0
   logical :: verbose = .false.

   integer, parameter :: NF = 6
   integer :: ierr, rank, npes, comm
   type(adios2_adios)     :: adios
   type(adios2_io)        :: io
   type(adios2_engine)    :: engine
   type(adios2_variable)  :: v_x, v_y, v_f, v_t
   type(adios2_variable), allocatable :: vars(:)
   type(adios2_attribute) :: attr
   logical :: removed
   integer :: face, ix, jy, nyf, r
   integer :: i1, i2, j1, j2, ni, nj
   integer :: n, ifile, launch
   integer(INT64) :: shape3(3), start3(3), count3(3)
   integer(INT64) :: bytes_per_var, bytes_per_file, total_bytes
   real(REAL32), allocatable :: base(:,:,:), buf(:,:,:), bufs(:,:,:,:)
   real(REAL64), allocatable :: coord(:)
   integer, allocatable :: faces(:)
   real(REAL64), allocatable :: t_io(:)
   real(REAL64) :: time_val
   real(REAL64) :: t0, t1, t_start, t_total
   real(REAL64) :: s_compute, s_close, s_define, s_open, s_put, s_endstep
   real(REAL64) :: tx, td, to, tp, te
   character(len=512) :: file_name
   character(len=64)  :: vname, io_name
   character(len=16)  :: nstr

   call MPI_Init(ierr)
   comm = MPI_COMM_WORLD
   call MPI_Comm_rank(comm, rank, ierr)
   call MPI_Comm_size(comm, npes, ierr)

   call parse_args()

   bytes_per_var = int(im, INT64) * im * NF * 4
   if (nvar <= 0) nvar = max(1, nint(target_mb * 1.0d6 / real(bytes_per_var, REAL64)))
   bytes_per_file = bytes_per_var * nvar
   total_bytes = bytes_per_file * nfiles
   call choose_layout(npes, nx, ny)
   nyf = ny / NF

   ! Map rank -> (face, ix, jy) and its i/j range on that face.
   face = rank / (nx * nyf)
   r    = mod(rank, nx * nyf)
   ix   = mod(r, nx)
   jy   = r / nx
   call block_range(im, nx, ix, i1, i2)
   call block_range(im, nyf, jy, j1, j2)
   ni = i2 - i1 + 1
   nj = j2 - j1 + 1

   if (rank == 0) then
      write(OUTPUT_UNIT, '(a)')         '==== ADIOS2 Plugin (NetCDF-4) parallel cubed-sphere multi-file writer ===='
      write(OUTPUT_UNIT, '(a,a,a,a)')   ' files       : ', trim(dir), '/', trim(prefix)//'_NNNN.nc4'
      write(OUTPUT_UNIT, '(a,a)')       ' plugin      : ', trim(plugin_lib)
      write(OUTPUT_UNIT, '(a,i0)')      ' file count  : ', nfiles
      write(OUTPUT_UNIT, '(a,i0)')      ' grid        : C', im
      write(OUTPUT_UNIT, '(a,i0)')      ' 2-D vars    : ', nvar
      write(OUTPUT_UNIT, '(a,i0,a,i0,a,i0,a)') ' MPI ranks   : ', npes, '  (nx=', nx, ', ny=', ny, ')'
      write(OUTPUT_UNIT, '(a,i0,a,i0)') ' local block : ', ni, ' x ', nj
      write(OUTPUT_UNIT, '(a,a,a,a)')   ' layout      : ', trim(layout), '   access: ', trim(access)
      write(OUTPUT_UNIT, '(a,a)')       ' put mode    : ', trim(put_mode)
      write(OUTPUT_UNIT, '(a,a)')       ' parameters  : ', trim(params)
      write(OUTPUT_UNIT, '(a,f12.3,a)') ' compute     : ', compute_s, ' s per file'
      write(OUTPUT_UNIT, '(a,f12.3,a)') ' per file    : ', real(bytes_per_file, REAL64) / 1.0d6, ' MB'
      write(OUTPUT_UNIT, '(a,f12.3,a)') ' total data  : ', real(total_bytes, REAL64) / 1.0d9, ' GB'
      flush(OUTPUT_UNIT)
   end if

   allocate(base(ni, nj, 1))
   base = real(face * 1000, REAL32)
   if (trim(put_mode) == 'deferred') then
      allocate(bufs(ni, nj, 1, nvar))
      launch = adios2_mode_deferred
   else
      allocate(buf(ni, nj, 1))
      launch = adios2_mode_sync
   end if
   allocate(coord(im), faces(NF))
   coord = [(real(n, REAL64), n = 1, im)]
   faces = [(n, n = 1, NF)]
   allocate(vars(nvar), t_io(nfiles))

   shape3 = [int(im, INT64), int(im, INT64), int(NF, INT64)]
   start3 = [int(i1 - 1, INT64), int(j1 - 1, INT64), int(face, INT64)]
   count3 = [int(ni, INT64), int(nj, INT64), 1_INT64]

   call adios2_init(adios, comm, ierr); call check(ierr, 'adios2_init')

   s_compute = 0; s_close = 0; s_define = 0; s_open = 0; s_put = 0; s_endstep = 0

   call MPI_Barrier(comm, ierr)
   t_start = MPI_Wtime()

   do ifile = 1, nfiles
      t0 = MPI_Wtime()
      call compute(compute_s)
      call MPI_Barrier(comm, ierr)
      t1 = MPI_Wtime()
      s_compute = s_compute + (t1 - t0)

      write(file_name, '(a,a,a,a,i4.4,a)') trim(dir), '/', trim(prefix), '_', ifile, '.nc4'
      write(io_name, '(a,i4.4)') 'cs_', ifile

      ! ---- define -------------------------------------------------------
      t0 = MPI_Wtime()
      call adios2_declare_io(io, adios, trim(io_name), ierr); call check(ierr, 'declare_io')
      call adios2_set_engine(io, 'Plugin', ierr);             call check(ierr, 'set_engine')
      call set_param('PluginName', 'NC4Writer')
      call set_param('PluginLibrary', trim(plugin_lib))
      call set_param('FileName', trim(file_name))
      call set_param('ArrayOrder', 'ColumnMajor')
      call set_param('Layout', trim(layout))
      call set_param('Access', trim(access))
      call set_param('NumSteps', '1')
      write(nstr, '(i0)') comm
      call set_param('MPIComm', trim(nstr))
      call set_params(params)
      call adios2_define_variable(v_x, io, 'Xdim', adios2_type_dp, 1, [int(im, INT64)], &
           [0_INT64], [int(im, INT64)], adios2_constant_dims, ierr); call check(ierr, 'def Xdim')
      call adios2_define_variable(v_y, io, 'Ydim', adios2_type_dp, 1, [int(im, INT64)], &
           [0_INT64], [int(im, INT64)], adios2_constant_dims, ierr); call check(ierr, 'def Ydim')
      call adios2_define_variable(v_f, io, 'nf', adios2_type_integer4, 1, [int(NF, INT64)], &
           [0_INT64], [int(NF, INT64)], adios2_constant_dims, ierr); call check(ierr, 'def nf')
      call adios2_define_variable(v_t, io, 'time', adios2_type_dp, ierr); call check(ierr, 'def time')
      call adios2_define_attribute(attr, io, 'units', 'minutes since 2000-01-01 00:00:00', &
           'time', '/', ierr)
      call check(ierr, 'att time')
      call adios2_define_attribute(attr, io, 'grid_type', 'cubed-sphere', ierr)
      call check(ierr, 'att grid')
      call adios2_define_attribute(attr, io, 'cubed_sphere_resolution', im, ierr)
      call check(ierr, 'att res')
      do n = 1, nvar
         write(vname, '(a,i4.4)') 'var', n
         call adios2_define_variable(vars(n), io, trim(vname), adios2_type_real, 3, &
              shape3, start3, count3, adios2_constant_dims, ierr)
         call check(ierr, 'def '//trim(vname))
         call adios2_define_attribute(attr, io, 'units', '1', trim(vname), '/', ierr)
         call check(ierr, 'att units')
         ! NetCDF dimension names for the plugin; the trailing 'time' makes it a record variable.
         call adios2_define_attribute(attr, io, 'dimensions', 'Xdim Ydim nf time', &
              trim(vname), '/', ierr)
         call check(ierr, 'att dimensions')
      end do
      call MPI_Barrier(comm, ierr)
      td = MPI_Wtime() - t0

      ! ---- open (creates and defines the NetCDF file) -------------------
      t0 = MPI_Wtime()
      call adios2_open(engine, io, trim(file_name), adios2_mode_write, ierr)
      call check(ierr, 'adios2_open')
      call MPI_Barrier(comm, ierr)
      to = MPI_Wtime() - t0

      ! ---- put ----------------------------------------------------------
      t0 = MPI_Wtime()
      call adios2_begin_step(engine, adios2_step_mode_append, ierr); call check(ierr, 'begin_step')
      if (rank == 0) then
         call adios2_put(engine, v_x, coord, adios2_mode_sync, ierr); call check(ierr, 'put Xdim')
         call adios2_put(engine, v_y, coord, adios2_mode_sync, ierr); call check(ierr, 'put Ydim')
         call adios2_put(engine, v_f, faces, adios2_mode_sync, ierr); call check(ierr, 'put nf')
         time_val = real((ifile - 1) * 60, REAL64)
         call adios2_put(engine, v_t, time_val, adios2_mode_sync, ierr); call check(ierr, 'put time')
      end if
      do n = 1, nvar
         if (launch == adios2_mode_deferred) then
            bufs(:, :, :, n) = base + real(n * 0.001 + ifile, REAL32)
            call adios2_put(engine, vars(n), bufs(:, :, :, n), launch, ierr)
         else
            buf = base + real(n * 0.001 + ifile, REAL32)
            call adios2_put(engine, vars(n), buf, launch, ierr)
         end if
         call check(ierr, 'put var')
      end do
      call MPI_Barrier(comm, ierr)
      tp = MPI_Wtime() - t0

      ! ---- end_step (writes deferred puts) ------------------------------
      t0 = MPI_Wtime()
      call adios2_end_step(engine, ierr); call check(ierr, 'end_step')
      call MPI_Barrier(comm, ierr)
      te = MPI_Wtime() - t0

      ! ---- close --------------------------------------------------------
      t0 = MPI_Wtime()
      call adios2_close(engine, ierr);                          call check(ierr, 'adios2_close')
      call adios2_remove_io(removed, adios, trim(io_name), ierr); call check(ierr, 'remove_io')
      call MPI_Barrier(comm, ierr)
      tx = MPI_Wtime() - t0

      s_close   = s_close + tx
      s_define  = s_define + td
      s_open    = s_open + to
      s_put     = s_put + tp
      s_endstep = s_endstep + te
      t_io(ifile) = td + to + tp + te + tx
      if (verbose .and. rank == 0) then
         write(OUTPUT_UNIT, '(a,i4.4,a,f9.3,a,f9.3,a,f9.3,a,f9.3,a,f9.3,a,f9.3,a,f9.3,a)') ' file ', ifile, &
              ': define ', td, '  open ', to, '  put ', tp, '  end_step ', te, '  close ', tx, &
              '  io ', t_io(ifile), ' s  ', &
              real(bytes_per_file, REAL64) / 1.0d9 / max(t_io(ifile), 1.0d-12), ' GB/s'
         flush(OUTPUT_UNIT)
      end if
   end do

   t_total = MPI_Wtime() - t_start

   if (rank == 0) then
      call report_io(t_io)
      write(OUTPUT_UNIT, '(a,f12.3)')   ' compute     : ', s_compute
      write(OUTPUT_UNIT, '(a,f12.3)')   ' define      : ', s_define
      write(OUTPUT_UNIT, '(a,f12.3)')   ' open        : ', s_open
      write(OUTPUT_UNIT, '(a,f12.3)')   ' put (all)   : ', s_put
      write(OUTPUT_UNIT, '(a,f12.3)')   ' end_step    : ', s_endstep
      write(OUTPUT_UNIT, '(a,f12.3)')   ' data write  : ', s_put + s_endstep
      write(OUTPUT_UNIT, '(a,f12.3)')   ' close       : ', s_close
      write(OUTPUT_UNIT, '(a,f12.3)')   ' io total    : ', sum(t_io)
      write(OUTPUT_UNIT, '(a,f12.3)')   ' total       : ', t_total
      call report_bandwidth(sum(t_io), t_total, s_compute)
   end if

   call adios2_finalize(adios, ierr)
   call MPI_Finalize(ierr)

contains

   subroutine set_param(key, val)
      character(len=*), intent(in) :: key, val
      call adios2_set_parameter(io, key, val, ierr)
      call check(ierr, 'set_parameter '//key)
   end subroutine set_param

   ! Busy loop, so the compute phase occupies the cores like a model would.
   subroutine compute(seconds)
      real(REAL64), intent(in) :: seconds
      real(REAL64) :: t_end
      t_end = MPI_Wtime() + seconds
      do while (MPI_Wtime() < t_end)
      end do
   end subroutine compute

   subroutine report_io(t)
      real(REAL64), intent(in) :: t(:)
      real(REAL64) :: s(size(t))
      call sort(t, s)
      write(OUTPUT_UNIT, '(a)')       '---- per-file I/O time (seconds, wall clock after barrier) ----'
      write(OUTPUT_UNIT, '(a,f12.3)') ' io min      : ', s(1)
      write(OUTPUT_UNIT, '(a,f12.3)') ' io median   : ', s((size(s) + 1) / 2)
      write(OUTPUT_UNIT, '(a,f12.3)') ' io max      : ', s(size(s))
      write(OUTPUT_UNIT, '(a)')       '---- totals (seconds) ----'
   end subroutine report_io

   subroutine report_bandwidth(io_time, total, comp)
      real(REAL64), intent(in) :: io_time, total, comp
      write(OUTPUT_UNIT, '(a,f12.3,a)') ' bandwidth (io total)           : ', &
           real(total_bytes, REAL64) / 1.0d9 / max(io_time, 1.0d-12), ' GB/s'
      write(OUTPUT_UNIT, '(a,f12.3,a)') ' io overhead (total - compute)  : ', total - comp, ' s'
      write(OUTPUT_UNIT, '(a,f12.3,a)') ' io fraction of wall clock      : ', &
           100.0d0 * (total - comp) / max(total, 1.0d-12), ' %'
      flush(OUTPUT_UNIT)
   end subroutine report_bandwidth

   subroutine sort(a, s)
      real(REAL64), intent(in) :: a(:)
      real(REAL64), intent(out) :: s(:)
      integer :: i, j
      real(REAL64) :: x
      s = a
      do i = 2, size(s)
         x = s(i)
         j = i - 1
         do while (j >= 1)
            if (s(j) <= x) exit
            s(j + 1) = s(j)
            j = j - 1
         end do
         s(j + 1) = x
      end do
   end subroutine sort

   subroutine parse_args()
      integer :: iarg, eq
      character(len=1024) :: arg, key, val
      do iarg = 1, command_argument_count()
         call get_command_argument(iarg, arg)
         eq = index(arg, '=')
         if (eq == 0) call fatal('argument must be key=value: '//trim(arg))
         key = arg(1:eq-1)
         val = arg(eq+1:)
         select case (trim(key))
         case ('dir');       dir = val
         case ('prefix');    prefix = val
         case ('nfiles');    read(val, *) nfiles
         case ('im');        read(val, *) im
         case ('nvar');      read(val, *) nvar
         case ('nx');        read(val, *) nx
         case ('ny');        read(val, *) ny
         case ('target_mb'); read(val, *) target_mb
         case ('compute_s'); read(val, *) compute_s
         case ('plugin');    plugin_lib = val
         case ('layout');    layout = val
         case ('access');    access = val
         case ('put_mode');  put_mode = val
         case ('params');    params = val
         case ('verbose');   verbose = (trim(val) == '1' .or. trim(val) == 'true')
         case default
            call fatal('unknown argument: '//trim(key))
         end select
      end do
      if (nfiles < 1) call fatal('nfiles must be positive')
      if (trim(layout) /= 'contiguous' .and. trim(layout) /= 'chunked') call fatal('layout must be contiguous|chunked')
      if (trim(access) /= 'collective' .and. trim(access) /= 'independent') call fatal('access must be collective|independent')
      if (trim(put_mode) /= 'sync' .and. trim(put_mode) /= 'deferred') call fatal('put_mode must be sync|deferred')
   end subroutine parse_args

   ! params is a comma-separated list of Key=Value engine parameters.
   subroutine set_params(list)
      character(len=*), intent(in) :: list
      integer :: p, q, eq
      character(len=256) :: item
      p = 1
      do while (p <= len_trim(list))
         q = index(list(p:), ',')
         if (q == 0) then
            item = list(p:len_trim(list))
            p = len_trim(list) + 1
         else
            item = list(p:p+q-2)
            p = p + q
         end if
         if (len_trim(item) == 0) cycle
         eq = index(item, '=')
         if (eq == 0) call fatal('engine parameter must be Key=Value: '//trim(item))
         call adios2_set_parameter(io, item(1:eq-1), trim(item(eq+1:)), ierr)
         call check(ierr, 'set_parameter '//trim(item))
      end do
   end subroutine set_params

   ! Pick nx*ny == npes with ny a multiple of 6 and each face block close to square.
   subroutine choose_layout(npes, nx, ny)
      integer, intent(in) :: npes
      integer, intent(inout) :: nx, ny
      integer :: per_face, a, best
      if (nx > 0 .and. ny > 0) then
         if (nx * ny /= npes) call fatal('nx*ny must equal the number of MPI ranks')
         if (mod(ny, NF) /= 0) call fatal('ny must be a multiple of 6')
         return
      end if
      if (mod(npes, NF) /= 0) call fatal('number of MPI ranks must be a multiple of 6')
      per_face = npes / NF
      best = 1
      do a = 1, int(sqrt(real(per_face))) + 1
         if (a <= per_face) then
            if (mod(per_face, a) == 0) best = a
         end if
      end do
      nx = best
      ny = (per_face / best) * NF
      if (nx > im .or. ny / NF > im) call fatal('too many ranks for this grid')
   end subroutine choose_layout

   ! 1-based [lo, hi] of block b (0-based) when n points are split over nb blocks.
   subroutine block_range(n, nb, b, lo, hi)
      integer, intent(in) :: n, nb, b
      integer, intent(out) :: lo, hi
      integer :: q, rem
      q = n / nb
      rem = mod(n, nb)
      lo = b * q + min(b, rem) + 1
      hi = lo + q - 1
      if (b < rem) hi = hi + 1
   end subroutine block_range

   subroutine check(stat, where)
      integer, intent(in) :: stat
      character(len=*), intent(in) :: where
      integer :: ie
      if (stat /= adios2_error_none) then
         write(OUTPUT_UNIT, '(a,i0,a,a,a,i0)') 'rank ', rank, ': ', where, ': ADIOS2 error ', stat
         flush(OUTPUT_UNIT)
         call MPI_Abort(MPI_COMM_WORLD, 1, ie)
      end if
   end subroutine check

   subroutine fatal(msg)
      character(len=*), intent(in) :: msg
      integer :: ie
      if (rank == 0) write(OUTPUT_UNIT, '(a,a)') 'ERROR: ', msg
      flush(OUTPUT_UNIT)
      call MPI_Abort(MPI_COMM_WORLD, 1, ie)
   end subroutine fatal

end program adios2_nc4plugin_multifile_cs_writer
