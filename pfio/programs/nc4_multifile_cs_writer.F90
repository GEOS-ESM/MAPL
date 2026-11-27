! Parallel NetCDF-4 (HDF5/MPI-IO) benchmark: many small cubed-sphere files.
!
! Writes nfiles files one after another, like model output times. Before each file
! every rank runs a busy compute phase of compute_s seconds; the I/O phase is
! create, define, write, close. Each file holds nvar float32 2-D variables
! var(Xdim, Ydim, nf, time) with one time record (C1440: 49.8 MB per variable).
! Each face is split into nx x (ny/6) blocks, so the number of MPI ranks must equal
! nx*ny with ny a multiple of 6.
!
! Usage (all arguments optional, key=value):
!   mpirun -np N nc4_multifile_cs_writer.x dir=. prefix=cs nfiles=100 im=1440 \
!          target_mb=500 [nvar=K] compute_s=5 [nx=..] [ny=..] \
!          [layout=contiguous|chunked] [access=collective|independent] [verbose=1]
!
!   nvar      2-D variables per file (default: nearest to target_mb)
!   layout    contiguous (default) or chunked, chunk = (im, im, 1, 1)
program nc4_multifile_cs_writer

   use, intrinsic :: iso_fortran_env, only: REAL32, REAL64, INT64, OUTPUT_UNIT
   use mpi
   use netcdf

   implicit none

   character(len=512) :: dir = '.'
   character(len=128) :: prefix = 'cs'
   character(len=32)  :: layout = 'contiguous'
   character(len=32)  :: access = 'collective'
   integer :: im = 1440, nfiles = 100, nvar = -1
   integer :: nx = -1, ny = -1
   real(REAL64) :: target_mb = 500.0d0, compute_s = 5.0d0
   logical :: verbose = .false.

   integer, parameter :: NF = 6
   integer :: ierr, rank, npes, comm, info
   integer :: ncid, cmode, old_mode
   integer :: dim_x, dim_y, dim_f, dim_t
   integer :: vid_x, vid_y, vid_f, vid_t
   integer, allocatable :: varids(:)
   integer :: face, ix, jy, nyf, r
   integer :: i1, i2, j1, j2, ni, nj
   integer :: n, ifile
   integer :: start(4), count(4)
   integer(INT64) :: bytes_per_var, bytes_per_file, total_bytes
   real(REAL32), allocatable :: base(:,:,:), buf(:,:,:)
   real(REAL64), allocatable :: coord(:)
   real(REAL64), allocatable :: t_io(:)
   real(REAL64) :: t0, t1, t_start, t_total
   real(REAL64) :: s_compute, s_create, s_define, s_write, s_close
   real(REAL64) :: tc, td, tw, tx
   character(len=512) :: file_name
   character(len=64)  :: vname

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
      write(OUTPUT_UNIT, '(a)')         '==== NetCDF-4 parallel cubed-sphere multi-file writer ===='
      write(OUTPUT_UNIT, '(a,a,a,a)')   ' files       : ', trim(dir), '/', trim(prefix)//'_NNNN.nc4'
      write(OUTPUT_UNIT, '(a,i0)')      ' file count  : ', nfiles
      write(OUTPUT_UNIT, '(a,i0)')      ' grid        : C', im
      write(OUTPUT_UNIT, '(a,i0)')      ' 2-D vars    : ', nvar
      write(OUTPUT_UNIT, '(a,i0,a,i0,a,i0,a)') ' MPI ranks   : ', npes, '  (nx=', nx, ', ny=', ny, ')'
      write(OUTPUT_UNIT, '(a,i0,a,i0)') ' local block : ', ni, ' x ', nj
      write(OUTPUT_UNIT, '(a,a,a,a)')   ' layout      : ', trim(layout), '   access: ', trim(access)
      write(OUTPUT_UNIT, '(a,f12.3,a)') ' compute     : ', compute_s, ' s per file'
      write(OUTPUT_UNIT, '(a,f12.3,a)') ' per file    : ', real(bytes_per_file, REAL64) / 1.0d6, ' MB'
      write(OUTPUT_UNIT, '(a,f12.3,a)') ' total data  : ', real(total_bytes, REAL64) / 1.0d9, ' GB'
      flush(OUTPUT_UNIT)
   end if

   allocate(base(ni, nj, 1), buf(ni, nj, 1))
   base = real(face * 1000, REAL32)
   allocate(coord(im))
   coord = [(real(n, REAL64), n = 1, im)]
   allocate(varids(nvar), t_io(nfiles))

   call MPI_Info_create(info, ierr)
   call MPI_Info_set(info, 'romio_cb_write', 'enable', ierr)
   call MPI_Info_set(info, 'romio_ds_write', 'disable', ierr)

   s_compute = 0; s_create = 0; s_define = 0; s_write = 0; s_close = 0
   start = [i1, j1, face + 1, 1]
   count = [ni, nj, 1, 1]

   call MPI_Barrier(comm, ierr)
   t_start = MPI_Wtime()

   do ifile = 1, nfiles
      t0 = MPI_Wtime()
      call compute(compute_s)
      call MPI_Barrier(comm, ierr)
      t1 = MPI_Wtime()
      s_compute = s_compute + (t1 - t0)

      write(file_name, '(a,a,a,a,i4.4,a)') trim(dir), '/', trim(prefix), '_', ifile, '.nc4'

      ! ---- create -------------------------------------------------------
      t0 = MPI_Wtime()
      cmode = ior(NF90_CLOBBER, ior(NF90_NETCDF4, NF90_MPIIO))
      call check(nf90_create(trim(file_name), cmode, ncid, comm=comm, info=info), 'nf90_create')
      call check(nf90_set_fill(ncid, NF90_NOFILL, old_mode), 'nf90_set_fill')
      call MPI_Barrier(comm, ierr)
      tc = MPI_Wtime() - t0

      ! ---- define -------------------------------------------------------
      t0 = MPI_Wtime()
      call check(nf90_def_dim(ncid, 'Xdim', im, dim_x), 'def_dim Xdim')
      call check(nf90_def_dim(ncid, 'Ydim', im, dim_y), 'def_dim Ydim')
      call check(nf90_def_dim(ncid, 'nf',   NF, dim_f), 'def_dim nf')
      call check(nf90_def_dim(ncid, 'time', 1,  dim_t), 'def_dim time')
      call check(nf90_def_var(ncid, 'Xdim', NF90_DOUBLE, [dim_x], vid_x), 'def_var Xdim')
      call check(nf90_def_var(ncid, 'Ydim', NF90_DOUBLE, [dim_y], vid_y), 'def_var Ydim')
      call check(nf90_def_var(ncid, 'nf',   NF90_INT,    [dim_f], vid_f), 'def_var nf')
      call check(nf90_def_var(ncid, 'time', NF90_DOUBLE, [dim_t], vid_t), 'def_var time')
      call check(nf90_put_att(ncid, vid_t, 'units', 'minutes since 2000-01-01 00:00:00'), 'att time')
      call check(nf90_put_att(ncid, NF90_GLOBAL, 'grid_type', 'cubed-sphere'), 'att grid')
      call check(nf90_put_att(ncid, NF90_GLOBAL, 'cubed_sphere_resolution', im), 'att res')
      do n = 1, nvar
         write(vname, '(a,i4.4)') 'var', n
         if (trim(layout) == 'chunked') then
            call check(nf90_def_var(ncid, trim(vname), NF90_FLOAT, [dim_x, dim_y, dim_f, dim_t], &
                 varids(n), chunksizes=[im, im, 1, 1]), 'def_var '//trim(vname))
         else
            call check(nf90_def_var(ncid, trim(vname), NF90_FLOAT, [dim_x, dim_y, dim_f, dim_t], &
                 varids(n), contiguous=.true.), 'def_var '//trim(vname))
         end if
         call check(nf90_put_att(ncid, varids(n), 'units', '1'), 'att units')
      end do
      call check(nf90_enddef(ncid), 'nf90_enddef')
      call MPI_Barrier(comm, ierr)
      td = MPI_Wtime() - t0

      ! ---- write: coordinates (rank 0, independent) and 2-D data --------
      t0 = MPI_Wtime()
      call check(nf90_var_par_access(ncid, vid_x, NF90_INDEPENDENT), 'par_access Xdim')
      call check(nf90_var_par_access(ncid, vid_y, NF90_INDEPENDENT), 'par_access Ydim')
      call check(nf90_var_par_access(ncid, vid_f, NF90_INDEPENDENT), 'par_access nf')
      call check(nf90_var_par_access(ncid, vid_t, NF90_INDEPENDENT), 'par_access time')
      if (rank == 0) then
         call check(nf90_put_var(ncid, vid_x, coord), 'put Xdim')
         call check(nf90_put_var(ncid, vid_y, coord), 'put Ydim')
         call check(nf90_put_var(ncid, vid_f, [(n, n = 1, NF)]), 'put nf')
         call check(nf90_put_var(ncid, vid_t, [real((ifile - 1) * 60, REAL64)]), 'put time')
      end if
      do n = 1, nvar
         if (trim(access) == 'independent') then
            call check(nf90_var_par_access(ncid, varids(n), NF90_INDEPENDENT), 'par_access var')
         else
            call check(nf90_var_par_access(ncid, varids(n), NF90_COLLECTIVE), 'par_access var')
         end if
         buf = base + real(n * 0.001 + ifile, REAL32)
         call check(nf90_put_var(ncid, varids(n), buf, start=start, count=count), 'put var')
      end do
      call MPI_Barrier(comm, ierr)
      tw = MPI_Wtime() - t0

      ! ---- close --------------------------------------------------------
      t0 = MPI_Wtime()
      call check(nf90_close(ncid), 'nf90_close')
      call MPI_Barrier(comm, ierr)
      tx = MPI_Wtime() - t0

      s_create = s_create + tc
      s_define = s_define + td
      s_write  = s_write + tw
      s_close  = s_close + tx
      t_io(ifile) = tc + td + tw + tx
      if (verbose .and. rank == 0) then
         write(OUTPUT_UNIT, '(a,i4.4,a,f9.3,a,f9.3,a,f9.3,a,f9.3,a,f9.3,a,f9.3,a)') ' file ', ifile, &
              ': create ', tc, '  define ', td, '  write ', tw, '  close ', tx, '  io ', t_io(ifile), &
              ' s  ', real(bytes_per_file, REAL64) / 1.0d9 / max(t_io(ifile), 1.0d-12), ' GB/s'
         flush(OUTPUT_UNIT)
      end if
   end do

   t_total = MPI_Wtime() - t_start

   if (rank == 0) then
      call report_io(t_io)
      write(OUTPUT_UNIT, '(a,f12.3)')   ' compute     : ', s_compute
      write(OUTPUT_UNIT, '(a,f12.3)')   ' create      : ', s_create
      write(OUTPUT_UNIT, '(a,f12.3)')   ' define      : ', s_define
      write(OUTPUT_UNIT, '(a,f12.3)')   ' data write  : ', s_write
      write(OUTPUT_UNIT, '(a,f12.3)')   ' close       : ', s_close
      write(OUTPUT_UNIT, '(a,f12.3)')   ' io total    : ', sum(t_io)
      write(OUTPUT_UNIT, '(a,f12.3)')   ' total       : ', t_total
      call report_bandwidth(sum(t_io), t_total, s_compute)
   end if

   call MPI_Info_free(info, ierr)
   call MPI_Finalize(ierr)

contains

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

   subroutine report_bandwidth(io, total, comp)
      real(REAL64), intent(in) :: io, total, comp
      write(OUTPUT_UNIT, '(a,f12.3,a)') ' bandwidth (io total)           : ', &
           real(total_bytes, REAL64) / 1.0d9 / max(io, 1.0d-12), ' GB/s'
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
      character(len=512) :: arg, key, val
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
         case ('layout');    layout = val
         case ('access');    access = val
         case ('verbose');   verbose = (trim(val) == '1' .or. trim(val) == 'true')
         case default
            call fatal('unknown argument: '//trim(key))
         end select
      end do
      if (nfiles < 1) call fatal('nfiles must be positive')
      if (trim(layout) /= 'contiguous' .and. trim(layout) /= 'chunked') call fatal('layout must be contiguous|chunked')
      if (trim(access) /= 'collective' .and. trim(access) /= 'independent') call fatal('access must be collective|independent')
   end subroutine parse_args

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
      if (stat /= NF90_NOERR) then
         write(OUTPUT_UNIT, '(a,i0,a,a,a,a)') 'rank ', rank, ': ', where, ': ', trim(nf90_strerror(stat))
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

end program nc4_multifile_cs_writer
