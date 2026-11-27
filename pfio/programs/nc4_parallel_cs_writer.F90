! Parallel NetCDF-4 (HDF5/MPI-IO) write benchmark on a cubed-sphere grid.
!
! Layout follows GEOS cubed-sphere output: var(Xdim, Ydim, nf, lev, time).
! Each face is split into nx x (ny/6) blocks, so the total number of MPI
! ranks must equal nx*ny with ny a multiple of 6.
!
! Usage (all arguments optional, key=value):
!   mpirun -np N nc4_parallel_cs_writer.x file=out.nc4 im=1440 lm=181 \
!          target_gb=800 [nvar=K] [ntime=1] [nx=..] [ny=..] \
!          [layout=contiguous|chunked] [access=collective|independent] [verbose=1]
!
!   nvar      number of 3-D float32 variables (default: enough for target_gb)
!   layout    contiguous (default) or chunked, chunk = (im, im, 1, 1, 1)
program nc4_parallel_cs_writer

   use, intrinsic :: iso_fortran_env, only: REAL32, REAL64, INT64, OUTPUT_UNIT
   use mpi
   use netcdf

   implicit none

   character(len=512) :: file_name = 'cs_c1440_L181.nc4'
   character(len=32)  :: layout = 'contiguous'
   character(len=32)  :: access = 'collective'
   integer :: im = 1440, lm = 181, ntime = 1, nvar = -1
   integer :: nx = -1, ny = -1
   real(REAL64) :: target_gb = 800.0d0
   logical :: verbose = .false.

   integer, parameter :: NF = 6
   integer :: ierr, rank, npes, comm, info
   integer :: ncid, cmode
   integer :: dim_x, dim_y, dim_f, dim_l, dim_t
   integer :: vid_x, vid_y, vid_f, vid_l, vid_t
   integer, allocatable :: varids(:)
   integer :: face, ix, jy, nyf, r
   integer :: i1, i2, j1, j2, ni, nj
   integer :: n, k, t, old_mode
   integer :: start(5), count(5)
   integer(INT64) :: bytes_per_var, total_bytes
   real(REAL32), allocatable :: base(:,:,:,:), buf(:,:,:,:)
   real(REAL64), allocatable :: coord(:)
   real(REAL64) :: t0, t_start, t_create, t_define, t_coords, t_write, t_close, t_total
   real(REAL64) :: tw0, tw, tw_max, tw_min, tw_all
   character(len=64) :: vname

   call MPI_Init(ierr)
   comm = MPI_COMM_WORLD
   call MPI_Comm_rank(comm, rank, ierr)
   call MPI_Comm_size(comm, npes, ierr)

   call parse_args()

   if (nvar <= 0) then
      bytes_per_var = int(im, INT64) * im * NF * lm * ntime * 4
      nvar = max(1, ceiling(target_gb * 1.0d9 / real(bytes_per_var, REAL64)))
   end if
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

   bytes_per_var = int(im, INT64) * im * NF * lm * ntime * 4
   total_bytes   = bytes_per_var * nvar

   if (rank == 0) then
      write(OUTPUT_UNIT, '(a)')         '==== NetCDF-4 parallel cubed-sphere writer ===='
      write(OUTPUT_UNIT, '(a,a)')       ' file        : ', trim(file_name)
      write(OUTPUT_UNIT, '(a,i0,a,i0)') ' grid        : C', im, '  levels: ', lm
      write(OUTPUT_UNIT, '(a,i0)')      ' time steps  : ', ntime
      write(OUTPUT_UNIT, '(a,i0)')      ' 3-D vars    : ', nvar
      write(OUTPUT_UNIT, '(a,i0,a,i0,a,i0,a)') ' MPI ranks   : ', npes, '  (nx=', nx, ', ny=', ny, ')'
      write(OUTPUT_UNIT, '(a,i0,a,i0)') ' local block : ', ni, ' x ', nj
      write(OUTPUT_UNIT, '(a,a,a,a)')   ' layout      : ', trim(layout), '   access: ', trim(access)
      write(OUTPUT_UNIT, '(a,f12.3,a)') ' per var     : ', real(bytes_per_var, REAL64) / 1.0d9, ' GB'
      write(OUTPUT_UNIT, '(a,f12.3,a)') ' total data  : ', real(total_bytes, REAL64) / 1.0d9, ' GB'
      flush(OUTPUT_UNIT)
   end if

   ! Local data: a cheap deterministic pattern, offset per variable/time.
   allocate(base(ni, nj, 1, lm), buf(ni, nj, 1, lm))
   do k = 1, lm
      base(:, :, 1, k) = real(face * 1000 + k, REAL32)
   end do

   ! Collective-buffering hints help large GPFS/Lustre writes.
   call MPI_Info_create(info, ierr)
   call MPI_Info_set(info, 'romio_cb_write', 'enable', ierr)
   call MPI_Info_set(info, 'romio_ds_write', 'disable', ierr)

   call MPI_Barrier(comm, ierr)
   t_start = MPI_Wtime()

   ! ---- create ---------------------------------------------------------
   t0 = MPI_Wtime()
   cmode = ior(NF90_CLOBBER, ior(NF90_NETCDF4, NF90_MPIIO))
   call check(nf90_create(trim(file_name), cmode, ncid, comm=comm, info=info), 'nf90_create')
   call check(nf90_set_fill(ncid, NF90_NOFILL, old_mode), 'nf90_set_fill')
   call MPI_Barrier(comm, ierr)
   t_create = MPI_Wtime() - t0

   ! ---- define ---------------------------------------------------------
   t0 = MPI_Wtime()
   call check(nf90_def_dim(ncid, 'Xdim', im, dim_x), 'def_dim Xdim')
   call check(nf90_def_dim(ncid, 'Ydim', im, dim_y), 'def_dim Ydim')
   call check(nf90_def_dim(ncid, 'nf',   NF, dim_f), 'def_dim nf')
   call check(nf90_def_dim(ncid, 'lev',  lm, dim_l), 'def_dim lev')
   call check(nf90_def_dim(ncid, 'time', ntime, dim_t), 'def_dim time')

   call check(nf90_def_var(ncid, 'Xdim', NF90_DOUBLE, [dim_x], vid_x), 'def_var Xdim')
   call check(nf90_def_var(ncid, 'Ydim', NF90_DOUBLE, [dim_y], vid_y), 'def_var Ydim')
   call check(nf90_def_var(ncid, 'nf',   NF90_INT,    [dim_f], vid_f), 'def_var nf')
   call check(nf90_def_var(ncid, 'lev',  NF90_DOUBLE, [dim_l], vid_l), 'def_var lev')
   call check(nf90_def_var(ncid, 'time', NF90_DOUBLE, [dim_t], vid_t), 'def_var time')
   call check(nf90_put_att(ncid, vid_l, 'units', 'layer'), 'att lev')
   call check(nf90_put_att(ncid, vid_t, 'units', 'minutes since 2000-01-01 00:00:00'), 'att time')
   call check(nf90_put_att(ncid, NF90_GLOBAL, 'grid_type', 'cubed-sphere'), 'att grid')
   call check(nf90_put_att(ncid, NF90_GLOBAL, 'cubed_sphere_resolution', im), 'att res')

   allocate(varids(nvar))
   do n = 1, nvar
      write(vname, '(a,i4.4)') 'var', n
      if (trim(layout) == 'chunked') then
         call check(nf90_def_var(ncid, trim(vname), NF90_FLOAT, [dim_x, dim_y, dim_f, dim_l, dim_t], &
              varids(n), chunksizes=[im, im, 1, 1, 1]), 'def_var '//trim(vname))
      else
         call check(nf90_def_var(ncid, trim(vname), NF90_FLOAT, [dim_x, dim_y, dim_f, dim_l, dim_t], &
              varids(n), contiguous=.true.), 'def_var '//trim(vname))
      end if
      call check(nf90_put_att(ncid, varids(n), 'units', '1'), 'att units')
   end do
   call check(nf90_enddef(ncid), 'nf90_enddef')
   call MPI_Barrier(comm, ierr)
   t_define = MPI_Wtime() - t0

   ! ---- coordinates (rank 0 only, independent access) -----------------
   t0 = MPI_Wtime()
   call check(nf90_var_par_access(ncid, vid_x, NF90_INDEPENDENT), 'par_access Xdim')
   call check(nf90_var_par_access(ncid, vid_y, NF90_INDEPENDENT), 'par_access Ydim')
   call check(nf90_var_par_access(ncid, vid_f, NF90_INDEPENDENT), 'par_access nf')
   call check(nf90_var_par_access(ncid, vid_l, NF90_INDEPENDENT), 'par_access lev')
   call check(nf90_var_par_access(ncid, vid_t, NF90_INDEPENDENT), 'par_access time')
   if (rank == 0) then
      allocate(coord(im))
      coord = [(real(n, REAL64), n = 1, im)]
      call check(nf90_put_var(ncid, vid_x, coord), 'put Xdim')
      call check(nf90_put_var(ncid, vid_y, coord), 'put Ydim')
      call check(nf90_put_var(ncid, vid_f, [(n, n = 1, NF)]), 'put nf')
      call check(nf90_put_var(ncid, vid_l, [(real(n, REAL64), n = 1, lm)]), 'put lev')
      call check(nf90_put_var(ncid, vid_t, [(real((n - 1) * 60, REAL64), n = 1, ntime)]), 'put time')
      deallocate(coord)
   end if
   call MPI_Barrier(comm, ierr)
   t_coords = MPI_Wtime() - t0

   ! ---- 3-D data -------------------------------------------------------
   do n = 1, nvar
      if (trim(access) == 'independent') then
         call check(nf90_var_par_access(ncid, varids(n), NF90_INDEPENDENT), 'par_access var')
      else
         call check(nf90_var_par_access(ncid, varids(n), NF90_COLLECTIVE), 'par_access var')
      end if
   end do

   start = [i1, j1, face + 1, 1, 1]
   count = [ni, nj, 1, lm, 1]

   call MPI_Barrier(comm, ierr)
   t0 = MPI_Wtime()
   do t = 1, ntime
      start(5) = t
      do n = 1, nvar
         buf = base + real(n * 0.001 + t, REAL32)
         tw0 = MPI_Wtime()
         call check(nf90_put_var(ncid, varids(n), buf, start=start, count=count), 'put var')
         tw = MPI_Wtime() - tw0
         if (verbose) then
            call MPI_Reduce(tw, tw_max, 1, MPI_DOUBLE_PRECISION, MPI_MAX, 0, comm, ierr)
            call MPI_Reduce(tw, tw_min, 1, MPI_DOUBLE_PRECISION, MPI_MIN, 0, comm, ierr)
            if (rank == 0) then
               write(OUTPUT_UNIT, '(a,i4,a,i4,a,f10.3,a,f10.3,a,f10.3,a)') ' t=', t, ' var', n, &
                    ': max ', tw_max, ' s  min ', tw_min, ' s  ', &
                    real(bytes_per_var / ntime, REAL64) / 1.0d9 / max(tw_max, 1.0d-12), ' GB/s'
               flush(OUTPUT_UNIT)
            end if
         end if
      end do
   end do
   call MPI_Barrier(comm, ierr)
   t_write = MPI_Wtime() - t0

   ! ---- close (flushes HDF5 metadata and any cached data) -------------
   t0 = MPI_Wtime()
   call check(nf90_close(ncid), 'nf90_close')
   call MPI_Barrier(comm, ierr)
   t_close = MPI_Wtime() - t0

   t_total = MPI_Wtime() - t_start

   call MPI_Reduce(t_write, tw_all, 1, MPI_DOUBLE_PRECISION, MPI_MAX, 0, comm, ierr)
   if (rank == 0) then
      write(OUTPUT_UNIT, '(a)')         '---- timing (seconds, wall clock after barrier) ----'
      write(OUTPUT_UNIT, '(a,f12.3)')   ' create      : ', t_create
      write(OUTPUT_UNIT, '(a,f12.3)')   ' define      : ', t_define
      write(OUTPUT_UNIT, '(a,f12.3)')   ' coordinates : ', t_coords
      write(OUTPUT_UNIT, '(a,f12.3)')   ' data write  : ', tw_all
      write(OUTPUT_UNIT, '(a,f12.3)')   ' close       : ', t_close
      write(OUTPUT_UNIT, '(a,f12.3)')   ' total       : ', t_total
      write(OUTPUT_UNIT, '(a,f12.3,a)') ' bandwidth (data write)         : ', &
           real(total_bytes, REAL64) / 1.0d9 / max(tw_all, 1.0d-12), ' GB/s'
      write(OUTPUT_UNIT, '(a,f12.3,a)') ' bandwidth (write + close)      : ', &
           real(total_bytes, REAL64) / 1.0d9 / max(tw_all + t_close, 1.0d-12), ' GB/s'
      write(OUTPUT_UNIT, '(a,f12.3,a)') ' bandwidth (end-to-end)         : ', &
           real(total_bytes, REAL64) / 1.0d9 / max(t_total, 1.0d-12), ' GB/s'
      flush(OUTPUT_UNIT)
   end if

   deallocate(base, buf, varids)
   call MPI_Info_free(info, ierr)
   call MPI_Finalize(ierr)

contains

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
         case ('file');      file_name = val
         case ('im');        read(val, *) im
         case ('lm');        read(val, *) lm
         case ('ntime');     read(val, *) ntime
         case ('nvar');      read(val, *) nvar
         case ('nx');        read(val, *) nx
         case ('ny');        read(val, *) ny
         case ('target_gb'); read(val, *) target_gb
         case ('layout');    layout = val
         case ('access');    access = val
         case ('verbose');   verbose = (trim(val) == '1' .or. trim(val) == 'true')
         case default
            call fatal('unknown argument: '//trim(key))
         end select
      end do
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

end program nc4_parallel_cs_writer
