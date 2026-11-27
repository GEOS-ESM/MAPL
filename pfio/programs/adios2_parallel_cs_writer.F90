! Parallel ADIOS2 (BP5) write benchmark on a cubed-sphere grid.
!
! Writes the same content as nc4_parallel_cs_writer.x: var(Xdim, Ydim, nf, lev)
! float32 global arrays plus Xdim, Ydim, nf, lev, time coordinates. Each time
! record is one ADIOS2 step. Each face is split into nx x (ny/6) blocks, so the
! number of MPI ranks must equal nx*ny with ny a multiple of 6.
!
! Usage (all arguments optional, key=value):
!   mpirun -np N adios2_parallel_cs_writer.x file=out.bp im=1440 lm=181 \
!          target_gb=800 [nvar=K] [ntime=1] [nx=..] [ny=..] \
!          [put_mode=sync|deferred] [params=Key1=V1,Key2=V2] [verbose=1]
!
!   params    BP5 engine parameters, e.g. params=NumAggregators=64,AsyncWrite=1
!   put_mode  sync (default) copies each variable into the BP5 buffer at Put;
!             deferred keeps every variable's buffer alive until EndStep.
program adios2_parallel_cs_writer

   use, intrinsic :: iso_fortran_env, only: REAL32, REAL64, INT64, OUTPUT_UNIT
   use mpi
   use adios2

   implicit none

   character(len=512)  :: file_name = 'cs_c1440_L181.bp'
   character(len=32)   :: put_mode = 'sync'
   character(len=1024) :: params = ''
   integer :: im = 1440, lm = 181, ntime = 1, nvar = -1
   integer :: nx = -1, ny = -1
   real(REAL64) :: target_gb = 800.0d0
   logical :: verbose = .false.

   integer, parameter :: NF = 6
   integer :: ierr, rank, npes, comm
   type(adios2_adios)     :: adios
   type(adios2_io)        :: io
   type(adios2_engine)    :: engine
   type(adios2_variable)  :: v_x, v_y, v_f, v_l, v_t
   type(adios2_variable), allocatable :: vars(:)
   type(adios2_attribute) :: attr
   integer :: face, ix, jy, nyf, r
   integer :: i1, i2, j1, j2, ni, nj
   integer :: n, k, t, launch
   integer(INT64) :: shape4(4), start4(4), count4(4)
   integer(INT64) :: bytes_per_var, total_bytes
   real(REAL32), allocatable :: base(:,:,:,:), buf(:,:,:,:), bufs(:,:,:,:,:)
   real(REAL64), allocatable :: coord(:), levs(:)
   integer, allocatable :: faces(:)
   real(REAL64) :: time_val
   real(REAL64) :: t0, t_start, t_init, t_define, t_open, t_put, t_endstep, t_close, t_total
   real(REAL64) :: tw0, tw, tw_max, tw_min
   real(REAL64) :: m_put, m_endstep, s_put, s_endstep
   character(len=64) :: vname

   call MPI_Init(ierr)
   comm = MPI_COMM_WORLD
   call MPI_Comm_rank(comm, rank, ierr)
   call MPI_Comm_size(comm, npes, ierr)

   call parse_args()

   bytes_per_var = int(im, INT64) * im * NF * lm * ntime * 4
   if (nvar <= 0) nvar = max(1, ceiling(target_gb * 1.0d9 / real(bytes_per_var, REAL64)))
   total_bytes = bytes_per_var * nvar
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
      write(OUTPUT_UNIT, '(a)')         '==== ADIOS2 BP5 parallel cubed-sphere writer ===='
      write(OUTPUT_UNIT, '(a,a)')       ' file        : ', trim(file_name)
      write(OUTPUT_UNIT, '(a,i0,a,i0)') ' grid        : C', im, '  levels: ', lm
      write(OUTPUT_UNIT, '(a,i0)')      ' time steps  : ', ntime
      write(OUTPUT_UNIT, '(a,i0)')      ' 3-D vars    : ', nvar
      write(OUTPUT_UNIT, '(a,i0,a,i0,a,i0,a)') ' MPI ranks   : ', npes, '  (nx=', nx, ', ny=', ny, ')'
      write(OUTPUT_UNIT, '(a,i0,a,i0)') ' local block : ', ni, ' x ', nj
      write(OUTPUT_UNIT, '(a,a)')       ' put mode    : ', trim(put_mode)
      write(OUTPUT_UNIT, '(a,a)')       ' parameters  : ', trim(params)
      write(OUTPUT_UNIT, '(a,f12.3,a)') ' per var     : ', real(bytes_per_var, REAL64) / 1.0d9, ' GB'
      write(OUTPUT_UNIT, '(a,f12.3,a)') ' total data  : ', real(total_bytes, REAL64) / 1.0d9, ' GB'
      flush(OUTPUT_UNIT)
   end if

   ! Same deterministic pattern as the NetCDF-4 writer.
   allocate(base(ni, nj, 1, lm))
   do k = 1, lm
      base(:, :, 1, k) = real(face * 1000 + k, REAL32)
   end do
   if (trim(put_mode) == 'deferred') then
      allocate(bufs(ni, nj, 1, lm, nvar))
      launch = adios2_mode_deferred
   else
      allocate(buf(ni, nj, 1, lm))
      launch = adios2_mode_sync
   end if

   call MPI_Barrier(comm, ierr)
   t_start = MPI_Wtime()

   ! ---- init -----------------------------------------------------------
   t0 = MPI_Wtime()
   call adios2_init(adios, comm, ierr);                 call check(ierr, 'adios2_init')
   call adios2_declare_io(io, adios, 'CSWriter', ierr); call check(ierr, 'declare_io')
   call adios2_set_engine(io, 'BP5', ierr);             call check(ierr, 'set_engine')
   call set_params(params)
   call MPI_Barrier(comm, ierr)
   t_init = MPI_Wtime() - t0

   ! ---- define ---------------------------------------------------------
   t0 = MPI_Wtime()
   call adios2_define_variable(v_x, io, 'Xdim', adios2_type_dp, 1, [int(im, INT64)], &
        [0_INT64], [int(im, INT64)], adios2_constant_dims, ierr); call check(ierr, 'def Xdim')
   call adios2_define_variable(v_y, io, 'Ydim', adios2_type_dp, 1, [int(im, INT64)], &
        [0_INT64], [int(im, INT64)], adios2_constant_dims, ierr); call check(ierr, 'def Ydim')
   call adios2_define_variable(v_f, io, 'nf', adios2_type_integer4, 1, [int(NF, INT64)], &
        [0_INT64], [int(NF, INT64)], adios2_constant_dims, ierr); call check(ierr, 'def nf')
   call adios2_define_variable(v_l, io, 'lev', adios2_type_dp, 1, [int(lm, INT64)], &
        [0_INT64], [int(lm, INT64)], adios2_constant_dims, ierr); call check(ierr, 'def lev')
   call adios2_define_variable(v_t, io, 'time', adios2_type_dp, ierr); call check(ierr, 'def time')

   call adios2_define_attribute(attr, io, 'units', 'layer', 'lev', '/', ierr)
   call check(ierr, 'att lev')
   call adios2_define_attribute(attr, io, 'units', 'minutes since 2000-01-01 00:00:00', &
        'time', '/', ierr)
   call check(ierr, 'att time')
   call adios2_define_attribute(attr, io, 'grid_type', 'cubed-sphere', ierr)
   call check(ierr, 'att grid')
   call adios2_define_attribute(attr, io, 'cubed_sphere_resolution', im, ierr)
   call check(ierr, 'att res')

   shape4 = [int(im, INT64), int(im, INT64), int(NF, INT64), int(lm, INT64)]
   start4 = [int(i1 - 1, INT64), int(j1 - 1, INT64), int(face, INT64), 0_INT64]
   count4 = [int(ni, INT64), int(nj, INT64), 1_INT64, int(lm, INT64)]

   allocate(vars(nvar))
   do n = 1, nvar
      write(vname, '(a,i4.4)') 'var', n
      call adios2_define_variable(vars(n), io, trim(vname), adios2_type_real, 4, &
           shape4, start4, count4, adios2_constant_dims, ierr)
      call check(ierr, 'def '//trim(vname))
      call adios2_define_attribute(attr, io, 'units', '1', trim(vname), '/', ierr)
      call check(ierr, 'att units')
   end do
   call MPI_Barrier(comm, ierr)
   t_define = MPI_Wtime() - t0

   ! ---- open -----------------------------------------------------------
   t0 = MPI_Wtime()
   call adios2_open(engine, io, trim(file_name), adios2_mode_write, ierr)
   call check(ierr, 'adios2_open')
   call MPI_Barrier(comm, ierr)
   t_open = MPI_Wtime() - t0

   allocate(coord(im), levs(lm), faces(NF))
   coord = [(real(n, REAL64), n = 1, im)]
   levs  = [(real(n, REAL64), n = 1, lm)]
   faces = [(n, n = 1, NF)]

   ! ---- steps ----------------------------------------------------------
   t_put = 0
   t_endstep = 0
   do t = 1, ntime
      call MPI_Barrier(comm, ierr)
      t0 = MPI_Wtime()
      call adios2_begin_step(engine, adios2_step_mode_append, ierr)
      call check(ierr, 'begin_step')

      if (rank == 0) then
         if (t == 1) then
            call adios2_put(engine, v_x, coord, adios2_mode_sync, ierr); call check(ierr, 'put Xdim')
            call adios2_put(engine, v_y, coord, adios2_mode_sync, ierr); call check(ierr, 'put Ydim')
            call adios2_put(engine, v_f, faces, adios2_mode_sync, ierr); call check(ierr, 'put nf')
            call adios2_put(engine, v_l, levs,  adios2_mode_sync, ierr); call check(ierr, 'put lev')
         end if
         time_val = real((t - 1) * 60, REAL64)
         call adios2_put(engine, v_t, time_val, adios2_mode_sync, ierr); call check(ierr, 'put time')
      end if

      do n = 1, nvar
         tw0 = MPI_Wtime()
         if (launch == adios2_mode_deferred) then
            bufs(:, :, :, :, n) = base + real(n * 0.001 + t, REAL32)
            call adios2_put(engine, vars(n), bufs(:, :, :, :, n), launch, ierr)
         else
            buf = base + real(n * 0.001 + t, REAL32)
            call adios2_put(engine, vars(n), buf, launch, ierr)
         end if
         call check(ierr, 'put var')
         tw = MPI_Wtime() - tw0
         if (verbose) then
            call MPI_Reduce(tw, tw_max, 1, MPI_DOUBLE_PRECISION, MPI_MAX, 0, comm, ierr)
            call MPI_Reduce(tw, tw_min, 1, MPI_DOUBLE_PRECISION, MPI_MIN, 0, comm, ierr)
            if (rank == 0) then
               write(OUTPUT_UNIT, '(a,i4,a,i4,a,f10.3,a,f10.3,a)') ' t=', t, ' var', n, &
                    ': put max ', tw_max, ' s  min ', tw_min, ' s'
               flush(OUTPUT_UNIT)
            end if
         end if
      end do
      tw = MPI_Wtime() - t0
      t_put = t_put + tw

      ! BP5 writes the buffered step to disk in EndStep.
      t0 = MPI_Wtime()
      call adios2_end_step(engine, ierr)
      call check(ierr, 'end_step')
      call MPI_Barrier(comm, ierr)
      tw = MPI_Wtime() - t0
      t_endstep = t_endstep + tw
      if (verbose .and. rank == 0) then
         write(OUTPUT_UNIT, '(a,i4,a,f10.3,a,f10.3,a)') ' t=', t, ': end_step ', tw, ' s  ', &
              real(bytes_per_var / ntime * nvar, REAL64) / 1.0d9 / max(tw, 1.0d-12), ' GB/s'
         flush(OUTPUT_UNIT)
      end if
   end do

   ! ---- close ----------------------------------------------------------
   t0 = MPI_Wtime()
   call adios2_close(engine, ierr)
   call check(ierr, 'adios2_close')
   call MPI_Barrier(comm, ierr)
   t_close = MPI_Wtime() - t0

   t_total = MPI_Wtime() - t_start

   call MPI_Reduce(t_put, m_put, 1, MPI_DOUBLE_PRECISION, MPI_MAX, 0, comm, ierr)
   call MPI_Reduce(t_endstep, m_endstep, 1, MPI_DOUBLE_PRECISION, MPI_MAX, 0, comm, ierr)
   s_put = m_put
   s_endstep = m_endstep
   if (rank == 0) then
      write(OUTPUT_UNIT, '(a)')         '---- timing (seconds, wall clock after barrier) ----'
      write(OUTPUT_UNIT, '(a,f12.3)')   ' init        : ', t_init
      write(OUTPUT_UNIT, '(a,f12.3)')   ' define      : ', t_define
      write(OUTPUT_UNIT, '(a,f12.3)')   ' open        : ', t_open
      write(OUTPUT_UNIT, '(a,f12.3)')   ' put (all)   : ', s_put
      write(OUTPUT_UNIT, '(a,f12.3)')   ' end_step    : ', s_endstep
      write(OUTPUT_UNIT, '(a,f12.3)')   ' close       : ', t_close
      write(OUTPUT_UNIT, '(a,f12.3)')   ' total       : ', t_total
      write(OUTPUT_UNIT, '(a,f12.3,a)') ' bandwidth (put + end_step)     : ', &
           real(total_bytes, REAL64) / 1.0d9 / max(s_put + s_endstep, 1.0d-12), ' GB/s'
      write(OUTPUT_UNIT, '(a,f12.3,a)') ' bandwidth (put + end_step + close): ', &
           real(total_bytes, REAL64) / 1.0d9 / max(s_put + s_endstep + t_close, 1.0d-12), ' GB/s'
      write(OUTPUT_UNIT, '(a,f12.3,a)') ' bandwidth (end-to-end)         : ', &
           real(total_bytes, REAL64) / 1.0d9 / max(t_total, 1.0d-12), ' GB/s'
      flush(OUTPUT_UNIT)
   end if

   call adios2_finalize(adios, ierr)
   call MPI_Finalize(ierr)

contains

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
         case ('file');      file_name = val
         case ('im');        read(val, *) im
         case ('lm');        read(val, *) lm
         case ('ntime');     read(val, *) ntime
         case ('nvar');      read(val, *) nvar
         case ('nx');        read(val, *) nx
         case ('ny');        read(val, *) ny
         case ('target_gb'); read(val, *) target_gb
         case ('put_mode');  put_mode = val
         case ('params');    params = val
         case ('verbose');   verbose = (trim(val) == '1' .or. trim(val) == 'true')
         case default
            call fatal('unknown argument: '//trim(key))
         end select
      end do
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

end program adios2_parallel_cs_writer
