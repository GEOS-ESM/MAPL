#define I_AM_MAIN
#include "MAPL_Generic.h"

program  time_ave

   use ESMF
   use MAPL
   use MAPL_FileMetadataUtilsMod
   use gFTL_StringVector
   use MPI
   use, intrinsic :: iso_fortran_env, only: int32, int64, int16, real32, real64
   use ieee_arithmetic, only: isnan => ieee_is_nan

   implicit none

   integer  comm,myid,npes,ierror
   integer  imglobal
   integer  jmglobal
   logical  root

! **********************************************************************
! **********************************************************************
! ****                                                              ****
! ****        Program to create time-averaged HDF files             ****
! ****                                                              ****
! **********************************************************************
! **********************************************************************

   integer  im,jm,lm

   integer  nymd, nhms
   integer  nymd0,nhms0
   integer  nymdp,nhmsp
   integer  nymdm,nhmsm
   integer  ntod, ndt, ntods
   integer  month,  year
   integer  monthp, yearp
   integer  monthm, yearm
   integer  begdate, begtime
   integer  enddate, endtime

   integer id,rc,timeinc,timeid
   integer ntime,nvars,ncvid,nvars2

   character(len=ESMF_MAXSTR), allocatable :: fname(:)
   character(len=ESMF_MAXSTR)  template
   character(len=ESMF_MAXSTR)  name
   character(len=ESMF_MAXSTR)  ext
   character(len=ESMF_MAXSTR)  output, doutput, hdfile, rcfile
   character(len=8)    date0
   character(len=2)    time0
   character(len=1)    char
   data output  /'monthly_ave'/
   data rcfile  /'NULL'/
   data doutput /'NULL'/
   data template/'NULL'/

   integer n,m,nargs,L,nfiles,nv,km,mvars,mv,ndvars

   real     plev,qming,qmaxg
   real     previous_undef,undef
   real,    allocatable ::    lev(:)
   integer, allocatable ::  kmvar(:)  ,  kmvar2(:)
   integer, allocatable :: yymmdd(:)
   integer, allocatable :: hhmmss(:)
   integer, allocatable ::   nloc(:)
   integer, allocatable ::   iloc(:)

   character(len=ESMF_MAXSTR), allocatable ::  vname(:),  vname2(:)
   character(len=ESMF_MAXSTR), allocatable :: vtitle(:), vtitle2(:)
   character(len=ESMF_MAXSTR), allocatable :: vunits(:), vunits2(:)

   real,    allocatable ::   qmin(:)
   real,    allocatable ::   qmax(:)
   real,    allocatable ::  dumz1(:,:)
   real,    allocatable ::  dumz2(:,:)
   real,    allocatable ::    dum(:,:,:)
   real(REAL64),  allocatable ::  q(:,:,:,:)
   integer, allocatable :: ntimes(:,:,:,:)

   integer timinc,i,j,k,nmax,kbeg,kend,loc1,loc2
   integer nstar
   logical tend, first, strict, diurnal, mdiurnal, lquad, ldquad
   logical ignore_nan
   data first  /.true./
   data strict /.true./

   type(ESMF_Config)   :: config

   integer,       allocatable ::       qloc(:,:)
   character(len=ESMF_MAXSTR), allocatable :: quadratics(:,:)
   character(len=ESMF_MAXSTR), allocatable ::    quadtmp(:,:)
   character(len=ESMF_MAXSTR), allocatable ::    aliases(:,:)
   character(len=ESMF_MAXSTR), allocatable ::   aliastmp(:,:)
   character(len=ESMF_MAXSTR)  name1, name2, name3, dummy
   integer        nquad
   integer        nalias
   logical,       allocatable :: lzstar(:)

   integer ntmin, ntcrit, nc

   type(FileMetadata) :: basic_metadata
   type(FileMetadataUtils) :: file_metadata
   type(NetCDF4_FileFormatter) :: file_handle
   integer :: status
   class(AbstractGridfactory), allocatable :: factory
   type(ESMF_Grid) :: output_grid,input_grid
   character(len=:), allocatable :: output_grid_name
   integer :: global_dims(3), local_dims(3)
   type(ESMF_Time), allocatable :: time_series(:)
   type(ESMF_TIme) :: etime
   type(ESMF_Clock) :: clock
   type(ESMF_TimeInterval) :: time_interval
   type(ESMF_FieldBundle) :: primary_bundle,final_bundle,diurnal_bundle
   type(ESMF_Field) :: field
   type(ServerManager) :: io_server
   type(FieldBundleWriter) :: standard_writer, diurnal_writer
   real(ESMF_KIND_R4), pointer :: ptr2d(:,:),ptr3d(:,:,:)
   character(len=ESMF_MAXSTR) :: grid_type
   logical :: allow_zonal_means
   character(len=ESMF_MAXPATHLEN) :: arg_str
   character(len=:), allocatable :: lev_name
   character(len=ESMF_MAXSTR) :: lev_units
   integer :: n_times
   type(verticalData) :: vertical_data
   logical :: file_has_lev
   type(DistributedProfiler), target :: t_prof
   type(ProfileReporter) :: reporter

! **********************************************************************
! ****                       Initialization                         ****
! **********************************************************************

!call timebeg ('main')

   call mpi_init                ( ierror ) 
   _verify(ierror)
   comm = mpi_comm_world
   call mpi_comm_rank ( comm,myid,ierror )
   _verify(ierror)
   call mpi_comm_size ( comm,npes,ierror )
   _verify(ierror)
   call ESMF_Initialize(logKindFlag=ESMF_LOGKIND_NONE,mpiCommunicator=MPI_COMM_WORLD, _rc)
   call MAPL_Initialize(_rc)
   t_prof = DistributedProfiler('time_ave_util',MpiTImerGauge(),MPI_COMM_WORLD)
   call t_prof%start(_rc)
   call io_server%initialize(MPI_COMM_WORLD,_rc)
   root = myid.eq.0
   call ESMF_CalendarSetDefault(ESMF_CALKIND_GREGORIAN,_rc)

! Read Command Line Arguments
! ---------------------------
   begdate = -999
   begtime = -999
   enddate = -999
   endtime = -999
   ndt = -999
   ntod = -999
   ntmin = -999
   nargs = command_argument_count()
   if( nargs.eq.0 ) then
      call usage(root)
   else
      lquad     = .TRUE.
      ldquad    = .FALSE.
      diurnal   = .FALSE.
      mdiurnal   = .FALSE.
      ignore_nan = .FALSE.
      do n=1,nargs
         call get_command_argument(n,arg_str)
         select case(trim(arg_str))
         case('-template')
            call get_command_argument(n+1,template)
         case('-tag')
            call get_command_argument(n+1,output)
         case('-rc')
            call get_command_argument(n+1,rcfile)
         case('-begdate')
            call get_command_argument(n+1,arg_str)
            read(arg_str,*)begdate
         case('-begtime')
            call get_command_argument(n+1,arg_str)
            read(arg_str,*)begtime
         case('-enddate')
            call get_command_argument(n+1,arg_str)
            read(arg_str,*)enddate
         case('-endtime')
            call get_command_argument(n+1,arg_str)
            read(arg_str,*)endtime
         case('-ntmin')
            call get_command_argument(n+1,arg_str)
            read(arg_str,*)ntmin
         case('-ntod')
            call get_command_argument(n+1,arg_str)
            read(arg_str,*)ntod
         case('-ndt')
            call get_command_argument(n+1,arg_str)
            read(arg_str,*)ndt
         case('-strict')
            call get_command_argument(n+1,arg_str)
            read(arg_str,*)strict
         case('-ogrid')
            call get_command_argument(n+1,arg_str)
            output_grid_name = trim(arg_str)
         case('-noquad')
            lquad = .FALSE.
         case('-ignore_nan')
            ignore_nan = .TRUE.
         case('-d')
            diurnal = .true.
            if (n+1 .le. nargs) then
               call get_command_argument(n+1,arg_str)
               read(arg_str,fmt='(a1)') char
               if (char.ne.'-') doutput=arg_str
            end if
         case('-md')
            mdiurnal = .true.
            if (n+1 .le. nargs) then
               call get_command_argument(n+1,arg_str)
               read(arg_str,fmt='(a1)') char
               if (char.ne.'-') doutput=arg_str
            end if
         case('-dv')
            ldquad = .true.
            diurnal = .true.
            if (n+1 .le. nargs) then
               call get_command_argument(n+1,arg_str)
               read(arg_str,fmt='(a1)') char
               if (char.ne.'-') doutput=arg_str
            end if
         case('-mdv')
            ldquad = .true.
            mdiurnal = .true.
            if (n+1 .le. nargs) then
               call get_command_argument(n+1,arg_str)
               read(arg_str,fmt='(a1)') char
               if (char.ne.'-') doutput=arg_str
            end if
         case('-eta')
            nfiles = 1
            call get_command_argument(n+nfiles,arg_str)
            read(arg_str,fmt='(a1)') char
            do while (char .ne. '-' .and. n+nfiles.ne.nargs)
               nfiles = nfiles + 1
               call get_command_argument(n+nfiles,arg_str)
               read(arg_str,fmt='(a1)') char
            enddo
            if (char.eq.'-') nfiles = nfiles-1
            allocate(fname(nfiles))
            do m=1,nfiles
               call get_command_argument(n+m,fname(m))
            enddo
         case('-hdf')
            nfiles = 1
            call get_command_argument(n+nfiles,arg_str)
            read(arg_str,fmt='(a1)') char
            do while (char .ne. '-' .and. n+nfiles.ne.nargs)
               nfiles = nfiles + 1
               call get_command_argument(n+nfiles,arg_str)
               read(arg_str,fmt='(a1)') char
            enddo
            if (char.eq.'-') nfiles = nfiles-1
            allocate(fname(nfiles))
            do m=1,nfiles
               call get_command_argument(n+m,fname(m))
            enddo
         end select
      enddo
   end if

   if( (diurnal.or.mdiurnal) .and. trim(doutput).eq.'NULL' ) then
      doutput = trim(output) // "_diurnal"
      if( mdiurnal ) diurnal = .FALSE.
   endif

   if (root .and. ignore_nan) print *,' ignore nan is true'


! Read RC Quadratics
! ------------------
   if( trim(rcfile).eq.'NULL' ) then
      nquad  = 0
      nalias = 0
   else
      config = ESMF_ConfigCreate    ( rc=rc )
      call   ESMF_ConfigLoadFile  ( config, trim(rcfile),  rc=rc )
      call   ESMF_ConfigFindLabel ( config, 'QUADRATICS:', rc=rc )
      tend = .false.
      m = 0
      do while (.not.tend)
         m = m+1
         allocate( quadtmp(3,m) )
         call ESMF_ConfigGetAttribute ( config,value=name1,   rc=rc )
         call ESMF_ConfigGetAttribute ( config,value=dummy,   rc=rc )
         call ESMF_ConfigGetAttribute ( config,value=name2,   rc=rc )
         call ESMF_ConfigGetAttribute ( config,value=dummy,   rc=rc )
         call ESMF_ConfigGetAttribute ( config,value=name3,default='XXX',rc=rc )
         call ESMF_ConfigNextLine     ( config,tableEnd=tend, rc=rc )
         if( m==1 ) then
            quadtmp(1,m)     = name1
            quadtmp(2,m)     = name2
            quadtmp(3,m)     = name3
            allocate( quadratics(3,m) )
            quadratics = quadtmp
         else
            quadtmp(1,1:m-1) = quadratics(1,:)
            quadtmp(2,1:m-1) = quadratics(2,:)
            quadtmp(3,1:m-1) = quadratics(3,:)
            quadtmp(1,m)     = name1
            quadtmp(2,m)     = name2
            quadtmp(3,m)     = name3
            deallocate( quadratics      )
            allocate( quadratics(3,m) )
            quadratics = quadtmp
         endif
         deallocate (quadtmp)
      enddo
      nquad = m

! Read RC Aliases
! ---------------
      call   ESMF_ConfigFindLabel ( config, 'ALIASES:', rc=rc )
      tend = .false.
      m = 0
      do while (.not.tend)
         m = m+1
         allocate( aliastmp(2,m) )
         call ESMF_ConfigGetAttribute ( config,value=name1,   rc=rc )
         call ESMF_ConfigGetAttribute ( config,value=dummy,   rc=rc )
         call ESMF_ConfigGetAttribute ( config,value=name2,   rc=rc )
         call ESMF_ConfigNextLine     ( config,tableEnd=tend  ,rc=rc )
         if( m==1 ) then
            aliastmp(1,m)     = name1
            aliastmp(2,m)     = name2
            allocate( aliases(2,m) )
            aliases = aliastmp
         else
            aliastmp(1,1:m-1) = aliases(1,:)
            aliastmp(2,1:m-1) = aliases(2,:)
            aliastmp(1,m)     = name1
            aliastmp(2,m)     = name2
            deallocate( aliases      )
            allocate( aliases(2,m) )
            aliases = aliastmp
         endif
         deallocate (aliastmp)
      enddo
      nalias = m
   endif
   if (.not. allocated(aliases)) allocate(aliases(0,0))

! **********************************************************************
! ****                        Read HDF File                         ****
! **********************************************************************

   call t_prof%start('initialize')

   if( trim(template).ne.'NULL' ) then
      name = template
   else
      name = fname(1)
   endif

   n = index(trim(name),'.',back=.true.)
   ext = trim(name(n+1:))

   call file_handle%open(trim(name),PFIO_READ,_rc)
   basic_metadata = file_handle%read(_rc)
   call file_handle%close(_rc)

   allocate(factory, source=grid_manager%make_factory(trim(name)))
   input_grid = grid_manager%make_grid(factory)
   file_has_lev = has_level(input_grid,_rc)
   call MAPL_GridGet(input_grid,globalCellCountPerDim=global_dims,_rc)
   lm = global_dims(3)

   if (file_has_lev) then
      call get_file_levels(trim(name),vertical_data,_rc)
   end if

   if (allocated(output_grid_name)) then
      output_grid = create_output_grid(output_grid_name,lm,_rc)
   else
      output_grid = input_grid
   end if
   call ESMF_AttributeGet(output_grid,'GridType',grid_type,_rc)
   allow_zonal_means = trim(grid_type) == 'LatLon'
   if (trim(grid_type) == "Cubed-Sphere") then
      _assert(mod(npes,6)==0,"If input files are Cubed-Sphere, must be run on multiple of 6 proccessors")
   end if
   call MAPL_GridGet(output_grid,localCellCountPerDim=local_dims,globalCellCountPerDim=global_dims,_rc)
   im = local_dims(1)
   jm = local_dims(2)
   lm = local_dims(3)
   imglobal = global_dims(1)
   jmglobal = global_dims(2)

   call file_metadata%create(basic_metadata,trim(name))
   call get_file_times(file_metadata,ntime,time_series,timinc,yymmdd,hhmmss,_rc)
   primary_bundle = ESMF_FieldBundleCreate(name="first_file",_rc)
   call ESMF_FieldBundleSet(primary_bundle,grid=output_grid,_rc)
   call MAPL_Read_Bundle(primary_bundle,trim(name),time=time_series(1),_rc)
   call ESMF_FieldBundleGet(primary_bundle,fieldCount=nvars,_rc)
   allocate(vname(nvars))
   call ESMF_FieldBundleGet(primary_bundle,fieldNameList=vname,_rc)
   kmvar = get_level_info(primary_bundle,_rc)
   vtitle = get_long_names(primary_bundle,_rc)
   vunits = get_units(primary_bundle,_rc)

   final_bundle = ESMF_FieldBundleCreate(name="first_file",_rc)
   call ESMF_FieldBundleSet(final_bundle,grid=output_grid,_rc)
   diurnal_bundle = ESMF_FieldBundleCreate(name="first_file",_rc)
   call ESMF_FieldBundleSet(diurnal_bundle,grid=output_grid,_rc)
   call copy_bundle_to_bundle(primary_bundle,final_bundle,_rc)

   if (size(time_series)>1) then
      time_interval = time_series(2) - time_series(1)
   else if (size(time_series)==1) then
      call ESMF_TimeIntervalSet(time_interval,h=6,_rc)
   end if
   clock = ESMF_ClockCreate(startTime=time_series(1),timeStep=time_interval,_rc)

   nvars2 = nvars

   if (file_has_lev) then
      lev_name = file_metadata%get_level_name(_rc)
      call file_metadata%get_coordinate_info(lev_name,coords=lev,coordUnits=lev_units,_rc)
   end if

   previous_undef = file_metadata%var_get_missing_value(trim(vname(1)),_rc)
   do i=2,size(vname)
      undef = file_metadata%var_get_missing_value(trim(vname(i)),_rc)
      _assert(undef == previous_undef,"conflicting undefined values in your variables")
      previous_undef = undef
   enddo
   undef = previous_undef


! Set NDT for Strict Time Testing
! -------------------------------
   if( ntod.ne.-999 ) ndt = 86400
   if( ndt .eq.-999 ) ndt = compute_nsecf (timinc)
   if( timinc .eq. 0 ) then
      timeId = ncvid (id, 'time', rc)
      call ncagt     (id, timeId, 'time_increment', timinc, rc)
      if( timinc .eq. 0 ) then
         if( root ) then
            print *
            print *, 'Warning, GFIO Inquire states TIMINC = ',timinc
            print *, '         This will be reset to 060000 '
            print *, '         Use -ndt NNN (in seconds) to overide this'
         endif
         timinc = 060000
      endif
      ndt = compute_nsecf (timinc)
   endif

! Determine Number of Time Periods within 1-Day
! ---------------------------------------------
   ntods = 0
   if( diurnal .or. mdiurnal ) then
      if( ndt.lt.86400 ) ntods = 86400/ndt
   endif

! Set Minimum Required Times for Time Average (Default: 10 Days for Monthly Mean)
! -------------------------------------------------------------------------------
   if( ntmin.eq.-999 ) then
      if( ntod.eq.-999 ) then
         ntcrit = 10 * ( 86400.0/real(compute_nsecf(timinc)) )
      else
         ntcrit = 10
      endif
   else
      ntcrit = ntmin
   endif

! Determine Location Index for Each Variable in File
! --------------------------------------------------
   if( root ) print *
   allocate ( nloc(nvars) )
   nloc(1) = 1
   if( root ) write(6,7000) 1,trim(vname(1)),nloc(1),trim(vtitle(1)),max(1,kmvar(1))
   do n=2,nvars
      nloc(n) = nloc(n-1)+max(1,kmvar(n-1))
      if( root ) write(6,7000) n,trim(vname(n)),nloc(n),trim(vtitle(n)),max(1,kmvar(n))
7000  format(1x,'Primary Field:  ',i4,'  Name: ',a12,'  at location: ',i4,3x,a40,2x,i2,3x,i2,3x,i2)
   enddo

   nmax  = nloc(nvars)+max(1,kmvar(nvars))-1
   allocate( dum  (im,jm,nmax) )
   allocate( dumz1(im,jm) )
   allocate( dumz2(im,jm) )

! Append Default Quadratics to User-Supplied List
! -----------------------------------------------
   if( lquad ) then
      if( nquad.eq.0 ) then
         allocate( quadratics(3,nvars) )
         do n=1,nvars
            quadratics(1,n) = trim( vname(n) )
            quadratics(2,n) = trim( vname(n) )
            quadratics(3,n) = 'XXX'
         enddo
         nquad = nvars
      else
         allocate( quadtmp(3,nquad+nvars) )
         quadtmp(1,1:nquad) = quadratics(1,:)
         quadtmp(2,1:nquad) = quadratics(2,:)
         quadtmp(3,1:nquad) = quadratics(3,:)
         do n=1,nvars
            quadtmp(1,nquad+n) = trim( vname(n) )
            quadtmp(2,nquad+n) = trim( vname(n) )
            quadtmp(3,nquad+n) = 'XXX'
         enddo
         nquad = nquad + nvars
         deallocate( quadratics   )
         allocate( quadratics(3,nquad) )
         quadratics = quadtmp
         deallocate( quadtmp )
      endif
   endif

   allocate ( qloc(2,nquad) )
   allocate ( lzstar(nquad) ) ; lzstar = .FALSE.

! Determine Possible Quadratics
! -----------------------------
   km=kmvar(nvars)
   m=      nvars
   do n=1,nquad
      call check_quad ( quadratics(1,n),vname,nvars,aliases,nalias,qloc(1,n) )
      if( qloc(1,n)*qloc(2,n).ne.0 ) then
         m=m+1
         allocate ( iloc(m) )
         iloc(1:m-1) = nloc
         iloc(m) = iloc(m-1)+max(1,km)
         deallocate ( nloc )
         allocate ( nloc(m) )
         nloc = iloc
         deallocate ( iloc )
         km=kmvar( qloc(1,n) )
      endif
   enddo

   mvars = m
   nmax  = nloc(m)+max(1,km)-1

   allocate (  vname2(  mvars) )
   allocate ( vtitle2(  mvars) )
   allocate ( vunits2(  mvars) )
   allocate (  kmvar2(  mvars) )

   vname2(  1:nvars) = vname
   vtitle2(  1:nvars) = vtitle
   vunits2(  1:nvars) = vunits
   kmvar2(  1:nvars) = kmvar

   if( root .and. mvars.gt.nvars ) print *
   mv=  nvars
   do nv=1,nquad
      if( qloc(1,nv)*qloc(2,nv).ne.0 ) then
         mv  = mv+1

         if( trim(quadratics(1,nv)).eq.trim(quadratics(2,nv)) ) then
            vname2(mv) = "Var_"         //  trim(vname(qloc(1,nv)))
            vtitle2(mv) = "Variance_of_" //  trim(vname(qloc(1,nv)))
         else
            vname2(mv) = "Cov_"            // trim(vname(qloc(1,nv))) // "_"     // trim(vname(qloc(2,nv)))
            vtitle2(mv) = "Covariance_of_"  // trim(vname(qloc(1,nv))) // "_and_" // trim(vname(qloc(2,nv)))
         endif

         if( trim(quadratics(3,nv)).ne.'XXX' ) vname2(mv) = trim(quadratics(3,nv))

         nstar = index( trim(quadratics(1,nv)),'star',back=.true. )
         if( nstar.ne.0 ) then
            _assert(allow_zonal_means,"grid is not lat-lon so cannot compute zonal means")
            lzstar(nv) = .TRUE.
            vtitle2(mv) = "Product_of_Zonal_Mean_Deviations_of_" // trim(vname(qloc(1,nv))) // "_and_" // trim(vname(qloc(2,nv)))
         endif

         vunits2(mv) = trim(vunits(qloc(1,nv))) // " " // trim(vunits(qloc(2,nv)))
         kmvar2(mv) =  kmvar(qloc(1,nv))

         call add_new_field_to_bundle(final_bundle,output_grid,kmvar(qloc(1,nv)),vname2(mv),vtitle2(mv),vunits2(mv),_rc)

         if( root ) write(6,7001) mv,trim(vname2(mv)),nloc(mv),trim(vtitle2(mv)),max(1,kmvar(qloc(1,nv))),qloc(1,nv),qloc(2,nv)
7001     format(1x,'   Quad Field:  ',i4,'  Name: ',a12,'  at location: ',i4,3x,a50,2x,i2,3x,i3,3x,i3)
      endif
   enddo

!deallocate ( lev )
   deallocate ( yymmdd )
   deallocate ( hhmmss )
   deallocate (  vname )
   deallocate ( vtitle )
   deallocate ( vunits )
   deallocate (  kmvar )

   allocate(   qmin(nmax) )
   allocate(   qmax(nmax) )
   allocate(      q(im,jm,nmax,0:ntods) )
   allocate( ntimes(im,jm,nmax,0:ntods) )
   ntimes = 0
   q = 0
   qmin =  abs(undef)
   qmax = -abs(undef)

   if( root ) then
      print *
      write(6,7002) mvars,nmax,im,jm,nmax,ntods
7002  format(1x,'Total Number of Variables: ',i3,/ &
            1x,'Total Size: ',i5,/ &
            1x,'Allocating q(',i4,',',i3,',',i5,',0:',i2.2,')')
      print *
      print *, 'Files: '
      do n=1,nfiles
         print *, n,trim(fname(n))
      enddo
      print *
      if( ntod.eq.-999 ) then
         print *, 'Averging Time-Period NHMS: ',ntod,' (ALL Possible Time Periods Used)'
      else
         print *, 'Averging Time-Period NHMS: ',ntod
      endif
      if( begdate.ne.-999 .or. begtime.ne.-999 ) print *, 'Beginning Date for Averaging: ',begdate,begtime
      if( enddate.ne.-999 .or. endtime.ne.-999 ) print *, '   Ending Date for Averaging: ',enddate,endtime
      if( strict ) then
         print *, 'Every Time Period Required for Averaging, STRICT = ',strict
      else
         print *, 'Only Averaging Time Periods Supplied, STRICT = ',strict
      endif
      write(6,7003) ntcrit
7003  format(1x,'Required Minimum Number of Defined Time Periods: ',i3,' (Otherwise, UNDEF)')
      print *
   endif

  call t_prof%stop('initialize')

! **********************************************************************
! ****                      Read HDF Files                          ****
! **********************************************************************

   k = 0

   do n=1,nfiles

      if (allocated(time_series)) deallocate(time_series)
      if (allocated(yymmdd)) deallocate(yymmdd)
      if (allocated(hhmmss)) deallocate(hhmmss)
      call file_handle%open(trim(fname(n)),PFIO_READ,_rc)
      basic_metadata = file_handle%read(_rc)
      call file_handle%close(_rc)
      call file_metadata%create(basic_metadata,trim(fname(n)))
      call get_file_times(file_metadata,ntime,time_series,timinc,yymmdd,hhmmss,_rc)


      do m=1,ntime
         nymd = yymmdd(m)
         nhms = hhmmss(m)
         if( nhms<0 ) then
            nhms = compute_nhmsf( compute_nsecf(nhms) + 86400 )
            call tick (nymd,nhms,-86400)
         endif

         if( ( begdate.ne.-999 .and. begtime.ne.-999 ) .and. &
               ( begdate.gt.nymd .or. &
               ( begdate.eq.nymd.and.begtime.gt.nhms ) ) ) cycle

         if( ( enddate.ne.-999 .and. endtime.ne.-999 ) .and. &
               ( enddate.lt.nymd .or. &
               ( enddate.eq.nymd.and.endtime.lt.nhms ) ) ) cycle

         k = k+1
         if( k.gt.ntods ) k = 1
         if( ntod.eq.-999 .or. ntod.eq.nhms ) then
            if( root ) write(6,3000) nymd,nhms,timinc,trim(fname(n)),k
3000        format(1x,'Reading nymd: ',i8.8,' nhms: ',i6.6,' TimInc: ',i6.6,'  from File: ',a,'  tod = ',i2)
            year  =     nymd/10000
            month = mod(nymd,10000)/100

! Check for Correct First Dataset
! -------------------------------
            if( strict .and. first ) then
               nymdm = nymd
               nhmsm = nhms
               call tick (nymdm,nhmsm,-ndt)
               yearm  =     nymdm/10000
               monthm = mod(nymdm,10000)/100
               if( year.eq.yearm .and. month.eq.monthm ) then
                  if( root ) print *, 'Date: ',nymd,' Time: ',nhms,' is NOT correct First Time Period!'
                  _fail("error processing dataset")
               endif
            endif

! Check Date and Time for STRICT Time Testing
! -------------------------------------------
            if( strict .and. .not.first ) then
               if( nymd.ne.nymdp .or. nhms.ne.nhmsp ) then
                  if( root ) print *, 'Date: ',nymdp,' Time: ',nhmsp,' not found!'
                  _fail("error processing dataset")
               endif
            endif
            nymdp = nymd
            nhmsp = nhms

! Primary Fields
! --------------

            etime = local_esmf_timeset(nymd,nhms,_rc)
            call MAPL_Read_Bundle(primary_bundle,trim(fname(1)),time=etime,file_override=trim(fname(n)),_rc)
            do nv=1,nvars2
               call ESMF_FieldBundleGet(primary_bundle,trim(vname2(nv)),field=field,_rc)
               call t_prof%start('PRIME')
               if( kmvar2(nv).eq.0 ) then
                  kbeg = 0
                  kend = 1
                  call ESMF_FieldGet(field,0,farrayPtr=ptr2d,_rc)
                  dum(:,:,nloc(nv))=ptr2d
               else
                  kbeg = 1
                  kend = kmvar2(nv)

                  call ESMF_FieldGet(field,0,farrayPtr=ptr3d,_rc)
                  dum(:,:,nloc(nv):nloc(nv)+kmvar2(nv)-1) = ptr3d
               endif

               rc = 0
               do L=1,max(1,kmvar2(nv))
                  do j=1,jm
                     do i=1,im
                        if( isnan( dum(i,j,nloc(nv)+L-1) ) .or. ( dum(i,j,nloc(nv)+L-1).gt.HUGE(dum(i,j,nloc(nv)+L-1)) ) ) then
!print *, 'Warning!  Nan or Infinity detected for ',trim(vname2(nv)),' at lat: ',lattice%jglobal(j),' lon: ',lattice%iglobal(i)
                           if( root .and. ignore_nan ) then
                              print *, 'Setting Nan or Infinity to UNDEF'
                              print *
                           else
                              rc = 1
                           endif
                           dum(i,j,nloc(nv)+L-1) = undef
                        endif
                        if( defined(dum(i,j,nloc(nv)+L-1),undef) ) then
                           q(i,j,nloc(nv)+L-1,0) =      q(i,j,nloc(nv)+L-1,0) + dum(i,j,nloc(nv)+L-1)
                           ntimes(i,j,nloc(nv)+L-1,0) = ntimes(i,j,nloc(nv)+L-1,0) + 1
                           if(   qmin(nloc(nv)+L-1).gt.dum(i,j,nloc(nv)+L-1) ) qmin(nloc(nv)+L-1) = dum(i,j,nloc(nv)+L-1)
                           if(   qmax(nloc(nv)+L-1).lt.dum(i,j,nloc(nv)+L-1) ) qmax(nloc(nv)+L-1) = dum(i,j,nloc(nv)+L-1)
                           if( ntods.ne.0 ) then
                              q(i,j,nloc(nv)+L-1,k) =      q(i,j,nloc(nv)+L-1,k) + dum(i,j,nloc(nv)+L-1)
                              ntimes(i,j,nloc(nv)+L-1,k) = ntimes(i,j,nloc(nv)+L-1,k) + 1
                           endif
                        endif
                     enddo
                  enddo
               enddo
               call t_prof%stop('PRIME')

            enddo

! Quadratics
! ----------
            call t_prof%start('QUAD')
            mv=  nvars2
            do nv=1,nquad
               if( qloc(1,nv)*qloc(2,nv).ne.0 ) then
                  mv=mv+1
                  do L=1,max(1,kmvar2(qloc(1,nv)))
                     if( lzstar(nv) ) then
                        call latlon_zstar (dum(:,:,nloc(qloc(1,nv))+L-1),dumz1,undef,output_grid,_rc)
                        call latlon_zstar (dum(:,:,nloc(qloc(2,nv))+L-1),dumz2,undef,output_grid,_rc)
                        do j=1,jm
                           do i=1,im
                              if( defined(dumz1(i,j),undef)  .and. &
                                    defined(dumz2(i,j),undef) ) then
                                 q(i,j,nloc(mv)+L-1,0) =      q(i,j,nloc(mv)+L-1,0) + dumz1(i,j)*dumz2(i,j)
                                 ntimes(i,j,nloc(mv)+L-1,0) = ntimes(i,j,nloc(mv)+L-1,0) + 1
                                 if( ntods.ne.0 ) then
                                    q(i,j,nloc(mv)+L-1,k) =      q(i,j,nloc(mv)+L-1,k) + dumz1(i,j)*dumz2(i,j)
                                    ntimes(i,j,nloc(mv)+L-1,k) = ntimes(i,j,nloc(mv)+L-1,k) + 1
                                 endif
                              endif
                           enddo
                        enddo
                     else
                        do j=1,jm
                           do i=1,im
                              if( defined(dum(i,j,nloc(qloc(1,nv))+L-1),undef)  .and. &
                                    defined(dum(i,j,nloc(qloc(2,nv))+L-1),undef) ) then
                                 q(i,j,nloc(mv)+L-1,0) = q(i,j,nloc(mv)+L-1,0) + dum(i,j,nloc(qloc(1,nv))+L-1) &
                                       * dum(i,j,nloc(qloc(2,nv))+L-1)
                                 ntimes(i,j,nloc(mv)+L-1,0) = ntimes(i,j,nloc(mv)+L-1,0) + 1
                                 if( ntods.ne.0 ) then
                                    q(i,j,nloc(mv)+L-1,k) = q(i,j,nloc(mv)+L-1,k) + dum(i,j,nloc(qloc(1,nv))+L-1) &
                                          * dum(i,j,nloc(qloc(2,nv))+L-1)
                                    ntimes(i,j,nloc(mv)+L-1,k) = ntimes(i,j,nloc(mv)+L-1,k) + 1
                                 endif
                              endif
                           enddo
                        enddo
                     endif
                  enddo
               endif
            enddo
            call t_prof%stop('QUAD')

            if( first ) then
               nymd0 = nymd
               nhms0 = nhms
               first = .false.
            endif

! Update Date and Time for Strict Test
! ------------------------------------
            call tick (nymdp,nhmsp,ndt)
            yearp  =     nymdp/10000
            monthp = mod(nymdp,10000)/100

         endif ! End ntod  Test
      enddo    ! End ntime Loop within file

      call MPI_BARRIER(comm,status)
      _verify(status)
   enddo

   do k=0,ntods
      if( k.eq.0 ) then
         nc = ntcrit
      else
         nc = max( 1,ntcrit/ntods )
      endif
      do n=1,nmax
         do j=1,jm
            do i=1,im
               if( ntimes(i,j,n,k).lt.nc ) then
                  q(i,j,n,k) = undef
               else
                  q(i,j,n,k) = q(i,j,n,k)/ntimes(i,j,n,k)
               endif
            enddo
         enddo
      enddo
   enddo

! **********************************************************************
! ****               Write HDF Monthly Output File                  ****
! **********************************************************************

call t_prof%start('Write_AVE')

! Check for Correct Last Dataset
! ------------------------------
   if( strict .and. ( year.eq.yearp .and. month.eq.monthp ) ) then
      if( root ) print *, 'Date: ',nymd,' Time: ',nhms,' is NOT correct Last Time Period!'
      _fail("Error processing dataset")
   endif

   write(date0,4000) nymd0/100
   write(time0,2000) nhms0/10000

   hdfile = trim(output) // "." // trim(date0) // "." // trim(ext)

1000 format(i8.8)
2000 format(i2.2)
4000 format(i6.6)

   timeinc   = 060000

! Primary Fields
! --------------
   if( root ) print *
   do n=1,nvars2
      call ESMF_FieldBundleGet(final_bundle,trim(vname2(n)),field=field,_rc)
      if( kmvar2(n).eq.0 ) then
         kbeg = 0
         kend = 1
         call ESMF_FieldGet(field,0,farrayPtr=ptr2d,_rc)
         ptr2d = q(:,:,nloc(n),0)
      else
         kbeg = 1
         kend = kmvar2(n)
         call ESMF_FieldGet(field,0,farrayPtr=ptr3d,_rc)
         ptr3d = q(:,:,nloc(n):nloc(n)+kend-1,0)
      endif
      if( root ) write(6,3001) trim(vname2(n)),nloc(n),trim(hdfile)
3001  format(1x,'Writing ',a,' at location ',i6,' into File: ',a)
      dum(:,:,1:kend) = q(:,:,nloc(n):nloc(n)+kend-1,0)
   enddo

! Quadratics
! ----------
   mv=  nvars2
   do nv=1,nquad
      if( qloc(1,nv)*qloc(2,nv).ne.0 ) then
         mv=mv+1
         if( root ) write(6,3001) trim(vname2(mv)),nloc(mv),trim(hdfile)
         call ESMF_FieldBundleGet(final_bundle,trim(vname2(mv)),field=field,_rc)

         if( kmvar2(qloc(1,nv)).eq.0 ) then
            kbeg = 0
            kend = 1
         else
            kbeg = 1
            kend = kmvar2(qloc(1,nv))
         endif
         loc1 = nloc( qloc(1,nv) )
         loc2 = nloc( qloc(2,nv) )
         if( .not.lzstar(nv) ) then
            where( q(:,:,nloc(mv):nloc(mv)+kend-1,0).ne.undef )
               dum(:,:,1:kend) = q(:,:,nloc(mv):nloc(mv)+kend-1,0) - q(:,:,loc1:loc1+kend-1,0) &
                     * q(:,:,loc2:loc2+kend-1,0)
            elsewhere
               dum(:,:,1:kend) = undef
            endwhere
         else
            dum(:,:,1:kend) = q(:,:,nloc(mv):nloc(mv)+kend-1,0)
         endif
         if( kmvar2(qloc(1,nv)).eq.0 ) then
            call ESMF_FieldGet(field,0,farrayPtr=ptr2d,_rc)
            ptr2d = dum(:,:,1)
         else
            kend = kmvar2(qloc(1,nv))
            call ESMF_FieldGet(field,0,farrayPtr=ptr3d,_rc)
            ptr3d = dum(:,:,1:kend)
         endif
      endif
   enddo

   if( root ) then
      print *
      print *, 'Created: ',trim(hdfile)
      print *
   endif
   call t_prof%stop('Write_AVE')
   etime = local_esmf_timeset(nymd0,nhms0,_rc)
   call ESMF_ClockSet(clock,currTime=etime, _rc)
   call standard_writer%create_from_bundle(final_bundle,clock,n_steps=1,time_interval=timeinc,vertical_data=vertical_data,_rc)
   call standard_writer%start_new_file(trim(hdfile),_rc)
   call standard_writer%write_to_file(_rc)

! **********************************************************************
! ****               Write HDF Monthly Diurnal Output File          ****
! **********************************************************************

   if( ntods.ne.0 ) then
      call t_prof%start('Write_Diurnal')
      timeinc   = compute_nhmsf( 86400/ntods )

      do k=1,ntods

         if( k.eq.1 .or. mdiurnal ) then

            write(date0,4000) nymd0/100
            write(time0,2000) nhms0/10000

            if(  diurnal ) hdfile = trim(doutput) // "." // trim(date0) // "." // trim(ext)
            if( mdiurnal ) hdfile = trim(doutput) // "." // trim(date0) // "_" // trim(time0) // "z." // trim(ext)

            if( ldquad ) then
               ndvars = mvars  ! Include Quadratics in Diurnal Files
               if (k==1) then
                  call copy_bundle_to_bundle(final_bundle,diurnal_bundle,_rc)
               end if
            else
               ndvars = nvars2 ! Only Include Primary Fields in Diurnal Files (Default)
               if (k==1) then
                  do n=1,nvars
                     call ESMF_FieldBundleGet(final_bundle,trim(vname2(n)),field=field,_rc)
                     call MAPL_FieldBundleAdd(diurnal_bundle,field,_rc)
                  enddo
               endif
            endif
         endif

! Primary Fields
! --------------
         do n=1,nvars2
            call ESMF_FieldBundleGet(diurnal_bundle,trim(vname2(n)),field=field,_rc)
            if( kmvar2(n).eq.0 ) then
               kbeg = 0
               kend = 1
               call ESMF_FieldGet(field,0,farrayPtr=ptr2d,_rc)
               ptr2d = q(:,:,nloc(n),k)
            else
               kbeg = 1
               kend = kmvar2(n)
               call ESMF_FieldGet(field,0,farrayPtr=ptr3d,_rc)
               ptr3d = q(:,:,nloc(n):nloc(n)+kend-1,k)
            endif
            dum(:,:,1:kend) = q(:,:,nloc(n):nloc(n)+kend-1,k)
         enddo

! Quadratics
! ----------
         if( ndvars.eq.mvars ) then
            mv=  nvars2
            do nv=1,nquad
               if( qloc(1,nv)*qloc(2,nv).ne.0 ) then
                  mv=mv+1
                  call ESMF_FieldBundleGet(diurnal_bundle,trim(vname2(mv)),field=field,_rc)
                  if( kmvar2(qloc(1,nv)).eq.0 ) then
                     kbeg = 0
                     kend = 1
                  else
                     kbeg = 1
                     kend = kmvar2(qloc(1,nv))
                  endif
                  loc1 = nloc( qloc(1,nv) )
                  loc2 = nloc( qloc(2,nv) )
                  if( .not.lzstar(nv) ) then
                     where( q(:,:,nloc(mv):nloc(mv)+kend-1,0).ne.undef )
                        dum(:,:,1:kend) = q(:,:,nloc(mv):nloc(mv)+kend-1,k) - q(:,:,loc1:loc1+kend-1,k) &
                              * q(:,:,loc2:loc2+kend-1,k)
                     elsewhere
                        dum(:,:,1:kend) = undef
                     endwhere
                  else
                     dum(:,:,1:kend) = q(:,:,nloc(mv):nloc(mv)+kend-1,k)
                  endif
                  if( kmvar2(qloc(1,nv)).eq.0 ) then
                     call ESMF_FieldGet(field,0,farrayPtr=ptr2d,_rc)
                     ptr2d = dum(:,:,1)
                  else
                     kend = kmvar2(qloc(1,nv))
                     call ESMF_FieldGet(field,0,farrayPtr=ptr3d,_rc)
                     ptr3d = dum(:,:,1:kend)
                  endif
               endif
            enddo
         endif


         etime = local_esmf_timeset(nymd0,nhms0,_rc)
         call ESMF_ClockSet(clock,currTime=etime, _rc)
         if (k==1 .or. mdiurnal) then
            if (mdiurnal) then
               n_times = 1
            else
               n_times = ntods
            end if
            if (k==1) then
               call diurnal_writer%create_from_bundle(diurnal_bundle,clock,n_steps=n_times,time_interval=timeinc,vertical_data=vertical_data)
            end if
            call diurnal_writer%start_new_file(trim(hdfile),_rc)
         end if
         call diurnal_writer%write_to_file(_rc)
         if( root .and. mdiurnal ) then
            print *, 'Created: ',trim(hdfile)
         endif
         call tick (nymd0,nhms0,ndt)
      enddo

      if( root .and. diurnal ) then
         print *, 'Created: ',trim(hdfile)
      endif
      if( root ) print *

      call t_prof%stop('Write_Diurnal')
   endif

! **********************************************************************
! ****                 Write Min/Max Information                    ****
! **********************************************************************

   if( root ) print *
   do n=1,nvars2
      do L=1,max(1,kmvar2(n))
         if( kmvar2(n).eq.0 ) then
            plev = 0
         else
            plev = lev(L)
         endif

         call mpi_reduce( qmin(nloc(n)+L-1),qming,1,mpi_real,mpi_min,0,comm,ierror )
         _verify(ierror)
         call mpi_reduce( qmax(nloc(n)+L-1),qmaxg,1,mpi_real,mpi_max,0,comm,ierror )
         _verify(ierror)
         if( root ) then
            if(L.eq.1) then
               write(6,3101) trim(vname2(n)),plev,qming,qmaxg
            else
               write(6,3102) trim(vname2(n)),plev,qming,qmaxg
            endif
         endif
3101     format(1x,'Primary Field: ',a20,' Level: ',f9.3,'  Min: ',g15.8,'  Max: ',g15.8)
3102     format(1x,'               ',a20,' Level: ',f9.3,'  Min: ',g15.8,'  Max: ',g15.8)
      enddo
      call MPI_BARRIER(comm,status)
      _verify(status)
      if( root ) print *
   enddo
   if( root ) print *

! **********************************************************************
! ****                     Timing Information                       ****
! **********************************************************************

   call io_server%finalize()
   call t_prof%stop()
   call t_prof%reduce()
   call t_prof%finalize()
   call generate_report()
   call MAPL_Finalize()
   call MPI_Finalize(status)
   stop

contains

   function create_output_grid(grid_name,lm,rc) result(new_grid)
      type(ESMF_Grid) :: new_grid
      character(len=*), intent(inout) :: grid_name
      integer, intent(in) :: lm
      integer, optional, intent(out) :: rc

      type(ESMF_Config) :: cf
      integer :: nn,im_world,jm_world,nx, ny
      character(len=5) :: imsz,jmsz
      character(len=2) :: pole,dateline

      nn   = len_trim(grid_name)
      imsz = grid_name(3:index(grid_name,'x')-1)
      jmsz = grid_name(index(grid_name,'x')+1:nn-3)
      pole = grid_name(1:2)
      dateline = grid_name(nn-1:nn)
      read(IMSZ,*) im_world
      read(JMSZ,*) jm_world

      cf = MAPL_ConfigCreate(_rc)
      call MAPL_ConfigSetAttribute(cf,value=lm, label=trim(grid_name)//".LM:",_rc)
      if (dateline=='CF') then
         call MAPL_MakeDecomposition(nx,ny,reduceFactor=6,_rc)
         call MAPL_ConfigSetAttribute(cf,value=NX, label=trim(grid_name)//".NX:",_rc)
         call MAPL_ConfigSetAttribute(cf,value="Cubed-Sphere", label=trim(grid_name)//".GRID_TYPE:",_rc)
         call MAPL_ConfigSetAttribute(cf,value=6, label=trim(grid_name)//".NF:",_rc)
         call MAPL_ConfigSetAttribute(cf,value=im_world,label=trim(grid_name)//".IM_WORLD:",_rc)
         call MAPL_ConfigSetAttribute(cf,value=ny, label=trim(grid_name)//".NY:",_rc)
      else if (dateline=='TM') then
         _fail("Tripolar not yet implemented for outpout")
      else
         call MAPL_MakeDecomposition(nx,ny,_rc)
         call MAPL_ConfigSetAttribute(cf,value=NX, label=trim(grid_name)//".NX:",_rc)
         call MAPL_ConfigSetAttribute(cf,value="LatLon", label=trim(grid_name)//".GRID_TYPE:",_rc)
         call MAPL_ConfigSetAttribute(cf,value=im_world,label=trim(grid_name)//".IM_WORLD:",_rc)
         call MAPL_ConfigSetAttribute(cf,value=jm_world,label=trim(grid_name)//".JM_WORLD:",_rc)
         call MAPL_ConfigSetAttribute(cf,value=ny, label=trim(grid_name)//".NY:",_rc)
         call MAPL_ConfigSetAttribute(cf,value=pole, label=trim(grid_name)//".POLE:",_rc)
         call MAPL_ConfigSetAttribute(cf,value=dateline, label=trim(grid_name)//".DATELINE:",_rc)
         if (pole=='XY' .and. dateline=='XY') then
            _fail("regional lat-lon output not supported")
         end if
      end if

      new_grid = grid_manager%make_grid(cf,prefix=trim(grid_name)//".",_rc)
      if (present(rc)) then
         rc=_success
      end if
   end function create_output_grid

   subroutine get_file_levels(filename,vertical_data,rc)
      character(len=*), intent(in) :: filename
      type(VerticalData), intent(inout) :: vertical_data
      integer, intent(out), optional :: rc

      integer :: status
      type(NetCDF4_fileFormatter) :: formatter
      type(FileMetadata) :: basic_metadata
      type(FileMetadataUtils) :: metadata
      character(len=:), allocatable :: lev_name
      character(len=ESMF_MAXSTR) :: long_name
      character(len=ESMF_MAXSTR) :: standard_name
      character(len=ESMF_MAXSTR) :: vcoord
      character(len=ESMF_MAXSTR) :: lev_units
      real, allocatable, target :: levs(:)
      real, pointer :: plevs(:)

      call formatter%open(trim(filename),pFIO_Read,_rc)
      basic_metadata=formatter%read(_rc)
      call metadata%create(basic_metadata,trim(filename))
      lev_name = metadata%get_level_name(_rc)
      if (lev_name /= '') then
         call metadata%get_coordinate_info(lev_name,coords=levs,coordUnits=lev_units,long_name=long_name,&
               standard_name=standard_name,coordinate_attr=vcoord,_rc)
         plevs => levs
         vertical_data = VerticalData(levels=plevs,vunit=lev_units,vcoord=vcoord,standard_name=standard_name,long_name=long_name, &
               force_no_regrid=.true.,_rc)
         nullify(plevs)
      end if

      if (present(rc)) then
         rc=_success
      end if

   end subroutine get_file_levels

   function has_level(grid,rc) result(grid_has_level)
      logical :: grid_has_level
      type(ESMF_Grid), intent(in) :: grid
      integer, intent(out), optional :: rc
      integer :: status, global_dims(3)
      call MAPL_GridGet(grid,globalCellCountPerDim=global_dims,_rc)
      grid_has_level = (global_dims(3)>0)
      if (present(rc)) then
         RC=_success
      end if
   end function has_level

   subroutine copy_bundle_to_bundle(input_bundle,output_bundle,rc)
      type(ESMF_FieldBundle), intent(inout) :: input_bundle
      type(ESMF_FieldBundle), intent(inout) :: output_bundle
      integer, intent(out), optional :: rc
      integer :: status
      character(len=ESMF_MAXSTR), allocatable :: field_list(:)
      type(ESMF_Field) :: field
      integer :: i,num_fields
      call ESMF_FieldBundleGet(input_bundle,fieldCount=num_fields,_rc)
      allocate(field_list(num_fields))
      call ESMF_FieldBundleGet(input_bundle,fieldNameList=field_list,_rc)
      do i=1,num_fields
         call ESMF_FieldBundleGet(input_bundle,field_list(i),field=field,_rc)
         call MAPL_FieldBundleAdd(output_bundle,field,_rc)
      enddo
      if (present(rc)) then
         RC=_success
      end if
   end subroutine copy_bundle_to_bundle

   subroutine add_new_field_to_bundle(bundle,grid,lm,field_name,long_name,units,rc)
      type(ESMF_FieldBundle), intent(inout) :: bundle
      type(ESMF_Grid), intent(in) :: grid
      integer, intent(in) :: lm
      character(len=*), intent(in) :: field_name
      character(len=*), intent(in) :: long_name
      character(len=*), intent(in) :: units
      integer, intent(out), optional :: rc

      integer :: status
      type(ESMF_Field) :: field

      if (lm == 0) then
         field = ESMF_FieldCreate(grid,name=trim(field_name),typekind=ESMF_TYPEKIND_R4,_rc)
      else if (lm > 0) then
         field = ESMF_FieldCreate(grid,name=trim(field_name),typekind=ESMF_TYPEKIND_R4, &
               ungriddedLBound=[1],ungriddedUBound=[lm],_rc)
      end if
      call ESMF_AttributeSet(field,name='LONG_NAME',value=trim(long_name),_rc)
      call ESMF_AttributeSet(field,name='UNITS',value=trim(units),_rc)
      if (lm == 0) then
         call ESMF_AttributeSet(field,name='DIMS',value=MAPL_DimsHorzOnly,_rc)
         call ESMF_AttributeSet(field,name='VLOCATION',value=MAPL_VLocationNone,_rc)
      else if (lm > 0) then
         call ESMF_AttributeSet(field,name='DIMS',value=MAPL_DimsHorzVert,_rc)
         call ESMF_AttributeSet(field,name='VLOCATION',value=MAPL_VLocationCenter,_rc)
      end if
      call MAPL_FieldBundleAdd(bundle,field,_rc)
      if (present(rc)) then
         RC=_success
      end if
   end subroutine add_new_field_to_bundle

   subroutine get_file_times(file_metadata,num_times,time_series,time_interval,yymmdd,hhmmss,rc)
      type(FileMetadataUtils), intent(inout) :: file_metadata
      integer, intent(out) :: num_times
      type(ESMF_Time), allocatable, intent(inout) :: time_series(:)
      integer, intent(inout), allocatable :: yymmdd(:)
      integer, intent(inout), allocatable :: hhmmss(:)
      integer, intent(out) :: time_interval
      integer, intent(out), optional :: rc

      integer :: status
      type(ESMF_TimeInterval) :: esmf_time_interval
      integer :: hour, minute, second, year, month, day, i

      num_times = file_metadata%get_dimension('time',_rc)
      call file_metadata%get_time_info(timeVector=time_series,_rc)
      if (num_times == 1) then
         time_interval = file_metadata%get_var_attr_int32('time','time_increment',_rc)
      else if (num_times > 1) then
         esmf_time_interval = time_series(2)-time_series(1)
         call ESMF_TimeIntervalGet(esmf_time_interval,h=hour,m=minute,s=second,_rc)
         time_interval = hour*10000+minute*100+second
      end if

      allocate(yymmdd(num_times),hhmmss(num_times))
      do i = 1,num_times
         call ESMF_TimeGet(time_series(i),yy=year,mm=month,dd=day,h=hour,m=minute,s=second,_rc)
         yymmdd(i)=year*10000+month*100+day
         hhmmss(i)=hour*10000+minute*100+second
      enddo
      if (present(rc)) then
         rc=_success
      end if
   end subroutine get_file_times

   function get_level_info(bundle,rc) result(kmvar)
      integer, allocatable :: kmvar(:)
      type(ESMF_FieldBundle), intent(in) :: bundle
      integer, optional, intent(out) :: rc

      integer :: status
      character(len=ESMF_MAXSTR), allocatable :: field_list(:)
      type(ESMF_Field) :: field
      integer :: rank,i,num_fields,lb(1),ub(1)
      call ESMF_FieldBundleGet(bundle,fieldCount=num_fields,_rc)
      allocate(field_list(num_fields))
      allocate(kmvar(num_fields))
      call ESMF_FieldBundleGet(bundle,fieldNameList=field_list,_rc)
      do i=1,num_fields
         call ESMF_FieldBundleGet(bundle,field_list(i),field=field,_rc)
         call ESMF_FieldGet(field,rank=rank,_rc)
         if (rank==2) then
            kmvar(i)=0
         else if (rank==3) then
            call ESMF_FieldGet(field,ungriddedLBound=lb,ungriddedUBound=ub,_rc)
            kmvar(i)=ub(1)-lb(1)+1
         else
            _fail("Unsupported rank")
         end if
      end do
      if (present(rc)) then
         RC=_success
      end if
   end function get_level_info

   function get_long_names(bundle,rc) result(long_names)
      character(len=ESMF_MAXSTR), allocatable :: long_names(:)
      type(ESMF_FieldBundle), intent(in) :: bundle
      integer, optional, intent(out) :: rc

      integer :: status
      character(len=ESMF_MAXSTR), allocatable :: field_list(:)
      type(ESMF_Field) :: field
      integer :: i,num_fields

      call ESMF_FieldBundleGet(bundle,fieldCount=num_fields,_rc)
      allocate(field_list(num_fields))
      allocate(long_names(num_fields))
      call ESMF_FieldBundleGet(bundle,fieldNameList=field_list,_rc)
      do i=1,num_fields
         call ESMF_FieldBundleGet(bundle,field_list(i),field=field,_rc)
         call ESMF_AttributeGet(field,name='LONG_NAME',value=long_names(i),_rc)
      enddo
      if (present(rc)) then
         RC=_success
      end if
   end function get_long_names

   function get_units(bundle,rc) result(units)
      character(len=ESMF_MAXSTR), allocatable :: units(:)
      type(ESMF_FieldBundle), intent(in) :: bundle
      integer, optional, intent(out) :: rc

      integer :: status
      character(len=ESMF_MAXSTR), allocatable :: field_list(:)
      type(ESMF_Field) :: field
      integer :: i,num_fields

      call ESMF_FieldBundleGet(bundle,fieldCount=num_fields,_rc)
      allocate(field_list(num_fields))
      allocate(units(num_fields))
      call ESMF_FieldBundleGet(bundle,fieldNameList=field_list,_rc)
      do i=1,num_fields
         call ESMF_FieldBundleGet(bundle,field_list(i),field=field,_rc)
         call ESMF_AttributeGet(field,name='UNITS',value=units(i),_rc)
      enddo
      if (present(rc)) then
         RC=_success
      end if
   end function get_units

   function local_esmf_timeset(yymmdd,hhmmss,rc) result(etime)
      type(ESMF_Time) :: etime
      integer, intent(in) :: yymmdd
      integer, intent(in) :: hhmmss
      integer, intent(out), optional :: rc

      integer :: year,month,day,hour,minute,second,status
      year = yymmdd/10000
      month = mod(yymmdd/100,100)
      day = mod(yymmdd,100)

      hour = hhmmss/10000
      minute = mod(hhmmss/100,100)
      second = mod(hhmmss,100)

      call ESMF_TimeSet(etime,yy=year,mm=month,dd=day,h=hour,m=minute,s=second,_rc)
      if (present(rc)) then
         rc=_success
      endif
   end function local_esmf_timeset

   function defined ( q,undef )
      implicit none
      logical  defined
      real     q,undef
      defined = q /= undef
   end function defined

   subroutine latlon_zstar (q,qp,undef,grid,rc)
      real, intent(inout) :: q(:,:)
      real, intent(out) :: qp(:,:)
      real, intent(in) :: undef
      type (ESMF_Grid), intent(inout) :: grid
      integer, optional, intent(out) :: rc

      integer :: local_dims(3)
      integer im,jm,i,j,status
      real, allocatable :: qz(:)

      call MAPL_GridGet(grid,localCellCountPerDim=local_dims,_rc)
      im = local_dims(1)
      jm = local_dims(2)
      allocate(qz(jm))

      call latlon_zmean ( q,qz,undef,grid )
      do j=1,jm
         if( qz(j).eq. undef ) then
            qp(:,j) = undef
         else
            do i=1,im
               if( defined( q(i,j),undef) ) then
                  qp(i,j) = q(i,j) - qz(j)
               else
                  qp(i,j) = undef
               endif
            enddo
         endif
      enddo
      if (present(rc)) then
         rc=_success
      endif
   end subroutine latlon_zstar

   subroutine latlon_zmean ( q,qz,undef,grid,rc)
      real, intent(inout) ::  q(:,:)
      real, intent(inout) ::  qz(:)
      real, intent(in) :: undef
      type(ESMF_Grid), intent(inout) :: grid
      integer, optional, intent(out) :: rc

      integer :: im,jm,im_global,jm_global,local_dims(3),global_dims(3),status,nx,ny
      real, allocatable ::   qg(:,:)
      real, allocatable :: buf(:,:)
      real :: qsum
      integer :: mpistatus(mpi_status_size)
      integer, allocatable :: ims(:),jms(:)
      integer j,n,peid,peid0,i1,j1,in,jn,mypet,i_start,i_end,isum
      type(ESMF_VM) :: vm

      call ESMF_VMGetCurrent(vm,_rc)
      call ESMF_VMGet(vm,localPet=mypet,_rc)
      call MAPL_GridGet(grid,localCellCountPerDim=local_dims,globalCellCountPerDim=global_dims,_rc)
      im = local_dims(1)
      jm = local_dims(2)
      im_global = global_dims(1)
      jm_global = global_dims(2)
      call get_esmf_grid_layout(grid,nx,ny,ims,jms,_rc)
      call mapl_grid_interior(grid,i1,in,j1,jn)

      qz = 0.0
      allocate( qg(im_global,jm) )
      peid0 = (mypet/nx)*ny
      if (i1==1) then
         i_start = 1
         i_end = ims(1)
         qg(i_start:i_end,:)=q
         do n=1,nx-1
            allocate(buf(ims(n+1),jm))
            peid = mypet + n
            call mpi_recv(buf,ims(n+1)*jm,MPI_FLOAT,peid,peid,MPI_COMM_WORLD,mpistatus,status)
            _verify(status)
            i_start=i_end+1
            i_end = i_start+ims(n)-1
            qg(i_start:i_end,:)=buf
            deallocate(buf)
         enddo
      else
         call mpi_send(q,im*jm,MPI_FLOAT,peid0,mypet,MPI_COMM_WORLD,status)
         _verify(status)
      end if

! compute zonal mean
      if (i1 == 1) then
         do j=1,jm
            isum = count(qg(:,j) /= undef)
            qsum = sum(qg(:,j),mask=qg(:,j)/=undef)
            if (isum == 0) then
               qz(j)=undef
            else
               qz(j)=qsum/real(isum)
            end if
         enddo

! send mean back to other ranks
         do n=1,nx-1
            peid = peid0+n
            call mpi_send(qz,jm,MPI_FLOAT,peid,peid0,MPI_COMM_WORLD,status)
            _verify(status)
         enddo
      else
         call mpi_recv(qz,jm,MPI_FLOAT,peid0,peid0,MPI_COMM_WORLD,mpistatus,status)
         _verify(status)
      end if

      if (present(rc)) then
         rc=_success
      endif

   end subroutine latlon_zmean

   subroutine get_esmf_grid_layout(grid,nx,ny,ims_out,jms_out,rc)
      type(ESMF_Grid), intent(inout) :: grid
      integer, intent(out) :: nx
      integer, intent(out) :: ny
      integer, intent(inout), allocatable :: ims_out(:)
      integer, intent(inout), allocatable :: jms_out(:)
      integer, optional, intent(out) :: rc

      type(ESMF_VM) :: vm
      integer :: status
      type(ESMF_DistGrid) :: dist_grid
      integer, allocatable :: minindex(:,:),maxindex(:,:)
      integer :: dim_count, ndes
      integer, pointer :: ims(:),jms(:)

      call ESMF_VMGetCurrent(vm,_rc)
      call ESMF_VMGet(vm,petCount=ndes,_rc)
      call ESMF_GridGet(grid,distgrid=dist_grid,dimCOunt=dim_count,_rc)
      allocate(minindex(dim_count,ndes),maxindex(dim_count,ndes))
      call MAPL_DistGridGet(dist_grid,minIndex=minindex,maxIndex=maxindex,_rc)
      call MAPL_GetImsJms(minindex(1,:),maxindex(1,:),minindex(2,:),maxindex(2,:),ims,jms,_rc)
      nx = size(ims)
      ny = size(jms)
      allocate(ims_out(nx),jms_out(ny))
      ims_out = ims
      jms_out = jms

      if (present(rc)) then
         rc=_success
      endif

   end subroutine get_esmf_grid_layout

   subroutine check_quad ( quad,vname,nvars,aliases,nalias,qloc )
      integer :: nvars, nalias
      character(len=ESMF_MAXSTR)  quad(2), aliases(2,nalias), vname(nvars)
      integer  qloc(2)
      integer  m,n

! Initialize Location of Quadratics
! ---------------------------------
      qloc = 0

! Check Quadratic Name against HDF Variable Names
! -----------------------------------------------
      do n=1,nvars
         if( trim(vname(n)).eq.trim(quad(1)) ) qloc(1) = n
         if( trim(vname(n)).eq.trim(quad(2)) ) qloc(2) = n
      enddo

! Check Quadratic Name against Aliases
! ------------------------------------
      do m=1,nalias
         if( trim(quad(1)).eq.trim(aliases(1,m)) ) then
            do n=1,nvars
               if( trim(vname(n)).eq.trim(quad(1)) .or. &
                     trim(vname(n)).eq.trim(aliases(2,m)) ) then
                  qloc(1) = n
                  exit
               endif
            enddo
         endif
         if( trim(quad(2)).eq.trim(aliases(1,m)) ) then
            do n=1,nvars
               if( trim(vname(n)).eq.trim(quad(2)) .or. &
                     trim(vname(n)).eq.trim(aliases(2,m)) ) then
                  qloc(2) = n
                  exit
               endif
            enddo
         endif
      enddo

   end subroutine check_quad

   function compute_nsecf (nhms) result(seconds)
      integer :: seconds
      integer, intent(in) :: nhms
      seconds =  nhms/10000*3600 + mod(nhms,10000)/100*60 + mod(nhms,100)
   end function compute_nsecf

   function compute_nhmsf (nsec) result(nhmsf)
      integer :: nhmsf
      integer, intent(in) :: nsec
      nhmsf =  nsec/3600*10000 + mod(nsec,3600)/60*100 + mod(nsec,60)
   end function compute_nhmsf

   subroutine tick (nymd,nhms,ndt)
      integer, intent(inout) :: nymd
      integer, intent(inout) :: nhms
      integer, intent(in) :: ndt

      integer :: nsec

      if(ndt.ne.0) then
         nsec = compute_nsecf(nhms) + ndt

         if (nsec.gt.86400)  then
            do while (nsec.gt.86400)
               nsec = nsec - 86400
               nymd = compute_incymd (nymd,1)
            enddo
         endif

         if (nsec.eq.86400)  then
            nsec = 0
            nymd = compute_incymd (nymd,1)
         endif

         if (nsec.lt.00000)  then
            do while (nsec.lt.0)
               nsec = 86400 + nsec
               nymd = compute_incymd (nymd,-1)
            enddo
         endif

         nhms = compute_nhmsf (nsec)
      endif

   end subroutine tick

   function compute_incymd (nymd,m) result(incymd)
      integer :: incymd
      integer, intent(in) :: nymd
      integer, intent(in) :: m
!***********************************************************************
!  purpose
!     incymd:  nymd changed by one day
!     modymd:  nymd converted to julian date
!  description of parameters
!     nymd     current date in yymmdd format
!     m        +/- 1 (day adjustment)
!
!***********************************************************************
!*                  goddard laboratory for atmospheres                 *
!***********************************************************************

      integer ndpm(12)
      data    ndpm /31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31/
      integer :: ny,nm,nd
!***********************************************************************
!
      ny = nymd / 10000
      nm = mod(nymd,10000) / 100
      nd = mod(nymd,100) + m

      if (nd.eq.0) then
         nm = nm - 1
         if (nm.eq.0) then
            nm = 12
            ny = ny - 1
         endif
         nd = ndpm(nm)
         if (nm.eq.2 .and. is_leap_year(ny))  nd = 29
      endif

      if (nd.eq.29 .and. nm.eq.2 .and. is_leap_year(ny))  go to 20

      if (nd.gt.ndpm(nm)) then
         nd = 1
         nm = nm + 1
         if (nm.gt.12) then
            nm = 1
            ny = ny + 1
         endif
      endif

20    continue
      incymd = ny*10000 + nm*100 + nd
      return

   end function compute_incymd

   logical function is_leap_year(year)
      integer, intent(in) :: year
      is_leap_year = (mod(year,4) == 0) .and. (mod(year,100) == 0 .or. mod(year,400) == 0)
   end function is_leap_year

   subroutine usage(root)
      logical, intent(in) :: root
      integer :: status,errorcode,rc
      if(root) then
         write(6,100)
100      format(  "usage:  ",/,/ &
               " time_ave.x -hdf      filenames (in hdf format)",/ &
               "           <-template template>"    ,/ &
               "           <-tag      tag>"    ,/ &
               "           <-rc       rcfile>" ,/ &
               "           <-ntod     ntod>"   ,/ &
               "           <-ntmin    ntmin>"  ,/ &
               "           <-strict   strict>" ,/ &
               "           <-d>"               ,/ &
               "           <-md>"              ,/,/ &
               "where:",/,/ &
               "  -hdf      filenames:  filenames (in hdf format) to average",/ &
               "  -template  template:  filename to use as template if hdf files differ (default: 1st filename)",/ &
               "  -begdate   yyyymmdd:  optional parameter for date to begin averaging",/ &
               "  -begtime     hhmmss:  optional parameter for time to begin averaging",/ &
               "  -enddate   yyyymmdd:  optional parameter for date to   end averaging",/ &
               "  -endtime     hhmmss:  optional parameter for time to   end averaging",/ &
               "  -tag            tag:  optional tag for output file (default: monthly_ave)",/ &
               "  -rc          rcfile:  optional resource filename for quadratics (default: no quadratics)",/ &
               "  -ntod          ntod:  optional time-of-day (hhmmss) to average (default: all time periods)",/ &
               "  -ntmin        ntmin:  optional parameter for required min. timeperiods (default: 10 days equiv)",/ &
               "  -strict      strict:  optional logical parameter for strict time testing (default: .true.)",/ &
               "  -d             dtag:  optional parameter to create & tag          monthly mean diurnal file  ", &
               "(all times included)",/ &
               "  -md            dtag:  optional parameter to create & tag multiple monthly mean diurnal files ", &
               "(one time  per file)",/ &
               "  -dv            dtag:  like -d  but includes diurnal variances",/ &
               "  -mdv           dtag:  like -md but includes diurnal variances",/ &
               )
      endif
      call MPI_Abort(MPI_COMM_WORLD,errorcode,status)
      _verify(status)
   end subroutine usage

    subroutine generate_report()

         character(:), allocatable :: report_lines(:)
         integer :: i
         character(1) :: empty(0)

         reporter = ProfileReporter(empty)
         call reporter%add_column(NameColumn(20))
         call reporter%add_column(FormattedTextColumn('Inclusive','(f12.2)', 12, InclusiveColumn('MEAN')))
         call reporter%add_column(FormattedTextColumn('% Incl','(f6.2)', 6, PercentageColumn(InclusiveColumn('MEAN'),'MAX')))
         call reporter%add_column(FormattedTextColumn('Exclusive','(f12.2)', 12, ExclusiveColumn('MEAN')))
         call reporter%add_column(FormattedTextColumn('% Excl','(f6.2)', 6, PercentageColumn(ExclusiveColumn('MEAN'))))
         call reporter%add_column(FormattedTextColumn(' Max Excl)','(f12.2)', 12, ExclusiveColumn('MAX')))
         call reporter%add_column(FormattedTextColumn(' Min Excl)','(f12.2)', 12, ExclusiveColumn('MIN')))
         call reporter%add_column(FormattedTextColumn('Max PE)','(1x,i5.5,1x)', 7, ExclusiveColumn('MAX_PE')))
         call reporter%add_column(FormattedTextColumn('Min PE)','(1x,i5.5,1x)', 7, ExclusiveColumn('MIN_PE')))
        report_lines = reporter%generate_report(t_prof)
         if (mapl_am_I_root()) then
            write(*,'(a)')'Final profile'
            write(*,'(a)')'============='
            do i = 1, size(report_lines)
               write(*,'(a)') report_lines(i)
            end do
            write(*,'(a)') ''
         end if
    end subroutine generate_report


end program time_ave
