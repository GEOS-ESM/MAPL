    program test

    use ESMF_CFIOGridMOD
    use ESMF_CFIOVarInfoMOD
    use ESMF_CFIOFileMOD
    use ESMF_CFIOSdfMOD
    use ESMF_CFIOMOD

!   define ESMF_CFIO,  ESMF_CFIOVarInfo, and ESMF_CFIOGrid objects
    type(ESMF_CFIO) :: cfio
    type(ESMF_CFIOVarInfo), pointer :: vars(:)
    type(ESMF_CFIOGrid), pointer :: grid

    character(len=20) :: fileName
    real, pointer :: ts(:), tmpu(:,:), u(:,:), v(:,:), ps(:)
    integer :: i, fmode, rc, km
    integer :: date, curTime, timeInc, t
    integer :: hhmmss(2) = (/0, 60000/)
    real :: lev(5) = (/1000., 850., 500., 300., 50./)
    real :: range(2) = (/-1.E10, 1.E10/)
    logical :: twoD
    real :: lon(5*5), lat(5*5), dlat, dlon
    integer :: im=5, jm=5
    logical :: passed = .true.

    real :: kappa = 0.28571428571428575

    fmode = 0

    dlat = 180./(jm-1)
    dlon = 360./im
    do j = 1, jm
    do i =1, im
      lon(i+(j-1)*im) = -180 + (i-1)*dlon
      lat(i+(j-1)*im) = -90 + (j-1)*dlat
    end do
    end do

! Create grid and set grid attributes
    allocate(grid)
    grid = ESMF_CFIOGridCreate(gName='station3d')
    call ESMF_CFIOGridSet(grid,lon=lon,lat=lat,lev=lev,levUnit='hPa')
    call ESMF_CFIOGridSet(grid, standardName='atmosphere_pressure_coordinate')
    call ESMF_CFIOGridSet(grid, coordinate = 'pressure')

!   read back im, jm, km
    call ESMF_CFIOGridGet(grid, km=km)

! Create variable object and set variable attributes
    allocate(vars(5))

    vars(1) = ESMF_CFIOVarInfoCreate(vName='ps')
    call ESMF_CFIOVarInfoSet(vars(1), vName='ps', vTitle='surface pressure', grid=grid)
    call ESMF_CFIOVarInfoSet(vars(1), amiss=1.E15, scaleFactor=1., addOffSet=0.)
    call ESMF_CFIOVarInfoSet(vars(1), standardName='ps', twoDimVar=.true.)
    call ESMF_CFIOVarInfoSet(vars(1), validRange=range, vUnits='K')
                                                                                   
    vars(2) = ESMF_CFIOVarInfoCreate(vName='ts')
    call ESMF_CFIOVarInfoSet(vars(2),vName='ts',vTitle='skin temperature', grid=grid)
    call ESMF_CFIOVarInfoSet(vars(2), standardName='tskin', twoDimVar=.true.)
    call ESMF_CFIOVarInfoSet(vars(2), validRange=range, vUnits='K')
                                                                                   
    vars(3) = ESMF_CFIOVarInfoCreate(vName='tmpu')
    call ESMF_CFIOVarInfoSet(vars(3), vName='tmpu', vTitle='temperature', grid=grid)
    call ESMF_CFIOVarInfoSet(vars(3), amiss=1.E15, scaleFactor=1., addOffSet=0.)
    call ESMF_CFIOVarInfoSet(vars(3), standardName='tmpu')
    call ESMF_CFIOVarInfoSet(vars(3), validRange=range, vUnits='K')

    vars(4) = ESMF_CFIOVarInfoCreate(vName='u')
    call ESMF_CFIOVarInfoSet(vars(4), vName='u', vTitle='zonal wind', grid=grid)
    call ESMF_CFIOVarInfoSet(vars(4), amiss=1.E15, scaleFactor=1., addOffSet=0.)
    call ESMF_CFIOVarInfoSet(vars(4), standardName='uwnd')
    call ESMF_CFIOVarInfoSet(vars(4), validRange=range, vUnits='m/s')

    vars(5) = ESMF_CFIOVarInfoCreate(vName='v')
    call ESMF_CFIOVarInfoSet(vars(5), vName='v', vTitle='V wind', grid=grid)
    call ESMF_CFIOVarInfoSet(vars(5), amiss=1.E15, scaleFactor=1., addOffSet=0.)
    call ESMF_CFIOVarInfoSet(vars(5), standardName='vwnd')
    call ESMF_CFIOVarInfoSet(vars(5), validRange=range, vUnits='m/s')

! Create CFIO object and set global attributes

    cfio =  ESMF_CFIOCreate(cfioObjName='ana')
    call ESMF_CFIOSet(cfio, fName='GEOS5.ana.hdf', varObjs=vars, &
                   grid=grid, date=20011201, BegTime=0, timeInc=60000)
    call ESMF_CFIOSet(cfio, title="c403_cer_01: FVGCM Diagnostics",  &
                   source="Global Modeling and Assimilation Office", &
                   contact="data@gmao.gsfc.nasa.gov")
    call ESMF_CFIOSet(cfio, history='File written by CFIO v1.0.0', &
           convention='ESMF', institution="Global Modeling and Assimilation Office,&
       &     NASA Goddard Space Flight Center, Greenbelt, MD 20771") 
    call ESMF_CFIOSet(cfio, references='see ESMF', comment='First CFIO test version',prec=0) 

! All metadata are set. Get some metadata to see what in the CFIO Obj.

    call ESMF_CFIOGet(cfio, nVars=nVars, date=date, begTime=curTime, &
                   timeInc=timeInc, fName=fileName)

! Create output file
    call ESMF_CFIOFileCreate(cfio)

! put some test data into the variables
    allocate(tmpu(im*jm,km), ts(im*jm),ps(im*jm),u(im*jm,km),v(im*jm,km))
  do t = 1, 2
    do j = 1, jm
         do i = 1, im
            ps(i+(j-1)*im) = 100000 + (t-1)*cos(2*3.14*lat(j)/180.)*sin(2*3.14*2*lon(i)/360.)
            ts(i+(j-1)*im) = 273 - (t-1)*sin(2*3.14*lat(j)/180.)*cos(2*3.14*2*lon(i)/360.)
         enddo
       enddo

    do k =1, km
     do j = 1, jm
       do i = 1, im
          tmpu(i+(j-1)*im,k) = ts(i+(j-1)*im)*(lev(k)/1000.)**kappa
          u(i+(j-1)*im,k) = 10*(t-1)*cos(2*3.14*lat(j)/180.)*sin(2*3.14*lon(i)/360.)*exp(lev(k)/1000.)
          v(i+(j-1)*im,k) = 10*(t-1)*cos(2*3.14*lat(j)/180.)*cos(2*3.14*lon(i)/360.)*exp(lev(k)/1000.)
       enddo
     enddo
    enddo

! Write data to the output file

    call ESMF_CFIOVarWrite(cfio, 'ps', ps, date, hhmmss(t), rc=rc)
    if ( rc .ne. 0 ) passed = .false.
    call ESMF_CFIOVarWrite(cfio, 'ts', ts, date, hhmmss(t), rc=rc)
    if ( rc .ne. 0 ) passed = .false.
    call ESMF_CFIOVarWrite(cfio, 'tmpu', tmpu, date, hhmmss(t), rc=rc)
    if ( rc .ne. 0 ) passed = .false.
    call ESMF_CFIOVarWrite(cfio, 'u', u, date, hhmmss(t), rc=rc)
    if ( rc .ne. 0 ) passed = .false.
    call ESMF_CFIOVarWrite(cfio, 'v', v, date, hhmmss(t), rc=rc)
    if ( rc .ne. 0 ) passed = .false.

  end do
! Close the file
    call ESMF_CFIOFileClose(cfio)

    if ( passed ) then
       print *, "testw_st: Passed"
    else
       print *, "testw_st: NOT Passed"
    end if

    stop
   end 
